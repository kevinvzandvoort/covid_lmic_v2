  #' default simulation parameters
  suppressPackageStartupMessages({
    require(data.table)
  })
  
  .args <- if (interactive()) c(
    "~/workspace/covid_lmic/interventions/generation_data/data_contacts_missing.csv",
    "~/workspace/covidm", "mauritius",
    "~/workspace/covid_lmic/interventions/reviewer_comments/inputs"
  ) else commandArgs(trailingOnly = TRUE)
  
  lmic_adjust=c(10,5,0)[1]
  fIa_value = c(0.5, 0, 1)[1]
  
  reference = fread(.args[1])
  cm_path = .args[2]
  target = .args[3]
  outdir = tail(.args, 1)
  
  cm_force_rebuild = F;
  cm_build_verbose = F;
  cm_force_shared = T;
  source(paste0(cm_path, "/R/covidm.R"))
  
  country <- cm_populations[gsub(" ","",gsub("[^a-zA-Z]","",tolower(as.character(name))))==target, unique(as.character(name))]
  matref <- reference[name == country, ifelse(cm, name, cm_name)]
  if (!length(matref)) matref <- country
  
  stopifnot(length(country)==1)
  
  #source("./helper_functions.R")
  
  #set up node-runs
  #country <- c("Uganda","Zimbabwe","Kenya")[3]
  #seed_cases <- 50
  
  #general population parameters
  params_set <- list()
  
  params1 <- cm_parameters_SEI3R(
    country, matref,
    deterministic=FALSE
  )
  
  #set up node-runs
  probs = fread(
    "Age_low,Age_high,Prop_symptomatic,IFR,Prop_inf_hosp,Prop_inf_critical,Prop_critical_fatal,Prop_noncritical_fatal,Prop_symp_hospitalised,Prop_hospitalised_critical
  0,9,0.66,8.59E-05,0.002361009,6.44E-05,0.5,0,0,0.3
  10,19,0.66,0.000122561,0.003370421,9.19E-05,0.5,9.47E-04,0.007615301,0.3
  20,29,0.66,0.000382331,0.010514103,0.000286748,0.5,0.001005803,0.008086654,0.3
  30,39,0.66,0.000851765,0.023423527,0.000638823,0.5,0.001231579,0.009901895,0.3
  40,49,0.66,0.001489873,0.0394717,0.001117404,0.5,0.002305449,0.018535807,0.3
  50,59,0.66,0.006933589,0.098113786,0.005200192,0.5,0.006754596,0.054306954,0.3
  60,69,0.66,0.022120421,0.224965092,0.016590316,0.5,0.018720727,0.150514645,0.3
  70,79,0.66,0.059223786,0.362002579,0.04441784,0.5,0.041408882,0.332927412,0.3
  80,100,0.66,0.087585558,0.437927788,0.065689168,0.5,0.076818182,0.617618182,0.3"
  )
  
  #increase CFR
  cfr_RR <- 1.5
  probs[, Prop_critical_fatal := Prop_critical_fatal * cfr_RR]
  probs[, Prop_noncritical_fatal := Prop_noncritical_fatal * cfr_RR]
  
  #min(1, x) does not work for noncritical_fatal, for some reason
  probs[Prop_critical_fatal > 1, Prop_critical_fatal := 1]
  probs[Prop_noncritical_fatal > 1, Prop_noncritical_fatal := 1]
  
  reformat = function(P, lmic_adjust=10) {
    # no info to re-weight these, so assume 70-74 is like 70-79, and 75+ is like 80+
    if(lmic_adjust==10){
      P <- P[2:length(P)]
      return ( rep(P[1:8], each = 2) )
    } else if(lmic_adjust==5) {
      return ( c(P[1], rep(P[2:8], each = 2), P[9]) ) 
    } else if(lmic_adjust==0) {
      prop75_79 <- data.table(
        country=c("niger","nigeria","mauritius"),
        prop=c(0.4756839,0.6595994,0.6841553)
      )
      return( c(rep(P[1:7], each = 2), P[8], (prop75_79[country == target, prop]*P[8])+( (1-prop75_79[country == target, prop])*P[9])) )
    } else {
      stop("cannot process this lmic_adjust")
    }
  }
  
  P.icu_symp     = reformat(probs[, Prop_symp_hospitalised * Prop_hospitalised_critical], lmic_adjust = lmic_adjust);
  P.nonicu_symp  = reformat(probs[, Prop_symp_hospitalised * (1 - Prop_hospitalised_critical)], lmic_adjust = lmic_adjust);
  P.death    = reformat(probs[, Prop_noncritical_fatal], lmic_adjust = lmic_adjust);
  
  #P.icu_symp10     = reformat(probs[, Prop_symp_hospitalised * Prop_hospitalised_critical], lmic_adjust = 10);
  #P.nonicu_symp10  = reformat(probs[, Prop_symp_hospitalised * (1 - Prop_hospitalised_critical)], lmic_adjust = 10);
  #P.death10    = reformat(probs[, Prop_noncritical_fatal], lmic_adjust = 10);
  
  #P.icu_symp5     = reformat(probs[, Prop_symp_hospitalised * Prop_hospitalised_critical], lmic_adjust = 5);
  #P.nonicu_symp5  = reformat(probs[, Prop_symp_hospitalised * (1 - Prop_hospitalised_critical)], lmic_adjust = 5);
  #P.death5    = reformat(probs[, Prop_noncritical_fatal], lmic_adjust = 5);
  
  #P.icu_symp0     = reformat(probs[, Prop_symp_hospitalised * Prop_hospitalised_critical], lmic_adjust = 0);
  #P.nonicu_symp0  = reformat(probs[, Prop_symp_hospitalised * (1 - Prop_hospitalised_critical)], lmic_adjust = 0);
  #P.death0    = reformat(probs[, Prop_noncritical_fatal], lmic_adjust = 0);
  
  max_time <- 60
  tres <- 0.25
  
  ponset2hosp <- cm_delay_gamma(7, 7, max_time, tres)$p
  pignore <- cm_delay_skip(max_time, tres)$p
  icustay <- cm_delay_gamma(10, 10, max_time, tres)$p
  nonicustay <- cm_delay_gamma(8, 8, max_time, tres)$p
  ponset2death <- cm_delay_gamma(22, 22, max_time, tres)$p
  
  cm_multinom_process <- function(
    src, outcomes, delays,
    report = ""
  ) {
    if ("null" %in% names(outcomes)) {
      if (length(report) != length(outcomes)) report <- rep(report, length(outcomes))
      report[which(names(outcomes)=="null")] <- ""
      if (!("null" %in% names(delays))) {
        delays$null <- c(1, rep(0, length(delays[[1]])-1))
      }
    } else if (!all(rowSums(outcomes)==1)) {
      report <- c(rep(report, length(outcomes)), "")
      outcomes$null <- 1-rowSums(outcomes)
      delays$null <- c(1, rep(0, length(delays[[1]])-1))
    }
    nrow <- length(outcomes)
    list(
      source = src, type="multinomial", names=names(outcomes), report = report,
      prob = t(as.matrix(outcomes)), delays = t(as.matrix(delays))
    )
  }
  
  cm_track_process <- function(src, name, delays, agecats = 16, report = "p") {
    list(
      source = src, type="multinomial", names = name, report = report,
      prob = matrix(1, nrow = 1, ncol = agecats),
      delays = t(delays)
    )
  }
  
  burden_processes = list(
    # process of sending symptomatic cases to hospital icu or ward
    cm_multinom_process(
      "Ip",
      outcomes = data.frame(to_icu = P.icu_symp, to_nonicu = P.nonicu_symp),
      delays = data.frame(to_icu = ponset2hosp, to_nonicu = ponset2hosp)
    ),
    #cm_multinom_process(
    #  "Ip",
    #  outcomes = data.frame(to_icu5 = P.icu_symp5, to_nonicu5 = P.nonicu_symp5),
    #  delays = data.frame(to_icu5 = ponset2hosp, to_nonicu5 = ponset2hosp)
    #),
    #cm_multinom_process(
    #  "Ip",
    #  outcomes = data.frame(to_icu0 = P.icu_symp0, to_nonicu0 = P.nonicu_symp0),
    #  delays = data.frame(to_icu0 = ponset2hosp, to_nonicu0 = ponset2hosp)
    #),
    # track icu prevalance
    cm_track_process("to_icu", "icu", icustay),
    #track icu incidence
    cm_track_process("to_icu", "icu", icustay, report = "o"),
    # track ward prevalence
    cm_track_process("to_nonicu", "nonicu", nonicustay),
    #trac
    cm_track_process("to_nonicu", "nonicu", nonicustay, report = "o"),
    
    ## track icu prevalance
    #cm_track_process("to_icu5", "icu5", icustay),
    ##track icu incidence
    #cm_track_process("to_icu5", "icu5", icustay, report = "o"),
    ## track ward prevalence
    #cm_track_process("to_nonicu5", "nonicu5", nonicustay),
    ##trac
    #cm_track_process("to_nonicu5", "nonicu5", nonicustay, report = "o"),
    #
    ## track icu prevalance
    #cm_track_process("to_icu0", "icu0", icustay),
    ##track icu incidence
    #cm_track_process("to_icu0", "icu0", icustay, report = "o"),
    ## track ward prevalence
    #cm_track_process("to_nonicu0", "nonicu0", nonicustay),
    ##trac
    #cm_track_process("to_nonicu0", "nonicu0", nonicustay, report = "o"),
    
    # track infections - get from delta R prevalence
    #  cm_track_process("S", "infection", pignore, report="i"),
    # send some cases to death, tracking outcidence
    cm_multinom_process(
      "Ip",
      outcomes = data.table(death=P.death),
      delays = data.table(death=ponset2death),
      report = "o"
    )
    #cm_multinom_process(
    #  "Ip",
    #  outcomes = data.table(death5=P.death5),
    #  delays = data.table(death5=ponset2death),
    #  report = "o"
    #),
    #cm_multinom_process(
    #  "Ip",
    #  outcomes = data.table(death0=P.death0),
    #  delays = data.table(death0=ponset2death),
    #  report = "o"
    #)
  )
  
  popnorm <- function(x){
    #no contact matrix - will be overwritten by empirical ones
    x$matrices$home <- x$matrices$work <- x$matrices$school <- x$matrices$other <- x$matrices$other*0
    
    #age-specific probability of being symptomatic
    #x$y <- c(rep(0.056, 3), rep(0.49, 8), rep(0.74, 8))
  
    x$y <- c(
      rep(0.2973718, 2), rep(0.2230287, 2), rep(0.4191036, 2),
      rep(0.4445867, 2), rep(0.5635720, 2), rep(0.8169443, 6)
    )
    
    x$fIa <- rep(fIa_value, length(x$fIa))
  	  
    return(x)
  }
  
  params1$pop <- lapply(params1$pop, popnorm)
  params1$time1 <- as.Date(params1$time1) + 365*0
  
  params1$processes <- burden_processes
  
  params_set[[1]] <- params1
  
  params2 <- cm_parameters_SEI3R(
    rep(country, 2), rep(matref, 2),
    deterministic=FALSE
  )
  
  #no shielding in <60yo, full eligibility for shielding in 60+
  prop_highrisk <- c(rep(0, 12), rep(1, 4))
  
  #no seeding events in high risk population
  params2$pop[[2]]$name <- paste0(params2$pop[[2]]$name, " - high risk")
  params2$pop[[2]]$size <- (prop_highrisk) * params2$pop[[1]]$size
  
  params2$pop[[1]]$name <- paste0(params2$pop[[1]]$name, " - low risk")
  params2$pop[[1]]$size <- (1 - prop_highrisk) * params2$pop[[1]]$size
  
  params2$pop <- lapply(params2$pop, popnorm)
  params2$pop[[2]]$seed_times <- 1e6
  
  #normal mixing between populations
  params2$travel <- matrix(rep(1, 4), 2)
  params2$time1 <- as.Date(params2$time1) + 365*0
  
  params2$processes <- burden_processes
  
  params_set[[2]] <- params2
  
  dir.create(sprintf("%s/%s", outdir, target))
  saveRDS(params_set, sprintf("%s/%s/params_set.rds", outdir, target))
  
