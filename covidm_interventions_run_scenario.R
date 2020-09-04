#.libPaths("/shared/rlib/")
cm_version = 1;
cm_force_rebuild = F;
cm_build_verbose = F;
cm_force_shared = T

suppressPackageStartupMessages({
  require(data.table)
})

.args <- if (interactive()) c(
  "/shared/covid/covidm", "helper_functions.R",
  "18", "niger",
  "/shared/covid/covid_lmic/interventions/reviewer_comments/inputs",
  "/shared/covid/covid_lmic/interventions/reviewer_comments/sceni_1_niger_cases.rds"
) else commandArgs(trailingOnly = TRUE)

cm_path = .args[1]
source(.args[2])
scenario_index <- as.integer(.args[3])
country <- .args[4]
inputpth <- sprintf("%s/%s",.args[5], tolower(country))
tarfile <- tail(.args, 1)
othertar <- gsub("bytime","byage",tarfile)
cm_force_rebuild = F;
cm_build_verbose = F;
cm_force_shared = T

source(paste0(cm_path, "/R/covidm.R"))

#this will read run_options.rds country specific
.inputfns <- list.files(inputpth, full.names = TRUE)
#' TODO check for presence of all relevant inputs
#' if country specific files don't exist
#'   go up one level, and use default files

for (.fn in .inputfns) {
  .nm <- gsub(sprintf("^%s/(.+)\\.rds$",path.expand(inputpth)),"\\1",.fn)
  assign(.nm, readRDS(.fn))
}

scenarios_overview <- readRDS(sprintf("%s/scenarios_overview.rds", .args[5]))
scenarios <- readRDS(sprintf("%s/scenarios.rds", .args[5]))

#run options now country specific, automatically loaded
#run_options <- readRDS(sprintf("%s/run_options.rds", .args[5]))

scen <- scenarios_overview[index == scenario_index, scen]
s <- scenarios_overview[index == scenario_index, s]

#' TODO this shadows country?
attach(scenarios[[scen]][s,])

if (scen != 1){
  unmitigated <- readRDS(gsub("_\\d+_","_1_", tarfile))
  unmitigated <- unmitigated[compartment == "cases"]
  unmitigated <- unmitigated[, .(value=sum(value)), by=c("run", "t", "compartment")]
  # TODO assert: params_set[[1]]$pop always has size 1
  tpop <- sum(params_set[[1]]$pop[[1]]$size)
  unmitigated[, incidence := value/tpop ]
}

#' set up paramaters
seed_cases = 50;
if(hirisk_prop_isolated > 0){
  
  params <- params_set[[2]]
  #assign population actually isolated in high-risk
  params$pop[[1]][["size"]] <- params$pop[[1]][["size"]] + (1-hirisk_prop_isolated)*params$pop[[2]][["size"]]
  params$pop[[2]][["size"]] <- params$pop[[2]][["size"]]*(hirisk_prop_isolated)
  
  #make sure contact patterns are the same as before shielding
  params$pop[[1]][["tau"]] = c( rep(1, 12), rep(1-hirisk_prop_isolated, 4) );
  params$pop[[2]][["tau"]] = c( rep(0, 12), rep(hirisk_prop_isolated, 4) );
  
  #distribute initial cases evenly
  params$pop[[1]]$dist_seed_ages <- c( rep(1, 12), rep(1-hirisk_prop_isolated, 4))
  params$pop[[2]]$dist_seed_ages <- c( rep(0, 12), rep(hirisk_prop_isolated, 4) )
  
  params$pop[[1]]$seed_times <- rep(0, round(seed_cases * hirisk_prop_isolated))
  params$pop[[2]]$seed_times <- rep(0, round(seed_cases * (1-hirisk_prop_isolated)))
  
} else {
  params <- params_set[[1]]
  params$pop[[1]]$seed_times <- rep(0, seed_cases)
}

#results_cases_byage <- list()
#results_cases_bytime <- list()
#results_cases <- list()
#results_I <- list()

results_time <- list()
results_total365 <- list()

params_back <- params

for(i in 1:nrow(run_options)){
#for(i in c(1:115)){
  print(i)
  params <- params_back
  
  if (gen_socdist | (hirisk_prop_isolated > 0)) {
    iv = cm_iv_build(params)
    #' general social distancing
    if(gen_socdist){
      if(is.list(gen_socdist_start)){
        gen_socdist_startdate <- sapply(
          1:length(gen_socdist_start[[1]]),
          function(x){
            if(gen_socdist_start[[1]][x] == "incidence"){
              threshold_time <- unmitigated[
                run == i &
                incidence >= gen_socdist_schedule_filter_on_threshold[[1]][x]
              ][1,t]
              if(is.na(threshold_time)){
                threshold_time <- 1e6
              }
              return(as.character(as.Date(params$date0) + threshold_time))
            } else {
              return(0)
            }
        })
      } else {
        threshold_time <- unmitigated[
          run == i &
          incidence >= gen_socdist_schedule_filter_on_threshold
        ][1,t]
        if(is.na(threshold_time)){threshold_time <- 1e6}
        gen_socdist_startdate <- as.Date(params$date0) + threshold_time
      }
      if(is.list(gen_socdist_stop)){
        gen_socdist_stopdate <- as.Date(gen_socdist_startdate) + sapply(
          1:length(gen_socdist_stop[[1]]),
          function(x){
            return(gen_socdist_stop[[1]][x]*round(365/12))
          }
        )
      } else {
          gen_socdist_stopdate <- as.Date(gen_socdist_startdate) + gen_socdist_stop*round(365/12) 
      }
      gen_socdist_startdate <- as.Date(gen_socdist_startdate)
      gen_socdist_stopdate <- as.Date(gen_socdist_stopdate)
      if(length(gen_socdist_startdate) > 1){
        for(d in 2:length(gen_socdist_startdate)){
          if(gen_socdist_startdate[d] <= gen_socdist_stopdate[d-1]){
            gen_socdist_startdate[d] <- gen_socdist_stopdate[d-1]+1
          }
        } 
      }
      cm_iv_general_socdist(iv, gen_socdist_startdate, gen_socdist_stopdate, gen_socdist_contact)  
    }
    
    #' check if we need to adjust travel patterns
    if(hirisk_prop_isolated > 0){
      #assume same risk high-risk to low-risk as low-risk to high-risk
      update_travel <- matrix(rep(hirisk_lorisk_contact, 4), 2)
      #how much do high_risk groups contact each-other
      diag(update_travel) <- hirisk_contact
      #how much does low-risk group contact high-risk groups
      update_travel[1,] <- hirisk_lorisk_contact
      #no change in lorisk_lorisk contact
      update_travel[1,1] <- 1
      
      #shielding
      if(hirisk_shield_start == "incidence"){
        threshold_time <- unmitigated[
          run == i &
          incidence >= hirisk_shield_schedule_filter_on_threshold
        ][1,t]
        if(is.na(threshold_time)){threshold_time <- 1e6}
        hirisk_shield_startdate <- as.Date(params$date0) + threshold_time
      }
      hirisk_shield_stopdate <- as.Date(hirisk_shield_startdate) + hirisk_shield_stop*round(365/12) 
      
      iv[, travel := list(params$travel)]
      cm_iv_travel(iv, hirisk_shield_startdate, hirisk_shield_stopdate, list(update_travel))
    }
    
    params = cm_iv_apply(params, iv)
    
  }
  
  # if we have bootstrap 
  refcm <- if (is.null(names(contact_matrices))) { contact_matrices[[i]] } else { contact_matrices }
  #names(refcm) <- gsub("cm_","",names(refcm))
  
  #use contact matrices for current sample
  params$pop <- lapply(
    params$pop,
    function(x){
      x$matrices <- refcm
      return(x)
    }
  )
  
  #adjust r0 to that in current sample
  target_R0 <- run_options[i, r0]
  params$pop <- lapply(
    params$pop,
    function(x){
      x$u <- x$u * (target_R0 / cm_calc_R0_extended(params))
      return(x)
    }
  )
  
  #' set up interventions
  params$pop <- lapply(
    params$pop,
    function(x){x$fIs <- x$fIs*symptomatic_contact; return(x)}
  )
  
  #run the model
  result <- cm_simulate(
    params,
    1,
    model_seed = run_options[i, model_seed]
  )$dynamics
  
  #also output overall number infected
  #result_cases <- result[compartment %in% c("cases", "icu10_p", "nonicu10_p", "death10_o", "icu5_p", "nonicu5_p", "death5_o", "icu0_p", "nonicu0_p", "death0_o")]
  #result_time <- result[compartment %in% c("cases", "icu10_p", "nonicu10_p", "death10_o", "icu5_p", "nonicu5_p", "death5_o", "icu0_p", "nonicu0_p", "death0_o"), .(group="all",value=sum(value)), by=c("run","t","population","compartment")]
  #result_total365 <- result[t <= 365 & compartment %in% c("cases", "icu10_p", "nonicu10_p", "icu10_o", "nonicu10_o", "death10_o", "icu5_p", "nonicu5_p", "icu5_o", "nonicu5_o", "death5_o", "icu0_p", "nonicu0_p", "icu0_o", "nonicu0_o", "death0_o"), .(t=365,value=sum(value)), by=c("run","population","compartment", "group")]
  
  result_time <- result[compartment %in% c("cases", "icu_p", "nonicu_p", "death_o"), .(group="all",value=sum(value)), by=c("run","t","population","compartment")]
  result_total365 <- result[t <= 365 & compartment %in% c("cases", "icu_p", "nonicu_p", "icu_o", "nonicu_o", "death_o"), .(t=365,value=sum(value)), by=c("run","population","compartment", "group")]
  
  result_time[, "run"] <- i
  result_time[, "s"] <- s
  
  result_total365[, "run"] <- i
  result_total365[, "s"] <- s
  
  #result_S[, "s"] <- s
  #result_S[, "run"] <- i
  
  #' TODO pretty sure this is OBE now w/ 160/161
  #if we are shielding population, need to merge populations into one
  # if(hirisk_prop_isolated > 0){
  #   result <- result[,
  #     .(
  #       value=sum(value),
  #       population=country
  #     ),
  #     by=c("run","t","group","compartment","scenario")
  #   ][, colnames(result), with=F]
  # }
  
  #result_cases_byage <- result[, .(value=sum(value)), by=c("run","group","compartment","population","scenario")]
  #result_cases_bytime <- result[, .(value=sum(value)), by=c("run","t","compartment","population","scenario")]
  
  #results_cases_byage[[length(results_cases_byage) + 1]] <- result_cases_byage
  #results_cases_bytime[[length(results_cases_bytime) + 1]] <- result_cases_bytime
  results_time[[length(results_time) + 1]] <- result_time
  results_total365[[length(results_total365) + 1]] <- result_total365
  #results_S[[length(results_S) + 1]] <- result_S
}

#saveRDS(rbindlist(results_cases_bytime) , tarfile)
#saveRDS(rbindlist(results_cases_byage), othertar)

saveRDS(rbindlist(results_time) , tarfile)



saveRDS(rbindlist(results_total365), sprintf("%s_total365.rds", tarfile))

