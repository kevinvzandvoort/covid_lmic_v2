library(data.table)
#setwd("~/workspace/covid_lmic/interventions/reviewer_comments/output/")

getData <- function(country, scenario, type="total", max_time=ceiling(365*1.5)){
  if(type=="total"){
    return(as.data.table(readRDS(sprintf("%s/%s/sceni_%d_%s_cases.rds_total365.rds", data_dir, country, scenario, country))[, scen := scenario]))
  } else if(type=="time"){
    return(as.data.table(readRDS(sprintf("%s/%s/sceni_%d_%s_cases.rds", data_dir, country, scenario, country))[t <= max_time][, scen := scenario]))
  }
}

getQuantileRuns <- function(data, qntls = c("low95"=0.025, "low50"=0.25, "median"=0.5, "high50"=0.75, "high95"=0.975), bycols=c("population", "compartment", "group")){
  data_qntls <- melt(
    data[,
      .(cumvalue=sum(value)), by=c("run", bycols)
    ][, names(qntls) := lapply(
        #quantile(cumvalue, qntls),
        #rep, each={
        #  x = unique(data[, .(Nruns=length(unique(.SD[, run]))), by=bycols][, Nruns]);
        #  ifelse(length(x)==1, x, stop("multiple length runs in bycols subset"))
        #}
        qntls, function(x) quantile(cumvalue, x)
    ), by=bycols],
    measure.vars=names(qntls)
  )
  data_qntls <- data_qntls[, diff := abs(cumvalue-value)][, first(.SD[diff==min(diff)]), by=c(bycols, "variable")]
  return(data_qntls[, c(bycols, "variable", "run"), with=FALSE]) 
}

getQuantileRunData <- function(data, qntls = c("low95"=0.025, "low50"=0.25, "median"=0.5, "high50"=0.75, "high95"=0.975), bycols=c("population", "compartment", "group")){
  data_qntls <- getQuantileRuns(data, qntls, bycols)
  data <- merge(data_qntls, data, by=c(bycols, "run"), all.x=TRUE)
  return(data)
}

getPeakValues <- function(data, qntls = c("low95"=0.025, "low50"=0.25, "median"=0.5, "high50"=0.75, "high95"=0.975), bycols=c("population", "compartment", "s")){
  data_peaks <- data[, first(.SD[value==max(value), .(peak_time=t, peak_value=value)]), by=c("run", bycols)]
  data_peaks <- data_peaks[, .(type=names(qntls), peak_time=quantile(peak_time, qntls), peak_value=quantile(peak_value, qntls)), by=bycols]
  return(melt(data_peaks, measure.vars=c("peak_time", "peak_value"), variable.factor=FALSE, value.factor=FALSE))
}

getQuantileValues <- function(data, qntls=c("low95"=0.025, "low75"=0.125, "low50"=0.25, "median"=0.5, "high50"=0.75, "high75"=0.875, "high95"=0.975), bycols=c("population", "compartment", "group")){
  if( ("group" %in% colnames(data)) & (length(unique(data[, group])) > 1) & (!"group" %in% bycols)) {
    data_qntls <- data[, .(value=sum(value)), by=c("run", bycols)][, group := "all"]
  }  else {
    data_qntls = data
  }
  if(length(is.na(data_qntls[, value])) > 0){warning("NA/NaNs in data for get QuantileValues. Possible division by 0. Removing Nas.")}
  data_qntls <- data_qntls[, .(type=names(qntls), value=quantile(value, qntls, na.rm=T)), by=bycols]
  return(data_qntls)
}

formatNumber <- function(x, max=5, min=1, decimal=0, postfix=""){
  n = nchar(strsplit(as.character(x), ".", fixed = T)[[1]][1])
  if(decimal>0){ n=1 } else if(n >= min(n, max) ){ n = min(n, max) } else if(n >= min(n, min)){ n = min(n, min) }
  x = round(x/(10^(n-1)), decimal)*(10^(n-1))
  return(paste0(format(x, big.mark = ",", big.interval = 3, scientific=FALSE), postfix))
}

makeTable <- function(data_total, data_time=NULL, return_relative=TRUE, bycols=c("population", "compartment", "scen", "s")){
  data_total_qntl <- getQuantileValues(data_total, bycols=bycols, qntls=c("low"=0.025, "median"=0.5, "high"=0.975)) 
  data_total_qntl[, "variable"] <- "total_value"
  
  data_total <- dcast(data_total_qntl, ...~type, value.var="value")
  data_total <- data_total[, .(format=sprintf("%s (%s to %s)", formatNumber(median), formatNumber(low), formatNumber(high))), by=c(bycols, "variable")]
  
  if(return_relative){
    popsize <- data.table(
      country=c("Niger", "Nigeria", "Mauritius"),
      popsize=c(24100000,202900000,1300000)
    )
    data_relative <- merge(data_total_qntl, popsize, by.x="population", by.y="country")
    data_relative[, "variable"] <- "proportion_population"
    data_relative[, value := value/popsize]
    data_relative <- dcast(data_relative, ...~type, value.var="value")
    data_relative <- data_relative[, .(format=sprintf(
      "%s (%s to %s)",
      ifelse(grepl("_o", compartment, fixed = TRUE), formatNumber(median*1000, decimal = 1), formatNumber(median*100, decimal = 1, postfix = "%")),
      ifelse(grepl("_o", compartment, fixed = TRUE), formatNumber(low*1000, decimal = 1), formatNumber(low*100, decimal=1)),
      ifelse(grepl("_o", compartment, fixed = TRUE), formatNumber(high*1000, decimal = 1), formatNumber(high*100, decimal=1))
    )), by=c(bycols, "variable")]
    data_relative[grepl("_o", compartment, fixed = TRUE), "variable"] <- "per1000"
    data_relative[!grepl("_o", compartment, fixed = TRUE), "variable"] <- "per100"
    data_total <- rbindlist(list(data_total, data_relative))
  }
  
  if(!is.null(data_time)){
    data_peak <- getPeakValues(data_time, bycols=bycols, qntls=c("low"=0.025, "median"=0.5, "high"=0.975))
    data_peak <- data_peak[, -"group"]
    data_peak[variable == "peak_time", value := round(value/(365/12))]
    data_peak <- dcast(data_peak, ...~type, value.var="value")
    data_peak <- data_peak[, .(format=sprintf("%s (%s to %s)",
      ifelse(variable=="peak_time", formatNumber(median, max=0), formatNumber(median)),
      ifelse(variable=="peak_time", formatNumber(low, max=0), formatNumber(low)),
      ifelse(variable=="peak_time", formatNumber(high, max=0), formatNumber(high))
    )), by=c(bycols, "variable")]
    return(rbindlist(list(data_total, data_peak)))
  } else {
    return(data_total) 
  }
}

getEffectiveness <- function(data_total, return_table=FALSE, return_quantiles=TRUE, bycols=c("population", "compartment", "group", "scen", "s"), run_options=NULL){
  data_unmitigated <- rbindlist(lapply(tolower(countries), function(x) getData(x, 1, "total")))
  bycols_unmitigaged <- c("run", vectorRemove(bycols, c("scen", "s")))
  
  if(!is.null(run_options)){
    data_unmitigated = merge(data_unmitigated, run_options, by.x=c("run", "population"), by.y=c("run", "country"))
  }
  bycols_unmitigaged_nogroup = vectorRemove(bycols_unmitigaged, "group")
  data_unmitigated <- data_unmitigated[, .(value=sum(value)), by=bycols_unmitigaged_nogroup][, group := "all"]
  
  data_total <- data_total[, population := strsplit(as.character(population), " - ")[[1]][1], by="population"]
  bycols = vectorRemove(bycols, "group")
  data_total <- data_total[, .(value=sum(value)), by=c("run", bycols)][, group := "all"]
  data_effectiveness <- merge(data_total, data_unmitigated[, .(unmitigated=value), by=bycols_unmitigaged], by=bycols_unmitigaged, all.x=TRUE)
  data_effectiveness <- data_effectiveness[, effectiveness := (1 - value/unmitigated)][, -c("value", "unmitigated")]
  
  if(return_quantiles){
    data_effectiveness <- getQuantileValues(data_effectiveness[, value := effectiveness], bycols=bycols) 
  }
  
  if(return_table){
    data_effectiveness <- dcast(data_effectiveness, ...~type, value.var="value")
    bycols_nogroup = vectorRemove(bycols, "group")
    data_effectiveness <- data_effectiveness[, .(format=sprintf("%s (%s to %s)", formatNumber(median*100, max=0, postfix = "%"), formatNumber(low95*100, max=0, decimal=0), formatNumber(high95*100, max=0, decimal=0))), by=bycols_nogroup]
  }
  
  return(data_effectiveness)
}

vectorRemove <- function(vector, remove){
  return(vector[!vector %in% remove])
}

addScenarioOptions <- function(data){
  use_scenarios <- unique(data[, scen])
  unique_scens <- unique(scenarios_overview[index %in% use_scenarios[which(use_scenarios != 1)]][, scen])
  scenario_options <- rbindlist(lapply(use_scenarios, function(x){
    scen = scenarios_overview[index==x, scen]
    s = scenarios_overview[index==x, s]
    scenarios[[scen]][s,][, s_index := x]
  }), fill=TRUE)
  return(merge(data, scenario_options, by.x="scen", by.y="s_index"))
}

#scenarios <- c(2,3)
#data_time <- rbindlist(lapply(scenarios, function(s) rbindlist(lapply(tolower(countries), function(x, s) getData(x, s, "time"), s))))
#data_total <- rbindlist(lapply(scenarios, function(s) rbindlist(lapply(tolower(countries), function(x, s) getData(x, s, "total"), s))))
#scenario_table <- makeTable(data_total, data_time)
#setorder(scenario_table, scen, s, population, compartment, variable)
#scenario_table

#getQuantileRuns(data_time[compartment %in% c("icu_p", "nonicu_p", "death_o")], bycols = c("population", "compartment", "group", "scen", "s"), qntls=c("low"=0.025, "median"=0.5, "high"=0.975))
#getQuantileRunData(data_time[compartment %in% c("icu_p", "nonicu_p", "death_o")], bycols = c("population", "compartment", "group", "scen", "s"), qntls=c("low"=0.025, "median"=0.5, "high"=0.975))
#getPeakValues(data_time, bycols=c("population", "compartment", "group", "scen", "s"), qntls=c("low"=0.025, "median"=0.5, "high"=0.975))
#getQuantileValues(data_total, bycols=c("population", "compartment", "scen", "s"), qntls=c("low"=0.025, "median"=0.5, "high"=0.975))
#makeTable(data_total, data_time)
#getEffectiveness(data_total)
#getEffectiveness(data_total, return_table = TRUE)