setwd("~/workspace/covid_lmic/interventions/reviewer_comments/")
epifc_wd = "~/workspace/epiforecasts-global-national/national"

library(data.table)

countries <- c("Nigeria","Niger","Mauritius")

#r0_types <- c("country_med" = 1, "global_med" = 0.5, "global_low" = 0.025, "global_high" = 0.975)[4]
r0_types <- c("country_med" = 1, "global_med" = 0.5)[2]
#runs_per_type = 150
runs_per_type = 450
runs <- length(r0_types) * runs_per_type

run_options <- data.table(
  index = c(1:runs),
  r0 = rnorm(runs, 2.6, 0.5),
  model_seed = sample(c(1:1e6), runs, T)
)

african_countries <- c(
  "Algeria", "Angola", "Benin", "Botswana", "Cameroon",
  "Cape Verde", "Central African Republic", "Chad",
  "Congo", "Cote dIvoire", "Democratic Republic of the Congo",
  "Djibouti", "Egypt", "Equatorial Guinea", "Eswatini", "Gabon",
  "Gambia", "Ghana", "Guinea", "Guinea Bissau", "Kenya",
  "Lesotho", "Madagascar", "Libya", "Malawi", "Mali",
  "Mauritania", "Morocco", "Mozambique", "Namibia",
  "Niger", "Nigeria", "Rwanda", "Senegal", "Seychelles",
  "Sierra Leone", "Somalia", "South Africa", "South Sudan",
  "Sudan", "Tunisia", "Uganda", "United Republic of Tanzania",
  "Western Sahara", "Zambia", "Zimbabwe"
)

values <- list()
for(a in african_countries){
  data <- readRDS(sprintf('%s/%s/latest/bigr_eff_plot.rds', epifc_wd, a))$data
  
  print(paste0(a, " - ", data[mean == max(mean), date], " - ", round(data[mean == max(mean), mean], 2)))
  
  data[, "country"] <- a
  values[[length(values)+1]] <- data[mean == max(mean),]
}

values <- rbindlist(values)

runs <- 100000
x <- numeric(runs * nrow(values))

for(i in 1:nrow(values)){
  x[(1:runs) + runs*(i-1)] <- rnorm(runs, values[i, mean], values[i, std])
}

mauritius <- sample(x[x>1], runs_per_type)
niger <- rnorm(runs_per_type*2, values[country == "Niger", mean], values[country == "Niger", std])
niger <- sample(niger[niger>1], runs_per_type)
nigeria <- rnorm(runs_per_type*2, values[country == "Nigeria", mean], values[country == "Nigeria", std])
nigeria <- sample(nigeria[nigeria>1], runs_per_type)

for(c in countries){
  country_run_options <- run_options
  for(i in 1:length(r0_types)){
    type = r0_types[i]
    #if(names(type) == "country_med"){
    #  country_run_options[(1:runs_per_type)+runs_per_type*(i-1), "r0"] <- get(tolower(c))
    #  country_run_options[(1:runs_per_type)+runs_per_type*(i-1), "r0_type"] <- names(type)
    #} else {
    #  country_run_options[(1:runs_per_type)+runs_per_type*(i-1), "r0"] <- rep(quantile( rnorm(runs, 2.6, 0.5), type ), runs_per_type)
      country_run_options[(1:runs_per_type)+runs_per_type*(i-1), "r0_type"] <- names(type)
    #}
  }
  saveRDS(country_run_options, sprintf("./inputs/%s/run_options.rds", tolower(c)))
}
