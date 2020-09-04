library(data.table)
source("~/workspace/covid_lmic/interventions/reviewer_comments/helper_functions.R")
setwd("~/workspace/covid_lmic/interventions/reviewer_comments/")

countries <- c("Nigeria","Niger","Mauritius")

#unmitigated
scenario_1 <- set_scenario_combinations(
  combine_each = list()
)

#self-isolation
scenario_2 <- set_scenario_combinations(
  list(symptomatic_contact=c(0, 0.2, 0.4, 0.6, 0.8))
)

#social distancing
scenario_3 <- set_scenario_combinations(
  list(
    gen_socdist=T,
    gen_socdist_stop=c(12),
    gen_socdist_house=1,
    gen_socdist_school="other",
    gen_socdist_work="other",
    gen_socdist_other=c(0.8, 0.6, 0.4, 0.2, 0),
    gen_socdist_start="incidence",
    gen_socdist_schedule_filter_on_threshold=c(1/10000)
  )
)

#shielding
scenario_4 <- set_scenario_combinations(
  list(
    gen_socdist=F,
    hirisk_shield_start="incidence",
    hirisk_shield_schedule_filter_on_threshold=c(1/10000),
    hirisk_prop_isolated=c(0.6, 0.7, 0.8, 0.9),
    hirisk_contact=c(0, 1, 4),
    hirisk_lorisk_contact=c(0.4, 0.3, 0.2, 0.1, 0),
    hirisk_shield_stop=12 
  )
)

#50% self-isolation and 20/50% social-distancing
scenario_5 <- set_scenario_combinations(
  combine_each=list(
    gen_socdist=T,
    gen_socdist_stop=c(12),
    gen_socdist_house=1,
    gen_socdist_school="other",
    gen_socdist_work="other",
    gen_socdist_other=c(0.5, 0.8),
    symptomatic_contact=c(1,0.5),
    gen_socdist_start="incidence",
    gen_socdist_schedule_filter_on_threshold=c(1/10000)
  ),
  individual_only=list()
)

shielding_coverage <- 0.8
shielding_eff <- 0.2
#50% self-isolation and 80% shielding
scenario_6 <- set_scenario_combinations(
  combine_each=list(
    symptomatic_contact=c(1,0.5),
    hirisk_shield_start="incidence",
    hirisk_shield_stop=12,
    hirisk_shield_schedule_filter_on_threshold=c(1/10000),
    hirisk_prop_isolated=shielding_coverage,
    hirisk_contact=1,
    hirisk_lorisk_contact=shielding_eff
  ),
  individual_only=list()
)

#50% self-isolation and 80% shielding and 20/50% social-distancing
scenario_7 <- set_scenario_combinations(
  combine_each=list(
    symptomatic_contact=c(1,0.5),
    hirisk_shield_start="incidence",
    hirisk_shield_schedule_filter_on_threshold=c(1/10000),
    hirisk_prop_isolated=shielding_coverage,
    hirisk_contact=1,
    hirisk_lorisk_contact=shielding_eff,
    hirisk_shield_stop=12,
    gen_socdist=T,
    gen_socdist_stop=c(12),
    gen_socdist_house=1,
    gen_socdist_school="other",
    gen_socdist_work="other",
    gen_socdist_other=c(0.5, 0.8),
    gen_socdist_start="incidence",
    gen_socdist_schedule_filter_on_threshold=c(1/10000)
  ),
  individual_only=list()
)

#LOCKDOWN + 20/50 dist
x <- scenario_5[symptomatic_contact == 0.5]
scenario_8 <- x
for(i in 1:nrow(scenario_8)){
  class(scenario_8$gen_socdist_stop) <- "list"
  class(scenario_8$gen_socdist_start) <- "list"
  class(scenario_8$gen_socdist_schedule_filter_on_threshold) <- "list"
  class(scenario_8$gen_socdist_contact) <- "list"
  scenario_8[i, "gen_socdist_stop"] <- list(c(2, x[i, gen_socdist_stop]))
  scenario_8[i, "gen_socdist_start"] <- list(c("incidence",x[i, gen_socdist_start]))
  scenario_8[i, "gen_socdist_schedule_filter_on_threshold"] <- list(c(1e-4,x[i, gen_socdist_schedule_filter_on_threshold]))
  scenario_8[i, "gen_socdist_contact"] <- list(list(c(1,0.2,0.2,0.2),x[i, gen_socdist_contact][[1]]))
}

#LOCKDOWN + 80% shielding
shielding_coverage <- 0.8
shielding_eff <- 0.2
scenario_9 <- set_scenario_combinations(
  combine_each=list(
    symptomatic_contact=c(0.5),
    hirisk_shield_start="incidence",
    hirisk_shield_stop=12,
    hirisk_shield_schedule_filter_on_threshold=c(1/10000),
    hirisk_prop_isolated=shielding_coverage,
    hirisk_contact=1,
    hirisk_lorisk_contact=shielding_eff,
    gen_socdist=T,
    gen_socdist_stop=c(2),
    gen_socdist_house=1,
    gen_socdist_school="other",
    gen_socdist_work="other",
    gen_socdist_other=c(0.2),
    gen_socdist_start="incidence",
    gen_socdist_schedule_filter_on_threshold=c(1/10000)
  ),
  individual_only=list()
)

#50% self isolation and 80% shielding and 20/50 socdist
x <- scenario_7[symptomatic_contact == 0.5]
scenario_10 <- x
for(i in 1:nrow(scenario_10)){
  class(scenario_10$gen_socdist_stop) <- "list"
  class(scenario_10$gen_socdist_start) <- "list"
  class(scenario_10$gen_socdist_schedule_filter_on_threshold) <- "list"
  class(scenario_10$gen_socdist_contact) <- "list"
  scenario_10[i, "gen_socdist_stop"] <- list(c(2,x[i, gen_socdist_stop]))
  scenario_10[i, "gen_socdist_start"] <- list(c("incidence",x[i, gen_socdist_start]))
  scenario_10[i, "gen_socdist_schedule_filter_on_threshold"] <- list(c(1e-4,x[i, gen_socdist_schedule_filter_on_threshold]))
  scenario_10[i, "gen_socdist_contact"] <- list(list(c(1,0.2,0.2,0.2),x[i, gen_socdist_contact][[1]]))  
}

scenarios <- list(
  scenario_1,
  scenario_2,
  scenario_3,
  scenario_4,
  scenario_5,
  scenario_6,
  scenario_7,
  scenario_8,
  scenario_9,
  scenario_10
)

scenarios_overview <- rbindlist(
  lapply(
    1:length(scenarios),
    function(x,z){ data.table(scen=rep(x, z[x]), s=1:z[x]) },
    z=sapply(scenarios, nrow)
  )
)
scenarios_overview[, "index"] <- c(1:nrow(scenarios_overview))

if(!dir.exists("./inputs")){
  dir.create("./inputs")
}

saveRDS(scenarios_overview, paste0("./inputs/scenarios_overview.rds"))
saveRDS(scenarios, paste0("./inputs/scenarios.rds"))