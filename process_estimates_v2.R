#' TO PROCESS ESTIMATES
#'  - create folder ./input
#'  - creater folders ./input/niger, ./input/nigeria, ./input/mauritius
#'  - extract tar.gz of scenario in ./output
#'  - move files to correct country folder, e.g.: mv *niger_* ./niger/
#'  - run script

library(data.table)
library(ggplot2)
library(ggh4x)
library(patchwork)
source("./functions_tables_v2.R")

#In which directory should output files be saved?
outputs_dir <- sprintf("~/Dropbox/covid_lmic_paper/final2/base/")

input_dir <- "./inputs"
data_dir <- "./output"

sindex <- c("unmitigated", "selfisolation", "socialdistancing", "shielding", "strgy_sodi", "strg_shie", "strg_sodi_shie", "str_lock_sodi", "str_lock_shie", "str_lock_sodi_shie")
countries <- c("Niger", "Nigeria", "Mauritius")

compartment_levels <- c("cases", "icu_p", "nonicu_p", "icu_o", "nonicu_o", "death_o")
compartment_labels <- c("cases", "ICU bed occupancy", "non-ICU bed occupancy", "critical cases", "severe_cases", "deaths")

dir.create(outputs_dir)
scenarios_overview <- readRDS(sprintf("%s/scenarios_overview.rds", input_dir))
scenarios <- readRDS(sprintf("%s/scenarios.rds", input_dir))

r0_types <- c("global_high", "global_low", "global_med", "country_med")
r0_labels <- c("Global R0 (upper)", "Global R0 (lower)", "Global R0", "Country Rt")

r0_colours <- c(
  "Global R0 (upper)" = "#FE5000",
  "Global R0 (lower)" = "#FFB81C",
  "Country Rt" = "#000000",
  "Global R0" = "#00BF6F"
)

run_options <- list()
for(c in countries){
  x = readRDS(sprintf("%s/%s/run_options.rds.bak", input_dir, tolower(c)))
  x[, "country"] <- c
  x[, run := index]
  run_options[[c]] <- x
}
run_options <- rbindlist(run_options)

#' FIGURE 1 - EPIDEMIC CURVES

use_scenarios <- 1
data_time <- rbindlist(lapply(use_scenarios, function(s) rbindlist(lapply(tolower(countries), function(x, s) getData(x, s, "time", 365), s))))
data_total <- rbindlist(lapply(use_scenarios, function(s) rbindlist(lapply(tolower(countries), function(x, s) getData(x, s, "total", 365), s))))
scenario_table <- makeTable(data_total, data_time)
setorder(scenario_table, scen, s, population, compartment, variable)
saveRDS(scenario_table, sprintf("%s/data_unmitigated_medianrun.RDS", outputs_dir))

data_time_r0 = merge(data_time, run_options, by.x=c("run", "population"), by.y=c("run", "country"))

ggplot(
)+geom_line(
  data=data_time_r0[compartment == "cases"],
  aes(x=t, y=value, group=run, colour = factor(r0_type, r0_types, r0_labels)),
  alpha=0.02
)+geom_line(
  data=getQuantileRunData(
    data_time_r0[compartment %in% c("cases")],
    bycols = c("population", "compartment", "group", "scen", "s", "r0_type"),
    qntls=c("median"=0.5)
  )[, r0_type := factor(r0_type, r0_types)][order(r0_type)],
  aes(x=t, y=value, colour = factor(r0_type, r0_types, r0_labels), group=r0_type),
  size=1
)+theme_bw(
)+theme(
  panel.grid.minor=element_blank()
)+scale_x_continuous(
  breaks=seq(0,365*1,365*1/6),
  labels=seq(0,12,2),
  limits=c(0,365*1)
)+scale_y_continuous(
  labels = scales::comma
)+labs(
  x="months",
  y="symptomatic cases",
  colour="R0"
)+facet_wrap(
  .~factor(population, countries),
  scales="free"
)+scale_colour_manual(
  values=r0_colours
)

for(extension in c("png", "pdf")) ggsave(
  sprintf("%s/unmitigated_medianrun.%s", outputs_dir, extension),
  width = 210, height = 70, units = "mm"
)

#' FIGURE 2A - SELF ISOLATION
intervention <- scenarios[[which(sindex=="selfisolation")]]
intervention[, "s"] <- scenarios_overview[scen == which(sindex=="selfisolation"), index]

use_scenarios <- c(1, intervention$s)
data_total <- rbindlist(lapply(use_scenarios, function(s) rbindlist(lapply(tolower(countries), function(x, s) getData(x, s, "total"), s))))
data_total_r0 = merge(data_total, run_options[, c("run", "country", "r0_type", "r0")], by.x=c("run", "population"), by.y=c("run", "country"))

data_plot <- addScenarioOptions(
  getEffectiveness(
    data_total_r0,
    bycols=c("population", "compartment", "group", "scen", "s", "r0_type"),
    run_options=run_options[, c("run", "country", "r0_type", "r0")]
  )
)
saveRDS(data_plot, sprintf("%s/fig2a_data.RDS", outputs_dir))

plot_selfisolation <- ggplot(
  dcast(data_plot[compartment=="nonicu_o"], ...~type, value.var = "value"),
  aes(
    x=1-symptomatic_contact,
    xend=1-symptomatic_contact,
    group=factor(r0_type, r0_types, r0_labels),
    fill=factor(r0_type, r0_types, r0_labels)
  )
)+geom_ribbon(
  aes(ymin=low95,ymax=high95),
  size=0.5,
  #fill="#00BF6F",
  alpha=0.15
)+geom_ribbon(
  aes(ymin=low50,ymax=high50),
  size=1,
  #fill="#0D5257",
  alpha=0.25
)+geom_line(
  aes(y=median, colour=factor(r0_type, r0_types, r0_labels)) 
)+geom_point(
  shape=21,
  aes(y=median, colour=factor(r0_type, r0_types, r0_labels)), 
  #colour="#000000",
  fill="#EEEEEE",
  size=2
)+facet_nested(
  .~factor(population, countries)
)+scale_y_continuous(
  limits=c(0,1),
  breaks=seq(0,1,0.1),
  labels=scales::percent
)+scale_x_continuous(
  labels=scales::percent,
  breaks=seq(0,1,0.2),
  limits=c(0,1)
)+labs(
  x = "Reduction in contacts during symptomatic period",
  y = "Reduction in severe cases",
  colour = "R0 type",
  fill = "R0 type"
)+theme_bw(
)+theme(
  panel.grid.minor=element_blank(),
  legend.position="none"
)+scale_colour_manual(
  values=r0_colours
)+scale_fill_manual(
  values=r0_colours
)

print(plot_selfisolation)
for(extension in c("png", "pdf")) ggsave(
  sprintf("%s/fig2a_selfisolation.%s", outputs_dir, extension),
  plot_selfisolation, width = 210, height = 80, units = "mm"
)

#' FIGURE 2B - SOCIAL DISTANCING
intervention <- scenarios[[which(sindex=="socialdistancing")]]
intervention[, "s"] <- scenarios_overview[scen == which(sindex=="socialdistancing"), index]

use_scenarios <- c(1, intervention$s)
data_total <- rbindlist(lapply(use_scenarios, function(s) rbindlist(lapply(tolower(countries), function(x, s) getData(x, s, "total"), s))))
data_total_r0 = merge(data_total, run_options[, c("run", "country", "r0_type", "r0")], by.x=c("run", "population"), by.y=c("run", "country"))
data_plot <- addScenarioOptions(
  getEffectiveness(
    data_total_r0,
    bycols=c("population", "compartment", "group", "scen", "s", "r0_type"),
    run_options=run_options[, c("run", "country", "r0_type", "r0")]
  )
)
saveRDS(data_plot, sprintf("%s/fig2b_data.RDS", outputs_dir))
  
plot_distancing <- ggplot(
  dcast(data_plot[compartment=="nonicu_o", -"gen_socdist_contact"], ...~type, value.var = "value")[scen==1, gen_socdist_other := 1],
  aes(
    x=1-gen_socdist_other,
    xend=1-gen_socdist_other,
    group=factor(r0_type, r0_types, r0_labels),
    fill=factor(r0_type, r0_types, r0_labels)
  )
)+geom_ribbon(
  aes(ymin=low75,ymax=high75),
  size=0.5,
  #fill="#00BF6F",
  alpha=0.15
)+geom_ribbon(
  aes(ymin=low50,ymax=high50),
  size=1,
  #fill="#0D5257",
  alpha=0.25
)+geom_line(
  aes(y=median, colour=factor(r0_type, r0_types, r0_labels)) 
)+geom_point(
  shape=21,
  aes(y=median, colour=factor(r0_type, r0_types, r0_labels)),
  #colour="#000000",
  fill="#EEEEEE",
  size=2
)+geom_point(
  shape=21,
  aes(y=median),
  colour="#000000",
  fill="#EEEEEE",
  size=2
)+facet_nested(
  .~factor(population, countries)
)+scale_y_continuous(
  limits=c(0,1),
  breaks=seq(0,1,0.1),
  labels=scales::percent
)+scale_x_continuous(
  limits=c(0,1),
  labels=scales::percent,
  breaks=seq(0,1,0.2)
)+labs(
  x = "Reduction in contacts outside household",
  y = "Reduction in severe cases",
  colour = "R0 type",
  fill = "R0 type"
)+theme_bw(
)+theme(
  panel.grid.minor=element_blank()
)+scale_colour_manual(
  values=r0_colours
)+scale_fill_manual(
  values=r0_colours
)
print(plot_distancing)

for(extension in c("png", "pdf")) ggsave(
  sprintf("%s/fig2b_socialdistancing.%s", outputs_dir, extension),
  plot_distancing, width = 210, height = 80, units = "mm"
)

#' FIGURE 2 - SELF-ISOLATION AND SOCIAL DISTANCING
patched <- (plot_selfisolation/plot_distancing) + plot_annotation(tag_levels = 'A') + plot_layout(guides = "collect") & theme(legend.position = 'bottom')
print(patched)

for(extension in c("png", "pdf")) ggsave(
  sprintf("%s/fig2_combined.%s", outputs_dir, extension),
  patched, width = 210, height = 150, units = "mm"
)

#' FIGURE 3 - SHIELDING
intervention <- scenarios[[which(sindex=="shielding")]]
intervention[, "s"] <- scenarios_overview[scen == which(sindex=="shielding"), index]

use_scenarios <- c(intervention$s)
data_total <- rbindlist(lapply(use_scenarios, function(s) rbindlist(lapply(tolower(countries), function(x, s) getData(x, s, "total"), s))))
data_total <- data_total[, population := strsplit(as.character(population), " - ")[[1]][1], by="population"]
data_total_r0 = merge(data_total, run_options[, c("run", "country", "r0_type", "r0")], by.x=c("run", "population"), by.y=c("run", "country"))
data_plot <- addScenarioOptions(
  getEffectiveness(
    data_total_r0,
    bycols=c("population", "compartment", "group", "scen", "s", "r0_type"),
    run_options=run_options[, c("run", "country", "r0_type", "r0")]
  )
)
saveRDS(data_plot, sprintf("%s/fig3_data.RDS", outputs_dir))

ggplot(
  dcast(data_plot[r0_type %in% c("country_med", "global_med") & compartment=="nonicu_o"], ...~type, value.var = "value"),
  #dcast(data_plot[hirisk_contact == 1 & compartment=="nonicu_o"], ...~type, value.var = "value"),
  aes(
    x=1-hirisk_lorisk_contact
  )
)+geom_ribbon(
  aes(
    ymin=low75,
    ymax=high75,
    fill=factor(hirisk_prop_isolated,
      hirisk_prop_isolated,
      paste0(hirisk_prop_isolated*100,"%")
    )
  ),
  alpha=0.25#alpha=0.125
)+geom_line(
  linetype=2,
  aes(
    y=median,
    shape=factor(hirisk_prop_isolated,
      hirisk_prop_isolated,
      paste0(hirisk_prop_isolated*100,"%")
    ),
    colour=factor(hirisk_prop_isolated,
      hirisk_prop_isolated,
      paste0(hirisk_prop_isolated*100,"%")
    )
  )
)+geom_point(
  aes(
    y=median,
    shape=factor(hirisk_prop_isolated,
      hirisk_prop_isolated,
      paste0(hirisk_prop_isolated*100,"%")
    ),
    colour=factor(hirisk_prop_isolated,
      hirisk_prop_isolated,
      paste0(hirisk_prop_isolated*100,"%")
    )
  ),
  size=2
)+facet_nested(
  #factor(r0_type,
  #  r0_types,
  #  r0_labels
  factor(hirisk_contact,
    c(0,1,4),
    c("Change in contact among\nshielded: zero contacts", "Change in contact among\nshielded: unchanged", "Change in contact among\nshielded: quadrupled")
  #)~factor(population, countries),
  )~factor(r0_type, r0_types, r0_labels)+factor(population, countries),
  labeller = ggplot2::labeller(
    hirisk_contact =  function(string, before = "Change in contact\namong shielded: ", after=""){
      paste0(before, string, after)
    }
  )
)+scale_y_continuous(
  breaks=seq(-1,1,0.1),
  labels=scales::percent_format(accuracy=1),
)+scale_x_continuous(
  limits=c(0.59,1.01),
  labels=scales::percent_format(accuracy=1),
)+labs(
  x = "Reduction in contacts between shielded and unshielded people",
  y = "Reduction in severe cases",
  fill = "Percentage 60+ shielded",
  colour = "Percentage 60+ shielded",
  shape = "Percentage 60+ shielded"
#)+scale_fill_manual(
#  values=c("60%"="#0D5257", "70%"="#00BF6F", "80%"="#00AEC7", "90%"="#FE5000")
#)+scale_colour_manual(
#  values=c("60%"="#0D5257", "70%"="#00BF6F", "80%"="#00AEC7", "90%"="#FE5000")
#)+theme_bw(
)+scale_fill_manual(
  values=c("60%"="#FE5000", "70%"="#FFB81C", "80%"="#00BF6F", "90%"="#000000")
)+scale_colour_manual(
  values=c("60%"="#FE5000", "70%"="#FFB81C", "80%"="#00BF6F", "90%"="#000000")
)+theme_bw(
)+theme(
  panel.grid.minor=element_blank(),
  legend.position="bottom"
)+coord_cartesian(
  ylim=c(0, 0.65)
)

for(extension in c("png", "pdf")) ggsave(
  sprintf("%s/fig3_shielding.%s", outputs_dir, extension),
  width = 210, height = 30+50*3, units = "mm"
)

#' STRATEGIES MAIN

#check scenario indexes
unlist(sapply(
  c("strgy_sodi", "strg_shie", "strg_sodi_shie"), function(x){
    scenarios_overview[scen == which(sindex==x), index][scenarios[[which(sindex==x)]][, symptomatic_contact] == 0.5]
  }
))
scenario_indexes = c(1, 75, 74, 77, 81, 79)
scenario_names = c("unmitigated", "20% distancing", "50% distancing", "80% shielding", "80% shielding + 20% distancing", "80% shielding + 50% distancing")

intervention <- data.table(
  s=scenario_indexes,
  name=scenario_names
)

use_scenarios <- c(intervention$s)
data_total <- rbindlist(lapply(use_scenarios, function(s) rbindlist(lapply(tolower(countries), function(x, s) getData(x, s, "total"), s))))
data_time <- rbindlist(lapply(use_scenarios, function(s) rbindlist(lapply(tolower(countries), function(x, s) getData(x, s, "time"), s))))

data_total[, population := strsplit(as.character(population), " - ")[[1]][1], by="population"]
data_total <- data_total[, .(value=sum(value)), by=c("run", "population", "compartment", "group", "t", "s", "scen")]
data_time[, population := strsplit(as.character(population), " - ")[[1]][1], by="population"]
data_time <- data_time[, .(value=sum(value)), by=c("run", "population", "compartment", "group", "t", "s", "scen")]

data_total_r0 = merge(data_total, run_options[, c("run", "country", "r0_type", "r0")], by.x=c("run", "population"), by.y=c("run", "country"))
data_time_r0 = merge(data_time, run_options[, c("run", "country", "r0_type", "r0")], by.x=c("run", "population"), by.y=c("run", "country"))

strategies_table <- makeTable(data_total_r0, data_time_r0, bycols=c("population", "compartment", "scen", "s", "r0_type"))
strategies_table[, scen := factor(scen, scenario_indexes, scenario_names)]
strategies_table <- dcast(strategies_table[, -"s"], ...~scen, value.var="format")
strategies_table[, compartment := factor(compartment, compartment_levels, compartment_labels)]
strategies_table[, r0_type := factor(r0_type, r0_types, r0_labels)]
fwrite(strategies_table, sprintf("%s/table3_strategies.csv", outputs_dir))

strategies_effectiveness <- getEffectiveness(data_total_r0, return_table = TRUE, bycols=c("population", "compartment", "group", "scen", "s", "r0_type"), run_options = run_options[, c("run", "country", "r0_type", "r0")])
strategies_effectiveness[, compartment := factor(compartment, compartment_levels, compartment_labels)]
strategies_effectiveness[, r0_type := factor(r0_type, r0_types, r0_labels)]
strategies_effectiveness[, scen := factor(scen, scenario_indexes, scenario_names)]
strategies_effectiveness <- dcast(strategies_effectiveness[scen != "unmitigated", -"s"], ...~scen, value.var="format")
fwrite(strategies_effectiveness, sprintf("%s/table4_strategies_effectiveness.csv", outputs_dir))

#'TABLE FOR MAIN STRATEGIES
data_plot <- getQuantileRunData(data_time_r0[, -"r0"], bycols = c("population", "compartment", "group", "scen", "r0_type"))
data_plot[, outcome_label := factor(compartment, compartment_levels, compartment_labels)]
saveRDS(data_plot, sprintf("%s/fig4_data.RDS", outputs_dir))

#' PLOT FOR MAIN STRATEGIES
ggplot(
  data_plot[r0_type %in% c("global_med", "country_med") & compartment %in% c("death_o", "icu_p", "nonicu_p")],
  aes(
    x=t, y=value, colour=factor(
      paste0(r0_type, "_", population),
      as.vector(sapply(rev(r0_types), function(x) paste0(x, "_", countries)))
    ),
    group=factor(paste0(r0_type, "_", variable), as.vector(sapply(r0_types, function(x) paste0(x, "_", c("low95", "low50", "median", "high50", "high95"))))),
    linetype=factor(variable, c("low95", "low50", "median", "high50", "high95"), c("95%", "50%", "median", "50%", "95%")),
    size=factor(variable, c("low95", "low50", "median", "high50", "high95"), c("95%", "50%", "median", "50%", "95%"))
  )
)+geom_line(
)+facet_nested(
  factor(population,
    countries
  )+outcome_label~factor(scen,
    c(1, scenario_indexes),
    c("unmitigated", scenario_names)
  ),
  scales="free"
)+theme_bw(
)+theme(
  panel.grid.minor=element_blank(),
  legend.position="bottom"
)+labs(
  y="Daily value", x="Time (months)",
  linetype = "quantile"
)+scale_y_continuous(
  labels = scales::comma
)+scale_colour_manual(
  values=c(
    "global_med_Mauritius"="#2A34B1", "country_med_Mauritius"="#000000",
    "global_med_Nigeria"="#009F60", "country_med_Nigeria"="#000000",
    "global_med_Niger"="#E95506", "country_med_Niger"="#000000"
  ),
  guide = "none"
)+scale_x_continuous(
  breaks=c((365*2)/12)*c(0:12),
  labels=seq(0,24,2),
  limits = c(0,365*1)
)+scale_linetype_manual(
  values=c("median" = 1, "95%" = 3, "50%" = 2)
)+scale_size_manual(
  values=c("median" = 1.2, "95%" = 0.8, "50%" = 0.5),
  guide = "none"
)

for(extension in c("png", "pdf")) ggsave(
  sprintf("%s/figs2_strategies.%s", outputs_dir, extension),
  width = 210*1.5, height = 30+40*5, units = "mm"
)

#' SAME DATA, MAIN STRATEGIES ONLY SHOW DEATHS
ggplot(
  data_plot[r0_type %in% c("global_med", "country_med") & compartment %in% c("death_o")],
  aes(
    x=t, y=value, colour=factor(
      paste0(r0_type, "_", population),
      as.vector(sapply(rev(r0_types), function(x) paste0(x, "_", countries)))
    ),
    group=factor(paste0(r0_type, "_", variable), as.vector(sapply(r0_types, function(x) paste0(x, "_", c("low95", "low50", "median", "high50", "high95"))))),
    linetype=factor(variable, c("low95", "low50", "median", "high50", "high95"), c("95%", "50%", "median", "50%", "95%")),
    size=factor(variable, c("low95", "low50", "median", "high50", "high95"), c("95%", "50%", "median", "50%", "95%"))
  )
)+geom_line(
)+facet_nested(
  factor(population,countries)~factor(scen,
    c(1, scenario_indexes),
    c("unmitigated", scenario_names)
  ),
  scales="free"
)+theme_bw(
)+theme(
  panel.grid.minor=element_blank(),
  legend.position="bottom"
)+labs(
  y="Daily deaths", x="Time (months)",
  linetype = "quantile"
)+scale_y_continuous(
  labels = scales::comma
)+scale_colour_manual(
  values=c(
    "global_med_Mauritius"="#2A34B1", "country_med_Mauritius"="#000000",
    "global_med_Nigeria"="#009F60", "country_med_Nigeria"="#000000",
    "global_med_Niger"="#E95506", "country_med_Niger"="#000000"
  ),
  guide = "none"
)+scale_x_continuous(
  breaks=c((365*2)/12)*c(0:12),
  labels=seq(0,24,2),
  limits = c(0,365*1)
)+scale_linetype_manual(
  values=c("median" = 1, "95%" = 3, "50%" = 2)
)+scale_size_manual(
  values=c("median" = 1.2, "95%" = 0.8, "50%" = 0.5),
  guide = "none"
)

for(extension in c("png", "pdf")) ggsave(
  sprintf("%s/fig4_strategies.%s", outputs_dir, extension),
  width = 210*1.5, height = 30+40*2, units = "mm"
)

#' ONLY RUN THIS FOR BASELINE SCENARIO

#' SAME DATA, MAIN STRATEGIES RAN FOR TWO YEARS

#data_time <- rbindlist(lapply(use_scenarios, function(s) rbindlist(lapply(tolower(countries), function(x, s) getData(x, s, "time", 365*2), s))))
#data_time[, population := strsplit(as.character(population), " - ")[[1]][1], by="population"]
#data_time <- data_time[, .(value=sum(value)), by=c("run", "population", "compartment", "group", "t", "s", "scen")]
#data_plot <- getQuantileRunData(data_time, bycols = c("population", "compartment", "group", "scen"))
#data_plot[, outcome_label := factor(compartment, compartment_levels, compartment_labels)]
#saveRDS(data_plot, sprintf("%s/figs3_data.RDS", outputs_dir))
#
#ggplot(
#  data_plot[compartment %in% c("death_o", "icu_p", "nonicu_p")],
#  aes(
#    x=t, y=value, colour=paste0(population,"_",compartment), group=variable,
#    linetype=factor(variable, c("low95", "low50", "median", "high50", "high95"), c("95%", "50%", "median", "50%", "95%")),
#    size=factor(variable, c("low95", "low50", "median", "high50", "high95"), c("95%", "50%", "median", "50%", "95%"))
#  )
#)+geom_line(
#)+facet_nested(
#  factor(population,countries)+outcome_label~factor(scen,
#    c(1, scenario_indexes),
#    c("unmitigated", scenario_names)
#  ),
#  scales="free"
#)+theme_bw(
#)+theme(
#  panel.grid.minor=element_blank(),
#  legend.position="bottom"
#)+labs(
#  y="Daily value", x="Time (months)",
#  linetype = "quantile"
#)+scale_y_continuous(
#  labels = scales::comma
#)+scale_colour_manual(
#  values=c(
#    "Mauritius_nonicu_p"="#101444", "Mauritius_icu_p"="#1A206D", "Mauritius_death_o"="#2A34B1",
#    "Nigeria_nonicu_p"="#00653C", "Nigeria_icu_p"="#008751", "Nigeria_death_o"="#009F60",
#    "Niger_nonicu_p"="#B44205", "Niger_icu_p"="#E05206", "Niger_death_o"="#E95506"),
#  guide = "none"
#)+scale_x_continuous(
#  breaks=c((365*2)/12)*seq(0,12,2),
#  labels=seq(0,24,4),
#  limits = c(0,365*2)
#)+scale_linetype_manual(
#  values=c("median" = 1, "95%" = 3, "50%" = 2)
#)+scale_size_manual(
#  values=c("median" = 1.2, "95%" = 0.8, "50%" = 0.5),
#  guide = "none"
#)
#
#for(extension in c("png", "pdf")) ggsave(
#  sprintf("%s/figs3_strategies_2y.%s", outputs_dir, extension),
#  width = 210*1.5, height = 30+40*5, units = "mm"
#)

#' LOCKDOWN
unlist(sapply(
  c("str_lock_sodi", "str_lock_shie", "str_lock_sodi_shie"), function(x){
    scenarios_overview[scen == which(sindex==x), index][scenarios[[which(sindex==x)]][, symptomatic_contact] == 0.5]
  }
))
scenario_indexes = c(1, 83, 82, 84, 86, 85)
scenario_names = c("unmitigated", "20% distancing", "50% distancing", "80% shielding", "80% shielding + 20% distancing", "80% shielding + 50% distancing")

intervention <- data.table(
  s=scenario_indexes,
  name=scenario_names
)

use_scenarios <- c(intervention$s)
data_total <- rbindlist(lapply(use_scenarios, function(s) rbindlist(lapply(tolower(countries), function(x, s) getData(x, s, "total"), s))))
data_time <- rbindlist(lapply(use_scenarios, function(s) rbindlist(lapply(tolower(countries), function(x, s) getData(x, s, "time"), s))))

data_total[, population := strsplit(as.character(population), " - ")[[1]][1], by="population"]
data_total <- data_total[, .(value=sum(value)), by=c("run", "population", "compartment", "group", "t", "s", "scen")]
data_time[, population := strsplit(as.character(population), " - ")[[1]][1], by="population"]
data_time <- data_time[, .(value=sum(value)), by=c("run", "population", "compartment", "group", "t", "s", "scen")]

data_total_r0 = merge(data_total, run_options[, c("run", "country", "r0_type", "r0")], by.x=c("run", "population"), by.y=c("run", "country"))
data_time_r0 = merge(data_time, run_options[, c("run", "country", "r0_type", "r0")], by.x=c("run", "population"), by.y=c("run", "country"))

strategies_table <- makeTable(data_total_r0, data_time_r0, bycols=c("population", "compartment", "scen", "s", "r0_type"))
strategies_table[, scen := factor(scen, scenario_indexes, scenario_names)]
strategies_table <- dcast(strategies_table[, -"s"], ...~scen, value.var="format")
strategies_table[, compartment := factor(compartment, compartment_levels, compartment_labels)]
strategies_table[, r0_type := factor(r0_type, r0_types, r0_labels)]
fwrite(strategies_table, sprintf("%s/table_strategies_lockdown.csv", outputs_dir))

strategies_effectiveness <- getEffectiveness(data_total_r0, return_table = TRUE, bycols=c("population", "compartment", "group", "scen", "s", "r0_type"), run_options = run_options[, c("run", "country", "r0_type", "r0")])
strategies_effectiveness[, compartment := factor(compartment, compartment_levels, compartment_labels)]
strategies_effectiveness[, r0_type := factor(r0_type, r0_types, r0_labels)]
strategies_effectiveness[, scen := factor(scen, scenario_indexes, scenario_names)]
strategies_effectiveness <- dcast(strategies_effectiveness[scen != "unmitigated", -"s"], ...~scen, value.var="format")
fwrite(strategies_effectiveness, sprintf("%s/table4_strategies_lockdown_effectiveness.csv", outputs_dir))

#'TABLE FOR MAIN STRATEGIES
data_plot <- getQuantileRunData(data_time_r0[, -"r0"], bycols = c("population", "compartment", "group", "scen", "r0_type"))
data_plot[, outcome_label := factor(compartment, compartment_levels, compartment_labels)]
saveRDS(data_plot, sprintf("%s/figs4_lockdown_data.RDS", outputs_dir))

ggplot(
  data_plot[r0_type %in% c("global_med", "country_med") & compartment %in% c("death_o", "icu_p", "nonicu_p")],
  aes(
    x=t, y=value, colour=factor(
      paste0(r0_type, "_", population),
      as.vector(sapply(rev(r0_types), function(x) paste0(x, "_", countries)))
    ),
    group=factor(paste0(r0_type, "_", variable), as.vector(sapply(r0_types, function(x) paste0(x, "_", c("low95", "low50", "median", "high50", "high95"))))),
    linetype=factor(variable, c("low95", "low50", "median", "high50", "high95"), c("95%", "50%", "median", "50%", "95%")),
    size=factor(variable, c("low95", "low50", "median", "high50", "high95"), c("95%", "50%", "median", "50%", "95%"))
  )
)+geom_line(
)+facet_nested(
  factor(population, countries)+outcome_label~factor(scen,
    c(1, scenario_indexes),
    c("unmitigated", scenario_names)
  ),
  scales="free"
)+theme_bw(
)+theme(
  panel.grid.minor=element_blank(),
  legend.position="bottom"
)+labs(
  y="Daily value", x="Time (months)",
  linetype = "quantile"
)+scale_y_continuous(
  labels = scales::comma
)+scale_colour_manual(
  values=c(
    "global_med_Mauritius"="#2A34B1", "country_med_Mauritius"="#000000",
    "global_med_Nigeria"="#009F60", "country_med_Nigeria"="#000000",
    "global_med_Niger"="#E95506", "country_med_Niger"="#000000"
  ),
  guide = "none"
)+scale_x_continuous(
  breaks=c((365*2)/12)*c(0:12),
  labels=seq(0,24,2),
  limits = c(0,365*1)
)+scale_linetype_manual(
  values=c("median" = 1, "95%" = 3, "50%" = 2)
)+scale_size_manual(
  values=c("median" = 1.2, "95%" = 0.8, "50%" = 0.5),
  guide = "none"
)

for(extension in c("png", "pdf")) ggsave(
  sprintf("%s/figs4_strategies_lockdown.%s", outputs_dir, extension),
  width = 210*1.5, height = 30+40*5, units = "mm"
)

#' NO SELF ISOLATION
unlist(sapply(
  c("strgy_sodi", "strg_shie", "strg_sodi_shie"), function(x){
    scenarios_overview[scen == which(sindex==x), index][scenarios[[which(sindex==x)]][, symptomatic_contact] == 1]
  }
))
scenario_indexes = c(1, 73, 72, 76, 80, 78)
scenario_names = c("unmitigated", "20% distancing", "50% distancing", "80% shielding", "80% shielding + 20% distancing", "80% shielding + 50% distancing")

intervention <- data.table(
  s=scenario_indexes,
  name=scenario_names
)

use_scenarios <- c(intervention$s)
data_total <- rbindlist(lapply(use_scenarios, function(s) rbindlist(lapply(tolower(countries), function(x, s) getData(x, s, "total"), s))))
data_time <- rbindlist(lapply(use_scenarios, function(s) rbindlist(lapply(tolower(countries), function(x, s) getData(x, s, "time"), s))))

data_total[, population := strsplit(as.character(population), " - ")[[1]][1], by="population"]
data_total <- data_total[, .(value=sum(value)), by=c("run", "population", "compartment", "group", "t", "s", "scen")]
data_time[, population := strsplit(as.character(population), " - ")[[1]][1], by="population"]
data_time <- data_time[, .(value=sum(value)), by=c("run", "population", "compartment", "group", "t", "s", "scen")]

data_total_r0 = merge(data_total, run_options[, c("run", "country", "r0_type", "r0")], by.x=c("run", "population"), by.y=c("run", "country"))
data_time_r0 = merge(data_time, run_options[, c("run", "country", "r0_type", "r0")], by.x=c("run", "population"), by.y=c("run", "country"))

strategies_table <- makeTable(data_total_r0, data_time_r0, bycols=c("population", "compartment", "scen", "s", "r0_type"))
strategies_table[, scen := factor(scen, scenario_indexes, scenario_names)]
strategies_table <- dcast(strategies_table[, -"s"], ...~scen, value.var="format")
strategies_table[, compartment := factor(compartment, compartment_levels, compartment_labels)]
strategies_table[, r0_type := factor(r0_type, r0_types, r0_labels)]
fwrite(strategies_table, sprintf("%s/table_s4_strategies_noselfisolation.csv", outputs_dir))

#'TABLE FOR MAIN STRATEGIES
data_plot <- getQuantileRunData(data_time_r0[, -"r0"], bycols = c("population", "compartment", "group", "scen", "r0_type"))
data_plot[, outcome_label := factor(compartment, compartment_levels, compartment_labels)]
saveRDS(data_plot, sprintf("%s/figs5_noselfisolation_data.RDS", outputs_dir))

ggplot(
  data_plot[r0_type %in% c("global_med", "country_med") & compartment %in% c("death_o", "icu_p", "nonicu_p")],
  aes(
    x=t, y=value, colour=factor(
      paste0(r0_type, "_", population),
      as.vector(sapply(rev(r0_types), function(x) paste0(x, "_", countries)))
    ),
    group=factor(paste0(r0_type, "_", variable), as.vector(sapply(r0_types, function(x) paste0(x, "_", c("low95", "low50", "median", "high50", "high95"))))),
    linetype=factor(variable, c("low95", "low50", "median", "high50", "high95"), c("95%", "50%", "median", "50%", "95%")),
    size=factor(variable, c("low95", "low50", "median", "high50", "high95"), c("95%", "50%", "median", "50%", "95%"))
  )
)+geom_line(
)+facet_nested(
  factor(population, countries)+outcome_label~factor(scen,
    c(1, scenario_indexes),
    c("unmitigated", scenario_names)
  ),
  scales="free"
)+theme_bw(
)+theme(
  panel.grid.minor=element_blank(),
  legend.position="bottom"
)+labs(
  y="Daily value", x="Time (months)",
  linetype = "quantile"
)+scale_y_continuous(
  labels = scales::comma
)+scale_colour_manual(
  values=c(
    "global_med_Mauritius"="#2A34B1", "country_med_Mauritius"="#000000",
    "global_med_Nigeria"="#009F60", "country_med_Nigeria"="#000000",
    "global_med_Niger"="#E95506", "country_med_Niger"="#000000"
  ),
  guide = "none"
)+scale_x_continuous(
  breaks=c((365*2)/12)*c(0:12),
  labels=seq(0,24,2),
  limits = c(0,365*1)
)+scale_linetype_manual(
  values=c("median" = 1, "95%" = 3, "50%" = 2)
)+scale_size_manual(
  values=c("median" = 1.2, "95%" = 0.8, "50%" = 0.5),
  guide = "none"
)

for(extension in c("png", "pdf")) ggsave(
  sprintf("%s/figs5_strategies_noselfisolation.%s", outputs_dir, extension),
  width = 210*1.5, height = 30+40*5, units = "mm"
)

##