#' default contact matrices
suppressPackageStartupMessages({
  require(data.table)
  #' this is the UPDATED socialmixr from https://github.com/jarvisc1/socialmixr
  require(socialmixr) 
})

.args <- if (interactive()) c(
  "~/workspace/covid_lmic/interventions/generation_data/data_contacts_missing.csv",
  "~/workspace/covidm", "niger", 150*4,
  "~/workspace/covid_lmic/interventions/reviewer_comments/inputs/niger/contact_matrices_synthetic.rds"
) else commandArgs(trailingOnly = TRUE)

lookup <- fread(.args[1])[, short := tolower(gsub("[^a-zA-Z]","",name))]
cm_path <- .args[2]
target <- .args[3]
runs <- as.numeric(.args[4])
result <- tail(.args, 1)

cm_home_all = list()
cm_other_all = list()

proxies <- c("kenya" = 2011, "zimbabwe" = 2013, "uganda" = 2014)

if (file.exists(result)) {
  warning(sprintf("Skipping %s; already exists.", target))
} else {
  
  for(p in 1:length(proxies)){
    #' generate a new set of bootstrapped matrices if available
    emp_contacts <- sprintf("../data/empirical_contact_matrices/%s/contacts.csv", names(proxies)[p])
    emp_participants <- sprintf("../data/empirical_contact_matrices/%s/participants.csv",names(proxies)[p])
    
    part <- fread(emp_participants)
    cont <- fread(emp_contacts)
    part[, "year"] <- p
    cont[, "year"] <- p
    
    survey_data <- survey(part, cont)
    cm_home <- contact_matrix(
      survey_data,
      filter = list(location = "house"), estimated.participant.age = "sample",
      estimated.contact.age = "sample", bootstrap = TRUE, n = runs, symmetric = FALSE,
      age.limits = seq(0, by = 5, length.out = 16)
    )
    
    cm_home <- lapply(cm_home$matrices, "[[", 1)
    #' replace any samples with missing values
    cm_home <- lapply(cm_home, function(mat){
      if (sum(is.na(mat)) > 0){
        mat_curr <- mat
        while(sum(is.na(mat_curr)) > 0){
          mat_curr <- contact_matrix(
            survey_data,
            filter = list(location = "house"), estimated.participant.age = "sample",
            estimated.contact.age = "sample", bootstrap = TRUE, n = 2, symmetric = FALSE,
            age.limits = seq(0, by = 5, length.out = 16)
          )$matrices[[1]]$matrix
        }
        mat <- mat_curr
      }
      return(mat)
    })
    
    cm_other <- contact_matrix(
      survey_data,
      filter = list(location = "other"), estimated.participant.age = "sample",
      estimated.contact.age = "sample", bootstrap = TRUE, n = runs, symmetric = FALSE,
      age.limits = seq(0, by = 5, length.out = 16)
    )
    cm_other <- lapply(cm_other$matrices, "[[", 1)
    #' replace any samples with missing values
    cm_other <- lapply(cm_other, function(mat){
      if (sum(is.na(mat)) > 0){
        mat_curr <- mat
        while(sum(is.na(mat_curr)) > 0){
          mat_curr <- contact_matrix(
            survey_data,
            filter = list(location = "other"), estimated.participant.age = "sample",
            estimated.contact.age = "sample", bootstrap = TRUE, n = 2, symmetric = FALSE,
            age.limits = seq(0, by = 5, length.out = 16)
          )$matrices[[1]]$matrix
        }
        mat <- mat_curr
      }
      return(mat)
    })
    
    old_length = length(cm_home_all)
    for(i in 1:length(cm_other)){
      cm_home_all[[old_length+i]] <- cm_home[[i]]
      cm_other_all[[old_length+i]] <- cm_other[[i]]
    }
    
    #make symmetric for country data
    N <- readRDS(sprintf("%s/data/wpp2019_pop2020.rds", cm_path))
    N[, total := (f+m)*1000]
    N <- N[tolower(name) == target]
    N <- c(
      N[age %in% paste0(seq(0,70,5),"-",seq(4,74,5)), total],
      sum(N[!age %in% paste0(seq(0,70,5),"-",seq(4,74,5)), total])
    )
    
    cm_home <- lapply(
      cm_home_all,
      function(m){
        #make symmetric
        sym <- t(m)
        for(i in 1:nrow(m)){
          for(j in 1:ncol(m)){
            sym[i,j] <- ( m[i,j] + m[j,i]*( N[j]/N[i] ) )/2
          }
        }
        return(t(sym))
      }
    )
    
    cm_other <- lapply(
      cm_other_all,
      function(m){
        #make symmetric
        sym <- t(m)
        for(i in 1:nrow(m)){
          for(j in 1:ncol(m)){
            sym[i,j] <- ( m[i,j] + m[j,i]*( N[j]/N[i] ) )/2
          }
        }
        return(t(sym))
      }
    )
    
    #' currently don't have stratification by school or work
    cm <- list()
    for(i in 1:runs){
      cm[[i]] <- list(
        "home" = cm_home[[i]],
        "work" = cm_other[[1]]*0,
        "school" = cm_other[[1]]*0,
        "other" = cm_other[[i]]
      ) 
    }
    saveRDS(cm, sprintf("~/workspace/covid_lmic/interventions/reviewer_comments/inputs/%s/contact_matrices_empcombined.rds", target))
    
    refcontactmatrices <- readRDS(sprintf("%s/data/all_matrices.rds", cm_path))
    found <- lookup[short == target, ifelse(cm, name, cm_name)]
    if (length(found)) {
      matrices <- refcontactmatrices[[found[1]]]
      saveRDS(matrices, tail(.args, 1))
    } else {
      warning(sprintf("[SYNTHETIC] No matches found for %s; alternative not available yet.", target))
    } 
  }
}
