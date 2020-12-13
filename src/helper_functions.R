library(data.table)


load_anon_files <- function(index_path, complete_path, mail_only_path) {
  # Load and anonymize the data
  
  # Load files
  index_dt    <- fread(index_path)
  complete_dt <- fread(complete_path)
  mail_dt     <- fread(mail_only_path)
  
  # Cut offending columns
  index_dt_cut    <- index_dt[ , c("LAST NAME", "FIRST NAME", "FULL NAME",
                                   "EMAIL ADDRESS") := NULL]
  complete_dt_cut <- complete_dt[ , c("First", "Last") := NULL]
  mail_dt_cut     <- mail_dt[ , c("First", "Last", "Address1", "Address2",
                                  "City", "State",
                                  "Donor Total Donations - NEW Smart Field\\Currency ID",
                                  "Largest payment amount Smart Field\\Currency ID",
                                  "Founders Society Levels Smart Field\\Currency ID",
                                  "Latest payment amount Smart Field\\Currency ID",
                                  "First payment amount Smart Field\\Currency ID",
                                  "QUERYRECID") := NULL]
  
  # Lowercase column names
  names(index_dt)    <- tolower(names(index_dt))
  
  # Save the files
  fwrite(index_dt_cut, "anonymized_index.csv")
  fwrite(complete_dt_cut, "anonymized_altru.csv")
  fwrite(mail_dt_cut, "anonymized_mail.csv")
}


read_subjects <- function(csv_path) {
  # Reads a CSV file with subject data and sets proper
  # column classes
  
  dt <- fread(csv_path)
  names(dt) <- c('lookup_id', 'is_member', 'num_gifts', 'total_gift_amt',
                 'largest_gift_amt', 'largest_gift_date', 'first_gift_amt',
                 'first_gift_date', 'latest_gift_amt', 'latest_gift_date',
                 'total_pledges')
  dt[ , is_member         := as.integer(is_member != '')]
  dt[ , largest_gift_date := as.Date(largest_gift_date, '%m/%d/%Y')]
  dt[ , first_gift_date   := as.Date(first_gift_date, '%m/%d/%Y')]
  dt[ , latest_gift_date  := as.Date(latest_gift_date, '%m/%d/%Y')]
  setkey(dt, lookup_id)
  
  return(dt)
}


read_subjects_mail <- function(csv_path) {
  # Reads a CSV file with subject data and sets proper
  # column classes
  
  dt <- fread(csv_path)
  names(dt) <- c('lookup_id', 'zip', 'is_member', 'num_gifts', 'total_gift_amt',
                 'largest_gift_amt', 'largest_gift_date', 'first_gift_amt',
                 'first_gift_date', 'latest_gift_amt', 'latest_gift_date',
                 'total_pledges', 'affinity_children', 'affinity_education',
                 'affinity_science', 'affinity_culture', 'tech_adoption')
  dt[ , is_member         := as.integer(is_member != '')]
  dt[ , largest_gift_date := as.Date(largest_gift_date, '%m/%d/%Y')]
  dt[ , first_gift_date   := as.Date(first_gift_date, '%m/%d/%Y')]
  dt[ , latest_gift_date  := as.Date(latest_gift_date, '%m/%d/%Y')]
  setkey(dt, lookup_id)
  
  return(dt)
}


anon_results <- function(data_path, randomization_path, results_path) {
  # Join data from MailChimp to metadata, anonymize, and save output
  
  chimp_data <- fread(data_path)
  names(chimp_data)[1:2] <- c('Email Address', 'treatment_received')
  
  randomization <- fread(randomization_path)
  results <- merge(randomization, chimp_data, 'Email Address', all.x = TRUE)

  names(results)[2] = 'lookup_id'

  results_cut <- results[ , c("Last Name", "First Name", "Email Address") := NULL]
  names(results_cut)[2] = 'treatment_assigned'
  
  fwrite(results_cut, results_path)
}


join_covariates <- function(results_path, covariates_path, combined_path) {
  # Joins MailChimp results back to the covariates keyed by lookup_id
  results <- fread(results_path)
  covariates <- fread(covariates_path)
  names(covariates)[1] = 'lookup_id'
  results_w_covariates <- merge(results, covariates, all.x = TRUE)
  fwrite(results_w_covariates, combined_path)
}

calculate_rse_ci <- function(lm.mod, dt, alpha = 0.05){
  lm.mod$se.ci_ <- confint(lm.mod, level = 1 - alpha)
  lm.mod$vcovHC_ <- vcovHC(lm.mod)
  lm.mod$rse_ <- sqrt(diag(lm.mod$vcovHC_))
  lm.mod$rse.ci_ <- confint(coeftest(lm.mod, vcov. = lm.mod$vcovHC_), level = 1 - alpha)
  
  return(lm.mod)
}