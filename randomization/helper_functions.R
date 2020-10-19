library(data.table)


load_anon_files <- function(index_path, complete_path) {
  # Load and anonymize the data
  
  # Load files
  index_dt    <- fread(index_path)
  complete_dt <- fread(complete_path)
  
  # Cut offending columns
  index_dt_cut <- index_dt[ , c("LAST NAME", "FIRST NAME", "FULL NAME", "EMAIL ADDRESS") := NULL]
  complete_dt_cut <- complete_dt[ , c("First", "Last") := NULL]
  
  # Lowercase column names
  names(index_dt)    <- tolower(names(index_dt))
  names(complete_dt) <- tolower(names(complete_dt))
  
  # Save the files
  fwrite(index_dt_cut, "anonymized_index.csv")
  fwrite(complete_dt_cut, "anonymized_altru.csv")
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