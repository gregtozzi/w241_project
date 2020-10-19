First Email Randomization
================

``` r
source('helper_functions.R')
library(magrittr)
```

### Anonymize the data and save as CSVs in the working directory

``` r
load_anon_files('../../additional_data/20201016_index_file.csv',
                '../../additional_data/20201018_data_file.csv')
```

### Load the anonymized data, clean it, and select only those IDs of interest

``` r
subject_data <- read_subjects('anonymized_altru.csv')
index_ids <- fread('anonymized_index.csv')
subject_data <- subject_data[lookup_id %in% index_ids$`LOOKUP ID`]
```

### De-duplicate

``` r
duplicates <- subject_data[ , .(which(.N > 1)), keyby = lookup_id]$lookup_id
subject_data[lookup_id %in% duplicates, ]
```

    ##     lookup_id is_member num_gifts total_gift_amt largest_gift_amt
    ## 1: 8-10017759         1         0              0                0
    ## 2: 8-10017759         1         0              0                0
    ## 3: 8-10019693         1         0              0                0
    ## 4: 8-10019693         1         0              0                0
    ## 5: 8-10030494         1         0              0                0
    ## 6: 8-10030494         1         0              0                0
    ##    largest_gift_date first_gift_amt first_gift_date latest_gift_amt
    ## 1:              <NA>              0            <NA>               0
    ## 2:              <NA>              0            <NA>               0
    ## 3:              <NA>              0            <NA>               0
    ## 4:              <NA>              0            <NA>               0
    ## 5:              <NA>              0            <NA>               0
    ## 6:              <NA>              0            <NA>               0
    ##    latest_gift_date total_pledges
    ## 1:             <NA>             0
    ## 2:             <NA>             0
    ## 3:             <NA>             0
    ## 4:             <NA>             0
    ## 5:             <NA>             0
    ## 6:             <NA>             0

Three IDs are repeated twice each. Remove the first instance of each.

``` r
duplicate_index <- which(subject_data$lookup_id %in% duplicates)
duplicate_index[c(1, 3, 5)]
```

    ## [1] 3392 3890 6601

``` r
subject_index <- setdiff(1:nrow(subject_data), duplicate_index[c(1, 3, 5)])
subject_data <- subject_data[subject_index , ]
```

We now have 12004 unique lookup IDs, which is what we want.

### Now for the bad part…

My plan was to block based on giving history since we don’t have Boodle
data. Unfortuantely, there’s ample evidence that the data that we were
provided is incomplete.

``` r
subject_data %>% summary
```

    ##   lookup_id           is_member        num_gifts    total_gift_amt
    ##  Length:12004       Min.   :0.0000   Min.   :0      Min.   :0     
    ##  Class :character   1st Qu.:0.0000   1st Qu.:0      1st Qu.:0     
    ##  Mode  :character   Median :0.0000   Median :0      Median :0     
    ##                     Mean   :0.2283   Mean   :0      Mean   :0     
    ##                     3rd Qu.:0.0000   3rd Qu.:0      3rd Qu.:0     
    ##                     Max.   :1.0000   Max.   :0      Max.   :0     
    ##                                      NA's   :1155   NA's   :1155  
    ##  largest_gift_amt   largest_gift_date    first_gift_amt    
    ##  Min.   :   0.000   Min.   :2011-05-24   Min.   :   0.000  
    ##  1st Qu.:   0.000   1st Qu.:2012-05-23   1st Qu.:   0.000  
    ##  Median :   0.000   Median :2012-05-23   Median :   0.000  
    ##  Mean   :   0.779   Mean   :2012-04-15   Mean   :   0.783  
    ##  3rd Qu.:   0.000   3rd Qu.:2012-05-23   3rd Qu.:   0.000  
    ##  Max.   :5177.160   Max.   :2013-05-07   Max.   :5177.160  
    ##                     NA's   :11994                          
    ##  first_gift_date      latest_gift_amt    latest_gift_date    
    ##  Min.   :2011-05-24   Min.   :   0.000   Min.   :2011-05-24  
    ##  1st Qu.:2012-05-26   1st Qu.:   0.000   1st Qu.:2012-05-26  
    ##  Median :2020-05-04   Median :   0.000   Median :2020-05-04  
    ##  Mean   :2017-09-19   Mean   :   0.784   Mean   :2017-10-02  
    ##  3rd Qu.:2020-06-22   3rd Qu.:   0.000   3rd Qu.:2020-06-22  
    ##  Max.   :2020-10-11   Max.   :5177.160   Max.   :2020-10-11  
    ##  NA's   :11973                           NA's   :11973       
    ##  total_pledges     
    ##  Min.   :   0.000  
    ##  1st Qu.:   0.000  
    ##  Median :   0.000  
    ##  Mean   :   1.854  
    ##  3rd Qu.:   0.000  
    ##  Max.   :5000.000  
    ## 

Note that the `num_gifts` and `total_gift_amt` columns are zero or NA
across the board, despite individuals clearly having given to the
Center. Emily went into Altru and spot checked the data for me. She
found evidence that the reported figures do not total across households.
As it stands, I don’t know that we can do better than simply pulling a
random subset of the IDs and praying for the best.
