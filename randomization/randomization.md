First Email Randomization
================

``` r
source('helper_functions.R')
library(magrittr)
set.seed(1505)
```

### Anonymize the data and save as CSVs in the working directory

``` r
#load_anon_files('~/MIDS/W241/additional_data/20201016_index_file.csv',
#                '~/MIDS/W241/additional_data/20201018_data_file.csv',
#                '~/MIDS/W241/additional_data/20200721_data_file.csv')
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
data. Unfortunately, there’s ample evidence that the data that we were
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

### What about the metadata we have for the people who were scored by Boodle?

``` r
subject_boodle <- read_subjects_mail('anonymized_mail.csv')
subject_boodle <- subject_boodle[lookup_id %in% index_ids$`LOOKUP ID`]
merged_dt      <- merge(subject_data, subject_boodle, all = TRUE)
```

### De-duplicate (again)

``` r
duplicates_merged <- merged_dt[ , .(which(.N > 1)), keyby = lookup_id]$lookup_id
duplicate_index_merged <- which(merged_dt$lookup_id %in% duplicates_merged)
merged_index <- setdiff(1:nrow(merged_dt), duplicate_index_merged[c(1, 3)])
merged_dt <- merged_dt[merged_index , ]
merged_dt %>% summary
```

    ##   lookup_id          is_member.x      num_gifts.x   total_gift_amt.x
    ##  Length:12004       Min.   :0.0000   Min.   :0      Min.   :0       
    ##  Class :character   1st Qu.:0.0000   1st Qu.:0      1st Qu.:0       
    ##  Mode  :character   Median :0.0000   Median :0      Median :0       
    ##                     Mean   :0.2283   Mean   :0      Mean   :0       
    ##                     3rd Qu.:0.0000   3rd Qu.:0      3rd Qu.:0       
    ##                     Max.   :1.0000   Max.   :0      Max.   :0       
    ##                                      NA's   :1155   NA's   :1155    
    ##  largest_gift_amt.x largest_gift_date.x  first_gift_amt.x  
    ##  Min.   :   0.000   Min.   :2011-05-24   Min.   :   0.000  
    ##  1st Qu.:   0.000   1st Qu.:2012-05-23   1st Qu.:   0.000  
    ##  Median :   0.000   Median :2012-05-23   Median :   0.000  
    ##  Mean   :   0.779   Mean   :2012-04-15   Mean   :   0.783  
    ##  3rd Qu.:   0.000   3rd Qu.:2012-05-23   3rd Qu.:   0.000  
    ##  Max.   :5177.160   Max.   :2013-05-07   Max.   :5177.160  
    ##                     NA's   :11994                          
    ##  first_gift_date.x    latest_gift_amt.x  latest_gift_date.x  
    ##  Min.   :2011-05-24   Min.   :   0.000   Min.   :2011-05-24  
    ##  1st Qu.:2012-05-26   1st Qu.:   0.000   1st Qu.:2012-05-26  
    ##  Median :2020-05-04   Median :   0.000   Median :2020-05-04  
    ##  Mean   :2017-09-19   Mean   :   0.784   Mean   :2017-10-02  
    ##  3rd Qu.:2020-06-22   3rd Qu.:   0.000   3rd Qu.:2020-06-22  
    ##  Max.   :2020-10-11   Max.   :5177.160   Max.   :2020-10-11  
    ##  NA's   :11973                           NA's   :11973       
    ##  total_pledges.x        zip             is_member.y     num_gifts.y  
    ##  Min.   :   0.000   Length:12004       Min.   :0.000   Min.   :0     
    ##  1st Qu.:   0.000   Class :character   1st Qu.:0.000   1st Qu.:0     
    ##  Median :   0.000   Mode  :character   Median :0.000   Median :0     
    ##  Mean   :   1.854                      Mean   :0.323   Mean   :0     
    ##  3rd Qu.:   0.000                      3rd Qu.:1.000   3rd Qu.:0     
    ##  Max.   :5000.000                      Max.   :1.000   Max.   :0     
    ##                                        NA's   :3738    NA's   :4651  
    ##  total_gift_amt.y   largest_gift_amt.y largest_gift_date.y  first_gift_amt.y  
    ##  Length:12004       Length:12004       Min.   :2011-05-24   Length:12004      
    ##  Class :character   Class :character   1st Qu.:2012-05-23   Class :character  
    ##  Mode  :character   Mode  :character   Median :2012-05-23   Mode  :character  
    ##                                        Mean   :2012-04-15                     
    ##                                        3rd Qu.:2012-05-23                     
    ##                                        Max.   :2013-05-07                     
    ##                                        NA's   :11994                          
    ##  first_gift_date.y    latest_gift_amt.y  latest_gift_date.y  
    ##  Min.   :2011-05-24   Length:12004       Min.   :2011-05-24  
    ##  1st Qu.:2012-05-23   Class :character   1st Qu.:2012-05-23  
    ##  Median :2019-10-24   Mode  :character   Median :2019-10-24  
    ##  Mean   :2016-12-24                      Mean   :2017-01-09  
    ##  3rd Qu.:2020-05-22                      3rd Qu.:2020-05-22  
    ##  Max.   :2020-07-12                      Max.   :2020-07-12  
    ##  NA's   :11979                           NA's   :11979       
    ##  total_pledges.y    affinity_children affinity_education affinity_science
    ##  Length:12004       Min.   : 0.00     Min.   : 0.00      Min.   : 0.0    
    ##  Class :character   1st Qu.: 0.00     1st Qu.: 0.00      1st Qu.: 0.0    
    ##  Mode  :character   Median : 0.00     Median : 0.00      Median : 0.0    
    ##                     Mean   :12.48     Mean   :13.23      Mean   :12.8    
    ##                     3rd Qu.:35.00     3rd Qu.:35.00      3rd Qu.:35.0    
    ##                     Max.   :90.00     Max.   :90.00      Max.   :90.0    
    ##                     NA's   :3738      NA's   :3738       NA's   :3738    
    ##  affinity_culture tech_adoption     
    ##  Min.   : 0.00    Length:12004      
    ##  1st Qu.: 0.00    Class :character  
    ##  Median : 0.00    Mode  :character  
    ##  Mean   :16.74                      
    ##  3rd Qu.:35.00                      
    ##  Max.   :90.00                      
    ##  NA's   :3738

### Make our random draw from this population

``` r
merged_dt[ , receive_email := as.integer(sample(.N) <= 1980)]
merged_dt[receive_email == 1, treatment := sample(rep(1:4, .N / 4))]
merged_dt[receive_email == 1, .N, keyby = treatment]
```

    ##    treatment   N
    ## 1:         1 495
    ## 2:         2 495
    ## 3:         3 495
    ## 4:         4 495

### Randomization checks

``` r
merged_dt$affinity_children[is.na(merged_dt$affinity_children)] <- -999
merged_dt$affinity_education[is.na(merged_dt$affinity_education)] <- -999
merged_dt$affinity_science[is.na(merged_dt$affinity_science)] <- -999
merged_dt$affinity_culture[is.na(merged_dt$affinity_culture)] <- -999
merged_dt[receive_email == 1, lm(treatment ~ factor(affinity_children) + factor(affinity_education) + factor(affinity_science) + factor(affinity_culture) + factor(is_member.x))] %>% summary
```

    ## 
    ## Call:
    ## lm(formula = treatment ~ factor(affinity_children) + factor(affinity_education) + 
    ##     factor(affinity_science) + factor(affinity_culture) + factor(is_member.x))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.6209 -0.6181  0.1692  0.8240  1.6387 
    ## 
    ## Coefficients: (3 not defined because of singularities)
    ##                              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                   2.46907    0.04483  55.072   <2e-16 ***
    ## factor(affinity_children)0   -1.55154    1.23066  -1.261    0.208    
    ## factor(affinity_children)35  -1.31069    1.16197  -1.128    0.259    
    ## factor(affinity_children)90  -1.38202    1.18619  -1.165    0.244    
    ## factor(affinity_education)0   0.14302    0.35913   0.398    0.691    
    ## factor(affinity_education)35 -0.03598    0.18938  -0.190    0.849    
    ## factor(affinity_education)90       NA         NA      NA       NA    
    ## factor(affinity_science)0     0.05496    0.38697   0.142    0.887    
    ## factor(affinity_science)35   -0.12239    0.26439  -0.463    0.643    
    ## factor(affinity_science)90         NA         NA      NA       NA    
    ## factor(affinity_culture)0     1.46324    1.12733   1.298    0.194    
    ## factor(affinity_culture)35    1.44647    1.12257   1.289    0.198    
    ## factor(affinity_culture)42    1.49850    1.12462   1.332    0.183    
    ## factor(affinity_culture)50    1.43265    1.12582   1.273    0.203    
    ## factor(affinity_culture)90         NA         NA      NA       NA    
    ## factor(is_member.x)1         -0.06985    0.06383  -1.094    0.274    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.12 on 1967 degrees of freedom
    ## Multiple R-squared:  0.003683,   Adjusted R-squared:  -0.002395 
    ## F-statistic: 0.606 on 12 and 1967 DF,  p-value: 0.8388

### Export the anonymized assignments

``` r
random_assignment <- merged_dt[receive_email == 1, c("lookup_id", "treatment")]
fwrite(random_assignment, "anonymized_assignments.csv")
```
