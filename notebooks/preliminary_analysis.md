preliminary\_analysis
================
Taeil
10/25/2020

``` r
calculate_rse_ci <- function(lm.mod, dt, alpha = 0.05){
    lm.mod$se.ci_ <- confint(lm.mod, level = 1 - alpha)
    lm.mod$vcovHC_ <- vcovHC(lm.mod)
    lm.mod$rse_ <- sqrt(diag(lm.mod$vcovHC_))
    lm.mod$rse.ci_ <- confint(coeftest(lm.mod, vcov. = lm.mod$vcovHC_), level = 1 - alpha)
    
    return(lm.mod)
}
```

## Preliminary analysis

``` r
#d <- fread("./data/20201025_pilot_data.csv")
d <- fread("./data/20201029_results_with_covariates")


d[treatment_assigned == 1 | treatment_assigned == 2, subject := 1]
d[treatment_assigned == 3 | treatment_assigned == 4, subject := 0]
d[treatment_assigned == 1 | treatment_assigned == 3, sender := 1]
d[treatment_assigned == 2 | treatment_assigned == 4, sender := 0]

#summary(d)
head(d)
```

    ##    lookup_id treatment_assigned treatment_received open           open_time
    ## 1:      1000                  2                  2    1 2020-10-20 23:13:52
    ## 2:      1001                  1                  1    1 2020-10-20 20:53:38
    ## 3:      1010                  1                 NA   NA                <NA>
    ## 4:      1012                  2                  2    0                <NA>
    ## 5:      1016                  4                  4    1 2020-10-21 00:07:21
    ## 6:      1026                  2                  2    0                <NA>
    ##    open_count click click_time click_count bounce bounce_time bounce_count
    ## 1:          4     0       <NA>           0      0        <NA>            0
    ## 2:          1     0       <NA>           0      0        <NA>            0
    ## 3:         NA    NA       <NA>          NA     NA        <NA>           NA
    ## 4:          0     0       <NA>           0      0        <NA>            0
    ## 5:          2     0       <NA>           0      0        <NA>            0
    ## 6:          0     0       <NA>           0      0        <NA>            0
    ##           ZIP Member Total   Number of GIfts Total Gifts Amount
    ## 1:                                        NA                 NA
    ## 2:                                        NA                 NA
    ## 3: 22066-3908                              0                  0
    ## 4:      20120                              0                  0
    ## 5: 20191-3804                              0                  0
    ## 6: 06877-1522                              0                  0
    ##    Largest Gift Amount Largest Gift Date First Gift Amount First Gift Date
    ## 1:                  NA                                  NA                
    ## 2:                  NA                                  NA                
    ## 3:                   0                                   0                
    ## 4:                   0                                   0                
    ## 5:                   0                                   0                
    ## 6:                   0                                   0                
    ##    Latest Gift Amount Lastest Gift Date Total Pledges
    ## 1:                 NA                                
    ## 2:                 NA                                
    ## 3:                  0                            0.00
    ## 4:                  0                            0.00
    ## 5:                  0                            0.00
    ## 6:                  0                            0.00
    ##    Predicted Affinity for Children's Causes
    ## 1:                                       NA
    ## 2:                                       NA
    ## 3:                                       35
    ## 4:                                       35
    ## 5:                                        0
    ## 6:                                        0
    ##    Predicted Affinity for Educational Causes Predicted Affinity for Science
    ## 1:                                        NA                             NA
    ## 2:                                        NA                             NA
    ## 3:                                        35                             90
    ## 4:                                        35                             35
    ## 5:                                         0                              0
    ## 6:                                         0                              0
    ##    Predicted Affinity for Arts & Culture Predicted Level of Technology Adoption
    ## 1:                                    NA                                       
    ## 2:                                    NA                                       
    ## 3:                                    42                             Apprentice
    ## 4:                                    35                                      0
    ## 5:                                     0                                      0
    ## 6:                                     0                                      0
    ##    subject sender
    ## 1:       1      0
    ## 2:       1      1
    ## 3:       1      1
    ## 4:       1      0
    ## 5:       0      0
    ## 6:       1      0

``` r
#names(d)
#d = na.omit(d)
```

Lets quickly check the open treatment effect.

``` r
m1 = d[ , lm(open ~ subject)]
m2 = d[ , lm(open ~ sender)]
m3 = d[ , lm(open ~ subject + sender + subject*sender)]
m1 = calculate_rse_ci(m1, d)
m2 = calculate_rse_ci(m2, d)
m3 = calculate_rse_ci(m3, d)

stargazer(m1, m2, m3, se = c(list(m1$rse_), list(m2$rse_), list(m3$rse_)), type="text")
```

    ## 
    ## =====================================================================================
    ##                                            Dependent variable:                       
    ##                     -----------------------------------------------------------------
    ##                                                   open                               
    ##                              (1)                   (2)                   (3)         
    ## -------------------------------------------------------------------------------------
    ## subject                    0.044**                                     0.058**       
    ##                            (0.019)                                     (0.028)       
    ##                                                                                      
    ## sender                                            -0.025               -0.010        
    ##                                                  (0.019)               (0.027)       
    ##                                                                                      
    ## subject:sender                                                         -0.029        
    ##                                                                        (0.039)       
    ##                                                                                      
    ## Constant                   0.223***              0.258***             0.229***       
    ##                            (0.013)               (0.014)               (0.019)       
    ##                                                                                      
    ## -------------------------------------------------------------------------------------
    ## Observations                1,957                 1,957                 1,957        
    ## R2                          0.003                 0.001                 0.004        
    ## Adjusted R2                 0.002                 0.0003                0.002        
    ## Residual Std. Error   0.430 (df = 1955)     0.430 (df = 1955)     0.430 (df = 1953)  
    ## F Statistic         5.058** (df = 1; 1955) 1.627 (df = 1; 1955) 2.412* (df = 3; 1953)
    ## =====================================================================================
    ## Note:                                                     *p<0.1; **p<0.05; ***p<0.01

``` r
m3$rse.ci_
```

    ##                      2.5 %     97.5 %
    ## (Intercept)     0.19129234 0.26585052
    ## subject         0.00329123 0.11273664
    ## sender         -0.06285655 0.04192768
    ## subject:sender -0.10496912 0.04761478
