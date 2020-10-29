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
d <- fread("./data/20201025_pilot_data.csv")


d[treatment_assigned == 1 | treatment_assigned == 2, subject := 1]
d[treatment_assigned == 3 | treatment_assigned == 4, subject := 0]
d[treatment_assigned == 1 | treatment_assigned == 3, sender := 1]
d[treatment_assigned == 2 | treatment_assigned == 4, sender := 0]

#summary(d)
head(d)
```

    ##     lookup_id treatment_assigned treatment_received open open_time subject
    ## 1: 8-10026640                  4                  4    0      <NA>       0
    ## 2: 8-10042057                  2                  2    0      <NA>       1
    ## 3: 8-10031487                  1                  1    0      <NA>       1
    ## 4: 8-10031709                  2                  2    0      <NA>       1
    ## 5: 8-10032213                  2                  2    0      <NA>       1
    ## 6: 8-10052426                  4                  4    0      <NA>       0
    ##    sender
    ## 1:      0
    ## 2:      0
    ## 3:      1
    ## 4:      0
    ## 5:      0
    ## 6:      0

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

stargazer(m1, m2, m3, type="text")
```

    ## 
    ## =====================================================================================
    ##                                            Dependent variable:                       
    ##                     -----------------------------------------------------------------
    ##                                                   open                               
    ##                              (1)                   (2)                   (3)         
    ## -------------------------------------------------------------------------------------
    ## subject                    0.047**                                     0.062**       
    ##                            (0.019)                                     (0.027)       
    ##                                                                                      
    ## sender                                            -0.022               -0.006        
    ##                                                  (0.019)               (0.027)       
    ##                                                                                      
    ## subject:sender                                                         -0.031        
    ##                                                                        (0.039)       
    ##                                                                                      
    ## Constant                   0.219***              0.254***             0.222***       
    ##                            (0.014)               (0.014)               (0.019)       
    ##                                                                                      
    ## -------------------------------------------------------------------------------------
    ## Observations                1,957                 1,957                 1,957        
    ## R2                          0.003                 0.001                 0.004        
    ## Adjusted R2                 0.002                 0.0001                0.002        
    ## Residual Std. Error   0.428 (df = 1955)     0.429 (df = 1955)     0.428 (df = 1953)  
    ## F Statistic         5.840** (df = 1; 1955) 1.261 (df = 1; 1955) 2.578* (df = 3; 1953)
    ## =====================================================================================
    ## Note:                                                     *p<0.1; **p<0.05; ***p<0.01
