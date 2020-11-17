Increase Donation with Email Campaign
================
November 14, 2020

## Research Questions

We sought to answer two questions posed by the Center. Both questions
were related to emails sent by the Center soliciting donations.

1.  Is there a difference in email opening or click-through caused by
    the choice of using the Executive Director’s name and title in the
    from line versus using the Board Chair’s name and title? While we
    did observe a difference in opening rates favoring emails from Jill
    McNabb, the difference was not significant.

2.  Is there a difference in email opening or click-through behavior
    caused by the choice of using one of two potential subject lines?
    The use of the subject line, “You can be a Catalyst for STEM
    Learning 💡,” caused significantly higher opening rates.

## Review of the Methodology

The Center provided our team with a list of approximately 12,000
potential subjects for this study. This list was compiled from the
Center’s Altru database but specifically excluded individuals who were
previously selected by the Center to receive a solicitation by
traditional mail. We randomly selected 1,980 subjects to receive an
email soliciting a donation. These individuals were evenly split into
four groups of 495 with each group being assigned a unique combination
of from and subject lines. The 68% of the population that had mailing
addresses on file with the Center also had metadata provided by
Boodle.ai related to their predicted affinity for various causes. We
used this metadata to check the balance of our random assignment

## Pre Experiment

`something related to preparing the experiment`

#### Power calculation

`place holder`

#### Covariate Balance Check (Pre-experiment)

`place holder`

## Post Experiment

#### Measurement

``` r
calculate_rse_ci <- function(lm.mod, dt, alpha = 0.05){
    lm.mod$se.ci_ <- confint(lm.mod, level = 1 - alpha)
    lm.mod$vcovHC_ <- vcovHC(lm.mod)
    lm.mod$rse_ <- sqrt(diag(lm.mod$vcovHC_))
    lm.mod$rse.ci_ <- confint(coeftest(lm.mod, vcov. = lm.mod$vcovHC_), level = 1 - alpha)
    
    return(lm.mod)
}
```

## Result Analysis

  - Subject: 6 percentage point \[0.3pp \~ 11pp\]
  - Sender: no significant difference

<!-- end list -->

``` r
#d <- fread("./data/20201025_pilot_data.csv")
d <- fread("../data/raw/20201029_results_with_covariates")

d[treatment_assigned == 1 | treatment_assigned == 2, subject := 1]
d[treatment_assigned == 3 | treatment_assigned == 4, subject := 0]
d[treatment_assigned == 1 | treatment_assigned == 3, sender := 1]
d[treatment_assigned == 2 | treatment_assigned == 4, sender := 0]

#summary(d)
#head(d)
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

#### Notes on Applying the Results

We are confident that the method we employed in this case produced
evidence that the difference in opening rates was caused by the
difference in subject lines. You can reasonably expect that this result
would generalize to the remainder of the candidate email recipients
during the current year-end fund drive. We suggest caution when drawing
broader conclusions, however for the following reasons: 1.
Responsiveness to the two subject lines may have been affected by
factors that we could not control for, including the current public
health and political situations. 2. We drew our subjects from a filtered
list of potential recipients. Adding individuals to the population of
recipients may invalidate the study’s results.

#### Potential Additional Topics

  - Attrition
  - additional outcome (e.g., unsubscription rate)
  - Mediation analysis: why more open rate?
