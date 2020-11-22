Examining the Effects of
================
Taeil Goh, Greg Tozzi & Max Ziff
November 21, 2020

## Abstract

`place holder`

## Introduction and Context

#### The Children’s Science Center

The Children’s Science Center (“the Center”) is a non-profit
organization headquartered in Fairfax, Virginia. Founded in TODO, the
Center’s principal objective is to build the Northern Virginia region’s
first interactive science center at a greenfield location on donated
land in Reston, Virginia. The capital campaign to raise fund to build
this facility is the Center’s primary line of effort. A recent
partnership with the Commonwealth of Virginia’s

Recognizing the long-term nature of the capital campaign and the need to
build interest and advocacy, the Center launched a series of
intermediate efforts beginning in `TODO.` The Center’s first outreach
effort was the Museum without Walls, a program that delivered
science-based programs to schools around the region. In `TODO`, the
Center launched the Lab, a test facility located in a shopping mall
sited 12 miles outside of the Washington, DC Beltway. The Lab hosted
approximately `TODO` guests per year until it was temporarily shut down
in response to the COVID-19 pandemic.

The Center funds operating expenses with Lab admissions, grants, and
philanthropic giving. The Center’s Development Department is responsible
for developing and managing the latter two revenue streams.

##### The Fundraising Surge

The Center conducts a fundraising surge toward the end of the calendar
year, consistent with the charitable giving cycle in the United States
\[`TODO`: add reference\].

The Center expected to engage the majority of its stakeholders by email.
The Center viewed email marketing as repeated iterations of a three step
process. The Development Director expected that several rounds of
engagement would be necessary to achieve a conversion in the form of a
charitable gift. Within each individual round, the path to conversion
proceeded from receipt to opening to click-through using an embedded
link or button.

The Center introduced two major changes to its process ahead of the 2020
fundraising surge. In previous year-end campaigns, the Center had used
Constant Contact to send branded emails to stakeholders. In the 2020
campaign, the Center chose to switch to Mail Chimp’s free tier. The
Center also engaged a consulting firm that applies machine learning to
the nonprofit space to predict top prospects from among the Center’s
contact list and to predict affinities to aspects of the Center’s
mission. The consulting firm’s model requires a physical address as an
entering argument, and it was, therefore, only applied to that subset of
the Center’s contact list for which physical addresses were entered.
Based on the consulting firm’s output, the Center chose to engage a
subset of their stakeholder list with physical mail and to wall this
cohort off from subsequent email engagement.

#### Research Questions

The Center’s development staff was most strongly interested in
conducting experiments to maximize the opening rates of the Center’s
fundraising emails. The Development Director and her staff were keenly
aware that email opening was the critical first step in achieving
conversion. The follow-on actions—click though and conversion—were areas
of secondary interest for this study. With this in mind, we sought to
answer two specific questions posed by the Center.

1.  Is there a difference in email opening or click-through caused by
    using the Executive Director’s name and title in the from line of
    the solicitation email versus using the Board Chair’s name and
    title?

2.  Is there a difference in email opening or click-through behavior
    caused by the choice of using one of two potential subject lines?
    The two subject lines considered were, “You can be a Catalyst for
    STEM Learning 💡,” and , “Invest in the Power of STEM Learning 💡.”

## Research Proposal

#### Research Hypotheses

We claim expertise neither in non-profit email fundraising nor in the
preferences of the Center’s stakeholders. The Center’s development team
believed that there would be a significant difference in the opening
rates associated with the two email from lines considered, though the
staff did not have a sense of which would result in significantly higher
opening rates. Underpinning the staff’s expectation that we would find a
significant difference was their belief that the Center’s stakeholders
had a meaningful sense of the Center’s leadership and governance
structure and would react differently when presented with emails from
either the Executive Director or the Board Chair.

Both of the subject lines considered were written by the Center’s
development staff. The staff believed that subject lines could result in
significantly different opening rates but did not have a going-in belief
of which subject line would cause a stronger response.

#### Treatment in Details

`see page 428`

The experiment was structured as two concurrant A/B tests conducted on a
single subject cohort. With the cohort randomly assigned to four groups,
and with the from and subject lines denoted \(F_a\), \(F_b\), \(S_a\),
and \(S_b\), we assigned treatment combinations across balanced groups:

  - Group 1 - \(F_a\) and \(S_a\)
  - Group 2 - \(F_a\) and \(S_b\)
  - Group 3 - \(F_b\) and \(S_a\)
  - Group 4 - \(F_b\) and \(S_b\)

This design allows us to explore heterogeneous treatment effects.

Each group would received a tailored email containing its assigned
treatments sent through Mail Chimp. We intended to track opening and
click through using Mail Chimp’s out-of-the-box analytics.

#### Power calculation

`place holder`

#### Enrollment Process & Criteria for Subjects

  - The criteria by which subjects were included
  - explain how subjects were randomly assigned to experimental groups
  - TG: need explanation of 2000 selection was randomized and no
    selection bias

The Center provided an anonymized list of TODO stakeholders extracted
from its contact database. The individuals represented the Center’s
complete contact list less a cohort of approximately 2,000 individuals
identified by the Center for a special traditional mail campaign and
less a relatively small number of individuals walled off from
fundraising activities due to age, relationship to board members, and
other similar factors. The Center provided the output of the consulting
firm’s model for individuals with mailing address on file. This output
consisted of buckted predictions of affinity for individuals to certain
causes related to the Center’s mission—educational causes, children’s
causes, cultural causes, and scientific causes.

We agreed to target a random sample of 1,980 individuals for this
experiment. The intent was to remain within the bounds of the Mail Chimp
free tier while leaving a small margin for the Center’s staff to send
test emails to individuals outside of the experiment including key
members of the Center’s staff and our group. The Center delivered one of
four solicitation emails via Mail Chimp’s web interface.

#### Summary of Experimental Design

#### Validation of Randomization Procedure

  - check the soundness of the randomization procedure
  - covariate balance (see chapter 4)

Balance checks using Boodle.ai data

#### Outcome Measures

  - Open and click-through rates.
  - mail chimp

#### Plan to analyze the data

  - how you plan to analyze the data

#### Accounting for Non-Compliance

## Research Report

#### Flow Diagram

`optional`

#### Experiment Results

  - Attrition
  - additional outcome (e.g., unsubscription rate)
  - Mediation analysis: why more open rate?
  - description of experimental results

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

``` r
calculate_rse_ci <- function(lm.mod, dt, alpha = 0.05){
    lm.mod$se.ci_ <- confint(lm.mod, level = 1 - alpha)
    lm.mod$vcovHC_ <- vcovHC(lm.mod)
    lm.mod$rse_ <- sqrt(diag(lm.mod$vcovHC_))
    lm.mod$rse.ci_ <- confint(coeftest(lm.mod, vcov. = lm.mod$vcovHC_), level = 1 - alpha)
    
    return(lm.mod)
}
```

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
