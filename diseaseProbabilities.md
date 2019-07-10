---
title: "Disease Probabilities"
author: "Gregory Maly"
date: "March 17, 2018"
output: html_document
---



# Disease Probabilities

While still under investigation, the patient has experienced two overarching diagnoses of rare disorders: Polymylagia Rheumatic (PMR), and Aplastic Anemia. This brief analysis will look at the probabilities of contracting each.

## Polymyalgia Rheumatica

According to a report from the Vasculities Foundation, "data from population-based studies estimate that about 59 out of 100,000 people over the age of 50 will developer PMR in a one-year period."

```r
PMRprobabilitiy <- 59/100000
print(PMRprobabilitiy)
```

```
## [1] 0.00059
```

Not accounting for medical history or demographic profiles, a person over the age of 50 has a .06% chance of experiencing PMR in a given year.

Source: [Vasculitis Foundation](https://www.vasculitisfoundation.org/education/forms/polymyalgia-rheumatica/)

## Aplastic Anemia

According to 2013 report by by the Harvard Medical School, "Aplastic Anemia strikes two to six of every 1 million people in the United States and Europe." 


```r
AAprobability <- 4/1000000
print(AAprobability)
```

```
## [1] 4e-06
```

Not accounting for medical history or demographic profiles, a person in the United States has a 0.0004% chance of experiencing Aplastic Anemia.

Source: [Harvard Health Publishing](https://www.health.harvard.edu/heart-health/aplastic-anemia)

## Probability of Both in a Given Year
While each diagnosis is still under investigation, the patient is being medically treated for PMR, and undergoing tests to identify the root cause of poor blood health. 


```r
AAandPMRprobability <- PMRprobabilitiy * AAprobability
print(AAandPMRprobability)
```

```
## [1] 2.36e-09
```

Based on national reporting, an individual has a 0.0000002% chance of experiencing both of these diagnoses in the same year.
