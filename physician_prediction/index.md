---
title       : Active Physician number Prediction
subtitle    : inference against on corresponding city population
author      : Shuo Zhao
job         : Data Scientist
framework   : io2012        # {io2012, html5slides, shower, dzslides, ...}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      # 
widgets     : []            # {mathjax, quiz, bootstrap}
mode        : selfcontained # {standalone, draft}
knit        : slidify::knit2slides
---



## Shiny App Intro

In any regular cities around the world, we need physicians. But how many physicians do we need in a particular city? The number of active physicians is expected to be related to total population, number of hospital beds, and total personal income etc. Here, I built a shiny application to apply linear regression model based on city's population to infer the number of active physicians for the city.

The data set I am using provides selected county demographic information (CDI) for 440 of the most populous counties in the United States. Each line of the data set has an identification number with a county name and state abbreviation and provides information on 14 variables for a single county. The information generally pertains to the years 1990 and 1992. There are 17 variables in the original dataset, we only use population and physicians in the application.

---

## Deployed Application

* You can find the deployed application from here: 
 + https://garfiny.shinyapps.io/project_part1

* The source code of the App & this slide shared in github:
 + https://github.com/garfiny/Developing-Data-Products-Prj

---

## Feature 1: Exploratory Analysis
![plot of chunk unnamed-chunk-2](assets/fig/unnamed-chunk-2-1.png) 

---

## Feature 2 - Regression Analysis

```r
predictions <- ceiling(predict(fit, newdata = data.frame(population = c(1000, 2000, 3000))))
```

- Linear Regression Model: #Physicians =  -131.361 + 2.853 * #Population
- Every 2.853 thousand people population increase will generate 1 more physician.
- Given populations 1000, 2000, 3000, our predictions respectively are 2722, 5574, 8427


```r
predictions <- predict(fit, data.frame(population = data$population))
mse <- sum((data$physicians - predictions)^2) / (n - 2)
yh <- predict(fit, newdata = data.frame(population = c(200, 2000)))
s_yh <- sqrt(mse * (1/n + (c(200, 2000) - p_mean)^2/sum((data$population - p_mean)^2)))
conf_interval1 <- yh[1] + c(-1, 1) * qt(0.995, n - 2) * s_yh[1]
conf_interval2 <- yh[2] + c(-1, 1) * qt(0.995, n - 2) * s_yh[2]
```

- 99% Confidence Interval for 200K population: 357.5930351, 520.6885685
- 99% Confidence Interval for 2000K population: 5245.2474047, 5902.0695887


