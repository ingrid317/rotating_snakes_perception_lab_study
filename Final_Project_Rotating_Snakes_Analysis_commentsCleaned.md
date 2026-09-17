Final_Project_Rotating_Snakes_Analysis
================
Ingrid Shen
2026-05-08

# 1. Set working directory

``` r
setwd("C:/Users/ingri/OneDrive/Desktop/JobApps/Rotating_Snakes")
```

# 2. Loading and Prepping The Data

``` r
filenames <- 
  list.files(
    "finaldata", 
    pattern="\\.csv$", 
    full.names=TRUE 
    )

system.time(
  ldf <- lapply(
    filenames, 
    read.csv, 
    header = TRUE, 
    skip = 0, 
    sep = ",") 
  )
```

    ##    user  system elapsed 
    ##    0.05    0.00    0.03

``` r
cat (
  "Number of Files Opened = ", 
  length(ldf)
  )
```

    ## Number of Files Opened =  36

### 2a. Remove unnecessary columns

``` r
for (i in 1:length(ldf)) {
  
  # Drop all unnecessary columns from our files
  ldf[[i]] <- ldf[[i]][, c( "respM.corr","respM.rt", "TestImage",   "FB_Image", "participant",  "CorrAns")]
  
  #  Include a Subject Number!
  ldf[[i]]$Subject <- i  
}
```

### 2b. Convert to a long format dataframe

``` r
mydata <- ldf[[1]]
for (i in 2:length(ldf)) { mydata <- rbind(mydata, ldf[[i]]) }
```

### 2c. Rename columns

``` r
colnames(mydata) <- c('Accuracy', 'Reaction_Time', 'Image', 'FeedbackImg', 'Subj','CorrAns')

# add column for rotation direction
mydata$direction[stringr::str_detect(mydata$Image, "/c")] <- "clockwise"
mydata$direction[stringr::str_detect(mydata$Image, "/cc")] <- "counterclockwise"

# add column for color condition
mydata$color[stringr::str_detect(mydata$Image, "by.png")] <- "blue_yellow"
mydata$color[stringr::str_detect(mydata$Image, "rg.png")] <- "red_green"
mydata$color[stringr::str_detect(mydata$Image, "gs1.png")] <- "greyscale_1"
mydata$color[stringr::str_detect(mydata$Image, "gs2.png")] <- "greyscale_2"
```

### 2d. Remove NAs

``` r
mydata_clean <- na.omit(mydata)
```

### 2e. Convert RT to milliseconds

``` r
mydata_clean$Reaction_Time <- trunc(mydata_clean$Reaction_Time * 1000)
```

### 2f. Remove RT outliers

``` r
mydata_clean_noOut <- 
  subset ( 
    mydata_clean, 
    mydata_clean$Reaction_Time > 249
    )

mydata_clean_noOut <- 
  subset ( 
    mydata_clean_noOut, 
    mydata_clean_noOut$Reaction_Time < 4000
    )
```

# 3. Prep: load packages and set factors

Be sure to install any packages you don’t already have!

``` r
#library(car)
library(psych)
library (plyr)
library(lsr)  #  eta-sqr
#library(rstatix) # useful

# We will need these two to reorganize the data!
library(tidyr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:plyr':
    ## 
    ##     arrange, count, desc, mutate, rename, summarise, summarize

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
# Set factors
options(contrasts=c("contr.sum","contr.poly"))  #  We will need these later on
```

# 4. RT Analysis

### 4a. Filter for correct trials only

``` r
corr_data_noOut <- 
  subset( 
    mydata_clean_noOut, 
    mydata_clean_noOut$Accuracy == 1
    )
```

### 4b. Set Factor Variables

Set the variables “subject” and “color” as factors

``` r
corr_data_noOut$color <- as.factor(corr_data_noOut$color)
corr_data_noOut$Subj <- as.factor(corr_data_noOut$Subj)
```

### 4c. descriptive stats

``` r
corr_data_noOut[,7] <- NULL

n = length(unique(corr_data_noOut$Subj))

RTdata_Long <- plyr::ddply(
  corr_data_noOut,
  c("Subj", "color"),
  summarise,
  MeanRT = mean(Reaction_Time),
  SE_RT = sd(Reaction_Time) / sqrt(n)
)

RTdata_Long_Trim <- RTdata_Long[,c('Subj','color','MeanRT')] 

RTdata_Wide <- RTdata_Long_Trim %>% pivot_wider(names_from = color, values_from = MeanRT, id_cols = Subj)


describe(RTdata_Wide)
```

    ##             vars  n    mean     sd  median trimmed    mad   min     max   range
    ## Subj*          1 36   18.50  10.54   18.50   18.50  13.34   1.0   36.00   35.00
    ## blue_yellow    2 36 1434.27 473.17 1409.70 1417.31 540.33 593.6 2328.25 1734.65
    ## greyscale_1    3 34 1576.69 629.82 1402.83 1533.29 648.76 729.0 3396.50 2667.50
    ## greyscale_2    4 35 1635.58 733.87 1447.50 1551.88 527.36 642.4 3808.00 3165.60
    ## red_green      5 36 1463.27 433.14 1449.08 1446.81 488.96 649.0 2370.67 1721.67
    ##             skew kurtosis     se
    ## Subj*       0.00    -1.30   1.76
    ## blue_yellow 0.32    -0.85  78.86
    ## greyscale_1 0.71    -0.01 108.01
    ## greyscale_2 1.14     1.01 124.05
    ## red_green   0.41    -0.86  72.19

### 4d. ANOVA

``` r
fullRTANOVA <- aov(Reaction_Time ~ color + Error(Subj/color), data= corr_data_noOut)
```

    ## Warning in aov(Reaction_Time ~ color + Error(Subj/color), data =
    ## corr_data_noOut): Error() model is singular

``` r
summary(fullRTANOVA)
```

    ## 
    ## Error: Subj
    ##           Df    Sum Sq Mean Sq F value Pr(>F)
    ## color      3  13183627 4394542   0.861  0.472
    ## Residuals 32 163411703 5106616               
    ## 
    ## Error: Subj:color
    ##            Df   Sum Sq Mean Sq F value Pr(>F)  
    ## color       3  5224523 1741508   3.051 0.0319 *
    ## Residuals 102 58214881  570734                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Error: Within
    ##            Df    Sum Sq Mean Sq F value Pr(>F)
    ## Residuals 888 443778344  499750

### 4e. t-tests

``` r
t.test(RTdata_Wide$blue_yellow, RTdata_Wide$red_green, paired = TRUE)
```

    ## 
    ##  Paired t-test
    ## 
    ## data:  RTdata_Wide$blue_yellow and RTdata_Wide$red_green
    ## t = -0.50808, df = 35, p-value = 0.6146
    ## alternative hypothesis: true mean difference is not equal to 0
    ## 95 percent confidence interval:
    ##  -144.84133   86.85418
    ## sample estimates:
    ## mean difference 
    ##       -28.99357

``` r
t.test(RTdata_Wide$blue_yellow, RTdata_Wide$greyscale_1, paired = TRUE)
```

    ## 
    ##  Paired t-test
    ## 
    ## data:  RTdata_Wide$blue_yellow and RTdata_Wide$greyscale_1
    ## t = -1.9217, df = 33, p-value = 0.06332
    ## alternative hypothesis: true mean difference is not equal to 0
    ## 95 percent confidence interval:
    ##  -337.048864    9.612286
    ## sample estimates:
    ## mean difference 
    ##       -163.7183

``` r
t.test(RTdata_Wide$blue_yellow, RTdata_Wide$greyscale_2, paired = TRUE)
```

    ## 
    ##  Paired t-test
    ## 
    ## data:  RTdata_Wide$blue_yellow and RTdata_Wide$greyscale_2
    ## t = -2.553, df = 34, p-value = 0.01534
    ## alternative hypothesis: true mean difference is not equal to 0
    ## 95 percent confidence interval:
    ##  -355.98127  -40.43258
    ## sample estimates:
    ## mean difference 
    ##       -198.2069

``` r
t.test(RTdata_Wide$red_green, RTdata_Wide$greyscale_1, paired = TRUE)
```

    ## 
    ##  Paired t-test
    ## 
    ## data:  RTdata_Wide$red_green and RTdata_Wide$greyscale_1
    ## t = -1.6619, df = 33, p-value = 0.106
    ## alternative hypothesis: true mean difference is not equal to 0
    ## 95 percent confidence interval:
    ##  -291.65926   29.40385
    ## sample estimates:
    ## mean difference 
    ##       -131.1277

``` r
t.test(RTdata_Wide$red_green, RTdata_Wide$greyscale_2, paired = TRUE)
```

    ## 
    ##  Paired t-test
    ## 
    ## data:  RTdata_Wide$red_green and RTdata_Wide$greyscale_2
    ## t = -1.8081, df = 34, p-value = 0.07945
    ## alternative hypothesis: true mean difference is not equal to 0
    ## 95 percent confidence interval:
    ##  -359.77129   21.00136
    ## sample estimates:
    ## mean difference 
    ##        -169.385

``` r
t.test(RTdata_Wide$greyscale_1, RTdata_Wide$greyscale_2, paired = TRUE)
```

    ## 
    ##  Paired t-test
    ## 
    ## data:  RTdata_Wide$greyscale_1 and RTdata_Wide$greyscale_2
    ## t = 0.060242, df = 33, p-value = 0.9523
    ## alternative hypothesis: true mean difference is not equal to 0
    ## 95 percent confidence interval:
    ##  -163.8420  173.8408
    ## sample estimates:
    ## mean difference 
    ##        4.999393

# 5. Accuracy Analysis

### 5a. Make a copy of mydata_clean_noOut

``` r
mydata_acc <- mydata_clean_noOut
```

### 5b. Set factor variables

Set the variables “subject” and “color” as factors

``` r
mydata_acc$color <- as.factor(mydata_acc$color)
mydata_acc$Subj <- as.factor(mydata_acc$Subj)
```

### 5c. Descriptive Stats

``` r
mydata_acc[,7] <- NULL

n = length(unique(mydata_acc$Subj))

ACCdata_Long <- plyr::ddply(
  mydata_acc,
  c("Subj", "color"),
  summarise,
  MeanACC = mean(Accuracy),
  SE_RT = sd(Accuracy) / sqrt(n)
)

ACCdata_Long_Trim <- ACCdata_Long[,c('Subj','color','MeanACC')] 

ACCdata_Wide <- ACCdata_Long_Trim %>% pivot_wider(names_from = color, values_from = MeanACC, id_cols = Subj)


describe(ACCdata_Wide)
```

    ##             vars  n  mean    sd median trimmed   mad  min max range  skew
    ## Subj*          1 36 18.50 10.54  18.50   18.50 13.34 1.00  36 35.00  0.00
    ## blue_yellow    2 36  0.95  0.14   1.00    0.98  0.00 0.33   1  0.67 -3.00
    ## greyscale_1    3 36  0.63  0.33   0.73    0.65  0.40 0.00   1  1.00 -0.44
    ## greyscale_2    4 36  0.76  0.29   0.88    0.79  0.19 0.00   1  1.00 -0.93
    ## red_green      5 36  0.91  0.16   1.00    0.94  0.00 0.29   1  0.71 -2.20
    ##             kurtosis   se
    ## Subj*          -1.30 1.76
    ## blue_yellow     8.75 0.02
    ## greyscale_1    -1.22 0.05
    ## greyscale_2    -0.35 0.05
    ## red_green       4.72 0.03

### 5d. ANOVA

``` r
fullAccANOVA <- aov(Accuracy ~ color + Error(Subj/color), data= mydata_acc) 
summary(fullAccANOVA)
```

    ## 
    ## Error: Subj
    ##           Df Sum Sq Mean Sq F value Pr(>F)
    ## color      3   1.66  0.5531   0.483  0.696
    ## Residuals 32  36.62  1.1445               
    ## 
    ## Error: Subj:color
    ##            Df Sum Sq Mean Sq F value   Pr(>F)    
    ## color       3  17.35   5.783   20.74 1.25e-10 ***
    ## Residuals 105  29.28   0.279                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Error: Within
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Residuals 1103  94.98 0.08611

### 5e. t-tests

``` r
t.test(ACCdata_Wide$blue_yellow, ACCdata_Wide$red_green, paired = TRUE)
```

    ## 
    ##  Paired t-test
    ## 
    ## data:  ACCdata_Wide$blue_yellow and ACCdata_Wide$red_green
    ## t = 1.4243, df = 35, p-value = 0.1632
    ## alternative hypothesis: true mean difference is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.01639737  0.09349172
    ## sample estimates:
    ## mean difference 
    ##      0.03854718

``` r
t.test(ACCdata_Wide$blue_yellow, ACCdata_Wide$greyscale_1, paired = TRUE)
```

    ## 
    ##  Paired t-test
    ## 
    ## data:  ACCdata_Wide$blue_yellow and ACCdata_Wide$greyscale_1
    ## t = 6.0552, df = 35, p-value = 6.531e-07
    ## alternative hypothesis: true mean difference is not equal to 0
    ## 95 percent confidence interval:
    ##  0.2086157 0.4190519
    ## sample estimates:
    ## mean difference 
    ##       0.3138338

``` r
t.test(ACCdata_Wide$blue_yellow, ACCdata_Wide$greyscale_2, paired = TRUE)
```

    ## 
    ##  Paired t-test
    ## 
    ## data:  ACCdata_Wide$blue_yellow and ACCdata_Wide$greyscale_2
    ## t = 4.6204, df = 35, p-value = 5.021e-05
    ## alternative hypothesis: true mean difference is not equal to 0
    ## 95 percent confidence interval:
    ##  0.1056416 0.2712323
    ## sample estimates:
    ## mean difference 
    ##       0.1884369

``` r
t.test(ACCdata_Wide$red_green, ACCdata_Wide$greyscale_1, paired = TRUE)
```

    ## 
    ##  Paired t-test
    ## 
    ## data:  ACCdata_Wide$red_green and ACCdata_Wide$greyscale_1
    ## t = 5.2537, df = 35, p-value = 7.444e-06
    ## alternative hypothesis: true mean difference is not equal to 0
    ## 95 percent confidence interval:
    ##  0.1689114 0.3816618
    ## sample estimates:
    ## mean difference 
    ##       0.2752866

``` r
t.test(ACCdata_Wide$red_green, ACCdata_Wide$greyscale_2, paired = TRUE)
```

    ## 
    ##  Paired t-test
    ## 
    ## data:  ACCdata_Wide$red_green and ACCdata_Wide$greyscale_2
    ## t = 3.1552, df = 35, p-value = 0.003289
    ## alternative hypothesis: true mean difference is not equal to 0
    ## 95 percent confidence interval:
    ##  0.05344858 0.24633096
    ## sample estimates:
    ## mean difference 
    ##       0.1498898

``` r
t.test(ACCdata_Wide$greyscale_1, ACCdata_Wide$greyscale_2, paired = TRUE)
```

    ## 
    ##  Paired t-test
    ## 
    ## data:  ACCdata_Wide$greyscale_1 and ACCdata_Wide$greyscale_2
    ## t = -4.0412, df = 35, p-value = 0.0002771
    ## alternative hypothesis: true mean difference is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.18839008 -0.06240357
    ## sample estimates:
    ## mean difference 
    ##      -0.1253968

# Graphs

``` r
library(ggplot2)
```

    ## 
    ## Attaching package: 'ggplot2'

    ## The following objects are masked from 'package:psych':
    ## 
    ##     %+%, alpha

``` r
p_rt <- 
  ggplot(
    RTdata_Long_Trim, 
    aes(
      x = color, 
      y = MeanRT,  
      fill = color) 
    ) + 
  geom_boxplot() + 
  labs( 
    x = "Color Combo" ,
    y ="RT (msec)",
    title = "RT ~ Color Condition"
    ) 


p_rt
```

![](Final_Project_Rotating_Snakes_Analysis_commentsCleaned_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
p_a <- 
  ggplot(
    ACCdata_Long_Trim, 
    aes(
      x = color, 
      y = MeanACC,  
      fill = color) 
    ) + 
  geom_boxplot() + 
  labs( 
    x = "Color Combo" ,
    y ="Accuracy",
    title = "Accuracy ~ Color Conditions"
    ) 


p_a
```

![](Final_Project_Rotating_Snakes_Analysis_commentsCleaned_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->
