---
title: "Mini Project 1"
subtitle: "Analysis of US Births from 2000 to 2014"
output: 
  html_document:
    keep_md: true
    toc: true 
    toc_float: true
---

Load Libraries

```r
library(tidyverse)
library(scales)
```

Read in data

```r
us_births <- read_csv("https://raw.githubusercontent.com/reisanar/datasets/master/us_births_00_14.csv", col_types = cols())
```

Check for null values

```r
sum(is.na.data.frame(us_births))
```

```
## [1] 0
```

Summary statistics 

```r
summary(us_births)
```

```
##       year          month        date_of_month        date           
##  Min.   :2000   Min.   : 1.000   Min.   : 1.00   Min.   :2000-01-01  
##  1st Qu.:2003   1st Qu.: 4.000   1st Qu.: 8.00   1st Qu.:2003-10-01  
##  Median :2007   Median : 7.000   Median :16.00   Median :2007-07-02  
##  Mean   :2007   Mean   : 6.523   Mean   :15.73   Mean   :2007-07-02  
##  3rd Qu.:2011   3rd Qu.:10.000   3rd Qu.:23.00   3rd Qu.:2011-04-01  
##  Max.   :2014   Max.   :12.000   Max.   :31.00   Max.   :2014-12-31  
##  day_of_week            births     
##  Length:5479        Min.   : 5728  
##  Class :character   1st Qu.: 8740  
##  Mode  :character   Median :12343  
##                     Mean   :11350  
##                     3rd Qu.:13082  
##                     Max.   :16081
```

Glimpse of columns

```r
glimpse(us_births)
```

```
## Rows: 5,479
## Columns: 6
## $ year          <dbl> 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 20~
## $ month         <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,~
## $ date_of_month <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 1~
## $ date          <date> 2000-01-01, 2000-01-02, 2000-01-03, 2000-01-04, 2000-01~
## $ day_of_week   <chr> "Sat", "Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat~
## $ births        <dbl> 9083, 8006, 11363, 13032, 12558, 12466, 12516, 8934, 794~
```

Messing around with data

```r
us_births %>% 
  filter(year==2000) %>% 
  summarize(mean(births))
```

```
## # A tibble: 1 x 1
##   `mean(births)`
##            <dbl>
## 1         11338.
```



Very simple first look at births over the years - Honestly, doesn't seem to be a crazy trend here. It does slightly look like there's been a dip since 2007, which could be in part of the 2007-08 financial crisis if were to make a big leap, but can't really know for sure. It has seemed to mellow out in recent years though. Considering population growth however, the stagnation in births could very well be a decrease per population.

```r
ggplot(data=us_births, aes(x=year,y=births)) +
  geom_bar(stat="identity")+
  scale_y_continuous()
```

![](mantini_us_births_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


Taking a look at a breakdown for births by days of the week show that much fewer births happen on the weekends, an anomaly we'll have to search in to at the end of our research.

```r
total_births_weekday <- us_births %>% 
  group_by(day_of_week) %>% 
  summarize(total = sum(births))

ggplot(data = total_births_weekday,
       mapping = aes(x = day_of_week, y = total, fill = day_of_week)) +
  geom_col() +
  guides(fill = "none")
```

![](mantini_us_births_files/figure-html/unnamed-chunk-8-1.png)<!-- -->


Taking another look at how births compare across days of the weeks in given months - can get the gist that the summer months seem good, but will need to create a better visualization to really demonstrate it.Some anomalies include Mondays in May, which is interesting.

```r
ggplot(us_births, aes(x=day_of_week,y=month,fill=births)) +
  geom_tile()+
  scale_fill_gradient(low = "white", high = "red")
```

![](mantini_us_births_files/figure-html/unnamed-chunk-9-1.png)<!-- -->


At this point I kind of realized that it would make a lot of sense to label the months and days to make for better visualizations.

Data Wrangling/Appending to Dataset

```r
month_names <- c("January", "February", "March", "April", "May", "June", "July",
                 "August", "September", "October", "November", "December")

day_names <- c("Monday", "Tuesday", "Wednesday", 
               "Thursday", "Friday", "Saturday", "Sunday")

us_births_labeled <- us_births %>% 
  # Making month an ordered factor, using the month_name list as labels
  mutate(month = factor(month, labels = month_names, ordered = TRUE)) %>% 
  mutate(day_of_week = factor(day_of_week, labels = day_names, ordered = TRUE),
         date_of_month_categorical = factor(date_of_month))

head(us_births_labeled)
```

```
## # A tibble: 6 x 7
##    year month   date_of_month date       day_of_week births date_of_month_categ~
##   <dbl> <ord>           <dbl> <date>     <ord>        <dbl> <fct>               
## 1  2000 January             1 2000-01-01 Wednesday     9083 1                   
## 2  2000 January             2 2000-01-02 Thursday      8006 2                   
## 3  2000 January             3 2000-01-03 Tuesday      11363 3                   
## 4  2000 January             4 2000-01-04 Saturday     13032 4                   
## 5  2000 January             5 2000-01-05 Sunday       12558 5                   
## 6  2000 January             6 2000-01-06 Friday       12466 6
```


Creating a heatmap for average births by day of the month for given months

```r
avg_births_month_day <- us_births_labeled %>% 
  group_by(month, date_of_month_categorical) %>% 
  summarize(avg_births = mean(births))
```

```
## `summarise()` has grouped output by 'month'. You can override using the `.groups` argument.
```

```r
ggplot(data = avg_births_month_day,
       # using fct(rev) to reverse having december at top
       mapping = aes(x = date_of_month_categorical, y = fct_rev(month), fill = avg_births)) +
  geom_tile() +
  #add different color palette
  scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn")) +
  # Add labels
  labs(x = "Day of the month", y = "Month",
       title = "Average births per day from 2000 to 2014",
       fill = "Average births") +
  theme_dark()
```

![](mantini_us_births_files/figure-html/unnamed-chunk-11-1.png)<!-- -->
 
A few interesting things of note here:

-Some months don't have 31 days (or even 30!)
-Some notable holidays have a much notably smaller; January 1st, July 4th, and Christmas Eve and Christmas day on the 24th/25th of december. Also, the range of days that Thanksgiving can be on is from November 22nd to 28th in any given year. This range of days has notably lower births.
-Summer months have notably higher number of births. While beyond the scope of this project, searches show that this is due to seasons and weather conditions resulting in more babies being conceived in cold winter months, which would project births being in the summer.



Distribution of number of births

```r
ggplot(us_births_labeled, aes(x=births,y=..density..)) +
  geom_histogram(color="blue",fill="white") +
  geom_vline(aes(xintercept=mean(births)),
             color="red", linetype="dashed", size=1) +
  geom_density(alpha=.2, fill="green")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](mantini_us_births_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

Based off of the information we've gathered, this Multimodal Distribution can be disected. It is very likely that the larger mode is on the average weekday, while the smaller mode are births on the weekends, as this gap is pretty similar to the plot prior showing births by day of the week. Furthermore, the variation between these is likely the variance in month/day of the month, in which the more populous months 


#Conclusions:

Realized a bit too late that I should add labeles to months and days, so for future work I would take more advantage of that and create visualizations with those. Original charts I planned to include were the heatmap and distribution histogram, in addition to a correlation matrix that I didn't get around to preprocessing well enough for. I tried to include principles of data visualization including decent use of color and storytelling.

To reiterate notes from previous visualizations:


- Found that there are much fewer births on the weekend as a result of scheduled births with c-sections and induced labors. Hospitals are less staffed and also won't charge as much on the weekend as opposed to normal operating hours, so this makes sense.

- For similar reasons I imagine, there were significantly fewer births on holidays. Less people would want to schedule on a holiday, and the hospital would again have less availability.

- Summer months have notably higher number of births. While beyond the scope of this project, searches show that this is due to seasons and weather conditions resulting in more babies being conceived in cold winter months, which would project births being in the summer.

- It does slightly look like there's been a dip in births since 2007, which could be in part of the 2007-08 financial crisis if were to make a big leap, but can't really know for sure. It has seemed to mellow out in recent years though. Considering population growth however, the stagnation in births could very well be a decrease per population.

