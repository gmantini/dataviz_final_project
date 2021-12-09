---
title: "Mini-Project 2"
subtitle: "Nobel Laureate Winners and Publications"
output: 
  html_document:
    keep_md: true
    toc: true
    toc_float: true
---

# Introduction

Was browsing through some of the datasets that had been featured in TidyTuesday, and the Nobel Laureate data caught my eye. I had little to no prior knowledge on Nobel Laureate winner data, so I was curious to learn basic information such as the number of winners across different fields/regions. Furthermore, it seemed like a fitting data set that would be able to incorporate relevant visualizations that were asked for. I.e., the spatial visualization being able to be used for showing distribution of winners from around the world. For the scope of this work, I decided to focus on the nobel winners data and not the data that included all of the separate publications from nobel winners.

# Libraries, Data Importing/Preprocessing

Load Libraries

```r
library(tidyverse) #mostly for ggplot
```

```
## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --
```

```
## v ggplot2 3.3.5     v purrr   0.3.4
## v tibble  3.1.4     v dplyr   1.0.7
## v tidyr   1.1.3     v stringr 1.4.0
## v readr   2.0.1     v forcats 0.5.1
```

```
## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(lubridate) #working with dates/times
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
library(plotly) #interactive plots
```

```
## 
## Attaching package: 'plotly'
```

```
## The following object is masked from 'package:ggplot2':
## 
##     last_plot
```

```
## The following object is masked from 'package:stats':
## 
##     filter
```

```
## The following object is masked from 'package:graphics':
## 
##     layout
```

```r
library(sf) #world shapefile
```

```
## Linking to GEOS 3.9.1, GDAL 3.2.1, PROJ 7.2.1
```

Set theme

```r
theme_set(theme_bw())
```

Read in data, add column to sort by decade for cleaner plots

```r
nobel_winners <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winners.csv") %>% #read_csv is significantly faster here than read.csv
  distinct(full_name, prize_year, category, .keep_all = TRUE) %>%
  mutate(decade = 10 * (prize_year %/% 10),
         age = prize_year - year(birth_date))
```

Glimpse at Nobel Winners 

```r
glimpse(nobel_winners)
```

```
## Rows: 911
## Columns: 20
## $ prize_year           <dbl> 1901, 1901, 1901, 1901, 1901, 1901, 1902, 1902, 1~
## $ category             <chr> "Chemistry", "Literature", "Medicine", "Peace", "~
## $ prize                <chr> "The Nobel Prize in Chemistry 1901", "The Nobel P~
## $ motivation           <chr> "\"in recognition of the extraordinary services h~
## $ prize_share          <chr> "1/1", "1/1", "1/1", "1/2", "1/2", "1/1", "1/1", ~
## $ laureate_id          <dbl> 160, 569, 293, 462, 463, 1, 161, 571, 294, 464, 4~
## $ laureate_type        <chr> "Individual", "Individual", "Individual", "Indivi~
## $ full_name            <chr> "Jacobus Henricus van 't Hoff", "Sully Prudhomme"~
## $ birth_date           <date> 1852-08-30, 1839-03-16, 1854-03-15, 1828-05-08, ~
## $ birth_city           <chr> "Rotterdam", "Paris", "Hansdorf (Lawice)", "Genev~
## $ birth_country        <chr> "Netherlands", "France", "Prussia (Poland)", "Swi~
## $ gender               <chr> "Male", "Male", "Male", "Male", "Male", "Male", "~
## $ organization_name    <chr> "Berlin University", NA, "Marburg University", NA~
## $ organization_city    <chr> "Berlin", NA, "Marburg", NA, NA, "Munich", "Berli~
## $ organization_country <chr> "Germany", NA, "Germany", NA, NA, "Germany", "Ger~
## $ death_date           <date> 1911-03-01, 1907-09-07, 1917-03-31, 1910-10-30, ~
## $ death_city           <chr> "Berlin", "Châtenay", "Marburg", "Heiden", "Paris~
## $ death_country        <chr> "Germany", "France", "Germany", "Switzerland", "F~
## $ decade               <dbl> 1900, 1900, 1900, 1900, 1900, 1900, 1900, 1900, 1~
## $ age                  <dbl> 49, 62, 47, 73, 79, 56, 50, 85, 45, 69, 59, 49, 3~
```


# Age Analysis

Linear fit of prize year as a function of age

```r
ggplot(nobel_winners, aes(x = prize_year, y = age)) +
  geom_point() +
  geom_smooth(method = "lm", 
              formula = "y ~ x") + 
  theme_minimal()
```

![](mantini_nobel_analysis_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
Get the jist that prize recipients have been older over time at the time of receiving. Let's take a closer look:

How has age of Nobel Prize recipients changed over time?

```r
nobel_winners %>%
  filter(!is.na(age)) %>%
  group_by(decade) %>%
  summarize(average_age = mean(age),
            median_age = median(age)) %>%
  ggplot(aes(decade, average_age)) +
  geom_line()
```

![](mantini_nobel_analysis_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
Definitely a noticeable spike since the mid 1900's.

How about over different categories?

Boxplot view of age per category

```r
nobel_categories <- nobel_winners %>%
  mutate(category = fct_reorder(category, age, median, na.rm = TRUE)) %>%
  ggplot(aes(category, age)) +
  geom_boxplot() +
  coord_flip()

ggplotly(nobel_categories)
```

```{=html}
<div id="htmlwidget-bdc2d22a146d842a9a13" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-bdc2d22a146d842a9a13">{"x":{"data":[{"x":[49,51,54,37,54,62,50,70,74,48,67,82,50,44,70,47,44,39,73,52,52,65,54,64,56,52,70,45,51,72,49,54,63,44,78,57,47,47,64,46,52,36,77,73,56,75,52,67,58,59,56,41,68,56,59,61,50,63,38,63,65,60,44,59,40,42,71,58,41,58,48,56,79,69,47,63,38,50,57,46,45,60,43,52,75,69,58,68,57,69,65,50,48,80,53,74,69,56,40,50,53,57,44,49,55,38,80,61,50,60,54,61,61,54,84,70,60,53,62,69,70,56,72,65,83,66,63,42,68,58,43,64,55,61,40,79,68,54,72,63,57,67,62,52,42,54,75,48,54,57,60,45,48,64,60,67,48,35,69,null,42,64,83,59,69,50,79,64,47,54,52,85,67,73,51,49,73,52,63,65,69,56,76,71,68,62,67,62,83,76,76,51,60,61,46,52,71,56,42,69,60,59,70,74,52,74,61,71,60,85,59,51,57,52,69,60,72,73,60,78,45,44,77,80,67,78,75,59,74,60,57,68,68,62,79,58,58,67,46,67,71,79,68,53,69,74,56,75,63,69,49,58,55,68,82,72,72,80,54,69,48,75,54,68,47,56,79,63,67,60,56,78,77,69,50,62,57,55,75,73,63,72,61,73,88,55,55,74,68,75,44,77,43,38,76,59,50,75,42,52,50,41,64,57,46,68,47,62,56,77,33,85,45,39,55,47,32,68,62,49,56,71,49,47,55,61,71,45,54,80,54,64,39,55,38,56,49,70,55,46,58,61,46,64,51,52,48,63,57,64,75,49,70,64,51,82,61,60,56,53,46,69,57,76,65,70,61,62,55,67,48,56,63,65,49,50,52,49,81,60,64,72,47,47,50,45,46,46,46,41,50,87,36,44,44,52,53,75,53,51,54,64,70,45,64,49,58,52,43,82,48,85,62,77,68,61,37,61,75,34,67,57,74,57,66,38,49,65,51,52,60,70,63,71,39,66,62,51,54,63,41,57,57,53,62,61,55,57,72,63,54,57,66,85,47,70,48,54,74,68,46,63,48,70,63,73,49,61,55,63,58,38,59,58,66,53,87,53,52,62,77,56,55,48,53,61,53,60,83,68,43,50,54,55,60,79,70,71,62,74,60,49,57,79,60,null,71,33,73,75,72,63,null,59,69,72,46,75,40,70,73,46,48,null,62,73,54,61,63,56,null,null,null,47,65,64,69,80,71,57,59,69,61,null,null,69,null,null,74,71,47,null,77,48,81,55,67,69,59,48,47,73,58,71,79,73,73,70,null,null,71,61,56,54,null,null,58,46,46,null,86,75,78,null,null,65,64,80,52,32,48,null,81,56,60,62,50,32,73,35,64,null,71,60,null,64,63,null,54,58,87,62,64,61,69,null,59,null,66,17,null,39,33,65,null,53,61,48,null,60,73,46,42,44,55,49,60,43,57,55,36,61,56,59,42,54,60,56,38,55,31,47,44,37,42,48,53,40,31,48,77,54,54,58,53,59,54,35,32,39,45,59,61,64,37,54,43,43,48,45,49,50,68,42,50,35,49,56,25,47,34,56,84,61,55,63,78,64,59,35,46,64,44,58,53,46,64,45,31,37,66,56,65,51,59,40,70,72,77,42,65,67,76,54,49,37,61,72,53,38,59,40,50,42,45,68,49,60,71,46,60,47,73,51,47,68,85,68,49,47,60,62,67,40,47,57,87,65,52,44,33,42,69,72,40,38,64,63,63,43,79,62,64,87,64,42,48,56,49,37,59,68,45,53,72,76,71,82,50,41,73,61,85,57,81,84,75,63,50,36,74,66,79,46,46,56,72,54,63,80,null,39,53,44,31,76,48,65,68,51,49,68,76,42,71,88,52,44,80,53,55,35,60,64,63,62,77,76,63,66,81,90,57,67,70,68,69,66,64,67,67,55,63,63,58,61,53,75,77,76,61,78,51,70,64,77,71,61,67,63,56,69,73,71,62,67,62,74,66,61,56,74,61,64,null,58,55,56,56,78,65,71,70,70,75,84,89,65,68,75,62,67,68,68,67,73,74,60,82,60,61],"y":[2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5],"hoverinfo":"x","type":"box","fillcolor":"rgba(255,255,255,1)","marker":{"opacity":null,"outliercolor":"rgba(0,0,0,1)","line":{"width":1.88976377952756,"color":"rgba(0,0,0,1)"},"size":5.66929133858268},"line":{"color":"rgba(51,51,51,1)","width":1.88976377952756},"showlegend":false,"xaxis":"x","yaxis":"y","orientation":"h","frame":null}],"layout":{"margin":{"t":26.2283105022831,"r":7.30593607305936,"b":40.1826484018265,"l":84.0182648401827},"plot_bgcolor":"rgba(255,255,255,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[13.35,93.65],"tickmode":"array","ticktext":["25","50","75"],"tickvals":[25,50,75],"categoryorder":"array","categoryarray":["25","50","75"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":{"text":"age","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.4,6.6],"tickmode":"array","ticktext":["Physics","Chemistry","Medicine","Peace","Economics","Literature"],"tickvals":[1,2,3,4,5,6],"categoryorder":"array","categoryarray":["Physics","Chemistry","Medicine","Peace","Economics","Literature"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":{"text":"category","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":"transparent","line":{"color":"rgba(51,51,51,1)","width":0.66417600664176,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":false,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"1ba03e2133d":{"x":{},"y":{},"type":"box"}},"cur_data":"1ba03e2133d","visdat":{"1ba03e2133d":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```
Get to see a couple interesting outliers here.

```r
nobel_winners %>% 
  filter(age == 17 | age == 90) %>% 
  arrange(full_name) %>% 
  select(full_name,age,prize_year,category)
```

```
## # A tibble: 2 x 4
##   full_name          age prize_year category 
##   <chr>            <dbl>      <dbl> <chr>    
## 1 Leonid Hurwicz      90       2007 Economics
## 2 Malala Yousafzai    17       2014 Peace
```

Change of age over time across categories

```r
nobel_age <- nobel_winners %>%
  filter(!is.na(age)) %>%
  group_by(decade, category) %>%
  summarize(average_age = mean(age),
            median_age = median(age)) %>%
  ggplot(aes(decade, average_age, color = category)) +
  geom_line()

ggplotly(nobel_age)
```

```{=html}
<div id="htmlwidget-8c3e3bb1563f621f1731" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-8c3e3bb1563f621f1731">{"x":{"data":[{"x":[1900,1910,1920,1930,1940,1950,1960,1970,1980,1990,2000,2010],"y":[51,49,52.3,46,54.4444444444444,53,55.8,61.9333333333333,56.2380952380952,63.1111111111111,65.2916666666667,69.1666666666667],"text":["decade: 1900<br />average_age: 51.00000<br />category: Chemistry","decade: 1910<br />average_age: 49.00000<br />category: Chemistry","decade: 1920<br />average_age: 52.30000<br />category: Chemistry","decade: 1930<br />average_age: 46.00000<br />category: Chemistry","decade: 1940<br />average_age: 54.44444<br />category: Chemistry","decade: 1950<br />average_age: 53.00000<br />category: Chemistry","decade: 1960<br />average_age: 55.80000<br />category: Chemistry","decade: 1970<br />average_age: 61.93333<br />category: Chemistry","decade: 1980<br />average_age: 56.23810<br />category: Chemistry","decade: 1990<br />average_age: 63.11111<br />category: Chemistry","decade: 2000<br />average_age: 65.29167<br />category: Chemistry","decade: 2010<br />average_age: 69.16667<br />category: Chemistry"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(248,118,109,1)","dash":"solid"},"hoveron":"points","name":"Chemistry","legendgroup":"Chemistry","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1960,1970,1980,1990,2000,2010],"y":[70,67,67.9,65.4705882352941,67.3157894736842,68.4285714285714],"text":["decade: 1960<br />average_age: 70.00000<br />category: Economics","decade: 1970<br />average_age: 67.00000<br />category: Economics","decade: 1980<br />average_age: 67.90000<br />category: Economics","decade: 1990<br />average_age: 65.47059<br />category: Economics","decade: 2000<br />average_age: 67.31579<br />category: Economics","decade: 2010<br />average_age: 68.42857<br />category: Economics"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(183,159,0,1)","dash":"solid"},"hoveron":"points","name":"Economics","legendgroup":"Economics","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1900,1910,1920,1930,1940,1950,1960,1970,1980,1990,2000,2010],"y":[64.9,59,60.1,56.4444444444444,64.3333333333333,63.7,67,67.2727272727273,67.6,67.5,66.4,72],"text":["decade: 1900<br />average_age: 64.90000<br />category: Literature","decade: 1910<br />average_age: 59.00000<br />category: Literature","decade: 1920<br />average_age: 60.10000<br />category: Literature","decade: 1930<br />average_age: 56.44444<br />category: Literature","decade: 1940<br />average_age: 64.33333<br />category: Literature","decade: 1950<br />average_age: 63.70000<br />category: Literature","decade: 1960<br />average_age: 67.00000<br />category: Literature","decade: 1970<br />average_age: 67.27273<br />category: Literature","decade: 1980<br />average_age: 67.60000<br />category: Literature","decade: 1990<br />average_age: 67.50000<br />category: Literature","decade: 2000<br />average_age: 66.40000<br />category: Literature","decade: 2010<br />average_age: 72.00000<br />category: Literature"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(0,186,56,1)","dash":"solid"},"hoveron":"points","name":"Literature","legendgroup":"Literature","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1900,1910,1920,1930,1940,1950,1960,1970,1980,1990,2000,2010],"y":[56,49.1666666666667,53.9090909090909,54.7142857142857,56,51.8,55,56.72,60.7391304347826,60.45,63.5384615384615,68.1875],"text":["decade: 1900<br />average_age: 56.00000<br />category: Medicine","decade: 1910<br />average_age: 49.16667<br />category: Medicine","decade: 1920<br />average_age: 53.90909<br />category: Medicine","decade: 1930<br />average_age: 54.71429<br />category: Medicine","decade: 1940<br />average_age: 56.00000<br />category: Medicine","decade: 1950<br />average_age: 51.80000<br />category: Medicine","decade: 1960<br />average_age: 55.00000<br />category: Medicine","decade: 1970<br />average_age: 56.72000<br />category: Medicine","decade: 1980<br />average_age: 60.73913<br />category: Medicine","decade: 1990<br />average_age: 60.45000<br />category: Medicine","decade: 2000<br />average_age: 63.53846<br />category: Medicine","decade: 2010<br />average_age: 68.18750<br />category: Medicine"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(0,191,196,1)","dash":"solid"},"hoveron":"points","name":"Medicine","legendgroup":"Medicine","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1900,1910,1920,1930,1940,1950,1960,1970,1980,1990,2000,2010],"y":[67.3076923076923,61.8,64.0909090909091,64.125,75.75,63.7142857142857,58.25,56.8333333333333,56.375,58.7142857142857,64.3,48.7142857142857],"text":["decade: 1900<br />average_age: 67.30769<br />category: Peace","decade: 1910<br />average_age: 61.80000<br />category: Peace","decade: 1920<br />average_age: 64.09091<br />category: Peace","decade: 1930<br />average_age: 64.12500<br />category: Peace","decade: 1940<br />average_age: 75.75000<br />category: Peace","decade: 1950<br />average_age: 63.71429<br />category: Peace","decade: 1960<br />average_age: 58.25000<br />category: Peace","decade: 1970<br />average_age: 56.83333<br />category: Peace","decade: 1980<br />average_age: 56.37500<br />category: Peace","decade: 1990<br />average_age: 58.71429<br />category: Peace","decade: 2000<br />average_age: 64.30000<br />category: Peace","decade: 2010<br />average_age: 48.71429<br />category: Peace"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(97,156,255,1)","dash":"solid"},"hoveron":"points","name":"Peace","legendgroup":"Peace","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1900,1910,1920,1930,1940,1950,1960,1970,1980,1990,2000,2010],"y":[49.2307692307692,48.1,45.5833333333333,41.2727272727273,51.1428571428571,49.8,50.2352941176471,53.72,59.4090909090909,60.0454545454545,68.5,63.875],"text":["decade: 1900<br />average_age: 49.23077<br />category: Physics","decade: 1910<br />average_age: 48.10000<br />category: Physics","decade: 1920<br />average_age: 45.58333<br />category: Physics","decade: 1930<br />average_age: 41.27273<br />category: Physics","decade: 1940<br />average_age: 51.14286<br />category: Physics","decade: 1950<br />average_age: 49.80000<br />category: Physics","decade: 1960<br />average_age: 50.23529<br />category: Physics","decade: 1970<br />average_age: 53.72000<br />category: Physics","decade: 1980<br />average_age: 59.40909<br />category: Physics","decade: 1990<br />average_age: 60.04545<br />category: Physics","decade: 2000<br />average_age: 68.50000<br />category: Physics","decade: 2010<br />average_age: 63.87500<br />category: Physics"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(245,100,227,1)","dash":"solid"},"hoveron":"points","name":"Physics","legendgroup":"Physics","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":26.2283105022831,"r":7.30593607305936,"b":40.1826484018265,"l":37.2602739726027},"plot_bgcolor":"rgba(255,255,255,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[1894.5,2015.5],"tickmode":"array","ticktext":["1920","1950","1980","2010"],"tickvals":[1920,1950,1980,2010],"categoryorder":"array","categoryarray":["1920","1950","1980","2010"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":{"text":"decade","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[39.5488636363636,77.4738636363636],"tickmode":"array","ticktext":["40","50","60","70"],"tickvals":[40,50,60,70],"categoryorder":"array","categoryarray":["40","50","60","70"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":{"text":"average_age","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":"transparent","line":{"color":"rgba(51,51,51,1)","width":0.66417600664176,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":true,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895},"title":{"text":"category","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"1ba02786baa":{"x":{},"y":{},"colour":{},"type":"scatter"}},"cur_data":"1ba02786baa","visdat":{"1ba02786baa":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```
Average age across the board has increased for the most part, especially for the "hard sciences". Literature is the oldest, and consistently has been for a while. Peace has by far been the most volatile, notably skewed by a few data points. For the 2010's, our data is skewed by having the 17 year old Malala Yousafzai win in 2014, in addition for data being only collected up to 2016 for this dataset.

Going back to plotting age as a function of prize_year: Adding categories with trend lines.

```r
ggplot(data = nobel_winners, aes(x = prize_year, y = age))+
    geom_point()+
    geom_smooth()+
    facet_wrap(~ category)
```

![](mantini_nobel_analysis_files/figure-html/unnamed-chunk-10-1.png)<!-- -->
Can confirm that the sciences have taken a noticeable trend upwards, while Peace has actually had a downward trend. Economics has had little change, with Literature having a slight trend upward.


# Regional Analysis

Now taking a look at distribution of birth country for Nobel Prize winners: 

```r
nobel_winners %>%
  filter(!is.na(birth_country)) %>%
  count(birth_country = fct_lump(birth_country, 25),
        sort = TRUE) %>%
  mutate(birth_country = fct_reorder(birth_country, n)) %>%
  ggplot(aes(birth_country, n)) +
  geom_col() +
  #facet_wrap(~ category) +
  coord_flip()
```

![](mantini_nobel_analysis_files/figure-html/unnamed-chunk-11-1.png)<!-- -->
The United States dominates the rest of the field, having well over 3 times the number of recipients as the next highest country (U.K.)

How does birth country look across different categories?

```r
nobel_regions <- nobel_winners %>%
  filter(!is.na(birth_country)) %>%
  count(birth_country = fct_lump(birth_country, 10),
        category,
        sort = TRUE) %>%
  mutate(birth_country = fct_reorder(birth_country, n)) %>%
  ggplot(aes(birth_country, n, fill = category)) +
  geom_col() +
  facet_wrap(~ category) +
  coord_flip()

ggplotly(nobel_regions)
```

```{=html}
<div id="htmlwidget-5f8fc8d014d6d66ef631" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-5f8fc8d014d6d66ef631">{"x":{"data":[{"orientation":"h","width":[0.899999999999999,0.899999999999999,0.899999999999999,0.899999999999999,0.9,0.9,0.9,0.9,0.9,0.9,0.9],"base":[0,0,0,0,0,0,0,0,0,0,0],"x":[52,51,22,19,9,6,4,4,4,3,1],"y":[10,11,9,8,7,4,3,1,5,2,6],"text":["birth_country: United States of America<br />n: 52<br />category: Chemistry","birth_country: Other<br />n: 51<br />category: Chemistry","birth_country: United Kingdom<br />n: 22<br />category: Chemistry","birth_country: Germany<br />n: 19<br />category: Chemistry","birth_country: France<br />n:  9<br />category: Chemistry","birth_country: Japan<br />n:  6<br />category: Chemistry","birth_country: Canada<br />n:  4<br />category: Chemistry","birth_country: Netherlands<br />n:  4<br />category: Chemistry","birth_country: Sweden<br />n:  4<br />category: Chemistry","birth_country: Russia<br />n:  3<br />category: Chemistry","birth_country: Italy<br />n:  1<br />category: Chemistry"],"type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(248,118,109,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"Chemistry","legendgroup":"Chemistry","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"h","width":[0.899999999999999,0.899999999999999,0.899999999999999,0.9,0.9,0.9,0.9,0.9,0.899999999999999,0.9],"base":[0,0,0,0,0,0,0,0,0,0],"x":[43,14,7,3,3,2,2,2,1,1],"y":[10,11,9,3,7,1,2,5,8,6],"text":["birth_country: United States of America<br />n: 43<br />category: Economics","birth_country: Other<br />n: 14<br />category: Economics","birth_country: United Kingdom<br />n:  7<br />category: Economics","birth_country: Canada<br />n:  3<br />category: Economics","birth_country: France<br />n:  3<br />category: Economics","birth_country: Netherlands<br />n:  2<br />category: Economics","birth_country: Russia<br />n:  2<br />category: Economics","birth_country: Sweden<br />n:  2<br />category: Economics","birth_country: Germany<br />n:  1<br />category: Economics","birth_country: Italy<br />n:  1<br />category: Economics"],"type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(183,159,0,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"Economics","legendgroup":"Economics","showlegend":true,"xaxis":"x2","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"h","width":[0.899999999999999,0.9,0.899999999999999,0.9,0.899999999999999,0.9,0.899999999999999,0.9,0.9,0.9],"base":[0,0,0,0,0,0,0,0,0,0],"x":[63,11,9,7,6,5,4,4,2,2],"y":[11,7,10,5,9,6,8,2,3,4],"text":["birth_country: Other<br />n: 63<br />category: Literature","birth_country: France<br />n: 11<br />category: Literature","birth_country: United States of America<br />n:  9<br />category: Literature","birth_country: Sweden<br />n:  7<br />category: Literature","birth_country: United Kingdom<br />n:  6<br />category: Literature","birth_country: Italy<br />n:  5<br />category: Literature","birth_country: Germany<br />n:  4<br />category: Literature","birth_country: Russia<br />n:  4<br />category: Literature","birth_country: Canada<br />n:  2<br />category: Literature","birth_country: Japan<br />n:  2<br />category: Literature"],"type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(0,186,56,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"Literature","legendgroup":"Literature","showlegend":true,"xaxis":"x3","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"h","width":[0.899999999999999,0.899999999999999,0.899999999999999,0.899999999999999,0.9,0.9,0.9,0.9,0.9,0.9,0.9],"base":[0,0,0,0,0,0,0,0,0,0,0],"x":[70,67,23,16,12,7,5,4,4,2,1],"y":[10,11,9,8,7,5,6,3,4,1,2],"text":["birth_country: United States of America<br />n: 70<br />category: Medicine","birth_country: Other<br />n: 67<br />category: Medicine","birth_country: United Kingdom<br />n: 23<br />category: Medicine","birth_country: Germany<br />n: 16<br />category: Medicine","birth_country: France<br />n: 12<br />category: Medicine","birth_country: Sweden<br />n:  7<br />category: Medicine","birth_country: Italy<br />n:  5<br />category: Medicine","birth_country: Canada<br />n:  4<br />category: Medicine","birth_country: Japan<br />n:  4<br />category: Medicine","birth_country: Netherlands<br />n:  2<br />category: Medicine","birth_country: Russia<br />n:  1<br />category: Medicine"],"type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(0,191,196,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"Medicine","legendgroup":"Medicine","showlegend":true,"xaxis":"x","yaxis":"y2","hoverinfo":"text","frame":null},{"orientation":"h","width":[0.899999999999999,0.899999999999999,0.9,0.899999999999999,0.9,0.899999999999999,0.9,0.9,0.9,0.9],"base":[0,0,0,0,0,0,0,0,0,0],"x":[57,19,9,5,5,5,1,1,1,1],"y":[11,10,7,8,5,9,3,4,1,2],"text":["birth_country: Other<br />n: 57<br />category: Peace","birth_country: United States of America<br />n: 19<br />category: Peace","birth_country: France<br />n:  9<br />category: Peace","birth_country: Germany<br />n:  5<br />category: Peace","birth_country: Sweden<br />n:  5<br />category: Peace","birth_country: United Kingdom<br />n:  5<br />category: Peace","birth_country: Canada<br />n:  1<br />category: Peace","birth_country: Japan<br />n:  1<br />category: Peace","birth_country: Netherlands<br />n:  1<br />category: Peace","birth_country: Russia<br />n:  1<br />category: Peace"],"type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(97,156,255,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"Peace","legendgroup":"Peace","showlegend":true,"xaxis":"x2","yaxis":"y2","hoverinfo":"text","frame":null},{"orientation":"h","width":[0.899999999999999,0.899999999999999,0.899999999999999,0.899999999999999,0.9,0.9,0.9,0.9,0.9,0.9,0.9],"base":[0,0,0,0,0,0,0,0,0,0,0],"x":[66,54,22,16,11,9,7,6,5,4,4],"y":[10,11,9,8,4,1,7,2,6,3,5],"text":["birth_country: United States of America<br />n: 66<br />category: Physics","birth_country: Other<br />n: 54<br />category: Physics","birth_country: United Kingdom<br />n: 22<br />category: Physics","birth_country: Germany<br />n: 16<br />category: Physics","birth_country: Japan<br />n: 11<br />category: Physics","birth_country: Netherlands<br />n:  9<br />category: Physics","birth_country: France<br />n:  7<br />category: Physics","birth_country: Russia<br />n:  6<br />category: Physics","birth_country: Italy<br />n:  5<br />category: Physics","birth_country: Canada<br />n:  4<br />category: Physics","birth_country: Sweden<br />n:  4<br />category: Physics"],"type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(245,100,227,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"Physics","legendgroup":"Physics","showlegend":true,"xaxis":"x3","yaxis":"y2","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":37.9178082191781,"r":7.30593607305936,"b":40.1826484018265,"l":165.844748858448},"plot_bgcolor":"rgba(255,255,255,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xaxis":{"domain":[0,0.325179386823222],"automargin":true,"type":"linear","autorange":false,"range":[-3.5,73.5],"tickmode":"array","ticktext":["0","20","40","60"],"tickvals":[0,20,40,60],"categoryorder":"array","categoryarray":["0","20","40","60"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y2","title":"","hoverformat":".2f"},"annotations":[{"text":"n","x":0.5,"y":0,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"top","annotationType":"axis","yshift":-21.9178082191781},{"text":"birth_country","x":0,"y":0.5,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xref":"paper","yref":"paper","textangle":-90,"xanchor":"right","yanchor":"center","annotationType":"axis","xshift":-150.502283105023},{"text":"Chemistry","x":0.162589693411611,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(26,26,26,1)","family":"","size":11.689497716895},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"bottom"},{"text":"Economics","x":0.5,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(26,26,26,1)","family":"","size":11.689497716895},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"bottom"},{"text":"Literature","x":0.837410306588389,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(26,26,26,1)","family":"","size":11.689497716895},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"bottom"},{"text":"Medicine","x":0.162589693411611,"y":0.470319634703196,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(26,26,26,1)","family":"","size":11.689497716895},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"bottom"},{"text":"Peace","x":0.5,"y":0.470319634703196,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(26,26,26,1)","family":"","size":11.689497716895},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"bottom"},{"text":"Physics","x":0.837410306588389,"y":0.470319634703196,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(26,26,26,1)","family":"","size":11.689497716895},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"bottom"}],"yaxis":{"domain":[0.529680365296804,1],"automargin":true,"type":"linear","autorange":false,"range":[0.4,11.6],"tickmode":"array","ticktext":["Netherlands","Russia","Canada","Japan","Sweden","Italy","France","Germany","United Kingdom","United States of America","Other"],"tickvals":[1,2,3,4,5,6,7,8,9,10,11],"categoryorder":"array","categoryarray":["Netherlands","Russia","Canada","Japan","Sweden","Italy","France","Germany","United Kingdom","United States of America","Other"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":"","hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":"transparent","line":{"color":"rgba(51,51,51,1)","width":0.66417600664176,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0,"x1":0.325179386823222,"y0":0.529680365296804,"y1":1},{"type":"rect","fillcolor":"rgba(217,217,217,1)","line":{"color":"rgba(51,51,51,1)","width":0.66417600664176,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0,"x1":0.325179386823222,"y0":0,"y1":23.37899543379,"yanchor":1,"ysizemode":"pixel"},{"type":"rect","fillcolor":"transparent","line":{"color":"rgba(51,51,51,1)","width":0.66417600664176,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0.341487279843444,"x1":0.658512720156556,"y0":0.529680365296804,"y1":1},{"type":"rect","fillcolor":"rgba(217,217,217,1)","line":{"color":"rgba(51,51,51,1)","width":0.66417600664176,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0.341487279843444,"x1":0.658512720156556,"y0":0,"y1":23.37899543379,"yanchor":1,"ysizemode":"pixel"},{"type":"rect","fillcolor":"transparent","line":{"color":"rgba(51,51,51,1)","width":0.66417600664176,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0.674820613176778,"x1":1,"y0":0.529680365296804,"y1":1},{"type":"rect","fillcolor":"rgba(217,217,217,1)","line":{"color":"rgba(51,51,51,1)","width":0.66417600664176,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0.674820613176778,"x1":1,"y0":0,"y1":23.37899543379,"yanchor":1,"ysizemode":"pixel"},{"type":"rect","fillcolor":"transparent","line":{"color":"rgba(51,51,51,1)","width":0.66417600664176,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0,"x1":0.325179386823222,"y0":0,"y1":0.470319634703196},{"type":"rect","fillcolor":"rgba(217,217,217,1)","line":{"color":"rgba(51,51,51,1)","width":0.66417600664176,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0,"x1":0.325179386823222,"y0":0,"y1":23.37899543379,"yanchor":0.470319634703196,"ysizemode":"pixel"},{"type":"rect","fillcolor":"transparent","line":{"color":"rgba(51,51,51,1)","width":0.66417600664176,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0.341487279843444,"x1":0.658512720156556,"y0":0,"y1":0.470319634703196},{"type":"rect","fillcolor":"rgba(217,217,217,1)","line":{"color":"rgba(51,51,51,1)","width":0.66417600664176,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0.341487279843444,"x1":0.658512720156556,"y0":0,"y1":23.37899543379,"yanchor":0.470319634703196,"ysizemode":"pixel"},{"type":"rect","fillcolor":"transparent","line":{"color":"rgba(51,51,51,1)","width":0.66417600664176,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0.674820613176778,"x1":1,"y0":0,"y1":0.470319634703196},{"type":"rect","fillcolor":"rgba(217,217,217,1)","line":{"color":"rgba(51,51,51,1)","width":0.66417600664176,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0.674820613176778,"x1":1,"y0":0,"y1":23.37899543379,"yanchor":0.470319634703196,"ysizemode":"pixel"}],"xaxis2":{"type":"linear","autorange":false,"range":[-3.5,73.5],"tickmode":"array","ticktext":["0","20","40","60"],"tickvals":[0,20,40,60],"categoryorder":"array","categoryarray":["0","20","40","60"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"domain":[0.341487279843444,0.658512720156556],"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y2","title":"","hoverformat":".2f"},"xaxis3":{"type":"linear","autorange":false,"range":[-3.5,73.5],"tickmode":"array","ticktext":["0","20","40","60"],"tickvals":[0,20,40,60],"categoryorder":"array","categoryarray":["0","20","40","60"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"domain":[0.674820613176778,1],"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y2","title":"","hoverformat":".2f"},"yaxis2":{"type":"linear","autorange":false,"range":[0.4,11.6],"tickmode":"array","ticktext":["Netherlands","Russia","Canada","Japan","Sweden","Italy","France","Germany","United Kingdom","United States of America","Other"],"tickvals":[1,2,3,4,5,6,7,8,9,10,11],"categoryorder":"array","categoryarray":["Netherlands","Russia","Canada","Japan","Sweden","Italy","France","Germany","United Kingdom","United States of America","Other"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"domain":[0,0.470319634703196],"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":"","hoverformat":".2f"},"showlegend":true,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895},"title":{"text":"category","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"1ba052361912":{"x":{},"y":{},"fill":{},"type":"bar"}},"cur_data":"1ba052361912","visdat":{"1ba052361912":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```
United States is dominant in Chemistry, Medicine, Physics, and Economics. Literature and Peace are the most evenly divided.

How does birth country look across gender?

```r
nobel_winners %>%
  filter(!is.na(birth_country)) %>%
  count(birth_country = fct_lump(birth_country, 10),
        gender,
        sort = TRUE) %>%
  mutate(birth_country = fct_reorder(birth_country, n)) %>%
  ggplot(aes(birth_country, n, fill = gender)) +
  geom_col() +
  facet_wrap(~ gender) +
  coord_flip()
```

![](mantini_nobel_analysis_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

Closer look at female distribution:

```r
nobel_winners %>%
  filter(!is.na(birth_country)) %>%
  filter(gender == "Female") %>% 
  count(birth_country = fct_lump(birth_country, 10),
        gender,
        sort = TRUE) %>%
  mutate(birth_country = fct_reorder(birth_country, n)) %>%
  ggplot(aes(birth_country, n)) +
  geom_col() +
  facet_wrap(~ gender) +
  coord_flip()
```

![](mantini_nobel_analysis_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

All countries that have had multiple recipients are the United States and Europe, outside of Liberia. Can likely attribute to higher degree of independence and accessibility of education in those areas for women.

Spatial Visualization:

Load world shapefile from Natural Earth

```r
# https://www.naturalearthdata.com/downloads/110m-cultural-vectors/
world_shapes <- read_sf("ne_110m_admin_0_countries/ne_110m_admin_0_countries.shp") 
#was having some real problems trying to path to this file when i put it in the data folder, so i cheated a bit and put it in my report folder as well
```

Rename nobel_winners in temp variable to join on to

```r
nobel_winners_geo <- nobel_winners
names(nobel_winners_geo)[names(nobel_winners_geo) == 'birth_country'] <- 'GEOUNIT'
```

Join on country

```r
nobel_map <- nobel_winners_geo %>%
  left_join(world_shapes, by = "GEOUNIT")
```


Map showing the average age of individuals in each (birth) country that a person was awarded a Nobel Prize.


```r
# Make a map of Noble Prize winners with ggplot() + geom_sf()
ggplot() +
  geom_sf(data = nobel_map, aes(geometry = geometry, fill = age),
          color = "white", size = 0.15) +
  coord_sf(crs = "+proj=robin") +
  scale_fill_gradient2(labels = scales::comma) +
  labs(fill = NULL) +
  theme_void() +
  theme(legend.position = "bottom") +
  ggtitle("Average Age of Nobel Prize Winners For Their Respective Birth Country")
```

![](mantini_nobel_analysis_files/figure-html/plot-2015-internet-uses-1.png)<!-- -->
As seen above, this correlates to the number of scientists being proportionally higher in these darker regions, and the scientists are getting their rewards at an older age. That being said, the frequency of rewards in certain countries may skew their regions.




# Conclusions
Nobel Prizes are predominantly won by men in the United States and Europe, who are in their late 60s. Outside research also shows that  the work they won an award for was published ~25 years ago, which was approximately 20% of the way in to their career at that point. This is notably more true for the sciences, with the Peace and Literature Prizes having more variation in age and birth country.


Future possible work:

Could explore other variables such as organizations, death country, affiliation, prize split, and more. Some ideas include doing spatial visualizations with Universities, countries of death, and also doing text mining on the 'motivation' and 'title'. Could also dive in to the partner data set, the nobel_winner_all_pubs, in which analysis could be done on possible variables such as papers before, total papers, position in career, first publication year among others.

