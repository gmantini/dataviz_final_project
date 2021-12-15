---
title: "Mini Project 3"
subtitle: "Visualizing Text and Distributions"
output: 
  html_document:
    keep_md: true
---

# Data Visualization Project 03


In this exercise you will explore methods to visualize text data and/or practice how to recreate charts that show the distributions of a continuous variable. 


## Visualizing Text Data

Review the set of slides (and additional resources linked in it) for visualizing text data: https://www.reisanar.com/slides/text-viz#1

Choose any dataset with text data, and create at least one visualization with it. For example, you can create a frequency count of most used bigrams, a sentiment analysis of the text data, a network visualization of terms commonly used together, and/or a visualization of a topic modeling approach to the problem of identifying words/documents associated to different topics in the text data you decide to use. 

Make sure to include a copy of the dataset in the `data/` folder, and reference your sources if different from the ones listed below:

- [Billboard Top 100 Lyrics](https://github.com/reisanar/datasets/blob/master/BB_top100_2015.csv)

- [RateMyProfessors comments](https://github.com/reisanar/datasets/blob/master/rmp_wit_comments.csv)

- [FL Poly News 2020](https://github.com/reisanar/datasets/blob/master/poly_news_FL20.csv)

- [FL Poly News 2019](https://github.com/reisanar/datasets/blob/master/poly_news_FL19.csv)

(to get the "raw" data from any of the links listed above, simply click on the `raw` button of the GitHub page and copy the URL to be able to read it in your computer using the `read_csv()` function)


```r
library(tidyverse)
library(tidytext)
library(gutenbergr)
library(sentimentr)
library(wordcloud)
library(textdata)
library(reshape2)
```


# Sentiment Analysis


```r
lyrics <- read_csv("https://raw.githubusercontent.com/reisanar/datasets/master/BB_top100_2015.csv");lyrics
```

```
## Rows: 100 Columns: 6
```

```
## -- Column specification --------------------------------------------------------
## Delimiter: ","
## chr (3): Song, Artist, Lyrics
## dbl (3): Rank, Year, Source
```

```
## 
## i Use `spec()` to retrieve the full column specification for this data.
## i Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```
## # A tibble: 100 x 6
##     Rank Song              Artist          Year Lyrics                    Source
##    <dbl> <chr>             <chr>          <dbl> <chr>                      <dbl>
##  1     1 uptown funk       mark ronson f~  2015 this hit that ice cold m~      1
##  2     2 thinking out loud ed sheeran      2015 when your legs dont work~      1
##  3     3 see you again     wiz khalifa f~  2015 its been a long day with~      1
##  4     4 trap queen        fetty wap       2015 im like hey wassup hello~      1
##  5     5 sugar             maroon 5        2015 im hurting baby im broke~      1
##  6     6 shut up and dance walk the moon   2015 oh dont you dare look ba~      1
##  7     7 blank space       taylor swift    2015 nice to meet you where y~      1
##  8     8 watch me          silento         2015 now watch me whip kill i~      1
##  9     9 earned it         the weeknd      2015 you make it look like it~      1
## 10    10 the hills         the weeknd      2015 your man on the road he ~      1
## # ... with 90 more rows
```


```r
lyrics <- select(lyrics, -Year, -Source); head(lyrics)
```

```
## # A tibble: 6 x 4
##    Rank Song              Artist                             Lyrics             
##   <dbl> <chr>             <chr>                              <chr>              
## 1     1 uptown funk       mark ronson featuring bruno mars   this hit that ice ~
## 2     2 thinking out loud ed sheeran                         when your legs don~
## 3     3 see you again     wiz khalifa featuring charlie puth its been a long da~
## 4     4 trap queen        fetty wap                          im like hey wassup~
## 5     5 sugar             maroon 5                           im hurting baby im~
## 6     6 shut up and dance walk the moon                      oh dont you dare l~
```

### Pre-processing


```r
top_100 <- lyrics %>%
  unnest_tokens(word, Lyrics, token = "words") %>%
  filter(!word %in% stop_words$word, str_detect(word, "[a-z]"))

top_100
```

```
## # A tibble: 14,747 x 4
##     Rank Song        Artist                           word    
##    <dbl> <chr>       <chr>                            <chr>   
##  1     1 uptown funk mark ronson featuring bruno mars hit     
##  2     1 uptown funk mark ronson featuring bruno mars ice     
##  3     1 uptown funk mark ronson featuring bruno mars cold    
##  4     1 uptown funk mark ronson featuring bruno mars michelle
##  5     1 uptown funk mark ronson featuring bruno mars pfeiffer
##  6     1 uptown funk mark ronson featuring bruno mars white   
##  7     1 uptown funk mark ronson featuring bruno mars gold    
##  8     1 uptown funk mark ronson featuring bruno mars hood    
##  9     1 uptown funk mark ronson featuring bruno mars girls   
## 10     1 uptown funk mark ronson featuring bruno mars girls   
## # ... with 14,737 more rows
```

### Sentiment analysis with afinn lexicon


```r
top_100 %>%
  inner_join(get_sentiments("afinn")) %>%
  count(Song, value) %>% 
  spread(value, n, fill = 0)
```

```
## Joining, by = "word"
```

```
## # A tibble: 97 x 10
##    Song                     `-5`  `-4`  `-3`  `-2`  `-1`   `1`   `2`   `3`   `4`
##    <chr>                   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
##  1 679                         2     4     1     1     0     5     6     2     0
##  2 711                         0     0     1     1     3     4     4     0     0
##  3 all about that bass         1     1     2     0     1     3     2     3     0
##  4 all eyes on you             5     2     0     6     1     2     1     5     0
##  5 animals                     0     0     1    10    10    19     0     1     0
##  6 ayo                        10    12     6     6     5     2     8     1     0
##  7 back to back                6     4     3     2     1     8     2     1     0
##  8 bad blood                   0     0    22     4     5     7     3     6     0
##  9 bang bang                   0     0     3     2     2     3     1     2     1
## 10 bitch better have my m~    32     2     0     1     7     0     3     0     0
## # ... with 87 more rows
```
 


```r
top_100 %>%
  group_by(word) %>%
  summarise(uses = n()) %>%
  arrange(desc(uses))
```

```
## # A tibble: 3,264 x 2
##    word   uses
##    <chr> <int>
##  1 im      548
##  2 dont    320
##  3 love    308
##  4 baby    173
##  5 wanna   155
##  6 youre   137
##  7 yeah    124
##  8 aint    123
##  9 gonna   117
## 10 hey     112
## # ... with 3,254 more rows
```

### The weeknd song

```r
the_weeknd <- lyrics %>% 
 filter(Artist == "the weeknd") %>% 
 unnest_tokens(word, Lyrics) %>% 
 anti_join(stop_words)
```

```
## Joining, by = "word"
```

```r
head(the_weeknd)
```

```
## # A tibble: 6 x 4
##    Rank Song      Artist     word    
##   <dbl> <chr>     <chr>      <chr>   
## 1     9 earned it the weeknd magic   
## 2     9 earned it the weeknd im      
## 3     9 earned it the weeknd confused
## 4     9 earned it the weeknd hey     
## 5     9 earned it the weeknd heyim   
## 6     9 earned it the weeknd love
```

Top words in the weeknd's songs

```r
the_weeknd %>%
  group_by(word) %>%
  summarise(uses = n()) %>%
  arrange(desc(uses))
```

```
## # A tibble: 146 x 2
##    word    uses
##    <chr>  <int>
##  1 love      44
##  2 im        28
##  3 feel      22
##  4 girl      15
##  5 cked      13
##  6 real      12
##  7 youre     12
##  8 yeah      11
##  9 dont      10
## 10 earned    10
## # ... with 136 more rows
```

### Word Cloud




```r
top_100_wordcloud <- the_weeknd %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words=100))
```

```
## Joining, by = "word"
```

![](mantini_bb_top100_files/figure-html/unnamed-chunk-9-1.png)<!-- -->


### Word cloud with *bing* lexicon




```r
the_weeknd %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word~sentiment, value.var ="n", fill=0) %>%
  comparison.cloud(colors = c("red","blue"), max.words = 100)
```

```
## Joining, by = "word"
```

![](mantini_bb_top100_files/figure-html/unnamed-chunk-10-1.png)<!-- -->
