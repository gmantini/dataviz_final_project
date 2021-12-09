---
title: "Mini-Project 02"
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
```{r}
library(tidyverse) #mostly for ggplot
library(lubridate) #working with dates/times
library(plotly) #interactive plots
library(sf) #world shapefile
```

Set theme
```{r}
theme_set(theme_bw())
```

Read in data, add column to sort by decade for cleaner plots
```{r,message=F,warning=F}
nobel_winners <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winners.csv") %>% #read_csv is significantly faster here than read.csv
  distinct(full_name, prize_year, category, .keep_all = TRUE) %>%
  mutate(decade = 10 * (prize_year %/% 10),
         age = prize_year - year(birth_date))
```

Glimpse at Nobel Winners 
```{r}
glimpse(nobel_winners)
```


# Age Analysis

Linear fit of prize year as a function of age
```{r,message=F,warning=F}
ggplot(nobel_winners, aes(x = prize_year, y = age)) +
  geom_point() +
  geom_smooth(method = "lm", 
              formula = "y ~ x") + 
  theme_minimal()
```
Get the jist that prize recipients have been older over time at the time of receiving. Let's take a closer look:

How has age of Nobel Prize recipients changed over time?
```{r,message=F,warning=F}
nobel_winners %>%
  filter(!is.na(age)) %>%
  group_by(decade) %>%
  summarize(average_age = mean(age),
            median_age = median(age)) %>%
  ggplot(aes(decade, average_age)) +
  geom_line()
```
Definitely a noticeable spike since the mid 1900's.

How about over different categories?

Boxplot view of age per category
```{r,message=F,warning=F}
nobel_categories <- nobel_winners %>%
  mutate(category = fct_reorder(category, age, median, na.rm = TRUE)) %>%
  ggplot(aes(category, age)) +
  geom_boxplot() +
  coord_flip()

ggplotly(nobel_categories)
```
Get to see a couple interesting outliers here.
```{r}
nobel_winners %>% 
  filter(age == 17 | age == 90) %>% 
  arrange(full_name) %>% 
  select(full_name,age,prize_year,category)
```

Change of age over time across categories
```{r,message=F,warning=F}
nobel_age <- nobel_winners %>%
  filter(!is.na(age)) %>%
  group_by(decade, category) %>%
  summarize(average_age = mean(age),
            median_age = median(age)) %>%
  ggplot(aes(decade, average_age, color = category)) +
  geom_line()

ggplotly(nobel_age)
```
Average age across the board has increased for the most part, especially for the "hard sciences". Literature is the oldest, and consistently has been for a while. Peace has by far been the most volatile, notably skewed by a few data points. For the 2010's, our data is skewed by having the 17 year old Malala Yousafzai win in 2014, in addition for data being only collected up to 2016 for this dataset.

Going back to plotting age as a function of prize_year: Adding categories with trend lines.
```{r,message=F,warning=F}
ggplot(data = nobel_winners, aes(x = prize_year, y = age))+
    geom_point()+
    geom_smooth()+
    facet_wrap(~ category)
```
Can confirm that the sciences have taken a noticeable trend upwards, while Peace has actually had a downward trend. Economics has had little change, with Literature having a slight trend upward.


# Regional Analysis

Now taking a look at distribution of birth country for Nobel Prize winners: 
```{r}
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
The United States dominates the rest of the field, having well over 3 times the number of recipients as the next highest country (U.K.)

How does birth country look across different categories?
```{r}
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
United States is dominant in Chemistry, Medicine, Physics, and Economics. Literature and Peace are the most evenly divided.

How does birth country look across gender?
```{r}
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

Closer look at female distribution:
```{r}
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

All countries that have had multiple recipients are the United States and Europe, outside of Liberia. Can likely attribute to higher degree of independence and accessibility of education in those areas for women.

Spatial Visualization:

Load world shapefile from Natural Earth
```{r load-libraries-data, warning=FALSE, message=FALSE}
# https://www.naturalearthdata.com/downloads/110m-cultural-vectors/
world_shapes <- read_sf("ne_110m_admin_0_countries/ne_110m_admin_0_countries.shp") 
#was having some real problems trying to path to this file when i put it in the data folder, so i cheated a bit and put it in my report folder as well
```

Rename nobel_winners in temp variable to join on to
```{r}
nobel_winners_geo <- nobel_winners
names(nobel_winners_geo)[names(nobel_winners_geo) == 'birth_country'] <- 'GEOUNIT'
```

Join on country
```{r combine-shapefile-internet-users}
nobel_map <- nobel_winners_geo %>%
  left_join(world_shapes, by = "GEOUNIT")
```


Map showing the average age of individuals in each (birth) country that a person was awarded a Nobel Prize.

```{r plot-2015-internet-uses}
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
As seen above, this correlates to the number of scientists being proportionally higher in these darker regions, and the scientists are getting their rewards at an older age. That being said, the frequency of rewards in certain countries may skew their regions.




# Conclusions
Nobel Prizes are predominantly won by men in the United States and Europe, who are in their late 60s. Outside research also shows that  the work they won an award for was published ~25 years ago, which was approximately 20% of the way in to their career at that point. This is notably more true for the sciences, with the Peace and Literature Prizes having more variation in age and birth country.


Future possible work:

Could explore other variables such as organizations, death country, affiliation, prize split, and more. Some ideas include doing spatial visualizations with Universities, countries of death, and also doing text mining on the 'motivation' and 'title'. Could also dive in to the partner data set, the nobel_winner_all_pubs, in which analysis could be done on possible variables such as papers before, total papers, position in career, first publication year among others.


