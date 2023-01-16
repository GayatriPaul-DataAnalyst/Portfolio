---
title: "Cyclistic Data Analysis - Data Processing"
author: "Gayatri Paul"
date: "2023-01-06"
output: html_document
editor_options:
  markdown: 
    wrap: 72
    toc: true
    toc_float: true
---


```{r setup, include=FALSE}
knitr::opts_chunk$set(warning = FALSE, message = FALSE) 
```


The data from October 2021 to October 2022 has been downloaded. Each
file is in csv format. Let's import it to R.

```{r}
oct_21 <- read.csv("C:/Users/paulg/Downloads/csv/1_oct_21.csv", header=T, na.strings=c("","NA"))

nov_21 <- read.csv("C:/Users/paulg/Downloads/csv/2_nov_21.csv", header=T, na.strings=c("","NA"))

dec_21 <- read.csv("C:/Users/paulg/Downloads/csv/3_dec_21.csv", header=T, na.strings=c("","NA"))

jan_22 <- read.csv("C:/Users/paulg/Downloads/csv/4_jan_22.csv", header=T, na.strings=c("","NA"))

feb_22 = read.csv("C:/Users/paulg/Downloads/csv/5_feb_22.csv", header=T, na.strings=c("","NA"))

mar_22 = read.csv("C:/Users/paulg/Downloads/csv/6_mar_22.csv", header=T, na.strings=c("","NA"))

apr_22 = read.csv("C:/Users/paulg/Downloads/csv/7_apr_22.csv", header=T, na.strings=c("","NA"))

may_22 = read.csv("C:/Users/paulg/Downloads/csv/8_may_22.csv", header=T, na.strings=c("","NA"))

jun_22 = read.csv("C:/Users/paulg/Downloads/csv/9_jun_22.csv", header=T, na.strings=c("","NA"))

jul_22 = read.csv("C:/Users/paulg/Downloads/csv/10_july_22.csv", header=T, na.strings=c("","NA"))

aug_22 = read.csv("C:/Users/paulg/Downloads/csv/11_aug_22.csv", header=T, na.strings=c("","NA"))

sept_22 = read.csv("C:/Users/paulg/Downloads/csv/12_sept_22.csv", header=T, na.strings=c("","NA"))

oct_22 = read.csv("C:/Users/paulg/Downloads/csv/13_oct_22.csv", header=T, na.strings=c("","NA"))
```

## All the data frames contain 13 variables, lets combine them for the purpose of analysis.

```{r}
df= rbind(oct_21, nov_21, dec_21, jan_22, feb_22, mar_22, apr_22, may_22, jun_22, jul_22, aug_22, sept_22, oct_22)
head(df)
```

```{r}
print(nrow(df))
```

```{r}
colnames(df)
```

```{r}
summary(df)
```

## Rename the column named "member_casual" to "rider_type" for clarity.

```{r}
library(dplyr)
df=rename(df,"rider_type"="member_casual")
head(df)
```

## Remove duplicate Rows

```{r}
df=df[!duplicated(df), ]
print(nrow(df))
```

Number of rows remained the same, which indicates that the data frame
contained no duplicate data.

## Find columns with missing information.

```{r}
colSums(is.na(df))
```

Columns namely: start_station_name, start_station_id, end_station_name,
end_station_id, end_lat, end_lng contain missing values. As we are not
using these columns for analysis, we will keep them as they are. If we
decide to use these columns, we will first remove rows with NA and then
proceed further.

## Convert date columns to proper date formats & split date and time into 2 columns.

```{r}
library(lubridate)

df$started_at <- ymd_hms(df$started_at)
df$ended_at <- ymd_hms(df$ended_at)

df$start_date <- as.Date(df$started_at)
df$start_time <- format(as.POSIXct(df$started_at), format = "%H:%M:%S")

df$end_date <- as.Date(df$ended_at)
df$end_time <- format(as.POSIXct(df$ended_at), format = "%H:%M:%S")
```

## Calculate ride length in mins

```{r}
df$ride_length <- round(difftime(df$ended_at,df$started_at, units = "mins"), 2)

df$ride_length <- as.numeric(df$ride_length)

```

## keep rows with only positive ride lengths

```{r}
df <- filter(df, ride_length > 0)
head(df)
```

There are rides which lasted less than a minute which seems odd but, but
as we don't have relevant data or source of information which can be
used to confirm that these entries are wrong, we will assume that these
are correct and continue with the analysis.

## Calculate number of days the ride lasted

```{r}
df$no_of_days <- as.numeric(difftime(df$end_date, df$start_date, units = "days" )+1)

count(filter(df, no_of_days > 1))
```

*38417* rides lasted more than a day. If we assume this data is correct,
these users had to be charged extra, which means their ride must costed
a lot, this case can be used to advocate annual memberships.

## Calculate day of week for the ride

```{r}
df$day_of_week_num <- wday(df$start_date)

df <- df %>%
  mutate(day_of_week=
           ifelse(day_of_week_num==1,"sunday",
                  ifelse(day_of_week_num==2, "monday",
                         ifelse(day_of_week_num==3, "tuesday",
                                ifelse(day_of_week_num==4, "wednesday",
                                       ifelse(day_of_week_num==5, "thursday",
                                              ifelse(day_of_week_num==6, "friday",
                                                     "saturday" )))))))
```

## Add month and Season columns.

```{r}
library(tidyverse)

df$month_col <- month(df$started_at)

df <- df %>% 
  mutate(season=
           ifelse(month_col==12 |month_col==1 |month_col==2,
                  "winter",
                  ifelse(month_col==3 |month_col==4 |month_col==5,
                         "spring",
                         ifelse(month_col==6 |month_col==7 |month_col==8,
                                "summer",
                                "fall"))))

df$year <- year(df$started_at)

df$month_year= paste(df$month_col, df$year)

head(df)
```

## Add holiday column.

```{r}
df <- df %>% 
  mutate(holiday=
           ifelse(start_date=="2021-11-11" |start_date=="2021-11-25"|start_date=="2021-12-24"
                  |start_date=="2021-12-31"|start_date=="2022-01-01"|start_date=="2022-03-17"
                  |start_date=="2022-05-30"|start_date=="2022-07-04"|start_date=="2022-09-05"
                  |day_of_week=="sunday"|day_of_week=="saturday",
                  "holiday",
                  "workday"))

```


#### Next step would be to start analysing. 
