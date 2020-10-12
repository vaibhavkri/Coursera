setwd('.')

if (!file.exists('StormData.csv.bz2')) {
   url <- 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2' 
   download.file(url, destfile = 'StormData.csv.bz2', method = 'curl')
}

data <- read.csv('StormData.csv.bz2', stringsAsFactors = FALSE)

library(ggplot2)
library(dplyr)

## 
## Attaching package: 'dplyr'

## The following objects are masked from 'package:stats':
## 
##     filter, lag

## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union


library(tidyr)

casualities.by.type <- data %>% 
    # Grouping by event type
    group_by(EVTYPE) %>%                            
    # Calculating summaries for fatalities and injuries
    summarise(FATALITIES = sum(FATALITIES),         
              INJURIES = sum(INJURIES)) %>%
    # Sorting by sum of fatalities and injuries in descending order
    arrange(desc(FATALITIES + INJURIES)) %>%
    # Taking only first 10 records
    slice(1:10) %>%
    # Melting injuries and fatalities 
    gather(CType, Value, c(FATALITIES, INJURIES))

    ggplot <- ggplot(data = casualities.by.type,
                 aes(x = reorder(EVTYPE, -Value), 
                     y = Value,
                     fill = (CType))) +
    geom_bar(stat = 'identity', col = 'black') +
    labs(title = 'Top 10 Events By Casualties', 
         x = 'Type of event',
         y = 'Counts',
         fill = 'Type') +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

print(ggplot)

dt2 <- data[c("EVTYPE", "CROPDMG", "CROPDMGEXP", "PROPDMG", "PROPDMGEXP")]

pd <- dt2$PROPDMG
pde <- dt2$PROPDMGEXP
cd <- dt2$CROPDMG
cde <- dt2$CROPDMGEXP

pde.n <- as.numeric(pde)

## Warning: NAs introduced by coercion


pd <- pd * 10 ** replace(pde.n, is.na(pde.n), 0)
pd[pde %in% "B"] <- pd[pde %in% "B"] * 1e9
pd[pde %in% c("M", "m")] <- pd[pde %in% c("M", "m")] * 1e6
pd[pde %in% c("K")] <- pd[pde %in% c("K")] * 1e3
pd[pde %in% c("H", "h")] <- pd[pde %in% c("H", "h")] * 1e2
pd[!(pde %in% c("B", "M", "m", "K", "H", "h"))] <- pd[!(pde %in% c("B", "M", 
                                                                   "m", "K", "H", "h"))] * 1

cde.n <- as.numeric(cde)


## Warning: NAs introduced by coercion


cd <- cd * 10 ** replace(cde.n, is.na(cde.n), 0)
cd[cde %in% "B"] <- cd[cde %in% "B"] * 1e9
cd[cde %in% c("M", "m")] <- cd[cde %in% c("M", "m")] * 1e6
cd[cde %in% c("K", "k")] <- cd[cde %in% c("K", "k")] * 1e3
cd[!(cde %in% c("B", "M", "m", "K", "k"))] <- cd[!(cde %in% c("B", "M", "m", 
                                                              "K", "k"))] * 1
dt2$PROPDMG <- pd
dt2$CROPDMG <- cd

dt2 <- dt2 %>% 
    # Droping the columns with exponents
    select(-c(CROPDMGEXP, PROPDMGEXP)) %>%
    # Grouping by event type
    group_by(EVTYPE) %>%
    # Aggregating by property damage and crops damage
    # also shifting to millions
    summarise(PROPDMG = sum(PROPDMG) / 1e6,
              CROPDMG = sum(CROPDMG) / 1e6) %>%
    # Sorting by sum of property damage and crops damage in descending order
    arrange(desc(PROPDMG + CROPDMG)) %>% 
    # Taking first 10 records
    slice(1:10) %>%
    # Melting crops/property damage by type for plotting
    gather(TYPE, VALUE, CROPDMG:PROPDMG)

ggp <- ggplot(dt2, 
              aes(x = reorder(EVTYPE, -VALUE), 
                  y = VALUE, fill = TYPE)) + 
    geom_bar(stat = "identity", col = 'black') +
    labs(x = "Type of event", y = "Value (in Millions)") +
    labs(title = "Top 10 Types of Events By Economic Consequences") +
    labs(fill = "Type") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0))

print(ggp)