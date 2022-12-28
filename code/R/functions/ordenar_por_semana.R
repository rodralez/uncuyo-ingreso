library(lubridate)
library(tidyverse)

set.seed(10)'replace = FALSE'

x <- seq( as.POSIXct("2021-02-01"), as.POSIXct("2021-03-31") , by = "day" )

df <- tibble(Datetime = x,  sold_quantity = sample( 1:100, length(x)) ) %>% arrange(Datetime)

head(df)

library(lubridate)
df <- df %>%  mutate(week = week(Datetime))
head(df)

df <- df %>%  mutate(week_day = wday(Datetime, label = TRUE, abbr = TRUE)) %>%  mutate(week = isoweek(Datetime))
head(df)

df_2 <- df %>% group_by(week_day) %>% summarise(sold = sum(sold_quantity))
head(df_2)

# ==============================================================================


set.seed(1)
mydata <- data.frame(Tmin = sample(0:3), Tmax = sample(4:7), Day = rep(1:4))

library(dplyr)
mydata_2 <-mydata %>% tidyr::uncount(24) 
# %>% group_by(Day) %>% mutate(hour = 1:24)

# ==============================================================================

df <- data.frame(x = c("a", "b"), y = c(3, 4), z = c(5, 6))

df2 <- df %>% spread(x, y) %>% gather("x", "y", a:b, na.rm = TRUE)


