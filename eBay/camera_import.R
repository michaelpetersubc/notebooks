# Author : Hongkai Yu (hongkaiyu1999@gmail.com)

library(tidyverse)
library(lubridate)

# ------------------- RUN BELOW TO GET CAMERA_NAMED ------------------- #
camera <- read_tsv("camera.txt", col_names = FALSE, col_types = 
                     cols(
                       X1 = col_integer(),
                       X2 = col_integer(),
                       X3 = col_character(),
                       X4 = col_character(),
                       X5 = col_double(),
                       X6 = col_character(),
                       X7 = col_double(),
                       X8 = col_integer(),
                       X9 = col_character(),
                       X10 = col_double(),
                       X11 = col_integer(),
                       X12 = col_logical(),
                       X13 = col_logical(),
                       X14 = col_logical(),
                       X15 = col_character(),  # use lubridate to parse them later
                       X16 = col_character(), 
                       X17 = col_character()
                     ))

camera_named <- camera %>% 
  mutate(X15 = ymd_hms(X15) + years(5), # should be in year 2005
         X16 = ymd_hms(X16) + years(5),
         X17 = ymd_hms(X17) + years(5)
         ) %>% 
  rename(bid_id = X1, 
         item_id = X2, 
         product = X3, 
         model = X4, 
         buy_it_now = X5,    
         seller = X6, 
         reserve_price = X7, 
         seller_feedback = X8, 
         bidder = X9, 
         bid_price = X10, 
         buyer_feedback = X11,
         X12 = X12,
         X13 = X13,
         X14 = X14,
         bid_time = X15,
         start_time = X16,
         end_time = X17) %>% 
  filter(!is.na(bid_time)) # get rid of invalid bids

# ------------------- RUN ABOVE TO GET CAMERA_NAMED ------------------- #

View(camera_named)

# BELOW ARE DISCUSSIONS OF THE LABELS OF THE VARIABLES

# I. X1, X2, X3, X4, X6, X9, X10 are clear, also it seems that X8, X11 are sellers' and buyers' feedbacks.

# II. X15, X16, X17 (datetime)
table(camera_named$bid_time > camera_named$start_time) # always true
table(camera_named$bid_time > camera_named$end_time) # always false

# III. X5, X7 (prices)
table(camera_named$bid_price >= camera_named$reserve_price) # always true

summary <- camera_named %>%
  group_by(product) %>%
  summarise(highest_bid = max(bid_price),
            buy_it_now = median(buy_it_now))
table(summary$buy_it_now < summary$highest_bid) # not always true
table(summary$buy_it_now > summary$highest_bid) # not always true
  
# IV. X12 - X14 (logical values), they are still mysterious
table(camera_named$X12) 
table(camera_named$X13) 
table(camera_named$X14) 

# they won't vary in a trade
summary <- camera_named %>%
  # a summary for all trades
  group_by(item_id) %>% 
  summarise(m_X12 = mean(X12),
            m_X13 = mean(X13),
            m_X14 = mean(X14)
  ) 
table(summary$m_X12)
table(summary$m_X13)
table(summary$m_X14)
mean(c(FALSE,TRUE,FALSE))
