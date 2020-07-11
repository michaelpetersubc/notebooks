# Author : Hongkai Yu (hongkaiyu1999@gmail.com)

# NOTE: Please run cameara_import.R to import the camera_named into the environment

library(tidyverse)
library(magrittr)
library(lubridate)
library(modelr)

# I. Setting up

investigate_id <- function(id) {
  # easier to investigate an auction
  auctions <- camera_named %>% 
    filter(item_id == id) %>% 
    arrange(desc(bid_time))
  
  p <- ggplot(auctions, aes(x = rank(bid_time), y = bid_price)) +
    geom_hline(aes(yintercept =  mean(buy_it_now), colour = 'BIN price'), alpha = 0.5, size = 2) +
    geom_hline(aes(yintercept =  mean(reserve_price), colour = 'reserve price')) +
    geom_hline(aes(yintercept =  max(bid_price), colour = 'highest price')) +
    scale_colour_manual(values = c("blue", "red", "green")) +
    geom_point() +
    scale_x_continuous(breaks = seq_along(auctions)) +
    labs(
      title = paste("Investigation for the auction of item #", id, sep = ""),
      x = "Rank of Bids (ordered by time)",
      y = "Bid price"
    )
  print(p)
  
  invisible(auctions)
}

trades <- camera_named %>%
  # a summary for all trades
  group_by(item_id) %>% 
  summarise(buy_it_now = median(buy_it_now),
            first_bid = first(bid_price, order_by = bid_time),
            highest_bid = max(bid_price),
            n_bids = n(),
            m_X12 = as.logical(mean(X12)),
            m_X13 = as.logical(mean(X13)),
            m_X14 = as.logical(mean(X14))
            ) %>% 
  arrange(desc(n_bids))

View(trades)
# II. Basic analysis

# 1) number of auctions: 4388
nrow(auctions) 

# 2) number of bidders: 18773, number of sellers: 1199
length(unique(camera_named$bidder))
length(unique(camera_named$seller))

# 3) average number of bidders per trade: 9.46
camera_named %>%  
  group_by(item_id) %>% 
  summarize(n = n_distinct(bidder, na.rm = TRUE)) %>% 
  summarize(mean = mean(n))

# The average number of bidders is substantially higher than (number of bidders) / (number of trades) 18773 / 4388 = 4.278259
# This means some of the bidders are very active

# 4) how active are the bidders
camera_named %>%  # the most active bidder bids 154 times
  filter(!is.na(bidder)) %>% 
  group_by(bidder) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n)) %T>% 
  print() %>% 
  ggplot(aes(x = n)) +
  geom_histogram(binwidth = 3)

# 5) bid time distribution: very large of bids occurs near the ends and right after the starts
camera_named %>% 
  mutate(bid_time_relative = as.duration(bid_time - start_time) / as.duration(end_time - start_time)) %>% 
  ggplot(aes(x = bid_time_relative)) +
  geom_freqpoly()

# 6) transaction price (highest price) distribution: 
#           for cameras, the prices are more diverse than expected, especially for the lower ranges
#           also, the distribution of the highest bids is very close to the distribution of the buy it now prices
trades %>% 
  ggplot() +
  geom_freqpoly(aes(x = buy_it_now), colour = 'red') +
  geom_freqpoly(aes(x = highest_bid), colour = 'blue')

trades %>%
  mutate(diff = highest_bid - buy_it_now) %>% 
  ggplot(aes(diff)) +
  geom_boxplot()

# 7) relative vs. bid prices, a positive correlation
camera_named %>% 
  mutate(bid_time_relative = as.duration(bid_time - start_time) / as.duration(end_time - start_time)) %$%
  cor(bid_time_relative, bid_price)
  
reg <- camera_named %>% 
  mutate(bid_time_relative = as.duration(bid_time - start_time) / as.duration(end_time - start_time)) %$%
  lm(bid_price ~ bid_time_relative)

summary(reg)

camera_named %>% 
  mutate(bid_time_relative = as.duration(bid_time - start_time) / as.duration(end_time - start_time)) %>% 
  add_predictions(reg) %>% 
  ggplot(aes(bid_time_relative)) +
  geom_point(aes(y = bid_price)) +
  geom_line(aes(y = pred), colour = "red", size = 2)
  
# III. Analysis for the buy-it-now mechanism

# NOTE: for the explanation of the buy-it-now mechanism, please refer to buy_it_now_mechanism.txt

# 1) there are many cases where the bid_price is higher than the buy_it_now price
ggplot(camera_named, aes(x = buy_it_now, y = bid_price, colour = (bid_price > buy_it_now))) + 
  geom_point(alpha = 0.8, size = 0.5, show.legend = FALSE)

# 2) some of the products have multiple bidders bidding the buy_it_now price
camera_named %>%  
  filter(bid_price == buy_it_now) %>% 
  count(item_id) %>% 
  arrange(desc(n))

# 3) surprisingly, even if the first bid equals to the buy-it-now price, many trades don't just end there. 194 cases
trades %>% 
  filter(first_bid == buy_it_now) %>% 
  filter(n_bids > 1)

# 4) even more surprisingly, some bidders bid higher than buy-it-now price in their first bid. 40 cases
#    They would obviously be better off by just bidding the buy-it-now price 
trades %>% 
  filter(first_bid > buy_it_now) %>% 
  arrange(desc(first_bid - buy_it_now))
  

# 5) now I assume the condition for distinguish the buy-it-now trades from is below, which classifies 208 trades as BIN trades
trades_BINcondtion <- trades %>% 
  mutate(isBIN = (first_bid == buy_it_now & n_bids == 1))

sum(trades_BINcondtion$isBIN)

# 6) none of the boolean values are related to buy-it-now, since all boolean variables still vary to isBIN
trades_BINcondtion %>% 
  select(isBIN, m_X12, m_X13, m_X14) %>% View

# 7) there could be more than one bids higher than buy-it-now price
camera_named %>%
  group_by(item_id) %>% 
  summarize(count = sum(bid_price > buy_it_now)) %>% 
  arrange(desc(count))

investigate_id(349399376)

# Explanation for the analysis:
#   I would assume the reason that 3) 4) happen because those bidders didn't understand how BIN worked. 
#   Although they could have just used BIN and be better off, they still chose to make an offer equals to or higher than BIN price.
#   However, the problem with this explanation is that the number of people misunderstand the rules (194 + 40), 
#   is more than the number of people used the BIN mechanism correctly (208). This is a very weird result.


 # ---------- DON'T READ, BELOW ARE SOME WORKING DRAFT ---------- #
camera_named %>% # none of the boolean values has anything to do with buy_it_now
  filter(bid_price == buy_it_now) %>% 
  summarise(n = n(),
            n_X12 = sum(X12),
            n_X13 = sum(X13),
            n_X14 = sum(X14)
            )


camera_named %>% 
  filter(bid_price > reserve_price) %>% 
  summarise(n = n(),
            n_X12 = sum(X12),
            n_X13 = sum(X13),
            n_X14 = sum(X14)
  )

camera_named %>% 
  filter(bid_price > buy_it_now) %>% 
  View

camera_named %>% 
  filter(bid_price > buy_it_now) %>% 
  summarise(n = n(),
            n_X12 = sum(X12),
            n_X13 = sum(X13),
            n_X14 = sum(X14)
  )

camera_named %>%  # multiple buyers can bid higher than the buy_it_now prices without ending the auction
  filter(bid_price > buy_it_now) %>% 
  group_by(product) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) # item_id: 349399376


camera_named %>%   # all bid_price are higher than reserve_price
  filter(bid_price > 0 & bid_price < reserve_price)


camera_named %>%      # control bid_time is good enough for filter out 0s
  filter(!is.na(bid_time)) %>% 
  filter(bid_price == 0)

camera_named %>% 
  filter(bidder == "nosurprisesplease") %>% 
  left_join(trades, by = "item_id") %>% 
  filter(bid_price == highest_bid)

trades %>% 
  filter(bid== 1) %>% 
  mutate(isBIN = highest_bid == buy_it_now) %>%
  View
  filter(isBIN == FALSE)

for (b in c("m_X12", "m_X13","m_X14")) {
  trades %>% 
    ggplot(aes(b, buy_it_now)) +
    geom_boxplot()
}

trades_BINcondtion %>% 
  ggplot(aes(isBIN, highest_bid)) +
  geom_boxplot()



