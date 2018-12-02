#Loading the libraries

library(tidyverse)
library(pscl)
library(UBL)
library(ggraph)
library(igraph)

#save(train, file = "./data/train.Rdata")
#save(test, file = "./data/test.Rdata")

# Overall Customers
dat <- train %>% 
  transmute(revenue = totals.transactionRevenue,
            browser = device.browser,
            os = device.operatingSystem,
            device = device.deviceCategory,
            region = geoNetwork.region,
            domain = geoNetwork.networkDomain,
            hits = totals.hits,
            pageviews = totals.pageviews,
            bounces = totals.bounces,
            medium = trafficSource.medium,
            campaign = trafficSource.campaign,
            keyword = trafficSource.keyword,
            channel = channelGrouping,
            visitor = fullVisitorId,
            visitId,
            date,
            timeOfVisit = visitStartTime)

#Paying Customers
paid <- dat %>% filter(revenue > 0)
free <- dat %>% filter(is.na(revenue) == T)


#EDA

# Channelwise

ggplot(paid, aes(channel,log(revenue),fill = channel)) + 
  geom_violin() +
  ggtitle("Revenue by channel") +
  coord_flip()
#theme(axis.text.x=element_text(angle = -45, hjust = 0))

ggplot(paid, aes(channel,hits,fill = channel)) + 
  geom_bar(stat = "identity") +
  ggtitle("Hits by channel") +
  coord_flip()
#theme(axis.text.x=element_text(angle = -45, hjust = 0))

ggplot(paid, aes(channel,fill = channel)) + 
  geom_bar(stat = "count") +
  ggtitle("Revenue by channel") +
  theme(axis.text.x=element_text(angle = -45, hjust = 0))


# Devicewise

ggplot(paid, aes(device,revenue,fill = device)) + 
  geom_boxplot() +
  ggtitle("Revenue by channel") +
  theme(axis.text.x=element_text(angle = -45, hjust = 0))

ggplot(paid, aes(device,hits,fill = device)) + 
  geom_bar(stat = "sum") +
  ggtitle("Revenue by channel") +
  theme(axis.text.x=element_text(angle = -45, hjust = 0))

ggplot(paid, aes(device,fill = device)) + 
  geom_bar(stat = "count") +
  ggtitle("Revenue by channel") +
  theme(axis.text.x=element_text(angle = -45, hjust = 0))

# Devicewise-Channelwise

ggplot(paid, aes(device,revenue,fill = device)) + 
  geom_boxplot() +
  ggtitle("Revenue by channel") +
  facet_wrap(~paid$channel) +
  theme(axis.text.x=element_text(angle = -45, hjust = 0))

ggplot(paid, aes(device,hits,fill = device)) + 
  geom_bar(stat = "sum") +
  ggtitle("Revenue by channel") +
  facet_wrap(~paid$channel) +
  theme(axis.text.x=element_text(angle = -45, hjust = 0))

ggplot(paid, aes(device,fill = device)) + 
  geom_bar(stat = "count") +
  ggtitle("Revenue by channel") +
  facet_wrap(~paid$channel) +
  theme(axis.text.x=element_text(angle = -45, hjust = 0))





