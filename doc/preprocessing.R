#Loading the libraries

library(tidyverse)
library(pscl)
library(UBL)
library(ggraph)
library(igraph)
library(MASS)
library(lubridate)
library(e1071)
library(gridExtra)
options(scipen = 999)

#save(train, file = "./data/train.Rdata")
#save(test, file = "./data/test.Rdata")

# Preprocessing
dat <- train %>% 
  transmute(revenue = ifelse(is.na(log(totals.transactionRevenue)) == T, 0, log(totals.transactionRevenue)),
            browser = factor(case_when(device.browser == "Chrome" ~ "chrome",
                                device.browser %in% c("Safari", "Safari (in-app)") ~ "safari",
                                device.browser == "Firefox" ~ "firefox",
                                device.browser %in% c("Edge", "Internet Explorer") ~ "edge",
                                device.browser %in% c("Opera", "Opera Mini") ~ "opera",
                                TRUE ~ "other")),
            os = factor(case_when(device.operatingSystem == "Windows" ~ "windows",
                           device.operatingSystem == "Mackintosh" ~ "mackintosh",
                           device.operatingSystem == "Android" ~ "android",
                           device.operatingSystem == "iOS" ~ "ios",
                           device.operatingSystem  == "Linux" ~ "linux",
                           device.operatingSystem  == "ChromeOS" ~ "chromeos",
                           is.na(device.operatingSystem) == T ~ "unknown",
                           TRUE ~ "other")),
            device = factor(device.deviceCategory),
            continent = factor(ifelse(is.na(geoNetwork.continent) == T, "unknown", geoNetwork.continent)),
            pageviews = ifelse(is.na(totals.pageviews) == T, 0, totals.pageviews),
            bounces = ifelse(is.na(totals.bounces) == T, 0, totals.bounces),
            medium = factor(ifelse(is.na(trafficSource.medium) == T, "unknown", trafficSource.medium)),
            campaign = factor(ifelse(is.na(trafficSource.campaign) == T, 0, 1)),
            #keyword = trafficSource.keyword,
            channel = factor(channelGrouping),
            #visitor = fullVisitorId,
            source = factor(case_when(trafficSource.source %in% train[grep("google", trafficSource.source),]$trafficSource.source ~ "google",
                               trafficSource.source %in% train[grep("(direct)", trafficSource.source),]$trafficSource.source ~ "direct",
                               trafficSource.source %in% train[grep("facebook", trafficSource.source),]$trafficSource.source ~ "facebook",
                               trafficSource.source %in% train[grep("android", trafficSource.source),]$trafficSource.source ~ "android",
                               trafficSource.source %in% train[grep("bing", trafficSource.source),]$trafficSource.source ~ "bing",
                               trafficSource.source %in% train[grep("baidu", trafficSource.source),]$trafficSource.source ~ "baidu",
                               trafficSource.source %in% train[grep("yahoo", trafficSource.source),]$trafficSource.source ~ "yahoo",
                               trafficSource.source %in% train[grep("Partners", trafficSource.source),]$trafficSource.source ~ "partners",
                               trafficSource.source %in% train[grep("ask", trafficSource.source),]$trafficSource.source ~ "ask",
                               TRUE ~ "other")),
            hour = hour(as_datetime(train$visitStartTime)),
            month = month(as_datetime(train$visitStartTime)),
            day = factor(weekdays(as_datetime(train$visitStartTime))))


newdat <- train %>% 
  transmute(revenue = ifelse(is.na(log(totals.transactionRevenue)) == T, 0, log(totals.transactionRevenue)),
            is.paid = factor(ifelse(revenue > 0, 1, 0)),
            browser = factor(case_when(device.browser == "Chrome" ~ "chrome",
                                       device.browser %in% c("Safari", "Safari (in-app)") ~ "safari",
                                       device.browser == "Firefox" ~ "firefox",
                                       device.browser %in% c("Edge", "Internet Explorer") ~ "edge",
                                       device.browser %in% c("Opera", "Opera Mini") ~ "opera",
                                       TRUE ~ "other")),
            os = factor(case_when(device.operatingSystem == "Windows" ~ "windows",
                                  device.operatingSystem == "Mackintosh" ~ "mackintosh",
                                  device.operatingSystem == "Android" ~ "android",
                                  device.operatingSystem == "iOS" ~ "ios",
                                  device.operatingSystem  == "Linux" ~ "linux",
                                  device.operatingSystem  == "ChromeOS" ~ "chromeos",
                                  is.na(device.operatingSystem) == T ~ "unknown",
                                  TRUE ~ "other")),
            device = factor(device.deviceCategory),
            continent = factor(ifelse(is.na(geoNetwork.continent) == T, "unknown", geoNetwork.continent)),
            pageviews = ifelse(is.na(totals.pageviews) == T, 0, totals.pageviews),
            bounces = ifelse(is.na(totals.bounces) == T, 0, totals.bounces),
            medium = factor(ifelse(is.na(trafficSource.medium) == T, "unknown", trafficSource.medium)),
            campaign = factor(ifelse(is.na(trafficSource.campaign) == T, 0, 1)),
            #keyword = trafficSource.keyword,
            channel = factor(channelGrouping),
            #visitor = fullVisitorId,
            source = factor(case_when(trafficSource.source %in% train[grep("google", trafficSource.source),]$trafficSource.source ~ "google",
                                      trafficSource.source %in% train[grep("(direct)", trafficSource.source),]$trafficSource.source ~ "direct",
                                      trafficSource.source %in% train[grep("facebook", trafficSource.source),]$trafficSource.source ~ "facebook",
                                      trafficSource.source %in% train[grep("android", trafficSource.source),]$trafficSource.source ~ "android",
                                      trafficSource.source %in% train[grep("bing", trafficSource.source),]$trafficSource.source ~ "bing",
                                      trafficSource.source %in% train[grep("baidu", trafficSource.source),]$trafficSource.source ~ "baidu",
                                      trafficSource.source %in% train[grep("yahoo", trafficSource.source),]$trafficSource.source ~ "yahoo",
                                      trafficSource.source %in% train[grep("Partners", trafficSource.source),]$trafficSource.source ~ "partners",
                                      trafficSource.source %in% train[grep("ask", trafficSource.source),]$trafficSource.source ~ "ask",
                                      TRUE ~ "other")),
            hour = hour(as_datetime(train$visitStartTime)),
            month = month(as_datetime(train$visitStartTime)),
            day = factor(weekdays(as_datetime(train$visitStartTime))))

#save(dat, file = "./data/cleaned.Rdata")
#save(newdat, file = "./data/oldcleaned.Rdata")

# Paying Customers
paid <- dat %>% filter(revenue > 0)

# Non-paying Customers
free <- dat %>% filter(revenue == 0)


#Exploratory Data Analysis

# Average Pageviews per user for each device (Paid vs Free)

aggregate(pageviews~device, data = paid, mean)
aggregate(pageviews~device, data = free, mean)

a <- ggplot(aggregate(pageviews~device, data = paid, mean), aes(device, pageviews)) +
  geom_bar(stat = "identity") + labs(title = "Pageviews per user per device (Paid)")
b <- ggplot(aggregate(pageviews~device, data = free, mean), aes(device, pageviews)) +
  geom_bar(stat = "identity") + labs(title = "Pageviews per user per device (Free)")
grid.arrange(a,b, ncol = 2)


# Busiest month of the year (Paid vs Free)

aggregate(pageviews~month, data = paid, mean) # August: busiest; May: least busy
aggregate(pageviews~month, data = free, mean) # August: busiest; Nov: least busy

a <- ggplot(aggregate(pageviews~month, data = paid, mean), aes(factor(month), pageviews)) +
  geom_bar(stat = "identity") + labs(title = "Pageviews per user per month (Paid)")
b <- ggplot(aggregate(pageviews~month, data = free, mean), aes(factor(month), pageviews)) +
  geom_bar(stat = "identity") + labs(title = "Pageviews per user per month (Free)")
grid.arrange(a,b, ncol = 2)

# Busiest weekday (Paid vs Free)

aggregate(pageviews~day, data = paid, mean) 
aggregate(pageviews~day, data = free, mean) 

a <- ggplot(aggregate(pageviews~day, data = paid, mean), aes(day, pageviews)) +
  geom_bar(stat = "identity") + labs(title = "Pageviews per user per day of the week (Paid)")
b <- ggplot(aggregate(pageviews~day, data = free, mean), aes(day, pageviews)) +
  geom_bar(stat = "identity") + labs(title = "Pageviews per user per day of the week (Free)")
grid.arrange(a,b, ncol = 2)


# Busiest Hour (Paid vs Free)

aggregate(pageviews~hour, data = paid, mean) 
aggregate(pageviews~hour, data = free, mean) 

a <- ggplot(aggregate(pageviews~hour, data = paid, mean), aes(factor(hour), pageviews)) +
  geom_bar(stat = "identity") + labs(title = "Pageviews per user per hour of day (Paid)")
b <- ggplot(aggregate(pageviews~hour, data = free, mean), aes(factor(hour), pageviews)) +
  geom_bar(stat = "identity") + labs(title = "Pageviews per user per hour of day (Free)")
grid.arrange(a,b, ncol = 2)


# Median revenue earned per device

aggregate(revenue~device, data = paid, median)

# Median revenue earned per month

aggregate(revenue~month, data = paid, median)

# Median revenue earned per day of the week

aggregate(revenue~day, data = paid, median)

# Median revenue earned per hour of day

aggregate(revenue~hour, data = paid, median)


# Plots

# Looking at the distribution of hits by channel for paying customers vs free customers

ggplot(dat, aes(channel,pageviews,fill = channel)) + 
  geom_bar(stat = "identity") +
  facet_wrap(~ifelse(!is.na(revenue) == T, "Paid", "Free"), scales = "free") +
  ggtitle("Pageviews by channel") +
  coord_flip() +
  theme(axis.text.x=element_text(angle = -45, hjust = 0))

# Looking at the distribution of hits by device for paying customers vs free customers

ggplot(dat, aes(device,pageviews,fill = device)) + 
  geom_bar(stat = "identity") +
  facet_wrap(~ifelse(!is.na(revenue) == T, "Paid", "Free"), scales = "free") +
  ggtitle("Pageviews by Device") +
  coord_flip() +
  theme(axis.text.x=element_text(angle = -45, hjust = 0))







