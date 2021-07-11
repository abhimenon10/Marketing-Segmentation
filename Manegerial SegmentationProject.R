getwd()
setwd("C:\\Users\\abhit\\OneDrive\\Documents\\Uni\\Marketing analytics") 
#Reading the Data
Seg1 = read.delim(file = 'rfm_data.txt', header = FALSE, sep = '\t', dec = '.')
#looking at the imported data
View(Seg1)
colnames(Seg1) = c('customer_id', 'purchase_amount', 'date_of_purchase')
Seg1$date_of_purchase = as.Date(Seg1$date_of_purchase, "%Y-%m-%d")
Seg1$year_of_purchase = as.numeric(format(Seg1$date_of_purchase, "%Y"))
Seg1$days_since       = as.numeric(difftime(time1 = "2016-01-01",
                                            time2 = Seg1$date_of_purchase,
                                            units = "days"))

head(Seg1)

library(sqldf)

# Compute recency, frequency, and Average purchase amount
customers_2015 = sqldf("SELECT customer_id,
                               MIN(days_since) AS 'recency',
                               MAX(days_since) AS 'first_purchase',
                               COUNT(*) AS 'frequency',
                               AVG(purchase_amount) AS 'amount'
                        FROM Seg1 GROUP BY 1")

#Creating segments based on our data for the year 2015
customers_2015$segment = "NA"
customers_2015$segment[which(customers_2015$recency > 365*3)] = "inactive"
customers_2015$segment[which(customers_2015$recency <= 365*3 & customers_2015$recency > 365*2)] = "cold"
customers_2015$segment[which(customers_2015$recency <= 365*2 & customers_2015$recency > 365*1)] = "warm"
customers_2015$segment[which(customers_2015$recency <= 365)] = "active"
customers_2015$segment[which(customers_2015$segment == "warm" & customers_2015$first_purchase <= 365*2)] = "new warm"
customers_2015$segment[which(customers_2015$segment == "warm" & customers_2015$amount < 100)] = "warm low value"
customers_2015$segment[which(customers_2015$segment == "warm" & customers_2015$amount >= 100)] = "warm high value"
customers_2015$segment[which(customers_2015$segment == "active" & customers_2015$first_purchase <= 365)] = "new active"
customers_2015$segment[which(customers_2015$segment == "active" & customers_2015$amount < 100)] = "active low value"
customers_2015$segment[which(customers_2015$segment == "active" & customers_2015$amount >= 100)] = "active high value"
table(customers_2015$segment)
aggregate(x = customers_2015[, 2:5], by = list(customers_2015$segment), mean)



#Creating segments based on our data for the year 2014

# Compute recency, frequency, and average purchase amount
customers_2014 = sqldf("SELECT customer_id,
                               MIN(days_since) - 365 AS 'recency',
                               MAX(days_since) - 365 AS 'first_purchase',
                               COUNT(*) AS 'frequency',
                               AVG(purchase_amount) AS 'amount'
                        FROM Seg1
                        WHERE days_since > 365
                        GROUP BY 1")


customers_2014$segment = "NA"
customers_2014$segment[which(customers_2014$recency > 365*3)] = "inactive"
customers_2014$segment[which(customers_2014$recency <= 365*3 & customers_2014$recency > 365*2)] = "cold"
customers_2014$segment[which(customers_2014$recency <= 365*2 & customers_2014$recency > 365*1)] = "warm"
customers_2014$segment[which(customers_2014$recency <= 365)] = "active"
customers_2014$segment[which(customers_2014$segment == "warm" & customers_2014$first_purchase <= 365*2)] = "new warm"
customers_2014$segment[which(customers_2014$segment == "warm" & customers_2014$amount < 100)] = "warm low value"
customers_2014$segment[which(customers_2014$segment == "warm" & customers_2014$amount >= 100)] = "warm high value"
customers_2014$segment[which(customers_2014$segment == "active" & customers_2014$first_purchase <= 365)] = "new active"
customers_2014$segment[which(customers_2014$segment == "active" & customers_2014$amount < 100)] = "active low value"
customers_2014$segment[which(customers_2014$segment == "active" & customers_2014$amount >= 100)] = "active high value"
table(customers_2014$segment)

#comparing 2014 segmentation with 2015
?pie
pie(table(customers_2014$segment)) #2014

pie(table(customers_2015$segment)) #2015

#calculating revenue for each segment in the year 2015
revenue_2015 = sqldf("SELECT customer_id, SUM(purchase_amount) AS 'revenue_2015'
                      FROM Seg1
                      WHERE year_of_purchase = 2015
                      GROUP BY customer_id")
View(revenue_2015)
summary(revenue_2015)
#merging average revenue of 2015 customer with original table
actual = merge(customers_2015, revenue_2015, all.x = TRUE)
actual$revenue_2015[is.na(actual$revenue_2015)] = 0
#avg revenue per customer per segment
aggregate(x = actual$revenue_2015, by = list(customers_2015$segment), mean)
#merging 2014 and 2015 data
forward = merge(customers_2014, revenue_2015, all.x = TRUE)
forward$revenue_2015[is.na(forward$revenue_2015)] = 0
View(forward)
View(customers_2014)
#showing average revenue per customer per segment
r = aggregate(x = forward$revenue_2015, by = list(customers_2014$segment), mean)
#arranging it in decreasing order
r = r[order(r$x, decreasing = TRUE), ]
print(r)
barplot(r$x, names.arg = r$Group.1)

#based on the results we got we can clearly see that our new active members tend to have a very low revenue in the following year so as a company we have to target these customers in order to keep them 
#this could be done by increasing a promotion or introduction of a points system in order to increase their average spend.
