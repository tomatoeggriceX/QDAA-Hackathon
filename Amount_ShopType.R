transactionData <- read.csv("C:/Users/Gary/Documents/2021/Winter 2021/QDAA Hackathon/Transactions.csv")
productData <- read.csv("C:/Users/Gary/Documents/2021/Winter 2021/QDAA Hackathon/Products.csv")
customerData <- read.csv("C:/Users/Gary/Documents/2021/Winter 2021/QDAA Hackathon/Customers.csv")

# Total Amount versus Store Type

# Format customer data and transaction data for data linkage
# change the column names for customer ID to be cust_id

colnames(customerData) <- c("cust_id", "DOB", "gender", "city_code")

# Merge the two datasets together
mergedCustomerTransaction <- merge(customerData, transactionData, by = "cust_id")

# Comparing DOB with Store Type

DOB_TotalAmt <- select(mergedCustomerTransaction, 
                        -c(1, 3, 4, 5, 6, 7, 8, 9, 10, 11, 13))

# Change DOB to YOB

DOB_TotalAmt[order(as.Date(DOB_TotalAmt$DOB, format = "%m/%d/%Y")),]
a <- DOB_TotalAmt$DOB
YOB <- as.integer(substring(a, 7, 10))
DOB_TotalAmt <- mutate(DOB_TotalAmt, YOB)
DOB_TotalAmt <- mutate(DOB_TotalAmt, DOB = NULL)

# Amount spent per visit vs DOB

by_year <- group_by(DOB_TotalAmt, YOB)
mean_amt_spent <- summarize(by_year, amtSpent = mean(total_amt, na.rm = TRUE))

ggplot(mean_amt_spent, aes(x = YOB, y = amtSpent), size = 12) +
  geom_line(color = "red") + labs(x = "Year of Birth", y = "Average Amount Spent per Visit")



