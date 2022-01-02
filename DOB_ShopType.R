transactionData <- read.csv("C:/Users/Gary/Documents/2021/Winter 2021/QDAA Hackathon/Transactions.csv")
productData <- read.csv("C:/Users/Gary/Documents/2021/Winter 2021/QDAA Hackathon/Products.csv")
customerData <- read.csv("C:/Users/Gary/Documents/2021/Winter 2021/QDAA Hackathon/Customers.csv")


# Format customer data and transaction data for data linkage
# change the column names for customer ID to be cust_id

colnames(customerData) <- c("cust_id", "DOB", "gender", "city_code")

# Merge the two datasets together
mergedCustomerTransaction <- merge(customerData, transactionData, by = "cust_id")

# Sort by consumer ID, gender, age, etc.

mergedCustomerTransaction <- arrange(mergedCustomerTransaction, cust_id)


# Comparing DOB with Store Type

DOB_StoreType <- select(mergedCustomerTransaction, 
                        -c(1, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))

# Graph of e-shop by age
DOB_eShop <- DOB_StoreType[(DOB_StoreType$Store_type == "e-Shop"),]
DOB_eShop[order(as.Date(DOB_eShop$DOB, format = "%m/%d/%Y")),]
a <- DOB_eShop$DOB
yearDOB <- as.integer(substring(a, 7, 10))
DOB_eShop <- mutate(DOB_eShop, yearDOB)
DOB_eShop <- mutate(DOB_eShop, DOB = NULL)
DOB_eShop_Plot <- ggplot(DOB_eShop, aes(x=yearDOB)) + geom_bar(fill = "lightblue")
DOB_eShop_Plot + labs(x = "Birth Year", y = "Frequency") + coord_cartesian(ylim=c(300, 500))

# Graph of Telephone Shopping by age
DOB_TeleShop <- DOB_StoreType[(DOB_StoreType$Store_type == "TeleShop"),]
DOB_TeleShop[order(as.Date(DOB_TeleShop$DOB, format = "%m/%d/%Y")),]
b <- DOB_TeleShop$DOB
yearDOBa <- as.integer(substring(b, 7, 10))
DOB_TeleShop <- mutate(DOB_TeleShop, yearDOBa, DOB = NULL)
DOB_TeleShop_Plot <- ggplot(DOB_TeleShop, aes(x=yearDOBa)) + geom_bar(fill = "lightskyblue1")
DOB_TeleShop_Plot + labs(x = "Birth Year", y = "Frequency") + coord_cartesian(ylim = c(150, 250))


# Graph of flagship store shopping by age
DOB_FlagShip <- DOB_StoreType[(DOB_StoreType$Store_type == "Flagship store"),]
DOB_FlagShip[order(as.Date(DOB_FlagShip$DOB, format = "%m/%d/%Y")),]
c <- DOB_FlagShip$DOB
yearDOBb <- as.integer(substring(c, 7, 10))
DOB_FlagShip <- mutate(DOB_FlagShip, yearDOBb, DOB = NULL)
DOB_FlagShip_Plot <- ggplot(DOB_FlagShip, aes(x=yearDOBb)) + geom_bar(fill = "lightskyblue3")
DOB_FlagShip_Plot + labs(x = "Birth Year", y = "Frequency") + coord_cartesian(ylim = c(150, 300))

#Graph of mortar and brick store by age
DOB_MBR <- DOB_StoreType[(DOB_StoreType$Store_type == "MBR"),]
DOB_MBR[order(as.Date(DOB_MBR$DOB, format = "%m/%d/%Y")),]
d <- DOB_MBR$DOB
yearDOBc <- as.integer(substring(d, 7, 10))
DOB_MBR <- mutate(DOB_MBR, yearDOBc, DOB = NULL)
DOB_MBR_Plot <- ggplot(DOB_MBR, aes(x=yearDOBc)) + geom_bar(fill = "royalblue3")
DOB_MBR_Plot + labs(x = "Birth Year", y = "Frequency") + coord_cartesian(ylim = c(150, 300))


