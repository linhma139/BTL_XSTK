rm(list = ls())
#install.packages(c('readr', 'ggplot2', 'tidyverse', 'here', 'mice'))
#install.packages("mice")
#install.packages("magrittr") # package installations are only needed the first time you use it
#install.packages("dplyr")    # alternative installation of the %>%
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
library(mice)
library(here)
library(tidyverse)
library(VIM)
library(readr)
library(zoo)
library(car)
source("functions.R")


CPU <- read_csv("Intel_CPUs.csv")

# REMOVE DUPLICATES
#CPU <- distinct(CPU)

# CHOOSE RELEVANT VARIABLES
CPU_Filtered <- CPU %>% 
  select(
    Vertical_Segment,
    Launch_Date,
    Lithography,
    Recommended_Customer_Price,
    nb_of_Cores,
    nb_of_Threads,
    Processor_Base_Frequency,
    Embedded_Options_Available,
    Max_nb_of_Memory_Channels,
    Instruction_Set
  )

# FORMAT MISSING DATA
CPU_Filtered[CPU_Filtered == ""] <- NA
CPU_Filtered[CPU_Filtered == "N/A"] <- NA

# FORMAT DIRTY DATA
sapply(CPU_Filtered,unique) #xem các loại dữ liệu trong từng cột
#View(CPU_Filtered)

# PERCENTAGE OF MISSING DATA
missingPercentage <- apply(is.na(CPU_Filtered),2,mean)
#View(missingPercentage)

columnToRemove <- missingPercentage[missingPercentage < 0.1]
columnToRemove <- names(columnToRemove)

columnToFill <- missingPercentage[missingPercentage >= 0.1]
columnToFill <- names(columnToFill)

# REMOVE MISSING DATA IN SELECTED COLUMN
CPU_Filtered <- CPU_Filtered %>% 
  filter(complete.cases(across(all_of(columnToRemove))))

#DIRTY DATA BELOW 10% IS FILTERED
apply(is.na(CPU_Filtered),2,mean)

# LAUNCH DATE
CPU_Filtered <- CPU_Filtered %>% 
  mutate(Launch_Date = substr(Launch_Date,nchar(Launch_Date)-1,nchar(Launch_Date))) %>% 
  mutate(Launch_Date = as.numeric(Launch_Date)) %>% 
  mutate(Launch_Date = ifelse(Launch_Date < 24,2000 + Launch_Date,1900 + Launch_Date))
#View(CPU_Filtered)

# LITHOGRAPHY
CPU_Filtered <- CPU_Filtered %>% 
  mutate(Lithography = gsub(" nm","",Lithography)) %>% 
  mutate(Lithography = as.numeric(Lithography))
#View(CPU_Filtered)

# RECOMMENDED_CUSTORMER_PRICE
CPU_Filtered <- CPU_Filtered %>% 
  mutate(Recommended_Customer_Price = gsub("\\$","",Recommended_Customer_Price)) %>% 
  mutate(Recommended_Customer_Price = gsub(",","",Recommended_Customer_Price)) %>%
  mutate(Recommended_Customer_Price = sapply(Recommended_Customer_Price,priceTransform)) %>% 
  mutate(Recommended_Customer_Price = as.numeric(Recommended_Customer_Price))
#View(CPU_Filtered)

# PROCESSOR_BASE_FREQUENCY
CPU_Filtered <- CPU_Filtered %>%
  mutate(Processor_Base_Frequency = gsub("GHz","",Processor_Base_Frequency)) %>%
  mutate(Processor_Base_Frequency = sapply(Processor_Base_Frequency,freqTransform)) %>%
  mutate(Processor_Base_Frequency = as.numeric(Processor_Base_Frequency))
#View(CPU_Filtered)
#plotTest(CPU_Filtered,columnToFill,columnToFill)

# IMPUTATION

#mice
# method <- mice(data = CPU_Filtered[, columnToFill], method = 'pmm', printFlag = FALSE, seed = 1889782160)
# imputed_data <- mice::complete(method)
#test
# plotTest(CPU_Filtered,imputed_data,columnToFill)
#test
# CPU_Filtered[, columnToFill] <- imputed_data[, columnToFill]

#median
# CPU_Filtered1 <- CPU_Filtered %>%
#   mutate(across(all_of(columnToFill), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))
# plotTest(CPU_Filtered,CPU_Filtered1,columnToFill)

#mean
# CPU_Filtered2 <- CPU_Filtered %>%
#   mutate(across(all_of(columnToFill), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
# plotTest(CPU_Filtered,CPU_Filtered2,columnToFill)

#GT phia trc
# CPU_Filtered3 <- CPU_Filtered %>%
#   mutate(across(all_of(columnToFill), ~ na.locf(., na.rm = FALSE)))
# plotTest(CPU_Filtered,CPU_Filtered3,columnToFill)

#GT phia sau
# CPU_Filtered4 <- CPU_Filtered %>%
#   mutate(across(all_of(columnToFill), ~ na.locf(., na.rm = FALSE, fromLast = TRUE)))
# plotTest(CPU_Filtered,CPU_Filtered4,columnToFill)

# Sử dụng hàm na.locf() để điền các giá trị thiếu của cột "Launch_Date" bằng các giá trị phía sau
CPU_Filtered1 <- CPU_Filtered %>%
  mutate(Launch_Date = na.locf(Launch_Date, na.rm = FALSE, fromLast = TRUE))

# Vẽ biểu đồ để kiểm tra kết quả nếu cần
#plotTest(CPU_Filtered, CPU_Filtered1, columnToFill)

#Lọc các cột còn lại của columnToFill bằng trung vị
CPU_Filtered2 <- CPU_Filtered %>%
  mutate(across(all_of(setdiff(columnToFill, "Launch_Date")), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

# Vẽ biểu đồ để kiểm tra kết quả nếu cần
#plotTest(CPU_Filtered, CPU_Filtered2, columnToFill)

CPU_Filtered$Launch_Date = CPU_Filtered1$Launch_Date
CPU_Filtered$Recommended_Customer_Price = CPU_Filtered2$Recommended_Customer_Price
CPU_Filtered$nb_of_Threads = CPU_Filtered2$nb_of_Threads
CPU_Filtered$Max_nb_of_Memory_Channels = CPU_Filtered2$Max_nb_of_Memory_Channels


#------------------------------------------Bài 1-2 : LÍ THUYẾT KIỂM ĐỊNH------------------------------------------------------------------------------------#
# Perform Shapiro-Wilk test for normality on Recommended_Customer_Price column
shapiro_test_result <- shapiro.test(CPU_Filtered$Recommended_Customer_Price)

# Print the test result
print(shapiro_test_result)

# Perform one-sample t-test
t_test_result <- t.test(CPU_Filtered$Recommended_Customer_Price, mu = 278.95, alternative = "less")

# Print the test result
print(t_test_result)

# Split the dataset into two samples: Desktop and Mobile
desktop_sample <- CPU_Filtered %>% filter(Vertical_Segment == "Desktop")
mobile_sample <- CPU_Filtered %>% filter(Vertical_Segment == "Mobile")

# Count the number of rows in desktop_sample
num_rows_desktop <- nrow(desktop_sample)

# Print the result
print(num_rows_desktop)

# Count the number of rows in desktop_sample
num_rows_mobile <- nrow(mobile_sample)

# Print the result
print(num_rows_mobile)

# Perform Shapiro-Wilk test for normality on Recommended_Customer_Price for Desktop sample
shapiro_desktop <- shapiro.test(desktop_sample$Recommended_Customer_Price)
print("Shapiro-Wilk test for Desktop sample:")
print(shapiro_desktop)

# Perform Shapiro-Wilk test for normality on Recommended_Customer_Price for Mobile sample
shapiro_mobile <- shapiro.test(mobile_sample$Recommended_Customer_Price)
print("Shapiro-Wilk test for Mobile sample:")
print(shapiro_mobile)

# Compare the variances of two samples using var.test()
var_test_result <- var.test(desktop_sample$Recommended_Customer_Price, mobile_sample$Recommended_Customer_Price)
print("Variance Test:")
print(var_test_result)

# Perform t-test to compare means of two samples
t_test_result <- t.test(desktop_sample$Recommended_Customer_Price, mobile_sample$Recommended_Customer_Price)
print("T-test:")
print(t_test_result)
#--------------------------------------------------------------------------------------------------------------------------------------------------------------#

#------------------------------------------Bài 3 : KIỂM ĐỊNH ANOVA 1 YẾU TỐ------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------------------------------------------------------------------#


#------------------------------------------Bài 4 : HỒI QUY TUYẾN TÍNH BỘI ------------------------------------------------------------------------------------#

#kiểm tra độ phụ thuộc mỗi biến
pairs(CPU_Filtered$Recommended_Customer_Price ~ CPU_Filtered$nb_of_Cores + CPU_Filtered$nb_of_Threads + CPU_Filtered$Max_nb_of_Memory_Channels  + CPU_Filtered$Lithography + CPU_Filtered$Launch_Date)
#setup mô hình
Model <- subset(CPU_Filtered,select = c(Recommended_Customer_Price , nb_of_Cores , nb_of_Threads , Lithography ,Launch_Date , Max_nb_of_Memory_Channels , Processor_Base_Frequency))
price.lm  = lm(Recommended_Customer_Price ~ nb_of_Cores + nb_of_Threads +  Launch_Date + Lithography ,data = Model)
summary(price.lm)
confint(price.lm)

#kiểm tra đa cộng tuyến
vif(price.lm)
#kiểm tra các giả định của mô hình hồi quy
par(mfrow=c(2,2))
plot(price.lm , pch = 20)
#dự đoán
predict(price.lm , newdata = data.frame(
  nb_of_Cores = mean(Model$nb_of_Cores),
  nb_of_Threads = mean(Model$nb_of_Threads),
  Lithography = mean(Model$Lithography),
  Launch_Date = mean(Model$Launch_Date)),
  interval = "confidence"
)
#--------------------------------------------------------------------------------------------------------------------------------------------------------------#
