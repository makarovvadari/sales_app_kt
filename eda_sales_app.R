# Exploratory data analysis

# Setup

library(dplyr)
library(ggplot2)
library(lubridate)
library(readxl)

# Working directory

project_folder <- "/home/daria/Документы/GitHub/sales_app_kt/"
setwd(project_folder)

# Read raw data

mydata <- read_xlsx("raw_data.xlsx")

# Rename columns

colnames(mydata) <- c("manager", "region", "city", "item", "dos", "customer",
                      "s_volume", "sales", "cost", "income")

# Descriptive statistics

summary(mydata)

# Histograms of numeric features

par(mfrow= c(2, 2))
hist(mydata$s_volume, col = "steelblue", main = "Объем продаж",
     xlab = "объем", ylab = "")
hist(mydata$sales, col = "skyblue", main = "Выручка", 
     xlab = "выручка", ylab = "")
hist(mydata$cost, col = "darkgreen", main = "Затраты на продажи",
     xlab = "сумма", ylab = "")
hist(mydata$income, col = "lightgreen", main = "Прибыль", xlab = "сумма",
     ylab = "")
par(mfrow = c(1, 1))

# Check how many unique values in character features

length(unique(mydata$manager))          # 3 менеджера
length(unique(mydata$region))           # 6 областей
length(unique(mydata$city))             # 35 городов
length(unique(mydata$item))             # 4 товара
length(unique(mydata$customer))         # 27 покупателей

# Sales by managers

man <- mydata %>% filter(manager == "Менеджер 1") %>% select(dos, region, sales)
man$region <- as.factor(man$region)
plot(man$dos, man$sales, col = man$region, frame = F, pch = 19)
legend("topleft", legend = as.factor(unique(man$region)), 
       col = c("red", "black"), pch = 19, cex = 0.8)

model <- lm(man$sales ~ man$dos, man)
abline(model, lwd = 2, col = "blue", lty = 2)

mydata %>% 
        group_by(region, item) %>% 
        summarise(sales = sum(sales)) %>% 
        mutate(weight = sales / sum(sales)) %>% 
        ggplot() +
        geom_point(aes(region, item, col = weight, size = sales)) +
        theme_light() + xlab("") + ylab("") +
        scale_color_continuous(low = "green", high = "red") +
        ggtitle("Sales matrix")

by_man <- mydata %>% group_by(manager) %>% summarise(sales = sum(sales))
pie(by_man$sales, labels = by_man$manager, radius = 1,
    col = c("#999999", "#E69F00", "#56B4E9"))


