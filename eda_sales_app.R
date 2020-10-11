# Разведочный анализ данных

# Настройка

library(dplyr)
library(ggplot2)
library(lubridate)
library(readxl)

# Устанавливаю рабочую папку

project_folder <- "/home/daria/Документы/GitHub/sales_app_kt/"
setwd(project_folder)

# Читаю сырые данные

mydata <- read_xlsx("raw_data.xlsx")

# Переименовываю колонки

colnames(mydata) <- c("manager", "region", "city", "item", "dos", "customer",
                      "s_volume", "sales", "cost", "income")

# Описательные статистики

summary(mydata)

# Manager, region, city, item, customer -- текстовые колонки
# DOS -- с 01.01.2018 по 24.02.2020
# s_volume -- от 1 до 168
# sales, cost, income -- числовые данные

# Сделаем гистограмму по числовым данным

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

# Проверим сколько уникальных значений в текстовых колонках

unique(mydata$manager)          # 3 менеджера
unique(mydata$region)           # 6 областей
length(unique(mydata$city))     # 35 городов
unique(mydata$item)             # 4 товара
length(unique(mydata$customer)) # 27 покупателей

# Продажи по дням

mydata$manager <- as.factor(mydata$manager)
plot(x = mydata$dos, y = mydata$sales, frame = F, main = "Продажи по дням",
     xlab = "", ylab = "выручка", col = mydata$manager, pch = 19)

# Добавляю модель продаж по всем менеджерам

ggplot(mydata) + 

