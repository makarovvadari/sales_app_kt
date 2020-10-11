library(shiny)
library(dplyr)
library(readxl)

# Prepare raw data to extract input variables

mydata <- read_xlsx("raw_data.xlsx")
colnames(mydata) <- c("manager", "region", "city", "item", "dos", "customer",
                      "s_volume", "sales", "cost", "income")

managers <- as.factor(unique(mydata$manager))
regions <- as.factor(unique(mydata$region))
items <- as.factor(unique(mydata$item))

pageWithSidebar(
    headerPanel("Анализ продаж"),
    sidebarPanel(
        selectInput("managers", "Менеджер", managers),
        selectInput("regions", "Регион", regions),
        selectInput("items", "Товар", items)
    ),
    mainPanel(
        plotOutput('plot1'),
        plotOutput('plot2')
    )
)