library(shiny)
library(dplyr)
library(readxl)
library(ggplot2)

mydata <- read_xlsx("raw_data.xlsx")
colnames(mydata) <- c("manager", "region", "city", "item", "dos", "customer",
                      "s_volume", "sales", "cost", "income")
by_matrix <- mydata %>% 
    group_by(region, item) %>% 
    summarise(sales = sum(sales)) %>% 
    mutate(weight = sales / sum(sales))

by_man <- mydata %>% group_by(manager) %>% summarise(sales = sum(sales))

function (input, output) ({
    selected_data <- reactive({
        mydata %>% filter(manager == input$managers,
                          region == input$regions,
                          item == input$items)
    })
    
    by_manager <- reactive ({
        mydata %>% filter(manager == input$managers)
    })
    
    model <- reactive({
        lm(sales ~ dos, selected_data())
    })
    
    by_man <- reactive({
        mydata %>%
            filter(item == input$items) %>% 
            group_by(manager) %>% 
            summarise(sales = sum(sales))
    })
    
    by_regions <- reactive ({
        mydata %>% 
            filter(region == input$regions)
    })
    
    model2 <- reactive({
        lm(sales ~ dos, by_regions())
    })
    
    output$plot1 <- renderPlot({
        par(mfrow = c(2, 2))
        plot(selected_data()$dos, 
             selected_data()$sales, 
             main = "Продажи по параметрам",
             xlab = "", 
             ylab = "выручка", 
             frame = F, 
             col = "steelblue",
             pch = 19)
        abline(model(),
               lwd = 2,
               col = "blue",
               lty = 2)
        plot(by_manager()$dos,
             by_manager()$sales,
             main = "Продажи по менеджеру",
             xlab = "",
             ylab = "выручка",
             col = as.factor(by_manager()$region),
             frame = F,
             pch = 19)
        legend("topleft",
               legend = as.factor(unique(by_manager()$region)),
               col = c("red", "black"),
               pch = 19,
               cex = 0.7)
        pie(by_man()$sales, 
            labels = by_man()$manager, 
            radius = 1,
            col = c("#999999", "#E69F00", "#56B4E9"),
            main = "Вклад менеджеров")
        plot(by_regions()$dos,
             by_regions()$sales,
             main = "Продажи по регионам",
             xlab = "",
             ylab = "выручка",
             col = as.factor(by_regions()$item),
             frame = F,
             pch = 19)
        abline(model2(),
               lwd = 2,
               col = "blue",
               lty = 2)
        legend("topleft",
               legend = as.factor(unique(by_regions()$item)),
               col = c("red", "black", "green", "blue"),
               pch = 19,
               cex = 0.7)
        
    })
    
    output$plot2 <- renderPlot({
        ggplot(by_matrix) +
            geom_point(aes(region, item, col = weight, size = sales)) +
            theme_light() + xlab("") + ylab("") +
            scale_color_continuous(low = "green", high = "red") +
            ggtitle("Матрица продаж")
    })
})
