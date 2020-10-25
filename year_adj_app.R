#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(MASS)
library(data.table)


# data mulnipulation
WI <- fread(file="World Indicators.csv",
            select=c("Country", "Year", "Internet Usage", "CO2 Emissions", "Health Exp % GDP"),
            col.names = c("Country", "Year", "Internet_Usage", "CO2_Emissions", "Health_Exp"))
WI_5C <- WI[WI$Country=="United States"|WI$Country=="Brazil"|WI$Country=="Russian Federation"|WI$Country=="India"|WI$Country=="China",]   

WI_5C <- WI_5C %>% mutate(YEAR=year(as.POSIXct(Year, format = "%d/%m/%Y"))) 

WI_5C <- WI_5C %>% mutate(
    Internet_Usage=as.numeric(unlist(strsplit(WI_5C$Internet_Usage,"%"))),
    Health_Exp=as.numeric(unlist(strsplit(WI_5C$Health_Exp,"%"))),
    CO2_Emissions=CO2_Emissions/1000)

WI_5C=WI_5C[c(1,3,4,5,6)]


colnames(WI_5C) <- c("Country", "Internet_Usage\n(%)","CO2_Emissions\n(thousand)","Health_Expend\n(% of GDP)","Year")

WI_5C_wide <- WI_5C %>% gather(key, value, -Country,-Year)


# app
ui<-fluidPage(
    titlePanel(""),
    sidebarLayout(
        position = "right",
        sidebarPanel(
            sliderInput(
                inputId = "year",
                label = "Select Year Range",
                min = 2000,
                max = 2013,
                value = c(2000,2013),
                sep = ""
            )
        ),
        mainPanel(
            plotOutput(outputId = "lineplot")
        )
    )
)

server<-function(input, output){
    dat<- reactive({
        WI_5C_wide[WI_5C_wide$Year %in% seq(from=min(input$year),to=max(input$year),by=1),]
    })
    
    output$lineplot<-renderPlot({
        ggplot(data=dat(), aes(x = Year, y = value, col=key))+
            geom_line()+
            facet_grid(key~Country, switch= "y", scale = "free")+
            ylab("")+
            xlab("Year")+
            labs(col = "Measurements")+
            scale_color_brewer(palette="Set2")+
            scale_x_continuous(breaks = seq(min(input$year), max(input$year), by = ifelse((max(input$year)-min(input$year))>3, as.integer((max(input$year)-min(input$year))/3),1)))+
            theme_minimal()+
            theme( axis.text.x = element_text(angle=45, hjust=1),
                   legend.key.size = unit(1.5,"cm"))
        
    }, height = 600, width = 1000)
}

shinyApp(ui=ui, server=server)
