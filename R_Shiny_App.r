
#------------------------------ Capitalone Data Challenge -------------------------

# This code is to build an interactive tool using Shiny R to present it to any non
# technical professional to give him a fast and easy to use tool to get started with
# understanding the data.

# Packages used for this App
library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(shinythemes)
library(sqldf)
library(DT)


# Loading the required data set

mydata = read_csv('Merged_data.csv',na = ('NA'))

# Converting the data types of required variables

mydata$As_of_Year = as.factor(mydata$As_of_Year)
mydata$Respondent_State_TS = as.factor(mydata$Respondent_State_TS)
mydata$Conv_Conf_status = as.factor(mydata$Conv_Conf_status)
mydata$Respondent_Name_TS = as.factor(mydata$Respondent_Name_TS)
mydata$Lien_Status_Description = as.factor(mydata$Lien_Status_Description)

# The code below is the server function which contains all the backend visulization
# styles needed for the app to run

server = function(input, output,session) {
  output$yeartrendplot = renderPlot({
    mydata = mydata[(mydata$As_of_Year == input$As_of_Year)&(mydata$Respondent_State_TS == input$Respondent_State_TS)&(mydata$Conv_Conf_status == input$Conv_Conf_status),]
    ggplot(mydata,aes(x = factor(As_of_Year),y = Loan_Amount_000))+stat_summary(aes(fill = factor(As_of_Year)), fun.y=sum, geom="bar")+stat_summary(aes(label=round(..y..,2)), fun.y=sum, geom="text", size=4,vjust = -0.5)+scale_fill_brewer(palette="Set3")+xlab('Year')+ylab('Total Loan Amount')+ggtitle('Plot between Year and Loan amount')
    
  })
  output$Stateplot = renderPlot({
    mydata = mydata[(mydata$As_of_Year == input$As_of_Year)&(mydata$Respondent_State_TS == input$Respondent_State_TS)&(mydata$Conv_Conf_status == input$Conv_Conf_status),]
    ggplot(mydata,aes(x = factor(Respondent_State_TS),y = Loan_Amount_000))+stat_summary(aes(fill = factor(Respondent_State_TS)), fun.y=sum, geom="bar")+stat_summary(aes(label=round(..y..,2)), fun.y=sum, geom="text", size=4,vjust = -0.5)+scale_fill_brewer(palette="Set1")+xlab('State')+ylab('Total Loan Amount')+ggtitle('Plot between State and Loan amount')

  })
    output$ConvConfstatus = renderPlot({
      mydata = mydata[(mydata$As_of_Year == input$As_of_Year)&(mydata$Respondent_State_TS == input$Respondent_State_TS)&(mydata$Conv_Conf_status == input$Conv_Conf_status),]
    ggplot(mydata,aes(x = factor(Conv_Conf_status),y = Loan_Amount_000))+stat_summary(aes(fill = factor(Conv_Conf_status)), fun.y=sum, geom="bar")+stat_summary(aes(label=round(..y..,3)), fun.y=sum, geom="text", size=4,vjust = -0.5)+scale_fill_brewer(palette="Set2")+xlab('Coventional Conforming Status')+ylab('Total Loan Amount')+ggtitle('Plot between Conventional Conforming status and Loan amount')

  })
    output$Riskplot = renderPlot({
      mydata = mydata[(mydata$As_of_Year == input$As_of_Year)&(mydata$Respondent_State_TS == input$Respondent_State_TS)&(mydata$Conv_Conf_status == input$Conv_Conf_status)&(mydata$Lien_Status_Description == input$Lien_Status_Description),]
      ggplot(mydata,aes(x = reorder(x = Lien_Status_Description,-Loan_Amount_000),y = Loan_Amount_000))+stat_summary(aes(fill = Lien_Status_Description), fun.y=sum, geom="bar")+stat_summary(aes(label=round(..y..,2)), fun.y=sum, geom="text", size=4,vjust = -0.5)+scale_fill_brewer(palette="Set2")+xlab('Risk of the loan')+ylab('Average Loan Amount')+ggtitle('Plot between Risk of the loan and Loan amount')
      
      
    })
    
}

# The UI function contains all the styles of tabs, texts, drop down boxes and many
# more pannels through which one can select options that in turn are reflected in the 
# visualizations 

ui = fluidPage(
  titlePanel("Capitalone Data Challenge"),
  
  sidebarLayout(
    sidebarPanel( selectInput('As_of_Year',"Year :",choices = levels(mydata$As_of_Year),multiple = T)
                  ,selectInput('Respondent_State_TS',"State :",choices = levels(mydata$Respondent_State_TS),multiple = T)
                  ,selectInput('Conv_Conf_status',"Conventional Conforming status :",choices = levels(mydata$Conv_Conf_status),multiple = T)
                  ,selectInput('Lien_Status_Description',"Loan risky?:",choices = levels(mydata$Lien_Status_Description),multiple = T)
                   ),
    mainPanel(plotOutput('yeartrendplot')
              ,plotOutput('Stateplot')
              ,plotOutput('ConvConfstatus')
              ,plotOutput('Riskplot')
    )
  )
)

# The code below will help us run the app and start using it right away.
shinyApp(ui = ui, server = server)