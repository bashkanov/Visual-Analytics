library(shiny)
library(shinydashboard)
library(readr)
library(magrittr)
library(dplyr)
library(DT)
source("global.R")

start_year = 2008
end_year = 2016

ui <- dashboardPage(
  
  dashboardHeader(title = "Visual Analytics Project 1"),
  
  dashboardSidebar(
                   sidebarMenu(
                     menuItem("1nd point requirements", tabName = "1nd_point"),
                     menuItem("2nd point requirements", tabName = "2nd_point"),
                     menuItem("3nd point requirements", tabName = "3nd_point"),
                     menuItem("Original dataset", tabName = "data_set")
                   ),
                   selectInput(inputId = "league",
                               label = h3("Select the league:"),
                               choices = league_names, 
                               selected = league_names[1]),
                   sliderInput("year_slider", h3("Select the year:"), 
                               min = start_year, 
                               max = end_year,
                               value = start_year,
                               step = 1,
                               sep = "")
                   ),
  
  dashboardBody(
    
    # Access to ccs styles
    #
    # tags$style(HTML("
    #                                                    .col-sm-auto {
    #                 padding-left: 15px;
    #                 padding-right: 15px;
    #                 }
    #                 ")),
    
    tabItems(
      tabItem(tabName = "1nd_point",
              fluidRow(
                box(
                  DTOutput('tbl1', height = "500px")
                ),
                box(
                  plotOutput(outputId = "main_plot", height = "500px")
                )
              )
      ),
      tabItem(tabName = "2nd_point",
              fluidRow(
                tabBox(
                  tabPanel("Distribution plot", 
                           plotOutput(outputId = "distribution_plot", height = "500px")
                           ),
                  tabPanel("Density plot", 
                           plotOutput(outputId = "density_plot", height = "500px")
                           )
                ),
                box(height = "564px", 
                  plotOutput(outputId = "proportion_plot", height = "535px")
                )
              ),
              fluidRow(
                box(
                  width = "12",
                  DTOutput('tbl2')
                )
              )
      ),
      tabItem(tabName = "3nd_point",
              conditionalPanel( condition = "output.cond_plot",
                                fluidRow(
                                  box(width = "12",
                                    plotOutput(outputId = "poss_plot", height = "400px")
                                  )
                                )
              ),
              conditionalPanel( condition = "output.cond_text",
                                fluidRow(
                                  box(
                                    textOutput("not_aval")
                                  )
                                )
              ),
              fluidRow(
                box(
                  plotOutput(outputId = "line_plot", height = "400px")
                )
              )
      ), 
      tabItem(tabName = "data_set",
              fluidRow(
                box(
                  width = "12",
                  DTOutput('full_tbl')
                )
              )  
      )
    )
  )
)

