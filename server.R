library(shiny)
library(readr)
library(magrittr)
library(dplyr)
library(DT)
library(ggplot2)
library(scales)
source("global.R")

server <- function(input, output) {

  # 1th tab ----------------
  
  output$tbl1 = renderDT(
    league_list[[input$league]], options = list(scrollX=TRUE,lengthChange = FALSE),
    class = 'cell-border stripe',
    rownames = FALSE
  )

  output$main_plot <- renderPlot({
    ggplot(data = stage_list[[input$league]],
           mapping = aes(x = stage, y = mean)) +
      geom_line() +
      geom_point() +
      labs(x = "Stage", y = "Average sum of goals",
           title = "Average sum of goals per stage ",
           subtitle = input$league)
  })
  
  # 2th tab ----------------
  
  output$tbl2 = renderDT(
    league_summary_year(dat, input$league, input$year_slider), 
    options = list(lengthChange = FALSE, scrollX=TRUE),
    class = 'cell-border stripe',
    rownames = FALSE
  )
  
  output$distribution_plot <- renderPlot({
    ggplot(data = win_hist(dat, input$league, input$year_slider),
           aes(x=total_wins, fill=type)) + 
      geom_histogram(binwidth = 0.5) +
      # geom_density(alpha=.3) +
      labs(x = "Wins", y = "Number of teams",
           title = "The distributions of home and away wins",
           subtitle = paste(input$league, as.character(input$year_slider), sep=", ") 
           )
  })
  
  output$density_plot <- renderPlot({
    ggplot(data = win_hist(dat, input$league, input$year_slider),
           aes(x=total_wins, fill=type)) + 
      geom_density(alpha=.3) +
      labs(x = "Wins", y = "Density_plot",
           title = "The distributions of home and away wins",
           subtitle = paste(input$league, as.character(input$year_slider), sep=", ") 
      )
  })
  
  output$proportion_plot <- renderPlot({
    ggplot() + 
      geom_bar(data = goals_proportion(league_summary_year(dat, input$league, input$year_slider)),
              # x = reorder(team, -difference) TO MAKE descending order
               aes(x = reorder(team, difference), y = difference,  
                   fill = cond),
               stat="identity") +
      labs(x = "Teams", y = "Goals",
           title = "Proportion of scored/conceived goals",
           subtitle = paste(input$league, as.character(input$year_slider), sep=", ")) +
      geom_text(data = goals_proportion(league_summary_year(dat, input$league, input$year_slider)),
                aes(y = difference, x = team, label = difference),
                position = position_stack(vjust = 0.5)) + 
      theme(axis.text.x = element_text(angle = 90, size = 10, hjust=0.95, vjust=0.5))
  })
  
  # 3th tab ----------------
  
  output$line_plot <- renderPlot({
    ggplot(data = average_per_month(dat, input$league, input$year_slider),
           mapping = aes(y = mean, x = month)) + 
      geom_line() +
      geom_point() +
      labs(y = "Mean", x = NULL,
           title = "Average number of scored goals",
           subtitle = paste(input$league, as.character(input$year_slider), sep=", "))+
      scale_x_date(
        date_labels = "%B") +
      theme(axis.text.x = element_text(size = 11))    
  })
  
    output$poss_plot <- renderPlot({
      if (availability_check(dat, input$league, input$year_slider)){
        ggplot(data = get_all_possessions(dat, input$league, input$year_slider), 
               mapping = aes(x = possession_breaks , y = goal)) +
          geom_boxplot()+
          scale_y_continuous(breaks=seq(0,10,1)) +
          labs(y = "Goal", x = "Ball possesion",
               title = "Ball possesion over scored  scored goals",
               subtitle = paste(input$league, as.character(input$year_slider), sep=", "))
      }      
    })
    
    output$not_aval <- renderText({ 
      paste("Unfortunately for ", input$league, " in the year " , input$year_slider ,"  the ball possession statistics aren't avaliable.
            Please try another laegue or year!")
    })
    
    # Dynamic UI
    # https://shiny.rstudio.com/articles/dynamic-ui.html
    
    output$cond_plot <- reactive({
      availability_check(dat, input$league, input$year_slider)
    })
    
    output$cond_text <- reactive({
      !availability_check(dat, input$league, input$year_slider)
    })
    
    outputOptions(output, "cond_plot", suspendWhenHidden = FALSE)  
    outputOptions(output, "cond_text", suspendWhenHidden = FALSE)  
    
  
  # 4th tab ----------------
  
  output$full_tbl = renderDT(
    dat, 
    options = list(scrollX=TRUE,
                   columnDefs = list(list(width = '20px', targets = "_all"))
                   ),
    filter = 'top',
    class = 'cell-border stripe',
    rownames = FALSE
  )
    
  
  
}
