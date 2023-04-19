library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(ggplot2)
library(DT)
library(summarytools)
library(htmlTable)
library(ochRe)
library(htmlwidgets)
library(tidyr)

# Load the dataset
df <- read.csv("athlete_events.csv")

# Adding image
title<-tags$a(tags$img(src="r.png",height='50',width='50'),"The Olympics")
# Define the UI
ui <- dashboardPage(skin="yellow",
  dashboardHeader(title = title),
                      dashboardSidebar(
                    selectInput("sport", "Select a Sport:",
                                choices = c("All", unique(df$Sport)),
                                selected = "All"),
                    selectInput("region", "Select a Region:",
                                choices = c("All", unique(df$region)),
                                selected = "All"),
                    selectInput("medal", "Select a Medal Type:",
                                choices = c("All", "Gold", "Silver", "Bronze"),
                                selected = "All"),
                    sliderInput(
                      "Year", label = "Year Range:",
                      min = 1896, value = c(1896,2016) , max = 2016
                    )
                  ),   
                  dashboardBody (
                    
                    tabsetPanel(
                      
                      # Introduction page
                      tabPanel(
                        "Introduction", tags$img(src="img.jpg",height="500px",width="auto"),h1("The Olympic Games R Shiny Dashboard"),
                        p("The 120 Years of Olympic History dataset is a comprehensive collection of athlete and event information from the modern Olympic Games dating back to 1896."),
                        p("It includes data on over 270,000 athletes who have participated in more than 40,000 events across more than 150 countries. This rich and diverse dataset provides a wealth of opportunities for data analysis, visualization, and exploration."),
                        p("Use the sidebar to filter the data  and navigate through the tabs to view different data visualizations.")
                      ),
                      tabPanel("Data",DT::dataTableOutput("table")),
                      tabPanel("Data Describe",htmlOutput("summary")),
                      tabPanel("Athletes' Height and Weight", plotOutput("height_weight")),
                      tabPanel("Number of Athletes by Year", plotOutput("athlete_barplot")),
                      tabPanel("Count of Sports by Year", plotOutput("sports_count")),
                      tabPanel("Teams by Year", plotOutput("teams_count")),
                      tabPanel("Athletes' Age", plotOutput("age_boxplot")),
                      tabPanel("Medal by country", plotOutput("medals_plot"))
                    )
                  )
                )

# Define the server
server <- function(input, output) {
  
  # Filter the dataset based on user input
  filtered_df <- reactive({
    df %>%
      filter(if (input$sport == "All") TRUE else Sport == input$sport) %>%
      filter(if (input$region == "All") TRUE else region == input$region) %>%
      filter(if (input$medal == "All") TRUE else Medal == input$medal)%>%
      filter(Year >= input$Year[1] & Year <= input$Year[2])%>%
      filter(!is.na(Medal))
  })
  output$table<-DT::renderDataTable({
    datatable(subset(df,select = -c(Height,Weight,Team,NOC,City,Games)),
              options = list(pageLength= 30))
  })
  
  # creating the summary for selected variables
  output$summary<-renderUI({
    print(dfSummary(subset(df,select =c(Sex,Age,Height,Weight,Year,Season,Sport,Medal))
    ),method = 'render',headings = FALSE, bootstracp.css = FALSE)
  })
  # Render the athletes' height and weight plot
  palette <- c("lightsalmon", "#709AE1FF")
  output$height_weight <- renderPlot({
    filtered_df() %>%
      select(Name, Sex, Age, Height, Weight) %>%
      ggplot(aes(x = Height, y = Weight, color = Sex)) +
      geom_point() +
      scale_color_manual(values = palette) +
      labs(x = "Height (cm)", y = "Weight (kg)", color = "Gender")
  })
  
  # Render the bar graph of number of athletes by year and gender
  output$athlete_barplot <- renderPlot({
    filtered_df() %>%
      group_by(Year, Sex) %>%
      summarise(n = n()) %>%
      ggplot(aes(x = Year, y = n, fill = Sex)) +
      geom_bar(stat = "identity", position = "fill") +
      scale_fill_brewer(palette = "Accent") +
      labs(x = "Year", y = "Number of Athletes", fill = "Gender")
  })
  
  
  # Render the count of sports by year plot
  output$sports_count <- renderPlot({
    filtered_df() %>%
      group_by(Year, Season) %>%
      summarize(count = n_distinct(Sport)) %>%
      ggplot(aes(x = Year, y = count, color = Season)) +
      geom_line(size = 1.2) +
      labs(x = "Year", y = "Number of Sports", color = "Season") +
      scale_color_manual(values = c("#F88fa2","#6A5ACD"))
  })
  
  # Render the teams by year plot
  output$teams_count <- renderPlot({
    filtered_df() %>%
      group_by(Year, Season, Team) %>%
      summarize(count = n()) %>%
      filter(rank(desc(count)) <= 5) %>%
      ggplot(aes(x = Year, y = count, size = count, color = Season)) +
      geom_point(alpha = 0.7) + scale_size(range = c(2, 14))+
      labs(x = "Year", y = "Number of Teams", color = "Season", size = "Teams") +
      scale_color_brewer(palette = "Set1") +
      facet_wrap(~Season)
  })
  # Render the age boxplot
  output$age_boxplot <- renderPlot({
    filtered_df() %>%
      select(Sex, Age) %>%
      ggplot(aes(x = Sex, y = Age, fill = Sex)) +
      geom_boxplot() +
      scale_fill_ochre(palette="winmar") +
      labs(x = "Gender", y = "Age", fill = "Gender") +
      theme_classic()
  })
  # Calculate total medals won by each country
  medals_by_country <- df %>%
    filter(!is.na(Medal)) %>%
    filter(!is.na(region)) %>%
    group_by(region,Medal) %>%
    summarize(Total_Medals = n()) %>%
    arrange(desc(Total_Medals)) %>%
    top_n(10)
  
  # Create the plot
  output$medals_plot <- renderPlot({
    ggplot(medals_by_country, aes(x = reorder(region, Total_Medals), y = Total_Medals,fill=Medal)) +
      geom_bar(stat = "identity") +
      xlab("Country") +
      ylab("Total Medals Won") +
      ggtitle("Countries with the Most Medals Won")+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      scale_fill_manual(values = c("gold","gray70","gold4"))
  })
  
}

# Run the app
shinyApp(ui, server)