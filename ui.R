

#installing all the required packages
install.packages("lubridate")
install.packages("dplyr")
install.packages("shiny")
install.packages("leaflet")
install.packages("plotly")
install.packages("shinythemes")

install.packages("engsoccerdata")
remove.packages(c("ggplot2", "data.table"))
install.packages('Rcpp', dependencies = TRUE)
install.packages('ggplot2', dependencies = TRUE)
install.packages('data.table', dependencies = TRUE)

require(lubridate)
require(dplyr)
require(shiny)
require(shinythemes)
require(leaflet)
require(plotly)
require(magrittr)
require(ggplot2)
require(data.table)
require(scales)
require(Rcpp)


#Loading ranking data
ranking_data <- read.csv("Ranking data.csv")


#Loading the raw data
stadiums <- read.csv("stadiums.csv")
head(stadiums)


#Changing the type of column
stadiums$Date <- lubridate::dmy(stadiums$Date)


#Creating a year column
stadiums$Year <- year(stadiums$Date)
stadiums$Year <- as.factor(stadiums$Year)


#Map data from the original data set
map_data <-
  stadiums %>% distinct(stadiums$Team, stadiums$Latitude, stadiums$Longitude)

#renaming the required columns
names(map_data)[1] <- "Team"
names(map_data)[2] <- "Latitude"
names(map_data)[3] <- "Longitude"

#removing whitespaces from the Team names
map_data$Team <- trimws(map_data$Team)


#Creating an aggregated data frame for the home goals scored
home_team_goals <-
  aggregate(
    stadiums$FTHG,
    by = list(
      HomeTeam = stadiums$HomeTeam,
      FTR = stadiums$FTR,
      year = stadiums$Year
    ),
    FUN = sum
  )

#renaming the column after aggregation to Total_goals
names(home_team_goals)[4] <- "Total_Goals"

#Creating an aggregated data frame for the away goals scored
away_team_goals <-
  aggregate(
    stadiums$FTAG,
    by = list(
      AwayTeam = stadiums$AwayTeam,
      FTR = stadiums$FTR,
      year = stadiums$Year
    ),
    FUN = sum
  )

#renaming the column after aggregation to Total_goals
names(away_team_goals)[4] <- "Total_Goals"

#getting all the unique team names
a <-
  c(as.character(unique(home_team_goals$HomeTeam)), as.character("All"))


##################################################################################
#                   creating a ui for the app
##################################################################################


ui <- fluidPage(
  shinythemes::themeSelector(),
  h1("English Premier League Dashboard") ,
  ####Creating tabs
  tabsetPanel(
    ###Creating the first tab
    tabPanel(
      "Maps",
      h3("Do you know where your favourite EPL team is located?"),
      selectInput(
        inputId = "team_select",
        label = "Select Team",
        choices = c("All", as.character(unique(map_data$Team)))
      ),
      leafletOutput("mymap")
    ),
    ###Creating the second tab
    tabPanel(
      "Goals by home team",
      h3("Have a look at your team's home performance"),
      ####Creating the input option for year
      selectInput(
        inputId = "year",
        label = "Select a year",
        choices = unique(home_team_goals$year),
      )
      ,
      ###Creating output to connect with server
      plotlyOutput("home")
    ),
    ###Creating the third tab
    tabPanel(
      "Goals by away team",
      h3("Have a look at your team's away performance"),
      ####Creating the input option for year
      selectInput(
        inputId = "year_away",
        label = "Select a year",
        choices = unique(home_team_goals$year)
      ),
      plotlyOutput("away")
    ),
    ###Creating the fourth tab
    tabPanel("Ranking Evolution",
             plotlyOutput("ranking"))
  )
)



##################################################################################
#                   creating a server for the app
##################################################################################




server <- function(input, output, session) {
  ####Creating an output plot for goals by home team
  output$home <- renderPlotly({
    ###subsetting the data and comparing the year selected by the user
    sub_Data <-
      home_team_goals[home_team_goals$year == input$year, ]
    ###creating the graph
    ggplot(data = sub_Data, aes(x = HomeTeam, y = Total_Goals, fill = HomeTeam)) +
      geom_bar(stat = "identity") +
      xlab("Home Team") +
      ylab("Total Goals") +
      labs(title = paste("Total goals for ", input$year)) +
      theme(
        plot.title = element_text(
          colour = "blue",
          size = 15,
          face = "bold",
          hjust = 0.7
        ),
        axis.title.x = element_text(
          angle = 90,
          hjust = 1,
          vjust = 0.7
        ),
        text = element_text(size = 7.5)
      )
    
  })
  ####Creating an output plot for goals by away team
  output$away <- renderPlotly({
    sub_Data_away <-
      away_team_goals[away_team_goals$year == input$year_away, ]
    ###creating the graph
    ggplot(data = sub_Data_away, aes(x = AwayTeam, y = Total_Goals, fill = AwayTeam)) +
      geom_bar(stat = "identity") +
      xlab("Away Team") +
      ylab("Total Goals") +
      labs(title = paste("Total goals for ", input$year_away)) +
      theme(
        plot.title = element_text(
          colour = "blue",
          size = 15,
          face = "bold",
          hjust = 0.5
        ),
        axis.title.x = element_text(
          angle = 90,
          hjust = 1,
          vjust = 0.5
        ),
        text = element_text(size = 7.5)
      )
  })
  ####Creating an output for the leaflet map
  output$mymap <- renderLeaflet({
    #if all teams are selected
    if (input$team_select == "All") {
      leaflet() %>%
        addTiles() %>%
        addMarkers(
          data = map_data,
          lng = ~ Longitude,
          lat = ~ Latitude,
          popup =  ~ Team
        )
    }
    else{
      map_new <- map_data[map_data$Team == input$team_select, ]
      
      map_new$Longitude <-  as.numeric(map_new$Longitude)
      map_new$Latitude <-  as.numeric(map_new$Latitude)
      
      
      leaflet() %>%
        addTiles() %>%
        addMarkers(
          data = map_new,
          lng = ~ Longitude,
          lat = ~ Latitude,
          popup =  ~ Team
        )
    }
    
  })
  ####Creating an output for the ranking graph
  output$ranking <- renderPlotly({
    ggplot(
      ranking_data,
      aes(
        x = ranking_data$Year,
        y = ranking_data$Ranking,
        group = ranking_data$Team
      )
    ) +
      geom_line(aes(color = ranking_data$Team, alpha = 1), size = 1) +
      geom_point(aes(color = ranking_data$Team, alpha = 1), size = 2) +
      scale_y_reverse(breaks = 1:length(ranking_data$Ranking)) +
      xlab("Year") +
      ylab("Rank")
    
  })
}

#Launching the app
shinyApp(ui, server)

