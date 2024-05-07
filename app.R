# Libraries used
library(shiny)
library(DT)
library(tidyverse)
library(tidyr)
library(stringr)
library(ggplot2)

# Preparing the dataset
f <- "https://raw.githubusercontent.com/difiore/ada-2024-datasets/main/IMDB-movies.csv"
d <- read_csv(f,col_names = TRUE)
d <- drop_na(d)
d$runhours <- round(d$runtimeMinutes/60, digits = 1)
d$splitgenres <- str_split(d$genres, ",")
d <- d |> mutate(Decade = case_when( #If...
      startYear >= 2020 & startYear <= 2024 ~ "2020s",
      startYear >= 2010 & startYear <= 2019 ~ "2010s",
      startYear >= 2000 & startYear <= 2009 ~ "2000s",
      startYear >= 1990 & startYear <= 1999 ~ "1990s",
      startYear >= 1980 & startYear <= 1989 ~ "1980s",
      startYear >= 1970 & startYear <= 1979 ~ "1970s",
      startYear >= 1960 & startYear <= 1969 ~ "1960s",
      startYear >= 1950 & startYear <= 1959 ~ "1950s",
      startYear >= 1940 & startYear <= 1949 ~ "1940s",
      startYear >= 1930 & startYear <= 1939 ~ "1930s",
      startYear >= 1920 & startYear <= 1929 ~ "1920s",
      startYear >= 1910 & startYear <= 1919 ~ "1910s",
      TRUE ~ "NA"
    ))
d <- d |> rename("Title" = "primaryTitle",
                 "Runtime (hours)" = "runhours",
                 "Genres" = "genres",
                 "Year" = "startYear",
                 "IMDB Rating" = "averageRating")

# Defining the UI
ui <- fluidPage(
  titlePanel(h1(strong("Help! What Movie Should I Watch?", style="color:purple"))),
  sidebarLayout(
    sidebarPanel(
      h4(strong("Ready? Let's find your movie!", style="color:purple")),
      h5(strong("Round 1", style="color:purple")),
      selectInput(
        "time",
        label =  "How many hours do you have?",
        choices = 1:12,
        selected = 1),
      h5(strong("Round 2", style="color:purple")),
      checkboxGroupInput(
        "genres",
        label = "What genre(s) would you watch?",
        choices = unique(unlist(d$splitgenres))),
      h5(strong("Round 3", style="color:purple")),
      selectInput(
        "decade",
        label = "What decade would you prefer?",
        choices = unique(d$Decade)),
      h5(strong("Round 4", style="color:purple")),
      selectInput(
        "rating",
        label = "How high are your (IMDB rating) standards?",
        choices = 1:10)
      ),
      
    mainPanel(
      img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/c/cb/ClapperboardColor.svg/1200px-ClapperboardColor.svg.png", width=100, style="display: block; margin: 0 auto;"),
      h1("Meet Your Movie Match", style="color:purple"),
      h3("Round 1"),
      textOutput("movietime"),
      h3("Round 2"),
      textOutput("moviegenres"),
      plotOutput("moviegenresplot"),
      h3("Round 3"),
      textOutput("moviedecade"),
      h3("Round 4"),
      textOutput("movierating"),
      h3("You've Got Options:", style="color:purple"),
      textOutput("initial"),
      DTOutput("initialtable"),
      h3("Still can't decide? We'll do it for you!", style="color:#FF69B4"),
      textOutput("final"),
      DTOutput("finaltable")
    )))

# Defining server logic
server <- function(input, output){
  
# Round 1
  time <- reactive({
    input$time
  })
  
 round1 <- reactive({
   d2 <- filter(d, `Runtime (hours)` <= time())
   nrow(d2)
  })
              
  output$movietime <- renderText({
    paste("Because you have", time(), "hour(s), we can narrow down your options to", round1(), "movie(s).")
  })
  
  # Round 2
  genre <- reactive({
    input$genres
  })
  
  round2 <- reactive({
    d3 <- filter(d, `Runtime (hours)` <= time(), splitgenres %in% input$genres)
    nrow(d3)
  })

  output$moviegenres <- renderText({
    d3 <- filter(d, `Runtime (hours)` <= time(), splitgenres %in% input$genres)
    if(nrow(d3) != 0){
    paste("Because you have", time(), "hour(s) and want to watch the ", paste(input$genres, collapse = " or "),"genre, we can narrow down your options to", round2(), "movie(s).")
    } else {print("No movies match your current preferences. Expand your horizons and try again!")}
  })
  
  output$moviegenresplot <- renderPlot({
    d3 <- filter(d, `Runtime (hours)` <= time(), splitgenres %in% input$genres)
    ggplot(d3, aes(x = Genres)) +
      geom_bar(stat = "count", fill = "#FF69B4") +
      labs(x = "Genre", y = "Number of Options Remaining") +
      theme(panel.background = element_rect(fill = "lavender"),
            axis.text = element_text(color = "#FF69B4", size = 15),
            axis.title = element_text(color = "purple", size = 15))
  })
 
  # Round 3
  decade <- reactive({
    input$decade
  })
  
  round3 <- reactive({
    d4 <- filter(d, `Runtime (hours)` <= time(), splitgenres %in% input$genres, Decade == decade())
    nrow(d4)
  })
  
  output$moviedecade <- renderText({
    d4 <- filter(d, `Runtime (hours)` <= time(), splitgenres %in% input$genres, Decade == decade())
    if(nrow(d4) != 0){
    paste("Because you have", time(), "hour(s), want to watch the ", paste(input$genres, collapse = " or "),"genre, and prefer a movie from the",decade(),", we can narrow down your options to", round3(), "movie(s).")
    } else {print("No movies match your current preferences. Expand your horizons and try again!")}
  })
  
  # Round 4
  
  rating <- reactive({
    input$rating
  })
  
  round4 <- reactive({
    d5 <- filter(d, `Runtime (hours)` <= time(), splitgenres %in% input$genres, Decade == decade(), `IMDB Rating` >= rating())
    nrow(d5)
  })
  
  output$movierating <- renderText({
    d5 <- filter(d, `Runtime (hours)` <= time(), splitgenres %in% input$genres, Decade == decade(), `IMDB Rating` >= rating())
    if(nrow(d5) != 0){
      paste("Because you have", time(), "hour(s), want to watch the ", paste(input$genres, collapse = " or "),"genre, prefer a movie from the", decade(),", and require an IMBD rating of", rating(),"or higher, we can narrow down your options to", round4(), "movie(s).")
    } else {print("No movies match your current preferences. Lower your standards and try again!")}
  })
  
  # Initial results
  
  output$initial <- renderText({
    d5 <- filter(d, `Runtime (hours)` <= time(), splitgenres %in% input$genres, Decade == decade(), `IMDB Rating` >= rating())
    if(nrow(d5) != 0){
      paste("Your initial matches are:")
    } else {print("No movies match your current preferences. Try again!")}
  })
  
  initialresult <- reactive({
    d6 <- filter(d, `Runtime (hours)` <= time(), splitgenres %in% input$genres, Decade == decade(), `IMDB Rating` >= rating()) |>
     select(`Title`,`Runtime (hours)`,`Year`,`IMDB Rating`,`Genres`)
  })
  
  output$initialtable <- renderDT({
    req(initialresult())
    datatable(initialresult(), options = list(pageLength = 10))
  })
  
  # Final result
  
  match <- reactive({
    d6 <- filter(d, `Runtime (hours)` <= time(), splitgenres %in% input$genres, `Decade` == decade(), `IMDB Rating` >= rating()) |>
    select(`Title`,`Runtime (hours)`,`Year`,`IMDB Rating`,`Genres`)
    sample_n(d6, 1)
  })

  output$final <- renderText({
    d5 <- filter(d, `Runtime (hours)` <= time(), splitgenres %in% input$genres, `Decade` == decade(), `IMDB Rating` >= rating())
    if(nrow(d5) != 0){
      paste("Your ultimate movie match is:")
    } else {print("No movie matches your current preferences. Try again!")}
  })
  
  output$finaltable <- renderDT({
    req(match())
    datatable(match()) 
  })
  
}

# Running the app 
shinyApp(ui = ui, server = server)
