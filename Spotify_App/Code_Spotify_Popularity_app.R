# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)
library(readr)
library(fastDummies)   
library(glmnet)   
library(tidyr)

# Load the spotify dataset
# We edit the dataset like in the rmd file:
# changing these variables to log variables to avoid skewness and to make the data more normally distributed
spotify <- read_csv("Spotify-2000.csv") %>%
  mutate(
    `Log Duration`    = log(`Length (Duration)` + 1),
    `Log Speechiness` = log(Speechiness + 1),
    `Log Liveness`    = log(Liveness + 1),
    decade_group = case_when(
      Year >= 1956 & Year <= 1969 ~ "1956-1969",
      Year >= 1970 & Year <= 1979 ~ "1970-1979",
      Year >= 1980 & Year <= 1989 ~ "1980-1989",
      Year >= 1990 & Year <= 1999 ~ "1990-1999",
      Year >= 2000 & Year <= 2009 ~ "2000-2009",
      Year >= 2010 & Year <= 2014 ~ "2010-2015",
      Year >= 2015 & Year <= 2019 ~ "2015-2019"
    )
  ) %>%
  # Creating decade with fast dummies instead of model matrix so we have more control over them, 
  # we make the dummies to increase sample size for the year variable.
  fastDummies::dummy_cols(select_columns = "decade_group", remove_first_dummy = TRUE) %>%
  drop_na()

# Load your precomputed grid search results
load("grid_search_results.RData")
# This loads: final_results and coefficients_storage

# Verify the data loaded correctly
cat("Loaded", nrow(final_results), "combinations\n")
cat("Available combinations:", length(coefficients_storage), "\n")

# Add MSE column if not already present
if(!"MSE" %in% colnames(final_results)) {
  final_results$MSE <- final_results$RMSE^2
}

# Function to get artists and genres for a specific configuration
# This recreates the same data preparation logic used in the grid search
get_model_choices <- function(top_n_genres, top_n_artists) {
  # Get top artists and genres based on frequency in the dataset
  top_genres <- spotify %>% 
    count(`Top Genre`, sort = TRUE) %>% 
    slice_head(n = top_n_genres) %>% 
    pull(`Top Genre`)
  
  top_artists <- spotify %>% 
    count(Artist, sort = TRUE) %>% 
    slice_head(n = top_n_artists) %>% 
    pull(Artist)
  
  # Add "Other" option to both lists
  return(list(
    artists = c(top_artists, "Other"),
    genres = c(top_genres, "Other")
  ))
}

# Define Spotify colors green to black palette
spotify_colors <- list(
  primary_green = "#1DB954",
  light_green = "#1ED760", 
  medium_green = "#1AA34A",
  dark_green = "#138F40",
  darker_green = "#0F7A35",
  very_dark_green = "#0C5E2A",
  almost_black = "#191414",
  dark = "#191414",
  white = "#FFFFFF",
  gray = "#535353"
)

# Custom green-to-black gradient (no brown, orange, or yellow)
green_to_black_gradient <- colorRampPalette(c(
  spotify_colors$light_green,    # Lightest green (best performance)
  spotify_colors$primary_green,  # Spotify green
  spotify_colors$medium_green,   # Medium green
  spotify_colors$dark_green,     # Dark green
  spotify_colors$darker_green,   # Darker green
  spotify_colors$very_dark_green, # Very dark green
  spotify_colors$almost_black    # Black (worst performance)
))(100)

# UI
ui <- dashboardPage(
  dashboardHeader(
    title = "Spotify Model Explorer",
    titleWidth = 250
  ),
  #sidebar for a nice overview of the app
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem("Data Overview", tabName = "overview", icon = icon("chart-bar")),
      menuItem("Hyperparameter Tuning", tabName = "tuning", icon = icon("sliders-h")),
      menuItem("Model Performance", tabName = "performance", icon = icon("chart-area")),
      menuItem("Model Coefficients", tabName = "coefficients", icon = icon("table")),
      menuItem("Popularity Prediction", tabName = "prediction", icon = icon("music")),
      menuItem("Model Interpretation", tabName = "interpretation", icon = icon("lightbulb"))
    )
  ),
  # Body of the dashboard
  dashboardBody(
    tags$head(
      tags$style(HTML(paste0("
        .content-wrapper, .right-side {
          background-color: ", spotify_colors$white, ";
        }
        .main-header .navbar {
          background-color: ", spotify_colors$dark, " !important;
        }
        .main-header .logo {
          background-color: ", spotify_colors$primary_green, " !important;
          color: ", spotify_colors$white, " !important;
        }
        .sidebar {
          background-color: ", spotify_colors$dark, " !important;
        }
        .sidebar-menu > li > a {
          color: ", spotify_colors$white, " !important;
        }
        .sidebar-menu > li.active > a {
          background-color: ", spotify_colors$primary_green, " !important;
        }
      ")))
    ),
    # All the pages
    tabItems(
      # Data Overview Tab
      tabItem(
        tabName = "overview",
        
        # Popularity distribution graph
        fluidRow(
          box(
            title = "Popularity Distribution",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            height = "600px",
            
            fluidRow(
              column(4,
                     h5("Histogram Controls"),
                     sliderInput("binwidth", 
                                 "Bin Width:", 
                                 min = 1, max = 20, value = 5,
                                 step = 1
                     ),
                     p(class = "text-muted", "Adjust the granularity of the popularity distribution"),
                     
                     br(),
                     div(style = "background: #f8f9fa; padding: 15px; border-radius: 10px; border-left: 4px solid #1DB954;",
                         h6("📈 Popularity Benchmarks", style = "color: #1DB954; margin-bottom: 10px;"),
                         uiOutput("popularityBenchmarks")
                     ),
                     
                     br(),
                     div(style = "background: #f8f9fa; padding: 15px; border-radius: 10px; border-left: 4px solid #1DB954;",
                         h6("📊 Understanding This Chart", style = "color: #1DB954; margin-bottom: 10px;"),
                         div(style = "font-size: 12px; line-height: 1.4;",
                             p("• Each green bar shows how many songs fall within that popularity range", style = "margin-bottom: 5px;"),
                             p("• The purple dashed line shows the average popularity (59.5)", style = "margin-bottom: 5px;"),
                             p("• Most songs cluster around 50-70 popularity, with fewer very popular (80+) or unpopular (20-) songs", style = "margin-bottom: 5px;"),
                             p("• This right-skewed distribution means it's harder to achieve very high popularity", style = "margin-bottom: 0; font-weight: 600;")
                         )
                     )
              ),
              column(8,
                     plotOutput("popularityHist", height = "450px")
              )
            )
          )
        ),
        
        # correlations graph
        fluidRow(
          box(
            title = "Feature Correlations",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            height = "600px",
            
            fluidRow(
              column(4,
                     h5("Variable Selection"),
                     selectInput("xvar", 
                                 "X-Axis Variable:", 
                                 choices = c("Energy", "Danceability", "Loudness (dB)", "Valence", 
                                             "Acousticness", "Beats Per Minute (BPM)", "Length (Duration)",
                                             "Speechiness", "Liveness", "Popularity", "Year",
                                             "Log Duration", "Log Speechiness", "Log Liveness"),
                                 selected = "Energy"
                     ),
                     selectInput("yvar", 
                                 "Y-Axis Variable:", 
                                 choices = c("Energy", "Danceability", "Loudness (dB)", "Valence", 
                                             "Acousticness", "Beats Per Minute (BPM)", "Length (Duration)",
                                             "Speechiness", "Liveness", "Popularity", "Year",
                                             "Log Duration", "Log Speechiness", "Log Liveness"),
                                 selected = "Popularity"
                     ),
                     p(class = "text-muted", "Explore relationships between different audio features"),
                     
                     br(),
                     # Move explanation box to the left column
                     div(style = "background: #f8f9fa; padding: 15px; border-radius: 10px; border-left: 4px solid #1DB954;",
                         h6("📊 How to Read This Chart", style = "color: #1DB954; margin-bottom: 10px;"),
                         uiOutput("correlationExplanation")
                     )
              ),
              column(8,
                     plotlyOutput("scatterPlot", height = "450px")
              )
            )
          )
        )
      ),
      
      # Parameter Tuning Tab
      tabItem(
        tabName = "tuning",
        fluidRow(
          box(
            title = "Parameter Tuning", 
            status = "success", 
            solidHeader = TRUE,
            width = 4,
            height = "600px",
            
            h4("Genre Limit:"),
            sliderInput("genre_limit", 
                        label = NULL,
                        min = 1, max = 20, value = 5, step = 1),
            
            h4("Artist Limit:"),
            sliderInput("artist_limit",
                        label = NULL, 
                        min = 1, max = 15, value = 3, step = 1),
            
            br(),
            actionButton("update_model", "UPDATE MODEL", 
                         style = paste0("background-color: ", spotify_colors$primary_green, 
                                        "; color: white; width: 100%; font-weight: bold;")),
            
            br(), br(),
            div(
              style = "background-color: #f9f9f9; padding: 15px; border-radius: 5px;",
              h5("Quick Guide", style = "color: #333; margin-bottom: 10px;"),
              tags$ul(
                tags$li("The alpha and lambda values are automatically selected based on the best model performance."),
                tags$li("Artist and genre choices in the prediction tab update automatically based on these limits.")
              )
            )
          ),
          
          box(
            title = "Current Model Performance",
            status = "success",
            solidHeader = TRUE,
            width = 8,
            height = "600px",
            
            fluidRow(
              column(6,
                     valueBoxOutput("rmse_box", width = NULL),
                     valueBoxOutput("rsquared_box", width = NULL)
              ),
              column(6,
                     valueBoxOutput("alpha_box", width = NULL),
                     valueBoxOutput("lambda_box", width = NULL)
              )
            ),
            
            fluidRow(
              column(12,
                     valueBoxOutput("vars_box", width = NULL)
              )
            )
          )
        )
      ),
      
      # Model Performance Tab  
      tabItem(
        tabName = "performance",
        fluidRow(
          box(
            title = "Model Performance Heatmap",
            status = "success",
            solidHeader = TRUE,
            width = 8,
            height = "700px",
            
            tabsetPanel(
              tabPanel("MSE Heatmap", 
                       plotlyOutput("mse_heatmap", height = "600px"))
            )
          ),
          
          box(
            title = "Understanding the Heatmap",
            status = "success",
            solidHeader = TRUE,
            width = 4,
            height = "700px",
            
            div(style = "background: #f8f9fa; padding: 15px; border-radius: 10px; border-left: 4px solid #1DB954;",
                h6("📊 How to Read This Chart", style = "color: #1DB954; margin-bottom: 10px;"),
                div(style = "font-size: 12px; line-height: 1.4;",
                    p("• Each cell shows the Mean Squared Error (MSE) for a specific combination of genre and artist limits", style = "margin-bottom: 8px;"),
                    p("• Lower MSE values (lighter green) indicate better model performance", style = "margin-bottom: 8px;"),
                    p("• Higher MSE values (darker colors) indicate worse performance", style = "margin-bottom: 8px;"),
                    p("• The numbers in each cell show the exact MSE value", style = "margin-bottom: 15px; font-weight: 600;")
                )
            ),
            
            br(),
            
            div(style = "background: #f8f9fa; padding: 15px; border-radius: 10px; border-left: 4px solid #1DB954;",
                h6("🎯 Key Insights", style = "color: #1DB954; margin-bottom: 10px;"),
                div(style = "font-size: 12px; line-height: 1.4;",
                    p("• The best performance (lowest MSE) appears in the top-right corner with more genres and artists", style = "margin-bottom: 8px;"),
                    p("• Using only 1 genre gives poor performance regardless of artist count", style = "margin-bottom: 8px;"),
                    p("• There's a sweet spot around 15-20 genres and 12-15 artists", style = "margin-bottom: 8px;"),
                    p("• More data (higher limits) generally leads to better predictions", style = "margin-bottom: 0; font-weight: 600;")
                )
            ),
            
            br(),
            
            div(style = "background: #f8f9fa; padding: 15px; border-radius: 10px; border-left: 4px solid #1DB954;",
                h6("⚖️ MSE Explained", style = "color: #1DB954; margin-bottom: 10px;"),
                div(style = "font-size: 12px; line-height: 1.4;",
                    p("• MSE measures how far off our predictions are from actual popularity scores", style = "margin-bottom: 8px;"),
                    p("• An MSE of 144 means our average prediction error is about √144 = 12 popularity points", style = "margin-bottom: 8px;"),
                    p("• The difference between best (144) and worst (181) models is significant for prediction accuracy", style = "margin-bottom: 0;")
                )
            )
          )
        )
      ),
      
      # Model Coefficients Tab
      tabItem(
        tabName = "coefficients", 
        fluidRow(
          box(
            title = "Model Coefficients",
            status = "success",
            solidHeader = TRUE, 
            width = 6,
            height = "800px",
            
            h4("Selected Variables and Coefficients"),
            DT::dataTableOutput("coefficient_table")
          ),
          
          box(
            title = "Coefficient Visualization",
            status = "success", 
            solidHeader = TRUE,
            width = 6,
            height = "800px",
            
            plotlyOutput("coefficient_plot", height = "600px")
          )
        ),
        
        fluidRow(
          box(
            title = "Understanding Model Coefficients",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            height = "300px",
            
            fluidRow(
              column(4,
                     div(style = "background: #f8f9fa; padding: 12px; border-radius: 8px; border-left: 4px solid #1DB954; height: 200px;",
                         h6("📊 What Coefficients Mean", style = "color: #1DB954; margin-bottom: 8px; font-size: 14px;"),
                         div(style = "font-size: 11px; line-height: 1.3;",
                             p("• Each coefficient shows how much that variable affects song popularity", style = "margin-bottom: 6px;"),
                             p("• Positive coefficients (green) increase popularity", style = "margin-bottom: 6px;"),
                             p("• Negative coefficients (black) decrease popularity", style = "margin-bottom: 6px;"),
                             p("• Larger absolute values mean stronger influence", style = "margin-bottom: 0;")
                         )
                     )
              ),
              column(4,
                     div(style = "background: #f8f9fa; padding: 12px; border-radius: 8px; border-left: 4px solid #1DB954; height: 200px;",
                         h6("🎵 Reading the Results", style = "color: #1DB954; margin-bottom: 8px; font-size: 14px;"),
                         div(style = "font-size: 11px; line-height: 1.3;",
                             p("• Artist effects show which artists tend to be more popular", style = "margin-bottom: 6px;"),
                             p("• Genre effects reveal which music styles boost popularity", style = "margin-bottom: 6px;"),
                             p("• Decade effects show time period influences on success", style = "margin-bottom: 6px;"),
                             p("• Audio features show which sonic qualities matter most", style = "margin-bottom: 0;")
                         )
                     )
              ),
              column(4,
                     div(style = "background: #f8f9fa; padding: 12px; border-radius: 8px; border-left: 4px solid #1DB954; height: 200px;",
                         h6("⚡ Variable Selection", style = "color: #1DB954; margin-bottom: 8px; font-size: 14px;"),
                         div(style = "font-size: 11px; line-height: 1.3;",
                             p("• Only variables with non-zero coefficients are shown", style = "margin-bottom: 6px;"),
                             p("• Regularization automatically removes less important features", style = "margin-bottom: 6px;"),
                             p("• Different model configurations select different variables", style = "margin-bottom: 6px;"),
                             p("• This helps prevent overfitting and improves predictions", style = "margin-bottom: 0;")
                         )
                     )
              )
            )
          )
        )
      ),
      
      # Popularity Prediction Tab - Updated with dynamic choices
      tabItem(
        tabName = "prediction",
        fluidRow(
          box(
            title = "Song Features Input",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            height = "1400px",
            
            h4("Artist & Genre"),
            # Dynamic artist selection that updates based on current model
            selectInput("pred_artist", "Artist:", 
                        choices = c("Loading..."),  # Will be updated dynamically
                        selected = NULL),
            
            # Dynamic genre selection that updates based on current model
            selectInput("pred_genre", "Top Genre:",
                        choices = c("Loading..."),  # Will be updated dynamically
                        selected = NULL),
            
            # Information box showing current model configuration
            div(style = "background: #e8f5e8; padding: 10px; border-radius: 5px; margin-bottom: 15px;",
                h6("ℹ️ Model Configuration", style = "color: #1DB954; margin-bottom: 5px;"),
                textOutput("model_config_info", inline = TRUE)
            ),
            
            h4("Audio Features"),
            sliderInput("pred_energy", "Energy:", min = 0, max = 100, value = 50),
            sliderInput("pred_danceability", "Danceability:", min = 0, max = 100, value = 50),
            sliderInput("pred_loudness", "Loudness (dB):", min = -60, max = 5, value = -10),
            sliderInput("pred_valence", "Valence:", min = 0, max = 100, value = 50),
            sliderInput("pred_acousticness", "Acousticness:", min = 0, max = 100, value = 20),
            sliderInput("pred_bpm", "BPM:", min = 60, max = 200, value = 120),
            
            h4("Other Features"),
            sliderInput("pred_log_duration", "Log Duration:", 
                        min = 3.4, max = 6.4, value = 5.3, step = 0.1),
            sliderInput("pred_log_speechiness", "Log Speechiness:", 
                        min = 0, max = 4.6, value = 1.6, step = 0.1),
            sliderInput("pred_log_liveness", "Log Liveness:", 
                        min = 0, max = 4.6, value = 2.7, step = 0.1),
            
            selectInput("pred_decade", "Decade:",
                        choices = list("1956-1969" = "Y1", "1970-1979" = "Y2", 
                                       "1980-1989" = "Y3", "1990-1999" = "Y4",
                                       "2000-2009" = "Y5", "2010-2014" = "Y6", 
                                       "2015-2019" = "Y7"),
                        selected = "Y5"),
            
            br(),
            actionButton("predict_btn", "PREDICT POPULARITY", 
                         style = paste0("background-color: ", spotify_colors$primary_green, 
                                        "; color: white; width: 100%; font-weight: bold;"))
          ),
          
          box(
            title = "Prediction Results",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            height = "1600px",
            
            div(style = "text-align: center; padding: 20px;",
                h2("Predicted Popularity", style = "color: #333;"),
                div(id = "prediction_value",
                    style = paste0("font-size: 72px; font-weight: bold; color: ", 
                                   spotify_colors$primary_green, "; margin: 20px 0;"),
                    textOutput("predicted_popularity", inline = TRUE)),
                h4("out of 100", style = "color: #666; margin-bottom: 30px;")
            ),
            
            hr(),
            
            # Add the popularity context section
            div(style = "background: #f8f9fa; padding: 15px; border-radius: 10px; margin-bottom: 20px;",
                h4("📊 Popularity Context", style = "color: #1DB954; margin-bottom: 15px;"),
                verbatimTextOutput("predictionExplanation")
            ),
            
            hr(),
            
            h4("📈 Where Your Song Ranks"),
            plotOutput("predictionPlot", height = "300px"),
            
            hr(),
            
            h4("Model Information"),
            fluidRow(
              column(6,
                     strong("Current Model:"),
                     br(),
                     textOutput("current_model_info"),
                     br(),
                     strong("Variables Used:"),
                     br(),
                     textOutput("vars_used_info")
              ),
              column(6,
                     strong("Top Contributing Factors:"),
                     br(),
                     tableOutput("contribution_table")
              )
            ),
            
            br(),
            
            h4("Prediction Breakdown"),
            plotOutput("prediction_breakdown", height = "200px")
          )
        )
      ),
      
      # Model Interpretation Tab
      tabItem(
        tabName = "interpretation",
        fluidRow(
          box(
            title = "Selected Model Summary",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            height = "500px",
            
            h4("Key Model Characteristics"),
            p("This summary provides an overview of the currently selected model's configuration and performance metrics."),
            
            br(),
            tableOutput("selectedModelSummary"),
            
            br(),
            actionButton("updateModel", "UPDATE MODEL", 
                         style = paste0("background-color: ", spotify_colors$primary_green, 
                                        "; color: white; width: 100%; font-weight: bold;"))
          ),
          
          box(
            title = "Model Interpretation",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            height = "400px",
            
            h4("Understanding Your Model"),
            p("This section explains what the model parameters mean and how the model makes its predictions."),
            
            br(),
            div(style = "background: #f8f9fa; padding: 20px; border-radius: 10px; border-left: 4px solid #1DB954; line-height: 1.6;",
                verbatimTextOutput("interpretationText")
            )
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive value to store current model selection which is necessary for the live prediction model
  current_model <- reactive({
    # Ensure we have valid inputs before proceeding, so the app doesn't crash
    req(input$genre_limit, input$artist_limit)
    
    # find the closest matching combination in our data
    available_genres <- sort(unique(final_results$Genre_Limit))
    available_artists <- sort(unique(final_results$Artist_Limit))
    
    # Find closest matches to what the user selected on the sliders
    closest_genre <- available_genres[which.min(abs(available_genres - input$genre_limit))]
    closest_artist <- available_artists[which.min(abs(available_artists - input$artist_limit))]
    
    # Get the corresponding row from our precomputed results
    model_data <- final_results %>%
      filter(Genre_Limit == closest_genre, Artist_Limit == closest_artist)
    
    if(nrow(model_data) > 0) {
      return(model_data[1, ])
    } else {
      return(final_results[1, ])
    }
  })
  
  # NEW: Dynamic choices that update based on current model configuration
  observe({
    model <- current_model()
    choices <- get_model_choices(model$Genre_Limit, model$Artist_Limit)
    
    # Update artist choices
    updateSelectInput(session, "pred_artist", 
                      choices = choices$artists,
                      selected = choices$artists[1])  # Select first artist by default
    
    # Update genre choices
    updateSelectInput(session, "pred_genre", 
                      choices = choices$genres,
                      selected = choices$genres[1])  # Select first genre by default
  })
  
  # NEW: Display current model configuration info
  output$model_config_info <- renderText({
    model <- current_model()
    paste0("Using top ", model$Genre_Limit, " genres and top ", model$Artist_Limit, " artists")
  })
  
  # Filtered results for the interpretation tab
  filtered_results <- reactive({
    final_results
  })
  
  # DATA OVERVIEW TAB OUTPUTS
  
  # Making a simple histogram of the popularity variable to show how it is distributed, which reveals the mean is around 59.5.
  output$popularityHist <- renderPlot({
    ggplot(spotify, aes(x = Popularity)) +
      geom_histogram(binwidth = input$binwidth, 
                     fill = spotify_colors$primary_green, 
                     color = "white", 
                     alpha = 0.8) +
      # We add a vertical line showing the mean because it shows that popularity is skewed to the right,
      geom_vline(xintercept = mean(spotify$Popularity), 
                 linetype = "dashed", 
                 color = "purple", 
                 linewidth = 1) +
      labs(title = "Distribution of Song Popularity",
           subtitle = paste("Mean popularity:", round(mean(spotify$Popularity), 1)),
           x = "Popularity Score",
           y = "Number of Songs") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12, color = "gray50"),
        axis.title = element_text(size = 12, face = "bold")
      )
  })
  
  # This creates those helpful numbers that show how popularity is distributed,
  # so the user knows what a good popularity score actually means
  # We calculate percentiles because numbers like "67.2" dont mean much without context, but saying
  # "top 10% of songs" immediately makes it clear and easy to understand to anyone. 
  # We chose the 90th, 75th, 50th and 25th percentiles
  output$popularityBenchmarks <- renderUI({
    mean_pop <- round(mean(spotify$Popularity), 1)
    median_pop <- round(median(spotify$Popularity), 1)
    max_pop <- max(spotify$Popularity)
    min_pop <- min(spotify$Popularity)
    
    percentiles <- quantile(spotify$Popularity, probs = c(0.1, 0.25, 0.5, 0.75))
    # We format this as a nice styled box with different font weights to emphasize the top 10%
    tags$div(
      style = "font-size: 11 px; line-height: 1.4;",
      p(paste("Top 10%: ≥", round(percentiles[1], 1)), style = "margin-bottom: 3px; font-weight: 600;"),
      p(paste("Top 25%: ≥", round(percentiles[2], 1)), style = "margin-bottom: 3px;"),
      p(paste("Median: ", round(percentiles[3], 1)), style = "margin-bottom: 3px;"),
      p(paste("Bottom 25%: ≤", round(percentiles[4], 1)), style = "margin-bottom: 0;")
    )
  })
  
  # NEW: Add explanation for correlation chart
  output$correlationExplanation <- renderUI({
    req(input$xvar, input$yvar)
    
    # Map display names to actual column names
    var_mapping <- c(
      "Energy" = "Energy", "Danceability" = "Danceability", 
      "Loudness (dB)" = "Loudness (dB)", "Valence" = "Valence",
      "Acousticness" = "Acousticness", "Beats Per Minute (BPM)" = "Beats Per Minute (BPM)",
      "Length (Duration)" = "Length (Duration)", "Speechiness" = "Speechiness",
      "Liveness" = "Liveness", "Popularity" = "Popularity", "Year" = "Year",
      "Log Duration" = "Log Duration", "Log Speechiness" = "Log Speechiness", 
      "Log Liveness" = "Log Liveness"
    )
    
    x_col <- var_mapping[input$xvar]
    y_col <- var_mapping[input$yvar]
    correlation <- cor(spotify[[x_col]], spotify[[y_col]], use = "complete.obs")
    
    # Create explanation based on correlation strength
    strength <- case_when(
      abs(correlation) >= 0.7 ~ "very strong",
      abs(correlation) >= 0.5 ~ "strong", 
      abs(correlation) >= 0.3 ~ "moderate",
      abs(correlation) >= 0.1 ~ "weak",
      TRUE ~ "very weak"
    )
    
    direction <- ifelse(correlation > 0, "positive", "negative")
    
    tags$div(
      style = "font-size: 12px; line-height: 1.4;",
      p(paste0("• Each green dot represents one song in our dataset"), style = "margin-bottom: 5px;"),
      p(paste0("• The purple line shows the overall trend between these two features"), style = "margin-bottom: 5px;"),
      p(paste0("• Correlation of ", round(correlation, 3), " indicates a ", strength, " ", direction, " relationship"), style = "margin-bottom: 5px;"),
      if(abs(correlation) > 0.3) {
        p(paste0("• This suggests that ", tolower(input$xvar), " and ", tolower(input$yvar), " tend to move together"), style = "margin-bottom: 0; font-weight: 600;")
      } else {
        p(paste0("• These features appear to be relatively independent of each other"), style = "margin-bottom: 0;")
      }
    )
  })
  
  # Making a scatter plot of the relationship between two selected variables,
  # with a linear regression line and correlation coefficient.
  # This allows users to explore how all the variables relate to song popularity.
  
  output$scatterPlot <- renderPlotly({
    req(input$xvar, input$yvar)
    
    # Map display names to actual column names in our dataset
    # We need this because the UI shows nice names but the data has different column names
    var_mapping <- c(
      "Energy" = "Energy",
      "Danceability" = "Danceability", 
      "Loudness (dB)" = "Loudness (dB)",
      "Valence" = "Valence",
      "Acousticness" = "Acousticness",
      "Beats Per Minute (BPM)" = "Beats Per Minute (BPM)",
      "Length (Duration)" = "Length (Duration)",
      "Speechiness" = "Speechiness",
      "Liveness" = "Liveness",
      "Popularity" = "Popularity",
      "Year" = "Year",
      "Log Duration" = "Log Duration",
      "Log Speechiness" = "Log Speechiness", 
      "Log Liveness" = "Log Liveness"
    )
    
    x_col <- var_mapping[input$xvar]
    y_col <- var_mapping[input$yvar]
    
    # We calculate the correlation coefficient because,
    # it's the most clear way to see the actual precise correlation.
    
    correlation <- cor(spotify[[x_col]], spotify[[y_col]], use = "complete.obs")
    
    # We use geom_smooth with method lm to add a regression line because it helps users visually see
    # the trend even when there's not a easy linear relationship to see
    p <- ggplot(spotify, aes(x = .data[[x_col]], y = .data[[y_col]])) +
      geom_point(color = spotify_colors$primary_green, alpha = 0.6, size = 1.5) +
      geom_smooth(method = "lm", se = TRUE, color = "purple", fill = "purple") +
      labs(title = paste("Relationship between", input$xvar, "and", input$yvar),
           subtitle = paste("Correlation coefficient:", round(correlation, 3)),
           x = input$xvar,
           y = input$yvar) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 11, color = "gray50")
      )
    
    # We convert to plotly because it makes the plot allot more interactive.
    ggplotly(p) %>%
      layout(title = list(text = paste0("Relationship between ", input$xvar, " and ", input$yvar,
                                        "<br><sub>Correlation: ", round(correlation, 3), "</sub>")))
  })
  
  # Value boxes for current model, which will  update automatically when sliders change
  output$rmse_box <- renderValueBox({
    # Add dependency on sliders to force reactivity when changed
    input$genre_limit
    input$artist_limit
    
    model <- current_model()
    valueBox(
      value = round(model$RMSE, 3),
      subtitle = "RMSE",
      icon = icon("bullseye"),
      color = "green"
    )
  })
  
  output$rsquared_box <- renderValueBox({
    # Add dependency on sliders to force reactivity when changed
    input$genre_limit
    input$artist_limit
    
    model <- current_model()
    valueBox(
      value = round(model$R_Squared, 3),
      subtitle = "R-squared", 
      icon = icon("chart-line"),
      color = "green"
    )
  })
  
  output$alpha_box <- renderValueBox({
    # Add dependency on sliders to force reactivity when changed
    input$genre_limit
    input$artist_limit
    
    model <- current_model()
    valueBox(
      value = model$Best_Alpha,
      subtitle = "Best Alpha",
      icon = icon("sliders-h"), 
      color = "green"
    )
  })
  
  output$lambda_box <- renderValueBox({
    # Add dependency on sliders to force reactivity when changed
    input$genre_limit
    input$artist_limit
    
    model <- current_model()
    valueBox(
      value = round(model$Best_Lambda, 4),
      subtitle = "Best Lambda",
      icon = icon("adjust"),
      color = "green"
    )
  })
  
  output$vars_box <- renderValueBox({
    # Add dependency on sliders to force reactivity when changed
    input$genre_limit
    input$artist_limit
    
    model <- current_model()
    valueBox(
      value = paste(model$Vars_Selected, "/", model$Total_Vars),
      subtitle = "Variables Selected",
      icon = icon("list"),
      color = "green"
    )
  })
  
  # This creates the interactive heatmap that shows model performance across different configurations
  # We chose a heatmap because it's the best way to visualize how the two parameters
  # (genre limit vs artist limit) affect model performance.
  # The color intensity immediately shows which combinations work best.
  output$mse_heatmap <- renderPlotly({
    p <- ggplot(final_results, aes(x = Genre_Limit, y = Artist_Limit, fill = MSE)) +
      geom_tile(color = "white", size = 0.5) +
      geom_text(aes(label = round(MSE, 1)), color = "white", size = 3, fontface = "bold") +
      scale_fill_gradientn(
        name = "MSE",
        colors = green_to_black_gradient,
        guide = guide_colorbar(title.position = "top", title.hjust = 0.5)
      ) +
      scale_x_continuous(breaks = unique(final_results$Genre_Limit), expand = c(0, 0)) +
      scale_y_continuous(breaks = unique(final_results$Artist_Limit), expand = c(0, 0)) +
      labs(title = "Model Performance Heatmap (MSE)",
           x = "Number of Top Genres", 
           y = "Number of Top Artists") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.grid = element_blank()
      )
    
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  # This coefficient table shows which variables the current model configuration actually uses
  # We only show non-zero coefficients because regularization causes many coefficients to be exactly zero.
  # This makes it easy to see how different configurations affect variable selection.
  output$coefficient_table <- DT::renderDataTable({
    model <- current_model()
    combo_key <- model$Combo_Key
    
    # Check if we have coefficients stored for this model configuration
    if(combo_key %in% names(coefficients_storage)) {
      coeffs <- coefficients_storage[[combo_key]]
      nonzero_coeffs <- coeffs[coeffs[,1] != 0, , drop = FALSE]
      
      # Create a nice table showing the coefficients sorted by importance
      coeff_df <- data.frame(
        Variable = rownames(nonzero_coeffs),
        Coefficient = round(as.numeric(nonzero_coeffs[,1]), 4),
        stringsAsFactors = FALSE
      ) %>%
        arrange(desc(abs(Coefficient)))
      
      # Use DT::datatable because it gives us functions like sorting and searching,
      # which makes it easier for users to find the coefficients they care about.
      DT::datatable(coeff_df, 
                    options = list(pageLength = 15, searching = TRUE),
                    rownames = FALSE) %>%
        DT::formatStyle('Coefficient',
                        backgroundColor = DT::styleInterval(0, c('#ffcccc', '#ccffcc')))
    } else {
      DT::datatable(data.frame(Variable = "No data", Coefficient = 0))
    }
  })
  
  # This creates a visual representation of the top coefficients
  # We limit to top 10 because showing all coefficients would be too cluttered
  output$coefficient_plot <- renderPlotly({
    model <- current_model()
    combo_key <- model$Combo_Key
    
    if(combo_key %in% names(coefficients_storage)) {
      coeffs <- coefficients_storage[[combo_key]]
      nonzero_coeffs <- coeffs[coeffs[,1] != 0, , drop = FALSE]
      
      coeff_df <- data.frame(
        Variable = rownames(nonzero_coeffs),
        Coefficient = as.numeric(nonzero_coeffs[,1]),
        stringsAsFactors = FALSE
      ) %>%
        filter(Variable != "(Intercept)") %>%  # Remove intercept because it's usually not interesting
        arrange(desc(abs(Coefficient))) %>%
        slice_head(n = 10)  # Show only top 10 to keep it readable
      
      if(nrow(coeff_df) > 0) {
        p <- ggplot(coeff_df, aes(x = reorder(Variable, abs(Coefficient)), 
                                  y = Coefficient,
                                  fill = ifelse(Coefficient > 0, "Positive", "Negative"))) +
          geom_col() +
          coord_flip() +  # Make it horizontal so variable names are readable
          scale_fill_manual(values = c("Positive" = spotify_colors$primary_green, 
                                       "Negative" = spotify_colors$almost_black)) +
          labs(title = "Top 10 Model Coefficients",
               x = "Variables", 
               y = "Coefficient Value",
               fill = "Effect") +
          theme_minimal() +
          theme(
            plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
            legend.position = "bottom"
          )
        
        ggplotly(p)
      } else {
        plotly_empty()
      }
    } else {
      plotly_empty()
    }
  })
  
  # This shows the predicted popularity using our current model configuration
  # We ensure it stays between 0-100 which makes sense for popularity scores
  output$predicted_popularity <- renderText({
    if(input$predict_btn > 0) {
      model <- current_model()
      combo_key <- model$Combo_Key
      
      if(combo_key %in% names(coefficients_storage)) {
        coeffs <- coefficients_storage[[combo_key]]
        
        # Create input vector based on current model's variables
        prediction <- predict_popularity(coeffs, input)
        
        round(max(0, min(100, prediction)), 1)  # Ensure between 0-100 because popularity can't be negative or over 100
      } else {
        "No model"
      }
    } else {
      "Press Predict"
    }
  })
  
  # Current model info, shows which configuration is being used
  output$current_model_info <- renderText({
    # Add dependency on sliders to force reactivity when changed
    input$genre_limit
    input$artist_limit
    
    model <- current_model()
    paste0("Top ", model$Genre_Limit, " genres, Top ", model$Artist_Limit, " artists\n",
           "Alpha: ", model$Best_Alpha, ", Lambda: ", round(model$Best_Lambda, 4))
  })
  
  # Variables used info, shows how many variables the model selected
  output$vars_used_info <- renderText({
    input$genre_limit
    input$artist_limit
    
    model <- current_model()
    paste0(model$Vars_Selected, " out of ", model$Total_Vars, " variables selected")
  })
  
  # This shows which features contribute most to the current prediction
  # We limit to top 5 to keep it simple and readable
  output$contribution_table <- renderTable({
    if(input$predict_btn > 0) {
      model <- current_model()
      combo_key <- model$Combo_Key
      
      if(combo_key %in% names(coefficients_storage)) {
        coeffs <- coefficients_storage[[combo_key]]
        contributions <- calculate_contributions(coeffs, input)
        head(contributions, 5)
      }
    }
  }, digits = 2)
  
  # nice visualisation of the top contributing factors to the popularity prediction
  # This helps users understand which features are pushing popularity up or down
  output$prediction_breakdown <- renderPlot({
    if(input$predict_btn > 0) {
      model <- current_model()
      combo_key <- model$Combo_Key
      
      if(combo_key %in% names(coefficients_storage)) {
        coeffs <- coefficients_storage[[combo_key]]
        contributions <- calculate_contributions(coeffs, input)
        
        # Plot top 8 contributions to keep the chart readable
        top_contrib <- head(contributions, 8)
        
        ggplot(top_contrib, aes(x = reorder(Variable, abs(Contribution)), 
                                y = Contribution,
                                fill = ifelse(Contribution > 0, "Positive", "Negative"))) +
          geom_col() +
          coord_flip() +  # Horizontal bars are easier to read
          scale_fill_manual(values = c("Positive" = spotify_colors$primary_green, 
                                       "Negative" = spotify_colors$almost_black)) +
          labs(title = "Top Contributing Factors", x = "", y = "Contribution to Popularity") +
          theme_minimal() +
          theme(legend.position = "none")  # Remove legend since colors are self-explanatory
      }
    }
  })
  
  # Provide context by explaining where this prediction falls in the distribution of all songs
  # This helps users understand if their song would be popular or not
  output$predictionExplanation <- renderText({
    req(input$predict_btn)
    model <- current_model()
    combo_key <- model$Combo_Key
    
    if(combo_key %in% names(coefficients_storage)) {
      coeffs <- coefficients_storage[[combo_key]]
      predicted <- predict_popularity(coeffs, input)
      
      # Calculate what percentile the prediction falls under
      # This tells the user what percentage of songs in our dataset have a lower popularity score
      percentile <- ecdf(spotify$Popularity)(predicted) * 100
      
      # add an explanation based on the percentile
      context <- case_when(
        percentile >= 90 ~ "exceptional - among the top 10% of all songs",
        percentile >= 75 ~ "very good - in the top 25% of songs",
        percentile >= 50 ~ "above average - better than half of all songs", 
        percentile >= 25 ~ "below average - in the bottom 50% of songs",
        TRUE ~ "poor - in the bottom 25% of songs"
      )
      
      # Combine the prediction with explanation text
      paste0("Predicted Popularity: ", round(predicted, 1), "/100\n\n",
             "This score is ", context, " in our dataset.\n\n",
             "Percentile ranking: ", round(percentile, 1), "%")
    } else {
      "Click PREDICT POPULARITY to see analysis"
    }
  })
  
  # Create a histogram graph showing where the prediction falls relative to all real songs
  # This visual helps users see exactly how their song compares to the entire dataset
  output$predictionPlot <- renderPlot({
    req(input$predict_btn)
    model <- current_model()
    combo_key <- model$Combo_Key
    
    if(combo_key %in% names(coefficients_storage)) {
      coeffs <- coefficients_storage[[combo_key]]
      predicted <- predict_popularity(coeffs, input)
      
      ggplot(spotify, aes(x = Popularity)) +
        # Show the distribution of all songs as green bars
        geom_histogram(binwidth = 2, fill = spotify_colors$primary_green, color = "white", alpha = 0.7) +
        # Add a purple line showing where the user's prediction falls
        geom_vline(xintercept = predicted, color = "purple", linetype = "dashed", linewidth = 1.5) +
        # Add a gray line showing the average for comparison
        geom_vline(xintercept = mean(spotify$Popularity), color = "gray50", linetype = "dotted", linewidth = 1) +
        # Label the prediction line so users know which line is theirs
        annotate("text", x = predicted, y = Inf, 
                 label = paste("Your Song:", round(predicted, 1)), 
                 vjust = 1.2, hjust = ifelse(predicted > 50, 1.1, -0.1),
                 color = "purple", fontface = "bold") +
        # Label the average line for context
        annotate("text", x = mean(spotify$Popularity), y = Inf,
                 label = paste("Average:", round(mean(spotify$Popularity), 1)),
                 vjust = 2.5, hjust = 0.5, color = "gray50") +
        labs(title = "Your Song's Popularity vs. All Songs",
             x = "Popularity Score",
             y = "Number of Songs") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          axis.title = element_text(size = 12, face = "bold")
        )
    }
  })
  
  # INTERPRETATION TAB OUTPUTS
  
  #this gives the user a summary of the currently selected model's configuration
  output$selectedModelSummary <- renderTable({
    # add dependency on sliders to force reactivity when changed
    input$genre_limit
    input$artist_limit
    
    best <- current_model()
    
    # These are the most important values for understanding the model's performance
    data.frame(
      Metric = c("Top Genres", "Top Artists", "Alpha", "Lambda", "MSE", "R²"),
      Value = c(
        best$Genre_Limit,           # How many genre categories the model uses
        best$Artist_Limit,          # How many artist categories the model uses  
        best$Best_Alpha,            # Regularization mixing parameter
        round(best$Best_Lambda, 4), # Regularization strength
        round(best$MSE, 3),         # Mean squared error (lower = better)
        round(best$R_Squared, 4)    # R-squared (higher = better, max = 1.0)
      )
    )
  }, rownames = FALSE)
  
  # This provides a explanation in a simple way.
  output$interpretationText <- renderText({
    # Add dependency on sliders to force reactivity when users move them
    input$genre_limit
    input$artist_limit
    
    best <- current_model()
    
    # Explain the type of regularization in simple terms
    # Alpha controls the type of regularization, which affects which variables get included
    alpha_explanation <- case_when(
      best$Best_Alpha == 0 ~ "Ridge regression (α=0) was used, which applies L2 regularization. This tends to shrink coefficients towards zero but keeps all variables in the model.",
      best$Best_Alpha == 1 ~ "Lasso regression (α=1) was used, which applies L1 regularization. This can set some coefficients exactly to zero, performing automatic feature selection.",
      TRUE ~ paste0("Elastic Net with α=", best$Best_Alpha, " was used, combining both L1 and L2 regularization for a balanced approach.")
    )
    
    # Explain what the lambda value means in simple terms
    lambda_explanation <- paste0("The regularization strength λ=", round(best$Best_Lambda, 4), 
                                 ifelse(best$Best_Lambda < 0.1, " indicates light regularization.",
                                        ifelse(best$Best_Lambda < 0.5, " indicates moderate regularization.", 
                                               " indicates strong regularization.")))
    
    # giving a explanation what the R2 is in simple terms
    performance_explanation <- paste0("The model achieves an R² of ", round(best$R_Squared, 4), 
                                      ", meaning it explains ", round(best$R_Squared * 100, 1), 
                                      "% of the variance in song popularity. The MSE of ", 
                                      round(best$MSE, 3), " indicates the average squared prediction error.")
    
    # explains what audio features the model uses
    feature_explanation <- paste0("The model uses the top ", best$Genre_Limit, " most common genres and top ", 
                                  best$Artist_Limit, " most frequent artists, along with audio features like danceability, energy, and valence.")
    
    # Combine all the explanations into a simple interpretation
    paste(alpha_explanation, lambda_explanation, performance_explanation, feature_explanation, sep = "\n\n")
  })
  
  # Handle the update button click just for feedback,
  # So the user will know the app works
  observeEvent(input$updateModel, {
    showNotification("Model updated with current parameters!", type = "message", duration = 3)
  })
}


# This takes all the user inputs and converts them into a popularity prediction
predict_popularity <- function(coeffs, input_values) {
  # Get intercept - this is the baseline prediction when all variables are 0
  prediction <- as.numeric(coeffs["(Intercept)", 1])
  
  # Add coefficients from each variable if it exists in the model
  var_names <- rownames(coeffs)
  
  # if else checks for each variable to see if it exists in the model
  artist_var <- paste0("Artist", input_values$pred_artist)
  if(artist_var %in% var_names) {
    prediction <- prediction + as.numeric(coeffs[artist_var, 1])
  }
  
  # Genre contribution
  genre_var <- paste0("Top.Genre", gsub(" ", ".", input_values$pred_genre))
  if(genre_var %in% var_names) {
    prediction <- prediction + as.numeric(coeffs[genre_var, 1])
  }
  
  # Audio features, scaled like in the precomputed data
  audio_features <- c("Energy", "Danceability", "Loudness..dB.", "Valence", "Acousticness", "Beats.Per.Minute..BPM.")
  input_audio <- c(input_values$pred_energy, input_values$pred_danceability, input_values$pred_loudness,
                   input_values$pred_valence, input_values$pred_acousticness, input_values$pred_bpm)
  
  for(i in 1:length(audio_features)) {
    if(audio_features[i] %in% var_names) {
      scaled_val <- (input_audio[i] - 50) / 25
      prediction <- prediction + as.numeric(coeffs[audio_features[i], 1]) * scaled_val
    }
  }
  
  # Log features
  # These were log-transformed in the original data to handle skewness
  if("Log.Duration" %in% var_names) {
    scaled_log_duration <- (input_values$pred_log_duration - 5.5) / 1 
    prediction <- prediction + as.numeric(coeffs["Log.Duration", 1]) * scaled_log_duration
  }
  
  if("Log.Speechiness" %in% var_names) {
    scaled_log_speech <- (input_values$pred_log_speechiness - 2) / 2
    prediction <- prediction + as.numeric(coeffs["Log.Speechiness", 1]) * scaled_log_speech
  }
  
  if("Log.Liveness" %in% var_names) {
    scaled_log_live <- (input_values$pred_log_liveness - 2.5) / 1.5
    prediction <- prediction + as.numeric(coeffs["Log.Liveness", 1]) * scaled_log_live
  }
  
  # Decade dummies
  decade_var <- paste0("decade_group_", input_values$pred_decade)
  if(decade_var %in% var_names) {
    prediction <- prediction + as.numeric(coeffs[decade_var, 1])
  }
  
  return(prediction)
}

# helper function to calculate contributions of each variable to the prediction
calculate_contributions <- function(coeffs, input_values) {
  contributions <- data.frame(Variable = character(), Contribution = numeric(), stringsAsFactors = FALSE)
  
  var_names <- rownames(coeffs)
  
  # Artist contribution
  artist_var <- paste0("Artist", input_values$pred_artist)
  if(artist_var %in% var_names) {
    contributions <- rbind(contributions, 
                           data.frame(Variable = input_values$pred_artist, 
                                      Contribution = as.numeric(coeffs[artist_var, 1]),
                                      stringsAsFactors = FALSE))
  }
  
  # Genre contribution 
  genre_var <- paste0("Top.Genre", gsub(" ", ".", input_values$pred_genre))
  if(genre_var %in% var_names) {
    contributions <- rbind(contributions,
                           data.frame(Variable = input_values$pred_genre,
                                      Contribution = as.numeric(coeffs[genre_var, 1]),
                                      stringsAsFactors = FALSE))
  }
  
  # Audio features
  audio_features <- c("Energy", "Danceability", "Loudness..dB.", "Valence", "Acousticness", "Beats.Per.Minute..BPM.")
  input_audio <- c(input_values$pred_energy, input_values$pred_danceability, input_values$pred_loudness,
                   input_values$pred_valence, input_values$pred_acousticness, input_values$pred_bpm)
  
  for(i in 1:length(audio_features)) {
    if(audio_features[i] %in% var_names) {
      # Simplified scaling for display
      scaled_val <- (input_audio[i] - 50) / 25
      contrib <- as.numeric(coeffs[audio_features[i], 1]) * scaled_val
      contributions <- rbind(contributions,
                             data.frame(Variable = audio_features[i],
                                        Contribution = contrib,
                                        stringsAsFactors = FALSE))
    }
  }
  
  # Log features contributions
  if("Log.Duration" %in% var_names) {
    scaled_log_duration <- (input_values$pred_log_duration - 5.5) / 1
    contrib <- as.numeric(coeffs["Log.Duration", 1]) * scaled_log_duration
    contributions <- rbind(contributions,
                           data.frame(Variable = "Log Duration",
                                      Contribution = contrib,
                                      stringsAsFactors = FALSE))
  }
  
  if("Log.Speechiness" %in% var_names) {
    scaled_log_speech <- (input_values$pred_log_speechiness - 2) / 2
    contrib <- as.numeric(coeffs["Log.Speechiness", 1]) * scaled_log_speech
    contributions <- rbind(contributions,
                           data.frame(Variable = "Log Speechiness",
                                      Contribution = contrib,
                                      stringsAsFactors = FALSE))
  }
  
  if("Log.Liveness" %in% var_names) {
    scaled_log_live <- (input_values$pred_log_liveness - 2.5) / 1.5
    contrib <- as.numeric(coeffs["Log.Liveness", 1]) * scaled_log_live
    contributions <- rbind(contributions,
                           data.frame(Variable = "Log Liveness",
                                      Contribution = contrib,
                                      stringsAsFactors = FALSE))
  }
  
  # Sort by absolute contribution so the most important variables
  # This makes it easy to see what's driving the prediction
  contributions <- contributions[order(abs(contributions$Contribution), decreasing = TRUE), ]
  
  return(contributions)
}

# Run the app
shinyApp(ui = ui, server = server)