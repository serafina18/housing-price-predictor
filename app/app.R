library(shiny)
library(tidyverse)
library(equatiomatic)
library(bslib)
library(thematic)

data <- read.delim(
  "housing-prices-ge19.txt",
  header = TRUE, sep = "\t",
  quote = "", comment.char = "",
  check.names = TRUE
)

data <- data %>%
  mutate(
    Waterfront   = as.factor(Waterfront),
    New.Construct = as.factor(New.Construct),
    Central.Air  = as.factor(Central.Air),
    Fuel.Type    = as.factor(Fuel.Type),
    Heat.Type    = as.factor(Heat.Type),
    Sewer.Type   = as.factor(Sewer.Type),
  )

# Clean
data_cleaned <- data %>%
  filter(Lot.Size > 0) %>%
  filter(!(Age == 0 & New.Construct == 0))

data_cleaned <- data_cleaned %>%
  mutate(
    Log.Price = log(Price),
    Log.Lot.Size = log(Lot.Size),
    Log.Age = ifelse(Age != 0, log(Age), 0),
    Log.Land.Value = log(Land.Value)
  ) %>%
  select(-Price, -Lot.Size, -Age, -Land.Value, -Test)

# Final model
final_model <- lm(formula = Log.Price ~ Bathrooms + Heat.Type + Living.Area + 
                    Log.Age + Log.Land.Value + New.Construct + Waterfront,
                  data = data_cleaned)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
    titlePanel(""),

    # Application title
    titlePanel("House prices in Saratoga, New York"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput("bathrooms",
                        "Number of bathrooms:",
                        val = 2,
                        min = 0.5, max = 7, step = 0.5),
            selectInput("heat_type",
                        "Type of heating system:",
                        choices = c("Electric", "Hot Air", "Hot Water", "None")),
            numericInput("living_area",
                         "Living area (square feet):",
                         value = 1610,
                         min = 1),
            numericInput("age",
                         "Age of house (years)",
                         value = 19,
                         min = 0, step = 1),
            numericInput("land_value",
                         "Value of land (US dollars):",
                         value = 25000,
                         min = 1),
            checkboxInput("new_construct",
                          "Is a new construction?"),
            checkboxInput("waterfront", 
                          "Includes waterfront?"),
            selectInput("sig_lvl",
                        "Significance level:",
                        choices = c("0.90", "0.95")),
        ),

        # Show a plot of the generated distribution
        mainPanel(
          withMathJax(),
          uiOutput("formula"),
          uiOutput("prediction"),
          uiOutput("ci"),
          uiOutput("pi"),
          imageOutput("image")
        )
    ),
    
    theme = bs_theme(bootswatch = "pulse"),
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  thematic::thematic_shiny()
  
    output$formula <- renderUI({
      withMathJax(
        HTML(
          equatiomatic::extract_eq(final_model, wrap = TRUE, terms_per_line = 3,
                                   use_coefs = TRUE, coef_digits = 4)
        )
      )
    })
    
    # output$histogram
    
    new_obs <- reactive({
      new_construct <- ifelse(input$new_construct == FALSE, 0, 1)
      waterfront <- ifelse(input$waterfront == FALSE, 0, 1)
      data.frame(Bathrooms = input$bathrooms, Heat.Type = input$heat_type,
                 Living.Area = input$living_area,
                 Log.Age = log(input$age), Log.Land.Value = log(input$land_value),
                 New.Construct = factor(new_construct, levels = c("0", "1")),
                 Waterfront = factor(waterfront, levels = c("0", "1")))
    })
    
    predict_pi <- reactive({
      as.data.frame(predict(final_model, new_obs(), interval = "prediction", level = as.integer(input$sig_lvl)))
    })
    
    predict_ci <- reactive({
      as.data.frame(predict(final_model, new_obs(), interval = "confidence", level = as.integer(input$sig_lvl)))
    })
    
    output$prediction <- renderUI({
      #paste("Predicted house price in USD is ", round(predict_pi()$fit,5), ".")
      paste("Predicted house price in USD is ", predict_pi()$fit, ".")
    })
    
    output$pi <- renderUI({
      tagList(
        #paste0("The prediction interval is (", round(predict_pi()$lwr,5), ", ", round(predict_pi()$upr,5), ")"),
        paste0("The prediction interval is (", predict_pi()$lwr, ", ", predict_pi()$upr, ")"),
        p()
      )
    })
    
    output$ci <- renderUI({
      tagList(
        paste0("The confidence interval is (", predict_ci()$lwr, ", ", predict_pi()$upr, ").")
      )
    })
    
    #output$ci
      
    output$image <- renderImage( 
      { 
        list(src = "house.jpeg", height = "70%") 
      }, 
      deleteFile = FALSE 
    ) 
}

# Run the application 
shinyApp(ui = ui, server = server)
