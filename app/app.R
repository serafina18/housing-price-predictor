library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(equatiomatic)
library(bslib)
library(thematic)
library(gt)
library(mplot)

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
  select(-Test)

# Final model
final_model <- lm(formula = Log.Price ~ Bathrooms + Heat.Type + Living.Area + 
                    Log.Age + Log.Land.Value + New.Construct + Waterfront,
                  data = data_cleaned)

load("modelStabReport.RData")

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Housing Prices in Saratoga, New York", 
                  titleWidth= 350),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("info-circle")),
      menuItem("Selected Model", tabName = "model", icon = icon("chart-line")),
      menuItem("Predict Housing Price", tabName = "prediction", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
  
      tabItem(tabName = "about",
              h2("About the study"),
              fluidRow(
              box(title = "Purpose", status = "primary", solidHeader = TRUE, width=4,
                  tags$p("This app employs a log-transformed multiple regression model to examine how changeable and fixed housing attributes influence property price in Saratoga, New York."),
                  tags$p("The results provide practical guidance for homeowners seeking to maximise housing price through renovations, and for prospective buyers and sellers to make accurate price valuations and informed investment decisions."),
                  tags$p("Toggle through to estimate the price of your house, given inputted variable values!")
                  ),
              box(width=4,
                  tags$img(
                    src = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcR-hkoPcq7iZUC0iWY9LYHjNxPuj_CiKvxrrg&s",
                    style = "width:100%; height:auto; border-radius:8px; box-shadow:0 0 8px rgba(0,0,0,0.1);"
                  )
                ),

              box(title = "How to use", status = "danger", solidHeader = TRUE, width=4,
                  tags$p("Users can toggle between tabs by clicking on the sidebar menu:"),
                  tags$ul(
                    tags$li("Selected Model: visualises model stability and variable inclusion; states the selected model and coefficient interpretations."),
                    tags$li("Predict Housing Price: predicts housing price using user inputs for the variables in the Selected Model. The prediction interval and confidence interval is outputted, to a level of the user’s choice.")
                    )),
              box(title = "Data Cleaning", status = "warning", solidHeader = TRUE, width=12,
                  tags$p("The data contained no missing values. Invalid and inconsistent entries were filtered out, including where Lot Size value was 0, and where Age was 0 while New Construction Status was False. Certain numeric variables such as Heat Type were also converted into categorical data."),
                  tags$p("Log transformation was applied for variables demonstrating positive skew, with most observations cluttered around the lower end. This included the variable Price (visualised in the histogram below) and predictors such as Lot Size, Age, and Land Value. These variables were transformed to stabilise variance and satisfy normality assumptions."),
                  plotOutput("histogram", height = "350px")
              ))),
      
      tabItem(tabName = "model",
              h2("Selected Model"),
              fluidRow(
                box(title = "Model Stability Plot", status = "primary", solidHeader = TRUE, width = 6,
                    plotOutput("stability_plot", height = "350px")),
                box(title = "Variable Inclusion Plot", status = "primary", solidHeader = TRUE, width = 6,
                    plotOutput("inclusion_plot", height = "350px"))
              ),
              fluidRow(
                box(title = "Model", status = "danger", solidHeader = TRUE, width = 12,
                    withMathJax(),
                    uiOutput("formula1")),
                box(title = "Coefficient Interpretation", status = "warning", solidHeader = TRUE, width = 12,
                    gt::gt_output("coef_table"))
              )),
              
      tabItem(tabName = "prediction",
              h2(" Prediction"),
              fluidRow(
                box(title = "Input Variables", status = "primary", solidHeader = TRUE, width = 4,
                    sliderInput("bathrooms", "Number of bathrooms (half bathroom represents no shower or bathtub):", value = 2, min = 0, max = 4.5, step = 0.5),
                    selectInput("heat_type", "Heating system:", choices = c("Electric", "Hot Air", "Hot Water", "None")),
                    #numericInput("living_area", "Living area (sq ft):", value = 1610, min = 1),
                    sliderInput("living_area", "Living area (sq ft):", value = 1610, min = 616, max = 5228, step = 1),
                    sliderInput("age", "Age (years):", value = 19, min = 0, max=225),
                    #numericInput("land_value", "Land value ($USD):", value = 25000, min = 1),
                    sliderInput("land_value", "Land value ($USD):", value = 25000, min = 200, max = 255000, step = 1),
                    checkboxInput("new_construct", "New construction?"),
                    checkboxInput("waterfront", "Has waterfront?"),
                    sliderInput("conf_lvl", "Confidence level for intervals:", value = 0.9, min = 0, max = 0.99, step = 0.01),
                    #selectInput("conf_lvl", "Confidence level for intervals:", choices = c("0.90", "0.95", "0.99"))
                ),
                box(title = "Model", status = "danger", solidHeader = TRUE, width = 8,
                    withMathJax(),
                    uiOutput("formula2")),
                box(title = "Prediction", status = "warning", solidHeader = TRUE, width = 8,
                    withMathJax(),
                    uiOutput("prediction_val"),
                    uiOutput("ci"),
                    uiOutput("pi"),
                    uiOutput("validation")
                )
              )
      )
    )
  ),
  skin = "purple"
)


# SERVER
server <- function(input, output) {
  
  thematic::thematic_shiny()
  
  options(scipen = 100)
  
  output$histogram <- renderPlot({
    ggplot(data, aes(x = Price)) + 
      geom_histogram(color = "darkslateblue", fill = "darkslategray3", bins = 50) +
      labs(title = "Histogram Distribution of Housing Prices in New York",
           x = "Housing Price ($USD)", y = "Frequency") +
      geom_vline(aes(xintercept = mean(Price)), colour = "red", linewidth = 1) +
      geom_vline(aes(xintercept = median(Price)), colour = "deeppink", linewidth = 1) +
      annotate("label", 
               x = mean(data$Price), y = 230, label = "Mean",
               fill = "red", color = 'white', size = 3.5) +
      annotate("label", 
               x = median(data$Price), y = 215, label = "Median",  
               fill = "deeppink", color = 'white', size = 3.5)
  })
  
  output$coef_table <- render_gt({
    effects <- tibble::tribble(
      ~Variable,           ~`Change in unit`, ~`Impact on price (%)`,
      "Bathrooms",         "+1",              "+10.42%",
      "Heat: hot air",     "Yes",             "+9.83%",
      "Heat: hot water",   "Yes",             "+7.69%",
      "Heat: none",        "Yes",             "-32.49%",
      "Living area",       "+100 sq ft",      "+3.00%",
      "Age",               "+10%",            "-0.57%",
      "Land value",        "+10%",            "+1.25%",
      "New construction",  "Yes",             "-22.68%",
      "Waterfront",        "Yes",             "+74.85%"
    )
    
    gt::gt(effects) %>%
      gt::tab_header(title = "Interpretation of Coefficient Effects") %>%
      gt::fmt_markdown(columns = everything())
  })
  
  #output$stability <- renderPlot({
  #  withSpinner(plotOutput(outputId="stability_plot", height = "350px"))
  #})
  
  output$stability_plot <- renderPlot({
    plot(vis.M1, which = "boot", interactive = FALSE)
  })
  
  output$inclusion_plot <- renderPlot({
    plot(vis.M1, which = "vip", interactive = FALSE)
  })
  
  output$formula1 <- renderUI({
    withMathJax(
      HTML(
        equatiomatic::extract_eq(final_model, wrap = TRUE, terms_per_line = 6,
                                 use_coefs = TRUE, coef_digits = 4)
      )
    )
  })
  
  output$formula2 <- renderUI({
    withMathJax(
      HTML(
        equatiomatic::extract_eq(final_model, wrap = TRUE, terms_per_line = 3,
                                 use_coefs = TRUE, coef_digits = 4)
      )
    )
  })
  
  new_obs <- reactive({
    new_construct <- ifelse(input$new_construct == FALSE, 0, 1)
    waterfront <- ifelse(input$waterfront == FALSE, 0, 1)
    data.frame(Bathrooms = input$bathrooms, Heat.Type = input$heat_type,
               Living.Area = input$living_area,
               Log.Age = ifelse(input$age != 0, log(input$age), 0),
               Log.Land.Value = log(input$land_value),
               New.Construct = factor(new_construct, levels = c("0", "1")),
               Waterfront = factor(waterfront, levels = c("0", "1")))
  })
  
  predict_pi <- reactive({
    as.data.frame(predict(final_model, new_obs(), interval = "prediction", level = as.numeric(input$conf_lvl)))
  })
  
  predict_ci <- reactive({
    as.data.frame(predict(final_model, new_obs(), interval = "confidence", level = as.numeric(input$conf_lvl)))
  })
  
  output$prediction_val <- renderUI({
    pred_val <- round(exp(predict_pi()$fit), 0) 
    HTML(paste0(
      "<h2 style='color:#2c3e50; font-weight:bold; text-align:center;'>
      Predicted House Price: $", format(pred_val, big.mark=","), 
      "</h1>"
    ))
  })
  
  output$ci <- renderUI({
    ci_lower <- round(exp(predict_ci()$lwr), 0)
    ci_upper <- round(exp(predict_ci()$upr), 0)
    HTML(paste0(
      "<p style='font-size:16px; text-align:center;'>
      <b>", input$conf_lvl*100, "% Confidence Interval:</b><br>
      $", format(ci_lower, big.mark=","), " – $", format(ci_upper, big.mark=","),
      "</p>"
    ))
  })
  
  output$pi <- renderUI({
    pi_lower <- round(exp(predict_pi()$lwr), 0)
    pi_upper <- round(exp(predict_pi()$upr), 0)
    HTML(paste0(
      "<p style='font-size:16px; text-align:center;'>
      <b>", input$conf_lvl*100, "% Prediction Interval:</b><br>
      $", format(pi_lower, big.mark=","), " – $", format(pi_upper, big.mark=","),
      "</p>"
    ))
  })
  
  output$validation <- renderUI({
    HTML(paste0(
      "<p style='font-size:16px;'>
      Input validation is achieved through the sliding scales, whose bounds reflect the minimum and maximum values of the dataset. This ensures the model is not extrapolated beyond realistic values.",
      "</p>"
    ))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
