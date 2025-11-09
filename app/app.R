library(shiny)
library(tidyverse)

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

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
