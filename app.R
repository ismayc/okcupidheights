if (!("shiny" %in% names(installed.packages()[,"Package"]))) {install.packages("shiny")}
suppressMessages(library(shiny, quietly = TRUE))

if (!("okcupiddata" %in% names(installed.packages()[,"Package"]))) {install.packages("okcupiddata")}
suppressMessages(library(okcupiddata, quietly = TRUE))
data(profiles)

if (!("tidyverse" %in% names(installed.packages()[,"Package"]))) {install.packages("tidyverse")}
suppressMessages(library(tidyverse, quietly = TRUE))

if (!("scales" %in% names(installed.packages()[,"Package"]))) {install.packages("scales")}
suppressMessages(library(scales, quietly = TRUE))

if (!("magrittr" %in% names(installed.packages()[,"Package"]))) {install.packages("magrittr")}
suppressMessages(library(magrittr, quietly = TRUE))

if (!("gridExtra" %in% names(installed.packages()[,"Package"]))) {install.packages("gridExtra")}
suppressMessages(library(gridExtra, quietly = TRUE))

filtered <- profiles %>% na.omit() %>% 
  filter(between(height, 55, 80)) 
pop_size <- filtered %>% nrow()
min_filtered <- min(filtered$height)
max_filtered <- max(filtered$height)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
  "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
statistic <- vector(mode = "numeric")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Self-reported Heights of San Francisco OKCupid users"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("sample_size",
        "Sample size:",
        min = 5,
        max = pop_size,
        value = 100),
      actionButton(inputId = "sample_one", label = "Generate a sample")
    ),
    
    # Show plots of population distribution, current sample,
    # and sampling distribution
    mainPanel(
      plotOutput("pop_plot", height = "300px"),
      hr(),
      plotOutput("samp_plots", height = "600px")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$pop_plot <- renderPlot({
    profiles %>% na.omit() %>% 
      filter(between(height, 55, 85)) %>% 
      ggplot(mapping = aes(x = height)) +
      geom_histogram(aes(y = ..count../sum(..count..)), 
        color = "white", bins = 20) +
      scale_y_continuous(labels = percent_format()) +
      scale_x_continuous(breaks = pretty_breaks()) +
      coord_cartesian(xlim = c(55, 85)) +
      labs(title = "Population Distribution of Heights",
        x = "Height",
        y = "Percent in Each Bin")
  })
  
  output$samp_plots <- renderPlot({
    
    rand_color <- sample(cbPalette, size = 1, replace = FALSE)
    sampled <- eventReactive(input$sample_one, {
      profiles %>%
        filter(between(height, 55, 85)) %>%
        sample_n(size = input$sample_size) })
    sample_mean <- mean(sampled()$height)
    
    statistic <<- append(x = statistic, values = sample_mean)
    last <- vector(mode = "logical", length = length(statistic)) 
    last[seq_along(statistic)] <- FALSE
    last[length(statistic)] <- TRUE
    df <- data_frame(statistic, last)
    
    sample_plot <- sampled() %>%
      ggplot(mapping = aes(x = height)) +
      geom_histogram(aes(y = ..count.. / sum(..count..)),
        color = "white", fill = rand_color, bins = 20) +
      scale_y_continuous(labels = percent_format()) +
      scale_x_continuous(breaks = pretty_breaks()) +
      coord_cartesian(xlim = c(55, 85)) +
      geom_vline(color = "black", xintercept = sample_mean) +
      annotate("text", x = 66, y = 0.12,
        label = paste("Observed~bar(italic(x)) == ", round(sample_mean, 2)),
        hjust = 1.5, vjust = 1, parse = TRUE, size = 5) +
      labs(title = "Sample Distribution of Heights",
        x = "Height",
        y = "Percent in Each Bin")
   
   sampl_dist <- df %>%
     ggplot(mapping = aes(x = statistic)) +
     geom_histogram(#aes(y = ..count.. / sum(..count..)),
       color = "white",
       aes(fill = last),
       bins = 20) +
     scale_fill_manual(values = c("FALSE" = "black", "TRUE" = rand_color)) +
     scale_y_continuous(breaks = 1:1000) +
     scale_x_continuous(breaks = pretty_breaks()) +
#     coord_cartesian(xlim = c(55, 85)) +
     labs(title = "Sampling Distribution of Sample Means",
       x = "Sample Mean", y = "Number in Each Bin") +
     guides(fill = FALSE)
   grid.arrange(sample_plot, sampl_dist, nrow = 2, ncol = 1)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)


