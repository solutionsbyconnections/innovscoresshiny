#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(trelliscope)
library(tidyverse)
library(plotly)
library(purrr)

innovresultsin  <- readRDS("Data/innovdf.RDS")
innovresultsin <- dplyr::mutate(innovresultsin, OutlierColor = ifelse(Outlier == "Yes", 8, 4))

innovresultsin <- innovresultsin %>%
  group_by(Category) %>%
  mutate(median_innovation = median(Innovation)) %>%
  mutate(median_scaledvaluation = median(ScaledValuation)) %>%
  ungroup()




# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Innovation Scores Interactive"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput("select", h3("Select Data Filter"), choices = list("All" = 1, "Outliers" = 2, "Series B or Greater" = 3), selected = 1),
          actionButton("goButton", "Go")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           uiOutput("iframe") 
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  observeEvent(input$select, {
    theselection <- input$select
    print(theselection)
    
    # Filter innovresults based on selection of filter
    if (theselection > 1) {
      if (theselection == 2) { # Filter to only include Outliers
        innovresults <- dplyr::filter(innovresultsin, innovresultsin$Outlier == "Yes")
      } else {
        if (theselection == 3) {
          innovresults <- dplyr::filter(innovresultsin, innovresultsin$Series >= "Series B")
        }
      } 
    } else {
      innovresults <-innovresultsin
    } 
    
    print(nrow(innovresults))
    innovstats <- innovresults |>
      summarise(
        median_innovation = median(Innovation),
        median_scaled_valuation = median(ScaledValuation),
        median_funding = median(Funding), 
        median_valuation = median(Valuation),
        .by = Category
      )
    
    # generate bins based on input$bins from ui.R
    pp <- innovresults |> ggplot(aes(x = Innovation, 
                                     y = ScaledValuation, 
                                     colour = OutlierColor,
                                     text =  paste("Company: ", Company, 
                                                   "<br>Class: ", Class,  
                                                   "<br>Site: ", Site, 
                                                   "<br>Series: ", Series, 
                                                   "<br>Funding: ", Funding, 
                                                   "<br>Valuation: ", Valuation,
                                                   "<br>Sentimaent: ", Sentiment, 
                                                   "<br>Date: ", Date))) +
      geom_point() +
      theme_bw()+
      
      theme(legend.position="none") +
      geom_segment(aes(x = 0, y = 5.0, xend = 1.0, yend = 5.0)) +
      geom_segment(aes(x = 0.5, y = 0.0, xend = 0.5, yend = 10.0)) +
      ggplot2::annotate("text", x = 0.25, y = 2.5, alpha = 0.35, size = 4,  label = "1 - Early" ) +
      ggplot2::annotate("text", x = 0.25, y = 7.5, alpha = 0.35, size = 4, label = "2 - Fully Valued") +
      ggplot2::annotate("text", x = 0.75, y = 2.5, alpha = 0.35, size = 4, label = "3 - Innovators") +
      ggplot2::annotate("text", x = 0.75, y = 7.5, alpha = 0.35, size = 4, label = "4 - Leaders") +
      facet_panels(ggplot2::vars(Category), 
                   add_plot_metrics = TRUE) 
    
    paneldf <- as_panels_df(pp, 
                            panel_col = "panel", 
                            as_plotly = TRUE,
                            plotly_args= c(tooltip = "text"),
                            plotly_cfg = c(displayModeBar = FALSE)) 
    
    tdf <- as_trelliscope_df(paneldf, name = "Innovation Scoring",
                             description = "Innovation Scores - Calculated by Category", path = "www"
    )
    tdf <- left_join(tdf, innovstats, by = "Category") |>
      set_default_labels( "Category") |>
      set_default_layout(ncol = 2) 
    
    write_trelliscope(tdf, jsonp = TRUE, force_write = TRUE)
  })
  
  observeEvent(input$goButton, {
    output$iframe <- renderUI({
      tags$iframe(src = "index.html", style="border: none; width: 100%; height: 1200px;")
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
