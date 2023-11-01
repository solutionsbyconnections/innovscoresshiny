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
library(DT)
library(gtsummary)
library(gtExtras)
library(mongolite)
#
print(serverInfo())
inserverflag <- serverInfo()$shinyServer

if (inserverflag) {
  print("Running in Shiny Server")
} else {
  print("Running locally")
  
  innovresultscon <- mongo(collection = "innovresultsactive",
                           url = "mongodb://roberts-macbook-pro.local:27017/cyberinnovation",
                           verbose = FALSE )
  
  innovresults <- innovresultscon$find('{}',
                                       fields = '{"_row": 0,
                                   "_id": 0,
                                   "Summary": 0,
                                   "SecondaryTopicCat": 0,
                                   "CategorySignature": 0,
                                   "KeyPhrases": 0,
                                   "AllUniquenessInnovPhrases": 0,
                                   "AllScaledValuation": 0,
                                   "AllInnovatorValuationRank": 0,
                                   "AllOutlierValuation": 0,
                                   "UniquenessSignature": 0 }')
  
  innovresults$rundate <- as.Date(innovresults$rundate)
  innovresults$BestFitCategory <- str_replace(innovresults$BestFitCategory, "Vertical-", "")
  innovresults$BestFitCategory <- str_replace(innovresults$BestFitCategory, "Finance", "")
  
  colnames(innovresults) <- c("Company", "Site", "Category", "Class", "Series",  "Funding", "Valuation", "Innovation",  "ScaledValuation", "Rank", "Outlier", "Sentiment", "Date")
  
  
  #  arrange(Category, group_by = TRUE)
  
  saveRDS(innovresults, "Data/innovdf.RDS")
}

innovresultsin  <- readRDS("Data/innovdf.RDS")

listofdates <- innovresultsin$Date %>% 
  unique() %>%
  sort(decreasing = TRUE)
print(listofdates)

# the selection list
listofmodes <- c("All", "Outliers", "Late Stage Companies", "Early Stage Companies")

innovresultsin <- dplyr::mutate(innovresultsin, OutlierColor = ifelse(Outlier == "Yes", 8, 4))

innovresultsin <- innovresultsin %>%
  group_by(Category) %>%
  mutate(median_innovation = median(Innovation)) %>%
  mutate(median_scaledvaluation = median(ScaledValuation)) %>%
  ungroup()


dttable <- innovresultsin

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Innovation Scores Interactive"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          actionButton("goButton", "Click to Visualize"),
          selectInput("dateInput", h3("Choose a date:"), choices = listofdates),
          selectInput("select", h3("Select Data Filter"), choices = listofmodes),
          uiOutput("statplot")
          
          
          #actionButton("view_table", "View Table")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          uiOutput("iframe"),
          DTOutput("datatable")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  observeEvent(input$goButton, {
    thedate <- input$dateInput
    print(thedate)
    # Select the date of interest
    innovresultsin <- dplyr::filter(innovresultsin, innovresultsin$Date == thedate)
    # The select the display mode
    theselection <- input$select
    print(theselection)
    
    # Filter innovresults based on selection of filter
    if (theselection == listofmodes[1]) { 
      innovresults <-innovresultsin
    } else if (theselection == listofmodes[2]) { 
      innovresults <- dplyr::filter(innovresultsin, innovresultsin$Outlier == "Yes")
    } else if (theselection == listofmodes[3]) {
      innovresults <- dplyr::filter(innovresultsin, innovresultsin$Series >= "Series B")
    } else if (theselection == listofmodes[4]) {
      innovresults <- dplyr::filter(innovresultsin, innovresultsin$Series < "Series B")
    } else {
      innovresults <- innovresultsin
    }
    
    # Select the subset to view
    dttable <- innovresults |>
      dplyr::select(Company, Site, Category, Class, Series, Funding, Valuation, Innovation, ScaledValuation, Rank, Outlier, Sentiment, Date)
    
    print(nrow(innovresults))
    print(nrow(dttable))
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
      geom_point(aes(size = Sentiment)) +
      
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
                             description = paste("Innovation Scores - Calculated by Category with filter: ", theselection, " for: ", thedate  ), path = "www"
    )
    tdf <- left_join(tdf, innovstats, by = "Category") |>
      set_default_labels( "Category") |>
      set_default_layout(ncol = 2) 
    
    write_trelliscope(tdf, jsonp = TRUE, force_write = TRUE)
    
    # Render the selected plot
    output$iframe <- renderUI({
      tags$iframe(src = "index.html", style="border: none; width: 100%; height: 1200px;")
    })
    
    # Render the data table
    output$datatable <- DT::renderDT({
      datatable(dttable, selection = "single", filter = "top",
                extensions = 'Buttons', 
                options = list(
                  dom = 'lBfrtip',
                  buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                  pageLength = 10,               # Initial number of rows displayed
                  lengthMenu = list(c(10, 25, 50, -1),   # Values for the dropdown
                                    c('10', '25', '50', 'All')),
                  #language = list(lengthMenu = "_MENU_  "),
                  #
                  columnDefs = list(
                    list(
                      targets = 2, 
                      render = JS("function(data, type, row, meta) {
                          return '<a href=\"' + row[2] + '\" target=\"_blank\">' + data + '</a>';
                      }"
                 ))))
                
                )
    })
    
    # Render the stats plot
    output$statplot <- renderUI({
      gt_plt_summary(dttable, title = "Summary Stats")
    })
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
