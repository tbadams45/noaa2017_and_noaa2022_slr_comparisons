#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(shiny)
library(leaflet)
library(htmltools)
library(ggthemes)
library(cowplot)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Comparisons between NOAA 2017 and NOAA 2022 Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput("pubsToPlot", "Publications to Plot", choices = c("NOAA 2017", "NOAA 2022"), selected = c("NOAA 2017", "NOAA 2022")),
            checkboxGroupInput("ScenToPlot", "Scenarios to Plot", 
                               choices  = c("Low", "Intermediate Low", "Intermediate", "Intermediate High", "High"),
                               selected = c("Low", "Intermediate Low", "Intermediate", "Intermediate High", "High")),
            textInput("SiteID",
                        "Site ID (find by panning the map and clicking the location you want - note that this is case sensitive.):",
                        value = "235"),
            leafletOutput("mymap")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           p("Both graphs show the same data; the top plot is interactive while the bottom plot is static."),
           plotlyOutput("NOAAComparisonPlot"),
           plotOutput("NOAAComparisonStatic"),
           p("NOTE: the table will only show data if you have both the NOAA 2017 and NOAA 2022 scenarios selected. If you want to copy the table to excel, you have to go through a few steps. What worked for me is to highlight the data you want, copy it, open Notepad, paste into Notepad. Then copy what you have in Notepad and paste into Excel."),
           dataTableOutput("NOAAComparisonTable")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    gage_data <- read_csv("./data/gages_and_grid_cells.csv") %>%
        filter(Site != "GMSL") %>%
        filter(is.na(str_locate(Site, "grid")))
    
    noaa2017_data <- read_csv("./data/techrpt083_data_as_readable_table.csv") %>%
        select(-Latitude, -Longitude, -Background_RSL_rate_mm_per_yr, -RSL_2120_cm, -RSL_2150_cm, -RSL_2200_cm) %>%
        pivot_longer(cols = contains("RSL"), names_to = "year_text", values_to = "SLR_since_2000_cm") %>%
        mutate(year = as.integer(str_extract(year_text, "[0-9]+"))) %>%
        mutate(SLR_since_2000_m = SLR_since_2000_cm / 100) %>%
        filter(Scenario %in% c("0.3 - MED", "0.5 - MED", "1.0 - MED", "1.5 - MED", "2.0 - MED")) %>%
        mutate(pub = "NOAA 2017") %>%
        mutate(Scenario = case_when(
            Scenario == "0.3 - MED" ~ "Low",
            Scenario == "0.5 - MED" ~ "Intermediate Low",
            Scenario == "1.0 - MED" ~ "Intermediate",
            Scenario == "1.5 - MED" ~ "Intermediate High",
            Scenario == "2.0 - MED" ~ "High"))
    
    noaa2022_data <- read_csv("./data/SLR_TF U.S. Sea Level Projections_NOAA_2022_readable_table.csv") %>%
        select(Site = `PSMSL Site`, 
               PSMSL_ID = `PSMSL ID`,
               Scenario, 
               offset_2000_to_2005_cm = `Offset 2000 to 2005 (cm)`, 
               RSL2005_cm = `RSL2005 (cm)`, 
               RSL2020_cm = `RSL2020 (cm)`, 
               RSL2030_cm = `RSL2030 (cm)`, 
               RSL2040_cm = `RSL2040 (cm)`, 
               RSL2050_cm = `RSL2050 (cm)`, 
               RSL2060_cm = `RSL2060 (cm)`, 
               RSL2070_cm = `RSL2070 (cm)`, 
               RSL2080_cm = `RSL2080 (cm)`, 
               RSL2090_cm = `RSL2090 (cm)`, 
               RSL2100_cm = `RSL2100 (cm)`) %>%
        pivot_longer(cols = contains("RSL"), names_to = "year_text", values_to = "SLR_since_2005_cm") %>%
        mutate(year = as.integer(str_extract(year_text, "[0-9]+"))) %>%
        mutate(SLR_since_2000_cm = SLR_since_2005_cm + offset_2000_to_2005_cm) %>%
        mutate(SLR_since_2000_m = SLR_since_2000_cm / 100) %>%
        filter(Scenario %in% c("0.3 - MED", "0.5 - MED", "1.0 - MED", "1.5 - MED", "2.0 - MED")) %>%
        mutate(pub = "NOAA 2022") %>%
        mutate(Scenario = case_when(
            Scenario == "0.3 - MED" ~ "Low",
            Scenario == "0.5 - MED" ~ "Intermediate Low",
            Scenario == "1.0 - MED" ~ "Intermediate",
            Scenario == "1.5 - MED" ~ "Intermediate High",
            Scenario == "2.0 - MED" ~ "High"))
    
    all_data <- noaa2017_data %>%
        bind_rows(noaa2022_data)
    
    output$mymap <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)) %>%
            addMarkers(data = gage_data, popup = ~paste0(Site, ": ", PSMSL_ID))
    })
    
    plot_data_fxn <- function() {
        return(all_data %>% filter(PSMSL_ID == as.double(input$SiteID), 
                                   pub %in% input$pubsToPlot,
                                   Scenario %in% input$ScenToPlot))
        # return(all_data %>% filter(((Site == input$SiteID) | (str_replace(input$SiteID, " ", "_") == Site)), 
        #                            pub %in% input$pubsToPlot,
        #                            Scenario %in% input$ScenToPlot))
    }
    
    output$NOAAComparisonPlot <- renderPlotly({
        ggplotly(
            ggplot(plot_data_fxn(), aes(year, SLR_since_2000_m, color = Scenario, linetype = pub)) +
                geom_line(size = 1) +
                scale_y_continuous(limits = c(0, 3)) +
                scale_x_continuous(limits = c(2000, 2100), breaks = seq(2000, 2100, by = 10)) +
                scale_linetype_manual(values = c("NOAA 2017" = "dashed", "NOAA 2022" = "solid")) +
                labs(x = "Year", y = "SLR in meters since 2000", lintype = "Publication") +
                theme_cowplot() +
                background_grid(major = "xy", minor = "y")
        )
    })
    

    output$NOAAComparisonStatic <- renderPlot({
        ggplot(plot_data_fxn(), aes(year, SLR_since_2000_m, color = Scenario, linetype = pub)) +
            geom_line(size = 1) +
            scale_y_continuous(limits = c(0, 3)) +
            scale_x_continuous(limits = c(2000, 2100), breaks = seq(2000, 2100, by = 10)) +
            scale_linetype_manual(values = c("NOAA 2017" = "dashed", "NOAA 2022" = "solid")) +
            labs(x = "Year", y = "SLR in meters since 2000", lintype = "Publication") +
            theme_cowplot() +
            background_grid(major = "xy", minor = "y")
    })
    
    output$NOAAComparisonTable <- renderDataTable({
        plot_data_fxn() %>% 
            select(Site, Scenario, Year = year, SLR_since_2000_m, pub) %>%
            pivot_wider(names_from = "pub", values_from = c("SLR_since_2000_m")) %>%
            rename(NOAA_2017_SLR_since_2000_m = `NOAA 2017`,
                   NOAA_2022_SLR_since_2000_m = `NOAA 2022`) %>%
            arrange(Site, Scenario, Year)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
