#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(pals)
library(here)

#read in data
eDNA_metadata <- read_csv(here("data",
                               "eDNA_metadata.csv")) %>%
    #remove least cost distance data
    select(- starts_with("GH"))

co1_taxonomy_metadata_long_nc_nsd_ei <- read_csv(here("data",
                                                      "co1_taxonomy_long_nc_nsd_ei.csv")) %>%
    #merge with metadata
    left_join(.,
              eDNA_metadata)

s12_taxonomy_metadata_long_nc_nsd_ei <- read_csv(here("data",
                                                      "s12_taxonomy_long_nc_nsd_ei.csv")) %>%
    #merge with metadata
    left_join(.,
              eDNA_metadata) %>%
    #add empty column for filter
    mutate(metazoa_holoplankton = 0)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Haida Gwaii biodiversity"),

    # Sidebar with a select input for plotting
    sidebarLayout(
        sidebarPanel(
            #choose between co1 and 12s
            #selectInput(inputId = "data_set",
             #           label = "Data set:",
              #          choices = c("Co1",
               #                     "12s")
                #        ),
            selectInput(inputId = "facet",
                        label = "Facet by:",
                        choices = eDNA_metadata %>%
                            #select columns we want to be able to face by
                            select(sample_id,
                                   type,
                                   site,
                                   habitat,
                                   sample_site,
                                   coast_face) %>%
                            #take column names
                            colnames()
                        ),
            selectInput(inputId = "taxonomic_level",
                        label = "Taxonomic level:",
                        choices = co1_taxonomy_metadata_long_nc_nsd_ei %>%
                            #select columns we want to be able to face by
                            select(phylum:species) %>%
                            #take column names
                            colnames()
                        ),
            selectInput(inputId = "bar_position",
                        label = "Bar position:",
                        choices = c("fill", "stack")
                            )  
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("taxonomic_barPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$taxonomic_barPlot <- renderPlot({
        req(input$taxonomic_level, input$facet, input$bar_position)
        co1_taxonomy_metadata_long_nc_nsd_ei %>%
            filter(kingdom == "Metazoa", #keep only metazoa
                   metazoa_holoplankton != 1, #remove holoplankton
                   metazoa_terrestrial != 1) %>% #remove terrestrial taxonomies
            #remove classes that are NA - maybe its interesting to keep NAs in?
            #drop_na(input$taxonomic_level) %>% 
            #change order of sample_id based on habitat
            mutate(habitat = factor(habitat,
                                    levels = c("Pelagic",
                                               "Seagrass",
                                               "Unvegetated",
                                               "Kelp",
                                               "Inside kelp",
                                               "Control",
                                               "Outside kelp"
                                    ))) %>%
            ggplot(data = .,
                   aes(x = sample_id, 
                       y = taxonomic_read_index
                       )
                   ) +
            geom_bar(stat = "identity", 
                     position = input$bar_position,
                     aes_string(fill = input$taxonomic_level)) + 
            labs(title = 
                     paste("Co1", 
                           input$taxonomic_level,
                           "- ordered by", 
                           input$facet,
                           sep = " "),
                 x = "Sample",
                 y = "Relative taxonomic read index") +
            theme_classic() +
            theme(axis.text.x = element_blank(),
                  axis.text.y = 
                      element_text(size = 5)
            ) +
            scale_fill_manual(values=as.vector(glasbey(28)))# +
            #facet_wrap( ~ as.name(input$facet), 
             #           strip.position = "bottom",
              #          nrow = 2,
               #         ncol = 4,
                #        scales = "free_x"
            #)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
