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
              eDNA_metadata)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Haida Gwaii biodiversity"),

    # have layout with side bar that has inputs and main panel with outputs
    sidebarLayout(
        
        sidebarPanel(
            #choose between co1 and 12s
            selectInput(inputId = "data_set",
                        label = "Data set:",
                        choices = c("Co1" = "co1_taxonomy_metadata_long_nc_nsd_ei" ,
                                    "12s" = "s12_taxonomy_metadata_long_nc_nsd_ei")
                        ),
            selectInput(inputId = "taxonomic_level",
                        label = "Taxonomic level:",
                        choices = co1_taxonomy_metadata_long_nc_nsd_ei %>%
                            #select columns we want to be able to face by
                            select(phylum:species) %>%
                            #take column names
                            colnames()
                        ),
            selectInput(inputId = "include_na",
                        label = "Include NAs?",
                        choices = c("Yes" ,
                                    "No")
                        ),
            selectInput(inputId = "bar_position",
                        label = "Bar position:",
                        choices = c("fill", "stack")
                            )  
        ),

        # Show plots in the main pannel
        mainPanel(
           plotOutput("taxonomic_index_barPlot"),
           plotOutput("taxonomic_read_number_barPlot")
        )
    )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #make data for plots
    #grab dataset of interest
    intermediate_data <- reactive({
        get(input$data_set) %>%
            filter(kingdom == "Metazoa", #keep only metazoa
                   metazoa_holoplankton != 1, #remove holoplankton
                   metazoa_terrestrial != 1) %>% #remove terrestrial taxonomies
            #order x axis
            mutate(habitat = factor(habitat,
                                    levels = c("Kelp",
                                               "Seagrass",
                                               "Unvegetated",
                                               "Pelagic",
                                               "Inside kelp",
                                               "Control",
                                               "Outside kelp"
                                    ))) %>%
            arrange(habitat) %>%
            mutate(row_order = c(1:n())) %>%
            mutate(sample_id = fct_reorder(sample_id,
                                           row_order)) %>%
            group_by(sample_id) %>%
            mutate(x_order = group_indices()) %>%
            ungroup() %>% 
            #set up data for x axis label
            group_by(habitat) %>%
            mutate(habi_label = mean(x_order)) %>%
            mutate(habi_line = max(x_order)) %>% 
            ungroup()
    })
    #filter outs Nas if user chooses
    plot_data <- reactive({ if(input$include_na == "Yes"){
        #if the user wants nas then leave dataset as is
        intermediate_data()
    }
        else{
            #if user does not want nas then filter them out for taxonomy of interest
            intermediate_data() %>%
                filter(!is.na(get(input$taxonomic_level)))
        }
        
    })
    
    output$taxonomic_index_barPlot <- renderPlot({
        #make sure user has interacted
        req(input$data_set, 
            input$taxonomic_level, 
            #input$facet, 
            input$bar_position,
            #input$include_na
            )
        
        #plot the data
        plot_data() %>%
            ggplot(data = .,
                   aes(x = x_order,
                       y = taxonomic_read_index,
                       fill = class)) +
            geom_bar(stat = "identity", 
                     position = input$bar_position,
                     aes_string(fill = input$taxonomic_level)) +
            #make titles and labels based on inputs
            labs(title = 
                     paste(if_else(condition = input$data_set == "co1_taxonomy_metadata_long_nc_nsd_ei",
                                   true = "Co1",
                                   false = "12s"), 
                           input$taxonomic_level,
                           #"- ordered by", 
                           #input$facet,
                           sep = " "),
                 x = "Sample",
                 y = if_else(condition = input$bar_position == "fill",
                             true = "Relative taxonomic read index",
                             false = "Taxonomic read index"),
                 fill = input$taxonomic_level) +
            theme_classic()+
            scale_fill_manual(values=as.vector(glasbey(28))) +
            geom_vline(xintercept = plot_data() %>%
                           pull(habi_line)) +
            annotate(geom = "text",
                     x = c(plot_data() %>%
                               pull(habi_label) %>%
                               unique()
                     ),
                     y = if_else(condition = input$bar_position == "fill",
                                 true = -0.05,
                                 false = -0.15),
                     label = c(plot_data() %>%
                                   pull(habitat) %>%
                                   unique() %>%
                                   as.character()
                     ),
                     size = 5) +
            theme(
                axis.ticks.x = element_blank(),
                axis.text.x = element_blank(),
                axis.text.y = element_text(size = 10),
                axis.title = element_text(size = 15),
                title = element_text(size = 20)
            ) #+
            #scale_y_continuous(breaks = seq(0, by = 0.5))
        
    })
    output$taxonomic_read_number_barPlot <- renderPlot({
        #make sure user has interacted
        req(input$data_set, 
            input$taxonomic_level, 
            #input$facet, 
            input$bar_position,
            #input$include_na
            )
        
        #plot data
        plot_data() %>%
            ggplot(data = .,
                   aes(x = x_order,
                       y = taxonomic_read_raw,
                       fill = class)) +
            geom_bar(stat = "identity", 
                     position = input$bar_position,
                     aes_string(fill = input$taxonomic_level)) +
            labs(title = 
                     paste(if_else(condition = input$data_set == "co1_taxonomy_metadata_long_nc_nsd_ei",
                                   true = "Co1",
                                   false = "12s"), 
                           input$taxonomic_level,
                           #"- ordered by", 
                           #input$facet,
                           sep = " "),
                 x = "Sample",
                 y = if_else(condition = input$bar_position == "fill",
                                   true = "Relative taxonomic read number",
                                   false = "Taxonomic read number"),
                 fill = input$taxonomic_level) +
            theme_classic()+
            scale_fill_manual(values=as.vector(glasbey(28))) +
            geom_vline(xintercept = plot_data() %>%
                           pull(habi_line)) +
            annotate(geom = "text",
                     x = c(plot_data() %>%
                               pull(habi_label) %>%
                               unique()
                     ),
                     y = if_else(condition = input$bar_position == "fill",
                                 true = -0.05,
                                 false = -15),
                     label = c(plot_data() %>%
                                   pull(habitat) %>%
                                   unique() %>%
                                   as.character()
                     ),
                     size = 5) +
            theme(
                axis.ticks.x = element_blank(),
                axis.text.x = element_blank(),
                axis.text.y = element_text(size = 10),
                axis.title = element_text(size = 15),
                title = element_text(size = 20)
            ) #+
            #scale_y_continuous(breaks = seq(0, by = 0.5))
        })
    }

# Run the application 
shinyApp(ui = ui, server = server)
