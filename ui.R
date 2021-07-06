library(tidyverse)
library(shiny)
library(visNetwork)
library(htmlwidgets)
library(shinyWidgets) #install.packages("shinyWidgets")
library(leaflet)
library("shinythemes")


# UI -------------------
fluidPage(theme = shinytheme("lumen"),
          
          ### Title and instructions
          titlePanel("Mammal food webs without Late Pleistocene extinctions"),
          #h4("How have extinctions and range changes affected mammal predator-prey interactions?"),
          h4("Using deep learning to reconstruct how today's food webs would have looked without extinction or habitat loss."),
          
          ##### Top row
          
          ### Map
          # fluidRow(
          #   ,
          # ),
          
          
          ##### Bottom row
          sidebarPanel(collapsable = F,# width = "250px",
                       
                       ### Data controls
                       h5("Show food webs that would occur naturally without mammal extinctions and range changes or that currently occur"),
                       radioGroupButtons("web_type", 
                                         label = NULL,#"Food web type",
                                         c("Natural" = "Natural",
                                           "Current" = "Current")),
                       h5("Click on the map to show food webs from a different location"),
                       leafletOutput("mymap"),
                       br(),
                       h5("Click on a species in the food web to view a photo"),
                       imageOutput("mammImage")
          ),
          
          ### Network
          mainPanel(
            tabsetPanel(
              tabPanel("World wide (food) web",
                       visNetworkOutput("network_proxy_nodes", 
                                        height = "800px")),
              tabPanel("FAQ/Methods",
                       h4("What do the links mean?"),
                       p("Links between nodes show predator-prey interactions that are likely to occur among species present in the selected region. Using interaction probabilities modelled using neural networks, we show links where the predicted probability of interaction is over 0.5."),
                       p("The arrows point from consumers to their resources. Only mammals that primarily eat other mammals are shown as predators. Note that consumer interactions can also show scavenging."),
                       h4("Why doesn't a species that I know is present show up in the food web?"),
                       p("Only species where we can estimate that at least one interaction is likely (with interaction probability > 0.5) show up in the food web. So if a species doesn't have any mammal predator (say, a bat), then it won't show up in the food web. Or if we can't be relatively certain (again, probability > 0.5) that a predator consumes any particular prey species, then that predator also won't show up in the food web."), 
                       p("This is similar to how food webs are constructed when observed in the field. Researchers typically report the species that were observed to be involved in predator-prey interactions, but this overlooks species that might be present but were not observed interacting."),
                       h4("[Will add more here!]")),
              tabPanel("Help",
                       h4("Interacting with the food web"),
                       p("Click on a node to see a photo of the species and highlight the species within two degrees of separation. You can click and drag nodes to rearrange the food web."),
                       p("You can zoom in and out!"),
                       h4("Why doesn't a food web show up for a selected region I selected?"),
                       p("Food webs are only shown for regions where there's at least one predator-prey interaction in the natural scenario. For example, we don't make food web predictions for Hawai'i because there are no native mammals that primarily eat mammals."),
                       h4("[Let me know what could be improved or what is confusing and I can add it here!]"))
            )
          ),
          
          #fluidRow(verbatimTextOutput("code_network_id"))
          
)