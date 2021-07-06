library(shiny)
library(visNetwork)
library(htmlwidgets)
library(shinyWidgets) #install.packages("shinyWidgets")
library(leaflet)
library("shinythemes")


load("global_food_webs_data.RData")

# Server -----------------
function(input, output){
  
  ### Map
  output$mymap <- renderLeaflet({
    leaflet() %>%
      setView(lng = -100, lat = 20, zoom = 2) %>%
      addProviderTiles(providers$Esri.WorldImagery,
                       options = providerTileOptions(minZoom = 1, maxZoom = 4)
      ) %>% addCircleMarkers(lng = -119, 
                             lat = 34.5)
  })
  
  
  #click_layer_id <- NULL
  
  observeEvent(input$mymap_click, {
    
    #if(!is.null(click_layer_id)){
    leafletProxy('mymap') %>% clearMarkers()
    #}
    
    leafletProxy('mymap') %>% addCircleMarkers(lng = input$mymap_click$lng, lat = input$mymap_click$lat)
  })
  
  ### Data controls
  
  #i <- 9802 #23325 # 120 #
  
  clicked_cell <- reactive({
    if(is.null(input$mymap_click$lng)){
      cell <- 11941
    } else{
      cell <- cell_coords$cell[which(abs(cell_coords$lng - input$mymap_click$lng) == 
                                       min(abs(cell_coords$lng - input$mymap_click$lng)) & 
                                       abs(cell_coords$lat - input$mymap_click$lat) == 
                                       min(abs(cell_coords$lat - input$mymap_click$lat)))]
    }
    
    return(cell)
  })
  
  
  # This returns the correct dataset
  datasetInput <- reactive({
    if (input$web_type == "Natural"){
      dataset <- web_pres_nat[[clicked_cell()]]
    }
    else if (input$web_type == "Current"){
      dataset <- web_current[[clicked_cell()]]
    }
    return(dataset)
  })
  
  # https://community.rstudio.com/t/changing-datasets-based-on-select-input-in-r-shiny/67891
  
  
  
  ### Network
  
  output$network_proxy_nodes <- renderVisNetwork({
    nodes <- tibble(id = datasetInput() %>% unlist() %>% unique() %>% sort())
    edges <- data.frame(from = datasetInput()[,1], 
                        to = datasetInput()[,2])
    if(dim(edges)[2] > 0){
      nodes <- nodes %>% mutate(group = factor(ifelse(nodes$id %in% edges[,1], "yes", "no"), labels = c("no", "yes")))
      nodes <- nodes[order(nodes$group),]
    }
    
    visNetwork(nodes, edges) %>%
      visLayout(randomSeed = 444) %>% 
      #visIgraphLayout() %>% 
      # visEvents(type = "once", startStabilizing = "function() {
      #       this.moveTo({scale:0.4})}") %>%
      visExport() %>%
      visEdges(arrows = "to", smooth = F) %>%  # 
      # visGroups(groupname = "yes", color = list(border = "#2B7CE9", background = "#97C2FC")) %>%
      # visGroups(groupname = "no", color = list(border = "#FFA500", background = "#FFFF00")) %>%
      # #visGroups(groupname = "C", color = list(border = "#FA0A10", background = "#FB7E81")) %>%
      visOptions(highlightNearest = list(enabled = TRUE, degree = 1),
                 nodesIdSelection = list(enabled = TRUE, main = "Species to highlight")) %>% 
      visPhysics(maxVelocity = 2,
                 barnesHut = list(gravitationalConstant = -50000),
                 stabilization = T) %>%  # , stabilization = F
      visInteraction(zoom = T) %>%
      visEvents(click = "function(nodes){
        Shiny.onInputChange('click', nodes.nodes[0]);
        ;}"
      )
    # %>% # hover = TRUE, 
    # visEvents(click = "function(nodes) {
    #   Shiny.onInputChange('current_node_id', nodes);
    # ;}")
    
  })
  
  # output$view_id <- renderText({
  #   paste("Current node selection : ", input$network_id_selected)
  # })
  
  #  output$code_network_id <- renderText({
  #    '
  #  visNetwork(nodes, edges, main = "Title", submain = "Subtitle") %>%
  #    visExport() %>%
  #    visOptions(highlightNearest = TRUE,
  #      nodesIdSelection = list(enabled = TRUE, selected = "1"))
  # '
  #  })
  
  ### Mammal Photos
  
  observe({
    #input$gosel
    if(length(input$click) == 1){
      visNetworkProxy("network_proxy_nodes") %>% visGetSelectedNodes()
    }
  })
  
  # observe({
  #   print(input$network_proxy_nodes_selectedNodes)
  # })
  
  output$mammImage <- renderImage({
    
    if(is.null(input$network_proxy_nodes_selectedNodes)){
      fn <- "./wikipics/Blank.jpg"
    } else{
      fn <- paste0(input$network_proxy_nodes_selectedNodes, ".jpg")
      
      if(fn %in% list.files("./wikipics/")){
        fn <- paste0("./wikipics/", fn)
      } else{
        fn <- "./wikipics/Unknown.jpg"
      }
    }
    
    list(src = fn, width = "100%")
    
  }, deleteFile = F)
  
  
  
}