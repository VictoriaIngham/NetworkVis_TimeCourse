library(shiny)
library(visNetwork)

network = read.csv('full_results.csv',header=TRUE)
all_nodes = read.csv('Node_info.csv',header=TRUE)

ui <- fluidPage(
      titlePanel("Transcriptional Control of Pyrethroid Response"),
      mainPanel( 
               sliderInput('PosteriorProb','Posterior Probability',min=0.1,max=0.8,step=0.05,value=0.8),
               submitButton("Update View", icon("refresh")),
               visNetworkOutput("network",height='1600px',width='1600px'), 
                )
)

server <- function(input,output) {
  
  edges = reactive({
    edges = network[network$width >= as.numeric(input$PosteriorProb),]
    edges$width = (5*edges$width)
    edges
  })
  
  nodes = reactive({
    edges2 = network[network$width >= as.numeric(input$PosteriorProb),]
    character_vector = c(as.character(unname(edges2$from)),as.character(unname(edges2$to)))
    nodes = as.data.frame(all_nodes[all_nodes$id %in% character_vector, ,drop=FALSE])
    nodes
  })
  
  output$network = renderVisNetwork({
    visNetwork(nodes(),edges(),width = '100%') %>%
      visIgraphLayout(physics=TRUE,smooth=FALSE) %>%
        visEdges(shadow = FALSE,smooth=FALSE,
                 arrows =list(to = list(enabled = TRUE, scaleFactor = 2)),
                 color = list(color = "lightblue", highlight = "red")) %>%
        visInteraction(hover = TRUE) %>%
        visOptions(highlightNearest=list(enabled = TRUE, degree = 1, hover = TRUE),
                   nodesIdSelection = TRUE,
                   autoResize = TRUE) %>%
        visInteraction(multiselect = TRUE)
    })
}

shinyApp(ui = ui, server = server)