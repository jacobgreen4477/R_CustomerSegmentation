# title: shiny+plotly 


# raw data
set.seed(0)
tmpData <- data.frame(cluster=clusterID,inputDt)
tmpData <- tmpData[sample(nrow(inputDt),100),]
tmpData


# scatter plot
scatterPlot <- function(dataset) {
  
  shinyApp(
    
    ui = fluidPage(headerPanel(h1("Bivariate Plot With Raw Data",style="color: #4d3a7d")),
                   fluidRow(column(4,selectInput('xcol', 'X Variable', names(dataset),selected=names(dataset)[3]))
                            ,column(4, selectInput('ycol', 'Y Variable', names(dataset),selected=names(dataset)[2]))),
                   fluidRow(plotlyOutput('scatterPlot',height = "600px"))),
    
    
    server = function(input, output, session) {
      selectedData       <- reactive({dataset})
      output$scatterPlot <- renderPlotly({
        p <- plot_ly(selectedData(),x=~get(input$xcol),y=~get(input$ycol)) 
             add_markers(p, color = ~factor(cluster),symbol = ~factor(cluster))})}
    
  )
  
}


print(scatterPlot(tmpData))


