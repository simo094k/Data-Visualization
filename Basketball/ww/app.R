## Only run this example in interactive R sessions
if (interactive()) {
  library(shiny)
  
  # javascript code to collapse box
  jscode <- "
shinyjs.collapse = function(boxid) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}
"

# A dashboard body with a row of infoBoxes and valueBoxes, and two rows of boxes
body <- dashboardBody(
  # Including Javascript
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(text = jscode,functions = c("Click")),
  # Boxes
  fluidRow(
    box(id="box1",title = actionLink("titleId", "Histogram box title",icon =icon("arrow-circle-up")), 
        status = "warning", solidHeader = TRUE, collapsible = T,
        plotOutput("plot", height = 500)
    ),
    box(id="box2",title = p("Histogram box title", 
                            actionButton("titleBtId", "", icon = icon("arrow-circle-up"),
                                         class = "btn-xs", title = "Update")), 
        status = "warning", solidHeader = TRUE, collapsible = T,
        plotOutput("plot1", height = 250)
    ),
    box(id="box3",title = actionButton("titleboxId", "Histogram box title",icon =icon("arrow-circle-up")), 
        status = "warning", solidHeader = TRUE, collapsible = T,
        plotOutput("plot2", height = "auto")
    )
  )
  
  
)

server <- function(input, output) {
  
  output$plot <- renderPlot({
    hist(rnorm(50))
  })
  output$plot1 <- renderPlot({
    hist(rnorm(50))
  })
  output$plot2 <- renderPlot({
    hist(rnorm(50))
  })
  
  observeEvent(input$titleId, {
    js$collapse("box1")
  })
  observeEvent(input$titleBtId, {
    js$collapse("box2")
  })
  observeEvent(input$titleboxId, {
    js$collapse("box3")
  })
}

shinyApp(
  ui = dashboardPage(
    dashboardHeader(),
    dashboardSidebar(),
    body
  ),
  server = server
)
}