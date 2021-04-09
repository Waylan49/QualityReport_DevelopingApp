library(shiny)
library(shinyMatrix)


ui <- fluidPage(

    titlePanel("Warranty Check Tool"),

    fluidRow(
       
        column(
            4,
            tags$h4("Data"),
            textAreaInput(inputId = "SN", placeholder = "Input you SN#", label = "Serial Number", height = "650px"),
            actionButton("button1","Submit!")
        ),

        
        column(
           8,
           dataTableOutput("table")
        )
    )
)


server <- function(input, output){
    observeEvent(input$button1, {
    output$table<-renderDataTable({
       SN_LIST<-str_split(input$SN, "\n")
       do.call(rbind, lapply(SN_LIST[[1]][1:(length(SN_LIST[[1]])-1)], wcheck))
    })
    })
    
}



shinyApp(ui = ui, server = server)
