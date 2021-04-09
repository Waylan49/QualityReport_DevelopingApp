library(shiny)
library(shinydashboard)

Qspeak

ui<-dashboardPage(
    dashboardHeader(title="Qspeak"),
    dashboardSidebar(),
    dashboardBody(
            
            fluidRow(
                    box(
                        fileInput(inputId = "file1", label = "Upload your Qspeak Report", accept = ".xlsx"),
                        height=80
                        )
            ),
            plotlyOutput('contents', width = "800px", height = "350px")
    ))



server<-function(input, output){
        output$contents <- renderPlotly({
                
                req(input$file1)
                inFile <- input$file1
                Qspeak<-read_xlsx(inFile$datapath, 1)
                temp_df<-Qspeak %>% select(AssemblyNo, Fault.Code.1)
                temp_df1<-temp_df %>% mutate(Index=ifelse(
                        is.na(Fault.Code.1), "NFF", ifelse(
                                Fault.Code.1=="101", "ECN", "Others"
                        )
                ))
                
                top10_assy<-as.data.frame(temp_df1 %>% count(AssemblyNo) %>% arrange(desc(n)))[1:10, ]
                colnames(top10_assy)[2]<-"Total_Q"
                
                data.f<-data.frame(Index=character(), Assy=character(), Quantity=numeric(), Percentage=numeric(), Total=numeric())
                
                for(i in 1:10){
                        Assy<-top10_assy[i,1]
                        temp<-temp_df1 %>% filter(AssemblyNo==Assy)
                        temp1<-as.data.frame(temp %>% group_by(Index) %>% count())
                        k<-dim(temp1)[1]
                        for(j in 1:k){
                                temp2<-data.frame(Index=temp1[j,1], Assy=Assy, Quantity=temp1[j,2], Percentage=0, Total=top10_assy[i, 2])
                                data.f<-rbind(data.f, temp2)
                        }
                }
                
               fig<-plot_ly(data.f, x=~reorder(Assy, -Total), 
                            y=~Quantity, color=~Index, type="bar")%>%layout(barmode="stack") 
               fig1<-fig %>% layout(title="Top_10 Return Parts Composition",
                                    yaxis=list(title="Quantity"),
                                    xaxis=list(title="Assy#"))
        })
}


# Run the application 
shinyApp(ui = ui, server = server)



