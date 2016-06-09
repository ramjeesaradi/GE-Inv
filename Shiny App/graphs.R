graphstab <- tabPanel("Graphs",
                      fluidRow(
                        column(9,offset = 1, 
                               h3("Item Vs Cost"),
                        plotOutput("graph1"),
                        h3("Item Vs Total Issue"),
                        plotOutput("graph2"),
                        h3("Item stock level boxplot"),
                        plotOutput("graph3")
                        
                        # uiOutput("partsel"),
                       
                        )
                      )
)