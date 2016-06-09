exprmnts <- tabPanel("Experiments",
                     br(),
                     fluidRow(
                       column(12,
                              column(6, 
                                     radioButtons("partCat","Select Category",
                                                  choices = c("A","B","C"),inline = T)),
                              column(6,uiOutput("partsel"))
                              
                       )
                     ),
                     fluidRow(
                       column(6,
                              uiOutput("predicted")
                              ,uiOutput("featsel")
                              # ,updateSelectInput("Update Features")
                              
                       )
                       ,column(6,
                               numericInput("sampleSize", "Sample Size",0)
                       )
                     ),wellPanel(
                     fluidRow(
                       column(12,
                              h3( textOutput("modeldesc"),align = "center"))
                     )),
                     fluidRow(
                       column(6,
                              plotOutput("Coefficients") )
                     )
)