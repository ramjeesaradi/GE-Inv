abctab <- tabPanel(title = "Categorisation",
                 
                   fluidRow(column(9,
                                   plotOutput("InvDailyValue")
                   )
                   ,fileinblock
                   ),
                   
                   fluidRow(
                     column(9,
                            plotOutput("abcPlot",height = "750px")
                     ),
                     column(3, h3("Type 1 Items"),
                            tableOutput("Aparts")
                     )
                   ))

fileinblock <- column(3,
                      wellPanel(
                        fileInput('file1', 'Choose CSV File',
                                  accept=c('text/csv', 
                                           'text/comma-separated-values,text/plain', 
                                           '.csv')),
                        tags$hr(),
                        checkboxInput('header', 'Header', TRUE),
                        radioButtons('sep', 'Separator',
                                     c(Comma=',',
                                       Semicolon=';',
                                       Tab='\t'),
                                     ','),
                        radioButtons('quote', 'Quote',
                                     c(None='',
                                       'Double Quote'='"',
                                       'Single Quote'="'"),
                                     '"'))
)