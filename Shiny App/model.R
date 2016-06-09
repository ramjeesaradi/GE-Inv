modeltab <- tabPanel("Causal Analysis",
         fluidRow(
           column(12,
                  h3("Item issue and Avail time series",align = "center"),
                  plotOutput("PartAvailIssuegraph"))
         ),
         fluidRow(
           column(12,
                  h3("Item Availability time series",align = "center"),
                  plotOutput("PartAvailgraph"))
         ),
         fluidRow(
           column(12,
                  h3("Item Isuue time series",align = "center"),
                  plotOutput("PartIssuegraph"))
         )
)