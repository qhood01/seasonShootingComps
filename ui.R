source("helpers.R")
players <- names(shooting)

bootstrapPage(
    fluidRow(
        column(12, align="center",
               titlePanel("Player Shot Location Comparison"))
    ),
    fluidRow(
        column(6, align="center",
               selectizeInput("p1",
                              label="Choose a Player",
                              choices = players,
                              selected = NULL)),
        column(6, align="center",
               selectizeInput("p2",
                              label="Choose a Player",
                              choices = players,
                              selected = NULL))
    ),
    fluidRow(
        column(6, align="center",
               uiOutput("years1")),
        column(6, align="center",
               uiOutput("years2"))
    ),
    fluidRow(
        column(12, align="center",
               plotOutput("graph1",width="85%"))
    )
)
