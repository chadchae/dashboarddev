library(shiny)
library(shinymanager)
library(shinymaterial)
library(ggplot2)
library(dplyr)



data<-readxl::read_xlsx("data.xlsx")
credentials <- read.csv('credentials.csv')

#
## you can use keyring package to set database key
#library(keyring)
#key_set("R-shinymanager-key", "cnddlfdlakstp")
#create_db(
#    credentials_data = credentials,
#    sqlite_path = "db/database.sqlite", # will be created
#    passphrase = key_get("R-shinymanager-key", "cnddlfdlakstp")
#    #passphrase = "cnddlfdlakstp"
#)


# Wrap your UI with secure_app, enabled admin mode or not
ui <-
    fluidPage(
        #auth_ui(
        #    id = "auth",
        #    # add image on top ?
        #    tags_top =
        #        tags$div(
        #            tags$h4("Demo", style = "align:center"),
        #            tags$img(
        #                src = "https://www.r-project.org/logo/Rlogo.png", width = 100
        #            )
        #        )
        #),
        verbatimTextOutput(outputId = "auth_output"),
        tags$h1("ActualScore and Grade"),
        tags$hr(),
        tags$br(),
        tags$p(strong("Prof. Chae (Chungil Chae)")),
        tags$p(em("MGS3101, Foundation of Business Analytics, 2022, Fall")),
        tags$a(href="https://chadchae.github.io","chadchae, copyright@Chungil Chae"),
        titlePanel("ActualScore and Grade"),
        sidebarLayout(
            position = "left",
            sidebarPanel(
                tags$p(strong("Grade Table")),
                tags$p("Your ActualScore is calculated based on the following criteria:)"),
                tags$p("Attendece 10%; Assignement 10% (SelfIntro+ReflectiveEssay+LabAssigment+Proposal);Final Project 10%; Poster Session 10%; Midterm 30%; Finalexam 30%"),
                tags$br("Your ActualScore>=93,A;"),
                tags$br("Your ActualScore>=90,A-;"),
                tags$br("Your ActualScore>=87,B+;"),
                tags$br("Your ActualScore>=84,B;"),
                tags$br("Your ActualScore>=80,B-;"),
                tags$br("Your ActualScore>=77,C+;"),
                tags$br("Your ActualScore>=70,C;"),
                tags$br("Your ActualScore>=60,D;"),
                tags$br("Your ActualScore <60F"),
                tags$br(""),
                tags$p(strong("Extra Point"),
                tags$p("Participation and Attitude in Class ActualScore added to your final ActualScore as extra-point ActualScore. The contribution rate of participation and attitude is up to 3%"),
                tags$br("120: Extra Ordinary"),
                tags$br("100: Actively Participate"),
                tags$br("80: Relatively Active Participation"),
                tags$br("50: Normal Participation"),
                tags$br("0: Distrubing Class or Bad Attitude")
                       ),
            ),
            mainPanel(
                p(strong("Overall Information")),
                tableOutput("myinfo1"),
                tableOutput("myinfo2"),
                tableOutput("myinfo3"),
                tableOutput("myinfo4"),
            )
        )
    )


ui <- secure_app(ui, enable_admin = TRUE)
server <- function(input, output, session) {
    # check_credentials directly on sqlite db
    res_auth <- secure_server(
        check_credentials = check_credentials(
            "db/database.sqlite",
            #passphrase = key_get("R-shinymanager-key", "cnddlfdlakstp")
            passphrase = "cnddlfdlakstp"
        ) )

    output$auth_output <- renderPrint({
        reactiveValuesToList(res_auth)$user
    })

    # authentication module
    #auth <- callModule(
    #    module = auth_server,
    #    id = "auth",
    #    check_credentials = check_credentials(credentials)
    #)


    # server logic
    user <- reactive(res_auth$user)
    indvalue <- reactive(filter(data, ID == user())$ActualScore)
    SENIORITYvalue <- reactive(filter(data, ID == user())$SENIORITY)
    SECTIONvalue <- reactive(filter(data, ID == user())$SECTION)
    SECTIONave <- reactive(data %>%
                            group_by(SECTION) %>%
                            summarise(mean=mean(ActualScore)))
    SENIORITYave <- reactive(data %>%
                            group_by(SENIORITY) %>%
                            summarise(mean=mean(ActualScore)))
    output$myinfo1 <- renderTable(data %>%
                                     filter(ID == user()) %>%
                                     select(ID, STUID, NAME, SECTION, SENIORITY)
    )
    output$myinfo2 <- renderTable(data %>%
                                     filter(ID == user()) %>%
                                     select(Attendence,
                                            IntroYoursef,
                                            ReflectiveEssay,
                                            LabExercise,
                                            DashboardProposal,
                                            FinalProject,
                                            PosterSession,
                                            Midterm,
                                            Final)
    )
    output$myinfo3 <- renderTable(data %>%
                                     filter(ID == user()) %>%
                                     select(Participation)
    )
    output$myinfo4 <- renderTable(data %>%
                                      filter(ID == user()) %>%
                                      select(Total1,
                                             Grade1,
                                             Total2,
                                             Grade2)
    )

}

shinyApp(ui, server)
