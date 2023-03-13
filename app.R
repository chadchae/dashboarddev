library(shiny)
library(shinymanager)
library(shinymaterial)
library(ggplot2)
library(dplyr)


## you can use keyring package to set database key
#library(keyring)
#key_set("R-shinymanager-key", "cnddlfdlakstp")
#create_db(
#    credentials_data = credentials,
#    sqlite_path = "db/database.sqlite", # will be created
#    #passphrase = key_get("R-shinymanager-key", "cnddlfdlakstp"),
#    passphrase = "cnddlfdlakstp"
#)

data <- read.csv("data.csv")

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
        tags$h1("Score and Grade"),
        tags$hr(),
        tags$br(),
        tags$p(strong("Prof. Chae (Chungil Chae)")),
        tags$p(em(
            "MGS3001, Python Programming for Business, 2023, spring"
        )),
        tags$a(href = "https://chadchae.github.io", "chadchae, copyright@Chungil Chae"),
        titlePanel("Score and Grade"),
        sidebarLayout(
            position = "left",
            sidebarPanel(
                tags$p(strong("Grade Table")),
                tags$p("Your score is calculated based on the following criteria:)"),
                tags$p("Quiz 10%, Assignment 21%, Exam 69%"),
                tags$p("Percentage has rounded up from 3 digits after decimal point"),
                tags$br("Your perc1>=93,A;"),
                tags$br("Your perc1>=90,A-;"),
                tags$br("Your perc1>=87,B+;"),
                tags$br("Your perc1>=84,B;"),
                tags$br("Your perc1>=80,B-;"),
                tags$br("Your perc1>=77,C+;"),
                tags$br("Your perc1>=70,C;"),
                tags$br("Your perc1>=60,D;"),
                tags$br("Your perc1 <60F"),
                tags$br(""),
                tags$p(
                    strong("Extra Point: Contribution"),
                    tags$br("Your contribution score will be applied in final grading"),
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
                tabsetPanel(
                    type = "tabs",
                    tabPanel(
                        "Scores",
                        tags$h2("Scores"),
                        p(
                            strong("Original score and grade before contribution extra point")
                        ),
                        p(
                            "Contribution extra point will be applied when your instructor enter your grade to the school system. "
                        ),
                        tableOutput("myinfo2"),
                        tableOutput("myinfo3"),
                        tableOutput("myinfo4"),
                        tableOutput("myinfo5"),
                    ),
                    tabPanel(
                        "Percentage and Grade",
                        tags$h2("Percentage and Grade"),
                        tableOutput("myinfo6"),
                        p(strong("Necessary percentage to reach to A Grade")),
                        tableOutput("myinfo7"),
                    ),
                    tabPanel(
                        "vs All students",
                        p(strong("Comparision vs all students"), textOutput("info1")),
                        plotOutput("myhist1"),
                    ),
                    tabPanel("in section",
                             p(
                                 strong("Comparision vs students in section"),
                                 textOutput("info2")
                             ),
                             plotOutput("myhist2"),),
                    tabPanel(
                        "in seniority",
                        p(
                            strong("Comparision vs students in same seniroity"),
                            textOutput("info3")
                        ),
                        plotOutput("myhist3"),
                    )
                )
            )
        )
    )


ui <- secure_app(ui, enable_admin = TRUE)
server <- function(input, output, session) {
    # check_credentials directly on sqlite db
    res_auth <-
        secure_server(check_credentials = check_credentials("db/database.sqlite",
                                                            #passphrase = key_get("R-shinymanager-key", "cnddlfdlakstp")
                                                            passphrase = "cnddlfdlakstp"))
    
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
    indvalue <- reactive(filter(data, email == user())$perc1)
    seniroityvalue <-
        reactive(filter(data, email == user())$seniroity)
    sectionvalue <- reactive(filter(data, email == user())$section)
    sectionave <- reactive(data %>%
                               group_by(section) %>%
                               summarise(mean = mean(perc1)))
    seniroityave <- reactive(data %>%
                                 group_by(seniroity) %>%
                                 summarise(mean = mean(perc1)))
    output$myinfo1 <- renderTable(data %>%
                                      filter(email == user()) %>%
                                      select(email, engname, section, seniroity))
    output$myinfo2 <- renderTable(
        data %>%
            filter(email == user()) %>%
            select(
                quiz1,
                quiz2,
                quiz3,
                quiz4,
                quiz5,
                quiz6,
                quiz7,
                quiz8,
                quiz9,
                quiz10
            )
    )
    output$myinfo3 <- renderTable(data %>%
                                      filter(email == user()) %>%
                                      select(exam1,
                                             exam2,
                                             exam3))
    output$myinfo4 <- renderTable(data %>%
                                      filter(email == user()) %>%
                                      select(assign1,
                                             assign2,
                                             assign3))
    output$myinfo5 <- renderTable(data %>%
                                      filter(email == user()) %>%
                                      select(contrib))
    output$myinfo6 <- renderTable(data %>%
                                      filter(email == user()) %>%
                                      select(perc1,
                                             grade1))
    output$myinfo7 <- renderTable(93 - data %>%
                                      filter(email == user()) %>%
                                      select(perc1))
    output$info1 <- renderPrint(nrow(data))
    output$info2 <- renderPrint(nrow(data %>%
                                         filter(section == sectionvalue())))
    output$info3 <- renderPrint(nrow(data %>%
                                         filter(seniroity == seniroityvalue())))
    
    output$myhist1 <- renderPlot(
        data %>%
            ggplot(aes(x = perc1)) +
            geom_histogram(
                binwidth = 0.1,
                colour = "white",
                fill = "grey"
            ) +
            geom_density(
                aes(y = ..density.. * (nrow(data) * 0.1)),
                alpha = .2,
                fill = "#FF6666"
            ) +
            #xlim(1, 5) +
            geom_vline(
                xintercept = as.numeric(indvalue()),
                color = "red",
                size = 2,
                linetype = "dashed"
            ) +
            geom_vline(
                xintercept = mean(data$perc1),
                color = "blue",
                size = 2,
                linetype = "dashed"
            ) +
            xlab("") +
            ylab("") +
            theme(
                axis.text = element_text(size = 16),
                axis.title = element_text(size = 16, face = "bold")
            )
    )
    
    output$myhist2 <- renderPlot(
        data %>%
            filter(section == sectionvalue()) %>%
            ggplot(aes(x = perc1)) +
            geom_histogram(
                binwidth = 0.1,
                colour = "white",
                fill = "grey"
            ) +
            geom_density(
                aes(y = ..density.. * (nrow(data) * 0.1)),
                alpha = .2,
                fill = "#FF6666"
            ) +
            #xlim(1, 5) +
            geom_vline(
                xintercept = as.numeric(indvalue()),
                color = "red",
                size = 2,
                linetype = "dashed"
            ) +
            xlab("") +
            ylab("") +
            theme(
                axis.text = element_text(size = 16),
                axis.title = element_text(size = 16, face = "bold")
            )
    )
    
    output$myhist3 <- renderPlot(
        data %>%
            filter(seniroity == seniroityvalue()) %>%
            ggplot(aes(x = perc1)) +
            geom_histogram(
                binwidth = 0.1,
                colour = "white",
                fill = "grey"
            ) +
            geom_density(
                aes(y = ..density.. * (nrow(data) * 0.1)),
                alpha = .2,
                fill = "#FF6666"
            ) +
            #xlim(1, 5) +
            geom_vline(
                xintercept = as.numeric(indvalue()),
                color = "red",
                size = 2,
                linetype = "dashed"
            ) +
            xlab("") +
            ylab("") +
            theme(
                axis.text = element_text(size = 16),
                axis.title = element_text(size = 16, face = "bold")
            )
    )
    
}

shinyApp(ui, server)
