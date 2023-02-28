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
        tags$h1("实际得分与总成绩"),
        tags$hr(),
        tags$br(),
        tags$p(strong("Prof. Chae (Chungil Chae) & RA. 徐绍源")),
        tags$p(em("学生各项得分成绩表")),
        tags$a(href="https://chadchae.github.io","chadchae, copyright@Chungil Chae"),
        titlePanel("实际得分与总成绩"),
        sidebarLayout(
            position = "left",
            sidebarPanel(
                tags$p(strong("成绩表")),
                tags$p("你的实际得分遵循下列计算准则 :"),
                tags$p("课堂出勤 10%;"),
                tags$p("日常作业 10% [自我介绍+反思随笔(ReflectiveEssay)+实验作业(LabAssigment)+提案(Proposal)];"),
                tags$p("结课设计 10%;"),
                tags$p("期中考试 30%;"),
                tags$p("期末考试 30%"),
                tags$br("你的实际得分>=93,A;"),
                tags$br("你的实际得分>=90,A-;"),
                tags$br("你的实际得分>=87,B+;"),
                tags$br("你的实际得分>=84,B;"),
                tags$br("你的实际得分>=80,B-;"),
                tags$br("你的实际得分>=77,C+;"),
                tags$br("你的实际得分>=70,C;"),
                tags$br("你的实际得分>=60,D;"),
                tags$br("你的实际得分 <60F"),
                tags$br(""),
                tags$p(strong("额外得分"),
                tags$p("在课堂上的积极参与和良好的上课态度将会作为额外的分数计入到你的期末成绩中。此两项的总额外得分可以达总成绩的3%"),
                tags$br("120: 卓越课堂表现"),
                tags$br("100: 积极课堂参与"),
                tags$br("80: 相对积极参与"),
                tags$br("50: 一般课堂参与"),
                tags$br("0: 扰乱课堂秩序或态度恶劣")
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
