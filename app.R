
library(shiny)
library(shinydashboard)
library(readr)
library(shinyWidgets)
library(shinyThings)  #devtools::install_github("gadenbuie/shinyThings")


data <- read_csv("sample_data_dashboard.csv")

ui <- dashboardPage(
    skin = "blue",
    dashboardHeader(title = "CCBS dashboard"),
    dashboardSidebar(
        sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("About", tabName = "about", icon = icon("question"))
        ),
        br(),
        dateRangeInput("date_filter", "Date range",
                       min= "2022-01-15",
                       max= Sys.Date(),
                       start= "2022-01-15",
                       end= Sys.Date()),
        checkboxGroupInput("gender_filter", 
                           h4("Gender"), 
                           choices = list("Male" = 1, 
                                          "Female" = 2, 
                                          "Other" = 3,
                                          "Not disclosed" = 4),
                           selected = c(1,2,3)),
        sliderInput("age_filter",
                    h4("Age"), 
                    min = 20, max = 100, value = c(20, 100)),
        checkboxGroupInput("race_filter", 
                           h4("Race/Ethnicity"), 
                           choices = list("White/ Caucasian" = 1, 
                                          "Black/ African American" = 2, 
                                          "Hispanic/ Latino" = 3,
                                          "Asian" = 4,
                                          "Alaskan Native" = 5,
                                          "Native American" = 6,
                                          "Native Hawaiian and Other Pacific Islander"=7,
                                          "Two or More Races" = 8,
                                          "Unknown/ not disclosed" = 9),
                           selected = seq(1,9))
    ),
    dashboardBody(
        style = "background-color:#2C3E50;",
        tabItem(tabName = "dashboard",
                fluidRow(
                    column(12, offset=1,
                    valueBoxOutput("box_signup", width=2),
                    valueBoxOutput("box_contact", width=2),
                    valueBoxOutput("box_consentdoc", width=2),
                    valueBoxOutput("box_app_scheduled", width=2),
                    valueBoxOutput("box_onboard", width = 2)
                    )
                ),
                fluidRow(
                    box(title="Sign up", solidHeader = F, status = "primary",width=6,
                        tabsetPanel(
                            tabPanel("History",
                                     br(),
                                     column(width=4,
                                            buttonGroup( inputId = "date_type1",
                                                         choices = c("Cumulative", "Per day", "Per week"), 
                                                         selected = "Cumulative", multiple= F)
                                     ),
                                     column(width=6,
                                            buttonGroup( inputId = "value_type1",
                                                         choices = c("Value", "Log"), 
                                                         selected = "Value", multiple= F)
                                     ),
                                     column(width=12,
                                            plotOutput("plot1", height = 250)
                                     )),
                            tabPanel("Demographics")
                        )),
                    box(title = "Contact", solidHeader = F,  status = "primary",width=6,
                        tabsetPanel(
                            tabPanel("History",
                                     br(),
                                     column(width=4,
                                            buttonGroup( inputId = "date_type2",
                                                         choices = c("Cumulative", "Per day", "Per week"), 
                                                         selected = "Cumulative", multiple= F)
                                     ),
                                     column(width=6,
                                            buttonGroup( inputId = "value_type2",
                                                         choices = c("Value", "Log"), 
                                                         selected = "Value", multiple= F)
                                     ),
                                    column(width=12,
                                    plotOutput("plot2", height = 250)
                                    )),
                            tabPanel("Call attempts stats"),
                            tabPanel("Eligible/ Interested stats")
                        )),
                    box(title = "E-Mail sent", solidHeader = F,  status = "primary",width=6,
                        tabsetPanel(
                            tabPanel("History",
                                     br(),
                                     column(width=4,
                                            buttonGroup( inputId = "date_type3",
                                                         choices = c("Cumulative", "Per day", "Per week"), 
                                                         selected = "Cumulative", multiple= F)
                                     ),
                                     column(width=6,
                                            buttonGroup( inputId = "value_type3",
                                                         choices = c("Value", "Log"), 
                                                         selected = "Value", multiple= F)
                                     ),
                                     column(width=12,
                                            plotOutput("plot3", height = 250)
                                     )),
                            tabPanel("E-Mail stats")
                        )),
                    box(title = "Appointment", solidHeader = F,  status = "primary",width=6,
                        tabsetPanel(
                            tabPanel("Appointment call",
                                     br(),
                                     column(width=4,
                                            buttonGroup( inputId = "date_type4",
                                                         choices = c("Cumulative", "Per day", "Per week"), 
                                                         selected = "Cumulative", multiple= F)
                                     ),
                                     column(width=6,
                                            buttonGroup( inputId = "value_type4",
                                                         choices = c("Value", "Log"), 
                                                         selected = "Value", multiple= F)
                                     ),
                                     column(width=12,
                                            plotOutput("plot4", height = 250)
                                     )),
                            tabPanel("Visit scheduled",
                                     br(),
                                     column(width=4,
                                            buttonGroup( inputId = "date_type5",
                                                         choices = c("Cumulative", "Per day", "Per week"), 
                                                         selected = "Cumulative", multiple= F),
                                            column(width=6,
                                                   buttonGroup( inputId = "value_type5",
                                                                choices = c("Value", "Log"), 
                                                                selected = "Value", multiple= F)
                                            )
                                     ))
                        )),
                    box(title = "Onboarded", solidHeader = T,  status = "success", width=12,
                        tabsetPanel(
                            tabPanel("First visit",
                                     br(),
                                     column(width=2,
                                            buttonGroup( inputId = "date_type6",
                                                         choices = c("Cumulative", "Per day", "Per week"), 
                                                         selected = "Cumulative", multiple= F)
                                     ),
                                     column(width=6,
                                            buttonGroup( inputId = "value_type6",
                                                         choices = c("Value", "Log"), 
                                                         selected = "Value", multiple= F)
                                     ),
                                     column(width=12,
                                            plotOutput("plot5", height = 250)
                                     )),
                            tabPanel("Second visit")
                        )),
                    box(title = "Demographics of onboarded participants", solidHeader = T,  status = "primary", width=12,
                        column(width=12,
                               br(),
                               column(width=12,
                               plotOutput("plot6", height = 250)
                               )
                        ))
                )
        )
    )
)

server <- function(input, output) {
    

    
    output$box_signup <- renderValueBox({
        valueBox(
            500, paste0("Sign up (today: ", 10,")"), icon = icon("sign-in"),
            color = "aqua"
        )
    })
    
    output$box_contact <- renderValueBox({
        valueBox(
            300, paste0("Contacted (today: ", 5,")"), icon = icon("phone"),
            color = "light-blue"
        )
    })
    
    output$box_consentdoc <- renderValueBox({
        valueBox(
            200, paste0("Consent doc sent (today: ", 3,")"), icon = icon("file-alt"),
            color = "blue"
        )
    })
    
    output$box_app_scheduled <- renderValueBox({
        valueBox(
            100, paste0("Appointment scheduled (today: ", 2,")"), icon = icon("phone"),
            color = "navy"
        )
    })
    
    output$box_onboard <- renderValueBox({
        valueBox(
            20, paste0("Onboarded (today: ", 10,")"),icon = icon("users"),
            color = "green"
        )
    })
    
    
    ### dummy plots
    
    output$plot1 <- renderPlot({
        hist(rnorm(500))
    })
    output$plot2 <- renderPlot({
        hist(rnorm(500))
    })
    output$plot3 <- renderPlot({
        hist(rnorm(500))
    })
    output$plot4 <- renderPlot({
        hist(rnorm(500))
    })
    output$plot5 <- renderPlot({
        hist(rnorm(500))
    })
    output$plot6 <- renderPlot({
        hist(rnorm(500))
    })
}

shinyApp(ui, server)
