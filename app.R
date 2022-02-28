
library(shiny)
library(shinydashboard)
library(readr)
library(shinyWidgets)
library(shinyThings)  
library(tidyverse)
library(plotly)
library(lubridate)
#devtools::install_github("gadenbuie/shinyThings")


data <- read_csv("sample_data_dashboard.csv")

end_date <- as.Date("2022-12-31") ##set a time in the future to account for visits that are sheduled for the future 
current_day <- Sys.time()
  
#data modification 
data_mod.df <- data %>% 
  mutate(sign_up_datetime = gsub(" .*","",sign_up_datetime) %>% as.Date(),
         initial_contact_date = gsub(" .*","",initial_contact_date) %>% as.Date(),
         appointment_call_date = gsub(" .*","",appointment_call_date) %>% as.Date(),
         visit_schedule_date = gsub(" .*","",visit_schedule_date) %>% as.Date()) %>% 
  mutate(race_combine = ifelse(is.na(race_combine),"Unknown/ not disclosed",race_combine),
         sex_combine = ifelse(is.na(sex_combine),"Unknown/ not disclosed",sex_combine)) %>% 
  mutate(ccf_patient = ifelse(ccf_patient == 1,"yes","no"))
  


#predefined plot functions 
overviewplot_1 <- function(var,xaxis,date_sel,age_filter,date_filter,race_filter,gender_filter){
  
  print(age_filter)
  
  start_date = as.Date(date_filter[1])
  end_date = as.Date(date_filter[2])
  
  data_filt.df <- data_mod.df %>% 
    rename(selected_var = var) %>% 
    filter(age_combine >= age_filter[1] & age_combine <= age_filter[2],
           selected_var >= date_filter[1] & selected_var <= date_filter[2],
           race_combine %in% race_filter,
           sex_combine %in% gender_filter) %>% 
    group_by(selected_var) %>% 
    summarise(n = n())
  
  ##if data should be aggregated per week 
  if(date_sel == "Per week"){
    data_filt.df <- data_filt.df %>% 
      mutate(year = gsub("-.*","",selected_var),
             week = week(selected_var),
             Dates = paste0(year,"-",week)) %>% 
      group_by(Dates) %>% 
      summarise(n = sum(n,na.rm = T)) %>% 
      filter(!is.na(Dates))
    
    date_template.df <- tibble(Dates = seq(start_date,end_date, by = "days")) %>% 
      mutate(year = gsub("-.*","",Dates),
             week = week(Dates),
             Dates = paste0(year,"-",week)) %>% 
      distinct(Dates)
    
    
  } else{ ##per days view 
    
    date_template.df <- tibble(Dates = seq(start_date,end_date, by = "days"))
    
  }
  
  date_input.df <- date_template.df %>% 
    left_join(data_filt.df %>% setNames(c("Dates","count"))) %>% 
    replace(is.na(.),0)
  
  
  if(date_sel == "Cumulative"){
    
    date_input.df <- date_input.df %>% 
      mutate(cumu_count = cumsum(count))
    
    py <- plot_ly(data = date_input.df,
                  type = 'scatter', 
                  mode = 'lines',
                  x = ~Dates,
                  y = ~cumu_count, 
                  fill = 'tozeroy') %>% 
      rangeslider(thickness = 0.1 ) %>% 
      layout(yaxis = list(title = xaxis),
             xaxis = list(title = ""))

  } else{
    
    py <- plot_ly(data = date_input.df,
                  type = "bar",
                  x = ~Dates,
                  y = ~count) %>%
      rangeslider(thickness = 0.1 ) %>% 
      layout(yaxis = list(title = xaxis),
             xaxis = list(title = ""))
    
  }
  
  return(py)
  
}

demographics_pie <- function(var_demo,var_date,title_sel,age_filter,date_filter,race_filter,gender_filter){
  
  pie_input.df <- data_mod.df %>% 
    ##duplicate to avoid issues when we rename certain variables 
    mutate(age_combine1 = age_combine,
           race_combine1 = race_combine,
           sex_combine1 = sex_combine) %>% 
    rename(selected_var_date = var_date,
           selected_var_demo = var_demo) %>% 
    filter(age_combine1 >= age_filter[1] & age_combine <= age_filter[2],
           selected_var_date >= date_filter[1] & selected_var_date <= date_filter[2],
           race_combine1 %in% race_filter,
           sex_combine1 %in% gender_filter) %>%
    group_by(selected_var_demo) %>% 
    summarise(n = n()) %>% 
    ungroup() %>% 
    mutate(prop = n/sum(n)*100)
  
  number_values = as.numeric(sum(pie_input.df[!is.na(pie_input.df["selected_var_demo"]),"n"]))
  
  plot_ly(pie_input.df,
          type = "pie",
          labels = ~selected_var_demo,
          values = ~prop,
          textinfo = "percent",
          hoverinfo = "text",
          text = ~paste("Number of individuals:",n),
          marker = list(line = list(color = 'white', width = 1))) %>% 
    layout(title = paste0(title_sel, " (n= ",number_values, ")"))
  
}

city_barplot <- function(var_demo,var_date,title_sel,age_filter,date_filter,race_filter,gender_filter){
  
  bar_input.df <- data_mod.df %>% 
    mutate(age_combine1 = age_combine,
           race_combine1 = race_combine,
           sex_combine1 = sex_combine) %>% 
    rename(selected_var_date = var_date,
           selected_var_demo = var_demo) %>% 
    filter(age_combine1 >= age_filter[1] & age_combine <= age_filter[2],
           selected_var_date >= date_filter[1] & selected_var_date <= date_filter[2],
           race_combine1 %in% race_filter,
           sex_combine1 %in% gender_filter) %>%
    count(selected_var_demo)
  
  number_values = as.numeric(sum(bar_input.df[!is.na(bar_input.df["selected_var_demo"]),"n"]))
  
  py <- plot_ly(data = bar_input.df,
                type = "bar",
                x = ~selected_var_demo,
                hoverinfo = "text",
                text = paste("Number of individuals:",bar_input.df$n),
                y = ~n) %>%
    layout(title = paste0(title_sel, " (n= ",number_values, ")"),
           yaxis = list(title = ""),
           xaxis = list(title = "",
                        tickangle = -45,
                        categoryorder = "total descending"))
  
}

age_histogram <- function(var_demo,var_date,title_sel,age_filter,date_filter,race_filter,gender_filter){
  
  histo_input.df <- data_mod.df %>% 
    mutate(age_combine1 = age_combine,
           race_combine1 = race_combine,
           sex_combine1 = sex_combine) %>% 
    rename(selected_var_date = var_date,
           selected_var_demo = var_demo) %>% 
    filter(age_combine1 >= age_filter[1] & age_combine1 <= age_filter[2],
           selected_var_date >= date_filter[1] & selected_var_date <= date_filter[2],
           race_combine1 %in% race_filter,
           sex_combine1 %in% gender_filter) 
  
  number_values = sum(!is.na(histo_input.df$selected_var_demo))
  binsize = age_filter[2] - (age_filter[1]-1)
  
  py <- plot_ly(histo_input.df, 
                x = ~selected_var_demo, 
                nbinsx = binsize,
                type = "histogram") %>%
    layout(xaxis = list(title = ""),
           title = paste0(title_sel, " (n= ",number_values, ")"))
  
}
  




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
                       min= "2021-12-01",
                       max= end_date,
                       start= "2021-12-01",
                       end= Sys.Date()),
        checkboxGroupInput("gender_filter", 
                           h4("Gender"), 
                           choices = list("Male", 
                                          "Female", 
                                          "Other",
                                          "Unknown/ not disclosed"),
                           selected = c("Male", 
                                        "Female", 
                                        "Other",
                                        "Unknown/ not disclosed")),
        sliderInput("age_filter",
                    h4("Age"), 
                    min = 20, max = 100, value = c(20, 100)),
        checkboxGroupInput("race_filter", 
                           h4("Race/Ethnicity"), 
                           choices = list("White", 
                                          "Black/ African American", 
                                          "Hispanic/ Latino",
                                          "Asian",
                                          "Alaskan Native",
                                          "Native American",
                                          "Native Hawaiian and Other Pacific Islander",
                                          "Multiracial/Multicultural",
                                          "Unknown/ not disclosed"),
                           selected = c("White", 
                                          "Black/ African American", 
                                          "Hispanic/ Latino",
                                          "Asian",
                                          "Alaskan Native",
                                          "Native American",
                                          "Native Hawaiian and Other Pacific Islander",
                                          "Multiracial/Multicultural",
                                          "Unknown/ not disclosed"))
    ),
    dashboardBody(
        style = "background-color:#2C3E50;",
        tabItem(tabName = "dashboard",
                fluidRow(
                    column(12, offset=2,
                    valueBoxOutput("box_signup", width=2),
                    valueBoxOutput("box_contact", width=2),
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
                                            plotlyOutput("sign_up_bar", height = 300)
                                     )),
                            tabPanel("Demographics",
                                     br(),
                                     column(width = 6,
                                            plotlyOutput("pie_race1", height = 250)
                                     ),
                                     column(width = 6,
                                            plotlyOutput("pie_gender1", height = 250)
                                     ),
                                     column(width = 6,
                                            plotlyOutput("pie_employee1", height = 250)
                                     ),
                                     column(width = 6,
                                            plotlyOutput("pie_ccfpatient1", height = 250)
                                     ),
                                     column(width = 6,
                                            plotlyOutput("bar_city1", height = 400)
                                     ),
                                     column(width = 6,
                                            plotlyOutput("bar_county1", height = 400)
                                     ),
                                     column(width = 12,
                                            plotlyOutput("age_histo1", height = 200)
                                     ))
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
                                    plotlyOutput("contact_bar", height = 300)
                                    )),
                            tabPanel("Call attempts stats",
                                     br(),
                                     column(width = 6,
                                            plotlyOutput("pie_call_attempts", height = 250)
                                     )),
                            tabPanel("Eligible/ Interested stats",
                                     br(),
                                     column(width = 6,
                                            plotlyOutput("pie_whether_eligible", height = 250)
                                     ),
                                     column(width = 6,
                                            plotlyOutput("pie_whether_ms_relative", height = 250)
                                     ),
                                     column(width = 6,
                                            plotlyOutput("pie_saturday_mri", height = 250)
                                     )),
                            tabPanel("Demographics",
                                     br(),
                                     column(width = 6,
                                            plotlyOutput("pie_race2", height = 250)
                                     ),
                                     column(width = 6,
                                            plotlyOutput("pie_gender2", height = 250)
                                     ),
                                     column(width = 6,
                                            plotlyOutput("pie_employee2", height = 250)
                                     ),
                                     column(width = 6,
                                            plotlyOutput("pie_ccfpatient2", height = 250)
                                     ),
                                     column(width = 6,
                                            plotlyOutput("bar_city2", height = 400)
                                     ),
                                     column(width = 6,
                                            plotlyOutput("bar_county2", height = 400)
                                     ),
                                     column(width = 12,
                                            plotlyOutput("age_histo2", height = 200)
                                     ))
                            
                        )),
                    # box(title = "E-Mail sent", solidHeader = F,  status = "primary",width=6,
                    #     tabsetPanel(
                    #         tabPanel("History",
                    #                  br(),
                    #                  column(width=4,
                    #                         buttonGroup( inputId = "date_type3",
                    #                                      choices = c("Cumulative", "Per day", "Per week"), 
                    #                                      selected = "Cumulative", multiple= F)
                    #                  ),
                    #                  column(width=6,
                    #                         buttonGroup( inputId = "value_type3",
                    #                                      choices = c("Value", "Log"), 
                    #                                      selected = "Value", multiple= F)
                    #                  ),
                    #                  column(width=12,
                    #                         plotOutput("plot3", height = 250)
                    #                  )),
                    #         tabPanel("E-Mail stats")
                    #     )),
                    box(title = "Appointment", solidHeader = F,  status = "primary",width=6,
                        tabsetPanel(
                            tabPanel("Appointment call",
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
                                            plotlyOutput("appointment_bar", height = 300)
                                     )),
                            tabPanel("Demographics",
                                     br(),
                                     column(width = 6,
                                            plotlyOutput("pie_race3", height = 250)
                                     ),
                                     column(width = 6,
                                            plotlyOutput("pie_gender3", height = 250)
                                     ),
                                     column(width = 6,
                                            plotlyOutput("pie_employee3", height = 250)
                                     ),
                                     column(width = 6,
                                            plotlyOutput("pie_ccfpatient3", height = 250)
                                     ),
                                     column(width = 6,
                                            plotlyOutput("bar_city3", height = 400)
                                     ),
                                     column(width = 6,
                                            plotlyOutput("bar_county3", height = 400)
                                     ),
                                     column(width = 12,
                                            plotlyOutput("age_histo3", height = 200)
                                     )),
                            tabPanel("Visit scheduled",
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
                                           plotlyOutput("visit_bar", height = 300)
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
    
  observeEvent(input$gender_filter,{
    print(input$gender_filter)
    print(input$race_filter)
  })

    output$box_signup <- renderValueBox({
      
        valueBox(
          #assuming that all individuals in this file signed up 
          nrow(data), paste0("Sign up (today: ", data_mod.df %>% 
                                filter(sign_up_datetime == current_day) %>% 
                                nrow(.),")"), icon = icon("sign-in"),
            color = "aqua"
        )
    })
    
    output$box_contact <- renderValueBox({
      
      value_input <- data_mod.df %>% 
        filter(!is.na(initial_contact_response)) %>% 
        nrow(.)
      
        valueBox(
          value_input, paste0("Contacted (today: ", data_mod.df %>% 
                                filter(initial_contact_date == current_day) %>% 
                                nrow(.),")"), icon = icon("phone"),
            color = "light-blue"
        )
    })
    
    output$box_app_scheduled <- renderValueBox({
      
      value_input <- data_mod.df %>% 
                  filter(step %in% c("first _isit","appointment_scheduled")) %>%      ###"first _isit"?
                  nrow(.)
      
        valueBox(
          value_input, paste0("Appointment scheduled (today: ", data_mod.df %>% 
                                filter(appointment_call_date == current_day) %>% 
                                nrow(.),")"), icon = icon("phone"),
            color = "navy"
        )
    })
    
    output$box_onboard <- renderValueBox({
      
      #not sure if this is the right way to get this number 
      value_input <- data_mod.df %>% 
        filter(step %in% c("first_visit")) %>% 
        nrow(.)
      
        valueBox(
          value_input, paste0("Onboarded (today: ", data_mod.df %>% 
                                filter(visit_schedule_date == current_day) %>% 
                                nrow(.),")"),icon = icon("users"),
            color = "green"
        )
    })
    
    
    
    
    #plot changes upon data_type selection 
    
    observeEvent(input$date_type1,{
      
      output$sign_up_bar <- renderPlotly({
            overviewplot_1("sign_up_datetime","Number of Sign-ups",input$date_type1,
                           input$age_filter,input$date_filter,input$race_filter,input$gender_filter)

          })
    })
    
    
    observeEvent(input$date_type2,{
      
      output$contact_bar <- renderPlotly({
        overviewplot_1("initial_contact_date","Number of Contacts",input$date_type2,
                       input$age_filter,input$date_filter,input$race_filter,input$gender_filter)
        
      })
    })
    
    
    observeEvent(input$date_type3,{
      
      output$appointment_bar <- renderPlotly({
        overviewplot_1("appointment_call_date","Number of Appointment calls",input$date_type3,
                       input$age_filter,input$date_filter,input$race_filter,input$gender_filter)
        
      })
    })
    
    observeEvent(input$date_type4,{
      
      output$visit_bar <- renderPlotly({
        overviewplot_1("visit_schedule_date","Number of scheduled visits",input$date_type4,
                       input$age_filter,input$date_filter,input$race_filter,input$gender_filter)
        
      })
    })
    
    
    #SIGN UP#
    #demographics sign up date
    output$pie_race1 <- renderPlotly({
      demographics_pie("race_combine","sign_up_datetime","Race",input$age_filter,input$date_filter,input$race_filter,input$gender_filter)
    })
    
    output$pie_gender1 <- renderPlotly({
      demographics_pie("sex_combine","sign_up_datetime","Sex",input$age_filter,input$date_filter,input$race_filter,input$gender_filter)
    })
    
    output$pie_employee1 <- renderPlotly({
      demographics_pie("employ_status_desc","sign_up_datetime","Employee status",input$age_filter,input$date_filter,input$race_filter,input$gender_filter)
    })
    
    output$pie_ccfpatient1 <- renderPlotly({
      demographics_pie("ccf_patient","sign_up_datetime","CCF patient",input$age_filter,input$date_filter,input$race_filter,input$gender_filter)
    })
    
    output$bar_city1 <- renderPlotly({
    city_barplot("pat_city","sign_up_datetime","City",input$age_filter,input$date_filter,input$race_filter,input$gender_filter)
    })
    
    output$bar_county1 <- renderPlotly({
      city_barplot("pat_county","sign_up_datetime","County",input$age_filter,input$date_filter,input$race_filter,input$gender_filter)
    })
    
    output$age_histo1 <- renderPlotly({
      age_histogram("age_combine","sign_up_datetime","Age distribution",input$age_filter,input$date_filter,input$race_filter,input$gender_filter)
    })

    #CONTACTS#
    ###demographics contacts###
    output$pie_race2 <- renderPlotly({
      demographics_pie("race_combine","initial_contact_date","Race",input$age_filter,input$date_filter,input$race_filter,input$gender_filter)
    })
    
    output$pie_gender2 <- renderPlotly({
      demographics_pie("sex_combine","initial_contact_date","Sex",input$age_filter,input$date_filter,input$race_filter,input$gender_filter)
    })
    
    output$pie_employee2 <- renderPlotly({
      demographics_pie("employ_status_desc","initial_contact_date","Employee status",input$age_filter,input$date_filter,input$race_filter,input$gender_filter)
    })
    
    output$pie_ccfpatient2 <- renderPlotly({
      demographics_pie("ccf_patient","initial_contact_date","CCF patient",input$age_filter,input$date_filter,input$race_filter,input$gender_filter)
    })
    
    output$bar_city2 <- renderPlotly({
      city_barplot("pat_city","initial_contact_date","City",input$age_filter,input$date_filter,input$race_filter,input$gender_filter)
    })
    
    output$bar_county2 <- renderPlotly({
      city_barplot("pat_county","initial_contact_date","County",input$age_filter,input$date_filter,input$race_filter,input$gender_filter)
    })
    
    output$age_histo2 <- renderPlotly({
      age_histogram("age_combine","initial_contact_date","Age distribution",input$age_filter,input$date_filter,input$race_filter,input$gender_filter)
    })
    
    #call atempts 
    output$pie_call_attempts <- renderPlotly({
      demographics_pie("number_initial_contact_attempt","appointment_call_date","Call attempts",input$age_filter,input$date_filter,input$race_filter,input$gender_filter)
    })
    
    #eligible
    output$pie_whether_eligible <- renderPlotly({
      demographics_pie("whether_eligible","appointment_call_date","Whether eligible",input$age_filter,input$date_filter,input$race_filter,input$gender_filter)
    })
    
    #ms relative
    output$pie_whether_ms_relative <- renderPlotly({
      demographics_pie("whether_ms_relative","appointment_call_date","Whether MS relative",input$age_filter,input$date_filter,input$race_filter,input$gender_filter)
    })
    
    #saturday mri
    output$pie_saturday_mri <- renderPlotly({
      demographics_pie("saturday_mri","appointment_call_date","Whether saturday MRI", input$age_filter,input$date_filter,input$race_filter,input$gender_filter)
    })
    
    #APPOINTMENTS#
    ###demographics appointment###
    output$pie_race3 <- renderPlotly({
      demographics_pie("race_combine","appointment_call_date","Race",input$age_filter,input$date_filter,input$race_filter,input$gender_filter)
    })
    
    output$pie_gender3 <- renderPlotly({
      demographics_pie("sex_combine","appointment_call_date","Sex",input$age_filter,input$date_filter,input$race_filter,input$gender_filter)
    })
    
    output$pie_employee3 <- renderPlotly({
      demographics_pie("employ_status_desc","appointment_call_date","Employee status",input$age_filter,input$date_filter,input$race_filter,input$gender_filter)
    })
    
    output$pie_ccfpatient3 <- renderPlotly({
      demographics_pie("ccf_patient","appointment_call_date","CCF patient",input$age_filter,input$date_filter,input$race_filter,input$gender_filter)
    })
    
    output$bar_city3 <- renderPlotly({
      city_barplot("pat_city","appointment_call_date","City",input$age_filter,input$date_filter,input$race_filter,input$gender_filter)
    })
    
    output$bar_county3 <- renderPlotly({
      city_barplot("pat_county","appointment_call_date","County",input$age_filter,input$date_filter,input$race_filter,input$gender_filter)
    })
    
    output$age_histo3 <- renderPlotly({
      age_histogram("age_combine","appointment_call_date","Age distribution",input$age_filter,input$date_filter,input$race_filter,input$gender_filter)
    })
    
    
    
    
    output$plot5 <- renderPlot({
        hist(rnorm(500))
    })
    output$plot6 <- renderPlot({
        hist(rnorm(500))
    })
}

shinyApp(ui, server)
