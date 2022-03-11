
library(odbc)

###################################################################################
##################### load data from SQL server
###################################################################################

con <- dbConnect(odbc::odbc(), driver = "SQL Server", server = "lri-sqldb-d", 
                 database = "CCBS")
query <- "
select 
  weekday_prefer_Saturday,
  whether_initial_contact,
  number_appointment_attempt,
  weekday_prefer_Friday,
  weekday_prefer_Monday,
  number_initial_contact_attempt,
  weekday_prefer_Sunday,
  weekday_prefer_Thursday,
  weekday_prefer_Tuesday,
  daytime_prefer_morning,
  daytime_prefer_evening,
  daytime_prefer_afternoon,
  ccf_patient,
  weekday_prefer_Wednesday,
  whether_appointment_call,
  whether_appointment_scheduled,
  whether_eligible,
  whether_first_visit,
  record_id,
  sign_up_datetime,
  study_id,
  visit_schedule_date,
  saturday_mri,
  whether_ms_relative,
  initial_contact_response3,
  initial_contact_response,
  initial_contact_date,
  heard_about_study,
  decline_reason,
  appointment_call_response_3,
  initial_contact_response2,
  not_eligible_reason,
  appointment_call_response_1,
  appointment_call_date,
  schedule_made_datetime,
  age_combine,
  appointment_call_response_2,
  sex_combine,
  ethnicity_combine,
  step,
  employ_status_desc,
  occupation,
  pat_city,
  pat_county,
  race_combine
from  [CCBS].[QU].[RedCap_feature_data]
"
data <- dbGetQuery(con, query)

dbDisconnect(con)



###################################################################################
##################### R shiny dashboard
###################################################################################



library(shiny)
library(shinydashboard)
library(readr)
library(shinyWidgets)
library(shinyThings)  
library(tidyverse)
library(plotly)
library(lubridate)
library(DT)
library(tippy)
#devtools::install_github("gadenbuie/shinyThings")


#data <- read_csv("github/sample_data_dashboard.csv")

end_date <- as.Date("2023-12-31") ##set a time in the future to account for visits that are sheduled for the future 
current_day <- as.Date(Sys.time() %>% str_split(.," ") %>% unlist() %>% .[1]) 
#Sys.time() %>% str_split(.," ") %>% unlist() %>% .[1]

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

#data modification 
data_mod.df <- data %>% 
  mutate(first_visit_date = ifelse(whether_first_visit == 1, as.character(schedule_made_datetime), NA)) %>%   
  mutate(sign_up_datetime = gsub(" .*","",sign_up_datetime) %>% as.Date(),
         initial_contact_date = gsub(" .*","",initial_contact_date) %>% as.Date(),
         appointment_call_date = gsub(" .*","",appointment_call_date) %>% as.Date(),
         #schedule_made_datetime = gsub(" .*","",schedule_made_datetime) %>% as.Date(),
         schedule_made_datetime = gsub(" .*","",schedule_made_datetime) %>% as.Date(),
         first_visit_date =  gsub(" .*","",first_visit_date) %>% as.Date(),
         visit_schedule_date = gsub(" .*","",visit_schedule_date) %>% as.Date()) %>% 
  mutate(race_combine = ifelse(is.na(race_combine),"Unknown/ not disclosed",race_combine),
         sex_combine = ifelse(is.na(sex_combine),"Unknown/ not disclosed",sex_combine)) %>% 
  mutate(ccf_patient = ifelse(ccf_patient == 1,"yes","no")) %>% 
  mutate(whether_ms_relative = case_when(is.na(whether_ms_relative) ~ "Not available",
                                         whether_ms_relative == 1 ~ "yes",
                                         whether_ms_relative == 0 ~ "no",
                                         T ~"stop"),
         whether_eligible_yn = ifelse(whether_eligible == 1 & !is.na(whether_eligible),"yes",
                                      ifelse(whether_eligible == 0 & !is.na(whether_eligible),"no", NA)),
         saturday_mri_yn = ifelse(saturday_mri == 1 & !is.na(saturday_mri),"yes",
                                  ifelse(saturday_mri == 0 & !is.na(saturday_mri),"no", NA))) %>% 
  mutate(pat_city = tolower(pat_city) %>% firstup(.)) %>% 
  mutate(pat_county = tolower(pat_county) %>% firstup(.)) %>% 
  mutate(employ_status_desc = tolower(employ_status_desc) %>% firstup(.),
         whether_sign_up = 1) 

# mutate(weekday_prefer_Saturday = sample(c(1,0),nrow(.),replace = T),
#        weekday_prefer_Monday = sample(c(1,0),nrow(.),replace = T),
#        daytime_prefer_morning = sample(c(1,0),nrow(.),replace = T))

#daily/weekly increase/decrease 

trend_time <- function(data_mod.df,current_day, var){
  
  percent_diff <- function(value_now,value_before){
    
    out_value <- (value_now/value_before-1)*100
    
    out_value <- case_when(is.nan(out_value) ~0,
                           out_value == Inf ~100,
                           TRUE ~ out_value)
    
    return(out_value)
    
  }
  
  data_mod_sel.df <- data_mod.df %>% 
    rename(var_sel = var) 
  
  
  value_yesterday <- data_mod_sel.df %>% 
    filter(var_sel == current_day - days(1)) %>% 
    nrow(.)
  
  value_yesterday_b <- data_mod_sel.df %>% 
    filter(var_sel == current_day - days(2)) %>% 
    nrow(.)
  
  value_7days <- data_mod_sel.df %>% 
    filter(var_sel %in% seq(as.Date(current_day - days(6)),as.Date(current_day- days(0)), by="days")) %>% 
    nrow(.)
  
  value_7days_b <- data_mod_sel.df %>% 
    filter(var_sel %in% seq(as.Date(current_day - days(13)),as.Date(current_day- days(7)), by="days")) %>% 
    nrow(.)
  
  value_month <- data_mod_sel.df %>% 
    filter(var_sel %in% seq(as.Date(current_day - days(29)),as.Date(current_day- days(0)), by="days")) %>% 
    nrow(.)
  
  value_month_b <- data_mod_sel.df %>% 
    filter(var_sel %in% seq(as.Date(current_day - days(59)),as.Date(current_day- days(30)), by="days")) %>%
    nrow(.)
  
  yesterday_per <- percent_diff(value_yesterday,value_yesterday_b) %>% round(.,1) %>% ifelse(. >0,paste0("+",.),.)
  
  day7_per <- percent_diff(value_7days,value_7days_b) %>% round(.,1) %>% ifelse(. >0,paste0("+",.),.)
  
  month_per <- percent_diff(value_month,value_month_b) %>% round(.,1) %>% ifelse(. >0,paste0("+",.),.)
  
  return(list(value_yesterday,yesterday_per,value_7days,day7_per,value_month,month_per))
}






#predefined plot functions 
overviewplot_1 <- function(data_mod.df,var,xaxis,date_sel,age_filter,date_filter,race_filter,gender_filter){
  
  start_date = as.Date(date_filter[1])
  end_date = as.Date(date_filter[2])
  
  data_filt.df <- data_mod.df %>% 
    rename(selected_var = var) %>% 
    # filter(age_combine >= age_filter[1] & age_combine <= age_filter[2],
    #        selected_var >= date_filter[1] & selected_var <= date_filter[2],
    #        race_combine %in% race_filter,
    #        sex_combine %in% gender_filter) %>% 
    group_by(selected_var) %>% 
    summarise(n = n())
  
  validate(need(nrow(data_filt.df) > 0, message = "No data after filtering."))
  
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
    
    date_input.df <- date_input.df %>% 
      mutate(day7_avg = 0,
             month_avg  = 0)
    
    for(i in 1:nrow(date_input.df)){
      
      end_day <- ifelse(i-6>1,i-6,1)
      
      end_month <- ifelse(i-30>1,i-30,1)
      
      date_input.df$day7_avg[i] <- mean(date_input.df$count[end_day:i],na.rm = T)
      date_input.df$month_avg[i] <- mean(date_input.df$count[end_month:i],na.rm = T)
      
    } 
    
    if(date_sel == "Per week"){
      
      py <- plot_ly(data = date_input.df %>% 
                      mutate(Dates = factor(Dates, levels = Dates)),
                    type = "bar",
                    x = ~Dates,
                    y = ~count,
                    name = "Number of individuals") %>%
        rangeslider(thickness = 0.1 ) %>% 
        add_trace(type = 'scatter', 
                  mode = 'lines',
                  line = list(color = "orange"),
                  x = ~Dates,
                  y = ~day7_avg,
                  name = "7 day average") %>% 
        add_trace(type = 'scatter', 
                  mode = 'lines',
                  line = list(color = "yellow"),
                  x = ~Dates,
                  y = ~month_avg,
                  name = "30 day average") %>% 
        layout(yaxis = list(title = xaxis),
               xaxis = list(title = ""),
               #legend = list(x = 0.1, y = 0.9),
               legend = list(orientation = "h",  
                             xanchor = "center",  
                             x = 0.5))
      
    } else if (date_sel == "Per day"){
      
      py <- plot_ly(data = date_input.df,
                    type = "bar",
                    x = ~Dates,
                    y = ~count,
                    name = "Number of individuals") %>%
        rangeslider(thickness = 0.1 ) %>% 
        add_trace(type = 'scatter', 
                  mode = 'lines',
                  line = list(color = "orange"),
                  x = ~Dates,
                  y = ~day7_avg,
                  name = "7 day average") %>% 
        add_trace(type = 'scatter', 
                  mode = 'lines',
                  line = list(color = "yellow"),
                  x = ~Dates,
                  y = ~month_avg,
                  name = "30 day average") %>% 
        layout(yaxis = list(title = xaxis),
               xaxis = list(title = ""),
               #legend = list(x = 0.1, y = 0.9),
               legend = list(orientation = "h",  
                             xanchor = "center",  
                             x = 0.5))
    }
    
  }
  
  
  return(py)
  
} 

demographics_pie <- function(data_mod.df,var_demo,var_date,title_sel,age_filter,date_filter,race_filter,gender_filter){
  
  pie_input.df <- data_mod.df %>% 
    ##duplicate to avoid issues when we rename certain variables 
    # mutate(age_combine1 = age_combine,
    #        race_combine1 = race_combine,
    #        sex_combine1 = sex_combine) %>% 
    rename(selected_var_date = var_date,
           selected_var_demo = var_demo) %>% 
    filter(#age_combine1 >= age_filter[1] & age_combine <= age_filter[2],
      ##selected_var_date >= date_filter[1] & selected_var_date <= date_filter[2],
      selected_var_date == 1,
      selected_var_demo != "Not available"
      #,
      #race_combine1 %in% race_filter,
      #sex_combine1 %in% gender_filter
    ) %>%
    group_by(selected_var_demo) %>% 
    summarise(n = n()) %>% 
    ungroup() %>% 
    mutate(prop = n/sum(n)*100)
  
  number_values = as.numeric(sum(pie_input.df[!is.na(pie_input.df["selected_var_demo"]),"n"]))
  
  validate(need(nrow(pie_input.df) > 0, message = "No data after filtering."))
  
  plot_ly(pie_input.df,
          type = "pie",
          labels = ~selected_var_demo,
          values = ~prop,
          textinfo = "percent",
          hoverinfo = "text",
          text = ~paste("Number of individuals:",n),
          marker = list(line = list(color = 'white', width = 1))) %>% 
    layout(title = list( text = paste0(title_sel, " (n= ",number_values, ")"),
                         font = list(family = "Arial",
                                     size = 14)))
  
}

city_barplot <- function(data_mod.df,var_demo,var_date,title_sel,age_filter,date_filter,race_filter,gender_filter){
  
  bar_input.df <- data_mod.df %>% 
    # mutate(age_combine1 = age_combine,
    #        race_combine1 = race_combine,
    #        sex_combine1 = sex_combine) %>% 
    rename(selected_var_date = var_date,
           selected_var_demo = var_demo) %>% 
    filter(#age_combine1 >= age_filter[1] & age_combine <= age_filter[2],
      ##selected_var_date >= date_filter[1] & selected_var_date <= date_filter[2],
      selected_var_date == 1
      #,
      #race_combine1 %in% race_filter,
      #sex_combine1 %in% gender_filter
    ) %>%
    count(selected_var_demo)
  
  validate(need(nrow(bar_input.df) > 0, message = "No data after filtering."))
  
  number_values = as.numeric(sum(bar_input.df[!is.na(bar_input.df["selected_var_demo"]),"n"]))
  
  bar_input.df = bar_input.df %>% top_n(n, 20)
  
  py <- plot_ly(data = bar_input.df,
                type = "bar",
                x = ~selected_var_demo,
                hoverinfo = "text",
                text = paste("Number of individuals:",bar_input.df$n),
                y = ~n) %>%
    layout(title = list( text = paste0(title_sel, " (n= ",number_values, ")"),
                         font = list(family = "Arial",
                                     size = 14)),
           yaxis = list(title = ""),
           xaxis = list(title = "",
                        tickangle = -45,
                        categoryorder = "total descending"))
  
}

age_histogram <- function(data_mod.df,var_demo,var_date,title_sel,age_filter,date_filter,race_filter,gender_filter){
  
  histo_input.df <- data_mod.df %>% 
    # mutate(age_combine1 = age_combine,
    #        race_combine1 = race_combine,
    #        sex_combine1 = sex_combine) %>% 
    rename(selected_var_date = var_date,
           selected_var_demo = var_demo) %>% 
    filter(#age_combine1 >= age_filter[1] & age_combine1 <= age_filter[2],
      #selected_var_date >= date_filter[1] & selected_var_date <= date_filter[2],
      selected_var_date == 1
      #,
      #race_combine1 %in% race_filter,
      #sex_combine1 %in% gender_filter
    ) 
  
  validate(need(nrow(histo_input.df) > 0, message = "No data after filtering."))
  
  number_values = sum(!is.na(histo_input.df$selected_var_demo))
  binsize = age_filter[2] - (age_filter[1]-1)
  
  py <- plot_ly(histo_input.df, 
                x = ~selected_var_demo, 
                nbinsx = binsize,
                type = "histogram") %>%
    layout(xaxis = list(title = ""),
           title = list( text = paste0(title_sel, " (n= ",number_values, ")"),
                         font = list(family = "Arial",
                                     size = 14)))
  
}

preferred_days <- function(age_filter,date_filter,race_filter,gender_filter){
  
  data_mod_filt.df <- data_mod.df %>% 
    filter(sign_up_datetime >= as.Date("2022-02-03")) 
  #%>%
  # mutate(age_combine1 = age_combine,
  #        race_combine1 = race_combine,
  #        sex_combine1 = sex_combine) %>% 
  # filter(age_combine1 >= age_filter[1] & age_combine1 <= age_filter[2],
  #        sign_up_datetime >= date_filter[1] & sign_up_datetime <= date_filter[2],
  #        race_combine1 %in% race_filter,
  #        sex_combine1 %in% gender_filter
  #        ) 
  
  
  pl_input.df <- data_mod_filt.df %>% 
    select(weekday_prefer_Monday,weekday_prefer_Tuesday,weekday_prefer_Wednesday,weekday_prefer_Thursday,weekday_prefer_Friday,weekday_prefer_Saturday,weekday_prefer_Sunday) %>% 
    mutate_all(as.numeric) %>%
    colSums() %>% 
    as_tibble() %>% 
    mutate(day = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")) %>% 
    mutate(prop = value/nrow(data_mod_filt.df)*100)
  pl_input.df$day = factor(pl_input.df$day, 
                           levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
  
  plot_ly(pl_input.df,
          type = "bar",
          x = ~day,
          y = ~prop,
          hoverinfo = "text",
          hovertext = paste("Number of individuals:",pl_input.df$value)) %>%
    layout(title = paste0("Preferred days of the week to be contaced"),
           yaxis = list(title = "Proportion of participants (%)"),
           xaxis = list(title = "",
                        tickangle = -45
                        #,categoryorder = "total descending"
                        ),
           margin = list(t = 70))
  
}

preferred_time <- function(age_filter,date_filter,race_filter,gender_filter){
  
  data_mod_filt.df <- data_mod.df %>% 
    filter(sign_up_datetime >= as.Date("2022-02-03")) 
  #%>%
  # mutate(age_combine1 = age_combine,
  #        race_combine1 = race_combine,
  #        sex_combine1 = sex_combine) %>% 
  # filter(age_combine1 >= age_filter[1] & age_combine1 <= age_filter[2],
  #        sign_up_datetime >= date_filter[1] & sign_up_datetime <= date_filter[2],
  #        race_combine1 %in% race_filter,
  #        sex_combine1 %in% gender_filter)
  
  pl_input.df <- data_mod_filt.df %>% 
    filter(sign_up_datetime >= as.Date("2022-02-03")) %>% 
    select(daytime_prefer_morning,daytime_prefer_afternoon,daytime_prefer_evening) %>% 
    mutate_all(as.numeric) %>%
    colSums() %>% 
    as_tibble() %>% 
    mutate(day = c("Morning","Afternoon","Evening")) %>% 
    mutate(prop = value/nrow(data_mod_filt.df)*100)
  
  pl_input.df$day = factor(pl_input.df$day, 
                           levels =c("Morning","Afternoon","Evening"))
  
  
  plot_ly(pl_input.df,
          type = "bar",
          x = ~day,
          y = ~prop,
          hoverinfo = "text",
          hovertext = paste("Number of individuals:",pl_input.df$value)) %>%
    layout(title = paste0("Preferred day time to be contaced"),
           yaxis = list(title = "Proportion of participants (%)"),
           xaxis = list(title = "",
                        tickangle = -45
                        #,categoryorder = "total descending"
                        ),
           margin = list(t = 70))
  
}


ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "CCBS dashboard"),
  dashboardSidebar(collapsed = TRUE),
  # dashboardSidebar(
  #   sidebarMenu(
  #     menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
  #     menuItem("About", tabName = "about", icon = icon("question"))
  #   ),
  #   br(),
  #   # dateRangeInput("date_filter", h5("Date range"),
  #   #                min= "2021-12-01",
  #   #                max= end_date,
  #   #                start= "2021-12-01",
  #   #                end= Sys.Date()),
  #   checkboxGroupInput("gender_filter", 
  #                      h5("Gender"), 
  #                      choices = list("Male", 
  #                                     "Female", 
  #                                     "Other",
  #                                     "Unknown/ not disclosed"),
  #                      selected = c("Male", 
  #                                   "Female", 
  #                                   "Other",
  #                                   "Unknown/ not disclosed")),
  #   sliderInput("age_filter",
  #               h5("Age"), 
  #               min = 20, max = 100, value = c(20, 100)),
  #   checkboxGroupInput("race_filter", 
  #                      h5("Race/Ethnicity"), 
  #                      choices = list("White", 
  #                                     "Black/ African American", 
  #                                     "Hispanic/ Latino",
  #                                     "Asian",
  #                                     "Alaskan Native",
  #                                     "Native American",
  #                                     "Native Hawaiian and Other Pacific Islander",
  #                                     "Multiracial/Multicultural",
  #                                     "Unknown/ not disclosed"),
  #                      selected = c("White", 
  #                                   "Black/ African American", 
  #                                   "Hispanic/ Latino",
  #                                   "Asian",
  #                                   "Alaskan Native",
  #                                   "Native American",
  #                                   "Native Hawaiian and Other Pacific Islander",
  #                                   "Multiracial/Multicultural",
  #                                   "Unknown/ not disclosed")),
  #   br()
  #   #,
  #   #div(actionButton("submit_filter", "Submit filter"), align= "center", style="font-weight:bold")
  # ),
  dashboardBody(
    style = "background-color:#2C3E50;",
    tabItem(tabName = "dashboard",
            fluidRow(
              column(12, offset=1,
                     valueBoxOutput("box_signup", width=2),
                     valueBoxOutput("box_contact", width=2),
                     valueBoxOutput("box_app_calls", width=2),
                     #valueBoxOutput("box_succ_app_calls", width=2),
                     valueBoxOutput("box_visit_scheduled", width=2),
                     valueBoxOutput("box_onboard", width = 2)
              )
            ),
            fluidRow(
              box(title = p("Enrolled participants",
                            tippy(icon("question-circle"),
                                  tooltip = h6(paste0("Participant came in for their scheduled visit time and is now in CCBS study."),
                                               align = "left"),
                                  animation = "scale", 
                                  theme = "light")),
                  solidHeader = T,  status = "success", width=12,
                  tabsetPanel(
                    tabPanel("First visit completed",
                             br(),
                             column(width=6, 
                                    column(width=6,
                                           buttonGroup( inputId = "date_type5",
                                                        choices = c("Cumulative", "Per day", "Per week"), 
                                                        selected = "Cumulative", multiple= F)
                                    ),
                                    column(width=12,
                                           br(),
                                           plotlyOutput("onboarded_bar", height = 400)
                                    )),
                             column(width = 6,
                                    column(width = 6,
                                           plotlyOutput("pie_gender5", height = 300)),
                                    column(width = 6,
                                           plotlyOutput("pie_race5", height = 300)),
                                    column(width=12,
                                           plotlyOutput("age_histo5", height = 200)))),
                    #tabPanel("Second visit"),
                    tabPanel("More demographics",
                             br(),
                             column(width=3,
                                    plotlyOutput("pie_employee5", height = 200)),
                             column(width=3,
                                    plotlyOutput("pie_ccfpatient5", height = 200)),
                             column(width=3,
                                    plotlyOutput("bar_city5", height = 300)),
                             column(width=3,
                                    plotlyOutput("bar_county5", height = 300))),
                    tabPanel("Future visits",
                             br(),
                             column(width=12, 
                                    column(width=3,
                                           buttonGroup( inputId = "date_type5_1",
                                                        choices = c("Cumulative", "Per day", "Per week"), 
                                                        selected = "Cumulative", multiple= F)
                                    ),
                                    column(width=12,
                                           br(),
                                           plotlyOutput("future_visits", height = 400)
                                    ))),
                  )),
              box(title= p("Sign up",
                           tippy(icon("question-circle"),
                                 tooltip = h6(paste0("Participant seeks interest in our study and signs up on website to receive more information."),
                                              align = "left"),
                                 animation = "scale", 
                                 theme = "light")),
                  solidHeader = F, status = "primary",width=6,
                  collapsible = T,
                  collapsed = T,
                  tabsetPanel(
                    tabPanel("History",
                             br(),
                             column(width=6,
                                    buttonGroup( inputId = "date_type1",
                                                 choices = c("Cumulative", "Per day", "Per week"), 
                                                 selected = "Cumulative", multiple= F)
                             ),
                             column(width=12,
                                    plotlyOutput("sign_up_bar", height = 300)
                             )),
                    tabPanel("Contact preferrences",
                             column(width=6,
                                    plotlyOutput("preferred_days", height = 387)
                             ),
                             column(width=6,
                                    plotlyOutput("preferred_daytime", height = 387)
                             ))
                  )),
              box(title = p("Contact",
                            tippy(icon("question-circle"),
                                  tooltip = h6(paste0("Research Coordinators first phone call with potential participant."),
                                               align = "left"),
                                  animation = "scale", 
                                  theme = "light")),
                  solidHeader = F,  status = "primary",width=6,
                  collapsible = T,
                  collapsed = T,
                  tabsetPanel(
                    tabPanel("History",
                             br(),
                             column(width=6,
                                    buttonGroup( inputId = "date_type2",
                                                 choices = c("Cumulative", "Per day", "Per week"), 
                                                 selected = "Cumulative", multiple= F)
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
                                    plotlyOutput("pie_saturday_mri", height = 250)
                             )),
                    tabPanel("MS relatives",
                             br(),
                             column(width = 12,
                                    plotlyOutput("pie_whether_ms_relative")),
                             column(width = 12,
                                    DT::dataTableOutput("ms_relative_table"))
                    )
                  )),
              fluidRow(
                column(12,
                       box(title = p("Appointment calls",
                                     tippy(icon("question-circle"),
                                           tooltip = h6(paste0("Return phone call to answer questions and schedule first visit to come in."),
                                                        align = "left"),
                                           animation = "scale", 
                                           theme = "light")),
                           solidHeader = F,  status = "primary",width=6,
                           collapsible = T,
                           collapsed = T,
                           tabsetPanel(
                             tabPanel("History",
                                      br(),
                                      column(width=6,
                                             buttonGroup( inputId = "date_type3",
                                                          choices = c("Cumulative", "Per day", "Per week"), 
                                                          selected = "Cumulative", multiple= F)
                                      ),
                                      column(width=6,
                                             buttonGroup( inputId = "date_type3_1",
                                                          choices = c("All","Only sucessful calls"), 
                                                          selected = "All", multiple= F)
                                      ),
                                      column(width=12,
                                             plotlyOutput("appointment_bar", height = 300)
                                      ))
                           )),
                       box(title = p("Scheduled visits",
                                     tippy(icon("question-circle"),
                                           tooltip = h6(paste0("Participants are scheduled appointments with RCs"),
                                                        align = "left"),
                                           animation = "scale", 
                                           theme = "light")),
                           solidHeader = F,  status = "primary",width=6,
                           collapsible = T,
                           collapsed = T,
                           tabsetPanel(
                             tabPanel("History",
                                      br(),
                                      column(width=6,
                                             buttonGroup( inputId = "date_type4",
                                                          choices = c("Cumulative", "Per day", "Per week"), 
                                                          selected = "Cumulative", multiple= F)
                                      ),
                                      column(width=12,
                                             plotlyOutput("visit_bar", height = 300)
                                      )),
                             # tabPanel("Scheduled visits",
                             #          br(),
                             #          column(width=6,
                             #                 buttonGroup( inputId = "date_type4_1",
                             #                              choices = c("Cumulative", "Per day", "Per week"), 
                             #                              selected = "Cumulative", multiple= F)
                             #          ),
                             #          column(width=12,
                             #                 plotlyOutput("visit_future_bar", height = 300)
                             #          ))
                           )))),
              box(title = "Demographics of enrollment steps", solidHeader = T,  status = "primary", width=12,
                  collapsible = T,
                  collapsed = T,
                  column(width=12,
                         br(),
                         box(width=3,
                             title= p("Sign up",
                                      tippy(icon("question-circle"),
                                            tooltip = h6(paste0("Participant seeks interest in our study and signs up on website to receive more information."),
                                                         align = "left"),
                                            animation = "scale", 
                                            theme = "light")),
                             solidHeader = T,  
                             status = "primary",
                             br(),
                             br(),
                             plotlyOutput("pie_race1", height = 250),
                             plotlyOutput("pie_gender1", height = 250),
                             plotlyOutput("pie_employee1", height = 250),
                             plotlyOutput("pie_ccfpatient1", height = 250),
                             plotlyOutput("bar_city1", height = 400),
                             plotlyOutput("bar_county1", height = 400),
                             plotlyOutput("age_histo1", height = 200)
                         ),
                         box(width=3,
                             title = p("Contacted",
                                       tippy(icon("question-circle"),
                                             tooltip = h6(paste0("Research Coordinators first phone call with potential participant."),
                                                          align = "left"),
                                             animation = "scale", 
                                             theme = "light")),
                             solidHeader = T,  
                             status = "primary",
                             buttonGroup(inputId = "date_type6",
                                         choices = c("All", "Only eligable"), 
                                         selected = "All", multiple= F), 
                             br(),
                             plotlyOutput("pie_race2", height = 250),
                             plotlyOutput("pie_gender2", height = 250),
                             plotlyOutput("pie_employee2", height = 250),
                             plotlyOutput("pie_ccfpatient2", height = 250),
                             plotlyOutput("bar_city2", height = 400),
                             plotlyOutput("bar_county2", height = 400),
                             plotlyOutput("age_histo2", height = 200)
                         ),
                         box(width=3, 
                             title=  p("Appointment call",
                                       tippy(icon("question-circle"),
                                             tooltip = h6(paste0("Return phone call to answer questions and schedule first visit to come in."),
                                                          align = "left"),
                                             animation = "scale", 
                                             theme = "light")),
                             solidHeader = T,  
                             status = "primary",
                             br(),
                             br(),
                             plotlyOutput("pie_race3", height = 250),
                             plotlyOutput("pie_gender3", height = 250),
                             plotlyOutput("pie_employee3", height = 250),
                             plotlyOutput("pie_ccfpatient3", height = 250),
                             plotlyOutput("bar_city3", height = 400),
                             plotlyOutput("bar_county3", height = 400),
                             plotlyOutput("age_histo3", height = 200)
                         ),
                         box(width=3,
                             title= p("Scheduled visit",
                                      tippy(icon("question-circle"),
                                            tooltip = h6(paste0("Participants are scheduled appointments with RCs."),
                                                         align = "left"),
                                            animation = "scale", 
                                            theme = "light")),
                             solidHeader = T,
                             status = "primary",
                             br(),
                             br(),
                             plotlyOutput("pie_race4", height = 250),
                             plotlyOutput("pie_gender4", height = 250),
                             plotlyOutput("pie_employee4", height = 250),
                             plotlyOutput("pie_ccfpatient4", height = 250),
                             plotlyOutput("bar_city4", height = 400),
                             plotlyOutput("bar_county4", height = 400),
                             plotlyOutput("age_histo4", height = 200)
                         )
                  )
              ))
    )
  )
)

server <- function(input, output) {
  
  #date_dummy currently replcaed by a dummy 
  date_dummy <- c(as.Date("2021-12-01"),current_day) #should become input$date_filter
  age_dummy <- c()
  race_dummy <- c()
  gender_dummy <-c()
  
  
  
  output$box_signup <- renderValueBox({
    
    
    past_values <- trend_time(data_mod.df,current_day,"sign_up_datetime")
    
    valueBox(
      nrow(data), 
      p(tags$p("Sign up", style = "font-size: 150%"),
        p(paste0("Yesterday: ", past_values[[1]]," (",past_values[[2]],"%)"),br(),
          paste0("Last 7 days: ", past_values[[3]]," (",past_values[[4]],"%)"),br(),
          paste0("Last 30 days: ", past_values[[5]]," (",past_values[[6]],"%)"))),
      icon = icon("sign-in"),
      color = "aqua"
    )
    
  })
  
  output$box_contact <- renderValueBox({
    
    value_input <- data_mod.df %>% 
      filter(whether_initial_contact == 1) %>% 
      nrow(.)
    
    
    past_values <- trend_time(data_mod.df,current_day,"initial_contact_date")
    
    valueBox(
      value_input, 
      p(tags$p("Contacted", style = "font-size: 150%"),
        p(paste0("Yesterday: ", past_values[[1]]," (",past_values[[2]],"%)"),br(),
          paste0("Last 7 days: ", past_values[[3]]," (",past_values[[4]],"%)"),br(),
          paste0("Last 30 days: ", past_values[[5]]," (",past_values[[6]],"%)"))),
      icon = icon("comment"),
      color = "light-blue"
    )
  })
  
  output$box_app_calls <- renderValueBox({
    
    value_input <- data_mod.df %>% 
      filter(whether_appointment_call == 1) %>%
      nrow(.)
    
    past_values <- trend_time(data_mod.df,current_day,"appointment_call_date")
    
    valueBox(
      value_input, 
      p(tags$p("Appointment calls", style = "font-size: 150%"),
        p(paste0("Yesterday: ", past_values[[1]]," (",past_values[[2]],"%)"),br(),
          paste0("Last 7 days: ", past_values[[3]]," (",past_values[[4]],"%)"),br(),
          paste0("Last 30 days: ", past_values[[5]]," (",past_values[[6]],"%)"))),
      icon = icon("phone"),
      color = "blue"
    )
  })
  
  # output$box_succ_app_calls <- renderValueBox({
  #   
  #   data_mod_sel.df <- data_mod.df %>%
  #     filter(!is.na(appointment_call_date),
  #            !is.na(schedule_made_datetime)) 
  #   
  #   value_input <- data_mod_sel.df %>% 
  #     nrow(.)
  #   
  #   past_values <- trend_time(data_mod_sel.df,current_day,"appointment_call_date")
  #   
  #   valueBox(
  #     value_input, 
  #     p(tags$p("Successful  calls", style = "font-size: 150%"),
  #       p(paste0("Yesterday: ", past_values[[1]]," (",past_values[[2]],"%)"),br(),
  #         paste0("Last 7 days: ", past_values[[3]]," (",past_values[[4]],"%)"),br(),
  #         paste0("Last month: ", past_values[[5]]," (",past_values[[6]],"%)"))),
  #     icon = icon("phone"),
  #     color = "blue"
  #   )
  # })
  
  output$box_visit_scheduled <- renderValueBox({
    
    value_input <- data_mod.df %>% 
      filter(whether_appointment_scheduled == 1) %>%
      nrow(.)
    
    
    past_values <- trend_time(data_mod.df,current_day,"schedule_made_datetime")
    
    valueBox(
      value_input, 
      p(tags$p("Scheduled visits", style = "font-size: 150%"),
        p(paste0("Yesterday: ", past_values[[1]]," (",past_values[[2]],"%)"),br(),
          paste0("Last 7 days: ", past_values[[3]]," (",past_values[[4]],"%)"),br(),
          paste0("Last 30 days: ", past_values[[5]]," (",past_values[[6]],"%)"))),
      icon = icon("calendar"),
      color = "navy"
    )
  })
  
  output$box_onboard <- renderValueBox({
    
    #not sure if this is the right way to get this number 
    value_input <- data_mod.df %>% 
      filter(whether_first_visit == 1) %>%
      nrow(.)
    
    
    past_values <- trend_time(data_mod.df,current_day,"visit_schedule_date")
    
    valueBox(
      value_input, 
      p(tags$p("Enrolled", style = "font-size: 150%"),
        p(paste0("Yesterday: ", past_values[[1]]," (",past_values[[2]],"%)"),br(),
          paste0("Last 7 days: ", past_values[[3]]," (",past_values[[4]],"%)"),br(),
          paste0("Last 30 days: ", past_values[[5]]," (",past_values[[6]],"%)"))),
      icon = icon("users"),
      color = "green"
    )
    
  })
  
  
  observeEvent(input$date_type1,{
    
    output$sign_up_bar <- renderPlotly({
      overviewplot_1(data_mod.df,"sign_up_datetime","Number of Sign-ups",input$date_type1,
                     age_dummy,date_dummy,race_dummy,gender_dummy)
      
    })
  })
  
  
  observeEvent(input$date_type2,{
    
    output$contact_bar <- renderPlotly({
      overviewplot_1(data_mod.df,"initial_contact_date","Number of Contacts",input$date_type2,
                     age_dummy,date_dummy,race_dummy,gender_dummy)
      
    })
  })
  
  
  change_app_output <- reactive({
    list(input$date_type3,input$date_type3_1)
  })
  
  
  observeEvent(change_app_output(),{
    
    
    #filters for people who had a call and have a visit scheduled 
    if(input$date_type3_1 == "Only sucessful calls"){
      
      data_mod_sel.df <- data_mod.df %>%
        filter(!is.na(appointment_call_date),
               !is.na(schedule_made_datetime)) 
      
      
    }else if(input$date_type3_1 == "All"){
      
      data_mod_sel.df <- data_mod.df
    }
    
    output$appointment_bar <- renderPlotly({
      overviewplot_1(data_mod_sel.df,"appointment_call_date","Number of Appointment calls",input$date_type3,
                     age_dummy,date_dummy,race_dummy,gender_dummy)
      
    })
    
  })
  
  observeEvent(input$date_type4,{
    
    output$visit_bar <- renderPlotly({
      overviewplot_1(data_mod.df,"schedule_made_datetime","Number of scheduled visits",input$date_type4,
                     age_dummy,date_dummy,race_dummy,gender_dummy)
      
    })
  })
  
  observeEvent(input$date_type4_1,{
    
    
    output$visit_future_bar <- renderPlotly({
      overviewplot_1(data_mod.df,"schedule_made_datetime","Number of scheduled visits",input$date_type4_1,
                     age_dummy,c(current_day,data_mod.df$schedule_made_datetime %>% max()),race_dummy,gender_dummy)
    })
  })
  
  observeEvent(input$date_type5,{
    
    output$onboarded_bar <- renderPlotly({
      
      overviewplot_1(data_mod.df,"visit_schedule_date","Number of enrolled participants",input$date_type5,
                     age_dummy, date_dummy,race_dummy,gender_dummy)
      
    })
  })
  
  observeEvent(input$date_type5_1,{
    
    output$future_visits <- renderPlotly({
      
      overviewplot_1(data_mod.df,"visit_schedule_date","Number of enrolled participants",input$date_type5_1,
                     age_dummy, c(current_day,as.Date(data_mod.df$visit_schedule_date %>% max(na.rm = T))),race_dummy,gender_dummy)
      
    })
  })
  
  
  #SIGN UP#
  #demographics sign up date
  output$pie_race1 <- renderPlotly({
    #demographics_pie(data_mod.df,"race_combine","sign_up_datetime","Race",age_dummy,date_dummy,race_dummy,gender_dummy)
    demographics_pie(data_mod.df,"race_combine","whether_sign_up","Race",age_dummy,date_dummy,race_dummy,gender_dummy)
  })
  
  output$pie_gender1 <- renderPlotly({
    #demographics_pie(data_mod.df,"sex_combine","sign_up_datetime","Sex",age_dummy,date_dummy,race_dummy,gender_dummy)
    demographics_pie(data_mod.df,"sex_combine","whether_sign_up","Sex",age_dummy,date_dummy,race_dummy,gender_dummy)
  })
  
  output$pie_employee1 <- renderPlotly({
    demographics_pie(data_mod.df,"employ_status_desc","whether_sign_up","Employee status",age_dummy,date_dummy,race_dummy,gender_dummy)
  })
  
  output$pie_ccfpatient1 <- renderPlotly({
    demographics_pie(data_mod.df,"ccf_patient","whether_sign_up","CCF patient",age_dummy,date_dummy,race_dummy,gender_dummy)
  })
  
  output$bar_city1 <- renderPlotly({
    city_barplot(data_mod.df,"pat_city","whether_sign_up","Top 20 Cities",age_dummy,date_dummy,race_dummy,gender_dummy)
  })
  
  output$bar_county1 <- renderPlotly({
    city_barplot(data_mod.df,"pat_county","whether_sign_up","Top 20 Counties",age_dummy,date_dummy,race_dummy,gender_dummy)
  })
  
  output$age_histo1 <- renderPlotly({
    age_histogram(data_mod.df,"age_combine","whether_sign_up","Age distribution",age_dummy,date_dummy,race_dummy,gender_dummy)
  })
  
  ##preferred days 
  output$preferred_days <- renderPlotly({
    
    preferred_days(age_dummy,date_dummy,race_dummy,gender_dummy)
    
  })
  #daytime
  
  output$preferred_daytime <- renderPlotly({
    
    preferred_time(age_dummy,date_dummy,race_dummy,gender_dummy)
    
  })
  
  #CONTACTS#
  ###demographics contacts###
  observeEvent(input$date_type6,{
    
    if(input$date_type6 != "All"){
      
      data_mod_sel.df <- data_mod.df %>% 
        filter(whether_eligible == 1)
      
      
    }else{
      
      data_mod_sel.df <- data_mod.df
    }
    
    output$pie_race2 <- renderPlotly({
      #demographics_pie(data_mod_sel.df,"race_combine","initial_contact_date","Race",age_dummy,date_dummy,race_dummy,gender_dummy)
      demographics_pie(data_mod_sel.df,"race_combine","whether_initial_contact","Race",age_dummy,date_dummy,race_dummy,gender_dummy)
    })
    
    output$pie_gender2 <- renderPlotly({
      demographics_pie(data_mod_sel.df,"sex_combine","whether_initial_contact","Sex",age_dummy,date_dummy,race_dummy,gender_dummy)
    })
    
    output$pie_employee2 <- renderPlotly({
      demographics_pie(data_mod_sel.df,"employ_status_desc","whether_initial_contact","Employee status",age_dummy,date_dummy,race_dummy,gender_dummy)
    })
    
    output$pie_ccfpatient2 <- renderPlotly({
      demographics_pie(data_mod_sel.df,"ccf_patient","whether_initial_contact","CCF patient",age_dummy,date_dummy,race_dummy,gender_dummy)
    })
    
    output$bar_city2 <- renderPlotly({
      city_barplot(data_mod_sel.df,"pat_city","whether_initial_contact","Top 20 Cities",age_dummy,date_dummy,race_dummy,gender_dummy)
    })
    
    output$bar_county2 <- renderPlotly({
      city_barplot(data_mod_sel.df,"pat_county","whether_initial_contact","Top 20 Counties",age_dummy,date_dummy,race_dummy,gender_dummy)
    })
    
    output$age_histo2 <- renderPlotly({
      age_histogram(data_mod_sel.df,"age_combine","whether_initial_contact","Age distribution",age_dummy,date_dummy,race_dummy,gender_dummy)
    })
    
    #call atempts 
    output$pie_call_attempts <- renderPlotly({
      demographics_pie(data_mod.df,"number_initial_contact_attempt","whether_initial_contact","Call attempts",age_dummy,date_dummy,race_dummy,gender_dummy)
      # output$pie_call_attempts <- renderPlotly({
      #   demographics_pie(data_mod.df,"number_initial_contact_attempt","appointment_call_date","Call attempts",age_dummy,date_dummy,race_dummy,gender_dummy)
    })
    
    #eligible
    output$pie_whether_eligible <- renderPlotly({
      demographics_pie(data_mod.df,"whether_eligible_yn","whether_initial_contact","Whether eligible",age_dummy,date_dummy,race_dummy,gender_dummy)
    })
    
    #ms relative
    output$pie_whether_ms_relative <- renderPlotly({
      demographics_pie(data_mod.df,"whether_ms_relative","whether_initial_contact","Whether MS relative",age_dummy,date_dummy,race_dummy,gender_dummy)
    })
    
    output$ms_relative_table <- DT::renderDataTable({
      
      data_mod.df <- data_mod.df %>% 
        # mutate(age_combine1 = age_combine,
        #        race_combine1 = race_combine,
        #        sex_combine1 = sex_combine) %>%
        filter(#initial_contact_date >= date_dummy[1] & initial_contact_date <= date_dummy[2],
          #age_combine1 >= age_dummy[1] & age_combine <= age_dummy[2],
          #race_combine1 %in% race_dummy,
          #sex_combine1 %in% gender_dummy,
          whether_ms_relative == "yes") %>%  
        select(record_id,sex_combine,age_combine, race_combine, step,study_id, sign_up_datetime, visit_schedule_date)%>%  
        setNames(c("Record ID", "Gender", "Age", "Race", "Enrollment step", "Study ID", "Sign up", "Scheduled first visit"))
      
      data_mod.df
      
    }, escape = FALSE, 
    filter = 'top',
    rownames = FALSE,
    options = list(paging = FALSE, scrollY= "250px", scrollCollapse = TRUE, dom = 't',
                   columnDefs = list(list(className = 'dt-center', targets = "_all"))
    ))
    
    #saturday mri
    output$pie_saturday_mri <- renderPlotly({
      demographics_pie(data_mod.df,"saturday_mri_yn","whether_initial_contact","Whether saturday MRI", age_dummy,date_dummy,race_dummy,gender_dummy)
    })
    
  })
  
  #APPOINTMENTS#
  ###demographics appointment###
  output$pie_race3 <- renderPlotly({
    demographics_pie(data_mod.df,"race_combine","whether_appointment_call","Race",age_dummy,date_dummy,race_dummy,gender_dummy)
    
    # demographics_pie(data_mod.df,"race_combine","appointment_call_date","Race",age_dummy,date_dummy,race_dummy,gender_dummy)
  })
  
  output$pie_gender3 <- renderPlotly({
    demographics_pie(data_mod.df,"sex_combine","whether_appointment_call","Sex",age_dummy,date_dummy,race_dummy,gender_dummy)
  })
  
  output$pie_employee3 <- renderPlotly({
    demographics_pie(data_mod.df,"employ_status_desc","whether_appointment_call","Employee status",age_dummy,date_dummy,race_dummy,gender_dummy)
  })
  
  output$pie_ccfpatient3 <- renderPlotly({
    demographics_pie(data_mod.df,"ccf_patient","whether_appointment_call","CCF patient",age_dummy,date_dummy,race_dummy,gender_dummy)
  })
  
  output$bar_city3 <- renderPlotly({
    city_barplot(data_mod.df,"pat_city","whether_appointment_call","Top 20 Cities",age_dummy,date_dummy,race_dummy,gender_dummy)
  })
  
  output$bar_county3 <- renderPlotly({
    city_barplot(data_mod.df,"pat_county","whether_appointment_call","Top 20 Counties",age_dummy,date_dummy,race_dummy,gender_dummy)
  })
  
  output$age_histo3 <- renderPlotly({
    age_histogram(data_mod.df,"age_combine","whether_appointment_call","Age distribution",age_dummy,date_dummy,race_dummy,gender_dummy)
  })
  
  #SCHEDULED VISITS#
  ###demographics scheduled visits###
  output$pie_race4 <- renderPlotly({
    
    demographics_pie(data_mod.df,"race_combine","whether_appointment_scheduled","Race",age_dummy,date_dummy,race_dummy,gender_dummy)
    
    # demographics_pie(data_mod.df,"race_combine","schedule_made_datetime","Race",age_dummy,date_dummy,race_dummy,gender_dummy)
  })
  
  output$pie_gender4 <- renderPlotly({
    demographics_pie(data_mod.df,"sex_combine","whether_appointment_scheduled","Sex",age_dummy,date_dummy,race_dummy,gender_dummy)
  })
  
  output$pie_employee4 <- renderPlotly({
    demographics_pie(data_mod.df,"employ_status_desc","whether_appointment_scheduled","Employee status",age_dummy,date_dummy,race_dummy,gender_dummy)
  })
  
  output$pie_ccfpatient4 <- renderPlotly({
    demographics_pie(data_mod.df,"ccf_patient","whether_appointment_scheduled","CCF patient",age_dummy,date_dummy,race_dummy,gender_dummy)
  })
  
  output$bar_city4 <- renderPlotly({
    city_barplot(data_mod.df,"pat_city","whether_appointment_scheduled","Top 20 Cities",age_dummy,date_dummy,race_dummy,gender_dummy)
  })
  
  output$bar_county4 <- renderPlotly({
    city_barplot(data_mod.df,"pat_county","whether_appointment_scheduled","Top 20 Counties",age_dummy,date_dummy,race_dummy,gender_dummy)
  })
  
  output$age_histo4 <- renderPlotly({
    age_histogram(data_mod.df,"age_combine","whether_appointment_scheduled","Age distribution",age_dummy,date_dummy,race_dummy,gender_dummy)
  })
  
  #ONBOARDED #
  ###demographics of onboarded participants###
  output$pie_race5 <- renderPlotly({
    
    # demographics_pie(data_mod.df,"race_combine","first_visit_date","Race",age_dummy,date_dummy,race_dummy,gender_dummy)
    
    demographics_pie(data_mod.df,"race_combine","whether_first_visit","Race",age_dummy,date_dummy,race_dummy,gender_dummy)
  })
  
  output$pie_gender5 <- renderPlotly({
    
    demographics_pie(data_mod.df,"sex_combine","whether_first_visit","Sex",age_dummy,date_dummy,race_dummy,gender_dummy)
  })
  
  output$pie_employee5 <- renderPlotly({
    
    demographics_pie(data_mod.df,"employ_status_desc","whether_first_visit","Employee status",age_dummy,date_dummy,race_dummy,gender_dummy)
  })
  
  output$pie_ccfpatient5 <- renderPlotly({
    
    demographics_pie(data_mod.df,"ccf_patient","whether_first_visit","CCF patient",age_dummy,date_dummy,race_dummy,gender_dummy)
  })
  
  output$bar_city5 <- renderPlotly({
    
    city_barplot(data_mod.df,"pat_city","whether_first_visit","Top 20 Cities",age_dummy,date_dummy,race_dummy,gender_dummy)
  })
  
  output$bar_county5 <- renderPlotly({
    
    city_barplot(data_mod.df,"pat_county","whether_first_visit","Top 20 Counties",age_dummy,date_dummy,race_dummy,gender_dummy)
  })
  
  output$age_histo5 <- renderPlotly({
    
    age_histogram(data_mod.df,"age_combine","whether_first_visit","Age distribution",age_dummy,date_dummy,race_dummy,gender_dummy)
  })
  
}

shinyApp(ui, server)










