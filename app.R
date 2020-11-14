# A simple app for viewing up-to-date data on COVID19 cases in your county

#John J Davis IV
#Biomechanics PhD Student
#Indiana University School of Public Health
#6 July 2020

library(shiny)
library(tidyverse)
library(zoo)
library(cowplot)

#Read data directly from New York Times' GitHub (updates ~daily)
covid_county_raw <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

all_states <- sort(unique(covid_county_raw$state))

# Define UI for application
ui <- fluidPage(titlePanel("County-level COVID-19 tracker"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("my_state", label = "State: ",
                                choices = all_states),
                    uiOutput("state_select"),
                    p("Data sourced from",
                    a("New York Times COVID19 repository", 
                      href = "https://github.com/nytimes/covid-19-data")),
                    p("App created by ", a("John Davis", 
                                           href = "https://twitter.com/JDruns")),
                    p("Code available ", 
                      a("on GitHub", href = "https://github.com/johnjdavisiv/covid-county-app"))
                    ),
                  mainPanel(plotOutput("covid_plot"),
                            p("Black points indiciate daily new case counts.", 
                              span("Red line", style="color:red"), 
                              " shows average daily case count over the past seven days. Note that this moving average will necessarily lag a bit behind the 'true' daily case count! Be patient - the plot takes a few moments to update."))
                )
)

# Define server logic
server <- function(input, output) {
    list_all_counties <- reactive(
        {
            covid_county_raw %>% 
            filter(state == input$my_state) %>%
            pull(county) %>% 
            unique() %>%
            sort()
        }
    )
    
    output$state_select <- renderUI({
        selectizeInput('my_county', 'Select county', 
                       choices = list_all_counties())
    })
    
    do_rx_plot <- reactive({
      #req() prevents the function from drawing anything before state/county are chosen
        req(input$my_state, input$my_county)
        
        county_df <- covid_county_raw %>%
            mutate(date = as.Date(date)) %>%
            filter(state == input$my_state, county == input$my_county) %>%
            mutate(new_cases = cases - dplyr::lag(cases)) %>% #new cases is diff over a 1-day lag
            mutate(new_cases_7d_avg = rollmean(new_cases, k=7, na.pad=TRUE, align="right")) %>%
            mutate(new_deaths = deaths - dplyr::lag(deaths)) %>%
            mutate(new_deaths_7d_avg = rollmean(new_deaths, k=7, na.pad=TRUE, align="right")) %>%
            mutate(new_cases = ifelse(new_cases < 0, 0, new_cases)) %>% #Set negative days to zero
            filter(!is.na(new_cases_7d_avg))
        
        plt_cases <- county_df %>%
          ggplot(aes(x=date, y=new_cases)) + 
          geom_point(size=2) + 
          geom_line(aes(y=new_cases_7d_avg), 
                    color="#00A5FF", size=2, alpha = 0.5) + 
            labs(x="Date", y="Daily new cases", 
                 title=sprintf("COVID-19 cases in %s County, %s", input$my_county, input$my_state)) + 
            theme(plot.title = element_text(hjust = 0.5),
                  text = element_text(size=16))
        
        plt_deaths <- county_df %>%
          ggplot(aes(x=date, y=new_deaths)) + 
          geom_point(size=2) + 
          geom_line(aes(y=new_deaths_7d_avg), 
                    color="red", size=2, alpha = 0.5) + 
          labs(x="Date", y="Daily new deaths", 
               title=sprintf("COVID-19 deaths in %s County, %s", input$my_county, input$my_state)) + 
          theme(plot.title = element_text(hjust = 0.5),
                text = element_text(size=16))
        
        plot_grid(plt_cases, plt_deaths, ncol=1)
        
    })
    

    #Might be clunky, but hey this is my first Shiny app
    output$covid_plot <- renderPlot({
        
        do_rx_plot()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
