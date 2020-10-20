library(shiny)
library(tidyverse)
library(drat)

# use the dataset strawberries
sberry <- read.csv("strawberries.csv")
df <- filter(sberry, 
             is.na(Value) == "FALSE"
             )


# Define UI for application that draws plots and create a table
ui=fluidPage(
    
    # Application title
    titlePanel('Strawberry Data from USDA'),
    
    # Sidebar with several inputs
    sidebarLayout(
        sidebarPanel(
            selectInput('chemical','Chemical', choices=sort(unique(df$Chemical))), 
            uiOutput('ui_materials'),
            uiOutput('ui_measures'),
            uiOutput('ui_state'),
            uiOutput('ui_year')
            ),
        mainPanel(
            # h3('Plot of values'), 
            # plotOutput('p_values'), 
            h3('Data selected'), 
            dataTableOutput('t_data')
            )
    )
)

server=function(input,output){
    
    # Interactive inputs
    ## interactive input of materials  
    s_materials=reactive({
        sort(unique(df$Materials[df$Chemical %in% input$chemical]))
    })
    
    output$ui_materials=renderUI({
        d_selected = ifelse('(TOTAL)' %in% s_materials(), '(TOTAL)', NA)
        selectInput('materials', 'Materials', s_materials(), d_selected)
    })

    ## interactive input of measures
    s_measures=reactive({
        sort(unique(df$Measures[df$Chemical %in% input$chemical 
                                & df$Materials %in% input$materials]))
    })
    output$ui_measures=renderUI({
        checkboxGroupInput('measures','Measures',s_measures(),s_measures()[1])
    })

    ## interactive input of state
    s_state=reactive({
        sort(unique(df$State[df$Chemical %in% input$chemical 
                             & df$Materials %in% input$materials 
                             & df$Measures %in% input$measures]))
    })
    output$ui_state=renderUI({
        checkboxGroupInput('state','State',s_state(),s_state()[1])
    })

    ## interactive input of year
    s_year=reactive({
        sort(unique(df$Year[df$Chemical%in%input$chemical
                                 &df$Materials%in%input$materials
                                 &df$Measures%in%input$measures
                                 &df$State%in%input$state]))
    })
    output$ui_year=renderUI({
        checkboxGroupInput('year','Year',s_year(),s_year())
    })
    
    # Data used to plot
    p_df=reactive({
        filter(df,
               Chemical%in%input$chemical,
               Materials%in%input$materials,
               Measures%in%input$measures,
               State%in%input$state,
               Year%in%input$year) %>% select(Year,State,Measures,Value)
    })
    
    # Export data frame and plot
    output$t_data=renderDataTable(p_df())
}

# Run the application 
shinyApp(ui = ui, server = server)
