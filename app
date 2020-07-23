library(shiny)
library(plotly)
require(shinydashboard)
library(dplyr)
library(DT)
library(scales)
library(highcharter)

#import data set
df_all <- read.csv("df_all.csv", stringsAsFactors = F, header = T)
macro <- read.csv("macro_brands.csv", stringsAsFactors = F, header = T)
macro_c <- read.csv("macro_brands_c.csv", stringsAsFactors = F, header = T)

#import data set
df_bbw_products <- read.csv("df_bbw_products.csv", stringsAsFactors = F, header = T)
df_bbw_products_14 <- read.csv("df_bbw_products_14.csv", stringsAsFactors = F, header = T)

#import data set
bbw_ksa_cvr <- read.csv("bbw_ksa_cvr.csv", stringsAsFactors = F, header = T)
bbw_uae_cvr <- read.csv("bbw_uae_cvr.csv", stringsAsFactors = F, header = T)
forecast <- read.csv("f_bbw.csv", stringsAsFactors = F, header = T)
inner_ga <- read.csv("iga.csv", stringsAsFactors = F, header = T)
inner_forecast <- read.csv("inner_forecast.csv", stringsAsFactors = F, header = T)
df1 <- read.csv("df1.csv", stringsAsFactors = F, header = T)
funnel_historical <- read.csv("funnel_historical.csv", stringsAsFactors = F, header = T)
funnel7 <- read.csv("funnel7.csv", stringsAsFactors = F, header = T)
funnelt <- read.csv("funnelt.csv", stringsAsFactors = F, header = T)


#import product PLPs
product <- read.csv("pr2.csv", stringsAsFactors = F, header = T)
product_60 <- read.csv("bb2.csv", stringsAsFactors = F, header = T)


### import more DF
df_cpo <- read.csv("df_cpo.csv", stringsAsFactors = F, header = T)
df_cpo_yesterday <- read.csv("df_cpo_yesterday.csv", stringsAsFactors = F, header = T)
cir_target <- read.csv("cir_target.csv", stringsAsFactors = F, header = T)
top_cir <- read.csv("cir_top.csv", stringsAsFactors = F, header = T)


### GA sessions and trends
df_sessions_i <- read.csv("df_sessions_i.csv", stringsAsFactors = F, header = T)
channel <- read.csv("organic.csv", stringsAsFactors = F, header = T)

### More DFs
ga_st <- read.csv("ga_st.csv", stringsAsFactors = F, header = T)


#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Monster Dashboard")

#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Macro", tabName = "macro", icon = icon("dashboard")),
    menuItem("Dashboard", tabName = "dashboard", icon = icon("globe")),
    menuItem("Paid", tabName = "paid", icon = icon("usd")),
    menuItem("EDA", tabName = "eda", icon = icon("usd")),
    menuItem("Grouping", tabName = "gtrends", icon = icon("usd")),
    menuItem("Products", tabName = "products", icon = icon("wrench")),
    menuItem("products_ga", tabName = "products_ga", icon = icon("wrench")),
    menuItem("Forecast Per Day", tabName = "per_day", icon = icon("road")),
    menuItem("forecast Per hour", tabName = "forecast", icon = icon("road")),
    menuItem("ga_data", tabName = "ga_data", icon = icon("signal")),
    menuItem("funnel", tabName = "funnel", icon = icon("signal")),
    menuItem("gaussian", tabName = "gaussian", icon = icon("search")),
    menuItem("outliers", tabName = "outliers", icon = icon("search")),
    menuItem("mediax", tabName = "mediax", icon = icon("headphones")),
    menuItem("geeky", tabName = "simulator", icon = icon("headphones")),
    menuItem("performance marketing", tabName = "", icon = icon("heart"))
  )
)


body <- dashboardBody(
  
  
  ## 3.1 Dashboard body --------------
  tabItems(
    
    
    ## 3.0 Time Series ----------------------------------------------------------
    tabItem(tabName = 'macro',
            ## contents for the dashboard tab
            
            fluidPage(
              selectizeInput(
                inputId = "brand",
                label = "brand",
                choices = unique(df_all$brand),
                selected = "bbw",
                multiple = TRUE
              ),
              
              
              plotlyOutput(outputId = "m"),
              plotlyOutput(outputId = "m1"),
              plotlyOutput(outputId = "m2"),
              plotlyOutput(outputId = "m3"),
              plotlyOutput(outputId = "m4"),
              plotlyOutput(outputId = "m5"),
              plotlyOutput(outputId = "m6")
            )),
    
    
    ## 3.1 Time Series ----------------------------------------------------------
    tabItem(tabName = 'dashboard',
            ## contents for the dashboard tab
            
            fluidPage(
              selectizeInput(
                inputId = "brand_country",
                label = "Select a brand and country",
                choices = unique(df_all$brand_country),
                selected = "BB_UAE",
                multiple = TRUE
              ),
              
              
              plotlyOutput(outputId = "p"),
              plotlyOutput(outputId = "p1"),
              plotlyOutput(outputId = "p2"),
              plotlyOutput(outputId = "p3"),
              plotlyOutput(outputId = "p4"),
              plotlyOutput(outputId = "p5"),
              plotlyOutput(outputId = "p6")
            )),
    
    
    ## 3.2 Paid Performance ----------------------------------------------------------
    tabItem(tabName = 'paid',
            ## contents for the dashboard tab
            
            fluidPage(
              selectizeInput(
                inputId = "paid",
                label = "select brand",
                choices = unique(df_all$brand_country),
                selected = "BB_UAE",
                multiple = TRUE
              ),
              
              
              plotlyOutput(outputId = "paid1"),
              plotlyOutput(outputId = "paid2"),
              plotlyOutput(outputId = "paid3"),
              plotlyOutput(outputId = "paid4"),
              plotlyOutput(outputId = "paid5"),
              plotlyOutput(outputId = "paid6")
            )),
    
    
    ## 3.3 EDA Performance ----------------------------------------------------------
    tabItem(tabName = 'eda',
            ## contents for the dashboard tab
            
            fluidPage(
              selectizeInput(
                inputId = "eda",
                label = "select brand",
                choices = unique(df_all$brand_country),
                selected = "BB_UAE",
                multiple = TRUE
              ),
              
              
              plotlyOutput(outputId = "eda1"),
              plotlyOutput(outputId = "eda2"),
              plotlyOutput(outputId = "eda3")
              
              
            )),
    
    
    ## 3.6 Forecast Performance ----------------------------------------------------------
    tabItem(tabName = 'per_day',
            ## contents for the dashboard tab
            
            fluidPage(
              selectizeInput(
                inputId = "per_day",
                label = "select brand",
                choices = unique(inner_forecast$brand_country),
                selected = "bbw_ksa",
                multiple = TRUE
              ),
              
              
              plotlyOutput(outputId = "vv3")
              
              
            )),
    
    
    ## 3.6 Forecast Performance ----------------------------------------------------------
    tabItem(tabName = 'forecast',
            ## contents for the dashboard tab
            
            fluidPage(
              selectizeInput(
                inputId = "forecast",
                label = "select brand",
                choices = unique(forecast$brand_country),
                selected = "hm_ksa",
                multiple = TRUE
              ),
              
              
              plotlyOutput(outputId = "pp1")
              
              
            )),
    
    
    ## 4 Funnel Performance ----------------------------------------------------------
    tabItem(tabName = 'funnel',
            ## contents for the dashboard tab
            
            fluidPage(
              selectizeInput(
                inputId = "funnel_historical",
                label = "select brand funnel historical",
                choices = unique(funnel_historical$brand_country),
                selected = "bbw_ksa",
                multiple = FALSE
              ),
              
              
              selectInput(inputId = 'funnel7',
                          label = 'select brand funnel 7 days',
                          choices = unique(funnel7$brand_country),
                          selected = "bbw_ksa",
              ),
              
              
              selectInput(inputId = 'funnelt',
                          label = 'select brand funnel today',
                          choices = unique(funnelt$brand_country),
                          selected = "bbw_ksa",
              ),
              
              
              plotlyOutput(outputId = "fu1"),
              plotlyOutput(outputId = "fu2"),
              plotlyOutput(outputId = "fu3")
              
              
            )),
    
    
    ## 3.7 GA Sessions x Transactions ----------------------------------------------------------
    tabItem(tabName = 'ga_data',
            ## contents for the dashboard tab
            
            fluidPage(
              selectizeInput(
                inputId = "ga_data",
                label = "select brand",
                choices = unique(inner_ga$brand_country),
                selected = "hm_ksa",
                multiple = TRUE
              ),
              
              
              plotlyOutput(outputId = "vv2")
              
              
            )),
    
    
    ## 6.4 Gaussian ----------------------------------------------------------
    tabItem(tabName = 'gaussian',
            ## contents for the dashboard tab
            
            fluidPage(
              selectizeInput(
                inputId = "top_cir",
                label = "select brand",
                choices = unique(top_cir$brand_country),
                selected = "BB_UAE",
                multiple = TRUE
              ),
              
              
              plotlyOutput(outputId = "topcir")
              
              
            )),
    
    
    ## 6.4 GtTrends ----------------------------------------------------------
    tabItem(tabName = 'gtrends',
            ## contents for the dashboard tab
            
            fluidPage(
              selectizeInput(
                inputId = "brand_name",
                label = "select brand",
                choices = unique(df_sessions_i$brand_name),
                selected = "bb_ksa",
                multiple = TRUE),
              
              
              plotlyOutput(outputId = "gt1")
              
              
            ),
            
            fluidPage(
              selectizeInput(
                inputId = "channel",
                label = "select brand",
                choices = unique(channel$brand_name),
                selected = "hm_uae",
                multiple = TRUE),
              
              
              plotlyOutput(outputId = "gt2")
              
              
            ),
            
            
    ),
    
    
    ## 6 Outliers CPO Kable ----------------------------------------------------------
    tabItem(tabName = 'outliers',
            navbarPage("Outliers",
                       navbarMenu("Last week",
                                  tabPanel("Table",
                                           DT::dataTableOutput("outliers"),
                                           fluidRow(
                                             valueBoxOutput("total_spend"),
                                             valueBoxOutput("cir")),
                                  )),
                       
                       
                       navbarMenu("yesterday",
                                  tabPanel("Table",
                                           DT::dataTableOutput("outliersy"),
                                           fluidRow(
                                             valueBoxOutput("total_spendy"),
                                             valueBoxOutput("ciry")),
                                           
                                           
                                  )))),
    
    
    ## Date Range Plot ----------------------------------------------------------
    tabItem(tabName = 'mediax',
            
            fluidPage(
              titlePanel("The Troubleshooter"),
              sidebarLayout(
                sidebarPanel(
                  selectizeInput(
                    inputId = "ga_brand_country", label = "Select a brand market",
                    choices = ga_st$brand_country, selected = "hm_uae", multiple = TRUE
                  ),
                  dateRangeInput(inputId = "myDateRange", label = "", start = Sys.Date() - 8, end = Sys.Date() - 2, min = NULL, max = NULL)
                ),
                mainPanel(
                  plotlyOutput("ga_st_bars")
                )
              )
            ),
            
            DT::dataTableOutput("daysFrequency")
            
    ),
    
    ## Products GA Sessions ----------------------------------------------------------
    tabItem(tabName = 'products_ga',
            ## contents for the dashboard tab
            
            
            selectizeInput(
              inputId = "product",
              label = "select brand",
              choices = unique(product$brand_country),
              selected = "bbw_ksa",
              multiple = TRUE),
            
            fluidRow(
              box(title = "Top 50",
                  solidHeader = T,
                  width = 12,
                  collapsible = T,
                  collapsed = F,
                  
                  plotlyOutput("pro"))
              
            ),
            
            selectizeInput(
              inputId = "product_60",
              label = "select brand",
              choices = unique(product_60$brand_country),
              selected = "bbw_ksa",
              multiple = TRUE),
            
            fluidRow(
              box(title = "Top 50",
                  solidHeader = T,
                  width = 12,
                  collapsible = T,
                  collapsed = F,
                  
                  plotlyOutput("product60"))
              
            ),
            
            
    ),
    
    
    ## 3.8 Forecast per Day ----------------------------------------------------------
    tabItem(tabName = 'simulator',
            ## contents for the dashboard tab
            
            fluidPage(
              titlePanel("Totally Cool Useless Stuff
                                                  
                                                  
#            .-.
#           |o,o|"),
              
              
              plotlyOutput(outputId = "sim")
              
            )),
    
    
    ## 3.5 products -----------------------------------------------------
    tabItem(tabName = 'products',
            ## 3.3.1 Help text first --------------
            fluidPage(
              selectizeInput(
                inputId = "store_id",
                label = "Select a market",
                choices = unique(df_bbw_products$store_id),
                selected = "15",
                multiple = TRUE
              ),
              
              plotlyOutput(outputId = "d"),
              plotlyOutput(outputId = "d7")
              
              
            ))))


ui <- dashboardPage(title = 'Monster', header, sidebar, body, skin = 'black')

server <- function(input, output, ...) {
  
  ###
  ### Brand Macro
  ####
  
  
  output$m <- renderPlotly({
    
    plot_ly(macro, x = ~date, y = ~gross_revenue, name = "gross_revenue") %>%
      filter(brand %in% input$brand) %>%
      group_by(brand) %>%
      add_lines()
    
    
  })
  
  output$m1 <- renderPlotly({
    
    plot_ly(macro, x = ~date, y = ~gross_margin, name = "gross_margin") %>%
      filter(brand %in% input$brand) %>%
      group_by(brand) %>%
      add_lines()
    
    
  })
  
  output$m2 <- renderPlotly({
    
    plot_ly(macro, x = ~date, y = ~gm_per_cent, name = "gm_per_cent") %>%
      filter(brand %in% input$brand) %>%
      group_by(brand) %>%
      add_lines()
    
    
  })
  
  
  output$m3 <- renderPlotly({
    
    plot_ly(macro, x = ~date, y = ~orders, name = "orders") %>%
      filter(brand %in% input$brand) %>%
      group_by(brand) %>%
      add_lines()
    
    
  })
  
  
  output$m4 <- renderPlotly({
    
    plot_ly(macro, x = ~date, y = ~session, name = "session") %>%
      filter(brand %in% input$brand) %>%
      group_by(brand) %>%
      add_lines()
    
    
  })
  
  
  output$m5 <- renderPlotly({
    
    plot_ly(macro, x = ~date, y = ~CvR, name = "CvR") %>%
      filter(brand %in% input$brand) %>%
      group_by(brand) %>%
      add_lines()
    
    
  })
  
  
  output$m6 <- renderPlotly({
    
    plot_ly(macro, x = ~date, y = ~CiR, name = "CiR") %>%
      filter(brand %in% input$brand) %>%
      group_by(brand) %>%
      add_lines()
    
    
  })
  
  
  ###
  ### Brand and Market
  ####
  
  output$p <- renderPlotly({
    
    plot_ly(df_all, x = ~date, y = ~gross_revenue, name = "gross_revenue") %>%
      filter(brand_country %in% input$brand_country) %>%
      group_by(brand_country) %>%
      add_lines()
    
    
  })
  
  output$p1 <- renderPlotly({
    
    plot_ly(df_all, x = ~date, y = ~gross_margin, name = "gross_margin") %>%
      filter(brand_country %in% input$brand_country) %>%
      group_by(brand_country) %>%
      add_lines()
    
    
  })
  
  output$p2 <- renderPlotly({
    
    plot_ly(df_all, x = ~date, y = ~gm_per_cent, name = "gm_per_cent") %>%
      filter(brand_country %in% input$brand_country) %>%
      group_by(brand_country) %>%
      add_lines()
    
    
  })
  
  
  output$p3 <- renderPlotly({
    
    plot_ly(df_all, x = ~date, y = ~orders, name = "orders") %>%
      filter(brand_country %in% input$brand_country) %>%
      group_by(brand_country) %>%
      add_lines()
    
    
  })
  
  
  output$p4 <- renderPlotly({
    
    plot_ly(df_all, x = ~date, y = ~session, name = "session") %>%
      filter(brand_country %in% input$brand_country) %>%
      group_by(brand_country) %>%
      add_lines()
    
    
  })
  
  
  output$p5 <- renderPlotly({
    
    plot_ly(df_all, x = ~date, y = ~CvR, name = "CvR") %>%
      filter(brand_country %in% input$brand_country) %>%
      group_by(brand_country) %>%
      add_lines()
    
    
  })
  
  
  output$p6 <- renderPlotly({
    
    plot_ly(macro_c, x = ~date, y = ~CiR, name = "CiR") %>%
      filter(brand_country %in% input$brand_country) %>%
      group_by(brand_country) %>%
      add_lines()
    
    
  })
  
  
  ###
  #### Paid Dashboard Render
  ####
  
  output$paid1 <- renderPlotly({
    
    plot_ly(df_all, x = ~date, y = ~facebook_impressions, name = "facebook impressions") %>%
      filter(brand_country %in% input$paid) %>%
      group_by(brand_country) %>%
      add_lines()
    
    
  })
  
  
  output$paid2 <- renderPlotly({
    
    plot_ly(df_all, x = ~date, y = ~facebook_sessions, name = "facebook sessions") %>%
      filter(brand_country %in% input$paid) %>%
      group_by(brand_country) %>%
      add_lines()
    
    
  })
  
  
  output$paid3 <- renderPlotly({
    
    plot_ly(df_all, x = ~date, y = ~orders, name = "orders") %>%
      filter(brand_country %in% input$paid) %>%
      group_by(brand_country) %>%
      add_lines()
    
    
  })
  
  
  output$paid4 <- renderPlotly({
    
    plot_ly(df_all, x = ~date, y = ~adwords_impressions, name = "adwords impressions") %>%
      filter(brand_country %in% input$paid) %>%
      group_by(brand_country) %>%
      add_lines()
    
    
  })
  
  
  output$paid5 <- renderPlotly({
    
    plot_ly(df_all, x = ~date, y = ~criteo_impressions, name = "criteo impressions") %>%
      filter(brand_country %in% input$paid) %>%
      group_by(brand_country) %>%
      add_lines()
    
    
  })
  
  
  output$paid6 <- renderPlotly({
    
    plot_ly(df_all, x = ~date, y = ~partnerize_sessions, name = "Partnerize Sessions") %>%
      filter(brand_country %in% input$paid) %>%
      group_by(brand_country) %>%
      add_lines()
    
    
  })
  
  
  ####
  #### EDA Render
  ####
  
  output$eda1 <- renderPlotly({
    
    plot_ly(df_all, x = ~date, y = ~prospecting_imp_new, name = "PSP impressions") %>%
      filter(brand_country %in% input$eda) %>%
      group_by(brand_country) %>%
      add_lines()
    
    
  })
  
  
  output$eda2 <- renderPlotly({
    
    plot_ly(df_all, x = ~date, y = ~remarketing_imp_new, name = "RTG impressions") %>%
      filter(brand_country %in% input$eda) %>%
      group_by(brand_country) %>%
      add_lines()
    
    
  })
  
  
  output$eda3 <- renderPlotly({
    
    plot_ly(df_all, x = ~date, y = ~prospecting_imp_return, name = "PSP impressions Returning") %>%
      filter(brand_country %in% input$eda) %>%
      group_by(brand_country) %>%
      add_lines()
    
    
  })
  
  
  ###
  ### new
  
  output$pp1 <- renderPlotly({
    
    ###
    ### The default order will be alphabetized unless specified as below:
    ###
    
    
    fig <- plot_ly(forecast, x = ~ds, y = ~yhat_upper, type = 'scatter', mode = 'lines',
                   line = list(color = 'transparent'),
                   showlegend = FALSE, name = 'High Orders')
    fig <- fig %>%
      filter(brand_country %in% input$forecast) %>%
      group_by(brand_country) %>%
      add_lines()
    fig <- fig %>% add_trace(y = ~yhat_lower, type = 'scatter', mode = 'lines',
                             fill = 'tonexty', fillcolor = 'rgba(0,100,80,0.2)', line = list(color = 'transparent'),
                             showlegend = FALSE, name = 'Low Orders')
    fig <- fig %>% add_trace(x = ~ds, y = ~yhat, type = 'scatter', mode = 'lines',
                             line = list(color = 'rgb(0,100,80)'),
                             name = 'Modeled Orders')
    fig <- fig %>% add_trace(x = ~ds, y = ~r_orders, type = 'scatter', mode = 'lines',
                             line = list(color = 'rgb(0,0,0)'),
                             name = 'Real Orders')
    fig <- fig %>% layout(title = "Forecast Per Hour",
                          paper_bgcolor = 'rgb(255,255,255)', plot_bgcolor = 'rgb(229,229,229)',
                          xaxis = list(title = "Months",
                                       gridcolor = 'rgb(255,255,255)',
                                       showgrid = TRUE,
                                       showline = FALSE,
                                       showticklabels = TRUE,
                                       tickcolor = 'rgb(127,127,127)',
                                       ticks = 'outside',
                                       zeroline = FALSE,
                                       autorange = TRUE,
                                       fixedrange = FALSE),
                          yaxis = list(title = "Modeled Orders",
                                       gridcolor = 'rgb(255,255,255)',
                                       showgrid = TRUE,
                                       showline = FALSE,
                                       showticklabels = TRUE,
                                       tickcolor = 'rgb(127,127,127)',
                                       ticks = 'outside',
                                       zeroline = FALSE,
                                       autorange = TRUE,
                                       fixedrange = FALSE))
    
    
    fig
    
    
  })
  
  
  output$vv3 <- renderPlotly({
    
    ###
    ### The default order will be alphabetized unless specified as below:
    ###
    
    
    fig <- plot_ly(inner_forecast, x = ~ds, y = ~yhat_upper, type = 'scatter', mode = 'lines',
                   line = list(color = 'transparent'),
                   showlegend = FALSE, name = 'High Orders')
    fig <- fig %>%
      filter(brand_country %in% input$per_day) %>%
      group_by(brand_country) %>%
      add_lines()
    fig <- fig %>% add_trace(y = ~yhat_lower, type = 'scatter', mode = 'lines',
                             fill = 'tonexty', fillcolor = 'rgba(0,100,80,0.2)', line = list(color = 'transparent'),
                             showlegend = FALSE, name = 'Low Orders')
    fig <- fig %>% add_trace(x = ~ds, y = ~yhat, type = 'scatter', mode = 'lines',
                             line = list(color = 'rgb(0,100,80)'),
                             name = 'Modeled Orders')
    fig <- fig %>% add_trace(x = ~ds, y = ~r_orders, type = 'scatter', mode = 'lines',
                             line = list(color = 'rgb(0,0,0)'),
                             name = 'Real Orders')
    fig <- fig %>% layout(title = "Forecast Per Day",
                          paper_bgcolor = 'rgb(255,255,255)', plot_bgcolor = 'rgb(229,229,229)',
                          xaxis = list(title = "Months",
                                       gridcolor = 'rgb(255,255,255)',
                                       showgrid = TRUE,
                                       showline = FALSE,
                                       showticklabels = TRUE,
                                       tickcolor = 'rgb(127,127,127)',
                                       ticks = 'outside',
                                       zeroline = FALSE,
                                       autorange = TRUE),
                          yaxis = list(title = "Modeled Orders",
                                       gridcolor = 'rgb(255,255,255)',
                                       showgrid = TRUE,
                                       showline = FALSE,
                                       showticklabels = TRUE,
                                       tickcolor = 'rgb(127,127,127)',
                                       ticks = 'outside',
                                       zeroline = FALSE,
                                       autorange = TRUE,
                                       fixedrange = FALSE))
    
    
    fig
    
    
  })
  
  
  #####
  #### GA_sessions
  ####
  
  output$vv2 <- renderPlotly({
    
    ###
    ### The default order will be alphabetized unless specified as below:
    ###
    
    
    fig <- plot_ly(inner_ga, x = ~date, y = ~sessions, mode = 'lines', name = "sessions")
    fig <- fig %>%
      filter(brand_country %in% input$ga_data) %>%
      group_by(brand_country) %>%
      add_lines()
    fig <- fig %>% add_trace(y = ~transactions, type = 'scatter', mode = 'lines', name = "transactions", yaxis = "y2")
    fig <- fig %>% layout(yaxis2 = list(overlaying = "y", side = "right"))
    
    
    fig
    
    
  })
  
  
  #####
  #### Funnel
  ####
  
  output$fu1 <- renderPlotly({
    
    
    fig <- plot_ly(funnel_historical)
    fig <- fig %>%
      filter(brand_country %in% input$funnel_historical) %>%
      group_by(brand_country) %>%
      add_trace(type = "funnel",
                y = ~shoppingStage,
                x = ~users,
                textposition = "inside",
                textinfo = "value+percent initial",
                opacity = 0.65,
                marker = list(color = c("deepskyblue", "darkblue", "tan", "teal", "silver", "darkgrey", "green"),
                              line = list(width = c(4, 2, 2, 3, 1, 1), color = c("wheat", "wheat", "blue", "wheat", "wheat"))),
                connector = list(line = list(color = "royalblue", dash = "dot", width = 3)))
    fig <- fig %>%
      layout(title = "Historical trend", yaxis = list(categoryarray = c("ALL_VISITS", "PRODUCT_VIEW", "CHECKOUT_1",
                                                                        "CHECKOUT_2", "CHECKOUT_3", "TRANSACTION")))
    
    
    fig
    
  })
  
  
  #####
  #### Funnel
  ####
  
  output$fu2 <- renderPlotly({
    
    
    fig <- plot_ly(funnel7)
    fig <- fig %>%
      filter(brand_country %in% input$funnel7) %>%
      group_by(brand_country) %>%
      add_trace(type = "funnel",
                y = ~shoppingStage,
                x = ~users,
                textposition = "inside",
                textinfo = "value+percent initial",
                opacity = 0.65,
                marker = list(color = c("deepskyblue", "darkblue", "tan", "teal", "silver", "darkgrey", "green"),
                              line = list(width = c(4, 2, 2, 3, 1, 1), color = c("wheat", "wheat", "blue", "wheat", "wheat"))),
                connector = list(line = list(color = "royalblue", dash = "dot", width = 3)))
    fig <- fig %>%
      layout(title = " 7 days Trend", yaxis = list(categoryarray = c("ALL_VISITS", "PRODUCT_VIEW", "CHECKOUT_1",
                                                                     "CHECKOUT_2", "CHECKOUT_3", "TRANSACTION")))
    
    
    fig
    
    
  })
  
  
  output$fu3 <- renderPlotly({
    
    
    fig <- plot_ly(funnelt)
    fig <- fig %>%
      filter(brand_country %in% input$funnelt) %>%
      group_by(brand_country) %>%
      add_trace(type = "funnel",
                y = ~shoppingStage,
                x = ~users,
                textposition = "inside",
                textinfo = "value+percent initial",
                opacity = 0.65,
                marker = list(color = c("deepskyblue", "darkblue", "tan", "teal", "silver", "darkgrey", "green"),
                              line = list(width = c(4, 2, 2, 3, 1, 1), color = c("wheat", "wheat", "blue", "wheat", "wheat"))),
                connector = list(line = list(color = "royalblue", dash = "dot", width = 3)))
    fig <- fig %>%
      layout(title = "Yesterday Trend", yaxis = list(categoryarray = c("ALL_VISITS", "PRODUCT_VIEW", "CHECKOUT_1",
                                                                       "CHECKOUT_2", "CHECKOUT_3", "TRANSACTION")))
    
    
    fig
    
    
  })
  
  
  ###
  #### Product render
  ###
  
  output$d <- renderPlotly({
    
    plot_ly(df_bbw_products, x = ~name, y = ~total, type = 'scatter', mode = 'markers',
            size = ~total, color = ~total, colors = 'Reds',
            sizes = c(10, 50),
            marker = list(opacity = 0.5, sizemode = 'diameter')) %>% layout(title = 'last 7 Days Top Sellers BBW',
                                                                            xaxis = list(showgrid = FALSE),
                                                                            yaxis = list(showgrid = FALSE),
                                                                            showlegend = FALSE)
    
  })
  
  
  output$d7 <- renderPlotly({
    
    plot_ly(df_bbw_products_14, x = ~name, y = ~total, type = 'scatter', mode = 'markers',
            size = ~total, color = ~total, colors = 'Reds',
            sizes = c(10, 50),
            marker = list(opacity = 0.5, sizemode = 'diameter')) %>% layout(title = '7-14 Days Top Sellers BBW',
                                                                            xaxis = list(showgrid = FALSE),
                                                                            yaxis = list(showgrid = FALSE),
                                                                            showlegend = FALSE)
    
  })
  
  
  #####
  #### Product GA BBW KSA
  ###
  
  
  output$pro <- renderPlotly({
    
    
    fig <- plot_ly(product)
    fig <- fig %>% filter(brand_country %in% input$product)
    fig <- fig %>% add_trace(x = ~productName, y = ~itemRevenue, type = 'bar',
                             textposition = 'auto', name = "revenue",
                             marker = list(color = 'rgb(158,202,225)',
                                           line = list(color = 'rgb(8,48,107)', width = 1.5)))
    fig <- fig %>% add_trace(x = ~productName, y = ~productListClicks, type = 'bar',
                             textposition = 'auto', name = "clicks",
                             marker = list(color = 'rgb(58,200,225)',
                                           line = list(color = 'rgb(8,48,107)', width = 1.5)))
    fig <- fig %>% layout(title = "Last Month",
                          barmode = 'group',
                          xaxis = list(title = ""),
                          yaxis = list(title = ""))
    
    fig
    
  })
  
  
  #####
  #### Product GA BBW KSA
  ###
  
  
  output$product60 <- renderPlotly({
    
    
    fig <- plot_ly(product_60)
    fig <- fig %>% filter(brand_country %in% input$product_60)
    fig <- fig %>% add_trace(x = ~productName, y = ~itemRevenue, type = 'bar',
                             textposition = 'auto', name = "revenue",
                             marker = list(color = 'rgb(158,202,225)',
                                           line = list(color = 'rgb(8,48,107)', width = 1.5)))
    fig <- fig %>% add_trace(x = ~productName, y = ~productListClicks, type = 'bar',
                             textposition = 'auto', name = "clicks",
                             marker = list(color = 'rgb(58,200,225)',
                                           line = list(color = 'rgb(8,48,107)', width = 1.5)))
    fig <- fig %>% layout(title = "This week",
                          barmode = 'group',
                          xaxis = list(title = ""),
                          yaxis = list(title = ""))
    
    fig
    
  })
  
  
  #####
  #### outliers table
  ###
  
  
  output$outliers <- DT::renderDataTable({
    DT::datatable(df_cpo) %>%
      formatPercentage('CIR',
                       2) %>%
      formatPercentage('CvR',
                       2) %>%
      formatPercentage('new_customers', 2) %>%
      formatPercentage('target_cir', 0) %>%
      formatRound("CPV",
                  digits = 2) %>%
      formatRound("sessions", digits = 0,
                  interval = 3) %>%
      formatCurrency(c(
        'CPO', 'CPA', 'AOV')) %>%
      formatRound("orders",
                  digits = 0,
                  interval = 3,
                  mark = ",") %>%
      formatCurrency('total_cost', digits = 0) %>%
      formatCurrency('gross_revenue', digits = 0)
    
    
  })
  
  
  #####
  #### outliers table
  ###
  
  
  output$outliersy <- DT::renderDataTable({
    DT::datatable(df_cpo_yesterday) %>%
      formatPercentage('CIR',
                       2) %>%
      formatPercentage('CvR',
                       2) %>%
      formatPercentage('new_customers', 2) %>%
      formatPercentage('target_cir', 0) %>%
      formatRound("CPV",
                  digits = 2) %>%
      formatRound("sessions", digits = 0,
                  interval = 3) %>%
      formatCurrency(c(
        'CPO', 'CPA', 'AOV')) %>%
      formatRound("orders",
                  digits = 0,
                  interval = 3,
                  mark = ",") %>%
      formatCurrency('total_cost', digits = 0) %>%
      formatCurrency('gross_revenue', digits = 0)
    
    
  })
  
  ####
  ### Plot Box with results.
  ####
  
  
  output$total_spend <- renderValueBox({
    valueBox(
      "Gross Revenue KD",
      scales::dollar(sum(df_cpo$gross_revenue) / 3.281),
      icon = icon("credit-card")
    )
  })
  
  
  output$cir <- renderValueBox({
    valueBox(
      "Gross CiR",
      scales::percent(sum(df_cpo$total_cost) / sum(df_cpo$gross_revenue)),
      icon = icon("signal"),
      color = "green"
    )
  })
  
  
  output$total_spendy <- renderValueBox({
    valueBox(
      "Gross Revenue KD",
      scales::dollar(sum(df_cpo_yesterday$gross_revenue) / 3.281),
      icon = icon("credit-card")
      
    )
  })
  
  
  output$ciry <- renderValueBox({
    valueBox(
      "Gross CiR",
      scales::percent(sum(df_cpo_yesterday$total_cost) / sum(df_cpo_yesterday$gross_revenue)),
      icon = icon("signal"),
      color = "green"
    )
  })
  
  
  ####
  ### Plot CIR EBITDA
  ####
  
  output$topcir <- renderPlotly({
    
    
    fig <- plot_ly(top_cir)
    fig <- fig %>%
      filter(brand_country %in% input$top_cir) %>%
      group_by(brand_country) %>%
      add_trace(x = ~CIR,
                y = ~brand_ctb,
                type = "scatter",
                mode = "markers",
                yaxis = "y2",
                name = "EBITDA") %>%
      add_trace(alpha = 0.6,
                nbinsx = 30,
                x = ~CIR,
                type = "histogram",
                name = "CIR")
    
    fig <- fig %>% layout(yaxis2 = list(overlaying = "y", side = "right"))
    
    
  })
  
  
  ####
  ### Plot GT trends
  ####
  
  output$gt1 <- renderPlotly({
    
    
    fig <- plot_ly(df_sessions_i)
    fig <- fig %>%
      filter(brand_name %in% input$brand_name) %>%
      group_by(brand_name) %>%
      add_trace(x = ~date, y = ~total_transactions, type = 'scatter', mode = 'lines', name = "Transactions") %>%
      add_trace(x = ~date, y = ~paid_sessions, type = 'scatter', mode = 'lines', name = "Paid", yaxis = "y2") %>%
      add_trace(x = ~date, y = ~organic_sessions, type = 'scatter', mode = 'lines', name = "Organic", yaxis = "y2")
    fig <- fig %>% layout(yaxis2 = list(overlaying = "y", side = "right"))
    
    
    fig
    
    
  })
  
  
  output$gt2 <- renderPlotly({
    
    
    fig <- plot_ly(channel)
    fig <- fig %>%
      filter(brand_name %in% input$channel) %>%
      group_by(brand_name) %>%
      add_trace(x = ~date, y = ~Organic, type = 'scatter', mode = 'lines', name = "Organic") %>%
      add_trace(x = ~date, y = ~Organic_Social, type = 'scatter', mode = 'lines', name = "Organic Social") %>%
      add_trace(x = ~date, y = ~Non_Brand_Paid_Search, type = 'scatter', mode = 'lines', name = "Non_Brand_Paid_Search ") %>%
      add_trace(x = ~date, y = ~Facebook, type = 'scatter', mode = 'lines', name = "Facebook") %>%
      add_trace(x = ~date, y = ~Brand_Paid_Search, type = 'scatter', mode = 'lines', name = "Brand_Paid_Search") %>%
      add_trace(x = ~date, y = ~CvR, type = 'scatter', mode = 'lines', name = "Basline CvR", yaxis = "y2")
    fig <- fig %>% layout(yaxis2 = list(overlaying = "y", side = "right"))
    
    
    fig
    
    
  })
  
  
  ####
  ### Reactive Functions
  ####
  filteredData <- reactive({
    req(input$myDateRange)
    req(input$ga_brand_country)
    
    print(input$myDateRange)
    
    ga_sub_set <- subset(ga_st, date >= input$myDateRange[1] &
                           date <= input$myDateRange[2]) %>% filter(brand_country %in% input$ga_brand_country)
    
    print(ga_sub_set)
  })
  
  filter_days_st <- reactive({
    req(input$myDateRange)
    req(input$brand_country)
    
    print(input$myDateRange)
    print(input$ga_brand_country)
    
    ga_sub_set <- subset(ga_st, date >= input$myDateRange[1] &
                           date <= input$myDateRange[2]) %>% filter(brand_country %in% input$ga_brand_country)
    
    print(ga_sub_set)
    
    sessions_f <- ga_sub_set %>%
      group_by(dayOfWeek) %>%
      summarise(Frequency = sum(sessions))
    transactions_f <- ga_sub_set %>%
      group_by(dayOfWeek) %>%
      summarise(Transactions = sum(transactions))
    
    transactions_f$sessions <- sessions_f$Frequency
    transactions_f$transactions <- transactions_f$Transactions
    
    # Sort by last performing
    dfs <- transactions_f[order(transactions_f$sessions),]
    dft <- transactions_f[order(transactions_f$transactions),]
    dfs$index_day <- dfs$dayOfWeek
    dft$index_day <- dft$dayOfWeek
    
    dfs$dayOfWeek <- as.numeric(dfs$dayOfWeek)
    dft$dayOfWeek <- as.numeric(dft$dayOfWeek)
    
    ### Final transformation
    
    df_final <- dft %>% select(dayOfWeek)
    df_final$deviation <- dft$dayOfWeek - dfs$dayOfWeek
    
    df_final$score <- ifelse(df_final$deviation == 0, "Sessions Index matching Transactions Index", "Sessions Index not matching Transactions Index")
    
    deviation_data <- df_final
    deviation_data$weekDay <- recode(
      deviation_data$dayOfWeek,
      "0" = "Sun",
      "1" = "Mon",
      "2" = "Tue",
      "3" = "Wed",
      "4" = "Thu",
      "5" = "Fri",
      "6" = "Sat"
    )
    
    days_status_data <- data.frame(deviation_data$weekDay)
    days_status_data$status <- unlist(deviation_data$score)
    print(days_status_data)
  })
  
  ###
  ##  Bar Plots
  ###
  
  output$ga_st_bars <- renderPlotly({
    
    req({ nrow(filteredData()) > 0 })
    
    #Day of the week, a one-digit number from 0 (Sunday) to 6 (Saturday).
    fig <- plot_ly(filteredData())
    fig <- fig %>% add_trace(x = ~dayOfWeek, y = ~sessions, type = 'bar',
                             textposition = 'auto', name = "sessions",
                             marker = list(color = c("#000099")))
    
    fig <- fig %>% add_trace(x = ~dayOfWeek, y = ~transactions, type = 'bar',
                             textposition = 'auto', name = "transactions", yaxis = "y2",
                             opacity = 0.3, marker = list(color = c("#000099")))
    
    
    fig <- fig %>% layout(yaxis2 = list(overlaying = "y", side = "right"))
    fig
    
    
  })
  
  
  ###
  ##  Table Plots
  ###
  
  output$daysFrequency <- DT::renderDataTable({
    DT::datatable(filter_days_st())
  })
  
  
  ####
  ### Simulator Zone
  ####
  
  
  output$sim <- renderPlotly({
    
    df1 <- df1
    fig <- df1 %>%
      plot_ly(
        x = ~total_cost,
        y = ~session,
        size = ~total_cost,
        color = ~brand,
        frame = ~month_id,
        text = ~year_id,
        hoverinfo = "text",
        type = 'scatter',
        mode = 'markers'
      )
    fig <- fig %>% layout(
      paper_bgcolor = 'rgb(255,255,255)',
      xaxis = list(
        type = "log"
      )
    )
    
    fig
    
  })
  
  
}


shinyApp(ui, server)
