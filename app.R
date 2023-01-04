
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
library(shiny)
library(tidyverse)
library(httr)
library(jsonlite)
library(dplyr)
library(shinydashboard)
library(leaflet)
library(shinycssloaders)
library(rgdal)
library(plotly)
library(htmltools)
library(DT)
library(shinyjs)
library(shinyWidgets)

options(warn = -1)

year_ <- "2020,2019,2018,2017,2016,2015,2014,2013,2012,2011,2010"
state_  <- "ar,ca,fl,ga,il,in,ia,ks,mn,mo,ni,nc,tx,wa,wi"
category_ <- "grp,sal,spec,age"
variable_ <- "kount,infi"
url_ <- paste0("https://api.ers.usda.gov/data/arms/surveydata?api_key=bpgH068vgKbfWr00Aev3F6iI70xbNCKDsHAH6f5X&year=",year_,"&state=",state_,"&category=",category_,"&variable=",variable_,"&report=income&farmtype=all")
my_content <- GET(url_)
my_content <- content(my_content, as = 'text')
content_json <- fromJSON(my_content)
df <- content_json$data
dfid <- df %>% filter(variable_id %in% "kount")

# ui <- navbarPage(
#   title = "USDA Farm Data",
#
#   tabPanel(
#     title = "Net Income plot",
#     titlePanel("Average Farm Net Income by State and Farm Type"),
#     sidebarLayout(
#       sidebarPanel(
#         selectInput(inputId = "state", label = "State", choices = sort(unique(df$state))),
#         selectInput(inputId = "farm_type", label = "Farm Type", choices = unique(c("Residence farms","Intermediate farms","Commercial farms"))),
#       ),
#       mainPanel(plotOutput("plot"))
#     ),
#     actionButton("reset", "Retrieve & Update Data"),
#     actionButton("plot", "Plot")
#   ),
#
#   tabPanel(
#     title = "Net Income summary table",dataTableOutput("table"))
# )

ui <- dashboardPage(
  dashboardHeader(title = "USDA Farm Data"),
  dashboardSidebar(
    
    useShinyjs(),
    sidebarMenu(
      id = "tabs",
      menuItem("Statistics in Map", tabName = "plotm", icon = icon("map")),
      menuItem("Net Income Plot", tabName = "plotb", icon = icon("chart-line")),
      menuItem("Net Income Summary Table", tabName = "table",icon = icon("table")),
      menuItem("About",tabName = "about", icon = icon("info"))
    ),
    
    actionButton("reset", "Retrieve & Update Data"),
    actionButton("help", "Help")
    
  ),
  dashboardBody(
    tabItems(
      tabItem("plotb",
              br(),
              span(style = "font-weight: 600; font-size: 30px; width: 100%;
         color: #022DB7;", "Average Net Income in Recent Years "),
              fluidRow(
                column(3,  selectInput(inputId = "state", label = "State", choices = sort(unique(df$state)))),
                column(3,  selectInput(inputId = "farm_type", label = "Farm Type", choices = unique(c("Residence farms","Intermediate farms","Commercial farms")))),
                column(3,  actionButton("plot", "Draw Plot"))),
              plotOutput("plotc")),
      tabItem("plotm",
              br(),
              span(style = "font-weight: 600; font-size: 30px; width: 100%;
         color: #022DB7;", "Farms Comparison Between States"),
              fluidRow(
                column(3, pickerInput('year', "Year",choices = seq(2010,2020),selected = 2010)),
                column(3, pickerInput('cate', "Category",choices = unique(dfid$category_value),selected = unique(dfid$category_value)[1], multiple = T)),
                column(6, h3('Available States in Data: '), h4(paste0(unique(dfid$state), collapse = ", "), style="color:#880ED4"))
              ),
              
              br(),br(),
              fluidRow(
                column(8, leafletOutput("Zone", height = "550px")%>% withSpinner(color="#0dc5c1")),
                column(4,
                       span("Select "), span( style="color:green", "State 1"), span(" and "), span( style="color:red", "State 2"),
                       span(" from the map:"),
                       br(),br(),
                       htmlOutput("od_info")%>% withSpinner(color="#0dc5c1"),
                       hr(),
                       htmlOutput("od_total")%>% withSpinner(color="#0dc5c1")
                )
              ),
              br(),br(),
              fluidRow(
                column(9, div(DT::dataTableOutput("od_vol"),  width = "100%", style = "font-size:100%"))
              ),
              fluidRow(
                column(5, plotlyOutput("od_ton_chart", width = "100%", height = "350px")%>% withSpinner(color="#0dc5c1")),
                column(4, plotlyOutput("od_ton_pie", width = "100%", height = "250px")%>% withSpinner(color="#0dc5c1")),
                column(3, plotlyOutput("od_ton_pie_5", width = "100%", height = "250px")%>% withSpinner(color="#0dc5c1"))
              )
      ),
      tabItem("table",
              br(),
              span(style = "font-weight: 600; font-size: 30px; width: 100%;
         color: #022DB7;", "Net Income Summary"),
              fluidRow(
                column(3,  selectInput(inputId = "state_2", label = "State", choices = sort(unique(df$state)))),
                column(3,  selectInput(inputId = "cate2", label = "Category", choices = sort(unique(df$category)))),
                column(3,  actionButton("draw", "Draw Table"))),
              fluidRow(box(width = 12, div(uiOutput('tt'), style='text-align: center;'),DT::dataTableOutput("tablec")))
      ),
      tabItem("about",includeMarkdown("about.Rmd"))
    )
  )
)


server <- function(input, output, session) {
  observeEvent(input$reset, {
    url_ <- paste0("https://api.ers.usda.gov/data/arms/surveydata?api_key=bpgH068vgKbfWr00Aev3F6iI70xbNCKDsHAH6f5X&year=",year_,"&state=",state_,"&category=",category_,"&variable=",variable_,"&report=income&farmtype=all")
    my_content <- GET(url_)
    my_content <- content(my_content, as = 'text')
    content_json <- fromJSON(my_content)
    df <- content_json$data
    dfid <- df %>% filter(variable_id %in% "kount")
    showNotification(paste0("Data Updated!  Retrieved time:  ",Sys.time()), duration = NULL)
    
  })
  observeEvent(input$help, {
    showModal(modalDialog(
      title = "How to Use the Map",
      HTML("1. Select State 1 and State 2 on the interactive map to start comparison. State 1 will be green dot, and State 2 will be red dot. Available states in the map are marked in this tab (text in purple). When choosing a state, only click on state Showing “Remainder of xxx state”, for example, “Remainder of Illinois”.<br><br>
         2.  Choose “Year” and “Category” using slide bars, you can choose more than one categories in “Category” side bar.<br><br>
         3.  Results and visualization will be shown below, if the window does not fully show them, try scrolling down.<br><br>
         4.  The map is about the farm quantities.<br><br>
         5.  To see category details and more information, click on “About” tab.<br><br>
         6.  Source data can be updated by clicking on  “Retrive & Update Data”"),
      easyClose = TRUE,
      footer = NULL
    ))
    
  })
  
  farm_state = reactive({
    df %>% filter(state == input$state)
  })
  
  farm_state_type = reactive({
    farm_state() %>% filter(category_value == input$farm_type, variable_id == "infi")
  })
  
  farm_year_income = reactive({
    df_ic <- df %>% filter(state == input$state_2,variable_id == "infi", category == input$cate2) %>%
      select(year,category_value,estimate) %>%
      group_by(year,category_value) %>%
      summarise(Income = sum(estimate, na.rm = TRUE)) %>%
      ungroup() %>%
      arrange(year)
    names(df_ic) <- c('year', 'Category', 'Income')
    
    finaldf <- tidyr::pivot_wider(df_ic, names_from = 'year', values_from = 'Income')
    finaldf
  })
  
  
  observeEvent(input$plot, {
    output$plotc = renderPlot({
      # ggplot(data = farm_state_type(), mapping = aes(x = year, y = estimate)) + geom_line(color="blue",fill="#69b3a2",size=2) + ggtitle("Trend by Year")
      ggplot(data = farm_state_type(), mapping = aes(x = year, y = estimate, fill = estimate, label = estimate)) +
        ylab("Net Income")+
        xlab("Year")+
        geom_col() +
        labs(fill="Net Income")+
        geom_text(vjust = -0.35, size = 3) +
        labs(title = input$state,
             subtitle = input$farm_type) +
        theme(plot.title = element_text(size = 25),
              plot.subtitle = element_text(size = 15),
              axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0), face = "bold"),
              axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0), face = "bold")) +
        scale_x_continuous(breaks = seq(2010, 2020, 1))
    })
  })
  
  observeEvent(input$draw, {
    
    output$tt <- renderUI(
      h4('Average Net Income for Specific Category and State' )
    )
    
    output$tablec = renderDataTable(
      
      datatable(data =farm_year_income(),  class = "caption-top",
                rownames = FALSE,
                filter = "none",
                options = list(info = FALSE, lengthChange = FALSE, ordering=F, searching = FALSE, paging=FALSE)) %>%
        formatStyle(columns = c("Category"), fontWeight = 'bold', `text-align` = 'left'))
    
    
  })
  
  
  
  
  #################### For map ######################
  zone.rg <- readOGR(dsn="FAF_Zones/faf4_zone2.shp",layer="faf4_zone2", encoding = "UTF-8")  # map background: used to construct map
  centroid <- read.csv(file = 'centroid.csv')   # coordinates: used to construct map
  
  dfid <- df %>% filter(variable_id %in% "kount")
  temp <- dfid %>% distinct(state)
  cname <- centroid$name
  
  dms_orig <- c()
  for (i in unique(temp$state)) {
    tmp <-centroid %>% filter(grepl(i,centroid$name), grepl("Remainder",centroid$name)) %>% select(id) %>% pull()
    dms_orig <- c(dms_orig,tmp)
  }
  
  dms_dest <- dms_orig
  year <- unique(dfid$year)
  category_value <- unique(dfid$category_value)
  alldf <- tidyr::crossing(year,category_value,dms_orig, dms_dest)
  
  dfid1 <- merge(dfid, data.frame(state=unique(temp$state), dmsid = dms_orig), by = 'state', all.x = TRUE) %>%
    select(year, category_value,dmsid, estimate, state)
  
  names(dfid1) <- c("year", "category_value","dms_orig", "estimate_orig", "state_orig")
  
  dfid2 <- dfid1
  
  names(dfid2) <- c("year", "category_value","dms_dest", "estimate_dest", "state_dest")
  
  alldf <- merge(alldf,dfid1,by=c("year", "category_value","dms_orig"), all.x=T)
  alldf <- merge(alldf,dfid2,by=c("year", "category_value","dms_dest"), all.x=T)
  
  
  
  click_count <- 0
  type <- 0
  origin <- ""
  dest <- ""
  origin_id <- 0
  dest_id <- 0
  
  selected_zone <- reactive({
    p <- input$Zone_shape_click
    subset(centroid, id==p$id )
  })
  
  selected_od <- reactive({
    p <- input$Zone_shape_click
    
    selected <- subset(centroid, id==p$id )
    od_pair <- data.frame()
    if (type ==0 ){
      origin <<- selected$name
      dest <<- ""
      origin_id <<- selected$id
      dest_id <<- 0
    }
    
    if (type == 1){
      dest_id <<- selected$id
      dest <<- selected$name
      od_pair <- data.frame(origin, origin_id, dest, dest_id)
      colnames(od_pair)<- c("origin", "origin_id", "dest", "dest_id")
    }
    od_pair
    
  })
  
  output$Zone <- renderLeaflet({
    zone_labels <- sprintf(
      "<strong>%s</strong><br/>",
      paste(zone.rg$id, "--", zone.rg$name, sep='')
    ) %>% lapply(htmltools::HTML)
    
    m<-leaflet() %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Default Maptile",
                       options = providerTileOptions(noWrap = TRUE))%>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite Maptile") %>%
      setView(lng = -95.0491410487803, lat = 38.8977674296551, zoom = 4)%>%
      addLayersControl(
        baseGroups = c("Default Maptile", "Satellite Maptile"),
        options = layersControlOptions(collapsed = TRUE)
      )%>%
      addPolygons(data=zone.rg, col="black", weight = 1, layerId = ~id, label = zone_labels,
                  highlight = highlightOptions(color = "blue",weight = 2, bringToFront = F, opacity = 0.7))
  })
  
  observe({
    p <- input$Zone_shape_click
    if (is.null(p))
      return()
    
    m2<-leafletProxy("Zone", session = session)
    
    zone_labels <- sprintf(
      "<strong>%s</strong><br/>",
      paste(centroid$id, "--", centroid$name, sep='')
    ) %>% lapply(htmltools::HTML)
    
    selected <- selected_zone()
    selected_zone_labels <- sprintf(
      "<strong>%s</strong><br/>",
      paste(selected$id, "--", selected$name, sep='')
    ) %>% lapply(htmltools::HTML)
    
    type <<- click_count%%2
    if (type ==0 ){
      m2 %>% clearMarkers()%>%
        addCircleMarkers(data=selected, radius=6, color="green", lng =~x, lat =~y, stroke=FALSE, label = selected_zone_labels,
                         fillOpacity=1, layerId = ~id)
    }
    
    if (type == 1){
      m2 %>%
        addCircleMarkers(data=selected, radius=6, color="red", lng =~x, lat =~y, stroke=FALSE, label = selected_zone_labels,
                         fillOpacity=1, layerId = ~id)
    }
    click_count <<- click_count+1
  })
  
  output$od_info <- renderText({
    p <- input$Zone_shape_click
    
    selected <- subset(centroid, id==p$id )
    
    if (type ==0 ){
      origin <<- selected$name
      dest <<- ""
      origin_id <<- selected$id
      dest_id <<- 0
    }
    
    if (type == 1){
      dest_id <<- selected$id
      dest <<- selected$name
    }
    
    paste(
      "<strong> <span style = \'font-weight: 700;\'> State 1:            </span> </strong>
        <strong> <span style = \'font-weight: 500;\'> ",origin, "</span> </strong>
          <br>",
      "<strong> <span style = \'font-weight: 700;\'> State 2:       </span> </strong>
        <strong> <span style = \'font-weight: 500;\'> ",dest, "</span> </strong>
          <br>"
      ,sep = '')
  })
  
  output$od_vol <- DT::renderDataTable(server = FALSE,{
    vol<-data.frame()
    selected <- selected_od()
    
    if (length(selected)){
      vol_df <- subset(alldf, dms_orig== selected$origin_id & dms_dest == selected$dest_id &
                         year %in% as.numeric(input$year) & category_value %in% input$cate)
      vol <- vol_df %>%
        select(category_value, estimate_orig, estimate_dest)
      
      vol$estimate_orig <- format(round(vol$estimate_orig, 2), big.mark=",")
      vol$estimate_dest <- format(round(vol$estimate_dest, 2), big.mark=",")
      colnames(vol) <- c('Category', "Farms(State 1)", "Farms(State 2)")
    }
    vol
  },
  rownames = FALSE,  class="compact", width="80%",
  options = list(paging = FALSE, searching = FALSE, ordering=F, info=F)
  )
  
  output$od_total <- renderText({
    value <- 0
    value2 <- 0
    s1 <- ""
    s2 <- ""
    selected <- selected_od()
    
    if (length(selected)){
      df_tmp <- subset(alldf, dms_orig== selected$origin_id & dms_dest == selected$dest_id &
                         year %in% as.numeric(input$year) & category_value %in% input$cate)
      df <- df_tmp%>%
        select(estimate_orig,estimate_dest) %>%
        summarize(val=sum(estimate_orig, na.rm = TRUE), val2=sum(estimate_dest, na.rm = TRUE))
      value <- format(round(df$val, 2), big.mark=",")
      value2 <- format(round(df$val2, 2), big.mark=",")
      s1 <- unique(df_tmp$state_orig)
      s2 <- unique(df_tmp$state_dest)
    }
    
    paste(
      "<strong> <span style = \'font-weight: 700;\'> Farm Quantities: selected categories & selected states </span> </strong>
          <br><br>",
      "<strong> <span style = \'font-weight: 500;\'> Total Farms (", s1,"):            ",value, "</span> </strong>
          <br>",
      "<strong> <span style = \'font-weight: 500;\'> Total Farms (", s2,"):            ",value2, "</span> </strong>
          <br>"
      ,sep = '')
  })
  
  
  output$od_ton_chart = renderPlotly({
    
    m <- list(l = 3, r = 10, b = 30, t = 80, pad = 4)
    selected <- selected_od()
    
    if (length(selected)){
      # df_sub <- subset(od_mode_vol, dms_orig== selected$origin_id & dms_dest == selected$dest_id) %>%
      #   select(mode, tons, tons5)
      df_sub <- subset(alldf, dms_orig== selected$origin_id & dms_dest == selected$dest_id &
                         year %in% as.numeric(input$year) & category_value %in% input$cate) %>%
        select(category_value, estimate_orig, estimate_dest)
      na.omit(df_sub)
      if(nrow(df_sub)){
        plot_ly(df_sub, x = ~category_value, y = ~estimate_orig, type = 'bar', name = 'State 1',paper_bgcolor='transparent')%>%
          add_trace(y = ~estimate_dest, name = 'State 2')%>%
          layout(yaxis = list(title = 'Farm Quantities'), paper_bgcolor='transparent', xaxis = list(title = 'Farm Categories'), barmode = 'group')
      }
    }
  })
  
  output$od_ton_pie = renderPlotly({
    
    m <- list(l = 3, r = 10, b = 30, t = 80, pad = 4)
    selected <- selected_od()
    
    if (length(selected)){
      df_sub <- subset(alldf, dms_orig== selected$origin_id & dms_dest == selected$dest_id &
                         year %in% as.numeric(input$year) & category_value %in% input$cate) %>%
        select(category_value, estimate_orig)
      if(nrow(df_sub)){
        colnames(df_sub) <- c("Mode", "Tons")
        p <- df_sub%>%
          plot_ly(labels = ~Mode, values = ~round(Tons, 2),
                  width = 350, height = 300) %>%
          add_pie(hole = 0.4)%>%
          layout(title = "State 1 by Categories",
                 font = list(family='Arial', size = 11), margin = m,
                 showlegend = T, autosize = F,
                 legend = list(orientation = 'h', x=0, font = list( family = 'Arial', size = 10)),
                 paper_bgcolor='transparent',
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      }
    }
  })
  #
  output$od_ton_pie_5 = renderPlotly({
    
    m <- list(l = 3, r = 10, b = 30, t = 80, pad = 4)
    selected <- selected_od()
    
    if (length(selected)){
      df_sub <- subset(alldf, dms_orig== selected$origin_id & dms_dest == selected$dest_id &
                         year %in% as.numeric(input$year) & category_value %in% input$cate) %>%
        select(category_value, estimate_dest)
      if(nrow(df_sub)){
        colnames(df_sub) <- c("Mode", "Tons")
        p <- df_sub%>%
          plot_ly(labels = ~Mode, values = ~round(Tons, 2),
                  width = 350, height = 300) %>%
          add_pie(hole = 0.4)%>%
          layout(title = "State 2 by Categories",
                 font = list(family='Arial', size = 11), margin = m,
                 showlegend = T, autosize = F,
                 legend = list(orientation = 'h', x=0, font = list( family = 'Arial', size = 10)),
                 paper_bgcolor='transparent',
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      }
    }
  })
  
  showModal(modalDialog(
    title = "How to Use the Map",
    HTML("1. Select State 1 and State 2 on the interactive map to start comparison. State 1 will be green dot, and State 2 will be red dot. Available states in the map are marked in this tab (text in purple). When choosing a state, only click on state Showing “Remainder of xxx state”, for example, “Remainder of Illinois”.<br><br>
         2.  Choose “Year” and “Category” using slide bars, you can choose more than one categories in “Category” side bar.<br><br>
         3.  Results and visualization will be shown below, if the window does not fully show them, try scrolling down.<br><br>
         4.  The map is about the farm quantities.<br><br>
         5.  To see category details and more information, click on “About” tab.<br><br>
         6.  Source data can be updated by clicking on  “Retrive & Update Data”"),
    easyClose = TRUE,
    footer = NULL
  ))
  
  
  ########################################
  
}


shinyApp(ui = ui, server = server)