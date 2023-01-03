############################
# WORKING AS OF 2023-01-02 #
############################

# Load Packages ----

if (!require ("plyr")) install.packages("plyr")
if (!require ("tidyverse")) install.packages("tidyverse")
if (!require ("plotly")) install.packages("plotly")
if (!require ("shinydashboard")) install.packages("shinydashboard")
if (!require ("shiny")) install.packages("shiny")
if (!require ("leaflet")) install.packages("leaflet")
if (!require ("DT")) install.packages("DT")
if (!require ("RColorBrewer")) install.packages("RColorBrewer")
if (!require ("rgdal")) install.packages("rgdal")
if (!require ("rworldxtra")) install.packages("rworldxtra")
if (!require ("rworldmap")) install.packages("rworldmap")
if (!require ("rgeos")) install.packages("rgeos")
if (!require ("lubridate")) install.packages("lubridate")

library('plyr')
library('tidyverse')
library('plotly')
library('shinydashboard')
library('shiny')
library('leaflet')
library('DT')
library('lubridate')
library('rgeos')
library('rworldmap')
library('rworldxtra')
library('rgdal')
library('RColorBrewer')

Corona_new <- read.csv("corona_main.csv") %>%
  mutate(Date = as.Date(Date))

corona_global <- read.csv("corona_global.csv") %>%
  mutate(Date = as.Date(Date))

##### Clean up data + create new for plotting points on graph
##### Read this shape file with the rgdal library. 
world_spdf <- readOGR( 
  dsn= paste0("world_shape_file") , 
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE
) 

## THIS WORLD MAP CONTAINS OUTDATED NAMES FROM 2005
## NEED TO UPDATE NAMES AS PART OF OUR PRE-PROCESSING STEP
world_spdf@data$NAME <- world_spdf@data$NAME %>%
  recode("Brunei Darussalam" = "Brunei",
         "Iran (Islamic Republic of)" = "Iran",
         "Korea, Democratic People's Republic of" = "North Korea", 
         "Korea, Republic of" = "South Korea",
         "Lao People's Democratic Republic" = "Laos",
         "Falkland Islands (Malvinas)" = "Falkland Islands",
         "Syrian Arab Republic"  = "Syria" ,
         "Viet Nam" = "Vietnam",
         "Wallis and Futuna Islands" = "Wallis and Futuna" ,
         "Burma" = "Myanmar",
         "Libyan Arab Jamahiriya" = "Libya",
         "United Republic of Tanzania" = "Tanzania",
         "Democratic Republic of the Congo" = "Democratic Republic of Congo",
         "Republic of Moldova" = "Moldova",
         "Macau" = "Macao",
         "The former Yugoslav Republic of Macedonia" = "North Macedonia",
         "Czech Republic" = "Czechia",
         "Swaziland" = "Eswatini"
  )



# Create Plot  ----

# Creating these as object saves space since we will recycle this code
# Dark theme code consistent with all of layout 

dark_theme <- theme(axis.title.x = element_blank(),
                    axis.ticks = element_line(color = '#928f8c'),
                    axis.title.y = element_blank(),
                    axis.text = element_text(color = '#928f8c'),
                    axis.line = element_line(color = '#928f8c'),
                    panel.grid.minor.y = element_blank(),
                    panel.grid.major = element_line(linetype = "dashed", colour = "#575757"),
                    panel.grid.minor.x = element_blank(),
                    panel.background = element_rect(fill = "#2a2928"),
                    plot.background = element_rect(fill = "#2a2928"),
                    legend.background = element_rect(fill = '#2a2928'),
                    legend.key = element_rect(fill = '#2a2928', colour = '#2a2928'),
                    legend.position = "bottom",
                    legend.text = element_text(color = '#928f8c'),
                    legend.title = element_blank())

# Get rid of the big default margin spaces ; fits better on shiny dashboard
m = list(
  l = 0,
  r = 0,
  b = 0,
  t = 25,
  pad = 0)


new_cases <- ggplotly(Corona_new %>%
                        filter(Date == last(Date)) %>%
                        group_by(NAME) %>%
                        summarize(Date = Date, new_cases = new_cases, NAME) %>%
                        arrange(desc(new_cases)) %>%
                        slice(1:5) %>%
                        ggplot(aes(x = Date, y = new_cases, fill = NAME)) +
                        geom_col(position = position_dodge(1.1)) +
                        dark_theme +
                        scale_fill_manual(values =c('#fff7ec',
                                                    '#fee8c8', 
                                                    '#fdbb84', 
                                                    "#fc8d59" , 
                                                    "#ef6548" )),
                      height = 200) %>%
  config(displayModeBar = F) %>%
  layout(hoverlabel = list(
    bgcolor = '#2a2928'),
    autosize = F,
    margin = m,
    font = list(color = "#ffffff"))


###############################
### Get all cases for world ###
###############################

# unclass() gives us days since UNIX epoch (1970-01-01) 
# make sure the last(date) is in a date format 
# unclass(as.Date("1970-01-01")) will return 0 because 0 days since UNIX epoch

# We want to get the last date of our data using last(date) 
w_min_range = unclass(last(corona_global$Date)-(365*1.5)) 
w_max_range = unclass(last(corona_global$Date + 14))

world_cases <- ggplotly(corona_global %>%
  ggplot() +
  geom_line(aes(x = Date, y = log(total_cases), col = location), linewidth = 1) +
  dark_theme +
  scale_color_manual(values =c('#ffffff',
                                '#fee8c8', 
                                '#fdbb84', 
                                "#fc8d59" , 
                                "#ef6548",
                                "#d7301f",
                                "#d7301f")), 
                      height = 225) %>%
  
  config(displayModeBar = T,
         modeBarButtonsToRemove = c("resetScale", "resetAxis", "hoverClosest", "hoverCompare", "zoom", "pan"),
         displaylogo = F) %>%
  layout(hoverlabel = list(
    bgcolor = '#2a2928'),
    dragmode = "pan",
    font = list(color = "#ffffff"),
    xaxis = list(range = c(w_min_range, w_max_range)),
    yaxis = list(range = c(10, log(max(corona_global$total_cases, na.rm = TRUE))+2)),
    legend = list(title = list(text = "")))


# Shiny Dashboard  ----
# Tables to be output in the server and shown in corresponding columns

Confirmed_table <- Corona_new %>%
  group_by(NAME) %>%
  summarize(total_cases = max(replace_na(total_cases, 0))) %>%
  arrange(desc(total_cases))

Deaths_table <- Corona_new %>%
  group_by(NAME) %>%
  summarize(total_deaths = max(replace_na(total_deaths, 0))) %>%
  arrange(desc(total_deaths))

Vaccinated_table <- Corona_new %>%
  group_by(NAME) %>%
  summarize(total_vaccinated = max(replace_na(total_vaccinations, 0))) %>%
  arrange(desc(total_vaccinated))

### PALETTE FOR SHINY CHLOROPETH
mb <- c(0, 10, 100, 500, 1000, 10000, 100000, 1500000, Inf)

mp <- colorBin(palette="OrRd", 
               domain=Corona_new$new_cases, 
               na.color="transparent", 
               bins=mb)


######### SHINY DASHBOARD #########
header <- dashboardHeader(
  disable = TRUE
)

# To edit backgrounds, colors, scroll bars, must use CSS (Inspect Element to find the class name)
CSS <- '
             /* logo */
               .skin-blue .main-header .logo {
               background-color: #2a2928;
               }
                
             /* navbar (rest of the header) */
                .skin-blue .main-header .navbar {
                background-color: #2a2928;
                }
                 
             /* body */
             .content-wrapper, .right-side {
              background-color: #000000;
             }
                 
            /* box header */
            .box.box-solid.box-primary>.box-header {
            color:#fff;
            background:#2a2928
            }
                 
            /* box contents */
            .box.box-solid.box-primary{
            border-bottom-color:#2a2928;
            border-left-color:#2a2928;
            border-right-color:#2a2928;
            border-top-color:#2a2928;
            background:#2a2928 !important;
            opacity: 0.88;
            }
            
            /* Dark Pretty fonts */
            .box-body {
            color: #F0ECEC;
            font-size: 14px;
            font-family: "Avenir Next W01","Avenir Next W00","Avenir Next",Avenir,"Helvetica Neue",sans-serif;
            }
            
            
                 
            /* Color of tabs */
           .nav-tabs-custom>.nav-tabs>li>a {
           color: #fff;
           }
           
           /* Edit the collapsible graph box */
            .nav-tabs-custom>.nav-tabs>li.header {
            line-height: 35px;
            padding: 0 10px;
            font-size: 20px;
            color: #fff;
            }
           
           /* tab body color */
           .nav-tabs-custom>.tab-content {
           color: #fff;
           background-color: #2a2928;
           }
           
           .nav-tabs-custom {
           color: #fff;
           background:#2a2928;
           }
           
           /* Color of active tabs */
          .nav-tabs-custom>.nav-tabs>li.active:hover>a, .nav-tabs-custom>.nav-tabs>li.active>a {
          background-color: #2a2928;
          color: #fff
          }
          
           /* Data table rows are sorted */
          table.dataTable td.sorting_1 {
          background-color: #2a2928;
          }
          
          /* changed the dt background */
          table.dataTable td {
          background-color: #2a2928;
          color: #F0ECEC
          }
          
          
          /* Remove horizontal scroll for data tables */
          .dataTables_scrollBody {
           overflow-x:hidden !important;
          }
          
          .js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {
                                                  background: #e8ad5f;
                                                  border-top: 1px solid #e8ad5f ;
                                                  border-bottom: 1px solid #e8ad5f ;}

            /* changes the colour of the number tags */
            .irs-from, .irs-to, .irs-single { background: #000069 }
            
            /* Column names for collapsible table */               
            div.datatables {
              color: #fff !important;
            }
            
            /* Value boxes */
            .bg-blue {
              background-color: #0073b7!important;
              opacity: 0.8;
            }
            .small-box {height: 80px}
            
            .small-box h3 {
              font-size: 24px;
              font-weight: 700;
              margin: 0 0 10px 0;
              white-space: nowrap;
              padding: 0;
            }
          
          .alert-danger, .alert-error, .bg-red, .callout.callout-danger, .label-danger, .modal-danger .modal-body {
                background-color: #dd4b39!important;
                opacity: 0.80;
          }
            
            .bg-orange {
              background-color: #ff851b!important;
              opacity: 0.80;
            }
            
            .cases_bar .element.style{
                  width: 100%;
                  height: 200px !important;
                  visibility: inherit;
            }
          
            '



body = dashboardBody(
  tags$head( 
    tags$style(HTML(CSS))),
  
  leafletOutput("chloro_map", height = "96vh"), # Viewport hight // This makes viewing the map consistent and browser-friendly for different screen sizes
  absolutePanel(bottom = 25, left = 220,
                draggable = FALSE,
                sliderInput(
                  inputId = "time", #InputID that filters the Corona df to reactive df for leaflet
                  label = NULL,
                  min(Corona_new$Date),
                  max(Corona_new$Date),
                  value = max(Corona_new$Date),
                  step = 30,
                  animate = animationOptions(interval = 5000, loop = FALSE)),
  ),
  
  absolutePanel(top = 23, right = 20,
                valueBoxOutput('total_cases', width = 18)
  ),
  absolutePanel(top = 110, right = 20,
                valueBoxOutput('total_deaths', width = 18)
  ),
  absolutePanel(top = 197, right = 20,
                valueBoxOutput('total_vacc', width = 18)
  ),
  absolutePanel(bottom = 20, right = 20,
                box(width = NULL,
                    height = NULL,
                    title = NULL,
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = T,
                    tabBox(width = NULL,
                           title = textOutput("current_date"), 
                           height = NULL,
                           
                           tabPanel("Totals",
                                    DTOutput("total_table")
                           ),
                           tabPanel("Global Cases (log)",
                                    plotlyOutput("world_cases", height = "200px")
                             
                           ),
                           tabPanel("Top Cases", 
                                    plotlyOutput("cases_bar", height = "200px")
                           ),
                           tabPanel("Top Deaths", 
                                    plotlyOutput("deaths_bar", height = "200px")
                           ),
                           tabPanel("Notes",
                             p("COVID-19 data for this dashboard is from https://ourworldindata.org/"),
                             p("You can find the code for this dashboard at https://github.com/Rguillermo21"),
                             p("Some of the country borders are disputed and may not be accurately reflected by the mapping polygons."),
                             p("Ex. Western Sahara is disputed by Morocco"),
                             p(""),
                             p("Dashboard last updated: ", paste0(last(Corona_new$Date)))
                           )
                    )
                )
  )
)


sidebar <- dashboardSidebar(disable = TRUE) # Disable for more screen real estate

ui <- dashboardPage(header = header,
                    sidebar = sidebar,
                    body = body)

server = function(input, output){
  ## Reactive DF that will filter whatever the chosen time is on slider
  covid_sp <- reactive({
    Corona_new %>% filter(Date == input$time)
    
  })
  
  covid_globaldf <- reactive({
    corona_global %>% filter(Date == input$time)
  })
  
  output$chloro_map <- renderLeaflet({
    
    leaflet() %>%
      addProviderTiles(provider = "CartoDB.DarkMatter",
                       providerTileOptions(minZoom = 1, maxZoom = 3))  %>%
      setView( lat=38,
               lng=25 ,
               zoom=2.5) %>%
      addLegend( pal=mp,
                 values= Corona_new$total_cases,
                 opacity=0.85,
                 title = "New Cases",
                 position = "bottomleft" )})
  
  
  observe ({
    if(!is.null(input$time)){
      world_spdf@data <- left_join(world_spdf@data, covid_sp(), by = "NAME")
      tt <- paste(
        "Country: ", world_spdf@data$NAME,"<br/>",    
        "New Cases: ", world_spdf@data$new_cases, "<br/>",        
        "Total Cases: ", world_spdf@data$total_cases, "<br/>",
        "Total Deaths: ", world_spdf@data$total_deaths, "<br/>",  
        "Population: ", world_spdf@data$population, 
        sep="") %>%
        lapply(htmltools::HTML)
      
      ## Use Proxy so we don't have to keep redrawing base map layer
      ## We are only changing the polygon data on top of map
      leafletProxy("chloro_map", data = world_spdf) %>%
        clearShapes() %>%
        addPolygons(
          fillColor = ~mp(new_cases),
          stroke=TRUE,
          fillOpacity = 0.9,
          color="grey",
          weight=0.3,
          label = tt,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal",
                         padding = "3px 8px"),
            textsize = "13px",
            direction = "auto"
          ))} })
  
  output$total_cases <- renderValueBox({
    valueBox(width = 4,
             subtitle = "Total Cases",
             color = 'orange',
             formatC(as.numeric(covid_globaldf() %>% filter(location == "World") %>% summarize(total_cases = total_cases)), 
                     digits = 0, big.mark = ",", big.interval = 3, format = "f"))
  })

  output$total_deaths <- renderValueBox({
    valueBox(width = 4,
             subtitle = "Total Deaths",
             color = 'red',
             formatC(as.numeric(covid_globaldf() %>% filter(location == "World") %>% summarize(total_deaths = total_deaths)), 
                     digits = 0, big.mark = ",", big.interval = 3, format = "f"))
  })
  
  
  output$total_vacc <- renderValueBox({
    valueBox(width = 4,
             subtitle = "Total Vaccinations",
             color = 'blue',
             formatC(as.numeric(covid_globaldf() %>% filter(location == "World") %>% summarize(total_vaccinations = total_vaccinations)), 
                     digits = 0, big.mark = ",", big.interval = 3, format = "f"))
  })
  
  output$world_cases <- renderPlotly({world_cases})
  

  output$total_table <- renderDT({covid_sp() %>% 
                                            group_by(NAME) %>%
                                            summarize(new_cases = sum(new_cases, na.rm = TRUE),
                                                      total_cases = sum(total_cases, na.rm = TRUE),
                                                      total_vaccinations = sum(total_vaccinations, na.rm = TRUE),
                                                      total_deaths = sum(total_deaths, na.rm = TRUE)) %>%
                                                      arrange(desc(new_cases))},
                                        
                                        colnames = c("Country", "New Cases", "Total Cases", "Total Vaccinations", "Total Deaths"), 
                                        rownames = FALSE,
                                        options = list(scrollY = '175px',
                                                       scrollX = FALSE,
                                                       searching = FALSE,
                                                       lengthChange = FALSE,
                                                       pageLength = nrow(covid_sp),
                                                       bPaginate = FALSE,
                                                       bInfo = FALSE,
                                                       columnDefs = list(list(className = 'dt-left',
                                                                              targets = 0:1))))
  
  
  output$cases_bar <- renderPlotly({ggplotly(covid_sp() %>%
                                               group_by(NAME) %>%
                                               summarize(Date = Date, new_cases = new_cases, NAME) %>%
                                               arrange(desc(new_cases)) %>%
                                               slice(1:5) %>%
                                               ggplot(aes(x = Date, y = new_cases, fill = NAME)) +
                                               geom_col(position = position_dodge(1.1)) +
                                               dark_theme +
                                               scale_fill_manual(values =c('#fff7ec',
                                                                           '#fee8c8', 
                                                                           '#fdbb84', 
                                                                           "#fc8d59" , 
                                                                           "#ef6548"  )),
                                             height = 210) %>%
      config(displayModeBar = F) %>%
      layout(hoverlabel = list(
        bgcolor = '#2a2928'),
        autosize = F,
        margin = m,
        font = list(color = "#ffffff"),
        legend = list(title = list(text = "")))})
  
  output$deaths_bar <- renderPlotly({ggplotly(covid_sp() %>%
                                               group_by(NAME) %>%
                                               summarize(Date = Date, new_deaths = new_deaths, NAME) %>%
                                               arrange(desc(new_deaths)) %>%
                                               slice(1:5) %>%
                                               ggplot(aes(x = Date, y = new_deaths, fill = NAME)) +
                                               geom_col(position = position_dodge(1.1)) +
                                               dark_theme +
                                               scale_fill_manual(values =c('#fff7ec',
                                                                           '#fee8c8', 
                                                                           '#fdbb84', 
                                                                           "#fc8d59" , 
                                                                           "#ef6548" )),
                                              height = 210) %>%
      config(displayModeBar = F) %>%
      layout(hoverlabel = list(
        bgcolor = '#2a2928'),
        autosize = F,
        margin = m,
        font = list(color = "#ffffff"),
        legend = list(title = list(text = "")))})
  
  output$current_date <- renderText({paste0(input$time)})
  
}

shinyApp(ui, server)
