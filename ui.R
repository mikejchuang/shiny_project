library(shiny)
library(shinydashboard)
library(DT)


shinyUI(dashboardPage( skin='black',
dashboardHeader( title=('Mike Chuang 311 Data Analysis')),

dashboardSidebar(
  sidebarUserPanel(name= "Mike Chuang", image='./MichaelC.jpg'),
  sidebarMenu(id='sideBarMenu',
    menuItem("Summary Information", tabName = "summary", icon = icon('book')),
    menuItem("Interactive Chart", tabName = "charts", icon = icon('line-chart')),
    menuItem("Interactive Map", tabName = "leafheat", icon = icon("fire")),
    menuItem("Dataset", tabName = "data", icon = icon("database"))),
  
  conditionalPanel("input.sideBarMenu == 'leafheat'",
          selectizeInput('borough','Borough', c('BRONX','QUEENS','BROOKLYN','MANHATTAN'), multiple = TRUE,selected = "MANHATTAN" ),
          selectizeInput('month','Month', c(1:12), multiple = TRUE,selected = c(1:12)),
          selectizeInput('weekday','Weekday', weekday_, multiple = TRUE,selected = c('Monday','Tuesday','Wednesday','Thursday','Friday' )),
          selectizeInput('hour','Hour', c(0:23), multiple=TRUE, selected = c(9:17) ),
          selectizeInput('complaint_type','Complaint Type',complaint_, multiple = TRUE,selected = complaint_ )),
  
  conditionalPanel("input.sideBarMenu == 'charts'",
                   selectizeInput('CDborough','Borough', c('BRONX','QUEENS','BROOKLYN','MANHATTAN'), multiple = TRUE,selected = "MANHATTAN" ),
                   selectizeInput('CDcomplaint_type','Complaint Type',complaint_, multiple = TRUE,selected = complaint_ ),
                   selectInput('timescale','Time Scale',
                                choices = c("By month" = 'month', "By week" = 'weeknum', "By weekday" = 'weekday', 'By day'= 'yday', 'By Hour' = 'hour'), 
                                selected = 'month'))
            ),
dashboardBody(
  tags$head(tags$style(HTML('
        .skin-black .main-header .logo {
                           background: black; 
                            color:white;
                            font-size: 10pt;
                            }
        .skin-black .main-header .logo:hover {
                            background: rgb(85,0,0);
                            }
        .skin-black .left-side, .skin-black .main-sidebar, .skin-black .wrapper{
                             background: -webkit-linear-gradient( black , white); 
                             }
        .skin-black .main-header .navbar{
                             background: -webkit-linear-gradient(left, black, white); 
                            }
        .skin-black .user-panel>.info{
                      display:none;
                             }
            .user-panel>.image>img{
                        max-width: 125px;
                      height: 125px;
                      border-radius:20%;
                       margin-left: 40px;

                              }
                            '))),
  
  tabItems(
    tabItem(tabName = "summary",
            fluidRow(box(
                         h2(tags$b("Background Information on 311 Data Analysis")),
                        tags$hr(),
                         h4('This project was inspired by.'),
                         p('This is a paragraph'),width=12,solidHeader = TRUE, status='primary')
                    )
            ),
    
    tabItem(tabName = "charts",
            fluidRow(box(plotlyOutput(outputId = "charts"), width=12
                                                  )),
            fluidRow(box(infoBoxOutput("chartAvgBox", width=12),solidHeader = TRUE), width=6,
                    box(infoBoxOutput("chartMBox", width=12), solidHeader = TRUE,width=6)
            )
            ),
          
    tabItem(tabName = "leafheat",
            fluidRow(infoBoxOutput("heatTotBox", width=6), infoBoxOutput("heatPropBox", width=6)
            ),
            fluidRow(box(
              leafletOutput("leafheat"),
              width = 12)),
            fluidRow(box(DT::dataTableOutput("topHeatTable"), width=12))
            ),
  
    tabItem(tabName = "data",
            # datatable
            fluidRow(box(DT::dataTableOutput("table"),
                         width=12)))
))
))

