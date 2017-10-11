
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
          selectizeInput('complaint_type','Complaint Type',complaint_, multiple = TRUE, selected = 'tree' )),
  
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
                        h2(tags$b("Background Information on 2015 311 Data Analysis")),
                        tags$hr(),
                         h4('Welcome to my shiny app, the main goals of my project were to provide an interactive web dashboard to explore a large dataset \
                         Taking inspiration from:', tags$a(href="https://www.youtube.com/watch?v=6xsvGYIxJok", "this video,"),'I also wanted to understand how NYC Open Data could provide a deeper understanding of the dynamic city we live in '),
                          tags$hr(),
                         p('The dataset initially contained over 2 million rows--when I first inspected the data I noticed over 250 complaint types coming from all of the various agencies.\
                           My first approach to consolidating this data was to group together as many similar complaints as \
                           possible, from which I narrowed it down to display the top ten. To illustrate, the category \"tree\" is an aggregation of several complaint_types found such as:',tags$li('Damaged Tree'),
                            tags$li("Overgrown Tree/Branches"), tags$li("Dead Tree"), tags$li("New Tree Request")),
                            tags$br(),
                        tags$p('After processing this data set, I displayed the data through two visualizations. The Interactive Chart provides a user a way to identify unique trends over time by filtering \
                                by complaint type or borough. To further investigate a discovered trend, the Interactive Map can be used to similarly filter and then view geographic representations of incident activity.\
                                More specifically, a user can zoom in and inspect the top 50 most reported addresses both on the map and within the data table beneath it.'), tags$br(),
                        tags$p('Lastly, this app is definitely a work in progress and some possible directions I would like to extend it would be:', tags$li("Add correlations between pricing (predicting rent) or crime data (proving broken windows theory) and 311 incident volume"),
                                   tags$li("Improve performance at higher data volumes"), tags$li('Provide statistics based on the agency that reported the incident')),width=12, status='primary')
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

