####
library(rjson)
library(shinydashboard)
library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(highcharter)
library(lubridate)
library(stringr)
library(withr)
library(treemap)
library(DT)
library(shinyBS)
library(shinyjs)
library(WDI)
library(geosphere)
library(magrittr)
library(shinycssloaders)
options(spinner.color="#006272")
# options(shiny.trace=TRUE)
library(timevis)
# library(comtradr)
library(memoise)
library(networkD3)
library(promises)
library(future)

## load data
# pr_p <- readRDS("pr_p.rds") ## pre-defined commodity list form SNZ
# labels <- read_xlsx("labels.xlsx")

## setup global variables
# max_date <- max(pr_p$date)


## load functions
source('helper_funs.R')
source('share_load.R')

## build ui.R -----------------------------------
## 1. header -------------------------------
header <- 
  dashboardHeader( title = HTML("Dashboard"), 
                   disable = FALSE, 
                   titleWidth  = 200
                   # dropdownMenuCustom( type = 'message',
                   #                     customSentence = customSentence,
                   #                     messageItem(
                   #                       from = "TR_SharedMailbox@mbie.govt.nz",#'Feedback and suggestions',
                   #                       message =  "",#paste0("TR_SharedMailbox@mbie.govt.nz" ),
                   #                       icon = icon("envelope"),
                   #                       href = "mailto:TR_SharedMailbox@mbie.govt.nz"
                   #                     ),
                   #                     icon = icon('comment')
                   # ),
                   # dropdownMenuCustom( type = 'message',
                   #                     customSentence = customSentence_share,
                   #                     icon = icon("share-alt"),
                   #                     messageItem(
                   #                       from = 'Twitter',
                   #                       message = "",
                   #                       icon = icon("twitter"),
                   #                       href = "https://twitter.com/intent/tweet?url=http%3A%2F%2Ftradeintelligence.mbie.govt.nz&text=New%20Zealand%20Trade%20Intelligence%20Dashboard"
                   #                     ),
                   #                     messageItem(
                   #                       from = 'Facebook',
                   #                       message = "",
                   #                       icon = icon("facebook"),
                   #                       href = "https://www.facebook.com/sharer/sharer.php?u=http%3A%2F%2Ftradeintelligence.mbie.govt.nz"
                   #                     ),
                   #                     messageItem(
                   #                       from = 'Google+',
                   #                       message = "",
                   #                       icon = icon("google-plus"),
                   #                       href = "https://plus.google.com/share?url=http%3A%2F%2Ftradeintelligence.mbie.govt.nz"
                   #                     ),
                   #                     messageItem(
                   #                       from = 'Sina Weibo',
                   #                       message = "",
                   #                       icon = icon("weibo"),
                   #                       href = "http://service.weibo.com/share/share.php?url=http://example.com&appkey=&title=New%20Zealand%20Trade%20Intelligence%20Dashboard%20http%3A%2F%2Ftradeintelligence.mbie.govt.nz&pic=&ralateUid=&language=zh_cn"
                   #                     ),
                   #                     messageItem(
                   #                       from = 'Pinterest',
                   #                       message = "",
                   #                       icon = icon("pinterest-p"),
                   #                       href = "http://pinterest.com/pin/create/button/?url=http%3A%2F%2Ftradeintelligence.mbie.govt.nz&media=&description=New%20Zealand%20Trade%20Intelligence%20Dashboard"
                   #                     ),
                   #                     messageItem(
                   #                       from = 'LinkedIn',
                   #                       message = "",
                   #                       icon = icon("linkedin"),
                   #                       href = "http://www.linkedin.com/shareArticle?mini=true&url=http%3A%2F%2Ftradeintelligence.mbie.govt.nz&title=New%20Zealand%20Trade%20Intelligence%20Dashboard"
                   #                     ),
                   #                     messageItem(
                   #                       from = 'Tumblr',
                   #                       message = "",
                   #                       icon = icon("tumblr"),
                   #                       href = "http://www.tumblr.com/share?v=3&u=http%3A%2F%2Ftradeintelligence.mbie.govt.nz&t=New%20Zealand%20Trade%20Intelligence%20Dashboard"
                   #                     )
                   # )
                   
  )

header$children[[2]]$children[[2]] <- header$children[[2]]$children[[1]]
header$children[[2]]$children[[1]] <- tags$a(href='https://www.cnb.cz',
                                             tags$img(src='logo_cnb.png', height='35', align = 'left'),
                                             # imageOutput('test.png'),
                                             target = '_blank')



## 2. siderbar ------------------------------
siderbar <- 
  dashboardSidebar( 
    width = 200,
    sidebarMenu(
      id = 'sidebar',
      style = "position: relative; overflow: visible;",
      #style = "position: relative; overflow: visible; overflow-y:scroll",
      #style = 'height: 90vh; overflow-y: auto;',
      ## 1st tab show the Main dashboard -----------
      menuItem("Main Dashboard", tabName = 'dashboard', icon = icon('dashboard')),
      
      ## add conditional panel to show more
      # conditionalPanel( "input.sidebar === 'dashboard'",
      #                   actionButton("btn_show_more",
      #                                paste0(' Show more details'),
      #                                icon = icon('chevron-circle-down'),
      #                                style='padding-top:0px; padding-bottom:0px;padding-left:3px;padding-right:3px; '
      #                                ) 
      #                   ),
      
      
      ## 5th tab Data source, definition , i.e., help ---------------
      menuItem( "FAQs", tabName = 'help', icon = icon('question-circle') ),
      
      ## 6th tab monthly update ----------------------
      menuItem( "Stats NZ Releases", tabName = 'monthly_update', icon = icon('bell'),
                badgeLabel = "new", badgeColor = "green" )
    )
  )

## 3. body --------------------------------
body <- dashboardBody( 
  ## 3.0. CSS styles in header ----------------------------
  tags$head(
    # ## JS codes
    # tags$script(src = "fixedElement.js" ),
    # tags$style(HTML(".scroller_anchor{height:0px; margin:0; padding:0;}; 
    #                  .scroller{background: white; 
    #                   border: 1px solid #CCC; 
    #                   margin:0 0 10px; 
    #                   z-index:100; 
    #                   height:50px; 
    #                   font-size:18px; 
    #                   font-weight:bold; 
    #                   text-align:center; 
    #                  width:500px;}")),
    
    #tags$script(src = "world.js" ),
    # tags$script("document.title = 'Česká národní banka - analytický dashboard'"),
    
    ### Styles 
    tags$style(HTML(".small-box {height: 65px}")),
    # tags$style(HTML(".fa { font-size: 20px; }")),
    # tags$style(HTML(".glyphicon { font-size: 20px; }")),  ## use glyphicon package
    # tags$style(HTML(".fa-dashboard { font-size: 2px; }")),
    # # tags$style(HTML(".glyphicon-euro { font-size: 15px; }")),
    # tags$style(HTML(".fa-globe { font-size: 20px; }")),
    # tags$style(HTML(".fa-barcode { font-size: 20px; }")),
    # tags$style(HTML(".tab-content { padding-left: 20px; padding-right: 30px; }")) ,
    # tags$style(HTML(".fa-wrench { font-size: 15px; }")),
    # tags$style(HTML(".fa-refresh { font-size: 15px; }")),
    # tags$style(HTML(".fa-search { font-size: 15px; }")),
    # tags$style(HTML(".fa-comment { font-size: 20px; }")),
    # tags$style(HTML(".fa-share-alt { font-size: 20px; }")),
    # tags$style(HTML(".fa-envelope { font-size: 20px; }")),
    # tags$style(HTML(".fa-question-circle { font-size: 20px; }")),
    # tags$style(HTML(".fa-chevron-circle-down { font-size: 15px; }")),
    # tags$style(HTML(".fa-bell { font-size: 17px; }")),
    # tags$style(HTML(".fa-check { font-size: 14px; }")),
    # tags$style(HTML(".fa-times { font-size: 14px; }")),
    # tags$style(HTML(".fa-user-helmet-safety { font-size: 14px; }")),
    # 
    
    #tags$style(HTML(".fa-twitter { font-size: 10px; color:red;}")),
    #tags$style(HTML(".fa-facebook { font-size: 10px; color:red;}")),
    #tags$style(HTML(".fa-google-plus { font-size: 10px; color:red;}")),
    #tags$style(HTML(".fa-pinterest-p { font-size: 10px; color:red;}")),
    #tags$style(HTML(".fa-linkedin { font-size: 10px; color:red;}")),
    #tags$style(HTML(".fa-tumblr { font-size: 10px; color:red;}")),
    
    ## modify the dashboard's skin color
    tags$style(HTML('
                       /* logo */
                       .skin-blue .main-header .logo {
                       background-color: #ECF0F5;
                       color:#000000;
                       # color:#2426A8;
                       font-size: 22px;
                       }

                       /* logo when hovered */
                       .skin-blue .main-header .logo:hover {
                       background-color: #2426A8;
                       }

                       /* navbar (rest of the header) */
                       .skin-blue .main-header .navbar {
                       background-color: #2426A8;
                       }
                       
                       /* main sidebar */
                       .skin-blue .main-sidebar {
                       background-color: #2426A8;
                       }

                       /* active selected tab in the sidebarmenu */
                       .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                       background-color: #2426A8;
                                 }
                       ')
    ),
    
    ## modify icon size in the sub side bar menu
    tags$style(HTML('
                       /* change size of icons in sub-menu items */
                      .sidebar .sidebar-menu .treeview-menu>li>a>.fa {
                      font-size: 15px;
                      }

                      .sidebar .sidebar-menu .treeview-menu>li>a>.glyphicon {
                      font-size: 13px;
                      }

                      /* Hide icons in sub-menu items */
                      .sidebar .sidebar-menu .treeview>a>.fa-angle-left {
                      display: none;
                      } 
                      '
    )) ,
    
    tags$style( HTML("hr {border-top: 1px solid #000000;}") ),
    
    ## to not show error message in shiny
    tags$style( HTML(".shiny-output-error { visibility: hidden; }") ),
    tags$style( HTML(".shiny-output-error:before { visibility: hidden; }") ),
    
    ## heand dropdown menu size
    #tags$style(HTML('.navbar-custom-menu>.navbar-nav>li>.dropdown-menu { width:100px;}'))
    tags$style(HTML('.navbar-custom-menu>.navbar-nav>li:last-child>.dropdown-menu { width:10px; font-size:10px; padding:1px; margin:1px;}')),
    tags$style(HTML('.navbar-custom-menu> .navbar-nav> li:last-child > .dropdown-menu > h4 {width:0px; font-size:0px; padding:0px; margin:0px;}')),
    tags$style(HTML('.navbar-custom-menu> .navbar-nav> li:last-child > .dropdown-menu > p {width:0px; font-size:0px; padding:0px; margin:0px;}'))
  ),
  
  ## 3.1 Dashboard body --------------
  tabItems(
    ## 3.1 Main dashboard ----------------------------------------------------------
    tabItem( tabName = 'dashboard',
             ## contents for the dashboard tab
             div(id = 'main_wait_message',
                 h1('Načítám data, prosím čekejte',
                    style = "color:#2426A8" , align = "center" ) ,
                 tags$hr()
             ),
             
             # 1.1 Industry boxes ---------------------------
             div(class = 'scroller_anchor'),
             div(class = 'scroller', ) ,
             
             

              h1(paste0("Zaměstnanost a mzdy ", format(max_date, format = "%B %Y") ),
                 style = "color: #2426A8") ,
              fluidRow(
                valueBoxOutput("PrZam",width = 3) %>% withSpinner(type=4),
                valueBoxOutput("PrMzdy",width = 3)
              ),
             # 
             # h2(paste0("Goods")),
             # fluidRow(
             #   valueBoxOutput("ExGBox") ,
             #   valueBoxOutput("ImGBox") ,
             #   valueBoxOutput("BlGBox")
             # ),
             # 
             # h2(paste0("Services")),
             # fluidRow(
             #   valueBoxOutput("ExSBox") ,
             #   valueBoxOutput("ImSBox") ,
             #   valueBoxOutput("BlSBox")
             # ) ,
             
             ## 1.2 Time serise plot ----------------------------------------
             h2(paste0("Meziměsíční a mezirční změny jednotlivých odvětví")),
             selectizeInput(inputId = "select_industry",
                            "Zvolte až 5 odvětví", 
                            choices =  unique(labels$label), 
                            selected = NULL,  width = 1000, 
                            multiple = T,
                            options = list(maxItems = 5, plugins= list('remove_button'))),
             
             # h2(paste0("New Zealand trade over the past 20 years")),
             fluidRow( column( width = 6,h4("Goods and services trade", align = 'center'), highchartOutput('pr_line_mom') ),
                       column( width = 6,h4("Goods and services trade", align = 'center'), highchartOutput('pr_line_mom2') ))
             # 
             # 
             # ## 1.3 Table shows growth rate ---------------------------------
             # h2(paste0("Short, medium, and long term growth")),
             # p("Compound annual growth rate (CAGR) for the past 1, 5, 10 and 20 years") ,
             # #fluidRow( h2(paste0("Short, medium, and long term growth")),
             # #          p("Compound annual growth rate (CAGR) for the past 1, 5, 10 and 20 years") ),
             # fluidRow( dataTableOutput('GrowthTab')  )
             
             
             
    )

))




## put UI together --------------------
ui <- 
  dashboardPage(header, siderbar, body )