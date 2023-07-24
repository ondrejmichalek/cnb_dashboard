####
library(rjson)
library(shinydashboard)
library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggtext)
library(plotly)
library(kableExtra)
library(highcharter)
library(lubridate)
library(stringr)
library(scales)
library(zoo)
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
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")

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
                                             tags$img(src='logo_cnb.png', height='35', align = 'right'),
                                             # imageOutput('test.png'),
                                             target = '_blank')



## 2. siderbar ------------------------------
siderbar <- 
  dashboardSidebar( 
    width = 200,
    sidebarMenu(
      id = 'sidebar',
      style = "position: relative; overflow: visible; font-size: 17px",
      #style = "position: relative; overflow: visible; overflow-y:scroll",
      #style = 'height: 90vh; overflow-y: auto;',
      ## 1st tab show the Main dashboard -----------
      menuItem("Ekonomická aktivita", tabName = 'ea', icon = icon('dashboard'),
               menuSubItem("HDP", tabName = "hdp", icon = icon('money-bill')),
               menuSubItem("Průmysl", tabName = "prumysl", icon = icon('truck-fast')),
               menuSubItem("Nefinanční podniky", tabName = "nefinancni_podniky", icon = icon('square-nfi'))),
      
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
  tags$script(HTML("
        var openTab = function(tabName){
          $('a', $('.sidebar')).each(function() {
            if(this.getAttribute('data-value') == tabName) {
              this.click()
            };
          });
        }
      ")),
  tags$head(
    # ## JS codes
    tags$script(src="js/index.js"),
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
    tags$style(HTML(".tab-content { padding-left: 25px; padding-right: 30px; font-size: 15px; font-family: Arial;}")) ,
    tags$style(HTML("hr {border-top: 1px solid #000000;}")),
    tags$style(HTML("h1 {padding-left: 0px; padding-right: 30px; color: #2426A8; font-weight: bold;}")),
    tags$style(HTML("ol {padding-left: 15px; padding-right: 30px; color: #000000; font-family: Arial;}")),
    # tags$style(HTML("brsmall {margin-bottom: 0em; display: block;}")),
    # tags$style(HTML("div {padding-left: 15px; padding-right: 30px; }")),
    
    # tags$style(HTML(".fa { font-size: 20px; }")),
    # tags$style(HTML(".glyphicon { font-size: 20px; }")),  ## use glyphicon package
    # tags$style(HTML(".fa-dashboard { font-size: 2px; padding-left: 20px}")),
    # # tags$style(HTML(".glyphicon-euro { font-size: 15px; }")),
    # tags$style(HTML(".fa-globe { font-size: 20px; }")),
    # tags$style(HTML(".fa-barcode { font-size: 20px; }")),
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
    
    # color of background original #ECF0F5
    # other dashboard skin colors
    # https://stackoverflow.com/questions/41284845/change-sidebar-menu-item-color-in-r-shiny
    ## modify the dashboard's skin color
    tags$style(HTML('
                       /* logo */
                       .skin-blue .main-header .logo {
                       background-color: #FFFFFF;
                       color:#2426A8;
                       # color:#2426A8;
                       font-size: 22px;
                       }

                       /* logo when hovered */
                       .skin-blue .main-header .logo:hover {
                       background-color: #FFFFFF;
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
                       background-color: #000000;
                       }
                       
                       /* other links in the sidebarmenu */
                       .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                       background-color: #2426A8;
                       } 
                       
                       # /* other links in the sidebarmenu when hovered */
                       #  .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                       #  background-color: rgb(232,245,251);
                       #  color: rgb(0,144,197);font-weight: bold;
                       #  }
                                 
                       /* body */
                       .content-wrapper, .right-side {
                       background-color: #FFFFFF;
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
    tabItem( tabName = 'hdp',
             ## contents for the dashboard tab
             div(id = 'main_wait_message',
                 h1('Načítám data, prosím čekejte',
                    style = "color:#2426A8" , align = "center" ) ,
                 tags$hr()
             ),
             
             ## 1.1 GDP ---------------------------
             # div(class = 'scroller_anchor'),
             # div(class = 'scroller', ) ,
             
      
             # 1. Ekonomická aktivita ---------------
             ## 1.1 HDP
             
             h1(paste0("Hrubý domácí produkt - výhled pro ", quarter(max_date_ea),".Q ", year(max_date_ea) ),
                style = "color: #2426A8"),
             
             # div(id = "table",
             #     tags$table(style = "border: 1px solid black; padding: 1%; width: 100%;",
             #                tags$tr(
             #                  tags$th("Car Name"),
             #                  tags$th("MPG"),
             #                  tags$th("CYL"),
             #                  tags$th("DISP"),
             #                  tags$th("HP")
             #                  
             #                ),
             #     )),
      
             div(id = 'ekonomicka_aktivita_uvod',
                     ea_uvod()),
             
             fluidRow(column(width = 12, plotlyOutput('nowcast_hdp_all_gg'))),
             
             div(id = 'ea_p2',
                 ea_p2()),
             
             fluidRow(column(width = 12, plotlyOutput('nowcast_dfm_all_gg'))),
             
             div(id = 'ea_p3',
                 ea_p3()),
            
             # 
             # ## 1.3 Table shows growth rate ---------------------------------
             # h2(paste0("Short, medium, and long term growth")),
             # p("Compound annual growth rate (CAGR) for the past 1, 5, 10 and 20 years") ,
             # #fluidRow( h2(paste0("Short, medium, and long term growth")),
             # #          p("Compound annual growth rate (CAGR) for the past 1, 5, 10 and 20 years") ),
             # fluidRow( dataTableOutput('GrowthTab')  )
             
             
             
    ),
    
    tabItem( tabName = 'prumysl',
             ## contents for the dashboard tab
             div(id = 'wait_message_prumysl',
                 h1('Načítám data, prosím čekejte',
                    style = "color:#2426A8" , align = "center" ) ,
                 tags$hr()),
             

            # 2. Průmysl --------------------------------------------------------------
            
            h1(paste0("Průmysl ČR"),
               style = "color: #2426A8"),
            
            div(id = 'pr_uvod',
                pr_uvod()),
            
            ## 2.1 Základní ukazatele průmyslu ČSÚ
            
            h2(paste0("Základní ukazatele ČSÚ")),
            
            ## 2.1.1 Zvolte ukazatel 
            
            fluidRow(column(width = 3, selectizeInput(inputId = "pr_select_var",
                                                      "Zvolte ukazatel", 
                                                      choices =  unique(pr$variable), 
                                                      selected = NULL,
                                                      multiple = F)),
                     column(width = 3, selectizeInput(inputId = "pr_select_ind",
                                                      "Zvolte až 5 odvětví", 
                                                      choices =  unique(pr$label), 
                                                      selected = unique(pr$label)[1], 
                                                      multiple = T, width = 1000,
                                                      options = list(maxItems = 5, plugins= list('remove_button')))),
                     column(width = 3, selectizeInput(inputId = "pr_select_trans",
                                                      "Zvolte transformaci dat", 
                                                      choices =  unique(pr$trans), 
                                                      selected = NULL,
                                                      multiple = F)),
                     column(width = 3, selectizeInput(inputId = "pr_select_unit",
                                                      "Zvolte jednotku", 
                                                      choices =  unique(pr$unit), 
                                                      selected = NULL,
                                                      multiple = F))),
            
            fluidRow(column(width = 12,sliderInput("pr_date_slider",
                                                   "",
                                                   min = min(pr$date),
                                                   max = max(pr$date),
                                                   step = 31, width = 1000,
                                                   value=c(as.Date("2018-01-01"),max(pr$date)),
                                                   timeFormat="%m/%y"))),
            
            
            fluidRow(column(width = 12, plotlyOutput('pr_multichart'))),
            


            h2(paste0("Zaměstnanost a mzdy ", format(max_date_pr, format = "%B %Y"), " - meziročně" ),
               style = "color: #2426A8") ,
            fluidRow(
              valueBoxOutput("PrZam",width = 3),
              valueBoxOutput("PrMzdy",width = 3)
              ),
            
            h1(paste0("Mezinárodní porovnání produkce v průmyslu "),
               style = "color: #2426A8") ,
            
            fluidRow(column(width = 4, selectizeInput(inputId = "pr_date_eurostat",
                                                      "Zvolte datum", 
                                                      choices =  unique(pr_p_eurostat$date), 
                                                      selected = unique(pr_p_eurostat$date)[1])),
                     column(width = 4, selectizeInput(inputId = "pr_eurostat_select_ind",
                                                      "Zvolte odvětví", 
                                                      choices =  unique(pr$label), 
                                                      selected = unique(pr$label)[1])),
                     column(width = 4, selectizeInput(inputId = "pr_eurostat_select_trans",
                                                      "Zvolte transformaci dat", 
                                                      choices =  unique(pr$trans), 
                                                      selected = NULL,
                                                      multiple = F))),
            
            
            fluidRow(column(width = 12, plotlyOutput('pr_eurostat_map'))),
            
            
            ),
             
    
    tabItem( tabName = 'nefinancni_podniky',
             ## contents for the dashboard tab
             div(id = 'wait_message_nefinancni_podniky',
                 h1('Načítám data, prosím čekejte',
                    style = "color:#2426A8" , align = "center" ) ,
                 tags$hr()),
             
             
             # 3. Nefinanční podniky --------------------------------------------------------------
             
             h1(paste0("Nefinanční podniky"),
                style = "color: #2426A8"),
             
             div(id = 'np_uvod',
                 np_uvod()),
             
             
             ## 3.1 Základní ukazatele P604
             
             h2(paste0("Základní ukazatele z individuálních dat o nefinančních podnicích ČSÚ (výkaz P 6-04)")),
             
             ## 3.1.1 Zvolte ukazatel 
             
             fluidRow(column(width = 4, selectizeInput(inputId = "np_select_var",
                                                       "Zvolte ukazatel", 
                                                       choices =  unique(p604_sa$l_v), 
                                                       selected = "Investice",
                                                       multiple = F)),
                      column(width = 4, selectizeInput(inputId = "np_select_odv",
                                                       "Zvolte odvětví", 
                                                       choices =  unique(p604_sa$label1), 
                                                       selected = unique(p604_sa$label1)[1],
                                                       multiple = F)),
                      column(width = 4, selectizeInput(inputId = "np_select_trans",
                                                       "Zvolte transformaci", 
                                                       choices =  unique(p604_sa$trans), 
                                                       selected = unique(p604_sa$trans)[1],
                                                       multiple = F))),

             
             fluidRow(column(width = 12,sliderInput("np_date_slider",
                                                    "",
                                                    min = min(p604_sa$date),
                                                    max = max(p604_sa$date),
                                                    width = 1000,
                                                    value=c(as.Date("2018-01-01"),
                                                            max(p604_sa$date)),
                                                    timeFormat = "%m/%y"))),
             # sliderInput(inputId = 'slider', 
             #             label = div(style='width:300px;', 
             #                         div(style='float:left;', 'sooner'), 
             #                         div(style='float:right;', 'later')), 
             #             min = 0, max = 10, value = 5, width = '300px'),
             
             fluidRow(column(width = 12, plotlyOutput('np_multichart'))),
             
             # h3(paste0("Definice zvolené proměnné")),
             
             fluidRow(column(width = 12, tableOutput("p604_def_table")))
             
             
             
             
                 ),

    tabItem( tabName = 'help',
             ## 3.1 Help text first ---------------------
             div(id = 'test',
                 howto_ci() ))



))




## put UI together --------------------
ui <- dashboardPage(header, siderbar, body )