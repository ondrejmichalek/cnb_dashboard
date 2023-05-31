
## load
source('share_load.R')

## 000 user input setup. Please pay close attention and change -----
## un comtrade max year, you can find it on https://comtrade.un.org/data/da
tmp_un_comtrade_max_year <- year(Sys.time()) - 2 # 2 years of lag

## build server.R
server <- 
  function(input, output, session) {
    ## I. Main dashboard -----------------------------
    i_prog <- 1
    tot_step <- 25
    
    # 1. Value boxes  ---------------------------------------------------------
    ## try add progress bars
    withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
      # Increment the progress bar, and update the detail text.
      incProgress( i_prog/tot_step, detail = NULL)
      ##Sys.sleep(0.1)
      
    })
    i_prog <- i_prog + 1
    
    tmp_zam <- 8.9
    tmp_mzdy <- 7.0
    
    
  
    ## build employment and wages
      output$PrZam <- renderValueBox({
        valueBox(
          VB_style( paste0(format(tmp_zam,big.mark=' ', decimal.mark = ","), " %"),  "font-size: 80%;"  ),
          VB_style( paste0()  ),
          tags$i(class = "fa-solid fa-person-digging", style="font-size: 37px"), #icon
          # icon = icon('dashboard'), #icon("sign-in"),
          color = ifelse(tmp_zam >0,"green", "red")
        )
      })
      
      output$PrMzdy <- renderValueBox({
        valueBox(
          VB_style( paste0(format(tmp_mzdy,big.mark=' ', decimal.mark = ","), " %"),  "font-size: 80%;"  ),
          VB_style( paste0()  ),
          tags$i(class = "fa-solid fa-euro-sign", style="font-size: 37px"), #icon
          color = ifelse(tmp_zam >0,"green", "red")
        )
      })
    
    
    
    # 2. Total Trade a line chart  -----------------------------------------------------------------
    withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
      # Increment the progress bar, and update the detail text.
      incProgress( i_prog/tot_step, detail = NULL)
      ##Sys.sleep(0.1)

    })
    i_prog <- i_prog + 1

    tmp_dtf <- pr_p %>%
      pivot_longer(-date, names_to = "id") %>% 
      left_join(labels, by = "id") %>% 
      group_by(label) %>% 
      arrange(date) %>% 
      mutate(yoy = value/lag(value, 12)*100-100,
             mom = value/lag(value, 1)*100-100)
  

        
    

    output$pr_line_mom <- renderHighchart({
      # highchart() %>%
      hchart(tmp_dtf %>% filter(label %in% input$select_industry), "line", 
             hcaes(x = date, y = mom, group = label))}) 
    
    output$pr_line_mom2 <- renderHighchart({
      highchart() %>%
        hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
        # hc_chart(type = 'line') %>%
        hc_add_series(data = tmp_dtf %>% filter(label %in% input$select_industry),
                      hcaes(x = date, y = mom, group = label), type = "line")
                      })

    # # 2.1 Total Trade balance a line chart  -----------------------------------------------------------------
    # withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
    #   # Increment the progress bar, and update the detail text.
    #   incProgress( i_prog/tot_step, detail = NULL)
    #   ##Sys.sleep(0.1)
    #   
    # })
    # i_prog <- i_prog + 1
    # 
    # tmp_dtf_balance <-
    #   dtf_shiny_full %>%
    #   filter( Country == 'World',
    #           #Type_ie == 'Imports',
    #           Year >= (max(Year) - 20) ) %>%
    #   group_by( Year, Country, Type_gs ) %>%
    #   mutate( Value = Value[ Type_ie == 'Exports'] - Value[ Type_ie == 'Imports']  ) %>%
    #   ungroup %>%
    #   filter( Type_ie == 'Exports' ) %>%
    #   mutate(  Type_gs = paste0(Type_gs, ' balance') )
    # 
    # tmp_dtf_balance_tot <-
    #   tmp_dtf_balance %>%
    #   group_by( Year, Country, Type_ie ) %>%
    #   summarise( Value = sum(Value, na.rm=T) ) %>%
    #   ungroup %>%
    #   mutate( Type_gs = 'Trade balance' )
    # 
    # tmp_dtf_balance %<>%
    #   bind_rows( tmp_dtf_balance_tot  ) %>%
    #   mutate( Value = round(Value/10^6) )
    # 
    # output$GSTotalBalanceLineHc <-renderHighchart({
    #   highchart() %>%
    #     hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
    #     hc_chart(type = 'line') %>%
    #     hc_series( list(name = 'Trade balance', data =tmp_dtf_balance$Value[tmp_dtf_balance$Type_gs=='Trade balance'], color='brown' , marker = list(enabled = F), lineWidth = 3 ),
    #                list(name = 'Goods balance', data =tmp_dtf_balance$Value[tmp_dtf_balance$Type_gs=='Goods balance'], color = 'darkgreen', dashStyle = 'shortDot', marker = list(symbol = 'circle') ),
    #                list(name = 'Services balance', data =tmp_dtf_balance$Value[tmp_dtf_balance$Type_gs=='Services balance'], color = 'darkblue', dashStyle = 'shortDot',  marker = list(symbol = 'triangle') )
    #     )%>%
    #     hc_xAxis( categories = unique(tmp_dtf_balance$Year) ) %>%
    #     hc_yAxis( title = list(text = "$ million, NZD"),
    #               labels = list( format = "${value:,.0f} m"),
    #               plotLines = list(
    #                 list(#label = list(text = "This is a plotLine"),
    #                   color = "#ff0000",
    #                   #dashStyle = 'shortDot',
    #                   width = 2,
    #                   value = 0 ) )
    #     ) %>%
    #     hc_plotOptions(column = list(
    #       dataLabels = list(enabled = F),
    #       #stacking = "normal",
    #       enableMouseTracking = T ) 
    #     )%>%
    #     hc_tooltip(table = TRUE,
    #                sort = TRUE,
    #                pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
    #                                      " {series.name}: ${point.y} m"),
    #                headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
    #     ) %>%
    #     hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = 000 )
    # })
    # 
    ## remove the waiting message -- 
     removeUI(selector = "#main_wait_message")
  }


















