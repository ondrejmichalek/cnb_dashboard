
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
    
    # nowcast_hdp_all1 <- nowcast_hdp_all %>% 
    #   mutate(name = as.character(name))
    # Nowcast HDP all
    output$nowcast_hdp_all <- renderHighchart({
      
      # highchart(type = "chart") %>%
        hchart(nowcast_hdp_all, "column", hcaes(x = name, y = values, color = name))
        # hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) 
        # hc_add_series(data = nowcast_hdp_all1,
        #               hcaes(y = values, group = name), type = "bar",
        #               color = colors_cnb()[1:nrow(nowcast_hdp_all1)]) %>%
        # hc_legend(enabled = F) %>% 
        # hc_xAxis(categories = c(nowcast_hdp_all1$name))
      # hc_tooltip(
      #   crosshairs = T,
      #   borderWidth = 1,
      #   sort = F,
      #   table = TRUE,
      #     formatter = JS("function() {
      #   return Highcharts.dateFormat('%Y-%b-%d',
      #                                 this.x);
      # }"),
      #   xDateFormat = '%Y-%m'
      # )  %>%
        # hc_add_theme(hc_theme_cnb())
      
    })
    
    nowcast_hdp_all <- nowcast_hdp_all %>% 
      mutate(var = paste0(str_replace(round(values,2), "\\.", ","), " %"),
             tooltip = case_when(grepl("ČNB", name) ~ paste0("Současná prognóza ČNB: ", var),
                                 TRUE ~ var))
    
    output$nowcast_hdp_all_gg <- renderPlotly({
    
        p <- ggplot(nowcast_hdp_all)+
        geom_hline(yintercept = 0)+
        geom_col(aes(x = name, y = values, fill = name,
                     text = tooltip),
                 color = "black", size = 0.1, position = position_dodge(),width = 0.75)+
        scale_fill_manual(values = colors_cnb()[1:nrow(nowcast_hdp_all)],
                          labels = color_labels(nowcast_hdp_all$name, colors_cnb()[1:nrow(nowcast_hdp_all)]))+
        scale_y_continuous(breaks = seq(-0.8,0.8, by = 0.2),
                           labels = label_number(decimal.mark = ",", accuracy = 0.1))+
        scale_x_discrete(expand = c(0.05,0.05), limits = levels(nowcast_hdp_all$name), labels = nowcast_hdp_all$name)+
        # coord_cartesian(ylim = c(-0.8,0.8))+
        theme_minimal()+
        theme(axis.text.y = element_text(size = 19, color = "black"),
        axis.text.x = element_markdown(size = 18, color = colors_cnb()[1:nrow(nowcast_hdp_all)]),
        axis.title=element_blank(),
        plot.title = element_text(color = colors_cnb()[2], size = 19,face = "bold",vjust = 0),
        plot.subtitle = element_text(color = "black", size = 11, vjust = 2.5),
        plot.caption = element_text(hjust = 0, vjust = 2, size = 9),
        legend.position = "none",
        legend.spacing = unit(-0.5,"cm"),
        legend.text = element_markdown(size = 19),
        panel.spacing = unit(1.5, "lines"),
        strip.text = element_text(color = "black", size = 12),
        text = element_text(family = "Arial"))+
        guides(linetype=guide_legend(title="", ncol = 1, order = 3),
               color=guide_legend(title="", ncol = 1, reverse = T, order = 2),
               fill=guide_legend(title="", ncol = 4, order = 1))
        # thm_noaxistitle()
        
        ggplotly(p, tooltip = c("tooltip")) %>% 
          layout(annotations = list(x = 1, y = -0.4, text = paste0("<span style='color:",colors_cnb()[1],"'>","Aktualizace dat ",
                                                                   format(update_nowcast_hdp, "%d.%m.%Y %H:%m:%S"),"</span>"),
                                    xref='paper', yref='paper', showarrow = F,
                                    xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                    font = list(size = 10)),
                 title = list(x = 0, text = paste0(
                   '<b><span style="font-size: 20px; color: #2426A8;">',"Nowcast HDP - ",date_zoi_q(nowcast_hdp_all$Date[1]),' <br>','</b></span>',
                   '<sup><span style="font-size: 12px; color: gray;">','(mezičtvrtletní změny HDP v %)','</sup></span>'
                 )),
                 margin = list(l = 50, r = 50, b = 100, t = 50)
          )

      })
    
    nowcast_dfm_all <- nowcast_dfm_all %>% 
      mutate(date = factor(`Date of forecast`,labels = format(unique(`Date of forecast`),"%d.%m.%Y")),
             color = ifelse(`New forecast` < 0, "a", "b"),
             rev = paste0(str_replace(round(`Impact of revisions`,2), "\\.", ","), " p. b."),
             dat = paste0(str_replace(round(`Impact of new data`,2), "\\.", ","), " p. b."),
             forecast = paste0(str_replace(round(`New forecast`,2), "\\.", ","), " %"),
             impact = paste0(str_replace(round(`sum`,2), "\\.", ","), " p. b."),
             tooltip_point = paste0("Prognóza z ", date, ": ", forecast, "\n\n",
                                    "Dopad nových informací oproti předchozí prognóze:\n",
                                    "   - Dopad nových dat: ", dat, "\n",
                                    "   - Dopad revizí dat: ", rev),
             tooltip_col = paste0("Kategorie ",Kategorie, ifelse(sum < 0, "\nsnížila", "\nzvýšila"),
                                  " prognózu HDP o ", str_replace(round(`sum`,2), "\\.", ","), " p. b."))
    
    output$nowcast_dfm_all_gg <- renderPlotly({
      
      p <- ggplot(nowcast_dfm_all)+
        geom_col(aes(x = date,
                     y = sum, fill = Kategorie, text = tooltip_col), 
                 color = "black", size = 0.1, position = position_stack(reverse = T))+
        geom_hline(yintercept = 0)+
        geom_point(aes(x = date, y = `New forecast`,
                       color = color, text = tooltip_point),
                   size = 3)+
        scale_fill_manual(values = colors_cnb()[1:nrow(nowcast_dfm_all)],
                          labels = unique(nowcast_dfm_all$Kategorie))+
        scale_color_manual(values = c("a" = colors_cnb()[9], "b" = colors_cnb()[10]))+
        scale_y_continuous(breaks = seq(-1,1, by = 0.2),
                           labels = label_number(decimal.mark = ",", accuracy = 0.1),
                           expand = c(0.05,0.05))+
        scale_x_discrete(expand = c(0.1,0.1))+
        theme_minimal()+
        theme(axis.text.x = element_text(size = 19, color = "black"),
              axis.text.y = element_text(size = 19, color = "black"),
              axis.title=element_blank(),
              plot.title = element_text(color = colors_cnb()[1], size = 20,face = "bold"),
              plot.subtitle = element_text(color = "black", size = 11, vjust = 2.5),
              plot.caption = element_text(hjust = 0, vjust = 2, size = 9),
              legend.position = "bottom",
              # legend.title = element_blank(),
              legend.direction = "horizontal",
              # legend.spacing = unit(-0.5,"cm"),
              # legend.text = ggtext::element_markdown(size = 19),
              panel.spacing = unit(1.5, "lines"),
              strip.text = element_text(color = "black", size = 12),
              text = element_text(family = "Arial"))+
        guides(linetype=guide_legend(title="", ncol = 1, order = 3),
               # color=guide_legend(title="", ncol = 1, reverse = T, order = 2),
               color = "none")
      
      p <- ggplotly(p, tooltip = c("tooltip_point", "tooltip_col"))
      
      # Pro styl a napravu legendy
      
      for (i in 1:length(p$x$data)){
        if (!is.null(p$x$data[[i]]$name)){
          p$x$data[[i]]$name =  gsub("\\(","",str_split(p$x$data[[i]]$name,",")[[1]][1])
        }
      }
      
      p <- ggplotly(p) %>%
        # add_trace(hoverinfo = "skip") %>% 
        layout(legend = list(orientation = 'h', 
                             y = -0.15,
                             title=list(text=''),
                             font = list(
                               family = "Arial",
                               size = 15,
                               color = "#000")),
               annotations = list(x = 1, y = -0.46, text = paste0("<span style='color:",colors_cnb()[1],"'>","Hodnota predikce v čase t (body) je sumou hodnoty predikce v čase t-1 a dopadů dat i revizí v čase t.",
                                                                 " Aktualizace dat ", format(update_nowcast_hdp, "%d.%m.%Y %H:%m:%S"),"</span>"),
                                  xref='paper', yref='paper', showarrow = F,
                                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                  font = list(size = 10)),
               title = list(x = 0, text = paste0(
                 '<b><span style="font-size: 20px; color: #2426A8;">',"Dopad nových dat do prognózy HDP - ",date_zoi_q(nowcast_hdp_all$Date[1]),' <br>','</b></span>',
                 '<sup><span style="font-size: 12px; color: gray;">','(mezičtvrtletní změny v % - body, příspěvky a dopady dat i revizí do prognózy v p. b.)','</sup></span>'
               )),
               margin = list(l = 50, r = 50, b = 100, t = 50))
      
 
        
      
    })
    
    # 1. Value boxes  ---------------------------------------------------------
    ## try add progress bars
    withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
      # Increment the progress bar, and update the detail text.
      incProgress( i_prog/tot_step, detail = NULL)
      ##Sys.sleep(0.1)
      
    })
    i_prog <- i_prog + 1
    

    # Prumysl -----------------------------------------------------------------
    
    ## Propojeni selectovatek  
    
    observe({
      updateSelectizeInput(session,"pr_select_var")
      updateSelectizeInput(session,"pr_select_ind")
      updateSelectizeInput(session,"pr_select_unit")
    })
    observeEvent({input$pr_select_ind},
                 {updateSelectizeInput(session,"pr_select_var",choices = unique(pr %>% filter(label %in% input$pr_select_ind) %>% pull(variable)), selected = input$pr_select_var)
                  updateSelectizeInput(session,"pr_select_unit",choices = unique(pr %>% filter(label %in% input$pr_select_ind) %>% pull(unit)), selected = input$pr_select_unit)}, ignoreNULL = T)
    observeEvent({input$pr_select_var},
                 {updateSelectizeInput(session,"pr_select_ind",choices = unique(pr %>% filter(variable %in% input$pr_select_var) %>% pull(label)), selected = input$pr_select_ind)
                  updateSelectizeInput(session,"pr_select_unit",choices = unique(pr %>% filter(variable %in% input$pr_select_var) %>% pull(unit)), selected = input$pr_select_unit)}, ignoreNULL = T)
    # observeEvent({input$pr_select_unit},
    #              {updateSelectizeInput(session,"pr_select_var",choices = unique(pr %>% filter(unit %in% input$pr_select_unit) %>% pull(variable)), selected = input$pr_select_var)}, ignoreNULL = T)
    
    pr <- pr %>% 
      mutate(date_label = format(date,"%m/%Y"),
             value_label = case_when(grepl("Index", trans) ~ paste0(str_replace(round(value,2), "\\.", ",")),
                                     TRUE ~ paste0(str_replace(round(value,2), "\\.", ","), " %")),
             Ukazatel = paste0(variable, "\n",
                              "Odvětví: ", label, "\n",
                              "Datum: ", date_label, "\n",
                              "Hodnota: ", value_label))
    
    pr_s <- reactive({
      pr %>% 
        filter(variable %in% input$pr_select_var &
                 label %in% input$pr_select_ind &
                 trans %in% input$pr_select_trans &
                 unit %in% input$pr_select_unit &
                 date >= input$pr_date_slider[1] & 
                 date <= input$pr_date_slider[2]) 
    })
    
    output$pr_multichart <- renderPlotly({
      

      
      p <- ggplot(pr_s())+
        geom_line(aes(x = date, y = value, color = label, label = Ukazatel))+
        scale_color_manual(values = colors_cnb()[1:length(input$pr_select_ind)])+
        scale_y_continuous(labels = label_number(decimal.mark = ",", accuracy = 0.1))+
        theme_minimal()+
        theme(axis.text.x = element_text(size = 19, color = "black"),
              axis.text.y = element_text(size = 19, color = "black"),
              axis.title=element_blank(),
              plot.title = element_text(color = colors_cnb()[1], size = 20,face = "bold"),
              plot.subtitle = element_text(color = "black", size = 11, vjust = 2.5),
              plot.caption = element_text(hjust = 0, vjust = 2, size = 9),
              legend.position = "bottom",
              # legend.title = element_blank(),
              legend.direction = "horizontal",
              # legend.spacing = unit(-0.5,"cm"),
              # legend.text = ggtext::element_markdown(size = 19),
              panel.spacing = unit(1.5, "lines"),
              strip.text = element_text(color = "black", size = 12),
              text = element_text(family = "Arial"))+
        {if (input$pr_select_trans == "Index")geom_hline(yintercept = 100)}+
        {if (input$pr_select_trans != "Index")geom_hline(yintercept = 0)}+
        guides(linetype=guide_legend(title="", ncol = 1, order = 3))
      
      p <- ggplotly(p, tooltip = c("Ukazatel"))
      
      # Pro styl a napravu legendy

      for (i in 1:length(p$x$data)){
        if (!is.null(p$x$data[[i]]$name)){
          p$x$data[[i]]$name =  gsub("\\(","",str_split(p$x$data[[i]]$name,",")[[1]][1])
        }
      }
      
      
      if(input$pr_select_trans == "Index"){

      p <- ggplotly(p) %>%
        layout(legend = list(orientation = 'h',
                             y = -0.15,
                             title=list(text=''),
                             font = list(
                               family = "Arial",
                               size = 15,
                               color = "#000")),
               annotations = list(x = 1, y = -0.46, text = paste0("<span style='color:",colors_cnb()[1],"'>"," Aktualizace dat ", format(update_pr, "%d.%m.%Y %H:%m:%S"),"</span>"),
                                  xref='paper', yref='paper', showarrow = F,
                                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                  font = list(size = 10)),
               title = list(x = 0, text = paste0(
                 '<b><span style="font-size: 20px; color: #2426A8;">',input$pr_select_var,' <br>','</b></span>',
                 '<sup><span style="font-size: 12px; color: gray;">',"(index 2015 = 100, ", str_to_lower(input$pr_select_unit),")",'</sup></span>'
               )),
               margin = list(l = 50, r = 50, b = 110, t = 50))

      }else{

        p <- ggplotly(p) %>%
          layout(legend = list(orientation = 'h',
                               y = -0.15,
                               title=list(text=''),
                               font = list(
                                 family = "Arial",
                                 size = 15,
                                 color = "#000")),
                 annotations = list(x = 1, y = -0.46, text = paste0("<span style='color:",colors_cnb()[1],"'>"," Aktualizace dat ", format(update_pr, "%d.%m.%Y %H:%m:%S"),"</span>"),
                                    xref='paper', yref='paper', showarrow = F,
                                    xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                    font = list(size = 10)),
                 title = list(x = 0, text = paste0(
                   '<b><span style="font-size: 20px; color: #2426A8;">',input$pr_select_var,' <br>','</b></span>',
                   '<sup><span style="font-size: 12px; color: gray;">',"(", str_to_lower(input$pr_select_trans)," v %, ", str_to_lower(input$pr_select_unit),")",'</sup></span>'
                 )),
                 margin = list(l = 50, r = 50, b = 110, t = 50))

      }
      
      
    })
    
  
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
      pivot_longer(-date, names_to = c("odv","id_p"), names_sep = "\\.") %>% 
      left_join(labels, by = "id_p") %>% 
      group_by(label) %>% 
      arrange(date) %>% 
      mutate(yoy = value/lag(value, 12)*100-100,
             mom = value/lag(value, 1)*100-100)
  

        
    

    output$pr_line_mom <- renderHighchart({
      
      
      hchart(mpg, "point", hcaes(displ, hwy, group = drv)) %>%
        hc_exporting(enabled = TRUE, filename = "threat_extract")
      
      }) 
    
    # output$pr_line_mom <- renderHighchart({
    #   # highchart() %>%
    #   hchart(tmp_dtf %>% filter(label %in% input$select_industry), "line", 
    #          hcaes(x = date, y = mom, group = label))
    # }) 
    
    output$pr_line_mom2 <- renderHighchart({
        
      tmp_dtfr <-  reactive({
        
        tmp_dtf %>% filter(label %in% input$select_industry)
      
        })
        
      hchart(tmp_dtfr(), "line", hcaes(x = date, y = mom, group = label)) %>% 
        hc_exporting(enabled = TRUE, filename = "threat_extract") %>% 
        hc_xAxis(type = 'datetime')
        # hc_rangeSelector(enabled = T)
        # hc_navigator(
        #   selected = 1,
        #   enabled = T,
        #   outlineColor = "gray",
        #   outlineWidth = 2,
        #   series = list(
        #     color = colors_cnb()[1:length(input$select_industry)],
        #     lineWidth = 2,
        #     type = "line"
        #   ),
        #   handles = list(
        #     backgroundColor = "white",
        #     borderColor = colors_cnb()[1:length(input$select_industry)]
        #   )
        # ) %>%
        # hc_legend(enabled = TRUE) %>% 
      #   hc_tooltip(
      #     crosshairs = T,
      #     borderWidth = 1,
      #     sort = F,
      #     table = TRUE,
      #     formatter = JS("function() {
      #   return Highcharts.dateFormat('%H:%M %d %b %Y',
      #                                 this.x);
      # }"),
      #     xDateFormat = '%Y-%m'
      #   )  %>%
        # hc_add_theme(hc_theme_cnb())
        
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


















