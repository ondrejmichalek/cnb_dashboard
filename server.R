
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
    
  
    # industry employment and wages
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
          color = ifelse(tmp_mzdy >0,"green", "red")
        )
      })
      
      
    # international comparism industry
      
      pr_eurostat <- reactive({
        pr_p_eurostat %>% 
          filter(date == input$pr_date_eurostat &
                   label %in% input$pr_eurostat_select_ind &
                   unit %in% input$pr_eurostat_select_trans) %>% 
          mutate(date_label = format(date,"%m/%Y"),
                 value_label = case_when(grepl("Index", unit) ~ paste0(str_replace(round(value,2), "\\.", ",")),
                                         TRUE ~ paste0(str_replace(round(value,2), "\\.", ","), " %")),
                 tooltip = paste0("Ukazatel: ", unit, "\n",
                                  "Země: ", geo, "\n",
                                  "Datum: ", date_label, "\n",
                                  "Odvětví: : ", label, "\n",
                                  "Hodnota: ", value_label),
                 Hodnota = value)
      })
      
      output$pr_eurostat_map <- renderPlotly({
        
        
        
        p <- ggplot(pr_eurostat())+
          geom_sf(data = europe)+
          geom_sf(aes(fill = Hodnota, text = tooltip))+
          coord_sf(xlim = c(20, 25), ylim = c(35, 70), expand = T)+
          # xlim(limits = c(20, 15))+
          # ylim(limits = c(35, 70))+
          scale_fill_viridis_b()+
          # scale_fill_gradient(low=colors_cnb()[2], high=colors_cnb()[10])+
          theme_void()+
          theme(axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                legend.title = element_text(color = "white"),
                legend.position = "right")
        
        
        p <- ggplotly(p, tooltip = c("tooltip")) %>% style(hoveron = "fill")
        
        # Pro styl a napravu legendy
        
        for (i in 1:length(p$x$data)){
          if (!is.null(p$x$data[[i]]$name)){
            p$x$data[[i]]$name =  gsub("\\(","",str_split(p$x$data[[i]]$name,",")[[1]][1])
          }
        }
        
        
        if(input$pr_eurostat_select_trans == "Index"){

          p <- ggplotly(p) %>%
            layout(legend = list(title=list(text='Hodnota'),
                                 font = list(
                                   family = "Arial",
                                   size = 15,
                                   color = "#000")),
                   title = list(x = 0, text = paste0(
                     '<b><span style="font-size: 20px; color: #2426A8;">',"Produkce - ", input$pr_eurostat_select_ind,' <br>','</b></span>',
                     '<sup><span style="font-size: 12px; color: gray;">',"(index 2015 = 100, stálé ceny, sezonně očištěno)",'</sup></span>'
                   )),
                   margin = list(l = 10, r = 10, b = 10, t = 60))

        }else{

          p <- ggplotly(p) %>%
            layout(legend = list(title=list(text='Hodnota'),
                                 font = list(
                                   family = "Arial",
                                   size = 15,
                                   color = "#000")),
                   title = list(x = 0, text = paste0(
                     '<b><span style="font-size: 20px; color: #2426A8;">',"Produkce - ", input$pr_eurostat_select_ind,' <br>','</b></span>',
                     '<sup><span style="font-size: 12px; color: gray;">',"(", str_to_lower(input$pr_eurostat_select_trans)," v %, stálé ceny, sezonně očištěno)",'</sup></span>'
                   )),
                   margin = list(l = 10, r = 10, b = 10, t = 60))

        }
        
        
      })
      
      
    # nfc table
    
      output$p604_def_table <- function() {

        p604_def %>%
          filter(`Proměnná` == input$np_select_var) %>% 
          knitr::kable("html") %>%
          kable_styling(full_width = T, bootstrap_options = c("striped", "hover", "condensed")) %>%
          column_spec(1, bold = T, border_right = F) 
      }  
      
    # nfc multichart
      
      p604_sa <- p604_sa %>%
        mutate(date_label = paste0(quarter(date),"Q/",format(date,"%y")),
               value_label_line = paste0(str_replace(round(Celkem,2), "\\.", ","), " %"),
               value_label_col = paste0(str_replace(round(value,2), "\\.", ","), " p. b."),
               tooltip_col = paste0("Ukazatel: ", l_v, "\n",
                                 "Odvětví: ", label2, "\n",
                                 "Datum: ", date_label, "\n",
                                 "Hodnota: ", value_label_col),
               Ukazatel = paste0(": ", l_v, "\n",
                                     "Odvětví: ", label1, "\n",
                                     "Datum: ", date_label, "\n",
                                     "Hodnota: ", value_label_line))
      
      p604_sa_s <- reactive({
        p604_sa %>% 
          filter(l_v %in% input$np_select_var &
                   label1 %in% input$np_select_odv &
                   trans %in% input$np_select_trans &
                   date >= input$np_date_slider[1] & 
                   date <= input$np_date_slider[2]) %>% 
          mutate(label2 = factor(label2, levels = translator %>% filter(label1 == input$np_select_odv) %>% pull(label2)))
      })
      
      output$np_multichart <- renderPlotly({
        
        
        
        p <- ggplot(p604_sa_s())+
          geom_line(aes(x = date, y = Celkem, color = paste0(unique(p604_sa_s()$label1)), label = Ukazatel))+
          geom_col(aes(x = date, y = value, fill = label2, text = tooltip_col),
                   color = "black", size = 0.1, position = position_stack(reverse = T))+
          scale_fill_manual(values = colors_cnb()[1:length(unique(p604_sa_s()$label2))])+
          scale_color_manual(values = "black", 
                             labels = paste0(unique(p604_sa_s()$label1)))+
          scale_y_continuous(labels = label_number(decimal.mark = ",", accuracy = 0.1))+
          scale_x_date(labels = date_zoi_q)+
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
            geom_hline(yintercept = 0)+
            guides(linetype = guide_legend(title="", ncol = 1, order = 1))
        
        p <- ggplotly(p, tooltip = c("Ukazatel","tooltip_col"))

        # Pro styl a napravu legendy

        for (i in 1:length(p$x$data)){
          if (!is.null(p$x$data[[i]]$name)){
            p$x$data[[i]]$name =  gsub("\\(","",str_split(p$x$data[[i]]$name,",1")[[1]][1])
          }
        }

        p <- ggplotly(p) %>%
          layout(legend = list(orientation = 'h',
                               y = -0.15,
                               title=list(text=''),
                               font = list(
                                 family = "Arial",
                                 size = 15,
                                 color = "#000")),
                 annotations = list(x = 1, y = -0.46, text = paste0("<span style='color:",colors_cnb()[1],"'>"," Aktualizace dat ", format(update_np, "%d.%m.%Y %H:%m:%S"),"</span>"),
                                    xref='paper', yref='paper', showarrow = F,
                                    xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                    font = list(size = 10)),
                 title = list(x = 0, text = paste0(
                   '<b><span style="font-size: 20px; color: #2426A8;">',input$np_select_var,' <br>','</b></span>',
                   '<sup><span style="font-size: 12px; color: gray;">',"(", str_to_lower(input$np_select_trans)," v %, běžné ceny)",'</sup></span>'
                 )),
                 margin = list(l = 50, r = 50, b = 110, t = 50))

        
      })
    
     
    ## remove the waiting message -- 
     removeUI(selector = "#main_wait_message")
     removeUI(selector = "#wait_message_prumysl")
     removeUI(selector = "#wait_message_nefinancni_podniky")
  }


















