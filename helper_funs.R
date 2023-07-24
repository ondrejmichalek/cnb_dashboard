#### vb style -----------------
VB_style <- function(msg = 'Hello', style="font-size: 100%;"){
  tags$p( msg , style = style )
}

#### colors -----------------
colors_cnb <- function () 
{
  color_codes <- c(rgb(36, 38, 169, maxColorValue = 256), 
                   rgb(213, 43, 30, maxColorValue = 256), rgb(255, 187, 
                                                              0, maxColorValue = 256), rgb(154, 205, 50, maxColorValue = 256), 
                   rgb(0, 206, 209, maxColorValue = 256), rgb(108, 111, 
                                                              112, maxColorValue = 256), rgb(138, 43, 226, maxColorValue = 256), 
                   rgb(157, 171, 226, maxColorValue = 256), rgb(255, 99, 
                                                                71, maxColorValue = 256), rgb(46, 167, 84, maxColorValue = 256))
  return(color_codes)
}


### CAGR function------------------
CAGR <- 
  function (ratio, period, digits = 1) {
    round((exp(log(ratio)/period) - 1) * 100, digits)
  }

## rgb to hex function---------------      
GetColorHexAndDecimal <- function(color)
{
  c <- col2rgb(color)
  sprintf("#%02X%02X%02X %3d %3d %3d", c[1],c[2],c[3], c[1], c[2], c[3])
}



## cnb theme--------------- 
hc_theme_cnb <- function(...) {
  theme <-
    list(
      colors = c("#E10033", "#000000", "#767676", "#E4E4E4"),
      chart = list(
        backgroundColor = "transparent",
        style = list(
          fontFamily = "Arial",
          color = "#000000"
        )
      ),
      title = list(
        align = "left",
        style = list(
          fontFamily = "Arial",
          color = "#000000",
          fontWeight = "bold"
        )
      ),
      subtitle = list(
        align = "left",
        style = list(
          fontFamily = "Arial",
          color = "#000000",
          fontWeight = "bold"
        )
      ),
      xAxis = list(
        lineColor = "#000000",
        lineWidth = 2,
        tickColor = "#000000",
        tickWidth = 2,
        gridLineWidth = 2,
        gridLineColor = "#F3F3F3",
        labels = list(
          style = list(
            color = "black"
          )
        ),
        title = list(
          style = list(
            color = "black"
          )
        )
      ),
      yAxis = list(
        opposite = TRUE,
        # gridLineDashStyle = "Dot",
        gridLineWidth = 2,
        gridLineColor = "#F3F3F3",
        lineColor = "#CEC6B9",
        minorGridLineColor = "#CEC6B9",
        labels = list(
          align = "left",
          style = list(
            color = "black"
          ),
          x = 0,
          y = -2
        ),
        tickLength = 0,
        tickColor = "#CEC6B9",
        tickWidth = 1,
        title = list(
          style = list(
            color = "black"
          )
        )
      ),
      tooltip = list(
        backgroundColor = "#FFFFFF",
        borderColor = "#76c0c1",
        style = list(
          color = "#000000"
        )
      ),
      legend = list(
        layout = "horizontal",
        align = "left",
        verticalAlign = "bottom",
        itemStyle = list(
          color = "#3C3C3C"
        ),
        itemHiddenStyle = list(
          color = "#606063"
        )
      ),
      credits = list(
        style = list(
          color = "#666"
        )
      ),
      labels = list(
        style = list(
          color = "#D7D7D8"
        )
      ),
      drilldown = list(
        activeAxisLabelStyle = list(
          color = "#F0F0F3"
        ),
        activeDataLabelStyle = list(
          color = "#F0F0F3"
        )
      ),
      navigation = list(
        buttonOptions = list(
          symbolStroke = "#DDDDDD",
          theme = list(
            fill = "#505053"
          )
        )
      ),
      legendBackgroundColor = "rgba(0, 0, 0, 0.5)",
      background2 = "#505053",
      dataLabelsColor = "#B0B0B3",
      textColor = "#C0C0C0",
      contrastTextColor = "#F0F0F3",
      maskColor = "rgba(255,255,255,0.3)"
    )
  
  theme <- structure(theme, class = "hc_theme")
  
  if (length(list(...)) > 0) {
    theme <- hc_theme_merge(
      theme,
      hc_theme(...)
    )
  }
  
  theme
}

# color_labels

color_labels <- function(labels_name_vector, palette_colors){
  
  l <- length(labels_name_vector)
  v <- c()
  
  for (i in 1:l) { 
    v[i] <- paste0("<span style='color:",palette_colors[i],"'>",labels_name_vector[i],"</span>")
  }
  
  return(v)
}

# Date ZoMP

date_zoi_q <- function (dt) 
{
  y <- lubridate::year(dt)
  q <- lubridate::quarter(dt)
  tibble(y, q) %>% mutate(q = as.roman(q) %>% as.character, 
                          y = as.character(y) %>% str_sub(3, -1L)) %>% mutate(dt_out = ifelse(q == 
                                                                                                "I", sprintf("%s/%s", q, y), q)) %>% pull(dt_out)
}

## Ekonomicka aktivita
# HDP

ea_uvod <- function(){
  fluidRow(
    # tags$li(
    tags$ol("Nowcastovací modely v prùmìru ukazují, že HDP se v 1. ètvrtletí letošního roku zvýší o 0,2 %.
            Na stejnou hodnotu je nastavena i souèasná prognóza ÈNB",
             # tags$a(tags$i("BPM6 Quarterly, Balance of payments major components (Qrtly-Mar/Jun/Sep/Dec)"),
             #        href = "http://archive.stats.govt.nz/infoshare/SelectVariables.aspx?pxID=aa7f4009-2651-404c-b3b6-e24ea781d803",
             #        target = "_blank"), 
             "."
    ))}

ea_p2 <- function(){
  fluidRow(
    tags$ol("Dle dynamického faktorového modelu z dílny FEDu (DFM FED) se pøi nejnovìjší aktualizaci
            dat prognóza HDP posunula o nìco níže. Vliv na to mìly pøedevším data ze zahranièního
            obchodu, která zmírnila prognózu o necelých 0,1 p. b. Ve stejném smìru, avšak viditelnì
            ménì, pùsobila horší než oèekávaná data z maloobchodu a služeb", 
            tags$a(tags$i(""), 
                   href = "#", onclick = "openTab('help')", 
                   target = "_self"))
  )

}

ea_p3 <- function(){
  fluidRow(
    tags$ol("Detailnìjší popis vývoje v prùmyslu je k dispozici ",
            tags$a(tags$i("na pøíslušné záložce"), 
                   href = "#", onclick = "openTab('prumysl')", 
                   target = "_self"),
            ". Bližší popis výše uvedených metod, je k dispozici na záložce ", 
            tags$a(tags$i("FAQs"), 
                   href = "#", onclick = "openTab('help')", 
                   target = "_self"),".")
  )
  
}

ea_uvod_t <- function(){
  fluidRow(
    h1(paste0("Ekonomická aktivita "),
       style = "color: #2426A8"))}


pr_uvod <- function(){
  fluidRow(
    # tags$li(
    tags$ol("Domácí prùmysl byl v dubnu nadále tažen zejména produkcí automobilù, 
            která rostla stále dvouciferným tempem vlivem nižší srovnávací základny 
            z minulého roku. Produkce u vetšiny ostatních prùmyslových odvìtví meziroènì klesala."
    ))}

np_uvod <- function(){
  fluidRow(
    # tags$li(
    tags$ol("Následující údaje pochází z individuálních dat pøibližnì 2000 nejvìtších 
            nefinanèních podnikù (dle velikosti aktiv). Bližší informace o uvedeném
            prùzkumu jsou k dispozici na stránkách ",
            tags$a(tags$i("ÈSÚ"),
                   href = "https://www.czso.cz/csu/vykazy/p-6-04-ctvrtletni-vykaz-o-financnich-ukazatelich_psz_2023",
                   target = "_blank"),"."
    ))}


## how to text for commodity intelligence report --------------
howto_ci <- function(){
  fluidRow(
    tags$h1('How to:'),
    tags$ol(
      tags$li( "Select pre-defined or self-defined commodities:",
               tags$ol(type="a",
                       tags$li(tags$b("Pre-defined commodities "), "are defined by StatsNZ"),
                       tags$li(tags$b("Self-defined "), 
                               "commodity groupings are created by users with the format of a .csv file with the first column containing any of level 2/4/6 HS codes, and the second column containing group names.",
                               tags$b("You can download such a template from ", tags$a(href="HS_group_template.csv", "here", target = "_blank"), ", and modify it to suit your need." )
                       )
               )
      ),
      tags$li("Browse or upload:",
              tags$ol(type='a',
                      tags$li( "Search and select one or multiple pre-defined commodities" ),
                      tags$li( "Upload your self-defined commodity groups with the correct format" )
              )
      ),
      tags$li(tags$b("Please click RESET before generating an other report!")),
      tags$li( "For more detailed explanations on how to use this part of the dasbhoard, please visit " ,
               tags$a( "here",
                       href= "https://nzprimarysectortrade.wordpress.com/2018/10/15/introducing-the-new-zealand-trade-intelligence-dashboard/",
                       target = "_blank"),
               "."
      )
    )
  )
}

## how to text for country intelligence report --------------
howto_country <- function(){
  fluidRow(
    tags$h1('How to:'),
    tags$ol(
      tags$li( "Search or select a market or multiple markets"),
      tags$li( "Search or select ONLY ONE of the market groups. We prebuilt several groups including ", 
               tags$a(href="https://en.wikipedia.org/wiki/Africa","Africa" , target = "_blank"), ", ", 
               tags$a(href="https://www.apec.org/","APEC" , target = "_blank"), ", ",  
               tags$a(href="https://www.uneca.org/oria/pages/amu-arab-maghreb-union","Arab Maghreb Union" , target = "_blank"), ", ",  
               tags$a(href="http://asean.org/","ASEAN" , target = "_blank"), ", ",  
               tags$a(href="http://china-trade-research.hktdc.com/business-news/article/The-Belt-and-Road-Initiative/The-Belt-and-Road-Initiative-Country-Profiles/obor/en/1/1X000000/1X0A36I0.htm","BRI (Belt and Road Initiative) countries" , target = "_blank"), ", ",  
               tags$a(href="https://en.wikipedia.org/wiki/Central_Africa","Central Africa" , target = "_blank"), ", ",  
               tags$a(href="https://www.mfat.govt.nz/en/trade/free-trade-agreements/free-trade-agreements-concluded-but-not-in-force/cptpp/","CPTPP" , target = "_blank"), ", ",  
               tags$a(href="https://en.wikipedia.org/wiki/East_Africa","Eastern Africa" , target = "_blank"), ", ",  
               tags$a(href="https://www.eac.int/","Eastern African Community" , target = "_blank"), ", ",  
               tags$a(href="http://www.ecowas.int/","Economic Community of West African States" , target = "_blank"), ", ",  
               tags$a(href="https://europa.eu/european-union/about-eu/countries_en","EU28" , target = "_blank"), ", ",  
               tags$a(href="https://www.gcsb.govt.nz/about-us/ukusa-allies/","Five Eyes" , target = "_blank"), ", ",  
               tags$a(href="https://www.mfat.govt.nz/en/trade/free-trade-agreements/free-trade-agreements-in-force/","FTA in force" , target = "_blank"), ", ",  
               tags$a(href="https://en.wikipedia.org/wiki/Group_of_Seven","G7" , target = "_blank"), ", ",  
               tags$a(href="http://www.gcc-sg.org/en-us/Pages/default.aspx","GCC" , target = "_blank"), ", ",  
               tags$a(href="https://en.wikipedia.org/wiki/Latin_America","Latin America" , target = "_blank"), ", ",  
               tags$a(href="https://en.wikipedia.org/wiki/Middle_East","Middle East" , target = "_blank"), ", ",  
               tags$a(href="https://en.wikipedia.org/wiki/North_Africa","Northern Africa" , target = "_blank"), ", ", 
               tags$a(href="http://www.oecd.org/", "OECD" , target = "_blank"), ", ", 
               tags$a(href="http://www.opec.org/", "OPEC" , target = "_blank"), ", ", 
               tags$a(href="https://www.forumsec.org/", "Pacific Islands Forum" , target = "_blank"), ", ", 
               tags$a(href="https://en.wikipedia.org/wiki/Southern_Africa", "Southern Africa" , target = "_blank"), ", ", 
               tags$a(href="https://www.sadc.int/", "Southern African Development Community" , target = "_blank"), ", and ", 
               tags$a(href="https://en.wikipedia.org/wiki/West_Africa", "Western Africa" , target = "_blank"), ". ", 
               "Please contact ",
               tags$a( href = "mailto:TR_SharedMailbox@mbie.govt.nz", "TR_SharedMailbox@mbie.govt.nz"), ', if you need extra groups built.' ),
      tags$li(tags$b("Please click RESET before generating an other report!")),
      tags$li( "For more detailed explanations on how to use this part of the dasbhoard, please visit " ,
               tags$a( "here",
                       href= "https://nzprimarysectortrade.wordpress.com/2018/10/15/introducing-the-new-zealand-trade-intelligence-dashboard/",
                       target = "_blank"),
               "."
      )
    )
  )
}

## how to text for hs finder tab ----------------
howto_hs_finder <- function(){
  #fluidRow(
  tags$h1('How to:')
  tags$ol(
    tags$li( "Search for either HS code or commodity names in the search box or the filter box above each column. Note that regular expression rules are built in all search boxes."),
    tags$li( "Show either the first 10 entries or the entire table."),
    tags$li( "Click one or multiple rows to generate a report."),
    tags$li( "The report is default to be based on exports but can be changed to imports by using the radio button on the left." ),
    tags$li( "For more detailed explanations on how to use this part of the dasbhoard, please visit " ,
             tags$a( "here",
                     href= "https://nzprimarysectortrade.wordpress.com/2018/10/15/introducing-the-new-zealand-trade-intelligence-dashboard/",
                     target = "_blank"),
             "."
    )
    #tags$li( "Copy to clipboard or export as a CSV file for later use")
  )
  #)
}

## contact for help ----------------
contact <- function(){
  fluidRow(
    h2( "Contact" ),
    tags$p("Your suggestions, feedback, complaints or compliments are highly valued and will guide us to improve the dashboard continuously. Please email them to ", 
           tags$a( href="mailto:TR_SharedMailbox@mbie.govt.nz",
                   "TR_SharedMailbox@mbie.govt.nz",
                   target = '_blank'),
           "."
    )
  )
}

## data_source of the report ---------------------
data_source <- function(){
  fluidRow(
    h2(paste0('What are the data sources?')),
    #tags$ol(
    tags$li( "Total goods and services exports and imports are sourced from ", 
             tags$a(tags$i("BPM6 Quarterly, Balance of payments major components (Qrtly-Mar/Jun/Sep/Dec)"),
                    href = "http://archive.stats.govt.nz/infoshare/SelectVariables.aspx?pxID=aa7f4009-2651-404c-b3b6-e24ea781d803",
                    target = "_blank"), 
             ", a table under Economic indicators and Balance of Payments - BOP from Inforshare Statistics New Zealand."
    ),
    tags$li("Goods exports and imports by country and commodity are sourced and compiled from ",
            tags$a("the overseas merchiandise trade datasets",
                   href = "http://archive.stats.govt.nz/browse_for_stats/industry_sectors/imports_and_exports/overseas-merchandise-trade/HS10-by-country.aspx",
                   target = "_blank"),
            " from Statistics New Zealand."
    ),
    tags$li("Services exports and imports by country are sourced from ",
            tags$a(tags$i("BPM6 Services by country, year ended in quarter (Qrtly-Mar/Jun/Sep/Dec)") ,
                   href = "http://archive.stats.govt.nz/infoshare/SelectVariables.aspx?pxID=e17bdb70-8fff-4f11-baf1-eb732a963099",
                   target = "_blank"),
            ", a table under Economic indicators and Balance of Payments - BOP from Inforshare Statistics New Zealand. For countries whose data are not available from this source, ",
            tags$a( tags$i("Goods and services trade by country: Year ended Qtr Year map data CSV"),
                    href = "https://www.stats.govt.nz/information-releases/goods-and-services-trade-by-country-year-ended-june-2018",
                    target = "_blank" ),
            "is then used."
    ),
    tags$li("Data used in the global trade analysis are sourced from ",
            tags$a(tags$i("UN Comtrade, International Trade Statistics Database") ,
                   href = "https://comtrade.un.org/",
                   target = "_blank"),
            ", by using its ", 
            tags$a(tags$i("API"),
                   href = "https://comtrade.un.org/data/dev/portal",
                   target = "_blank"),
            " via an R package called ",
            tags$a(tags$i("comtradr"),
                   href = "https://cran.r-project.org/web/packages/comtradr/index.html",
                   target = "_blank"),
            ". Please note that the maximum number of queries is 100 per hour."
    ),
    tags$li("Directional basis stock of direct investment are sourced from ",
            tags$a(tags$i("BPM6 Annual, Directional basis stock of direct investment by country (Annual-Mar)") ,
                   href = "http://archive.stats.govt.nz/infoshare/SelectVariables.aspx?pxID=e17bdb70-8fff-4f11-baf1-eb732a963099",
                   target = "_blank"),
            ", a table under Economic indicators and International Investment Position - IIP from Inforshare Statistics New Zealand."),
    tags$li("New Zealand visitor travelling overseas data is sourced from ",
            tags$a(tags$i("NZ-resident traveller departures by EVERY country of main dest and purpose (Qrtly-Mar/Jun/Sep/Dec)") ,
                   href = "http://archive.stats.govt.nz/infoshare/SelectVariables.aspx?pxID=e17bdb70-8fff-4f11-baf1-eb732a963099",
                   target = "_blank"),
            ", a table under Tourism and International Travel and Migration - ITM from Inforshare Statistics New Zealand."),
    tags$li("Foreign visitor travelling to New Zealand data is sourced from ",
            tags$a(tags$i("Visitor arrivals by EVERY country of residence and purpose (Qrtly-Mar/Jun/Sep/Dec)") ,
                   href = "http://archive.stats.govt.nz/infoshare/SelectVariables.aspx?pxID=e17bdb70-8fff-4f11-baf1-eb732a963099",
                   target = "_blank"),
            ", a table under Tourism and International Travel and Migration - ITM from Inforshare Statistics New Zealand.")
    #)
  )
}


## when the dashobard will be updated ---------------------
when_update <- function() {
  fluidRow(
    h2(paste0('When will the dashboard be updated?')),
    tags$p(
      "The dashboard will be updated quarterly with around two and half months lag. For example, March quarter data will be updated around the second week in June."
    )
  )
}

## hs_code_explain of the report ---------------------
hs_code_explain <- function() {
  fluidRow(
    h2(paste0('What is HS code?')),
    tags$p(
      "The Harmonized Commodity Description and Coding System, also known as the Harmonized System (HS) of tariff nomenclature is an internationally standardized system of names and numbers to classify traded products. It came into effect in 1988 and has since been developed and maintained by the World Customs Organization (WCO) (formerly the Customs Co-operation Council), an independent intergovernmental organization based in Brussels, Belgium, with over 200 member countries."
    ),
    tags$p("More information on New Zealand harmonised system classification can be found ", 
           tags$a( href="http://archive.stats.govt.nz/browse_for_stats/industry_sectors/imports_and_exports/overseas-merchandise-trade/HS2017.aspx",
                   "here.",
                   target = '_blank') 
    ),
    tags$p("In addition, the Customs Service are responsible for the classification of goods at the border. More information can be found ", 
           tags$a( href="https://www.customs.govt.nz/business/tariffs/tariff-classifications-and-rates/",
                   "here.",
                   target = '_blank') )
  )
}

### trade terms explained-----------------
trade_terms <- function(){
  fluidRow(
    h2(paste0('What do these trade terms mean?')),
    #tags$ol(
    tags$li( tags$b("Trade balance, trade surplus or trade deficit:"), tags$br(),
             "The balance of trade, commercial balance, or net exports (sometimes symbolized as NX), is the difference between the monetary value of a nation's exports and imports over a certain period. Sometimes a distinction is made between a balance of trade for goods versus one for services. If a country exports a greater value than it imports, it has a trade surplus, if a country imports a greater value than it exports, it has a trade deficit."
    ),
    tags$li( tags$b("Two-way trade:"), tags$br(),
             "The sum of total exports and imports."
    )
    #)
  )
}

### confidential trade data --------------------
confidential_trade_data <- function(){
  fluidRow(
    h2(paste0('What is confidential data?')),
    tags$p(
      "International Merchandise Trade Statistics confidentiality policy can be found ",
      tags$a( href="http://archive.stats.govt.nz/about_us/legisln-policies-protocols/trade-confidentiality/international-merchandise-trade-confidentiality-policy.aspx",
              "here. ",
              target = '_blank') ,
      "Confidential items in overseas trade and cargo statistics can be found ",
      tags$a( href="http://archive.stats.govt.nz/about_us/legisln-policies-protocols/trade-confidentiality.aspx",
              "here, ",
              target = '_blank'),
      "where you also find the confidential items for both exports and imports."
    )
  )
}

### Urgent updates explained-----------------
urgent_updates <- function(){
  fluidRow(
    h2(paste0('Urgent updates:')),
    tags$li( "The dashboard with the latest data upto the year ended June 2018 is updated on 4 October 2018. This is due to Stats NZ's correction on its goods and services by country data for the year ended June 2018 on 3 October 2018. Stats NZ's media release on the correction can be found ",
             tags$a( href="https://www.stats.govt.nz/news/correction-to-goods-and-services-trade-by-country-year-ended-june-2018",
                     "here.",
                     target = '_blank'))
  )
}



# var conall = $(this.chart.container).parents(".shinyjs-hide).find("#CIExportPercentLine");
## shared legend for highchager ---------------------
sharelegend = JS('function(event){
                  var vis = this.visible;
                 var conall = $(this.chart.container).parents(".row").find("div.highchart");
                 for(var i = 0; i < conall.length; i++){
                    var hc = $(conall[i]).highcharts();
                    var series = hc.get(this.options.id);
                    if(series){
                       if(vis){
                          series.hide();
                        } else{
                          series.show();
                        }
                     }
                 }
                 return false;
                 }')

sharelegend = JS("function (event) {
                    var visibility = this.visible ? 'visible' : 'hidden';
                 if (!confirm('The series is currently ' +
                 visibility + '. Do you want to change that?')) {
                 return false;
                 }
                 }")


## produce summary country table -------------------
sum_selected_country <- function( arg_countries ){
  ## for selected countries
  tmp_country_all_base <- 
    dtf_shiny_country_gs %>%
    filter( Country %in% arg_countries,
            Year >= 2007 ) 
  
  tmp_country_all_twoway <-
    tmp_country_all_base %>%
    group_by( Year ) %>%
    summarise( Value = sum( Value, na.rm=T ) ) %>%
    ungroup %>%
    mutate( Name = 'Two-way trade' ) 
  
  tmp_country_all_balance <- 
    tmp_country_all_base %>%
    group_by( Year, Type_ie ) %>%
    summarise( Value = sum( Value, na.rm=T ) ) %>%
    ungroup %>%
    group_by( Year ) %>%
    #summarise( Value = Value[Type_ie=='Exports'] - Value[Type_ie=='Imports'] ) %>%
    do( Value = .$Value[.$Type_ie=='Exports'] - .$Value[.$Type_ie=='Imports'] ) %>%
    ungroup %>%
    mutate( Value = as.numeric(Value) ) %>%
    mutate( Name = 'Trade balance' ) 
  
  tmp_country_all_balance_g <- 
    tmp_country_all_base %>%
    filter( Type_gs == 'Goods' ) %>%
    group_by( Year, Type_ie ) %>%
    summarise( Value = sum( Value, na.rm=T ) ) %>%
    ungroup %>%
    group_by( Year ) %>%
    #summarise( Value = Value[Type_ie=='Exports'] - Value[Type_ie=='Imports'] ) %>%
    do( Value = .$Value[.$Type_ie=='Exports'] - .$Value[.$Type_ie=='Imports'] ) %>%
    ungroup %>%
    mutate( Value = as.numeric(Value) ) %>%
    mutate( Name = 'Goods balance' ) 
  
  tmp_country_all_balance_s <- 
    tmp_country_all_base %>%
    filter( Type_gs == 'Services' ) %>%
    group_by( Year, Type_ie ) %>%
    summarise( Value = sum( Value, na.rm=T ) ) %>%
    ungroup %>%
    group_by( Year ) %>%
    #summarise( Value = Value[Type_ie=='Exports'] - Value[Type_ie=='Imports'] ) %>%
    do( Value = .$Value[.$Type_ie=='Exports'] - .$Value[.$Type_ie=='Imports'] ) %>%
    ungroup %>%
    mutate( Value = as.numeric(Value) ) %>%
    mutate( Name = 'Services balance' ) 
  
  tmp_country_all_tot_ex <- 
    tmp_country_all_base %>%
    filter( Type_ie == 'Exports' ) %>%
    group_by( Year) %>%
    summarise( Value = sum( Value, na.rm=T ) ) %>%
    ungroup %>%
    mutate( Name = 'Total exports' ) 
  
  tmp_country_all_gs_ex <- 
    tmp_country_all_base %>%
    filter( Type_ie == 'Exports' ) %>%
    group_by( Year, Type_gs) %>%
    summarise( Value = sum( Value, na.rm=T ) ) %>%
    ungroup %>%
    mutate( Name = paste0(Type_gs,' exports') ) %>%
    dplyr::select( -Type_gs )
  
  tmp_country_all_tot_im <- 
    tmp_country_all_base %>%
    filter( Type_ie == 'Imports' ) %>%
    group_by( Year) %>%
    summarise( Value = sum( Value, na.rm=T ) ) %>%
    ungroup %>%
    mutate( Name = 'Total imports' ) 
  
  tmp_country_all_gs_im <- 
    tmp_country_all_base %>%
    filter( Type_ie == 'Imports' ) %>%
    group_by( Year, Type_gs) %>%
    summarise( Value = sum( Value, na.rm=T ) ) %>%
    ungroup %>%
    mutate( Name = paste0(Type_gs,' imports') ) %>%
    dplyr::select( -Type_gs )
  
  tmp_dtf_country_all <-
    tmp_country_all_twoway %>%
    bind_rows(tmp_country_all_balance) %>%
    bind_rows(tmp_country_all_balance_g) %>%
    bind_rows(tmp_country_all_balance_s) %>%
    bind_rows( tmp_country_all_tot_ex ) %>%
    bind_rows( tmp_country_all_gs_ex ) %>%
    bind_rows( tmp_country_all_tot_im ) %>%
    bind_rows( tmp_country_all_gs_im ) %>%
    mutate( Country = 'Total selected countries' )
  
  ## for world
  tmp_world_base <- 
    dtf_shiny_country_gs %>%
    filter( Country %in% 'World',
            Year >= 2007 ) 
  
  tmp_world_twoway <-
    tmp_world_base %>%
    group_by( Year ) %>%
    summarise( Value = sum( Value, na.rm=T ) ) %>%
    ungroup %>%
    mutate( Name = 'Two-way trade' ) 
  
  tmp_world_balance <- 
    tmp_world_base %>%
    group_by( Year, Type_ie ) %>%
    summarise( Value = sum( Value, na.rm=T ) ) %>%
    ungroup %>%
    group_by( Year ) %>%
    summarise( Value = Value[Type_ie=='Exports'] - Value[Type_ie=='Imports'] ) %>%
    ungroup %>%
    mutate( Name = 'Trade balance' ) 
  
  tmp_world_balance_g <- 
    tmp_world_base %>%
    filter( Type_gs == 'Goods') %>%
    group_by( Year, Type_ie ) %>%
    summarise( Value = sum( Value, na.rm=T ) ) %>%
    ungroup %>%
    group_by( Year ) %>%
    summarise( Value = Value[Type_ie=='Exports'] - Value[Type_ie=='Imports'] ) %>%
    ungroup %>%
    mutate( Name = 'Goods balance' ) 
  
  tmp_world_balance_s <- 
    tmp_world_base %>%
    filter( Type_gs == 'Services') %>%
    group_by( Year, Type_ie ) %>%
    summarise( Value = sum( Value, na.rm=T ) ) %>%
    ungroup %>%
    group_by( Year ) %>%
    summarise( Value = Value[Type_ie=='Exports'] - Value[Type_ie=='Imports'] ) %>%
    ungroup %>%
    mutate( Name = 'Services balance' ) 
  
  tmp_world_tot_ex <- 
    tmp_world_base %>%
    filter( Type_ie == 'Exports' ) %>%
    group_by( Year) %>%
    summarise( Value = sum( Value, na.rm=T ) ) %>%
    ungroup %>%
    mutate( Name = 'Total exports' ) 
  
  tmp_world_gs_ex <- 
    tmp_world_base %>%
    filter( Type_ie == 'Exports' ) %>%
    group_by( Year, Type_gs) %>%
    summarise( Value = sum( Value, na.rm=T ) ) %>%
    ungroup %>%
    mutate( Name = paste0(Type_gs,' exports') ) %>%
    dplyr::select( -Type_gs )
  
  tmp_world_tot_im <- 
    tmp_world_base %>%
    filter( Type_ie == 'Imports' ) %>%
    group_by( Year) %>%
    summarise( Value = sum( Value, na.rm=T ) ) %>%
    ungroup %>%
    mutate( Name = 'Total imports' ) 
  
  tmp_world_gs_im <- 
    tmp_world_base %>%
    filter( Type_ie == 'Imports' ) %>%
    group_by( Year, Type_gs) %>%
    summarise( Value = sum( Value, na.rm=T ) ) %>%
    ungroup %>%
    mutate( Name = paste0(Type_gs,' imports') ) %>%
    dplyr::select( -Type_gs )
  
  tmp_dtf_world <-
    tmp_world_twoway %>%
    bind_rows(tmp_world_balance) %>%
    bind_rows(tmp_world_balance_g) %>%
    bind_rows(tmp_world_balance_s) %>%
    bind_rows( tmp_world_tot_ex ) %>%
    bind_rows( tmp_world_gs_ex ) %>%
    bind_rows( tmp_world_tot_im ) %>%
    bind_rows( tmp_world_gs_im ) %>%
    mutate( Country = 'World' )
  
  ### produce summary
  tmp_all <-
    tmp_dtf_country_all %>%
    bind_rows( tmp_dtf_world ) 
  
  tmp_value <-
    tmp_all %>%
    filter( Country != 'World', Year == max(Year) ) %>%
    dplyr::select( -Country )
  
  
  tmp_share <- 
    tmp_all %>%
    group_by( Year, Name ) %>%
    #summarise( 'Share' = Value[Country=='Total selected countries']/Value[Country=='World'] ) %>%
    do( Share = .$Value[.$Country=='Total selected countries']/.$Value[.$Country=='World'] ) %>%
    ungroup %>%
    mutate( Share = as.numeric(Share) ) %>%
    mutate( Share = ifelse( Name %in% c('Trade balance','Goods balance','Services balance'), NA, Share ) ) %>%
    filter( Year == max(Year) )
  
  tmp_cagr <-
    tmp_all %>%
    filter( Country != 'World' ) %>%
    group_by( Name, Country ) %>%
    do( CAGR1 = CAGR(.$Value[.$Year==max(.$Year)]/
                       .$Value[.$Year== (max(.$Year)-1) ], 1)/100,
        CAGR5 = CAGR(.$Value[.$Year==max(.$Year)]/
                       .$Value[.$Year== (max(.$Year)-5) ], 5)/100,
        CAGR10 = CAGR(.$Value[.$Year==max(.$Year)]/
                        .$Value[.$Year== (max(.$Year)-10) ], 10)/100
    ) %>%
    mutate( CAGR1 = ifelse(length(CAGR1)==0,NA, as.numeric( CAGR1)),
            CAGR5 =  ifelse(length(CAGR5)==0,NA, as.numeric( CAGR5)) ,
            CAGR10 = ifelse(length(CAGR10)==0,NA, as.numeric( CAGR10)) 
    ) %>%
    mutate( CAGR1 = ifelse( Name %in% c('Trade balance','Goods balance','Services balance'), NA, CAGR1 ),
            CAGR5 = ifelse( Name %in% c('Trade balance','Goods balance','Services balance'), NA, CAGR5 ),
            CAGR10 = ifelse( Name %in% c('Trade balance','Goods balance','Services balance'), NA, CAGR10 )
    )
  
  tmp_tab <-
    tmp_value %>%
    left_join( tmp_share ) %>%
    left_join( tmp_cagr ) %>%
    mutate( Name = factor(Name, levels = c('Total exports', 'Goods exports', 'Services exports',
                                           'Total imports','Goods imports', 'Services imports',
                                           'Two-way trade', 'Trade balance','Goods balance', 'Services balance') ),
            Value = Value/10^6) %>%
    dplyr::select( Name, Value, Share, CAGR1, CAGR5, CAGR10) %>%
    arrange( Name )
  
  return(tmp_tab)
}

## produce summary country table individually ---------------
sum_selected_country_individual <- function( arg_countries){
  required_name <- c("Total exports", "Goods exports", 'Services exports',
                     "Total imports", 'Goods imports', 'Services imports',
                     "Two-way trade", "Trade balance", "Goods balance", "Services balance")
  tab_list <- 
    lapply( arg_countries,
            function(i_country){
              print(i_country)
              tmp_tab <- sum_selected_country( i_country )
              ## check if any missing
              missing_name <- setdiff( required_name, tmp_tab$Name)
              
              if( length(missing_name) > 0 ){
                for(i_name in missing_name){
                  tmp_tab <- 
                    tmp_tab %>%
                    bind_rows( data.frame(Name = i_name) )
                }
              }
              return(tmp_tab)
            })
  
  names(tab_list) <- arg_countries
  
  
  ex_tab <-
    do.call( 'rbind', 
             lapply( arg_countries,
                     function(i_country){
                       tmp_tab <- tab_list[[i_country]]
                       tmp_tab_ex <-
                         data.frame( Country = i_country,
                                     TotExValue = tmp_tab$Value[tmp_tab$Name=='Total exports'],
                                     TotExShare = tmp_tab$Share[tmp_tab$Name=='Total exports'],
                                     TotExCAGR5 = tmp_tab$CAGR5[tmp_tab$Name=='Total exports'],
                                     GExValue = tmp_tab$Value[tmp_tab$Name=='Goods exports'],
                                     GExShare = tmp_tab$Share[tmp_tab$Name=='Goods exports'],
                                     GExCAGR5 = tmp_tab$CAGR5[tmp_tab$Name=='Goods exports'],
                                     SExValue = tmp_tab$Value[tmp_tab$Name=='Services exports'],
                                     SExShare = tmp_tab$Share[tmp_tab$Name=='Services exports'],
                                     SExCAGR5 = tmp_tab$CAGR5[tmp_tab$Name=='Services exports']
                         )
                       return( tmp_tab_ex) 
                     }) )
  
  ex_tab %<>% arrange(-TotExValue)
  
  
  im_tab <- 
    do.call( 'rbind', 
             lapply( arg_countries,
                     function(i_country){
                       tmp_tab <- tab_list[[i_country]]
                       tmp_tab_im <-
                         data.frame( Country = i_country,
                                     TotImValue = tmp_tab$Value[tmp_tab$Name=='Total imports'],
                                     TotImShare = tmp_tab$Share[tmp_tab$Name=='Total imports'],
                                     TotImCAGR5 = tmp_tab$CAGR5[tmp_tab$Name=='Total imports'],
                                     GImValue = tmp_tab$Value[tmp_tab$Name=='Goods imports'],
                                     GImShare = tmp_tab$Share[tmp_tab$Name=='Goods imports'],
                                     GImCAGR5 = tmp_tab$CAGR5[tmp_tab$Name=='Goods imports'],
                                     SImValue = tmp_tab$Value[tmp_tab$Name=='Services imports'],
                                     SImShare = tmp_tab$Share[tmp_tab$Name=='Services imports'],
                                     SImCAGR5 = tmp_tab$CAGR5[tmp_tab$Name=='Services imports']
                         )
                       return(tmp_tab_im ) }) )
  im_tab %<>% arrange(-TotImValue)
  
  twoway_balance_tab <- 
    do.call( 'rbind', 
             lapply( arg_countries,
                     function(i_country){
                       tmp_tab <- tab_list[[i_country]]
                       tmp_tab_tb <-
                         data.frame( Country = i_country,
                                     TwowayValue = tmp_tab$Value[tmp_tab$Name=='Two-way trade'],
                                     TwowayShare = tmp_tab$Share[tmp_tab$Name=='Two-way trade'],
                                     TwowayCAGR5 = tmp_tab$CAGR5[tmp_tab$Name=='Two-way trade'],
                                     BalanceValue = tmp_tab$Value[tmp_tab$Name=='Trade balance'],
                                     BalanceValue_g = tmp_tab$Value[tmp_tab$Name=='Goods balance'],
                                     BalanceValue_s = tmp_tab$Value[tmp_tab$Name=='Services balance']
                                     
                         )
                       return( tmp_tab_tb  ) }) )
  
  twoway_balance_tab %<>% arrange(-TwowayValue)
  
  return( list(Ex = ex_tab, Im = im_tab, TB = twoway_balance_tab ))
}

## function to production SNZ defined commodity by country ------------------
get_snz_gs_country <- function( Exports_or_imports = 'Exports', selected_country ){
  ## Exports ---------------------------
  if( Exports_or_imports == 'Exports' ){
    tmp_dtf_commodity_tot <- 
      dtf_shiny_full %>%
      filter( Type_ie == Exports_or_imports ,
              Country %in% selected_country,
              Type_gs == 'Goods',
              Commodity %in% c("Total goods"),
              Year >= 2007 ) %>%
      rename( SNZ_commodity = Commodity ) %>%
      group_by( Year, Type_ie, Type_gs, SNZ_commodity, Note ) %>%
      summarise( Value = sum(Value, na.rm=T) ) %>%
      ungroup %>%
      mutate( Country = 'The selected countries' )
    
    tmp_dtf_commodity_country <- 
      dtf_shiny_full %>%
      filter( Type_ie == Exports_or_imports ,
              Country %in% selected_country,
              !Commodity %in% c("Total goods", "Total services"),
              Type_gs == 'Goods', Year>=2007) %>%
      left_join( concord_snz_eg, by = c('Commodity' = 'HS_codes') ) %>%
      filter( !is.na(SNZ_commodity) ) %>%
      group_by( Year, Type_ie, Type_gs, SNZ_commodity, Note) %>%
      summarise( Value = sum(Value, na.rm=T) ) %>%
      ungroup %>%
      mutate( Country = 'The selected countries' ) %>%
      bind_rows( tmp_dtf_commodity_tot )
    
    tmp_dtf_other_commodity <-
      tmp_dtf_commodity_country %>%
      group_by( Year, Type_ie, Type_gs, Note ) %>%
      do( Value = .$Value[.$SNZ_commodity=='Total goods'] - sum(.$Value[.$SNZ_commodity!='Total goods']) ) %>%
      ungroup %>%
      mutate( Value = unlist(Value) ) %>%
      mutate( SNZ_commodity = 'Other goods' ) %>%
      mutate( Country = 'The selected countries' ) 
    
    ## services
    tmp_dtf_service_tot <- 
      dtf_shiny_full %>%
      filter( Type_ie == Exports_or_imports ,
              Country %in% selected_country,
              Type_gs == 'Services',
              Commodity %in% c("Total services"),
              Year >= 2007 ) %>%
      rename( SNZ_commodity = Commodity ) %>%
      group_by( Year, Type_ie, Type_gs, SNZ_commodity, Note ) %>%
      summarise( Value = sum(Value, na.rm=T) ) %>%
      ungroup %>%
      mutate( Country = 'The selected countries' )
    
    tmp_dtf_service_country <- 
      dtf_shiny_full %>%
      filter( Type_ie == Exports_or_imports ,
              Country %in% selected_country,
              !Commodity %in% c("Total goods", "Total services"),
              Type_gs == 'Services', Year>=2007) %>%
      rename(SNZ_commodity = Commodity ) %>%
      group_by( Year, Type_ie, Type_gs, SNZ_commodity, Note ) %>%
      summarise( Value = sum(Value, na.rm=T) ) %>%
      ungroup %>%
      bind_rows( tmp_dtf_service_tot ) %>%
      mutate( Country = 'The selected countries' )
    
    ## just to balance the offical total service exports and the sum of service exports by country
    tmp_dtf_other_service <-
      tmp_dtf_service_country %>%
      group_by(  Year, Type_ie, Type_gs, Note) %>%
      do( Value = .$Value[.$SNZ_commodity=='Total services'] - sum(.$Value[.$SNZ_commodity!='Total services']) ) %>%
      ungroup %>%
      mutate( Country = 'The selected countries' )
    
    if( nrow(tmp_dtf_other_service) >0 ){
      tmp_dtf_other_service %<>%
        mutate( Value = ifelse(!is.null(Value), unlist(Value), NA ) ) %>%
        mutate( SNZ_commodity = 'Other services' )
    }
    
    ### bind toghter 
    tmp_dtf_commodity_service_ex_country <-
      tmp_dtf_commodity_country %>%
      filter( SNZ_commodity != 'Total goods' ) %>%
      bind_rows( tmp_dtf_other_commodity ) %>%
      bind_rows(
        tmp_dtf_service_country %>%
          filter( SNZ_commodity != 'Total services' ) %>%
          bind_rows( tmp_dtf_other_service  ) %>%
          group_by( Year, Type_ie, Type_gs, SNZ_commodity, Note, Country ) %>%
          summarise( Value = sum(Value, na.rm=T) ) %>%
          ungroup
      ) #%>%
    #bind_rows( tmp_dtf_other_service  ) 
    
    ## calculate CAGR5
    tmp_dtf_commodity_service_ex_country %<>%
      left_join(tmp_dtf_commodity_service_ex_country %>%
                  group_by(  Type_ie, Type_gs, SNZ_commodity, Note) %>%
                  do( CAGR5 = CAGR( .$Value[.$Year == max(.$Year)]/
                                      .$Value[.$Year == (max(.$Year)-5) ], 5) ) %>%
                  ungroup %>%
                  mutate( CAGR5 = as.numeric(CAGR5) ) %>%
                  mutate( CAGR5 = ifelse(CAGR5==Inf, NA, CAGR5) )
      )
    
    #### select world
    tmp_dtf_commodity_service_world <-
      dtf_shiny_commodity_service_ex %>%
      filter( Type_ie == Exports_or_imports ,
              Country %in% 'World',
              Year >= 2007  )
    
    ## calculate share
    tmp_dtf_commodity_service_ex_country %<>%
      bind_rows( tmp_dtf_commodity_service_world ) %>%
      group_by( Year, Type_ie, Type_gs, SNZ_commodity, Note ) %>%
      mutate( Share = Value/Value[Country=='World']*100 ) %>%
      ungroup %>%
      filter( Country != 'World' ) %>%
      filter( !is.na(CAGR5), Share>=0 )
    
    return(tmp_dtf_commodity_service_ex_country)
  }
  #### Improts ----------------------------
  if( Exports_or_imports == 'Imports' ){
    tmp_dtf_commodity_tot <- 
      dtf_shiny_full %>%
      filter( Type_ie == Exports_or_imports ,
              Country %in% selected_country,
              Type_gs == 'Goods',
              Commodity %in% c("Total goods"),
              Year >= 2007 ) %>%
      rename( SNZ_commodity = Commodity ) %>%
      group_by( Year, Type_ie, Type_gs, SNZ_commodity, Note ) %>%
      summarise( Value = sum(Value, na.rm=T) ) %>%
      ungroup %>%
      mutate( Country = 'The selected countries' )
    
    tmp_dtf_commodity_country <- 
      dtf_shiny_full %>%
      filter( Type_ie == Exports_or_imports ,
              Country %in% selected_country,
              !Commodity %in% c("Total goods", "Total services"),
              Type_gs == 'Goods', Year>=2007) %>%
      left_join( concord_snz_ig, by = c('Commodity' = 'HS_codes') ) %>%
      filter( !is.na(SNZ_commodity) ) %>%
      group_by( Year, Type_ie, Type_gs, SNZ_commodity, Note) %>%
      summarise( Value = sum(Value, na.rm=T) ) %>%
      ungroup %>%
      mutate( Country = 'The selected countries' ) %>%
      bind_rows( tmp_dtf_commodity_tot )
    
    tmp_dtf_other_commodity <-
      tmp_dtf_commodity_country %>%
      group_by( Year, Type_ie, Type_gs, Note ) %>%
      do( Value = .$Value[.$SNZ_commodity=='Total goods'] - sum(.$Value[.$SNZ_commodity!='Total goods']) ) %>%
      ungroup %>%
      mutate( Value = unlist(Value) ) %>%
      mutate( SNZ_commodity = 'Other goods' ) %>%
      mutate( Country = 'The selected countries' ) 
    
    ## services
    tmp_dtf_service_tot <- 
      dtf_shiny_full %>%
      filter( Type_ie == Exports_or_imports ,
              Country %in% selected_country,
              Type_gs == 'Services',
              Commodity %in% c("Total services"),
              Year >= 2007 ) %>%
      rename( SNZ_commodity = Commodity ) %>%
      group_by( Year, Type_ie, Type_gs, SNZ_commodity, Note ) %>%
      summarise( Value = sum(Value, na.rm=T) ) %>%
      ungroup %>%
      mutate( Country = 'The selected countries' )
    
    tmp_dtf_service_country <- 
      dtf_shiny_full %>%
      filter( Type_ie == Exports_or_imports ,
              Country %in% selected_country,
              !Commodity %in% c("Total goods", "Total services"),
              Type_gs == 'Services', Year>=2007) %>%
      rename(SNZ_commodity = Commodity ) %>%
      group_by( Year, Type_ie, Type_gs, SNZ_commodity, Note ) %>%
      summarise( Value = sum(Value, na.rm=T) ) %>%
      ungroup %>%
      bind_rows( tmp_dtf_service_tot ) %>%
      mutate( Country = 'The selected countries' )
    
    ## just to balance the offical total service exports and the sum of service exports by country
    tmp_dtf_other_service <-
      tmp_dtf_service_country %>%
      group_by(  Year, Type_ie, Type_gs, Note) %>%
      do( Value = .$Value[.$SNZ_commodity=='Total services'] - sum(.$Value[.$SNZ_commodity!='Total services']) ) %>%
      ungroup %>%
      mutate( Country = 'The selected countries' )
    
    if( nrow(tmp_dtf_other_service) >0 ){
      tmp_dtf_other_service %<>%
        mutate( Value = ifelse(!is.null(Value), unlist(Value), NA ) ) %>%
        mutate( SNZ_commodity = 'Other services' )
    }
    
    ### bind toghter 
    tmp_dtf_commodity_service_im_country <-
      tmp_dtf_commodity_country %>%
      filter( SNZ_commodity != 'Total goods' ) %>%
      bind_rows( tmp_dtf_other_commodity ) %>%
      bind_rows(
        tmp_dtf_service_country %>%
          filter( SNZ_commodity != 'Total services' )%>%
          bind_rows( tmp_dtf_other_service  ) %>%
          group_by( Year, Type_ie, Type_gs, SNZ_commodity, Note, Country ) %>%
          summarise( Value = sum(Value, na.rm=T) ) %>%
          ungroup
      ) #%>%
    #bind_rows( tmp_dtf_other_service  ) 
    
    ## calculate CAGR5
    tmp_dtf_commodity_service_im_country %<>%
      left_join(tmp_dtf_commodity_service_im_country %>%
                  group_by(  Type_ie, Type_gs, SNZ_commodity, Note) %>%
                  do( CAGR5 = CAGR( .$Value[.$Year == max(.$Year)]/
                                      .$Value[.$Year == (max(.$Year)-5) ], 5) ) %>%
                  ungroup %>%
                  mutate( CAGR5 = as.numeric(CAGR5) ) %>%
                  mutate( CAGR5 = ifelse(CAGR5==Inf, NA, CAGR5) )
      )
    
    #### select world
    tmp_dtf_commodity_service_world <-
      dtf_shiny_commodity_service_im %>%
      filter( Type_ie == Exports_or_imports ,
              Country %in% 'World',
              Year >= 2007  )
    
    ## calculate share
    tmp_dtf_commodity_service_im_country %<>%
      bind_rows( tmp_dtf_commodity_service_world ) %>%
      group_by( Year, Type_ie, Type_gs, SNZ_commodity, Note ) %>%
      mutate( Share = Value/Value[Country=='World']*100 ) %>%
      ungroup %>%
      filter( Country != 'World' ) %>%
      filter( !is.na(CAGR5), Share>=0 )
    
    return(tmp_dtf_commodity_service_im_country)
  }
}

# Function to call in place of dropdownMenu --------------
customSentence <- function(numItems, type) {
  paste("Feedback & suggestions")
}

customSentence_share <- function(numItems, type) {
  paste("Love it? Share it!")
}

##

##
dropdownMenuCustom <-     function (..., type = c("messages", "notifications", "tasks"), 
                                    badgeStatus = "primary", icon = NULL, .list = NULL, customSentence = customSentence) 
{
  type <- match.arg(type)
  if (!is.null(badgeStatus)) shinydashboard:::validateStatus(badgeStatus)
  items <- c(list(...), .list)
  lapply(items, shinydashboard:::tagAssert, type = "li")
  dropdownClass <- paste0("dropdown ", type, "-menu")
  if (is.null(icon)) {
    icon <- switch(type, messages = shiny::icon("envelope"), 
                   notifications = shiny::icon("warning"), tasks = shiny::icon("tasks"))
  }
  numItems <- length(items)
  if (is.null(badgeStatus)) {
    badge <- NULL
  }
  else {
    badge <- tags$span(class = paste0("label label-", badgeStatus), 
                       numItems)
  }
  tags$li(
    class = dropdownClass, 
    a(
      href = "#", 
      class = "dropdown-toggle", 
      `data-toggle` = "dropdown", 
      icon, 
      badge
    ), 
    tags$ul(
      class = "dropdown-menu", 
      tags$li(
        class = "header", 
        customSentence(numItems, type)
      ), 
      tags$li(
        tags$ul(class = "menu", items)
      )
    )
  )
}
