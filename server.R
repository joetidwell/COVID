library(shiny)
library(shinyBS)
library(shinydashboard)
library(ggplot2)
library(scales)
library(data.table)
library(plotly)
library(jsonlite)
library(dplyr)
library(readxl)

library(rgdal)
library(RColorBrewer)
library(leaflet)
library(tigris)


# library(ggthemes)

theme_set(theme_classic())


###
### Colors
###

c.fig.bg <- "#1e282c"


###
### COVID Data
###


txt <- "Data Source: https://cosacovid-cosagis.hub.arcgis.com/datasets/covid-19-dashboard-data/data?showData=true"
json <- fromJSON("data/SA.json")
# json <- fromJSON("https://services.arcgis.com/g1fRTDLeMgspWrYp/arcgis/rest/services/vDateCOVID19_Tracker_Public/FeatureServer/0/query?where=1%3D1&objectIds=&time=&resultType=none&outFields=*&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&sqlFormat=none&f=pjson&token=")

mydt <- as.data.table(json$features)
mydt <- mydt[!is.na(attributes.Date)]
names(mydt) <- gsub("attributes.","",names(mydt))
mydt[,Date:=as.IDate(as.POSIXct(Date/1000, origin="1970-01-01"))]
mydt.SA <- mydt[,.(Date,COVIDnICU,COVIDonVent,Deceased)]
mydt.SA <- melt(mydt.SA, id.vars=c("Date"))
mydt.SA <- mydt.SA[!is.na(value)]
mydt.SA[,rolling:=frollmean(value, 7, align="center"), by=variable]

levels(mydt.SA$variable) <- c("ICU Patients", "Ventilated Patients", "Patient Deaths")


mydt.vents <- mydt[,.(Date, TotalVents, AvailVent, TotalStaffedBeds, AvailStaffedBeds, COVIDonVent)]
mydt.vents[,perVent:=AvailVent/TotalVents*100]
mydt.vents[,perBeds:=AvailStaffedBeds/TotalStaffedBeds*100]
mydt.vents[,perCovidVent:=COVIDonVent/TotalVents*100]

mydt.vents <- melt(mydt.vents, id.vars=c("Date"))
mydt.vents <- mydt.vents[!is.na(value)]
mydt.vents[,rolling:=frollmean(value, 7, align="center"), by=variable]


levels(mydt.vents$variable) <- c("TotalVents", "AvailVent", "TotalStaffedBeds", "AvailStaffedBeds",
                                 "COVIDonVent", "Available Ventilators", "Available Beds", "Ventilators used<br>by COVID Patients")


# Load cumulative data from https://covidtracking.com/data/state
# mydt.state <- read.csv("https://covidtracking.com/api/v1/states/daily.csv") %>% data.table
mydt.state <- read.csv("data/daily.csv") %>% data.table

mydt.state[,Date:=as.IDate(as.character(date), format="%Y%m%d")]



setkey(mydt.state, Date)
mydt.state[, Deaths := as.numeric(death)]
mydt.state <- mydt.state[!is.na(Deaths)]
mydt.state[,day:=1:.N,by=state]


# Convert cumulative to daily counts
mydt.state[, Deaths := Deaths - shift(Deaths, 1), by=state]
mydt.state[, DeathsRaw := Deaths]

# Convert counts to daily per 1,000,000
pops <- read.csv("data/nst-est2019-01.csv") %>% data.table
setkey(pops, State)
setkey(mydt.state, state)
mydt.state <- pops[mydt.state]
mydt.state[, Deaths := Deaths/Population * 1000000]

# Rolling Average
setkey(mydt.state, Date)
mydt.state[,rollingDeaths:=frollmean(Deaths, 7, align="center"), by=State]

mydt.state[,rankDeath:=rank(death/Population),by=Date]


mydt.state <- mydt.state[,State:=as.character(State)]

mydt.countries <- read.csv("data/countries.csv") %>% data.table
mydt.countries[,Date:=as.IDate(dateRep, format="%d/%m/%Y")]
mydt.countries[,Country:=countriesAndTerritories]
mydt.countries[, deaths := as.numeric(deaths)]
# Because Spain reported -1k+ deaths on a day...
mydt.countries[deaths<0, deaths:=0]
# Adjust for deaths per 1,000,000
mydt.countries[, Deaths:=deaths/popData2019*1000000]
# Rolling Average
mydt.countries[,rollingDeaths:=frollmean(Deaths, 7, align="center"), by=Country]
# Start from day of first reported death
mydt.baselined <- mydt.countries[deaths!=0]
mydt.baselined[, Date:=as.IDate(dateRep, format="%d/%m/%Y")]
setkey(mydt.baselined, Date)
mydt.baselined[,day:=1:.N,by=Country]
# Rolling Average
mydt.baselined[,rollingDeaths:=frollmean(Deaths, 7, align="center"), by=Country]

mydt.baselined[,Country:=as.character(Country)]


###
### Texas DSHS data
###

###
### By-county state deaths, cumulative
###

fname <- "data/TexasCOVID19DailyCountyFatalityCountData.xlsx"
colnames <- read_excel(fname, sheet = 1, col_names = FALSE, col_types = NULL, na = "")[3,] %>% as.character()
colnames <- c("County","Population",as.character(seq.Date(from=as.IDate("2020-03-04"),by="day",length.out=length(colnames)-2)))
mydt.tx <- read_excel(fname, sheet = 1, col_names = FALSE, col_types = NULL, na = "")[-c(1:3,259:267),] %>%
  data.table()
setnames(mydt.tx, colnames)
mydt.tx <- melt(mydt.tx, id.vars=c("County","Population"), variable.name="Date", value.name="Deaths.Cumulative") %>%
  data.table()
mydt.tx[,Date:=as.IDate(Date)]
mydt.tx[,Population:=as.numeric(Population)]
# Convert cumulative to daily counts
setkey(mydt.tx,Date)
mydt.tx[,Deaths.Cumulative:=as.numeric(Deaths.Cumulative)]
mydt.tx[, Deaths := Deaths.Cumulative - shift(Deaths.Cumulative, 1), by=County]


###
### By-county state cases, cumulative
###

fname <- "data/TexasCOVID19DailyCountyCaseCountData.xlsx"
colnames <- read_excel(fname, sheet = 1, col_names = FALSE, col_types = NULL, na = "")[3,] %>% as.character()
colnames <- c("County",
              "Population",
              as.character(seq.Date(from=as.IDate("2020-03-04"),by="day",length.out=3)),
              as.character(seq.Date(from=as.IDate("2020-03-09"),by="day",length.out=5)),
              as.character(seq.Date(from=as.IDate("2020-03-15"),by="day",length.out=length(colnames)-10))              
              )
mydt.tx.case <- read_excel(fname, sheet = 1, col_names = FALSE, col_types = NULL, na = "")[-c(1:3,259:268),] %>%
  data.table()
setnames(mydt.tx.case, colnames)
mydt.tx.case <- melt(mydt.tx.case, id.vars=c("County","Population"), variable.name="Date", value.name="Cases.Cumulative") %>%
  data.table()
mydt.tx.case[,Cases.Cumulative := as.numeric(Cases.Cumulative)]
mydt.tx.case[,Date:=as.IDate(Date)]
mydt.tx.case[,Population:=as.numeric(Population)]
# Convert cumulative to daily counts
setkey(mydt.tx.case,Date)
mydt.tx.case[, Cases := Cases.Cumulative - shift(Cases.Cumulative, 1), by=County]
setkeyv(mydt.tx.case,c("County","Date"))
setkeyv(mydt.tx,c("County","Date"))
mydt.tx <- mydt.tx.case[,.(County,Date,Cases.Cumulative,Cases)][mydt.tx]

###
### Nursing home and long term facility deaths, state-wide
###

fname <- "data/COVID-19OutbreaksinLong-termCareFacilities.xlsx"
nurse.dt <- read_excel(fname, sheet = 1, col_names = FALSE, col_types = NULL, na = "")[11,3:5] %>% data.table()
setnames(nurse.dt, c("Confirmed Cases","Fatalities","Reported Recoveries"))
nurse.dt[,Source:="Nursing Homes"]
fname <- "data/COVID-19OutbreaksinLong-termCareFacilities.xlsx"
long.dt <- read_excel(fname, sheet = 2, col_names = FALSE, col_types = NULL, na = "")[11,3:5] %>% data.table()
setnames(long.dt, c("Confirmed Cases","Fatalities","Reported Recoveries"))
long.dt[,Source:="Long-Term Care Facilities"]
nurse.tx.dt <- rbind(nurse.dt, long.dt)




###
### Mapping Data
###

load(file="data/txCounties.RData")
# Merge sata with shapefile
tmpdata <- data.table(county=unique(tx.counties$NAME), value=1:length(unique(tx.counties$NAME)))
tmpdt <- mydt.tx[Date==max(Date),
                 .(d.per.1000=Deaths.Cumulative/Population*100000,
                   Deaths=Deaths.Cumulative),
                 by=County]
tmpdt[d.per.1000==0,d.per.1000:=NA]
tx.counties_merged_sb <- geo_join(tx.counties, tmpdt, "NAME", "County")


# Getting rid of rows with NA values
# Using the Base R method of filtering subset() because we're dealing with a SpatialPolygonsDataFrame and not a normal data frame, thus filter() wouldn't work
tx.counties_merged_sb <- subset(tx.counties_merged_sb, !is.na(d.per.1000))
pal <- colorBin("Reds", 
                domain=tx.counties_merged_sb$d.per.1000, 
                bins=c(0,1,5,10,25,50,75,100), 
                reverse=FALSE)


# Setting up the pop up text
popup_sb <- paste0("<b>",as.character(tx.counties_merged_sb$County)," County</b><br/>" ,
                   "<em>Deaths per 100k</em>: ", as.character(round(tx.counties_merged_sb$d.per.1000,2)),"<br/>",
                   "<em>Total Deaths</em>: ", as.character(tx.counties_merged_sb$Deaths))
popup_sb <- lapply(popup_sb, htmltools::HTML)







####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Some functions
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


tscovid1 <- function(txtsize=1, inflation=FALSE) {

    plot_ly(data = mydt.SA[!(variable%in%"Patient Deaths")], 
            y = ~rolling, 
            x = ~Date,
            color = ~variable,
            type = "scatter",
            mode = "lines",
            legendgroup = ~variable,
            hoverinfo = "text",
            text = ~paste('</br>', Date,
                          '</br>', `variable`,
                          '</br> 7 Day Moving Avg : ', round(`rolling`,2))
      ) %>%
      add_trace(y = ~value, 
                name = 'Daily Count', 
                mode = 'markers', 
                color = ~variable,
                hoverinfo = "text",
                legendgroup = ~variable,
                text = ~paste('</br>', Date,
                              '</br>', variable, ': ', value),
                showlegend = FALSE,
                opacity = 1
      ) %>%
      layout(
        xaxis = list(title="Date", 
                     tickcolor = "#666",
                     gridcolor = "#666",
                     zerolinecolor = "#CCC"),
        yaxis = list(title="Count", 
                     tickcolor = "#666",
                     gridcolor = "#666",
                     zerolinecolor = "#CCC"),
        font = list(color="#CCCCCC"),
        paper_bgcolor = c.fig.bg,
        plot_bgcolor = c.fig.bg,
        margin = list(l = 50, r=20, t=50)    
      ) %>%
      config(displayModeBar = TRUE,
             displaylogo = FALSE,
             cloud = FALSE,
             modeBarButtonsToRemove = c('sendDataToCloud','hoverCompareCartesian','zoom2d')
      ) 
 
}


tscovid1Deaths <- function(txtsize=1, inflation=FALSE) {

    plot_ly(data = mydt.SA[(variable%in%"Patient Deaths")], 
            y = ~rolling, 
            x = ~Date,
            color = ~variable,
            type = "scatter",
            mode = "lines",
            legendgroup = ~variable,
            hoverinfo = "text",
            text = ~paste('</br>', Date,
                          '</br>', `variable`,
                          '</br> 7 Day Moving Avg : ', round(`rolling`,2))
      ) %>%
      add_trace(y = ~value, 
                name = 'Daily Count', 
                mode = 'markers', 
                color = ~variable,
                hoverinfo = "text",
                legendgroup = ~variable,
                text = ~paste('</br>', Date,
                              '</br>', variable, ': ', value),
                showlegend = FALSE,
                opacity = 1
      ) %>%
      layout(
        xaxis = list(title="Date", 
                     tickcolor = "#666",
                     gridcolor = "#666",
                     zerolinecolor = "#CCC"),
        yaxis = list(title="Count", 
                     tickcolor = "#666",
                     gridcolor = "#666",
                     zerolinecolor = "#CCC"),
        font = list(color="#CCCCCC"),
        paper_bgcolor = c.fig.bg,
        plot_bgcolor = c.fig.bg,
        margin = list(l = 50, r=20, t=50)    
      ) %>%
      config(displayModeBar = TRUE,
             displaylogo = FALSE,
             cloud = FALSE,
             modeBarButtonsToRemove = c('sendDataToCloud','hoverCompareCartesian','zoom2d')
      ) 
 
}

tsCovidVentsSA <- function() {



    plot_ly(data = mydt.vents[variable %in% c("Available Ventilators", "Available Beds", "Ventilators used<br>by COVID Patients")], 
            y = ~rolling, 
            x = ~Date,
            color = ~variable,
            type = "scatter",
            mode = "lines",
            hoverinfo = "text",
            legendgroup = ~variable,
            showlegend = TRUE,
            text = ~paste('</br>', Date,
                          '</br>', `variable`,
                          '</br> 7 Day Moving Avg : ', round(`rolling`,2))
      ) %>%
      add_markers(y = ~value, 
                legendgroup = ~variable,
                mode = 'markers', 
                color = ~variable,
                hoverinfo = "text",
                text = ~paste('</br>', Date,
                              '</br>', variable, ': ', round(value,1), "%"),
                showlegend = FALSE,
                opacity = 1
      ) %>%
      layout(
        xaxis = list(title="Date", 
                     tickcolor = "#666",
                     gridcolor = "#666",
                     zerolinecolor = "#CCC"),
        yaxis = list(title="Percentage", 
                     range=c(0,100),
                     tickcolor = "#666",
                     gridcolor = "#666",
                     zerolinecolor = "#CCC"),
        font = list(color="#CCCCCC"),
        paper_bgcolor = c.fig.bg,
        plot_bgcolor = c.fig.bg,
        margin = list(l = 50, r=20, t=50)    
      ) %>%
      config(displayModeBar = TRUE,
             displaylogo = FALSE,
             cloud = FALSE,
             modeBarButtonsToRemove = c('sendDataToCloud','hoverCompareCartesian','zoom2d')
      ) 


}


tsCovidStates <- function(input) {

    if(is.null(input$selectState)) {
      choices <- c("TX","NY","MD")
    } else {
      choices <- input$selectState  
    }
    # tmp <- copy(mydt.state[State%in%choices])    
    # tmp[,State:=as.character(State)]

    plot_ly(data = mydt.state[State%in%choices], 
            y = ~rollingDeaths, 
            x = ~day,
            color = ~State,
            legendgroup = ~State,
            type = "scatter",
            mode = "lines",
            hoverinfo = "text",
            text = ~paste('</br>', day,
                          '</br>', State,
                          '</br> 7 Day Moving Avg : ', round(`rollingDeaths`,2))
      ) %>%
      add_markers(y = ~Deaths, 
                name = 'Count', 
                mode = 'markers', 
                legendgroup = ~State,
                color = ~State,
                hoverinfo = "text",
                text = ~paste('</br>', State,
                              '</br>', Date,
                              '</br>',day, 'days since 1st death',
                              '</br> Deaths : ', comma(round(DeathsRaw)), ' (',round(Deaths, 1), '/ 1M)'),
                showlegend = FALSE,
                opacity = .25
      ) %>%
      layout(
        xaxis = list(title="Days Since First Reported Death", 
                     tickcolor = "#666",
                     gridcolor = "#666",
                     zerolinecolor = "#CCC"),
        yaxis = list(title="Deaths per 1,000,000 Residents", 
                     tickcolor = "#666",
                     gridcolor = "#666",
                     zerolinecolor = "#CCC"),
        margin = list(l = 50, r=20, t=50),
        font = list(color="#CCCCCC"),
        paper_bgcolor = c.fig.bg,
        plot_bgcolor = c.fig.bg
      ) %>%
      config(displayModeBar = TRUE,
             displaylogo = FALSE,
             cloud = FALSE,
             modeBarButtonsToRemove = c('sendDataToCloud','hoverCompareCartesian','zoom2d')
      ) 

}



tsCovidCountries <- function(input) {

    if(is.null(input$selectCountry)) {
      choices <- c("United_States_of_America","United_Kingdom","Sweden","Germany")
    } else {
      choices <- input$selectCountry  
    }


    plot_ly(data = mydt.baselined[Country%in%choices], 
            y = ~rollingDeaths, 
            x = ~day,
            color = ~Country,
            legendgroup = ~Country,
            type = "scatter",
            mode = "lines",
            hoverinfo = "text",
            text = ~paste('</br>', day,
                          '</br>', Country,
                          '</br> 7 Day Moving Avg : ', round(`rollingDeaths`,2))
      ) %>%
      add_markers(y = ~Deaths, 
                name = 'Count', 
                mode = 'markers', 
                legendgroup = ~Country,
                color = ~Country,
                hoverinfo = "text",
                text = ~paste('</br>', Country,
                              '</br>', Date,
                              '</br>', day, 'days since 1st death',
                              '</br> Deaths : ', comma(round(deaths)), ' (',round(Deaths, 1), '/ 1M)'),
                showlegend = FALSE,
                opacity = .25
      ) %>%
      layout(
        xaxis = list(title="Days Since First Reported Death", 
                     tickcolor = "#666",
                     gridcolor = "#666",
                     zerolinecolor = "#CCC"),
        yaxis = list(title="Deaths per 1,000,000 Residents", 
                     tickcolor = "#666",
                     gridcolor = "#666",
                     zerolinecolor = "#CCC"),
        margin = list(l = 50, r=20, t=50),
        font = list(color="#CCCCCC"),
        paper_bgcolor = c.fig.bg,
        plot_bgcolor = c.fig.bg
      ) %>%
      config(displayModeBar = TRUE,
             displaylogo = FALSE,
             cloud = FALSE,
             modeBarButtonsToRemove = c('sendDataToCloud','hoverCompareCartesian','zoom2d')
      ) 

}


mapTX <- function(input) {
  leaflet() %>%
    addProviderTiles("CartoDB.DarkMatter") %>%
    setView(-98.483330, 38.712046, zoom = 4) %>%
    addPolylines(data = tx.counties, color = "white", opacity = 1, weight = 1) %>%
    addPolygons(data = tx.counties_merged_sb , 
                fillColor = ~pal(tx.counties_merged_sb$d.per.1000), 
                fillOpacity = 0.5,
                # fill=FALSE, 
                weight = 2,
                color="#CCC",
                opacity=0, 
                smoothFactor = 0.2, 
                # popup = ~popup_sb,
                highlight = highlightOptions(
                    weight = 50,
                    color = "#1ce45c",
                    fillOpacity = 1,
                    bringToFront = TRUE),
                label=popup_sb,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto")
    ) %>%
    addLegend(pal = pal, 
              values = tx.counties_merged_sb$d.per.1000, 
              position = "bottomright", 
              title = "Deaths per 100,000") %>%
    setView(-99.9018,31.9686,
                zoom = 6  )    
}

shinyServer(function(input, output, session) {

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### San Antonio Covid
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  output$plotCovidSA <- renderPlotly({
    tscovid1()
  })

  output$plotCovidSADeaths <- renderPlotly({
    tscovid1Deaths()
  })

  output$plotCovidVentsSA <- renderPlotly({
    tsCovidVentsSA()
  })


  output$plotCovidSATX <- renderPlotly({
    tscovid1()
  })

  output$plotCovidDeathsSATX <- renderPlotly({
    tscovid1Deaths()
  })


  output$plotCovidVentsSATX <- renderPlotly({
    tsCovidVentsSA()
  })


  output$plotCovidStates <- renderPlotly({
    tsCovidStates(input)
  })

  output$plotCovidCountries <- renderPlotly({
    tsCovidCountries(input)
  })


  output$variables = renderUI({
    selectInput('variables2', 'Variables', outVar())
  })

  output$boxUSDeaths <- renderValueBox({
    valueBox(
      paste0(comma(mydt.countries[Country=="United_States_of_America",.(Country,Date,deaths)][,sum(deaths)])), 
      "Progress", icon = icon("list"),
      color = "purple"
    )
  })

  output$boxTXDeaths <- renderValueBox({
    valueBox(
      paste0(comma(mydt.tx[County=="Total",max(Deaths.Cumulative)])), 
      "Progress", icon = icon("list"),
      color = "purple"
    )
  })



  output$titleTXDaily <- renderUI({ 
    tmpdate <- mydt.tx[County=="Total",max(Date)]
    HTML(paste0("<span class='box-title-l1'>Texas Daily Summary - ", month.abb[month(tmpdate)], " ", scales::ordinal(mday(tmpdate)) ,"</span>"))
  })


  output$TXTodayDeaths <- renderText({ 
    mydt.tx[County=="Total"][Date==max(Date)]$Deaths
  })


####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Maps
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  output$leafTX <- renderLeaflet({
    mapTX()
  })
  


  # updateSelectizeInput(session, 'selectState', choices = mydt.state[,sort(unique(State))], server = TRUE)


})



