library(shiny)
library(shinyBS)
library(shinydashboard)
library(ggplot2)
library(scales)
library(data.table)
library(plotly)
library(jsonlite)
library(dplyr)
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

levels(mydt.SA$variable) <- c("In ICU", "On Ventilator", "Deaths")


mydt.vents <- mydt[,.(Date, TotalVents, AvailVent, TotalStaffedBeds, AvailStaffedBeds, COVIDonVent)]
mydt.vents[,perVent:=AvailVent/TotalVents*100]
mydt.vents[,perBeds:=AvailStaffedBeds/TotalStaffedBeds*100]
mydt.vents[,perCovidVent:=COVIDonVent/TotalVents*100]

mydt.vents <- melt(mydt.vents, id.vars=c("Date"))
mydt.vents <- mydt.vents[!is.na(value)]
mydt.vents[,rolling:=frollmean(value, 7, align="center"), by=variable]


levels(mydt.vents$variable) <- c("TotalVents", "AvailVent", "TotalStaffedBeds", "AvailStaffedBeds",
                                 "COVIDonVent", "% Available Ventilators", "% Available Beds", "% Ventilators used<br>by COVID Patients")


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


mydt.countries <- read.csv("data/countries.csv") %>% data.table
mydt.countries[,Date:=as.IDate(dateRep, format="%d/%m/%Y")]
mydt.countries[,Country:=countriesAndTerritories]
mydt.countries[, deaths := as.numeric(deaths)]
# Because Spain reported -1k+ deaths on a day...
mydt.countries[deaths<0, deaths:=0]
# Adjust for deaths per 1,000,000
mydt.countries[, Deaths:=deaths/popData2018*1000000]
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




####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Some functions
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


tscovid1 <- function(txtsize=1, inflation=FALSE) {

    plot_ly(data = mydt.SA, 
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



    plot_ly(data = mydt.vents[variable %in% c("% Available Ventilators", "% Available Beds", "% Ventilators used<br>by COVID Patients")], 
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
                              '</br>', variable, ': ', round(value,1)),
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
                opacity = 1
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

shinyServer(function(input, output, session) {

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### San Antonio Covid
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  output$plotCovidSA <- renderPlotly({
    tscovid1()
  })

  output$plotCovidVentsSA <- renderPlotly({
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


  # updateSelectizeInput(session, 'selectState', choices = mydt.state[,sort(unique(State))], server = TRUE)


})



