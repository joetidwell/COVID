library(shiny)
library(shinyBS)
library(shinythemes)
library(shinydashboard)
library(data.table)
library(plotly)
library(dashboardthemes)
library(data.table)
library(dplyr)
library(shinycssloaders)
library(leaflet)

mydt.country <- read.csv("data/countries.csv") %>% data.table
countries <- as.character(mydt.country[, unique(countriesAndTerritories)])



theme_grey_dark <- shinyDashboardThemeDIY(

  ### general
  appFontFamily = "Arial"
  ,appFontColor = "rgb(205,205,205)"
  ,primaryFontColor = "rgb(255,255,255)"
  ,infoFontColor = "rgb(255,255,255)"
  ,successFontColor = "rgb(255,255,255)"
  ,warningFontColor = "rgb(255,255,255)"
  ,dangerFontColor = "rgb(255,255,255)"
  ,bodyBackColor = "rgb(45,55,65)"

  ### header
  ,logoBackColor = "rgb(55,90,127)"

  ,headerButtonBackColor = "rgb(55,90,127)"
  ,headerButtonIconColor = "rgb(205,205,205)"
  ,headerButtonBackColorHover = "rgb(55,90,127)"
  ,headerButtonIconColorHover = "rgb(255,255,255)"

  ,headerBackColor = "rgb(55,90,127)"
  ,headerBoxShadowColor = ""
  ,headerBoxShadowSize = "0px 0px 0px"

  ### sidebar
  ,sidebarBackColor = "rgb(52,62,72)"
  ,sidebarPadding = 0

  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = 0
  ,sidebarMenuBorderRadius = 0

  ,sidebarShadowRadius = ""
  ,sidebarShadowColor = "0px 0px 0px"

  ,sidebarUserTextColor = "rgb(205,205,205)"

  ,sidebarSearchBackColor = "rgb(45,55,65)"
  ,sidebarSearchIconColor = "rgb(153,153,153)"
  ,sidebarSearchBorderColor = "rgb(45,55,65)"

  ,sidebarTabTextColor = "rgb(205,205,205)"
  ,sidebarTabTextSize = 14
  ,sidebarTabBorderStyle = "none"
  ,sidebarTabBorderColor = "none"
  ,sidebarTabBorderWidth = 0

  ,sidebarTabBackColorSelected = "rgb(55,90,127)"
  ,sidebarTabTextColorSelected = "rgb(255,255,255)"
  ,sidebarTabRadiusSelected = "5px"

  ,sidebarTabBackColorHover = "rgb(47,70,95)"
  ,sidebarTabTextColorHover = "rgb(255,255,255)"
  ,sidebarTabBorderStyleHover = "none"
  ,sidebarTabBorderColorHover = "none"
  ,sidebarTabBorderWidthHover = 0
  ,sidebarTabRadiusHover = "5px"

  ### boxes
  ,boxBackColor = "rgb(52,62,72)"
  ,boxBorderRadius = 5
  ,boxShadowSize = "0px 0px 0px"
  ,boxShadowColor = ""
  ,boxTitleSize = 16
  ,boxDefaultColor = "rgb(52,62,72)"
  ,boxPrimaryColor = "rgb(200,200,200)"
  ,boxInfoColor = "rgb(80,95,105)"
  ,boxSuccessColor = "rgb(155,240,80)"
  ,boxWarningColor = "rgb(240,80,210)"
  ,boxDangerColor = "rgb(240,80,80)"

  ,tabBoxTabColor = "rgb(52,62,72)"
  ,tabBoxTabTextSize = 14
  ,tabBoxTabTextColor = "rgb(205,205,205)"
  ,tabBoxTabTextColorSelected = "rgb(205,205,205)"
  ,tabBoxBackColor = "rgb(52,62,72)"
  ,tabBoxHighlightColor = "rgb(70,80,90)"
  ,tabBoxBorderRadius = 5

  ### inputs
  ,buttonBackColor = "rgb(230,230,230)"
  ,buttonTextColor = "rgb(0,0,0)"
  ,buttonBorderColor = "rgb(50,50,50)"
  ,buttonBorderRadius = 5

  ,buttonBackColorHover = "rgb(180,180,180)"
  ,buttonTextColorHover = "rgb(50,50,50)"
  ,buttonBorderColorHover = "rgb(50,50,50)"

  ,textboxBackColor = "rgb(68,80,90)"
  ,textboxBorderColor = "rgb(76,90,103)"
  ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "rgb(80,90,100)"
  ,textboxBorderColorSelect = "rgb(255,255,255)"

  ### tables
  ,tableBackColor = "rgb(52,62,72)"
  ,tableBorderColor = "rgb(70,80,90)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1

)


style.boxtitle <- "style='font-size: 1.35em; color: #FFF;'"

joeBox <- function(metric="70%", text="Some Text", color="olive", width=3) {
  box(
      width=width,
      background=color,
      align="center",
      HTML(paste0("<span style='font-size: 2.5em; font-weight: bold; color: white;'>",metric,"</span>
                  <p style='font-size: 1.5em;'>",text,"</p>"))
    )
}

joeBox2 <- function(colors=c("#4CAF50","#4CAF50","#4CAF50","#4CAF50")) {

paste0("
<h1>Howdy</h1>
<table class='darkTable' style='border: 1px solid black;'>
  <tbody>
    <tr>
      <td width='50%' style='background-color: ",colors[1],"; border: 8px solid rgb(52,62,72);'>
        <span style='font-size: 1.5em;'>Deaths</span><br/>
        <span style='font-size: 3em;'>200</span><br/>
        <span style='font-style: italic;'>Confirmed COVID-19 Deaths To Date</span>
      </td>
      <td width='50%' style='background-color: ",colors[2],"; border: 8px solid rgb(52,62,72);'>
        <span style='font-size: 1.5em;'>Deaths</span><br/>
        <span style='font-size: 3em;'>200</span><br/>
        <span style='font-style: italic;'>Confirmed COVID-19 Deaths To Date</span>
      </td>
    </tr>
    <tr>
      <td width='50%' style='background-color: ",colors[3],"; border: 8px solid rgb(52,62,72);'>
        <span style='font-size: 1.5em;'>Deaths</span><br/>
        <span style='font-size: 3em;'>200</span><br/>
        <span style='font-style: italic;'>Confirmed COVID-19 Deaths To Date</span>
      </td>
      <td width='50%' style='background-color: ",colors[4],"; border: 8px solid rgb(52,62,72);'>
        <span style='font-size: 1.5em;'>Deaths</span><br/>
        <span style='font-size: 3em;'>200</span><br/>
        <span style='font-style: italic;'>Confirmed COVID-19 Deaths To Date</span>
      </td>
    </tr>
  </tbody>
</table>
")

}


width.main <- 10
width.side <- 2

dashboardPage(
  dashboardHeader(title = "COVID-19 Explorer"),
  # Sidebar content
  dashboardSidebar(
    HTML('<br/><br/>'),
    sidebarMenu(
      menuItem("Texas", tabName = "texas", icon = icon("dna")),
      menuItem("All Data", tabName = "covid", icon = icon("dna"))
    )
  )
  ,
  # dashboardSidebar(disable=TRUE),
  dashboardBody(
    # shinyDashboardThemes(
    #   theme = "theme_grey_dark"
    # ),
    theme_grey_dark,
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      tabItem(tabName = "texas",
        fluidPage(        
          fluidRow(
            column(width.main, 
                   box(title=HTML("<span class='box-title-l1'>COVID-19 Data Explorer</span>"),
                       footer=HTML("WebApp Source Code: <i><a href='https://github.com/joetidwell/COVID' target='_blank'>https://github.com/joetidwell/COVID</a></i>"),
                       HTML("<p>This application visualizes various COVID-19 data, particulary for San Antonio, Texas. 
                             I created it because most COVID data dashboards I've seen focus on (what I think are) 
                             unhelpful metrics to understand how the pandemic is progressing. 
                             I have no political agenda behind this site other than providing people access to useful data.</p>
                             <p>Source code and data for this site can be found at <a href='https://github.com/joetidwell/COVID'>https://github.com/joetidwell/COVID</a>.</p>"), width=12)
            )
          ),
          fluidRow(
            column(5,
              box(width=12,
                title=HTML("Texas County COVID-19 Deaths per 100,000 Residents"),
                footer=HTML("Footer"),
                leafletOutput("leafTX") %>% withSpinner(color="#0dc5c1")
              )
            ),
            column(5,
              # box(width=12,
              #   title=HTML("Percent Change Since Peak Week"),
              #   footer=HTML("Footer"),
              #     joeBox(metric=textOutput("TXTodayDeaths"), text="New Deaths", width=3),
              #     joeBox(color="blue"),
              #     joeBox(color="purple"),
              #     joeBox(color="orange")
              # ),
              box(width=12,
                title=HTML("Percent Change Since Previous Week"),
                footer=HTML("Footer"),
                HTML(joeBox2())
              )
            )            
          ),
          # fluidRow(
          #   column(width.main, 
          #     box(title=uiOutput("titleTXDaily"),
          #         joeBox(metric=textOutput("TXTodayDeaths"), text="New Deaths", width=3),
          #         joeBox(color="blue"),
          #         joeBox(color="purple"),
          #         joeBox(color="orange"),
          #         width=12
          #     )
          #  ),
          # ),
          fluidRow(
            column(10, 
                   box(title=HTML("<span class='box-title-l1'>San Antonio Hospital Resources</span><p class='box-title-l2'>Daily Percentages w/ 7-day Moving Average</p>"),
                       footer=HTML("Data Source: <i><a href='https://cosacovid-cosagis.hub.arcgis.com/datasets/covid-19-dashboard-data/' target='_blank'>https://cosacovid-cosagis.hub.arcgis.com/datasets/covid-19-dashboard-data/</a></i>"),
                       plotlyOutput("plotCovidVentsSATX", height = 300), width=12)
                   ),
          ),          fluidRow(
            column(10, 
                   box(title=HTML("<span class='box-title-l1'>San Antonio Active COVID-19 Patients</span><p class='box-title-l2'>Daily Counts w/ 7-day Moving Average</p>"),
                       footer=HTML("Data Source: <i><a href='https://cosacovid-cosagis.hub.arcgis.com/datasets/covid-19-dashboard-data/' target='_blank'>https://cosacovid-cosagis.hub.arcgis.com/datasets/covid-19-dashboard-data/</a></i>"),
                       plotlyOutput("plotCovidSATX", height = 300), width=12)
                   ),
          ),
          fluidRow(
            column(10, 
                   box(title=HTML("<span class='box-title-l1'>San Antonio COVID-19 Patient Deaths</span><p class='box-title-l2'>Daily Counts w/ 7-day Moving Average</p>"),
                       footer=HTML("Data Source: <i><a href='https://cosacovid-cosagis.hub.arcgis.com/datasets/covid-19-dashboard-data/' target='_blank'>https://cosacovid-cosagis.hub.arcgis.com/datasets/covid-19-dashboard-data/</a></i>"),
                       plotlyOutput("plotCovidDeathsSATX", height = 300), width=12)
                   ),
          )
        )
      ),
      tabItem(tabName = "covid",
        fluidPage(        
          fluidRow(
            column(width.main, 
                   box(title=HTML("<span class='box-title-l1'>COVID-19 Data Explorer</span>"),
                       footer=HTML("WebApp Source Code: <i><a href='https://github.com/joetidwell/COVID' target='_blank'>https://github.com/joetidwell/COVID</a></i>"),
                       HTML("<p>This application visualizes various COVID-19 data, particulary for San Antonio, Texas. 
                             I created it because most COVID data dashboards I've seen focus on (what I think are) 
                             unhelpful metrics to understand how the pandemic is progressing. 
                             I have no political agenda behind this site other than providing people access to useful data.</p>
                             <p>Source code and data for this site can be found at <a href='https://github.com/joetidwell/COVID'>https://github.com/joetidwell/COVID</a>.</p>"), width=12)
                   )
          ),
          fluidRow(
            column(width.main, 
                   box(title=HTML("<span class='box-title-l1'>San Antonio Active COVID-19 Patients</span><p class='box-title-l2'>Daily Counts w/ 7-day Moving Average</p>"),
                       footer=HTML("Data Source: <i><a href='https://cosacovid-cosagis.hub.arcgis.com/datasets/covid-19-dashboard-data/' target='_blank'>https://cosacovid-cosagis.hub.arcgis.com/datasets/covid-19-dashboard-data/</a></i>"),
                       plotlyOutput("plotCovidSA", height = 300), width=12)
                   ),
          ),
          fluidRow(
            column(width.main, 
                   box(title=HTML("<span class='box-title-l1'>San Antonio Hospital Resources</span><p class='box-title-l2'>Daily Percentages w/ 7-day Moving Average</p>"),
                       footer=HTML("Data Source: <i><a href='https://cosacovid-cosagis.hub.arcgis.com/datasets/covid-19-dashboard-data/' target='_blank'>https://cosacovid-cosagis.hub.arcgis.com/datasets/covid-19-dashboard-data/</a></i>"),
                       plotlyOutput("plotCovidVentsSA", height = 300), width=12)
                   ),
          ),
          fluidRow(
            column(width.main, 
              box(title=HTML("<span class='box-title-l1'>Daily U.S. State COVID-19 Deaths per 1,000,000 Residents</span><p class='box-title-l2'>Daily Deaths w/ 7-day Moving Average</p>"),
                  footer=HTML("Data Sources: <i>
                    <ul>
                      <li><a href='https://covidtracking.com/api/v1/states/daily.csv' target='_blank'>https://covidtracking.com/api/v1/states/daily.csv</a></li>
                      <li><a href='https://www.census.gov/data/tables/time-series/demo/popest/2010s-national-total.html' target='_blank'>https://www.census.gov/data/tables/time-series/demo/popest/2010s-national-total.html</a></li>
                    </ul>
                      </i>"),
                  plotlyOutput("plotCovidStates", height = 300), width=12)),
            column(width.side, 
              selectizeInput(
               'selectState', 'Select States to Display', 
                choices = state.abb,
                selected = c("TX","NY","MD"),
                multiple = TRUE, 
                # options = list(maxItems = 3)
              )
            )
          ),
          fluidRow(
            column(width.main, 
              box(title=HTML("<span class='box-title-l1'>Daily Country COVID-19 Deaths per 1,000,000 Residents</span><p class='box-title-l2'>Daily Deaths w/ 7-day Moving Average</p>"),
                  footer=HTML("Data Source: <i><a href='http://opendata.ecdc.europa.eu/covid19/casedistribution/csv/' target='_blank'>http://opendata.ecdc.europa.eu/covid19/casedistribution/csv/</a></li></i>"),
                  plotlyOutput("plotCovidCountries", height = 300), width=12)),
            column(width.side, 
              selectizeInput(
               'selectCountry', 'Select Countries to Display', 
                choices = countries,
                selected = c("United_States_of_America","United_Kingdom","Sweden","Germany"),
                multiple = TRUE, 
                # options = list(maxItems = 3)
              )
            )
          )
        )
      )
    )
  )
)



