#Burgh's iView
#Fire Map
##Author: Geoffrey Arnold
##Created for: Pittsburgh Bureau of Fire
##Development: 7/25/2016
##Production: NA

getwd()
setwd("/Users/mmadaio/R Projects/Metro21/burghs_eye_view")

#install.packages("shinythemes")
#install.packages("lubridate")
#install.packages("readr")
#install.packages("leaflet")
#install.packages("maptools")
#install.packages('rgeos', type="source")
#install.packages('rgdal') ## Manual download and install from https://cran.r-project.org/web/packages/rgdal/index.html

# Base Packages
library(shiny)
library(shinythemes)
library(zoo)
library(lubridate)
library(plyr)
# "Dogfooding" Packages
library(httr)
library(jsonlite)
library(readr)
library(curl)
# Mapping Packages
library(leaflet)
library(maptools)
library(htmltools)
library(sp)
library(rgdal)


# Load Boundary Files
#Zones
load.zones <- readShapeSpatial("Pittsburgh_Fire_Zones/Fire_Zones.shp")
proj4string(load.zones)=CRS("+proj=lcc +lat_1=40.96666666666667 +lat_2=39.93333333333333 +lat_0=39.33333333333334 +lon_0=-77.75 +x_0=600000 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs")
load.zones <- spTransform(load.zones,CRS("+init=epsg:4326"))

#Sectors
load.districts <- readShapeSpatial("Pittsburgh_Fire_Districts/Fire_Districts.shp")
proj4string(load.districts)=CRS("+proj=lcc +lat_1=40.96666666666667 +lat_2=39.93333333333333 +lat_0=39.33333333333334 +lon_0=-77.75 +x_0=600000 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs")
load.districts <- spTransform(load.districts,CRS("+init=epsg:4326"))

# City Facilities
load.facilities <- read.csv("City_Facilities.csv")

# Clean Usage
load.facilities <- transform(load.facilities, usage = as.factor(mapvalues(prime_usag, c("ACTIVITY", "CABIN", "COMMUNITY", "CONCESSION", "DUGOUT", "FIREHOUSE" , "MEDIC STATION", "OFFICE", "POLICE", "POOL", "POOL CLOSED", "POOL/REC", "REC", "RECYCLING", "RESTROOMS", "SALT DOME", "SENIOR", "SERVICE", "SHELTER", "STORAGE", "TRAINING", "UTILITY", "VACANT"),
                                                                         c("Activity", "Cabin", "Community", "Concession", "Dugout", "Firehouse", "Medic Station", "Office", "Police", "Pool", "Pool - Closed", "Pool/Recreation", "Recreation", "Recycling", "Restrooms", "Salt Dome", "Senior", "Service", "Shelter", "Storage", "Training", "Utility", "Vacant"))))

load.facilities <- transform(load.facilities, icon = as.factor(mapvalues(prime_usag, c("ACTIVITY", "CABIN", "COMMUNITY", "CONCESSION", "DUGOUT", "FIREHOUSE" , "MEDIC STATION", "OFFICE", "POLICE", "POOL", "POOL CLOSED", "POOL/REC", "REC", "RECYCLING", "RESTROOMS", "SALT DOME", "SENIOR", "SERVICE", "SHELTER", "STORAGE", "TRAINING", "UTILITY", "VACANT"),
                                                                         c("ACTIVITY", "CABIN", "COMMUNITY", "CONCESSION", "DUGOUT", "FIREHOUSE" , "MEDIC_STATION", "OFFICE", "POLICE", "POOL", "POOL_CLOSED", "POOL/REC", "REC", "RECYCLING", "RESTROOMS", "SALT_DOME", "SENIOR", "SERVICE", "SHELTER", "STORAGE", "TRAINING", "UTILITY", "VACANT"))))

# Create Icons
icons_facilities <- iconList(
  ACTIVITY = makeIcon("./icons/facilities/activity.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  CABIN = makeIcon("./icons/facilities/cabin.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  COMMUNITY = makeIcon("./icons/facilities/community.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  CONCESSION = makeIcon("./icons/facilities/concession.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  DUGOUT = makeIcon("./icons/facilities/dugout.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  FIREHOUSE = makeIcon("./icons/facilities/firehouse.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  MEDIC_STATION = makeIcon("./icons/facilities/medic_station.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  OFFICE = makeIcon("./icons/facilities/office.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  POLICE = makeIcon("./icons/facilities/police.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  POOL = makeIcon("./icons/facilities/pool.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  POOL_CLOSED = makeIcon("./icons/facilities/pool_closed.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  POOL_REC = makeIcon("./icons/facilities/pool_recreation.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  REC = makeIcon("./icons/facilities/recreation.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  RECYCLING = makeIcon("./icons/facilities/recycling.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  RESTROOMS = makeIcon("./icons/facilities/restrooms.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  SALT_DOME= makeIcon("./icons/facilities/salt_dome.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  SENIOR = makeIcon("./icons/facilities/senior.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  SERVICE = makeIcon("./icons/facilities/service.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  SHELTER = makeIcon("./icons/facilities/shelter.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  STORAGE = makeIcon("./icons/facilities/storage.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  TRAINING = makeIcon("./icons/facilities/training.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  UTILITY = makeIcon("./icons/facilities/utility.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  VACANT = makeIcon("./icons/facilities/vacant.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48)
)

# Load Condmened Buildings CSV
load.condemned <- read.csv("./PLI_Condemned.csv")
# Read as Date
load.condemned$Date <- as.Date(load.condemned$Date)
load.condemned$FullAddress <- ifelse(is.na(load.condemned$Direction), paste(load.condemned$StreetNum, load.condemned$Address), paste(load.condemned$StretNum, load.condemned$Direction, load.condemned$Address))

load.condemned <-  transform(load.condemned, icon = as.factor(mapvalues(Status, c('Condemned', 'Fire Demo', 'Imminent Danger'), c("condemned", "fire_demo",  "imminent_danger"))))

# Create Icons
icons_condemned <- iconList(
  condemned = makeIcon("./icons/PLI/condemned.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  fire_demo = makeIcon("./icons/PLI/fire_demo.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  imminent_danger = makeIcon("./icons/PLI/imminent_danger.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48)
)

# Load PLI Violations Data
load.violations <- read.csv("./PLI_Violations_Report.csv")
# Read Date
load.violations$INSPECTION_DATE <- as.character(load.violations$INSPECTION_DATE)
# Get Offenses Columns
violationsCol <- as.numeric(ncol(load.violations))
violations1 <- which(colnames(load.violations)=="V1")
# Date Clean
load.violations$date <- as.Date(load.violations$INSPECTION_DATE, format = "%M/%d/%Y")
# Create Court Date/Docket Number
load.violations$DocketText <- ifelse(load.violations$NEXT_ACTION == "SENT TO COURT", paste("<br><b>Docket #(s):</b>", load.violations$DocketNumber,"<br><b>Court Date(s):</b>", load.violations$CourtDate), "")
# Create URL
load.violations$url <-  paste0('<a href="http://www2.county.allegheny.pa.us/RealEstate/GeneralInfo.aspx?ParcelID=',load.violations$PIN, '" target="_blank">', load.violations$PIN, '</a>')

# Create Icons
icons_violations <- iconList(
  violations_abated = makeIcon("./icons/PLI/violations_abated.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  violations_found = makeIcon("./icons/PLI/violations_found.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  violations_void = makeIcon("./icons/PLI/violations_void.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48)
)

# Load Firehouse Datapull
load.fh <- read.csv("./PBF_FireCalls_2013-2016.csv", fileEncoding="latin1")

# Reformat Date
load.fh$date <- as.Date(load.fh$alm_dttm)
# Unmappables
load.unmapped <- subset(load.fh, XCOORD == 0 & YCOORD == 0)
load.unmapped2 <- subset(load.fh, is.na(XCOORD) | is.na(YCOORD) | is.null(XCOORD) | is.null(YCOORD))
load.unmapped <- unique(rbind(load.unmapped, load.unmapped2))

# Clean
load.fh <- load.fh[!(load.fh$CALL_NO %in% load.unmapped$CALL_NO),]
#print(load.fh)
# Create Icons
icons_fh <- iconList(
  ems = makeIcon("./icons/fire/ems.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  explosion = makeIcon("./icons/fire/explosion.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  false_alarm = makeIcon("./icons/fire/false_alarm.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  fire = makeIcon("./icons/fire/fire.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  fire_brush = makeIcon("./icons/fire/fire_brush.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  fire_building = makeIcon("./icons/fire/fire_building.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  fire_cooking = makeIcon("./icons/fire/fire_cooking.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  fire_trash = makeIcon("./icons/fire/fire_trash.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  fire_vehicle = makeIcon("./icons/fire/fire_vehicle.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  good_intent = makeIcon("./icons/fire/good_intent.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  hazardous_condition = makeIcon("./icons/fire/hazardous_condition.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  other = makeIcon("./icons/fire/other.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  service_call = makeIcon("./icons/fire/service_call.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  severe_weather = makeIcon("./icons/fire/severe_weather.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  special_type = makeIcon("./icons/fire/special_type.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48)
)

###################

# Load Fire Risk Data

load.risk <- read.csv("./pitt_risk_inspection_merged.csv")
icons_risk <- iconList(
  risk_icon = makeIcon("./icons/risk/risk_icon.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48)
)

###################

# Load 311 Data
load311 <- read.csv("./311_wide_short.csv", encoding="latin1")

# Make Selection
requests311 <- c("Fire Department", "Fire Lane", "Fire Prevention", "Hydrant - Fire Admin", "Smoke detectors")

# Clean Date
load311$CREATED_ON <- as.Date(load311$CREATED_ON)
#load311$closed.date <- as.Date(load311$closed.date)

#Remove "NULL Island"
load311 <- subset(load311, X != 0 | Y != 0)
load311 <- subset(load311, !is.na(X) | !is.na(Y))

# Create Icons
icons_311 <- iconList(
  abandoned_vehicle = makeIcon("./icons/311/abandoned_vehicle.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  drug_enforcement = makeIcon("./icons/311/drug_enforcement.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  gang_activity = makeIcon("./icons/311/gang_activity.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  graffiti = makeIcon("./icons/311/graffiti.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 18, popupAnchorY = -48),
  noise = makeIcon("./icons/311/noise.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  other311 = makeIcon("./icons/311/other311.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  panhandling = makeIcon("./icons/311/panhandling.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  patrol = makeIcon("./icons/311/patrol.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  fire_dept = makeIcon("./icons/311/fire_dept.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  fire_lane = makeIcon("./icons/311/fire_truck.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  fire_prevention = makeIcon("./icons/311/fire_ex.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  hydrant = makeIcon("./icons/311/fire_hydrant.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  smoke_detectors = makeIcon("./icons/311/smoke_detector.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48)
)

ui <- navbarPage(windowTitle = "Burgh's Eye View", 
                 collapsible = TRUE, 
                 theme = shinytheme("flatly"), 
                 title = HTML('<img src="fire.png" alt="Burghs Eye View" height="85%">'),
                 tabPanel('Map',
                          #Run script to determine if user is loading from a mobile device
                          #Set favicon
                          tags$head(tags$link(rel = "icon", type = "image/png", href="favicon.png")),
                          #Remove unwanted padding and margins
                          tags$style(type="text/css", ".container-fluid {padding:0;}"),
                          tags$style(type="text/css", ".navbar-static-top {margin-bottom:0;}"),
                          tags$style(type="text/css", ".navbar-brand {height:60px; padding:0;}"),
                          #Edit top bar
                          tags$style(type= "text/css", ".form-group {
                                     margin-bottom: 0px;
                                     }"),
                          #Fix Navbar positioning
                          tags$style(type="text/css", ".navbar {border-right-width: 20px;
                                     border-left-width: 65px;}"),
                          #Generate search & layer panel & Map (checks for mobile devices)
                          tags$style(type = "text/css", "#map {height: calc(100vh - 60px) !important;}"),
                          leafletOutput("map"),
                          absolutePanel(
                            #Input panel for Desktops (alpha'd)
                            top = 70, left = 50, width = '300px',
                            wellPanel(id = "tPanel", style = "overflow-y:scroll; max-height: calc(100vh - 85px) !important; margin-bottom: 0px;",
                              dateRangeInput("dates",
                                             label = NULL,
                                             start = "2013-01-02", ## Sys.Date()-14, ## TODO Change back to current date - 2 weeks
                                             end = Sys.Date(),
                                             min = min(load.fh$date, na.rm = TRUE),
                                             max = max(load.fh$date, na.rm = TRUE),
                                             startview = "day"),
                              HTML('<br>'),
                              textInput("search_street", 
                                        value = "",
                                        label = NULL, 
                                        placeholder = "Address Search"),
                              HTML('<br>'),
                              uiOutput("firez_UI"),
                              uiOutput("hood_UI"),
                              HTML('<font color="#BA1924">'),
                              checkboxInput("toggleFire",
                                            label = "Fire Incidents",
                                            value= TRUE),
                              HTML('</font>'),
                              selectInput("unit_select",
                                          label = NULL,
                                          c(`PBF Unit`='', levels(load.fh$PRIMARY_UNIT)),
                                          multiple = TRUE,
                                          selectize = TRUE,
                                          selected = ""),
                              selectInput("type_select",
                                          label = NULL,
                                          c(`Incident Type`='', levels(load.fh$Type)),
                                          multiple = TRUE,
                                          selectize = TRUE,
                                          selected = ""),
                              selectInput("desc_select",
                                          label = NULL,
                                          c(`Code Description`='', levels(load.fh$full.code)),
                                          multiple = TRUE,
                                          selectize = TRUE,
                                          selected = ""),
                              uiOutput("response_UI"),
                              HTML('<font color="#BA1924">'),
                              checkboxInput("toggleRisk",
                                            label = "Non-Residential Property Risk",
                                            value= FALSE),
                              HTML('</font>'),
                              selectInput("statedesc_select",
                                          label = NULL,
                                          c(`State Description`='', levels(load.risk$STATEDESC)),
                                          multiple = TRUE,
                                          selectize = TRUE,
                                          selected = ""),
                              selectInput("usedesc_select",
                                          label = NULL,
                                          c(`Usage Description`='', levels(load.risk$USEDESC)),
                                          multiple = TRUE,
                                          selectize = TRUE,
                                          selected = ""),
                              uiOutput("risk_UI"),
                              radioButtons("assist_select",
                                           "Assist Types:",
                                           choices = c("All", "Lift Assist/Refusal", "CPR/Cardiac Arrest"),
                                           selected = "All"),
                              selectInput("med_select",
                                          label = NULL,
                                          c(`Medical Assist`='', c("Narcan", "Albuterol", "Glucose", "Nitro", "Epi")),
                                          multiple = TRUE,
                                          selectize = TRUE,
                                          selected = ""),
                              HTML('<font color="#662D91">'),
                              checkboxInput("toggleCondemned",
                                            label = "Condemned Buildings",
                                            value = FALSE),
                              HTML('</font>'),
                              selectInput("condemned.status",
                                          label = NULL,
                                          c(`Condemned Status`='', levels(load.condemned$Status)),
                                          multiple = TRUE,
                                          selectize = TRUE,
                                          selected = ""),
                              HTML('<font color="#0B9444">'),
                              checkboxInput("toggleViolations",
                                            label = "Code Violations", 
                                            value = FALSE),
                              HTML('</font>'),
                              uiOutput("violation_UI"),
                              checkboxGroupInput("result_select",
                                                 "Inspection Result(s):",
                                                 choices = levels(load.violations$INSPECTION_RESULT),
                                                 selected = ""),
                              checkboxGroupInput("nextAct_select",
                                                 "Next Action(s):",
                                                 choices = levels(load.violations$NEXT_ACTION),
                                                 selected = ""),
                              HTML('<font color="#F47B25">'),
                              checkboxInput("toggle311",
                                            label = "311 Requests",
                                            value = FALSE),
                              HTML('</font>'),
                              selectInput("req.type",
                                          label = NULL,
                                          c(`Request Type`='', levels(load311$REQUEST_TYPE)),
                                          multiple = TRUE,
                                          selectize = TRUE,
                                          selected = requests311),
                              HTML('<font color="#474545">'),
                              checkboxInput("toggleFacilities",
                                            label = "City Facilities",
                                            value = TRUE),
                              HTML('</font>'),
                              selectInput("usage_select",
                                          tagList(shiny::icon("building"), "Facility Type(s):"),
                                          choices =  levels(load.facilities$usage),
                                          multiple = TRUE,
                                          selectize = TRUE,
                                          selected = "Firehouse"),
                              selectInput("basemap_select",
                                          label = "Basemap",
                                          choices = c(`OSM Mapnik` = "OpenStreetMap.Mapnik", `OSM France` = "OpenStreetMap.France", `OSM Humanitarian` = "OpenStreetMap.HOT", `Esri Satellite` = "Esri.WorldImagery", `Stamen Toner` = "Stamen.Toner", Esri = "Esri.WorldStreetMap", Pioneer = "Thunderforest.Pioneer"))
                            ), style = "opacity: 0.88"
                          )
    ),
    tabPanel('Report Tables',
      inputPanel(
        radioButtons("report_select", 
                    tagList(shiny::icon("map-marker"), "Select Layer:"),
                    choices = c("Unmapped Incidents", "Fire Incidents", "311 Requests", "Condemned Buildings", "Code Violations"),
                    selected= "Unmapped Incidents"),
        downloadButton("downloadData", "Export Layer Report")
      ),
       dataTableOutput("report.table")
    ),
  tabPanel('Disclaimer & Acknowledgements',
     HTML('<h2>Disclaimer</h2>
          <ul>
             <li>This map is designed for tactical analysis and does not represent the official statistics of the Pittsburgh Bureau of Fire.
             <li>Firehouse Data is available since 2014-01-01.
             <li>Information is pulled hourly at 23 minutes past the hour.
             <li>Addresses are geocoded based off the Allegheny County CAD/911 Call records.
             <li>Unmappable locations are provided in the table above.
             <li>Neighborhoods are estimated based off of Census Tract and may not be entirely accurate.
           </ul>
          <br>
          <h2 align="center">Acknowledgements</h2>
          <p align="center"><i> This Map was generated by the City of Pittsburgh Department of Innovation & Performance in parternship with the Pittsburgh Bureau of Fire.</p></i><br>')
  ),
  tabPanel("Recent Updates",
     HTML('<h3>4/25/17</h3>
            <ul>
              <li>Basemap change added to input bar.
            </ul>
          <h3>2/21/17</h3>
            <ul>
              <li> Added District and Zone outlines to map.
              <li> Refined unmappable Incidents code, should now draw Fire Incidents all the time.
            </ul>
          <h3>11/2/16</h3>
            <ul>
              <li>Updated User Interface to match the Public facing version of <a href="analytics.pittsburghpa.gov/BurghsEyeView">Burgh&#39s Eye View</a>.
            </ul>
          <h3>8/23/16</h3>
            <ul>
            <li>Address search filter has been added.
          </ul>
          <h3>8/9/16</h3>
            <ul>
              <li>Fire Zones filter has been added.
            </ul>')
   )
)

server <- function(input, output) {
  output$unmapped.text <- renderText({
    unmapped <- unmappedInput()
    fh <- fhInput()
    
    paste(nrow(unmapped), " Unmappable Incidents of ", nrow(fh), " Total Incidents ", "(",round(nrow(unmapped)/(nrow(fh)+nrow(unmapped)) * 100,2), "%)", sep="")
  })
  zonesInput <- reactive({
    zones <- load.zones
    #Zone Filter
    if (length(input$firez_select) > 0){
      zones <- zones[zones$DIST_ZONE %in% input$firez_select,]
    }
    zones
  })
  fhInput <- reactive({
    fh <- load.fh
   
    
    #Apply Date Filter
    fh <- subset(fh, date  >= input$dates[1] & date <= input$dates[2])
    
    #Zone Filter
    if (length(input$firez_select) > 0){
      fh <- fh[fh$REP_DIST %in% input$firez_select,]
    }
    #Unit Filter
    if (length(input$unit_select) > 0){
      fh <- fh[fh$PRIMARY_UNIT %in% input$unit_select,]
    }
    
    #Apply Neighborhood filter
    if (length(input$hood_select) > 0) {
      fh <- fh[fh$NEIGHBORHOOD %in% input$hood_select,]
    }
    
    #Type Filter
    if (length(input$type_select) > 0){
      fh <- fh[fh$Type %in% input$type_select,]
    }
    
    #Description Filter
    if (length(input$desc_select) > 0){
      fh <- fh[fh$full.code %in% input$desc_select,]
    }
    
    
    #New Med Filter
    if ("Narcan" %in% input$med_select) {
      narc <- subset(fh, pbf_narcan == TRUE)
    } else {
      narc <- fh[0,]
    }
    if ("Albuterol" %in% input$med_select) {
      alb <- subset(fh, pbf_albut == TRUE)
    } else {
      alb <- fh[0,]
    }
    if ("Glucose" %in% input$med_select) {
      glu <- subset(fh, meds_glucose == TRUE)
    } else {
      glu <- fh[0,]
    }
    if ("Nitro" %in% input$med_select) {
      nitro <- subset(fh, meds_nitro == TRUE)
    } else {
      nitro <-fh[0,]
    }
    if ("Epi" %in% input$med_select) {
      epi <- subset(fh, meds_epi == TRUE)
    } else {
      epi <- fh[0,]
    }
    
    #Only runs if Med assist types are selected
    if (length(input$med_select) > 0) {
      test <- rbind.fill(narc, alb, glu, nitro, epi)
      fh <- unique(test)
    }
    
    #Assist Select Filter
    if (input$assist_select == "Lift Assist/Refusal"){
      fh <- subset(fh, Lift_Ref == TRUE)
    } else if (input$assist_select == "CPR/Cardiac Arrest"){
      fh <- subset(fh, Card_CPR == TRUE)
    }
    
    fh <- subset(fh, response_time >= input$response_select[1] & response_time <= input$response_select[2])
    
    #Address Filter
    if (!is.null(input$search_street) && input$search_street != "") {
      fh <- fh[grep(input$search_street, fh$LOCATION, ignore.case = TRUE), ]
    }
    
    fh
  })
  
  riskInput <- reactive({
    risk <- load.risk
    
    
    # #Apply Neighborhood filter
    # if (length(input$hood_select) > 0) {
    #   risk <- risk[risk$hood_x %in% input$hood_select,]
    # }
    
    #StateDesc Filter
    if (length(input$statedesc_select) > 0){
      risk <- risk[risk$STATEDESC %in% input$statedesc_select,]
    }
    
    #UseDesc Filter
    if (length(input$usedesc_select) > 0){
      risk <- risk[risk$USEDESC %in% input$usedesc_select,]
    }
   
    
    risk <- subset(risk, RiskScore >= input$risk_select[1] & RiskScore <= input$risk_select[2])
    
    #Address Filter
    if (!is.null(input$search_street) && input$search_street != "") {
      risk <- risk[grep(input$search_street, risk$Address, ignore.case = TRUE), ]
    }
    
    risk
  })
  
  unmappedInput <- reactive({
    unmapped <- load.unmapped
    
    #Apply Date Filter
    unmapped <- subset(unmapped, date  >= input$dates[1] & date <= input$dates[2])
    
    #Zone Filter
    if (length(input$firez_select) > 0){
      unmapped <- unmapped[unmapped$REP_DIST %in% input$firez_select,]
    }
    #Unit Filter
    if (length(input$unit_select) > 0){
      unmapped <- unmapped[unmapped$PRIMARY_UNIT %in% input$unit_select,]
    }
    
    #Description Filter
    if (length(input$desc_select) > 0){
      unmapped <- unmapped[unmapped$full.code %in% input$desc_select,]
    }
    
    #New Med Filter
    if ("Narcan" %in% input$med_select) {
      narc <- subset(unmapped, pbf_narcan == TRUE)
    } else {
      narc <- unmapped[0,]
    }
    if ("Albuterol" %in% input$med_select) {
      alb <- subset(unmapped, pbf_albut == TRUE)
    } else {
      alb <- unmapped[0,]
    }
    if ("Glucose" %in% input$med_select) {
      glu <- subset(unmapped, meds_glucose == TRUE)
    } else {
      glu <- unmapped[0,]
    }
    if ("Nitro" %in% input$med_select) {
      nitro <- subset(unmapped, meds_nitro == TRUE)
    } else {
      nitro <-unmapped[0,]
    }
    if ("Epi" %in% input$med_select) {
      epi <- subset(unmapped, meds_epi == TRUE)
    } else {
      epi <- unmapped[0,]
    }
    
    #Only runs if Med assist types are selected
    if (length(input$med_select) > 0) {
      test <- rbind.fill(narc, alb, glu, nitro, epi)
      unmapped <- unique(test)
    }
    
    #Assit Select Filter
    if (input$assist_select == "Lift Assist/Refusal"){
      unmapped <- subset(unmapped, Lift_Ref == TRUE)
    } else if (input$assist_select == "CPR/Cardiac Arrest"){
      unmapped <- subset(fh, Card_CPR == TRUE)
    }
    
    unmapped <- subset(unmapped, response_time >= input$response_select[1] & response_time <= input$response_select[2])
    
    #Address Filter
    if (!is.null(input$search_street) && input$search_street != "") {
      unmapped <- unmapped[grep(input$search_street, unmapped$LOCATION, ignore.case = TRUE), ]
    }
    
    unmapped
  })
  violationsInput <- reactive({
    violations <- load.violations
    
    #Date Filter
    violations <- subset(violations, date >= input$dates[1] & date <= input$dates[2])
    
    #Date Filter
    violations <- subset(violations, date >= input$dates[1] & date <= input$dates[2])
    
    #Violation Filter
    if (length(input$violation_select) > 0) { 
      for (i in violations1:violationsCol) {
        if (i == violations1) {
          out <- violations[violations[,i] %in% input$violation_select,]
        } else {
          new <- violations[violations[,i] %in% input$violation_select,]
          out <- rbind(out, new)
        }
      }
      violations <- unique(out)
    }
    
    #Next Action Filter
    if (length(input$nextAct_select) > 0){
      violations <- violations[violations$NEXT_ACTION %in% input$nextAct_select,]
    }
    
    #Result Filter
    if (length(input$result_select) > 0){
      violations <- violations[violations$INSPECTION_RESULT %in% input$result_select,]
    }
    
    #Geographic Filters
    if (length(input$hood_select) > 0) {
      violations <- violations[violations$NEIGHBORHOOD %in% input$hood_select,]
    }
    if (length(input$firez_select) > 0){
      violations <- violations[violations$DIST_ZONE %in% input$firez_select,]
    }
    
    #Address Filter
    if (!is.null(input$search_street) && input$search_street != "") {
      violations <- violations[grep(input$search_street, violations$FullAddress, ignore.case = TRUE), ]
    }
    
    violations
  })
  condemnedInput <- reactive({
    condemned <- load.condemned
    
    #Neighborhood Filter
    if (length(input$hood_select) > 0) {
      condemned <- condemned[condemned$HOOD %in% input$hood_select,]
    }
    
    #Condemned Filter
    if (length(input$condemned.status) > 0){
      condemned <- condemned[condemned$Status %in% input$condemned.status,]
    }
    if (length(input$firez_select) > 0){
      condemned <- condemned[condemned$DIST_ZONE %in% input$firez_select,]
    }
    
    #Address Filter
    if (!is.null(input$search_street) && input$search_street != "") {
      condemned <- condemned[grep(input$search_street, condemned$FullAddress, ignore.case = TRUE), ]
    }
    
    condemned
  })
  dat311Input <- reactive({
    dat311 <- load311
    
    #Date/Open-Closed Filter
    #open <- subset(dat311, status == 0 | status == 3)
    open <- subset(dat311, CREATED_ON  >= input$dates[1] & CREATED_ON <= input$dates[2])
    dat311 <- rbind(open)
    
    #311 Filter
    dat311 <- dat311[dat311$REQUEST_TYPE %in% input$req.type,]
    
    #Neighborhood Filter
    if (length(input$hood_select) > 0) {
      dat311 <- dat311[dat311$NEIGHBORHOOD %in% input$hood_select,]
    }
    if (length(input$firez_select) > 0){
      dat311 <- dat311[dat311$FIRE_ZONE %in% input$firez_select,]
    }
    
    #Address Filter
    if (!is.null(input$search_street) && input$search_street != "") {
      dat311 <- dat311[grep(input$search_street, dat311$FullAddress, ignore.case = TRUE), ]
    }
    
    dat311
  })
  facilitiesInput <- reactive({
    facilities <- load.facilities
    
    #Geographic Filters
    if (length(input$hood_select) > 0) {
      facilities <- facilities[facilities$HOOD %in% input$hood_select,]
    }
    if (length(input$firez_select) > 0){
      facilities <- facilities[facilities$DIST_ZONE %in% input$firez_select,]
    }
    #Usage Filter
    if (length(input$usage_select) > 0) {
      facilities <- facilities[facilities$usage %in% input$usage_select,]
    }
    
    facilities
  })
  reportInput <- reactive({
    
    if(input$report_select == "Unmapped Incidents") {
      unmapped <- unmappedInput()
      
      #Select Columns of Interest
      unmapped <- subset(unmapped, select = c(Type, full.code, alm_dttm, arv_dttm, response_time, LOCATION, PRIMARY_UNIT, NEIGHBORHOOD, MAP_PAGE, Med_Assist, Lift_Ref, Card_CPR, CALL_NO))
      
      #Rename Columns
      colnames(unmapped) <- c("Type", "Description", "Alarm", "Arrival", "Response", "Address", "Engine", "Neighborhood", "Census", "Med. Assist", "Lift/Refusal", "CPR", "Call #")
      
      report <- unmapped
      
    } else if (input$report_select == "Fire Incidents"){
      fh <- fhInput()
      unmapped <- unmappedInput()
      
      fh <- rbind(fh, unmapped)
      
      #Select Columns of Interest
      fh <- subset(fh, select = c(Type, full.code, alm_dttm, arv_dttm, response_time, LOCATION, PRIMARY_UNIT, NEIGHBORHOOD, MAP_PAGE, Med_Assist, Lift_Ref, Card_CPR, CALL_NO))
      
      #Rename Columns
      colnames(fh) <- c("Type", "Description", "Alarm", "Arrival", "Response", "Address", "Engine", "Neighborhood", "Census", "Med. Assist", "Lift/Refusal", "CPR", "Call #")
      
      report <- fh
    } else if (input$report_select == "Condemned Buildings"){
      condemned <- condemnedInput()
      
      condemned <- subset(condemned, select = c(Status, Date, CityProp, PartyWall, ZONE, HOOD, FullAddress, MAPBLOCKLOT))
      
      colnames(condemned) <- c("Status", "Date Condemned", "City Prop", "Party Wall", "Police Zone", "Neighborhood", "Address", "Block/Lot #")
      
      report <- condemned
      
    } else if (input$report_select == "311 Requests") {
      dat311 <- dat311Input()
      
      dat311 <- subset(dat311, select = c(REQUEST_TYPE, open.date, closed.date, POLICE_ZONE, NEIGHBORHOOD, open_closed, days.open, FullAddress, url))
      
      colnames(dat311) <- c("Request Type", "Open Date", "Closed Date", "Police Zone", "Neighborhood", "Status", "Days Open", "Address", "QAlert")
      
      report <- dat311
    } else if (input$report_select == "Code Violations"){
      violations <- violationsInput()
      
      violations <- subset(violations, select = c(VIOLATION, INSPECTION_RESULT, date, NEXT_ACTION, NEXT_ACTION_DATE, FullAddress, NEIGHBORHOOD, COUNCIL, ZONE, CASE_NUMBER))
      
      colnames(violations) <- c("Violation(s)", "Result", "Inspection Date", "Next Action", "Next Action Date",  "Address", "Neighborhood", "Council District", "Police Zone", "Case #")
      report <- violations
    } 
    report
  })
  output$report.table <- renderDataTable({
    reportInput()
  }, escape = FALSE)
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$report_select, ".csv", sep="") },
    content = function(file) {
      write.csv(reportInput(), file)
    }
  )
  output$response_UI <- renderUI({
    #Load FireHouse Data
    fh <- load.fh
    
    #Apply Date Filter
    fh <- subset(fh, date  >= input$dates[1] & date <= input$dates[2])
    
    #Unit Filter
    if (length(input$unit_select) > 0){
      fh <- fh[fh$PRIMARY_UNIT %in% input$unit_select,]
    }
    
    #Type Filter
    if (length(input$type_select) > 0){
      fh <- fh[fh$Type %in% input$type_select,]
    }
    
    #Max Days
    max.days <- round_any(max(na.omit(fh$response_time)), 5, ceiling)
    print(max.days)
    
    sliderInput("response_select",
                "Response Time (Min) Range:",
                dragRange = TRUE,
                min = 0, 
                max = max.days, 
                value = c(0, max.days))
  })
  
  output$risk_UI <- renderUI({
    #Load Property Risk Data
    risk <- load.risk
    
    
    #State Desc Filter
    if (length(input$statedesc_select) > 0){
      risk <- risk[risk$STATEDESC %in% input$statedesc_select,]
    }
    
    #Use Desc Filter
    if (length(input$usedesc_select) > 0){
      risk <- risk[risk$USEDESC %in% input$usedesc_select,]
    }
    
    
    sliderInput("risk_select",
                "Risk Range:",
                dragRange = TRUE,
                min = 0, 
                max = 10, 
                value = c(0, 10))
  })
  
  output$firez_UI <- renderUI({
    #Load FireHouse Data
    fh <- load.fh
    
    #Apply Date Filter
    fh <- subset(fh, date  >= input$dates[1] & date <= input$dates[2])
    
    #Type Filter
    if (length(input$type_select) > 0){
      fh <- fh[fh$Type %in% input$type_select,]
    }
    
    #Get just selected Neighborhoods
    fh$REP_DIST <- as.character(fh$REP_DIST)
    fh$REP_DIST <- as.factor(fh$REP_DIST)
    
    #Build UI
    selectInput("firez_select",
                label = NULL,
                c(`Fire Zone`='', levels(fh$REP_DIST)),
                multiple = TRUE,
                selectize = TRUE,
                selected = "")
  })
  output$hood_UI <- renderUI({
    #Load FireHouse Data
    fh <- load.fh
    
    #Apply Date Filter
    fh <- subset(fh, date  >= input$dates[1] & date <= input$dates[2])
    
    #Zone Filter
    if (length(input$firez_select) > 0){
      fh <- fh[fh$REP_DIST %in% input$firez_select,]
    }
    
    #Type Filter
    if (length(input$type_select) > 0){
      fh <- fh[fh$Type %in% input$type_select,]
    }
    
    #Get just selected Neighborhoods
    fh$NEIGHBORHOOD <- as.character(fh$NEIGHBORHOOD)
    fh$NEIGHBORHOOD <- as.factor(fh$NEIGHBORHOOD)
    
    #Build UI
    selectInput("hood_select",
                label = NULL,
                c(`Neighborhood`='', levels(fh$NEIGHBORHOOD)),
                multiple = TRUE,
                selectize = TRUE,
                selected = "")
  })
  output$violation_UI <- renderUI({
    violations <- load.violations
    
    #Date Filter
    violations <- subset(violations, date >= input$dates[1] & date <= input$dates[2])
    
    violations <- violations[,violations1:violationsCol]
    
    for (i in 1:ncol(violations)) {
      #Apply character
      lvls <- as.character(violations[,i])
      
      if (i == 1) {
        #Create list beginning
        violationList <- as.data.frame(lvls)
      } else {
        #Append List
        violationList <- rbind(lvls, violationList)
      }
      #Trim list
      violationList <- unique(violationList)
    } 
    
    #Apply factor
    violationList$lvls <- as.factor(violationList$lvls)
    
    selectInput("violation_select",
                label = NULL,
                c(`Violations`='', levels(violationList$lvls)),
                multiple = TRUE,
                selectize = TRUE,
                selected = "")
  })
  output$map <- renderLeaflet({
    #Load Datasets for Map
    fh <- fhInput()
    condemned <- condemnedInput()
    dat311 <- dat311Input()
    violations <- violationsInput()
    facilities <- facilitiesInput()
    risk <- riskInput()
    
    print(dat311)
    
    #Remove Null Island
    condemned <- subset(condemned, X != 0 | Y != 0)
    
    #Build Map
    map <- leaflet() %>% 
      addProviderTiles(input$basemap_select,
                       options = providerTileOptions(noWrap = TRUE)
      )
    # Zone Boundary
    if (is.null(input$hood_select)){
      if (is.null(input$firez_select)){
        # Sector Boundary
        map <- addPolygons(map, data = load.districts,
                           stroke = TRUE, smoothFactor = 0, weight = 3, color = "#000000", opacity = 1,
                           fill = TRUE, fillColor = "#00FFFFFF", fillOpacity = 0
                           )
      }
      zones <- zonesInput()
      map <- addPolygons(map, data = zones,
                         stroke = TRUE, smoothFactor = 0.5, weight = 0.75, color = "#000000", opacity = 0.75,
                         fill = TRUE, fillColor = "#00FFFFFF", fillOpacity = 0,
                         popup = ~paste("<font color='black'><b>District & Zone:</b> ", htmlEscape(DIST_ZONE))
                         )
    }
    if (input$toggle311 & nrow(dat311) > 0) {
    map <- addMarkers(map, data = dat311,
                       clusterOptions = markerClusterOptions(iconCreateFunction=JS("function (cluster) {    
                                                                                    var childCount = cluster.getChildCount();  
                                                                                   if (childCount < 10) {  
                                                                                   c = 'rgba(252, 236, 214, 1);'
                                                                                   } else if (childCount < 100) {  
                                                                                   c = 'rgba(252, 188, 101, 1);'  
                                                                                   } else { 
                                                                                   c = 'rgba(248, 155, 59, 1);'  
                                                                                   }    
                                                                                   return new L.DivIcon({ html: '<div style=\"background-color:'+c+'\"><span>' + childCount + '</span></div>', className: 'marker-cluster', iconSize: new L.Point(40, 40) });
    }")), ~longitude, ~latitude, icon = ~icons_311[icon],
                       popup = ~(paste("<font color='black'><b>Request:</b>", dat311$REQUEST_TYPE,
                                       "<br><b>Open Date:</b>", dat311$open.date,
                                       "<br><b>Address:</b>", paste(dat311$streetNum, dat311$streetName),
                                       "<br><b>Fire Zone:</b>", dat311$FIRE_ZONE,
                                       "<br><b>Police Zone:</b>", dat311$POLICE_ZONE,
                                       "<br><b>Neighborhood:</b>", dat311$NEIGHBORHOOD,
                                       "<br><b>Status:</b>", dat311$open_closed, 
                                       "<br><b>Days Open:</b>", round(dat311$days.open,2),
                                       "<br><b>Closed Date:</b>", dat311$closed.date, 
                                       "<br><b>Request ID:</b>", dat311$url, "</font>"))
      )
    }
    if (input$toggleCondemned & nrow(condemned) > 0) {
      map <- addMarkers(map, data=condemned,
                        clusterOptions = markerClusterOptions(iconCreateFunction=JS("function (cluster) {    
                                                                                    var childCount = cluster.getChildCount();  
                                                                                    if (childCount < 10) {  
                                                                                    c = 'rgba(226, 212, 232, 1);'
                                                                                    } else if (childCount < 100) {  
                                                                                    c = 'rgba(139, 91, 172, 1);'  
                                                                                    } else { 
                                                                                    c = 'rgba(114, 46, 144, 1);'  
                                                                                    }    
                                                                                    return new L.DivIcon({ html: '<div style=\"background-color:'+c+'\"><span>' + childCount + '</span></div>', className: 'marker-cluster', iconSize: new L.Point(40, 40) });
    }")), ~X, ~Y, icon = ~icons_condemned[icon],
                        popup = ~(paste("<font color='black'><b>Status:</b>", condemned$Status,
                                        "<br><b>Date Condemned:</b>", condemned$Date,
                                        "<br><b>Address:</b>", condemned$FullAddress,
                                        "<br><b>Fire Zone:</b>", condemned$DIST_ZONE,
                                        "<br><b>Police Zone:</b>", condemned$ZONE,
                                        "<br><b>Neighborhood:</b>", condemned$HOOD,
                                        "<br><b>City Owned:</b>", condemned$CityProp,
                                        "<br><b>Party Wall:</b>", condemned$PartyWall,
                                        "<br><b>Block/Lot:</b>", condemned$MAPBLOCKLOT, "</font>"))
      )
    }
    if (input$toggleViolations & nrow(violations) > 0) {
      map <- addMarkers(map, data=violations, 
                        clusterOptions = markerClusterOptions(iconCreateFunction=JS("function (cluster) {    
                                                                                    var childCount = cluster.getChildCount();  
                                                                                    if (childCount < 10) {  
                                                                                    c = 'rgba(115, 201, 158, 1);'
                                                                                    } else if (childCount < 100) {  
                                                                                    c = 'rgba(57, 168, 113, 1);'  
                                                                                    } else { 
                                                                                    c = 'rgba(0, 136, 68, 1);'  
                                                                                    }    
                                                                                    return new L.DivIcon({ html: '<div style=\"background-color:'+c+'\"><span>' + childCount + '</span></div>', className: 'marker-cluster', iconSize: new L.Point(40, 40) });
    }")), ~X, ~Y, icon = ~icons_violations[icon],
                        popup = ~(paste("<font color='black'><b>Violation:</b>", violations$VIOLATION,
                                        "<br><b>Inspection Result:</b>", violations$INSPECTION_RESULT,
                                        "<br><b>Date Inspected:</b>", violations$date,
                                        "<br><b>Next Action:</b>", violations$NEXT_ACTION,
                                        violations$DocketText,
                                        "<br><b>Next Action Date:</b>", violations$NEXT_ACTION_DATE,
                                        "<br><b>Address:</b>", violations$FullAddress,
                                        "<br><b>Ward:</b>", violations$WARD,
                                        "<br><b>Neighborhood:</b>", violations$NEIGHBORHOOD,
                                        "<br><b>DPW Division:</b>", violations$PUBLIC_WORKS_DIVISION,
                                        "<br><b>Police Zone:</b>", violations$ZONE,
                                        "<br><b>Parcel ID:</b>", violations$url,
                                        "<br><b>Case #:</b>", violations$CASE_NUMBER, "</font>"))
      )
    }
    if (input$toggleFire & nrow(fh) > 0) {
      map <- addMarkers(map, data=fh,
                        clusterOptions = markerClusterOptions(iconCreateFunction=JS("function (cluster) {    
                                                                                    var childCount = cluster.getChildCount();  
                                                                                    if (childCount < 10) {  
                                                                                    c = 'rgba(249, 208, 209, 1);'
                                                                                    } else if (childCount < 100) {  
                                                                                    c = 'rgba(230, 83, 92, 1);'  
                                                                                    } else { 
                                                                                    c = 'rgba(212, 30, 39, 1);'  
                                                                                    }    
                                                                                    return new L.DivIcon({ html: '<div style=\"background-color:'+c+'\"><span>' + childCount + '</span></div>', className: 'marker-cluster', iconSize: new L.Point(40, 40) });
    }")), ~XCOORD, ~YCOORD, icon = ~icons_fh[icon],
                        popup = ~(paste("<font color='black'><b>Type:</b>", fh$Type,
                                        "<br><b>Description:</b>", fh$full.code,
                                        "<br><b>Alarm Time:</b>", fh$alm_dttm,
                                        "<br><b>Arrival Time:</b>", fh$arv_dttm,
                                        "<br><b>Response Time:</b>", fh$response_time, "min.",
                                        "<br><b>Address:</b>", fh$LOCATION,
                                        "<br><b>Engine Unit:</b>", fh$PRIMARY_UNIT,
                                        "<br><b>Fire Zone:</b>", fh$REP_DIST,
                                        "<br><b>Neighborhood:</b>", fh$NEIGHBORHOOD,
                                        "<br><b>Census:</b>", fh$MAP_PAGE,
                                        "<br><b>Med. Assist:</b>", fh$Med_Assist,
                                        "<br><b>Lift/Refusal:</b>", fh$Lift_Ref,
                                        "<br><b>Cardiac Arrest/CPR:</b>", fh$Card_CPR,
                                        "<br><b>Call #:</b>", fh$CALL_NO, "</font>"))
      )
    }
      #print(risk)
      print(input$toggleRisk)
      # Risk layer
      if (input$toggleRisk & nrow(risk) > 0) {
        map <- addMarkers(map, data=risk,
                          clusterOptions = markerClusterOptions(iconCreateFunction=JS("function (cluster) {    
                                                                                      var childCount = cluster.getChildCount();  
                                                                                      if (childCount < 10) {  
                                                                                      c = 'rgba(249, 208, 209, 1);'
                                                                                      } else if (childCount < 100) {  
                                                                                      c = 'rgba(230, 83, 92, 1);'  
                                                                                      } else { 
                                                                                      c = 'rgba(212, 30, 39, 1);'  
                                                                                      }    
                                                                                      return new L.DivIcon({ html: '<div style=\"background-color:'+c+'\"><span>' + childCount + '</span></div>', className: 'marker-cluster', iconSize: new L.Point(40, 40) });
      }")), ~x_x, ~y_x, #icon = ~icons_risk[icon],
                        popup = ~(paste("<font color='black'><b>State Type:</b>", risk$STATEDESC,
                                        "<br><b>Usage Type:</b>", risk$USEDESC,
                                        "<br><b>Parcel:</b>", risk$PARCEL,
                                        "<br><b>Address:</b>", risk$Address,
                                        "<br><b>Owner:</b>", risk$PROPERTYOWNER,
                                        "<br><b>Risk Score::</b>", risk$RiskScore,
                                        "<br><b>Neighborhood:</b>", risk$hood_x,
                                        "<br><b>Census:</b>", risk$MAP_PAGE,
                                        "<br><b>Council District:</b>", risk$geo_name_countycouncil_x,
                                        "<br><b>Fire District:</b>", risk$Pgh_FireDistrict_x,
                                        "</font>"))
        )
      }
      
      
      
      
      # Facilities Layer
      if (input$toggleFacilities & nrow(facilities) > 0) {
        map <- addMarkers(map, data=facilities,
                          clusterOptions = markerClusterOptions(iconCreateFunction=JS("function (cluster) {    
                                                                                      var childCount = cluster.getChildCount();  
                                                                                      if (childCount < 10) {  
                                                                                      c = 'rgba(217, 217, 224, 1);'
                                                                                      } else if (childCount < 100) {  
                                                                                      c = 'rgba(171, 171, 182, 1);'  
                                                                                      } else { 
                                                                                      c = 'rgba(150, 150, 163, 1);'  
                                                                                      }    
                                                                                      return new L.DivIcon({ html: '<div style=\"background-color:'+c+'\"><span>' + childCount + '</span></div>', className: 'marker-cluster', iconSize: new L.Point(40, 40) });
      }")), ~CENTROID_X, ~CENTROID_Y, icon = ~icons_facilities[icon],
                        popup = ~(paste("<font color='black'><b>Usage:</b>", facilities$usage,
                                        "<br><b>Name:</b>", facilities$name,
                                        "<br><b>Dept:</b>", facilities$departme_1, 
                                        "<br><b>Classification:</b>", facilities$class,
                                        "<br><b>Neighborhood:</b>", facilities$HOOD,
                                        "<br><b>Council District:</b>", facilities$COUNCIL,
                                        "<br><b>DPW Div:</b>", facilities$DIVISION,
                                        "<br><b>Police Zone:</b>", facilities$ZONE,
                                        "<br><b>Fire Zone:</b>", facilities$DIST_ZONE,
                                        "<br><b>Ward:</b>", facilities$WARD,
                                        "<br><b>Parcel ID:</b>", facilities$pin, "</font>"))
        )
  
  }
  map
  })
}

shinyApp(ui = ui, server = server)