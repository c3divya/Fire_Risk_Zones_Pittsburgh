require(plyr)
require(lubridate)
require(dplyr)

#Load CAD Dataset
system("python -v FireCAD.py")

firecad <- read.csv("FireCAD.csv")
#colnames(firecad)<- c("AGENCY", CALL_NO, PRIMARY_UNIT, XCOORD, YCOORD, CALL_CREATED_DATE, CENSUS)

#Clean Districts
firecad$CURR_DGROUP <- as.character(firecad$CURR_DGROUP)
firecad$CURR_DGROUP <- gsub("CFD", "", firecad$CURR_DGROUP)

#Clean Fire Zone
firecad$REP_DIST <- as.character(firecad$REP_DIST)
firecad$REP_DIST <- gsub("231-1", "1-", firecad$REP_DIST)
firecad$REP_DIST <- gsub("231-2", "2-", firecad$REP_DIST)
firecad$REP_DIST <- gsub("231-3", "3-", firecad$REP_DIST)
firecad$REP_DIST <- gsub("231-4", "4-", firecad$REP_DIST)
firecad$REP_DIST <- as.factor(firecad$REP_DIST)

#Clean CAD Call #'s
firecad$CALL_NO <- gsub("F", "", firecad$CALL_NO)
firecad$CALL_NO <- as.numeric(firecad$CALL_NO)

#Load Firehouse Dataset
fh <- read.csv("FireHouse.csv")
colnames(fh)[1] <- ("CALL_NO")

#Clean Firehouse Call #'s
fh$CALL_NO <- gsub("-1", "0", fh$CALL_NO)
fh$CALL_NO <- as.factor(fh$CALL_NO)

#Flatten Incidents
fh$icon <- "other"
fh$icon <- ifelse(fh$inci_type < 200 & fh$inci_type >= 100, "fire", fh$icon)
fh$icon <- ifelse(fh$inci_type < 300 & fh$inci_type >= 200, "explosion", fh$icon)
fh$icon <- ifelse(fh$inci_type < 400 & fh$inci_type >= 300, "ems", fh$icon)
fh$icon <- ifelse(fh$inci_type < 500 & fh$inci_type >= 400, "hazardous_condition", fh$icon)
fh$icon <- ifelse(fh$inci_type < 600 & fh$inci_type >= 500, "service_call", fh$icon)
fh$icon <- ifelse(fh$inci_type < 700 & fh$inci_type >= 600, "good_intent", fh$icon)
fh$icon <- ifelse(fh$inci_type < 800 & fh$inci_type >= 700, "false_alarm", fh$icon)
fh$icon <- ifelse(fh$inci_type < 900 & fh$inci_type >= 800, "severe_weather", fh$icon)
fh$icon <- ifelse(fh$inci_type < 1000 & fh$inci_type >= 900, "special_type", fh$icon)

fh$icon <- ifelse(fh$inci_type == 5001, "false_alarm", fh$icon)

fh$icon <- as.factor(fh$icon) 

fh <- transform(fh, Type = as.factor(mapvalues(icon, c("ems", "explosion", "false_alarm", "fire", "good_intent", "hazardous_condition", "other", "service_call", "severe_weather", "special_type"),
                                               c( "Rescue/EMS", "Explosion", "False Alarm", "Fire", "Good Intent", "Hazardous Conditions", "Other", "Service Call", "Severe Weather", "Special Type"))))

#Transform Med, Lift and Carddiac Types
fh$pbf_narcan <- ifelse(fh$pbf_narcan == "True", TRUE, FALSE)
fh$meds_glucose <- ifelse(fh$meds_glucose == "True", TRUE, FALSE)
fh$meds_epi <- ifelse(fh$meds_epi == "True", TRUE, FALSE)
fh$meds_nitro <- ifelse(fh$meds_nitro== "True", TRUE, FALSE)
fh$pbf_albut <- ifelse(fh$pbf_albut == "True", TRUE, FALSE)
fh$cpr <- ifelse(fh$cpr == "True", TRUE, FALSE)
fh$car_arr <- ifelse(fh$car_arr == "True", TRUE, FALSE)
fh$aed <- ifelse(fh$aed == "True", TRUE, FALSE)
fh$none <- ifelse(fh$none == "True", TRUE, FALSE)
fh$pbf_lift_ass <- ifelse(fh$pbf_lift_ass == "True", TRUE, FALSE)

#Create Filters
fh$Med_Assist <- ifelse(fh$pbf_albut | fh$pbf_narcan | fh$meds_nitro | fh$meds_epi | fh$meds_glucose, TRUE, FALSE)
fh$Lift_Ref <- ifelse(fh$none | fh$pbf_lift_ass, TRUE, FALSE)
fh$Card_CPR <- ifelse(fh$car_arr | fh$aed | fh$cpr, TRUE, FALSE)

fh <- subset(fh, descript != "NULL")

fire.dat <- merge(fh, firecad, by ="CALL_NO", all.x = TRUE)

#Transform Map_Page to Neighborhood
fire.dat <- transform(fire.dat, NEIGHBORHOOD = as.factor(mapvalues(MAP_PAGE, c('2204',	'2201',	'1803',	'1603',	'1604',	'2023',	'509',	'1916',	'1920',	'1809',	'903',	'809',	'806',	'802',	'804',	'103',	'1806',	'2708',	'2701',	'2703',	'1917',	'3206',	'1919',	'1918',	'2507',	'2901',	'2902',	'2904',	'901',	'902',	'2503',	'2206',	'405',	'406',	'2021',	'2108',	'2814',	'2815',	'305',	'1911',	'2304',	'2805',	'1306',	'1113',	'1115',	'2020',	'2017',	'2808',	'2509',	'807',	'1016',	'1114',	'1017',	'1504',	'201',	'1516',	'1517',	'3101',	'1501',	'1515',	'1106',	'1102',	'1301',	'1302',	'1303',	'1304',	'1207',	'3001',	'1204',	'1208',	'3102',	'1201',	'1203',	'1202',	'603',	'2107',	'2715',	'2704',	'501',	'1014',	'1903',	'1914',	'1807',	'1915',	'4810',	'1607',	'3103',	'507',	'403',	'404',	'2205',	'2609',	'2812',	'5599',	'6699',	'7799',	'3204',	'3207',	'2602',	'2607',	'2615',	'2614',	'1404',	'1406',	'1405',	'605',	'1410',	'2016',	'708',	'705',	'709',	'706',	'703',	'2018',	'2022',	'409',	'1921',	'1702',	'1609',	'1706',	'1608',	'2412',	'2620',	'1402',	'1401',	'1403',	'1413',	'1408',	'1414',	'1606',	'1018',	'1005',	'203',	'2612',	'1411',	'510',	'511',	'2406',	'506',	'1011',	'2019',	'402',	'2811',	'2807'), 
                                                                     c('Allegheny Center',	'Allegheny West',	'Allentown',	'Arlington',	'Arlington Heights',	'Banksville',	'Bedford Dwellings',	'Beechview',	'Beechview',	'Beltzhoover',	'Bloomfield',	'Bloomfield',	'Bloomfield',	'Bloomfield',	'Bloomfield',	'Bluff',	'Bon Air',	'Brighton Heights',	'Brighton Heights',	'Brighton Heights',	'Brookline',	'Brookline',	'Brookline',	'Brookline',	'California-Kirkbride',	'Carrick',	'Carrick',	'Carrick',	'Central Lawrenceville',	'Central Lawrenceville',	'Central North Side',	'Central North Side',	'Central Oakland',	'Central Oakland',	'Chartiers City',	'Chateau',	'Crafton Heights',	'Crafton Heights',	'Crawford-Roberts',	'Duquesne Heights',	'East Allegheny',	'East Carnegie',	'East Hills',	'East Liberty',	'East Liberty',	'Elliott',	'Esplen',	'Fairywood',	'Fineview',	'Friendship',	'Garfield',	'Garfield',	'Garfield',	'Glen Hazel',	'Central Business District',	'Greenfield',	'Greenfield',	'Hays',	'Hazelwood',	'Hazelwood',	'Highland Park',	'Highland Park',	'Homewood North',	'Homewood North',	'Homewood South',	'Homewood South',	'Homewood West',	'Knoxville',	'Larimer',	'Larimer',	'Lincoln Place',	'Lincoln-Lemington-Belmar',	'Lincoln-Lemington-Belmar',	'Lincoln-Lemington-Belmar',	'Lower Lawrenceville',	'Manchester',	'Marshall-Shadeland',	'Marshall-Shadeland',	'Middle Hill',	'Morningside',	'Mount Washington',	'Mount Washington',	'Mount Washington',	'Mount Washington',	'Mount Oliver',	'Mount Oliver',	'New Homestead',	'North Oakland',	'North Oakland',	'North Oakland',	'North Shore',	'Northview Heights',	'Oakwood',	'Outside City',	'Outside County',	'Outside State',	'Overbrook',	'Overbrook',	'Perry North',	'Perry North',	'Perry South',	'Perry South',	'Point Breeze',	'Point Breeze',	'Point Breeze North',	'Polish Hill',	'Regent Square',	'Ridgemont',	'Shadyside',	'Shadyside',	'Shadyside',	'Shadyside',	'Shadyside',	'Sheraden',	'Sheraden',	'South Oakland',	'South Shore',	'South Side Flats',	'South Side Flats',	'South Side Slopes',	'South Side Slopes',	'Spring Garden',	'Spring Hill-City View',	'Squirrel Hill North',	'Squirrel Hill North',	'Squirrel Hill North',	'Squirrel Hill South',	'Squirrel Hill South',	'Squirrel Hill South',	'St. Clair',	'Stanton Heights',	'Stanton Heights',	'Strip District',	'Summer Hill',	'Swisshelm Park',	'Terrace Village',	'Terrace Village',	'Troy Hill',	'Upper Hill',	'Upper Lawrenceville',	'West End',	'West Oakland',	'Westwood',	'Windgap'))))

#Transform Neighborhood to Council District
fire.dat <- transform(fire.dat, COUNCIL = as.factor(mapvalues(NEIGHBORHOOD, c("Allegheny Center", "Allegheny West", "Allentown", "Arlington", "Arlington Heights", "Banksville", "Bedford Dwellings", "Beechview", "Beltzhoover", "Bloomfield", "Bluff", "Bon Air", "Brighton Heights", "Brookline", "California-Kirkbride", "Carrick", "Central Business District", "Central Lawrenceville", "Central North Side", "Central Oakland", "Chartiers City", "Chateau", "Crafton Heights", "Crawford-Roberts", "Duquesne Heights", "East Allegheny", "East Carnegie", "East Hills", "East Liberty", "Elliott", "Esplen", "Fairywood", "Fineview", "Friendship", "Garfield", "Glen Hazel", "Greenfield", "Hays", "Hazelwood", "Highland Park", "Homewood North", "Homewood South", "Homewood West", "Knoxville", "Larimer", "Lincoln Place", "Lincoln-Lemington-Belmar", "Lower Lawrenceville", "Manchester", "Marshall-Shadeland", "Middle Hill", "Morningside", "Mount Washington", "Mount Oliver", "New Homestead", "North Oakland", "North Shore", "Northview Heights", "Oakwood", "Overbrook", "Perry North", "Perry South", "Point Breeze", "Point Breeze North", "Polish Hill", "Regent Square", "Ridgemont", "Shadyside", "Sheraden", "South Oakland", "South Shore", "South Side Flats", "South Side Slopes", "Spring Garden", "Spring Hill-City View", "Squirrel Hill North", "Squirrel Hill South", "St. Clair", "Stanton Heights", "Strip District", "Summer Hill", "Swisshelm Park", "Terrace Village", "Troy Hill", "Upper Hill", "Upper Lawrenceville", "West End", "West Oakland", "Westwood", "Windgap"),
                                                                c("1", "1", "3", "3", "3", "2", "6", "4", "3", "7", "6", "4", "1", "4", "6", "4", "6", "7", "1", "3", "2", "6", "2", "6", "2", "1", "2", "9", "9", "2", "2", "2", "1", "7", "9", "5", "5", "5", "5", "7", "9", "9", "9", "3", "9", "5", "9", "7", "6", "1", "6", "7", "2", "3", "5", "8", "1", "1", "2", "4", "1", "6", "9", "9", "7", "5", "2", "8", "2", "3", "2", "3", "3", "1", "1", "8", "5", "3", "7", "7", "1", "5", "6", "1", "6", "7", "2", "6", "2", "2"))))


fire.dat <- transform(fire.dat, WARD = as.factor(mapvalues(REP_DIST, c("1-4", "2-4", "2-29", "2-29A", "1-3", "2-3", "2-25", "2-1", "2-2", "2-10", "2-28", "2-14", "2-7", "2-28A", "2-11", "2-5", "2-22", "2-23","2-24", "3-2", "2-6", "2-9", "3-22", "3-11", "3-13", "3-1", "3-23", "3-6", "3-3", "3-4", "3-5", "3-7", "3-8", "3-9", "3-10", "3-12", "3-15", "3-18", "3-24", "3-17", "2-26", "2-17", "2-8", "2-18", "3-14", "2-16", "2-21", "2-19", "2-20", "2-15", "2-12", "2-27A", "2-13", "4-13", "4-8", "4-22", "4-24", "4-3", "4-2", "4-5", "4-6", "4-16", "4-18", "4-1", "4-14A", "4-11A", "4-17A", "4-27", "4-28", "4-26", "1-27", "1-17", "4-9","4-10A", "1-8", "1-9", "1-7", "1-6", "1-20", "1-5", "1-24", "1-2", "1-1", "1-21", "1-10", "1-12", "1-22", "1-11", "1-23", "1-13", "1-15", "1-9A", "1-14A", "1-14", "1-16", "1-18", "1-19", "4-12", "4-15", "4-23", "4-21", "4-7", "4-19", "4-20", "4-20A", "4-25"), 
                                                            c("1", "1", "1", "1", "2", "2", "2", "3", "4", "4" ,"4", "4", "4", "4", "4", "5", "5", "5", "5", "6", "6", "7", "7", "7", "7", "8", "8", "9", "9", "10", "10", "10", "11", "11", "12", "12", "12", "12", "12", "13", "14", "14", "14", "14", "14", "14", "14", "14", "14", "15", "15", "15", "15", "16", "16", "16", "16", "17", "17", "18", "18", "18", "18", "19", "19", "19", "19", "19", "19", "19", "20", "20", "20", "20", "21", "21", "22", "22", "23", "23", "24", "24", "24", "25", "25", "26", "26", "26", "26", "26", "26", "27", "27","27", "28", "28", "28", "29", "29", "29", "30", "30", "31", "31", "31", "32"))))

fire.dat$full.code <- paste(fire.dat$inci_type, "-", fire.dat$descript)
fire.dat$alm_dttm <- as.character(fire.dat$alm_dttm)
fire.dat$alm_dttm <- strptime(fire.dat$alm_dttm, format = "%Y-%m-%d %H:%M:%S")
fire.dat$arv_dttm <- as.character(fire.dat$arv_dttm)
fire.dat$arv_dttm <- strptime(fire.dat$arv_dttm, format = "%Y-%m-%d %H:%M:%S")
fire.dat$response_time <- round(time_length(fire.dat$arv_dttm - fire.dat$alm_dttm, unit="minute"), 2)

write.csv(fire.dat, "/opt/shiny-server/samples/sample-apps/PBF/Fire_Map/Fire_Incidents.csv")

fire.dat <- transform(fire.dat, unit = as.factor(mapvalues(PRIMARY_UNIT, c("2314", "231401", "2314011", "2314012", "2314013", "2314014", "231402", "2314020", "2314021", "2314022", "2314023", "2314024", "231403",  "2314031", "2314032", "2314033", "2314034", "231404",  "2314041", "2314042", "2314043", "2314044", "2314110", "2314112", "2314114", "2314508", "231614",  "2316141", "2316142", "23138411", "23138413", "23138415", "231FOAM", "231SPILL", "231MAC1"),
  c("Chief, Bureau of Fire", "Deputy Chief A PLatoon", "A Platoon - Battalion One - BC", "A Platoon - Battalion Two - BC", "A Platoon - Battalion Three - BC", "A Platoon - Battalion Four - BC", "Deputy Chief B Platoon", "HazMat Battalion Chief", "B Platoon - Battalion One - BC", "B Platoon - Battalion Two - BC", "B Platoon - Battalion Three - BC", "B Platoon - Battalion Four - BC", "Deputy Chief C Platoon", "C Platoon - Battalion One - BC", "C Platoon - Battalion Two - BC", "C Platoon - Battalion Three - BC", "C Platoon - Battalion Four - BC", "Deputy Chief D Platoon", "D Platoon - Battalion One - BC", "D Platoon - Battalion Two - BC", "D Platoon - Battalion Three - BC", "D Platoon - Battalion Four - BC", "Arson Division", "Arson Division", "Arson Division", "Special Units - Communications Captain", "Training Division - Battalion Chief", "Training Division - Instructor", "Training Division - Instructor", "Arson Division", "Arson Division", "Arson Division", "HaxMat - Foam", "HazMat - Spill", "Mobile Air Compressor"))))

fire.dat$unit <- as.character(fire.dat$unit)
fire.dat$unit <- gsub("231EN", "Engine ", fire.dat$unit)
fire.dat$unit <- gsub("231TK", "Truck ", fire.dat$unit)
fire.dat$unit <- as.factor(fire.dat$unit)

fire.dat$CALL_TYPE_FINAL <- fire.dat$Type
fire.dat$CALL_TYPE_FINAL_D <- fire.dat$descript
fire.dat$date_time <- fire.dat$alm_dttm
fire.dat$DISPOSITION <- fire.dat$PRIMARY_UNIT
fire.dat$ZONE <- NA
fire.dat$REPORT_NO <- NA
fire.dat$CALL_NO <- paste0("F",fire.dat$CALL_NO)
fire.dat$date <- as.Date(fire.dat$alm_dttm)
fire.dat$propFire <- ifelse(fire.dat$inci_type >= 100 & fire.dat$inci_type < 200, TRUE, FALSE)

prop_fires <- subset(fire.dat, propFire, select = c(CALL_TYPE_FINAL, CALL_TYPE_FINAL_D, date, date_time, LOCATION, ZONE, NEIGHBORHOOD, COUNCIL, DISPOSITION, CALL_NO, REPORT_NO, icon, XCOORD, YCOORD, WARD, REP_DIST))

write.csv(prop_fires, "/opt/shiny-server/samples/sample-apps/PLI/PLI_Map/Fire_Incidents.csv")

# Mayor's Dashboard Page
this_year <- as.Date(format(Sys.Date(), format="%Y-01-01"))
last_year_start <- as.Date(format(Sys.Date()-365, format="%Y-01-01"))
last_year <- Sys.Date() - 365

fire.dat$alm_dttm <- as.POSIXct(fire.dat$alm_dttm)
fire.dat$arv_dttm <- as.POSIXct(fire.dat$arv_dttm)
fire.dat$date_time <- as.POSIXct(fire.dat$date_time)

fire_ytd <- subset(fire.dat, date >= this_year)
fire_ly <- subset(fire.dat, date >= last_year_start & date <= last_year)
fire_ly$Period <- "LY"
fire_ytd$Period <- "YTD"

dash.fire <- rbind(fire_ly, fire_ytd)

table <- dash.fire %>%
  group_by(Period) %>%
  summarise(Calls = length(CALL_NO), Avg_Resposne_Time = mean(response_time, na.rm = TRUE), propertyFires = length(CALL_NO[propFire]))

write.csv(table, "/opt/shiny-server/samples/sample-apps/Mayors Office/CityDashboard/fire.kpis.csv", row.names = FALSE)

# Council BEV & Fire
fire.dat <- subset(fire.dat, date >= Sys.Date()-28)

prep.fh <- subset(fire.dat, select = c(CALL_TYPE_FINAL, CALL_TYPE_FINAL_D, date, date_time, ZONE, NEIGHBORHOOD, COUNCIL, DISPOSITION, CALL_NO, REPORT_NO, icon, XCOORD, YCOORD, REP_DIST, prop_use_code, prop_use_descript))

write.csv(prep.fh, "/opt/shiny-server/samples/sample-apps/Council/Council_Map/Fire_Incidents.csv")
write.csv(prep.fh, "/home/linadmin/FirePred/outputFire/Fire_Incidents.csv")