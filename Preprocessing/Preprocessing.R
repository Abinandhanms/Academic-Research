#NDVI Preprocessing
setwd('D:/MS/Trisemester-3/Data/New_Data/NDVI')
library(MASS)
library(reshape2)
library(reshape)

NDVI_Filenames <- list.files(full.names=TRUE)


NDVI <- lapply(NDVI_Filenames,function(i){
 read.csv(i, header=FALSE)
})

NDVI <- do.call(rbind.data.frame, NDVI)
NDVI <- subset( NDVI, select = -V1 )
NDVI <- subset( NDVI, select = -V2 )
NDVI <- subset( NDVI, select = -V5 )
NDVI <- subset( NDVI, select = -V6 )

NDVI <- melt(NDVI, id = c("V3","V4"))
NDVI <- subset( NDVI, select = -variable )


#Removing unwanted info
NDVI$LL<-gsub("Samp161Line161","",NDVI$LL)
NDVI$Date<-gsub("A","",NDVI$Date)
NDVI<-NDVI[!(NDVI$Value=="F"), ]
#Splitting to readable format for date
NDVI$Year <- substr(NDVI$Date, 1, 4)
NDVI$Day <- substr(NDVI$Date, 5, 7)
NDVI <- NDVI[c("Year","Day","Date","LL","Value")]

NDVI<-NDVI[!(NDVI$Year=="2010"),]
NDVI<-NDVI[!(NDVI$Year=="2013"),]
NDVI<-NDVI[!(NDVI$Year=="2011"),]
NDVI <- subset( NDVI, select = -Date )

NDVI$Latitude[NDVI$LL=="Lat51.78542Lon-122.23159"]<-"51.78542"
NDVI$Latitude[NDVI$LL=="Lat50.721241Lon-121.283544"]<-"50.721241"
NDVI$Latitude[NDVI$LL=="Lat56.27903Lon-120.69588"]<-"56.27903"
NDVI$Latitude[NDVI$LL=="Lat59.63127Lon-133.68331"]<-"59.63127"
NDVI$Latitude[NDVI$LL=="Lat56.6463Lon-121.20407"]<-"56.6463"
NDVI$Latitude[NDVI$LL=="Lat53.16601Lon-121.45265"]<-"53.16601"
NDVI$Latitude[NDVI$LL=="Lat49.539781Lon-115.755509"]<-"49.539781"
NDVI$Latitude[NDVI$LL=="Lat49.26094Lon-123.24713"]<-"49.26094"
NDVI$Latitude[NDVI$LL=="Lat54.02681Lon-123.947807"]<-"54.02681"
NDVI$Latitude[NDVI$LL=="Lat49.12513Lon-116.58533"]<-"49.12513"
NDVI$Latitude[NDVI$LL=="Lat52.156779Lon-123.715281"]<-"52.156779"
NDVI$Latitude[NDVI$LL=="Lat50.14597Lon-121.57292"]<-"50.14597"
NDVI$Latitude[NDVI$LL=="Lat50.209342Lon-126.595777"]<-"50.209342"
NDVI$Latitude[NDVI$LL=="Lat55.75479Lon-120.04333"]<-"55.75479"
NDVI$Latitude[NDVI$LL=="Lat49.161131Lon-121.951489"]<-"49.161131"
NDVI$Latitude[NDVI$LL=="Lat50.113448Lon-123.386444"]<-"50.113448"
NDVI$Latitude[NDVI$LL=="Lat49.267269Lon-123.1194"]<-"49.267269"
NDVI$Latitude[NDVI$LL=="Lat58.99362Lon-123.90609"]<-"58.99362"
NDVI$Latitude[NDVI$LL=="Lat58.806285Lon-122.693959"]<-"58.806285"
NDVI$Latitude[NDVI$LL=="Lat56.78182Lon-123.87048"]<-"56.78182"
NDVI$Latitude[NDVI$LL=="Lat51.93811Lon-122.98102"]<-"51.93811"
NDVI$Latitude[NDVI$LL=="Lat49.60099Lon-117.05016"]<-"49.60099"
NDVI$Latitude[NDVI$LL=="Lat49.42771Lon-123.64706"]<-"49.42771"
NDVI$Latitude[NDVI$LL=="Lat50.87804Lon-119.90687"]<-"50.87804"
NDVI$Latitude[NDVI$LL=="Lat49.35676Lon-115.85358"]<-"49.35676"
NDVI$Latitude[NDVI$LL=="Lat53.871914Lon-123.479109"]<-"53.871914"
NDVI$Latitude[NDVI$LL=="Lat52.17482Lon-119.9843"]<-"52.17482"
NDVI$Latitude[NDVI$LL=="Lat54.05759Lon-124.8563"]<-"54.05759"
NDVI$Latitude[NDVI$LL=="Lat54.59471Lon-128.54665"]<-"54.59471"
NDVI$Latitude[NDVI$LL=="Lat49.766667Lon-119.75"]<-"49.766667"
NDVI$Latitude[NDVI$LL=="Lat49.87119Lon-119.16039"]<-"49.87119"
NDVI$Latitude[NDVI$LL=="Lat51.30368Lon-121.3974"]<-"51.30368"
NDVI$Latitude[NDVI$LL=="Lat49.45813Lon-120.50519"]<-"49.45813"
NDVI$Latitude[NDVI$LL=="Lat52.208042Lon-124.055797"]<-"52.208042"
NDVI$Latitude[NDVI$LL=="Lat52.979428Lon-122.493627"]<-"52.979428"
NDVI$Latitude[NDVI$LL=="Lat49.20665Lon-119.00507"]<-"49.20665"
NDVI$Latitude[NDVI$LL=="Lat49.02903Lon-119.25659"]<-"49.02903"
NDVI$Latitude[NDVI$LL=="Lat49.81488Lon-125.03448"]<-"49.81488"
NDVI$Latitude[NDVI$LL=="Lat50.19408Lon-121.59316"]<-"50.19408"
NDVI$Latitude[NDVI$LL=="Lat54.86665Lon-128.36853"]<-"54.86665"
NDVI$Latitude[NDVI$LL=="Lat50.91118Lon-119.43535"]<-"50.91118"
NDVI$Latitude[NDVI$LL=="Lat49.281361Lon-123.122793"]<-"49.281361"
NDVI$Latitude[NDVI$LL=="Lat50.813174Lon-121.324211"]<-"50.813174"
NDVI$Latitude[NDVI$LL=="Lat50.6412Lon-117.54325"]<-"50.6412"
NDVI$Latitude[NDVI$LL=="Lat50.25636Lon-125.943"]<-"50.25636"
NDVI$Latitude[NDVI$LL=="Lat52.46949Lon-121.19687"]<-"52.46949"
NDVI$Latitude[NDVI$LL=="Lat50.03652Lon-119.40214"]<-"50.03652"

NDVI$Longitude[NDVI$LL=="Lat51.78542Lon-122.23159"]<-"-122.23159"
NDVI$Longitude[NDVI$LL=="Lat50.721241Lon-121.283544"]<-"-121.283544"
NDVI$Longitude[NDVI$LL=="Lat56.27903Lon-120.69588"]<-"-120.69588"
NDVI$Longitude[NDVI$LL=="Lat59.63127Lon-133.68331"]<-"-133.68331"
NDVI$Longitude[NDVI$LL=="Lat56.6463Lon-121.20407"]<-"-121.20407"
NDVI$Longitude[NDVI$LL=="Lat53.16601Lon-121.45265"]<-"-121.45265"
NDVI$Longitude[NDVI$LL=="Lat49.539781Lon-115.755509"]<-"-115.755509"
NDVI$Longitude[NDVI$LL=="Lat49.26094Lon-123.24713"]<-"-123.24713"
NDVI$Longitude[NDVI$LL=="Lat54.02681Lon-123.947807"]<-"-123.947807"
NDVI$Longitude[NDVI$LL=="Lat49.12513Lon-116.58533"]<-"-116.58533"
NDVI$Longitude[NDVI$LL=="Lat52.156779Lon-123.715281"]<-"-123.715281"
NDVI$Longitude[NDVI$LL=="Lat50.14597Lon-121.57292"]<-"-121.57292"
NDVI$Longitude[NDVI$LL=="Lat50.209342Lon-126.595777"]<-"-126.595777"
NDVI$Longitude[NDVI$LL=="Lat55.75479Lon-120.04333"]<-"-120.04333"
NDVI$Longitude[NDVI$LL=="Lat49.161131Lon-121.951489"]<-"-121.951489"
NDVI$Longitude[NDVI$LL=="Lat50.113448Lon-123.386444"]<-"-123.386444"
NDVI$Longitude[NDVI$LL=="Lat49.267269Lon-123.1194"]<-"-123.1194"
NDVI$Longitude[NDVI$LL=="Lat58.99362Lon-123.90609"]<-"-123.90609"
NDVI$Longitude[NDVI$LL=="Lat58.806285Lon-122.693959"]<-"-122.693959"
NDVI$Longitude[NDVI$LL=="Lat56.78182Lon-123.87048"]<-"-123.87048"
NDVI$Longitude[NDVI$LL=="Lat51.93811Lon-122.98102"]<-"-122.98102"
NDVI$Longitude[NDVI$LL=="Lat49.60099Lon-117.05016"]<-"-117.05016"
NDVI$Longitude[NDVI$LL=="Lat49.42771Lon-123.64706"]<-"-123.64706"
NDVI$Longitude[NDVI$LL=="Lat50.87804Lon-119.90687"]<-"-119.90687"
NDVI$Longitude[NDVI$LL=="Lat49.35676Lon-115.85358"]<-"-115.85358"
NDVI$Longitude[NDVI$LL=="Lat53.871914Lon-123.479109"]<-"-123.479109"
NDVI$Longitude[NDVI$LL=="Lat52.17482Lon-119.9843"]<-"-119.9843"
NDVI$Longitude[NDVI$LL=="Lat54.05759Lon-124.8563"]<-"-124.8563"
NDVI$Longitude[NDVI$LL=="Lat54.59471Lon-128.54665"]<-"-128.54665"
NDVI$Longitude[NDVI$LL=="Lat49.766667Lon-119.75"]<-"-119.75"
NDVI$Longitude[NDVI$LL=="Lat49.87119Lon-119.16039"]<-"-119.16039"
NDVI$Longitude[NDVI$LL=="Lat51.30368Lon-121.3974"]<-"-121.3974"
NDVI$Longitude[NDVI$LL=="Lat49.45813Lon-120.50519"]<-"-120.50519"
NDVI$Longitude[NDVI$LL=="Lat52.208042Lon-124.055797"]<-"-124.055797"
NDVI$Longitude[NDVI$LL=="Lat52.979428Lon-122.493627"]<-"-122.493627"
NDVI$Longitude[NDVI$LL=="Lat49.20665Lon-119.00507"]<-"-119.00507"
NDVI$Longitude[NDVI$LL=="Lat49.02903Lon-119.25659"]<-"-119.25659"
NDVI$Longitude[NDVI$LL=="Lat49.81488Lon-125.03448"]<-"-125.03448"
NDVI$Longitude[NDVI$LL=="Lat50.19408Lon-121.59316"]<-"-121.59316"
NDVI$Longitude[NDVI$LL=="Lat54.86665Lon-128.36853"]<-"-128.36853"
NDVI$Longitude[NDVI$LL=="Lat50.91118Lon-119.43535"]<-"-119.43535"
NDVI$Longitude[NDVI$LL=="Lat49.281361Lon-123.122793"]<-"-123.122793"
NDVI$Longitude[NDVI$LL=="Lat50.813174Lon-121.324211"]<-"-121.324211"
NDVI$Longitude[NDVI$LL=="Lat50.6412Lon-117.54325"]<-"-117.54325"
NDVI$Longitude[NDVI$LL=="Lat50.25636Lon-125.943"]<-"-125.943"
NDVI$Longitude[NDVI$LL=="Lat52.46949Lon-121.19687"]<-"-121.19687"
NDVI$Longitude[NDVI$LL=="Lat50.03652Lon-119.40214"]<-"-119.40214"

NDVI <- subset( NDVI, select = -LL )

#removing the lighting fire
NDVI<-NDVI[!(NDVI$Latitude=="54.02681" & NDVI$Longitude=="-123.947807"), ]
NDVI<-NDVI[!(NDVI$Latitude=="50.721241" & NDVI$Longitude=="-121.283544"), ]
NDVI<-NDVI[!(NDVI$Latitude=="49.281361" & NDVI$Longitude=="-123.122793"), ]


NDVI$Month[NDVI$Day=="001"]<-"01"
NDVI$Month[NDVI$Day=="017"]<-"01"
NDVI$Month[NDVI$Day=="033"]<-"02"
NDVI$Month[NDVI$Day=="049"]<-"02"
NDVI$Month[NDVI$Day=="065"]<-"03"
NDVI$Month[NDVI$Day=="081"]<-"03"
NDVI$Month[NDVI$Day=="097"]<-"04"
NDVI$Month[NDVI$Day=="113"]<-"04"
NDVI$Month[NDVI$Day=="129"]<-"05"
NDVI$Month[NDVI$Day=="145"]<-"05"
NDVI$Month[NDVI$Day=="161"]<-"06"
NDVI$Month[NDVI$Day=="177"]<-"06"
NDVI$Month[NDVI$Day=="193"]<-"07"
NDVI$Month[NDVI$Day=="209"]<-"07"
NDVI$Month[NDVI$Day=="225"]<-"08"
NDVI$Month[NDVI$Day=="241"]<-"08"
NDVI$Month[NDVI$Day=="257"]<-"09"
NDVI$Month[NDVI$Day=="273"]<-"09"
NDVI$Month[NDVI$Day=="289"]<-"10"
NDVI$Month[NDVI$Day=="305"]<-"11"
NDVI$Month[NDVI$Day=="321"]<-"11"
NDVI$Month[NDVI$Day=="337"]<-"12"
NDVI$Month[NDVI$Day=="353"]<-"12"

NDVI$WDay[NDVI$Day=="001"]<-"01"
NDVI$WDay[NDVI$Day=="017"]<-"17"
NDVI$WDay[NDVI$Day=="033"]<-"02"
NDVI$WDay[NDVI$Day=="049"]<-"18"
NDVI$WDay[NDVI$Day=="065"]<-"06"
NDVI$WDay[NDVI$Day=="081"]<-"22"
NDVI$WDay[NDVI$Day=="097"]<-"07"
NDVI$WDay[NDVI$Day=="113"]<-"23"
NDVI$WDay[NDVI$Day=="129"]<-"09"
NDVI$WDay[NDVI$Day=="145"]<-"25"
NDVI$WDay[NDVI$Day=="161"]<-"10"
NDVI$WDay[NDVI$Day=="177"]<-"26"
NDVI$WDay[NDVI$Day=="193"]<-"12"
NDVI$WDay[NDVI$Day=="209"]<-"28"
NDVI$WDay[NDVI$Day=="225"]<-"13"
NDVI$WDay[NDVI$Day=="241"]<-"29"
NDVI$WDay[NDVI$Day=="257"]<-"14"
NDVI$WDay[NDVI$Day=="273"]<-"30"
NDVI$WDay[NDVI$Day=="289"]<-"16"
NDVI$WDay[NDVI$Day=="305"]<-"01"
NDVI$WDay[NDVI$Day=="321"]<-"17"
NDVI$WDay[NDVI$Day=="337"]<-"03"
NDVI$WDay[NDVI$Day=="353"]<-"19"



NDVI <- subset( NDVI, select = -Day )
NDVI <- NDVI[c("Year","Month","Day","Latitude","Longitude","Value")]


#LST Preprocessing
setwd('D:/MS/Trisemester-3/Data/New_Data/LST')
LST_Filenames <- list.files(full.names=TRUE)


LST <- lapply(LST_Filenames,function(i){
 read.csv(i, header=FALSE)
})

LST <- do.call(rbind.data.frame, LST)

LST <- subset( LST, select = -V1 )
LST <- subset( LST, select = -V2 )
LST <- subset( LST, select = -V5 )
LST <- subset( LST, select = -V6 )

LST <- melt(LST, id = c("V3","V4"))
LST <-read.csv("LST.csv",header = T,na.strings = c(""),stringsAsFactors = F)
LST <- subset( LST, select = -variable )

LST$value<-as.character(LST$value)
names(LST)[1]<-"Date"
names(LST)[2]<-"LL"

#Removing unwanted info
LST$LL<-gsub("Samp41Line41","",LST$LL)
LST$Date<-gsub("A","",LST$Date)

#Splitting to readable format for date
LST$Year <- substr(LST$Date, 1, 4)
LST$Day <- substr(LST$Date, 5, 7)
LST <- LST[c("Year","Day","Date","LL","value")]



LST$Month[LST$Day=="001"]<-"01"
LST$Month[LST$Day=="009"]<-"01"
LST$Month[LST$Day=="017"]<-"01"
LST$Month[LST$Day=="025"]<-"01"
LST$Month[LST$Day=="033"]<-"02"
LST$Month[LST$Day=="041"]<-"02"
LST$Month[LST$Day=="049"]<-"02"
LST$Month[LST$Day=="057"]<-"02"
LST$Month[LST$Day=="065"]<-"03"
LST$Month[LST$Day=="073"]<-"03"
LST$Month[LST$Day=="081"]<-"03"
LST$Month[LST$Day=="089"]<-"03"
LST$Month[LST$Day=="097"]<-"04"
LST$Month[LST$Day=="105"]<-"04"
LST$Month[LST$Day=="113"]<-"04"
LST$Month[LST$Day=="121"]<-"05"
LST$Month[LST$Day=="129"]<-"05"
LST$Month[LST$Day=="137"]<-"05"
LST$Month[LST$Day=="145"]<-"05"
LST$Month[LST$Day=="153"]<-"06"
LST$Month[LST$Day=="161"]<-"06"
LST$Month[LST$Day=="169"]<-"06"
LST$Month[LST$Day=="177"]<-"06"
LST$Month[LST$Day=="185"]<-"07"
LST$Month[LST$Day=="193"]<-"07"
LST$Month[LST$Day=="201"]<-"07"
LST$Month[LST$Day=="209"]<-"07"
LST$Month[LST$Day=="217"]<-"08"
LST$Month[LST$Day=="225"]<-"08"
LST$Month[LST$Day=="233"]<-"08"
LST$Month[LST$Day=="241"]<-"08"
LST$Month[LST$Day=="249"]<-"09"
LST$Month[LST$Day=="257"]<-"09"
LST$Month[LST$Day=="265"]<-"09"
LST$Month[LST$Day=="273"]<-"09"
LST$Month[LST$Day=="281"]<-"10"
LST$Month[LST$Day=="289"]<-"10"
LST$Month[LST$Day=="297"]<-"10"
LST$Month[LST$Day=="305"]<-"11"
LST$Month[LST$Day=="313"]<-"11"
LST$Month[LST$Day=="321"]<-"11"
LST$Month[LST$Day=="329"]<-"11"
LST$Month[LST$Day=="337"]<-"12"
LST$Month[LST$Day=="345"]<-"12"
LST$Month[LST$Day=="353"]<-"12"
LST$Month[LST$Day=="361"]<-"12"

LST$WDay[LST$Day=="001"]<-"01"
LST$WDay[LST$Day=="009"]<-"09"
LST$WDay[LST$Day=="017"]<-"17"
LST$WDay[LST$Day=="025"]<-"25"
LST$WDay[LST$Day=="033"]<-"02"
LST$WDay[LST$Day=="041"]<-"10"
LST$WDay[LST$Day=="049"]<-"18"
LST$WDay[LST$Day=="057"]<-"26"
LST$WDay[LST$Day=="065"]<-"06"
LST$WDay[LST$Day=="073"]<-"14"
LST$WDay[LST$Day=="081"]<-"22"
LST$WDay[LST$Day=="089"]<-"30"
LST$WDay[LST$Day=="097"]<-"07"
LST$WDay[LST$Day=="105"]<-"15"
LST$WDay[LST$Day=="113"]<-"23"
LST$WDay[LST$Day=="121"]<-"01"
LST$WDay[LST$Day=="129"]<-"09"
LST$WDay[LST$Day=="137"]<-"17"
LST$WDay[LST$Day=="145"]<-"25"
LST$WDay[LST$Day=="153"]<-"02"
LST$WDay[LST$Day=="161"]<-"10"
LST$WDay[LST$Day=="169"]<-"18"
LST$WDay[LST$Day=="177"]<-"26"
LST$WDay[LST$Day=="185"]<-"04"
LST$WDay[LST$Day=="193"]<-"12"
LST$WDay[LST$Day=="201"]<-"20"
LST$WDay[LST$Day=="209"]<-"28"
LST$WDay[LST$Day=="217"]<-"05"
LST$WDay[LST$Day=="225"]<-"13"
LST$WDay[LST$Day=="233"]<-"21"
LST$WDay[LST$Day=="241"]<-"29"
LST$WDay[LST$Day=="249"]<-"06"
LST$WDay[LST$Day=="257"]<-"14"
LST$WDay[LST$Day=="265"]<-"22"
LST$WDay[LST$Day=="273"]<-"30"
LST$WDay[LST$Day=="281"]<-"08"
LST$WDay[LST$Day=="289"]<-"16"
LST$WDay[LST$Day=="297"]<-"24"
LST$WDay[LST$Day=="305"]<-"01"
LST$WDay[LST$Day=="313"]<-"09"
LST$WDay[LST$Day=="321"]<-"17"
LST$WDay[LST$Day=="329"]<-"25"
LST$WDay[LST$Day=="337"]<-"03"
LST$WDay[LST$Day=="345"]<-"11"
LST$WDay[LST$Day=="353"]<-"19"
LST$WDay[LST$Day=="361"]<-"27"

LST <- subset( LST, select = -Day )
LST <- subset( LST, select = -Date )
names(LST)[5]<-"Day"
LST <- LST[c("Year","Month","Day","LL","value")]

LST$Latitude[LST$LL=="Lat51.78542Lon-122.23159"]<-"51.78542"
LST$Latitude[LST$LL=="Lat50.721241Lon-121.283544"]<-"50.721241"
LST$Latitude[LST$LL=="Lat56.27903Lon-120.69588"]<-"56.27903"
LST$Latitude[LST$LL=="Lat59.63127Lon-133.68331"]<-"59.63127"
LST$Latitude[LST$LL=="Lat56.6463Lon-121.20407"]<-"56.6463"
LST$Latitude[LST$LL=="Lat53.16601Lon-121.45265"]<-"53.16601"
LST$Latitude[LST$LL=="Lat49.539781Lon-115.755509"]<-"49.539781"
LST$Latitude[LST$LL=="Lat49.26094Lon-123.24713"]<-"49.26094"
LST$Latitude[LST$LL=="Lat54.02681Lon-123.947807"]<-"54.02681"
LST$Latitude[LST$LL=="Lat49.12513Lon-116.58533"]<-"49.12513"
LST$Latitude[LST$LL=="Lat52.156779Lon-123.715281"]<-"52.156779"
LST$Latitude[LST$LL=="Lat50.14597Lon-121.57292"]<-"50.14597"
LST$Latitude[LST$LL=="Lat50.209342Lon-126.595777"]<-"50.209342"
LST$Latitude[LST$LL=="Lat55.75479Lon-120.04333"]<-"55.75479"
LST$Latitude[LST$LL=="Lat49.161131Lon-121.951489"]<-"49.161131"
LST$Latitude[LST$LL=="Lat50.113448Lon-123.386444"]<-"50.113448"
LST$Latitude[LST$LL=="Lat49.267269Lon-123.1194"]<-"49.267269"
LST$Latitude[LST$LL=="Lat58.99362Lon-123.90609"]<-"58.99362"
LST$Latitude[LST$LL=="Lat58.806285Lon-122.693959"]<-"58.806285"
LST$Latitude[LST$LL=="Lat56.78182Lon-123.87048"]<-"56.78182"
LST$Latitude[LST$LL=="Lat51.93811Lon-122.98102"]<-"51.93811"
LST$Latitude[LST$LL=="Lat49.60099Lon-117.05016"]<-"49.60099"
LST$Latitude[LST$LL=="Lat49.42771Lon-123.64706"]<-"49.42771"
LST$Latitude[LST$LL=="Lat50.87804Lon-119.90687"]<-"50.87804"
LST$Latitude[LST$LL=="Lat49.35676Lon-115.85358"]<-"49.35676"
LST$Latitude[LST$LL=="Lat53.871914Lon-123.479109"]<-"53.871914"
LST$Latitude[LST$LL=="Lat52.17482Lon-119.9843"]<-"52.17482"
LST$Latitude[LST$LL=="Lat54.05759Lon-124.8563"]<-"54.05759"
LST$Latitude[LST$LL=="Lat54.59471Lon-128.54665"]<-"54.59471"
LST$Latitude[LST$LL=="Lat49.766667Lon-119.75"]<-"49.766667"
LST$Latitude[LST$LL=="Lat49.87119Lon-119.16039"]<-"49.87119"
LST$Latitude[LST$LL=="Lat51.30368Lon-121.3974"]<-"51.30368"
LST$Latitude[LST$LL=="Lat49.45813Lon-120.50519"]<-"49.45813"
LST$Latitude[LST$LL=="Lat52.208042Lon-124.055797"]<-"52.208042"
LST$Latitude[LST$LL=="Lat52.979428Lon-122.493627"]<-"52.979428"
LST$Latitude[LST$LL=="Lat49.20665Lon-119.00507"]<-"49.20665"
LST$Latitude[LST$LL=="Lat49.02903Lon-119.25659"]<-"49.02903"
LST$Latitude[LST$LL=="Lat49.81488Lon-125.03448"]<-"49.81488"
LST$Latitude[LST$LL=="Lat50.19408Lon-121.59316"]<-"50.19408"
LST$Latitude[LST$LL=="Lat54.86665Lon-128.36853"]<-"54.86665"
LST$Latitude[LST$LL=="Lat50.91118Lon-119.43535"]<-"50.91118"
LST$Latitude[LST$LL=="Lat49.281361Lon-123.122793"]<-"49.281361"
LST$Latitude[LST$LL=="Lat50.813174Lon-121.324211"]<-"50.813174"
LST$Latitude[LST$LL=="Lat50.6412Lon-117.54325"]<-"50.6412"
LST$Latitude[LST$LL=="Lat50.25636Lon-125.943"]<-"50.25636"
LST$Latitude[LST$LL=="Lat52.46949Lon-121.19687"]<-"52.46949"
LST$Latitude[LST$LL=="Lat50.03652Lon-119.40214"]<-"50.03652"

LST$Longitude[LST$LL=="Lat51.78542Lon-122.23159"]<-"-122.23159"
LST$Longitude[LST$LL=="Lat50.721241Lon-121.283544"]<-"-121.283544"
LST$Longitude[LST$LL=="Lat56.27903Lon-120.69588"]<-"-120.69588"
LST$Longitude[LST$LL=="Lat59.63127Lon-133.68331"]<-"-133.68331"
LST$Longitude[LST$LL=="Lat56.6463Lon-121.20407"]<-"-121.20407"
LST$Longitude[LST$LL=="Lat53.16601Lon-121.45265"]<-"-121.45265"
LST$Longitude[LST$LL=="Lat49.539781Lon-115.755509"]<-"-115.755509"
LST$Longitude[LST$LL=="Lat49.26094Lon-123.24713"]<-"-123.24713"
LST$Longitude[LST$LL=="Lat54.02681Lon-123.947807"]<-"-123.947807"
LST$Longitude[LST$LL=="Lat49.12513Lon-116.58533"]<-"-116.58533"
LST$Longitude[LST$LL=="Lat52.156779Lon-123.715281"]<-"-123.715281"
LST$Longitude[LST$LL=="Lat50.14597Lon-121.57292"]<-"-121.57292"
LST$Longitude[LST$LL=="Lat50.209342Lon-126.595777"]<-"-126.595777"
LST$Longitude[LST$LL=="Lat55.75479Lon-120.04333"]<-"-120.04333"
LST$Longitude[LST$LL=="Lat49.161131Lon-121.951489"]<-"-121.951489"
LST$Longitude[LST$LL=="Lat50.113448Lon-123.386444"]<-"-123.386444"
LST$Longitude[LST$LL=="Lat49.267269Lon-123.1194"]<-"-123.1194"
LST$Longitude[LST$LL=="Lat58.99362Lon-123.90609"]<-"-123.90609"
LST$Longitude[LST$LL=="Lat58.806285Lon-122.693959"]<-"-122.693959"
LST$Longitude[LST$LL=="Lat56.78182Lon-123.87048"]<-"-123.87048"
LST$Longitude[LST$LL=="Lat51.93811Lon-122.98102"]<-"-122.98102"
LST$Longitude[LST$LL=="Lat49.60099Lon-117.05016"]<-"-117.05016"
LST$Longitude[LST$LL=="Lat49.42771Lon-123.64706"]<-"-123.64706"
LST$Longitude[LST$LL=="Lat50.87804Lon-119.90687"]<-"-119.90687"
LST$Longitude[LST$LL=="Lat49.35676Lon-115.85358"]<-"-115.85358"
LST$Longitude[LST$LL=="Lat53.871914Lon-123.479109"]<-"-123.479109"
LST$Longitude[LST$LL=="Lat52.17482Lon-119.9843"]<-"-119.9843"
LST$Longitude[LST$LL=="Lat54.05759Lon-124.8563"]<-"-124.8563"
LST$Longitude[LST$LL=="Lat54.59471Lon-128.54665"]<-"-128.54665"
LST$Longitude[LST$LL=="Lat49.766667Lon-119.75"]<-"-119.75"
LST$Longitude[LST$LL=="Lat49.87119Lon-119.16039"]<-"-119.16039"
LST$Longitude[LST$LL=="Lat51.30368Lon-121.3974"]<-"-121.3974"
LST$Longitude[LST$LL=="Lat49.45813Lon-120.50519"]<-"-120.50519"
LST$Longitude[LST$LL=="Lat52.208042Lon-124.055797"]<-"-124.055797"
LST$Longitude[LST$LL=="Lat52.979428Lon-122.493627"]<-"-122.493627"
LST$Longitude[LST$LL=="Lat49.20665Lon-119.00507"]<-"-119.00507"
LST$Longitude[LST$LL=="Lat49.02903Lon-119.25659"]<-"-119.25659"
LST$Longitude[LST$LL=="Lat49.81488Lon-125.03448"]<-"-125.03448"
LST$Longitude[LST$LL=="Lat50.19408Lon-121.59316"]<-"-121.59316"
LST$Longitude[LST$LL=="Lat54.86665Lon-128.36853"]<-"-128.36853"
LST$Longitude[LST$LL=="Lat50.91118Lon-119.43535"]<-"-119.43535"
LST$Longitude[LST$LL=="Lat49.281361Lon-123.122793"]<-"-123.122793"
LST$Longitude[LST$LL=="Lat50.813174Lon-121.324211"]<-"-121.324211"
LST$Longitude[LST$LL=="Lat50.6412Lon-117.54325"]<-"-117.54325"
LST$Longitude[LST$LL=="Lat50.25636Lon-125.943"]<-"-125.943"
LST$Longitude[LST$LL=="Lat52.46949Lon-121.19687"]<-"-121.19687"
LST$Longitude[LST$LL=="Lat50.03652Lon-119.40214"]<-"-119.40214"


LST <- subset( LST, select = -LL )
LST <- LST[c("Year","Month","Day","Latitude","Longitude","value")]

#BA Preprocessing
setwd('D:/MS/Trisemester-3/Data/New_Data/BA')


BA_Filenames <- list.files(full.names=TRUE)


BA <- lapply(BA_Filenames,function(i){
  read.csv(i, header=FALSE)
})

BA <- do.call(rbind.data.frame, BA)

BA <- subset( BA, select = -V1 )
BA <- subset( BA, select = -V2 )
BA <- subset( BA, select = -V5 )
BA <- subset( BA, select = -V6 )

BA <- melt(BA, id = c("V3","V4"))
BA <-read.csv("BA.csv",header = T,na.strings = c(""),stringsAsFactors = F)
BA <- subset( BA, select = -variable )

BA$value<-as.character(BA$value)
BA<-na.omit(BA)
#renaing col
names(BA)[1]<-"Date"
names(BA)[2]<-"LL"

#Removing unwanted info
BA$LL<-gsub("Samp41Line41","",BA$LL)
BA$Date<-gsub("A","",BA$Date)
#Splitting to readable format for date
BA$Year <- substr(BA$Date, 1, 4)
BA$Day <- substr(BA$Date, 5, 7)
BA <- BA[c("Year","Day","Date","LL","value")]


BA$Month[BA$Day=="001"]<-"01"
BA$Month[BA$Day=="009"]<-"01"
BA$Month[BA$Day=="017"]<-"01"
BA$Month[BA$Day=="025"]<-"01"
BA$Month[BA$Day=="033"]<-"02"
BA$Month[BA$Day=="041"]<-"02"
BA$Month[BA$Day=="049"]<-"02"
BA$Month[BA$Day=="057"]<-"02"
BA$Month[BA$Day=="065"]<-"03"
BA$Month[BA$Day=="073"]<-"03"
BA$Month[BA$Day=="081"]<-"03"
BA$Month[BA$Day=="089"]<-"03"
BA$Month[BA$Day=="097"]<-"04"
BA$Month[BA$Day=="105"]<-"04"
BA$Month[BA$Day=="113"]<-"04"
BA$Month[BA$Day=="121"]<-"05"
BA$Month[BA$Day=="129"]<-"05"
BA$Month[BA$Day=="137"]<-"05"
BA$Month[BA$Day=="145"]<-"05"
BA$Month[BA$Day=="153"]<-"06"
BA$Month[BA$Day=="161"]<-"06"
BA$Month[BA$Day=="169"]<-"06"
BA$Month[BA$Day=="177"]<-"06"
BA$Month[BA$Day=="185"]<-"07"
BA$Month[BA$Day=="193"]<-"07"
BA$Month[BA$Day=="201"]<-"07"
BA$Month[BA$Day=="209"]<-"07"
BA$Month[BA$Day=="217"]<-"08"
BA$Month[BA$Day=="225"]<-"08"
BA$Month[BA$Day=="233"]<-"08"
BA$Month[BA$Day=="241"]<-"08"
BA$Month[BA$Day=="249"]<-"09"
BA$Month[BA$Day=="257"]<-"09"
BA$Month[BA$Day=="265"]<-"09"
BA$Month[BA$Day=="273"]<-"09"
BA$Month[BA$Day=="281"]<-"10"
BA$Month[BA$Day=="289"]<-"10"
BA$Month[BA$Day=="297"]<-"10"
BA$Month[BA$Day=="305"]<-"11"
BA$Month[BA$Day=="313"]<-"11"
BA$Month[BA$Day=="321"]<-"11"
BA$Month[BA$Day=="329"]<-"11"
BA$Month[BA$Day=="337"]<-"12"
BA$Month[BA$Day=="345"]<-"12"
BA$Month[BA$Day=="353"]<-"12"
BA$Month[BA$Day=="361"]<-"12"

BA$WDay[BA$Day=="001"]<-"01"
BA$WDay[BA$Day=="009"]<-"09"
BA$WDay[BA$Day=="017"]<-"17"
BA$WDay[BA$Day=="025"]<-"25"
BA$WDay[BA$Day=="033"]<-"02"
BA$WDay[BA$Day=="041"]<-"10"
BA$WDay[BA$Day=="049"]<-"18"
BA$WDay[BA$Day=="057"]<-"26"
BA$WDay[BA$Day=="065"]<-"06"
BA$WDay[BA$Day=="073"]<-"14"
BA$WDay[BA$Day=="081"]<-"22"
BA$WDay[BA$Day=="089"]<-"30"
BA$WDay[BA$Day=="097"]<-"07"
BA$WDay[BA$Day=="105"]<-"15"
BA$WDay[BA$Day=="113"]<-"23"
BA$WDay[BA$Day=="121"]<-"01"
BA$WDay[BA$Day=="129"]<-"09"
BA$WDay[BA$Day=="137"]<-"17"
BA$WDay[BA$Day=="145"]<-"25"
BA$WDay[BA$Day=="153"]<-"02"
BA$WDay[BA$Day=="161"]<-"10"
BA$WDay[BA$Day=="169"]<-"18"
BA$WDay[BA$Day=="177"]<-"26"
BA$WDay[BA$Day=="185"]<-"04"
BA$WDay[BA$Day=="193"]<-"12"
BA$WDay[BA$Day=="201"]<-"20"
BA$WDay[BA$Day=="209"]<-"28"
BA$WDay[BA$Day=="217"]<-"05"
BA$WDay[BA$Day=="225"]<-"13"
BA$WDay[BA$Day=="233"]<-"21"
BA$WDay[BA$Day=="241"]<-"29"
BA$WDay[BA$Day=="249"]<-"06"
BA$WDay[BA$Day=="257"]<-"14"
BA$WDay[BA$Day=="265"]<-"22"
BA$WDay[BA$Day=="273"]<-"30"
BA$WDay[BA$Day=="281"]<-"08"
BA$WDay[BA$Day=="289"]<-"16"
BA$WDay[BA$Day=="297"]<-"24"
BA$WDay[BA$Day=="305"]<-"01"
BA$WDay[BA$Day=="313"]<-"09"
BA$WDay[BA$Day=="321"]<-"17"
BA$WDay[BA$Day=="329"]<-"25"
BA$WDay[BA$Day=="337"]<-"03"
BA$WDay[BA$Day=="345"]<-"11"
BA$WDay[BA$Day=="353"]<-"19"
BA$WDay[BA$Day=="361"]<-"27"

BA <- subset( BA, select = -Day )
BA <- subset( BA, select = -Date )
names(BA)[5]<-"Day"
BA <- BA[c("Year","Month","Day","LL","value")]


BA$Latitude[BA$LL=="Lat51.78542Lon-122.23159"]<-"51.78542"
BA$Latitude[BA$LL=="Lat50.721241Lon-121.283544"]<-"50.721241"
BA$Latitude[BA$LL=="Lat56.27903Lon-120.69588"]<-"56.27903"
BA$Latitude[BA$LL=="Lat59.63127Lon-133.68331"]<-"59.63127"
BA$Latitude[BA$LL=="Lat56.6463Lon-121.20407"]<-"56.6463"
BA$Latitude[BA$LL=="Lat53.16601Lon-121.45265"]<-"53.16601"
BA$Latitude[BA$LL=="Lat49.539781Lon-115.755509"]<-"49.539781"
BA$Latitude[BA$LL=="Lat49.26094Lon-123.24713"]<-"49.26094"
BA$Latitude[BA$LL=="Lat54.02681Lon-123.947807"]<-"54.02681"
BA$Latitude[BA$LL=="Lat49.12513Lon-116.58533"]<-"49.12513"
BA$Latitude[BA$LL=="Lat52.156779Lon-123.715281"]<-"52.156779"
BA$Latitude[BA$LL=="Lat50.14597Lon-121.57292"]<-"50.14597"
BA$Latitude[BA$LL=="Lat50.209342Lon-126.595777"]<-"50.209342"
BA$Latitude[BA$LL=="Lat55.75479Lon-120.04333"]<-"55.75479"
BA$Latitude[BA$LL=="Lat49.161131Lon-121.951489"]<-"49.161131"
BA$Latitude[BA$LL=="Lat50.113448Lon-123.386444"]<-"50.113448"
BA$Latitude[BA$LL=="Lat49.267269Lon-123.1194"]<-"49.267269"
BA$Latitude[BA$LL=="Lat58.99362Lon-123.90609"]<-"58.99362"
BA$Latitude[BA$LL=="Lat58.806285Lon-122.693959"]<-"58.806285"
BA$Latitude[BA$LL=="Lat56.78182Lon-123.87048"]<-"56.78182"
BA$Latitude[BA$LL=="Lat51.93811Lon-122.98102"]<-"51.93811"
BA$Latitude[BA$LL=="Lat49.60099Lon-117.05016"]<-"49.60099"
BA$Latitude[BA$LL=="Lat49.42771Lon-123.64706"]<-"49.42771"
BA$Latitude[BA$LL=="Lat50.87804Lon-119.90687"]<-"50.87804"
BA$Latitude[BA$LL=="Lat49.35676Lon-115.85358"]<-"49.35676"
BA$Latitude[BA$LL=="Lat53.871914Lon-123.479109"]<-"53.871914"
BA$Latitude[BA$LL=="Lat52.17482Lon-119.9843"]<-"52.17482"
BA$Latitude[BA$LL=="Lat54.05759Lon-124.8563"]<-"54.05759"
BA$Latitude[BA$LL=="Lat54.59471Lon-128.54665"]<-"54.59471"
BA$Latitude[BA$LL=="Lat49.766667Lon-119.75"]<-"49.766667"
BA$Latitude[BA$LL=="Lat49.87119Lon-119.16039"]<-"49.87119"
BA$Latitude[BA$LL=="Lat51.30368Lon-121.3974"]<-"51.30368"
BA$Latitude[BA$LL=="Lat49.45813Lon-120.50519"]<-"49.45813"
BA$Latitude[BA$LL=="Lat52.208042Lon-124.055797"]<-"52.208042"
BA$Latitude[BA$LL=="Lat52.979428Lon-122.493627"]<-"52.979428"
BA$Latitude[BA$LL=="Lat49.20665Lon-119.00507"]<-"49.20665"
BA$Latitude[BA$LL=="Lat49.02903Lon-119.25659"]<-"49.02903"
BA$Latitude[BA$LL=="Lat49.81488Lon-125.03448"]<-"49.81488"
BA$Latitude[BA$LL=="Lat50.19408Lon-121.59316"]<-"50.19408"
BA$Latitude[BA$LL=="Lat54.86665Lon-128.36853"]<-"54.86665"
BA$Latitude[BA$LL=="Lat50.91118Lon-119.43535"]<-"50.91118"
BA$Latitude[BA$LL=="Lat49.281361Lon-123.122793"]<-"49.281361"
BA$Latitude[BA$LL=="Lat50.813174Lon-121.324211"]<-"50.813174"
BA$Latitude[BA$LL=="Lat50.6412Lon-117.54325"]<-"50.6412"
BA$Latitude[BA$LL=="Lat50.25636Lon-125.943"]<-"50.25636"
BA$Latitude[BA$LL=="Lat52.46949Lon-121.19687"]<-"52.46949"
BA$Latitude[BA$LL=="Lat50.03652Lon-119.40214"]<-"50.03652"

BA$Longitude[BA$LL=="Lat51.78542Lon-122.23159"]<-"-122.23159"
BA$Longitude[BA$LL=="Lat50.721241Lon-121.283544"]<-"-121.283544"
BA$Longitude[BA$LL=="Lat56.27903Lon-120.69588"]<-"-120.69588"
BA$Longitude[BA$LL=="Lat59.63127Lon-133.68331"]<-"-133.68331"
BA$Longitude[BA$LL=="Lat56.6463Lon-121.20407"]<-"-121.20407"
BA$Longitude[BA$LL=="Lat53.16601Lon-121.45265"]<-"-121.45265"
BA$Longitude[BA$LL=="Lat49.539781Lon-115.755509"]<-"-115.755509"
BA$Longitude[BA$LL=="Lat49.26094Lon-123.24713"]<-"-123.24713"
BA$Longitude[BA$LL=="Lat54.02681Lon-123.947807"]<-"-123.947807"
BA$Longitude[BA$LL=="Lat49.12513Lon-116.58533"]<-"-116.58533"
BA$Longitude[BA$LL=="Lat52.156779Lon-123.715281"]<-"-123.715281"
BA$Longitude[BA$LL=="Lat50.14597Lon-121.57292"]<-"-121.57292"
BA$Longitude[BA$LL=="Lat50.209342Lon-126.595777"]<-"-126.595777"
BA$Longitude[BA$LL=="Lat55.75479Lon-120.04333"]<-"-120.04333"
BA$Longitude[BA$LL=="Lat49.161131Lon-121.951489"]<-"-121.951489"
BA$Longitude[BA$LL=="Lat50.113448Lon-123.386444"]<-"-123.386444"
BA$Longitude[BA$LL=="Lat49.267269Lon-123.1194"]<-"-123.1194"
BA$Longitude[BA$LL=="Lat58.99362Lon-123.90609"]<-"-123.90609"
BA$Longitude[BA$LL=="Lat58.806285Lon-122.693959"]<-"-122.693959"
BA$Longitude[BA$LL=="Lat56.78182Lon-123.87048"]<-"-123.87048"
BA$Longitude[BA$LL=="Lat51.93811Lon-122.98102"]<-"-122.98102"
BA$Longitude[BA$LL=="Lat49.60099Lon-117.05016"]<-"-117.05016"
BA$Longitude[BA$LL=="Lat49.42771Lon-123.64706"]<-"-123.64706"
BA$Longitude[BA$LL=="Lat50.87804Lon-119.90687"]<-"-119.90687"
BA$Longitude[BA$LL=="Lat49.35676Lon-115.85358"]<-"-115.85358"
BA$Longitude[BA$LL=="Lat53.871914Lon-123.479109"]<-"-123.479109"
BA$Longitude[BA$LL=="Lat52.17482Lon-119.9843"]<-"-119.9843"
BA$Longitude[BA$LL=="Lat54.05759Lon-124.8563"]<-"-124.8563"
BA$Longitude[BA$LL=="Lat54.59471Lon-128.54665"]<-"-128.54665"
BA$Longitude[BA$LL=="Lat49.766667Lon-119.75"]<-"-119.75"
BA$Longitude[BA$LL=="Lat49.87119Lon-119.16039"]<-"-119.16039"
BA$Longitude[BA$LL=="Lat51.30368Lon-121.3974"]<-"-121.3974"
BA$Longitude[BA$LL=="Lat49.45813Lon-120.50519"]<-"-120.50519"
BA$Longitude[BA$LL=="Lat52.208042Lon-124.055797"]<-"-124.055797"
BA$Longitude[BA$LL=="Lat52.979428Lon-122.493627"]<-"-122.493627"
BA$Longitude[BA$LL=="Lat49.20665Lon-119.00507"]<-"-119.00507"
BA$Longitude[BA$LL=="Lat49.02903Lon-119.25659"]<-"-119.25659"
BA$Longitude[BA$LL=="Lat49.81488Lon-125.03448"]<-"-125.03448"
BA$Longitude[BA$LL=="Lat50.19408Lon-121.59316"]<-"-121.59316"
BA$Longitude[BA$LL=="Lat54.86665Lon-128.36853"]<-"-128.36853"
BA$Longitude[BA$LL=="Lat50.91118Lon-119.43535"]<-"-119.43535"
BA$Longitude[BA$LL=="Lat49.281361Lon-123.122793"]<-"-123.122793"
BA$Longitude[BA$LL=="Lat50.813174Lon-121.324211"]<-"-121.324211"
BA$Longitude[BA$LL=="Lat50.6412Lon-117.54325"]<-"-117.54325"
BA$Longitude[BA$LL=="Lat50.25636Lon-125.943"]<-"-125.943"
BA$Longitude[BA$LL=="Lat52.46949Lon-121.19687"]<-"-121.19687"
BA$Longitude[BA$LL=="Lat50.03652Lon-119.40214"]<-"-119.40214"


BA <- subset( BA, select = -LL )
BA <- BA[c("Year","Month","Day","Latitude","Longitude","value")]



#merging BA and LST
Data <- cbind(LST, BA)
Data<-na.omit(Data)

Data <- subset( Data, select = -Year )
Data <- subset( Data, select = -Day.1 )
Data <- subset( Data, select = -Month.1 )
Data <- subset( Data, select = -Latitude.1 )
Data <- subset( Data, select = -Longitude.1 )

Data <- Data[c("Year","Month","Day","Latitude","Longitude","value","value.1")]

names(Data)[6]<-"LST"
names(Data)[7]<-"Burned_Area"


#merging NDVI and LST and BA
DataF <- cbind(NDVI, Data)
rm(Data)
Data<-na.omit(DataF)

Data <- subset( Data, select = -Year )
Data <- subset( Data, select = -Day.1 )
Data <- subset( Data, select = -Month.1 )
Data <- subset( Data, select = -Latitude.1 )
Data <- subset( Data, select = -Longitude.1 )

Data <- Data[c("Year","Month","Day","Latitude","Longitude","value","value.1")]

names(Data)[6]<-"LST"
names(Data)[7]<-"Burned_Area"

#removing the uncmmon dates from NDVI
dates <- c(19,2,25,10,1,29,17,13,3,6,23,18,12,28,30,16,22,9,14,7,26)
LST<-LST[LST$Day %in% dates,]
BA<-BA[BA$Day %in% dates,]


#Now all the three attributes where merged

#Removing the NA values, F value and redadunt values

Data<-Data[!(Data$LST=="F" & Data$Burned_Area=="F" & Data$NDVI=="F" ), ]
Data<-Data[!(Data$LST=="F"), ]
Data<-Data[!(Data$Burned_Area=="F"), ]
Data<-Data[!(Data$NDVI=="F"), ]

Data <- Data[!duplicated(Data)]

LSTBA <- data.frame(lapply(LSTBA, as.character), stringsAsFactors=FALSE)
NDVI <- data.frame(lapply(NDVI, as.character), stringsAsFactors=FALSE)


#Soil moisture Preprocessing
#Surface Soil moisture
setwd('D:/MS/Trisemester-3/Data/New_Data/Sm')
SM_Filenames <- list.files(full.names=TRUE)


SM <- lapply(SM_Filenames,function(i){
read.csv(i, header=FALSE)
})

SM <- do.call(rbind.data.frame, SM)

SM <-read.csv("SM.csv",header = T,na.strings = c(""),stringsAsFactors = F)
names(SM)[1]<-"Date"
names(SM)[2]<-"Soil moisture"

SM$Month <- substr(SM$Date, 1, 4)
SM$WDAY <- substr(SM$Date, 5, 7)
SM$Year <- substr(SM$Date, 8, 16)

SM<-SM[!(SM$Year=="time_star"),]
SM<-SM[!(SM$Year=="2010"),]
SM<-SM[!(SM$Year=="2011"),]
SM<-SM[!(SM$Year=="2013"),]

SM <- subset( SM, select = -Date )


#Sub-surface soil moisture
setwd('D:/MS/Trisemester-3/Data/New_Data/Susm')
Susm <-read.csv("Susm.csv",header = T,na.strings = c(""),stringsAsFactors = F)
names(Susm)[1]<-"Date"
names(Susm)[2]<-"Soil moisture"

Susm$Month <- substr(Susm$Date, 1, 4)
Susm$WDAY <- substr(Susm$Date, 5, 7)
Susm$Year <- substr(Susm$Date, 8, 16)

Susm<-Susm[!(Susm$Year=="time_star"),]
Susm<-Susm[!(Susm$Year=="2010"),]
Susm<-Susm[!(Susm$Year=="2011"),]
Susm<-Susm[!(Susm$Year=="2013"),]

SM <- subset( SM, select = -Date )

#merging SM and SUSM
DataS <- cbind(SM, Susm)
DataS <- subset( DataS, select = -Latitude )
DataS <- subset( DataS, Data = -Longitude )
DataS <- subset( DataS, select = -Month )
DataS <- subset( DataS, select = -WDAY )
DataS <- subset( DataS, select = -Year )
DataS <- subset( DataS, select = -Date )
DataS <- subset( DataS, select = -Longitude.1 )

names(DataS)[1]<-"Soil moisture"
names(DataS)[3]<-"Sub Soil Moisture"

DataS <- DataS[c("year","Month.1","WDAY.1","Latitude","Longitude", "Soil moisture" ,"Sub Soil Moisture")]

names(DataS)[2]<-"Month"
names(DataS)[3]<-"Day"



DataS$Day<-gsub(",","",DataS$Day)


#Merging all data together
Soil <- Soil[with(Soil, order(Year,Month,Day,Latitude,Longitude)), ]
row.names(Soil) <- c(1:nrow(Soil))
names(Soil)[1]<-"Year"

Data <- Data[with(Data, order(Year,Month,Day,Latitude,Longitude)), ]
row.names(Data) <- c(1:nrow(Data))

test <- Soil[with(Soil, order(Year,Month,Day,Latitude,Longitude)), ]
row.names(Soil) <- c(1:nrow(Soil))
Data<-merge(Data,DataS)


#labelling data
setwd('D:/MS/Trisemester-3/Data')
Wf <-read.csv("D:/MS/Trisemester-3/Data/Wildfires_dates.csv",header = T,na.strings = c(""),stringsAsFactors = F)
#Labelling the Data
Data$Class<-"0"
Data$Class[Data$Year =="2012" & Data$Month =="10" & Data$Latitude =="50.813174" & Data$Longitude=="-121.324211"] <- "1"
Data$Class[Data$Year =="2012" & Data$Month =="10" & Data$Latitude =="49.766667" & Data$Longitude=="-119.75"] <- "1"
Data$Class[Data$Year =="2012" & Data$Month =="10" & Data$Latitude =="58.806285" & Data$Longitude=="-122.693959"] <- "1"

Data$Class[Data$Year =="2014" & Data$Month =="7" & Data$Latitude =="52.979428" & Data$Longitude=="-122.493627"] <- "1"
Data$Class[Data$Year =="2014" & Data$Month =="7" & Data$Latitude =="52.156779" & Data$Longitude=="-123.715281"] <- "1"

#2015

Data$Class[Data$Year =="2015" & Data$Month =="5" & Data$Day %in% c('1','9') & Data$Latitude =="52.208042" & Data$Longitude=="-123.479109"] <- "1"

Data$Class[Data$Year =="2015" & Data$Month =="7" & Data$Day =="12" & Data$Latitude =="52.208042" & Data$Longitude=="-124.055797"] <- "1"

Data$Class[Data$Year =="2015" & Data$Month =="6" & Data$Day %in% c('10','18') & Data$Latitude =="50.113448" & Data$Longitude=="-123.386444"] <- "1"

Data$Class[Data$Year =="2015" & Data$Month =="6" & Data$Day %in% c('10','18') & Data$Latitude =="49.53978" & Data$Longitude=="-115.755509"] <- "1"

Data$Class[Data$Year =="2015" & Data$Month =="7" & Data$Day == '12' & Data$Latitude =="50.209342" & Data$Longitude=="-126.595777"] <- "1"

Data$Class[Data$Year =="2015" & Data$Month =="7" & Data$Day == '12' & Data$Latitude =="49.161131" & Data$Longitude=="-121.951489"] <- "1"

Data$Class[Data$Year =="2015" & Data$Month =="8" & Data$Day == '13' & Data$Latitude =="50.03652" & Data$Longitude=="-119.40214"] <- "1"

Data$Class[Data$Year =="2015" & Data$Month =="8" & Data$Day == '13' & Data$Latitude =="54.59471" & Data$Longitude=="-128.54665"] <- "1"

Data$Class[Data$Year =="2015" & Data$Month =="6" & Data$Day %in% c('10','18') & Data$Latitude =="50.14597" & Data$Longitude=="-121.57292"] <- "1"

Data$Class[Data$Year =="2015" & Data$Month =="6" & Data$Day =='10' & Data$Latitude =="49.42771" & Data$Longitude=="-123.64706"] <- "1"

Data$Class[Data$Year =="2015" & Data$Month =="8" & Data$Day == '13' & Data$Latitude =="50.6412" & Data$Longitude=="-117.54325"] <- "1"

Data$Class[Data$Year =="2015" & Data$Month =="8" & Data$Day == '13' & Data$Latitude =="49.02903" & Data$Longitude=="-119.25659"] <- "1"

Data$Class[Data$Year =="2015" & Data$Day %in% c('13','28') & Data$Month =="8" & Data$Latitude =="52.46949" & Data$Longitude=="-121.19687"] <- "1"

Data$Class[Data$Year =="2015" & Data$Day %in% c('13','28') & Data$Month =="8" & Data$Latitude =="50.91118" & Data$Longitude=="-119.43535"] <- "1"

Data$Class[Data$Year =="2015" & Data$Month =="6" & Data$Day =='10' & Data$Latitude =="53.16601" & Data$Longitude=="-121.45265"] <- "1"

Data$Class[Data$Year =="2015" & Data$Month =="6" & Data$Day =='10' & Data$Latitude =="49.81488" & Data$Longitude=="-125.03448"] <- "1"

Data$Class[Data$Year =="2015" & Data$Day %in% c('13','28') & Data$Month =="8" & Data$Latitude =="49.20665" & Data$Longitude=="-119.00507"] <- "1"

Data$Class[Data$Year =="2015" & Data$Day %in% c('13','28') & Data$Month =="8" & Data$Latitude =="54.86665" & Data$Longitude=="-128.36853"] <- "1"

#2016

Data$Class[Data$Year =="2016" & Data$Day %in% c('23') & Data$Month =="4" & Data$Latitude =="56.27903" & Data$Longitude=="-120.69588"] <- "1"
Data$Class[Data$Year =="2016" & Data$Day %in% c('23') & Data$Month =="4" & Data$Latitude =="56.6463" & Data$Longitude=="-121.20407"] <- "1"
Data$Class[Data$Year =="2016" & Data$Day %in% c('23') & Data$Month =="4" & Data$Latitude =="56.78182" & Data$Longitude=="-123.87048"] <- "1"

Data$Class[Data$Year =="2016" & Data$Day %in% c('29') & Data$Month =="8" & Data$Latitude =="59.63127" & Data$Longitude=="-133.68331"] <- "1"

Data$Class[Data$Year =="2016" & Data$Day %in% c('6') & Data$Month =="9" & Data$Latitude =="50.19408" & Data$Longitude=="-121.59316"] <- "1"

#2017 
Data$Class[Data$Year =="2017" & Data$Day %in% c('12') & Data$Month =="7" & Data$Latitude =="49.267269" & Data$Longitude=="-123.1194"] <- "1"
Data$Class[Data$Year =="2017" & Data$Day %in% c('12') & Data$Month =="7" & Data$Latitude =="49.45813" & Data$Longitude=="-120.50519"] <- "1"
Data$Class[Data$Year =="2017" & Data$Day %in% c('12') & Data$Month =="7" & Data$Latitude =="52.17482" & Data$Longitude=="-119.9843"] <- "1"

Data$Class[Data$Year =="2017" & Data$Day %in% c('12') & Data$Month =="7" & Data$Latitude =="51.30368" & Data$Longitude=="-121.3974"] <- "1"
Data$Class[Data$Year =="2017" & Data$Day %in% c('12') & Data$Month =="7" & Data$Latitude =="51.93811" & Data$Longitude=="-122.98102"] <- "1"
Data$Class[Data$Year =="2017" & Data$Day %in% c('12') & Data$Month =="7" & Data$Latitude =="49.12513" & Data$Longitude=="-116.58533"] <- "1"

Data$Class[Data$Year =="2017" & Data$Day %in% c('28') & Data$Month =="7" & Data$Latitude =="55.75479" & Data$Longitude=="-120.04333"] <- "1"
Data$Class[Data$Year =="2017" & Data$Day %in% c('28') & Data$Month =="7" & Data$Latitude =="49.60099" & Data$Longitude=="-117.05016"] <- "1"

Data$Class[Data$Year =="2017" & Data$Day %in% c('29') & Data$Month =="8" & Data$Latitude =="49.87119" & Data$Longitude=="-119.16039"] <- "1"
Data$Class[Data$Year =="2017" & Data$Day %in% c('6') & Data$Month =="9" & Data$Latitude =="58.99362" & Data$Longitude=="-123.90609"] <- "1"



Data$Class[Data$Year =="2017" & Data$Day %in% c('28') & Data$Month =="7" & Data$Latitude =="49.35676" & Data$Longitude=="-115.85358"] <- "1"
Data$Class[Data$Year =="2017" & Data$Day %in% c('13') & Data$Month =="8" & Data$Latitude =="49.35676" & Data$Longitude=="-115.85358"] <- "1"


Data$Class[Data$Year =="2017" & Data$Day %in% c('29','13') & Data$Month =="8" & Data$Latitude =="50.25636" & Data$Longitude=="-125.943"] <- "1"
Data$Class[Data$Year =="2017" & Data$Day %in% c('29','13') & Data$Month =="8" & Data$Latitude =="50.87804" & Data$Longitude=="-119.90687"] <- "1"

#2018
Data$Class[Data$Year =="2018" & Data$Day %in% c('12','28') & Data$Month =="7" & Data$Latitude =="54.05759" & Data$Longitude=="-124.8563"] <- "1"
Data$Class[Data$Year =="2018" & Data$Day %in% c('29','13') & Data$Month =="8" & Data$Latitude =="50.87804" & Data$Longitude=="-119.90687"] <- "1"

Data$Class[Data$Year =="2018" & Data$Day %in% c('28') & Data$Month =="7" & Data$Latitude =="50.87804" & Data$Longitude=="-119.90687"] <- "1"
Data$Class[Data$Year =="2018" & Data$Day %in% c('13') & Data$Month =="8" & Data$Latitude =="51.78542" & Data$Longitude=="-122.23159"] <- "1"


write.csv(Data,"Wildfire.csv", row.names=FALSE)
