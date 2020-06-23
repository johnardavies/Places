library("rgdal")
library("timeDate")
library("classInt")
library('maptools')
library('RColorBrewer')
library('rgeos')
library('sp')



#This code merges the buildings dataset with the listed buildings dataset

#1. Reads in the shape files for London buildings############################################
Londonbuild<-readOGR("Londobbuildfilepath", "london_buildings_open_map_local_250315")

#Sests the projection up
Londonbuild<-spTransform(Londonbuild, CRS("+proj=longlat +datum=WGS84"))


#Reads the listed buildings data shape files
Listbuild<-readOGR("D:\\Listedbuildings", "20120224_ListedBuilding")


#Sests the projection so that it is the same 
Listbuild<-spTransform(Listbuild, CRS("+proj=longlat +datum=WGS84"))

##############################################################################################
#2. Removes listed buildings that are not in London using the NUTS shape files
states2<-readOGR("NUTSfilepathdata","NUTS_RG_03M_2006")

London<-subset(states2, states2@data$NUTS_ID=='UKI')

London@data$NUTS_ID<-as.character(London@data$NUTS_ID)  #Sets as character to remove the other blank NUTS_IDs

London<-spTransform(London, CRS("+proj=longlat +datum=WGS84"))

#Looks at the Listed buildings falling within London and selects them
inL<-over(Listbuild, London)

Listbuild@data<-cbind(Listbuild@data, inL)

#Selects listed buildings that are in London only
Listbuild<-subset(Listbuild,Listbuild@data$NUTS_ID=='UKI')

Listbuild<-spTransform(Listbuild, CRS("+proj=longlat +datum=WGS84"))

###############################################################################################
#3. Converts the listed buildings data from a spatial polygons dataframe to a spatial points dataframe
pts<-coordinates(Listbuild)

pts = SpatialPoints(pts)

#sets up the projection for the points
proj4string(pts) <- CRS("+proj=longlat +datum=WGS84")


Londonlist = SpatialPointsDataFrame(pts, Listbuild@data)
#################################################################################################
#4. Sees which London building is also listed
f1<-over(Londonbuild, Londonlist)

#Binds the results to the Londonbuilding data frame so that, where a building is listed the building's
#metadata contains it as additional information

Londonbuild@data<-cbind(Londonbuild@data,f1)
##################################################################################################
#5. Creates a separate listed buildings dataset and writes as a geojson
#Creates a spatial polygon dataframe that cover listed buildings in London only
listed<-subset(Londonbuild, is.na(Londonbuild@data$ListEntry)==FALSE)


#Writes the resulting file as a geojson file called listed
writeOGR(listed, "outputfilepath\\listed", layer='listed',driver='GeoJSON') 



