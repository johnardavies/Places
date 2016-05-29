library("rgdal")
library("timeDate")
library("classInt")
library('maptools')
library('RColorBrewer')
library('rgeos')

#This code adds a flag according to:

#1.  whether the photo was taken in inner or outer London

#2.  the ward that the photo was taken in

#3.  adds a flag according to whether the picture had been taken by a tourist
     #calculated according to whether they have been taking photos for less than three weeks

 
################################################################################################################################
#####################Reads in the photos data###################################################################################
################################################################################################################################
photo=read.csv("photofilepath")


#selects only those with unique photos ids
photo<-unique(photo)

photom<-subset(photo, photo[,3]>50) #Removes some of the anomalous latitude data

photoid<-matrix(photo[1:dim(photo)[1],1],dim(photo)[1], byrow=T)

#Selects the spatial components from the photo data
photo1<-matrix(photo[,2],dim(photo)[1], byrow=T)
photo2<-matrix(photo[,3],dim(photo)[1], byrow=T)
photom<- cbind(photo1,photo2)


####creates the points into spatial points
pts = SpatialPoints(photom)

##sets up the projection for the photos as the same
proj4string(pts) <- CRS("+proj=longlat +datum=WGS84")

#1. Code that calculates a flag according to whether a photo is in inner of outer London
##############################################################################################################
#Reads in the the EU NUTS shape files NUTS_RG_03M_2006

EUNUTS<-readOGR("filepath1","NUTS_RG_03M_2006")

#UKI1 is inner London UKI2 is outer London
Lmap<-subset(EUNUTS, EUNUTS@data$NUTS_ID=='UKI1'|EUNUTS@data$NUTS_ID=='UKI2',)

#Sets the variable as a character. If this is not done you import a lot of empty variable names
Lmap@data$NUTS_ID<-as.character(Lmap@data$NUTS_ID)

#Sets the projection of the London ward maps so that it is the same as that of the photos
Lmap<-spTransform(Lmap, CRS("+proj=longlat +datum=WGS84"))

##############################################################################################
#Adds a flag for inner and outer London

#returnswhether the photos fell in inner or outer London
f<-over(pts,Lmap, returnlist=TRUE) 

#Recodes the photos falling outside London those outside appear as NAs
f$NUTS_ID<-ifelse(is.na(f$NUTS_ID)==TRUE ,'OUTSIDE', f$NUTS_ID)

#binds the results to the photo data 
dataphoto<-cbind(photo, f)

#creates a spatial points data frame this will create a new variable called spdf@data$NUTS_ID
spdf = SpatialPointsDataFrame(pts, dataphoto)


#2. Code that creates a flag accrording to which London ward a photograph was taken in
########################################################################################################
####Reads in the London wards data######################################################################

#Reads in the London wards data \London-wards-2014_ESRI","London_Ward_CityMerged"
Lwards<-readOGR("filepath2","London_Ward_CityMerged")

#sets up the lWards projection so that it is the same as everything else
Lwards<-spTransform(Lwards, CRS("+proj=longlat +datum=WGS84"))

###Overlays the photos data with the wards level data###################################################

#selects the wards that the photographs fall within
ward<-over(pts,Lwards, returnlist=TRUE)

#binds to the spatial points data frame
spdf@data<-cbind(spdf@data, ward)  #The new ward level variable is spdf@data$NAME


# 3. Creates a flag according to how long the period people have been taking photographs for
##########################################################################################################
#daysactive is a function that calculates how long people have been taking photographs for ###############

daysactive<-function(x){max(x) - min(x)}

spdf@data$V4<-as.timeDate(spdf@data$V4, zone = "GMT") #Converts the date taken variable to a time date object

#Creates a year variable
spdf@data$yr<-format(spdf@data$V4,"%Y")

#Creates an hour variable
spdf@data$hr<-format(spdf@data$V4,"%H")

#Creates a months variable
spdf@data$months<-months(spdf@data$V4)

#Calculates the number of days between people's oldest and newest photos
s<-aggregate(spdf@data$V4, by=list(spdf@data$ï..V1),FUN=daysactive)

s$tourist<-ifelse(s[, c("GMT:x")]<7*3 ,1,0)

#Merges the tourist flag with the photographs data

spdf@data<-merge(spdf@data , s , by.x="ï..V1" , by.y="Group.1" , all.x=TRUE)

#The tourist variable is spdf@data$tourist###########################################################
