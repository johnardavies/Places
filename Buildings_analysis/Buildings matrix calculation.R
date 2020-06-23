###############################################################################################################
#This calculates how many photographers inside building x also took photographs inside building y
#Returns a matrix
##############################################################################################################

#Reads in the photo data matched to the buildings that the photographs fell within
photowbuild<-read.csv("filepath1",header=T, stringsAsFactors = FALSE)
#If stringsasfactors not set to FALSE this raises issues with building ids being included even if they have been excluded due to low levels of photography

#selects only photographs that fall within a building
photowbs<-subset(photowbuild, photowbuild$ID!="<NA>")

#Calculates number of photographs per building
removett<-table(photowbs$ID)
#Converts to a data frame
removett<-as.data.frame(removett)

#Merges in the data on the number of photographs per building
photowbs<-merge(photowbs,removett, by.x="ID", by.y="Var1", all.x=TRUE)

#selects buildings that have more than 500 photographs inside them
photowbs<-subset(photowbs, photowbs$Freq>500, drop=TRUE) 
############################################################################################################

#Creates a crosstab matrix of people and which buildings they have photographs that fall within
k<-table(photowbs$ï..V1, photowbs$ID)

########################Creates the matrix that will store the results######################################

#Create a matrix with the row and column dimensions are equal to the number of buildings
store<<-matrix(nrow=dim(k)[2], ncol=dim(k)[2])
store[1:dim(k)[2],1:dim(k)[2]]<-0 #initialises the starting values of the matrix to 0

#Sets the group ids as the row and column names of the store matrix
rownames(store)<-colnames(k)
colnames(store)<-colnames(k)

#############The two functions that are applied#############################################################
############################################################################################################
#Function 2 which is called by function 1
#This function takes the matrix of different combinations and writes it to a store matrix

func2<-function(x){
store[c(x[1]), c(x[2])]<<-store[c(x[1]), c(x[2])]+1
}
############################################################################################################
#Function 1
#This function takes a person, selects all the buildings that they photographed and works out all the combinations
func1<-function(x){

#Select all the buildings that were photographed by a given person
hs<-subset(x, x>0)
#Gets the building names
nam<-names(hs)
#print(nam)
if (length(nam)>1){
#Works through the building names and generates all combinations
s<-combn(nam,2)
apply(s,2,func2)

}
}

################################################################################################################
#Applies the function and writes the results to the computer
################################################################################################################
#This applies function 1 row by row i.e. 1 is the input the apply function  
#Each represents a photographer and the buildings he/she has photographed
apply(k,1,func1)


#Then write the resulting matrix to the computer as a csv
write.csv(store, "filepath2")

#Note: to read csv adjacency matrices into gephi need to convert commas to semi colons
##############################################################################################################
##############################################################################################################

#Loads in the matrix data with the names added in, better to automate this stage
dat=read.csv(filepath\\Flickr work\\photoswbuildingmatrix2.csv",header=TRUE,row.names=1,check.names=FALSE) # choose an adjacency matrix from a .csv file
m=as.matrix(dat) # coerces the data set as a matrix

#These operations remove the data that corresponds to names which have alphanumeric coding
#1.Creates a list of the alphanumeric names
alpha<-(grep("0D4",colnames(m), value=TRUE))
#2. Removes  the alphanumeric names
mcut<-subset(colnames(m), (colnames(m) %in% alpha )==FALSE)

#Subsets the matrix to the row and column names that we want using the non alphanumeric names
m<-m[mcut,mcut]

print(dim(m)) #Checks the matrix has been reduced
library(igraph)

#Converts the matrix to a graphfile
g=graph.adjacency(m,mode="undirected",weighted=NULL) 

#writes the graphfile
write.graph(g,"filepath\\buildingswnames2.graphml" ,format=c("graphml"))

