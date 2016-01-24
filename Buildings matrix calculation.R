###############################################################################################################
###############This calculates how many photographers inside building x also took photographs inside building y
##############################################################################################################

#Reads in the photo data matched to the buildings that the photographs fell within
photowbuild<-read.csv("C:\\Users\\John\\Documents\\Flickr work\\photoswbuildings.csv",header=T, stringsAsFactors = FALSE)

#If stringsasfactors not set to fault this raises issues with building ids being included even if they have been excluded due to low levels of photography


#selects only photographs that fall within a building
photowbs<-subset(photowbuild, photowbuild$ID!="<NA>")

#Removes buildings with 1 photo in them#######################################################################
removett<-table(photowbs$ID)
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
#This function takes the matrix of different combinaations and writes it to a store matrix

func2<-function(x){

store[c(x[1]), c(x[2])]<<-store[c(x[1]), c(x[2])]+1


}
############################################################################################################
#Function 1
#This function takes a person, selects all the buildings that they photographed and works out all the combinations
func1<-function(x){


#Select all the buildings that were photographed by a given person
hs<-subset(x, x>0)

nam<-names(hs)
#print(nam)
if (length(nam)>1){

#Works through the building names and generates all combinations
s<-combn(nam,2)

apply(s,2,func2)

}


}

################################################################################################################
# Applies the function and writes the results to the computer
################################################################################################################
#This applies function 1 row by row i.e. 1 is the input the apply function  
#Each represents a photographer and the buildings he has photographed
apply(k,1,func1)


#Then write the resulting matrix to the computer as a csv
write.csv(store, "filepath")

#Note: to read csv adjacency matrices into gephi need to convert commas to semi colons
# and format nodes as text and edge weights as numbers
##############################################################################################################
##############################################################################################################
