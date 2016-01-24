#####################################################################################################
###############This calculates how many photographers inside building x also took photographs inside building y
######################################################################################################
#selects only photographs that fall within a building
photowbs<-subset(photowbuild, photowbuild$ID!="<NA>")
#Creates a crosstab matrix of people and which buildings they have photographs that fall within
k<-table(photowbs$ï..V1, photowbs$ID)
#The rows are the photographer ids the columns are the building ids
##Removes all rows where a person only photographed one building
k<-subset(k, rowsum(k)>1)

########################Creates the matrix that will store the results#################################

#Create a matrix with the row and column dimensions are equal to the number of buildings
store<<-matrix(nrow=dim(k)[2], ncol=dim(k)[2])
store[1:dim(k)[2],1:dim(k)[2]]<-0 #initialises the starting values of the matrix to 0

#Sets the group ids as the row and column names of the store matrix
rownames(store)<-colnames(k)
colnames(store)<-colnames(k)

#############The two functions that are applied########################################################
#######################################################################################################
#Function 2 which is called by function 1
#This function writes the results to the store matrix 

func2<-function(x){

store[c(x[1]), c(x[2])]<-store[c(x[1]), c(x[2])]+1


}
###################################################################################################
#Function 1
#This function takes a person, selects all the buildings that they photographed and works out all the combinations
func1<-function(x){


#Select all the buildings that were photographed by a given person
hs<-subset(f, f>0)
print(length(hs))

#nam<<-names(hs)
#print(nam)

#Works through the building names and generates all combinations
#s<-combn(nam,2)

#apply(f,2,func1)


}

################################################################################################################
# Applies the function and writes the results to the computer
################################################################################################################
#This applies function 1 row by row i.e. 1 is the input the apply function  
#a row index variable is added to the k matrix (the crosstab of the groupids by the userids) he data to keep track of the row names
apply(k,1,func1)

#These operations set diagonal and upper diagonal elements of the matrix to nothing as we are interested in groups 
#linked by common members and need to format so that gephi can read it

store[upper.tri(store)]<-""
diag(store)<-""

#Then write the resulting matrix to the computer as a csv
write.csv(store, "filepath")

#Note: to read csv adjacency matrices into gephi need to convert commas to semi colons
# and format nodes as text and edge weights as numbers
##############################################################################################################
##############################################################################################################
