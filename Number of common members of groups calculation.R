#####################################################################################################
###############This calculates how many photographers inside building x also took photographs inside building y
######################################################################################################
#selects only photographs that fall within a building
photowbs<-subset(photowb, photowb$ID!="<NA>")
#Creates a crosstab matrix of people and which buildings they have photographs that fall within
k<-table(photowbs$ï..V1, photowbs$ID)
#The rows are the people ids the columns are the building ids

########################Creates the matrix that will store the results#################################

#Create a matrix with the row and column dimensions are equal to the number of groups
store<<-matrix(nrow=dim(k)[1], ncol=dim(k)[1])
store[1:dim(k)[1],1:dim(k)[1]]<-0 #initialises the starting values of the matrix to 0

#Sets the group ids as the row and column names of the store matrix
rownames(store)<-rownames(k)
colnames(store)<-rownames(k)

#############The two functions that are applied########################################################
#######################################################################################################
#Function 2 which is called by function 1
#This function writes the results to the store matrix indexed by x the rows and y the columns

func2<-function(x, y){

store[c(x), c(y)]<<-store[c(x), c(y)]+grptab[[c(as.character(y))]]
print(store[c(x), c(y)])

}
###################################################################################################
#Function 1
#This function takes a group, selects all the people that go to that group, and then returns all the groups
#those people are members are of and the number of people per group
func1<-function(x){

# Removes the first column which is used to index the row loop from the analysis
f = x[-1] 

#Select all people that are members of the group we are analysing
hs<-subset(f, f>0)

#This extracts the groupid of the group that we are analysing, using the artificial
#row index variable we have created in the cbind(seq_len(nrow(k)),k) command below
p<-rownames(k)[x[1]] 
  
#Gets the list of names of people that correspond to the groups
nam<-names(hs)

#This selects the set of all groups (and associated data) that those people attended by matching userids on the nam list we have just created
grp<-photowbs[photowbs$ï..V1 %in% nam,]

#This table calculates how many people who went to group x also attended other groups
grptab<<-table(grp[,c("ID")])

# Extracts the names of the groups
cs<-names(grptab)

#For each of the groups associated with people going to group x=p write the
#number of attendees to the matrix using the second function
lapply(cs, func2, x=p)

#deletes the grptab table
rm(grptab)

}

################################################################################################################
# Applies the function and writes the results to the computer
################################################################################################################
#This applies function 1 row by row i.e. 1 is the input the apply function  
#a row index variable is added to the k matrix (the crosstab of the groupids by the userids) he data to keep track of the row names
apply(cbind(seq_len(nrow(k)),k),1,func1)

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
