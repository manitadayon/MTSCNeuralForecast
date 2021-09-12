## First reading the time series:
cat("\014") 
folder='C:\Users\User_name\Desktop'
All_CSV= list.files(path=folder,pattern = "*.csv")  ## Reading all the csv files
Data=list() 

Data=lapply(All_CSV, function(x) {read.csv(x,header=TRUE,sep=",")})
##### Removing the first column since this is for 
#### ID and then convert the dataframe to numeric

Data_adjusted=lapply(Data, function(x) {x[-1] <- lapply(x[-1], as.numeric); x[-1]})

Data_matrix=lapply(Data_adjusted, function(x) {data.matrix(x, rownames.force = NA)})

### Now it is time to find the distance 
SIZE=length(Data_matrix)
DIST2=matrix(data=NA,nrow=SIZE,ncol=SIZE)  # Allocate matrix
########## 
start_time <- Sys.time()
for (ii in 1:SIZE)
{
  for (jj in 1:SIZE)
  {
    Intermediate= proxy::dist(Data_matrix[[ii]],Data_matrix[[jj]])
    DIST2[ii,jj]=dtw(Intermediate)$distance
  }
}
end_time <- Sys.time()
end_time - start_time

######
# To improve the performance we know dtw is symmetric therefore 
# we can just calculate the lower traingular version and copy it for
## upper  part.
###########################################
### Clustering using hierarchical clustering
DIST2_no_nan<- na.omit(DIST2)
DIST2_no_nan=as.dist(DIST2_no_nan)  # Convert to distance object
clusters_data <- hclust(DIST2_no_nan, method = "ward.D2")
plot(clusters_data)
clusters_cut_data <- cutree(clusters_data, k = 4)
########## Visualization
# Plot clusters boundery
plot(clusters_data)
rect.hclust(clusters_data , k = 4)
######################
### Cluster validity
library(NbClust)

nb <- NbClust(diss=DIST2_no_nan, distance = NULL, min.nc=3, max.nc=15, method = "ward.D2", index =c("frey"), alphaBeale = 0.1)
### Create a dataframe where first column is well name 
### and secod column is cluster labels.
Data_data_frame<- data.frame('Name'=All_CSV,'Cluster_Labels'=clusters_cut_data)
##############################
### Save the dataframe in a csv file
write.csv(Data_data_frame, file = 'C:\Users\User_name\Desktop',row.names=FALSE)
#################################################


