# Copyright  (c) 2021  California  Institute  of Technology ("Caltech"). U.S. Government sponsorship acknowledged.
# 
# All  rights  reserved.
# 
# Redistribution  and  use  in  source  and  binary  forms,  with  or  without  modification,  are  permitted  provided that the  following  conditions are  met:
# 
# - Redistributions  of  source  code  must  retain  the  above  copyright  notice,  this  list  of  conditions  and the  following  disclaimer.
# - Redistributions  in  binary  form  must  reproduce  the  above  copyright  notice,  this  list  of  conditions and  the  following  disclaimer  in  the  documentation  and/or other materials provided  with  the distribution.
# - Neither  the  name  of  Caltech  nor  its  operating  division,  the  Jet  Propulsion  Laboratory,  nor  the names  of  its  contributors  may  be  used  to  endorse  or  promote  products  derived  from  this  software without  specific  prior  written  permission.
# 
# THIS  SOFTWARE  IS  PROVIDED  BY  THE  COPYRIGHT  HOLDERS  AND  CONTRIBUTORS  "AS IS" AND  ANY  EXPRESS  OR  IMPLIED  WARRANTIES, INCLUDING, BUT  NOT  LIMITED  TO, THE  IMPLIED  WARRANTIES  OF  MERCHANTABILITY  AND  FITNESS  FOR A  PARTICULAR PURPOSE ARE DISCLAIMED. 
# IN NO EVENT SHALL THE  COPYRIGHT  OWNER OR CONTRIBUTORS BE  LIABLE  FOR  ANY  DIRECT,  INDIRECT,  INCIDENTAL,  SPECIAL, EXEMPLARY, OR  CONSEQUENTIAL  DAMAGES (INCLUDING,  BUT  NOT  LIMITED  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS  OF  USE, DATA, OR  PROFITS; 
# OR  BUSINESS  INTERRUPTION)  HOWEVER  CAUSED  AND  ON  ANY  THEORY  OF  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)  ARISING  IN  ANY  WAY  OUT  OF  THE  USE  OF  THIS  SOFTWARE,  EVEN  IF ADVISED OF  THE  POSSIBILITY  OF  SUCH  DAMAGE.


### Example of using the Dynamic Time Warping.
cat("\014") 
folder='C:\Users\User_name\Desktop'
All_CSV= list.files(path=folder,pattern = "*.csv")  ## Reading all the csv files
Data=list() 

Data=lapply(All_CSV, function(x) {read.csv(x,header=TRUE,sep=",")})
##### Removing the first column since this is for 
#### ID and then convert the dataframe to numeric

Data=lapply(Data, function(x) {x[-1] <- lapply(x[-1], as.numeric); x[-1]})

Data_matrix=lapply(Data, function(x) {data.matrix(x, rownames.force = NA)})

### Now it is time to find the distance 
SIZE=length(Data_matrix)
DIST=matrix(data=NA,nrow=SIZE,ncol=SIZE)  # Allocate matrix
########## 
start_time <- Sys.time()
for (ii in 1:SIZE)
{
  for (jj in 1:SIZE)
  {
    Intermediate= proxy::dist(Data_matrix[[ii]],Data_matrix[[jj]])
    DIST[ii,jj]=dtw(Intermediate)$distance
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
DIST_no_nan<- na.omit(DIST)
DIST_no_nan=as.dist(DIST_no_nan)  # Convert to distance object
clusters_data <- hclust(DIST_no_nan, method = "ward.D2")
plot(clusters_data)
clusters_cut_data <- cutree(clusters_data, k = 4)    ## Or any number. The best way is to draw the tree and see where you should draw the line.
########## Visualization
# Plot clusters boundery
plot(clusters_data)
rect.hclust(clusters_data , k = 4)
######################
### Cluster validity
library(NbClust)
nb <- NbClust(diss=DIST_no_nan, distance = NULL, min.nc=3, max.nc=15, method = "ward.D2", index =c("frey"), alphaBeale = 0.1)



