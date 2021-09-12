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
# IN NO EVENT SHALL THE  COPYRIGHT  OWNER OR CONTRIBUTORS BE  LIABLE  FOR  ANY  DIRECT,  INDIRECT,  INCIDENTAL,  SPECIAL, EXEMPLARY, OR  CONSEQUENTIAL  DAMAGES (INCLUDING,  BUT  NOT  LIMITED  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS  OF  USE, DATA, OR  PROFITS; OR  BUSINESS  INTERRUPTION)  HOWEVER  CAUSED  AND  ON  ANY  THEORY  OF  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)  ARISING  IN  ANY  WAY  OUT  OF  THE  USE  OF  THIS  SOFTWARE,  EVEN  IF ADVISED OF  THE  POSSIBILITY  OF  SUCH  DAMAGE.


####### This is the script used to generate simulated data for arbitarly number of 
### columns and arbitarly number of multivariate time series
library(forecast)
library(dtw)
library(NbClust)
library(tsfeatures)
# devtools::install_github("jabiru/tictoc")
library(tictoc)   # how long takes
cat("\014")
dev.off()
N_mts=400  # Number of Time series
Num_col<-3
Num_col_arima<-2
low_no_arima<-c(1000)
upp_no_arima<-c(3000)
Col_Name1<-c('Water','Gas')
Col_Name2<-c('Oil')
diff_order<-2
Time_length<-400
low_arima<-numeric(Num_col_arima)
upp_arima<-c(1000,2000)
num_zero<-5
num_outlier<-5
outlier_range<-4000  ## Bigger than all the upp values
### We will create N_mts number of data in a loop
Var_divider<-40
Synthetic_data<-list()
for (ii in 1:N_mts) {
  AR=runif(1,0,1)
  # MA=runif(1,0,1)
  MA<-0
  
  Synthetic_data[[ii]]<-Simulated_Data(Num_col,Num_col_arima,Col_Name1,Col_Name2,diff_order,AR,MA,Var_divider,Time_length,low_arima,upp_arima,low_no_arima,upp_no_arima,num_zero,num_outlier,outlier_range)
}
plot(Synthetic_data[[4]][,2],type = 'l')
###########################################
### Anamoly detection and handling
flag_anamoly<-1   # Imputing missing values and outliers
Synthetic_data_outlier<-outlier_detect(Synthetic_data,flag_anamoly)
#####################################
### Plotting the data with/without outlier
File_Name=c(1:N_mts)
data_original<-Synthetic_data
data_no_outlier<-Synthetic_data_outlier
save_loc<-'/Users/User_name/Documents/Synthetic_MTS/'
col<-c(1,2,3)
flag<-0
save_plotv2(data_original,data_no_outlier,File_Name,save_loc,col,flag,Col_Name)
############################################################
#### Add auxiliary varibale to include vector of time to each matrix
### This variable used for saving not the actual variable since
### most functions use Data_creation function as it excludes the first vector
### which is the time vector.
data_original_w_Time<-Synthetic_data
Time<-1:(diff_order+Time_length)
data_original_w_Time<-lapply(data_original_w_Time, function(x) {cbind(Time,x) }) ## Adding Time Column to all matrix

data_No_Outlier_w_Time<-Synthetic_data_outlier
data_No_Outlier_w_Time<-lapply(data_No_Outlier_w_Time, function(x) {cbind(Time,x) }) ## Adding Time Column to all matrix

################################################################
### Save the actual data iand save the one without outlier in csv
### different location
original_loc<-'/Users/User_name/Documents/Synthetic_MTS/Original_CSV_Files/'
save_to_file(data_original_w_Time,File_Name,original_loc)
No_outlier_loc<-'/Users/User_name/Documents/Synthetic_MTS/No_Outlier_Files/'
save_to_file(data_No_Outlier_w_Time,File_Name,No_outlier_loc)
#####################################################
############## Finding correlation using correlation matrix
####  Since it is important for clustering and forecasting
Corr_Matrix<-Correlation_matrix(Synthetic_data_outlier)

##### Make the data as cumulative data as follows:
## data_2=data_1+data_2
## data_3=data_1+data_2+data_3
cumulative_data_with_Time<-lapply(data_No_Outlier_w_Time, function(x) {apply(x, 2, cumsum)})
cumulative_loc<-'/Users/User_name/Documents/Synthetic_MTS/Cumulative_No_outlier/'
save_to_file(cumulative_data_with_Time,File_Name,cumulative_loc)
############## Finding correlation for cumulaive data correlation matrix
####  Since it is important for clustering and forecasting
cumulative_data_No_Time<-lapply(Synthetic_data_outlier, function(x) {apply(x, 2, cumsum)})
Corr_Matrix<-Correlation_matrix(cumulative_data_No_Time)


##########################################################
##########Clustering Feature based using tsfeature
### Choose columns for clustering of the target-the Gas- here which is the
### third column and choose other columns whoose correlation is low to 
### the target as helper which here will be all other columns
### Therefore for forecasting and clustering all columns are used.
Path<- '/Users/User_name/Documents/Synthetic_MTS/Cumulative_No_outlier'
cwd<- setwd(Path)
CSV_File_Name<-file.info(list.files(path=Path,pattern = "*.csv"))  # Get the info of file
## To sort it using modification time
CSV_File_Name = CSV_File_Name[with(CSV_File_Name, order(as.POSIXct(mtime))), ]
CSV_File_Name=rownames(CSV_File_Name)

features=c(acf_features,entropy,lumpiness, stability,crossing_points,holt_parameters,flat_spots,motiftwo_entro3,pacf_features,std1st_der,histogram_mode)  
feats_len=c(6,1,1,1,1,2,1,1,3,1,1)  
tic("All_Feature_Tsfeature_Clustering")
feat<-c(1,2,3)   # since time is first removed
flag_cvi<-1# If this is zero we use predetermined number of clusters
K<-2
flag_feat_builtin<-0
flag_Norm<-1  # for normalization
flag_pca<-0  # with PCA
all_feat_clusters<-Cluster_Formation(CSV_File_Name,features,feat,feats_len,flag_Norm,flag_cvi,flag_pca,flag_feat_builtin,n_pc,K,cwd)
toc()

tic("Gas_Feature_Tsfeature_Clustering")
n_pc<-10
feat2=c(3)
flag_feat_builtin<-0
K<-2
flag_cvi<-1
flag_pca<-0  # with PCA
Gas_clusters<-Cluster_Formation(CSV_File_Name,features,feat2,feats_len,flag_Norm,flag_cvi,flag_pca,flag_feat_builtin,n_pc,K,cwd)
toc()

###################################################
##### Creating data for static data
N_Col<-4
Len<- N_mts
Low<-c(100,300,500,2000)
Upp<-c(200,400,700,3000)
ColName<-1:N_Col
Row_prefix<-'Row '
Static_Data<-Static_Data_Creation(N_Col,Len,Low,Upp,ColName,Row_prefix)
Static_Data<-apply(Static_Data, 2, as.numeric)  # in all the columns are list and not numeric
save_loc<- '/Users/User_name/Documents/Synthetic_MTS/Static_data.csv'
write.csv(Static_Data, file = save_loc,row.names=TRUE)  # Saving the Static Data

#################################################3
Static_Data<-read.csv("/Users/User_name/Documents/Synthetic_MTS/Static_data.csv",header = TRUE,sep = ',')

