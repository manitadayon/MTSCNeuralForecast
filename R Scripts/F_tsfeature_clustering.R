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
# IN NO EVENT SHALL THE  COPYRIGHT  OWNER OR CONTRIBUTORS BE  LIABLE  FOR  ANY  DIRECT,  INDIRECT,  INCIDENTAL,  SPECIAL, EXEMPLARY, OR  CONSEQUENTIAL  DAMAGES (INCLUDING,  BUT  NOT  LIMITED  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS  OF  USE, DATA, 
# OR  PROFITS; OR  BUSINESS  INTERRUPTION)  HOWEVER  CAUSED  AND  ON  ANY  THEORY  OF  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)  ARISING  IN  ANY  WAY  OUT  OF  THE  USE  OF  THIS  SOFTWARE,  EVEN  IF ADVISED OF  THE  POSSIBILITY  OF  SUCH  DAMAGE.


### This is the script for clustering using tsfeature
library(dtw)
library(NbClust)
library(tsfeatures)
library(caret)
install.packages('e1071', dependencies=TRUE)  # Run it once forever
library(e1071)
cat("\014") 
Path='/Users/User_name/Documents/anamoly_imputation/'
All_CSV = list.files(path=Path,pattern = "*.csv")
### All_CSV has all the csv files
tsfeature_well=list()
cwd<- setwd(Path)
######################
flag=1 # Normalize
feat2=c(5)  # Only one feature
tsfeature_well=Data_Creation(All_CSV,feat2,flag,cwd) # Data_preparation function
corr_matrix=Correlation_matrix(tsfeature_well)  # Caluclate the correlation matrix


features=c(acf_features,entropy,lumpiness, stability,crossing_points,holt_parameters,flat_spots,motiftwo_entro3,pacf_features,std1st_der,histogram_mode)  # Original

feats_len=c(6,1,1,1,1,2,1,1,3,1,1)  # Original
tsfeatures_matrix<-function(data,num_measure,feats_len,features)
{
  Feature_matrix=matrix(data = NA, nrow = length(data), ncol = sum(feats_len)*num_measure)  
  if(num_measure==1)
  {
    mm=0
    iterative_fun <- function(data, fun) {
      lapply(fun, function(x) lapply(data, x))
    }
    for (ii  in 1: length(features)) 
      {
      A<-iterative_fun(data, features[ii])
      B<-lapply(A[[1]], function(x) {replace(x,is.nan(x),0)})
      B<-lapply(A[[1]], function(x) {replace(x,is.na(x),0)})
      for (jj  in 1:length(data)) 
        {
        Feature_matrix[jj,(1+mm):(feats_len[ii]+mm)]=unlist(B[[jj]])
        }
    
    # A<-iterative_fun(tsfeature_well, c(acf_features))
    
    mm=mm+feats_len[ii]
    }
    return(Feature_matrix)
  }
  
  else
  {
    mm=0
    iterative_fun <- function(data, fun) {
      lapply(fun, function(x) sapply(data, function(x1) {apply(x1,2, x)}))
    }
    for (ii  in 1: length(features)) 
    {
      A<-iterative_fun(data, features[ii])
      B<-apply(A[[1]],2, function(x) {replace(x,is.nan(x),0)})
      B<-apply(A[[1]],2, function(x) {replace(x,is.na(x),0)})
      for (jj  in 1:length(data)) 
      {
        Feature_matrix[jj,(1+mm):(feats_len[ii]*num_measure+mm)]=B[,jj]
      }
      
      # A<-iterative_fun(tsfeature_well, c(acf_features))
      
      mm=mm+feats_len[ii]*num_measure
    }
    return(Feature_matrix)
  }
  
}


# Feat_matrix<-tsfeatures_matrix(tsfeature_well,1,feats_len,features)  # for one feature

#################### 
### Optimal Number of Clusters using CVI
# Feat_matrix_normalize=scale(Feat_matrix)
# MIN=2
# MAX=8
# Method= 'complete'
# Index="alllong"
# Nb<- Cluster_Validity_index(Feat_matrix_normalize2,"euclidean",MIN,MAX,Method,Index) 
###############################################

feat1=c(5)  # only one feature
flag_cvi<-0# If this is zero we use predetermined number of clusters
K<-2
flag_feat_builtin<-1   # Using Rob hydman Function. Only true for univariate time series
flag1<-1  # for normalization
flag_pca<-0  # with PCA
n_pc<-10 # if flag PCA is on
cumulative_feat_PCA<-Cluster_Formation(All_CSV,features,feat1,feats_len,flag1,flag_cvi,flag_pca,flag_feat_builtin,n_pc,K,cwd)
flag_pca<-1
flag_cvi<-0
cumulative_feat_PCA<-Cluster_Formation(All_CSV,features,feat1,feats_len,flag1,flag_cvi,flag_pca,flag_feat_builtin,n_pc,K,cwd)

