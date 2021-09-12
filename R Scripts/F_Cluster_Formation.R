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

##### Function to do feature-based clustering plus the following additional parameters:
## flag1: Normalization: Normalizing featuees before clustering.
## flag2: Cluster Validity Indexes: Automatically finding the number of clusters.
## flag3: PCA: Dimensionality Reduction.
## flag4: tsfeature built in command only for univariate series. Automatic feature extraction
## Well_name: Name of the file
## n_pc: Number of principal components
## cwd: current working directory

## It is running the Data_Creation function.
Cluster_Formation <-function (well_name,feature,feat,feats_len,flag1,flag2,flag3,flag4,n_pc,K,cwd)
{
  if (flag3==0 && flag4==0)  # PCA is not applied and we are not using the built in tsfeature
  {
    if (flag1==1) # Normalize
    {
      A<-Data_Creation(well_name,feat,flag1,cwd)
      B<-tsfeatures_matrix(A,length(feat),feats_len,features)
      B=scale(B)
      B=B[ , apply(B, 2, function(x) !all(is.na(x)))]
      B[is.nan(B)]<-0
      if(flag2==1)  # cluster validity index: Automatically tell you the number of clusters
      {
        Nb<- Cluster_Validity_index(B,"euclidean",2,8,"complete","all")  
        num_cluster<-as.numeric(names(sort(table(Nb$Best.nc[1,]),decreasing=TRUE)[1]))
      }
      else 
      {
        num_cluster=K
      }
      E<-kmeans(B,num_cluster)$cluster
    return(E)
    }
  }
    else if (flag3==1 && flag4==0)# PCA is applied
    {
      if (flag1==1) # Normalize
      {
        A<-Data_Creation(well_name,feat,flag1,cwd)
        B<-tsfeatures_matrix(A,length(feat),feats_len,features)
        B=scale(B)
        B=B[ , apply(B, 2, function(x) !all(is.na(x)))]
        B[is.nan(B)]<-0
        C<- prcomp(B,scale. = FALSE)
        C<-C$x[,1:n_pc]
        if(flag2==1)
        {
          Nb<- Cluster_Validity_index(C,"euclidean",2,8,"complete","all")  
          num_cluster<-as.numeric(names(sort(table(Nb$Best.nc[1,]),decreasing=TRUE)[1]))
        }
        else
        {
          num_cluster=K
        }
        E<-kmeans(C,num_cluster)$cluster
        return(E)
      }
      
      
      
      
      
    }
  else if(flag4==1)   # Using tsfeature built in command by Rob Hyndman, remember this can only be used on univariate time series.
  {
    if (flag1==1) # Normalize
    {
      A<-Data_Creation(well_name,feat,flag1,cwd)
      B<-tsfeatures(A)
      B=scale(B)
      B=B[ , apply(B, 2, function(x) !all(is.na(x)))]
      B[is.nan(B)]<-0
      if(flag2==1)
      {
        Nb<- Cluster_Validity_index(B,"euclidean",2,8,"complete","all")  
        num_cluster<-as.numeric(names(sort(table(Nb$Best.nc[1,]),decreasing=TRUE)[1]))
      }
      else
      {
        num_cluster=K
      }
      E<-kmeans(B,num_cluster)$cluster
      return(E)
    }
    
    
    
    
  }
    
  
}

