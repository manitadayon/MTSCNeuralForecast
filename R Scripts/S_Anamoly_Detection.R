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

### Anamoly and outlier detection
install.packages("devtools")
devtools::install_github("twitter/AnomalyDetection")
library(AnomalyDetection)
install.packages("forecast")
library(forecast)
install.packages('ggplot2')
library(ggplot2)
################################
All_CSV = list.files(path="/Users/User_name/CSV_file",pattern = "*.csv")   ## Reading all the CSV files.
#################################
Dataset_with_NaN<-lapply(All_CSV, function(x) {read.csv(x,header=TRUE,sep=",")})
Dataset_with_NaN=lapply(Dataset_with_NaN, function(x) {x[-1] <- lapply(x[-1], as.numeric); x[-1]})
Dataset_with_NaN=lapply(Dataset_with_NaN, function(x) {data.matrix(x, rownames.force = NA)})
######################################
#### tsoutliers for one list
onelist_outlier<-function(List)
{
  outlier_value<-apply(List, 2, function(x1) {tsoutliers(x1,iterate = 10)})
  
}

#########################################
outlier_detect<-function(data,flag)
{
  if(flag==1)  # Impute the missing value by interpolation and change the outlier
  {
    Dataset_new<-lapply(data, function(x) {apply(x, 2, function(x1) {tsclean(x1, replace.missing = TRUE, lambda = NULL)})})
    
  }
  else if(flag==2) # Just replace the outliers
  {
    # outlier_value<-lapply(data, function(x) {apply(x, 2, function(x1) {tsoutliers(x1,iterate = 10)})})
    for (ii in 1:length(data)) {
      outlier=onelist_outlier(data[[ii]])
      data[[ii]][outlier$Casing$index,1]=outlier$Casing$replacement
      data[[ii]][outlier$Tubing$index,2]=outlier$Tubing$replacement
      data[[ii]][outlier$Line$index,3]=outlier$Line$replacement
      data[[ii]][outlier$Hours$index,4]=outlier$Hours$replacement
      data[[ii]][outlier$Gas$index,5]=outlier$Gas$replacement
      data[[ii]][outlier$Water$index,6]=outlier$Water$replacement
      data[[ii]][outlier$oil$index,7]=outlier$oil$replacement
      data[[ii]][outlier$Choke$index,8]=outlier$Choke$replacement
    }
    return(data)
  }
}
##############3 outlier handling and imputing missing values
flag_anamoly=1
Dataset_new<-outlier_detect(Dataset,flag_anamoly)
loc3<- 'C:\Users\User_name\Desktop'
save_to_file(Dataset_new,All_CSV,loc3)
###############  outlier handling and missing value handling by another function
flag_anamoly2=2
Dataset_new2<-outlier_detect(Dataset,flag_anamoly2)
loc4<- 'C:\Users\User_name\Desktop'
save_to_file(Dataset_new2,All_CSV,loc4)


save_loc<-'C:\Users\User_name\Desktop'   ### Location to save the files.

save_plot(Dataset,Dataset_new,All_CSV,save_loc,c(1,2,3,4,5,6,7,8),0)  # outlier and missing value for all measurements

####### Plotting gas only
save_loc<-'C:\Users\User_name\Desktop' 

save_plot(Dataset,Dataset_new,All_CSV,save_loc,c(5),1)  # outlier and missing value for all measurements






