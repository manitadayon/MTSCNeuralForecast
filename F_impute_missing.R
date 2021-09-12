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

### This is the function to impute the missing values for the time series.
### It can be next observation carry backward (nocb)
### It can be last observation carry forward (locf)
### Interpolation (Linear), # Kalman Smoothing and many more
### This can be done using the imputeTS for each column of data.
install.packages('imputeTS')
library(imputeTS)
nan_to_zero<-function(data)
{
  if(is.list(data))
  {
  data<-lapply(data,function(x) {apply(x,2, function(x1) {if(all(is.na(x1))) {rep(0,length(x1))} else {x1}})})
  }
  else if(is.matrix(data) | is.data.frame(data))
  {
    data<-apply(data,2, function(x)  {if(all(is.na(x))) {rep(0,length(x))} else {x}})
  }
}
impute_missing<- function(data, method)
{
  if(is.list(data))
  {
    if(method=='linear')
    {
       data<-lapply(data,function(x) {apply(x,2, function(x1) {na_interpolation(x1)})})
    }
    else if(method=='spline')
    {
      data<-lapply(data,function(x) {apply(x,2, function(x1) {na_interpolation(x1,option = 'spline')})})
    }
    else if(method=='kalman')
    {
      data<-lapply(data,function(x) {apply(x,2, function(x1) {na_kalman(x1)})})
    }
    else if(method=='locf')
    {
      data<-lapply(data,function(x) {apply(x,2, function(x1) {na_locf(x1)})})
    }
    else if(method=='nocb')
    {
      data<-lapply(data,function(x) {apply(x,2, function(x1) {na_locf(x1,option = "nocb")})})
    }
    else if(method=='simple_average')
    {
      data<-lapply(data,function(x) {apply(x,2, function(x1) {na_ma(x1,weighting = "simple")})})
    }
    else if(method=='linear_average')
    {
      data<-lapply(data,function(x) {apply(x,2, function(x1) {na_ma(x1,weighting = "linear")})})
    }
    else if(method=='exponential')
    {
      data<-lapply(data,function(x) {apply(x,2, function(x1) {na_ma(x1,weighting = "exponential")})})
    }
    else if(method=='seasonal_remove')
    {
      data<-lapply(data,function(x) {apply(x,2, function(x1) {na_seasplit(x1)})})
    }
  }
  
  else if (is.matrix(data) | is.data.frame((data)))
  {
    if(method=='linear')
    {
      data<-apply(data,2, function(x1) {na_interpolation(x1)})
    }
    else if(method=='spline')
    {
      data<-apply(data,2, function(x1) {na_interpolation(x1,option = 'spline')})
    }
    else if(method=='kalman')
    {
      data<-apply(data,2, function(x1) {na_kalman(x1)})
    }
    else if(method=='locf')
    {
      data<-apply(data,2, function(x1) {na_locf(x1)})
    }
    else if(method=='nocb')
    {
      data<-apply(data,2, function(x1) {na_locf(x1,option = "nocb")})
    }
    else if(method=='simple_average')
    {
      data<-apply(data,2, function(x1) {na_ma(x1,weighting = "simple")})
    }
    else if(method=='linear_average')
    {
      data<-apply(data,2, function(x1) {na_ma(x1,weighting = "linear")})
    }
    else if(method=='exponential')
    {
      data<-apply(data,2, function(x1) {na_ma(x1,weighting = "exponential")})
    }
    else if(method=='seasonal_remove')
    {
      data<-apply(data,2, function(x1) {na_seasplit(x1)})
    }
  }
}

###########################################
Dataset_with_NaN<-lapply(All_CSV, function(x) {read.csv(x,header=TRUE,sep=",")})
Dataset_with_NaN=lapply(Dataset_with_NaN, function(x) {x[-1] <- lapply(x[-1], as.numeric); x[-1]})  ## Remove the first column: Feel free to ignore this.
Dataset_with_NaN=lapply(Dataset_with_NaN, function(x) {data.matrix(x, rownames.force = NA)})
######################################
## Lets make the column of all NaN to zeros
## Impute the NaN values.
Dataset<-nan_to_zero(Dataset_with_NaN)
Imputed_Dataset<-impute_missing(Dataset,"locf")  
#################################
loc2<- '/Users/User_name/Documents/Imputed_CSV/'
save_to_file(Imputed_Dataset,All_CSV,loc2)
  