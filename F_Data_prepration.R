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


### Preparing data by choosing approprate features.Remove the first column (it is ID column)
### Feel free to remove this line if you do not want this: Data=lapply(Data, function(x) {x[-1] <- lapply(x[-1], as.numeric); x[-1]})

Data_Creation<-function(wells_name,feat,flag,cwd)
  # flag determines whether data should be normalized or not.
  # flag=0 not normalized
  # num_feat: It corresponds to column of dataframe..
  # wells_name: name of the wells, the string.
  # cwd: current working directory.
{
  ##### Read the training file 
  setwd(cwd)
  if (flag==0)
  {
    Data=lapply(wells_name, function(x) {read.csv(x,header=TRUE,sep=",")})
    Data=lapply(Data, function(x) {x[-1] <- lapply(x[-1], as.numeric); x[-1]})
    Data=lapply(Data, function(x) {data.matrix(x, rownames.force = NA)})
    Data=lapply(Data, function(x) {x[,feat]})
    
  }
  if (flag==1 & length(feat) > 1)
  {
    Data=lapply(wells_name, function(x) {read.csv(x,header=TRUE,sep=",")})
    Data=lapply(Data, function(x) {x[-1] <- lapply(x[-1], as.numeric); x[-1]})
    Data=lapply(Data, function(x) {data.matrix(x, rownames.force = NA)})
    Data=lapply(Data, function(x) {x[,feat]})
    Data<- lapply(Data, function(x) {apply(x,2,function(x1) {if(max(x1)==min(x1)) {rep(0,length(x1))} else {(x1-min(x1))/(max(x1)-min(x1))}})})
  }
  if(flag==1 & length(feat) ==1 )
  {
    Data=lapply(wells_name, function(x) {read.csv(x,header=TRUE,sep=",")})
    Data=lapply(Data, function(x) {x[-1] <- lapply(x[-1], as.numeric); x[-1]})
    Data=lapply(Data, function(x) {data.matrix(x, rownames.force = NA)})
    Data=lapply(Data, function(x) {x[,feat]})
    Data<- lapply(Data, function(x1) {if(max(x1)==min(x1)) {rep(0,length(x1))} else {(x1-min(x1))/(max(x1)-min(x1))}})

    
  }
  return(Data)
}
