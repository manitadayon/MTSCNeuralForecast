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

Simulated_Data<-function(Num_col,Num_col_arima,Col_Name1,Col_Name2,diff_order,AR,MA,Var_divider,Time_length,low_arima,upp_arima,low_no_arima,upp_no_arima,num_zero,num_outlier,outlier_range)
{

  A <- as.matrix(ts(replicate(n = Num_col_arima, 
                    arima.sim(model = list(order = c(1, diff_order ,1),ar=AR,ma=MA), n = Time_length)), names = Col_Name1))
  SD<-apply(A,2,sd)
  A<-A+sapply(SD, function(x) {rnorm(Time_length+diff_order,0,x/Var_divider)})
  A<-apply(A, 2, function(x) {abs(x)})   # Change negative value to positive
  
  A<- apply(A,2,function(x1) {if(max(x1)==min(x1)) {rep(0,length(x1))} else {(x1-min(x1))/(max(x1)-min(x1))}})  # Normalize the columns
  A<-sweep(A,2,upp_arima,"*")
  A<-sweep(A,2,low_arima,"+")     # change the range to arbitarly range
  ###################################################
  ## Create different type of data than arima model
  Data=list()
  for (jj in 1:(Num_col-Num_col_arima))   # In order to go loop once 
    {
    N<-sample(1:(diff_order+Time_length),1)
    B<-c(rep(sample(low_no_arima:upp_no_arima,1),times=N),rep(sample(low_no_arima:upp_no_arima,1),times=(diff_order+Time_length-N)))
    Data=cbind(B,Data)
  }
  Data<-apply(Data, 2, as.numeric)
  colnames(Data)=Col_Name2
  A=cbind(Data,A)
  
  
  for (ii in 1:Num_col) {
    Loc=sample(Time_length,size = sample(num_zero,1))
    A[Loc,ii]<-0
  }
  for (jj in 1:Num_col) {
    outlier=sample(Time_length,size = sample(num_outlier,1))
    A[outlier,jj]<-sample(outlier_range,size = length(outlier))
  }
  return(A)
}
