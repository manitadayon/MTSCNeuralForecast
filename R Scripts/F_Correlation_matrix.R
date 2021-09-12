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

Correlation_matrix<-function(Data)
{
## This is the function to calculate the correlation matrix namely correlation
## between every two features so if data has 8 columns this will be 8 by 8 columns
## You can change the correlation method to whatever method you like.
  COL<- ncol(Data[[1]])
  SUM=0
  for (kk  in 1:length(Data)) 
  {
    cor_matrix=matrix(data = NA, nrow=COL, ncol = COL)
    for (ii in  1:COL) 
    {
      for (jj in 1:COL) 
       {
         cor_matrix[ii,jj]=cor(diff(Data[[kk]][,ii]),diff(Data[[kk]][,jj]),method = 'spearman')
         
       }
    print(ii)
    }
    cor_matrix <- as.data.frame(cor_matrix)
    cor_matrix[is.na(cor_matrix)] <- 0
    SUM=SUM+cor_matrix
    SUM_Cor_matrix=SUM
  }
  avg_matrix=SUM_Cor_matrix/(length(Data))
  return(avg_matrix)
}
