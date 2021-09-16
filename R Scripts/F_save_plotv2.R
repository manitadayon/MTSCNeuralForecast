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

library(grid)
# install.packages("gridExtra")
library(gridExtra)
save_plotv2<-function(data_original,data_no_outlier,well_name,save_loc,col,flag,col_name)
{
  if(flag==0)
  {
    # plot and save all measurements.
    for (ii in 1:length(data_original)) {
      A=list()
      jpeg(paste(save_loc,gsub(".csv",'',well_name[ii]),".jpg"),width = 5500,height = 6000,res = 550)
      for (jj in 1:length(col)) {
        A[[jj]]<-autoplot(ts(data_original[[ii]][,jj]), series="Original") +
          autolayer(ts(data_no_outlier[[ii]][,jj]), series="No_outlier")+ggtitle(well_name[ii])+
          ylab(col_name[jj])+scale_colour_manual(
            values=c(`No_outlier`="red",`Original`="blue"))
        
        
      }
      print(A)
      
      do.call(grid.arrange,A)
      dev.off()
    }
  }
  else if(flag==1)  # Plots and save some measurements
  {
    for (ii in 1:length(data_original)) {
      A=list()
      m=1
      jpeg(paste(save_loc,gsub(".csv",'',well_name[ii]),".jpg"),width = 2000,height = 2500,res = 300)
      for (jj in col) {
        A[[m]]<-autoplot(ts(data_original[[ii]][,jj]), series="Original") +
          autolayer(ts(data_no_outlier[[ii]][,jj]), series="No_outlier")+ggtitle(well_name[ii])+
          ylab(col_name[jj])+scale_colour_manual(
            values=c(`No_outlier`="red",`Original`="blue"))+theme_bw(base_size = 14)
        
        m=m+1
      }
      print(A)
      
      do.call(grid.arrange,A)
      dev.off()
    }
    
    
    
    
    
    
  }
  
}
