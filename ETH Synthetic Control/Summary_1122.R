
rm(list = ls(all = TRUE))

libraries = c("extrafont","SHAPforxgboost","shapr","xgboost","readxl","iml","ggplot2", "data.table", "igraph","timeDate", "stringr", "graphics","magick", "scales", "tidyr", "zoo","xts", "foreach", "doParallel",
              "xgboost","shapr","randomForest", "rpart", "quantreg", "readxl","dplyr", "xlsx", "psych","qgraph", "gganimate","av",
              "gifski", "strex","matrixStats","tools","Hmisc","vars","aTSA","quantreg","rapport","sjmisc","haven","foreign","e1071")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

wdir = "/Users/ruting/Library/Mobile Documents/com~apple~CloudDocs/ETH chronicles"
setwd(wdir)

data_file = "data"

# Statistics
Sum_sta <- function(Data_stata, Bond_ind, City_ind){
  
  Tool_sum <- function(ind, Data){
    for (i_B in ind){
      Temp_out = data.frame(Var = i_B, obs = length(which(!is.na(Data[,i_B]))), 
                            Mean = mean(Data[,i_B], na.rm = TRUE),
                            std = sd(Data[,i_B], na.rm = TRUE),
                            Min = min(Data[,i_B], na.rm = TRUE),
                            Max = max(Data[,i_B], na.rm = TRUE),
                            Median = median(Data[,i_B], na.rm = TRUE),
                            P25 = quantile(Data[,i_B], 0.25, na.rm = TRUE),
                            P75 = quantile(Data[,i_B], 0.75, na.rm = TRUE))
      if (i_B == ind[1]){
        Bond_out = Temp_out
      }else{
        Bond_out = rbind(Bond_out, Temp_out)
      }
    }
    Bond_out[,c(3:ncol(Bond_out))] = round(Bond_out[,c(3:ncol(Bond_out))],digits = 3)
    return(Bond_out)
    
  }
  
  Bond_out <- Tool_sum(ind = Bond_ind, Data = Data_stata)
  
  Temp_city = unique(Data_stata[,c('code_city','date_begin_year',City_ind)])
  City_out <- Tool_sum(ind = City_ind, Data = Temp_city)
  
  Out = rbind(Bond_out, City_out)
  
  return(Out)
}

# daily return, daily RV, weekly RV
Sum_list = c("RV_daily","RV_weekly","return_daily")
select = "ETH"


for (iFre in Sum_list){
  setwd(wdir)
  
  if (iFre == 'RV_daily'){
    Data = readRDS(paste0('Result/',iFre,'.Rds'))
    Data <- Data[,c("date",paste0(select,"_RV_d"))]
  }else if(iFre == 'RV_weekly'){
    Data = readRDS(paste0('Result/',iFre,'.Rds'))
    Data <- Data[,c("date",paste0(select,"_RV_w"))]
  }else {
    Data = readRDS(paste0('Result/',iFre,'.Rds'))
    Data <- Data[,c("date",select)]
  }
  
  loc = which(Data$date == '2015-08-09')
  Data = Data[loc:nrow(Data),] 
  
  Temp_out = data.frame(Var = iFre, obs = length(which(!is.na(Data[,2]))), 
                        Mean = mean(Data[,2], na.rm = TRUE),
                        std = sd(Data[,2], na.rm = TRUE),
                        Min = min(Data[,2], na.rm = TRUE),
                        Max = max(Data[,2], na.rm = TRUE),
                        Median = median(Data[,2], na.rm = TRUE),
                        P25 = quantile(Data[2], 0.25, na.rm = TRUE),
                        P75 = quantile(Data[,2], 0.75, na.rm = TRUE))
  if (iFre == Sum_list[1]){
    Sum_out = Temp_out
  }else{
    Sum_out = rbind(Sum_out, Temp_out)
  }
}

write.xlsx(Sum_out,file = paste0(wdir, '/Result/Summary.xlsx'))



