
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


# SCM 
# install.packages('tidysynth')
library('tidysynth')
deal_data <- function(data_file, iFile){
  Temp_data = read.csv(file = paste0(data_file, "/Crypto_",iFile,"_20240108.csv"), header = TRUE)%>% data.frame()
  Temp_data$date = as.Date(Temp_data$date, format="%Y-%m-%d")
  Temp_data = Temp_data[Temp_data$date %in% stock_RV$date,]
  Temp_data[,-1] = sapply(Temp_data[,-1], as.numeric)
  Temp_data[sapply(Temp_data, is.numeric)] <- lapply(Temp_data[sapply(Temp_data, is.numeric)], function(x) replace(x, is.na(x), 0))
  
  return(Temp_data)
}
# tidysynth
stock_mktcap = read.csv(file = paste0(data_file, "/Crypto_Mktcap_20240108.csv"), header = TRUE)%>% data.frame()
stock_mktcap$date = as.Date(stock_mktcap$date, format="%Y-%m-%d")


# 
# stock_RV <- read_excel(paste0(data_file, "/Realized_volatility.xlsx")) %>% data.frame()
# stock_RV$date = as.Date(stock_RV$date, format="%Y-%m-%d")


# "ETH"
# "BTC" "XRP"  "BCH"  "BSV"  "LTC"  "XMR"  "DASH" "ETC"  "ZEC"  "DOGE"

# Main test: ETH, ETC
# past
# select = c("ETH", "BTC", "BCH","BSV","LTC", "XMR", "ZEC")
select = c("ETH", "BTC", "BCH","BSV","LTC", "XMR", "ZEC","XRP","DASH")

mute = "ETH"

# Placebo ETC
# select = c("BTC", "BCH","BSV","LTC", "XMR", "ZEC")
# 
# mute = "ZEC"
# start = as.Date("2021-04-01")
# end = as.Date("2022-10-01")
# stock_mktcap = stock_mktcap[,c("date",select)]
# stock_RV = stock_RV[,c("date",paste0(select,'_RV'))]

Compare = c('daily','weekly')
# Compare = c('return_daily','return_weekly','return_monthly')
Event_list = as.Date(c("2015-09-07","2016-03-14","2016-07-20","2016-10-18","2016-11-22","2017-10-16","2019-02-28",'2019-12-08',"2020-01-02",
                       "2020-10-14","2020-12-01","2021-04-15", "2021-08-06","2021-09-27","2021-10-27","2021-12-09",'2022-06-30','2022-09-06','2022-09-15','2023-04-12'))
# Event_list = as.Date('2019-12-08')
events_df <- data.frame(date = Event_list)

OutSam_Month = 2/3
folder = paste0(OutSam_Month*30,'days')

dir.create(paste0('Figure_RV/',folder))


for (iFre in Compare){
  setwd(wdir)
  if (iFre == 'daily'){
    stock_RV = readRDS('Result/RV_daily.Rds')
    stock_RV <- stock_RV[,c("date",paste0(select,"_RV_d"))]
    RV_col = "_RV_d"
  }else if(iFre == 'weekly'){
    stock_RV = readRDS('Result/RV_weekly.Rds')
    stock_RV <- stock_RV[,c("date",paste0(select,"_RV_w"))]
    RV_col = "_RV_w"
  }else if (iFre == 'monthly'){
    stock_RV = readRDS('Result/RV_monthly.Rds')
    stock_RV <- stock_RV[,c("date",paste0(select,"_RV_m"))]
    RV_col = "_RV_m"
  }else if (iFre == 'return_daily'){
    stock_RV = readRDS('Result/return_daily.Rds')
    RV_col = ""
  }else if (iFre == 'return_weekly'){
    stock_RV = readRDS('Result/return_weekly.Rds')
    RV_col = "_w"
  }else{
    stock_RV = readRDS('Result/return_monthly.Rds')
    RV_col = "_m"
  }
  
  # loc = which(stock_RV$date == '2020-01-01')
  loc = which(stock_RV$date == '2015-01-01')
  stock_RV = stock_RV[loc:nrow(stock_RV),] 
  
  stock_RV$date = as.Date(stock_RV$date, format="%Y-%m-%d")
  
  other_character = c("blocksize","blocktime","miningdifi","hashrate","transaction","transactionfee","transactionvalue")
  for (iFile in other_character){
    assign(iFile, deal_data(data_file, iFile))
  }
  stock_mktcap = stock_mktcap[stock_mktcap$date %in% stock_RV$date,]
  stock_mktcap[,-1] = sapply(stock_mktcap[,-1], as.numeric)
  stock_mktcap[sapply(stock_mktcap, is.numeric)] <- lapply(stock_mktcap[sapply(stock_mktcap, is.numeric)], function(x) replace(x, is.na(x), 0))
  
  
  for (Coin in select){
    Temp = data.frame(date = stock_RV$date, RV = stock_RV[,paste0(Coin,RV_col)],
                      Mktcap = stock_mktcap[,Coin],
                      blocksize = blocksize[,Coin],
                      blocktime = blocktime[,Coin],
                      miningdifi = miningdifi[,Coin],
                      hashrate = hashrate[,Coin],
                      transaction = transaction[,Coin],
                      transactionfee = transactionfee[,Coin],
                      transactionvalue = transactionvalue[,Coin],
                      Coin = Coin)
    
    if (Coin == select[1]){
      Panel_Coin = Temp
      
    }else{
      Panel_Coin = rbind(Panel_Coin, Temp)
    }
    
  }
  Panel_Coin$date_num = as.numeric(Panel_Coin$date)
  Panel_Coin$lnMktcap = log(Panel_Coin$Mktcap+1)
  
  log_colum = c("blocksize","miningdifi","hashrate","transaction")
  for (i in log_colum){
    Panel_Coin[,paste0("ln",i)] = log(Panel_Coin[,i]+1)
  }
  
  for (i in c(1:length(Event_list))){
    iDate = Event_list[i]
    
    date_num_loc = unique(Panel_Coin$date_num[Panel_Coin$date == iDate])
    # loc = which(Panel_Coin$date == iDate)
    loc_start = date_num_loc - 30*2
    loc_end = date_num_loc+30*OutSam_Month
    
    Panel_Coin_put = Panel_Coin[Panel_Coin$date_num>=loc_start & Panel_Coin$date_num<=loc_end,]
    # Panel_Coin_put = Panel_Coin_put[!(Panel_Coin_put$Coin %in% c("ETH")),]
    
    # Panel_Coin_put = Panel_Coin_put[!is.na(Panel_Coin_put$RV) & !is.na(Panel_Coin_put$lnMktcap),] 
    
    Panel_Coin_put$RV[is.na(Panel_Coin_put$RV)] = 0
    Panel_Coin_put$lnMktcap[is.na(Panel_Coin_put$lnMktcap)] = 0
    
   
    Panel_Coin_put = Panel_Coin_put[,-1]
    
    Coin_out <-
      
      Panel_Coin_put %>%
      
      # initial the synthetic control object
      synthetic_control(outcome = RV, # outcome
                        unit = Coin, # unit index in the panel data
                        time = date_num, # time index in the panel data
                        i_unit = mute, # unit where the intervention occurred
                        i_time = date_num_loc, # time period when the intervention occurred
                        generate_placebos=T # generate placebo synthetic controls (for inference)
      ) %>%
      
      # Generate the aggregate predictors used to fit the weights
      
      # average log income, retail price of cigarettes, and proportion of the
      # population between 15 and 24 years of age from 1980 - 1988
      # c("blocksize","miningdifi","hashrate","transaction")
      # c("blocksize","blocktime","miningdifi","hashrate","transaction","transactionfee","transactionvalue")
      generate_predictor(time_window = min(Panel_Coin_put$date_num):(date_num_loc-1),
                         ln_Mktcap = mean(lnMktcap, na.rm = T),
                         ln_blocksize = mean(lnblocksize, na.rm = T),
                         ln_miningdifi = mean(miningdifi, na.rm = T),
                         ln_hashrate = mean(hashrate, na.rm = T),
                         ln_transaction = mean(transaction, na.rm = T),
                         transactionvalue = mean(transactionvalue, na.rm = T),
                         transactionfee = mean(transactionfee, na.rm = T),
                         blocktime =  mean(blocktime, na.rm = T)
      ) %>%
      
      
      # Generate the fitted weights for the synthetic control
      generate_weights(optimization_window = min(Panel_Coin_put$date_num):date_num_loc, # time to use in the optimization task
                       margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6 # optimizer options
      ) %>%
      
      # Generate the synthetic control
      generate_control()
    
    unique_date = unique(Panel_Coin[,c("date","date_num")])
    Plot = Coin_out[[6]][[2]]
    Plot$date = unique_date$date[unique_date$date_num%in% Plot$time_unit]
    Plot$diff = Plot$real_y-Plot$synth_y
    
    loc_cal = which(Plot$date == iDate) 
    mean_diff = round(mean(Plot$diff[loc_cal:nrow(Plot)]),3)
    
    setwd(paste0(wdir,'/SCM_Figure'))
    
    if (str_find(iFre, 'return') == 1){
      # png(paste0("Figure_return/",iFre,"_Real_SCM_",mute,"_",iDate,".png"), width = 900, height = 600)
      save_name = paste0("Figure_return/",folder,'/',iFre,"_Real_SCM_",mute,"_",iDate,"_",OutSam_Month,".png")
    }else{
      # png(paste0("Figure_RV/",iFre,"_Real_SCM_",mute,"_",iDate,".png"), width = 900, height = 600)
      save_name = paste0('Figure_RV/',folder,'/',iFre,"_Real_SCM_",mute,"_",iDate,"_",OutSam_Month,".png")
    }
    
    plot <- ggplot(data = Plot, aes(x = date)) +
            geom_line(aes(y = real_y, color = "Real Y"), color = "blue") +
            geom_line(aes(y = synth_y, color = "Synth Y"), color = "red") +
            geom_vline(data = data.frame(date = iDate), aes(xintercept = date), linetype = "dotted", color = "blue") +
            labs(title = paste0('Treatment effect: ',mean_diff),x = "Date", y = "Value",
                 color = "Legend") +
            scale_x_date(date_labels = "%Y-%m-%d") + 
            theme_minimal()+theme(text = element_text(family = "Times New Roman"),
                                  panel.background = element_rect(fill = "transparent", color = NA),  # Make panel background transparent
                                  plot.background = element_rect(fill = "transparent", color = NA),  # Make plot background transparent
                                  panel.grid.major = element_blank(),  # Remove major grid lines
                                  panel.grid.minor = element_blank(),
                                  axis.text = element_text(size = 12),  # Set axis text size
                                  axis.title = element_text(size = 12),  # Set axis title size
                                  axis.line = element_line(color = "black"),
                                  axis.ticks = element_line(color = "black"),
                                  axis.ticks.length=unit(-0.25, "cm"))
    ggsave(save_name, plot = plot, bg = "transparent",width = 10, height = 6)
    
    # if (str_find(iFre, 'return') == 1){
    #   png(paste0("Figure_return/",iFre,"_Treatment_",mute,"_",iDate,".png"), width = 900, height = 600)
    #   
    # }else{
    #   png(paste0("Figure_RV/",iFre,"_Treatment_",mute,"_",iDate,".png"), width = 900, height = 600)
    #   
    # }
    # 
    # print(ggplot(data = Plot, aes(x = date)) +
    #         geom_line(aes(y = diff, color = "Real Y"), color = "blue") +
    #         geom_vline(data =  data.frame(date = iDate), aes(xintercept = date), linetype = "dotted", color = "blue") +
    #         labs(title = "Comparison of Real and Synthetic Y over Time",
    #              x = "Date", y = "Value",
    #              color = "Legend") +
    #         theme_minimal())
    # 
    # dev.off()
    
    sig = Coin_out %>% grab_significance()
    sig$Treatment = 0
    sig$Treatment[sig$unit_name == mute] = mean_diff 
    if (str_find(iFre, 'return') == 1){
      write.csv(sig, file = paste0("Figure_return/",folder,'/',iFre,"_Pvalue_",mute,"_",iDate,"_",OutSam_Month,'.csv'), row.names = FALSE)
      
    }else{
      write.csv(sig, file = paste0("Figure_RV/",folder,'/',iFre,"_Pvalue_",mute,"_",iDate,"_",OutSam_Month,'.csv'), row.names = FALSE)
      
    }
    
    # Coin_out %>% plot_placebos()
    # Coin_out %>% plot_weights()
    # Coin_out %>% plot_placebos(prune = FALSE)
    # Coin_out %>% plot_mspe_ratio()
    
    # date_plot = Plot[,c(1,4)]
    # Placebo = Coin_out %>% grab_synthetic_control(placebo = T)
    # Placebo = merge(Placebo, date_plot, all.x=TRUE)
    # Placebo$diff = Placebo$real_y-Placebo$synth_y
    # 
    # ggplot(Placebo, aes(x = date, y = diff, group = .id)) + 
    #   geom_line(aes(color = ifelse(.id == "ETH", "red", "grey"))) + # 为线条指定颜色
    #   geom_point(aes(color = ifelse(.id == "ETH", "red", "grey"))) +  # 为点指定颜色
    #   labs(title = "Placebo result", x = "date", y = "diff") +
    #   scale_color_identity() + 
    #   theme_minimal()
    

  }

  
}


# Coin_out %>% grab_balance_table()
# Coin_out %>% plot_trends()
# Coin_out %>% plot_differences()
# Coin_out %>% grab_significance()


# 
# 
# Github code
# 
# require(tidysynth)
# data("smoking")
# smoking %>% dplyr::glimpse()
# 
# smoking_out <-
# 
#   smoking %>%
# 
#   # initial the synthetic control object
#   synthetic_control(outcome = cigsale, # outcome
#                     unit = state, # unit index in the panel data
#                     time = year, # time index in the panel data
#                     i_unit = "California", # unit where the intervention occurred
#                     i_time = 1988, # time period when the intervention occurred
#                     generate_placebos=T # generate placebo synthetic controls (for inference)
#   ) %>%
# 
#   # Generate the aggregate predictors used to fit the weights
# 
#   # average log income, retail price of cigarettes, and proportion of the
#   # population between 15 and 24 years of age from 1980 - 1988
#   generate_predictor(time_window = 1980:1988,
#                      ln_income = mean(lnincome, na.rm = T),
#                      ret_price = mean(retprice, na.rm = T),
#                      youth = mean(age15to24, na.rm = T)) %>%
# 
#   # average beer consumption in the donor pool from 1984 - 1988
#   generate_predictor(time_window = 1984:1988,
#                      beer_sales = mean(beer, na.rm = T)) %>%
# 
#   # Lagged cigarette sales
#   generate_predictor(time_window = 1975,
#                      cigsale_1975 = cigsale) %>%
#   generate_predictor(time_window = 1980,
#                      cigsale_1980 = cigsale) %>%
#   generate_predictor(time_window = 1988,
#                      cigsale_1988 = cigsale) %>%
# 
# 
#   # Generate the fitted weights for the synthetic control
#   generate_weights(optimization_window = 1970:1988, # time to use in the optimization task
#                    margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6 # optimizer options
#   ) %>%
# 
#   # Generate the synthetic control
#   generate_control()
# 
# smoking_out %>% plot_trends()
# smoking_out %>% plot_differences()
# smoking_out %>% plot_placebos()
