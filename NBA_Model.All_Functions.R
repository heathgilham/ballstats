#
# Heath Gilham
# NBA functions
#

Season_of_date = function(date){
  if(length(date)==1){
    
    if(class(date)=="numeric"& date > SOD_numeric_dates[1] & date < SOD_numeric_dates[length(SOD_numeric_dates)]){
      return(SOD_season[which(cumsum(date<SOD_numeric_dates)==1)-1])
      
    }else if (class(date)=="character"& date>SOD_dates[1] & date<SOD_dates[length(SOD_dates)]){
      return(SOD_season[which(cumsum(date<SOD_dates)==1)-1])
      
      
    }else if (class(date)=="Date"& date>SOD_dates[1] & date<SOD_dates[length(SOD_dates)]){
      date = as.numeric(as.Date(date))
      return(SOD_season[which(cumsum(date<SOD_numeric_dates)==1)-1])
      
    }else{
      return("Input Class Error or date out of range")
    }
    
    
    
  }else{
    df = data.frame(date = date,season = 0)
    
    if(class(date)=="numeric" & length(date[date > SOD_numeric_dates[1] & date < SOD_numeric_dates[length(SOD_numeric_dates)]])){
      for(i in 1:length(date)){
        df$season[i] = SOD_season[which(cumsum(date[i]<SOD_numeric_dates)==1)-1]
      }
      return(df$season)
      
    }else if (class(date)=="character"& length(date[date > SOD_dates[1] & date < SOD_dates[length(SOD_dates)]])){
      for(i in 1:length(date)){
        df$season[i] = SOD_season[which(cumsum(date[i]<SOD_dates)==1)-1]
      }
      return(df$season)
      
      #return(SOD_season[which(cumsum(date<SOD_dates)==1)-1])
      
      
    }else if (class(date)=="Date"& length(date[date > SOD_dates[1] & date < SOD_dates[length(SOD_dates)]])){
      #date = as.numeric(as.Date(date))
      for(i in 1:length(date)){
        df$season[i] = SOD_season[which(cumsum(date[i]<SOD_dates)==1)-1]
      }
      return(df$season)
      
      #return(SOD_season[which(cumsum(date<SOD_numeric_dates)==1)-1])
      
    }else{
      return("Input Class Error or date out of range")
    }
    
  }
  
} # Issues

# Average of last K games - ISSUE with multiple dates, teams
kaverage = function(dataset, team,date,k=NULL){
  
  season = Season_of_date(date)
  
  Teams1 = dataset
  Teams1$Date = as.numeric(as.Date(Teams1$Date))
  
  if(class(date)=="character"){
    date = as.numeric(as.Date(date))
  }
  
  if(length(date)==1){
    teamgames = subset(Teams1,Season == season & Team == team & Date >  date)
    
    if(is.null(k)){
      return(colMeans(teamgames[rel_cols]))
    }else if(k>nrow(teamgames)){
      print(paste0("Not enough entries for ",k," game average!"))
      return(rep(0,length(rel_cols)))
    }else{
      teamgames = teamgames[(nrow(teamgames)-k+1):nrow(teamgames),]
      return(colMeans(teamgames[rel_cols]))
    }
  }else{
    df = data.frame()
    
    for(i in 1:length(date)){
      teamgames = subset(Teams1,Season == season[i] & Team == team & Date > date[i])
      
      if(is.null(k)){
        df = rbind(df,colMeans(teamgames[rel_cols]))
      }else if(k>nrow(teamgames)){
        df = rbind(df, rep(0,length(rel_cols)))
        print(paste0("Not enough entries for ",k," game average!"))
      }else{
        df = rbind(df,colMeans(teamgames[(nrow(teamgames)-k+1):nrow(teamgames),rel_cols]))
      }
      
    }
    names(df) = names(Teams1[rel_cols])
    return(df)
    
  }
}

ladder = function(dataset, date){
  if(class(date)!='numeric'){
    date = as.numeric(as.Date(date))
  }
  
  Wins = c()
  Losses = c()
  WLR = c()
  statistics = data.frame()
  teamlist = unique(dataset$Team)
  season = Season_of_date(date)
  rel_cols = c(7,8,10,11,13,14:16,18:29,31:36,40)
  Teams1 = dataset
  Teams1$Date = as.numeric(as.Date(Teams1$Date))
  
  for (team in teamlist){
    teamgames = subset(Teams1,Season == season & Team == team & Date <  date)
    Wins = c(Wins,sum(teamgames$WBin))
    Losses = c(Losses,(nrow(teamgames) - sum(teamgames$WBin)))
    WLR = c(WLR,sum(teamgames$WBin)/nrow(teamgames))
    
    statistics = rbind(statistics,colMeans(teamgames[rel_cols]))
  }
  lad = cbind(teamlist,Wins,Losses,WLR,statistics)
  names(lad) = c("Team",names(lad)[2:4],names(teamgames)[rel_cols])
  
  lad = lad[order(Wins, decreasing = TRUE),]
  lad$LP = 1:30
  return(lad)
}

HAaverage = function(team,date,HOA){
  if(class(date)=="character" ){
    date = as.numeric(as.Date(date))
  }
  season = Season_of_date(date)
  Teams1 = Teams
  Teams1$Date = as.numeric(as.Date(Teams1$Date))
  teamgames = subset(Teams,Season == season & Team == team & Date >  date & HA == HA)
  return(colMeans(teamgames[rel_cols]))
  
}


SOD_Testing = function(){
  results = c()
  
  results = c(results, Season_of_date("0004-05-01") == "Input Class Error or date out of range")
  results = c(results, Season_of_date("0014-05-01") == 13.14)
  results = c(results, Season_of_date("0014-08-02") == 14.15)
  
  results = c(results, Season_of_date(as.numeric(as.Date("0004-05-01"))) == "Input Class Error or date out of range")
  results = c(results, Season_of_date(as.numeric(as.Date("0014-05-01"))) == 13.14)
  results = c(results, Season_of_date(as.numeric(as.Date("0014-08-02"))) == 14.15)
  
  results = c(results, Season_of_date(as.Date("0004-05-01")) == "Input Class Error or date out of range")
  results = c(results, Season_of_date(as.Date("0014-05-01")) == 13.14)
  results = c(results, Season_of_date(as.Date("0014-08-02")) == 14.15)
  
  results = c(results, Season_of_date(c("0004-05-01","0004-05-01")) == "Input Class Error or date out of range")
  results = c(results, Season_of_date(c("0014-05-01","0014-05-01")) == c(13.14,13.14))
  results = c(results, Season_of_date(c("0014-08-02","0014-08-02")) == c(14.15,14.15)) 
  
  results = c(results, Season_of_date(as.numeric(as.Date(c("0004-05-01","0004-05-01")))) == "Input Class Error or date out of range") 
  results = c(results, Season_of_date(as.numeric(as.Date(c("0014-05-01","0014-05-01")))) == c(13.14,13.14))
  results = c(results, Season_of_date(as.numeric(as.Date(c("0014-08-02","0014-08-02")))) == c(14.15,14.15))
  
  results = c(results, Season_of_date(as.Date(c("0004-05-01","0004-05-01"))) == "Input Class Error or date out of range")
  results = c(results, Season_of_date(as.Date(c("0014-05-01","0014-05-01"))) == c(13.14,13.14))
  results = c(results, Season_of_date(as.Date(c("0014-08-02","0014-08-02"))) == c(14.15,14.15))


  if(FALSE %in% results){
    return(FALSE)
  }else{
    return(TRUE)
  }
  
  
  
}

KAV_Testing = function(){
  results = c()
  date1 = "0005-01-24"
  date2 = "0015-01-24"
  date3 = "0015-08-13"
  
  kav1 = kaverage(Teams, "NYK", date1)
  kav2 = kaverage(Teams, "NYK", date2,5)
  kav3 = kaverage(Teams, "NYK", date3,100)
  
  kav1 = kaverage("NYK",rep(date1,5))
  kav2 = kaverage("NYK",rep(date2,5),5)
  kav3 = kaverage("NYK",rep(date3,5),100)
  kav4 = kaverage("NYK",c(date2, date3),5)
  
  results = c(results, sum(is.nan(kaverage(Teams, "NYK", date1))))
}


HAV_Testing = function(){
  hav1 = HAaverage("NYK","0005-03-07","H")
  hav2 = HAaverage("NYK","0014-12-16","A")
  hav3 = HAaverage("NYK","0015-01-24","H")
  rbind(hav1, hav2, hav3)
}