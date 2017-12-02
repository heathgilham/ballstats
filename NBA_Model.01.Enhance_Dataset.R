#
# Heath Gilham
# NBA data preparation
#


# Libraries
require(readr)
require(data.table)
source("C:/Users/Heathyboy/OneDrive/Data/NBA Team Analysis/Functions.R")

# Import 
Teams <- fread("C:/Users/Heathyboy/OneDrive/Data/NBA Team Analysis/Data/Teams.csv")
#Teams <- read_csv("~/Dropbox/My R Projects/Data/Basketball Staistics/Teams.csv")
Teams$Date = as.character(Teams$Date)
Teams$Date = as.Date.character(Teams$Date, "%d-%m-%Y")
Teams$DateF = as.factor(Teams$Date)
names(Teams)[c(8:11,14,24:29,31:34)] = c("FGP","P3","P3A","P3P","FTP","TSP","EFGP","P3AR","FTR","ORBP","DRBP","ASTP","STLP","BLKP","TOVP")
Teams = subset(Teams,Date>"0010-07-01")
Teams$TeamNF = as.character(Teams$Team)
Teams$Team = as.factor(Teams$Team)


# W/L binary
Teams$WBin = 0
Teams$WBin[seq(1,nrow(Teams),2)] = ifelse(Teams$PTS[seq(1,nrow(Teams),2)] > Teams$PTS[seq(2,nrow(Teams),2)],1,0)
Teams$WBin[seq(2,nrow(Teams),2)] = ifelse(Teams$WBin[seq(1,nrow(Teams),2)]==1,0,1)

# Points Difference
Teams$Diff = 0
Teams$Diff[seq(1,nrow(Teams),2)] = Teams$PTS[seq(1,nrow(Teams),2)] - Teams$PTS[seq(2,nrow(Teams),2)]
Teams$Diff[seq(2,nrow(Teams),2)] = -Teams$Diff[seq(1,nrow(Teams),2)]


# Season, team names
Teams$Team = as.character(Teams$Team)
Teams$Season = 0

Teams$HT[Teams$HT=="NOH"] = "NOP"
Teams$HT[Teams$HT=="CHO"] = "CHA"
Teams$HT[Teams$HT=="NJN"] = "NYK"

Teams$AT[Teams$AT=="NOH"] = "NOP"
Teams$AT[Teams$AT=="CHO"] = "CHA"
Teams$AT[Teams$AT=="NJN"] = "NYK"

Teams$Team[Teams$Team=="NOH"] = "NOP"
Teams$Team[Teams$Team=="CHO"] = "CHA"
Teams$Team[Teams$Team=="NJN"] = "NYK"

Teams$Season = Season_of_date(Teams$Date)
Teams$Team = as.factor(Teams$Team)


#H/A binary
Teams$HA = 1
Teams$HA[seq(1,nrow(Teams),2)] = 0


k=5
date = 
rel_cols = c(7,8,10,11,13,14:16,18:29,31:36,40)
ladder = Teams[Date<date & Season == Season_of_date(date), .(sum(WBin), .N), by = .(Team) ]
  
kav = Teams[order(Team), tail(.SD, k), by = Team]
kav = kav[, lapply(.SD, mean), by = Team, .SDcols = rel_cols]
names(kav) = paste0("KAV_", k, "_", names(kav))
names(kav)[1] = "Team"
kav$Date = date

hav = Teams[HA == 1]
hav = hav[order(Team), tail(.SD, k), by = Team]
hav = hav[, lapply(.SD, mean), by = Team, .SDcols = rel_cols]
names(hav) = paste0("HAV_", k, "_", names(hav))
names(hav)[1] = "Team"
hav$Date = date

mer = merge(Teams, kav, all.x = TRUE, by = c("Date", "Team"))
mer2 = merge(mer, hav, all.x = TRUE, by = c("Date", "Team"))
