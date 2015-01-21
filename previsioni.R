library(RODBC)

conn <- odbcConnect('Soccer')

olanda <- sqlFetch(conn,'niederlande')
# olanda$team <- as.character(olanda$team)
# olanda$GS <- as.numeric(olanda$GS)
# olanda$GF <- as.numeric(olanda$GF)
# olanda$anno <- as.factor(olanda$anno)
# olanda$casa <- as.factor(olanda$casa)
# 
# PSV <- filter(olanda, grepl('NAC', team))
# mediaGSP <- PSV %>% group_by(anno,casa) %>% summarize(goalFatti = mean(GF, na.rm = T), goalSubiti = mean(GS, na.rm = T))
# 
# Ajax <- filter(olanda, grepl('Dord', team))
# mediaGSA <- Ajax %>% group_by(anno,casa) %>% summarize(goalFatti = mean(GF, na.rm = T), goalSubiti = mean(GS, na.rm = T))

champ <- filter(olanda, anno==2015)
champ <- champ %>% group_by(team,casa) %>% summarize(goalFatti = mean(GF, na.rm = T), goalSubiti = mean(GS, na.rm = T))

partitaA <- function(teamA, teamB)
{
  A <- champ[which(grepl(teamA, champ$team) & champ$casa==1),]
  B <- champ[which(grepl(teamB, champ$team) & champ$casa==2),]
  
  a <- ((A$goalFatti + 9*B$goalSubiti)/10) + ((B$goalFatti + 9*A$goalSubiti)/10)
  return(a)
}

partitaB <- function(teamA, teamB)
{
  A <- champ[which(grepl(teamA, champ$team) & champ$casa==1),]
  B <- champ[which(grepl(teamB, champ$team) & champ$casa==2),]
  
  a <- ((9*A$goalFatti + B$goalSubiti)/10) + ((9*B$goalFatti + A$goalSubiti)/10)
  return(a)
}

partitaC <- function(teamA, teamB)
{
  A <- champ[which(grepl(teamA, champ$team) & champ$casa==1),]
  B <- champ[which(grepl(teamB, champ$team) & champ$casa==2),]
  
  a <- ((A$goalFatti + 0*B$goalSubiti)) + ((B$goalFatti + 0*A$goalSubiti))
  return(a)
}

partitaD <- function(teamA, teamB)
{
  A <- champ[which(grepl(teamA, champ$team) & champ$casa==1),]
  B <- champ[which(grepl(teamB, champ$team) & champ$casa==2),]
  
  a <- ((0*A$goalFatti + B$goalSubiti)) + ((0*B$goalFatti + A$goalSubiti))
  return(a)
}

partitaZ <- function(teamA, teamB)
{
  A <- champ[which(grepl(teamA, champ$team) & champ$casa==1),]
  B <- champ[which(grepl(teamB, champ$team) & champ$casa==2),]
  
  a <- (3*(A$goalFatti + B$goalSubiti)) + ((B$goalFatti + A$goalSubiti))
  return(a)
}
res <- apply(X = filter(olanda, casa==1), MARGIN = 1, function(x) partitaZ(x['team'], x['avversario']))

comp <- cbind(mutate(filter(olanda, casa==1), officiale = GF + GS), atteso = res)
comp$U <- ifelse(comp$officiale>=2.5, 'O', 'U')
comp$A <- ifelse(comp$atteso>=2.5, 'O', 'U')

nrow(filter(comp, A!=U))



rating <- rating %>% mutate(gruppo = ifelse(goalFatti > mean(goalFatti) & goalSubiti <= mean(goalSubiti), 'A', ifelse(goalFatti > mean(goalFatti) & goalSubiti > mean(goalSubiti), 'B', ifelse(goalFatti <= mean(goalFatti) & goalSubiti <= mean(goalSubiti), 'C', ifelse(goalFatti <= mean(goalFatti) & goalSubiti > mean(goalSubiti),'D','E')))))

partitaE <- function(teamA, teamB) {
  A <- rating[which(grepl(teamA, rating$team)),'gruppo']
  B <- rating[which(grepl(teamB, rating$team)),'gruppo']
  
  res <- as.character(ifelse(A=='A' & B == 'A', 'O', ifelse(A=='A' && B=='B', 'O', ifelse(A=='A' & B=='C', 'U',ifelse( A=='A' & B=='D', 'O', ifelse( A =='B', 'O', ifelse( A=='C' & B=='A', 'U', ifelse(A=='C' & B=='B', 'O', ifelse(A=='C' & B=='C', 'U', ifelse(A=='C' & B=='D', 'U', ifelse(A=='D' & B=='A', 'O', ifelse(A=='D' & B=='B', 'O', ifelse(A=='D' & B== 'C','U', ifelse(A=='D' & B=='D', 'U'))))))))))))))
  
  return(res)
}

partitaF <- function(teamA, teamB) {
A <- rating[which(grepl(teamA, rating$team)),'gruppo']
B <- rating[which(grepl(teamB, rating$team)),'gruppo']
res <- as.character(ifelse(A=='A' & B == 'A', 'U', ifelse(A=='A' && B=='B', 'O', ifelse(A=='A' & B=='C', 'O',ifelse( A=='A' & B=='D', 'U', ifelse( A =='B', 'O', ifelse( A=='C' & B=='A', 'O', ifelse(A=='C' & B=='B', 'O', ifelse(A=='C' & B=='C', 'U', ifelse(A=='C' & B=='D', 'O', ifelse(A=='D' & B=='A', 'U', ifelse(A=='D' & B=='B', 'O', ifelse(A=='D' & B== 'C', 'O', ifelse(A=='D' & B=='D', 'O'))))))))))))))
return(res)
}


partitaG <- function(teamA, teamB) {
  A <- rating[which(grepl(teamA, rating$team), casa==1),'gruppo']
  B <- rating[which(grepl(teamB, rating$team), casa==2),'gruppo']
  res <- as.character(ifelse(A=='A' & B == 'A', 'U', ifelse(A=='A' && B=='B', 'O', ifelse(A=='A' & B=='C', 'O',ifelse( A=='A' & B=='D', 'U', ifelse( A =='B', 'O', ifelse( A=='C' & B=='A', 'O', ifelse(A=='C' & B=='B', 'O', ifelse(A=='C' & B=='C', 'U', ifelse(A=='C' & B=='D', 'O', ifelse(A=='D' & B=='A', 'U', ifelse(A=='D' & B=='B', 'O', ifelse(A=='D' & B== 'C', 'O', ifelse(A=='D' & B=='D', 'O'))))))))))))))
  return(res)
}


res <- apply(X = filter(olanda, casa==1), MARGIN = 1, function(x) partitaG(x['team'], x['avversario']))

comp <- cbind(mutate(filter(olanda, casa==1), officiale = GF + GS), atteso = res)
comp$U <- ifelse(comp$officiale>=2.5, 'O', 'U')
