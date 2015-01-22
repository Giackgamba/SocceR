library(RODBC)
library(dplyr)

## Scarica i dati da DB
conn <- odbcConnect('Soccer')
olanda <- sqlFetch(conn,'niederlande')
odbcCloseAll()

## Metodi di stima dei goal fatti e ricevuti da ciascuna squadra

## Dataset con medie di goal fatti e subiti
## SOLO ULTIMO ANNO
champ <- olanda %>% 
  filter(anno==2015) %>% 
  group_by(team,casa) %>% 
  summarize(goalFatti = mean(GF, na.rm = T), goalSubiti = mean(GS, na.rm = T))


## Media semplice dei goal fatti e ricevuti da ciascuna squadra
play_goal <- function(teamA, teamB)
{
  A <- champ[which(grepl(teamA, champ$team) & champ$casa==1),]
  B <- champ[which(grepl(teamB, champ$team) & champ$casa==2),]
  
  a <- (A$goalFatti + B$goalSubiti)/2 + (B$goalFatti + A$goalSubiti)/2
  return(a)
}


## Maggiore peso ai goal fatti da ciascuna squadra
play_goalFatti <- function(teamA, teamB)
{
  A <- champ[which(grepl(teamA, champ$team) & champ$casa==1),]
  B <- champ[which(grepl(teamB, champ$team) & champ$casa==2),]
  
  a <- ((9*A$goalFatti + B$goalSubiti)/10) + ((9*B$goalFatti + A$goalSubiti)/10)
  return(a)
}


## Maggiore peso ai goal subiti da ciascuna squadra
play_goalSubiti <- function(teamA, teamB)
{
  A <- champ[which(grepl(teamA, champ$team) & champ$casa==1),]
  B <- champ[which(grepl(teamB, champ$team) & champ$casa==2),]
  
  a <- ((A$goalFatti + 9*B$goalSubiti)/10) + ((B$goalFatti + 9*A$goalSubiti)/10)
  return(a)
}


## Conta solo goal fatti da ciascuna squadra
play_soloGoalFatti <- function(teamA, teamB)
{
  A <- champ[which(grepl(teamA, champ$team) & champ$casa==1),]
  B <- champ[which(grepl(teamB, champ$team) & champ$casa==2),]
  
  a <- (A$goalFatti + 0*B$goalSubiti) + (B$goalFatti + 0*A$goalSubiti)
  return(a)
}


## Conta solo goal ricevuti da ciascuna squadra
play_soloGoalSubiti <- function(teamA, teamB)
{
  A <- champ[which(grepl(teamA, champ$team) & champ$casa==1),]
  B <- champ[which(grepl(teamB, champ$team) & champ$casa==2),]
  
  a <- (0*A$goalFatti + B$goalSubiti) + (0*B$goalFatti + A$goalSubiti)
  return(a)
}


## Maggiore peso alla sqadra di casa
play_Casa <- function(teamA, teamB)
{
  A <- champ[which(grepl(teamA, team) & champ$casa==1),]
  B <- champ[which(grepl(teamB, team) & champ$casa==2),]
  
  a <- 3*(A$goalFatti + B$goalSubiti) + (B$goalFatti + A$goalSubiti)
  return(a)
}


#Maggiore peso alla squadra ospite
play_Ospiti <- function(teamA, teamB)
{
  A <- champ[which(grepl(teamA, team) & champ$casa==1),]
  B <- champ[which(grepl(teamB, team) & champ$casa==2),]
  
  a <- (A$goalFatti + B$goalSubiti) + 3*(B$goalFatti + A$goalSubiti)
  return(a)
}


## Controlla quante volte la previsione sbaglia
## TUTTI I 5 ANNI
checkRes_numerico <- function (data, partita) {
  res <- unlist(apply(X = data, MARGIN = 1, function(x) partita(x['team'], x['avversario'])))
  comp <- cbind(mutate(data, officiale = GF + GS), atteso = unlist(res))
  comp$U <- ifelse(comp$officiale>=2.5, 'O', 'U')
  comp$A <- ifelse(comp$atteso>=3, 'O', 'U')
  a <- nrow(filter(comp, A!=U))
  return(a)
}


## Dataset con categorizzazione della squadra in 4 'divisioni'
##            
##            ATTACCO 
##            \Forte\Debole\
##  DIFESA  __\_____\_____\
##      Forte \  A  \  B  \
##          __\_____\_____\
##      Debole\  C  \  D  \
##            \     \     \
##
rating <- champ %>% 
  mutate(gruppo = ifelse(goalFatti > mean(goalFatti) & goalSubiti <= mean(goalSubiti), 'A', 
                         ifelse(goalFatti > mean(goalFatti) & goalSubiti > mean(goalSubiti), 'B', 
                                ifelse(goalFatti <= mean(goalFatti) & goalSubiti <= mean(goalSubiti), 'C', 
                                       ifelse(goalFatti <= mean(goalFatti) & goalSubiti > mean(goalSubiti),'D','E')))))

## 
play_gruppi1 <- function(teamA, teamB) {
  A <- rating[which(grepl(teamA, rating$team)),'gruppo']
  B <- rating[which(grepl(teamB, rating$team)),'gruppo']
  
  res <- as.character(ifelse(A=='A' & B == 'A', 'O', ifelse(A=='A' && B=='B', 'O', ifelse(A=='A' & B=='C', 'U',ifelse( A=='A' & B=='D', 'O', ifelse( A =='B', 'O', ifelse( A=='C' & B=='A', 'U', ifelse(A=='C' & B=='B', 'O', ifelse(A=='C' & B=='C', 'U', ifelse(A=='C' & B=='D', 'U', ifelse(A=='D' & B=='A', 'O', ifelse(A=='D' & B=='B', 'O', ifelse(A=='D' & B== 'C','U', ifelse(A=='D' & B=='D', 'U'))))))))))))))
  
  return(res)
}


play_gruppi2 <- function(teamA, teamB) {
  A <- rating[which(grepl(teamA, rating$team)),'gruppo']
  B <- rating[which(grepl(teamB, rating$team)),'gruppo']
  res <- as.character(ifelse(A=='A' & B == 'A', 'U', ifelse(A=='A' && B=='B', 'O', ifelse(A=='A' & B=='C', 'O',ifelse( A=='A' & B=='D', 'U', ifelse( A =='B', 'O', ifelse( A=='C' & B=='A', 'O', ifelse(A=='C' & B=='B', 'O', ifelse(A=='C' & B=='C', 'U', ifelse(A=='C' & B=='D', 'O', ifelse(A=='D' & B=='A', 'U', ifelse(A=='D' & B=='B', 'O', ifelse(A=='D' & B== 'C', 'O', ifelse(A=='D' & B=='D', 'O'))))))))))))))
  return(res)
}


play_gruppi3 <- function(teamA, teamB) {
  A <- rating[which(grepl(teamA, rating$team) & rating$casa==1),'gruppo']
  B <- rating[which(grepl(teamB, rating$team) & rating$casa==2),'gruppo']
  res <- as.character(ifelse(A=='A' & B == 'A', 'U', ifelse(A=='A' && B=='B', 'O', ifelse(A=='A' & B=='C', 'O',ifelse( A=='A' & B=='D', 'U', ifelse( A =='B', 'O', ifelse( A=='C' & B=='A', 'O', ifelse(A=='C' & B=='B', 'O', ifelse(A=='C' & B=='C', 'U', ifelse(A=='C' & B=='D', 'O', ifelse(A=='D' & B=='A', 'U', ifelse(A=='D' & B=='B', 'O', ifelse(A=='D' & B== 'C', 'O', ifelse(A=='D' & B=='D', 'O'))))))))))))))
  return(res)
}


checkRes_fattori <- function (data, partita) {
  res <- unlist(apply(X = data, MARGIN = 1, function(x) partita(x['team'], x['avversario'])))
  comp <- cbind(mutate(data, officiale = GF + GS), atteso = unlist(res))
  comp$U <- ifelse(comp$officiale>=2.5, 'O', 'U')
  a <- nrow(filter(comp, atteso!=U))
  return(a)
}


prob <- olanda %>%
  group_by(team, casa, GS, GF) %>%
  summarize(freq = n()) %>%
  mutate(prob = freq/sum(freq))


play_probabil <- function(teamA,teamB){
  prob_ <- ungroup(prob)
  A <- filter(prob_, grepl(teamA, team), casa==1)
  B <- filter(prob_, grepl(teamB, team), casa==2)
  res <- full_join(A, B, by = c('GF' = 'GS', 'GS' = 'GF')) %>%
    filter(GF+GS<3) %>%
    transmute(GF, GS, prob = prob.x*prob.y) %>%
    summarize(probabilità = sum(prob, na.rm = T))
  
  return(res)
}

checkRes_probabil <- function (data, partita) {
  res <- unlist(apply(X = data, MARGIN = 1, function(x) partita(x['team'], x['avversario'])))
  comp <- cbind(mutate(data, officiale = GF + GS), atteso = unlist(res))
  comp$U <- ifelse(comp$officiale>=2.5, 'O', 'U')
  comp$A <- ifelse(comp$atteso>0.5, 'U', 'O')
  a <- nrow(filter(comp, A!=U))
  return(a)
}

ultimiRis <- function(teamA, teamB) {
  ris <- olanda %>%filter(grepl(teamA, team), grepl(teamB, avversario))
  return(ris)
}

