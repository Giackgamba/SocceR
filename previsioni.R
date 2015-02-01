library(RODBC)
library(dplyr)
library(XML)

## Scarica i dati da DB
conn <- odbcConnect('Soccer')
olanda <- sqlFetch(conn,'niederlande')
odbcCloseAll()

## Metodi di stima dei goal fatti e ricevuti da ciascuna squadra

## Dataset con medie di goal fatti e subiti
## SOLO ULTIMO ANNO
champ <- olanda %>% 
  filter(anno == 2015) %>% 
  group_by(team,casa) %>% 
  summarize(goalFatti = mean(GF, na.rm = T), goalSubiti = mean(GS, na.rm = T), varFatti = var(GF, na.rm = T), varSubiti = var(GS, na.rm = T))


## Media semplice dei goal fatti e ricevuti da ciascuna squadra
play_goal <- function(teamA, teamB)
{
  A <- filter(champ, grepl(teamA, team), casa == 1)
  B <- filter(champ, grepl(teamB, team), casa == 2)
  
  a <- (A$goalFatti + B$goalSubiti) / 2 + (B$goalFatti + A$goalSubiti) / 2
  return(a)
}


## Maggiore peso ai goal fatti da ciascuna squadra
play_goalFatti <- function(teamA, teamB)
{
  A <- filter(champ, grepl(teamA, team), casa == 1)
  B <- filter(champ, grepl(teamB, team), casa == 2)
  
  a <- ((7 * A$goalFatti + 3 * B$goalSubiti)/10) + ((7 * B$goalFatti + 3 * A$goalSubiti)/10)
  return(a)
}


## Maggiore peso ai goal subiti da ciascuna squadra
play_goalSubiti <- function(teamA, teamB)
{
  A <- filter(champ, grepl(teamA, team), casa == 1)
  B <- filter(champ, grepl(teamB, team), casa == 2)
  
  a <- ((A$goalFatti + 9 * B$goalSubiti)/10) + ((B$goalFatti + 9 * A$goalSubiti)/10)
  return(a)
}


## Conta solo goal fatti da ciascuna squadra
play_soloGoalFatti <- function(teamA, teamB)
{
  A <- filter(champ, grepl(teamA, team), casa == 1)
  B <- filter(champ, grepl(teamB, team), casa == 2)
  
  a <- (A$goalFatti + 0*B$goalSubiti) + (B$goalFatti + 0*A$goalSubiti)
  return(a)
}


## Conta solo goal ricevuti da ciascuna squadra
play_soloGoalSubiti <- function(teamA, teamB)
{
  A <- filter(champ, grepl(teamA, team), casa == 1)
  B <- filter(champ, grepl(teamB, team), casa == 2)
  
  a <- (0*A$goalFatti + B$goalSubiti) + (0*B$goalFatti + A$goalSubiti)
  return(a)
}


## Maggiore peso alla sqadra di casa
play_Casa <- function(teamA, teamB)
{
  A <- filter(champ, grepl(teamA, team), casa == 1)
  B <- filter(champ, grepl(teamB, team), casa == 2)
  
  a <- 3*(A$goalFatti + B$goalSubiti) + (B$goalFatti + A$goalSubiti)
  return(a)
}


#Maggiore peso alla squadra ospite
play_Ospiti <- function(teamA, teamB)
{
  A <- filter(champ, grepl(teamA, team), casa == 1)
  B <- filter(champ, grepl(teamB, team), casa == 2)
  
  a <- (A$goalFatti + B$goalSubiti) + 3*(B$goalFatti + A$goalSubiti)
  return(a)
}


## Controlla quante volte la previsione sbaglia
## TUTTI I 5 ANNI
checkRes_numerico <- function (data, partita) {
  res <- unlist(apply(X = data, MARGIN = 1, function(x) partita(x['team'], x['avversario'])))
  comp <- cbind(mutate(data, officiale = GF + GS), atteso = unlist(res))
  comp$U <- ifelse(comp$officiale >= 3, 'O', 'U')
  comp$A <- ifelse(comp$atteso >= 3, 'O', 'U')
  a <- nrow(filter(comp, A != U))
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
rating <-olanda %>% 
  filter(anno == 2015) %>% 
  group_by(team, casa) %>% 
  summarize(goalFatti = mean(GF, na.rm = T), goalSubiti = mean(GS, na.rm = T)) %>%
  mutate(gruppo = ifelse(goalFatti > mean(goalFatti) & goalSubiti <= mean(goalSubiti), 'A', 
                         ifelse(goalFatti > mean(goalFatti) & goalSubiti > mean(goalSubiti), 'B', 
                                ifelse(goalFatti <= mean(goalFatti) & goalSubiti <= mean(goalSubiti), 'C', 
                                       ifelse(goalFatti <= mean(goalFatti) & goalSubiti > mean(goalSubiti),'D','E')))))

## 
play_gruppi1 <- function(teamA, teamB) {
  A <- select(filter(rating, grepl(teamA, team), casa == 1),gruppo)
  B <- select(filter(rating, grepl(teamB, team), casa == 2),gruppo)
  
  res <- as.character(ifelse(A =='A' & B == 'A', 'O', 
                             ifelse(A =='A' && B =='B', 'O', 
                                    ifelse(A =='A' & B =='C', 'U', 
                                           ifelse( A =='A' & B =='D', 'O', 
                                                   ifelse( A =='B', 'O', 
                                                           ifelse( A =='C' & B =='A', 'U', 
                                                                   ifelse(A =='C' & B =='B', 'O', 
                                                                          ifelse(A =='C' & B =='C', 'U', 
                                                                                 ifelse(A =='C' & B =='D', 'U', 
                                                                                        ifelse(A =='D' & B =='A', 'O', 
                                                                                               ifelse(A =='D' & B =='B', 'O', 
                                                                                                      ifelse(A =='D' & B == 'C','U', 
                                                                                                             ifelse(A =='D' & B =='D', 'U')
                                                                                                      )
                                                                                               )
                                                                                        )
                                                                                 )
                                                                          )
                                                                   )
                                                           )
                                                   )
                                           )
                                    )
                             )
  )
  )
  
  return(res)
}


play_gruppi2 <- function(teamA, teamB) {
  rating_ <- ungroup(rating)
  A <- select(filter(rating_, grepl(teamA, team), casa == 1), gruppo)
  B <- select(filter(rating_, grepl(teamB, team), casa == 2), gruppo)
  res <- as.character(ifelse(A =='A' & B == 'A', 'U',
                             ifelse(A =='A' && B =='B', 'O', 
                                    ifelse(A =='A' & B =='C', 'O',
                                           ifelse(A =='A' & B =='D', 'U', 
                                                  ifelse(A =='B', 'O', 
                                                         ifelse( A =='C' & B =='A', 'O', 
                                                                 ifelse(A =='C' & B =='B', 'O', 
                                                                        ifelse(A =='C' & B =='C', 'U', 
                                                                               ifelse(A =='C' & B =='D', 'O', 
                                                                                      ifelse(A =='D' & B =='A', 'U', 
                                                                                             ifelse(A =='D' & B =='B', 'O', 
                                                                                                    ifelse(A =='D' & B == 'C', 'O', 
                                                                                                           ifelse(A =='D' & B =='D', 'O', 'X')
                                                                                                    )
                                                                                             )
                                                                                      )
                                                                               )
                                                                        )
                                                                 )
                                                         )
                                                  )
                                           )
                                    )
                             )
  )
  )
  return(res)
}


checkRes_fattori <- function (data, partita) {
  res <- unlist(apply(X = data, MARGIN = 1, function(x) partita(x['team'], x['avversario'])))
  comp <- cbind(mutate(data, officiale = GF + GS), atteso = unlist(res))
  comp$U <- ifelse(comp$officiale >= 2.5, 'O', 'U')
  a <- nrow(filter(comp, atteso != U))
  return(a)
}


prob <- olanda %>%
  group_by(team, casa, GF, GS) %>%
  summarize(freq = n()) %>%
  ungroup() %>%
  group_by(team, casa) %>%
  mutate(prob = freq  /sum(freq))


play_probabil <- function(teamA, teamB){
  prob_ <- ungroup(prob)
  A <- filter(prob_, grepl(teamA, team), casa == 1)
  B <- filter(prob_, grepl(teamB, team), casa == 2)
  res <- full_join(A, B, by = c('GF' = 'GS', 'GS' = 'GF')) %>%
    #filter(GF + GS < 3) %>%
    transmute(GF, GS, prob = ifelse(is.na(prob.x), 1, prob.x) * ifelse(is.na(prob.y), 1, prob.y)) %>%
    summarize(prob = sum(prob, na.rm = T))
  
  return(res)
}

play_probabilCasa <- function(teamA, teamB){
  prob_ <- ungroup(prob)
  A <- filter(prob_, grepl(teamA, team), casa == 1)
  B <- filter(prob_, grepl(teamB, team), casa == 2)
  res <- full_join(A, B, by = c('GF' = 'GS', 'GS' = 'GF')) %>%
    filter(GF + GS < 3) %>%
    transmute(GF, GS, prob = ( prob.x^2 * prob.y)) %>%
    summarize(prob = sum(prob, na.rm = T))
  
  return(res)
}

checkRes_probabil <- function (data, partita) {
  res <- unlist(apply(X = data, MARGIN = 1, function(x) partita(x['team'], x['avversario'])))
  comp <- cbind(mutate(data, officiale = GF + GS), atteso = unlist(res))
  comp$U <- ifelse(comp$officiale >= 2.5, 'O', 'U')
  comp$A <- ifelse(comp$atteso > 0.5, 'U', 'O')
  a <- nrow(filter(comp, A != U))
  return(a)
}

ultimiRis <- function(teamA, teamB) {
  casa <- olanda %>%
    filter(grepl(teamA, team), grepl(teamB, avversario), casa == 1)
  ospiti <- olanda %>%
    filter(grepl(teamA, team), grepl(teamB, avversario), casa == 2) %>%
    rename(team = avversario, avversario = team)
  ris <- bind_rows(casa,ospiti) %>%
    arrange(anno,giornata) %>%
    transmute(team, goalA = GF, goalB = GS, avversario, UO = GS + GF, Over = ifelse(UO >= 3, 'O', ''))
  return(ris)
}


nat <- 'niederlande'
a <- 2015
i <- 19
giornata <- scraper(nat, a, i) %>%
  filter(casa == 1)


risG <- data.frame(soloGoalFatti = unlist(apply(giornata, MARGIN = 1, function(x) play_soloGoalFatti(x['team'],x['avversario']))))
risG <- ifelse(risG['soloGoalFatti'] >= 3, 'O', 'U')

risGB <- data.frame(goalFatti = unlist(apply(giornata, MARGIN = 1, function(x) play_goalFatti(x['team'],x['avversario']))))
risGB <- ifelse(risGB['goalFatti'] >= 3, 'O', 'U')

risGC <- data.frame(goalCasa = unlist(apply(giornata, MARGIN = 1, function(x) play_Casa(x['team'],x['avversario']))))
risGC <- ifelse(risGC['goalCasa'] >= 3, 'O', 'U')

risGM <- data.frame(goalMedi = unlist(apply(giornata, MARGIN = 1, function(x) play_goal(x['team'],x['avversario']))))
risGM <- ifelse(risGM['goalMedi'] >= 3, 'O', 'U')

risP <- data.frame(probabil = unlist(apply(giornata, MARGIN = 1, function(x) play_probabil(x['team'],x['avversario']))))
risP <- ifelse(risP['probabil'] > 0.5, 'U', 'O')

risD <- data.frame(gruppi2 = unlist(apply(giornata, MARGIN = 1, function(x) play_gruppi2(x['team'],x['avversario']))))

previsione <- as.data.frame(cbind(soloGoalFatti = risG, goalFatti = risGB, goalCasa = risGC, goal = risGM, probab = risP, gruppi2 = risD, jacopo = c('O','O','U','O','U','O','U','U','U')))


quote <- c(2.55,1.45,2.30,1.55,2.10,1.65,2.00,1.72,2.40,1.50,2.00,1.72,2.10,1.65,2.00,1.72,2.10,1.65)
even <- c(1:9*2)
odd <- c(1:9*2-1)
quote <- as.data.frame(cbind(under = quote[odd],over = quote[even]))



tabellone <- giornata %>%
  bind_cols(quote) %>%
  bind_cols(previsione) %>%
  transmute(team, GF = as.numeric(GF), GS = as.numeric(GS), avversario, under, over, soloGoalFatti, goalFatti, goalCasa, goalMedi, probabil, gruppi2, jacopo) %>%
  mutate(risultato = ifelse(GF + GS >= 3, 'O', 'U'))

prese <- tabellone %>%
  transmute(under, over, 
            soloGoalFatti = ifelse(soloGoalFatti == risultato, 'X', ''),
            goalFatti = ifelse(goalFatti == risultato, 'X', ''),
            goalCasa = ifelse(goalCasa == risultato, 'X', ''),
            goalMedi = ifelse(goalMedi == risultato, 'X', ''),
            probabil = ifelse(probabil == risultato, 'X', ''),
            gruppi2 = ifelse(gruppi2 == risultato, 'X', ''),
            jacopo = ifelse(jacopo == risultato, 'X','')
  )

vincite <- tabellone %>%
  transmute(soloGoalFatti = ifelse(soloGoalFatti == risultato , ifelse(risultato == 'U', under-1, over-1), -1),
            goalFatti= ifelse(goalFatti == risultato, ifelse(risultato == 'U', under-1, over-1), -1),
            goalCasa = ifelse(goalCasa == risultato , ifelse(risultato == 'U', under-1, over-1), -1),
            goalMedi = ifelse(goalMedi == risultato , ifelse(risultato == 'U', under-1, over-1), -1),
            probabil = ifelse(probabil == risultato , ifelse(risultato == 'U', under-1, over-1), -1),
            gruppi2 = ifelse(gruppi2 == risultato , ifelse(risultato == 'U', under-1, over-1), -1),
            jacopo = ifelse(jacopo == risultato, ifelse(risultato == 'U', under-1, over-1), -1)
  )

summarize(vincite, sum(soloGoalFatti), sum(goalFatti), sum(goalCasa), sum(goalMedi), sum(probabil), sum(gruppi2), sum(jacopo))
