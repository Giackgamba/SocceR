library(XML)
library(dplyr)
library(RODBC)

tabellone <- list()
conn <- odbcConnect('Soccer')

#'italien','niederlande','tuerkei','belgien'
for (nat in c('niederlande')) {
  
  for (a in 2015:2015){
    
    for (i in 1:34) {
      
      theurl <- paste("http://fussballdaten.de", nat, a, i, sep="/")
      tables <- readHTMLTable(theurl)
      
      results <- tables[1]$'NULL' %>% select(c(4,6,7)) %>% filter(!is.na(V7))
      rm(tables)
      
      names(results) <- c('casa','ospiti','risultato')
      results$risultato <- as.character(results$risultato)
      
      odd <- c(1:nrow(results)*2-1)
      even <- c(1:nrow(results)*2)
      results <- results %>% 
        mutate(goalFinali = unlist(strsplit(risultato, "[[:space:]]"))[odd], goalPrimoT = unlist(strsplit(risultato, "[[:space:]]"))[even]) %>%
        mutate(goalPrimoT = gsub('\\(|\\)',replacement = '', goalPrimoT)) %>%
        mutate(goalCasaF = unlist(strsplit(goalFinali, ":"))[odd], goalOspitiF = unlist(strsplit(goalFinali, ":"))[even]) %>%
        mutate(goalCasaPT = unlist(strsplit(goalPrimoT, ":"))[odd], goalOspitiPT = unlist(strsplit(goalPrimoT, ":"))[even])
      results <- cbind(giornata = rep(i,nrow(results)),results)
      results <- cbind(anno = rep(a,nrow(results)),results)
      
      casa <- results %>% select(anno, giornata, team = casa, GF = goalCasaF, GS = goalOspitiF, avversario = ospiti) %>% cbind(casa = rep('1', nrow(results)))
      ospiti <- results %>% select(anno, giornata, team = ospiti, GF = goalOspitiF, GS = goalCasaF, avversario = casa) %>% cbind(casa = rep('2', nrow(results)))
      giornata <- rbind(casa,ospiti)
      
      sqlSave(conn, dat = giornata, tablename = nat, append = T, rownames = F )
      cat(nat, a, i)
    }
  }
}