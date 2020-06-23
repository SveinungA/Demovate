rm(list = ls())                     # Slette allerede eksisterende data

library(foreign)
library(plyr)
library(tidyverse)
library(readxl)
library(xml2)
library(knitr)
library(scales)
library(haven)
library(kableExtra)

d <- read_sav("responsanalyse.SAV")

variables_with_labels = map(d, function(x) attr(x, "class") == "haven_labelled") %>% unlist() %>%  names()
d_factored = d %>% mutate_at( vars(variables_with_labels), as_factor)

d_factored$Bydel <- as.character((d_factored$Bydel))
d_factored$bydel_8 <- as.character((d_factored$bydel_8))
d_factored$R1DEMOVATE21 <- as.character((d_factored$R1DEMOVATE21))
d_factored$aldrkat <- as.character((d_factored$aldrkat))
d_factored$R1DEMOVATE16 <- as.character((d_factored$R1DEMOVATE16))
d_factored$R1DEMOVATE18 <- as.character((d_factored$R1DEMOVATE18))
d_factored$R1DEMOVATE19 <- as.character((d_factored$R1DEMOVATE19))
d_factored$R1DEMOVATE15 <- as.character((d_factored$R1DEMOVATE15))

d_factored$R1DEMOVATE1 <- as.character((d_factored$R1DEMOVATE1))
d_factored$R1DEMOVATE1 <- str_remove(d_factored$R1DEMOVATE1, " - Jeg støtter det fullstendig")
d_factored$R1DEMOVATE1 <- str_remove(d_factored$R1DEMOVATE1, " - Jeg motsetter meg det fullstendig")
d_factored$R1DEMOVATE1 <- str_remove(d_factored$R1DEMOVATE1, " - Ikke sikker/Ubesvart")
d_factored$R1DEMOVATE1 <- as.numeric((d_factored$R1DEMOVATE1))

d_factored$R1DEMOVATE2 <- as.character((d_factored$R1DEMOVATE2))
d_factored$R1DEMOVATE2 <- str_remove(d_factored$R1DEMOVATE2, " - Jeg støtter det fullstendig")
d_factored$R1DEMOVATE2 <- str_remove(d_factored$R1DEMOVATE2, " - Jeg motsetter meg det fullstendig")
d_factored$R1DEMOVATE2 <- str_remove(d_factored$R1DEMOVATE2, " - Ikke sikker/Ubesvart")
d_factored$R1DEMOVATE2 <- as.numeric((d_factored$R1DEMOVATE2))

d_factored$R1DEMOVATE3 <- as.character((d_factored$R1DEMOVATE3))
d_factored$R1DEMOVATE3 <- str_remove(d_factored$R1DEMOVATE3, " - Jeg støtter det fullstendig")
d_factored$R1DEMOVATE3 <- str_remove(d_factored$R1DEMOVATE3, " - Jeg motsetter meg det fullstendig")
d_factored$R1DEMOVATE3 <- str_remove(d_factored$R1DEMOVATE3, " - Ikke sikker/Ubesvart")
d_factored$R1DEMOVATE3 <- as.numeric((d_factored$R1DEMOVATE3))

d_factored$R1DEMOVATE4 <- as.character((d_factored$R1DEMOVATE4))
d_factored$R1DEMOVATE4 <- str_remove(d_factored$R1DEMOVATE4, " - Jeg støtter det fullstendig")
d_factored$R1DEMOVATE4 <- str_remove(d_factored$R1DEMOVATE4, " - Jeg motsetter meg det fullstendig")
d_factored$R1DEMOVATE4 <- str_remove(d_factored$R1DEMOVATE4, " - Ikke sikker/Ubesvart")
d_factored$R1DEMOVATE4 <- as.numeric((d_factored$R1DEMOVATE4))

d_factored$R1DEMOVATE5 <- as.character((d_factored$R1DEMOVATE5))
d_factored$R1DEMOVATE5 <- str_remove(d_factored$R1DEMOVATE5, " - Jeg støtter det fullstendig")
d_factored$R1DEMOVATE5 <- str_remove(d_factored$R1DEMOVATE5, " - Jeg motsetter meg det fullstendig")
d_factored$R1DEMOVATE5 <- str_remove(d_factored$R1DEMOVATE5, " - Ikke sikker/Ubesvart")
d_factored$R1DEMOVATE5 <- as.numeric((d_factored$R1DEMOVATE5))

####
## 0 = Ingen tillit i det hele tatt, 10 = Full tillit
d_factored$R1DEMOVATE8 <- as.character((d_factored$R1DEMOVATE8))
d_factored$R1DEMOVATE8 <- str_remove(d_factored$R1DEMOVATE8, " - Ingen tillit i det hele tatt")
d_factored$R1DEMOVATE8 <- str_remove(d_factored$R1DEMOVATE8, " - Full tillit")
d_factored$R1DEMOVATE8 <- str_replace(d_factored$R1DEMOVATE8, "Ikke sikker/ubesvart", "11")
d_factored$R1DEMOVATE8 <- as.numeric((d_factored$R1DEMOVATE8))

d_factored$R1DEMOVATE9 <- as.character((d_factored$R1DEMOVATE9))
d_factored$R1DEMOVATE9 <- str_remove(d_factored$R1DEMOVATE9, " - Ingen tillit i det hele tatt")
d_factored$R1DEMOVATE9 <- str_remove(d_factored$R1DEMOVATE9, " - Full tillit")
d_factored$R1DEMOVATE9 <- str_replace(d_factored$R1DEMOVATE9, "Ikke sikker/ubesvart", "11")
d_factored$R1DEMOVATE9 <- as.numeric((d_factored$R1DEMOVATE9))

## 0 = Ikke i det hele tatt, 10 = Fullt og helt
d_factored$R1DEMOVATE10 <- as.character((d_factored$R1DEMOVATE10))
d_factored$R1DEMOVATE10 <- str_remove(d_factored$R1DEMOVATE10, " - Ikke i det hele tatt")
d_factored$R1DEMOVATE10 <- str_remove(d_factored$R1DEMOVATE10, " - Fullt og helt")
d_factored$R1DEMOVATE10 <- str_replace(d_factored$R1DEMOVATE10, "Ikke sikker/ubesvart", "11")
d_factored$R1DEMOVATE10 <- as.numeric((d_factored$R1DEMOVATE10))

# 0 = Passer ikke i det hele tatt, 10 = Passer fullt og helt
d_factored$R1DEMOVATE11 <- as.character((d_factored$R1DEMOVATE11))
d_factored$R1DEMOVATE11 <- str_remove(d_factored$R1DEMOVATE11, " - Passer fullt og helt")
d_factored$R1DEMOVATE11 <- str_remove(d_factored$R1DEMOVATE11, " - Passer ikke i det hele tatt")
d_factored$R1DEMOVATE11 <- str_replace(d_factored$R1DEMOVATE11, "Ikke sikker/ubesvart", "11")
d_factored$R1DEMOVATE11 <- as.numeric((d_factored$R1DEMOVATE11))

# Venstre høyre akse. 0 = Venstre, 10 = Høyre
d_factored$R1DEMOVATE12 <- as.character((d_factored$R1DEMOVATE12))
d_factored$R1DEMOVATE12 <- str_remove(d_factored$R1DEMOVATE12, " - Venstre")
d_factored$R1DEMOVATE12 <- str_remove(d_factored$R1DEMOVATE12, " - Høyre")
d_factored$R1DEMOVATE12 <- str_replace(d_factored$R1DEMOVATE12, "Ikke sikker/Ubesvart", "11")
d_factored$R1DEMOVATE12 <- as.numeric((d_factored$R1DEMOVATE12))

# Hvilket parti stemte du på?
# Her må jeg endre på "Andre"-kolonnen.
d_factored$R1DEMOVATE13 <- as.character((d_factored$R1DEMOVATE13))

################
d_factored$R1DEMOVATE14 <- as.character((d_factored$R1DEMOVATE14))

d_factored$R1DEMOVATE23 <- as.character((d_factored$R1DEMOVATE23))

##################
d_factored$R1DEMOVATE7_1 <- as.numeric(d_factored$R1DEMOVATE7_1)
d_factored$R1DEMOVATE7_2 <- as.numeric(d_factored$R1DEMOVATE7_2)
d_factored$R1DEMOVATE7_3 <- as.numeric(d_factored$R1DEMOVATE7_3)
