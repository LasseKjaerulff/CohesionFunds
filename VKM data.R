library(tidyverse)
library(stringr)
library(foreign)
library(ggplot2)
library(gridExtra)
library(stargazer)
library(estimatr)

#############################################################
#################### LOADE GEMTE DATASÆT ####################
#############################################################

setwd("/Users/LasseJensen/Documents/Statskundskab/Kandidat/9. semester/Videregående kvantitative metoder/Eksamen/EU strukturfonde")

df_final <- readRDS("df_final_strukturfonde_bnp_ess_05122019_3.RData")
bnp_fonde_endelig <- readRDS("BNP_fonde_05012019_2.RData")
ess_lang <- readRDS("ess_lang_03012019.RData")

##Klargøring af data om midler fra strukturfonde


ecf <- read_csv("Historic_EU_payments_-_regionalised_and_modelled.csv") #Loader data om midler fra strukturfonde
View(ecf) #Inspicerer

summary(ecf$EU_Payment_annual) #Negative tal?

strukturfonde <- ecf %>%
  select(Country, NUTS1_ID, NUTS2_ID, NUTS2_name, Fund, Year, Programming_Period,
         EU_Payment_annual) %>%
  as_tibble() #Beholder kun brugbar information

strukturfonde$Year <- as.character(strukturfonde$Year) #Laver "Year" til chr.

strukturfonde <- strukturfonde %>%
  filter(Fund == "CF") %>% #Beholder kun data fra Cohesion fonden
  group_by(Country, Year, NUTS1_ID, NUTS2_ID, NUTS2_name) %>% #Grupperer på alt undtagen programme-period
  summarise(midler = mean(EU_Payment_annual, na.rm = T)) #Tager gennemsnittet af tildelte midler for hvert år

gdpnuts2_1 <- read.table(file = "tgs00006.tsv", sep = '\t', header = TRUE) #Loader BNP data

bnp_reg <- gather(gdpnuts2_1, year, gdp, X2005:X2016) #Laver om til langt format

bnp_reg %>%
  group_by(year) %>%
  filter(gdp > 75 & gdp < 80) %>%
  count() #Antal lige over 75

bnp_reg %>%
  group_by(year) %>%
  filter(gdp < 75 & gdp > 70) %>%
  count() #Antal lige under 75
  
bnp_reg <- bnp_reg %>%
  mutate(`Location Cd` = str_sub(unit.geo.time, -4),
         year = str_sub(year, -4),
         gdp = as.integer(gdp)) %>% #Laver var m. NUTS2 og år (tager sidste fire værdier i hhv. unit.geo.time og year. Gdp til num.)
  select(`Location Cd`, year, gdp) %>% #Udtrækker kun relevante variable
  as.tibble() 

colnames(bnp_reg)[2] <- "Year" #Omdøber til stort mhp. merge
colnames(bnp_reg)[1] <- "NUTS2_ID" 

View(bnp_reg)
str(bnp_reg) #OK

### MERGING
View(strukturfonde)

bnp_fonde <- bnp_reg %>%
  left_join(strukturfonde, by = c("NUTS2_ID", "Year")) %>%
  as.tibble() #Merger

View(bnp_fonde)

bnp_fonde_endelig <- bnp_fonde %>%
  mutate(gdp = as.numeric(gdp),
         Year = as.numeric(Year),
         treat = ifelse(gdp < 75, 1, 0),
         running_var = gdp-75) %>% #Laver treatment og tærskelvariabel.
  as.tibble()

colnames(bnp_fonde_endelig)[1] <- "NUTS2id" #Omdøber til stort mhp. merge

View(bnp_fonde_endelig) #OK

saveRDS(bnp_fonde_endelig, file = "BNP_fonde_05012019_2.RData")

### LOADER ESS-data

ess08 <- read.dta("ESS04_2008.dta")
ess10 <- read.dta("ESS05_2010.dta")
ess12 <- read.dta("ESS06_2012.dta")
ess14 <- read.dta("ESS07_2014.dta")

ess08 <- ess08[-7] #Fjerner NUTS-level
ess10 <- ess10[-10:-11] #Fjerner unødvendige variable om medievaner 

ess08 <- ess08[-3]
ess10 <- ess10[-3]
ess12 <- ess12[-3]
ess14 <- ess14[-3] #Fjerner kolonner med ess_reg

ess08$essround[ess08$essround == 4] <- "2008"
ess10$essround[ess10$essround == 5] <- "2010"
ess12$essround[ess12$essround == 6] <- "2012"
ess14$essround[ess14$essround == 7] <- "2014" #Laver "essround" om til år mhp. merging

colnames(ess08)[6] <- "Year"
colnames(ess10)[6] <- "Year"
colnames(ess12)[6] <- "Year"
colnames(ess14)[6] <- "Year" #Laver navne om mhp. merging

colnames(ess14)[1] <- "ESS7_id"
colnames(ess14)[3] <- "NUTS1"
colnames(ess14)[4] <- "NUTS2"
colnames(ess14)[5] <- "NUTS3" #Ensretter navne

colnames(ess08)[1] <- "ESSID"
colnames(ess10)[1] <- "ESSID"
colnames(ess12)[1] <- "ESSID"
colnames(ess14)[1] <- "ESSID" #Laver navne om mhp. merging

dim(ess08)
dim(ess10)
dim(ess12)
dim(ess14) #Samme antal kolonner i hver df

names(ess08)
names(ess10)
names(ess12)
names(ess14) #Samme navne i samme rækkefølge

ess08 <- as_tibble(ess08)
ess10 <- as_tibble(ess10)
ess12 <- as_tibble(ess12)
ess14 <- as_tibble(ess14) #Laver til tibbles for kompatibilitet i tidyverse

ess_samlet <- ess08 %>% #Samler alle bølger af ESS 
  left_join(ess10, by = c("cntry", "idno"), suffix = c("2008", "2010")) %>%
  left_join(ess12, by = c("cntry", "idno"), suffix = c("2010", "2012")) %>%
  left_join(ess14, by = c("cntry", "idno"), suffix = c("2012", "2014")) #Assigner årstal til hver variabel

ess_samlet_lim <- ess_samlet %>% 
  dplyr::select(-contains(c("weight"))) %>%
  dplyr::select(-contains(c("pspwght"))) %>%
  dplyr::select(-contains(c("NUTS1"))) %>%
  dplyr::select(-contains(c("NUTS3"))) %>%
  dplyr::select(-contains(c("ess4"))) %>%
  dplyr::select(-contains(c("ess5"))) %>%
  dplyr::select(-contains(c("ESS6"))) %>%
  dplyr::select(-contains(c("ESS7"))) #Fjerner irelevante variable

as_tibble(ess_samlet_lim)

#Laver først dataframes med hver variabel for at lave datasættet om til langt format

ess_samlet_nuts2id <- ess_samlet_lim %>%
  select(cntry, idno, contains(c("NUTS2")))

ess_samlet_tvpol <- ess_samlet_lim %>%
  select(cntry, idno, contains(c("tvpol")))

ess_samlet_trustparl <- ess_samlet_lim %>%
  select(cntry, idno, contains(c("trstprl")))

ess_samlet_trustpolit <- ess_samlet_lim %>%
  select(cntry, idno, contains(c("trstplt")))

ess_samlet_trustep <- ess_samlet_lim %>%
  select(cntry, idno, contains(c("trstep")))

ess_samlet_satiseco <- ess_samlet_lim %>%
  select(cntry, idno, contains(c("stfeco")))

ess_samlet_satisdemo <- ess_samlet_lim %>%
  select(cntry, idno, contains(c("stfdem")))

ess_samlet_domic <- ess_samlet_lim %>%
  select(cntry, idno, contains(c("domicil")))

ess_samlet_indk <- ess_samlet_lim %>%
  select(cntry, idno, contains(c("hinctnta")))

# Laver nye dataframes i langt format med gather

NUTS2_lang <- ess_samlet_nuts2id %>%
  gather(., nutsaar, NUTS2id, NUTS22008:NUTS22014) %>%
  mutate(Year = str_sub(nutsaar, -4)) %>%
  select(-contains(c("nutsaar"))) %>%
  select(cntry, idno, Year, NUTS2id)

tvpol_lang <- ess_samlet_tvpol %>%
  gather(., tvpol, polopm, tvpol2008:tvpol2014) %>%
  mutate(Year = str_sub(tvpol, -4)) %>%
  select(-contains(c("tvpol"))) %>%
  select(cntry, idno, Year, polopm)

trstparl_lang <- ess_samlet_trustparl %>%
  gather(., parltrustaar, trstparl, trstprl2008:trstprl2014) %>%
  mutate(Year = str_sub(parltrustaar, -4)) %>%
  select(-contains(c("parltrustaar"))) %>%
  select(cntry, idno, Year, trstparl)

trstpolit_lang <- ess_samlet_trustpolit %>%
  gather(., polittrustaar, trstpol, trstplt2008:trstplt2014) %>%
  mutate(Year = str_sub(polittrustaar, -4)) %>%
  select(-contains(c("polittrustaar"))) %>%
  select(cntry, idno, Year, trstpol)

trstep_lang <- ess_samlet_trustep %>%
  gather(., eptrustaar, trstep, trstep2008:trstep2014) %>%
  mutate(Year = str_sub(eptrustaar, -4)) %>%
  select(-contains(c("eptrustaar"))) %>%
  select(cntry, idno, Year, trstep)

ecosatis_lang <- ess_samlet_satiseco %>%
  gather(., satisecoaar, ecosatis, stfeco2008:stfeco2014) %>%
  mutate(Year = str_sub(satisecoaar, -4)) %>%
  select(-contains(c("satisecoaar"))) %>%
  select(cntry, idno, Year, ecosatis)

demsatis_lang <- ess_samlet_satisdemo %>%
  gather(., satisdemoaar, demosatis, stfdem2008: stfdem2014) %>%
  mutate(Year = str_sub(satisdemoaar, -4)) %>%
  select(-contains(c("satisdemoaar"))) %>%
  select(cntry, idno, Year, demosatis)

domic_lang <- ess_samlet_domic %>%
  gather(., aardicl, domicil, domicil2008:domicil2014) %>%
  mutate(Year = str_sub(aardicl, -4)) %>%
  select(-contains(c("aardicl"))) %>%
  select(cntry, idno, Year, domicil)

indk_lang <- ess_samlet_indk %>%
  gather(., aarikd, indk, hinctnta2008:hinctnta2014) %>%
  mutate(Year = str_sub(aarikd, -4)) %>%
  select(-contains(c("aarikd"))) %>%
  select(cntry, idno, Year, indk)

ess_lang <- NUTS2_lang %>%
  left_join(., tvpol_lang, by = c("cntry", "idno", "Year")) %>%
  left_join(., trstparl_lang, by = c("cntry", "idno", "Year")) %>%
  left_join(., trstpolit_lang, by = c("cntry", "idno", "Year")) %>%
  left_join(., trstep_lang, by = c("cntry", "idno", "Year")) %>%
  left_join(., ecosatis_lang, by = c("cntry", "idno", "Year")) %>%
  left_join(., demsatis_lang, by = c("cntry", "idno", "Year")) %>%
  left_join(., domic_lang, by = c("cntry", "idno", "Year")) %>%
  left_join(., indk_lang, by = c("cntry", "idno", "Year")) #Samler det hele i ét datasæt
  
ess_lang$Year <- as.numeric(ess_lang$Year) #Laver Year til numeric for at kunne merge

summary(ess_lang$Year) #2008-2014

#saveRDS(ess_lang, file = "ess_lang_03012019.RData")

### MERGER DATA OM MIDLER FRA STRUKTURFONDE, BNP OG ESS

df_final <- ess_lang %>% 
  left_join(., bnp_fonde_endelig, by = c("NUTS2id", "Year"))

#Omkoder variable i df_final så alle bliver numeric og får rette værdi på skala
names(df_final)
str(df_final)
View(df_final)

df_final$trstep[df_final$trstep=="Complete trust"] <- 10
df_final$trstep[df_final$trstep=="No trust at all"] <- 0
df_final$trstep <- as.numeric(df_final$trstep)
class(df_final$trstep) #OK

df_final$trstparl[df_final$trstparl=="Complete trust"] <- 10
df_final$trstparl[df_final$trstparl=="No trust at all"] <- 0
df_final$trstparl <- as.numeric(df_final$trstparl)

df_final$trstpol[df_final$trstpol=="Complete trust"] <- 10
df_final$trstpol[df_final$trstpol=="No trust at all"] <- 0
df_final$trstpol <- as.numeric(df_final$trstpol)

df_final$ecosatis[df_final$ecosatis=="Extremely satisfied"] <- 10
df_final$ecosatis[df_final$ecosatis=="Extremely dissatisfied"] <- 0
df_final$ecosatis <- as.numeric(df_final$ecosatis)

df_final$demosatis[df_final$demosatis=="Extremely satisfied"] <- 10
df_final$demosatis[df_final$demosatis=="Extremely dissatisfied"] <- 0
df_final$demosatis <- as.numeric(df_final$demosatis)

df_final$polopm[df_final$polopm=="No time at all"] <- 0
df_final$polopm[df_final$polopm=="Less than 0,5 hour"] <- 1
df_final$polopm[df_final$polopm=="0,5 hour to 1 hour"] <- 2
df_final$polopm[df_final$polopm=="More than 1 hour, up to 1,5 hours"] <- 3
df_final$polopm[df_final$polopm=="More than 1,5 hours, up to 2 hours"] <- 4
df_final$polopm[df_final$polopm=="More than 2 hours, up to 2,5 hours"] <- 5
df_final$polopm[df_final$polopm=="More than 2,5 hours, up to 3 hours"] <- 6
df_final$polopm[df_final$polopm=="More than 3 hours"] <- 7
df_final$polopm <- as.numeric(df_final$polopm)

df_final$domicil[df_final$domicil=="A big city"] <- 1
df_final$domicil[df_final$domicil=="Suburbs or outskirts of big city"] <- 2
df_final$domicil[df_final$domicil=="Town or small city"] <- 3
df_final$domicil[df_final$domicil=="Country village"] <- 4
df_final$domicil[df_final$domicil=="Farm or home in countryside"] <- 5
df_final$domicil <- as.numeric(df_final$domicil)
unique(df_final$domicil)

df_final$indk[df_final$indk=="J - 1st decile"] <- 1
df_final$indk[df_final$indk=="R - 2nd decile"] <- 2
df_final$indk[df_final$indk=="C - 3rd decile"] <- 3
df_final$indk[df_final$indk=="M - 4th decile"] <- 4
df_final$indk[df_final$indk=="F - 5th decile"] <- 5
df_final$indk[df_final$indk=="S - 6th decile"] <- 6
df_final$indk[df_final$indk=="K - 7th decile"] <- 7
df_final$indk[df_final$indk=="P - 8th decile"] <- 8
df_final$indk[df_final$indk=="D - 9th decile"] <- 9
df_final$indk[df_final$indk=="H - 10th decile"] <- 10
df_final$indk <- as.numeric(df_final$indk)

View(df_final)

df_final <- df_final%>%
  mutate(NUTS1 = substr(NUTS2id, 1, 3)) #Laver ny variabel med NUTS1 regioner

#saveRDS(df_final, file = "df_final_strukturfonde_bnp_ess_05122019_3.RData")

df_final %>%
  filter(Year == 2008 | Year == 2010 | Year == 2012 | Year == 2014,
         NUTS2id != "",
         running_var >= -5 & running_var <= 5) %>%
  unique() %>%
  count() #8784 observationer (inkl. missing)

df_final_rdd_5 <- df_final %>%
  filter(Year == 2008 | Year == 2010 | Year == 2012 | Year == 2014,
         NUTS2id != "",
         running_var >= -5 & running_var <= 5) %>%
  mutate(NUTS1 = substr(NUTS2id, 1, 3))  #BW = 10

df_final_rdd_20 <- df_final %>%
  filter(Year == 2008 | Year == 2010 | Year == 2012 | Year == 2014,
         NUTS2id != "",
         running_var >= -20 & running_var <= 20) %>%
  mutate(NUTS1 = substr(NUTS2id, 1, 3)) #BW = 40

df_final_rdd_15 <- df_final %>%
  filter(Year == 2008 | Year == 2010 | Year == 2012 | Year == 2014,
         NUTS2id != "",
         running_var >= -15 & running_var <= 15) %>% #BW = 30
  mutate(NUTS1 = substr(NUTS2id, 1, 3))

df_final_rdd_10 <- df_final %>%
  filter(Year == 2008 | Year == 2010 | Year == 2012 | Year == 2014,
         NUTS2id != "",
         running_var >= -10 & running_var <= 10) %>% #BW = 20
  mutate(NUTS1 = substr(NUTS2id, 1, 3))

bnp_reg_means_5 <- bnp_fonde_endelig %>%
  group_by(running_var) %>%
  filter(running_var >= -5 & running_var <= 5) %>%
  summarise(meangdp = mean(gdp, na.rm = T),
            meanmidler = mean(midler, na.rm = T)) %>%
  mutate(treat = ifelse(running_var < 0, 1, 0)) #Laver subset af data på regionsniveau med ± 5 til cutoff

bnp_reg_means_10 <- bnp_fonde_endelig %>%
  group_by(running_var) %>%
  filter(running_var >= -10 & running_var <= 10) %>%
  summarise(meangdp = mean(gdp, na.rm = T),
            meanmidler = mean(midler, na.rm = T)) %>%
  mutate(treat = ifelse(running_var < 0, 1, 0)) #Laver subset af data på regionsniveau med ± 10 til cutoff

bnp_reg_means_15 <- bnp_fonde_endelig %>%
  group_by(running_var) %>%
  filter(running_var >= -15 & running_var <= 15) %>%
  summarise(meangdp = mean(gdp, na.rm = T),
            meanmidler = mean(midler, na.rm = T)) %>%
  mutate(treat = ifelse(running_var < 0, 1, 0)) #Laver subset af data på regionsniveau med ± 15 til cutoff

bnp_reg_means_20 <- bnp_fonde_endelig %>%
  group_by(running_var) %>%
  filter(running_var >= -20 & running_var <= 20) %>%
  summarise(meangdp = mean(gdp, na.rm = T),
            meanmidler = mean(midler, na.rm = T)) %>%
  mutate(treat = ifelse(running_var < 0, 1, 0)) #Laver subset af data på regionsniveau med ± 20 til cutoff

bnp_50 <- bnp_fonde_endelig %>%
  filter(running_var >= -50 & running_var <= 50) %>%
  group_by(running_var) %>%
  summarise(meanmidler = mean(midler, na.rm = T)) %>%
  mutate(treat = ifelse(running_var < 0, 1, 0)) #Bred bandwidth (BW = 50)


df_final_rdd_means_5 <- df_final_rdd_5 %>%
  group_by(running_var) %>%
  summarise(meanpolopm = mean(polopm, na.rm = T),
            meantrstparl = mean(trstparl, na.rm = T),
            meantrstpol = mean(trstpol, na.rm = T),
            meantrstep = mean(trstep, na.rm = T),
            meanecosatis = mean(ecosatis, na.rm = T),
            meandemosatis = mean(demosatis, na.rm = T),
            meanindk = mean(indk, na.rm = T),
            meangdp = mean(gdp, na.rm = T)) %>% #Beholde BNP
  mutate(treat = ifelse(running_var < 0, 1, 0))

df_final_rdd_means_15 <- df_final_rdd_15 %>%
  group_by(running_var) %>%
  summarise(meanpolopm = mean(polopm, na.rm = T),
            meantrstparl = mean(trstparl, na.rm = T),
            meantrstpol = mean(trstpol, na.rm = T),
            meantrstep = mean(trstep, na.rm = T),
            meanecosatis = mean(ecosatis, na.rm = T),
            meandemosatis = mean(demosatis, na.rm = T),
            meanindk = mean(indk, na.rm = T),
            meangdp = mean(gdp, na.rm = T)) %>% #Beholde BNP
  mutate(treat = ifelse(running_var < 0, 1, 0))

### SLUT