######################################
################ PLOTS ###############
######################################

library(gridExtra)
library(ggplot2)

# PLOTS FRA OPGAVEN
# kør scriptet til ende og load nedenstående

hist_ecosatis # Figur 1
grid.arrange(bnpplot, rddplot_reg_5) #Figur 3
grid.arrange(bnpplot_loess, rddplot_reg_5_loess) #Figur 4
rddplot_ess_5 #Figur 5
grid.arrange(rddplot_means_ind_2SLS, rddplot_means_ind_2SLS_15) #Figur 6
grid.arrange(rddplot_reg_15, rddplot_reg_20) #Appendix

### Laver funktion til at justere akser med

formattermio <- function(x){ 
  x/1000000 
}

### Deskriptiv statistik

hist_ecosatis <- df_final_rdd_5 %>%
  ggplot(aes(x = (ecosatis)) ) +
  geom_density(fill = "azure4", alpha = 0.8, bw = 0.5) + #bw justeres for at smoothe mere
  scale_x_continuous(breaks = seq(0,10,1),
                     limits = c(0,10)) +
  xlab("Tilfredshed med samfundsøkonomien") +
  ylab("Densitet") + 
  ggtitle("Fordelingen af den afhængige variabel",
          subtitle = "Kilde: ESS bølge 4-7 (2008 - 2014)") +
  theme_minimal()
  
hist_ecosatis

### PLOTS MED REGIONER SOM ANALYSEENHED

# RDD plots til afsnit 5.1

rddplot_reg_5 <- bnp_reg_means_5 %>%
  ggplot(aes(x = running_var, y = meanmidler)) +
  geom_point(aes(color = factor(treat))) +
  scale_color_brewer(palette = "Set1",
                     name = "Over/under\n75 % af GDP",
                       labels = c(">= 75 %", "< 75 %")) +
  scale_x_continuous(breaks = seq(-5,5,1)) +
  scale_y_continuous(labels = formattermio) +
  geom_smooth(method = "lm", data = subset(bnp_reg_means_5, running_var < 1), col = "black") +
  geom_smooth(method = "lm", data = subset(bnp_reg_means_5, running_var >= 0), col = "black") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Regionalt BNP relativt til tærskelværdi") +
  ylab("Gns. midler fra Samhørighedsfonden (mio. €)") +
  ggtitle("Antal modtagne midler fra Samhørighedsfonden omkring tærskelværdien",
          subtitle = "Kilde: Gns. på tværs af regioner for udvalgte værdier af BNP") +
  theme_minimal()

bnpplot <- bnp_50 %>%
  ggplot(aes(x = running_var, y = meanmidler)) +
  geom_point(aes(color = factor(treat))) +
  scale_color_brewer(palette = "Set1",
                     name = "Over/under\n75 % af GDP",
                     labels = c(">= 75 %", "< 75 %")) +
  scale_x_continuous(breaks = seq(-50,50,10)) +
  scale_y_continuous(labels = formattermio) +
  geom_smooth(method = "lm", data = subset(bnp_50, running_var < 1), col = "black") +
  geom_smooth(method = "lm", data = subset(bnp_50, running_var >= 0), col = "black") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Regionalt BNP relativt til tærskelværdi") +
  ylab("Gns. midler fra Samhørighedsfonden (mio. €)") +
  ggtitle("Antal modtagne midler fra Samhørighedsfonden omkring tærskelværdien",
          subtitle = "Kilde: Gns. på tværs af regioner for udvalgte værdier af BNP") +
  theme_minimal()

bnpplot_loess <- bnp_50 %>%
  ggplot(aes(x = running_var, y = meanmidler)) +
  geom_point(aes(color = factor(treat))) +
  scale_color_brewer(palette = "Set1",
                     name = "Over/under\n75 % af GDP",
                     labels = c(">= 75 %", "< 75 %")) +
  scale_x_continuous(breaks = seq(-50,50,10)) +
  scale_y_continuous(labels = formattermio) +
  geom_smooth(method = "lm", data = subset(bnp_50, running_var < 1), col = "black") +
  geom_smooth(method = "lm", data = subset(bnp_50, running_var >= 0), col = "black") +
  geom_smooth(method = "loess", data = subset(bnp_50, running_var >= 0), 
              col = "azure4", linetype = "longdash", se = F) +
  geom_smooth(method = "loess", data = subset(bnp_50, running_var < 1), 
              col = "azure4", linetype = "longdash", se = F) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Regionalt BNP relativt til tærskelværdi") +
  ylab("Gns. midler fra Samhørighedsfonden (mio. €)") +
  ggtitle("Antal modtagne midler fra Samhørighedsfonden omkring tærskelværdien",
          subtitle = "Kilde: LOESS-smoother estimeret på begge sider af tærskelværdi") +
  theme_minimal()

rddplot_reg_5_loess <- bnp_reg_means_5 %>%
  ggplot(aes(x = running_var, y = meanmidler)) +
  geom_point(aes(color = factor(treat))) +
  scale_color_brewer(palette = "Set1",
                     name = "Over/under\n75 % af GDP",
                     labels = c(">= 75 %", "< 75 %")) +
  scale_x_continuous(breaks = seq(-5,5,1)) +
  scale_y_continuous(labels = formattermio) +
  geom_smooth(method = "lm", data = subset(bnp_reg_means_5, running_var < 1), col = "black") +
  geom_smooth(method = "lm", data = subset(bnp_reg_means_5, running_var >= 0), col = "black") +
  geom_smooth(method = "loess", data = subset(bnp_reg_means_5, running_var >= 0), 
              col = "azure4", linetype = "longdash", se = F) +
  geom_smooth(method = "loess", data = subset(bnp_reg_means_5, running_var < 1), 
              col = "azure4", linetype = "longdash", se = F) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Regionalt BNP relativt til tærskelværdi") +
  ylab("Gns. midler fra Samhørighedsfonden (mio. €)") +
  ggtitle("Antal modtagne midler fra Samhørighedsfonden omkring tærskelværdien",
          subtitle = "Kilde: Gns. på tværs af regioner for udvalgte værdier af BNP") +
  theme_minimal()

bnpplot_loess
rddplot_reg_5_loess
rddplot_reg_5
bnpplot

grid.arrange(bnpplot, rddplot_reg_5) #Figur 3
grid.arrange(bnpplot_loess, rddplot_reg_5_loess) #Figur 4

rddplot_reg_15 <- bnp_reg_means_15 %>%
  ggplot(aes(x = running_var, y = meanmidler)) +
  geom_point(aes(color = factor(treat))) +
  scale_color_brewer(palette = "Set1",
                     name = "Over/under\n75 % af GDP",
                     labels = c(">= 75 %", "< 75 %")) +
  scale_y_continuous(labels = formattermio) +
  geom_smooth(method = "lm", data = subset(bnp_reg_means_15, running_var < 1), col = "black") +
  geom_smooth(method = "lm", data = subset(bnp_reg_means_15, running_var >= 0), col = "black") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Regionalt BNP relativt til tærskelværdi") +
  ylab("Gns. midler fra Samhørighedsfonden (mio. €)") +
  ggtitle("Antal modtagne midler fra Samhørighedsfonden omkring tærskelværdien (BW = 30)",
          subtitle = "Kilde: Gns. på tværs af regioner for udvalgte værdier af BNP") +
  theme_minimal()

rddplot_reg_20 <- bnp_reg_means_20 %>%
  ggplot(aes(x = running_var, y = meanmidler)) +
  geom_point(aes(color = factor(treat))) +
  scale_color_brewer(palette = "Set1",
                     name = "Over/under\n75 % af GDP",
                     labels = c(">= 75 %", "< 75 %")) +
  scale_y_continuous(labels = formattermio) +
  geom_smooth(method = "lm", data = subset(bnp_reg_means_20, running_var < 1), col = "black") +
  geom_smooth(method = "lm", data = subset(bnp_reg_means_20, running_var >= 0), col = "black") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Regionalt BNP relativt til tærskelværdi") +
  ylab("Gns. midler fra Samhørighedsfonden (mio. €)") +
  ggtitle("Antal modtagne midler fra Samhørighedsfonden omkring tærskelværdien (BW = 40)",
          subtitle = "Kilde: Gns. på tværs af regioner for udvalgte værdier af BNP") +
  theme_minimal()

rddplot_reg_15 #APPENDIX
rddplot_reg_20 #APPENDIX
grid.arrange(rddplot_reg_15, rddplot_reg_20) #Appendix


### PLOTS MED GENNEMSNIT OVER INDIVIDER SOM ANALYSEENHED

rddplot_means_ind_2SLS <- df_final_rdd_means_5 %>%
  ggplot(aes(x = running_var, y = meanecosatis)) +
  geom_point(aes(color = factor(treat))) +
  scale_color_brewer(palette = "Set1",
                     name = "Over/under\n75 % af GDP",
                     labels = c(">= 75 %", "< 75 %")) +
  scale_x_continuous(breaks = seq(-5,5,1)) +
  geom_smooth(method = "lm", data = subset(df_final_rdd_means_5, running_var < 1), col = "black") +
  geom_smooth(method = "lm", data = subset(df_final_rdd_means_5, running_var >= 0), col = "black") +
  geom_smooth(data = subset(df_final_rdd_means_5, running_var < 1), col = "azure4", se = F,
              linetype = 6) +
  geom_smooth(data = subset(df_final_rdd_means_5, running_var >= 0), col = "azure4", se = F,
              linetype = 6) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Regionalt BNP relativt til tærskelværdi") +
  ylab("Samfundsøkonomisk tilfredshed") +
  ggtitle("Samfundsøkonomisk tilfredshed omkring tærskelværdien",
          subtitle = "Data er gennemsnit på tværs af alle tærskelværdier") +
  theme_minimal()

rddplot_means_ind_2SLS  

rddplot_means_ind_2SLS_15 <- df_final_rdd_means_15 %>%
  ggplot(aes(x = running_var, y = meanecosatis)) +
  geom_point(aes(color = factor(treat))) +
  scale_color_brewer(palette = "Set1",
                     name = "Over/under\n75 % af GDP",
                     labels = c(">= 75 %", "< 75 %")) +
  scale_x_continuous(breaks = seq(-15,15,3)) +
  geom_smooth(method = "lm", data = subset(df_final_rdd_means_15, running_var < 1), col = "black") +
  geom_smooth(method = "lm", data = subset(df_final_rdd_means_15, running_var >= 0), col = "black") +
  geom_smooth(data = subset(df_final_rdd_means_15, running_var < 1), col = "azure4",
              linetype = 6, se = F) +
  geom_smooth(data = subset(df_final_rdd_means_15, running_var >= 0), col = "azure4",
              linetype= 6, se = F) +
  geom_vline(xintercept = 0, linetype = "dashed") + 
  xlab("Regionalt BNP relativt til tærskelværdi") +
  ylab("Samfundsøkonomisk tilfredshed") +
  ggtitle("Samfundsøkonomisk tilfredshed omkring tærskelværdien",
          subtitle = "Data er gennemsnit på tværs af alle tærskelværdier") +
  theme_minimal()

rddplot_means_ind_2SLS_15

grid.arrange(rddplot_means_ind_2SLS, rddplot_means_ind_2SLS_15) #Figur 6

### PLOTS MED INDIVIDER UDEN GENNEMSNIT

rddplot_ess_5 <- df_final_rdd_5 %>%
  ggplot(aes(x = running_var, y = ecosatis)) +
  geom_point(aes(color = factor(treat))) +
  scale_color_brewer(palette = "Set1",
                     name = "Over/under\n75 % af GDP",
                     labels = c(">= 75 %", "< 75 %")) +
  geom_jitter(alpha = 0.1) +
  geom_smooth(method = "lm", data = subset(df_final_rdd_5, running_var < 1), col = "black") +
  geom_smooth(method = "lm", data = subset(df_final_rdd_5, running_var >= 0), col = "black") +
  geom_smooth(method = "loess", data = subset(df_final_rdd_5, running_var < 1), col = "azure4",
              linetype = "longdash") +
  geom_smooth(method = "loess", data = subset(df_final_rdd_5, running_var >= 0), col = "azure4",
              linetype = "longdash") +
  geom_vline(xintercept = 0, linetype = "dashed") + 
  scale_y_continuous(breaks = seq(0,10,1),
                     limits = c(0,10)) +
  scale_x_continuous(breaks = seq(-5,5,1)) +
  xlab("Regionalt BNP relativt til tærskelværdi") +
  ylab("Tilfredshed med samfundsøkonomien (10 = meget tilfreds)") +
  ggtitle("Tilfredshed med samfundsøkonomien omkring tærskelværdien",
          subtitle = "Individdata med tilfældig støj omkring datapunkterne") +
  theme_minimal()

rddplot_ess_5 #Figur 5

rddplot_ess_5_facet_nuts1 <- df_final_rdd_5 %>%
  ggplot(aes(x = running_var, y = ecosatis)) +
  scale_color_brewer(palette = "Set1",
                     name = "Over/under\n75 % af GDP",
                     labels = c(">= 75 %", "< 75 %")) +
  scale_x_continuous(breaks = seq(-5,5,1)) +
  scale_y_continuous(breaks = seq(0,10,2)) +
  geom_point(aes(color = factor(treat))) +
  geom_jitter(alpha = 0.1) +
  geom_smooth(method = "lm", data = subset(df_final_rdd_5, running_var < 1), col = "black") +
  geom_smooth(method = "lm", data = subset(df_final_rdd_5, running_var >= 0), col = "black") +
  geom_smooth(se = F, alpha = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Regionalt BNP relativt til tærskelværdi") +
  ylab("Gns. midler fra Samhørighedsfonden (mio. €)") +
  ggtitle("Den effektive sample i estimationerne (± 5 fra tærskelværdien)",
          subtitle = "Opdelt på NUTS-1 regioner") +
  theme_bw() +
  facet_wrap(. ~ NUTS1)

rddplot_ess_5_facet_nuts1 # Figur 7

##############################################
################ ESTIMATIONER ################
##############################################

# Test af relevanskriterium

#ols1_1S_10sample <- lm(midler ~ treat + factor(NUTS1), data = df_final_rdd_10) # 1. stage i ± 10 sample
ols2_1S_5sample <- lm(midler ~ treat + factor(NUTS1), data = df_final_rdd_5) # 1. stage i reduceret sample (± 5 fra BNP = 75 %)

# OLS Modeller til Stargazer
ols_fuldsample <- lm(ecosatis ~ midler, data = df_final)
ols2_5sample <- lm(ecosatis ~ midler, data = df_final_rdd_5)
ols_fuldsample_fuldmodel <- lm(ecosatis ~ midler + gdp + factor(NUTS1), data = df_final)
ols2_5sample_fuldmodel <- lm(ecosatis ~ midler + gdp + factor(NUTS1), data = df_final_rdd_5)

# 2SLS-estimationer

red_form <- lm_robust(ecosatis ~ treat + factor(NUTS1), clusters = factor(NUTS2id),
                      se_type = "stata", data = df_final_rdd_5)
first_stage <- lm_robust(midler ~ treat + factor(NUTS1), clusters = factor(NUTS2id),
                         se_type = "stata", data = df_final_rdd_5)
summary(first_stage)
summary(red_form) 

LATE <- -0.29933/11885983

LATE # = -0.00000002518345, dvs. 0

iv_5sample <- iv_robust(ecosatis ~ midler | treat, data = df_final_rdd_5,
                         clusters = factor(NUTS2id), se_type = "stata") #IV

iv_5sample_fuld <- iv_robust(ecosatis ~ midler + factor(NUTS1) | treat + factor(NUTS1), data = df_final_rdd_5,
                        clusters = factor(NUTS2id), se_type = "stata") #IV

iv_10sample <- iv_robust(ecosatis ~ midler | treat, data = df_final_rdd_15,
                        clusters = factor(NUTS2id), se_type = "stata") #IV

iv_10sample_fuld <- iv_robust(ecosatis ~ midler + factor(NUTS1) | treat + factor(NUTS1), data = df_final_rdd_15,
                             clusters = factor(NUTS2id), se_type = "stata") #IV

##########################################
################ TABELLER ################
##########################################

#Relevanskriterium

stargazer(ols2_1S_5sample, 
          type = "html", out = "relevanskriterium.html", digits = 2,
          title = "Tabel 2: Instrumentets relevans",
          dep.var.labels.include = F,
          omit = "factor",
          dep.var.caption = "<em> Afhaengig variabel: Midler fra EU fonde </em>",
          covariate.labels = c("Under 75 % af BNP (ref. over 75%)", "Konstant"),
          column.labels = c("Sample +/- 5"),
          add.lines = list(c("NUTS1 FE", "Y")),
          notes.label = "Note: Standardfejl i parantes")

# OLS resultater

stargazer(ols_fuldsample, ols2_5sample, ols_fuldsample_fuldmodel, ols2_5sample_fuldmodel, 
          type = "html", omit = "factor", out = "OLS.html", digits = 2,
          dep.var.labels.include = F,
          df = F,
          dep.var.caption = "<em> Afhaengig variabel: Tilfredshed med oekonomien (0 - 10) </em>",
          covariate.labels = c("Midler fra EU fonde", "BNP (NUTS2)"),
          column.labels = c("Fuld sample", "GDP 70-80", "Fuld sample", "GDP 70-80"),
          add.lines = list(c("NUTS1 FE", "", "", "Y", "Y")),
          notes.label = "Note: Standardfejl i parantes")

# 2SLS estimater

# Laver resultater til dataframe og trækker korrekte statistiske informationer ud
iv_5sample_tidy <- tidy(iv_5sample)
iv_5sample_fuldtidy <- tidy(iv_5sample_fuld)
iv_10sample_tidy <- tidy(iv_10sample)
iv_10sample_fuld_tidy <- tidy(iv_10sample_fuld)

iv_5sample$r.squared
iv_5sample_fuld$r.squared
iv_10sample$r.squared
iv_10sample_fuld$r.squared

iv_5sample$adj.r.squared
iv_5sample_fuld$adj.r.squared
iv_10sample$adj.r.squared
iv_10sample_fuld$adj.r.squared

iv_5sample$N
iv_5sample_fuld$N
iv_10sample$N
iv_10sample_fuld$N

#Tabel 4 

stargazer(ols_fuldsample, ols_fuldsample, ols_fuldsample, ols_fuldsample,
          type = "html",
          se=list(iv_5sample_tidy$std.error, iv_5sample_fuldtidy$std.error,iv_10sample_tidy$std.error, iv_10sample_fuld_tidy$std.error),
          coef = list(iv_5sample_tidy$estimate, iv_5sample_fuldtidy$estimate, iv_10sample_tidy$estimate, iv_10sample_fuld_tidy$estimate),
          df = F,
          omit.stat = c("rsq", "adj.rsq", "n", "f", "ser"), 
          out = "IV - 2SLS.html",
          digits = 2,
          align = T,
          title = "Tabel 4: 2SLS estimationer",
          dep.var.labels.include = T,
          dep.var.labels = "2SLS",
          dep.var.caption = "<em> Afhaenging variabel: Tilfreds med oekonomien </em>",
          covariate.labels = c("Midler fra EU fonde", "Konstant"),
          add.lines = list(c("NUTS1 Fixed Effects", "", "Y", "", "Y"),
                           c("Observationer", "8459", "8459", "20598", "20598"),
                           c("Justeret R2", "0.028", "-0.019", "-4.226", "-0.663"),
                           c("R2", "0.029", "0.018", "-4.226", "-0.662")),
          column.labels = c("Sample: +/- 5", "Sample: +/- 15"),
          column.separate = c(2, 2),
          notes.label = "Note: Klyngerobuste standardfejl (NUTS2)")
