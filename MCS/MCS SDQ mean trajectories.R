
library(tidyverse)
library(ggplot2)
library(tidyr)
library(data.table)
library(tidyverse)
library(lavaan)
library(RColorBrewer)

# Cr. Genie
MCS_autism <- read.csv("/Users/zhangxinhe/Documents/who recieves a diagnosis/MCS/MCS_SDQ_autism.csv") # 188 in total
MCS_sdq_autism <- MCS_autism[,c(1:21,34:39,22:33,40:45)]
MCS_sdq_autism$sex_num <- ifelse(MCS_sdq_autism$Sex =="Male", 1, 0)
MCS_no_diag <- read.csv("/Users/zhangxinhe/Documents/who recieves a diagnosis/MCS/MCS_SDQ_control.csv") # 6047 in total
MCS_sdq_no_diag <- MCS_no_diag[,c(1:21,34:39,22:33,40:45)]


## -------------------------------------------------------------------------------
mcs2_parent_interview <- read_delim("/Users/zhangxinhe/Documents/who recieves a diagnosis/MCS/MCS_data/UKDA-5350-tab/tab/mcs2_parent_interview.tab")
mcs_ID_identifier <- read.csv("/Users/zhangxinhe/Documents/who recieves a diagnosis/MCS/mcs_ID_identifier.csv")
mcs_ethnicity_mbirth_age <- read.csv("/Users/zhangxinhe/Documents/who recieves a diagnosis/MCS/mcs_race_mbirthage.csv")
mcs2_ses_identifier <- read.csv("/Users/zhangxinhe/Documents/who recieves a diagnosis/MCS/mcs_ses_identifier.csv")
mcs2_dep_identifier <- read.csv("/Users/zhangxinhe/Documents/who recieves a diagnosis/MCS/mcs_deprivation_identifier.csv")
mcs2_family_derived <- read_delim("/Users/zhangxinhe/Documents/who recieves a diagnosis/MCS/MCS_data/UKDA-5350-tab/tab/mcs2_family_derived.tab")
mcs2_other_ses_identifier <- read.csv("/Users/zhangxinhe/Documents/who recieves a diagnosis/MCS/mcs_other_ses_identifier.csv")


## -------------------------------------------------------------------------------
# Group autstic group according to their diagnosis age
# Though some parents provided specific diagnosis age in years, to conform to analysis in other cohorts so that cross-population comparisons could be allowed in the future
# "Early" = first reported diagnosis at w3 (age5) or w4 (age7)
# "Late = first reported diagnosis at w5 (age11) or w6 (age14)

library(plotrix)
MCS_sdq_autism_early <- MCS_sdq_autism %>% filter (ASD.Diagnosis.Age %in% c("at W3","at W4","at W5")) 
MCS_sdq_autism_late <- MCS_sdq_autism %>% filter (ASD.Diagnosis.Age %in% c("at W6")) 



## -------------------------------------------------------------------------------
# Early 
MCS_autism_early_sdq_ttl <- MCS_sdq_autism_early[,c(1,9,15,21,27,33,39,45)]

MCS_autism_early_sdq_ttl_long <- melt(setDT(MCS_autism_early_sdq_ttl), id.vars = c("MCSID","Sex"), variable.name = "sweep")

MCS_autism_early_ttl_summary <- MCS_autism_early_sdq_ttl_long %>%
  dplyr::group_by(sweep) %>%
  dplyr::summarise(
    sd = sd(value),
    ttl_mean = mean(value),
    se = std.error(value)
  )
MCS_autism_early_ttl_summary$group <- "early"

# Late

MCS_autism_late_sdq_ttl <- MCS_sdq_autism_late[,c(1,9,15,21,27,33,39,45)]

MCS_autism_late_sdq_ttl_long <- melt(setDT(MCS_autism_late_sdq_ttl), id.vars = c("MCSID","Sex"), variable.name = "sweep")

MCS_autism_late_ttl_summary <- MCS_autism_late_sdq_ttl_long %>%
   dplyr::group_by(sweep) %>%
   dplyr::summarise(
    sd = sd(value),
    ttl_mean = mean(value),     
     se = std.error(value)
  )
MCS_autism_late_ttl_summary$group <- "late"

# No diagnosis

MCS_no_diag_sdq_ttl <- MCS_sdq_no_diag[,c(1,9,15,21,27,33,39,42)]

MCS_no_diag_sdq_ttl_long <- melt(setDT(MCS_no_diag_sdq_ttl), id.vars = c("MCSID","Sex"), variable.name = "sweep")

MCS_no_diag_ttl_summary <- MCS_no_diag_sdq_ttl_long %>%
   dplyr::group_by(sweep) %>%
   dplyr::summarise(
    sd = sd(value),
    ttl_mean = mean(value),     
    se = std.error(value)
  )
MCS_no_diag_ttl_summary$group <- "no diag"


## -------------------------------------------------------------------------------
MCS_autism_early_ttl_summary$lb = MCS_autism_early_ttl_summary$ttl_mean - 1.96*MCS_autism_early_ttl_summary$se
MCS_autism_early_ttl_summary$ub = MCS_autism_early_ttl_summary$ttl_mean + 1.96*MCS_autism_early_ttl_summary$se
MCS_autism_late_ttl_summary$lb = MCS_autism_late_ttl_summary$ttl_mean - 1.96*MCS_autism_late_ttl_summary$se
MCS_autism_late_ttl_summary$ub = MCS_autism_late_ttl_summary$ttl_mean + 1.96*MCS_autism_late_ttl_summary$se
MCS_no_diag_ttl_summary$ub = MCS_no_diag_ttl_summary$ttl_mean + 1.96*MCS_no_diag_ttl_summary$se
MCS_no_diag_ttl_summary$lb = MCS_no_diag_ttl_summary$ttl_mean - 1.96*MCS_no_diag_ttl_summary$se
MCS_sdq_total_mean_full <- full_join(full_join(MCS_autism_early_ttl_summary, MCS_autism_late_ttl_summary), MCS_no_diag_ttl_summary)
MCS_sdq_total_mean_full <- MCS_sdq_total_mean_full %>% mutate(sweep.age = case_when(
  sweep == "BEBDTOT" ~ 3,
  sweep == "CEBDTOT" ~ 5,
  sweep == "DDDEBDTOT" ~ 7,
   sweep == "EEBDTOT" ~ 11,
   sweep == "FEBDTOT" ~ 14,
   sweep == "GEBDTOT" ~ 17,
))

## -------------------------------------------------------------------------------
# Early 
MCS_autism_early_sdq_emo <- MCS_sdq_autism_early[,c(1,4,10,16,22,28,34,45)]

MCS_autism_early_sdq_emo_long <- melt(setDT(MCS_autism_early_sdq_emo), id.vars = c("MCSID","Sex"), variable.name = "sweep")

MCS_autism_early_emo_summary <- MCS_autism_early_sdq_emo_long %>%
  dplyr::group_by(sweep) %>%
  dplyr::summarise(
    sd = sd(value),
    emo_mean = mean(value),     
    se = std.error(value)
  )
MCS_autism_early_emo_summary$group <- "early"

# Late

MCS_autism_late_sdq_emo <- MCS_sdq_autism_late[,c(1,4,10,16,22,28,34,45)]

MCS_autism_late_sdq_emo_long <- melt(setDT(MCS_autism_late_sdq_emo), id.vars = c("MCSID","Sex"), variable.name = "sweep")

MCS_autism_late_emo_summary <- MCS_autism_late_sdq_emo_long %>%
  dplyr::group_by(sweep) %>%
  dplyr::summarise(
    sd = sd(value),
    emo_mean = mean(value),     
    se = std.error(value)
  )
MCS_autism_late_emo_summary$group <- "late"

# No diagnosis

MCS_no_diag_sdq_emo <- MCS_sdq_no_diag[,c(1,4,10,16,22,28,34,42)]

MCS_no_diag_sdq_emo_long <- melt(setDT(MCS_no_diag_sdq_emo), id.vars = c("MCSID","Sex"), variable.name = "sweep")

MCS_no_diag_emo_summary <- MCS_no_diag_sdq_emo_long %>%
 dplyr::group_by(sweep) %>%
  dplyr::summarise(
    sd = sd(value),
    emo_mean = mean(value),     
    se = std.error(value)
  )
MCS_no_diag_emo_summary$group <- "no diag"



## -------------------------------------------------------------------------------
MCS_autism_early_emo_summary$lb = MCS_autism_early_emo_summary$emo_mean - 1.96*MCS_autism_early_emo_summary$se
MCS_autism_early_emo_summary$ub = MCS_autism_early_emo_summary$emo_mean + 1.96*MCS_autism_early_emo_summary$se
MCS_autism_late_emo_summary$lb = MCS_autism_late_emo_summary$emo_mean - 1.96*MCS_autism_late_emo_summary$se
MCS_autism_late_emo_summary$ub = MCS_autism_late_emo_summary$emo_mean + 1.96*MCS_autism_late_emo_summary$se
MCS_no_diag_emo_summary$ub = MCS_no_diag_emo_summary$emo_mean + 1.96*MCS_no_diag_emo_summary$se
MCS_no_diag_emo_summary$lb = MCS_no_diag_emo_summary$emo_mean - 1.96*MCS_no_diag_emo_summary$se
MCS_sdq_Emotional_mean_full <- full_join(full_join(MCS_autism_early_emo_summary, MCS_autism_late_emo_summary), MCS_no_diag_emo_summary)
MCS_sdq_Emotional_mean_full <- MCS_sdq_Emotional_mean_full %>% mutate(sweep.age = case_when(
  sweep == "BEMOTION" ~ 3,
  sweep == "CEMOTION" ~ 5,
  sweep == "DDEMOTION" ~ 7,
   sweep == "EEMOTION" ~ 11,
   sweep == "FEMOTION" ~ 14,
   sweep == "GEMOTION" ~ 17,
))




## -------------------------------------------------------------------------------
# Early 
MCS_autism_early_sdq_con <- MCS_sdq_autism_early[,c(1,5,11,17,23,29,35,45)]

MCS_autism_early_sdq_con_long <- melt(setDT(MCS_autism_early_sdq_con), id.vars = c("MCSID","Sex"), variable.name = "sweep")

MCS_autism_early_con_summary <- MCS_autism_early_sdq_con_long %>%
  dplyr::group_by(sweep) %>%
  dplyr::summarise(
    sd = sd(value),
    con_mean = mean(value),     
    se = std.error(value)
  )
MCS_autism_early_con_summary$group <- "early"

# Late

MCS_autism_late_sdq_con <- MCS_sdq_autism_late[,c(1,5,11,17,23,29,35,45)]

MCS_autism_late_sdq_con_long <- melt(setDT(MCS_autism_late_sdq_con), id.vars = c("MCSID","Sex"), variable.name = "sweep")

MCS_autism_late_con_summary <- MCS_autism_late_sdq_con_long %>%
  dplyr::group_by(sweep) %>%
  dplyr::summarise(
    sd = sd(value),
    con_mean = mean(value),     
    se = std.error(value)
  )
MCS_autism_late_con_summary$group <- "late"

# No diagnosis

MCS_no_diag_sdq_con <- MCS_sdq_no_diag[,c(1,5,11,17,23,29,35,42)]

MCS_no_diag_sdq_con_long <- melt(setDT(MCS_no_diag_sdq_con), id.vars = c("MCSID","Sex"), variable.name = "sweep")

MCS_no_diag_con_summary <- MCS_no_diag_sdq_con_long %>%
  dplyr::group_by(sweep) %>%
  dplyr::summarise(
    sd = sd(value),
    con_mean = mean(value),     
    se = std.error(value)
  )
MCS_no_diag_con_summary$group <- "no diag"

## -------------------------------------------------------------------------------
MCS_autism_early_con_summary$lb = MCS_autism_early_con_summary$con_mean - 1.96*MCS_autism_early_con_summary$se
MCS_autism_early_con_summary$ub = MCS_autism_early_con_summary$con_mean + 1.96*MCS_autism_early_con_summary$se
MCS_autism_late_con_summary$lb = MCS_autism_late_con_summary$con_mean - 1.96*MCS_autism_late_con_summary$se
MCS_autism_late_con_summary$ub = MCS_autism_late_con_summary$con_mean + 1.96*MCS_autism_late_con_summary$se
MCS_no_diag_con_summary$ub = MCS_no_diag_con_summary$con_mean + 1.96*MCS_no_diag_con_summary$se
MCS_no_diag_con_summary$lb = MCS_no_diag_con_summary$con_mean - 1.96*MCS_no_diag_con_summary$se
MCS_sdq_Conduct_mean_full <- full_join(full_join(MCS_autism_early_con_summary, MCS_autism_late_con_summary), MCS_no_diag_con_summary)
MCS_sdq_Conduct_mean_full <- MCS_sdq_Conduct_mean_full %>% mutate(sweep.age = case_when(
  sweep == "BCONDUCT" ~ 3,
  sweep == "CCONDUCT" ~ 5,
  sweep == "DDCONDUCT" ~ 7,
   sweep == "ECONDUCT" ~ 11,
   sweep == "FCONDUCT" ~ 14,
   sweep == "GCONDUCT" ~ 17,
))


## -------------------------------------------------------------------------------
# Early 
MCS_autism_early_sdq_hyp <- MCS_sdq_autism_early[,c(1,6,12,18,24,30,36,45)]

MCS_autism_early_sdq_hyp_long <- melt(setDT(MCS_autism_early_sdq_hyp), id.vars = c("MCSID","Sex"), variable.name = "sweep")

MCS_autism_early_hyp_summary <- MCS_autism_early_sdq_hyp_long %>%
  dplyr::group_by(sweep) %>%
  dplyr::summarise(
    sd = sd(value),
    hyp_mean = mean(value),    
    se = std.error(value)
  )
MCS_autism_early_hyp_summary$group <- "early"

# Late

MCS_autism_late_sdq_hyp <- MCS_sdq_autism_late[,c(1,6,12,18,24,30,36,45)]

MCS_autism_late_sdq_hyp_long <- melt(setDT(MCS_autism_late_sdq_hyp), id.vars = c("MCSID","Sex"), variable.name = "sweep")

MCS_autism_late_hyp_summary <- MCS_autism_late_sdq_hyp_long %>%
  dplyr::group_by(sweep) %>%
  dplyr::summarise(
    sd = sd(value),
    hyp_mean = mean(value),     
    se = std.error(value)
  )
MCS_autism_late_hyp_summary$group <- "late"

# No diagnosis

MCS_no_diag_sdq_hyp <- MCS_sdq_no_diag[,c(1,6,12,18,24,30,36,42)]

MCS_no_diag_sdq_hyp_long <- melt(setDT(MCS_no_diag_sdq_hyp), id.vars = c("MCSID","Sex"), variable.name = "sweep")

MCS_no_diag_hyp_summary <- MCS_no_diag_sdq_hyp_long %>%
  dplyr::group_by(sweep) %>%
  dplyr::summarise(
    sd = sd(value),
    hyp_mean = mean(value),     
    se = std.error(value)
  )
MCS_no_diag_hyp_summary$group <- "no diag"


## -------------------------------------------------------------------------------
MCS_autism_early_hyp_summary$lb = MCS_autism_early_hyp_summary$hyp_mean - 1.96*MCS_autism_early_hyp_summary$se
MCS_autism_early_hyp_summary$ub = MCS_autism_early_hyp_summary$hyp_mean + 1.96*MCS_autism_early_hyp_summary$se
MCS_autism_late_hyp_summary$lb = MCS_autism_late_hyp_summary$hyp_mean - 1.96*MCS_autism_late_hyp_summary$se
MCS_autism_late_hyp_summary$ub = MCS_autism_late_hyp_summary$hyp_mean + 1.96*MCS_autism_late_hyp_summary$se
MCS_no_diag_hyp_summary$ub = MCS_no_diag_hyp_summary$hyp_mean + 1.96*MCS_no_diag_hyp_summary$se
MCS_no_diag_hyp_summary$lb = MCS_no_diag_hyp_summary$hyp_mean - 1.96*MCS_no_diag_hyp_summary$se
MCS_sdq_hyperactivity_mean_full <- full_join(full_join(MCS_autism_early_hyp_summary, MCS_autism_late_hyp_summary), MCS_no_diag_hyp_summary)
MCS_sdq_hyperactivity_mean_full <- MCS_sdq_hyperactivity_mean_full %>% mutate(sweep.age = case_when(
  sweep == "BHYPER" ~ 3,
  sweep == "CHYPER" ~ 5,
  sweep == "DDHYPER" ~ 7,
   sweep == "EHYPER" ~ 11,
   sweep == "FHYPER" ~ 14,
   sweep == "GHYPER" ~ 17,
))



## -------------------------------------------------------------------------------
# Early 
MCS_autism_early_sdq_peer <- MCS_sdq_autism_early[,c(1,7,13,19,25,31,37,45)]

MCS_autism_early_sdq_peer_long <- melt(setDT(MCS_autism_early_sdq_peer), id.vars = c("MCSID","Sex"), variable.name = "sweep")

MCS_autism_early_peer_summary <- MCS_autism_early_sdq_peer_long %>%
  dplyr::group_by(sweep) %>%
  dplyr::summarise(
    sd = sd(value),
    peer_mean = mean(value),     
    se = std.error(value)
  )
MCS_autism_early_peer_summary$group <- "early"

# Late

MCS_autism_late_sdq_peer <- MCS_sdq_autism_late[,c(1,7,13,19,25,31,37,45)]

MCS_autism_late_sdq_peer_long <- melt(setDT(MCS_autism_late_sdq_peer), id.vars = c("MCSID","Sex"), variable.name = "sweep")

MCS_autism_late_peer_summary <- MCS_autism_late_sdq_peer_long %>%
  dplyr::group_by(sweep) %>%
  dplyr::summarise(
    sd = sd(value),
    peer_mean = mean(value),     
    se = std.error(value)
  )
MCS_autism_late_peer_summary$group <- "late"

# No diagnosis

MCS_no_diag_sdq_peer <- MCS_sdq_no_diag[,c(1,7,13,19,25,31,37,42)]

MCS_no_diag_sdq_peer_long <- melt(setDT(MCS_no_diag_sdq_peer), id.vars = c("MCSID","Sex"), variable.name = "sweep")

MCS_no_diag_peer_summary <- MCS_no_diag_sdq_peer_long %>%
  dplyr::group_by(sweep) %>%
  dplyr::summarise(
    sd = sd(value),
    peer_mean = mean(value),     
    se = std.error(value)
  )
MCS_no_diag_peer_summary$group <- "no diag"



## -------------------------------------------------------------------------------
MCS_autism_early_peer_summary$lb = MCS_autism_early_peer_summary$peer_mean - 1.96*MCS_autism_early_peer_summary$se
MCS_autism_early_peer_summary$ub = MCS_autism_early_peer_summary$peer_mean + 1.96*MCS_autism_early_peer_summary$se
MCS_autism_late_peer_summary$lb = MCS_autism_late_peer_summary$peer_mean - 1.96*MCS_autism_late_peer_summary$se
MCS_autism_late_peer_summary$ub = MCS_autism_late_peer_summary$peer_mean + 1.96*MCS_autism_late_peer_summary$se
MCS_no_diag_peer_summary$ub = MCS_no_diag_peer_summary$peer_mean + 1.96*MCS_no_diag_peer_summary$se
MCS_no_diag_peer_summary$lb = MCS_no_diag_peer_summary$peer_mean - 1.96*MCS_no_diag_peer_summary$se
MCS_sdq_peer_mean_full <- full_join(full_join(MCS_autism_early_peer_summary, MCS_autism_late_peer_summary), MCS_no_diag_peer_summary)
MCS_sdq_peer_mean_full <- MCS_sdq_peer_mean_full %>% mutate(sweep.age = case_when(
sweep == "BPEER" ~ 3,
  sweep == "CPEER" ~ 5,
  sweep == "DDPEER" ~ 7,
   sweep == "EPEER" ~ 11,
   sweep == "FPEER" ~ 14,
   sweep == "GPEER" ~ 17
))


## -------------------------------------------------------------------------------
# Early 
MCS_autism_early_sdq_pro <- MCS_sdq_autism_early[,c(1,8,14,20,26,32,38,45)]

MCS_autism_early_sdq_pro_long <- melt(setDT(MCS_autism_early_sdq_pro), id.vars = c("MCSID","Sex"), variable.name = "sweep")

MCS_autism_early_pro_summary <- MCS_autism_early_sdq_pro_long %>%
   dplyr::group_by(sweep) %>%
   dplyr::summarise(
    sd = sd(value),
    pro_mean = mean(value),     
     se = std.error(value)
  )
MCS_autism_early_pro_summary$group <- "early"

# Late

MCS_autism_late_sdq_pro <- MCS_sdq_autism_late[,c(1,8,14,20,26,32,38,45)]

MCS_autism_late_sdq_pro_long <- melt(setDT(MCS_autism_late_sdq_pro), id.vars = c("MCSID","Sex"), variable.name = "sweep")

MCS_autism_late_pro_summary <- MCS_autism_late_sdq_pro_long %>%
   dplyr::group_by(sweep) %>%
   dplyr::summarise(
    sd = sd(value),
    pro_mean = mean(value),     
     se = std.error(value)
  )
MCS_autism_late_pro_summary$group <- "late"

# No diagnosis

MCS_no_diag_sdq_pro <- MCS_sdq_no_diag[,c(1,8,14,20,26,32,38,42)]

MCS_no_diag_sdq_pro_long <- melt(setDT(MCS_no_diag_sdq_pro), id.vars = c("MCSID","Sex"), variable.name = "sweep")

MCS_no_diag_pro_summary <- MCS_no_diag_sdq_pro_long %>%
   dplyr::group_by(sweep) %>%
   dplyr::summarise(
    sd = sd(value),
    pro_mean = mean(value),     
     se = std.error(value)
  )
MCS_no_diag_pro_summary$group <- "no diag"


## -------------------------------------------------------------------------------
MCS_autism_early_pro_summary$lb = MCS_autism_early_pro_summary$pro_mean - 1.96*MCS_autism_early_pro_summary$se
MCS_autism_early_pro_summary$ub = MCS_autism_early_pro_summary$pro_mean + 1.96*MCS_autism_early_pro_summary$se
MCS_autism_late_pro_summary$lb = MCS_autism_late_pro_summary$pro_mean - 1.96*MCS_autism_late_pro_summary$se
MCS_autism_late_pro_summary$ub = MCS_autism_late_pro_summary$pro_mean + 1.96*MCS_autism_late_pro_summary$se
MCS_no_diag_pro_summary$ub = MCS_no_diag_pro_summary$pro_mean + 1.96*MCS_no_diag_pro_summary$se
MCS_no_diag_pro_summary$lb = MCS_no_diag_pro_summary$pro_mean - 1.96*MCS_no_diag_pro_summary$se
MCS_sdq_prosocial_mean_full <- full_join(full_join(MCS_autism_early_pro_summary, MCS_autism_late_pro_summary), MCS_no_diag_pro_summary)
MCS_sdq_prosocial_mean_full <- MCS_sdq_prosocial_mean_full %>% mutate(sweep.age = case_when(sweep == "BPROSOC" ~ 3,
  sweep == "CPROSOC" ~ 5,
  sweep == "DDPROSOC" ~ 7,
   sweep == "EPROSOC" ~ 11,
   sweep == "FPROSOC" ~ 14,
   sweep == "GPROSOC" ~ 17
                                                                                ))
