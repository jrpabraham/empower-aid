# Author: Justin Abraham
# R Version: R-3.6.2
# RStudio Version: 1.2.5033

###############################
## Install required packages ##
###############################

set.seed(47269801)

install.packages("pacman", repos='https://cloud.r-project.org/')
library("pacman")

p_load("here", "tidyr", "dplyr", "lmtest", "multiwayvcov", "multcomp", "reshape2", "knitr", "flextable", "officer", "forestplot", "cowplot", "ggplot2", "matrixStats", "ggthemes", "ggsignif", "rstudioapi", "iptools")

source(here("r", "Funs.r"), echo = TRUE)

################
## Clean data ##
################

## Read data ##

varnames <- as.vector(read.delim(file = here("data", "U1_MTurk_Eligible.csv"), sep = ",", header = FALSE, stringsAsFactors = FALSE, na.strings = "", nrows = 1))
u1_df <- read.delim(file = here("data", "U1_MTurk_Eligible.csv"), sep = ",", header = FALSE, stringsAsFactors = FALSE, na.strings = "", skip = 1, col.names = varnames)

## Treatment assignment ##

u1_df$treat[u1_df$condition == "poverty"] <- 0
u1_df$treat[u1_df$condition == "individual"] <- 1
u1_df$treat[u1_df$condition == "community"] <- 2
u1_df$treat <- factor(u1_df$treat, labels = c("Pov", "Ind", "Com"), levels = c(0, 1, 2))

u1_df$pov <- ifelse(u1_df$condition == "poverty", 1, 0)
u1_df$ind <- ifelse(u1_df$condition == "individual", 1, 0)
u1_df$com <- ifelse(u1_df$condition == "community", 1, 0)

## Recoding vars ##

u1_df$spot1[which(u1_df$spot1 == 4)] <- 3
u1_df$donation_me <- u1_df$donation_pov_1
u1_df$donation_org <- u1_df$donation_pov_2
u1_df$donated <- as.logical(u1_df$donation_org > 0)
d.donated <- u1_df %>%
  dplyr::filter(donated==1)

u1_df$donor_status <- NA
u1_df$donor_status <- u1_df$stat.ladder_14

u1_df$recip.impact_r <- 8-u1_df$recip.impact
u1_df$recip.improve_r <- 8-u1_df$recip.improve
u1_df$recip.ladder_r <- 11-u1_df$recip.ladder_14

## Clean sociodems ##

u1_df$priordonor <- as.numeric(as.logical(u1_df$priorgiving == 2))
u1_df$age <- as.numeric(u1_df$age)
u1_df$gen.fem <- as.numeric(as.logical(u1_df$gender ==2))
u1_df$income.hh <- as.numeric(u1_df$income/sqrt(u1_df$hhsize))
u1_df$hi.income.hh <- as.numeric(as.logical(u1_df$income.hh > 31305))
u1_df$hi.income <- as.numeric(as.logical(u1_df$income > 50000))
u1_df$edu.self.ba <- as.numeric(as.logical(u1_df$educ >4))
u1_df$edu.par.ba <- as.numeric(as.logical(u1_df$educ.parent >4))
u1_df$edu.par.ba <- as.numeric(as.logical(u1_df$educ.parent >4))
u1_df$race.minor <- as.numeric(as.logical(u1_df$race !=1))
u1_df$is.religi <- as.numeric(as.logical(u1_df$religiosity >2))
u1_df$is.chr <- as.numeric(as.logical(u1_df$religion == 6))
u1_df$is.dem <- as.numeric(as.logical(u1_df$party == 3))

# need to add religion dummy

## Center covariates ##

u1_df$priordonor.c <- scale(u1_df$priordonor, scale = FALSE)
u1_df$gen.fem.c <- scale(u1_df$gen.fem, scale = FALSE)
u1_df$hi.income.hh.c <- scale(u1_df$hi.income.hh, scale = FALSE)
u1_df$edu.self.ba.c <- scale(u1_df$edu.self.ba, scale = FALSE)
u1_df$race.minor.c <- scale(u1_df$race.minor, scale = FALSE)
u1_df$is.religi.c <- scale(u1_df$is.religi, scale = FALSE)
u1_df$is.chr.c <- scale(u1_df$is.chr, scale = FALSE)
u1_df$is.dem.c <- scale(u1_df$is.dem, scale = FALSE)

save(u1_df, file = here("data", "USData.RData"))