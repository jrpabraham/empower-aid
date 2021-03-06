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

varnames <- as.vector(read.delim(file = here("data", "K1_Field_Survey.csv"), sep = ",", header = FALSE, stringsAsFactors = FALSE, na.strings = "", nrows = 1))
k1_df <- read.delim(file = here("data", "K1_Field_Survey.csv"), sep = ",", header = FALSE, stringsAsFactors = FALSE, na.strings = "", skip = 2, nrows = 600, col.names = varnames)

## Survey meta data ##

k1_df$start.time.mst <- as.POSIXct(as.character(k1_df$V3), format = "%m/%d/%y %H:%M")
k1_df$start.time.eat <- k1_df$start.time.mst + (3600 * 9)

k1_df$end.time.mst <- as.POSIXct(as.character(k1_df$V4), format = "%m/%d/%y %H:%M")
k1_df$end.time.eat <- k1_df$end.time.mst + (3600 * 9)

## Participant ID ##

k1_df$survey.id <- k1_df$V1
k1_df <- k1_df[complete.cases(k1_df$survey.id), ]

nonentry <- c("R_5ZkChbiXDIj6KsK", "R_7N8rNtLsxF5a9on", "R_97Tx2cAjy30fqMR", "R_lYnRvOAJhuax6LS", "R_8dky4iSC7rfEuNc", "R_nczo7KPxLkkKkgo", "R_5j1OiNu3wMp265N", "R_0GYX0scNN16ICfQ", "R_6PoFtAhwvSBNYzi", "R_696kAyWai9bDkFI", "R_hPaLwAaYCnY0l69", "R_oGNBAVexMWYhMml", "R_3rwTGdEULwOGH2y", "R_bhNv0SnArTa32Xe", "R_5txHVIbQY6twLIZ", "R_kaSM6nunZ9Ynatj", "R_0ppcidBVCPaEkXK", "R_oWceQFJG5NnSokN", "R_h5Cw4tvVUeDY8NI", "R_9ib4ASBi450NZPt", "R_mAlfPdxj5GQsJF5", "R_kbW6NDTS1FWXyn3", "R_b1GC7jpoQrJKrFN", "R_5YpNpbPWOxnSNWc", "R_mwLFSjScVyrgs9J", "R_1jbOtmMIrvgTgaE", "R_if5tz3h1N9MzTp2", "R_aFmo1jRrWIwsLjI", "R_2itxVUcUstO3Syp", "R_9LP4exOnWpJ1TrE", "R_cQLu9PDCtFwbn7K", "R_oFl39knSVL3SKpz", "R_11Y1KTzawxBJmvy")
k1_df <- k1_df[!k1_df$survey.id %in% nonentry, ]

## Treatment assignment ##

k1_df$treat[k1_df$condition == "poor"] <- 0
k1_df$treat[k1_df$condition == "individual"] <- 1
k1_df$treat[k1_df$condition == "community"] <- 2
k1_df$treat <- factor(k1_df$treat, labels = c("Pov", "Ind", "Com"))

k1_df$condition.order <- factor(k1_df$treat, labels = c("Poverty Alleviation", "Individual Empowerment", "Community Empowerment"))

k1_df$pov <- ifelse(k1_df$condition == "poor", 1, 0)
k1_df$ind <- ifelse(k1_df$condition == "individual", 1, 0)
k1_df$com <- ifelse(k1_df$condition == "community", 1, 0)

k1_df$msg1 <- recode(as.numeric(as.factor(k1_df$ORG_MESSAGE)), `2` = 0, `3` = 2)
k1_df$msg2 <- recode(as.numeric(as.factor(k1_df$ORG_MESSAGE_2)), `2` = 0)
k1_df$msg3 <- recode(as.numeric(as.factor(k1_df$ORG_MESSAGE_3)), `3` = 2)

## Video selection ##

k1_df$vid.imp1 <- k1_df$vid.dec1 %in% c(3, 5)
k1_df$vid.imp2 <- k1_df$vid.dec2 %in% c(3, 5)
k1_df$vid.num <- k1_df$vid.imp1 + k1_df$vid.imp2

## Intertemporal choice ##

k1_df$sav.save <- k1_df$sav.dec > 1
k1_df$sav.save[k1_df$sav.dec < 0] <- NA

k1_df$sav.amt[k1_df$sav.dec == 1] = 0
k1_df$sav.amt[k1_df$sav.dec == 2] = 100
k1_df$sav.amt[k1_df$sav.dec == 3] = 200
k1_df$sav.amt[k1_df$sav.dec < 0] <- NA

## Query theory (savings) ##

que_df <- k1_df[names(k1_df) %in% c("survey.id", "que.rat1", "que.rat2", "que.rat3", "que.rat4", "que.rat5")]

k1_df$que.nonm <- apply(que_df[, 1:5], 1, function(x) length(x[is.na(x) == FALSE]))

que_df <- melt(que_df, id = c("survey.id"))
que_df$variable <- as.numeric(que_df$variable)
que_df <- dcast(que_df[is.na(que_df$value) == FALSE & que_df$value > 0, ], survey.id ~ value, median, value.var = "variable")
names(que_df) <- c("survey.id", "que.mri", "que.mrp")
k1_df <- merge(k1_df, que_df, all.x = TRUE)

k1_df$que.smrd <- (2 * (k1_df$que.mrp - k1_df$que.mri)) / k1_df$que.nonm
k1_df$que.smrd[is.na(k1_df$que.mrp)] <- 1
k1_df$que.smrd[is.na(k1_df$que.mri)] <- -1

# missing values are imputed with upper and lower bounds

## Message of support (5-point scale) ##

k1_df$msg.dec[k1_df$msg.dec < 0] = NA
k1_df$msg.dec <- k1_df$msg.dec - 1

k1_df$msg.emp[k1_df$msg.emp < 0] = NA

k1_df$msg.lik[k1_df$msg.lik < 0] = NA
k1_df$msg.lik.r <- (7 - k1_df$msg.lik) * (5/6)  

k1_df$msg.avg <- (k1_df$msg.emp + k1_df$msg.lik.r) / 2

## Frame evaluation ##

k1_df$eva.poor[k1_df$msg1 == 0] <- k1_df$eva.msg1[k1_df$msg1 == 0]
k1_df$eva.poor[k1_df$msg2 == 0] <- k1_df$eva.msg2[k1_df$msg2 == 0]
k1_df$eva.poor[k1_df$msg3 == 0] <- k1_df$eva.msg3[k1_df$msg3 == 0]

k1_df$eva.ind[k1_df$msg1 == 1] <- k1_df$eva.msg1[k1_df$msg1 == 1]
k1_df$eva.ind[k1_df$msg2 == 1] <- k1_df$eva.msg2[k1_df$msg2 == 1]
k1_df$eva.ind[k1_df$msg3 == 1] <- k1_df$eva.msg3[k1_df$msg3 == 1]

k1_df$eva.com[k1_df$msg1 == 2] <- k1_df$eva.msg1[k1_df$msg1 == 2]
k1_df$eva.com[k1_df$msg2 == 2] <- k1_df$eva.msg2[k1_df$msg2 == 2]
k1_df$eva.com[k1_df$msg3 == 2] <- k1_df$eva.msg3[k1_df$msg3 == 2]

k1_df$eva.vid.poor <- k1_df$eva.rank.vid_8
k1_df$eva.vid.ind <- k1_df$eva.rank.vid_9
k1_df$eva.vid.com <- k1_df$eva.rank.vid_10

k1_df$eva.conf[k1_df$eva.conf < 0] <- NA

k1_df$eva.emp.poor <- k1_df$eva.rank.emp_5
k1_df$eva.emp.ind <- k1_df$eva.rank.emp_6
k1_df$eva.emp.com <- k1_df$eva.rank.emp_7

## Ladder scales ##

k1_df$ses.lad.now[k1_df$ses.lad.now < 0] <- NA
k1_df$ses.lad.now.z <- scale(k1_df$ses.lad.now)

k1_df$ses.lad.y2[k1_df$ses.lad.y2 < 0] <- NA
k1_df$ses.lad.y2.z <- scale(k1_df$ses.lad.y2)

k1_df$ses.lad.diff <- k1_df$ses.lad.y2 - k1_df$ses.lad.now
k1_df$ses.lad.avg <- (k1_df$ses.lad.y2 + k1_df$ses.lad.now) / 2

## Sociodemographics ##

k1_df$soc.age[k1_df$soc.age < 0] <- NA

k1_df$soc.pri <- as.numeric(k1_df$soc.edu > 3)

k1_df$soc.fem <- k1_df$soc.gen - 1
k1_df$soc.fem[k1_df$soc.fem < 0] <- NA

k1_df$soc.chr <- k1_df$soc.rel %in% c(1, 2)

k1_df$ses.unemp <- k1_df$ses.emp %in% c(1, 2)

k1_df$soc.inc[k1_df$soc.inc < 0] <- NA
k1_df$soc.inc.wins[k1_df$soc.inc <= quantile(k1_df$soc.inc, .99)] <- k1_df$soc.inc[k1_df$soc.inc <= quantile(k1_df$soc.inc, .99)]
k1_df$soc.inc.wins.ln <- log(k1_df$soc.inc.wins + sqrt(k1_df$soc.inc.wins^2 + 1))

k1_df$soc.con[k1_df$soc.con < 0] <- NA
k1_df$soc.con.wins[k1_df$soc.con <= quantile(k1_df$soc.con, .99)] <- k1_df$soc.con[k1_df$soc.con <= quantile(k1_df$soc.con, .99)]
k1_df$soc.con.wins.ln <- log(k1_df$soc.con.wins + sqrt(k1_df$soc.con.wins^2 + 1))

k1_df$soc.sav <- k1_df$soc.sav - 1

k1_df$soc.eme.z <- scale(k1_df$soc.eme)

## Survey validity ##

k1_df$end.hear <- k1_df$end.hear - 1
k1_df$end.hear[k1_df$end.hear < 0] <- NA

## Center covariates ##

k1_df$soc.fem.c <- scale(k1_df$soc.fem, scale = FALSE)
k1_df$soc.pri.c <- scale(k1_df$soc.pri, scale = FALSE)
k1_df$soc.age.c <- scale(k1_df$soc.age, scale = FALSE)
k1_df$ses.unemp.c <- scale(k1_df$ses.unemp, scale = FALSE)
k1_df$soc.sav.c <- scale(k1_df$soc.sav, scale = FALSE)

## Remove extraneous variables ##

k1_df <- k1_df[grep("RA_coder", names(k1_df), value = TRUE, invert = TRUE)]

save(k1_df, file = here("data", "KenyaData.RData"))
write.csv(k1_df, file = here("data", "KenyaData.csv"))