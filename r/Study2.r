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

## Dataset for stigma coding ##

tidy.codes.2 <- k1_df %>%
  dplyr::select(por.oth_1_RA_CODE, por.oth_2_RA_CODE, por.oth_3_RA_CODE,
                ind.oth_1_RA_CODE, ind.oth_2_RA_CODE, ind.oth_3_RA_CODE,
                com.oth_1_RA_CODE, com.oth_2_RA_CODE, com.oth_3_RA_CODE,
                condition.order, survey.id) %>%
  dplyr::mutate_at(vars(matches("oth")),funs(as.factor)) %>%
  rename(oth.1_por = por.oth_1_RA_CODE, oth.2_por = por.oth_2_RA_CODE, oth.3_por = por.oth_3_RA_CODE,
         oth.1_ind = ind.oth_1_RA_CODE, oth.2_ind = ind.oth_2_RA_CODE, oth.3_ind = ind.oth_3_RA_CODE,
         oth.1_com = com.oth_1_RA_CODE, oth.2_com = com.oth_2_RA_CODE, oth.3_com = com.oth_3_RA_CODE) 

tidy.codes.2$oth.1 <- coalesce(tidy.codes.2$oth.1_por, tidy.codes.2$oth.1_ind, tidy.codes.2$oth.1_com) 
tidy.codes.2$oth.2 <- coalesce(tidy.codes.2$oth.2_por, tidy.codes.2$oth.2_ind, tidy.codes.2$oth.2_com) 
tidy.codes.2$oth.3 <- coalesce(tidy.codes.2$oth.3_por, tidy.codes.2$oth.3_ind, tidy.codes.2$oth.3_com) 

tidy.codes.3 <- tidy.codes.2 %>%
  dplyr::select(oth.1, oth.2, oth.3, condition.order, survey.id) %>% 
  gather(contains('oth'), key=prompt, value=oth.value)

tidy.codes.3$oth.neg <- as.logical(tidy.codes.3$oth.value == "negative") 
tidy.codes.3$oth.pos <- as.logical(tidy.codes.3$oth.value == "positive") 

tidy.codes.sum <- tidy.codes.3 %>%
  group_by(survey.id) %>%
  mutate(neg.prop = sum(oth.value=="negative", na.rm=T) / length(which(!is.na(oth.value)))) %>%
  mutate(pos.prop = sum(oth.value=="positive", na.rm=T) / length(which(!is.na(oth.value)))) %>%
  mutate(amb.prop = sum(oth.value=="ambig", na.rm=T) / length(which(!is.na(oth.value)))) 

## Self-efficacy ##

for (var in c(k1_df$sel.con, k1_df$sel.pers, k1_df$sel.com, k1_df$sel.prob, k1_df$sel.bett)) {

    var[var < 0] <- NA

}

k1_df$sel.score.avg <- scale.means(k1_df, "sel.con", "sel.pers", "sel.com", "sel.prob", "sel.bett", na.rm = T)
k1_df$sel.score <- scale(k1_df$sel.con) + scale(k1_df$sel.pers) + scale(k1_df$sel.com) + scale(k1_df$sel.prob) + scale(k1_df$sel.bett)
k1_df$sel.score.z <- scale(k1_df$sel.score)

## Stigma ##

for (var in c(k1_df$jud.fam, k1_df$jud.com, k1_df$jud.judg, k1_df$jud.emb, k1_df$jud.ups)) {

    var[var < 0] <- NA

}

k1_df$jud.fam.r <- 6 - k1_df$jud.fam
k1_df$jud.com.r <- 6 - k1_df$jud.com

k1_df$sti.score.avg <- scale.means(k1_df, "jud.fam.r", "jud.com.r", "jud.judg", "jud.emb", "jud.ups", na.rm = T)
k1_df$sti.score <- scale(k1_df$jud.fam.r) + scale(k1_df$jud.com.r) + scale(k1_df$jud.judg) + scale(k1_df$jud.emb) + scale(k1_df$jud.ups)
k1_df$sti.score.z <- scale(k1_df$sti.score)

## Affect (5-point scale) ##

for (var in c(k1_df$aff.pos, k1_df$aff.ash, k1_df$aff.pow, k1_df$aff.fina)) {

    var[var < 0] <- NA

}

k1_df$aff.pos.s <- k1_df$aff.pos * (5/6)

k1_df$aff.ash.r <- 6 - k1_df$aff.ash
k1_df$aff.fina.r <- 6 - k1_df$aff.fina

k1_df$aff.score.avg <- scale.means(k1_df, "aff.pos.s", "aff.pow", "aff.ash.r", "aff.fina.r", na.rm = T)
k1_df$aff.score <- scale(k1_df$aff.pos) + scale(k1_df$aff.pow) + scale(k1_df$aff.ash.r) + scale(k1_df$aff.fina.r)
k1_df$aff.score.z <- scale(k1_df$aff.score)

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

write.csv(k1_df, file = here("data", "K1_Clean_Data.csv"), na = "")
attach(k1_df)

############################
## Initialize results doc ##
############################

big_border = fp_border(width = 2)
std_border = fp_border(width = 1)

appendix <- read_docx()
appendix <- body_add_par(appendix , "Forecasting data analysis for Kenya recipient experiment (study 2)", style = "heading 1")

#########################
## Forecasting results ##
#########################

#k1_df is the name of the dataframe

k1_df$vid.imp1 <- k1_df$vid.dec1 %in% c(3, 5)
forecast <-subset(k1_df, select = c(vid.imp1, eva.msg1, eva.msg2, eva.msg3,treat,pov,ind,com,eva.conf, survey.id))

#Recoding into proportion selecting a business video for first video

forecast$eva.m1_rec <- eva.msg1/(10)
forecast$eva.m2_rec <- eva.msg2/(10)
forecast$eva.m3_rec <- eva.msg3/(10)

#Reshaping

forecast.long <- forecast %>% gather(WhichTreat, pred, eva.m1_rec, eva.m1_rec,eva.m2_rec,eva.m3_rec)

#Dummies for own condition

forecast.long$Own<-0
forecast.long$Own[forecast.long$WhichTreat == "eva.m1_rec" & forecast.long$pov == 1] <- 1 
forecast.long$Own[forecast.long$WhichTreat == "eva.m2_rec" & forecast.long$ind == 1] <- 1 
forecast.long$Own[forecast.long$WhichTreat == "eva.m3_rec" & forecast.long$com == 1] <- 1

refmeans <- c(mean(forecast.long[which(forecast.long$WhichTreat == "eva.m1_rec"), "pred"]), mean(forecast.long[which(forecast.long$WhichTreat == "eva.m1_rec"), "pred"]), mean(forecast.long[which(forecast.long$WhichTreat == "eva.m3_rec"), "pred"]))

## Without controls ##

FR <- RegTest("pred ~ WhichTreat", clustvars = forecast.long$survey.id, hypotheses = c("WhichTreateva.m2_rec = 0", "WhichTreateva.m3_rec = 0", "WhichTreateva.m2_rec - WhichTreateva.m3_rec = 0"), data = forecast.long)
FR <- cbind(FR[, -ncol(FR)], refmeans, FR[, ncol(FR)])
FR <- cbind(format(round(FR[, -ncol(FR)], 3), nsmall = 3), round(FR[, ncol(FR)], 0))
FR <- cbind(c("Individual = Poverty", "Community = Poverty", "Individual = Community"), FR[, -2])

## With controls ##

FRC <- RegTest("pred ~ WhichTreat + Own", clustvars = forecast.long$survey.id, hypotheses = c("WhichTreateva.m2_rec = 0", "WhichTreateva.m3_rec = 0", "WhichTreateva.m2_rec - WhichTreateva.m3_rec = 0"), data = forecast.long)
FRC <- cbind(FRC[, -ncol(FRC)], refmeans, FRC[, ncol(FRC)])
FRC <- cbind(format(round(FRC[, -ncol(FRC)], 3), nsmall = 3), round(FRC[, ncol(FRC)], 0))
FRC <- cbind(c("Individual = Poverty", "Community = Poverty", "Individual = Community"), FRC[, -2])

## Combine panels ##

FR.tab <- rbind(c(" ", " ", " ", " ", " ", " "), FR, c("Controlling for own treatment assignment", " ", " ", " ", " ", " "), FRC)

colnames(FR.tab) <- c("Hypothesis", "Coefficient", "Std. error", "p-value", "Reference mean", "Obs.")

## Print table ##

appendix <- body_add_par(appendix , " ", style = "Normal") %>%
    body_add_par("Forecasting results", style = "Normal") %>%
    body_add_flextable(value = FTable(FR.tab, panels = 2, note = "Note: The dependent variable is the proportion selecting a business video for first video. Each of the 565 participants made three forecasts for a total of 565 x 3 = 1,695 observations. The first and second panels respectively exclude and include a dummy for own treatment assignment. The first column reports the mean difference between groups. The second column reports robust standard errors. The reference mean column lists the mean of the poverty alleviation condition for the first two hypotheses and the mean of the community empowerment condition for the third hypothesis."))

print(appendix, target = here("doc", "FC_appendix.docx"))

################################
## Forecasting bar graph (2A) ##
################################ 

attach(forecast.long)

WhichTreat <- factor(WhichTreat, labels = c("Poverty \n Alleviation", "Individual \n Empowerment", "Community \n Empowerment"))

for.stats <- forecast.long[complete.cases(pred), ] %>% group_by(WhichTreat) %>% summarise(mean = mean(pred), sd = sd(pred), obs = length(pred))
for.stats <- cbind(as.data.frame(c("Forecast results", "Forecast results", "Forecast results")), as.data.frame(table(WhichTreat))[, 1], as.data.frame(for.stats[, 2]), as.data.frame(for.stats[, 3] / sqrt(for.stats[, 4])))
colnames(for.stats) <- c("type", "treat", "mean", "SE")

attach(k1_df)

exp.stats <- k1_df[complete.cases(vid.imp1), ] %>% group_by(treat) %>% summarise(mean = mean(vid.imp1), sd = sd(vid.imp1), obs = length(vid.imp1))
exp.stats <- cbind(as.data.frame(c("Experimental results", "Experimental results", "Experimental results")), as.data.frame(table(treat))[, 1], as.data.frame(exp.stats[, 2]), as.data.frame(exp.stats[, 3] / sqrt(exp.stats[, 4])))
colnames(exp.stats) <- c("type", "treat", "mean", "SE")

Fig2aData <- rbind(exp.stats, for.stats)

## Plot means ##

Figure2A <- ggplot(Fig2aData, aes(fill=treat, y=mean, x=type)) + 
  geom_bar(position = position_dodge(width=0.9), stat = "identity", width = .7) +
  ggtitle("A. Comparison of forecasts \n to experimental results") +
  geom_errorbar(aes(x = type, ymin = mean - 1.96 * SE, ymax = mean + 1.96 * SE), position = position_dodge(width=0.9), width=.3, size=0.7) +
  scale_fill_manual(values=c('#c6c6c7', '#7ca6c0', '#c05746')) +
  labs(y = "Prop. selecting a business skills video first", x ="") +
  coord_cartesian(ylim=c(0.4, 0.85)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(size = .15, color = "black", linetype = "dotted"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(size=10, face="bold"),
        text = element_text(size=8)) +
        geom_signif(y_position=c(0.82, 0.67, 0.72), xmin=c(0.7, 1.7, 1.7), xmax=c(1.3, 2, 2.3), annotation=c("*", "*", "*"), vjust = 0.2)

###############################
## Histogram of video choice ##
###############################

FigureS1 <- ggplot(k1_df, aes(x= vid.num,  group=condition.order)) + geom_bar(aes(y = ..prop..)) + facet_grid(~condition.order) + geom_text(aes( label = scales::percent(..prop..), y= ..prop.. ), stat= "count", vjust = -.5) + labs(y = "Percent") + guides(fill = FALSE) + xlab("No. of business videos chosen") + scale_y_continuous(labels = scales::percent) + aes(fill = condition.order) + scale_fill_manual(values = c('#c6c6c7', '#7ca6c0', '#c05746')) + theme_minimal()

save_plot(here("graphics", "FigureS1.png"), FigureS1, base_height = 5, base_width = 8, dpi=300)

##################################
## Figures for synthetic pilots ##
##################################

# Load data ####################################################################

  # load("data/k1_df.rda")

################################################################################

# Random sample function #######################################################

  randomSample = function(df,n) {
         return (df[sample(nrow(df), n),])
    }

################################################################################

# For experimental results #####################################################
  simulatorEXP <- function(df, n_sims, sample_n) {

  OneSim<-function(df, sample_n){

     smallerDF<-randomSample(df, sample_n)

     ATEs<-summary(lm(vid.imp1~as.factor(treat), data=smallerDF))$coefficient[2:3,c(1,4)] %>%
       as.vector %>% matrix(ncol = 4) %>% data.frame()

     return(ATEs)
     names(ATEs) <- c("A","B","Ap","Bp")

  }

    # Run sim n_sims times with given parameters
     sims_dt <- lapply(
      X = 1:n_sims,
      FUN = function(i) OneSim(df, sample_n)) %>%
       bind_rows()

    # Return sim_dt
    return(sims_dt)

     }

################################################################################

# For forecasting results######################################################

  simulatorFOR <- function(df, n_sims, sample_n) {

    OneSimFOR<-function(df, sample_n){

      smallerDF<-randomSample(df, sample_n)

      smallerDF.forecast.long <- smallerDF %>% gather(WhichTreat, pred, eva.msg1, eva.msg2, eva.msg3)

      ATEs<-summary(lm(pred~as.factor(WhichTreat),
                       data=smallerDF.forecast.long))$coefficient[2:3,c(1,4)] %>%
        as.vector %>%
        matrix(ncol = 4) %>%
        data.frame()

      return(ATEs)
      names(ATEs) <- c("A","B","Ap","Bp")

    }

    # Run n_sims times with given parameters
    sims_dt <- lapply(
      X = 1:n_sims,
      FUN = function(i) OneSimFOR(df, sample_n)) %>%
      bind_rows()

    # Return sim_dt
    return(sims_dt)

  }

################################################################################

# Simulations ##################################################################

#Recode forecasts:
forecast$eva.msg1<-forecast$eva.msg1/10
forecast$eva.msg2<-forecast$eva.msg2/10
forecast$eva.msg3<-forecast$eva.msg3/10

#Range of sample sizes
minsize=30
maxsize=150

numsims <- 10000

#Experimental
B1_EXP <- matrix(nrow=(maxsize-minsize), ncol=1)
B2_EXP <- matrix(nrow=(maxsize-minsize), ncol=1)
B1p_EXP <- matrix(nrow=(maxsize-minsize), ncol=1)
B2p_EXP <- matrix(nrow=(maxsize-minsize), ncol=1)

for(i in minsize:maxsize){
    TEMP <- simulatorEXP(k1_df, numsims, i)
    B1_EXP[i-(minsize-1)]<-as.vector(TEMP[1])
    B2_EXP[i-(minsize-1)]<-TEMP[2]
    B1p_EXP[i-(minsize-1)]<-TEMP[3]
    B2p_EXP[i-(minsize-1)]<-TEMP[4]
}

B1_EXP_DF= as.data.frame(t(as.data.frame(B1_EXP)))
B2_EXP_DF= as.data.frame(t(as.data.frame(B2_EXP)))
B1p_EXP_DF= as.data.frame(t(as.data.frame(B1p_EXP)))
B2p_EXP_DF= as.data.frame(t(as.data.frame(B2p_EXP)))

#Forecasting
B1_FOR <- matrix(nrow=(maxsize-minsize), ncol=1)
B2_FOR <- matrix(nrow=(maxsize-minsize), ncol=1)
B1p_FOR <- matrix(nrow=(maxsize-minsize), ncol=1)
B2p_FOR <- matrix(nrow=(maxsize-minsize), ncol=1)
for(i in minsize:maxsize){
TEMP <- simulatorFOR(forecast, numsims, i)
B1_FOR[i-(minsize-1)]<-TEMP[1]
B2_FOR[i-(minsize-1)]<-TEMP[2]
B1p_FOR[i-(minsize-1)]<-TEMP[3]
B2p_FOR[i-(minsize-1)]<-TEMP[4]
}

B1_FOR_DF= as.data.frame(t(as.data.frame(B1_FOR)))
B2_FOR_DF= as.data.frame(t(as.data.frame(B2_FOR)))
B1p_FOR_DF= as.data.frame(t(as.data.frame(B1p_FOR)))
B2p_FOR_DF= as.data.frame(t(as.data.frame(B2p_FOR)))

B1_EXP_DF_ERROR<- -abs(B1_EXP_DF-0.0697)
B1_Exp_MeanError<-as.data.frame(rowMeans(B1_EXP_DF_ERROR))

B2_EXP_DF_ERROR<- -abs(B2_EXP_DF-0.1292)
B2_Exp_MeanError<-as.data.frame(rowMeans(B2_EXP_DF_ERROR))

B1_FOR_DF_ERROR<- -abs(B1_FOR_DF-0.0697)
B1_FOR_MeanError<-as.data.frame(rowMeans(B1_FOR_DF_ERROR))

B2_FOR_DF_ERROR<- -abs(B2_FOR_DF-0.1292)
B2_FOR_MeanError<-as.data.frame(rowMeans(B2_FOR_DF_ERROR))

NUM<-c(minsize:maxsize)%>%as.data.frame()

MeanERR<-bind_cols(B1_Exp_MeanError,B2_Exp_MeanError,B1_FOR_MeanError,B2_FOR_MeanError,  c(minsize:maxsize)%>%as.data.frame())
names(MeanERR)<-c("B1_Exp_MeanError","B2_Exp_MeanError","B1_FOR_MeanError","B2_FOR_MeanError","N")

df2c <- melt(MeanERR, id = "N")

  #Figure2B: Negative absolute error

Figure2B <- ggplot(df2c, aes(x = N, y = value, color = variable)) +
    geom_smooth(size = 0.8, se = FALSE, aes(linetype = variable)) +
    scale_linetype_manual(values = c("solid","solid", "dashed", "dashed"), name="Coefficients (Ref: Poverty Alleviation)",
                          labels=c("A/B pilot: Ind. Empowerment",
                                   "A/B pilot: Com. Empowerment",
                                   "Forecasting: Ind. Empowerment",
                                   "Forecasting: Com. Empowerment")) +
    scale_color_manual(values=c('#7ca6c0', '#c05746','#7ca6c0', '#c05746'),
                       name="Coefficients (Ref: Poverty Alleviation)",
                       labels=c("A/B pilot: Ind. Empowerment",
                                "A/B pilot: Com. Empowerment",
                                "Forecasting: Ind. Empowerment",
                                "Forecasting: Com. Empowerment")) +
    ggtitle("B. Accuracy of forecasts versus A/B pilots") +
    xlab("Bootstrap sample size across all groups") +
    ylab("Prediction accuracy (negative absolute error)") +
    scale_x_continuous(breaks=seq(25, 150, 25)) +
    coord_cartesian(xlim=c(25, 150), ylim=c(-0.2,0)) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.grid.major.y = element_line(size = .5, color = "gray", linetype = "dashed"),
          axis.line = element_line(colour = "black"),
          axis.text.x = element_text(size = 10),
          legend.position = c(0.8, 0.15),
          plot.title = element_text(size=10, face="bold"),
          text = element_text(size=8)) +
    guides(fill = guide_legend(keywidth = 1, keyheight = 1),
           linetype=guide_legend(keywidth = 1.5, keyheight = 0.7),
           colour=guide_legend(keywidth = 1.5, keyheight = 0.7))

Figure2 <- plot_grid(Figure2A, Figure2B, nrow = 1, ncol = 2, rel_widths = c(1, 1.61803398875))
save_plot(here("graphics", "Figure2.png"), Figure2, base_height = 4, base_width = 8, dpi=300)

#Figure S2

#B1 Experimental
B1_EXP_DF_T<-bind_cols(as.data.frame(B1_EXP),rep(c("B1_EXP"), times = numsims)%>%as.data.frame)

names(B1_EXP_DF_T)<-c(sprintf("N%d", 30:150),"VAR")

B1_EXP_DF_T <- reshape(data=B1_EXP_DF_T, idvar="SIZE",
                          varying = c(sprintf("N%d", 30:150)),
                          v.names = c("COEF"),
                          times = c(sprintf("N%d", 30:150)),
                                               direction = "long")
#B2 Experimental
B2_EXP_DF_T<-bind_cols(as.data.frame(B2_EXP),rep(c("B2_EXP"), times = numsims)%>%as.data.frame)

names(B2_EXP_DF_T)<-c(sprintf("N%d", 30:150),"VAR")

B2_EXP_DF_T <- reshape(data=B2_EXP_DF_T, idvar="SIZE",
                       varying = c(sprintf("N%d", 30:150)),
                       v.names = c("COEF"),
                       times = c(sprintf("N%d", 30:150)),
                       direction = "long")

#B1 Forecasting
B1_FOR_DF_T<-bind_cols(as.data.frame(B1_FOR),rep(c("B1_FOR"), times = numsims)%>%as.data.frame)

names(B1_FOR_DF_T)<-c(sprintf("N%d", 30:150),"VAR")

B1_FOR_DF_T <- reshape(data=B1_FOR_DF_T, idvar="SIZE",
                       varying = c(sprintf("N%d", 30:150)),
                       v.names = c("COEF"),
                       times = c(sprintf("N%d", 30:150)),
                       direction = "long")

#B2 Forecasting
B2_FOR_DF_T<-bind_cols(as.data.frame(B2_FOR),rep(c("B2_FOR"), times = numsims)%>%as.data.frame)

names(B2_FOR_DF_T)<-c(sprintf("N%d", 30:150),"VAR")

B2_FOR_DF_T <- reshape(data=B2_FOR_DF_T, idvar="SIZE",
                       varying = c(sprintf("N%d", 30:150)),
                       v.names = c("COEF"),
                       times = c(sprintf("N%d", 30:150)),
                       direction = "long")

#Create data set
DenDat<-bind_rows(B1_EXP_DF_T,B2_EXP_DF_T,B1_FOR_DF_T,B2_FOR_DF_T%>%as.data.frame)

#Create data set
  DenDatN50B1<-DenDat[ which(DenDat$time=='N50'
                             & (DenDat$VAR=='B1_EXP' | DenDat$VAR=='B1_FOR')  ), ]
  DenDatN50B2 <-DenDat[ which(DenDat$time=='N50'
                              & (DenDat$VAR=='B2_EXP' | DenDat$VAR=='B2_FOR')  ), ]
  DenDatN100B1<-DenDat[ which(DenDat$time=='N100'
                             & (DenDat$VAR=='B1_EXP' | DenDat$VAR=='B1_FOR')  ), ]
  DenDatN100B2 <-DenDat[ which(DenDat$time=='N100'
                              & (DenDat$VAR=='B2_EXP' | DenDat$VAR=='B2_FOR')  ), ]
  DenDatN150B1<-DenDat[ which(DenDat$time=='N150'
                              & (DenDat$VAR=='B1_EXP' | DenDat$VAR=='B1_FOR')  ), ]
  DenDatN150B2 <-DenDat[ which(DenDat$time=='N150'
                               & (DenDat$VAR=='B2_EXP' | DenDat$VAR=='B2_FOR')  ), ]
 
####

#B1
  PanelA <- ggplot(DenDatN50B1,
       aes(COEF, fill = as.factor(VAR))) +
       geom_vline(xintercept = 0.06970, size=.9)+
       ggtitle("N=50") +
  ylab("") +
  theme_classic(base_size = 12) +
  scale_x_continuous(limits=c(-.6, .6),name='') +
  coord_cartesian(ylim=c(0,20)) +
  geom_density(alpha = 1/4) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size=12, face="bold", hjust=-0.5),
        legend.position = "none")
 
 
  PanelC <- ggplot(DenDatN100B1,
         aes(COEF, fill = as.factor(VAR))) +
    geom_vline(xintercept = 0.06970, size=.9)+
    ggtitle("N=100") +
    ylab("") +
    theme_classic(base_size = 12) +
    scale_x_continuous(limits=c(-.6, .6),name='') +
    coord_cartesian(ylim=c(0,20)) +
    geom_density(alpha = 1/4) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          plot.title = element_text(size=12, face="bold", hjust=-0.5),
          legend.position = "none")
 
  PanelE <- ggplot(DenDatN150B1,
         aes(COEF, fill = as.factor(VAR))) +
    geom_vline(xintercept = 0.06970, size=.9)+
    ggtitle("N=150") +
    ylab("") +
    theme_classic(base_size = 12) +
    scale_x_continuous(limits=c(-.6, .6),name='') +
    coord_cartesian(ylim=c(0,20)) +
    geom_density(alpha = 1/4) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          plot.title = element_text(size=12, face="bold", hjust=-0.5),
          legend.position = "none")
 
#B2
 
  PanelB <- ggplot(DenDatN50B2,
         aes(COEF, fill = as.factor(VAR))) +
    geom_vline(xintercept = 0.12916, size=.9)+
    ylab("") +
    theme_classic(base_size = 12) +
    scale_x_continuous(limits=c(-.6, .6),name='') +
    coord_cartesian(ylim=c(0,20)) +
    geom_density(alpha = 1/4) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.position = "none")
 
  PanelD <- ggplot(DenDatN100B2,
         aes(COEF, fill = as.factor(VAR))) +
    geom_vline(xintercept = 0.12916, size=.9)+
    ylab("") +
    theme_classic(base_size = 12) +
    scale_x_continuous(limits=c(-.6, .6),name='') +
    coord_cartesian(ylim=c(0,20)) +
    geom_density(alpha = 1/4) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.position = "none")
 
  PanelF <- ggplot(DenDatN150B2,
         aes(COEF, fill = as.factor(VAR))) +
    geom_vline(xintercept = 0.12916, size=.9)+
    ylab("") +
    theme_classic(base_size = 12) +
    scale_x_continuous(limits=c(-.6, .6),name='') +
    coord_cartesian(ylim=c(0,20)) +
    geom_density(alpha = 1/4) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.position = "none")

FigureS2 <- plot_grid(PanelA, PanelB, PanelC, PanelD, PanelE, PanelF, nrow = 3, ncol = 2, labels = c("Individual Empowerment", "Community Empowerment"), scale = 0.8, hjust = -0.2, align="hv")
save_plot(here("graphics", "FigureS2.png"), FigureS2, base_height = 9, base_width = 6, dpi=300)