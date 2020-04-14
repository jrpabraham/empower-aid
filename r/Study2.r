# Author: Justin Abraham
# R Version: R-3.6.2
# RStudio Version: 1.2.5033

###############################
## Install required packages ##
###############################

set.seed(47269801)

if (!require("pacman")) {
    install.packages("pacman", repos='https://cloud.r-project.org/')
    library("pacman")
}

p_load("here", "tidyr", "dplyr", "lmtest", "multiwayvcov", "multcomp", "reshape2", "knitr", "flextable", "officer", "forestplot", "cowplot", "ggplot2", "matrixStats", "ggthemes", "ggsignif", "rstudioapi", "iptools", "ggpubr")

source(here("r", "Funs.r"))

load(here("data", "KenyaData.RData"))

attach(k1_df)

################################
## Create forecasting dataset ##
################################

k1_df$vid.imp1 <- k1_df$vid.dec1 %in% c(3, 5)

forecast <-subset(k1_df, select = c(vid.imp1, eva.msg1, eva.msg2, eva.msg3,treat,pov,ind,com,eva.conf, survey.id))

#Recoding into proportion selecting a business video for first video

forecast$eva.m1_rec <- eva.msg1/(10)
forecast$eva.m2_rec <- eva.msg2/(10)
forecast$eva.m3_rec <- eva.msg3/(10)

#Reshaping

forecast.long <- forecast %>% gather(WhichTreat, pred, eva.m1_rec, eva.m1_rec,eva.m2_rec,eva.m3_rec)

#Dummies for own condition

forecast.long$Own <- 0
forecast.long$Own[forecast.long$WhichTreat == "eva.m1_rec" & forecast.long$pov == 1] <- 1
forecast.long$Own[forecast.long$WhichTreat == "eva.m2_rec" & forecast.long$ind == 1] <- 1
forecast.long$Own[forecast.long$WhichTreat == "eva.m3_rec" & forecast.long$com == 1] <- 1

############################
## Forecasting regression ##
############################

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

appendix <- read_docx()

WriteHeading(appendix , "Forecasting data analysis for Kenya recipient experiment (study 2)")
LineBreak(appendix)
WriteTitle(appendix, "Forecasting results")

appendix <- body_add_flextable(appendix, value = FTable(FR.tab, panels = 2, note = "Note: The dependent variable is the proportion selecting a business video for first video. Each of the 565 participants made three forecasts for a total of 565 x 3 = 1,695 observations. The first and second panels respectively exclude and include a dummy for own treatment assignment. The first column reports the mean difference between groups. The second column reports robust standard errors. The reference mean column lists the mean of the poverty alleviation condition for the first two hypotheses and the mean of the community empowerment condition for the third hypothesis."))

print(appendix, target = here("doc", "S2_appendix.docx"))

################################
## Forecasting bar graph (2A) ##
################################ 

attach(forecast.long)

WhichTreat <- factor(WhichTreat, labels = c("Poverty \n Alleviation", "Individual \n Empowerment", "Community \n Empowerment"))

for.stats <- forecast.long[complete.cases(pred), ] %>% group_by(WhichTreat) %>% summarise(mean = mean(pred), sd = sd(pred), obs = length(pred))
for.stats <- cbind(as.data.frame(c("Forecast results", "Forecast results", "Forecast results")), as.data.frame(table(WhichTreat))[, 1], as.data.frame(for.stats[, 2]), as.data.frame(for.stats[, 3] / sqrt(for.stats[, 4])))
colnames(for.stats) <- c("type", "treat", "mean", "SE")

attach(k1_df)

k1_df$treat.long <- factor(treat, labels = c("Poverty \n Alleviation", "Individual \n Empowerment", "Community \n Empowerment"))

exp.stats <- k1_df[complete.cases(vid.imp1), ] %>% group_by(k1_df$treat.long) %>% summarise(mean = mean(vid.imp1), sd = sd(vid.imp1), obs = length(vid.imp1))
exp.stats <- cbind(as.data.frame(c("Experimental results", "Experimental results", "Experimental results")), as.data.frame(table(k1_df$treat.long))[, 1], as.data.frame(exp.stats[, 2]), as.data.frame(exp.stats[, 3] / sqrt(exp.stats[, 4])))
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

FigureS1 <- ggplot(k1_df, aes(x= vid.num,  group=condition.order)) + geom_bar(aes(y = ..prop..)) + facet_grid(~condition.order) + geom_text(aes( label = scales::percent(..prop..), y= ..prop.. ), stat= "count", vjust = -.5) + labs(y = "Percent") + guides(fill = FALSE) + xlab("No. of business videos chosen") + scale_y_continuous(labels = scales::percent) + aes(fill = condition.order) + scale_fill_manual(values = c('#c6c6c7', '#7ca6c0', '#c05746')) + theme_bw()

save_plot(here("graphics", "FigureS1.png"), FigureS1, base_height = 5, base_width = 8, dpi=3120)

##################################
## Figures for synthetic pilots ##
##################################

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
    TEMP <- Simulator(k1_df, iterations = numsims, n = i, by = k1_df$survey.id, "vid.imp1", "treat")
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
TEMP <- Simulator(forecast.long, iterations = numsims, n = i, by = forecast.long$survey.id, "pred", "WhichTreat")
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
                          labels=c("Sim. pilot: Ind. Empowerment",
                                   "Sim. pilot: Com. Empowerment",
                                   "Forecasting: Ind. Empowerment",
                                   "Forecasting: Com. Empowerment")) +
    scale_color_manual(values=c('#7ca6c0', '#c05746','#7ca6c0', '#c05746'),
                       name="Coefficients (Ref: Poverty Alleviation)",
                       labels=c("Sim. pilot: Ind. Empowerment",
                                "Sim. pilot: Com. Empowerment",
                                "Forecasting: Ind. Empowerment",
                                "Forecasting: Com. Empowerment")) +
    ggtitle("B. Accuracy of forecasts versus simulated experimental pilots") +
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
save_plot(here("graphics", "Figure2.png"), Figure2, base_height = 4, base_width = 8, dpi=3120)

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
 
#Generate figure S2
FigTheme <- function ()
{
 theme(panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(),
       panel.background = element_blank(),
       axis.line = element_line(colour = "black"),
       axis.text.x = element_text(size = 11),
       plot.title = element_text(size=11, face="bold"),
       text = element_text(size=11),
       legend.position = "none")
}

#B1

PanelA <- ggplot(DenDatN50B1,
                aes(COEF, fill = as.factor(VAR))) +
 geom_vline(xintercept = 0.06970, size=.9)+
 labs(subtitle="N=50") +
 ylab("")+xlab("") +xlim(-.6,.6)+
 coord_cartesian(ylim = c(0, 20))+
 geom_density(alpha = 1/4) + FigTheme()

PanelC <- ggplot(DenDatN100B1,
                aes(COEF, fill = as.factor(VAR))) +
 geom_vline(xintercept = 0.06970, size=.9)+
 labs(subtitle="N=100") +
 ylab("")+xlab("") +xlim(-.6,.6)+ coord_cartesian(ylim = c(0, 20))+
 geom_density(alpha = 1/4) + FigTheme()

PanelE <- ggplot(DenDatN150B1,
                aes(COEF, fill = as.factor(VAR))) +
 geom_vline(xintercept = 0.06970, size=.9)+
 labs(subtitle="N=150") +
 ylab("")+xlab("Individual Empowerment coefs.")+xlim(-.6,.6)+
 coord_cartesian(ylim = c(0, 20))+
 geom_density(alpha = 1/4) + FigTheme()

#B2

PanelB <- ggplot(DenDatN50B2,
                aes(COEF, fill = as.factor(VAR))) +
 geom_vline(xintercept = 0.12916, size=.9)+
 ylab("")+xlab("") +xlim(-.6,.6)+
 coord_cartesian(ylim = c(0, 20))+  
 labs(subtitle="") +
 geom_density(alpha = 1/4) + FigTheme()

PanelD <- ggplot(DenDatN100B2,
                aes(COEF, fill = as.factor(VAR))) +
 geom_vline(xintercept = 0.12916, size=.9)+
 ylab("")+xlab("") +xlim(-.6,.6)+
 coord_cartesian(ylim = c(0, 20))+  
 labs(subtitle="") +
 geom_density(alpha = 1/4) + FigTheme()

PanelF <- ggplot(DenDatN150B2,
                aes(COEF, fill = as.factor(VAR))) +
 geom_vline(xintercept = 0.12916, size=.9)+
 ylab("")+xlab("Community Empowerment coefs.") +xlim(-.6,.6)+
 coord_cartesian(ylim = c(0, 20))+  
 labs(subtitle="") +
 geom_density(alpha = 1/4) + FigTheme()

FigureS2<-plot_grid(ggarrange(PanelA,
                   PanelC,
                   PanelE,ncol=1),
         ggarrange(PanelB,
                   PanelD,
                   PanelF,ncol=1))

save_plot(here("graphics", "FigureS2.png"), FigureS2, base_height = 9, base_width = 6, dpi=1200)