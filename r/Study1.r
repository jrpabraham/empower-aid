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

load(here("data", "KenyaData.RData"))

attach(k1_df)

############################
## Initialize results doc ##
############################

appendix <- read_docx()
appendix <- body_add_par(appendix , "Pre-specified data analysis for Kenya recipient experiment (study 1)", style = "heading 1")

########################
## Summary statistics ##
########################

SumTable <- SumStats(varlist = c("soc.fem", "soc.pri", "soc.age", "ses.unemp", "soc.sav", "soc.con", "soc.inc"), labels = c("Female", "Completed std. 8", "Age", "Unemployed", "Holds savings", "Consumption (Ksh.)", "Income (Ksh.)"), data = k1_df)

## Print table to doc ##

big_border <- fp_border(width = 2)
std_border <- fp_border(width = 1)

appendix <- body_add_par(appendix , " ", style = "Normal") %>%
    body_add_par("Summary of sample sociodemographic characteristics for study 1", style = "Normal")

ftable <- regulartable(as.data.frame(SumTable)) %>%
    align(align = "center") %>%
    align(j = 1, part = "body", align = "left") %>%
    border_remove() %>%
    hline_top(part = "all", border = big_border) %>%
    hline_top(part = "body", border = big_border) %>%
    hline_bottom(part = "body", border = big_border) %>%
    autofit()

appendix <- body_add_flextable(appendix, value = ftable)

##################################
## Randomization balance checks ##
##################################

hypotheses <- c("treatInd = 0", "treatCom = 0", "treatInd - treatCom = 0")
htitle <- c("Individual - Poverty", "Community - Poverty", "Individual - Community")
depvars <- c("soc.fem", "soc.pri", "soc.age", "ses.unemp")
depvarnames <- c("Female", "Completed std. 8", "Age", "Unemployed")

RES.print <- matrix(nrow = 1, ncol = 9)

for (i in 1:3) {

    RES.bal <- matrix(nrow = 1, ncol = 6)

    for (depvar in depvars) {

        eqn <- paste(depvar, "~ treat", sep = " ")
        RES.bal <- rbind(RES.bal, PermTest(eqn, treatvars = c("treat", "pov", "ind", "com"), clustvars = k1_df$survey.id, hypotheses = hypotheses[i], iterations = 10, data = k1_df))

    }

    RES.bal <- cbind(RES.bal[2:nrow(RES.bal), 1:ncol(RES.bal)], FDR(RES.bal[2:nrow(RES.bal), 4]))

    if (hypotheses[i] == "treatInd - treatCom = 0") {
        RES.bal <- cbind(RES.bal, colMeans(k1_df[which(k1_df$com == 1), depvars], na.rm = TRUE))
    } else RES.bal <- cbind(RES.bal, colMeans(k1_df[which(k1_df$pov == 1), depvars], na.rm = TRUE))

    RES.bal[, 5] <- round(RES.bal[, 5], 0)
    RES.bal[, -5] <- format(round(RES.bal[, -5], 3), nsmall = 3)

    RES.bal <- cbind(depvarnames, RES.bal)

    colnames(RES.bal)[1] <- "Outcome"
    colnames(RES.bal)[8] <- "Min. q-value"
    colnames(RES.bal)[9] <- "Reference mean"

    print("----------------------------------------------------------------", quote = FALSE)
    print(paste("H_0:", htitle[i]), quote = FALSE)
    print(RES.bal, quote = FALSE)

    RES.bal <- rbind(c(htitle[i], "", "", "", "", "", "", "", ""), RES.bal)
    RES.print <- rbind(RES.print, RES.bal)

}

RES.print[, c(1:9)] <- RES.print[, c(1, 2, 3, 4, 5, 7, 8, 9, 6)]
colnames(RES.print)[c(1:9)] <- colnames(RES.print)[c(1, 2, 3, 4, 5, 7, 8, 9, 6)]

## Print table to doc ##

appendix <- body_add_par(appendix , " ", style = "Normal") %>%
    body_add_par("Balance checks on subject demographic characteristics for study 1", style = "Normal") %>%
    body_add_flextable(value = FTable(RES.print[2:nrow(RES.print), c(1:2,4:9)], note = "Note: Each panel corresponds to a single hypothesis for the group of outcome variables. The first column reports the mean difference between groups. The second column reports robust standard errors. The third column reports standard p-values. The fourth column reports exact p-values from randomization inference. The fifth column reports the minimum q-values. FDR correction is applied over all outcomes within a hypothesis. The reference mean column lists the mean of the poverty alleviation condition for the first two panels and the mean of the community empowerment condition for the third panel."))

###########################################
## Equivalence test for primary outcomes ##
###########################################

hypotheses <- c("treatInd = 0", "treatCom = 0", "treatInd - treatCom = 0")
htitle <- c("Individual - Poverty", "Community - Poverty", "Individual - Community")
depvars <- c("vid.num", "sav.amt", "msg.dec")
depvarnames <- c("No. of videos", "Amount saved", "Recorded message")

RES.print <- matrix(nrow = 1, ncol = 9)

for (i in 1:3) {

    RES.eq <- matrix(nrow = 1, ncol = 6)

    for (depvar in depvars) {

        firststage <- paste(depvar, "~ soc.fem + soc.age + soc.pri + ses.unemp", sep = " ")
        predicted <- predict(lm(firststage, data = k1_df, na.action = na.omit))

        secondstage <- paste("predicted", "~ treat", sep = " ")
        RES.eq <- rbind(RES.eq, PermTest(secondstage, treatvars = c("treat", "pov", "ind", "com"), clustvars = k1_df[complete.cases(soc.fem, soc.age, soc.pri, ses.unemp), "survey.id"], hypotheses = hypotheses[i], iterations = 10, data = k1_df[complete.cases(soc.fem, soc.age, soc.pri, ses.unemp), ]))

    }

    RES.eq <- cbind(RES.eq[2:nrow(RES.eq), 1:ncol(RES.eq)], FDR(RES.eq[2:nrow(RES.eq), 4]))

    if (hypotheses[i] == "treatInd - treatCom = 0") {
        RES.eq <- cbind(RES.eq, colMeans(k1_df[which(k1_df$com == 1), depvars], na.rm = TRUE))
    } else RES.eq <- cbind(RES.eq, colMeans(k1_df[which(k1_df$pov == 1), depvars], na.rm = TRUE))

    RES.eq[, 5] <- round(RES.eq[, 5], 0)
    RES.eq[, -5] <- format(round(RES.eq[, -5], 3), nsmall = 3)

    RES.eq <- cbind(depvarnames, RES.eq)

    colnames(RES.eq)[1] <- "Outcome"
    colnames(RES.eq)[8] <- "Min. q-value"
    colnames(RES.eq)[9] <- "Reference mean"

    print("----------------------------------------------------------------", quote = FALSE)
    print(paste("H_0:", htitle[i]), quote = FALSE)
    print(RES.eq, quote = FALSE)

    RES.eq <- rbind(c(htitle[i], "", "", "", "", "", "", "", ""), RES.eq)
    RES.print <- rbind(RES.print, RES.eq)

}

RES.print[, c(1:9)] <- RES.print[, c(1, 2, 3, 4, 5, 7, 8, 9, 6)]
colnames(RES.print)[c(1:9)] <- colnames(RES.print)[c(1, 2, 3, 4, 5, 7, 8, 9, 6)]

## Print table to doc ##

appendix <- body_add_par(appendix , " ", style = "Normal") %>%
    body_add_par("Equivalence tests on primary outcomes for study 1", style = "Normal") %>%
    body_add_flextable(value = FTable(RES.print[2:nrow(RES.print), c(1:2,4:9)], note = "Note: This table computes predicted values of outcomes as a function of baseline covariates and regresses the predictions on the treatment indicators. Each panel corresponds to a single hypothesis comparing the treatment conditions for the group of predicted outcome variables. The first column reports the mean difference between groups. The second column reports robust standard errors. The third column reports standard p-values. The fourth column reports exact p-values from randomization inference. The fifth column reports the minimum q-values. FDR correction is applied over all outcomes within a hypothesis. The reference mean column lists the mean of the poverty alleviation condition for the first two panels and the mean of the community empowerment condition for the third panel."))

####################################
## Plain OLS for primary outcomes ##
####################################

hypotheses <- c("treatInd = 0", "treatCom = 0", "treatInd - treatCom = 0")
htitle <- c("Individual - Poverty", "Community - Poverty", "Individual - Community")
depvars <- c("vid.num", "sav.amt", "msg.dec")
depvarnames <- c("No. of videos", "Amount saved", "Recorded message")

RES.print <- matrix(nrow = 1, ncol = 9)

for (i in 1:3) {

    RES.pri <- matrix(nrow = 1, ncol = 6)

    for (depvar in depvars) {

        eqn <- paste(depvar, "~ treat", sep = " ")
        RES.pri <- rbind(RES.pri, PermTest(eqn, treatvars = c("treat", "pov", "ind", "com"), clustvars = k1_df$survey.id, hypotheses = hypotheses[i], iterations = 10, data = k1_df))

    }

    RES.pri <- cbind(RES.pri[2:nrow(RES.pri), 1:ncol(RES.pri)], FDR(RES.pri[2:nrow(RES.pri), 4]))

    if (hypotheses[i] == "treatInd - treatCom = 0") {
        RES.pri <- cbind(RES.pri, colMeans(k1_df[which(k1_df$com == 1), depvars], na.rm = TRUE))
    } else RES.pri <- cbind(RES.pri, colMeans(k1_df[which(k1_df$pov == 1), depvars], na.rm = TRUE))

    RES.pri[, 5] <- round(RES.pri[, 5], 0)
    RES.pri[, -5] <- format(round(RES.pri[, -5], 3), nsmall = 3)

    RES.pri <- cbind(depvarnames, RES.pri)

    colnames(RES.pri)[1] <- "Outcome"
    colnames(RES.pri)[8] <- "Min. q-value"
    colnames(RES.pri)[9] <- "Reference mean"

    print("----------------------------------------------------------------", quote = FALSE)
    print(paste("H_0:", htitle[i]), quote = FALSE)
    print(RES.pri, quote = FALSE)

    RES.pri <- rbind(c(htitle[i], "", "", "", "", "", "", "", ""), RES.pri)
    RES.print <- rbind(RES.print, RES.pri)

}

RES.print[, c(1:9)] <- RES.print[, c(1, 2, 3, 4, 5, 7, 8, 9, 6)]
colnames(RES.print)[c(1:9)] <- colnames(RES.print)[c(1, 2, 3, 4, 5, 7, 8, 9, 6)]

## Print table to doc ##

appendix <- body_add_par(appendix , " ", style = "Normal") %>%
    body_add_par("Treatment effects on primary outcomes for study 1", style = "Normal") %>%
    body_add_flextable(value = FTable(RES.print[2:nrow(RES.print), c(1:2,4:9)], note = "Note: Each panel corresponds to a single hypothesis comparing the treatment conditions for the group of outcome variables. The first column reports the mean difference between groups. The second column reports robust standard errors. The third column reports standard p-values. The fourth column reports exact p-values from randomization inference. The fifth column reports the minimum q-values. FDR correction is applied over all outcomes within a hypothesis. The reference mean column lists the mean of the poverty alleviation condition for the first two panels and the mean of the community empowerment condition for the third panel."))

######################################
## Plain OLS for secondary outcomes ##
######################################

hypotheses <- c("treatInd = 0", "treatCom = 0", "treatInd - treatCom = 0")
htitle <- c("Individual - Poverty", "Community - Poverty", "Individual - Community")
depvars <- c("sel.score.avg", "sti.score.avg", "aff.score.avg", "ses.lad.now", "ses.lad.y2", "msg.avg", "que.smrd")
depvarnames <- c("Self-Efficacy (avg.)", "Stigma (avg.)", "Affect (avg.)", "Social status", "Anticipated social mobility", "Message support", "Query ordering")

RES.print <- matrix(nrow = 1, ncol = 9)

for (i in 1:3) {

    RES.sec <- matrix(nrow = 1, ncol = 6)

    for (depvar in depvars) {

        eqn <- paste(depvar, "~ treat", sep = " ")
        RES.sec <- rbind(RES.sec, PermTest(eqn, treatvars = c("treat", "pov", "ind", "com"), clustvars = k1_df$survey.id, hypotheses = hypotheses[i], iterations = 10, data = k1_df))

    }

    RES.sec <- cbind(RES.sec[2:nrow(RES.sec), 1:ncol(RES.sec)], FDR(RES.sec[2:nrow(RES.sec), 4]))

    if (hypotheses[i] == "treatInd - treatCom = 0") {
        RES.sec <- cbind(RES.sec, colMeans(k1_df[which(k1_df$com == 1), depvars], na.rm = TRUE))
    } else RES.sec <- cbind(RES.sec, colMeans(k1_df[which(k1_df$pov == 1), depvars], na.rm = TRUE))

    RES.sec[, 5] <- round(RES.sec[, 5], 0)
    RES.sec[, -5] <- format(round(RES.sec[, -5], 3), nsmall = 3)

    RES.sec <- cbind(depvarnames, RES.sec)

    colnames(RES.sec)[1] <- "Outcome"
    colnames(RES.sec)[8] <- "Min. q-value"
    colnames(RES.sec)[9] <- "Reference mean"

    print("----------------------------------------------------------------", quote = FALSE)
    print(paste("H_0:", htitle[i]), quote = FALSE)
    print(RES.sec, quote = FALSE)

    RES.sec <- rbind(c(htitle[i], "", "", "", "", "", "", "", ""), RES.sec)
    RES.print <- rbind(RES.print, RES.sec)

}

RES.print[, c(1:9)] <- RES.print[, c(1, 2, 3, 4, 5, 7, 8, 9, 6)]
colnames(RES.print)[c(1:9)] <- colnames(RES.print)[c(1, 2, 3, 4, 5, 7, 8, 9, 6)]

## Print table to doc ##

appendix <- body_add_par(appendix , " ", style = "Normal") %>%
    body_add_par("Treatment effects on secondary outcomes for study 1", style = "Normal") %>%
    body_add_flextable(value = FTable(RES.print[2:nrow(RES.print), c(1:2,4:9)], note = "Note: Each panel corresponds to a single hypothesis comparing the treatment conditions for the group of outcome variables. The first column reports the mean difference between groups. The second column reports robust standard errors. The third column reports standard p-values. The fourth column reports exact p-values from randomization inference. The fifth column reports the minimum q-values. FDR correction is applied over all outcomes within a hypothesis. The reference mean column lists the mean of the poverty alleviation condition for the first two panels and the mean of the community empowerment condition for the third panel."))


###############################################
## Covariate adjustment for primary outcomes ##
###############################################

hypotheses <- c("treatInd = 0", "treatCom = 0", "treatInd - treatCom = 0")
htitle <- c("Individual - Poverty", "Community - Poverty", "Individual - Community")
depvars <- c("vid.num", "sav.amt", "msg.dec")
depvarnames <- c("No. of videos", "Amount saved", "Recorded message")
covariates <- c("soc.fem.c", "soc.pri.c", "soc.age.c", "ses.unemp.c")

RES.print <- matrix(nrow = 1, ncol = 9)

for (i in 1:3) {

    RES.pri.cov <- matrix(nrow = 1, ncol = 6)

    for (depvar in depvars) {

        eqn <- paste(depvar, "~", Interact("treat", covariates), sep = " ")
        RES.pri.cov <- rbind(RES.pri.cov, PermTest(eqn, treatvars = c("treat", "pov", "ind", "com"), clustvars = k1_df$survey.id, hypotheses = hypotheses[i], iterations = 10, data = k1_df))

    }

    RES.pri.cov <- cbind(RES.pri.cov[2:nrow(RES.pri.cov), 1:ncol(RES.pri.cov)], FDR(RES.pri.cov[2:nrow(RES.pri.cov), 4]))

    if (hypotheses[i] == "treatInd - treatCom = 0") {
        RES.pri.cov <- cbind(RES.pri.cov, colMeans(k1_df[which(k1_df$com == 1), depvars], na.rm = TRUE))
    } else RES.pri.cov <- cbind(RES.pri.cov, colMeans(k1_df[which(k1_df$pov == 1), depvars], na.rm = TRUE))

    RES.pri.cov[, 5] <- round(RES.pri.cov[, 5], 0)
    RES.pri.cov[, -5] <- format(round(RES.pri.cov[, -5], 3), nsmall = 3)

    RES.pri.cov <- cbind(depvarnames, RES.pri.cov)

    colnames(RES.pri.cov)[1] <- "Outcome"
    colnames(RES.pri.cov)[8] <- "Min. q-value"
    colnames(RES.pri.cov)[9] <- "Reference mean"

    print("----------------------------------------------------------------", quote = FALSE)
    print(paste("H_0:", htitle[i]), quote = FALSE)
    print(RES.pri.cov, quote = FALSE)

    RES.pri.cov <- rbind(c(htitle[i], "", "", "", "", "", "", "", ""), RES.pri.cov)
    RES.print <- rbind(RES.print, RES.pri.cov)

}

RES.print[, c(1:9)] <- RES.print[, c(1, 2, 3, 4, 5, 7, 8, 9, 6)]
colnames(RES.print)[c(1:9)] <- colnames(RES.print)[c(1, 2, 3, 4, 5, 7, 8, 9, 6)]

## Print table to doc ##

appendix <- body_add_par(appendix , " ", style = "Normal") %>%
    body_add_par("Treatment effects on primary outcomes with covariate adjustment for study 1", style = "Normal") %>%
    body_add_flextable(value = FTable(RES.print[2:nrow(RES.print), c(1:2,4:9)], note = "Note: We include as control variables indicators for being female, for having completed primary schooling, above median age, and unemployment status. Each panel corresponds to a single hypothesis comparing the treatment conditions for the group of outcome variables. The first column reports the mean difference between groups. The second column reports robust standard errors. The third column reports standard p-values. The fourth column reports exact p-values from randomization inference. The fifth column reports the minimum q-values. FDR correction is applied over all outcomes within a hypothesis. The reference mean column lists the mean of the poverty alleviation condition for the first two panels and the mean of the community empowerment condition for the third panel."))

#################################################
## Covariate adjustment for secondary outcomes ##
#################################################

hypotheses <- c("treatInd = 0", "treatCom = 0", "treatInd - treatCom = 0")
htitle <- c("Individual - Poverty", "Community - Poverty", "Individual - Community")
depvars <- c("sel.score.avg", "sti.score.avg", "aff.score.avg", "ses.lad.now", "ses.lad.y2", "msg.avg", "que.smrd")
depvarnames <- c("Self-Efficacy (avg.)", "Stigma (avg.)", "Affect (avg.)", "Social status", "Anticipated social mobility", "Message support", "Query ordering")
covariates <- c("soc.fem.c", "soc.pri.c", "soc.age.c", "ses.unemp.c")

RES.print <- matrix(nrow = 1, ncol = 9)

for (i in 1:3) {

    RES.sec.cov <- matrix(nrow = 1, ncol = 6)

    for (depvar in depvars) {

        eqn <- paste(depvar, "~", Interact("treat", covariates), sep = " ")
        RES.sec.cov <- rbind(RES.sec.cov, PermTest(eqn, treatvars = c("treat", "pov", "ind", "com"), clustvars = k1_df$survey.id, hypotheses = hypotheses[i], iterations = 10, data = k1_df))

    }

    RES.sec.cov <- cbind(RES.sec.cov[2:nrow(RES.sec.cov), 1:ncol(RES.sec.cov)], FDR(RES.sec.cov[2:nrow(RES.sec.cov), 4]))

    if (hypotheses[i] == "treatInd - treatCom = 0") {
        RES.sec.cov <- cbind(RES.sec.cov, colMeans(k1_df[which(k1_df$com == 1), depvars], na.rm = TRUE))
    } else RES.sec.cov <- cbind(RES.sec.cov, colMeans(k1_df[which(k1_df$pov == 1), depvars], na.rm = TRUE))

    RES.sec.cov[, 5] <- round(RES.sec.cov[, 5], 0)
    RES.sec.cov[, -5] <- format(round(RES.sec.cov[, -5], 3), nsmall = 3)

    RES.sec.cov <- cbind(depvarnames, RES.sec.cov)

    colnames(RES.sec.cov)[1] <- "Outcome"
    colnames(RES.sec.cov)[8] <- "Min. q-value"
    colnames(RES.sec.cov)[9] <- "Reference mean"

    print("----------------------------------------------------------------", quote = FALSE)
    print(paste("H_0:", htitle[i]), quote = FALSE)
    print(RES.sec.cov, quote = FALSE)

    RES.sec.cov <- rbind(c(htitle[i], "", "", "", "", "", "", "", ""), RES.sec.cov)
    RES.print <- rbind(RES.print, RES.sec.cov)

}

RES.print[, c(1:9)] <- RES.print[, c(1, 2, 3, 4, 5, 7, 8, 9, 6)]
colnames(RES.print)[c(1:9)] <- colnames(RES.print)[c(1, 2, 3, 4, 5, 7, 8, 9, 6)]

## Print table to doc ##

appendix <- body_add_par(appendix , " ", style = "Normal") %>%
    body_add_par("Treatment effects on secondary outcomes with covariate adjustment for study 1", style = "Normal") %>%
    body_add_flextable(value = FTable(RES.print[2:nrow(RES.print), c(1:2,4:9)], note = "Note: We include as control variables indicators for being female, for having completed primary schooling, above median age, and unemployment status. Each panel corresponds to a single hypothesis comparing the treatment conditions for the group of outcome variables. The first column reports the mean difference between groups. The second column reports robust standard errors. The third column reports standard p-values. The fourth column reports exact p-values from randomization inference. The fifth column reports the minimum q-values. FDR correction is applied over all outcomes within a hypothesis. The reference mean column lists the mean of the poverty alleviation condition for the first two panels and the mean of the community empowerment condition for the third panel."))

################################################
## Heterogeneous effects for primary outcomes ##
################################################

depvars <- c("vid.num", "sav.amt", "msg.dec")
depvarnames <- c("No. of videos", "Amount saved", "Recorded message")
hetvars <- c("soc.fem", "soc.pri")
hetvarnames <- c("gender (female)", "completion of primary schooling")

for (i in 1:2) {

    hypotheses <- c(paste("treatInd:", hetvars[i], " = 0", sep = ""), paste("treatCom:", hetvars[i], " = 0", sep = ""), paste("treatInd:", hetvars[i], " - ", "treatCom:", hetvars[i], " = 0", sep = ""))
    htitle <- c("Individual - Poverty", "Community - Poverty", "Individual - Community")

    appendix <- body_add_par(appendix , " ", style = "Normal")

    RES.print <- matrix(nrow = 1, ncol = 8)

    for (j in 1:3) {

        RES.pri.het <- matrix(nrow = 1, ncol = 6)

        for (depvar in depvars) {

            eqn <- paste(depvar, " ~ treat*", hetvars[i], sep = "")
            RES.pri.het <- rbind(RES.pri.het, PermTest(eqn, treatvars = c("treat", "pov", "ind", "com"), clustvars = k1_df$survey.id, hypotheses = hypotheses[j], iterations = 10, data = k1_df))

        }

        RES.pri.het <- RES.pri.het[2:nrow(RES.pri.het), 1:ncol(RES.pri.het)]
        RES.pri.het <- cbind(RES.pri.het, FDR(RES.pri.het[, 4]))

        RES.pri.het[, 5] <- round(RES.pri.het[, 5], 0)
        RES.pri.het[, -5] <- format(round(RES.pri.het[, -5], 3), nsmall = 3)

        RES.pri.het <- cbind(depvarnames, RES.pri.het)

        colnames(RES.pri.het)[1] <- "Outcome"
        colnames(RES.pri.het)[8] <- "Min. q-value"

        print("----------------------------------------------------------------", quote = FALSE)
        print(paste("H_0:", htitle[j]), quote = FALSE)
        print(RES.pri.het, quote = FALSE)

        RES.pri.het <- rbind(c(htitle[j], "", "", "", "", "", "", ""), RES.pri.het)
        RES.print <- rbind(RES.print, RES.pri.het)

    }

    RES.print[, c(1:8)] <- RES.print[, c(1, 2, 3, 4, 5, 7, 8, 6)]
    colnames(RES.print)[c(1:8)] <- colnames(RES.print)[c(1, 2, 3, 4, 5, 7, 8, 6)]

    ## Print table to doc ##

    appendix <- body_add_par(appendix , " ", style = "Normal") %>%

        body_add_par(paste("Heterogeneous treatment effects on primary outcomes by", hetvarnames[i], "for study 1"), style = "Normal") %>%
        body_add_flextable(value = FTable(RES.print[2:nrow(RES.print), c(1:2,4:7)], note = "Note: This table reports coefficient estimates on each experimental comparison interacted with a baseline variable. Each panel corresponds to a single hypothesis comparing the treatment conditions for the group of outcome variables. The first column reports the mean difference between groups. The second column reports robust standard errors. The third column reports standard p-values. The fourth column reports exact p-values from randomization inference."))

}

##################################################
## Heterogeneous effects for secondary outcomes ##
##################################################

depvars <- c("sel.score.avg", "sti.score.avg", "aff.score.avg", "ses.lad.now", "ses.lad.y2", "msg.avg", "que.smrd")
depvarnames <- c("Self-Efficacy (avg.)", "Stigma (avg.)", "Affect (avg.)", "Social status", "Anticipated social mobility", "Message support", "Query ordering")
hetvars <- c("soc.fem", "soc.pri")
hetvarnames <- c("gender (female)", "completion of primary schooling")

for (i in 1:2) {

    hypotheses <- c(paste("treatInd:", hetvars[i], " = 0", sep = ""), paste("treatCom:", hetvars[i], " = 0", sep = ""), paste("treatInd:", hetvars[i], " - ", "treatCom:", hetvars[i], " = 0", sep = ""))
    htitle <- c("Individual - Poverty", "Community - Poverty", "Individual - Community")

    appendix <- body_add_par(appendix , " ", style = "Normal")

    RES.print <- matrix(nrow = 1, ncol = 8)

    for (j in 1:3) {

        RES.sec.het <- matrix(nrow = 1, ncol = 6)

        for (depvar in depvars) {

            eqn <- paste(depvar, " ~ treat*", hetvars[i], sep = "")
            RES.sec.het <- rbind(RES.sec.het, PermTest(eqn, treatvars = c("treat", "pov", "ind", "com"), clustvars = k1_df$survey.id, hypotheses = hypotheses[j], iterations = 10, data = k1_df))

        }

        RES.sec.het <- RES.sec.het[2:nrow(RES.sec.het), 1:ncol(RES.sec.het)]
        RES.sec.het <- cbind(RES.sec.het, FDR(RES.sec.het[, 4]))

        RES.sec.het[, 5] <- round(RES.sec.het[, 5], 0)
        RES.sec.het[, -5] <- format(round(RES.sec.het[, -5], 3), nsmall = 3)

        RES.sec.het <- cbind(depvarnames, RES.sec.het)

        colnames(RES.sec.het)[1] <- "Outcome"
        colnames(RES.sec.het)[8] <- "Min. q-value"

        print("----------------------------------------------------------------", quote = FALSE)
        print(paste("H_0:", htitle[j]), quote = FALSE)
        print(RES.sec.het, quote = FALSE)

        RES.sec.het <- rbind(c(htitle[j], "", "", "", "", "", "", ""), RES.sec.het)
        RES.print <- rbind(RES.print, RES.sec.het)

    }

    RES.print[, c(1:8)] <- RES.print[, c(1, 2, 3, 4, 5, 7, 8, 6)]
    colnames(RES.print)[c(1:8)] <- colnames(RES.print)[c(1, 2, 3, 4, 5, 7, 8, 6)]

    ## Print table to doc ##

    appendix <- body_add_par(appendix , " ", style = "Normal") %>%

        body_add_par(paste("Heterogeneous treatment effects on secondary outcomes by", hetvarnames[i], "for study 1"), style = "Normal") %>%
        body_add_flextable(value = FTable(RES.print[2:nrow(RES.print), c(1:2,4:7)], note = "Note: This table reports coefficient estimates on each experimental comparison interacted with a baseline variable. Each panel corresponds to a single hypothesis comparing the treatment conditions for the group of outcome variables. The first column reports the mean difference between groups. The second column reports robust standard errors. The third column reports standard p-values. The fourth column reports exact p-values from randomization inference."))

}

# Print results appendix #

print(appendix, target = here("doc", "K1_appendix.docx"))

###########################
## Stigma coding results ##
###########################

# This is exploratory analysis on the stigma qualitative coding

ms.oth.neg.prop <- tidy.codes.sum %>% 
   group_by(condition.order) %>% 
   summarise(Prop_Neg = mean(neg.prop, na.rm=TRUE), Prop_Pos = mean(pos.prop, na.rm=TRUE), Prop_Amb=mean(amb.prop, na.rm=T), "n"=sum(!is.na(oth.value)))

summary(lm(neg.prop ~ condition.order, data=tidy.codes.sum))

##################################
## Bar graphs for main findings ##
##################################

treat <- factor(treat, labels = c("Poverty \n Alleviation", "Individual \n Empowerment", "Community \n Empowerment"))

vid.graph <- BarChart(depvar = vid.num, groupvar = treat, data = k1_df, ytitle = "No. of videos (0-2)", title = "Skills building", xtitle = "", fillcolor = c('#c6c6c7', '#7ca6c0', '#c05746'), bounds = c(1, 1.75))

sel.graph <- BarChart(depvar = sel.score.avg, groupvar = treat, data = k1_df, ytitle = "Self-rating (1-5)", title = "Self-efficacy", xtitle = "", fillcolor = c('#c6c6c7', '#7ca6c0', '#c05746'), bounds = c(3, 3.75))

sti.graph <- BarChart(depvar = sti.score.avg, groupvar = treat, data = k1_df, ytitle = "Self-rating (1-5)", title = "Stigma", xtitle = "", fillcolor = c('#c6c6c7', '#7ca6c0', '#c05746'), bounds = c(2, 2.75))

ses.lad.y2.graph <- BarChart(depvar = ses.lad.y2, groupvar = treat, data = k1_df, title = "Anticipated social mobility", ytitle = "Ladder score (1-10)", xtitle = "", fillcolor = c('#c6c6c7', '#7ca6c0', '#c05746'), bounds = c(mean(ses.lad.y2, na.rm = TRUE) - 0.5 * sd(ses.lad.y2, na.rm = TRUE), mean(ses.lad.y2, na.rm = TRUE) + 0.5 * sd(ses.lad.y2, na.rm = TRUE)))

# Annotate with significance levels #

vid.graph <- vid.graph +
    geom_signif(comparisons = list(c("Poverty \n Alleviation", "Individual \n Empowerment")), annotations = "†", textsize = 3, y_position = 1.5, vjust = -0.2) + 
    geom_signif(comparisons=list(c("Poverty \n Alleviation", "Community \n Empowerment")), annotations = "*", textsize = 5, y_position = 1.625, vjust = 0.2)


sel.graph <- sel.graph + 
    geom_signif(comparisons = list(c("Poverty \n Alleviation", "Individual \n Empowerment")), annotations = "*", textsize = 5, y_position = 3.525, vjust = 0.2) + 
    geom_signif(comparisons=list(c("Poverty \n Alleviation", "Community \n Empowerment")), annotations = "*", textsize = 5, y_position = 3.625, vjust = 0.2)

sti.graph <- sti.graph + 
    geom_signif(comparisons=list(c("Poverty \n Alleviation", "Community \n Empowerment")), annotations = "*", textsize = 5, y_position = 2.7, vjust = 0.2)

ses.lad.y2.graph <- ses.lad.y2.graph + 
    geom_signif(comparisons = list(c("Poverty \n Alleviation", "Individual \n Empowerment")), annotations = "*", textsize = 5, y_position = 6.55, vjust = 0.2) + 
    geom_signif(comparisons=list(c("Poverty \n Alleviation", "Community \n Empowerment")), annotations = "*", textsize = 5, y_position = 6.75, vjust = 0.2)

# Arrange figures in grid #

Figure1 <- plot_grid(vid.graph, sel.graph, ses.lad.y2.graph, sti.graph, nrow = 1, ncol = 4, labels = c("A. Economic Behavior", "B. Psychological Outcomes", "", ""), label_size = 12, scale = 0.85, hjust = -0.2)
save_plot(here("graphics", "Figure1.png"), Figure1, base_height = 3, base_width = 10, dpi=300)
