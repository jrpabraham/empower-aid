# Author: Justin Abraham
# R Version: R-3.6.2
# RStudio Version: 1.2.5033

##############################
## Install missing packages ##
##############################

set.seed(47269801)

install.packages("pacman")
library("pacman")

p_load("here", "dplyr", "multiwayvcov", "multcomp", "reshape2", "knitr", "flextable", "officer", "forestplot", "iptools", "cowplot", "ggplot2", "matrixStats", "ggthemes", "ggsignif")

######################
## Define functions ##
######################

## RegTest estimates a linear model and conducts statistical inference for linear combinations of coefficients ##

RegTest <- function(equation, clustvars, hypotheses, data) {

    model <- lm(equation, data = data, na.action = na.omit)

    if (missing(clustvars)) model$vcov <- vcov(model)
    else model$vcov <- cluster.vcov(model, cluster = clustvars)

    model$test <- summary(glht(model, linfct = hypotheses, vcov = model$vcov))$test

    numhyp <- length(hypotheses)

    EST <- matrix(nrow = numhyp, ncol = 5)

    for (i in 1:numhyp) {

        EST[i, 1] <- model$test$coefficients[i]
        EST[i, 2] <- model$test$tstat[i]
        EST[i, 3] <- model$test$sigma[i]
        EST[i, 4] <- model$test$pvalues[i]
        EST[i, 5] <- nobs(model)

    }

    colnames(EST) <- c("Coefficient", "t-stat", "Std. error", "p-value", "Obs.")

    return(EST)

}

## PermTest estimates a linear model and conducts statistical inference by permuting the treatment assignment (randomization inference) ##

PermTest <- function(equation, treatvars, clustvars, hypotheses, iterations, data) {

    stopifnot(length(hypotheses) <= 1)

    obsEST <- RegTest(equation, clustvars, hypotheses, data)
    obsStat <- obsEST[1, 2]

    simEST <- matrix(ncol = 5)

    for (i in 1:iterations) {

        simTreat <- data[, treatvars, drop = FALSE]
        simTreat <- simTreat[sample(nrow(simTreat)),]

        simData <- cbind(simTreat, data[, !(names(data) %in% treatvars), drop = FALSE])
        colnames(simData)[1:length(treatvars)] <- treatvars

        simEST <- rbind(simEST, RegTest(equation, clustvars, hypotheses, data = simData))

    }

    simSTAT <- simEST[2:nrow(simEST), 2]
    countSTAT <- matrix(abs(simSTAT) >= abs(obsStat), ncol = 1)

    ExactP <- colSums(countSTAT) / iterations

    EST <- matrix(cbind(obsEST, ExactP), nrow = 1, ncol = 6)

    colnames(EST) <- c("Coefficient", "t-stat", "Std. error", "p-value", "Obs.", "Exact p-value")

    return(EST)

}

## FDR returns p-values correcting for the false discovery rate for a set of hypothesis tests ##

FDR <- function(pvals, step) {

    if (sum(is.na(pvals) == FALSE) <= 1) {return(pvals)}
    if (missing(step)) {step <- 0.001}

    allpvals <- cbind(as.matrix(pvals), matrix(1:nrow(as.matrix(pvals)), ncol = 1))

    pvals <- na.omit(allpvals)
    nump <- nrow(pvals)

    pvals <- pvals[order(pvals[, 1]), ]
    rank <- matrix(1:nump, ncol = 1)
    pvals <- cbind(pvals, rank, matrix(1, nrow = nump, ncol = 1))

    qval <- 1

    while (qval > 0) {

        qfirst <- qval / (1 + qval)
        fdrtemp <- (qfirst * rank) / nump

        subrank <- which(fdrtemp >= as.matrix(pvals[, 1]))

        if (length(subrank) < 1) {
            numreject <- 0
        } else numreject <- max(subrank)

        qsec <- qfirst * (nump / (nump - numreject))
        fdrtemp <- (qsec * rank) / nump

        subrank <- which(fdrtemp >= as.matrix(pvals[, 1]))

        if (length(subrank) < 1) {
            numreject <- 0
        } else numreject <- max(subrank)

        pvals[which(pvals[, 3] <= numreject), 4] <- qval

        qval <- qval - step

    }

    pvals <- pvals[order(pvals[, 2]), ]

    qvals <- matrix(nrow = nrow(allpvals), ncol = 1)
    qvals[match(pvals[, 2], allpvals[, 2]), 1] <- pvals[, 4]

    return(as.matrix(qvals))

}

## Interact returns a string of interacted variables ##

Interact <- function(d, x) {

    catstring <- ""

    for (var in x) {

        catstring <- paste(catstring, " + ", d, "*", var, sep = "")

    }

    return(substr(catstring, 3, nchar(catstring)))

}

## SumStats creates a table of summary statistics

SumStats <- function(varlist, labels, data) {

    M <- matrix(nrow = length(varlist), ncol = 6)
    rownames(M) <- varlist

    for (var in varlist) {

        M[var, 1] <- format(round(mean(na.omit(data[, var])), 3), nsmall = 3)
        M[var, 2] <- format(round(sd(na.omit(data[, var])), 3), nsmall = 3)
        M[var, 3] <- format(round(median(na.omit(data[, var])), 0), nsmall = 0)
        M[var, 4] <- format(round(min(na.omit(data[, var])), 0), nsmall = 0)
        M[var, 5] <- format(round(max(na.omit(data[, var])), 0), nsmall = 0)
        M[var, 6] <- format(round(length(na.omit(data[, var])), 0), nsmall = 0)

    }

    table <- cbind(labels, M)
    colnames(table) <- c("Variable", "Mean", "Std. dev.", "Median", "Min.", "Max.", "Obs.")
    return(table)

}

## FTable creates a FlexTable out of a results matrix

FTable <- function(results, note) {

    if(is.data.frame(results) == FALSE) results <- as.data.frame(results)

    boldix <- seq(1, 1 + 2 * nrow(results) / 3, nrow(results) / 3)

    ftable <- regulartable(results) %>%

        align(align = "center") %>%
        align(j = 1, part = "body", align = "left") %>%

        border_remove() %>%
        hline_top(part = "all", border = big_border) %>%
        hline_top(part = "body", border = big_border) %>%
        hline_bottom(part = "body", border = big_border) %>%
        border(i = boldix[-1], border.top = std_border) %>%

        bold(i = boldix, j = 1) %>%

        add_footer(Outcome = note) %>%
        merge_at(j = 1:(ncol(results)), part = "footer") %>%

        width(j = 1, width = 2) %>%
        width(j = seq(2, ncol(results), 1), width = 0.5) %>%
        fontsize(size = 9, part = "all")

    return(ftable)

}

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

write.csv(u1_df, file = here("data", "U1_Clean_Data.csv"), na = "")
attach(u1_df)

############################
## Initialize results doc ##
############################

big_border = fp_border(width = 2)
std_border = fp_border(width = 1)

appendix <- read_docx()
appendix <- body_add_par(appendix , "Pre-specified data analysis for US donor experiment (study 3)", style = "heading 1")

########################
## Summary statistics ##
########################

SumStats <- SumStats(varlist = c("priordonor", "age", "gen.fem", "income", "hhsize", "edu.self.ba", "edu.par.ba", "race.minor", "religiosity", "is.chr", "is.dem"), labels = c("Prior donor", "Age", "Female", "Annual income (USD)", "HH size", "College educated", "Social class", "Racial minority", "Religiosity", "Christian", "Democrat"), data = u1_df)

## Print table to doc ##

appendix <- body_add_par(appendix , " ", style = "Normal") %>%
    body_add_par("Summary of sample sociodemographic characteristics for study 3", style = "Normal")

ftable <- regulartable(as.data.frame(SumStats)) %>%
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
depvars <- c("priordonor", "age", "gen.fem", "income", "hhsize", "edu.self.ba", "edu.par.ba", "race.minor", "religiosity", "is.chr", "is.dem")
depvarnames <- c("Prior donor", "Age", "Female", "Annual income (USD)", "HH size", "College educated", "Social class", "Racial minority", "Religiosity", "Christian", "Democrat")

RES.print <- matrix(nrow = 1, ncol = 9)

for (i in 1:3) {

    RES.bal <- matrix(nrow = 1, ncol = 6)

    for (depvar in depvars) {

        eqn <- paste(depvar, "~ treat", sep = " ")
        RES.bal <- rbind(RES.bal, PermTest(eqn, treatvars = c("treat", "pov", "ind", "com"), clustvars = u1_df$survey.id, hypotheses = hypotheses[i], iterations = 10000, data = u1_df))

    }

    RES.bal <- cbind(RES.bal[2:nrow(RES.bal), 1:ncol(RES.bal)], FDR(RES.bal[2:nrow(RES.bal), 4]))

    if (hypotheses[i] == "treatInd - treatCom = 0") {
        RES.bal <- cbind(RES.bal, colMeans(u1_df[which(u1_df$com == 1), depvars], na.rm = TRUE))
    } else RES.bal <- cbind(RES.bal, colMeans(u1_df[which(u1_df$pov == 1), depvars], na.rm = TRUE))

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
    body_add_par("Balance checks on subject demographic characteristics for study 3", style = "Normal") %>%
    body_add_flextable(value = FTable(RES.print[2:nrow(RES.print), c(1:2,4:9)], "Note: Each panel corresponds to a single hypothesis for the group of outcome variables. The first column reports the mean difference between groups. The second column reports robust standard errors. The third column reports standard p-values. The fourth column reports exact p-values from randomization inference. The fifth column reports the minimum q-values. FDR correction is applied over all outcomes within a hypothesis. The reference mean column lists the mean of the poverty alleviation condition for the first two panels and the mean of the community empowerment condition for the third panel."))

####################################
## Plain OLS for primary outcomes ##
####################################

hypotheses <- c("treatInd = 0", "treatCom = 0", "treatInd - treatCom = 0")
htitle <- c("Individual - Poverty", "Community - Poverty", "Individual - Community")
depvars <- c("donation_org")
depvarnames <- c("Donation amount (USD)")

RES.print <- matrix(nrow = 1, ncol = 8)

for (i in 1:3) {

    RES.pri <- matrix(nrow = 1, ncol = 6)

    for (depvar in depvars) {

        eqn <- paste(depvar, "~ treat", sep = " ")
        RES.pri <- rbind(RES.pri, PermTest(eqn, treatvars = c("treat", "pov", "ind", "com"), clustvars = u1_df$survey.id, hypotheses = hypotheses[i], iterations = 10000, data = u1_df))

    }

    RES.pri <- RES.pri[2:nrow(RES.pri), 1:ncol(RES.pri)]
    dim(RES.pri) <- c(1, 6)

    if (hypotheses[i] == "treatInd - treatCom = 0") {
        RES.pri <- cbind(RES.pri, mean(u1_df[which(u1_df$com == 1), depvars]))
    } else RES.pri <- cbind(RES.pri, mean(u1_df[which(u1_df$pov == 1), depvars]))

    RES.pri[5] <- round(RES.pri[5], 0)
    RES.pri[-5] <- format(round(RES.pri[-5], 3), nsmall = 3)

    RES.pri <- cbind(depvarnames, RES.pri)
    colnames(RES.pri) <- c("Outcome", "Coefficient", "t-stat", "Std. error", "p-value", "Obs.", "Exact p-value", "Reference mean")

    print("----------------------------------------------------------------", quote = FALSE)
    print(paste("H_0:", htitle[i]), quote = FALSE)
    print(RES.pri, quote = FALSE)

    RES.pri <- rbind(c(htitle[i], "", "", "", "", "", "", ""), RES.pri)
    RES.print <- rbind(RES.print, RES.pri)

}

RES.print[, c(1:8)] <- RES.print[, c(1, 2, 3, 4, 5, 7, 8, 6)]
colnames(RES.print)[c(1:8)] <- colnames(RES.print)[c(1, 2, 3, 4, 5, 7, 8, 6)]

## Print table to doc ##

appendix <- body_add_par(appendix , " ", style = "Normal") %>%
    body_add_par("Treatment effects on donation amount for study 3", style = "Normal") %>%
    body_add_flextable(value = FTable(RES.print[2:nrow(RES.print), c(1:2,4:8)], "Note: Each panel corresponds to a single hypothesis comparing the treatment conditions for the group of outcome variables. The first column reports the mean difference between groups. The second column reports robust standard errors. The third column reports standard p-values. The fourth column reports exact p-values from randomization inference. The reference mean column lists the mean of the poverty alleviation condition for the first two panels and the mean of the community empowerment condition for the third panel."))

######################################
## Plain OLS for secondary outcomes ##
######################################

hypotheses <- c("treatInd = 0", "treatCom = 0", "treatInd - treatCom = 0")
htitle <- c("Individual - Poverty", "Community - Poverty", "Individual - Community")
depvars <- c("donated", "encourage", "donor_status")
depvarnames <- c("Made donation", "Encourage donation", "Donor status (ladder)")

RES.print <- matrix(nrow = 1, ncol = 9)

for (i in 1:3) {

    RES.sec <- matrix(nrow = 1, ncol = 6)

    for (depvar in depvars) {

        eqn <- paste(depvar, "~ treat", sep = " ")
        RES.sec <- rbind(RES.sec, PermTest(eqn, treatvars = c("treat", "pov", "ind", "com"), clustvars = u1_df$survey.id, hypotheses = hypotheses[i], iterations = 10000, data = u1_df))

    }

    RES.sec <- cbind(RES.sec[2:nrow(RES.sec), 1:ncol(RES.sec)], FDR(RES.sec[2:nrow(RES.sec), 4]))

    if (hypotheses[i] == "treatInd - treatCom = 0") {
        RES.sec <- cbind(RES.sec, colMeans(u1_df[which(u1_df$com == 1), depvars], na.rm = TRUE))
    } else RES.sec <- cbind(RES.sec, colMeans(u1_df[which(u1_df$pov == 1), depvars], na.rm = TRUE))

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
    body_add_par("Treatment effects on secondary outcomes for study 3", style = "Normal") %>%
    body_add_flextable(value = FTable(RES.print[2:nrow(RES.print), c(1:2,4:9)], "Note: Each panel corresponds to a single hypothesis comparing the treatment conditions for the group of outcome variables. The first column reports the mean difference between groups. The second column reports robust standard errors. The third column reports standard p-values. The fourth column reports exact p-values from randomization inference. The fifth column reports the minimum q-values. FDR correction is applied over all outcomes within a hypothesis. The reference mean column lists the mean of the poverty alleviation condition for the first two panels and the mean of the community empowerment condition for the third panel."))

###############################################
## Covariate adjustment for primary outcomes ##
###############################################

hypotheses <- c("treatInd = 0", "treatCom = 0", "treatInd - treatCom = 0")
htitle <- c("Individual - Poverty", "Community - Poverty", "Individual - Community")
depvars <- c("donation_org")
depvarnames <- c("Donation amount (USD)")
covariates <- c("priordonor.c", "gen.fem.c", "hi.income.hh.c", "edu.self.ba.c", "race.minor.c", "is.chr.c", "is.religi.c", "is.dem.c")

RES.print <- matrix(nrow = 1, ncol = 8)

for (i in 1:3) {

    RES.pri.cov <- matrix(nrow = 1, ncol = 6)

    for (depvar in depvars) {

        eqn <- paste(depvar, "~", Interact("treat", covariates), sep = " ")
        RES.pri.cov <- rbind(RES.pri.cov, PermTest(eqn, treatvars = c("treat", "pov", "ind", "com"), clustvars = u1_df$survey.id, hypotheses = hypotheses[i], iterations = 10000, data = u1_df))

    }

    RES.pri.cov <- RES.pri.cov[2:nrow(RES.pri.cov), 1:ncol(RES.pri.cov)]
    dim(RES.pri.cov) <- c(1, 6)

    if (hypotheses[i] == "treatInd - treatCom = 0") {
        RES.pri.cov <- cbind(RES.pri.cov, mean(u1_df[which(u1_df$com == 1), depvars]))
    } else RES.pri.cov <- cbind(RES.pri.cov, mean(u1_df[which(u1_df$pov == 1), depvars]))

    RES.pri.cov[5] <- round(RES.pri.cov[5], 0)
    RES.pri.cov[-5] <- format(round(RES.pri.cov[-5], 3), nsmall = 3)

    RES.pri.cov <- cbind(depvarnames, RES.pri.cov)
    colnames(RES.pri.cov) <- c("Outcome", "Coefficient", "t-stat", "Std. error", "p-value", "Obs.", "Exact p-value", "Reference mean")

    print("----------------------------------------------------------------", quote = FALSE)
    print(paste("H_0:", htitle[i]), quote = FALSE)
    print(RES.pri.cov, quote = FALSE)

    RES.pri.cov <- rbind(c(htitle[i], "", "", "", "", "", "", ""), RES.pri.cov)
    RES.print <- rbind(RES.print, RES.pri.cov)

}

RES.print[, c(1:8)] <- RES.print[, c(1, 2, 3, 4, 5, 7, 8, 6)]
colnames(RES.print)[c(1:8)] <- colnames(RES.print)[c(1, 2, 3, 4, 5, 7, 8, 6)]

## Print table to doc ##

appendix <- body_add_par(appendix , " ", style = "Normal") %>%
    body_add_par("Treatment effects on donation amount with covariate adjustment for study 3", style = "Normal") %>%
    body_add_flextable(value = FTable(RES.print[2:nrow(RES.print), c(1:2,4:8)], "Note: We include as control variables indicators for being a prior donor, being female, high income, education level, being a racial minority, religious affiliation, religiosity, and political affiliation. Each panel corresponds to a single hypothesis comparing the treatment conditions for the group of outcome variables. The first column reports the mean difference between groups. The second column reports robust standard errors. The third column reports standard p-values. The fourth column reports exact p-values from randomization inference. The reference mean column lists the mean of the poverty alleviation condition for the first two panels and the mean of the community empowerment condition for the third panel."))

#################################################
## Covariate adjustment for secondary outcomes ##
#################################################

hypotheses <- c("treatInd = 0", "treatCom = 0", "treatInd - treatCom = 0")
htitle <- c("Individual - Poverty", "Community - Poverty", "Individual - Community")
depvars <- c("donated", "encourage", "donor_status")
depvarnames <- c("Made donation", "Encourage donation", "Donor status (ladder)")
covariates <- c("priordonor.c", "gen.fem.c", "hi.income.hh.c", "edu.self.ba.c", "race.minor.c", "is.chr.c", "is.religi.c", "is.dem.c")

RES.print <- matrix(nrow = 1, ncol = 9)

for (i in 1:3) {

    RES.sec.cov <- matrix(nrow = 1, ncol = 6)

    for (depvar in depvars) {

        eqn <- paste(depvar, "~", Interact("treat", covariates), sep = " ")
        RES.sec.cov <- rbind(RES.sec.cov, PermTest(eqn, treatvars = c("treat", "pov", "ind", "com"), clustvars = u1_df$survey.id, hypotheses = hypotheses[i], iterations = 10000, data = u1_df))

    }

    RES.sec.cov <- cbind(RES.sec.cov[2:nrow(RES.sec.cov), 1:ncol(RES.sec.cov)], FDR(RES.sec.cov[2:nrow(RES.sec.cov), 4]))

    if (hypotheses[i] == "treatInd - treatCom = 0") {
        RES.sec.cov <- cbind(RES.sec.cov, colMeans(u1_df[which(u1_df$com == 1), depvars], na.rm = TRUE))
    } else RES.sec.cov <- cbind(RES.sec.cov, colMeans(u1_df[which(u1_df$pov == 1), depvars], na.rm = TRUE))

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
    body_add_par("Treatment effects on secondary outcomes with covariate adjustment for study 3", style = "Normal") %>%
    body_add_flextable(value = FTable(RES.print[2:nrow(RES.print), c(1:2,4:9)], "Note: We include as control variables indicators for being a prior donor, being female, high income, education level, being a racial minority, religious affiliation, religiosity, and political affiliation. Each panel corresponds to a single hypothesis comparing the treatment conditions for the group of outcome variables. The first column reports the mean difference between groups. The second column reports robust standard errors. The third column reports standard p-values. The fourth column reports exact p-values from randomization inference. The fifth column reports the minimum q-values. FDR correction is applied over all outcomes within a hypothesis. The reference mean column lists the mean of the poverty alleviation condition for the first two panels and the mean of the community empowerment condition for the third panel."))

################################################
## Heterogeneous effects for primary outcomes ##
################################################

depvars <- c("donation_org")
depvarnames <- c("Donation amount (USD)")
hetvars <- c("priordonor", "gen.fem", "hi.income.hh", "edu.par.ba", "is.religi", "is.dem")
hetvarnames <- c("prior donor status (have made a prior donation)", "gender (female)", "income (above $31,305)", "social class (parents are college-educated)", "religiosity (some religiosity or greater)", "political affiliation (Democrat)")

for (i in 1:6) {

    hypotheses <- c(paste("treatInd:", hetvars[i], " = 0", sep = ""), paste("treatCom:", hetvars[i], " = 0", sep = ""), paste("treatInd:", hetvars[i], " - ", "treatCom:", hetvars[i], " = 0", sep = ""))
    htitle <- c("Individual - Poverty", "Community - Poverty", "Individual - Community")

    appendix <- body_add_par(appendix , " ", style = "Normal")

    RES.print <- matrix(nrow = 1, ncol = 7)

    for (j in 1:3) {

        RES.pri.het <- matrix(nrow = 1, ncol = 6)

        for (depvar in depvars) {

            eqn <- paste(depvar, " ~ treat*", hetvars[i], sep = "")
            RES.pri.het <- rbind(RES.pri.het, PermTest(eqn, treatvars = c("treat", "pov", "ind", "com"), clustvars = u1_df$survey.id, hypotheses = hypotheses[j], iterations = 10000, data = u1_df))

        }

        RES.pri.het <- RES.pri.het[2:nrow(RES.pri.het), 1:ncol(RES.pri.het)]

        RES.pri.het[5] <- round(RES.pri.het[5], 0)
        RES.pri.het[-5] <- format(round(RES.pri.het[-5], 3), nsmall = 3)

        dim(RES.pri.het) <- c(1, 6)
        RES.pri.het <- cbind(depvarnames, RES.pri.het)
        colnames(RES.pri.het) <- c("Outcome", "Coefficient", "t-stat", "Std. error", "p-value", "Obs.", "Exact p-value")

        print("----------------------------------------------------------------", quote = FALSE)
        print(paste("H_0:", htitle[j]), quote = FALSE)
        print(RES.pri.het, quote = FALSE)

        RES.pri.het <- rbind(c(htitle[j], "", "", "", "", "", ""), RES.pri.het)
        RES.print <- rbind(RES.print, RES.pri.het)

    }

    RES.print[, c(1:7)] <- RES.print[, c(1, 2, 3, 4, 5, 7, 6)]
    colnames(RES.print)[c(1:7)] <- colnames(RES.print)[c(1, 2, 3, 4, 5, 7, 6)]

    ## Print table to doc ##

    appendix <- body_add_par(appendix , " ", style = "Normal") %>%
        body_add_par(paste("Heterogeneous treatment effects on donation amount by", hetvarnames[i], "for study 3"), style = "Normal") %>%
        body_add_flextable(value = FTable(RES.print[2:nrow(RES.print), c(1:2,4:7)], "Note: This table reports coefficient estimates on each experimental comparison interacted with a baseline variable. Each panel corresponds to a single hypothesis comparing the treatment conditions for the group of outcome variables. The first column reports the mean difference between groups. The second column reports robust standard errors. The third column reports standard p-values. The fourth column reports exact p-values from randomization inference."))

}

##################################################
## Heterogeneous effects for secondary outcomes ##
##################################################

depvars <- c("donated", "encourage", "donor_status")
depvarnames <- c("Made donation", "Encourage donation", "Donor status (ladder)")
hetvars <- c("priordonor", "gen.fem", "hi.income.hh", "edu.par.ba", "is.religi", "is.dem")
hetvarnames <- c("prior donor status (have made a prior donation)", "gender (female)", "income (above $31,305)", "social class (parents are college-educated)", "religiosity (some religiosity or greater)", "political affiliation (Democrat)")

for (i in 1:6) {

    hypotheses <- c(paste("treatInd:", hetvars[i], " = 0", sep = ""), paste("treatCom:", hetvars[i], " = 0", sep = ""), paste("treatInd:", hetvars[i], " - ", "treatCom:", hetvars[i], " = 0", sep = ""))
    htitle <- c("Individual - Poverty", "Community - Poverty", "Individual - Community")

    appendix <- body_add_par(appendix , " ", style = "Normal")

    RES.print <- matrix(nrow = 1, ncol = 8)

    for (j in 1:3) {

        RES.sec.het <- matrix(nrow = 1, ncol = 6)

        for (depvar in depvars) {

            eqn <- paste(depvar, " ~ treat*", hetvars[i], sep = "")
            RES.sec.het <- rbind(RES.sec.het, PermTest(eqn, treatvars = c("treat", "pov", "ind", "com"), clustvars = u1_df$survey.id, hypotheses = hypotheses[j], iterations = 10000, data = u1_df))

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
        body_add_par(paste("Heterogeneous treatment effects on secondary outcomes by", hetvarnames[i], "for study 3"), style = "Normal") %>%
        body_add_flextable(value = FTable(RES.print[2:nrow(RES.print), c(1:2,4:8)], "Note: This table reports coefficient estimates on each experimental comparison interacted with a baseline variable. Each panel corresponds to a single hypothesis comparing the treatment conditions for the group of outcome variables. The first column reports the mean difference between groups. The second column reports robust standard errors. The third column reports standard p-values. The fourth column reports exact p-values from randomization inference. The fifth column reports the minimum q-values. FDR correction is applied over all outcomes within a hypothesis."))

}

# Print results appendix #

print(appendix, target = here("doc", "U1_appendix.docx"))

##################################
## Bar graphs for main findings ##
##################################

BarChart <- function(depvar, groupvar, data, title, ytitle, xtitle, fillcolor, bounds, tick) {

    quo_groupvar <- enquo(groupvar)
    quo_depvar <- enquo(depvar)

    stats <- data[complete.cases(depvar), ] %>% group_by(!!quo_groupvar) %>% summarise(mean = mean(!!quo_depvar), sd = sd(!!quo_depvar), obs = length(!!quo_depvar))
    stats <- cbind(as.data.frame(table(treat))[, 1], as.data.frame(stats[, 2]), as.data.frame(stats[, 3] / sqrt(stats[, 4])))
    colnames(stats) <- c(deparse(substitute(groupvar)), "mean", "SE")

    Graph <- ggplot(stats,
      aes(!!quo_groupvar, mean, fill = as.factor(!!quo_groupvar))) +
      ggtitle(title) +
      labs(y = ytitle, x = xtitle) +
      coord_cartesian(ylim = bounds) +
      theme_classic(base_size = 9, base_family = "sans") +
      geom_bar(stat = "identity", width = .66) +
      geom_errorbar(aes(ymin = mean - 1.96*SE, ymax = mean + 1.96*SE), width = 0.2) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            panel.grid.major.y = element_line(size = .1, color = "black", linetype = "dotted"),
            axis.line = element_line(colour = "black"),
            axis.text.x = element_text(size = 7),
            legend.position = "none",
            plot.title = element_text(size=14, face="bold")) +
      scale_fill_manual(values = fillcolor) # + 
      # scale_y_continuous(breaks = seq(bounds[1], bounds[2], tick))

    return(Graph)

}


# Generate figures #

treat <- factor(treat, labels = c("Poverty \n Alleviation", "Individual \n Empowerment", "Community \n Empowerment"))

amount.graph <- BarChart(depvar = donation_org, groupvar = treat, data = u1_df, ytitle = "Dollars donated (USD)", title = "A", xtitle = "", fillcolor = c('#c6c6c7', '#7ca6c0', '#c05746'), bounds = c(0, 100), tick = 20)

donate.graph <- BarChart(depvar = donated, groupvar = treat, data = u1_df, ytitle = "Proportion who donated any amount", title = "B", xtitle = "", fillcolor = c('#c6c6c7', '#7ca6c0', '#c05746'), bounds = c(0, 1), tick = 0.2)

encourage.graph <- BarChart(depvar = encourage, groupvar = treat, data = u1_df, ytitle = "Level (1-5)", title = "C) Encourage others to donate", xtitle = "", fillcolor = c('#c6c6c7', '#7ca6c0', '#c05746'), bounds = c(0, 5), tick = 1)

status.graph <- BarChart(depvar = donor_status, groupvar = treat, data = u1_df, ytitle = "Ladder score (1-10)", title = "D) Donor status", xtitle = "", fillcolor = c('#c6c6c7', '#7ca6c0', '#c05746'), bounds = c(0, 10), tick = 2)

# Annotate with significance levels #

amount.graph <- amount.graph +
    geom_signif(comparisons=list(c("Individual \n Empowerment", "Community \n Empowerment")), annotations= "†", textsize = 3, y_position = 50, vjust = -0.2, tip_length = 0.1)

donate.graph <- donate.graph + 
    geom_signif(comparisons=list(c("Individual \n Empowerment", "Community \n Empowerment")), annotations = "*", textsize = 5, y_position = 0.925, vjust = 0.3, tip_length = 0.1)

# Arrange figures in grid #

FigureS3 <- plot_grid(amount.graph, donate.graph, nrow = 1, ncol = 2, labels = c("", "Donor support"), hjust = 0.5, label_size = 12, scale = 0.85)
save_plot(here("graphics", "FigureS3.png"), FigureS3, base_height = 4, base_width = 7, dpi = 300)