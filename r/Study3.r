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

p_load("here", "tidyr", "dplyr", "lmtest", "multiwayvcov", "multcomp", "reshape2", "knitr", "flextable", "officer", "forestplot", "cowplot", "ggplot2", "matrixStats", "ggthemes", "ggsignif", "rstudioapi", "iptools")

source(here("r", "Funs.r"), echo = TRUE)

load(here("data", "USData.RData"))

attach(u1_df)

############################
## Initialize results doc ##
############################

appendix <- read_docx()

WriteHeading(appendix , "Data analysis for US donor experiment (study 3)")

########################
## Summary statistics ##
########################

SumTable <- SumStats(varlist = c("priordonor", "age", "gen.fem", "income", "hhsize", "edu.self.ba", "edu.par.ba", "race.minor", "religiosity", "is.chr", "is.dem"), labels = c("Prior donor", "Age", "Female", "Annual income (USD)", "HH size", "College educated", "Social class", "Racial minority", "Religiosity", "Christian", "Democrat"), data = u1_df)

## Print table to doc ##

LineBreak(appendix)
WriteTitle(appendix, "Summary of sample sociodemographic characteristics for study 3")

big_border <- fp_border(width = 2)
std_border <- fp_border(width = 1)

note <- "Note: \"Prior donor\" is an indicator variable for having made charitable contributions in the past. \"Age\" is the self-reported age of the respondent. \"Female\" is an indicator for identifying as a woman. \"Annual income (USD)\" is the self-reported annual income of the respondent. \"HH size\" is household size including the respondent. \"College educated\" is an indicator for having received a college education. \"Social class\" is an indicator for having at least one parent receive a college education. \"Racial minority\" is an indicator variable for identifying as Asian, Black, Hispanic, Native American, or Other. \"Religiosity\" measures strength of religious belief on a 5-point scale. \"Christian\" is an indicator for identifying as a Christian. \"Democrat\" is an indicator for affiliating with the Democratic Party."

flextable <- regulartable(as.data.frame(SumTable)) %>%
    align(align = "center") %>%
    align(j = 1, part = "body", align = "left") %>%
    border_remove() %>%
    hline_top(part = "all", border = big_border) %>%
    hline_top(part = "body", border = big_border) %>%
    hline_bottom(part = "body", border = big_border) %>%
    add_footer(Outcome = note) %>%
    merge_at(j = 1:7, part = "footer") %>%
    flextable::font(fontname = "Times New Roman", part = "all") %>%
    italic(part = "footer") %>%
    fontsize(size = 10, part = "all") %>%
    autofit()

appendix <- body_add_flextable(appendix, value = flextable)

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

LineBreak(appendix)
WriteTitle(appendix, "Balance checks on subject demographic characteristics for study 3")

appendix <- body_add_flextable(appendix, value = FTable(RES.print[2:nrow(RES.print), c(1:2,4:9)], note = "Note: Each panel corresponds to a single hypothesis for the group of outcome variables. The first column reports the mean difference between groups. The second column reports robust standard errors. The third column reports standard p-values. The fourth column reports exact p-values from randomization inference. The fifth column reports the minimum q-values. FDR correction is applied over all outcomes within a hypothesis. The reference mean column lists the mean of the poverty alleviation condition for the first two panels and the mean of the community empowerment condition for the third panel."))

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

LineBreak(appendix)
WriteTitle(appendix, "Treatment effects on donation amount for study 3")

appendix <- body_add_flextable(appendix, value = FTable(RES.print[2:nrow(RES.print), c(1:2,4:8)], note = "Note: Each panel corresponds to a single hypothesis comparing the treatment conditions for the group of outcome variables. The first column reports the mean difference between groups. The second column reports robust standard errors. The third column reports standard p-values. The fourth column reports exact p-values from randomization inference. The reference mean column lists the mean of the poverty alleviation condition for the first two panels and the mean of the community empowerment condition for the third panel."))

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

LineBreak(appendix)
WriteTitle(appendix, "Treatment effects on secondary outcomes for study 3")

appendix <- body_add_flextable(appendix, value = FTable(RES.print[2:nrow(RES.print), c(1:2,4:9)], note = "Note: Each panel corresponds to a single hypothesis comparing the treatment conditions for the group of outcome variables. The first column reports the mean difference between groups. The second column reports robust standard errors. The third column reports standard p-values. The fourth column reports exact p-values from randomization inference. The fifth column reports the minimum q-values. FDR correction is applied over all outcomes within a hypothesis. The reference mean column lists the mean of the poverty alleviation condition for the first two panels and the mean of the community empowerment condition for the third panel."))

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

LineBreak(appendix)
WriteTitle(appendix, "Treatment effects on donation amount with covariate adjustment for study 3")

appendix <- body_add_flextable(appendix, value = FTable(RES.print[2:nrow(RES.print), c(1:2,4:8)], note = "Note: We include as control variables indicators for being a prior donor, being female, high income, education level, being a racial minority, religious affiliation, religiosity, and political affiliation. Each panel corresponds to a single hypothesis comparing the treatment conditions for the group of outcome variables. The first column reports the mean difference between groups. The second column reports robust standard errors. The third column reports standard p-values. The fourth column reports exact p-values from randomization inference. The reference mean column lists the mean of the poverty alleviation condition for the first two panels and the mean of the community empowerment condition for the third panel."))

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

LineBreak(appendix)
WriteTitle(appendix, "Treatment effects on secondary outcomes with covariate adjustment for study 3")

appendix <- body_add_flextable(appendix, value = FTable(RES.print[2:nrow(RES.print), c(1:2,4:9)], note = "Note: We include as control variables indicators for being a prior donor, being female, high income, education level, being a racial minority, religious affiliation, religiosity, and political affiliation. Each panel corresponds to a single hypothesis comparing the treatment conditions for the group of outcome variables. The first column reports the mean difference between groups. The second column reports robust standard errors. The third column reports standard p-values. The fourth column reports exact p-values from randomization inference. The fifth column reports the minimum q-values. FDR correction is applied over all outcomes within a hypothesis. The reference mean column lists the mean of the poverty alleviation condition for the first two panels and the mean of the community empowerment condition for the third panel."))

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

    LineBreak(appendix)
    WriteTitle(appendix, paste("Heterogeneous treatment effects on donation amount by", hetvarnames[i], "for study 3"))

    appendix <- body_add_flextable(appendix, value = FTable(RES.print[2:nrow(RES.print), c(1:2,4:7)], note = "Note: This table reports coefficient estimates on each experimental comparison interacted with a baseline variable. Each panel corresponds to a single hypothesis comparing the treatment conditions for the group of outcome variables. The first column reports the mean difference between groups. The second column reports robust standard errors. The third column reports standard p-values. The fourth column reports exact p-values from randomization inference."))

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

    LineBreak(appendix)
    WriteTitle(appendix, paste("Heterogeneous treatment effects on secondary outcomes by", hetvarnames[i], "for study 3"))

    appendix <- body_add_flextable(appendix, value = FTable(RES.print[2:nrow(RES.print), c(1:2,4:8)], note = "Note: This table reports coefficient estimates on each experimental comparison interacted with a baseline variable. Each panel corresponds to a single hypothesis comparing the treatment conditions for the group of outcome variables. The first column reports the mean difference between groups. The second column reports robust standard errors. The third column reports standard p-values. The fourth column reports exact p-values from randomization inference. The fifth column reports the minimum q-values. FDR correction is applied over all outcomes within a hypothesis."))

}

# Print results appendix #

print(appendix, target = here("doc", "S3_appendix.docx"))

##################################
## Bar graphs for main findings ##
##################################

treat <- factor(u1_df$treat, labels = c("Poverty \n Alleviation", "Individual \n Empowerment", "Community \n Empowerment"))

amount.graph <- BarChart(depvar = u1_df$donation_org, groupvar = treat, ytitle = "Dollars donated (USD)", title = "A", xtitle = "", fillcolor = c('#c6c6c7', '#7ca6c0', '#c05746'), bounds = c(0, 100), tick = 20)

donate.graph <- BarChart(depvar = u1_df$donated, groupvar = treat, ytitle = "Proportion who donated any amount", title = "B", xtitle = "", fillcolor = c('#c6c6c7', '#7ca6c0', '#c05746'), bounds = c(0, 1), tick = 0.2)

encourage.graph <- BarChart(depvar = u1_df$encourage, groupvar = treat, ytitle = "Level (1-5)", title = "C) Encourage others to donate", xtitle = "", fillcolor = c('#c6c6c7', '#7ca6c0', '#c05746'), bounds = c(0, 5), tick = 1)

status.graph <- BarChart(depvar = u1_df$donor_status, groupvar = treat, ytitle = "Ladder score (1-10)", title = "D) Donor status", xtitle = "", fillcolor = c('#c6c6c7', '#7ca6c0', '#c05746'), bounds = c(0, 10), tick = 2)

# Annotate with significance levels #

amount.graph <- amount.graph +
    geom_signif(comparisons=list(c("Individual \n Empowerment", "Community \n Empowerment")), annotations= "†", textsize = 3, y_position = 50, vjust = -0.2, tip_length = 0.1)

donate.graph <- donate.graph + 
    geom_signif(comparisons=list(c("Individual \n Empowerment", "Community \n Empowerment")), annotations = "*", textsize = 5, y_position = 0.925, vjust = 0.3, tip_length = 0.1)

# Arrange figures in grid #

FigureS3 <- plot_grid(amount.graph, donate.graph, nrow = 1, ncol = 2, labels = c("", ""), hjust = 0.5, label_size = 12, scale = 0.85)
save_plot(here("graphics", "FigureS3.png"), FigureS3, base_height = 4, base_width = 7, dpi = 300)