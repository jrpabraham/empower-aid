# Author: Justin Abraham
# R Version: R-3.6.2
# RStudio Version: 1.2.5033

###############################
## Install required packages ##
###############################

set.seed(47269801)

install.packages("pacman")
library("pacman")

p_load("here", "tidyr", "dplyr", "lmtest", "multiwayvcov", "multcomp", "reshape2", "knitr", "flextable", "officer", "forestplot", "cowplot", "ggplot2", "matrixStats", "ggthemes", "ggsignif", "rstudioapi")

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

    EST <- cbind(obsEST, ExactP)

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

## This is for psych means

scale.means = function (df, ..., na.rm=FALSE) {
    vars = unlist(list(...))
    mean_vars = rowMeans(df[,vars], na.rm=na.rm)
    return(mean_vars)
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

FTable <- function(results, panels = 3, note) {

    if (is.data.frame(results) == FALSE) {results <- as.data.frame(results)}
    
    boldix <- seq(from = 1, to = nrow(results), by = (nrow(results)) / panels)

    # return(boldix)

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
appendix <- body_add_par(appendix , "Pre-specified data analysis for Kenya recipient experiment (study 1)", style = "heading 1")

########################
## Summary statistics ##
########################

SumStats <- SumStats(varlist = c("soc.fem", "soc.pri", "soc.age", "ses.unemp", "soc.sav", "soc.con", "soc.inc"), labels = c("Female", "Completed std. 8", "Age", "Unemployed", "Holds savings", "Consumption (Ksh.)", "Income (Ksh.)"), data = k1_df)

## Print table to doc ##

appendix <- body_add_par(appendix , " ", style = "Normal") %>%
    body_add_par("Summary of sample sociodemographic characteristics for study 1", style = "Normal")

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
depvars <- c("soc.fem", "soc.pri", "soc.age", "ses.unemp")
depvarnames <- c("Female", "Completed std. 8", "Age", "Unemployed")

RES.print <- matrix(nrow = 1, ncol = 9)

for (i in 1:3) {

    RES.bal <- matrix(nrow = 1, ncol = 6)

    for (depvar in depvars) {

        eqn <- paste(depvar, "~ treat", sep = " ")
        RES.bal <- rbind(RES.bal, PermTest(eqn, treatvars = c("treat", "pov", "ind", "com"), clustvars = k1_df$survey.id, hypotheses = hypotheses[i], iterations = 10000, data = k1_df))

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
        RES.eq <- rbind(RES.eq, PermTest(secondstage, treatvars = c("treat", "pov", "ind", "com"), clustvars = k1_df[complete.cases(soc.fem, soc.age, soc.pri, ses.unemp), "survey.id"], hypotheses = hypotheses[i], iterations = 10000, data = k1_df[complete.cases(soc.fem, soc.age, soc.pri, ses.unemp), ]))

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
        RES.pri <- rbind(RES.pri, PermTest(eqn, treatvars = c("treat", "pov", "ind", "com"), clustvars = k1_df$survey.id, hypotheses = hypotheses[i], iterations = 10000, data = k1_df))

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
        RES.sec <- rbind(RES.sec, PermTest(eqn, treatvars = c("treat", "pov", "ind", "com"), clustvars = k1_df$survey.id, hypotheses = hypotheses[i], iterations = 10000, data = k1_df))

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
        RES.pri.cov <- rbind(RES.pri.cov, PermTest(eqn, treatvars = c("treat", "pov", "ind", "com"), clustvars = k1_df$survey.id, hypotheses = hypotheses[i], iterations = 10000, data = k1_df))

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
        RES.sec.cov <- rbind(RES.sec.cov, PermTest(eqn, treatvars = c("treat", "pov", "ind", "com"), clustvars = k1_df$survey.id, hypotheses = hypotheses[i], iterations = 10000, data = k1_df))

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
            RES.pri.het <- rbind(RES.pri.het, PermTest(eqn, treatvars = c("treat", "pov", "ind", "com"), clustvars = k1_df$survey.id, hypotheses = hypotheses[j], iterations = 10000, data = k1_df))

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
            RES.sec.het <- rbind(RES.sec.het, PermTest(eqn, treatvars = c("treat", "pov", "ind", "com"), clustvars = k1_df$survey.id, hypotheses = hypotheses[j], iterations = 10000, data = k1_df))

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
            axis.text.x = element_text(size = 5),
            legend.position = "none") +
      scale_fill_manual(values = fillcolor) # + 
      # scale_y_continuous(breaks = seq(bounds[1], bounds[2], tick))

    return(Graph)

}

# Generate figures #

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
