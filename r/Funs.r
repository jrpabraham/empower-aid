# Author: Justin Abraham
# R Version: R-3.6.2
# RStudio Version: 1.2.5033

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

    big_border <- fp_border(width = 2)
    std_border <- fp_border(width = 1)

    flextable <- regulartable(results) %>%

        align(align = "center") %>%
        align(j = 1, part = "body", align = "left") %>%

        border_remove() %>%
        hline_top(part = "all", border = big_border) %>%
        hline_top(part = "body", border = big_border) %>%
        hline_bottom(part = "body", border = big_border) %>%
        hline(i = boldix[-1] - 1, border = std_border) %>%

        bold(i = boldix, j = 1) %>%

        add_footer(Outcome = note) %>%
        merge_at(j = 1:(ncol(results)), part = "footer") %>%

        width(j = 1, width = 2) %>%
        width(j = seq(2, ncol(results), 1), width = 0.5) %>%
        fontsize(size = 9, part = "all")

    return(flextable)

}

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
      scale_fill_manual(values = fillcolor)

    return(Graph)

}