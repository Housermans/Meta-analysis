
# Example datasets --------------------------
# example data from package meta (https://cran.r-project.org/web//packages/meta/meta.pdf)

help(package=metadat) #info practice packages and content
# Example meta analysis

# Do not back transform results, e.g. print logit transformed
# proportions if sm = "PLOGIT" and store old settings
#
oldset <- settings.meta(backtransf = FALSE)

# Use old settings
#
settings.meta(oldset)

# Example from Miller (1978):
#
death <- c(3, 6, 10, 1)
animals <- c(11, 17, 21, 6)
#
m3 <- metaprop(death, animals, sm = "PLOGIT", comb.fixed=FALSE, comb.random=TRUE, method = "Inverse")
forest(m3)

# Data examples from Newcombe (1998)
# - apply various methods to estimate confidence intervals for
#   individual studies
#
event <- c(81, 15, 0, 1)
n <- c(263, 148, 20, 29)
#
m1 <- metaprop(event, n, method.ci = "SA", method = "Inverse")
m2 <- update(m1, method.ci = "SACC")
m3 <- update(m1, method.ci = "WS")
m4 <- update(m1, method.ci = "WSCC")
m5 <- update(m1, method.ci = "CP")
#
lower <- round(rbind(NA, m1$lower, m2$lower, NA, m3$lower,
                     m4$lower, NA, m5$lower), 4)
upper <- round(rbind(NA, m1$upper, m2$upper, NA, m3$upper,
                     m4$upper, NA, m5$upper), 4)
#
tab1 <- data.frame(
  scen1 = meta:::formatCI(lower[, 1], upper[, 1]),
  scen2 = meta:::formatCI(lower[, 2], upper[, 2]),
  scen3 = meta:::formatCI(lower[, 3], upper[, 3]),
  scen4 = meta:::formatCI(lower[, 4], upper[, 4])
)
names(tab1) <- c("r=81, n=263", "r=15, n=148",
                 "r=0, n=20", "r=1, n=29")
row.names(tab1) <- c("Simple", "- SA", "- SACC",
                     "Score", "- WS", "- WSCC", "Binomial", "- CP")
tab1[is.na(tab1)] <- ""
# Newcombe (1998), Table I, methods 1-5:
tab1



