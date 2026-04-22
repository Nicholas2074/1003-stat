# //SECTION - covariate

# //ANCHOR - hospMortality

# subset
print("covariate")
print(morNamesConf)

dfCovMor <- dfMor[, c(
    "icuid", 
    "hospMortality", 
    "age", 
    "gender", 
    "bmi", 
    "hypertension", 
    "cerebrovascular_disease", 
    # "losmean",
    "peakmean",
    "ttpmean",
    # "ptemean",
    # "icpdosemean", 
    "icpavgmean"
    )]

summary(dfCovMor$ttpmean)

library(rpart)

CutoffMor <- rpart(hospMortality ~ ttpmean,
    data = dfCovMor,
    control = rpart.control(maxdepth = 2)
)

CutoffMor$splits
#         count ncat    improve index adj
# ttpmean   503   -1 0.06423419 5.825   0
# ttpmean   465    1 0.01175268 0.240   0

# //ANCHOR - disgcs

# subset
print(disNames)
print(disNamesConf)

dfCovDis <- dfDis[, c(
    "icuid", 
    "disgcs", 
    "age", 
    "gender", 
    "bmi", 
    "hypertension", 
    "cerebrovascular_disease", 
    # "losmean",
    "peakmean",
    "ttpmean",
    # "ptemean",
    # "icpdosemean", 
    "icpavgmean"
    )]

summary(dfCovDis$ttpmean)

library(rpart)

CutoffDis <- rpart(disgcs ~ ttpmean,
    data = dfCovDis,
    control = rpart.control(maxdepth = 2)
)

CutoffDis$splits
#         count ncat    improve index adj
# ttpmean   503   -1 0.06826023  5.69   0

# //ANCHOR - devgcs

# subset
print(devNames)
print(devNamesConf)

dfCovDev <- dfDev[, c(
    "icuid", 
    "devgcs", 
    "age", 
    "gender", 
    "bmi", 
    "hypertension", 
    "cerebrovascular_disease", 
    # "losmean",
    "peakmean",
    "ttpmean",
    # "ptemean",
    # "icpdosemean", 
    "icpavgmean"
    )]

summary(dfCovDev$ttpmean)

library(rpart)

CutoffDev <- rpart(devgcs ~ ttpmean,
    data = dfCovDev,
    control = rpart.control(maxdepth = 2)
)

CutoffDev$splits
#         count ncat    improve index adj
# ttpmean   503   -1 0.01845331 6.365   0
# ttpmean   473    1 0.02430764 0.030   0

# //ANCHOR - pivot

dfCovMor <- dfCovMor %>%
    mutate(ttpCat = case_when(
        ttpmean < 5.825 ~ 0,
        ttpmean >= 5.825 ~ 1
    ))

dfCovDis <- dfCovDis %>%
    mutate(ttpCat = case_when(
        ttpmean < 5.825 ~ 0,
        ttpmean >= 5.825 ~ 1
    ))

dfCovDev <- dfCovDev %>%
    mutate(ttpCat = case_when(
        ttpmean < 5.825 ~ 0,
        ttpmean >= 5.825 ~ 1
    ))

dfCovMor[, c(4, 6, 7, 11)] <- lapply(dfCovMor[, c(4, 6, 7, 11)], as.character)

dfCovDis[, c(4, 6, 7, 11)] <- lapply(dfCovDis[, c(4, 6, 7, 11)], as.character)

dfCovDev[, c(4, 6, 7, 11)] <- lapply(dfCovDev[, c(4, 6, 7, 11)], as.character)

# //SECTION - glm

# adjusted logistic regression #

# //ANCHOR - hospMortality

# cor
dfCorMor <- apply(dfCovMor, 2, as.numeric)

corMor <- cor(dfCorMor[, -1:-2])

# heatmap(corMor)

# glm
print(names(dfCovMor))

logMorAdjustedCat <- glm(
    hospMortality ~ .,
    family = binomial,
    data = dfCovMor[, c(-1, -9)]
)

# summary
summary(logMorAdjustedCat)

pMorAdjustedCat <- coef(summary(logMorAdjustedCat))[, "Pr(>|z|)"]

orMorAdjustedCat <- exp(coef(logMorAdjustedCat))

ciMorAdjustedCat <- exp(confint(logMorAdjustedCat))

print(pMorAdjustedCat)
print(orMorAdjustedCat)
print(ciMorAdjustedCat)

# //ANCHOR - disgcs

# cor
dfCorDis <- apply(dfCovDis, 2, as.numeric)

corDis <- cor(dfCorDis[, -1:-2])

# heatmap(corDis)

# glm
print(names(dfCovDis))

logDisAdjustedCat <- glm(
    disgcs ~ .,
    family = binomial,
    data = dfCovDis[, c(-1, -9)]
)

# summary
summary(logDisAdjustedCat)

pDisAdjustedCat <- coef(summary(logDisAdjustedCat))[, "Pr(>|z|)"]

orDisAdjustedCat <- exp(coef(logDisAdjustedCat))

ciDisAdjustedCat <- exp(confint(logDisAdjustedCat))

print(pDisAdjustedCat)
print(orDisAdjustedCat)
print(ciDisAdjustedCat)

# //ANCHOR - devgcs

# cor
dfCorDev <- apply(dfCovDev, 2, as.numeric)

corDev <- cor(dfCorDev[, -1:-2])

# heatmap(corDev)

# glm
print(names(dfCovDev))

logDevAdjustedCat <- glm(
    devgcs ~ .,
    family = binomial,
    data = dfCovDev[, c(-1, -9)]
)

# summary
summary(logDevAdjustedCat)

pDevAdjustedCat <- coef(summary(logDevAdjustedCat))[, "Pr(>|z|)"]

orDevAdjustedCat <- exp(coef(logDevAdjustedCat))

ciDevAdjustedCat <- exp(confint(logDevAdjustedCat))

print(pDevAdjustedCat)
print(orDevAdjustedCat)
print(ciDevAdjustedCat)

# //!SECTION

# //SECTION - forestploter

# //ANCHOR - hospMortality

library(tidyverse)

dfForestMorAdjustedCat <- data.frame(
    "Variable" = names(pMorAdjustedCat),
    "P value" = pMorAdjustedCat,
    "OR" = orMorAdjustedCat,
    "Lower" = ciMorAdjustedCat[, 1],
    "Upper" = ciMorAdjustedCat[, 2],
    row.names = NULL
)

dfForestMorAdjustedCat[, -1] <- round(dfForestMorAdjustedCat[, -1], 3)

dfForestMorAdjustedCat <- dfForestMorAdjustedCat %>%
    mutate("OR(95%CI)" = paste(OR, "(", Lower, ",", Upper, ")"), )

print(dfForestMorAdjustedCat)

resMorCat <- dfForestMorAdjustedCat

resMorCat$" " <- paste(rep("    ", nrow(resMorCat)), collapse = " ")

colnames(resMorCat) <- paste0(colnames(resMorCat), "1")

dim(resMorCat)
print(resMorCat)

# //ANCHOR - disgcs

library(tidyverse)

dfForestDisAdjustedCat <- data.frame(
    "Variable" = names(pDisAdjustedCat),
    "P value" = pDisAdjustedCat,
    "OR" = orDisAdjustedCat,
    "Lower" = ciDisAdjustedCat[, 1],
    "Upper" = ciDisAdjustedCat[, 2],
    row.names = NULL
)

dfForestDisAdjustedCat[, -1] <- round(dfForestDisAdjustedCat[, -1], 3)

dfForestDisAdjustedCat <- dfForestDisAdjustedCat %>%
    mutate("OR(95%CI)" = paste(OR, "(", Lower, ",", Upper, ")"), )

print(dfForestDisAdjustedCat)

resDisCat <- dfForestDisAdjustedCat

resDisCat$" " <- paste(rep("    ", nrow(resDisCat)), collapse = " ")

colnames(resDisCat) <- paste0(colnames(resDisCat), "2")

dim(resDisCat)
print(resDisCat)

# //ANCHOR - devgcs

library(tidyverse)

dfForestDevAdjustedCat <- data.frame(
    "Variable" = names(pDevAdjustedCat),
    "P value" = pDevAdjustedCat,
    "OR" = orDevAdjustedCat,
    "Lower" = ciDevAdjustedCat[, 1],
    "Upper" = ciDevAdjustedCat[, 2],
    row.names = NULL
)

dfForestDevAdjustedCat[, -1] <- round(dfForestDevAdjustedCat[, -1], 3)

dfForestDevAdjustedCat <- dfForestDevAdjustedCat %>%
    mutate("OR(95%CI)" = paste(OR, "(", Lower, ",", Upper, ")"), )

print(dfForestDevAdjustedCat)

resDevCat <- dfForestDevAdjustedCat

# Add a blank column for the forest plot to display CI
# Adjust the column width with space
resDevCat$" " <- paste(rep("    ", nrow(resDevCat)), collapse = " ")

colnames(resDevCat) <- paste0(colnames(resDevCat), "3")

dim(resDevCat)
print(resDevCat)

# //ANCHOR - cbind

resAllCat <- cbind(resMorCat, resDisCat, resDevCat)

resAllCat$" " <- paste(rep("NA", nrow(resAllCat)))

resAllCat <- resAllCat %>%
    mutate(Variable1 = recode(Variable1,
        "age" = "Age",
        "gender" = "Gender",
        "bmi" = "BMI",
        "hypertension" = "Hypertension",
        "cerebrovascular_disease" = "Cerebrovascular Disease",
        "peakmean" = "Peak",
        "ttpCat" = "TTP Cutoff",
        "icpavgmean" = "Mean ICP",
        .default = Variable1
    ))

dim(resAllCat)
resAllCat[, 1]

# //ANCHOR - plot

library(forestploter)

resForestCat <- forest(
    data = resAllCat[, c(1, 2, 7, 6, 22, 9, 14, 13, 22, 16, 21, 20)],
    lower = list(
        resAllCat$Lower1, 
        resAllCat$Lower2,
        resAllCat$Lower3
    ),
    upper = list(
        resAllCat$Upper1, 
        resAllCat$Upper2,
        resAllCat$Upper3
    ),
    est = list(
        resAllCat$OR1, 
        resAllCat$OR2,
        resAllCat$OR3
    ),
    ci_column = c(3, 7, 11),
    ref_line = 1
    # xlim = c(0, 1.5)
)

# Insert text at the top
library(grid)

resForestCat <- insert_text(resForestCat,
    text = c("In-hospital mortality", "Discharge GCS", "GCS difference"),
    col = c(3, 7, 11),
    part = "header",
    just = "center",
    gp = gpar(fontface = "bold")
)

# Add underline at the bottom of the header
resForestCat <- add_border(resForestCat, part = "header", row = 1, where = "top", gp = gpar(lwd = 1))
resForestCat <- add_border(resForestCat, part = "header", row = 2, where = "bottom", gp = gpar(lwd = 1))
resForestCat <- add_border(resForestCat, part = "header", row = 1, where = "bottom", col = c(2:4, 6:8, 10:12), gp = gpar(lwd = 0.5))

print(resForestCat)

# //!SECTION