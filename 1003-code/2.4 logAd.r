# //SECTION - covariate

# //ANCHOR - hospMortality

library(tidyverse)

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

logMorAdjustedCont <- glm(
    hospMortality ~ .,
    family = binomial,
    data = dfCovMor[, c(-1, -11)]
)

# summary
summary(logMorAdjustedCont)

pMorAdjustedCont <- coef(summary(logMorAdjustedCont))[, "Pr(>|z|)"]

orMorAdjustedCont <- exp(coef(logMorAdjustedCont))

ciMorAdjustedCont <- exp(confint(logMorAdjustedCont))

print(pMorAdjustedCont)
print(orMorAdjustedCont)
print(ciMorAdjustedCont)

# //ANCHOR - disgcs

# cor
dfCorDis <- apply(dfCovDis, 2, as.numeric)

corDis <- cor(dfCorDis[, -1:-2])

# heatmap(corDis)

# glm
print(names(dfCovDis))

logDisAdjustedCont <- glm(
    disgcs ~ .,
    family = binomial,
    data = dfCovDis[, c(-1, -11)]
)

# summary
summary(logDisAdjustedCont)

pDisAdjustedCont <- coef(summary(logDisAdjustedCont))[, "Pr(>|z|)"]

orDisAdjustedCont <- exp(coef(logDisAdjustedCont))

ciDisAdjustedCont <- exp(confint(logDisAdjustedCont))

print(pDisAdjustedCont)
print(orDisAdjustedCont)
print(ciDisAdjustedCont)

# //ANCHOR - devgcs

# cor
dfCorDev <- apply(dfCovDev, 2, as.numeric)

corDev <- cor(dfCorDev[, -1:-2])

# heatmap(corDev)

# glm
print(names(dfCovDev))

logDevAdjustedCont <- glm(
    devgcs ~ .,
    family = binomial,
    data = dfCovDev[, c(-1, -11)]
)

# summary
summary(logDevAdjustedCont)

pDevAdjustedCont <- coef(summary(logDevAdjustedCont))[, "Pr(>|z|)"]

orDevAdjustedCont <- exp(coef(logDevAdjustedCont))

ciDevAdjustedCont <- exp(confint(logDevAdjustedCont))

print(pDevAdjustedCont)
print(orDevAdjustedCont)
print(ciDevAdjustedCont)

# //!SECTION

# //SECTION - forestploter

# //ANCHOR - hospMortality

library(tidyverse)

dfForestMorAdjustedCont <- data.frame(
    "Variable" = names(pMorAdjustedCont),
    "P value" = pMorAdjustedCont,
    "OR" = orMorAdjustedCont,
    "Lower" = ciMorAdjustedCont[, 1],
    "Upper" = ciMorAdjustedCont[, 2],
    row.names = NULL
)

dfForestMorAdjustedCont[, -1] <- round(dfForestMorAdjustedCont[, -1], 3)

dfForestMorAdjustedCont <- dfForestMorAdjustedCont %>%
    mutate("OR(95%CI)" = paste(OR, "(", Lower, ",", Upper, ")"), )

print(dfForestMorAdjustedCont)

resMorCont <- dfForestMorAdjustedCont

resMorCont$" " <- paste(rep("    ", nrow(resMorCont)), collapse = " ")

colnames(resMorCont) <- paste0(colnames(resMorCont), "1")

dim(resMorCont)
print(resMorCont)

# //ANCHOR - disgcs

library(tidyverse)

dfForestDisAdjustedCont <- data.frame(
    "Variable" = names(pDisAdjustedCont),
    "P value" = pDisAdjustedCont,
    "OR" = orDisAdjustedCont,
    "Lower" = ciDisAdjustedCont[, 1],
    "Upper" = ciDisAdjustedCont[, 2],
    row.names = NULL
)

dfForestDisAdjustedCont[, -1] <- round(dfForestDisAdjustedCont[, -1], 3)

dfForestDisAdjustedCont <- dfForestDisAdjustedCont %>%
    mutate("OR(95%CI)" = paste(OR, "(", Lower, ",", Upper, ")"), )

print(dfForestDisAdjustedCont)

resDisCont <- dfForestDisAdjustedCont

resDisCont$" " <- paste(rep("    ", nrow(resDisCont)), collapse = " ")

colnames(resDisCont) <- paste0(colnames(resDisCont), "2")

dim(resDisCont)
print(resDisCont)

# //ANCHOR - devgcs

library(tidyverse)

dfForestDevAdjustedCont <- data.frame(
    "Variable" = names(pDevAdjustedCont),
    "P value" = pDevAdjustedCont,
    "OR" = orDevAdjustedCont,
    "Lower" = ciDevAdjustedCont[, 1],
    "Upper" = ciDevAdjustedCont[, 2],
    row.names = NULL
)

dfForestDevAdjustedCont[, -1] <- round(dfForestDevAdjustedCont[, -1], 3)

dfForestDevAdjustedCont <- dfForestDevAdjustedCont %>%
    mutate("OR(95%CI)" = paste(OR, "(", Lower, ",", Upper, ")"), )

print(dfForestDevAdjustedCont)

resDevCont <- dfForestDevAdjustedCont

# Add a blank column for the forest plot to display CI
# Adjust the column width with space
resDevCont$" " <- paste(rep("    ", nrow(resDevCont)), collapse = " ")

colnames(resDevCont) <- paste0(colnames(resDevCont), "3")

dim(resDevCont)
print(resDevCont)

# //ANCHOR - cbind

resAllCont <- cbind(resMorCont, resDisCont, resDevCont)

resAllCont$" " <- paste(rep("NA", nrow(resAllCont)))

resAllCont <- resAllCont %>%
    mutate(Variable1 = recode(Variable1,
        "age" = "Age",
        "gender" = "Gender",
        "bmi" = "BMI",
        "hypertension" = "Hypertension",
        "cerebrovascular_disease" = "Cerebrovascular Disease",
        "peakmean" = "Peak",
        "ttpmean" = "TTP",
        "icpavgmean" = "Mean ICP",
        .default = Variable1
    ))

dim(resAllCont)
resAllCont[, 1]

# //ANCHOR - plot

library(forestploter)

resForestCont <- forest(
    data = resAllCont[, c(1, 2, 7, 6, 22, 9, 14, 13, 22, 16, 21, 20)],
    lower = list(
        resAllCont$Lower1, 
        resAllCont$Lower2,
        resAllCont$Lower3
    ),
    upper = list(
        resAllCont$Upper1, 
        resAllCont$Upper2,
        resAllCont$Upper3
    ),
    est = list(
        resAllCont$OR1, 
        resAllCont$OR2,
        resAllCont$OR3
    ),
    ci_column = c(3, 7, 11),
    ref_line = 1
    # xlim = c(0, 1.5)
)

# Insert text at the top
library(grid)

resForestCont <- insert_text(resForestCont,
    text = c("In-hospital mortality", "Discharge GCS", "GCS difference"),
    col = c(3, 7, 11),
    part = "header",
    just = "center",
    gp = gpar(fontface = "bold")
)

# Add underline at the bottom of the header
resForestCont <- add_border(resForestCont, part = "header", row = 1, where = "top", gp = gpar(lwd = 1))
resForestCont <- add_border(resForestCont, part = "header", row = 2, where = "bottom", gp = gpar(lwd = 1))
resForestCont <- add_border(resForestCont, part = "header", row = 1, where = "bottom", col = c(2:4, 6:8, 10:12), gp = gpar(lwd = 0.5))

print(resForestCont)

# //!SECTION