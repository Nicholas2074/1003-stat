# //SECTION - multiglm

library(tidyverse)

# //ANCHOR - icdcode

# import

# mimic
michid <- read.csv("D:/Hai/321-stat/1003-stat/1003-oridata/michid_0.csv", header = TRUE)

michid0 <- as.data.frame(michid[, 3])
names(michid0)[1] <- c("icuid")

msahid <- read.csv("D:/Hai/321-stat/1003-stat/1003-oridata/msahid_0.csv", header = TRUE)

msahid0 <- as.data.frame(msahid[, 3])
names(msahid0)[1] <- c("icuid")

mtbiid <- read.csv("D:/Hai/321-stat/1003-stat/1003-oridata/mtbiid_0.csv", header = TRUE)

mtbiid0 <- as.data.frame(mtbiid[, 3])
names(mtbiid0)[1] <- c("icuid")

# eicu
eichid <- read.csv("D:/Hai/321-stat/1003-stat/1003-oridata/eichid_0.csv", header = TRUE)

eichid0 <- eichid
names(eichid0)[1] <- c("icuid")

esahid <- read.csv("D:/Hai/321-stat/1003-stat/1003-oridata/esahid_0.csv", header = TRUE)

esahid0 <- esahid
names(esahid0)[1] <- c("icuid")

etbiid <- read.csv("D:/Hai/321-stat/1003-stat/1003-oridata/etbiid_0.csv", header = TRUE)

etbiid0 <- etbiid
names(etbiid0)[1] <- c("icuid")

# merge
ichid <- rbind(eichid0, michid0)
sahid <- rbind(esahid0, msahid0)
tbiid <- rbind(etbiid0, mtbiid0)

# summary

print(length(unique(varsImp$icuid)))

print(length(unique(doseAvgDfAll$icuid)))

print(sum(unique(doseAvgDfAll$icuid) %in% ichid$icuid))

print(sum(unique(doseAvgDfAll$icuid) %in% sahid$icuid))

print(sum(unique(doseAvgDfAll$icuid) %in% tbiid$icuid))

print(length(unique(df3$icuid)))

df1 <- merge(varsImp, mortality, by = "icuid", all = FALSE)
df2 <- merge(df1, gcs[, c(1, 3)], by = "icuid", all = FALSE)
df3 <- merge(df2, gcs[, c(1, 4)], by = "icuid", all = FALSE)

df4 <- df3 %>% 
  mutate(
    icd = case_when(
      icuid %in% ichid$icuid ~ 1,
      icuid %in% sahid$icuid ~ 2,
      TRUE ~ 3
    ),
    icd = factor(
      icd,
      levels = c(1, 2, 3),
      labels = c("ICH", "SAH", "TBI")
    )
  )

dfSubGroup <- df4[, c(
    "icuid",
    "hospMortality",
    "disgcs",
    "devgcs",
    "ttpmean",
    "age",
    "gender",
    "bmi",
    "gcs",
    "hypertension",
    "cerebrovascular_disease",
    "diabetes",
    "craniotomy",
    "ventriculostomy",
    "icd"
)]

subVars <- c(
    "icd",
    "age",
    "gender",
    "bmi",
    # "gcs",
    "hypertension",
    "cerebrovascular_disease",
    "diabetes",
    "craniotomy",
    "ventriculostomy"
)

# factor
summary(dfSubGroup$age)

dfSubGroup <- dfSubGroup %>%
    mutate(
        age = case_when(
            age < 56 ~ 1,
            age < 71 ~ 2,
            TRUE ~ 3
        ),
        age = factor(
            age,
            levels = c(1, 2, 3),
            labels = c("< 56", "56 ~ 70", "≥ 71")
        )
    )

summary(dfSubGroup$bmi)

dfSubGroup <- dfSubGroup %>%
    mutate(bmi = case_when(
        bmi < 27.4 ~ 1,
        bmi < 31.2 ~ 2,
        TRUE ~ 3
    ),
    bmi = factor(
        bmi,
        levels = c(1, 2, 3),
        labels = c("< 27.4", "27.4 ~ 31.2", "≥ 31.2")
    )
    )

summary(dfSubGroup$gcs)

dfSubGroup <- dfSubGroup %>%
    mutate(gcs = factor(
        gcs,
        levels = c(1, 2),
        labels = c("≤ 8", "≥ 9")
    ))

dfSubGroup <- dfSubGroup %>%
    mutate(
        gender = factor(
        gender,
        levels = c(0, 1),
        labels = c("Male", "Female")
        ),
        hypertension = factor(
        hypertension,
        levels = c(1, 2),
        labels = c("NO", "YES")
        ),
        cerebrovascular_disease = factor(
        cerebrovascular_disease,
        levels = c("1", "2"),
        labels = c("NO", "YES")
        ),
        diabetes = factor(
        diabetes,
        levels = c(1, 2),
        labels = c("NO", "YES")
        ),
        craniotomy = factor(
        craniotomy,
        levels = c(1, 2),
        labels = c("NO", "YES")
        ),
        ventriculostomy = factor(
        ventriculostomy,
        levels = c(1, 2),
        labels = c("NO", "YES")
    ))

# //ANCHOR - hospMortality

# install.packages("jstable")

library(jstable)

jsMor <- TableSubgroupMultiGLM(
    formula = hospMortality ~ ttpmean,
    var_subgroups = subVars,
    data = dfSubGroup,
    family = "binomial"
)

print(jsMor)

jsMor <- jsMor[, c(
    "Variable", "Count", "OR", "Lower", "Upper", "P value", "P for interaction"
)]

jsMor[, 3:5] <- lapply(jsMor[, 3:5], as.numeric)

jsMor <- jsMor %>% 
    mutate("OR(95%CI)" = paste(OR, "(", Lower, ",", Upper, ")"), )

jsMor$" " <- paste(rep("  ", nrow(jsMor)), collapse = " ")

names(jsMor)

colnames(jsMor) <- paste0(colnames(jsMor), "1")

jsMor[, c(2, 6, 7)][is.na(jsMor[, c(2, 6, 7)])] <- " "

# //ANCHOR - disgcs

library(jstable)

jsDis <- TableSubgroupMultiGLM(
    formula = disgcs ~ ttpmean,
    var_subgroups = subVars,
    data = dfSubGroup,
    family = "binomial"
)

print(jsDis)

jsDis <- jsDis[, c(
    "Variable", "Count", "OR", "Lower", "Upper", "P value", "P for interaction"
)]

jsDis[, 3:5] <- lapply(jsDis[, 3:5], as.numeric)

jsDis <- jsDis %>% 
    mutate("OR(95%CI)" = paste(OR, "(", Lower, ",", Upper, ")"), )

jsDis$" " <- paste(rep("  ", nrow(jsDis)), collapse = " ")

colnames(jsDis) <- paste0(colnames(jsDis), "2")

jsDis[, c(2, 6, 7)][is.na(jsDis[, c(2, 6, 7)])] <- " "

# //ANCHOR - devgcs

library(jstable)

jsDev <- TableSubgroupMultiGLM(
    formula = devgcs ~ ttpmean,
    var_subgroups = subVars,
    data = dfSubGroup,
    family = "binomial"
)

print(jsDev)

jsDev <- jsDev[, c(
    "Variable", "Count", "OR", "Lower", "Upper", "P value", "P for interaction"
)]

jsDev[, 3:5] <- lapply(jsDev[, 3:5], as.numeric)

jsDev <- jsDev %>% 
    mutate("OR(95%CI)" = paste(OR, "(", Lower, ",", Upper, ")"), )

jsDev$" " <- paste(rep("  ", nrow(jsDev)), collapse = " ")

colnames(jsDev) <- paste0(colnames(jsDev), "3")

jsDev[, c(2, 6, 7)][is.na(jsDev[, c(2, 6, 7)])] <- " "

# //ANCHOR - cbind

jsAll <- cbind(jsMor, jsDis, jsDev)

jsAll$" " <- paste(rep("NA", nrow(jsAll)))

jsAll[, 1]

jsAll <- jsAll %>%
    mutate(Variable1 = recode(Variable1,
        "age" = "Age",
        "gender" = "Gender",
        "bmi" = "BMI",
        "hypertension" = "Hypertension",
        "cerebrovascular_disease" = "Cerebrovascular Disease",
        "diabetes" = "Diabetes",
        "craniotomy" = "Craniotomy",
        "ventriculostomy" = "Ventriculostomy",
        "icd" = "Diagnosis",
        .default = Variable1
    ))

dim(jsAll)
names(jsAll)

# //ANCHOR - forestplot

library(forestploter)

jsForest <- forest(
    data = jsAll[, c(1, 2, 8, 9, 6, 7, 28, 17, 18, 15, 16, 28, 26, 27, 24, 25)],
    lower = list(
        jsAll$Lower1, 
        jsAll$Lower2,
        jsAll$Lower3
    ),
    upper = list(
        jsAll$Upper1, 
        jsAll$Upper2,
        jsAll$Upper3
    ),
    est = list(
        jsAll$OR1, 
        jsAll$OR2,
        jsAll$OR3
    ),
    ci_column = c(4, 9, 14),
    ref_line = 1
    # xlim = c(0, 1.5)
)

# Insert text at the top
library(grid)

jsForest <- insert_text(jsForest,
    text = c("In-hospital mortality", "Discharge GCS", "GCS difference"),
    col = c(4, 9, 14),
    part = "header",
    just = "center",
    gp = gpar(fontface = "bold")
)

# Add underline at the bottom of the header
jsForest <- add_border(jsForest, part = "header", row = 1, where = "top", gp = gpar(lwd = 1))
jsForest <- add_border(jsForest, part = "header", row = 2, where = "bottom", gp = gpar(lwd = 1))
jsForest <- add_border(jsForest, part = "header", row = 1, where = "bottom", col = c(3:6, 8:11, 13:16), gp = gpar(lwd = 0.5))

print(jsForest)

# //!SECTION