# //ANCHOR - hospMortality

# library(rcssci)

# rcs_logistic.ushap(
#     # knot = 3,
#     data = dfCovMor,
#     y = "hospMortality",
#     x = "ttpmean",
#     covs = c(
#         "age",
#         "gender",
#         "bmi",
#         "hypertension",
#         "cerebrovascular_disease"
#     ),
#     prob = 0.1,
#     filepath = "D:/"
# )

library(rms)

distMor <- datadist(dfCovMor)

options(datadist = dist)

fitMor <- lrm(
    hospMortality ~ rcs(ttpmean, 4) +
        age + gender + bmi + hypertension + cerebrovascular_disease,
    data = dfCovMor,
    x = TRUE, y = TRUE
)

summary(fitMor)

anova(fitMor)

predMor <- rms::Predict(
    fitMor, 
    ttpmean, 
    fun = exp
    )

ggplot(predMor)

# //ANCHOR - disgcs

library(rms)

dist <- datadist(dfCovDis)

options(datadist = dist)

fitDis <- lrm(
    disgcs ~ rcs(ttpmean, 4) +
        age + gender + bmi + hypertension + cerebrovascular_disease,
    data = dfCovDis,
    x = TRUE, y = TRUE
)

summary(fitDis)

anova(fitDis)

predDis <- rms::Predict(
    fitDis, 
    ttpmean, 
    fun = exp
    )

ggplot(predDis)

# //ANCHOR - devgcs

library(rms)

dist <- datadist(dfCovDev)

options(datadist = dist)

fitDev <- lrm(
    devgcs ~ rcs(ttpmean, 4) +
        age + gender + bmi + hypertension + cerebrovascular_disease,
    data = dfCovDev,
    x = TRUE, y = TRUE
)

summary(fitDev)

anova(fitDev)

predDev <- rms::Predict(
    fitDev, 
    ttpmean, 
    fun = exp
    )

ggplot(predDev)
