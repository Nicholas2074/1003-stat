# //ANCHOR - mortality

# including patient, score, diagnosis, surgery, ttpmean
baselineGroup1 <- dfMor[, c(1:29, 77:80, 82)]

library(compareGroups)

tableMor <- descrTable(hospMortality ~ . - icuid,
    data = baselineGroup1,
    method = NA,
    show.all = TRUE
)
# tableMor

# export2word(tableMor, file = "tableMor.docx")

# //ANCHOR - ttpCat

# including ttpCat
baselineGroup2 <- merge(baselineGroup1[, -2], dfCovMor[, c(1, 11)], by = "icuid", all = FALSE)

library(compareGroups)

tableTTP <- descrTable(ttpCat ~ . - icuid,
    data = baselineGroup2,
    method = NA,
    show.all = TRUE
)
# tableTTP

# export2word(tableTTP, file = "tableTTP.docx")
