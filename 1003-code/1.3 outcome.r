# //ANCHOR - hospMortality

# Denoising the combined outcomes from two databases #

# ---------------------------------------------------------------------------- #
#                              General code block                              #
# ---------------------------------------------------------------------------- #

# import eicu
emortality <- read.csv("D:/Hai/321-stat/1003-stat/1003-oridata/emortality.csv", header = TRUE)

emortality0 <- emortality
names(emortality0) <- c("icuid", "hospMortality")

# import mimic
mmortality <- read.csv("D:/Hai/321-stat/1003-stat/1003-oridata/mmortality.csv", header = TRUE)

mmortality0 <- mmortality
names(mmortality0) <- c("icuid", "hospMortality")

# combine
mortality <- rbind(emortality0, mmortality0)

# filling
mortality$hospMortality[is.na(mortality$hospMortality)] <- 0

# //ANCHOR - gcs

# import eicu
egcs <- read.csv("D:/Hai/321-stat/1003-stat/1003-oridata/edev_gcs.csv", header = TRUE)

egcs0 <- egcs
names(egcs0) <- c("icuid", "admgcs", "disgcs", "devgcs")

# import mimic
mgcs <- read.csv("D:/Hai/321-stat/1003-stat/1003-oridata/mdev_gcs.csv", header = TRUE)

mgcs0 <- mgcs
names(mgcs0) <- c("icuid", "admgcs", "disgcs", "devgcs")

# combine
gcs <- rbind(egcs0, mgcs0)

# filling
# gcs$disgcs[is.na(gcs$disgcs)] <- 0
# # without missing data !!!

# gcs$devgcs[is.na(gcs$devgcs)] <- 0
# # without missing data !!!

# relabel
gcs$disgcs <- ifelse(gcs$disgcs <= 8, 1, 0)

gcs$devgcs <- ifelse(gcs$devgcs <= 0, 1, 0)

# //ANCHOR - link

dfMor <- merge(mortality, varsImp, by = "icuid", all = FALSE)
dfDis <- merge(gcs[, c(1, 3)], varsImp, by = "icuid", all = FALSE)
dfDev <- merge(gcs[, c(1, 4)], varsImp, by = "icuid", all = FALSE)

print(length(unique(dfMor$icuid)))
print(length(unique(dfDis$icuid)))
print(length(unique(dfDev$icuid)))
