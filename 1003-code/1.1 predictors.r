# //SECTION - preprocess

# Merging and denoising ICP data from two databases #

library(tidyverse)

# ---------------------------------------------------------------------------- #
#                             time interval: 5 mins                            #
# ---------------------------------------------------------------------------- #

# //ANCHOR - import

# mimic
micp <- read.csv("D:/Hai/321-stat/1003-stat/1003-oridata/micp.csv", header = TRUE)

micp0 <- micp
names(micp0)[1] <- c("icuid")

# eicu
eicp <- read.csv("D:/Hai/321-stat/1003-stat/1003-oridata/eicp.csv", header = TRUE)

eicp0 <- eicp
names(eicp0)[1] <- c("icuid")

# //ANCHOR - combine

# 5 min time window

# combine
icp <- rbind(eicp0, micp0)

# //ANCHOR - denoise

icp0 <- icp

# denoise of icp
icp0$icp <-
    ifelse(icp0$icp >= 100, NA, icp0$icp)

# summary
# hist(icp0$icp)
# qqnorm(icp0$icp)
# qqline(icp0$icp)

print(length(unique(icp0$icuid)))

# //!SECTION

# //SECTION - indicators

# Calculate the parameters for each episode of intracranial hypertension separately #

library(tidyverse)

# //ANCHOR - locate

# filter
ih0 <- icp0 %>%
    filter(!is.na(icp), icp >= 20)

print(length(unique(ih0$icuid)))

# calculate day number
ih1 <- ih0 %>%
    mutate(day = ((interval * 5) %/% (60 * 24)) + 1)

# calculate seq
ih1 <- ih1 %>%
    group_by(icuid) %>%
    arrange(icuid, interval) %>%
    mutate(
        seq = cumsum(c(0, diff(interval) != 1L))
    ) %>%
    ungroup()

# filter the data with 2 consecutive records of ih
ih1 <- ih1 %>%
  group_by(icuid, seq) %>%
  filter(n() >= 2) %>%
  ungroup()

# calculate start time and end time
ih1 <- ih1 %>%
    group_by(icuid, seq) %>%
    mutate(
        starttime = first(interval),
        endtime = last(interval)
    ) %>%
    ungroup()

# calculate los of intracranial hypertension
ih1 <- ih1 %>%
    group_by(icuid, seq) %>%
    mutate(los = endtime - starttime) %>%
    ungroup()

# filter the data with >= 24h los
ih1 <- ih1 %>%
    group_by(icuid) %>%
    filter(!any(los >= 288)) %>% # the interval is set at 5 mins, and the unit of los is also 5 mins
    ungroup()

# calculate icp peak and time to peak
ih1 <- ih1 %>%
    group_by(icuid, seq) %>%
    mutate(
        peak = max(icp),
        ttp = interval[which.max(icp)] - starttime,
        pte = endtime - interval[which.max(icp)]
    ) %>%
    ungroup()

# calculate the slope
ih1 <- ih1 %>%
    group_by(icuid, seq) %>%
    mutate(
        posslope = ifelse(
            ttp != 0, peak / ttp, NA
        ),
        negslope = ifelse(
            pte != 0, peak / pte, NA
        )
    ) %>%
    ungroup()

# //ANCHOR - dose

# using the trapezoidal rule to calculate the pressure time dose
area_under_curve <- function(interval, value) {
    sum(0.5 * (value[-length(value)] + value[-1]) * diff(interval))
}

# icp dose during each episode of intracranial hypertension
ih2 <- ih1 %>%
    group_by(icuid, seq) %>%
    mutate(icpdose = area_under_curve(interval, icp) * 5) %>%
    ungroup()

# //ANCHOR - avg

# mean icp during each episode of intracranial hypertension
ih3 <- ih2 %>%
    group_by(icuid, seq) %>%
    mutate(icpavg = mean(icp)) %>%
    ungroup()

# delete cols
names(ih3)

doseDf <- ih3 %>%
    select(-interval, -icp) %>%
    distinct()

# //!SECTION

# //SECTION - features

# Parameter Summary: 
# Single intracranial hypertension episode parameters, 
# daily cumulative intracranial hypertension episode parameters, 
# and day-by-day cumulative intracranial hypertension episode parameters

doseDf0 <- doseDf

names(doseDf0)

# //ANCHOR - single wave and avg

# single wave
# posslopemean and negslopemean exhibit significant errors and should be discarded
doseDf1 <- doseDf0 %>%
    select(-day, -seq, -starttime, -endtime) %>%
    group_by(icuid) %>%
    summarize(
        losmean = mean(los),
        peakmean = mean(peak),
        ttpmean = mean(ttp),
        ptemean = mean(pte),
        # posslopemean = mean(posslope),
        # negslopemean = mean(negslope),
        icpdosemean = mean(icpdose),
        icpavgmean = mean(icpavg)
    ) %>%
    ungroup() %>%
    distinct()

sinDf <- doseDf1

# //ANCHOR - daily dose and avg

# icp dose and icp avg during daily episodes of intracranial hypertension
doseDf2 <- doseDf0 %>%
    select(icuid, day, icpdose, icpavg) %>%
    group_by(icuid, day) %>%
    summarize(
        icpdose = sum(icpdose),
        icpavg = sum(icpavg)
    ) %>%
    distinct()

# daily complete
doseDf3 <- doseDf2 %>%
    group_by(icuid) %>%
    complete(day = 1:5, fill = list(
        icpdose = 0,
        icpavg = 0
    ))

# cumulative icp dose and icp avg of intracranial hypertension episodes per day
doseDf4 <- doseDf3 %>%
    group_by(icuid) %>%
    mutate(
        icpdosesum = cumsum(icpdose),
        icpavgsum = cumsum(icpavg)
    ) %>%
    ungroup()

names(doseDf4)

doseDfSinDay <- doseDf4 %>% 
    select(icuid, day, icpdose) %>% 
    pivot_wider(names_from = day, values_from = icpdose) %>% 
    rename_with(~ paste0("doseDay", .), -1)

avgDfSinDay <- doseDf4 %>% 
    select(icuid, day, icpavg) %>% 
    pivot_wider(names_from = day, values_from = icpavg) %>% 
    rename_with(~ paste0("avgDay", .), -1)

doseDfSumDay <- doseDf4 %>% 
    select(icuid, day, icpdosesum) %>% 
    pivot_wider(names_from = day, values_from = icpdosesum) %>% 
    rename_with(~ paste0("doseSum", .), -1)

avgDfSumDay <- doseDf4 %>% 
    select(icuid, day, icpavgsum) %>% 
    pivot_wider(names_from = day, values_from = icpavgsum) %>% 
    rename_with(~ paste0("avgSum", .), -1)

df1 <- merge(doseDfSinDay, avgDfSinDay, by = "icuid", all = FALSE)
df2 <- merge(df1, doseDfSumDay, by = "icuid", all = FALSE)
df3 <- merge(df2, avgDfSumDay, by = "icuid", all = FALSE)

daySumDf <- df3

# //ANCHOR - all

# merge
doseAvgDfAll <- merge(sinDf, daySumDf, by = "icuid", all = FALSE)

# round
doseAvgDfAll[, -1] <- round(doseAvgDfAll[, -1], 2)

print(length(unique(doseAvgDfAll$icuid)))
print(names(doseAvgDfAll))

print(sum(unique(doseAvgDfAll$icuid) %in% micp0$icuid))
print(sum(unique(doseAvgDfAll$icuid) %in% eicp0$icuid))

# # //ANCHOR - wave plot

# sampleDf <- ih0[160:200, ]

# library(ggplot2)

# ggplot(
#     sampleDf, 
#     aes(x = interval, y = icp)) +
#     geom_line()

# //ANCHOR - description

summary(doseAvgDfAll)

# //!SECTION
