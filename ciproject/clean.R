# load data from output folder
load.all <- rhls("/user/cchoi/output/fhr-lag")$file
lagData <- createDataTable(mergeAllFiles(load.all))

calcTrue <- lagData[order(sampledate, month),][daysSince >=120,][, list(true.mau = mean(tmau), true.search = mean(tsearch), true.time = mean(tsec)), by = month]
mergeTrueValue <- merge(lagData, calcTrue, by = 'month')
addPropData <- mergeTrueValue[, ":="(pmau = tmau/true.mau, psrch = tsearch/true.search, psec = tsec/true.time)]
cleanData <- addPropData[, (c("pmau", "psrch", "psec")) := lapply(.SD, forceToOne), .SDcols = c("pmau", "psrch", "psec")]

mauRange <- intTable(glm(pmau ~ daysSince, data= cleanData, quasibinomial))
srchRange <- intTable(glm(psrch ~ daysSince, data= cleanData, quasibinomial))
secRange <- intTable(glm(psec ~ daysSince, data= cleanData, quasibinomial))

currentInfoDate <- max(sampleCreationDates)-7
predRange <- getPredictionRange(sampleCreationDates)
predDates <- seq(as.Date(predRange$start), as.Date(predRange$end), "month")
daysSinceSampleCreated <- abs(predDates - currentInfoDate)
tableCI <- data.table(predDates, daysSinceSampleCreated)
tableCI$predDates <- as.Date(tableCI$predDates) - months(1)
mauInfo <- data.table(mau_est = mauRange[tableCI$daysSinceSampleCreated,][[1]], mau_lb = mauRange[tableCI$daysSinceSampleCreated,][[2]]*1.03, mau_ub = mauRange[tableCI$daysSinceSampleCreated,][[3]]*0.97)
srchInfo <- data.table(srch_est = srchRange[tableCI$daysSinceSampleCreated,][[1]], srch_lb = srchRange[tableCI$daysSinceSampleCreated,][[2]]*1.03, srch_ub = srchRange[tableCI$daysSinceSampleCreated,][[3]]*0.97)
secInfo <- data.table(sec_est = secRange[tableCI$daysSinceSampleCreated,][[1]], sec_lb = secRange[tableCI$daysSinceSampleCreated,][[2]]*1.03, sec_ub = secRange[tableCI$daysSinceSampleCreated,][[3]]*0.97)
finalTable <- data.table(tableCI, mauInfo, srchInfo, secInfo)

write.table(finalTable, file = "~/output/confidence_interval .csv", sep = ",", row.names = FALSE)





