sampleCreationDates <-getSampleCreationDates("/user/sguha/fhr/samples/backup")
fileCreationDates <- getSampleCreationDates("/user/cchoi/output/fhr-lag")
listDates <- getStartAndEnd(sampleCreationDates)
listOfMonths <- createMonthList(listDates)

m = function(key,jsonData){
	rData <- fromJSON(jsonData)
	allDaysInfo <- rData$data$days
	if(!is.standard.profile(rData)) return()
	for (month in listOfMonths){
		if(!isWithinDateRange(sampledate)) return()
		days <- getInterestDaterangeInfo(month, allDaysInfo)
		daysSinceMonthEnded <- as.numeric(sampledate - month$end.date + 1)
		whichmonth <- as.numeric(format(month$start.date, "%m"))
		totalsec <- totalActivity(days)$totalsec
		totalsrch <- getTotalSearch(days)
		totalmau <- getMAUInfo(month, rData)
		# if sample was created before the month was over, the sample should not be calculating the tmau
		rhcollect(list(month = whichmonth, sampledate = as.character(sampledate), daysSince = daysSinceMonthEnded), 
		c(tmau = totalmau, tsec = totalsec, tsearch = totalsrch))
	}
}

sampleToRun <- checkForFilesNotRun(sampleCreationDates, fileCreationDates)
all.samples <- rhls("/user/sguha/fhr/samples/backup")$file
for (sample in all.samples){
	sampledate <- as.Date(tail(strsplit(sample,"/")[[1]],1))
	if(!(sampledate %in% sampleToRun)) next
	if(!sampleCreatedAfterMonthEnded(listDates, sample)) next
		z = rhwatch(map=m,
		mapred=list(mapred.job.priority = "VERY_HIGH"),
		jobname = "monthly confidence interval",
		input=sqtxt(sample),
		param = list(sampledate = sampledate),
		output = sprintf("/user/cchoi/output/fhr-lag/%s", sampledate),
		reduce = rhoptions()$template$colsummer,
		setup = expression(map = {library(rjson)}),
		debug = "count",
		read = FALSE
	)
}

# if the folder dates are greater than the month but not specified end date, then don't run.
# if the folder is after the specified date, then run