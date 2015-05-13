loadYahoo()
library(rjson)
version.release.date <- as.Date("2015-03-31")

m = function(key,jsonData){
	rData <- fromJSON(jsonData)
	allDaysInfo <- tagDaysByVersion(rData)
	# Check the profile is new, targeted, and has version history does not contain anything less than desired
	isNewProfile <- checkNewProfileOnLatestVersion(rData, version.release.date)
	if(!isNewProfile) return()
	isTargeted <- checkTargetedProfile(rData)
	if(!isTargeted) return()
	hasInvalidVersionHistory <- checkVersionHistory(allDaysInfo, 37)
	if(hasInvalidVersionHistory) return()
	# To determine switching, a profile must have used the browser and have search default info
	dailyDefault <- getDefaultForEachDay(rData, allDaysInfo)
	meetConditions <- checkStatusConditions(dailyDefault)
	if(!meetConditions) return()
	defaultInfo <- checkDefaultSwitch(allDaysInfo, dailyDefault)
	rhcollect(list(status = defaultInfo$reason, days = defaultInfo$timeToSwitch), c(profile = 1))
}

z = rhwatch(map=m
	,input=sqtxt("/user/sguha/fhr/samples/backup/2015-04-20/1pct")
	,reduce = rhoptions()$template$colsummer
	,setup = expression(map = {library(rjson)})
	,debug = "count"
	,read = FALSE
	)