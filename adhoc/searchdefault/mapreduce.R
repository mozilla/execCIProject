# Q2 2015 
# Daily unsets of Yahoo as Default Search Set by previous Yahoo DSS users 
# (and which provider is set, as a percentage of new sets) 

loadYahoo()
library(rjson)
version.release.date <- as.Date("2015-03-31")
current.version <- 37

m = function(key,jsonData){
	rData <- fromJSON(jsonData)
	allDaysInfo <- tagDaysByVersion(rData)
	# Only look for standard profiles on release channel
	# Check that it is a new profile, created after version release date, and on the latest version
	# checkNewProfileOnLatestVersion takes care of these concerns
    if(!(is.standard.profile(rData) & on.release.channel(rData))) return()
    # If profile creation date is available, get creation date, else NULL
    pcd <- tryCatch(getProfileCreationDate(rData), error = function(e) { NULL }) 
    if(is.null(pcd)) return()
    # If it isn't null, check whether profile was created after version release date
    wantedProfile <- pcd >= version.release.date & get.current.version(rData) >= current.version
	if(!(wantedProfile)) return()
	# make sure that the valid profile has at least one days entry
	if(length(allDaysInfo) == 0) return()
	# check if the profile is Yahoo targeted. Yahoo targeted profiles should be in en-US locale, version 34 higher, in geo US, and correct distrib
	isTargeted <- checkTargetedProfile(rData)
	# just to check again, make sure that all the version history is 37 and higher to ensure "new" profile
	hasInvalidVersionHistory <- checkVersionHistory(allDaysInfo, current.version)
	if(hasInvalidVersionHistory) return()
	# To determine switching, a profile must have used the browser and have search default info
	dailyDefault <- getDefaultForEachDay(rData, allDaysInfo)
	#make sure we have info about default status
	if(length(dailyDefault) == sum(is.na(dailyDefault))) return()
	defaultInfo <- checkDefaultSwitch(allDaysInfo, dailyDefault)
	rhcollect(list(targetedY = isTargeted, status = defaultInfo$reason, days = c(defaultInfo$timeToSwitch)), c(profile = 1))
}

z = rhwatch(map=m
	,input=sqtxt("/user/sguha/fhr/samples/output/1pct")
	,reduce = rhoptions()$template$colsummer
	,setup = expression(map = {library(rjson)})
	,debug = "count"
	,read = FALSE
	)

s = rhread(z)