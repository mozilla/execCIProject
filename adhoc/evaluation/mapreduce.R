# Evaluation project
# Goal of the project is to predict a profile's usage hours for the rest of the month, based on the first week of use. 
# Model will be based on all new users who arrived on 2015-03-01

loadYahoo()
start.date <- as.Date("2015-03-01")
m = function(key,jsonData){
	rData <- fromJSON(jsonData)
	if(!(is.standard.profile(rData) && on.release.channel(rData))) return()
	pcd <- tryCatch(getProfileCreationDate(rData), error = function(e) { NULL}) 
	if(is.null(pcd) || pcd != as.Date("2015-03-01")) return()  #only keep all new profiles created on 2015-03-01
	profinfo <- getProfInfo(rData)
   	firstWeek <- getFirstWeekInfo(rData, start.date)
   	defaultStatus <- getDefaultStatus(firstWeek)
	# rawSearches <- getSearches(interestWeek)
	weekSearchInfo <- getSearchesbyKeyNames(profinfo)
    weekActivityInfo <- totalActivity(firstWeek)


