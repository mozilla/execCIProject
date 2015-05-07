Sys.setenv(USER="cchoi")
source("/usr/local/share/rhipe.mozilla.setup.R")
source("~/code/default-analysis-functions.R")
library(data.table)
library(rjson)
		
##################################################################################################
##################################################################################################
# Doorhanger launched. Aurora V34. Get default proportions for new profiles.
# Date: Sept 30, 2014.

allversion <- list(
list(releaseVersion = "31", releaseDate = "2014-04-29"),
list(releaseVersion = "32", releaseDate = "2014-06-10"),
list(releaseVersion = "33", releaseDate = "2014-07-21"),
list(releaseVersion = "34", releaseDate = "2014-09-03"))

m = function(k,r){
	r <- fromJSON(r)
	if(!(checkBasicReq(r, "Firefox", "aurora", "2014-04-29"))) return()
	days <- tagDaysByBuildVersion(r)
	creationDate <- as.Date(r$data$last$org.mozilla.profile.age$profileCreation, origin = "1970-01-01")
	daysInterested <- days[names(days) >= creationDate & names(days) <= creationDate + 7]
	for(v in allversion){
		version <- v$releaseVersion
		if(!(checkProfileReq(days, creationDate, v$releaseDate, v$releaseVersion))) next()
		if(length(daysInterested) == 0) return()
		if(checkSameVersion(daysInterested, v$releaseVersion)){
			default <- defaultDirect(daysInterested)
			} else {
			default <- defaultNonDirect(daysInterested, v$releaseVersion)
		}
		if(is.na(default)) return()
		rhcollect(list(version = as.character(version)), c(default = default, profile = 1))	
		}
	}
	

##################################################################################################
##################################################################################################

#look at just V34 by BuildID for Aurora

defaultCalc <- function(daysInterested){
	eligibleData <- Filter(function(r) {
		!is.null(r$org.mozilla.appInfo.appinfo$isDefaultBrowser)
	}, daysInterested)
	if(length(eligibleData) == 0) return(NA)
	defaultInput <- unlist(lapply(daysInterested, function(s){
				s$org.mozilla.appInfo.appinfo$isDefaultBrowser
			}))
	ifelse(sum(defaultInput) > (0.5*length(defaultInput)), 1, 0)
}

m = function(k,r){
	r <- fromJSON(r)
	if(!(checkBasicReq(r, "Firefox", "aurora", "2014-09-03"))) return()
	days <- tagDaysByBuildVersion(r)
	creationDate <- as.Date(r$data$last$org.mozilla.profile.age$profileCreation, origin = "1970-01-01")
	releaseVersion <- "34"
	releaseDate <- as.Date("2014-09-03")
	if(!(checkProfileReq(days, creationDate, releaseDate, releaseVersion))) return()
	if(length(days) == 0) return()
	historyBuildID <- unlist(lapply(days, function(s){
		buildInfo <- s$buildInfo$build
	}))
	minBuild <- min(historyBuildID, na.rm = TRUE)
	if(is.na(minBuild)) return()
	if(!(minBuild > "20140901")) return()

	if(minBuild < "20141013"){
		buildID <- "old"
		default <- defaultCalc(days)
	} else {
		buildID <- "new"
		default <- defaultCalc(days)
	}
	if(is.na(default)) return()
	s = rhcollect(list(buildID = buildID), c(default = default, profile = 1))	
	}

	z = rhwatch(map=m
		,input=sqtxt("/user/sguha/fhr/samples/output/aurora")
		,reduce = rhoptions()$template$colsummer
		,setup = expression(map = {library(rjson)})
		#,debug = "collect"
		,debug = "count"
		,read = FALSE
		)

# s = rhread(z)
# dd = make.dt(s,c("buildID", "defaultStatus", "profile"))


