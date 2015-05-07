
#if null or length 0, return true. else return false.
isNull <- function(d){
	if (is.null(d) || length(d) == 0){
		return(TRUE)
	}
	else{
		return(FALSE)
	}
}

replaceMissing <- function(d, replacement){
	if (is.null(d) || length(d) == 0){
		replacement
	}
	else {
		d
	}
}


#check basic requirements for the profile
checkBasicReq <- function(d, name, channel, dateAfter){
	# if name is null, invalid
	if(isNull(d$geckoAppInfo$name)) 
	    return(FALSE)
    # if channel is null, invalid
	if(isNull(d$geckoAppInfo$updateChannel)) 
	   return(FALSE)
    # if date creations is null, invalid
	if(isNull(d$data$last$org.mozilla.profile.age$profileCreation)) 
	    return(FALSE)
	creationDate <- as.Date(d$data$last$org.mozilla.profile.age$profileCreation, origin = "1970-01-01")
	# if creation date is on/before the dateAfter, invalid
	if(creationDate <= dateAfter) 
		return(FALSE)
	# valid!
	return(TRUE)
}

#check if profile quilifies for two conditions
#1. profile was created after version was released
#1. the minimum version in profile history is the same as release version of interest
checkProfileReq <- function(days, creationDate, releaseDate, releaseVersion){
	#if date creation is not on/after releaseDate, invalid
	if(!(creationDate >= releaseDate)) 
		return(FALSE)
	#return all versions in profile history in allHistory
	allHistory <- unlist(lapply(days, function(s){
		versionInfo <- substring(s$buildInfo$version, 1, 2)
		versionInfo <- as.numeric(as.character(versionInfo))
	}))
	minVersion <- min(allHistory, na.rm = TRUE)
	# if minimum version is not the same as the release version of interest, invalid
	if(!(minVersion == as.numeric(as.character(releaseVersion)))) 
		return(FALSE)
	#v valid!
		return(TRUE)
}


#check that the profile did not have update in daysInterest = period of interest
checkSameVersion<- function(daysInterested, releaseVersion){
	interestHistory <- unlist(lapply(daysInterested, function(s){
		versionInfo <- substring(s$buildInfo$version, 1, 2)
		versionInfo <- as.numeric(as.character(versionInfo))
	}))
	# if there is a missing value in the list, then replace with the releaseVersion
	interestHistory <- ifelse(is.na(interestHistory), releaseVersion, interestHistory)
	# if there are more than one unique version return TRUE
	if(!(length(unique(interestHistory)) == 1)) 
		return(FALSE)
	# otherwise return FALSE
		return(TRUE)
}


#calculate default status looking at the days of interest if 
#the profile has just one unique version in this specific history

defaultDirect <- function(daysInterested){
	eligibleData <- Filter(function(r) {
		!is.null(r$org.mozilla.appInfo.appinfo$isDefaultBrowser)
	}, daysInterested)
	if(length(eligibleData) == 0) return(NA)
	defaultInput <- unlist(lapply(daysInterested, function(s){
				s$org.mozilla.appInfo.appinfo$isDefaultBrowser
			}))
	ifelse(sum(defaultInput) > (0.5*length(defaultInput)), 1, 0)
}

#calculate default status looking at the days of interest if 
#the profile has more than one unique version in this specific history
#daysInterested = all the data$days in the interest date range
#releaseVersion = the version element
defaultNonDirect <- function(daysInterested, releaseVersion){
	interestHistory <- unlist(lapply(daysInterested, function(s){
		versionInfo <- substring(s$buildInfo$version, 1, 2)
		versionInfo <- as.numeric(as.character(versionInfo))
	}))
	interestHistory <- ifelse(is.na(interestHistory), releaseVersion, interestHistory)
	changePointDate <- as.Date(names(which.max(interestHistory != releaseVersion)))
	versionData <- days[names(days) >= creationDate & names(days) < changePointDate]
	eligibleData <- Filter(function(r) {
		!is.null(r$org.mozilla.appInfo.appinfo$isDefaultBrowser)
			}, versionData)
			if(length(eligibleData) == 0) return(NA)
			defaultInput <- unlist(lapply(eligibleData, function(s){
					s$org.mozilla.appInfo.appinfo$isDefaultBrowser
					}))
			ifelse(sum(defaultInput) > (0.5*length(defaultInput)), 1, 0)
	
}



####################################

# saptarshi's function to fill in missing missing values
tagDaysByBuildVersion <- function(d){
    if(length(d$data$days)==0) return(d$data$days)
    days <- d$data$days [ order(names(d$data$days)) ]
    dates <- names(days)
    dversion <- dbuildID <- rep(NA, length(dates))
    iversion <- ibuild <- NA
    verupdate <- bdupdate <- rep(FALSE,length(dates))
    for(i in 1:length(dates)){
        vs <- days[[i]]$org.mozilla.appInfo.versions
        if(is.null(vs$appVersion)){
            dversion[i] <- iversion
        }else{
            iversion <- dversion[i] <- max(vs$appVersion) ## there can be several on a day
            verupdate[i] <- TRUE
        }
        if(is.null(vs$appBuildID)){
            dbuildID[i] <- ibuild
        }else{
            ibuild <- dbuildID[i] <- substr(max(vs$appBuildID),1,8) ## there can be several on a day
            bdupdate[i] <- TRUE
        }
    }
    ## If alll of dversion/dbuild is NA, there was never an update
    ## Force them to be equal to gecko values
    if(all(is.na(dversion))) dversion <- rep(d$geckoAppInfo$platformVersion, length(dates))
    if(all(is.na(dbuildID))) dbuildID <- rep(substr(d$geckoAppInfo$appBuildID,1,8),length(dates))
    days <- mapply(function(dc, dv, db,vu,bu){
        dc$buildInfo <- list(version=dv, build=db,vup=vu, bup=bu)
        dc
    }, days, dversion,dbuildID,verupdate,bdupdate, SIMPLIFY=FALSE)
    return(days)
}



fn = function(r,  base=1, index,queryString=NULL,F=function(w) w,doUnlist=TRUE){
  ## every list element is a k-v pair and the queryString is
  ## always element 1 and index 1
  base <- if(base=="v") 2 else if(base=="k") 1 else stop("wrong value of base")
  r <- if(!is.null(queryString))  r[ unlist(lapply(r, function(s) queryString(s[[1]]))) ] else r
  h <- if(index>0) (lapply(r,function(s) F(s[[ base ]] [[ index]])))
  else (lapply(r,function(s) F(s[[ base ]] )))
  if(is.logical(doUnlist) && doUnlist)
    unlist(h)
  else if(is.logical(doUnlist) && !doUnlist)
    h
  else if(is.function(doUnlist))
    doUnlist(h)
}


## however expects [,) type intervals
## changing the above function slightly
## so that >=0 and <90, >=90 and <180, >=180 and <270 and so on ...
creationGroup2 <- function(daysSince, intervalVector, nameVector){
    nameVector[ findInterval(example,intervalVector) ]
}

creationVector <- list(intervals=c(-Inf,0,90, 180, 270, 360, 450,Inf),names=c("negative","0", "90d","180d","270d","360d","450d","+450d"))
example <- c(-10, 0,10,90,100,180,200,270,300,360,400,450,500)
data.frame( a=example, b=creationGroup2( example, creationVector$intervals, creationVector$names))
