# Saptarshi's function to tag version by day
tagDaysByVersion <- function(d){
    ##  takes $data$days and returns a modified data$days
    ## with versioninfo attached as a field
    ## i've not done much error checking with this
    ## if you get errors notify me
    if(length(d$data$days)==0) return(d$data$days)
    days <- d$data$days [ order(names(d$data$days)) ]
    dates <- names(days)
    dversion <- rep(NA, length(dates))
    iversion <- NA
    verupdate <- rep(FALSE,length(dates))
    for(i in 1:length(dates)){
        vs <- days[[i]]$org.mozilla.appInfo.versions
        if(is.null(vs$appVersion)){
            dversion[i] <- iversion
        }else{
            iversion <- dversion[i] <- max(vs$appVersion) ## there can be several on a day
            verupdate[i] <- TRUE
        }
    }
    ## If alll of dversion is NA, there was never an update
    ## Force them to be equal to gecko values
    if(all(is.na(dversion))) dversion <- rep(d$geckoAppInfo$platformVersion, length(dates))
    days <- mapply(function(dc, dv, vu){
        dc$versioninfo <- list(version=dv,vup=vu)
        dc
    }, days, dversion,verupdate, SIMPLIFY=FALSE)
    return(days)
}

# Check that it is a new profile, created after version release date
checkNewProfileOnLatestVersion <- function(rData, version.release.date){
    # If it is standard profile on release channel
    if(is.standard.profile(rData) & on.release.channel(rData)){
        # If profile creation date is available, get creation date, else NULL
        profile.creation.date <- tryCatch(getProfileCreationDate(rData), error = function(e) { NULL }) 
        # If it isn't null, check whether profile was created after version release date
        if(!is.null(profile.creation.date)) {
            profile.creation.date > version.release.date
        }
    }
    else{
        return(FALSE)
    }
}

# Check if profile was targeted 
checkTargetedProfile <- function(rData){
    profInfo <- rData$data$last$org.mozilla.appInfo.appinfo 
    locale <- profInfo$locale 
    geo <- rData$geo
    distribtype <- profInfo$distributionID
    targeted <- isTargetedY(locale, distribtype, geo)
}

getDefaultForEachDay <- function(rData, allDaysInfo){
    distribtype <- rData$data$last$org.mozilla.appInfo.appinfo$distributionID
    getProviders <- list(
        yahoo = yahoo.searchnames(distribtype),
        google = google.searchnames(distribtype),
        bing = bing.searchnames(distribtype),
        other = NA
    )
    default.search <- inferDefaultSearchEngine(allDaysInfo)
    gf <- groupingFunction(getProviders)
    not.na <- !is.na(default.search)
    default.seq <- ifelse(not.na, gf(default.search), default.search)
    default.seq[order(names(default.seq))]
}

checkVersionHistory <- function(allDaysInfo, version){
    history <- unlist(lapply(allDaysInfo, function(day){
        dayVersion <- day$versioninfo$version
        if(is.na(dayVersion)) {
            return(dayVersion)
            } else {
                return(as.numeric(majorVersion(dayVersion)))
            }
            }))
    any(history < version , na.rm = TRUE)
    }

checkStatusConditions <- function(dailyDefault){
    didNotUseBrowser <- length(names(allDaysInfo)) <= 0 
    hasNoDefaultInfo <- length(dailyDefault) == sum(is.na(dailyDefault))
    if(didNotUseBrowser == FALSE & hasNoDefaultInfo == FALSE) return(TRUE)
}

checkDefaultSwitch <- function(allDaysInfo, dailyDefault){
        dailyDefault <- dailyDefault[!is.na(dailyDefault)]
        reason <- ifelse(length(dailyDefault) < 2, "didNotBrowseEnough", ifelse(dailyDefault == "yahoo", "didNotSwitch", "Switched"))
        if(reason == "Switched"){
            first.use <- names(dailyDefault[names(dailyDefault)][1])
            last.use <- names(dailyDefault[names(dailyDefault) != "yahoo"][1])
            time.switch <- as.Date(last.use) - as.Date(first.use)
            } else {
                time.switch <- 0   
            }
    return(list(reason = reason, timeToSwitch = time.switch))
}



