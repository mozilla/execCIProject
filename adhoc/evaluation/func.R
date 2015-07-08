library(rjson)
library(lubridate)

getActivityInfo <- function(rData, start.date, end.date){
	all.days <- rData$data$days
	all.days[names(all.days) >= as.Date(start.date) & names(all.days) <= as.Date(end.date)]
}

getFirstWeekInfo <- function(rData, start.date){
    one.week <- start.date + days(7)
    get.active.days(rData, from = start.date, to = one.week)
}

getDefaultStatus <- function(days){
    if(length(days) == 0) return(0)
    default <- unlist(lapply(days,function(s){
    	s$org.mozilla.appInfo.appinfo$isDefaultBrowser
    }))
	1*(sum(default) > 0.5*length(days))
    }


getProfInfo <- function(rData){
	locale <- get.locale(rData)
    geo <- get.val(rData, "geoCountry")
    os <- standardOSValue(rData$geckoAppInfo$os)
    distrib <- get.distribution.type(rData)
	distrib <- if(distrib %in% c("mozilla", "yahoo")) distrib else "other"
	list(distrib = distrib, locale = locale, geo = geo)
}

getProvidersList <- function(profinfo){
	## Include official Yahoo plugin names, and other-prefixed for all non-mozilla builds
	yahoo <- yahoo.searchnames(if(identical(profinfo$distrib, "other")) { 
		"yahoo" } else { profinfo$distrib })
	## For Google, look for "google", "google-*", or "other-Google *"
	google <- function(spvs) { 
        grepl("^(other-)?google([-[:blank:]][a-z]{2})?$", spvs, 
            ignore.case = TRUE)
    }
    bing <- c("bing", "other-Bing")
    other <- NA
    list(yahoo = yahoo, google = google, bing = bing, other = other)
}

getSearches <- function(interestTimeFrame){
	rawsearches <- allSearches(interestTimeFrame)
	# remove extreme search counts
	# not common, but precaution
	rawsearches <- lapply(rawsearches, function(s) {
    	toremove <- s$count > 1e6
        if(all(toremove)) return(NULL)
        if(any(toremove)) {
        	s$provider <- s$provider[!toremove]
            s$sap <- s$sap[!toremove]
            s$count <- s$count[!toremove]
        }
        s
    })
    newnulls <- unlist(lapply(rawsearches, is.null))
    if(any(newnulls)) {
        rawsearches <- if(all(newnulls)) NULL else rawsearches[!newnulls]
    }
    rawsearches
}


getSearchNameGrouping <- function(profinfo){
	grouping <- list(yahoo = yahoo.searchnames(profinfo$distrib),
					google = google.searchnames(profinfo$distrib), 
					bing = bing.searchnames(profinfo$distrib))
	officialsn <- official.searchnames(profinfo$distrib)
	grouping[["otherofficial"]] <- officialsn[!(officialsn %in% unlist(grouping, use.names = FALSE))]
    grouping[["other"]] <- NA
    grouping
}

getSearchesbyName <- function(profinfo){
	grouping <- getSearchNameGrouping(profinfo)
	totalSearchCounts(interestWeek, provider = grouping, sap = FALSE)
}

getSearchesbyKeyNames <- function(searchesbyname){
	allsearch <- getSearchesbyName(profinfo)
	totalsearch <- computeSearchCounts(allsearch)
	googlesearch <- computeSearchCounts(allsearch, "google")
	yahoosearch <- computeSearchCounts(allsearch, "yahoo")
	bingsearch <- computeSearchCounts(allsearch, "bing")
	list(totalsearch = totalsearch, googlesearch = googlesearch, yahoosearch = yahoosearch, bingsearch = bingsearch)
}


