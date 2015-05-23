library(rjson)
library(lubridate)
library(sendmailR)

# For MapReduce job
getSampleCreationDates <- function(file.path){
	all.samples <- rhls(file.path)$file
	do.call("c",(lapply(all.samples, function(r){
		sampledate  <- as.Date(tail(strsplit((r),"/")[[1]],1))
	})))
}

getStartAndEnd <- function(samples.created){
	start.month<- ceiling_date(min(samples.created), "month")
	end.month <- floor_date(max(samples.created), "month")
	month.interval <- as.numeric(round((end.month - start.month)/(365.21/12)))
	list(start.month = start.month, end.month = end.month, month.interval =month.interval)
}

createMonthList <- function(start.end){
	start.dates <- seq(start.end$start.month, by = "month", length.out = start.end$month.interval)
	end.dates <-  (start.dates + months(1))-1
	lapply(seq_along(start.dates), function(x){
		list(start.date = start.dates[[x]], end.date = end.dates[[x]])
	})
}

getInterestDaterangeInfo <- function(month, allDaysInfo){
	start.date <- strftime(month$start.date)
	end.date <- strftime(month$end.date)
	allDaysInfo[names(allDaysInfo) >= as.Date(start.date) & names(allDaysInfo) <= as.Date(end.date)]
}

getTotalSearch <- function(days){
	tsearch <- totalSearchCounts(days, provider = FALSE, sap=FALSE)
	if(is.null(tsearch)) {
		0 
	} else {
		tsearch
	}
}

getMAUInfo <- function(month, rData){
	start.date <- strftime(month$start.date)
	end.date <- strftime(month$end.date)
	if(count.active.days(rData, start.date, end.date) > 0) {
		1
	} else {
		0
	} 
}

checkForFilesNotRun <- function(sampleCreation, fileCreation){
	if(length(sampleCreation) != length(fileCreation)){
	sampleCreation[!(sampleCreation %in% fileCreation)]
} else{
	return()
}
}

meetsRunConditions <- function(samples.created){
	start.month<- ceiling_date(min(samples.created), "month")
	lapply(samples.created, function(sampledate){
		sampledate < start.month
		})
}

sampleCreatedAfterMonthEnded <- function(getinfo, sample){
	sampledate  <- as.Date(tail(strsplit(sample,"/")[[1]],1))
	startmonth <- getinfo$start.month
	sampledate > startmonth
}

## For creating tables
mergeAllFiles <- function(load.all){
	lapply(load.all, function(onefile){
		file <- onefile
		if(length(rhread(file)) != 0) 
			make.dt(rhread(file), c("month","sampledate", "daysSince", "tmau", "tsec", "tsearch"))
			})}

createDataTable <- function(mergeAllFiles){
	df <- do.call(rbind, mergeAllFiles)
	save(df, file = "~/output/fhr-lag.RData")
	data.table(df)
}

forceToOne <- function(xdat){
	ifelse(xdat > 1, as.integer(1), xdat)
}

intTable <- function(model){
	mynewdata <- data.frame(daysSince = seq(1:150))
	pi.hat <- predict.glm(model, mynewdata, type = "response", se.fit = TRUE)
	l.hat <- predict.glm(model, mynewdata, se.fit = TRUE)
	lb <- l.hat$fit - 1.96*l.hat$se.fit
	ub <- l.hat$fit + 1.96*l.hat$se.fit
	ci <- data.frame(lb, ub)
	ci95 <- exp(ci)/(1+exp(ci))
	data.frame(pi.hat$fit, ci95)
}	

email <- function(subj="blank subject", body="blank body",to="<cchoi@mozilla.com>"){
	tryCatch({
		bodyWithAttachment <- list(body)
		sendmail(from="<cchoi@mozilla.com>",
			to=to,
			subject=subj,msg=bodyWithAttachment,
			control=list(smtpServer='smtp.mozilla.org'))
		list(TRUE,NA)
       }, error=function(e) list(FALSE,e))
    } 

getPredictionRange <- function(sampleCreationDates){
	latestDate <- max(sampleCreationDates) - 7
	earliestDate <- latestDate - 150
	latestMonth <- floor_date(max(latestDate), "month")
	earliestMonth <- ceiling_date(max(earliestDate), "month")
	list(start = earliestMonth, end =latestMonth)
}


