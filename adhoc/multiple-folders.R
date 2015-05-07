Sys.setenv(USER="cchoi")
source("/usr/local/share/rhipe.mozilla.setup.R")
library(rjson)
library(data.table)

##################################################################################################
##################################################################################################

# For each day in December 2014, go through all samples created after 2014-12-01.
# Think of each day as a patient (obs.key is the patient ID)
# Each patient should have ~ 8 observations (weekly samples)
# Challenge: To loop through the sampled data. Use param = in rhwatch
# Daily active user prediction

# create list with day and date
date <- seq(as.Date("2014-12-01"), by= "day", length.out = 30)
day <- seq(1:30)
bi <- data.frame(day, date)
all.inc <- split(bi, day)
mylist <- lapply(all.inc, function(x) list(day = x[[1]], date = x[[2]]))

m = function(k,r) {
	r <-  fromJSON(r)
	if(r$geckoAppInfo$name != "Firefox") return()
	if(r$gecko$vendor != "Mozilla") return()
	d <- r$data$days
	for (m in mylist){
		obs.day <- m$date
		obs.key <- m$day
		day <- d[names(d) == obs.day]
		active.prof <- ifelse(length(day) == 1, 1, 0)
		tot.search <- sum(unlist(lapply(day, function(d) {
			sv <- d$org.mozilla.searches.counts
			if(is.null(sv)) return()
			sv$"_v" <- NULL
			sum(unlist(sv))	
		})))
		tot.sec <- sum(unlist(lapply(day, function(r) {
			tt <- c(r$org.mozilla.appSessions.previous$cleanTotalTime, r$org.mozilla.appSessions.previous$abortedTotalTime)
			tt[tt>0]
		})))	
		tot.ticks <- sum(unlist(lapply(day, function(r) {
		tt <- c(r$org.mozilla.appSessions.previous$cleanActiveTicks, r$org.mozilla.appSessions.previous$abortedActiveTicks)
		tt[tt>0]
		})))
		if (sampledate < obs.day){
			active.prof = 0
			tot.search = 0
			tot.sec = 0
			tot.ticks = 0
		}
		rhcollect(list(obs.day = obs.day, obs.key = obs.key, sampledate = sampledate), c(active.prof = active.prof, tot.search = tot.search, tot.sec = tot.sec, tot.ticks = tot.ticks))
	}
}

# loop through all the backup folders
# test with: rhls("/user/sguha/fhr/samples/backup")$file[19:20]

all.samples <- rhls("/user/sguha/fhr/samples/backup")$file
for (p in all.samples)
{
	sampledate  = as.Date(tail(strsplit(p,"/")[[1]],1))
	if (sampledate < as.Date("2014-12-01")) next
	z = rhwatch(map=m
		,input=sqtxt(p)
		,param = list(sampledate = sampledate)
		#sprintf extremely helpful to store results in separate folders, name ending with sampledate
		,output = sprintf("/user/cchoi/output/fhr-lag/%s", sampledate)
		,reduce = rhoptions()$template$colsummer
		,setup = expression(map = {library(rjson)})
		,debug = "count"
		,read = FALSE
	)
}

s = rhread(z)


##################################################################################################
##################################################################################################


all.results <- rhls("/user/cchoi/output/fhr-lag")$file
s = lapply(all.results, function(x){
	sample.date = tail(strsplit(x,"/")[[1]],1)
	if(sample.date < as.Date("2014-12-01")) return()
	else {
	folder = rhread(sprintf("/user/cchoi/output/fhr-lag/%s", sample.date))
	make.dt(folder, c("obs.day", "obs.key", "sampledate", "active.prof", "tot.search", "tot.sec", "tot.ticks"))
	}
})

all.results <- rhls("/user/cchoi/output/fhr-lag")$file
s = lapply(all.results, function(x){
	dat <- rhread(x)
	make.dt(dat, c("obs.day", "obs.key", "sampledate", "active.prof", "tot.search", "tot.sec", "tot.ticks"))
})

df <- do.call(rbind, s)
df <- data.frame(df)
df[ ,c(1,3)] <- lapply(df[, c(1,3)], function(x){
	t1 <- as.numeric(x)
	as.Date(t1, origin = "1970-01-01")
})

save(df, file = "/home/cchoi/output/fhr-lag.RData")

##################################################################################################
##################################################################################################
# Monthly active user

s.date <- seq(as.Date("2014-08-01"), by= "month", length.out = 7)
e.date <- seq(as.Date("2014-09-01"), by= "month", length.out = 7)-1
month <- c(s.date)
bi <- data.frame(month, s.date, e.date)
all.inc <- split(bi, month)
mylist <- lapply(all.inc, function(x) list(month = x[[1]], start.date = x[[2]], end.date = x[[3]]))

m = function(k,r) {
	r <-  fromJSON(r)
	if(r$geckoAppInfo$name != "Firefox") return()
	if(r$gecko$vendor != "Mozilla") return()
	d <- r$data$days
	for (m in mylist){
		month <- m$month
		start.date <- m$start.date
		end.date <- m$end.date
		firstday <- m$end.date+1
		days <- days <- d[names(d) >= as.Date(start.date) & names(d) <= as.Date(end.date)]
		active <- if(length(days)>0) 1 else 0
		if (sampledate < end.date){
			active = 0
		}
		rhcollect(list(month = month, firstday = firstday, sampledate = sampledate), c(active = active, profile = 1))
	}
}

all.samples <- rhls("/user/sguha/fhr/samples/backup")$file
for (p in all.samples)
{
	sampledate  = as.Date(tail(strsplit(p,"/")[[1]],1))
	if (sampledate < as.Date("2014-09-01")) next
	z = rhwatch(map=m
		,mapred=list(mapred.job.priority = "VERY_HIGH")
		,jobname = "stop_failing"
		,input=sqtxt(p)
		,param = list(sampledate = sampledate)
		#sprintf extremely helpful to store results in separate folders, name ending with sampledate
		,output = sprintf("/user/cchoi/output/fhr-lag/%s", sampledate)
		,reduce = rhoptions()$template$colsummer
		,setup = expression(map = {library(rjson)})
		,debug = "count"
		,read = FALSE
	)
}

all.results <- rhls("/user/cchoi/output/fhr-lag")$file
s = lapply(all.results, function(x){
	dat <- rhread(x)
	make.dt(dat, c("month", "firstday","sampledate", "active", "profile"))
})

df <- do.call(rbind, s)
df <- data.frame(df)
df$month <- as.Date(as.numeric(df$month), origin = "1970-01-01")
df$sampledate <- as.Date(as.numeric(df$sampledate), origin = "1970-01-01")
df$firstday <- as.Date(as.numeric(df$firstday), origin = "1970-01-01")

save(df, file = "/home/cchoi/output/fhr-monthly-predict.RData")




