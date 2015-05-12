library(rjson)
library(lubridate)


# monthly CI
# read the backups ao see all the dates that samples were created
all.samples <- rhls("/user/sguha/fhr/samples/backup")$file
samples.created <- do.call("c",(lapply(all.samples, function(r){
	sampledate  = as.Date(tail(strsplit((r),"/")[[1]],1))
	})))
# find the months (startdate and enddate) of the month interval based on sample creation dates
start.month<- ceiling_date(min(samples.created), "month")
end.month <- floor_date(max(samples.created), "month")
month.interval <- as.numeric(round((end.month - start.month)/(365.21/12)))
# create a list with all the relevant start, end, month to look at
start.dates <- seq(start.month, by = "month", length.out = month.interval)
end.dates <-  (start.dates + months(1))-1
my.list <- lapply(seq_along(start.dates), function(x){
	list(start.date = start.dates[[x]], end.date = end.dates[[x]])
	})

m = function(k,r){
	r <- fromJSON(r)
	d <- r$data$days
	if(!is.standard.profile(r)) return()
	# Look through each month
	for (m in my.list){
		# number of days since the month is over
		daysSince <- as.numeric(sampledate - (m$end.date+1))
		# if it has been less than 0 days, next
		if(daysSince < 0 || daysSince > 150) next
		month <- as.numeric(format(m$start.date, "%m"))
		start.date <- strftime(m$start.date)
		end.date <- strftime(m$end.date)
		days <- d[names(d) >= as.Date(start.date) & names(d) <= as.Date(end.date)]
		tsec <- totalActivity(days)$totalsec
		tsearch <- totalSearchCounts(days, provider = FALSE, sap=FALSE)
		tsearch <- if(is.null(tsearch)) 0 else tsearch
		# if sample was created before the month was over, the sample should not be calculating the tmau
		tmau <- if(count.active.days(r, start.date, end.date) > 0) 1 else 0
		rhcollect(list(month = month, sampledate = as.character(sampledate), daysSince = daysSince), c(tmau = tmau, tsec = tsec, tsearch = tsearch))
	}
}

all.samples <- rhls("/user/sguha/fhr/samples/backup")$file
for (p in all.samples)
{
	sampledate  = as.Date(tail(strsplit(p,"/")[[1]],1))
	if (sampledate < start.month) next
	z = rhwatch(map=m
		#,mapred=list(mapred.job.priority = "VERY_HIGH")
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
qdf <- lapply(all.results, function(x){
	file <- x
	if(length(rhread(file)) != 0) 
	make.dt(rhread(file), c("month","sampledate", "daysSince", "tmau", "tsec", "tsearch"))
	})
df <- do.call(rbind, qdf)
save(df, "~/Data/fhr-lag.RData")

t1 <- data.table(df)
t1 <- t1[order(sampledate, month),]
t2 <- t1[daysSince >= 120,]
true.value <- t2[, list(true.mau = mean(tmau), true.search = mean(tsearch), true.time = mean(tsec)), by = month]
t3 <- merge(t1, true.value, by = 'month')
t3 <- t3[, ":="(pmau = tmau/true.mau, psrch = tsearch/true.search, psec = tsec/true.time)]
force.to.1 <- function(xdat){
	ifelse(xdat > 1, as.integer(1), xdat)
}
cols = c("pmau", "psrch", "psec")
t3[, (cols) := lapply(.SD, force.to.1), .SDcols = cols]
print(t3)

model1 <- glm(pmau ~ daysSince, data= t3, quasibinomial) #mau
model2 <- glm(psrch ~ daysSince, data= t3, quasibinomial) #search
model3 <- glm(psec ~ daysSince, data= t3, quasibinomial) #sec

int.table <- function(model){
	mynewdata <- data.frame(daysSince = seq(1:150))
	pi.hat <- predict.glm(model, mynewdata, type = "response", se.fit = TRUE)
	l.hat <- predict.glm(model, mynewdata, se.fit = TRUE)
	lb <- l.hat$fit - 1.96*l.hat$se.fit
	ub <- l.hat$fit + 1.96*l.hat$se.fit
	ci <- data.frame(lb, ub)
	ci95 <- exp(ci)/(1+exp(ci))
	data.frame(pi.hat$fit, ci95)
}

pmau.int <- int.table(model1)
psrch.int <- int.table(model2)
psec.int <- int.table(model3)


		

