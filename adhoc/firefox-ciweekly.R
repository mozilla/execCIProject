Sys.setenv(USER="cchoi")
source("/usr/local/share/rhipe.mozilla.setup.R")
source("~/code/search-reporting/yahoo-retention/init.R")
library(rjson)
library(lubridate)


# monthly CI
# read the backups ao see all the dates that samples were created
all.samples <- rhls("/user/sguha/fhr/samples/backup")$file
samples.created <- do.call("c",(lapply(all.samples, function(r){
	sampledate  = as.Date(tail(strsplit((r),"/")[[1]],1))
	})))
# find the months (startdate and enddate) of the month interval based on sample creation dates
start.month<- ceiling_date(min(samples.created), "week")
end.month <- floor_date(max(samples.created), "month")
month.interval <- as.numeric(round((end.month - start.month)/(365.21/12)))
# create a list with all the relevant start, end, month to look at
start.dates <- seq(start.month, by = "month", length.out = month.interval)
end.dates <-  (start.dates + months(1))-1
my.list <- lapply(seq_along(start.dates), function(x){
	list(start.date = start.dates[[x]], end.date = end.dates[[x]])
	}