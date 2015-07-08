
# Before launching the job, check on one payload
# FHR V2 payload# 
Sys.setenv(USER="cchoi")
source("/usr/local/share/rhipe.mozilla.setup.R")
library(rjson)
rhcollect = function(a,b) list(a,b)
jsonData = rhread("/user/sguha/fhr/samples/output/1pct", textual=TRUE,max=50) 
ss = jsonData[[5]][[2]]
jsonData = ss

# FHR V3 Check (Fennec)
r = rhread("/user/bcolloran/deorphaned/2015-03-23/v3", textual=TRUE,max=10) 
ss = r[[10]][[2]]
r=ss
r <-fromJSON(r)
if(r$environments$current$geckoAppInfo$name != "fennec") return()
d <- r$data$days
names(d)

jsonData <- fhr.load.some(1)
mapply(m, jsonData) #de-json
k <- capture.output(mapply(m, jsonData, 1))
