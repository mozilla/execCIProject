# Search provider distribution on Fennec

m = function(k,r) {
	r <-  fromJSON(r)
	if(r$environments$current$geckoAppInfo$name != "fennec") return()
	d <- r$data$days
	start.date <- as.Date("2015-01-01")
	end.date <- as.Date("2015-01-31")
	sessions <- d[names(d) >= as.Date(start.date) & names(d) <= as.Date(end.date)]
		# have to amke sure that appSession is recorded, to varify that it was active
		searches <- lapply(sessions, function(e){
			lapply(e, function(f){
				s = f$org.mozilla.searches.counts
				s$"_v" <- NULL
				lapply(s, function(sap.list){
					lapply(names(sap.list), function(provider){
						print(rhcollect(list(provider = provider), c(count = sap.list[[provider]])))
						})
					})
				})
			})
		}

z = rhwatch(map=m
	,input=sqtxt("/user/bcolloran/deorphaned/2015-03-23/v3")
	,reduce = rhoptions()$template$colsummer
	,setup = expression(map = {library(rjson)})
	,debug = "count"
	,read = FALSE
	)

s = rhread(z)
df <- make.dt(s, c("provider", "count"))