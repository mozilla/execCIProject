#!/usr/local/bin/Rscript
setwd("~/fhr/ciproject")
tryCatch({
    source("funct.R")
    }, error=function(e) {
    	email("CIProject: FAIL, funct.R",body=as.character(e))
    	stop(e)
 })


tryCatch({
    source("mapreduce.R")
    }, error=function(e) {
    	email("CIProject: FAIL, mapreduce.R",body=as.character(e))
    	stop(e)
 })


tryCatch({
    source("clean.R")
    }, error=function(e) {
    	email("CIProject: FAIL, clean.R",body=as.character(e))
    	stop(e)
 })

email("CIProject: Success", body = sprintf("The last sample used: %s", as.character(max(sampleCreationDates))))

