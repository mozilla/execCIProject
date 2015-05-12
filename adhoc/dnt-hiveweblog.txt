#find list of addons on FHR dictionary

library(data.table)
Sys.setenv(USER="cchoi")
source("/usr/local/share/rhipe.mozilla.setup.R")
load("/usr/local/share/addon-dict.RData")

adblock <- addons[grep("Adblock Plus", addons$name, ignore.case = TRUE)]
ghost <- addons[grep("Ghostery", addons$name, ignore.case = TRUE)]
noscript <- addons[grep("NoScript", addons$name, ignore.case = TRUE)]
disconnect <- addons[grep("Disconnect", addons$name, ignore.case = TRUE)]
donot <- addons[grep("DoNotTrackMe", addons$name, ignore.case = TRUE)]
ublock <- addons[grep("uBlock", addons$name, ignore.case = TRUE)]

unique(privacy$id)


# hive -f /home/cchoi/code/dnt > dnt-data.txt
# ssh peach-gw.peach.metrics.scl3.mozilla.com
# domain=versioncheck.addons.mozilla.org

show tables;
show partitions v2_raw_logs;
describe v2_raw_logs;
describe v2_raw_logs partition(ds = '2015-02-01', domain = 'addons.ddons.mozilla.org');

SELECT * from v2_raw_logs where domain= 'versioncheck.addons.mozilla.org' and ds='2015-02-02' limit 1;
EXPLAIN SELECT * from v2_raw_logs where domain= 'versioncheck.addons.mozilla.org' and ds='2015-02-02' limit 3;

hive -e "SELECT custom_field_1, count(*) FROM v2_anonymous_logs where ( domain='versioncheck.addons.mozilla.org') AND ds='2015-02-03' AND custom_field_1 like 'DNT%' GROUP BY custom_field_1;" > dnt-check.txt
hive -f /home/cchoi/code/adblock > adblock.txt
hive -f /home/cchoi/code/disconnect > disconnect.txt
hive -f /home/cchoi/code/donottrack > donottrack.txt
hive -f /home/cchoi/code/ghosetery > ghostery.txt
hive -f /home/cchoi/code/noscript > noscript.txt
hive -f /home/cchoi/code/privacy > privacy.txt
hive -f /home/cchoi/code/ublock > ublock.txt


SELECT custom_field_1,
	COUNT (*) 
FROM v2_anonymous_logs 
WHERE request_url LIKE '%{2b10c1c8-a11f-4bad-fe9c-1c11e82cac42}%'
AND domain = 'versioncheck.addons.mozilla.org'
AND ds ='2015-02-03'
AND custom_field_1 LIKE 'DNT%' 
GROUP BY custom_field_1;

		request_url LIKE '%{d10d0bf8-f5b5-c8b4-a8b2-2b9879e08c5d}%' 
	AND request_url LIKE '%disconnect.me%'
	AND request_url LIKE '%donottrackplus@abine.com%'
	AND request_url LIKE '%firefox@ghostery.com%'
	AND request_url LIKE '%{73a6fe31-595d-460b-a920-fcc0f8843232}%'
	AND request_url LIKE '%jid1-MnnxcxisBPnSXQ@jetpack%'
	AND request_url LIKE '%{2b10c1c8-a11f-4bad-fe9c-1c11e82cac42}%'