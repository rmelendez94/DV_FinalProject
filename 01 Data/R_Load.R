require("jsonlite")
require("RCurl")

#Gather data from database fill into local data frame and retrieve headers and summary 
# Change the USER and PASS below to be your UTEid
df <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from BNKMKTG"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_rm46926', PASS='orcl_rm46926', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

head(df)
summary(df)


