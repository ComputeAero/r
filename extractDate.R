# detectDate function is used to extract date from a string
# input:
#   line: a string containing date information
#   format: order of date, one of "ymd" (default), "mdy", "dmy"
# output:
#   a list with 3 names: year, month, day

# The following formats are supported (from 1800 to 2099):
# 1. yyyymmdd
# 2. yyyy-mm-dd
# 3. yyyy/mm/dd
# 4. yyyy.mm.dd
# 5. yymmdd
# 6. yy-mm-dd
# 7. yy/mm/dd
# 8. yy.mm.dd

library(stringr)
detectDate <- function(line, format="ymd"){
    regYear = "([18|19|20]?\\d+)"
    regMonth = "(0[1-9]|1[012])"
    regDay = "(0[1-9]|[12][0-9]|3[01])"
    conn = "[- /.]?"
    regexpression = ""
    if (format == "ymd"){
        regexpression = paste(regYear, conn, regMonth, conn, regDay, sep="")
    }else if(format == "mdy"){
        regexpression = paste(regMonth, conn, regDay, conn, regYear, sep="")
    }else if (format == "dmy"){
        regexpression = paste(regDay, conn, regMonth, conn, regYear, sep="")
    }else{
        stop("Wrong format specified in detectDate()")
    }
    ret = str_match(line, regex(regexpression))
    retDateObj = list(year="", month="", day="")
    if (format == "ymd"){
        retDateObj$year = ret[2]
        retDateObj$month = ret[3]
        retDateObj$day = ret[4]
    }else if(format=="mdy"){
        retDateObj$year = ret[4]
        retDateObj$month = ret[2]
        retDateObj$day = ret[3]
    }else if(format=="dmy"){
        retDateObj$year = ret[4]
        retDateObj$month = ret[3]
        retDateObj$day = ret[2]
    }
    retDateObj
}

######## Some examples ##########
print("Examples for date foramts in Year-Month-Day order")
ymd_examples <- c("blabla 20211107 blabla",
               "blabla 2021-11-07 blabla", 
               "blabla 2021/11/07 blabla", 
               "blabla 2021.11.07 blabla", 
               "blabla 211107 blabla", 
               "blabla 21-11-07 blabla", 
               "blabla 21/11/07 blabla",
               "blabla 21.11.07 blabla"
              )
for (line in ymd_examples){
    ret = detectDate(line, format="ymd")
    print(c(ret$year, ret$month, ret$day))
}

print("Examples for date foramts in Month-Day-Year order")
mdy_examples <- c("blabla 11072021 blabla",
                  "blabla 11-07-2021 blabla", 
                  "blabla 11/07/2021 blabla", 
                  "blabla 11.07.2021 blabla", 
                  "blabla 110721 blabla", 
                  "blabla 11-07-21 blabla", 
                  "blabla 11/07/21 blabla",
                  "blabla 11.07.21 blabla"
)
for (line in mdy_examples){
    ret = detectDate(line, format="mdy")
    print(c(ret$year, ret$month, ret$day))
}

print("Examples for date foramts in Day-Month-Year order")
dmy_examples <- c("blabla 07112021 blabla",
                  "blabla 07-11-2021 blabla", 
                  "blabla 07/11/2021 blabla", 
                  "blabla 07.11.2021 blabla", 
                  "blabla 071121 blabla", 
                  "blabla 07-11-21 blabla", 
                  "blabla 07/11/21 blabla",
                  "blabla 07.11.21 blabla"
)
for (line in dmy_examples){
    ret = detectDate(line, format="dmy")
    print(c(ret$year, ret$month, ret$day))
}

