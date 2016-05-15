#########################################
## ETL Australian data for PISA 2012
#########################################

library(data.table)


identifiers <- c("STRATUM", "SCHOOLID", "StIDStd",
                 "ST03Q01", "ST03Q02" # birth month, birth year
                 )

demographics <- c("ST04Q01", # gender
                  "ESCS", 
                  "")


stu <- fread("Data/Raw/AUS_INT_STU12_DEC03.csv")



#########################################
## Rename state
#########################################


states <- c("ACT", "VIC", "NSW", "QLD", "SA", "WA", "TAS", "NT")
stu$nameSTATE <- "X"

for(i in 1:8){
  stu[ausSTATE == i, nameSTATE := states[i]]
}

stu[, nameSTATE := factor(nameSTATE, levels = states)]


#########################################
## Rename geolocation
#########################################

geolocs <- c("Urban", "Provincial", "Rural")
stu$nameGEOLOC <- "X"

for(i in 1:3){
  stu[ausGEOLOC_3 == i, nameGEOLOC := geolocs[i]]
}

stu[, nameGEOLOC := factor(nameGEOLOC, levels = geolocs)]


#########################################
## Rename gender
#########################################

mfs <- c("Female", "Male")
stu$nameGENDER <- "X"

for(i in 1:2){
  stu[ST04Q01 == i, nameGENDER := mfs[i]]
}


#########################################
## Rename indig
#########################################

indig <- c("Indigenous", "Non-indigenous")
stu$nameINDIG <- "Indigenous"
stu[ausINDIG == 0, nameINDIG := "Non-indigenous"]


#########################################
## Rename ESCS
#########################################

stu[, qESCS := ESCS]
stu[is.na(qESCS), qESCS := mean(stu$ESCS, na.rm = T)]
stu[, "nameESCS" := "Top quartile"]
stu[qESCS <= quantile(qESCS)[4], nameESCS := "Third quartile"]
stu[qESCS <= quantile(qESCS)[3], nameESCS := "Second quartile"]
stu[qESCS <= quantile(qESCS)[2], nameESCS := "Bottom quartile"]
stu[, qESCS := NULL]
stu[, nameESCS := factor(nameESCS, levels = c("Top quartile", "Third quartile", "Second quartile", "Bottom quartile"))]

