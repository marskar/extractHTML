library(XML)

setwd("/Users/marskar/GitHub/extractHTML")
getwd()

#this works 
#tbl<- readHTMLTable("http://www.seed-db.com/investorgraph/investorview?investorid=4903702712287232", which=1)
#write.csv(tbl, "406 Ventures.csv")
inv<-read.csv("Accelerator Framework - SeedDB-Investors.csv")

#try a for loop
# for(i in 1:7124) {
#   investor<-as.character(inv[i,1])
#   investorid<-inv[i,2]
#   url<-as.character(inv[i,3])
#   try(tbl<- readHTMLTable(url, which=1))
#   try(write.csv(tbl, paste0(investor,".csv")))
#   remove(tbl)
# }

nchar("http://www.seed-db.com/investorgraph/investorview?investorid=5778225418469376")

inv[,3]<-strtrim(inv[,3],77)

#make a function then lapply
get.html <- function(i) {
  print(i)
  investor<-as.character(inv[i,1])
  investorid<-inv[i,2]
  url<-as.character(inv[i,3])
  try(tbl<- readHTMLTable(url, which=1, stringsAsFactors = FALSE))
  try(write.csv(tbl, paste0(investor,".csv")))
}

lapply(1:7124,get.html)

#tbl<- readHTMLTable("http://www.seed-db.com/investorgraph/investorview?investorid=4903702712287232", which=1)


#Command+Shift+c to comment out multiple selected lines
# install.packages("RCurl")
# library(RCurl)
# get.html <- function(i) {
#   print(i)
#   investor<-as.character(inv[i,1])
#   investorid<-inv[i,2]
#   url<-as.character(inv[i,3])
#   urldata <- getURL(url)
#   tbl<- readHTMLTable(urldata, stringsAsFactors = FALSE)
#   write.csv(tbl, paste0(investor,".csv"))
# }


