library(XML)

setwd("/Users/marskar/GitHub/extractHTML")
getwd()

#TODO: How to deal with duplicates? How to fix broken links?

#Step1: Copy HTML from source in Chrome again - *DONE* saved as InvestorIDs.csv
#Step2: Create links and make sure links work for all rows

inv<-read.csv("InvestorIDs.csv", encoding = "Unicode", header = FALSE)
inv<-gsub("\x8d","c", inv$V1)
inv[87]
inv<-as.data.frame(inv)
#Remove front tag
head(inv)
InvID<-gsub("<option value=\"","", inv$inv)
head(InvID)
#Remove name and back tag
InvID<-gsub(">.*.>","", InvID)
head(InvID)
InvID<-gsub("[^0-9]", "", InvID)
head(InvID)
InvID<-grep("[0-9]", InvID, value = TRUE)
head(InvID)

InvName<-gsub("<.*?>","", inv$inv)
InvName<-gsub(".xfc.xbe.x99.x83.xa0.xb","c", InvName)
InvName[87]
head(InvName)
InvID<-as.data.frame(InvID)
website<-paste0("http://www.seed-db.com/investorgraph/investorview?investorid=",InvID$InvID)

df<-cbind(InvName,InvID,website)
write.csv(df, "SeedDB-InvestorNameIDwebsite.csv")


#this works 
#tbl<- readHTMLTable("http://www.seed-db.com/investorgraph/investorview?investorid=4903702712287232", which=1)
#write.csv(tbl, "406 Ventures.csv")

#try a for loop
# for(i in 1:7124) {
#   investor<-as.character(inv[i,1])
#   investorid<-inv[i,2]
#   url<-as.character(inv[i,3])
#   try(tbl<- readHTMLTable(url, which=1))
#   try(write.csv(tbl, paste0(investor,".csv")))
#   remove(tbl)
# }


#make a function then lapply
library(XML)
get.html <- function(i) {
  print(i)
  try(remove(tbl))
  try(tbl<- readHTMLTable(website[i], which=1, stringsAsFactors = FALSE))
  try(write.csv(tbl, paste0("./csv/",InvName[i],".csv")))
}

lapply(1:7032,get.html)

#tbl<- readHTMLTable("http://www.seed-db.com/investorgraph/investorview?investorid=4903702712287232", which=1)

setwd("~/GitHub/extractHTML/")
ls<-list.files("./csv/")
# dat<-read.csv(ls[10], header = TRUE)
# sum(as.numeric(gsub(",", "",dat[,4])))
# mean(as.numeric(gsub(",", "",dat[,4])))
# median(as.numeric(gsub(",", "",dat[,4])))
# nrow(dat)
# dat[,4]
# dat$Funding.roundsize....
#initialize dataframe
df<- data.frame(rep(NA,10), rep(NA,10), rep(NA,10), rep(NA,10), rep(NA,10))
names(df)<-c("Name","TotalInvestment", "NumberOfInvestments", "Mean", "Median")

# df[1,1]<-gsub(".csv","",ls[1])
# df[1,2]<-sum(as.numeric(gsub(",", "",dat[,4])))
# df[1,3]<-nrow(dat)
# df[1,4]<-mean(as.numeric(gsub(",", "",dat[,4])))
# df[1,5]<-median(as.numeric(gsub(",", "",dat[,4])))
# 
# ls<-list.files(pattern = "csv")
# 
# dat<-read.csv(ls[100])

#This for loop works 
for(i in 1:length(ls)){
  try(remove(dat))
  try(dat<-read.csv(paste0("./csv/",ls[i])))
  print(ls[i])
  df[i,1]<-gsub(".csv","",ls[i])
  if(exists("dat")){
  df[i,2]<-sum(as.numeric(gsub(",", "",dat[,4])))
  df[i,3]<-nrow(dat)
  df[i,4]<-mean(as.numeric(gsub(",", "",dat[,4])))
  df[i,5]<-median(as.numeric(gsub(",", "",dat[,4])))
  }
}

max(df$TotalInvestment, na.rm=TRUE)
#copy df, just in case
df2<-df
colnames(df2)[1]<-"Investor"
df3<-merge(df2,websites,by="Investor")
df3<-df3[,-6]

write.csv(df3,"../SeedDBinvestors.csv")
#pull.stats <- function(i)

#lapply(1:10,pull.stats)


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


# library(XML)
# url<-"https://www.linkedin.com/search/results/people/?facetPastCompany=%5B%221068%22%2C%221382%22%5D&keywords=Goldman%20Sachs&origin=FACETED_SEARCH&page=3"
# tbl<- readHTMLTable(url, which=1, stringsAsFactors = FALSE)
# download.file(url, destfile = "data.htm", method="auto")
# list.files()
# getwd()
