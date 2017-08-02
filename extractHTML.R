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

InvWebsite<-paste0("http://www.seed-db.com/investorgraph/investorview?investorid=",InvID)

df<-cbind(InvName,InvID,InvWebsite)

Acc<-read.csv("AcceleratorIDs.csv", encoding = "Unicode", header = FALSE)

#Remove front tag
head(Acc)
Acc<-Acc$V1
AccID<-gsub("<option value=\"","", Acc)
head(AccID)
#Remove name and back tag
AccID<-gsub(">.*.>","", AccID)
head(AccID)
AccID<-gsub("[^0-9]", "", AccID)
head(AccID)
AccID<-grep("[0-9]", AccID, value = TRUE)
head(AccID)

AccName<-gsub("<.*?>","", Acc)
head(AccName)
AccWebsite<-paste0("http://www.seed-db.com/investorgraph/acceleratorview?acceleratorid=",AccID)
AccWebsite[16]
df2<-cbind(AccName,AccID,AccWebsite)


write.csv(df, "InvNameIDwebsite.csv")
write.csv(df2, "AccNameIDwebsite.csv")


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
  if(!file.exists(paste0("./inv/",InvID[i],".csv"))){
    print("File does not exist. Making a new file.")
    try(tbl<- readHTMLTable(InvWebsite[i], which=1, stringsAsFactors = FALSE))
    try(write.csv(tbl, paste0("./inv/",InvID[i],".csv")))
    if(file.exists(paste0("./inv/",InvID[i],".csv"))){
      print("Success!")
    }
    else {print("Failed to create csv")
    }
  }else{print("File already exists, skipping to the next file.")}
}


lapply(1:7032,get.html)



get.html2 <- function(i) {
  print(i)
  try(remove(tbl))
  if(!file.exists(paste0("./acc/",AccID[i],".csv"))){
    print("File does not exist. Making a new file.")
    try(tbl<- readHTMLTable(AccWebsite[i], which=1, stringsAsFactors = FALSE))
    try(write.csv(tbl, paste0("./acc/",AccID[i],".csv")))
    if(file.exists(paste0("./acc/",AccID[i],".csv"))){
      print("Success!")
    }
    else {print("Failed to create csv")
    }
  }else{print("File already exists, skipping to the next file.")}
}


lapply(1:196,get.html2)

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

ls<-list.files("./csv/")
ls<-gsub(".csv", "", ls)
difs <- setdiff(InvName2,ls)
uniq<-unique(InvName)
difs <- setdiff(ls, InvName2)

'%nin%' <- Negate('%in%')

difs<-InvName2[InvName2 %nin% ls]


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

#compare names

#rerun without changes to see if any files were missed
#confirm that missing csvs link to empty tables

#identify and count duplicates
df2<-data.frame(df2)
colnames(df2)<-colnames(df)

df3<-rbind(df, df2)

df3

NameCount <- data.frame(table(df3$InvName))
dups<-NameCount[NameCount$Freq > 1,]
dups<-dups[order(-dups$Freq),]
#initialize dataframe
df4<- data.frame(rep(NA,10), rep(NA,10))

# cr8df<-function(i){
#   df4[i,1]<-as.character(dups$Var1[i])
#   df4[i,2]<-as.vector(df3[which(df3$InvName==dups$Var1[i]),]$InvID)[1]
#   df4[i,3]<-as.vector(df3[which(df3$InvName==dups$Var1[i]),]$InvID)[2]
#   df4[i,4]<-as.vector(df3[which(df3$InvName==dups$Var1[i]),]$InvID)[3]
#   df4[i,5]<-as.vector(df3[which(df3$InvName==dups$Var1[i]),]$InvID)[4]
#   df4[i,6]<-as.vector(df3[which(df3$InvName==dups$Var1[i]),]$InvID)[5]
#   df4[i,7]<-as.vector(df3[which(df3$InvName==dups$Var1[i]),]$InvID)[6]
#   df4[i,8]<-as.vector(df3[which(df3$InvName==dups$Var1[i]),]$InvID)[7]  
#   print(i)
# }
# lapply(1:739,cr8df)
# names(df4)[1]<-"InvName"
# for(i in 1:7){
# names(df4)[1+i]<-paste0("InvID",i)
# }

#For some reason the apply method did not work all the way through
#I filled in the rest of df4 with a loop
for(i in 1:739){
  df4[i,1]<-as.character(dups$Var1[i])
  df4[i,2]<-as.vector(df3[which(df3$InvName==dups$Var1[i]),]$InvID)[1]
  df4[i,3]<-as.vector(df3[which(df3$InvName==dups$Var1[i]),]$InvID)[2]
  df4[i,4]<-as.vector(df3[which(df3$InvName==dups$Var1[i]),]$InvID)[3]
  df4[i,5]<-as.vector(df3[which(df3$InvName==dups$Var1[i]),]$InvID)[4]
  df4[i,6]<-as.vector(df3[which(df3$InvName==dups$Var1[i]),]$InvID)[5]
  df4[i,7]<-as.vector(df3[which(df3$InvName==dups$Var1[i]),]$InvID)[6]
  df4[i,8]<-as.vector(df3[which(df3$InvName==dups$Var1[i]),]$InvID)[7]  
  print(i)
}
#now I add csv to all ID values
for(i in 1:739){
  
  print(i)
  df4[i,-1][which(!is.na(df4[i,-1]))]<-paste0(df4[i,-1][which(!is.na(df4[i,-1]))],".csv")
  
}



add.csv<-function(i){
  print(i)
  df4[i,-1][which(!is.na(df4[i,-1]))]<-paste0(df4[i,-1][which(!is.na(df4[i,-1]))],".csv")
}
lapply(1:739,add.csv)
for(1:length(!is.na(df4[1,-1])),read.csv(df4[i,-1][i]
x<-df4[i,-1][which(!is.na(df4[i,-1]))]  
paste0(x)
setwd("./invacc")
LeCamping<-system2("cat",x)                                         
df4$InvName[739]                                                                             
                                         
x<-NA                                         
catCSV<-function(i){
  try(remove(x))
  x<-paste(df4$InvName[i])
    if(file.exists(paste0("./invacc/",df4[i,-1][1],".csv"))){
      print("x1")
      x1<-read.csv(paste0("./invacc/",df4[i,-1][1],".csv"))
    }
    if(file.exists(paste0("./invacc/",df4[i,-1][2],".csv"))){ 
      print("x2")
      x2<-read.csv(paste0("./invacc/",df4[i,-1][2],".csv"))
    }    
    if(file.exists(paste0("./invacc/",df4[i,-1][3],".csv"))){ 
      print("x3")
      x3<-read.csv(paste0("./invacc/",df4[i,-1][3],".csv"))
    } else{x3<-rbind(x1,x2)
      write.csv(x3, paste0("./dups/",x,".csv"))}   
    if(file.exists(paste0("./invacc/",df4[i,-1][4],".csv"))){ 
      print("x4")
      x4<-read.csv(paste0("./invacc/",df4[i,-1][4],".csv"))
    } else{x4<-rbind(x1,x2,x3)
    write.csv(x3, paste0("./dups/",x,".csv"))}  
    if(file.exists(paste0("./invacc/",df4[i,-1][5],".csv"))){ 
      print("x5")
      x5<-read.csv(paste0("./invacc/",df4[i,-1][5],".csv"))
    } else{x5<-rbind(x1,x2,x3,x4)
    write.csv(x5, paste0("./dups/",x,".csv"))}   
    if(file.exists(paste0("./invacc/",df4[i,-1][6],".csv"))){ 
      print("x6")
      x6<-read.csv(paste0("./invacc/",df4[i,-1][6],".csv"))
    } else{x6<-rbind(x1,x2,x3,x4,x5)
      write.csv(x6, paste0("./dups/",x,".csv"))}
    if(file.exists(paste0("./invacc/",df4[i,-1][7],".csv"))){ 
      print("x7")
      x7<-read.csv(paste0("./invacc/",df4[i,-1][7],".csv"))
    } else{x7<-rbind(x1,x2,x3,x4,x5,x6)
      write.csv(x7, paste0("./dups/",x,".csv"))}
  print("x7")
  x8<-rbind(x1,x2,x3,x4,x5,x6,x7)
  write.csv(x8, paste0("./dups/",x,".csv"))
}   

catCSV(2)
