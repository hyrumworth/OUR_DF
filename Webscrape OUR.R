library(rvest)
library(dplyr)
library(tidyr)
library(stringr)
library(beepr)
#### getting all the individual web pages of the johns####
# reading in the url 
##getting this to work before I set up the function
list_o_johns <- c()
## this allows us to move from page to page.  
for (i in 1:20){
url.1 <-"INSERT FIRST HALF OF WEBSITE"
url.2 <-"INSERT SECOND HALF OF WEBSITE"
page <- i
page <- as.character(page)
url <- paste(url.1,page,url.2,sep = "")

#This exracts the url contents 
yourl <- read_html(url)
alpha <- yourl %>%
  html_text()
yourl  

#this extracted all of the links that I needed to get all the charges on the johns rather than just the first 4. 
gama <- yourl%>%
  html_nodes("div.title")
gama.1 <- as.character(gama)
gama.2 <- str_extract_all(gama.1,"Arrests\\/.*\\?")
gama.2 <- gsub("\\?","",gama.2)
gama.2 <- gama.2[1:14]
list_o_johns <- append(list_o_johns,gama.2)
}
# A SUCCESS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
list_o_johns
list_o_johns <- as.data.frame(list_o_johns)
list_o_johns <- drop_na(list_o_johns)
list_o_johns <- list_o_johns$list_o_johns

#### now for the crime extraction####
master_df <- data.frame(name.2=NA,date.1=NA, gender.2=NA, height.2=NA,wieght.2=NA,prosecution=NA, state = NA, county = NA)
master_df

#Sys.sleep(runif(1,4,8))
for (i in 1:length(list_o_johns)){
subsite <- "https://arizona.arrests.org/"
subsite.1 <- list_o_johns[i]
subsite.1.5 <- paste(subsite,subsite.1, sep = "")
subsite.2 <- read_html(subsite.1.5)
#name
#Sys.sleep(runif(1,4,8))
state_county <- subsite.2%>%
  html_node("h3")%>%
  html_text()
state_county <- gsub("\\t","",state_county)
state_county <- gsub("\\r", "", state_county)
state_county <- gsub("\\n"," ", state_county)
state_county
county <- gsub('[[:space:]][[:alpha:]]+[[:space:]]+', ' ',state_county) 
county
state <- gsub('[[:space:]][[:space:]][[:alpha:]]+[[:space:]][[:alpha:]]+',"", state_county)
state
name.1 <- subsite.2%>%
  html_nodes("h2")%>% # or you could try div node and name.2[24]
  html_text()
name.2 <- name.1[3]
#date
#Sys.sleep(runif(1,4,8))
date.1 <- subsite.2%>%
  html_node("time")%>% # or div node name.2[25]
  html_text()
#Gender
#Sys.sleep(runif(1,4,8))
gender.1 <- subsite.2%>%
  html_nodes("span")%>% # or div node name.2[28] sometimes..... 
  html_text()
gender.2 <- gender.1[5]
#height
#Sys.sleep(runif(1,4,8))
hight.1 <- subsite.2%>%
  html_nodes("div")%>%
  html_text()
hight.2 <- hight.1[29]
hight.2
#weight as an additional definer
#Sys.sleep(runif(1,4,8))
weight.1 <- subsite.2%>%
  html_nodes("div")%>%
  html_text()
weight.2 <- weight.1[30]
# convicted of 
#Sys.sleep(runif(1,4,8))
prosecution <- subsite.2%>%
  html_nodes("div.charge-title")%>%
  html_text()
#sys.slee(runif(1,8,12))
for (j in 1:length(prosecution)){
sub_prosecution <- prosecution[j]
sublist <- c(name.2,date.1,gender.2,hieght.2,weight.2,sub_prosecution, state, county)
master_df <- rbind(master_df, sublist)
Sys.sleep(round(runif(1,2,4)))
# creating a vector with their information
}
}#so far this part of the code works but I need to go back and fix a repeating 
#issue in the pages extraction process 12-01-2020
# update: fixed!
master_df

beep(8)

write.csv(master_df, "2017_prostitution_Arizona_arrests.csv")
