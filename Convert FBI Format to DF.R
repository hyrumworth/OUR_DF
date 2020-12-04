library(dplyr)
library(ggplot2)
library(tidyr)
library(beepr)
#reading in the dataframes 
df <- read.csv("human-trafficking-2018.txt", sep = "\n", row.names = NULL, header = F)

# With the new desired format of the dataframe, I will need to change this row by row instead of column by column
head(df)
df$Raw <- df$V1
df <- subset(df, select = Raw)
formattedf <- data.frame()

#cl <- makePSOCKcluster(5)
#registerDoParallel(cl, 3)

timestamp()
#### creating the for loop####
for (i in 1:length(df$V1)){
####creating the semi identifying variables ####
# creating the variable crime type. 
crime_type <- substr(df[i,1], 1,1)
# creating the variable ORI_stae_code
ORI_state_code <- substr(df[i,1], 2,3)
#Variable ORI code
ORI_Code <- substr(df[i,1], 4,10)
State_Abbrev <- substr(df$ORI_Code,1,2)
# variable region
Region <- substr(df[i,1], 13,13)
#variable year
Year <- substr(df[i,1],14,15)
# MSA variable 
MSA <- substr(df[i,1],21,21)
# Agency name variable 
Agency_Name <- substr(df[i,1],46,69)
# Agency population variable 
Agency_pop <- as.numeric(substr(df[i,1],37,45))
#list of the basic identifying information.
bil<- c(crime_type,ORI_Code, ORI_state_code,Region,Year,MSA,Agency_Name,
        Agency_pop)
####January ####
Month <- "Jan"
Report_type <- substr(df[i,1],76,76)
RK_SA <- substr(df[i,1],88,92)
RK_IS <- substr(df[i,1],93,97)
RK_GT <- substr(df[i,1],98,102)
UF_SA <-substr(df[i,1],103,107)
UF_IS <-substr(df[i,1],108,112)
UF_GT <-substr(df[i,1],113,117)
AO_SA <-substr(df[i,1],118,122)
AO_IS <-substr(df[i,1],123,127)
AO_GT <-substr(df[i,1],128,132)
OC_SA <-substr(df[i,1],133,137)
OC_IS  <-substr(df[i,1],138,142)
OC_GT <-substr(df[i,1],143,147)
CU18_SA <-substr(df[i,1],148,152)
CU18_IS <-substr(df[i,1],153,157)
CU18_GT <-substr(df[i,1],158,162)
Jan_firstR <- c(Month, Report_type,RK_SA,RK_IS,RK_GT,UF_SA,UF_IS,UF_GT,
                AO_SA,AO_IS,AO_GT,OC_SA,OC_IS,OC_GT,CU18_SA,CU18_IS,
                CU18_GT)
Jan_firstR <- c(bil, Jan_firstR)
####Feb numbers ####
Month <- "Feb"
Report_type <- substr(df[i,1],77,77)
RK_SA <- substr(df[i,1],88+75,92+75)
RK_IS <- substr(df[i,1],93+75,97+75)
RK_GT <- substr(df[i,1],98+75,102+75)
UF_SA <-substr(df[i,1],103+75,107+75)
UF_IS <-substr(df[i,1],108+75,112+75)
UF_GT <-substr(df[i,1],113+75,117+75)
AO_SA <-substr(df[i,1],118+75,122+75)
AO_IS <-substr(df[i,1],123+75,127+75)
AO_GT <-substr(df[i,1],128+75,132+75)
OC_SA <-substr(df[i,1],133+75,137+75)
OC_IS  <-substr(df[i,1],138+75,142+75)
OC_GT <-substr(df[i,1],143+75,147+75)
CU18_SA <-substr(df[i,1],148+75,152+75)
CU18_IS <-substr(df[i,1],153+75,157+75)
CU18_GT <-substr(df[i,1],158+75,162+75)
Feb_firstR <- c(Month, Report_type,RK_SA,RK_IS,RK_GT,UF_SA,UF_IS,UF_GT,
                AO_SA,AO_IS,AO_GT,OC_SA,OC_IS,OC_GT,CU18_SA,CU18_IS,
                CU18_GT)
Feb_firstR <- c(bil, Feb_firstR)
#### March ####
Month <- "Mar"
Report_type <- substr(df[i,1],78,78)
RK_SA <- substr(df[i,1],88+75*2,92+75*2)
RK_IS <- substr(df[i,1],93+75*2,97+75*2)
RK_GT <- substr(df[i,1],98+75*2,102+75*2)
UF_SA <-substr(df[i,1],103+75*2,107+75*2)
UF_IS <-substr(df[i,1],108+75*2,112+75*2)
UF_GT <-substr(df[i,1],113+75*2,117+75*2)
AO_SA <-substr(df[i,1],118+75*2,122+75*2)
AO_IS <-substr(df[i,1],123+75*2,127+75*2)
AO_GT <-substr(df[i,1],128+75*2,132+75*2)
OC_SA <-substr(df[i,1],133+75*2,137+75*2)
OC_IS  <-substr(df[i,1],138+75*2,142+75*2)
OC_GT <-substr(df[i,1],143+75*2,147+75*2)
CU18_SA <-substr(df[i,1],148+75*2,152+75*2)
CU18_IS <-substr(df[i,1],153+75*2,157+75*2)
CU18_GT <-substr(df[i,1],158+75*2,162+75*2)
Mar_firstR <- c(Month, Report_type,RK_SA,RK_IS,RK_GT,UF_SA,UF_IS,UF_GT,
                AO_SA,AO_IS,AO_GT,OC_SA,OC_IS,OC_GT,CU18_SA,CU18_IS,
                CU18_GT)
Mar_firstR <- c(bil, Mar_firstR)
#### April Numbers####
Month <- "Apr"
x = 3
Report_type <- substr(df[i,1],79,79)
RK_SA <- substr(df[i,1],88+75*x,92+75*x)
RK_IS <- substr(df[i,1],93+75*x,97+75*x)
RK_GT <- substr(df[i,1],98+75*x,102+75*x)
UF_SA <-substr(df[i,1],103+75*x,107+75*x)
UF_IS <-substr(df[i,1],108+75*x,112+75*x)
UF_GT <-substr(df[i,1],113+75*x,117+75*x)
AO_SA <-substr(df[i,1],118+75*x,122+75*x)
AO_IS <-substr(df[i,1],123+75*x,127+75*x)
AO_GT <-substr(df[i,1],128+75*x,132+75*x)
OC_SA <-substr(df[i,1],133+75*x,137+75*x)
OC_IS  <-substr(df[i,1],138+75*x,142+75*x)
OC_GT <-substr(df[i,1],143+75*x,147+75*x)
CU18_SA <-substr(df[i,1],148+75*x,152+75*x)
CU18_IS <-substr(df[i,1],153+75*x,157+75*x)
CU18_GT <-substr(df[i,1],158+75*x,162+75*x)
Apr_firstR <- c(Month, Report_type,RK_SA,RK_IS,RK_GT,UF_SA,UF_IS,UF_GT,
                AO_SA,AO_IS,AO_GT,OC_SA,OC_IS,OC_GT,CU18_SA,CU18_IS,
                CU18_GT)
Apr_firstR <- c(bil, Apr_firstR)

####May####
Month <- "May"
x = 4
Report_type <- substr(df[i,1],80,80)
RK_SA <- substr(df[i,1],88+75*x,92+75*x)
RK_IS <- substr(df[i,1],93+75*x,97+75*x)
RK_GT <- substr(df[i,1],98+75*x,102+75*x)
UF_SA <-substr(df[i,1],103+75*x,107+75*x)
UF_IS <-substr(df[i,1],108+75*x,112+75*x)
UF_GT <-substr(df[i,1],113+75*x,117+75*x)
AO_SA <-substr(df[i,1],118+75*x,122+75*x)
AO_IS <-substr(df[i,1],123+75*x,127+75*x)
AO_GT <-substr(df[i,1],128+75*x,132+75*x)
OC_SA <-substr(df[i,1],133+75*x,137+75*x)
OC_IS  <-substr(df[i,1],138+75*x,142+75*x)
OC_GT <-substr(df[i,1],143+75*x,147+75*x)
CU18_SA <-substr(df[i,1],148+75*x,152+75*x)
CU18_IS <-substr(df[i,1],153+75*x,157+75*x)
CU18_GT <-substr(df[i,1],158+75*x,162+75*x)
May_firstR <- c(Month, Report_type,RK_SA,RK_IS,RK_GT,UF_SA,UF_IS,UF_GT,
                AO_SA,AO_IS,AO_GT,OC_SA,OC_IS,OC_GT,CU18_SA,CU18_IS,
                CU18_GT)
May_firstR <- c(bil, May_firstR)
####June ####
Month <- "June"
x = 5
Report_type <- substr(df[i,1],81,81)
RK_SA <- substr(df[i,1],88+75*x,92+75*x)
RK_IS <- substr(df[i,1],93+75*x,97+75*x)
RK_GT <- substr(df[i,1],98+75*x,102+75*x)
UF_SA <-substr(df[i,1],103+75*x,107+75*x)
UF_IS <-substr(df[i,1],108+75*x,112+75*x)
UF_GT <-substr(df[i,1],113+75*x,117+75*x)
AO_SA <-substr(df[i,1],118+75*x,122+75*x)
AO_IS <-substr(df[i,1],123+75*x,127+75*x)
AO_GT <-substr(df[i,1],128+75*x,132+75*x)
OC_SA <-substr(df[i,1],133+75*x,137+75*x)
OC_IS  <-substr(df[i,1],138+75*x,142+75*x)
OC_GT <-substr(df[i,1],143+75*x,147+75*x)
CU18_SA <-substr(df[i,1],148+75*x,152+75*x)
CU18_IS <-substr(df[i,1],153+75*x,157+75*x)
CU18_GT <-substr(df[i,1],158+75*x,162+75*x)
Jun_firstR <- c(Month, Report_type,RK_SA,RK_IS,RK_GT,UF_SA,UF_IS,UF_GT,
                AO_SA,AO_IS,AO_GT,OC_SA,OC_IS,OC_GT,CU18_SA,CU18_IS,
                CU18_GT)
Jun_firstR <- c(bil, Jun_firstR)

#### July####
Month <- "Jul"
x = 6
Report_type <- substr(df[i,1],82,82)
RK_SA <- substr(df[i,1],88+75*x,92+75*x)
RK_IS <- substr(df[i,1],93+75*x,97+75*x)
RK_GT <- substr(df[i,1],98+75*x,102+75*x)
UF_SA <-substr(df[i,1],103+75*x,107+75*x)
UF_IS <-substr(df[i,1],108+75*x,112+75*x)
UF_GT <-substr(df[i,1],113+75*x,117+75*x)
AO_SA <-substr(df[i,1],118+75*x,122+75*x)
AO_IS <-substr(df[i,1],123+75*x,127+75*x)
AO_GT <-substr(df[i,1],128+75*x,132+75*x)
OC_SA <-substr(df[i,1],133+75*x,137+75*x)
OC_IS  <-substr(df[i,1],138+75*x,142+75*x)
OC_GT <-substr(df[i,1],143+75*x,147+75*x)
CU18_SA <-substr(df[i,1],148+75*x,152+75*x)
CU18_IS <-substr(df[i,1],153+75*x,157+75*x)
CU18_GT <-substr(df[i,1],158+75*x,162+75*x)
Jul_firstR <- c(Month, Report_type,RK_SA,RK_IS,RK_GT,UF_SA,UF_IS,UF_GT,
                AO_SA,AO_IS,AO_GT,OC_SA,OC_IS,OC_GT,CU18_SA,CU18_IS,
                CU18_GT)
Jul_firstR <- c(bil, Jul_firstR)
#### Aug ####
Month <- "Aug"
x = 7
Report_type <- substr(df[i,1],83,83)
RK_SA <- substr(df[i,1],88+75*x,92+75*x)
RK_IS <- substr(df[i,1],93+75*x,97+75*x)
RK_GT <- substr(df[i,1],98+75*x,102+75*x)
UF_SA <-substr(df[i,1],103+75*x,107+75*x)
UF_IS <-substr(df[i,1],108+75*x,112+75*x)
UF_GT <-substr(df[i,1],113+75*x,117+75*x)
AO_SA <-substr(df[i,1],118+75*x,122+75*x)
AO_IS <-substr(df[i,1],123+75*x,127+75*x)
AO_GT <-substr(df[i,1],128+75*x,132+75*x)
OC_SA <-substr(df[i,1],133+75*x,137+75*x)
OC_IS  <-substr(df[i,1],138+75*x,142+75*x)
OC_GT <-substr(df[i,1],143+75*x,147+75*x)
CU18_SA <-substr(df[i,1],148+75*x,152+75*x)
CU18_IS <-substr(df[i,1],153+75*x,157+75*x)
CU18_GT <-substr(df[i,1],158+75*x,162+75*x)
Aug_firstR <- c(Month, Report_type,RK_SA,RK_IS,RK_GT,UF_SA,UF_IS,UF_GT,
                AO_SA,AO_IS,AO_GT,OC_SA,OC_IS,OC_GT,CU18_SA,CU18_IS,
                CU18_GT)
Aug_firstR <- c(bil, Aug_firstR)
#### September ####
Month <- "Sep"
x = 8
Report_type <- substr(df[i,1],84,84)
RK_SA <- substr(df[i,1],88+75*x,92+75*x)
RK_IS <- substr(df[i,1],93+75*x,97+75*x)
RK_GT <- substr(df[i,1],98+75*x,102+75*x)
UF_SA <-substr(df[i,1],103+75*x,107+75*x)
UF_IS <-substr(df[i,1],108+75*x,112+75*x)
UF_GT <-substr(df[i,1],113+75*x,117+75*x)
AO_SA <-substr(df[i,1],118+75*x,122+75*x)
AO_IS <-substr(df[i,1],123+75*x,127+75*x)
AO_GT <-substr(df[i,1],128+75*x,132+75*x)
OC_SA <-substr(df[i,1],133+75*x,137+75*x)
OC_IS  <-substr(df[i,1],138+75*x,142+75*x)
OC_GT <-substr(df[i,1],143+75*x,147+75*x)
CU18_SA <-substr(df[i,1],148+75*x,152+75*x)
CU18_IS <-substr(df[i,1],153+75*x,157+75*x)
CU18_GT <-substr(df[i,1],158+75*x,162+75*x)
Sep_firstR <- c(Month, Report_type,RK_SA,RK_IS,RK_GT,UF_SA,UF_IS,UF_GT,
                AO_SA,AO_IS,AO_GT,OC_SA,OC_IS,OC_GT,CU18_SA,CU18_IS,
                CU18_GT)
Sep_firstR <- c(bil, Sep_firstR)
#### October####
Month <- "Oct"
x = 9
Report_type <- substr(df[i,1],85,85)
RK_SA <- substr(df[i,1],88+75*x,92+75*x)
RK_IS <- substr(df[i,1],93+75*x,97+75*x)
RK_GT <- substr(df[i,1],98+75*x,102+75*x)
UF_SA <-substr(df[i,1],103+75*x,107+75*x)
UF_IS <-substr(df[i,1],108+75*x,112+75*x)
UF_GT <-substr(df[i,1],113+75*x,117+75*x)
AO_SA <-substr(df[i,1],118+75*x,122+75*x)
AO_IS <-substr(df[i,1],123+75*x,127+75*x)
AO_GT <-substr(df[i,1],128+75*x,132+75*x)
OC_SA <-substr(df[i,1],133+75*x,137+75*x)
OC_IS  <-substr(df[i,1],138+75*x,142+75*x)
OC_GT <-substr(df[i,1],143+75*x,147+75*x)
CU18_SA <-substr(df[i,1],148+75*x,152+75*x)
CU18_IS <-substr(df[i,1],153+75*x,157+75*x)
CU18_GT <-substr(df[i,1],158+75*x,162+75*x)
Oct_firstR <- c(Month, Report_type,RK_SA,RK_IS,RK_GT,UF_SA,UF_IS,UF_GT,
                AO_SA,AO_IS,AO_GT,OC_SA,OC_IS,OC_GT,CU18_SA,CU18_IS,
                CU18_GT)
Oct_firstR <- c(bil, Oct_firstR)
####November ####
Month <- "Nov"
x = 10
Report_type <- substr(df[i,1],86,86)
RK_SA <- substr(df[i,1],88+75*x,92+75*x)
RK_IS <- substr(df[i,1],93+75*x,97+75*x)
RK_GT <- substr(df[i,1],98+75*x,102+75*x)
UF_SA <-substr(df[i,1],103+75*x,107+75*x)
UF_IS <-substr(df[i,1],108+75*x,112+75*x)
UF_GT <-substr(df[i,1],113+75*x,117+75*x)
AO_SA <-substr(df[i,1],118+75*x,122+75*x)
AO_IS <-substr(df[i,1],123+75*x,127+75*x)
AO_GT <-substr(df[i,1],128+75*x,132+75*x)
OC_SA <-substr(df[i,1],133+75*x,137+75*x)
OC_IS  <-substr(df[i,1],138+75*x,142+75*x)
OC_GT <-substr(df[i,1],143+75*x,147+75*x)
CU18_SA <-substr(df[i,1],148+75*x,152+75*x)
CU18_IS <-substr(df[i,1],153+75*x,157+75*x)
CU18_GT <-substr(df[i,1],158+75*x,162+75*x)
Nov_firstR <- c(Month, Report_type,RK_SA,RK_IS,RK_GT,UF_SA,UF_IS,UF_GT,
                AO_SA,AO_IS,AO_GT,OC_SA,OC_IS,OC_GT,CU18_SA,CU18_IS,
                CU18_GT)
Nov_firstR <- c(bil, Nov_firstR)
#### December ####
Month <- "Dec"
x = 11
Report_type <- substr(df[i,1],87,87)
RK_SA <- substr(df[i,1],88+75*x,92+75*x)
RK_IS <- substr(df[i,1],93+75*x,97+75*x)
RK_GT <- substr(df[i,1],98+75*x,102+75*x)
UF_SA <-substr(df[i,1],103+75*x,107+75*x)
UF_IS <-substr(df[i,1],108+75*x,112+75*x)
UF_GT <-substr(df[i,1],113+75*x,117+75*x)
AO_SA <-substr(df[i,1],118+75*x,122+75*x)
AO_IS <-substr(df[i,1],123+75*x,127+75*x)
AO_GT <-substr(df[i,1],128+75*x,132+75*x)
OC_SA <-substr(df[i,1],133+75*x,137+75*x)
OC_IS  <-substr(df[i,1],138+75*x,142+75*x)
OC_GT <-substr(df[i,1],143+75*x,147+75*x)
CU18_SA <-substr(df[i,1],148+75*x,152+75*x)
CU18_IS <-substr(df[i,1],153+75*x,157+75*x)
CU18_GT <-substr(df[i,1],158+75*x,162+75*x)
Dec_firstR <- c(Month, Report_type,RK_SA,RK_IS,RK_GT,UF_SA,UF_IS,UF_GT,
                AO_SA,AO_IS,AO_GT,OC_SA,OC_IS,OC_GT,CU18_SA,CU18_IS,
                CU18_GT)
Dec_firstR <- c(bil, Dec_firstR)

Omega <- rbind(Jan_firstR,Feb_firstR,Mar_firstR,Apr_firstR,May_firstR,
               Jun_firstR,Jul_firstR,Aug_firstR,Sep_firstR,Oct_firstR,
               Nov_firstR,Dec_firstR)
Omega <- as.data.frame(Omega)

formattedf <- rbind(formattedf,Omega)

}
timestamp()
library(beepr)
beep(sound = 3, expr = NULL)
