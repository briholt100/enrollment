library(RSelenium)
library(rvest)
library(tidyverse)


#Using selenium
# sign in to inside seattle colleges --------------------------------------

driver <- rsDriver(browser =c("firefox"))
##https://github.com/ropensci/RSelenium/issues/116
remote_driver <- driver[["client"]]
#remote_driver$close()

remote_driver$navigate("https://inside.seattlecolleges.edu/default.aspx?svc=enrollment&page=enrollment")
RSelenium:::selKeys %>% names()
remote_driver$refresh()
el<- remote_driver$findElement(using='xpath', '//*[@id="TxSID"]')
el$highlightElement()

el$clickElement()

el$sendKeysToElement(list('uid'))


el.1 <- remote_driver$findElement(using = 'xpath', '//*[@id="TxPIN"]')
el.1$highlightElement()

el.1$sendKeysToElement(list('id'))
el$sendKeysToElement(list(key='enter'))




# Components of dataframe -------------------------------------------------

# 1 select college , quarter and year navigate to that page

col <- c('062','063','064') # central north south
q <- c('B903')  #After 2010, 2019 verging on 2020, quarter: winter "3"
qn <- c('Winter 20')

qnum <- c(1:4)
year.start.num <- 0:9
year.end.num <- 0:9
###warning
century <- c(8,9,'A','B') #A is pre 2000, B is after
#So, A232 is Fall 1992
##or does it?

# this for loop will create all combo's of quarter and year code ----------
quart.code<- expand.grid(century,0:9,0:9,1:4)
colnames(quart.code) <- c("century","start.year","end.year","season")
quart.code[quart.code$start.year >= quart.code$end.year  ,] <- NA
qc<- quart.code[complete.cases(quart.code),]

qc[order(qc$century,qc$start.year),]

qc[(qc$century=='8' & qc$start.year< 4),] <- NA
qc <- qc[complete.cases(qc),]
qc[qc$end.year-qc$start.year >1,] <- NA
qc <- qc[complete.cases(qc),]
qc <- qc[order(qc$century,qc$start.year),]
qc<- paste0(qc$century,qc$start.year,qc$end.year,qc$season)
qc<- as.data.frame(qc)
qc$qc <- as.character(qc$qc)

colnames(qc) <- "code"

missing <- data.frame(c('8901','8902','8903','8904',
                        '9901','9902','9903','9904',
                        'A901','A902','A903','A904'))
colnames(missing) <- "code"

qc <- rbind(qc,missing)

qc<- qc[order(qc$code),]
qc<- as.data.frame(qc)
colnames(qc) <- "code"


# Create main dataframe ---------------------------------------------------

main.df<- data.frame(matrix(ncol=11,nrow=3*nrow(qc)))
colnames(main.df) <- c('Date_acc','Time_acc',
                       'college',
                       'code',
                       'URL.link',
                       'site_enrol',
                       'scrap_enrol',
                       'enrol_diff',
                       'site_clus_enrol',
                       'scrap_clus_enrol',
                       'clus_diff')

Sys.Date()
Sys.time()

main.df[,3:4] <- expand.grid(col,qc$code)
main.df <- main.df[order(main.df$college),]
rownames(main.df) <- 1:nrow(main.df)

Season <- main.df %>% select(code) %>% 
  transmute(
    Season=case_when(
      substr(code,4,4)=="1"~paste0("Summer"),
      substr(code,4,4)=="2"~paste0("Fall"),
      substr(code,4,4)=="3"~paste0("Winter"),
      substr(code,4,4)=="4"~paste0("Spring")
    )
  )
main.df <- cbind(main.df[,1:4],Season,main.df[5:11])


#main.df$yr <- 

 main.df<- main.df %>%
    mutate(
      yr=case_when(
        substr(code,4,4)=="1"~paste0(substr(code,1,2)),
        substr(code,4,4)=="2"~paste0(substr(code,1,2)),
        substr(code,4,4)=="3"~paste0(substr(code,1,1),substr(code,3,3)),
        substr(code,4,4)=="4"~paste0(substr(code,1,1),substr(code,3,3))
      )
    )
 main.df[,c("code","yr")]
 main.df$yr <- sub('B(.)','1\\1',main.df$yr)
 main.df[,c("code","yr")]
main.df$yr <- sub('A(.)','0\\1',main.df$yr)

main.df$yr <- ifelse(main.df$yr =="90" & substr(main.df$code,1,3)=='990',"00",main.df$yr)

#  if(substr(x,1,3) == "890" & y == "80") {y <- "90"}}
#apply(main.df[,c("code","yr")],1,function(x) correct_year(x[,1],x[,2]))

main.df %>% mutate(URL.link=paste0("https://inside.seattlecolleges.edu/enrollment/content/displayReport.aspx?col=",college,"&q=",code,"&qn=",Season,"&nc=false&in=&cr="))

for (i in 1:nrow(main.df)){
  main.df$URL.link<- paste0("https://inside.seattlecolleges.edu/enrollment/content/displayReport.aspx?col=",main.df$college[i],"&q=",quarter.year,"&qn=",qc$qy[j],"&nc=false&in=&cr=")
}

for (i in 1:length(main.df$URL.link)){
  if(substr(main.df$URL.link[i],start=78,stop=80 )=='064'){print(i);print(T)}
  
}




# create quarter year title -----------------------------------------------

qc %>% select(code) %>% 
  mutate(
        qy=case_when(
          substr(code,4,4)=="1"~paste0("Summer ",substr(code,1,2)),
          substr(code,4,4)=="2"~paste0("Fall ",substr(code,1,2)),
          substr(code,4,4)=="3"~paste0("Winter ",substr(code,1,1),substr(code,3,3)),
          substr(code,4,4)=="4"~paste0("Spring ",substr(code,1,1),substr(code,3,3))
      )
)->qc

qc[,2] <- sub("A","0",qc$qy)
qc[,2] <- sub("B","1",qc$qy)
(qc)
##Items #23,24,63,64 are wrong; fix below
qc[23,2] <- "Winter 90"
qc[24,2] <- "Spring 90"
qc[63,2] <- "Winter 00"
qc[64,2] <- "Spring 00"


# Adding to the corpus ----------------------------------------------------


qc[141,] <- c("B901","Summer 19")
qc[142,] <- c("B902","Fall 19")
qc[143,] <- c("B903","Winter 20")
qc[144,] <- c("B904","Spring 20")
qc
# Make URL Corpus ---------------------------------------------------------


# selecting college -------------------------------------------------------



pick_col<- remote_driver$findElement(using = 'xpath', '//*[@id="ctl08_ddlCollegeView"]')
pick_col$highlightElement()
##selecting quarter
pick_qu <- remote_driver$findElement(using = 'xpath', '//*[@id="ctl08_ddlQuarterView"]')
pick_qu$clickElement()
#A782  = =  07 - 08 years, 2 =  fall.  If you want after 2010, must use B782, which would be fall 2017-18
##

# The following beings with fall 2006 at North, noted by these  --------
# col=063&q=A012&qn=Fall 01

url <- "https://inside.seattlecolleges.edu/enrollment/content/displayReport.aspx?col=063&q=A672&qn=Fall 06&nc=false&in=&cr="

#summer 04 the 'new' AU codes went into effect

# check valid request -----------------------------------------------------

#this is the xpath for "sorry data not found page"
#/html/body/form/div[3]/div[2]/div

#
# 1 scrape full page

remote_driver$navigate(url)

page<- remote_driver$getPageSource() %>% .[[1]] %>% read_html()




# 1a scrape site's campus and quarter/year
#
# 2. scrape and tidy generated eroll count
# get site generated count for all non clusterd classes: ------------------

xpath.from.src <- '//*[@id="lblEnrollNon"]'  #xpath for total from the bottom of site page
page %>% html_nodes(.,xpath=xpath.from.src)->node.out
site.enrolled.count<- html_text (node.out)
site.enrolled.count<- as.numeric(sub(",","",site.enrolled.count))

site.enrolled.count

#
# 3. scrape and tidy main table
#
xpath.from.src <- "/html/body/form/div[3]/table[1]" #xpath for non-clusterd classes
page %>% html_nodes(.,xpath=xpath.from.src)->table.out

table.out %>% html_table(fill = T)-> enroll.report

### the following just saves a single enroll.report for practice in tidying
remote_driver$navigate(url.1)

save(enroll.report,file='enroll_report.RData')





##after this,  stip out the headers for each sub table--tricky because 2 rows with different headers.


xpath.from.src.clustered <- '/html/body/form/div[3]/table[2]'#clustered class table



remote_driver$close()

# Load and Clean ----------------------------------------------------------

#load("enroll_report.RData")
d<- enroll.report[[1]]

# Get and correct column names --------------------------------------------

var.names <- d[1,]
colnames(d) <- var.names

colnames(d) <- c("Item","Course ID","Title","CR","Days.meet","Start.Time","End.Time","Room","Instructor","Enrolled","divider","Class.Size",'Waitlist', "Total.FTES","State.FTES","Pro.Budget","Org.Budget","AU.Budget","empty")

# The following drops non-data rows
d <- d[grep('[^item]',d$Item,ignore.case=T,value=F),]
head(d)

d[,c(2,3,5,6,8,9)] <- apply(d[,c(2,3,5,6,8,9)],2,function(x) gsub(" {2,}"," ",x))
head(d)
str(d)

# drop 'divider', var number 11
d<- d[,-11]

# convert variables to factors, int, numeric ------------------------------

#num:  4,10:14
d[,c(4,10:14)] <- apply(d[,c(4,10:14)],2,as.numeric)

#time: 6:7
# d[,c(6:7)] <- apply(d[,c(6,7)],2,as.Date)

# factor: 1:3,5,8:9,15:18
col.names <- colnames(d[,c(1:3,5,8:9,15:18)])
d[col.names] <- lapply(d[col.names],factor)
str(d)

# 4. count enrollments


d %>% mutate(cancel=ifelse(Instructor == "Cancelled",1,0)) %>% group_by(cancel) %>% summarize(sumEnrol=sum(Enrolled),sumFTE=sum(Total.FTES),sumStateFte=sum(State.FTES),meanEnr=sumEnrol/n())

#should do a histogram of class caps...so many are 0
hist(d$Class.Size)
hist(d$Class.Size[d$Instructor!="Cancelled"])
stem(d$Class.Size)

d %>% mutate(cancel=ifelse(Instructor == "Cancelled",1,0)) %>% group_by(cancel) %>% ggplot(aes(x=Class.Size)) + geom_histogram()+facet_wrap(~cancel)
# Histograms of class sizes -----------------------------------------------





#
# 5. compare site vs calculated enrollment
#
sum(d$Enrolled)-site.enrolled.count

# 6. store data into following fields:
#
#   campus
# site quarter
# site year
# site enrollment
# scraped enrollment
# difference between site and scraped enrollment






