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


# 1 select college , quarter and year navigate to that page

col <- c('062','063','064') # central north south
q <- c('B903')  #After 2010, 2019 verging on 2020, quarter: winter "3"
qn <- c('Winter 20')

qnum <- c(1:4)
year.start.num <- 9
year.end.num <- 10


qy <- c('Winter 20')
season <- c("Summer","Fall","Winter","Spring")

year.quarter<- paste0('B','.',rep(2019:2000,each=4),'.',4:1,'.',"y")
year.quarter

cbind(year.quarter)


##selecting college

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






