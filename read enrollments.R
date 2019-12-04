library(RSelenium)
library(rvest)
library(tidyverse)
col <- c('062','063','064')
q <- c('B903')
qn <- c('Winter 20')

url.1 <- "https://inside.seattlecolleges.edu/default.aspx?svc=enrollment&page=enrollment"

content <- read_html(url.1)
 content %>%  html_text(.,trim = F) %>%
  jsonlite::fromJSON(.)




download.file(url.1, destfile = "scrapedpage.html", quiet=TRUE)
content <- read_html("scrapedpage.html")
content %>% html_nodes(xpath='/html/head/title')
content %>%  html_table(.,fill=T) -> table.list
content %>% html_nodes("tr")


#selenium
robotstxt:::get_robotstxt('https://inside.seattlecolleges.edu/default.aspx?svc=enrollment&page=enrollment')
driver <- rsDriver(browser =c("firefox"))
##https://github.com/ropensci/RSelenium/issues/116
remote_driver <- driver[["client"]]
#remote_driver$open()

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

col <- c('062','063','064')
q <- c('B903')
qnum <- c(1:4)
year.start.num <- 9
year.end.num <- 10


qy <- c('Winter 20')
season <- c("Summer","Fall","Winter","Spring")

year.quarter<- paste0('B','.',rep(2019:2000,each=4),'.',4:1,'.',"y")
year.quarter

cbind(year.quarter)




read_html(url.1)

##selecting college

pick_col<- remote_driver$findElement(using = 'xpath', '//*[@id="ctl08_ddlCollegeView"]')
pick_col$highlightElement()
##selecting quarter
pick_qu <- remote_driver$findElement(using = 'xpath', '//*[@id="ctl08_ddlQuarterView"]')
pick_qu$clickElement()
#A782  = =  07 - 08 years, 2 =  fall.  If you want after 2010, must use B782, which would be fall 2017-18
##

url <- "https://inside.seattlecolleges.edu/enrollment/content/displayReport.aspx?col=063&q=A012&qn=Fall 01&nc=false&in=&cr="
remote_driver$navigate(url)

url_item<- paste0("https://inside.seattlecolleges.edu/enrollment/content/displayReport.aspx?col=",col[2],"&q=",q,"&qn=",qn,"&nc=false&in=&cr=")
remote_driver$navigate(url_item)

page<- remote_driver$getPageSource() %>% .[[1]] %>% read_html()

xpath.from.src <- "/html/body/form/div[3]/table[1]" #xpath for non-clusterd classes
page %>% html_nodes(.,xpath=xpath.from.src)->table.out

table.out %>% html_table(fill = T)-> enroll.report

##after this,  stip out the headers for each sub table--tricky because 2 rows with different headers.


xpath.from.src.clustered <- '/html/body/form/div[3]/table[2]'#clustered class table



remote_driver$close()
