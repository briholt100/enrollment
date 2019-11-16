library(RSelenium)
library(rvest)
library(tidyr)
library(dplyr)
library(tidyverse)
library(robotstxt)

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


#selenium
robotstxt:::get_robotstxt('https://inside.seattlecolleges.edu/default.aspx?svc=enrollment&page=enrollment')
driver <- rsDriver(browser=c("firefox"))
remote_driver <- driver[["client"]]
remote_driver$open()

remote_driver$navigate("https://inside.seattlecolleges.edu/default.aspx?svc=enrollment&page=enrollment")
RSelenium:::selKeys %>% names()
remote_driver$refresh()
el<- remote_driver$findElement(using='xpath', '//*[@id="TxSID"]') 
el$highlightElement()

el$clickElement()

el$sendKeysToElement(list('login'))


el.1 <- remote_driver$findElement(using = 'xpath', '//*[@id="TxPIN"]')
el.1$highlightElement()
el.1$sendKeysToElement(list(''))
el$sendKeysToElement(list(key='enter'))

col <- c('062','063','064')
q <- c('B903')
qn <- c('Winter 20')


read_html(url.1)

##selecting college

pick_col<- remote_driver$findElement(using = 'xpath', '//*[@id="ctl08_ddlCollegeView"]')
pick_col$highlightElement()
##selecting quarter
pick_qu <- remote_driver$findElement(using = 'xpath', '//*[@id="ctl08_ddlQuarterView"]')
pick_qu$clickElement()
#A782  = =  07 - 08 years, 2 =  fall.  If you want after 2010, must use B782, which would be fall 2017-18
##
#remote_driver$navigate("https://inside.seattlecolleges.edu/enrollment/content/displayReport.aspx?col=063&q=A782&qn=Fall 07&nc=false&in=&cr=")

url_item<- paste0("https://inside.seattlecolleges.edu/enrollment/content/displayReport.aspx?col=",col[2],"&q=",q,"&qn=",qn,"&nc=false&in=&cr=")
remote_driver$navigate(url_item)

page<- remote_driver$getPageSource() %>% .[[1]] %>% read_html()
page_tr<- page %>% html_nodes("tr") %>% html_text()

rows<- page_tr %>% str_squish()
grep('clus',rows,ignore.case=T)
#return class=open
##next, find the "cluster  class" xpath:  /html/body/form/div[3]/span[4]

##after this,  stip out the headers for each sub table--tricky because 2 rows with different headers.
