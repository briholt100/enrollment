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
remote_driver$open()
#remote_driver$close()
remote_driver$navigate("https://inside.seattlecolleges.edu/default.aspx?svc=enrollment&page=enrollment")
RSelenium:::selKeys %>% names()
remote_driver$refresh()
el<- remote_driver$findElement(using='xpath', '//*[@id="TxSID"]')
el$highlightElement()

el$clickElement()

el$sendKeysToElement(list('id'))


el.1 <- remote_driver$findElement(using = 'xpath', '//*[@id="TxPIN"]')
el.1$highlightElement()

el.1$sendKeysToElement(list('pass'))
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
#remote_driver$navigate("https://inside.seattlecolleges.edu/enrollment/content/displayReport.aspx?col=063&q=A782&qn=Fall 07&nc=false&in=&cr=")

url_item<- paste0("https://inside.seattlecolleges.edu/enrollment/content/displayReport.aspx?col=",col[2],"&q=",q,"&qn=",qn,"&nc=false&in=&cr=")
remote_driver$navigate(url_item)

page<- remote_driver$getPageSource() %>% .[[1]] %>% read_html()


page %>% html_nodes("#rptClusterRpt_ctl417_row") #reads the last tr row, 417
page_tr<- page %>% html_nodes("tr") %>% html_text()

rows<- page_tr %>% str_squish()
grep('clus',rows,ignore.case=T)
#return class=open
##next, find the "cluster  class" xpath:  /html/body/form/div[3]/span[4]

##after this,  stip out the headers for each sub table--tricky because 2 rows with different headers.




<td style="text-align: center; border-right: solid 1px #FFFFFF;">
  9650
</td>
  <td style="text-align: left; padding-left: 4px; border-right: solid 1px #FFFFFF; white-space: nowrap;">
  SSC  101&nbsp;D1</td>
  <td style="border-right: solid 1px #FFFFFF;" width="*">
  INTRO TO INFO RES FOR SS</td>
  <td style="text-align: center; border-right: solid 1px #FFFFFF;">
  <span id="rptClusterRpt_ctl417_lblCredit">3</span>
  </td>
  <td style="border-right: solid 1px #FFFFFF; min-width: 50px;">
  <span id="rptClusterRpt_ctl417_lbldays" style="font-weight:bold;">Online</span>
  </td>
  <td style="border-right: solid 1px #FFFFFF; white-space: nowrap;">
  <span id="rptClusterRpt_ctl417_lblStartTime"><center>-</center></span>
  </td>
  <td style="border-right: solid 1px #FFFFFF; white-space: nowrap;">
  <span id="rptClusterRpt_ctl417_lblEndTime"><center>-</center></span>
  </td>
  <td style="white-space: nowrap; border-right: solid 1px #FFFFFF;">
  <span id="rptClusterRpt_ctl417_lblRoom"><center>-</center></span>
  </td>
  <td style="border-right: solid 1px #FFFFFF;">
  <span id="rptClusterRpt_ctl417_Instructor">Avillar</span>
  </td>
  <td style="text-align: right; width: 20px;">
  <b>3</b>
  </td>
  <td style="text-align: center;">/</td>
  <td style="text-align: left; width: 20px; border-right: solid 1px #FFFFFF;">
  <b class="small">18</b>
  </td>
  <td style="text-align: center;">
  <span id="rptClusterRpt_ctl417_lblWaitList">0</span>
  </td>
  <td style="text-align: center; border-left: solid 1px #FFFFFF;">
  <span id="rptClusterRpt_ctl417_FTE">0.6</span>
  </td>
  <td style="text-align: center; border-left: solid 1px #FFFFFF;">
  <span id="rptClusterRpt_ctl417_FTE_state">0.2</span>
  </td>
  <td style="text-align: center; border-left: solid 1px #FFFFFF;">
  <span id="rptClusterRpt_ctl417_PRGindx">011</span></td>
  <td style="text-align: center; border-left: solid 0px #FFFFFF;">
  <span id="rptClusterRpt_ctl417_ORGindx">3G05</span></td>
  <td style="text-align: center; border-left: solid 1px #FFFFFF;">
  <span id="rptClusterRpt_ctl417_adminUnit">KK</span></td>
  <td style="text-align: center; border-left: solid 1px #FFFFFF;">
  <span id="rptClusterRpt_ctl417_Cluster">9650B903</span>
  </td>
