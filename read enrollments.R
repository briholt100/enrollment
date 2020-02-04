library(tidyverse)
library(dplyr)
library(lubridate)
library(microbenchmark)
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

# Adding new quarters to the corpus ----------------------------------------------------

qc[141,] <- "B901" # "Summer 19"
qc[142,] <- "B902" #"Fall 19")
qc[143,] <- "B903" #"Winter 20")
#qc[144,] <- "B904" #"Spring 20")

# Create main dataframe ---------------------------------------------------

main.df<- data.frame(matrix(ncol=12,nrow=3*nrow(qc)))
colnames(main.df) <- c('Date_Time_acc',
                       'college',
                       'code',
                       'URL.link',
                       'site_enrol',
                       'scrap_tbl',
                       'scrap_enrol',
                       'enrol_diff',
                       'site_clus_enrol',
					   'scrap__clust_tbl',
                       'scrap_clus_enrol',
                       'clus_diff')


main.df[,c("college","code")] <- expand.grid(col,qc$code)    ##creates all possible combinations of college and code
main.df <- main.df[order(main.df$college),]   ##sorts by collge
rownames(main.df) <- 1:nrow(main.df)          ##renames rown names so that they are in order

main.df <- main.df %>% 
  mutate(
    Season=case_when(
      substr(code,4,4)=="1"~paste0("Summer"),
      substr(code,4,4)=="2"~paste0("Fall"),
      substr(code,4,4)=="3"~paste0("Winter"),
      substr(code,4,4)=="4"~paste0("Spring")
    )
  ) %>% select(Date_Time_acc:code,Season,URL.link:clus_diff)


 main.df<- main.df %>%                              ##creates new 'yr' variable to be paired with season in URL
    mutate(
      yr=case_when(
        substr(code,4,4)=="1"~paste0(substr(code,1,2)),
        substr(code,4,4)=="2"~paste0(substr(code,1,2)),
        substr(code,4,4)=="3"~paste0(substr(code,1,1),substr(code,3,3)),
        substr(code,4,4)=="4"~paste0(substr(code,1,1),substr(code,3,3))
      )
    )

# Makes these data tidy and accurate
 
main.df$yr <- sub('B(.)','1\\1',main.df$yr)
main.df$yr <- sub('A(.)','0\\1',main.df$yr)

main.df$yr <- ifelse(main.df$yr =="10" & substr(main.df$code,1,3)=='B90',"20",main.df$yr)
main.df$yr <- ifelse(main.df$yr =="00" & substr(main.df$code,1,3)=='A90',"10",main.df$yr)
main.df$yr <- ifelse(main.df$yr =="90" & substr(main.df$code,1,3)=='990',"00",main.df$yr)
main.df$yr <- ifelse(main.df$yr =="80" & substr(main.df$code,1,3)=='890',"90",main.df$yr)

main.df <- main.df %>% 
  select(Date_Time_acc:Season,yr,URL.link:clus_diff)  #reorder columns
# Create official link to scrape ------------------------------------------


Sys.time()


main.df<- main.df %>% 
  mutate(URL.link=
           paste0("https://inside.seattlecolleges.edu/enrollment/content/displayReport.aspx?col=",college,"&q=",code,"&qn=",Season," ",yr,"&nc=false&in=&cr=")) 

# Do the crawl ------------------------------------------------------------

# pseudo code ------------------------------------------------------------

# 1. start selenium and open browser

# Using Rselenium ------------------------------------------------------------
library(RSelenium)
library(rvest)

# Open driver and sign in to inside seattle colleges --------------------------------------

driver <- rsDriver(browser =c("firefox"))
##https://github.com/ropensci/RSelenium/issues/116
remote_driver <- driver[["client"]]
#remote_driver$close()

remote_driver$navigate("https://inside.seattlecolleges.edu/default.aspx?svc=enrollment&page=enrollment")
RSelenium:::selKeys %>% names()
remote_driver$refresh()



# 2. log in to inside colleges ------------------------------------------

el<- remote_driver$findElement(using='xpath', '//*[@id="TxSID"]')
el$highlightElement()

el$clickElement()

el$sendKeysToElement(list('uid'))  #change to user ID
el.1 <- remote_driver$findElement(using = 'xpath', '//*[@id="TxPIN"]')
el.1$highlightElement()

el.1$sendKeysToElement(list('pid'))  #change to Pass ID
el$sendKeysToElement(list(key='enter'))


#temp <- main.df[sample(1:nrow(main.df),10),]
#temp[,2:5]


for (i in 1:nrow(main.df)){
  main.df$Date_Time_acc[i] <- Sys.time()
  remote_driver$navigate(main.df$URL.link[i])
  print(main.df$URL.link[i])
  
  #insert system delay
  
  #Sys.sleep(sample(0:2,1))
  
  page<- remote_driver$getPageSource() %>% .[[1]] %>% read_html()

  # below uses xpath to pull the site reported count
    xpath.from.src <- '//*[@id="lblEnrollNon"]'  #xpath for total from the bottom of site page
  
    page %>% html_nodes(.,xpath=xpath.from.src)->node.out
    
  print(node.out)
    site.enrolled.count<- html_text (node.out)
    site.enrolled.count<- as.numeric(sub(",","",site.enrolled.count))
    
  main.df$site_enrol[i] <- site.enrolled.count
  
  # below uses xpath to pull the site reported count
  
  xpath.from.src.clustered <- '//*[@id="lblEnrollCluster"]'#clustered class table
    page %>% html_nodes(.,xpath=xpath.from.src.clustered)->node.out
  
    print(node.out)
    site.clust.enrolled.count<- html_text (node.out)
    site.clust.enrolled.count<- as.numeric(sub(",","",site.clust.enrolled.count))
    main.df$site_clus_enrol[i] <- site.clust.enrolled.count
    
    # full table
    
    xpath.from.src <- "/html/body/form/div[3]/table[1]" #xpath for non-clusterd classes
    page %>% html_nodes(.,xpath=xpath.from.src)->table.out
    
    main.df$scrap_tbl[i]<- table.out %>% html_table(fill = T)
	
	
# clustered table
    
    xpath.from.src <- "/html/body/form/div[3]/table[2]" #xpath for clusterd classes
    page %>% html_nodes(.,xpath=xpath.from.src)->table.out
    
    main.df$scrap__clust_tbl[i]<- table.out %>% html_table(fill = T)
    
    print(paste("we are on iteration ",i))
}
save(main.df,file = "main.df.Rdata")

#temp[,c(7,11)]

# 3. for each URL, 
#   3a. open page
#   3b. store sys date and time
#   3c. Check for bad connection (if bad, stop)
#   3d. find and scrape site enrollment  #for comparisons
#   3e. scrape full table and store
#   3g. find and scrape site enrollment of clustered classes  #for comparisons
#   3h. scrape clusterd table and store

# 4. calculate:  
#   4a. differences between site and scraped (2 differences)
#   4b. calc and store scraped enrollments # single number for a quick glance





#summer 04 the 'new' AU codes went into effect

# check valid request -----------------------------------------------------

#this is the xpath for "sorry data not found page"
#/html/body/form/div[3]/div[2]/div


##after this,  stip out the headers for each sub table--tricky because 2 rows with different headers.

#remote_driver$close()

# clean and update non-clustered --------------------------------------------
for (i in 1:nrow(main.df)){
  #load("enroll_report.RData")
  d<- main.df$scrap_tbl[i]#    enroll.report[[1]]
  d <- d[[1]]
  head(d)
  # Get and correct column names --------------------------------------------
  
  var.names <- d[1,]
  colnames(d) <- var.names
  
  colnames(d) <- c("Item","Course ID","Title",
                   "CR","Days.meet","Start.Time",
                   "End.Time","Room","Instructor",
                   "Enrolled","divider","Class.Size",
                   'Waitlist', "Total.FTES","State.FTES",
                   "Pro.Budget","Org.Budget","AU.Budget","empty")
  
  # The following drops non-data rows
  d <- d[grep('[^item]',d$Item,ignore.case=T,value=F),]
  head(d)

  d[,c(2,3,5,6,8,9)] <- apply(d[,c(2,3,5,6,8,9)],2,function(x) gsub(" {2,}"," ",x)) #removes heading space; converts to character
  head(d)
 # str(d)
  
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
  main.df$scrap_tbl[[i]] <- d
  main.df$scrap_enrol[i] <- sum(main.df$scrap_tbl[[i]]$Enrolled,na.rm=T)
}

# clean and update clustered --------------------------------------------
for (i in 1:nrow(main.df)){
  d<- main.df$scrap__clust_tbl[i]#    enroll.report[[1]]
  d <- d[[1]]
  head(d)
  var.names <- d[1,]
  colnames(d) <- var.names

  colnames(d) <- c("Item","Course ID","Title","CR",
                   "Days.meet","Start.Time","End.Time",
                   "Room","Instructor","Enrolled",
                   "divider","Class.Size",'Waitlist', 
                   "Total.FTES","State.FTES","Pro.Budget",
                   "Org.Budget","AU.Budget","Cluster")
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

  # factor: 1:3,5,8:9,15:18
  col.names <- colnames(d[,c(1:3,5,8:9,15:18)])
  d[col.names] <- lapply(d[col.names],factor)
  #str(d)
  main.df$scrap__clust_tbl[[i]] <- d
  main.df$scrap_clus_enrol <- sum(main.df$scrap__clust_tbl[[i]]$Enrolled,na.rm=T)
}


# Stitching main df it together
datalist <- list()
for (i in 1:nrow(main.df)){
  row.count <- nrow(main.df$scrap_tbl[[i]])
 datalist[[i]] <- main.df$scrap_tbl[[i]]
datalist[[i]] <- (cbind(main.df[i,2:5],datalist[[i]]))
}
df <- do.call(rbind,datalist)

# rename Empty column to 'Cluster'
df<- df[,-22]
df$Cluster <- NA
str(df)
# Stitching main clustered df it together  ---------------------------
datalist.clus <- list()
for (i in 1:nrow(main.df)){
  row.count <- nrow(main.df$scrap__clust_tbl[[i]])
  datalist.clus[[i]] <- main.df$scrap__clust_tbl[[i]]
  datalist.clus[[i]] <- (cbind(main.df[i,2:5],datalist.clus[[i]]))
}
df.clus <- do.call(rbind,datalist.clus)

str(df.clus)


# combining two data frames cluster and non -------------------------------

df.tot <- rbind(df,df.clus)
# adding cancel variable, checking differences ----------------------------

df.tot<- df.tot %>% 
  mutate(cancel=ifelse(grepl("cancel",Instructor,ignore.case=T),1,0)) 


# create true 'Date' variable ---------------------------------------------

df.tot<- df.tot %>%
  mutate(Date=
           ifelse(grepl("A|B|9903|9904",code),
                  paste0('20',yr),
                  paste0('19',yr))) %>% 
  mutate(Date=case_when(
    Season=='Summer'~paste0(Date,'-','07','-','20'),
    Season=='Fall'~paste0(Date,'-','10','-','20'),
    Season=='Winter'~paste0(Date,'-','01','-','20'),
    Season=='Spring'~paste0(Date,'-','04','-','20')
  )) %>% mutate(Date = ymd(Date))

save(df.tot,file='seattle_col_enrol_thru_Win_2020.RData')
  


# Fix South's cancelling labeling. ----------------------------------------

load('main.df.b.RData')
South_link_list <- main.df %>% filter(college=='064') %>% select(URL.link) 

South_canceled_main.df <- main.df[0,c(1:5,8,11)]



el<- remote_driver$findElement(using='xpath', '//*[@id="TxSID"]')
el$highlightElement()

el$clickElement()

el$sendKeysToElement(list('uid'))  #change to user ID
el.1 <- remote_driver$findElement(using = 'xpath', '//*[@id="TxPIN"]')
el.1$highlightElement()

el.1$sendKeysToElement(list('pid'))  #change to Pass ID
el$sendKeysToElement(list(key='enter'))


#temp <- main.df[sample(1:nrow(main.df),10),]
#temp[,2:5]

canceled_items <- list()
for (i in 1:nrow(South_link_list)){
  
  remote_driver$navigate(South_link_list[i,])
  print(South_link_list[i,])
  
  #insert system delay
  
  #Sys.sleep(sample(0:2,1))
  
  page<- remote_driver$getPageSource() %>% .[[1]] %>% read_html()
  
  # below uses xpath to pull the canclled classes for all classes both clustered and non
  
  xpath.from.src <- '//*[@class="cancelled"]'  #xpath for total from the bottom of site page
  
  canceled_items[[i]]<- page %>% html_nodes(.,xpath=xpath.from.src) %>% html_text() %>% str_trim() %>% substr(start=1,stop=4)
  
  #but then what?  I just to note them and cross walk them to original using item number + courseId? 
  #which means I don't need to tidy/clean the whole row, I just need to pull the first 2 columns (maybe just item.  )
 # ->table.out
  
 # main.df$scrap_tbl[i]<- table.out %>% html_table(fill = T)
  
  print(paste("we are on iteration ",i))
}
save(canceled_items,file = "South_Canceled_classes.Rdata")




