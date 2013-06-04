rm(list=ls())
library(psych)
library(venneuler)
library(lattice)
library(ggplot2)

#load("~/My Data Sources/SCCD enrollment reports/SccdEnrollmentsNonCancelled.RData")

#some notes for analyzing data
#you want to look at data, check for reasonable ranges, scores
#tapply(x,INDEX--c(variable1,2,3),range, mean, median)

#on home
SccdEnrollments<-read.csv("C:\\Documents and Settings\\brian\\My Documents\\My Data Sources\\SCCD enrollment reports\\SccdEnrollments.csv",header=T, sep=",")

#On AFT
#SccdEnrollments<-read.csv("C:\\Users\\Brian\\Desktop\\R-Data\\SCCDEnrollments\\SccdEnrollments.csv",header=T, sep=",")

#oncampus
#SccdEnrollments<-read.csv("I:\\My Data Sources\\SCCDEnrollments/SccdEnrollments.csv",header=T, sep=",")
ls()

attach(SccdEnrollments)
describe(SccdEnrollments)
#describeBy(list(c(Enrolled,classCap)),list(c(Campus,Type)))
SccdEnrollments<-SccdEnrollments[,-18]
levels(TYPE.days)
Type<-"Typical"
levels(Type)
SccdEnrollments<-data.frame(SccdEnrollments, Type)
rm(Type)
head(SccdEnrollments)

online.list<-grep("online", SccdEnrollments$TYPE.days, ignore.case=T, value=F)
odd.list<-grep("\\.|Arr|Correspondence|Other|tele", SccdEnrollments$TYPE.days, ignore.case=T, value=F)
hybrid.list<-grep("hybrid", SccdEnrollments$TYPE.days, ignore.case=T, value=F)

SccdEnrollments$Type<-factor(SccdEnrollments$Type, 
                       sort(c(
                         "Typical", 
                         "Odd", 
                         "Hybrid", 
                         "Online"
                       )
                       ))

SccdEnrollments$Type[online.list]<-"Online"
SccdEnrollments$Type[SccdEnrollments$TYPE.days==""]<-"Odd"
SccdEnrollments$Type[hybrid.list]<-"Hybrid"
SccdEnrollments$Type[odd.list]<-"Odd"

SccdEnrollments$Type<-relevel(SccdEnrollments$Type, "Odd")
table(SccdEnrollments$Type)

#fixed below is whether it returns a logical.

cancelled<-grep("can",Instructor,ignore.case=T,fixed=F)


SccdEnrollments<-cbind(SccdEnrollments,"cancel"= c("live","cancelled"))
SccdEnrollments$cancel="live"
SccdEnrollments$cancel[cancelled]="cancelled"
table(SccdEnrollments$cancel)
SccdEnrollments$cancel<-factor(SccdEnrollments$cancel)
SccdEnrollments[SccdEnrollments$Enrolled != 0 & SccdEnrollments$cancel=="cancelled",10:11]
SccdEnrollments$cancel[SccdEnrollments$Enrolled != 0 & SccdEnrollments$cancel=="canclled"]="live"

names(SccdEnrollments)
head(SccdEnrollments[c(-12,-21)])

SccdEnrollments<-SccdEnrollments[c(-12,-21)]
split.by.cancel<-split(SccdEnrollments,SccdEnrollments$cancel)
split.by.campus.and.type<-split(SccdEnrollments,list(SccdEnrollments$Campus,SccdEnrollments$Type))
split.by.campus.type.cancel<-split(SccdEnrollments,list(SccdEnrollments$Campus,SccdEnrollments$Type,SccdEnrollments$cancel))
split.by.type<-split(SccdEnrollments,list(SccdEnrollments$Type))

SccdEnrollments$Title<-gsub("ENGLISH COMPOSITION|ENGLISH COMPOSITION I","ENGLISH COMPOSITION I",SccdEnrollments$Title, ignore.case=T)
SccdEnrollments$Title<-gsub("US HISTORY 2","US HISTORY II:1800-1900",SccdEnrollments$Title, ignore.case=T)
SccdEnrollments$Title<-gsub("IS-ENVIRONMNTL ISSUES/PR","IS-ENVIRON ISSUES/PROB I",SccdEnrollments$Title, ignore.case=T)
SccdEnrollments$Title<-gsub("LAW AND SOCIETY","LAW & SOCIETY",SccdEnrollments$Title, ignore.case=T)
SccdEnrollments$Title<-gsub("US HISTORY 1","US HIST 1: TO 1800",SccdEnrollments$Title, ignore.case=T)
SccdEnrollments$Title<-gsub("US HISTORY I: TO 1800","US HIST 1: TO 1800",SccdEnrollments$Title, ignore.case=T)
SccdEnrollments$Title<-gsub("US HIST I:TO 1800","US HIST 1: TO 1800",SccdEnrollments$Title, ignore.case=T)
SccdEnrollments$Title<-gsub("TODLRS","TODDLR",SccdEnrollments$Title, ignore.case=T)

SccdEnrollments$Instructor<-gsub(" $","",SccdEnrollments$Instructor, ignore.case=T,perl= T)
grep(" $",SccdEnrollments$Instructor, ignore.case=T)
SccdEnrollments$Instructor<-factor(SccdEnrollments$Instructor)


unique.instructors<-match(unique(SccdEnrollments$Instructor),SccdEnrollments$Instructor)

SccdEnrollments[unique.instructors,c(5,10)] #this looks promising for narrowing down the instructors.  eg, ending with a "."

##the following will put a space after the "&"
SccdEnrollments$Course.ID<-sub("\\&", "\\& ",SccdEnrollments$Course.ID)
##Follow this with a sub to change all two spaces into 1
SccdEnrollments$Course.ID<-gsub("  ", " ",SccdEnrollments$Course.ID)
SccdEnrollments$Course.ID<-gsub("HITEC232 01","HITEC 232 01",SccdEnrollments$Course.ID)
SccdEnrollments$Course.ID<-gsub("  ", " ",SccdEnrollments$Course.ID)
splitCourse<-strsplit(as.character(SccdEnrollments$Course.ID)," ")  #this splits Course.ID into parts

##the following is a check to see how many entries have more than just 3 parts (course, level, section)
#CoursePartLength<-1:nrow(SccdEnrollments)
#for (i in 1:nrow(SccdEnrollments)){
#  CoursePartLength[i]<-length(splitCourse[[i]])
#}

#table(CoursePartLength)

##Now make 3 new variables: Course (eg, psych), level (eg., 100), section






SccdEnrollments$credit.count<-equal.count(SccdEnrollments$CREDIT,4) #used in lattice
SccdEnrollments$enrolled.count<-equal.count(SccdEnrollments$Enrolled,4) #used in lattice
SccdEnrollments$classCap.count<-equal.count(SccdEnrollments$classCap,4) #used in lattice
SccdEnrollments$Wait.List.count<-equal.count(SccdEnrollments$Wait.List,4) #used in lattice
SccdEnrollments$FTEstate.count<-equal.count(SccdEnrollments$FTES.STATE,4) #used in lattice
SccdEnrollments$waitlist.ratio<-SccdEnrollments$Wait.List/max(SccdEnrollments$Wait.List) 

SccdEnrollments$credit.cut<-cut(SccdEnrollments$CREDIT,4) 
SccdEnrollments$enrolled.cut<-cut(SccdEnrollments$Enrolled,4) 
SccdEnrollments$classCap.cut<-cut(SccdEnrollments$classCap,4) 
SccdEnrollments$Wait.List.cut<-cut(SccdEnrollments$Wait.List,4) 
SccdEnrollments$FTEstate.cut<-cut(SccdEnrollments$FTES.STATE,4) 





df<-SccdEnrollments[SccdEnrollments$cancel=="live",]
nrow(df)
plot(df[,c(11:13,17)],pch=19,sub=df$Type,col=df$Type,cex=df$waitlist.ratio+1*1.5)
title.table<-table(df$Title,df$Type)
titles<-title.table[,2:4]
titles<-data.frame(titles)
grep.titles<-titles[(titles$Online>0 & titles$Typical>0)|(titles$Hybrid>0 & titles$Online>0),]
nrow(titles[(titles$Online>0 & titles$Typical>0)|(titles$Hybrid>0 & titles$Online>0),])
needed.titles<-row.names(titles[(titles$Online>0 & titles$Typical>0)|(titles$Hybrid>0 & titles$Online>0),])
#analysis.df<-df[grep(grep.titles,df$Title,ignore.case=T,value=F),]
#nrow(analysis.df)

mult.modal.title<-titles[(titles$Online>0 & titles$Typical>0)|(titles$Hybrid>0 & titles$Online>0),]
dup.title<-rownames(mult.modal.title)
dup.title.grep<-paste(dup.title, collapse="|")
dup.df<-SccdEnrollments[grep(dup.title.grep,SccdEnrollments$Title,ignore.case=T),]


names(SccdEnrollments)

num.matrix<-NULL
num.matrix<-SccdEnrollments[SccdEnrollments$cancel=="live",][,-18]#this will remove "cancel" as a variable
length(num.matrix)
for( i in c(1:4,6:length(num.matrix))){ 
  num.matrix[,i]<-as.numeric(num.matrix[,i])
  num.matrix[,5]<-as.numeric(as.factor(num.matrix[,5])) #this will work on the 1 char var
}
set.seed(1234)
trainSamples<-sample(1:nrow(num.matrix),nrow(num.matrix)/3,replace=F)#try dividing by 3 and assigning 1, 2 or 3
trainNum.matrix<- num.matrix[-trainSamples,]
testNum.matrix<-num.matrix[trainSamples,]

nrow(trainNum.matrix)
names(trainNum.matrix)
#plot(num.matrix,pch=19, col=num.matrix$Type)

hh<-hclust(dist(trainNum.matrix[trainNum.matrix$Campus==1,]))

dataMatrixOrdered <- trainNum.matrix[trainNum.matrix$Campus==1,][hh$order,]
myplclust <- function( hclust, lab=hclust$labels, lab.col=rep(1,length(hclust$labels)), hang=0.1,...){
  ## modifiction of plclust for plotting hclust objects *in colour*!
  ## Copyright Eva KF Chan 2009
  ## Arguments:
  ##    hclust:    hclust object
  ##    lab:        a character vector of labels of the leaves of the tree
  ##    lab.col:    colour for the labels; NA=default device foreground colour
  ##    hang:     as in hclust & plclust
  ## Side effect:
  ##    A display of hierarchical cluster with coloured leaf labels.
  y <- rep(hclust$height,2)
  x <- as.numeric(hclust$merge)
  y <- y[which(x<0)]
  x <- x[which(x<0)]
  x <- abs(x)
  y <- y[order(x)]
  x <- x[order(x)]
  plot( hclust, labels=FALSE, hang=hang, ... )
  text( x=x, y=y[hclust$order]-(max(hclust$height)*hang), labels=lab[hclust$order], col=lab.col[hclust$order], srt=90, adj=c(1,0.5), xpd=NA, ... )
}



myplclust(hh, lab.col=unique(trainNum.matrix$enrolled.cut) )

 svd1 <- svd(scale(trainNum.matrix[,-1]))

plot(rowMeans(dataMatrixOrdered),pch=19,col=trainNum.matrix$Type)
plot(rowMeans(dataMatrixOrdered),pch=19,col=unique(trainNum.matrix$enrolled.cut))

legend("topright",legend=unique(trainNum.matrix$enrolled.cut),col=unique(trainNum.matrix$enrolled.cut),pch=19)

plot(colMeans(dataMatrixOrdered))

names(dataMatrixOrdered)

plot(svd1$u[,23],pch=19, col=trainNum.matrix$enrolled.cut)


svd1$v[svd1$v[,1]>.99*max(abs(svd1$v[,1])),1]  #this lists the values of top 1% of right sing vectors in the first column

group<-which(svd1$v[,2]>.995*max(abs(svd1$v[,2]))) #indicates which contributors are above 99%

trial<-NULL
for(i in 1:length(nrow(trainNum.matrix))) {
  trial<-c(trial,which(svd1$v[,i]>.995*max(abs(svd1$v[,i]))))
}

trial2<-NULL
for(i in 1:length(nrow(trainNum.matrix))){
  trial2<-c(trial2,which.max(abs(svd1$v[,i])))
}
topMaxContrib<-unique(trial2)#which maximum contributor, not based on cutoff


topContrib<-unique(trial)  # after looping through the columns of SVD to find max contrib.  I believe it is these variables that have a lot of influence on the clusters.

table(trial,trial2,topContrib,topMaxContrib,useNA="ifany")

names(trainNum.matrix[topContrib])
names(trainNum.matrix[,group])

plot(trainDup.df[,c(11:13,17)],pch=19,sub=df$Type,col=df$Type,cex=df$waitlist.ratio+1*1.5)

plot(trainDup.df$Wait.List,trainDup.df$Enrolled, pch=19, cex=trainDup.df$classCap*.03,
     col=trainDup.df$Type,
     main="Training Set, Enrollved vs Wait List",
     xlab="Wait List",
     ylab="Enrolled",
     sub="classCap represented by size of point"
  )

legend("topright",legend=unique(trainDup.df$Type),col=unique(trainDup.df$Type), pch=19)

lma<-lm(trainDup.df$Enrolled[trainDup.df$Type=="Online"]~trainDup.df$Wait.List[trainDup.df$Type=="Online"])
abline(lma,col="green")

lmb<-lm(trainDup.df$Enrolled[trainDup.df$Type=="Typical"]~trainDup.df$Wait.List[trainDup.df$Type=="Typical"])
abline(lmb,col="blue")

lmc<-lm(trainDup.df$Enrolled[trainDup.df$Type=="Hybrid"]~trainDup.df$Wait.List[trainDup.df$Type=="Hybrid"])
abline(lmc,col="red")


FTEF.factor<-633.47/2678
detach(SccdEnrollments)
total.lm<-lm(SccdEnrollments$Enrolled~SccdEnrollments$classCap+SccdEnrollments$Type)
summary(total.lm)

coef(total.lm)[1]+coef(total.lm)[2]*30+coef(total.lm)[4] #1=intercept 2=cap,3=hybrid,4=online,5=typical

titled.lm<-lm(testDup.df$Enrolled~testDup.df$classCap+testDup.df$Type)
summary(titled.lm)

newdata <- data.frame(classCap=30,Type=1)
predict(titled.lm,newdata)

coef(titled.lm)[1]+coef(titled.lm)[2]*25+coef(titled.lm)[4] #1=intercept 2=cap,3=hybrid,4=online,5=typical
coef(titled.lm)[1]+coef(titled.lm)[2]*30+coef(titled.lm)[4]
plot(testDup.df$classCap,testDup.df$Enrolled, pch=19,col=testDup.df$Type,
     main="Enrolled by classCap", ylab="Enrolled",xlab="Class Cap")
abline(titled.lm,col="gray")

mean(dup.df$Enrolled[dup.df$Type=="Online"])
mean(SccdEnrollments$Enrolled[SccdEnrollments$Type+="Online"])
mean(SccdEnrollments$Enrolled)

qplot(dup.df$classCap,dup.df$Enrolled,col=dup.df$Type,data=dup.df,
      geom=c("point","smooth"),
      main="subsetted courses with Online pressence",
      xlab="Class Capacity", 
      ylab="Enrolled Students",
      method="lm",
      formula = dup.df$Enrolled ~ dup.df$classCap+dup.df$Type)+
  labs(colour="Class Venue")

ggplot(data=dup.df, 
       aes(dup.df$classCap,dup.df$Enrolled, 
           colour=dup.df$Type, 
           cex=dup.df$waitlist.ratio+1))+geom_point()

ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    opts(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "; Intercept =",signif(fit$coef[[1]],5 ),
                       "; Slope =",signif(fit$coef[[2]], 5),
                       "; P =",signif(summary(fit)$coef[2,4], 5)))
}

#After specifying this function, all you would have to run is:
  fit1 <- titled.lm
ggplotRegression(fit1)



cor(analysis.df[,11:14])

plot(analysis.df[11:14], pch=20, col=analysis.df$Type)
plot(analysis.df$classCap,analysis.df$Enrolled, pch=20, col=analysis.df$Type)
abline(lm(analysis.df$Enrolled~analysis.df$classCap*analysis.df$Type), col="black", lty=5)

boxplot(analysis.df$Enrolled~analysis.df$Type)

par(mfrow=c(2,2))
plot(analysis.df$Enrolled[analysis.df$Type=="Online"]~analysis.df$classCap[analysis.df$Type=="Online"], pch=19, cex=.5, col="blue")
abline(lm(analysis.df$Enrolled~analysis.df$classCap*analysis.df$Type), col="black", lty=5)
plot(analysis.df$Enrolled[analysis.df$Type=="Hybrid"]~analysis.df$classCap[analysis.df$Type=="Hybrid"], pch=19, cex=.5, col="green")
abline(lm(analysis.df$Enrolled~analysis.df$classCap*analysis.df$Type), col="black", lty=5)
plot(analysis.df$Enrolled[analysis.df$Type=="Typical"]~analysis.df$classCap[analysis.df$Type=="Typical"], pch=19, cex=.5, col="red")
abline(lm(analysis.df$Enrolled~analysis.df$classCap*analysis.df$Type), col="black", lty=5)

df[sample(nrow(df),15),c(11:15,17)]

set.seed(3435)
trainEnroll<-rbinom(nrow(df),1,.5)
table(trainEnroll)

trainDf<-df[trainEnroll==0,]
testDf<-df[trainEnroll==1,]


qplot(SccdEnrollments$Type, SccdEnrollments$Enrolled, geom="violin")

qplot(SccdEnrollments$classCap,SccdEnrollments$Enrolled, facets=SccdEnrollments$Enrolled~SccdEnrollments$classCap)





#t= transpose
# the following will calc means for the listed variables split by cancel
t(round(sapply(split.by.cancel,function(x) 
  colMeans(x[,c(
    #"CREDIT",
    "Enrolled",
    "classCap", 
    "Wait.List"
    #,
    #"FTES.TOTAL", 
    #"FTES.STATE"
  )])),3))

# the following will calc means for the listed variables split by Type
t(round(sapply(split.by.type,function(x) 
  colMeans(x[,c(
    #"CREDIT",
    "Enrolled",
    "classCap", 
    "Wait.List"
    #,
    #"FTES.TOTAL", 
    #"FTES.STATE"
  )])),3))
#the funciton in the preceding command is created just after.


# the following will calc means for the listed variables split by Campus and Type
t(round(sapply(split.by.campus.and.type,function(x) 
  colMeans(x[,c(
    #"CREDIT",
    "Enrolled",
    "classCap", 
    "Wait.List"
    #,
    #"FTES.TOTAL", 
    #"FTES.STATE"
          )])),3))
#the funciton in the preceding command is created in that line.

t(round(sapply(split.by.campus.type.cancel,function(x) 
  colMeans(x[,c(
    #"CREDIT",
    "Enrolled",
    "classCap", 
    "Wait.List"
    #,
    #"FTES.TOTAL", 
    #"FTES.STATE"
  )])),3))
#the funciton in the preceding command is created in that line.


plot(density(Wait.List))
plot(density(Wait.List[cancel=="live"]))

points(Enrolled[cancel=="live" & Campus=="North"], classCap[cancel=="live" & Campus=="North"], col="green")



# X<-data.frame(cbind(CREDIT,Enrolled,classCap,Wait.List,FTES.TOTAL))
 #splom(~X, col= rgb(0,0,0,0.2))
 histogram(~Enrolled|Type*classCap.cut)

#this one is interesting
densityplot(SccdEnrollments$FTES.State~SccdEnrollments$Enrolled|Type*classCap.cut, 
            data=SccdEnrollments,
            as.table = T, pch = 20, lwd=2, lty=1, col= rgb(0,0,0,0.2),
            panel = function(x, y, ...){
              panel.densityplot(x,  ...)
              #fit<-lm(y~x)
             # panel.abline(fit, lwd=2, col= "green")
             # panel.loess(x,y, lwd=2, col="red", lty=2)
              panel.abline(v=c(25,30,36),lty=5,col=2)
            } , xlab= "Enrolled", ylab="Density", 
            main="classCap by Enrolled"
            )



densityplot(~FTES.STATE|Type*enrolled.cut, type = c("count"))

xyplot(FTES.STATE~Enrolled|Type*classCap.cut,
       #layout=c(1,4), 
       as.table = T, pch = 20, col= rgb(0,0,0,0.2),
       panel = function(x, y, ...){
         panel.xyplot(x, y, ...)
         fit<-lm(y~x)
         panel.abline(fit, lwd=2, col= "green")
         panel.loess(x,y, lwd=2, col="red", lty=2)
         panel.abline(v=c(25,30,36),lty=5,col=1)
       } , xlab= "Enrolled", ylab="FTES.STATE", 
       main="FTES.STate by Enrolled" )
cloud(dup.df$Enrolled~dup.df$classCap*dup.df$Wait.List, data=dup.df, groups=dup.df$Type, auto.key=T, pch=20, col=c("red","blue","green","black")
      ,screen= list(
        z=40
        , x=-60
        #, y=90
       )
      )


cloud(Enrolled~classCap*Wait.List, data=SccdEnrollments, groups=Type, phi=40, auto.key=T, pch=20,
    cex=0.8, 
    screen=list(z = 30, x = -70, y = 0),
    scales=list(arrows=FALSE, cex=0.6, col="black", font=3, 
                tck=0.6, distance=1) ,
    panel=function(...) {
      L <- list(...)
      L$x <- L$y <- L$z <- c(0,1)
      L$type <- "l"
      L$col <- "black"
      L$lty <-3
      p <- panel.cloud(...)
      do.call(panel.cloud,L)
    })




data<-data.frame(SccdEnrollments$Enrolled[SccdEnrollments$cancel=="live"],
                 SccdEnrollments$classCap[SccdEnrollments$cancel=="live"], 
                 SccdEnrollments$Wait.List[SccdEnrollments$cancel=="live"],
                 SccdEnrollments$Type[SccdEnrollments$cancel=="live"],
                 SccdEnrollments$FTES.TOTAL[SccdEnrollments$cancel=="live"],
                 SccdEnrollments$FTES.STATE[SccdEnrollments$cancel=="live"],
                 SccdEnrollments$Quarter[SccdEnrollments$cancel=="live"],
                 SccdEnrollments$Campus[SccdEnrollments$cancel=="live"])
colnames(data)<-c("enroll", "cap", "waitlist", "type", "FTES.TOTAL", "FTES.STATE","quarter","campus")
data$type<-as.factor(data$type)
by(data[,c(-4,-7,-8)],data$type,cor)


data.split.by.campus.and.type<-split(data,list(data$campus,data$type))
data.split.by.type<-split(data,list(data$type))
data.split.by.campus.and.quarter<-split(data,list(data$campus,data$quarter))

t(round(sapply(data.split.by.campus.and.quarter,function(x) 
  colSums(x[,c(
    "enroll",
    #"cap",
    #"waitlist"
    #,
    "FTES.STATE"
  )])),3))
#the funciton in the preceding command is created in that line.



par(mfrow=c(2,2))
online.lm<-lm(data$enroll[data$type=="Online"]~data$cap[data$type=="Online"])
typical.lm<-lm(data$enroll[data$type=="Typical"]~data$cap[data$type=="Typical"])
hybrid.lm<-lm(data$enroll[data$type=="Hybrid"]~data$cap[data$type=="Hybrid"])
capEnroll.plot<-plot(data$enroll~data$cap,pch=20)
capEnroll.plot
abline(online.lm,col="red")
abline(typical.lm,col="blue")
abline(hybrid.lm,col="green")
legend("topleft", legend=c("Online","Typical","Hybrid"),
       col=c("red","blue",'green'),pch=19)
identify(capEnroll.plot)
par(mfrow=c(1,1))



A <- model.matrix(data$enroll ~ data$cap +data$type, data)
head(A)

Enrolled.lm<-lm(data$enroll~A)
new <- data.frame(x = sample(36,19006,replace=T))
prediction<-predict(Enrolled.lm,new,interval="prediction")
plot(prediction)
abline(22.5,0,col="blue")
Enrolled.lme<-lme(SccdEnrollments$Enrolled[cancel=="live"]~A)

FTES.State.lm<-lm(data$FTES.STATE[data$type!="Odd"]~data$cap[data$type!="Odd"]+data$type[data$type!="Odd"])

Enrolled.lm<-lm(SccdEnrollments$Enrolled~SccdEnrollments$classCap+SccdEnrollments$Type)
Enrolled.lm
Enrolled.lm.resid<-resid(Enrolled.lm)
plot(fitted(Enrolled.lm))
length(Enrolled.lm.resid)
plot(data$enroll, Enrolled.lm.resid)
abline(Enrolled.lm, col="yellow")

summary(Enrolled.lm, correlations=T)
by(SccdEnrollments,SccdEnrollments$cancel, function(x) summary(FTES.State.lm))

t(round(sapply(split.by.campus.type.cancel,function(x) 
  colMeans(x[,c(
    #"CREDIT",
    "Enrolled",
    "classCap", 
    "Wait.List"
    #,
    #"FTES.TOTAL", 
    #"FTES.STATE"
  )])),3))
#the funciton in the preceding command is created in that line.
par(mfrow=c(2,2))
plot(classCap[Type=="Odd"],Enrolled[Type=="Odd"])
abline(lm(Enrolled[Type=="Odd"]~classCap[Type=="Odd"]),col="2")
plot(classCap[Type=="Hybrid"],Enrolled[Type=="Hybrid"])
abline(lm(Enrolled[Type=="Hybrid"]~classCap[Type=="Hybrid"]),col="2")
plot(classCap[Type=="Online"],Enrolled[Type=="Online"])
abline(lm(Enrolled[Type=="Online"]~classCap[Type=="Online"]),col="2")
plot(classCap[Type=="Typical"],Enrolled[Type=="Typical"])
abline(lm(Enrolled[Type=="Typical"]~classCap[Type=="Typical"]),col="2")

library(car)
scatterplot.matrix(~Enrolled+classCap|Type, data=SccdEnrollments,
                   main="Class Modality")


sample.50<-SccdEnrollments[sample(nrow(SccdEnrollments), 50),c(11,13,14,18,19)]
lm(sample.50$Enrolled~sample.50$classCap*sample.50$Type)


#Student faculty ratio
Fac.Stu.ratio<-data.frame(t(round(sapply(data.split.by.campus.and.quarter,function(x) 
  colSums(x[,c(
    "enroll",
    #"cap",
    #"waitlist"
    #,
    "FTES.STATE"
  )])),3)))

data.frame(Fac.Stu.ratio,Fac.Stu.ratio$FTES.STATE*FTEF.factor)
           





set.seed(3435)
trainEnrollments = rbinom(nrow(data), size = 1, prob = 0.5)
table(trainEnrollments)
plot(log10(trainEnrollments[,1:4 ] + 1))


data$venuetype = as.numeric(data$type) - 1
costFunction = function(x, y) {
  sum(x != (y > 0.5))
}
cvError = rep(NA, 8)
library(boot)
for (i in 1:8) {
  lmFormula = as.formula(paste("venuetype~", names(data)[i], sep = ""))
  glmFit = glm(lmFormula, family = "binomial", data = data)
  cvError[i] = cv.glm(data, glmFit, costFunction, 2)$delta[2]
}



hCluster = hclust(dist(t(trainEnrollments[,1:8])))
plot(hCluster)

kmeanData<-train.df[,11:14]
kmeansObj <- kmeans(kmeanData,centers=7)

par(mar=rep(0.2,4))
plot(kmeanData[,1:2],col=kmeansObj$cluster,pch=19,cex=.5)
points(kmeansObj$centers,col=1:7,pch=3,cex=3,lwd=3)



df<-x
online.fill.ratio<-x$Enrolled[x$classCap>0 & x$Type=="Online"]/x$classCap[x$classCap>0 & x$Type=="Online"]
Typical.fill.ratio<-x$Enrolled[x$classCap>0 & x$Type=="Typical"]/x$classCap[x$classCap>0 & x$Type=="Typical"]
table(1>online.fill.ratio)
table(1>Typical.fill.ratio)

table(.95>online.fill.ratio)

quantile(online.fill.ratio)
quantile(Typical.fill.ratio)
boxplot(online.fill.ratio)
range(online.fill.ratio)

mean(x$Enrolled[x$Type=="Online"])
mean(x$classCap[x$Type=="Online"])
18.25/22
addmargins(table(x$Type))

boxplot(online.fill.ratio,Typical.fill.ratio, col=c('yellow','blue'),main="Course Fill Ratios\n Online vs Typical",xlab="quanitles")
legend("topleft", legend=c("Online Courses",'Typical Courses','median'), col=c("yellow","blue",'black'),pch=19, cex=1.1)

x<-df
enrolled.lm<-lm(x$Enrolled~x$classCap+x$Type)
est.enrol<-function(bo=enrolled.lm$coef[1], b1=enrolled.lm$coef[2], cap, type=enrolled.lm$coef[4]){
  bo + (b1 * cap) + type
}

est.fte<-function(bo=ftes.lm$coef[1], b1=ftes.lm$coef[2], enrolled){
  bo +  b1 * enrolled 
}
  
  
  
mean(x$classCap[x$Type=="Online"])
est.enrol(cap=22)
#(Intercept) 
19.97257 
est.enrol(cap=28)
#(Intercept) 
21.69442 
est.fte(enrolled=20)
#(Intercept) 
5.191192 
est.fte(enrolled=21.6)
#(Intercept) 
5.570607 
est.enrol(cap=30)
#(Intercept) 
22.84233 
est.fte(enrolled=23)
#(Intercept) 
5.902595 est
