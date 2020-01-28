

## R Selenium Web Scrape

# install google extensions "selector gadget" to choose what elements you want to scrape

# parse data with regular expressions

# (optional) if searching for hyperlinks.. loops through and scrape those too!

# youtube vid on how to install chrome driver
# https://www.google.com/search?q=where+do+I+save+my+chrome+driver&rlz=1C1CHZL_enUS751US751&oq=where+do+I+save+my+chrome+driver+&aqs=chrome..69i57j33.143464j0j7&sourceid=chrome&ie=UTF-8#kpvalbx=1

library(RSelenium) # usefule for scraping data that is dynamically updated..
library(rJava) # must make sure that you have the developer version of JAVA installed

getwd()
setwd("C:/webdriver")

binman::list_versions("chromedriver")

D <- rsDriver(
  browser = c("chrome"),
  version = "latest",
  #chromever = "75.0.3770.90", # Must use this version of chrome driver for some reason
  chromever = "79.0.3945.36",
  verbose = TRUE,
  check = TRUE
)

#D[["server"]]$stop() 

remDr <- D[["client"]]
remDr$navigate("https://matchmaking.grip.events/distributechconnect/event-login") ### change this
library(XML)

library(dplyr)
library(tidyr)

  
  #names
  webElems1 <- remDr$findElements(using = 'css selector', ".link-is-positive")
  attendee_names <- unlist(lapply(webElems1, function(x){x$getElementText()}))
  
  
  #jobs
  webElems1 <- remDr$findElements(using = 'css selector', "p.ng-star-inserted")
  attendee_jobs <- unlist(lapply(webElems1, function(x){x$getElementText()}))
  
  # Jobs
  
Jobs <- gsub(" at.*$", "",attendee_jobs)
Jobs<-as.data.frame(Jobs)

companies <- gsub(".*? at ", "",attendee_jobs)
companies <- as.data.frame(companies)

df<-cbind(Jobs, companies)

df$Jobs<-as.character(df$Jobs)
df$companies<-as.character(df$companies)  

toDelete <- seq(0, length(df$Jobs),2)

jobs_companies<-df[-toDelete,]
  

  
  ## Names
  
  a<-as.data.frame(attendee_names)
  
  toDelete <- seq(1, length(a$attendee_names),2)
  
  b<-a[-toDelete,]
  
  names <- as.data.frame(b)
  

  # Join Data Frames
  jobs_companies<-jobs_companies[-1,]
  
  ScrapedData<-cbind(names,jobs_companies)
  
  
  # write Data
  
  library(readxl)
  
  getwd()
  setwd("C:/Users/justi/OneDrive/2020/Darcy")
  
  write.csv(ScrapedData, "ScrapedData.csv")
  
  #########################################################
  
  D[["server"]]$stop()
