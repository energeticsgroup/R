

# youtube vid on how to install chrome driver
# https://www.google.com/search?q=where+do+I+save+my+chrome+driver&rlz=1C1CHZL_enUS751US751&oq=where+do+I+save+my+chrome+driver+&aqs=chrome..69i57j33.143464j0j7&sourceid=chrome&ie=UTF-8#kpvalbx=1

library(RSelenium) # usefule for scraping data that is dynamically updated..
library(rJava) # must make sure that you have the developer version of JAVA installed


#######################################################################################################################################

####################################################### Haynesville Shale data ############################################################
getwd()
#setwd("C:/webdriver")

## binman::list_versions("chromedriver")

D <- rsDriver(
  browser = c("chrome"),
  version = "latest",
  chromever = "75.0.3770.90", # Must use this version of chrome driver for some reason
  verbose = TRUE,
  check = TRUE
)

remDr <- D[["client"]]
remDr$navigate("http://sonlite.dnr.state.la.us/sundown/cart_prod/cart_con_wellinfo2?p_wsn=240191") ### change this
library(XML)

library(dplyr)
library(tidyr)

Well.Data <- function(well_data_URL){
  
  well_data_URL
  
  #api number
  webElems1 <- remDr$findElements(using = 'css selector', "table:nth-child(4) :nth-child(12)")
  api <- unlist(lapply(webElems1, function(x){x$getElementText()}))
  
  #serial number
  webElems1 <- remDr$findElements(using = 'css selector', "td:nth-child(1) a , table:nth-child(4) th:nth-child(1)")
  serial <- unlist(lapply(webElems1, function(x){x$getElementText()}))
  
  #org id number
  webElems1 <- remDr$findElements(using = 'css selector', "b+ table td:nth-child(4) a , table:nth-child(4) th:nth-child(4)")
  well_data1 <- unlist(lapply(webElems1, function(x){x$getElementText()}))
  
  #well number
  webElems1 <- remDr$findElements(using = 'css selector', "table:nth-child(4) :nth-child(3)")
  well_data2 <- unlist(lapply(webElems1, function(x){x$getElementText()}))
  
  #effective date
  webElems1 <- remDr$findElements(using = 'css selector', "table:nth-child(4) :nth-child(11)")
  well_data3 <- unlist(lapply(webElems1, function(x){x$getElementText()}))
  
  #sec
  webElems1 <- remDr$findElements(using = 'css selector', "table:nth-child(4) :nth-child(8)")
  well_data4 <- unlist(lapply(webElems1, function(x){x$getElementText()}))
  
  #twn
  webElems1 <- remDr$findElements(using = 'css selector', "table:nth-child(4) :nth-child(9)")
  well_data5 <- unlist(lapply(webElems1, function(x){x$getElementText()}))
  
  #rge
  webElems1 <- remDr$findElements(using = 'css selector', "table:nth-child(4) :nth-child(10)")
  well_data6 <- unlist(lapply(webElems1, function(x){x$getElementText()}))
  
  #prod type
  webElems1 <- remDr$findElements(using = 'css selector', "table:nth-child(4) :nth-child(7)")
  well_data7 <- unlist(lapply(webElems1, function(x){x$getElementText()}))
  
  #well name
  webElems1 <- remDr$findElements(using = 'css selector', "table:nth-child(4) :nth-child(2)")
  well_data8 <- unlist(lapply(webElems1, function(x){x$getElementText()}))
  
  
  well_data <- as.data.frame(rbind(api, serial, well_data1,well_data2,well_data3,well_data4,well_data5,well_data6,well_data7,c(well_data8[[1]],well_data8[[3]])))
  rownames(well_data) <- c()
  
  return(well_data)
}


## create a for loop that changes the URL based on the well serial number

getwd()
setwd("C:/Users/justi/OneDrive/00 Energeteics/0.Acc/Tellurian")

haynesville_shale_data <- read.csv(file="haynesville_shale.csv", header=TRUE, sep=",")

## loop by organization then by well

unique_operators <- haynesville_shale_data%>%
  dplyr::distinct(`Organization.Id`)

## there appear to be 19 operators

## put each of their well data into a list... a list of lists if you will
# ex:

Tellurian <- haynesville_shale_data%>%
  filter(Organization.Id == "F135")

i <- 1
Tellurian_wells <- list()
for (i in 1:length(Tellurian$Api.Num)){

  Tellurian_wells[[i]]<-Well.Data(remDr$navigate(paste("http://sonlite.dnr.state.la.us/sundown/cart_prod/cart_con_wellinfo2?p_wsn=", Tellurian[i,3], sep = "")))

  }


# Now write one that iterates over the list of operators and creates a list of list
# store by org id values

i<-1
operaters <- list()
op_wells <- list()
ops_well_data <- list()

final_data <-list()

for(i in 1:length(unique_operators$Organization.Id)){
  
  operaters[[i]]<-unique_operators[[i,1]]
  
  op_wells[[i]] <- haynesville_shale_data%>%
    filter(Organization.Id == paste(unique_operators[[i,1]]))%>%
    select(Well.Serial.Num)
  
  test <- merge(operaters[[i]], op_wells[[i]])
  j <- 1
  ops_well_data <- list()
  for (j in 1:length(test$Well.Serial.Num)){
    
    ops_well_data[[j]]<-Well.Data(remDr$navigate(paste("http://sonlite.dnr.state.la.us/sundown/cart_prod/cart_con_wellinfo2?p_wsn=", test$Well.Serial.Num[j], sep = "")))
    
    Sys.sleep(5)
    
  }
  
 final_data <- append(final_data, ops_well_data)
  
}

# Once you have all of the original well information..
  # parse that data by company.. then loop through each well operated by that company for further data associated

# https://www.upstreamdb.com/louisiana/operators
op_names <- c("Comstock Oil & Gas","Chesapeake Operating","Indigo Minerals","Goodrich Petroleum","Aethon Energy Operating","Covey Park Gas","Exco Operating","Matador Production","BHP Biliton Petro (KCS Resources)","Gep Haynesville","BHP Biliton Petro (Txla Operating)","BMR","XTO","Ensight","Vine Oil & Gas","BHP Billiton (WSF Operating)","Tellurian","Franks Operating Company","Petro-Chem Operating")
Unique_Operators_Names <-cbind(unique_operators, op_names)


final_data[[1]][2,2]

Operator_List_Filter <- function(org_id){

i <- 1
wd <- list()

for(i in 1:length(final_data)){
  x <- list()
  if (final_data[[i]][3,2] == paste(org_id)){
    
    x[[1]] <- final_data[[i]]
    
    wd<-append(wd, x)
    
    }
  }
return(wd)
}

PetroChem <- Operator_List_Filter("P289")

## now loop through each operator to build dope data base of Haynesville shale operators


Production.Data <- function(well_data_URL){
  
  well_data_URL
  
  #gas production
  webElems1 <- remDr$findElements(using = 'css selector', "table:nth-child(25) td:nth-child(8)")
  gas_production <- unlist(lapply(webElems1, function(x){x$getElementText()}))
  
  #rpt date
  webElems1 <- remDr$findElements(using = 'css selector', "table:nth-child(25) td:nth-child(1)")
  rpt_date <- unlist(lapply(webElems1, function(x){x$getElementText()}))
  
  production_table <- as.data.frame(cbind(rpt_date, gas_production))
  
  colnames(production_table) <- c("Report Date", "Gas Production")

  return(production_table)
}

Production_list_builder <- function(Operator_well_data){
  i<-1
  pd <- list()
  for (i in 1:length(Operator_well_data)){
    p <- list()
    p[[1]]<-Production.Data(remDr$navigate(paste("http://sonlite.dnr.state.la.us/sundown/cart_prod/cart_con_wellinfo2?p_wsn=",
                                                 paste(as.character(Operator_well_data[[i]][2,2])),
                                                 sep = "")))
    p[[1]]$Serial_Number <- c(paste(Operator_well_data[[i]][2,2]))
    
    pd<-append(pd, p)
  }
  return(pd)
}
  
PetroChem <- Production_list_builder(PetroChem)

# aggregate the number of wells & cumulative production by building a data frame that has (company name, wellserial-number, cum well production) 

i <- 1
cum_prod_data <- data.frame()

for(i in 1:length(PetroChem)){
  
  Company <- "Petro Chem"
  Well <- unique(as.character(PetroChem[[i]]$Serial_Number))
  Cumulative_Production<-sum(as.numeric(as.character(PetroChem[[i]]$`Gas Production`)))
  
  x<-as.data.frame(cbind(Company, Well, Cumulative_Production))
  
  cum_prod_data<-rbind(cum_prod_data,x)
  
}
  
D[["server"]]$stop() 



binman::list_versions("chromedriver")
