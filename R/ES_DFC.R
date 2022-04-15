#####Ecosystem Service daily fish catch for the Tonle Sap lake####

#' Calculate the Ecosystem Service for daily fish catch using Method 2 and a single threshold this will give a single number across multiple CFi's.
#'
#' @param x input file
#' @param DFC_thresh Daily catch threshold weight
#' @param S_Date Start date for the assessment period
#' @param E_Date End date for the assessment period
#' @rawNamespace import(dplyr, except = c(
#' last,
#' combine,
#' first,
#' failwith,
#' id,
#' summarize,
#' count,
#' desc,
#' mutate,
#' arrange,
#' rename,
#' summarise))
#' @import
#' plyr
#' gdata
#' reshape2
#' @examples
#' ES_DFC(DailyCatch, 1.5, '2015-01-01', '2021-01-08')

#' @export

## Ecosystem service for daily fish catch on the Tonle Sap Lake above a single threshold, yes, 1; no, 0.
## DailyCatch<-read.csv("D:\\Rdata\\FHI\\Daily_fish_catch.csv", header=TRUE)
## attach(DailyCatch)


ES_DFC<- function (x, DFC_thresh, S_Date, E_Date){
  x$DFC_thresh <- DFC_thresh
  DFC_F1_a<-plyr::ddply(x,.(CatchDate),mutate,
                        CatchDate = lubridate::ymd(CatchDate))%>%
    plyr::ddply(.(CatchDate), filter,(CatchDate >= as.Date(S_Date) & CatchDate <= as.Date(E_Date)))



  DFC_F1_1<-plyr::ddply(DFC_F1_a,.(WeightCaught),transform,
                        WeightCaught_1 = ifelse(WeightCaught < DFC_thresh,0,1))%>%
    ##calculate F1 values
    plyr::ddply(.(Station), plyr::summarise,
                WeightCaught_0 = sum(WeightCaught_1=="0",na.rm=T), WeightCaught_1 = sum(WeightCaught_1=="1",na.rm=T))

  ##calculate individual site values for F1 and F2, an F1 of 1 means a failure
  DFC_F1_2<-plyr::ddply(DFC_F1_1,.(WeightCaught_0), transform, DFC_F1 = ifelse(WeightCaught_0==0,0,1))%>%

    ##summary F1 values
    ## melt data prior to analysis if you get an error object 'Station' not found then you need to attach the names to the input data file
    reshape2::melt(id.vars=c("Station"))

  ## number of occurrances which exceeded threshold value this needs to be its own variable for later use
  DFC_F1_3<-plyr::ddply(DFC_F1_2,. (variable), summarise, Sum_var = sum(value))

  ##determine number of sites
  DFC_sites_1 <- plyr::ddply(DFC_F1_2, .(Station), summarise, Numb = length(unique(Station)))

  DFC_sites_2 <- plyr::count(DFC_sites_1$Numb)

  ##Final calculations of F1 values
  DFC_F1_3$freq<-DFC_sites_2$freq
  DFC_F1_4 <- plyr::ddply(DFC_F1_3,. (variable, Sum_var), transform,
                            F1 = (Sum_var/(freq)*100),
                            ESI_F1 = 100-((Sum_var/(freq))*100)
  )
  DFC_F1_4<-DFC_F1_4[,-3]


  ##F1 values for difference in site
  F1_output <- subset(DFC_F1_4, select = c("variable", "F1", "ESI_F1"), variable == c("DFC_F1"))


  ############################################################################################################
  ## CALCULATE F2##
  ##calculate total number of instances monitored for each site
  DFC_F2_1 <- plyr::ddply(DFC_F1_1,. (Station, WeightCaught_0, WeightCaught_1), transform,
                            WeightCaught_T = WeightCaught_0 + WeightCaught_1, Factor = "F2")%>%
    subset(., select = c("WeightCaught_0", "WeightCaught_T", "Factor"))

  DFC_F2_2 <-  plyr::ddply(DFC_F2_1, .(Factor), colwise(sum))


  ##Calculate F2 values
  F2_output <- plyr::ddply(DFC_F2_2,. (WeightCaught_0, WeightCaught_T), transform, WeightCaught_F2 = ((WeightCaught_0/WeightCaught_T)*100))%>%
    ## melt data prior to analysis
    reshape2::melt(., id.vars=c("Factor"))%>%
    subset(., select = c("variable", "value"), variable == c("WeightCaught_F2"))


  ###########################################################################################################
  ## calculate ENI for F1 AND F2
  ## combine the F1 and F2 output and then clean it up
  F2_output <-rename(F2_output, c("variable"="var"))
  F1_F2<-cbind(F1_output,F2_output)%>%
    rename(., c("value"="F2"))%>%
    plyr::ddply(.,.(variable), transform, Parameter=ifelse(variable=="DFC_F1","WeightCaught","WeightCaught"))%>%
    subset(., select = c("Parameter","F1", "F2"))

  ## calculate ENI for F1 and F2 for Method 2 this isn't used for water quality
  ## ESI_F1_F2 <- plyr::ddply(F1_F2,. (F1,F2), transform,
  ##                 F2_ESI = 100-(sqrt(F1*F2)))

  ##calculate ENI for F1 and F2 variation on M1 also not used
  ## ESI_F1_F2 <- plyr::ddply(F1_F2,. (F1,F2), transform,
  ##                         F2_ESI = 100-sqrt(((F1^2+F2^2)/2)))



  ########################################################################################
  ##calculate F3
  DFC_F3_1<-plyr::ddply(DFC_F1_a,.(WeightCaught),transform,
                          DFC_Exc = ifelse(WeightCaught < DFC_thresh,(DFC_thresh/WeightCaught)-1,NA),
                          Factor = "F3")%>%

    ##sum of all excursions
    subset(., select = c("DFC_Exc", "Factor"))%>%
    plyr::ddply(., .(Factor), colwise(sum), na.rm=T)
  ## total number of instances


  ##combine sum of excursions and instances
  DFC_F3_2<-subset(DFC_F2_2, select = c("Factor", "WeightCaught_T"))%>%
    rename(., c("Factor"="Fact"))

  DFC_F3_3<-cbind(DFC_F3_1,DFC_F3_2)%>%
    subset(., select = c("Factor", "DFC_Exc", "WeightCaught_T"))%>%
    plyr::ddply(.,. (DFC_Exc), transform, DFC_nse = DFC_Exc/WeightCaught_T)%>%
    plyr::ddply(., . (DFC_nse), transform, DFC_F3 = ((DFC_nse)/(DFC_nse+1))*100)%>%
    ## summarise F3 calculations
    subset(., select = c("Factor", "DFC_F3"))%>%
    ## calculate ENI for F3
    reshape2::melt(., id.vars=c("Factor"))

  ## combine the F1, F2 and F3 output and then clean it up
  F1_F2_F3<-cbind(F1_F2, DFC_F3_3)%>%
    rename(., c("value"="F3"))%>%
    subset(., select = c("Parameter","F1", "F2", "F3"))

  ## calculate ENI for F1 and F2
  ##Method 1 ESI not used
  ## ESI_F1_F2_F3 <- plyr::ddply(F1_F2_F3,. (F1,F2,F3), transform,
  ##                        ESI_F3 = 100-sqrt(((F1^2+F2^2+F3^2)/3))
  ##                      )
  ##Method 2 ESI
  ESI_F1_F3_DFC <- plyr::ddply(F1_F2_F3,. (F1,F3), transform,
                              ESI_F1_3 = 100-(sqrt(F1*F3)))

  return(ESI_F1_F3_DFC)
}

#######################################################################################
#######################################################################################

#' Calculate the Ecosystem Service for daily fish catch using Method 2 and monthly thresholds across multiple CFi's
#'
#' @param x input file
#' @param Jan_t Daily catch threshold weight for January
#' @param Feb_t Daily catch threshold weight for February
#' @param Mar_t Daily catch threshold weight for March
#' @param Apr_t Daily catch threshold weight for April
#' @param May_t Daily catch threshold weight for May
#' @param Jun_t Daily catch threshold weight for June
#' @param Jul_t Daily catch threshold weight for July
#' @param Aug_t Daily catch threshold weight for August
#' @param Sep_t Daily catch threshold weight for September
#' @param Oct_t Daily catch threshold weight for October
#' @param Nov_t Daily catch threshold weight for November
#' @param Dec_t Daily catch threshold weight for December
#' @param S_Date Start date for assessment period
#' @param E_Date End date for assessment period
#' @import
#' plyr
#' gdata
#' lubridate
#' reshape2
#' @rawNamespace import(dplyr, except = c(
#' last,
#' combine,
#' first,
#' failwith,
#' id,
#' summarize,
#' count,
#' desc,
#' mutate,
#' arrange,
#' rename,
#' summarise))
#' @examples
#' ES_DFC_M(DailyCatch, 50, 50, 20, 20, 10, 50, 50, 50, 50, 50, 50, 50, '2021-01-01', '2021-12-31')

#' @export

## Ecosystem service for daily fish catch on the Tonle Sap Lake above a single threshold, yes, 1; no, 0.
## DailyCatch<-read.csv("D:\\Rdata\\FHI\\Daily_fish_catch.csv", header=TRUE)
## attach(DailyCatch)
##Market minimum monthly catch weight (15, 9, 8, 4, 4, 5, 5, 5, 9, 9, 10, 10)
##Best case monthly catch weight (50, 50, 20, 20, 10, 50, 50, 50, 50, 50, 50, 50)

ES_DFC_M<- function (x, Jan_t, Feb_t, Mar_t, Apr_t, May_t, Jun_t, Jul_t, Aug_t, Sep_t,  Oct_t, Nov_t, Dec_t, S_Date, E_Date){
  x$Jan_t <- Jan_t
  x$Feb_t <- Feb_t
  x$Mar_t <- Mar_t
  x$Apr_t <- Apr_t
  x$May_t <- May_t
  x$Jun_t <- Jun_t
  x$Jul_t <- Jul_t
  x$Aug_t <- Aug_t
  x$Sep_t <- Sep_t
  x$Oct_t <- Oct_t
  x$Nov_t <- Nov_t
  x$Dec_t <- Dec_t

  ##Generate a column of months from the date column
  DFC_F1_a<-plyr::ddply(x,.(CatchDate),mutate,
                        CatchDate = lubridate::ymd(CatchDate))%>%
    plyr::ddply(.(CatchDate), filter,(CatchDate >= as.Date(S_Date) & CatchDate <= as.Date(E_Date)))%>%
    plyr::ddply(.(CatchDate),transform,
                        Catchmonth = lubridate::month(CatchDate, label = FALSE))



  DFC_F1_1<- plyr::ddply(DFC_F1_a,.(Catchmonth, WeightCaught), mutate,
                                WeightCaught_1 = dplyr::case_when(
                        (Catchmonth = 1 & WeightCaught < Jan_t) ~0,
                        (Catchmonth = 2 & WeightCaught < Feb_t) ~0,
                        (Catchmonth = 3 & WeightCaught < Mar_t) ~0,
                        (Catchmonth = 4 & WeightCaught < Apr_t) ~0,
                        (Catchmonth = 5 & WeightCaught < May_t) ~0,
                        (Catchmonth = 6 & WeightCaught < Jun_t) ~0,
                        (Catchmonth = 7 & WeightCaught < Jul_t) ~0,
                        (Catchmonth = 8 & WeightCaught < Aug_t) ~0,
                        (Catchmonth = 9 & WeightCaught < Sep_t) ~0,
                        (Catchmonth = 10 & WeightCaught < Oct_t) ~0,
                        (Catchmonth = 11 & WeightCaught < Nov_t) ~0,
                        (Catchmonth = 12 & WeightCaught < Dec_t) ~0,
                        TRUE ~ 1
              )
                                                )%>%
    ##calculate F1 values
    plyr::ddply(.(Station), plyr::summarise,
                WeightCaught_0 = sum(WeightCaught_1=="0",na.rm=T), WeightCaught_1 = sum(WeightCaught_1=="1",na.rm=T))

  ##calculate individual site values for F1 and F2, an F1 of 1 means a failure
  DFC_F1_2<-plyr::ddply(DFC_F1_1,.(WeightCaught_0), transform, DFC_F1 = ifelse(WeightCaught_0==0,0,1))%>%

    ##summary F1 values
    ## melt data prior to analysis if you get an error object 'Station' not found then you need to attach the names to the input data file
    reshape2::melt(id.vars=c("Station"))

  ## number of occurrances which exceeded threshold value this needs to be its own variable for later use
  DFC_F1_3<-plyr::ddply(DFC_F1_2,. (variable), summarise, Sum_var = sum(value))

  ##determine number of sites
  DFC_sites_1 <- plyr::ddply(DFC_F1_2, .(Station), summarise, Numb = length(unique(Station)))

  DFC_sites_2 <- plyr::count(DFC_sites_1$Numb)

  ##Final calculations of F1 values
  DFC_F1_3$freq<-DFC_sites_2$freq
  DFC_F1_4 <- plyr::ddply(DFC_F1_3,. (variable, Sum_var), transform,
                          F1 = (Sum_var/(freq)*100),
                          ESI_F1 = 100-((Sum_var/(freq))*100)
  )
  DFC_F1_4<-DFC_F1_4[,-3]


  ##F1 values for difference in site
  F1_output <- subset(DFC_F1_4, select = c("variable", "F1", "ESI_F1"), variable == c("DFC_F1"))


  ############################################################################################################
  ## CALCULATE F2##
  ##calculate total number of instances monitored for each site
  DFC_F2_1 <- plyr::ddply(DFC_F1_1,. (Station, WeightCaught_0, WeightCaught_1), transform,
                          WeightCaught_T = WeightCaught_0 + WeightCaught_1, Factor = "F2")%>%
    subset(., select = c("WeightCaught_0", "WeightCaught_T", "Factor"))

  DFC_F2_2 <-  plyr::ddply(DFC_F2_1, .(Factor), colwise(sum))

  ##Calculate F2 values
  F2_output <- plyr::ddply(DFC_F2_2,. (WeightCaught_0, WeightCaught_T), transform, WeightCaught_F2 = ((WeightCaught_0/WeightCaught_T)*100))%>%
    ## melt data prior to analysis
    reshape2::melt(., id.vars=c("Factor"))%>%
    subset(., select = c("variable", "value"), variable == c("WeightCaught_F2"))

  ###########################################################################################################
  ## calculate ENI for F1 AND F2
  ## combine the F1 and F2 output and then clean it up
  F2_output <-rename(F2_output, c("variable"="var"))
  F1_F2<-cbind(F1_output,F2_output)%>%
    rename(., c("value"="F2"))%>%
    plyr::ddply(.,.(variable), transform, Parameter=ifelse(variable=="DFC_F1","WeightCaught","WeightCaught"))%>%
    subset(., select = c("Parameter","F1", "F2"))

  ## calculate ENI for F1 and F2 for Method 2 this isn't used for water quality
  ## ESI_F1_F2 <- plyr::ddply(F1_F2,. (F1,F2), transform,
  ##                 F2_ESI = 100-(sqrt(F1*F2)))

  ##calculate ENI for F1 and F2 variation on M1 also not used
  ## ESI_F1_F2 <- plyr::ddply(F1_F2,. (F1,F2), transform,
  ##                         F2_ESI = 100-sqrt(((F1^2+F2^2)/2)))



  ########################################################################################
  ##calculate F3

  DFC_F3_1<- plyr::ddply(DFC_F1_a,.(Catchmonth, WeightCaught), mutate,
                           DFC_Ex = dplyr::case_when(
                             (Catchmonth = 1 & WeightCaught < Jan_t) ~ (Jan_t/WeightCaught)-1,
                             (Catchmonth = 2 & WeightCaught < Feb_t) ~ (Feb_t/WeightCaught)-1,
                             (Catchmonth = 3 & WeightCaught < Mar_t) ~ (Mar_t/WeightCaught)-1,
                             (Catchmonth = 4 & WeightCaught < Apr_t) ~ (Apr_t/WeightCaught)-1,
                             (Catchmonth = 5 & WeightCaught < May_t) ~ (May_t/WeightCaught)-1,
                             (Catchmonth = 6 & WeightCaught < Jun_t) ~ (Jun_t/WeightCaught)-1,
                             (Catchmonth = 7 & WeightCaught < Jul_t) ~ (Jul_t/WeightCaught)-1,
                             (Catchmonth = 8 & WeightCaught < Aug_t) ~ (Aug_t/WeightCaught)-1,
                             (Catchmonth = 9 & WeightCaught < Sep_t) ~ (Sep_t/WeightCaught)-1,
                             (Catchmonth = 10 & WeightCaught < Oct_t) ~ (Oct_t/WeightCaught)-1,
                             (Catchmonth = 11 & WeightCaught < Nov_t) ~ (Nov_t/WeightCaught)-1,
                             (Catchmonth = 12 & WeightCaught < Dec_t) ~ (Dec_t/WeightCaught)-1,
                             TRUE ~ -9999),
                            Factor = "F3",
                         DFC_Exc= dplyr::na_if(DFC_Ex, -9999)

    ) %>%


    ##sum of all excursions
    subset(., select = c("DFC_Exc", "Factor"))%>%
    plyr::ddply(., .(Factor), colwise(sum), na.rm=T)
  ## total number of instances

  ##combine sum of excursions and instances
  DFC_F3_2<-subset(DFC_F2_2, select = c("Factor", "WeightCaught_T"))%>%
    rename(., c("Factor"="Fact"))

  DFC_F3_3<-cbind(DFC_F3_1,DFC_F3_2)%>%
    subset(., select = c("Factor", "DFC_Exc", "WeightCaught_T"))%>%
    plyr::ddply(.,. (DFC_Exc), transform, DFC_nse = (DFC_Exc/WeightCaught_T))%>%
    plyr::ddply(., . (DFC_nse), transform, DFC_F3 = ((DFC_nse)/(DFC_nse+1))*100)%>%
    ## summarise F3 calculations
    subset(., select = c("Factor", "DFC_F3"))%>%
    ## calculate ENI for F3
    reshape2::melt(., id.vars=c("Factor"))

  ## combine the F1, F2 and F3 output and then clean it up
  F1_F2_F3<-cbind(F1_F2, DFC_F3_3)%>%
    rename(., c("value"="F3"))%>%
    subset(., select = c("Parameter","F1", "F2", "F3"))

  ## calculate ENI for F1 and F2
  ##Method 1 ESI not used
  ## ESI_F1_F2_F3 <- plyr::ddply(F1_F2_F3,. (F1,F2,F3), transform,
  ##                        ESI_F3 = 100-sqrt(((F1^2+F2^2+F3^2)/3))
  ##                      )
  ##Method 2 ESI
  ESI_F1_F3_DFC <- plyr::ddply(F1_F2_F3,. (F1,F3), transform,
                               ESI_F1_3 = 100-(sqrt(F1*F3)))

  return(ESI_F1_F3_DFC)
}

#########################################################################################################################################################################
#########################################################################################################################################################################

#' Calculate the Ecosystem Service for daily fish catch using Method 2 and a single threshold this will give a single number for a single CFi.
#'
#' @param x input file
#' @param CFi Community fishery for which the metric will be calculated, this is listed under Station in the input file
#' @param DFC_thresh Daily catch threshold weight
#' @param S_Date Start date for assessment period
#' @param E_Date End date for assessment period
#' @import
#' plyr
#' gdata
#' reshape2
#' @rawNamespace import(dplyr, except = c(
#' last,
#' combine,
#' first,
#' failwith,
#' id,
#' summarize,
#' count,
#' desc,
#' mutate,
#' arrange,
#' rename,
#' summarise))
#' @examples
#' ES_DFC_S(DailyCatch, "Balat", 1.5, '2018-01-01', '2021-08-31')

#' @export

## Ecosystem service for daily fish catch on the Tonle Sap Lake above a single threshold, yes, 1; no, 0.
## DailyCatch<-read.csv("D:\\Rdata\\FHI\\Daily_fish_catch.csv", header=TRUE)
## attach(DailyCatch)


ES_DFC_S<- function (x, CFi, DFC_thresh, S_Date, E_Date){
  x$DFC_thresh <- DFC_thresh
 ##
  DFC_F1_a<-plyr::ddply(x,.(Station),filter,(Station == CFi))%>%
    plyr::ddply(.(CatchDate),mutate,
                        CatchDate = lubridate::ymd(CatchDate))%>%
    plyr::ddply(.(CatchDate), filter,(CatchDate >= as.Date(S_Date) & CatchDate <= as.Date(E_Date)))

  DFC_F1_1<-plyr::ddply(DFC_F1_a,.(WeightCaught),transform,
                        WeightCaught_1 = ifelse(WeightCaught < DFC_thresh,0,1))%>%
    ##calculate F1 values
    plyr::ddply(.(Location), plyr::summarise,
                WeightCaught_0 = sum(WeightCaught_1=="0",na.rm=T), WeightCaught_1 = sum(WeightCaught_1=="1",na.rm=T))

  ##calculate individual site values for F1 and F2, an F1 of 1 means a failure
  DFC_F1_2<-plyr::ddply(DFC_F1_1,.(WeightCaught_0), transform, DFC_F1 = ifelse(WeightCaught_0==0,0,1))%>%

    ##summary F1 values
    ## melt data prior to analysis if you get an error object 'Location' not found then you need to attach the names to the input data file
    reshape2::melt(id.vars=c("Location"))

  ## number of occurrances which exceeded threshold value this needs to be its own variable for later use
  DFC_F1_3<-plyr::ddply(DFC_F1_2,. (variable), summarise, Sum_var = sum(value))

  ##determine number of sites
  DFC_sites_1 <- plyr::ddply(DFC_F1_2, .(Location), summarise, Numb = length(unique(Location)))

  DFC_sites_2 <- plyr::count(DFC_sites_1$Numb)

  ##Final calculations of F1 values
  DFC_F1_3$freq<-DFC_sites_2$freq
  DFC_F1_4 <- plyr::ddply(DFC_F1_3,. (variable, Sum_var), transform,
                          F1 = (Sum_var/(freq)*100),
                          ESI_F1 = 100-((Sum_var/(freq))*100)
  )
  DFC_F1_4<-DFC_F1_4[,-3]


  ##F1 values for difference in site
  F1_output <- subset(DFC_F1_4, select = c("variable", "F1", "ESI_F1"), variable == c("DFC_F1"))


  ############################################################################################################
  ## CALCULATE F2##
  ##calculate total number of instances monitored for each site
  DFC_F2_1 <- plyr::ddply(DFC_F1_1,. (Location, WeightCaught_0, WeightCaught_1), transform,
                          WeightCaught_T = WeightCaught_0 + WeightCaught_1, Factor = "F2")%>%
    subset(., select = c("WeightCaught_0", "WeightCaught_T", "Factor"))

  DFC_F2_2 <-  plyr::ddply(DFC_F2_1, .(Factor), colwise(sum))


  ##Calculate F2 values
  F2_output <- plyr::ddply(DFC_F2_2,. (WeightCaught_0, WeightCaught_T), transform, WeightCaught_F2 = ((WeightCaught_0/WeightCaught_T)*100))%>%
    ## melt data prior to analysis
    reshape2::melt(., id.vars=c("Factor"))%>%
    subset(., select = c("variable", "value"), variable == c("WeightCaught_F2"))


  ###########################################################################################################
  ## calculate ENI for F1 AND F2
  ## combine the F1 and F2 output and then clean it up
  F2_output <-rename(F2_output, c("variable"="var"))
  F1_F2<-cbind(F1_output,F2_output)%>%
    rename(., c("value"="F2"))%>%
    plyr::ddply(.,.(variable), transform, Parameter=ifelse(variable=="DFC_F1","WeightCaught","WeightCaught"))%>%
    subset(., select = c("Parameter","F1", "F2"))




  ########################################################################################
  ##calculate F3
  DFC_F3_1<-plyr::ddply(DFC_F1_a,.(WeightCaught),transform,
                        DFC_Exc = ifelse(WeightCaught < DFC_thresh,(DFC_thresh/WeightCaught)-1,NA),
                        Factor = "F3") %>%


    ##sum of all excursions
    subset(., select = c("DFC_Exc", "Factor"))%>%
    plyr::ddply(., .(Factor), colwise(sum), na.rm=T)
  ## total number of instances


  ##combine sum of excursions and instances
  DFC_F3_2<-subset(DFC_F2_2, select = c("Factor", "WeightCaught_T"))%>%
    rename(., c("Factor"="Fact"))

  DFC_F3_3<-cbind(DFC_F3_1,DFC_F3_2)%>%
    subset(., select = c("Factor", "DFC_Exc", "WeightCaught_T"))%>%
    plyr::ddply(.,. (DFC_Exc), transform, DFC_nse = DFC_Exc/WeightCaught_T)%>%
    plyr::ddply(., . (DFC_nse), transform, DFC_F3 = ((DFC_nse)/(DFC_nse+1))*100)%>%
    ## summarise F3 calculations
    subset(., select = c("Factor", "DFC_F3"))%>%
    ## calculate ENI for F3
    reshape2::melt(., id.vars=c("Factor"))

  ## combine the F1, F2 and F3 output and then clean it up
  F1_F2_F3<-cbind(F1_F2, DFC_F3_3)%>%
    rename(., c("value"="F3"))%>%
    subset(., select = c("Parameter","F1", "F2", "F3"))


  ##Method 2 ESI
  ESI_F1_F3_DFC <- plyr::ddply(F1_F2_F3,. (F1,F3), transform,
                               ESI_F1_3 = 100-(sqrt(F1*F3)))

  return(ESI_F1_F3_DFC)
}

#######################################################################################
#######################################################################################

#' Calculate the Ecosystem Service for daily fish catch using Method 2 and monthly thresholds for a single CFi \ Station
#'
#' @param x input file
#' @param CFi Community fishery for which the metric will be calculated, this is listed under Station in the input file
#' @param Jan_t Daily catch threshold weight for January
#' @param Feb_t Daily catch threshold weight for February
#' @param Mar_t Daily catch threshold weight for March
#' @param Apr_t Daily catch threshold weight for April
#' @param May_t Daily catch threshold weight for May
#' @param Jun_t Daily catch threshold weight for June
#' @param Jul_t Daily catch threshold weight for July
#' @param Aug_t Daily catch threshold weight for August
#' @param Sep_t Daily catch threshold weight for September
#' @param Oct_t Daily catch threshold weight for October
#' @param Nov_t Daily catch threshold weight for November
#' @param Dec_t Daily catch threshold weight for December
#' @param S_Date Start date for assessment period
#' @param E_Date End date for assessment period
#' @import
#' gdata
#' lubridate
#' plyr
#' reshape2
#' @rawNamespace import(dplyr, except = c(
#' last,
#' combine,
#' first,
#' failwith,
#' id,
#' summarize,
#' count,
#' desc,
#' mutate,
#' arrange,
#' rename,
#' summarise))
#' @examples
#' ES_DFC_SM(DailyCatch, "Balat", 50, 50, 20, 20, 10, 50, 50,
#' 50, 50, 50, 50, 50, '2021-01-01', '2021-12-31')

#' @export

## Ecosystem service for daily fish catch on the Tonle Sap Lake above a single threshold, yes, 1; no, 0.
## DailyCatch<-read.csv("D:\\Rdata\\FHI\\Daily_fish_catch.csv", header=TRUE)
## attach(DailyCatch)
##Market minimum monthly catch weight (15, 9, 8, 4, 4, 5, 5, 5, 9, 9, 10, 10)
##Best case monthly catch weight (50, 50, 20, 20, 10, 50, 50, 50, 50, 50, 50, 50)

ES_DFC_SM<- function (x, CFi, Jan_t, Feb_t, Mar_t, Apr_t, May_t, Jun_t, Jul_t, Aug_t, Sep_t,  Oct_t, Nov_t, Dec_t, S_Date, E_Date){
  x$Jan_t <- Jan_t
  x$Feb_t <- Feb_t
  x$Mar_t <- Mar_t
  x$Apr_t <- Apr_t
  x$May_t <- May_t
  x$Jun_t <- Jun_t
  x$Jul_t <- Jul_t
  x$Aug_t <- Aug_t
  x$Sep_t <- Sep_t
  x$Oct_t <- Oct_t
  x$Nov_t <- Nov_t
  x$Dec_t <- Dec_t

  ##Generate a column of months from the date column
  DFC_F1_a<-plyr::ddply(x,.(Station),filter,(Station == CFi))%>%
    plyr::ddply(.(CatchDate),mutate,
                        CatchDate = lubridate::ymd(CatchDate))%>%
    plyr::ddply(.(CatchDate), filter,(CatchDate >= as.Date(S_Date) & CatchDate <= as.Date(E_Date)))%>%
    plyr::ddply(.(CatchDate),transform,
                Catchmonth = lubridate::month(CatchDate, label = FALSE))



  DFC_F1_1<- plyr::ddply(DFC_F1_a,.(Catchmonth, WeightCaught), mutate,
                         WeightCaught_1 = dplyr::case_when(
                           (Catchmonth = 1 & WeightCaught < Jan_t) ~0,
                           (Catchmonth = 2 & WeightCaught < Feb_t) ~0,
                           (Catchmonth = 3 & WeightCaught < Mar_t) ~0,
                           (Catchmonth = 4 & WeightCaught < Apr_t) ~0,
                           (Catchmonth = 5 & WeightCaught < May_t) ~0,
                           (Catchmonth = 6 & WeightCaught < Jun_t) ~0,
                           (Catchmonth = 7 & WeightCaught < Jul_t) ~0,
                           (Catchmonth = 8 & WeightCaught < Aug_t) ~0,
                           (Catchmonth = 9 & WeightCaught < Sep_t) ~0,
                           (Catchmonth = 10 & WeightCaught < Oct_t) ~0,
                           (Catchmonth = 11 & WeightCaught < Nov_t) ~0,
                           (Catchmonth = 12 & WeightCaught < Dec_t) ~0,
                           TRUE ~ 1
                         )
  )%>%
    ##calculate F1 values
    plyr::ddply(.(Location), plyr::summarise,
                WeightCaught_0 = sum(WeightCaught_1=="0",na.rm=T), WeightCaught_1 = sum(WeightCaught_1=="1",na.rm=T))

  ##calculate individual site values for F1 and F2, an F1 of 1 means a failure
  DFC_F1_2<-plyr::ddply(DFC_F1_1,.(WeightCaught_0), transform, DFC_F1 = ifelse(WeightCaught_0==0,0,1))%>%

    ##summary F1 values
    ## melt data prior to analysis if you get an error object 'Location' not found then you need to attach the names to the input data file
    reshape2::melt(id.vars=c("Location"))

  ## number of occurrances which exceeded threshold value this needs to be its own variable for later use
  DFC_F1_3<-plyr::ddply(DFC_F1_2,. (variable), summarise, Sum_var = sum(value))

  ##determine number of sites
  DFC_sites_1 <- plyr::ddply(DFC_F1_2, .(Location), summarise, Numb = length(unique(Location)))

  DFC_sites_2 <- plyr::count(DFC_sites_1$Numb)

  ##Final calculations of F1 values
  DFC_F1_3$freq<-DFC_sites_2$freq
  DFC_F1_4 <- plyr::ddply(DFC_F1_3,. (variable, Sum_var), transform,
                          F1 = (Sum_var/(freq)*100),
                          ESI_F1 = 100-((Sum_var/(freq))*100)
  )
  DFC_F1_4<-DFC_F1_4[,-3]


  ##F1 values for difference in site
  F1_output <- subset(DFC_F1_4, select = c("variable", "F1", "ESI_F1"), variable == c("DFC_F1"))


  ############################################################################################################
  ## CALCULATE F2##
  ##calculate total number of instances monitored for each site
  DFC_F2_1 <- plyr::ddply(DFC_F1_1,. (Location, WeightCaught_0, WeightCaught_1), transform,
                          WeightCaught_T = WeightCaught_0 + WeightCaught_1, Factor = "F2")%>%
    subset(., select = c("WeightCaught_0", "WeightCaught_T", "Factor"))

  DFC_F2_2 <-  plyr::ddply(DFC_F2_1, .(Factor), colwise(sum))

  ##Calculate F2 values
  F2_output <- plyr::ddply(DFC_F2_2,. (WeightCaught_0, WeightCaught_T), transform, WeightCaught_F2 = ((WeightCaught_0/WeightCaught_T)*100))%>%
    ## melt data prior to analysis
    reshape2::melt(., id.vars=c("Factor"))%>%
    subset(., select = c("variable", "value"), variable == c("WeightCaught_F2"))

  ###########################################################################################################
  ## calculate ENI for F1 AND F2
  ## combine the F1 and F2 output and then clean it up
  F2_output <-rename(F2_output, c("variable"="var"))
  F1_F2<-cbind(F1_output,F2_output)%>%
    rename(., c("value"="F2"))%>%
    plyr::ddply(.,.(variable), transform, Parameter=ifelse(variable=="DFC_F1","WeightCaught","WeightCaught"))%>%
    subset(., select = c("Parameter","F1", "F2"))

  ## calculate ENI for F1 and F2 for Method 2 this isn't used for water quality
  ## ESI_F1_F2 <- plyr::ddply(F1_F2,. (F1,F2), transform,
  ##                 F2_ESI = 100-(sqrt(F1*F2)))

  ##calculate ENI for F1 and F2 variation on M1 also not used
  ## ESI_F1_F2 <- plyr::ddply(F1_F2,. (F1,F2), transform,
  ##                         F2_ESI = 100-sqrt(((F1^2+F2^2)/2)))



  ########################################################################################
  ##calculate F3
  ##  DFC_F3_1<-plyr::ddply(x,.(WeightCaught),transform,
  ##                      DFC_Exc = ifelse(WeightCaught < DFC_thresh,(WeightCaught/DFC_thresh),NA),
  ##                    Factor = "F3")%>%


  DFC_F3_1<- plyr::ddply(DFC_F1_a,.(Catchmonth, WeightCaught), mutate,
                         DFC_Ex = dplyr::case_when(
                           (Catchmonth = 1 & WeightCaught < Jan_t) ~ (Jan_t/WeightCaught)-1,
                           (Catchmonth = 2 & WeightCaught < Feb_t) ~ (Feb_t/WeightCaught)-1,
                           (Catchmonth = 3 & WeightCaught < Mar_t) ~ (Mar_t/WeightCaught)-1,
                           (Catchmonth = 4 & WeightCaught < Apr_t) ~ (Apr_t/WeightCaught)-1,
                           (Catchmonth = 5 & WeightCaught < May_t) ~ (May_t/WeightCaught)-1,
                           (Catchmonth = 6 & WeightCaught < Jun_t) ~ (Jun_t/WeightCaught)-1,
                           (Catchmonth = 7 & WeightCaught < Jul_t) ~ (Jul_t/WeightCaught)-1,
                           (Catchmonth = 8 & WeightCaught < Aug_t) ~ (Aug_t/WeightCaught)-1,
                           (Catchmonth = 9 & WeightCaught < Sep_t) ~ (Sep_t/WeightCaught)-1,
                           (Catchmonth = 10 & WeightCaught < Oct_t) ~ (Oct_t/WeightCaught)-1,
                           (Catchmonth = 11 & WeightCaught < Nov_t) ~ (Nov_t/WeightCaught)-1,
                           (Catchmonth = 12 & WeightCaught < Dec_t) ~ (Dec_t/WeightCaught)-1,
                           TRUE ~ -9999),
                         Factor = "F3",
                         DFC_Exc= dplyr::na_if(DFC_Ex, -9999)

  ) %>%


    ##sum of all excursions
    subset(., select = c("DFC_Exc", "Factor"))%>%
    plyr::ddply(., .(Factor), colwise(sum), na.rm=T)
  ## total number of instances

  ##combine sum of excursions and instances
  DFC_F3_2<-subset(DFC_F2_2, select = c("Factor", "WeightCaught_T"))%>%
    rename(., c("Factor"="Fact"))

  DFC_F3_3<-cbind(DFC_F3_1,DFC_F3_2)%>%
    subset(., select = c("Factor", "DFC_Exc", "WeightCaught_T"))%>%
    plyr::ddply(.,. (DFC_Exc), transform, DFC_nse = (DFC_Exc/WeightCaught_T))%>%
    plyr::ddply(., . (DFC_nse), transform, DFC_F3 = ((DFC_nse)/(DFC_nse+1))*100)%>%
    ## summarise F3 calculations
    subset(., select = c("Factor", "DFC_F3"))%>%
    ## calculate ENI for F3
    reshape2::melt(., id.vars=c("Factor"))

  ## combine the F1, F2 and F3 output and then clean it up
  F1_F2_F3<-cbind(F1_F2, DFC_F3_3)%>%
    rename(., c("value"="F3"))%>%
    subset(., select = c("Parameter","F1", "F2", "F3"))

  ## calculate ENI for F1 and F2
  ##Method 1 ESI not used
  ## ESI_F1_F2_F3 <- plyr::ddply(F1_F2_F3,. (F1,F2,F3), transform,
  ##                        ESI_F3 = 100-sqrt(((F1^2+F2^2+F3^2)/3))
  ##                      )
  ##Method 2 ESI
  ESI_F1_F3_DFC <- plyr::ddply(F1_F2_F3,. (F1,F3), transform,
                               ESI_F1_3 = 100-(sqrt(F1*F3)))

  return(ESI_F1_F3_DFC)
}

