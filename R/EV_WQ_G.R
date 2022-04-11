#####Generic Ecosystem Vitality Water Quality Indicators calculated using Method 2####

#' Calculate the Ecosystem Vitality Water Quality Indicator for a parameter within lower and upper bounds using Method 2
#'
#' @param x input file
#' @param parameter column name of water quality parameter to be assessed
#' @param WQ_low lower water quality threshold value
#' @param WQ_high upper water quality threshold value
#' @import
#' plyr
#' dplyr
#' gdata
#' reshape2
#' @examples
#' EV_WQ(Wq3S_Cam, "TOTN_mgL", 0, 1.6)

#' @export

##Water quality parameter within  WQ index values, yes, 1; no, 0.

##I took another approach by first calculating parameter_1 first without ifelse and then defining the relevant water quality
#column using the grouping available in the input data (i.e., River) or any other categorical column (Name, Ccode)
#I have passed WQ_low, WQ_high, and parameter in a small function so no need to append WQ_low and WQ_high to the input database

#WQ refers to Low and High as this function will calculate a water quality parameter which has both low and high thresholds

EV_WQ<- function (x, parameter, WQ_low, WQ_high){
   Wq_WQ_F1_1<-  plyr::ddply(x, ~Name, function(d)
                                                    {
                                                      parameter_1 <- dplyr::case_when (
                                                                                  d[[parameter]] < WQ_low ~ 0,
                                                                                  between(d[[parameter]],WQ_low,WQ_high) ~ 1,
                                                                                  d[[parameter]] > WQ_high ~ 0,
                                                                                  TRUE ~ 999
                                                                                )
                                                      data.frame(d,parameter_1)
                                                                                })%>%

    #calculate F1 values
    plyr::ddply(~Name, transform, WQ_0 = sum(parameter_1==0), WQ_1 = sum(parameter_1==1)) %>%

    ##I added the following to tidy the previous output up as the next phase only needs single set of values for WQ_0 and WQ_1, using the minimum is crude but seems to work.
    plyr::ddply(.(Name), plyr::summarise,
                 WQ_0 = min(WQ_0), WQ_1 = min(WQ_1))

  ##calculate individual site values for F1 and F2, an F1 of 1 means a failure
  Wq_WQ_F1_2<-plyr::ddply(Wq_WQ_F1_1,.(WQ_0), transform, WQ_F1 = ifelse(WQ_0==0,0,1))%>%

    ##summary F1 values
    ## melt data prior to analysis if you get an error object 'Name' not found then you need to attach the names to the input data file
    reshape2::melt(id.vars=c("Name"))

  ## number of occurrences which exceeded threshold value this needs to be its own variable for later use
  Wq_WQ_F1_3<-plyr::ddply(Wq_WQ_F1_2,. (variable), summarise, Sum_var = sum(value))

  ##determine number of sites
  Wq_WQ_sites_1 <- plyr::ddply(Wq_WQ_F1_2, .(Name), summarise, Numb = length(unique(Name)))

  Wq_WQ_sites_2 <- plyr::count(Wq_WQ_sites_1$Numb)

  ##Final calculations of F1 values
  Wq_WQ_F1_3$freq<-Wq_WQ_sites_2$freq
  Wq_WQ_F1_4 <- plyr::ddply(Wq_WQ_F1_3,. (variable, Sum_var), transform,
                            F1 = (Sum_var/(freq)*100),
                            ESI_F1 = 100-((Sum_var/(freq))*100)
  )
  Wq_WQ_F1_4<-Wq_WQ_F1_4[,-3]


  ##F1 values for difference in site
  F1_output <- subset(Wq_WQ_F1_4, select = c("variable", "F1", "ESI_F1"), variable == c("WQ_F1"))


  ############################################################################################################
  ## CALCULATE F2##
  ##calculate total number of instances monitored for each site
  Wq_WQ_F2_1 <- plyr::ddply(Wq_WQ_F1_1,. (Name, WQ_0, WQ_1), transform,
                            WQ_T = WQ_0 + WQ_1, Factor = "F2")%>%
    subset(., select = c("WQ_0", "WQ_T", "Factor"))

  Wq_WQ_F2_2 <-  plyr::ddply(Wq_WQ_F2_1, .(Factor), colwise(sum))

  ##Calculate F2 values
  F2_output <- plyr::ddply(Wq_WQ_F2_2,. (WQ_0, WQ_T), transform, WQ_F2 = ((WQ_0/WQ_T)*100))%>%
    ## melt data prior to analysis
    reshape2::melt(., id.vars=c("Factor"))%>%
    subset(., select = c("variable", "value"), variable == c("WQ_F2"))

  ###########################################################################################################
  ## calculate ENI for F1 AND F2
  ## combine the F1 and F2 output and then clean it up
  F2_output <-rename(F2_output, c("variable"="var"))
  F1_F2<-cbind(F1_output,F2_output)%>%
    rename(., c("value"="F2"))%>%
    plyr::ddply(.,.(variable), transform, Parameter=ifelse(variable=="WQ_F1","WQ","WQ"))%>%
    subset(., select = c("Parameter","F1", "F2"))


  ## calculate ENI for F1 and F2 for Method 2 this isnt used for water quality
  ## ESI_F1_F2 <- plyr::ddply(F1_F2,. (F1,F2), transform,
  ##                 F2_ESI = 100-(sqrt(F1*F2)))

  ##calculate ENI for F1 and F2 variation on M1 also not used
  ## ESI_F1_F2 <- plyr::ddply(F1_F2,. (F1,F2), transform,
  ##                         F2_ESI = 100-sqrt(((F1^2+F2^2)/2)))

  ########################################################################################
  ##calculate F3

  ##Works up to here this was the code specific for a singel WQ parameter - in this case TOTN_mgL

  #Wq_TN_F3_1<-plyr::ddply(x,.(TOTN_mgL),transform,
  #                        TN_Exc = ifelse(TOTN_mgL < TN_low,(TN_low/TOTN_mgL)-1,
  #                                        ifelse(TOTN_mgL > TN_high,(TOTN_mgL/TN_high)-1,NA)),
  #                        Factor = "F3")%>%
  Wq_WQ_F3_1<-plyr::ddply(x, ~Name, function(d)
                                                        {
                                                            WQ_Exc <- dplyr::case_when (
                                                                                  d[[parameter]] < WQ_low ~ (WQ_low/d[[parameter]])-1,
                                                                                  d[[parameter]] > WQ_high ~ (d[[parameter]]/WQ_high)-1,
                                                                                  TRUE ~ 0
                                                                                )
                                                                      data.frame(d,Factor = "F3",WQ_Exc)
                                                          }

                                                              ) %>%

   # return(Wq_WQ_F3_1)}

    ##sum of all excursions
    subset(., select = c("WQ_Exc", "Factor"))%>%
    plyr::ddply(., .(Factor), colwise(sum), na.rm=T)
  ## total number of instances

  ##combine sum of excursions and instances
  Wq_WQ_F3_2<-subset(Wq_WQ_F2_2, select = c("Factor", "WQ_T"))%>%
    rename(., c("Factor"="Fact"))

  Wq_WQ_F3_3<-cbind(Wq_WQ_F3_1,Wq_WQ_F3_2)%>%
    subset(., select = c("Factor", "WQ_Exc", "WQ_T"))%>%
    plyr::ddply(.,. (WQ_Exc), transform, WQ_nse = WQ_Exc/WQ_T)%>%
    plyr::ddply(., . (WQ_nse), transform, WQ_F3 = ((WQ_nse)/(WQ_nse+1))*100)%>%
    ## summarise F3 calculations
    subset(., select = c("Factor", "WQ_F3"))%>%
    ## calculate ENI for F3
    reshape2::melt(., id.vars=c("Factor"))

  ## combine the F1, F2 and F3 output and then clean it up
  F1_F2_F3<-cbind(F1_F2, Wq_WQ_F3_3)%>%
    rename(., c("value"="F3"))%>%
    subset(., select = c("Parameter","F1", "F2", "F3"))

  ## calculate ENI for F1 and F2
  ##Method 1 ESI not used
  ## ESI_F1_F2_F3 <- plyr::ddply(F1_F2_F3,. (F1,F2,F3), transform,
  ##                        ESI_F3 = 100-sqrt(((F1^2+F2^2+F3^2)/3))
  ##                      )
  ##Method 2 ESI
  ESI_F1_F3_WQ <- plyr::ddply(F1_F2_F3,. (F1,F3), transform,
                              ESI_F1_3 = 100-(sqrt(F1*F3)))

  return(ESI_F1_F3_WQ)
}
######################################################################################################
#' Calculate the Ecosystem Vitality Water Quality Indicator for parameters with undefined thresholds using Method 2
#'
#' @param x input file
#' @param parameter column name of water quality parameter to be assessed
#' @param cutoff Threshold year. This is the final year of the pre assessment period, thus if your impact period starts in 2012, enter 2011
#' @import
#' plyr
#' dplyr
#' gdata
#' reshape2
#' @examples
#' EV_WQU(Wq3S_Cam, "TOTP_mgL", 2008)

#' @export

##calcualte threshold values for WQU data from before a cut off date. x=data, parameter = water quality parameter, cutoff= threshold year, the last year before the assessment period
EV_WQU<- function (x, parameter, cutoff){
  ##subset to parameter only
  WQU <- subset(x, select = c("Name", "Year", "Month", parameter))

  ##subset of WQU prior and equal to the cut-off year
  WQU_prior <- subset(WQU, Year <= cutoff)

 ##Mean with 99% confidence intervals not used
  WQU_prior_summary<-Summary_stats(WQU_prior, measurevar=parameter, groupvars=c("Month"), na.rm=T)


  ## monthly max and min values used instead of confidence intervals
  WQU_prior_min_max <- plyr::ddply(WQU_prior_summary,. (parameter, min, max), transform,
                                   low_ci=min,
                                   high_ci=max
                        )


  WQU_prior_min_max$Month <- factor(WQU_prior_min_max$Month, levels = month.abb)


  ##sort by month
  WQU_prior_min_max <-WQU_prior_min_max [order(WQU_prior_min_max$Month), ]

  WQU_thresholds <- subset(WQU_prior_min_max, select = c("Month", "low_ci", "high_ci"))
  WQU_thresholds$Month <- factor(WQU_thresholds$Month, levels = month.abb)
  ##sort by month
  WQU_thresholds <-WQU_thresholds [order(WQU_thresholds$Month), ]

  ## WQU_thresholds_1 <- WQU_thresholds[,-1]


  ## rownames(WQU_thresholds_1) <- WQU_thresholds[,1]

  Jan_thres <- filter(WQU_thresholds, Month == "Jan")%>%
    .[,-1]
  names (Jan_thres)[1] <- "Jan_low_ci"
  names (Jan_thres)[2] <- "Jan_high_ci"

  Feb_thres <- filter(WQU_thresholds, Month == "Feb")%>%
    .[,-1]
  names (Feb_thres)[1] <- "Feb_low_ci"
  names (Feb_thres)[2] <- "Feb_high_ci"

  Mar_thres <- filter(WQU_thresholds, Month == "Mar")%>%
    .[,-1]
  names (Mar_thres)[1] <- "Mar_low_ci"
  names (Mar_thres)[2] <- "Mar_high_ci"

  Apr_thres <- filter(WQU_thresholds, Month == "Apr")%>%
    .[,-1]
  names (Apr_thres)[1] <- "Apr_low_ci"
  names (Apr_thres)[2] <- "Apr_high_ci"

  May_thres <- filter(WQU_thresholds, Month == "May")%>%
    .[,-1]
  names (May_thres)[1] <- "May_low_ci"
  names (May_thres)[2] <- "May_high_ci"

  Jun_thres <- filter(WQU_thresholds, Month == "Jun")%>%
    .[,-1]
  names (Jun_thres)[1] <- "Jun_low_ci"
  names (Jun_thres)[2] <- "Jun_high_ci"

  Jul_thres <- filter(WQU_thresholds, Month == "Jul")%>%
    .[,-1]
  names (Jul_thres)[1] <- "Jul_low_ci"
  names (Jul_thres)[2] <- "Jul_high_ci"

  Aug_thres <- filter(WQU_thresholds, Month == "Aug")%>%
    .[,-1]
  names (Aug_thres)[1] <- "Aug_low_ci"
  names (Aug_thres)[2] <- "Aug_high_ci"

  Sep_thres <- filter(WQU_thresholds, Month == "Sep")%>%
    .[,-1]
  names (Sep_thres)[1] <- "Sep_low_ci"
  names (Sep_thres)[2] <- "Sep_high_ci"

  Oct_thres <- filter(WQU_thresholds, Month == "Oct")%>%
    .[,-1]
  names (Oct_thres)[1] <- "Oct_low_ci"
  names (Oct_thres)[2] <- "Oct_high_ci"

  Nov_thres <- filter(WQU_thresholds, Month == "Nov")%>%
    .[,-1]
  names (Nov_thres)[1] <- "Nov_low_ci"
  names (Nov_thres)[2] <- "Nov_high_ci"

  Dec_thres <- filter(WQU_thresholds, Month == "Dec")%>%
    .[,-1]
  names (Dec_thres)[1] <- "Dec_low_ci"
  names (Dec_thres)[2] <- "Dec_high_ci"

  WQU_thresholds_1 <- merge(Jan_thres, Feb_thres)%>%
    merge(.,Mar_thres)%>%
    merge(.,Apr_thres)%>%
    merge(.,May_thres)%>%
    merge(.,Jun_thres)%>%
    merge(.,Jul_thres)%>%
    merge(.,Aug_thres)%>%
    merge(.,Sep_thres)%>%
    merge(.,Oct_thres)%>%
    merge(.,Nov_thres)%>%
    merge(.,Dec_thres)


  ##subset of post period WQU data
  WQU_post <- subset(WQU, Year > cutoff)%>%
    reshape2::dcast(., Name + Year~Month,  value.var = parameter)%>%
    cbind(., WQU_thresholds_1)


  ##WQ within WQ index values, yes,1; no,0.
  WQU_thresh<-plyr::ddply(WQU_post,.(Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec, Jan_low_ci, Jan_high_ci,
                                     Feb_low_ci, Feb_high_ci, Mar_low_ci, Mar_high_ci, Apr_low_ci, Apr_high_ci,
                                     May_low_ci, May_high_ci, Jun_low_ci, Jun_high_ci, Jul_low_ci, Jul_high_ci,
                                     Aug_low_ci, Aug_high_ci, Sep_low_ci, Sep_high_ci, Oct_low_ci, Oct_high_ci,
                                     Nov_low_ci, Nov_high_ci, Dec_low_ci, Dec_high_ci),transform,
                          WQU_Jan = ifelse(Jan <= Jan_low_ci,0,
                                           ifelse(Jan >= Jan_high_ci,0,1)),
                          WQU_Feb = ifelse(Feb <= Feb_low_ci,0,
                                           ifelse(Feb >= Feb_high_ci,0,1)),
                          WQU_Mar = ifelse(Mar <= Mar_low_ci,0,
                                           ifelse(Mar >= Mar_high_ci,0,1)),
                          WQU_Apr = ifelse(Apr <= Apr_low_ci,0,
                                           ifelse(Apr >= Apr_high_ci,0,1)),
                          WQU_May = ifelse(May <= May_low_ci,0,
                                           ifelse(May >= May_high_ci,0,1)),
                          WQU_Jun = ifelse(Jun <= Jun_low_ci,0,
                                           ifelse(Jun >= Jun_high_ci,0,1)),
                          WQU_Jul = ifelse(Jul <= Jul_low_ci,0,
                                           ifelse(Jul >= Jul_high_ci,0,1)),
                          WQU_Aug = ifelse(Aug <= Aug_low_ci,0,
                                           ifelse(Aug >= Aug_high_ci,0,1)),
                          WQU_Sep = ifelse(Sep <= Sep_low_ci,0,
                                           ifelse(Sep >= Sep_high_ci,0,1)),
                          WQU_Oct = ifelse(Oct <= Oct_low_ci,0,
                                           ifelse(Oct >= Oct_high_ci,0,1)),
                          WQU_Nov = ifelse(Nov <= Nov_low_ci,0,
                                           ifelse(Nov >= Nov_high_ci,0,1)),
                          WQU_Dec = ifelse(Dec <= Dec_low_ci,0,
                                           ifelse(Dec >= Dec_high_ci,0,1))
  )%>%
    subset(., select = c("Name", "WQU_Jan", "WQU_Feb", "WQU_Mar", "WQU_Apr", "WQU_May", "WQU_Jun",
                         "WQU_Jul", "WQU_Aug", "WQU_Sep", "WQU_Oct", "WQU_Nov", "WQU_Dec"))%>%
    reshape2::melt(., id.vars=c("Name"))

  ###############################################################################################
  ###############################################################################################

  ##Calculate F1
  WQU_F1_1 <- plyr::ddply(WQU_thresh,. (Name), summarise,
                          WQU_0=sum(value=="0",na.rm=T), WQU_1=sum(value=="1",na.rm=T))

  ##calculate individual site values for F1 and F2
  WQU_F1_2 <- plyr::ddply(WQU_F1_1,. (Name), transform, WQU_F1 = ifelse(WQU_0==0,0,1))

  ##summary F1 values
  ## melt data prior to analysis
  WQU_F1_3 <- reshape2::melt(WQU_F1_2, id.vars=c("Name"))

  ## number of sites which exceeded threshold value
  WQU_F1_4 <- plyr::ddply(WQU_F1_3,. (variable), summarise, Sum_var = sum(value))


  ##determine number of sites
  WQU_sites_1 <- plyr::ddply(WQU_F1_3, .(Name), summarise, count = length(unique(Name)))
  WQU_sites_2 <- plyr::count(WQU_sites_1$count)

  ##Final calculations of F1 values
  WQU_F1_4$freq<-WQU_sites_2$freq
  WQU_F1_5 <- plyr::ddply(WQU_F1_4,. (variable, Sum_var), transform,
                          F1 = (Sum_var/freq)*100,
                          ESI_F1 = 100-((Sum_var/freq)*100))

  ##F1 values
  WQU_F1_output <- subset(WQU_F1_5, select = c("variable", "F1", "ESI_F1"), variable == c("WQU_F1"))



  #############################################################################################################
  ## CALCULATE F2
  ##calculate total number of instances monitored for each site
  WQU_F2_1 <- plyr::ddply(WQU_F1_1,. (Name,WQU_0,WQU_1), transform,
                          WQU_T = WQU_0+ WQU_1, Factor = "F2")


  ## calculate total number of instances for each site
  WQU_F2_2 <- subset(WQU_F2_1, select = c("WQU_0", "WQU_T", "Factor"))

  WQU_F2_3 <- plyr::ddply(WQU_F2_2, .(Factor), colwise(sum))

  ##Calculate F2 values
  WQU_F2_4 <- plyr::ddply(WQU_F2_3,. (WQU_0, WQU_T), transform,
                          WQU_F2 = ((WQU_0/WQU_T)*100))

  ## melt data prior to analysis
  WQU_F2_5 <- reshape2::melt(WQU_F2_4, id.vars=c("Factor"))

  WQU_F2_output <- subset(WQU_F2_5, select = c("variable", "value"), variable == c("WQU_F2"))


  ###########################################################################################################
  ## calculate ENI for F1 AND F2

  ## combine the F1 and F2 output and then clean it up
  WQU_F2_output <-rename(WQU_F2_output, c("variable"="var"))
  WQU_F1_F2<-cbind(WQU_F1_output,WQU_F2_output)
  WQU_F1_F2<-rename(WQU_F1_F2, c("value"="F2"))

  ##rename parameters
  WQU_F1_F2<-F1_F2 <- plyr::ddply(WQU_F1_F2,.(variable), transform,
                                  Parameter=ifelse(variable=="WQU_F1","WQU"))

  WQU_F1_F2<-subset(WQU_F1_F2, select = c("Parameter","F1", "F2"))

  ## calculate ENI for F1 and F2

  WQU_ESI_F1_F2 <- plyr::ddply(WQU_F1_F2,. (F1,F2), transform,
                               F2_ESI = 100-sqrt(((F1^2+F2^2)/2)))



  ########################################################################################
  ##calculate F3

  WQU_F3_1<-plyr::ddply(WQU_post,.(Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec, Jan_low_ci, Jan_high_ci,
                                   Feb_low_ci, Feb_high_ci, Mar_low_ci, Mar_high_ci, Apr_low_ci, Apr_high_ci,
                                   May_low_ci, May_high_ci, Jun_low_ci, Jun_high_ci, Jul_low_ci, Jul_high_ci,
                                   Aug_low_ci, Aug_high_ci, Sep_low_ci, Sep_high_ci, Oct_low_ci, Oct_high_ci,
                                   Nov_low_ci, Nov_high_ci, Dec_low_ci, Dec_high_ci),transform,
                        WQU_Jan_Exc = ifelse(Jan <= Jan_low_ci,(Jan_low_ci/Jan)-1,
                                             ifelse(Jan >= Jan_high_ci,(Jan/Jan_high_ci)-1,NA)),
                        WQU_Feb_Exc = ifelse(Feb <= Feb_low_ci,(Feb_low_ci/Feb)-1,
                                             ifelse(Feb >= Feb_high_ci,(Feb/Feb_high_ci)-1,NA)),
                        WQU_Mar_Exc = ifelse(Mar <= Mar_low_ci,(Mar_low_ci/Mar)-1,
                                             ifelse(Mar >= Mar_high_ci,(Mar/Mar_high_ci)-1,NA)),
                        WQU_Apr_Exc = ifelse(Apr <= Apr_low_ci,(Apr_low_ci/Apr)-1,
                                             ifelse(Apr >= Apr_high_ci,(Apr/Apr_high_ci)-1,NA)),
                        WQU_May_Exc = ifelse(May <= May_low_ci,(May_low_ci/May)-1,
                                             ifelse(May >= May_high_ci,(May/May_high_ci)-1,NA)),
                        WQU_Jun_Exc = ifelse(Jun <= Jun_low_ci,(Jun_low_ci/Jun)-1,
                                             ifelse(Jun >= Jun_high_ci,(Jun/Jun_high_ci)-1,NA)),
                        WQU_Jul_Exc = ifelse(Jul <= Jul_low_ci,(Jul_low_ci/Jul)-1,
                                             ifelse(Jul >= Jul_high_ci,(Jul/Jul_high_ci)-1,NA)),
                        WQU_Aug_Exc = ifelse(Aug <= Aug_low_ci,(Aug_low_ci/Aug)-1,
                                             ifelse(Aug >= Aug_high_ci,(Aug/Aug_high_ci)-1,NA)),
                        WQU_Sep_Exc = ifelse(Sep <= Sep_low_ci,(Sep_low_ci/Sep)-1,
                                             ifelse(Sep >= Sep_high_ci,(Sep/Sep_high_ci)-1,NA)),
                        WQU_Oct_Exc = ifelse(Oct <= Oct_low_ci,(Oct_low_ci/Oct)-1,
                                             ifelse(Oct >= Oct_high_ci,(Oct/Oct_high_ci)-1,NA)),
                        WQU_Nov_Exc = ifelse(Nov <= Nov_low_ci,(Nov_low_ci/Nov)-1,
                                             ifelse(Nov >= Nov_high_ci,(Nov/Nov_high_ci)-1,NA)),
                        WQU_Dec_Exc = ifelse(Dec <= Dec_low_ci,(Dec_low_ci/Dec)-1,
                                             ifelse(Dec >= Dec_high_ci,(Dec/Dec_high_ci)-1,NA)),
                        Factor = "F3")%>%

    ##sum of all excursions
    subset(., select = c("WQU_Jan_Exc", "WQU_Feb_Exc", "WQU_Mar_Exc", "WQU_Apr_Exc", "WQU_May_Exc", "WQU_Jun_Exc",
                         "WQU_Jul_Exc", "WQU_Aug_Exc", "WQU_Sep_Exc", "WQU_Oct_Exc", "WQU_Nov_Exc", "WQU_Dec_Exc", "Factor"))%>%
    reshape2::melt(., id.vars=c("Factor"))%>%
    rename(., c("value"="WQU_Exc"))

  WQU_F3_2 <-subset(WQU_F3_1, select = c("Factor","WQU_Exc"))%>%
    plyr::ddply(., .(Factor), colwise(sum), na.rm=TRUE)



  ## total number of instances
  WQU_F3_3 <- subset(WQU_F2_3, select = c("Factor","WQU_T"))%>%

    ##combine sum of excursions and instances
    rename(., c("Factor"="Fact"))


  WQU_F3_4 <- cbind(WQU_F3_2, WQU_F3_3)%>%
    subset(., select = c("Factor","WQU_Exc", "WQU_T"))%>%
    plyr::ddply(.,. (WQU_Exc, WQU_T), transform, WQU_nse = WQU_Exc/WQU_T)%>%
    plyr::ddply(.,. (WQU_nse), transform, WQU_F3 = ((WQU_nse)/(WQU_nse+1))*100)%>%

    ## summarise F3 calculations
    subset(., select = c("Factor", "WQU_F3"))

  ## combine the F1, F2 and F3 output and then clean it up
  WQU_F1_F2_F3<-cbind(WQU_F1_F2, WQU_F3_4)%>%
    rename(., c("WQU_F3"="F3"))%>%
    subset(., select = c("Parameter","F1", "F2", "F3"))

  ## calculate ENI for F1 and F2
  WQU_ESI_F1_3 <- plyr::ddply(WQU_F1_F2_F3,. (F1,F2,F3), transform, ESI_F1_3 = 100-sqrt(F1*F3))
  return(WQU_ESI_F1_3)}
