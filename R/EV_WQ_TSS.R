#' Calculate the Ecosystem Vitality Water Quality Indicator for Total Suspended Solids where thresholds are unknown using Method 2
#'
#' @param x input file
#' @param y threshold year this is the year previous to the first assessment year
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
#' EV_TSS(Wq3S_Cam, 2008)

#' @export

##calcualte threshold values for TSS data from before a cut off date. x=data, y= threshold year
EV_TSS<- function (x,y){
  ##subset TSS only
  TSS <- subset(x, select = c("Name", "Year", "Month", "TSS_mgL"))
  ## TSS <- subset(x, select = c("Name", "Year", "Month", "TSS_mgL"))

  ##subset of TSS prior to the cut-off year
  TSS_prior <- subset(TSS, Year <= y)

  ##Mean with 99% confidence intervals not used
  TSS_prior_summary<-Summary_stats(TSS_prior, measurevar="TSS_mgL", groupvars=c("Month"), na.rm=T)

  ## monthly max and min values used instead of confidence intervals
  TSS_prior_min_max <- plyr::ddply(TSS_prior_summary,. (TSS_mgL, min, max), transform,
                    low_ci=min,
                    high_ci=max
                  )
  TSS_prior_min_max$Month <- factor(TSS_prior_min_max$Month, levels = month.abb)

  ##sort by month
  TSS_prior_min_max <-TSS_prior_min_max [order(TSS_prior_min_max$Month), ]

  TSS_thresholds <- subset(TSS_prior_min_max, select = c("Month", "low_ci", "high_ci"))
  TSS_thresholds$Month <- factor(TSS_thresholds$Month, levels = month.abb)
  ##sort by month
  TSS_thresholds <-TSS_thresholds [order(TSS_thresholds$Month), ]

 ## TSS_thresholds_1 <- TSS_thresholds[,-1]


 ## rownames(TSS_thresholds_1) <- TSS_thresholds[,1]

  Jan_thres <- filter(TSS_thresholds, Month == "Jan")%>%
    .[,-1]
    names (Jan_thres)[1] <- "Jan_low_ci"
    names (Jan_thres)[2] <- "Jan_high_ci"

  Feb_thres <- filter(TSS_thresholds, Month == "Feb")%>%
    .[,-1]
    names (Feb_thres)[1] <- "Feb_low_ci"
    names (Feb_thres)[2] <- "Feb_high_ci"

  Mar_thres <- filter(TSS_thresholds, Month == "Mar")%>%
    .[,-1]
    names (Mar_thres)[1] <- "Mar_low_ci"
    names (Mar_thres)[2] <- "Mar_high_ci"

  Apr_thres <- filter(TSS_thresholds, Month == "Apr")%>%
    .[,-1]
    names (Apr_thres)[1] <- "Apr_low_ci"
    names (Apr_thres)[2] <- "Apr_high_ci"

  May_thres <- filter(TSS_thresholds, Month == "May")%>%
    .[,-1]
    names (May_thres)[1] <- "May_low_ci"
    names (May_thres)[2] <- "May_high_ci"

  Jun_thres <- filter(TSS_thresholds, Month == "Jun")%>%
     .[,-1]
     names (Jun_thres)[1] <- "Jun_low_ci"
     names (Jun_thres)[2] <- "Jun_high_ci"

  Jul_thres <- filter(TSS_thresholds, Month == "Jul")%>%
    .[,-1]
    names (Jul_thres)[1] <- "Jul_low_ci"
    names (Jul_thres)[2] <- "Jul_high_ci"

  Aug_thres <- filter(TSS_thresholds, Month == "Aug")%>%
    .[,-1]
    names (Aug_thres)[1] <- "Aug_low_ci"
    names (Aug_thres)[2] <- "Aug_high_ci"

  Sep_thres <- filter(TSS_thresholds, Month == "Sep")%>%
    .[,-1]
    names (Sep_thres)[1] <- "Sep_low_ci"
    names (Sep_thres)[2] <- "Sep_high_ci"

  Oct_thres <- filter(TSS_thresholds, Month == "Oct")%>%
    .[,-1]
    names (Oct_thres)[1] <- "Oct_low_ci"
    names (Oct_thres)[2] <- "Oct_high_ci"

    Nov_thres <- filter(TSS_thresholds, Month == "Nov")%>%
    .[,-1]
    names (Nov_thres)[1] <- "Nov_low_ci"
    names (Nov_thres)[2] <- "Nov_high_ci"

  Dec_thres <- filter(TSS_thresholds, Month == "Dec")%>%
    .[,-1]
    names (Dec_thres)[1] <- "Dec_low_ci"
    names (Dec_thres)[2] <- "Dec_high_ci"

  TSS_thresholds_1 <- merge(Jan_thres, Feb_thres)%>%
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


  ##subset of post period TSS data
  TSS_post <- subset(TSS, Year > y)%>%
  reshape2::dcast(., Name + Year~Month,  value.var = "TSS_mgL")%>%
  cbind(., TSS_thresholds_1)

  ##WQ within WQ index values, yes,1; no,0.
  TSS_thresh<-plyr::ddply(TSS_post,.(Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec, Jan_low_ci, Jan_high_ci,
                               Feb_low_ci, Feb_high_ci, Mar_low_ci, Mar_high_ci, Apr_low_ci, Apr_high_ci,
                               May_low_ci, May_high_ci, Jun_low_ci, Jun_high_ci, Jul_low_ci, Jul_high_ci,
                               Aug_low_ci, Aug_high_ci, Sep_low_ci, Sep_high_ci, Oct_low_ci, Oct_high_ci,
                               Nov_low_ci, Nov_high_ci, Dec_low_ci, Dec_high_ci),transform,
              TSS_Jan = ifelse(Jan <= Jan_low_ci,0,
                               ifelse(Jan >= Jan_high_ci,0,1)),
              TSS_Feb = ifelse(Feb <= Feb_low_ci,0,
                              ifelse(Feb >= Feb_high_ci,0,1)),
              TSS_Mar = ifelse(Mar <= Mar_low_ci,0,
                              ifelse(Mar >= Mar_high_ci,0,1)),
              TSS_Apr = ifelse(Apr <= Apr_low_ci,0,
                              ifelse(Apr >= Apr_high_ci,0,1)),
              TSS_May = ifelse(May <= May_low_ci,0,
                              ifelse(May >= May_high_ci,0,1)),
              TSS_Jun = ifelse(Jun <= Jun_low_ci,0,
                              ifelse(Jun >= Jun_high_ci,0,1)),
              TSS_Jul = ifelse(Jul <= Jul_low_ci,0,
                              ifelse(Jul >= Jul_high_ci,0,1)),
              TSS_Aug = ifelse(Aug <= Aug_low_ci,0,
                              ifelse(Aug >= Aug_high_ci,0,1)),
              TSS_Sep = ifelse(Sep <= Sep_low_ci,0,
                              ifelse(Sep >= Sep_high_ci,0,1)),
              TSS_Oct = ifelse(Oct <= Oct_low_ci,0,
                              ifelse(Oct >= Oct_high_ci,0,1)),
              TSS_Nov = ifelse(Nov <= Nov_low_ci,0,
                              ifelse(Nov >= Nov_high_ci,0,1)),
              TSS_Dec = ifelse(Dec <= Dec_low_ci,0,
                              ifelse(Dec >= Dec_high_ci,0,1))
                  )%>%
    subset(., select = c("Name", "TSS_Jan", "TSS_Feb", "TSS_Mar", "TSS_Apr", "TSS_May", "TSS_Jun",
                                  "TSS_Jul", "TSS_Aug", "TSS_Sep", "TSS_Oct", "TSS_Nov", "TSS_Dec"))%>%
    reshape2::melt(., id.vars=c("Name"))


###############################################################################################
###############################################################################################

##Calculate F1
    TSS_F1_1 <- plyr::ddply(TSS_thresh,. (Name), summarise,
                     TSS_0=sum(value=="0",na.rm=T), TSS_1=sum(value=="1",na.rm=T))

##calculate individual site values for F1 and F2
    TSS_F1_2 <- plyr::ddply(TSS_F1_1,. (Name), transform, TSS_F1 = ifelse(TSS_0==0,0,1))

##summary F1 values
## melt data prior to analysis
    TSS_F1_3 <- reshape2::melt(TSS_F1_2, id.vars=c("Name"))

## number of sites which exceeded threshold value
    TSS_F1_4 <- plyr::ddply(TSS_F1_3,. (variable), summarise, Sum_var = sum(value))


##determine number of sites
    TSS_sites_1 <- plyr::ddply(TSS_F1_3, .(Name), summarise, count = length(unique(Name)))
    TSS_sites_2 <- plyr::count(TSS_sites_1$count)

  ##Final calculations of F1 values
    TSS_F1_4$freq<-TSS_sites_2$freq
    TSS_F1_5 <- plyr::ddply(TSS_F1_4,. (variable, Sum_var), transform,
                                    F1 = (Sum_var/freq)*100,
                                    ESI_F1 = 100-((Sum_var/freq)*100))

##F1 values
    TSS_F1_output <- subset(TSS_F1_5, select = c("variable", "F1", "ESI_F1"), variable == c("TSS_F1"))


#############################################################################################################
## CALCULATE F2
##calculate total number of instances monitored for each site
    TSS_F2_1 <- plyr::ddply(TSS_F1_1,. (Name,TSS_0,TSS_1), transform,
                      TSS_T = TSS_0+ TSS_1, Factor = "F2")


## calculate total number of instances for each site
    TSS_F2_2 <- subset(TSS_F2_1, select = c("TSS_0", "TSS_T", "Factor"))

    TSS_F2_3 <- plyr::ddply(TSS_F2_2, .(Factor), colwise(sum))

##Calculate F2 values
    TSS_F2_4 <- plyr::ddply(TSS_F2_3,. (TSS_0, TSS_T), transform,
                            TSS_F2 = ((TSS_0/TSS_T)*100))

## melt data prior to analysis
   TSS_F2_5 <- reshape2::melt(TSS_F2_4, id.vars=c("Factor"))

   TSS_F2_output <- subset(TSS_F2_5, select = c("variable", "value"), variable == c("TSS_F2"))


###########################################################################################################
## calculate ENI for F1 AND F2

## combine the F1 and F2 output and then clean it up
  TSS_F2_output <-rename(TSS_F2_output, c("variable"="var"))
  TSS_F1_F2<-cbind(TSS_F1_output,TSS_F2_output)
  TSS_F1_F2<-rename(TSS_F1_F2, c("value"="F2"))

##rename parameters
  TSS_F1_F2<-F1_F2 <- plyr::ddply(TSS_F1_F2,.(variable), transform,
                                  Parameter=ifelse(variable=="TSS_F1","TSS"))

  TSS_F1_F2<-subset(TSS_F1_F2, select = c("Parameter","F1", "F2"))

## calculate ENI for F1 and F2

  TSS_ESI_F1_F2 <- plyr::ddply(TSS_F1_F2,. (F1,F2), transform,
                         F2_ESI = 100-sqrt(((F1^2+F2^2)/2)))

########################################################################################
##calculate F3

  TSS_F3_1<-plyr::ddply(TSS_post,.(Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec, Jan_low_ci, Jan_high_ci,
                                   Feb_low_ci, Feb_high_ci, Mar_low_ci, Mar_high_ci, Apr_low_ci, Apr_high_ci,
                                   May_low_ci, May_high_ci, Jun_low_ci, Jun_high_ci, Jul_low_ci, Jul_high_ci,
                                   Aug_low_ci, Aug_high_ci, Sep_low_ci, Sep_high_ci, Oct_low_ci, Oct_high_ci,
                                   Nov_low_ci, Nov_high_ci, Dec_low_ci, Dec_high_ci),transform,
            TSS_Jan_Exc = ifelse(Jan <= Jan_low_ci,(Jan_low_ci/Jan)-1,
                          ifelse(Jan >= Jan_high_ci,(Jan/Jan_high_ci)-1,NA)),
            TSS_Feb_Exc = ifelse(Feb <= Feb_low_ci,(Feb_low_ci/Feb)-1,
                          ifelse(Feb >= Feb_high_ci,(Feb/Feb_high_ci)-1,NA)),
            TSS_Mar_Exc = ifelse(Mar <= Mar_low_ci,(Mar_low_ci/Mar)-1,
                          ifelse(Mar >= Mar_high_ci,(Mar/Mar_high_ci)-1,NA)),
            TSS_Apr_Exc = ifelse(Apr <= Apr_low_ci,(Apr_low_ci/Apr)-1,
                          ifelse(Apr >= Apr_high_ci,(Apr/Apr_high_ci)-1,NA)),
            TSS_May_Exc = ifelse(May <= May_low_ci,(May_low_ci/May)-1,
                          ifelse(May >= May_high_ci,(May/May_high_ci)-1,NA)),
            TSS_Jun_Exc = ifelse(Jun <= Jun_low_ci,(Jun_low_ci/Jun)-1,
                          ifelse(Jun >= Jun_high_ci,(Jun/Jun_high_ci)-1,NA)),
            TSS_Jul_Exc = ifelse(Jul <= Jul_low_ci,(Jul_low_ci/Jul)-1,
                          ifelse(Jul >= Jul_high_ci,(Jul/Jul_high_ci)-1,NA)),
            TSS_Aug_Exc = ifelse(Aug <= Aug_low_ci,(Aug_low_ci/Aug)-1,
                          ifelse(Aug >= Aug_high_ci,(Aug/Aug_high_ci)-1,NA)),
            TSS_Sep_Exc = ifelse(Sep <= Sep_low_ci,(Sep_low_ci/Sep)-1,
                          ifelse(Sep >= Sep_high_ci,(Sep/Sep_high_ci)-1,NA)),
            TSS_Oct_Exc = ifelse(Oct <= Oct_low_ci,(Oct_low_ci/Oct)-1,
                          ifelse(Oct >= Oct_high_ci,(Oct/Oct_high_ci)-1,NA)),
            TSS_Nov_Exc = ifelse(Nov <= Nov_low_ci,(Nov_low_ci/Nov)-1,
                          ifelse(Nov >= Nov_high_ci,(Nov/Nov_high_ci)-1,NA)),
            TSS_Dec_Exc = ifelse(Dec <= Dec_low_ci,(Dec_low_ci/Dec)-1,
                          ifelse(Dec >= Dec_high_ci,(Dec/Dec_high_ci)-1,NA)),
            Factor = "F3")%>%

##sum of all excursions
    subset(., select = c("TSS_Jan_Exc", "TSS_Feb_Exc", "TSS_Mar_Exc", "TSS_Apr_Exc", "TSS_May_Exc", "TSS_Jun_Exc",
                       "TSS_Jul_Exc", "TSS_Aug_Exc", "TSS_Sep_Exc", "TSS_Oct_Exc", "TSS_Nov_Exc", "TSS_Dec_Exc", "Factor"))%>%
    reshape2::melt(., id.vars=c("Factor"))%>%
    rename(., c("value"="TSS_Exc"))

  TSS_F3_2 <-subset(TSS_F3_1, select = c("Factor","TSS_Exc"))%>%
    plyr::ddply(., .(Factor), colwise(sum), na.rm=TRUE)


## total number of instances
  TSS_F3_3 <- subset(TSS_F2_3, select = c("Factor","TSS_T"))%>%


##combine sum of excursions and instances
    rename(., c("Factor"="Fact"))


  TSS_F3_4 <- cbind(TSS_F3_2, TSS_F3_3)%>%
    subset(., select = c("Factor","TSS_Exc", "TSS_T"))%>%
    plyr::ddply(.,. (TSS_Exc, TSS_T), transform, TSS_nse = TSS_Exc/TSS_T)%>%
    plyr::ddply(.,. (TSS_nse), transform, TSS_F3 = ((TSS_nse)/(TSS_nse+1))*100)%>%

## summarise F3 calculations
   subset(., select = c("Factor", "TSS_F3"))

## combine the F1, F2 and F3 output and then clean it up
  TSS_F1_F2_F3<-cbind(TSS_F1_F2, TSS_F3_4)%>%
    rename(., c("TSS_F3"="F3"))%>%
    subset(., select = c("Parameter","F1", "F2", "F3"))


## calculate ENI for F1 and F2
  TSS_ESI_F1_3 <- plyr::ddply(TSS_F1_F2_F3,. (F1,F2,F3), transform, ESI_F1_3 = 100-sqrt(F1*F3))
  return(TSS_ESI_F1_3)}
################################################################################################

