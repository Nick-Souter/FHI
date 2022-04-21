#####Ecosystem Vitality Water Quality Indicators calculated using Method 2####

#' Calculate the Ecosystem Vitality Water Quality Indicator for Total Nitrogen using Method 2
#'
#' @param x input file
#' @param TN_low lower threshold value
#' @param TN_high upper threshold value
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
#' EV_TN(Wq3S_Cam, 0, 1.6)

#' @export

##Total nitrogen within  WQ index values, yes, 1; no, 0.
EV_TN<- function (x, TN_low, TN_high){
    x$TN_low <- TN_low
    x$TN_high <- TN_high
    Wq_TN_F1_1<-plyr::ddply(x,.(TOTN_mgL),transform,
                      TOTN_mgL_1 = ifelse(TOTN_mgL < TN_low,0,(ifelse(TOTN_mgL > TN_high,0,1))))%>%
    ##calculate F1 values
      plyr::ddply(.(Name), plyr::summarise,
                 TN_0 = sum(TOTN_mgL_1=="0",na.rm=T), TN_1 = sum(TOTN_mgL_1=="1",na.rm=T))


    ##calculate individual site values for F1 and F2, an F1 of 1 means a failure
    Wq_TN_F1_2<-plyr::ddply(Wq_TN_F1_1,.(TN_0), transform, TN_F1 = ifelse(TN_0==0,0,1))%>%

    ##summary F1 values
    ## melt data prior to analysis if you get an error object 'Name' not found then you need to attach the names to the input data file
      reshape2::melt(id.vars=c("Name"))

    ## number of occurrances which exceeded threshold value this needs to be its own variable for later use
    Wq_TN_F1_3<-plyr::ddply(Wq_TN_F1_2,. (variable), summarise, Sum_var = sum(value))

    ##determine number of sites
    Wq_TN_sites_1 <- plyr::ddply(Wq_TN_F1_2, .(Name), summarise, Numb = length(unique(Name)))

    Wq_TN_sites_2 <- plyr::count(Wq_TN_sites_1$Numb)


    ##Final calculations of F1 values
    Wq_TN_F1_3$freq<-Wq_TN_sites_2$freq
    Wq_TN_F1_4 <- plyr::ddply(Wq_TN_F1_3,. (variable, Sum_var), transform,
                     F1 = (Sum_var/(freq)*100),
                     ESI_F1 = 100-((Sum_var/(freq))*100)
    )
    Wq_TN_F1_4<-Wq_TN_F1_4[,-3]


    ##F1 values for difference in site
    F1_output <- subset(Wq_TN_F1_4, select = c("variable", "F1", "ESI_F1"), variable == c("TN_F1"))


############################################################################################################
   ## CALCULATE F2##
   ##calculate total number of instances monitored for each site
   Wq_TN_F2_1 <- plyr::ddply(Wq_TN_F1_1,. (Name, TN_0, TN_1), transform,
                             TN_T = TN_0 + TN_1, Factor = "F2")%>%
     subset(., select = c("TN_0", "TN_T", "Factor"))

   Wq_TN_F2_2 <-  plyr::ddply(Wq_TN_F2_1, .(Factor), colwise(sum))

   ##Calculate F2 values
   F2_output <- plyr::ddply(Wq_TN_F2_2,. (TN_0, TN_T), transform, TN_F2 = ((TN_0/TN_T)*100))%>%
    ## melt data prior to analysis
    reshape2::melt(., id.vars=c("Factor"))%>%
    subset(., select = c("variable", "value"), variable == c("TN_F2"))

###########################################################################################################
  ## calculate ENI for F1 AND F2
  ## combine the F1 and F2 output and then clean it up
  F2_output <-rename(F2_output, c("variable"="var"))
  F1_F2<-cbind(F1_output,F2_output)%>%
    rename(., c("value"="F2"))%>%
    plyr::ddply(.,.(variable), transform, Parameter=ifelse(variable=="TN_F1","TN","TN"))%>%
    subset(., select = c("Parameter","F1", "F2"))

  ## calculate ENI for F1 and F2 for Method 2 this isnt used for water quality
  ## ESI_F1_F2 <- plyr::ddply(F1_F2,. (F1,F2), transform,
  ##                 F2_ESI = 100-(sqrt(F1*F2)))

  ##calculate ENI for F1 and F2 variation on M1 also not used
  ## ESI_F1_F2 <- plyr::ddply(F1_F2,. (F1,F2), transform,
  ##                         F2_ESI = 100-sqrt(((F1^2+F2^2)/2)))

########################################################################################
  ##calculate F3
  Wq_TN_F3_1<-plyr::ddply(x,.(TOTN_mgL),transform,
                         TN_Exc = ifelse(TOTN_mgL < TN_low,(TN_low/TOTN_mgL)-1,
                                   ifelse(TOTN_mgL > TN_high,(TOTN_mgL/TN_high)-1,NA)),
                         Factor = "F3")%>%

    ##sum of all excursions
    subset(., select = c("TN_Exc", "Factor"))%>%
    plyr::ddply(., .(Factor), colwise(sum), na.rm=T)
    ## total number of instances

  ##combine sum of excursions and instances
  Wq_TN_F3_2<-subset(Wq_TN_F2_2, select = c("Factor", "TN_T"))%>%
    rename(., c("Factor"="Fact"))

  Wq_TN_F3_3<-cbind(Wq_TN_F3_1,Wq_TN_F3_2)%>%
    subset(., select = c("Factor", "TN_Exc", "TN_T"))%>%
    plyr::ddply(.,. (TN_Exc), transform, TN_nse = TN_Exc/TN_T)%>%
    plyr::ddply(., . (TN_nse), transform, TN_F3 = ((TN_nse)/(TN_nse+1))*100)%>%
    ## summarise F3 calculations
    subset(., select = c("Factor", "TN_F3"))%>%
    ## calculate ENI for F3
    reshape2::melt(., id.vars=c("Factor"))

  ## combine the F1, F2 and F3 output and then clean it up
  F1_F2_F3<-cbind(F1_F2, Wq_TN_F3_3)%>%
    rename(., c("value"="F3"))%>%
    subset(., select = c("Parameter","F1", "F2", "F3"))

  ## calculate ENI for F1 and F2
  ##Method 1 ESI not used
   ## ESI_F1_F2_F3 <- plyr::ddply(F1_F2_F3,. (F1,F2,F3), transform,
   ##                        ESI_F3 = 100-sqrt(((F1^2+F2^2+F3^2)/3))
   ##                      )
  ##Method 2 ESI
  ESI_F1_F3_TN <- plyr::ddply(F1_F2_F3,. (F1,F3), transform,
                     ESI_F1_3 = 100-(sqrt(F1*F3)))

  return(ESI_F1_F3_TN)
}

#######################################################################################

#' Calculate the Ecosystem Vitality Water Quality Indicator for Total Phosphorus using Method 2
#'
#' @param x input file
#' @param TP_high upper threshold value
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
#' EV_TP(Wq3S_Cam, 0.13)

#' @export

##Total Phosphorous within  WQ index values, yes, 1; no, 0.
EV_TP<- function (x, TP_high){
  x$TP_high <- TP_high
  Wq_TP_F1_1<-plyr::ddply(x,.(TOTP_mgL),transform,
                          TOTP_mgL_1 = ifelse(TOTP_mgL > TP_high,0,1))%>%
    ##calculate F1 values
    plyr::ddply(.(Name), plyr::summarise,
                TP_0 = sum(TOTP_mgL_1=="0",na.rm=T), TP_1 = sum(TOTP_mgL_1=="1",na.rm=T))

  ##calculate individual site values for F1 and F2, an F1 of 1 means a failure
  Wq_TP_F1_2<-plyr::ddply(Wq_TP_F1_1,.(TP_0), transform, TP_F1 = ifelse(TP_0==0,0,1))%>%

    ##summary F1 values
    ## melt data prior to analysis if you get an error object 'Name' not found then you need to attach the names to the input data file
    reshape2::melt(id.vars=c("Name"))

  ## number of occurrances which exceeded threshold value this needs to be its own variable for later use
  Wq_TP_F1_3<-plyr::ddply(Wq_TP_F1_2,. (variable), summarise, Sum_var = sum(value))

  ##determine number of sites I THINK I NEED TO MAKE THIS ITS OWN FUNCTION
  Wq_TP_sites_1 <- plyr::ddply(Wq_TP_F1_2, .(Name), summarise, Numb = length(unique(Name)))

  Wq_TP_sites_2 <- plyr::count(Wq_TP_sites_1$Numb)

  ##Final calculations of F1 values
  Wq_TP_F1_3$freq<-Wq_TP_sites_2$freq
  Wq_TP_F1_4 <- plyr::ddply(Wq_TP_F1_3,. (variable, Sum_var), transform,
                            F1 = (Sum_var/(freq)*100),
                            ESI_F1 = 100-((Sum_var/(freq))*100)
  )
  Wq_TP_F1_4<-Wq_TP_F1_4[,-3]


  ##F1 values for difference in site
  F1_output <- subset(Wq_TP_F1_4, select = c("variable", "F1", "ESI_F1"), variable == c("TP_F1"))

  ############################################################################################################
  ## CALCULATE F2##
  ##calculate total number of instances monitored for each site
  Wq_TP_F2_1 <- plyr::ddply(Wq_TP_F1_1,. (Name, TP_0, TP_1), transform,
                            TP_T = TP_0 + TP_1, Factor = "F2")%>%
    subset(., select = c("TP_0", "TP_T", "Factor"))

  Wq_TP_F2_2 <-  plyr::ddply(Wq_TP_F2_1, .(Factor), colwise(sum))

  ##Calculate F2 values
  F2_output <- plyr::ddply(Wq_TP_F2_2,. (TP_0, TP_T), transform, TP_F2 = ((TP_0/TP_T)*100))%>%
    ## melt data prior to analysis
    reshape2::melt(., id.vars=c("Factor"))%>%
    subset(., select = c("variable", "value"), variable == c("TP_F2"))

  ###########################################################################################################
  ## calculate ENI for F1 AND F2
  ## combine the F1 and F2 output and then clean it up
  F2_output <-rename(F2_output, c("variable"="var"))
  F1_F2<-cbind(F1_output,F2_output)%>%
    rename(., c("value"="F2"))%>%
    plyr::ddply(.,.(variable), transform, Parameter=ifelse(variable=="TP_F1","TP","TP"))%>%
    subset(., select = c("Parameter","F1", "F2"))

  ## calculate ENI for F1 and F2 for Method 2 this isn't used for water quality
  ## ESI_F1_F2 <- plyr::ddply(F1_F2,. (F1,F2), transform,
  ##                 F2_ESI = 100-(sqrt(F1*F2)))

  ##calculate ENI for F1 and F2 variation on M1 also not used
  ## ESI_F1_F2 <- plyr::ddply(F1_F2,. (F1,F2), transform,
  ##                         F2_ESI = 100-sqrt(((F1^2+F2^2)/2)))

  ########################################################################################
  ##calculate F3
  Wq_TP_F3_1<-plyr::ddply(x,.(TOTP_mgL),transform,
                          TP_Exc = ifelse(TOTP_mgL > TP_high,(TOTP_mgL/TP_high)-1,NA),
                          Factor = "F3")%>%

    ##sum of all excursions
    subset(., select = c("TP_Exc", "Factor"))%>%
    plyr::ddply(., .(Factor), colwise(sum), na.rm=T)

  ##combine sum of excursions and instances
  Wq_TP_F3_2<-subset(Wq_TP_F2_2, select = c("Factor", "TP_T"))%>%
    rename(., c("Factor"="Fact"))

  Wq_TP_F3_3<-cbind(Wq_TP_F3_1,Wq_TP_F3_2)%>%
    subset(., select = c("Factor", "TP_Exc", "TP_T"))%>%
    plyr::ddply(.,. (TP_Exc), transform, TP_nse = TP_Exc/TP_T)%>%
    plyr::ddply(., . (TP_nse), transform, TP_F3 = ((TP_nse)/(TP_nse+1))*100)%>%
    ## summarise F3 calculations
    subset(., select = c("Factor", "TP_F3"))%>%
    ## calculate ENI for F3
    reshape2::melt(., id.vars=c("Factor"))

  ## combine the F1, F2 and F3 output and then clean it up
  F1_F2_F3<-cbind(F1_F2, Wq_TP_F3_3)%>%
    rename(., c("value"="F3"))%>%
    subset(., select = c("Parameter","F1", "F2", "F3"))

  ## calculate ENI for F1 and F2
  ##Method 1 ESI not used
  ## ESI_F1_F2_F3 <- plyr::ddply(F1_F2_F3,. (F1,F2,F3), transform,
  ##                        ESI_F3 = 100-sqrt(((F1^2+F2^2+F3^2)/3))
  ##                      )
  ##Method 2 ESI
  ESI_F1_F3_TP <- plyr::ddply(F1_F2_F3,. (F1,F3), transform,
                           ESI_F1_3 = 100-(sqrt(F1*F3)))

  return(ESI_F1_F3_TP)
}

#######################################################################################

#' Calculate the Ecosystem Vitality Water Quality Indicator for pH using Method 2
#'
#' @param x input file
#' @param pH_low lower threshold value
#' @param pH_high upper threshold value
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
#' EV_pH(Wq3S_Cam, 6, 9)

#' @export

##pH within  WQ index values, yes, 1; no, 0.
EV_pH<- function (x, pH_low, pH_high){
  x$pH_low <- pH_low
  x$pH_high <- pH_high
  Wq_pH_F1_1<-plyr::ddply(x,.(pH),transform,
                          pH_1 = ifelse(pH < pH_low,0,(ifelse(pH > pH_high,0,1))))%>%
    ##calculate F1 values
    plyr::ddply(.(Name), plyr::summarise,
                pH_0 = sum(pH_1=="0",na.rm=T), pH_1 = sum(pH_1=="1",na.rm=T))

  ##calculate individual site values for F1 and F2, an F1 of 1 means a failure
  Wq_pH_F1_2<-plyr::ddply(Wq_pH_F1_1,.(pH_0), transform, pH_F1 = ifelse(pH_0==0,0,1))%>%

    ##summary F1 values
    ## melt data prior to analysis if you get an error object 'Name' not found then you need to attach the names to the input data file
    reshape2::melt(id.vars=c("Name"))

  ## number of occurrances which exceeded threshold value this needs to be its own variable for later use
  Wq_pH_F1_3<-plyr::ddply(Wq_pH_F1_2,. (variable), summarise, Sum_var = sum(value))

  ##determine number of sites I THINK I NEED TO MAKE THIS ITS OWN FUNCTION
  Wq_pH_sites_1 <- plyr::ddply(Wq_pH_F1_2, .(Name), summarise, Numb = length(unique(Name)))

  Wq_pH_sites_2 <- plyr::count(Wq_pH_sites_1$Numb)

  ##Final calculations of F1 values
  Wq_pH_F1_3$freq<-Wq_pH_sites_2$freq
  Wq_pH_F1_4 <- plyr::ddply(Wq_pH_F1_3,. (variable, Sum_var), transform,
                            F1 = (Sum_var/(freq)*100),
                            ESI_F1 = 100-((Sum_var/(freq))*100)
  )
  Wq_pH_F1_4<-Wq_pH_F1_4[,-3]


  ##F1 values for difference in site
  F1_output <- subset(Wq_pH_F1_4, select = c("variable", "F1", "ESI_F1"), variable == c("pH_F1"))

  ############################################################################################################
  ## CALCULATE F2##
  ##calculate total number of instances monitored for each site
  Wq_pH_F2_1 <- plyr::ddply(Wq_pH_F1_1,. (Name, pH_0, pH_1), transform,
                            pH_T = pH_0 + pH_1, Factor = "F2")%>%
    subset(., select = c("pH_0", "pH_T", "Factor"))

  Wq_pH_F2_2 <-  plyr::ddply(Wq_pH_F2_1, .(Factor), colwise(sum))

  ##Calculate F2 values
  F2_output <- plyr::ddply(Wq_pH_F2_2,. (pH_0, pH_T), transform, pH_F2 = ((pH_0/pH_T)*100))%>%
    ## melt data prior to analysis
    reshape2::melt(., id.vars=c("Factor"))%>%
    subset(., select = c("variable", "value"), variable == c("pH_F2"))

  ###########################################################################################################
  ## calculate ENI for F1 AND F2
  ## combine the F1 and F2 output and then clean it up
  F2_output <-rename(F2_output, c("variable"="var"))
  F1_F2<-cbind(F1_output,F2_output)%>%
    rename(., c("value"="F2"))%>%
    plyr::ddply(.,.(variable), transform, Parameter=ifelse(variable=="pH_F1","pH","pH"))%>%
    subset(., select = c("Parameter","F1", "F2"))

  ## calculate ENI for F1 and F2 for Method 2 this isnt used for water quality
  ## ESI_F1_F2 <- plyr::ddply(F1_F2,. (F1,F2), transform,
  ##                 F2_ESI = 100-(sqrt(F1*F2)))

  ##calculate ENI for F1 and F2 variation on M1 also not used
  ## ESI_F1_F2 <- plyr::ddply(F1_F2,. (F1,F2), transform,
  ##                         F2_ESI = 100-sqrt(((F1^2+F2^2)/2)))

  ########################################################################################
  ##calculate F3
  Wq_pH_F3_1<-plyr::ddply(x,.(pH),transform,
                          pH_Exc = ifelse(pH < pH_low,(pH_low/pH)-1,
                                          ifelse(pH > pH_high,(pH/pH_high)-1,NA)),
                          Factor = "F3")%>%

    ##sum of all excursions
    subset(., select = c("pH_Exc", "Factor"))%>%
    plyr::ddply(., .(Factor), colwise(sum), na.rm=T)
  ## total number of instances

  ##combine sum of excursions and instances
  Wq_pH_F3_2<-subset(Wq_pH_F2_2, select = c("Factor", "pH_T"))%>%
    rename(., c("Factor"="Fact"))

  Wq_pH_F3_3<-cbind(Wq_pH_F3_1,Wq_pH_F3_2)%>%
    subset(., select = c("Factor", "pH_Exc", "pH_T"))%>%
    plyr::ddply(.,. (pH_Exc), transform, pH_nse = pH_Exc/pH_T)%>%
    plyr::ddply(., . (pH_nse), transform, pH_F3 = ((pH_nse)/(pH_nse+1))*100)%>%
    ## summarise F3 calculations
    subset(., select = c("Factor", "pH_F3"))%>%
    ## calculate ENI for F3
    reshape2::melt(., id.vars=c("Factor"))

  ## combine the F1, F2 and F3 output and then clean it up
  F1_F2_F3<-cbind(F1_F2, Wq_pH_F3_3)%>%
    rename(., c("value"="F3"))%>%
    subset(., select = c("Parameter","F1", "F2", "F3"))

  ## calculate ENI for F1 and F2
  ##Method 1 ESI not used
  ## ESI_F1_F2_F3 <- plyr::ddply(F1_F2_F3,. (F1,F2,F3), transform,
  ##                        ESI_F3 = 100-sqrt(((F1^2+F2^2+F3^2)/3))
  ##                      )
  ##Method 2 ESI
  ESI_F1_F3_pH <- plyr::ddply(F1_F2_F3,. (F1,F3), transform,
                              ESI_F1_3 = 100-(sqrt(F1*F3)))

  return(ESI_F1_F3_pH)
}

