#' Calculate the Ecosystem Service Indicator using Method 1
#'
#' @param x input file
#' @param Threshold upper threshold value
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
#' ES_M1(ES_test, 0.13)

#' @export

##Variable within threshold, yes, 1; no, 0.
ES_M1<- function (x, Threshold){
  x$Threshold <- Threshold
  ES_F1_1<-plyr::ddply(x,.(Variable),transform,
                          Variable_1 = ifelse(Variable < Threshold,0,1))%>%
    ##calculate F1 values
    plyr::ddply(.(Name), plyr::summarise,
                ES_0 = sum(Variable_1=="0",na.rm=T), ES_1 = sum(Variable_1=="1",na.rm=T))

  ##calculate individual site values for F1 and F2, an F1 of 1 means a failure
  ES_F1_2<-plyr::ddply(ES_F1_1,.(ES_0), transform, ES_F1 = ifelse(ES_0==0,0,1))%>%

    ##summary F1 values
    ## melt data prior to analysis if you get an error object 'Name' not found then you need to attach the names to the input data file
    reshape2::melt(id.vars=c("Name"))

  ## number of occurrances which exceeded threshold value this needs to be its own variable for later use
  ES_F1_3<-plyr::ddply(ES_F1_2,. (variable), summarise, Sum_var = sum(value))

  ##determine number of sites I THINK I NEED TO MAKE THIS ITS OWN FUNCTION
  ES_sites_1 <- plyr::ddply(ES_F1_2, .(Name), summarise, Numb = length(unique(Name)))

  ES_sites_2 <- plyr::count(ES_sites_1$Numb)

  ##Final calculations of F1 values
  ES_F1_3$freq<-ES_sites_2$freq
  ES_F1_4 <- plyr::ddply(ES_F1_3,. (variable, Sum_var), transform,
                            F1 = (Sum_var/(freq)*100),
                            ESI_F1 = 100-((Sum_var/(freq))*100)
  )
  ES_F1_4<-ES_F1_4[,-3]


  ##F1 values for difference in site
  F1_output <- subset(ES_F1_4, select = c("variable", "F1", "ESI_F1"), variable == c("ES_F1"))

  ############################################################################################################
  ## CALCULATE F2##
  ##calculate total number of instances monitored for each site
  ES_F2_1 <- plyr::ddply(ES_F1_1,. (Name, ES_0, ES_1), transform,
                            ES_T = ES_0 + ES_1, Factor = "F2")%>%
    subset(., select = c("ES_0", "ES_T", "Factor"))

  ES_F2_2 <-  plyr::ddply(ES_F2_1, .(Factor), colwise(sum))

  ##Calculate F2 values
  F2_output <- plyr::ddply(ES_F2_2,. (ES_0, ES_T), transform, ES_F2 = ((ES_0/ES_T)*100))%>%
    ## melt data prior to analysis
    reshape2::melt(., id.vars=c("Factor"))%>%
    subset(., select = c("variable", "value"), variable == c("ES_F2"))

  ###########################################################################################################
  ## calculate ENI for F1 AND F2
  ## combine the F1 and F2 output and then clean it up
  F2_output <-rename(F2_output, c("variable"="var"))
  F1_F2<-cbind(F1_output,F2_output)%>%
    rename(., c("value"="F2"))%>%
    plyr::ddply(.,.(variable), transform, Parameter=ifelse(variable=="ES_F1","ES","ES"))%>%
    subset(., select = c("Parameter","F1", "F2"))

  ########################################################################################
  ##calculate F3
  ES_F3_1<-plyr::ddply(x,.(Variable),transform,
                          ES_Exc = ifelse(Variable < Threshold,(Threshold/Variable)-1,NA),
                          Factor = "F3")%>%

    ##sum of all excursions
    subset(., select = c("ES_Exc", "Factor"))%>%
    plyr::ddply(., .(Factor), colwise(sum), na.rm=T)
  ## total number of instances

  ##combine sum of excursions and instances
  ES_F3_2<-subset(ES_F2_2, select = c("Factor", "ES_T"))%>%
    rename(., c("Factor"="Fact"))

  ES_F3_3<-cbind(ES_F3_1,ES_F3_2)%>%
    subset(., select = c("Factor", "ES_Exc", "ES_T"))%>%
    plyr::ddply(.,. (ES_Exc), transform, ES_nse = ES_Exc/ES_T)%>%
    plyr::ddply(., . (ES_nse), transform, ES_F3 = ((ES_nse)/(ES_nse+1))*100)%>%
    ## summarise F3 calculations
    subset(., select = c("Factor", "ES_F3"))%>%
    ## calculate ENI for F3
    reshape2::melt(., id.vars=c("Factor"))

  ## combine the F1, F2 and F3 output and then clean it up
  F1_F2_F3<-cbind(F1_F2, ES_F3_3)%>%
    rename(., c("value"="F3"))%>%
    subset(., select = c("Parameter","F1", "F2", "F3"))

  ##calculate ENI for F1 and F2 variation on M1 not used as not in Shaad et al 2021 paper
  ## ESI_F1_F2 <- plyr::ddply(F1_F2,. (F1,F2), transform,
  ##                         F2_ESI = 100-sqrt(((F1^2+F2^2)/2)))


   ESI_F1_3 <- plyr::ddply(F1_F2_F3,. (F1,F2,F3), transform,
                          ESI_F3 = 100-sqrt(((F1^2+F2^2+F3^2)/3))
                       )


  return(ESI_F1_3)
}

###############################################################################
###############################################################################
#' Calculate the Ecosystem Service Indicator using Method 2
#'
#' @param x input file
#' @param Threshold upper threshold value
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
#' ES_M2(ES_test, 0.09)

#' @export

##Variable within threshold, yes, 1; no, 0.
ES_M2<- function (x, Threshold){
  x$Threshold <- Threshold
  ES_F1_1<-plyr::ddply(x,.(Variable),transform,
                       Variable_1 = ifelse(Variable < Threshold,0,1))%>%
    ##calculate F1 values
    plyr::ddply(.(Name), plyr::summarise,
                ES_0 = sum(Variable_1=="0",na.rm=T), ES_1 = sum(Variable_1=="1",na.rm=T))

  ##calculate individual site values for F1 and F2, an F1 of 1 means a failure
  ES_F1_2<-plyr::ddply(ES_F1_1,.(ES_0), transform, ES_F1 = ifelse(ES_0==0,0,1))%>%

    ##summary F1 values
    ## melt data prior to analysis if you get an error object 'Name' not found then you need to attach the names to the input data file
    reshape2::melt(id.vars=c("Name"))

  ## number of occurrences which exceeded threshold value this needs to be its own variable for later use
  ES_F1_3<-plyr::ddply(ES_F1_2,. (variable), summarise, Sum_var = sum(value))

  ##determine number of sites I THINK I NEED TO MAKE THIS ITS OWN FUNCTION
  ES_sites_1 <- plyr::ddply(ES_F1_2, .(Name), summarise, Numb = length(unique(Name)))

  ES_sites_2 <- plyr::count(ES_sites_1$Numb)

  ##Final calculations of F1 values
  ES_F1_3$freq<-ES_sites_2$freq
  ES_F1_4 <- plyr::ddply(ES_F1_3,. (variable, Sum_var), transform,
                         F1 = (Sum_var/(freq)*100),
                         ESI_F1 = 100-((Sum_var/(freq))*100)
  )
  ES_F1_4<-ES_F1_4[,-3]


  ##F1 values for difference in site
  F1_output <- subset(ES_F1_4, select = c("variable", "F1", "ESI_F1"), variable == c("ES_F1"))

  ############################################################################################################
  ## CALCULATE F2##
  ##calculate total number of instances monitored for each site
  ES_F2_1 <- plyr::ddply(ES_F1_1,. (Name, ES_0, ES_1), transform,
                         ES_T = ES_0 + ES_1, Factor = "F2")%>%
    subset(., select = c("ES_0", "ES_T", "Factor"))

  ES_F2_2 <-  plyr::ddply(ES_F2_1, .(Factor), colwise(sum))

  ##Calculate F2 values
  F2_output <- plyr::ddply(ES_F2_2,. (ES_0, ES_T), transform, ES_F2 = ((ES_0/ES_T)*100))%>%
    ## melt data prior to analysis
    reshape2::melt(., id.vars=c("Factor"))%>%
    subset(., select = c("variable", "value"), variable == c("ES_F2"))

  ###########################################################################################################
  ## calculate ENI for F1 AND F2
  ## combine the F1 and F2 output and then clean it up
  F2_output <-rename(F2_output, c("variable"="var"))
  F1_F2<-cbind(F1_output,F2_output)%>%
    rename(., c("value"="F2"))%>%
    plyr::ddply(.,.(variable), transform, Parameter=ifelse(variable=="ES_F1","ES","ES"))%>%
    subset(., select = c("Parameter","F1", "F2"))

  ########################################################################################
  ##calculate F3
  ES_F3_1<-plyr::ddply(x,.(Variable),transform,
                       ES_Exc = ifelse(Variable < Threshold,(Threshold/Variable)-1,NA),
                       Factor = "F3")%>%


    ##sum of all excursions
    subset(., select = c("ES_Exc", "Factor"))%>%
    plyr::ddply(., .(Factor), colwise(sum), na.rm=T)

  ##combine sum of excursions and instances
  ES_F3_2<-subset(ES_F2_2, select = c("Factor", "ES_T"))%>%
    rename(., c("Factor"="Fact"))

  ES_F3_3<-cbind(ES_F3_1,ES_F3_2)%>%
    subset(., select = c("Factor", "ES_Exc", "ES_T"))%>%
    plyr::ddply(.,. (ES_Exc), transform, ES_nse = ES_Exc/ES_T)%>%
    plyr::ddply(., . (ES_nse), transform, ES_F3 = ((ES_nse)/(ES_nse+1))*100)%>%
    ## summarise F3 calculations
    subset(., select = c("Factor", "ES_F3"))%>%
    ## calculate ENI for F3
    reshape2::melt(., id.vars=c("Factor"))

  ## combine the F1, F2 and F3 output and then clean it up
  F1_F2_F3<-cbind(F1_F2, ES_F3_3)%>%
    rename(., c("value"="F3"))%>%
    subset(., select = c("Parameter","F1", "F2", "F3"))

  ##Method 2 ESI
  ESI_F1_3 <- plyr::ddply(F1_F2_F3,. (F1,F3), transform,
                          ESI_F1 = 100-F1,
                          ESI_F1_2 = 100-(sqrt(F1*F2)),
                          ESI_F1_3 = 100-(sqrt(F1*F3)))

  return(ESI_F1_3)
}
