#' calculate DvNF for modelled flow/gauge height data
#'
#' @param x input file
#' @import
#' dplyr
#' lubridate
#' @examples
#' DvNF_model(KL_MOD)

#' @export

DvNF_model <- function (x){
  ##Summarise daily flow/height records into mean monthly records for each year
  ##date saved in form YYY/MM/DD
  Org_regulated <- dplyr::mutate(x, Date = lubridate::ymd(Date),Year = lubridate::year(Date), Month = lubridate::month(Date)) %>%
    dplyr::group_by(Month, Year) %>%
    dplyr::summarise(Regulated = mean(Regulated))
  Org_unregulated <- dplyr::mutate(x, Date = lubridate::ymd(Date),Year = lubridate::year(Date), Month = lubridate::month(Date)) %>%
    dplyr::group_by(Month, Year) %>%
    dplyr::summarise(Unregulated = mean(Unregulated))
  ##first AAPFD calculation
  Reg_unreg<- cbind(Org_regulated, Unregulated = Org_unregulated$Unregulated) %>%
    dplyr::mutate(Difference = (Regulated - Unregulated))

  ##determine unregulated flow for each month
  Unreg <- subset(Org_unregulated) %>%
    dplyr::summarise(Unreg = mean(Unregulated))
  Reg_unreg_1<-dplyr::full_join(Reg_unreg,Unreg, by="Month")

  ##determine number of years
  All_years <- Reg_unreg[2]
  Num_Years<-dplyr::summarise(All_years, dplyr::n_distinct(Year))
  Num_Years_1<-as.numeric(Num_Years)

  ##next AAPFD calculation
  Reg_unreg_2 <- dplyr::mutate(Reg_unreg_1, Division = (Difference/Unreg)^2)

  ##progressing on with AAPFD sum difference for each year

  Reg_unreg_3 <-  dplyr::group_by(Reg_unreg_2, Year ) %>%
    dplyr::summarise(Year_sum = sum(Division))
  Reg_unreg_4 <- dplyr::mutate(Reg_unreg_3, Rooted = (sqrt(Reg_unreg_3$Year_sum))/Num_Years_1)

  ##calculate AAPFD metric
  ##this isnt correct need to sum months for each year then divide by Num_years then sum
  AAPFD <- sum(Reg_unreg_4$Rooted)

  ##calculate DvNF metric
  DvNF <- ifelse(AAPFD < 0.3, (100 - (100*AAPFD)),
                 ifelse(AAPFD <0.5, (85-(50*AAPFD)),
                        ifelse(AAPFD <2, (80-(20*AAPFD)),
                               ifelse(AAPFD <5, (50-(10*AAPFD)),0))))
  return(data.frame(AAPFD = AAPFD, DvNF = DvNF))
}

#' calculate DvNF for measured flow/gauge height data
#'
#' @param x input file
#' @param y start year for the regulated period
#' @param z end year of the unregulated data
#' @import
#' dplyr
#' lubridate
#' @examples
#' DvNF_gauge(PK_HT, 2014, 1990)

#' @export



##A function to calculate APFD using pre and post disturbance gauge data summarised to a single year
##x = input file, y = start year for regulated period,
##z, end year of unregulated data

DvNF_gauge <- function (x,y,z){
  ##this function summarises the pre and post regulation period flow data into a single "year" for comparison.
  ##This is done as the pre and post regulation periods are likely to be different lengths thus a direct comparison of 'years'
  ##as with modelled data is not possible.
  ##Summarise daily flow/height records into mean monthly records for each year
  Org_data <- dplyr::mutate(x, Date = lubridate::ymd(Date),Year = lubridate::year(Date), Month = lubridate::month(Date)) %>%
    dplyr::group_by(Month, Year) %>%
    dplyr:: summarise(result = mean(Height))
  ##subset the regulated flow data based on the start year for regulation
  ##all data within and after this year will be selected
  Reg <-  subset(Org_data, Year >= y) %>%
    dplyr::summarise(Regulated = mean(result))
  ##subset the unregulated flow data based on last unregulated year
  ##all data before and including this year will be selected
  Unreg <- subset(Org_data, Year <= z) %>%
    dplyr::summarise(Unregulated = mean(result))
  Combined <- cbind(Reg, Unregulated = Unreg$Unregulated) %>%
    dplyr::mutate(Difference = ((Regulated - Unregulated)/Unregulated)^2)
  ##calculate AAPFD metric
  AAPFD <- sqrt(sum(Combined$Difference))
  ##calculate DvNF metric
  DvNF <- ifelse(AAPFD < 0.3, (100 - (100*AAPFD)),
                 ifelse(AAPFD <0.5, (85-(50*AAPFD)),
                        ifelse(AAPFD <2, (80-(20*AAPFD)),
                               ifelse(AAPFD <5, (50-(10*AAPFD)),0))))
  return(data.frame(AAPFD = AAPFD, DvNF = DvNF))
}

