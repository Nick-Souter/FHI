#' Modelled flow/guage height data
#'
#' A dataset containing the modelled height of the Tonle Sap Lake at Kampong Luong from 1 January 1985
#' to 31 December 2008
#'
#' @format A dataframe with 8766 rows and 3 variables:
#' \describe{
#'   \item{Date}{Date of the format, YYYY/MM/DD}
#'   \item{Regulated}{Modelled regulated flow/guage height}
#'   \item{Unregulated}{Modelled unregulated flow/guage height}
#'  ...
#' }
#' @source Mekong River Commission
"KL_MOD"



#' Measured flow/gauge height data
#'
#' A dataset containing the daily measured height of the Tonle Sap Lake at Prek Kdam from 1 January 1960
#' to 31 December 2018
#'
#' @format A dataframe with 21550 rows and 2 variables:
#' \describe{
#'   \item{Date}{Date of the format, YYYY/MM/DD}
#'   \item{Height}{Measured flow/guage height}
#'  ...
#' }
#' @source Mekong River Commission
"PK_HT"


#' Daily fish catch data
#'
#' A dataset containing daily fish catch data from the Tonle Sap Lake
#' in Cambodia
#'
#' @format A dataframe with 5065 rows and 8 variables:
#' \describe{
#'   \item{CatchRecord}{Unique value for each daily catch}
#'   \item{WeightCaught}{Total weight in kg for daily catch}
#'   \item{Station}{Broad location descriptor of daily catch e.g. Community fishery}
#'   \item{Latitude}{Latitude of fishing location}
#'   \item{Longitude}{Longitude of fishing location}
#'   \item{Habitat}{Habitat of fishing location}
#'   \item{Location}{Local location descriptor of daily catch}
#'  ...
#' }
#' @source Conservation International - Greater Mekong
"DailyCatch"

#' Water quality data
#'
#' A dataset containing water quality data collected from sites in Cambodia in the 3S river basin from October 2004
#' to December 2014
#'
#' @format A dataframe with 406 rows and 26 variables:
#' \describe{
#'   \item{Name}{Site name}
#'   \item{Year}{Calendar year of measurement}
#'   \item{Month_1}{calendar month of measurement as a number}
#'   \item{Month}{Calendar month of measurement as an abbreviation}
#'   \item{TEMP_C}{Water temperature in degrees centigrade}
#'   \item{pH}{pH}
#'   \item{TSS_mgL}{Total Suspended Solids in mg/L}
#'   \item{COND_mSm}{Conductivity, microSeimens/m}
#'   \item{Ca_meqL}{Calcium, mEq/L}
#'   \item{Mg_meqL}{Magnesium, mEq/L}
#'   \item{Na_meqL}{Sodium, mEq/L}
#'   \item{K_meqL}{Potassium, mEq/L}
#'   \item{ALK_meqL}{Alkalinity, mEq/L}
#'   \item{Cl_meqL}{Chloride, mEq/L}
#'   \item{SO4_meqL}{Sulphate, mEq/L}
#'   \item{NO32_mgL}{Total Nitrite and Nitrate, mg/L}
#'   \item{NH4N_mgL}{Ammonium, mg/L}
#'   \item{TOTN_mgL}{Total Nitrogen, mg/L}
#'   \item{TOTP_mgL}{Total Phosphorus, mg/L}
#'   \item{DO_mgL}{Dissolved Oxygen, mg/L}
#'   \item{CODMN_mgL}{Chemical Oxygen Demand, mg/L}
#'   \item{Ca_Mg}{Calcium/Magnesium, ratio}
#'   \item{Na_Cl}{Sodium/Chloride, ratio}
#'   \item{Na_K}{Sodium/Potassium, ratio}
#'   \item{Ca_SO4}{Calcium/Sulphate, ratio}
#'   \item{NH3_mgL}{Ammonia, mg/L}
#'  ...
#' }
#' @source Mekong River Commission
"Wq3S_Cam"

#' Ecosystem Service data
#'
#' A dataset containing dummy Ecosystem service data
#'
#' @format A dataframe with 395 rows and 2 variables:
#' \describe{
#'   \item{Name}{Site name}
#'   \item{Variable}{Measured Ecosystem service variable}
#'  ...
#' }
#' @source Mekong River Commission
"ES_test"
