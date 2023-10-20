#' Delirium Dataset
#'
#' A Binary Dataset for CNMA of Incidence of Delirium in long format
#'
#' Derived from (with permision)
#' Burton JK, Craig L, Yong SQ, Siddiqi N,
#' Teale EA, Woodhouse R, Barugh AJ, Shepherd AM,
#' Brunton A, Freeman SC, Sutton AJ, Quinn TJ.
#' Non‐pharmacological interventions for preventing delirium in
#' hospitalised non‐ICU patients.
#' Cochrane Database of Systematic Reviews 2021,
#' Issue 11. Art. No.: CD013307.
#' DOI:\url{https://doi.org/10.1002/14651858.CD013307.pub3}
#'
#' @format ## `binary`
#' A data frame with 28 rows and 4 columns:
#' \describe{
#'   \item{Study}{Unique study identifier: author, year}
#'   \item{Events}{Number of events per arm}
#'   \item{Total}{Total patients per arm}
#'   \item{Components}{Components per arm seperated by +}
#'   ...
#' }
#'
#' @author Burton JK et al. \email{jenni.burton@glasgow.ac.uk}
#' @references \url{https://doi.org/10.1002/14651858.CD013307.pub3}
"binary"

#' Length of Stay Dataset
#'
#' A continuous dataset for CNMA of mean length of stay in long format
#'
#' Data derived from (with permision)
#'
#' Nicky J. Welton, D. M. Caldwell, E. Adamopoulos, K. Vedhara,
#' Mixed Treatment Comparison Meta-Analysis of Complex Interventions:
#' Psychological Interventions in Coronary Heart Disease,
#' American Journal of Epidemiology,
#' Volume 169, Issue 9, 1 May 2009,
#' Pages 1158--1165, \url{https://doi.org/10.1093/aje/kwp014}
#'
#' @format ## `continuous`
#' A data frame with 28 rows and 5 columns:
#' \describe{
#'   \item{Study}{Unique study identifier: author, year}
#'   \item{Components}{Components per arm seperated by +}
#'   \item{Mean}{Mean length of stay per arm}
#'   \item{SD}{Standard deviation of the mean length of stay per arm}
#'   \item{Total}{Total patients per arm}
#'   ...
#' }
#'
#' @author Welton NJ et al. \email{Nicky.Welton@bristol.ac.uk}
#' @references \url{https://doi.org/10.1093/aje/kwp014}
"continuous"