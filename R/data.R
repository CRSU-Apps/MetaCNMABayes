#' Linde2016 Dataset
#'
#' A Binary Dataset for CNMA of response after treatment in long format
#'
#' Derived (modified) from
#' Balduzzi S, Rücker G, Nikolakopoulou A, Papakonstantinou T,
#' Salanti G, Efthimiou O, Schwarzer G (2023):
#' netmeta: An R package for network meta-analysis using frequentist methods.
#' Journal of Statistical Software, 106, 1-40
#' DOI:\url{https://doi.org/10.1002/14651858.CD013307.pub3}
#' Under GPL-2.0 License
#' Modified Date: 06/04/2024
#'
#' Originally from
#'
#' Klaus Linde, Gerta Rücker, Antonius Schneider, Levente Kriston,
#' Questionable assumptions hampered interpretation of a network meta-analysis
#' of primary care depression treatments,
#' Journal of Clinical Epidemiology, Volume 71, 2016, Pages 86-96,
#' ISSN 0895-4356,
#' DOI:\url{https://doi.org/10.1016/j.jclinepi.2015.10.010}
#'
#' @format ## `binary`
#' A data frame with 201 rows and 4 columns:
#' \describe{
#'   \item{Study}{Unique study identifier: author, year}
#'   \item{Events}{Number of events per arm}
#'   \item{Total}{Total patients per arm}
#'   \item{Components}{Components per arm seperated by +}
#'   ...
#' }
#'
#' @author Balduzzi S et al. \email{balduzzi@imbi.uni-freiburg.de}
#' @references \url{https://doi.org/10.18637/jss.v106.i02}
"binary"

#' Length of Stay Dataset
#'
#' A continuous dataset for CNMA of mean length of stay in long format
#'
#' Data derived from (with permision)
#'
#' Powell, R., Scott, N. W., Freeman, S. C., Sutton, A. J., Cooper,
#' N. J., Manyande, A., Bruce, J., Vögele, C., Byrne-Davis, L. M.,
#' Unsworth, M., Osmer, C., & Johnston, M. (2023).
#' Dataset: Psychological preparation and postoperative outcomes for
#' adults undergoing surgery under general anaesthesia [Data set].
#' Zenodo. \url{https://doi.org/10.5281/zenodo.8006703}
#'
#' Originally from
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