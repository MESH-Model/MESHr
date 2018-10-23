#' MESH streamflow data
#'
#' @description A data frame containing MESH outputs for the Simonette river, as read in using the function \code{read_MESH_OutputTimeSeries_csv}.
#'
#' @format A data frame with 4381 rows and 5 variables (including the datetime):
#' \describe{
#'   \item{DATE}{date and time as an \R date object}
#'   \item{QOMEAS1}{measured flows at station 1}
#'   \item{QOSIM1}{simulated flows at station 1}
#'   \item{QOMEAS2}{measured flows at station 2}
#'   \item{QOSIM2}{simulated flows at station 2}
#' }
#' @source This data iwas obtained by running MESH.
"MESH_streamflows"
