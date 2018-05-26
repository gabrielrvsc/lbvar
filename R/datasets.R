#' A dataset used in the examples
#'
#' This data is from Garcia, Medeiros and Vasconcelos (2017). It has 93 variables which are divided between inflation indexes, macroeconomic variables and expectation variables.
#'
#' @docType data
#' @keywords datasets
#' @name BRinf
#' @usage data(BRinf)
#' @format A matrix with 156 rows and 92 variables.
#' @references Garcia, Medeiros and Vasconcelos (2017).
NULL


#' A dataset used in the examples
#'
#' This is a panel of volatilities with daily data for the 30 Dow stocks estimated using a garch(1,1) for each serie. The returns used on the garch are from the dataset dji30ret from te package rugarch.
#'
#' @docType data
#' @keywords datasets
#' @name voldata
#' @usage data(voldata)
#' @format A dataframe with 5521 rows and 30 variables.
#' @references Alexios Ghalanos (2015). rugarch: Univariate GARCH models. R package version 1.3-6.
NULL

#' Dataset from Barboza and Vasconcelos 2018
#'
#' This dataset contains all the variables used in the Bayesian VAR estimated by Barboza and Varsoncelos 2018. The base model does not use the capital goods CG variable. Variables are in the same order used in the identification. The variables are already treated fo the model.
#'
#' @docType data
#' @keywords datasets
#' @name BNDESdata
#' @usage data(BNDESdata)
#' @format A matrix with 180 rows and 12 variables.
#' @references Barboza and Vasconcelos (2018). Measuring the aggregate effects of the Brazilian Development Bank on investment
NULL
