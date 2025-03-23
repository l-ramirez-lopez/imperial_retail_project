#' Compute MAPE for a Multiplicative Demand Model
#'
#' The \code{sales_optim()} function calculates predicted sales using
#' a multiplicative marketing mix model that includes baseline, trend, adstock
#' carryover, competitor pricing, own price elasticity, and seasonality factors.
#' It returns the mean absolute percentage error (MAPE) of the model's predictions.
#'
#' @param x A numeric vector of parameters (length 17). Expected order:
#'   \enumerate{
#'     \item \code{const} (overall constant term)
#'     \item \code{trend_base} (trend exponent base)
#'     \item \code{adeffect} (scale factor for adstock effect)
#'     \item \code{competitor_elasticity} (competitive price elasticity)
#'     \item \code{elasticity} (own price elasticity)
#'     \item \code{s1} through \code{s12} (seasonality coefficients for months 1 to 12)
#'   }
#' @param price A numeric vector of own prices.
#' @param com_price A numeric vector of competitor prices. If \code{NA}, the competitor
#'   price effect defaults to 1 (neutral).
#' @param year A numeric vector indicating the time index for the trend component.
#' @param monthofyear An integer vector (1 through 12) used to apply monthly seasonality.
#' @param adstock A numeric vector representing the carryover effect from advertising
#'   (e.g., output from \code{adstock}).
#' @param sales A numeric vector of actual observed sales to be compared against
#'   the model prediction.
#'
#' @return A single numeric value indicating the mean absolute percentage error (MAPE)
#'   between the model's predicted sales and the actual \code{sales}.
#'
#' @details
#' The function implements a multiplicative model:
#' \deqn{
#'   \hat{Y} = \mathrm{const} \times \bigl(\mathrm{trend\_base}^{(\mathrm{year})} + \mathrm{adeffect} \times \mathrm{adstock}\bigr)
#'            \times \bigl(\mathrm{price}/\mathrm{com\_price}\bigr)^{-\mathrm{competitor\_elasticity}}
#'            \times \mathrm{price}^{-\mathrm{elasticity}}
#'            \times \mathrm{seasonal\_index}.
#' }
#'
#' @examples
#' \dontrun{
#' param_guess <- rep(1, 17)
#' error_value <- sales_optim(
#'   x = param_guess,
#'   price = df$price,
#'   com_price = df$com_price,
#'   year = df$trend,
#'   monthofyear = df$month,
#'   adstock = df$Adstock,
#'   sales = df$weekly_sales
#' )
#' }
#'
#' @export


sales_optim <- function(x, price, com_price, year, monthofyear, adstock, sales) {
  #assigning parameters
  const <- x[1]
  trend_base <- x[2]
  adeffect <- x[3]
  competitor_elasticity <-  x[4]
  elasticity <- x[5]
  s1 <- x[6]
  s2 <- x[7]
  s3 <- x[8]
  s4 <- x[9]
  s5 <- x[10]
  s6 <- x[11]
  s7 <- x[12]
  s8 <- x[13]
  s9 <- x[14]
  s10 <- x[15]
  s11 <- x[16]
  s12 <- x[17]
  
  # Calculations
  seasonal_index <- case_when(
    monthofyear == 1  ~ s1,
    monthofyear == 2  ~ s2,
    monthofyear == 3  ~ s3,
    monthofyear == 4  ~ s4,
    monthofyear == 5  ~ s5,
    monthofyear == 6  ~ s6,
    monthofyear == 7  ~ s7,
    monthofyear == 8  ~ s8,
    monthofyear == 9  ~ s9,
    monthofyear == 10 ~ s10,
    monthofyear == 11 ~ s11,
    monthofyear == 12 ~ s12,
    TRUE ~ 1  # Default case (optional, in case of missing values)
  )
  
  # Competitor price effect
  competitor_price_effect <- ifelse(
    is.na(com_price), 
    1, ## 1 if com_price is NA ("no effect" from competitor price)
    (price / com_price) ^ (-competitor_elasticity)
  )
  
  sales_hat <- const * (trend_base^(year) + adeffect * adstock) * 
    competitor_price_effect *  
    (price ^ (-elasticity)) *  # Own Price Effect
    seasonal_index
  
  # Mean Absolute Percentage Error (MAPE)
  mape <- mean(abs((sales - sales_hat) / pmax(sales, 0.01)), na.rm = TRUE)
  
  return(mape)
}
