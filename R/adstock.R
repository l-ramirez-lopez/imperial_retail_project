#' Calculate Adstock for Each SKU
#'
#' The \code{adstock()} function computes a carryover (adstock) effect of
#' featuring an SKU on the main page. It arranges the data by \code{sku}
#' and \code{week}, then accumulates the \code{feat_main_page} indicator over time
#' using a decay parameter \code{lambda}.
#'
#' @param x A data frame containing at least the columns \code{sku}, \code{week},
#'   and \code{feat_main_page}.
#' @param lambda A numeric decay parameter (default is 0.7). Higher values
#'   indicate a slower decay of the advertising effect over time.
#'
#' @return A data frame identical to \code{x} but with an additional column
#'   \code{Adstock}, representing the cumulative carryover effect.
#'
#' @details
#' The adstock concept assumes that advertising in one period has an effect
#' that carries over into subsequent periods. The parameter \code{lambda} controls
#' how strongly prior advertising contributes to the current \code{Adstock} level.
#'
#' @examples
#' \dontrun{
#' df_with_adstock <- adstock(df, lambda = 0.7)
#' }
#'
#' @export
adstock <- function(x, lambda = 0.7) {
  df <- x %>%
    arrange(sku, week) %>%
    group_by(sku) %>%
    mutate(Adstock = accumulate(feat_main_page, ~ .x + lambda * .y, .init = first(feat_main_page))[-1]) %>%
    ungroup()
  return(df)
}
