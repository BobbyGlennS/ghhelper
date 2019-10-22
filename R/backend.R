#' Title
#'
#' @param gh_res Response from calls to gh()
#'
#' @return
#' @export
#'
#' @examples
gh_response_to_tibble <- function(gh_res) {
  tibble::tibble(data_col = unclass(gh_res)) %>%
    tidyr::unnest_wider(col = "data_col")
}


#' Title
#'
#' @param url Relative endpoint url for Github API
#' @param res_type Type of data returned
#'
#' @return
#' @export
#'
#' @examples
GET_gh <- function(url, res_type = "raw") {
  api_res <- gh::gh(url, .method = "GET", .limit = Inf)

  switch(res_type,
         "raw" = api_res,
         "tibble" = gh_response_to_tibble(api_res))
}

#' Title
#'
#' @param url Relative endpoint url for Github API
#' @param ... Named parameters to be sent as POST body content
#' @param res_type Type of data returned
#'
#' @return
#' @export
#'
#' @examples
POST_gh <- function(url, ..., res_type = "raw") {
  api_res <- gh::gh(url, ..., .method = "POST")

  switch(res_type,
         "raw" = api_res,
         "tibble" = gh_response_to_tibble(api_res))
}
