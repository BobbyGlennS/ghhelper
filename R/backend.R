#' Title
#'
#' @param url Relative endpoint url for Github API
#'
#' @return
#' @export
#'
#' @examples
GET_gh <- function(url) {
  gh::gh(url, .method = "GET")
}

#' Title
#'
#' @param url Relative endpoint url for Github API
#' @param ... Named parameters to be sent as POST body content
#'
#' @return
#' @export
#'
#' @examples
POST_gh <- function(url, ...) {
  gh::gh(url, ..., .method = "POST")
}
