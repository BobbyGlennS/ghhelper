#' Get repo's milestones
#'
#' @param owner Owner of the Github Repo (org or individual user)
#' @param repo Name of the Github Repo
#' @param res_type Type of data returned
#'
#' @return
#' @export
#'
#' @examples
get_milestones <- function(owner, repo, res_type = "tibble") {
  GET_gh(
    stringr::str_glue("/repos/{owner}/{repo}/milestones"),
    res_type = res_type)
}


#' Create an issue on a repo from R list data
#'
#' @param owner Owner of the Github Repo (org or individual user)
#' @param repo Name of the Github Repo
#' @param milestone_content R List with milestone content
#'
#' @return
#' @export
#'
#' @examples
create_milestone <- function(owner, repo, milestone_content) {
  stopifnot(!is.null(milestone_content$title),
            !is.null(milestone_content$due_on),
            lubridate::is.Date(milestone_content$due_on),
            is.null(milestone_content$url))

  milestone_content$due_on <- format(milestone_content$due_on,
                                     "%Y-%m-%dT%H:%M:%S")

  url <- stringr::str_glue("/repos/{owner}/{repo}/milestones")
  do.call(POST_gh, append(list(url=url), milestone_content))
}

