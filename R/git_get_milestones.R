#' Get repo's milestones
#'
#' @param owner Owner of the Github Repo (org or individual user)
#' @param repo Name of the Github Repo
#'
#' @return
#' @export
#'
#' @examples
get_milestones <- function(owner, repo) {
  GET_gh(
    stringr::str_glue("/repos/{owner}/{repo}/milestones"))
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
            is.null(milestone_content$url))
  url <- stringr::str_glue("/repos/{owner}/{repo}/milestones")
  do.call(POST_gh, append(list(url=url), milestone_content))
}
