#' Get repo's issues
#'
#' @param owner Owner of the Github Repo (org or individual user)
#' @param repo Name of the Github Repo
#' @param res_type Type of data returned
#'
#' @return
#' @export
#'
#' @examples
#' get_issues("xvrdm", "ggrough")
get_issues <- function(owner, repo, res_type = "tibble") {
  GET_gh(
    stringr::str_glue("/repos/{owner}/{repo}/issues?",
                      "state=all", # Default is "open"
    ), res_type = res_type)
}


#' Create an issue on a repo from R list data
#'
#' @param owner Owner of the Github Repo (org or individual user)
#' @param repo Name of the Github Repo
#' @param issue_content R List with issue content
#'
#' @return
#' @export
#'
#' @examples
create_issue <- function(owner, repo, issue_content) {
  stopifnot(!is.null(issue_content$title),
            is.null(issue_content$url))
  url <- stringr::str_glue("/repos/{owner}/{repo}/issues")
  do.call(POST_gh, append(list(url=url), issue_content))
}

#' Close an issue
#'
#' @param owner Owner of the Github Repo (org or individual user)
#' @param repo Name of the Github Repo
#' @param issue_number Number id of the issue
#'
#' @return
#' @export
#'
#' @examples
close_issue <- function(owner, repo, issue_number) {
  url <- stringr::str_glue("/repos/{owner}/{repo}/issues/{issue_number}")
  do.call(PATCH_gh, append(list(url=url), list(state = "closed")))
}

