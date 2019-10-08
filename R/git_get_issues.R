#' Get repo's issues
#'
#' @param owner Owner of the Github Repo (org or individual user)
#' @param repo Name of the Github Repo
#'
#' @return
#' @export
#'
#' @examples
#' get_issues("xvrdm", "ggrough")
get_issues <- function(owner, repo) {
  GET_gh(
    stringr::str_glue("/repos/{owner}/{repo}/issues?",
                      "state=all", # Default is "open"
    ))
}


#' Title
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
