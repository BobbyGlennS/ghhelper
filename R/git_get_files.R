#' Title
#'
#' @param owner Owner of the Github Repo (org or individual user)
#' @param repo Name of the Github Repo
#'
#' @return
#' @export
#'
#' @examples
get_last_commits_summary <- function(owner, repo) {
  GET_gh(
    stringr::str_glue("/repos/{owner}/{repo}/commits?sha=master"))
}

#' Title
#'
#' @param owner Owner of the Github Repo (org or individual user)
#' @param repo Name of the Github Repo
#' @param commit_sha Sha of a the commit of interest
#'
#' @return
#' @export
#'
#' @examples
get_commit_by_sha <- function(owner, repo, commit_sha) {
  GET_gh(
    stringr::str_glue("/repos/{owner}/{repo}/git/commits/{commit_sha}"))
}

#' Title
#'
#' @param owner Owner of the Github Repo (org or individual user)
#' @param repo Name of the Github Repo
#' @param tree_sha Sha of a the tree of interest
#'
#' @return
#' @export
#'
#' @examples
get_tree_by_sha <- function(owner, repo, tree_sha) {
  GET_gh(
    stringr::str_glue("/repos/{owner}/{repo}/git/trees/{tree_sha}"))
}

#' Title
#'
#' @param owner Owner of the Github Repo (org or individual user)
#' @param repo Name of the Github Repo
#'
#' @return
#' @export
#'
#' @examples
get_last_commit <- function(owner, repo) {
  last_commits <- get_last_commits_summary(owner, repo)

  last_commit_sha <- last_commits[[1]] %>%
    purrr::pluck("sha")

  last_commit <- get_commit_by_sha(owner, repo, last_commit_sha)
  last_commit
}


#' Title
#'
#' @param commit  Commit JSON already converted to R list
#'
#' @return
#' @export
#'
#' @examples
find_tree_sha_in_commit <- function(commit){
  commit %>%
    purrr::pluck("tree", "sha")
}

#' Title
#'
#' @param tree Tree JSON already converted to R list
#'
#' @return
#' @export
#'
#' @examples
get_files_from_tree <- function(tree) {
  dirs <- tree %>%
    purrr::pluck("tree") %>%
    purrr::keep( ~ .$type == "tree") %>%
    purrr::map_chr("url")

  if (length(dirs) > 0) {
    nested_files <- purrr::map(dirs, get_files_from_tree)
  } else {
    nested_files <- list()
  }

  files <- tree %>%
    purrr::pluck("tree") %>%
    purrr::map("path")

  append(files, purrr::flatten(nested_files))
}

#' Get the names of all files in repo
#'
#' @param owner Owner of the Github Repo (org or individual user)
#' @param repo Name of the Github Repo
#'
#' @return
#' @export
#'
#' @examples
#' get_files_from_repo("xvrdm", "ggrough")
get_files_from_repo <- function(owner, repo) {
  last_commit <- get_last_commit(owner, repo)
  last_tree_sha <- find_tree_sha_in_commit(last_commit)
  current_repo_tree <- get_tree_by_sha(owner, repo, last_tree_sha)
  current_repo_files <- get_files_from_tree(current_repo_tree) %>%
    purrr::flatten_chr()
  current_repo_files
}


