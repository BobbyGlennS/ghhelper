#' Get a summary of the most recents commits
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

#' Get all data about a commit
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

#' Get all data about a tree
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

#' Get all data about the most recent commit
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


#' Extract the tree's sha of a commit
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

#' List all files included in a tree
#'
#' This function will recursively also list the files in the trees
#' listed in the root tree and so on
#'
#' @param tree_sha Tree JSON already converted to R list
#' @param src_dir Directory path in which the tree files are
#' @param owner Owner of the Github Repo (org or individual user)
#' @param repo Name of the Github Repo
#'
#' @return
#' @export
#'
#' @examples
get_files_from_tree_sha <- function(tree_sha, src_dir = NULL, owner, repo) {
  tree <- ghhelper::get_tree_by_sha(owner, repo, tree_sha)

  tree_list <- purrr::pluck(tree, "tree")

  tree_tib <- tibble::tibble(tree_files = tree_list) %>%
    tidyr::unnest_wider(col = "tree_files")

  if(!is.null(src_dir)) {
    tree_tib$path <- fs::path(src_dir, tree_tib$path)
  }

  dirs <- dplyr::filter(tree_tib, .data$type == "tree")

  if (nrow(dirs) > 0) {
    nested_files <- purrr::map2_df(.x = dirs$sha,
                                   .y = dirs$path,
                                   .f = get_files_from_tree_sha,
                                   owner, repo)
  } else {
    nested_files <- tibble::tibble()
  }

  files <- dplyr::filter(tree_tib, .data$type == "blob")

  dplyr::bind_rows(files, nested_files) %>%
    dplyr::mutate(name = fs::path_file(.data$path))
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
#' get_files_from_repo(owner = "xvrdm", repo = "ggrough")
get_files_from_repo <- function(owner, repo) {
  last_commit <- get_last_commit(owner, repo)
  last_tree_sha <- find_tree_sha_in_commit(last_commit)
  get_files_from_tree_sha(last_tree_sha, owner = owner, repo = repo)
}


