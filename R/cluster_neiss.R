#' Cluster \code{neiss1617}
#'
#' The user can cluster the age groups in \code{neiss1617} after filtering the
#' \code{Narrative_1} using a regular expression.
#'
#' @param narrative_regex A regular expression for filtering \code{neiss1617$Narrative_1}.
#' @param dist_method The method to use for calculating distance.
#' @param hclust_method The method to use for hierarhcical clustering.
#'
#' @return A \code{hclust} object.
#'
#' @importFrom dplyr %>%
#' @export cluster_neiss
cluster_neiss <- function(narrative_regex = ".*", dist_method = "euclidean", hclust_method = "complete") {

    neiss1617_filtered <- neiss1617 %>%
        dplyr::filter(
            stringr::str_detect(stringr::str_to_lower(Narrative_1), narrative_regex)
        ) %>%
        dplyr::group_by(age_group, Product) %>%
        dplyr::count(name = "num_injuries") %>%
        tidyr::spread(Product, num_injuries, fill = 0) %>%
        tibble::column_to_rownames("age_group")

    d <- stats::dist(neiss1617_filtered, method = dist_method)
    hc <- stats::hclust(d, method = hclust_method)

    return(hc)
}

# include the following code, too
utils::globalVariables(
    c("Product", "Narrative_1", "age_group", "neiss1617", "num_injuries"),
    add = TRUE
)
