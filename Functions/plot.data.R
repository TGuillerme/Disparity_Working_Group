#' @title Plot data
#'
#' @description Plot the data sorting
#'
#' @param sorted.data the sorted data per categories
#' @param proportion whether to use proportional data or not (default = FALSE)
#' @param decreasing in which order to plot the categories (default, TRUE, right to left decreasing)
#' @param las las parameter (default = 3)
#' @param table whether to print the table in "R" format (default), "tex" or "md".
#' @param ... optional arguments to be passed to \coe{barplot}
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

plot.data <- function(sorted.data, proportion = FALSE, decreasing = TRUE, las = 3, ...) {

    ## Make the data table
    plot_data <- apply(table(sorted.data), 2, sum)

    ## Proportions
    if(proportion) {
        plot_data <- plot_data/sum(plot_data)*100
    }

    ## Plot the data
    barplot(sort(plot_data, decreasing = decreasing), las = las, ...)

}

table.data <- function(sorted.data, proportion = FALSE, decreasing = TRUE, table = "R", ...) {
    ## Make the data table
    table_data <- apply(table(sorted.data), 2, sum)

    ## Proportions
    if(proportion) {
        table_data <- table_data/sum(table_data)*100
    }
    
    table_data <- t(t(table_data))

    ## Table
    if(table == "R") {
        return(table_data)
    }
    if(table == "latex") {
        return(xtable::xtable(table_data, ...))
    }
    if(table == "md") {
        return(knitr::kable(table_data, ...))
    }
}