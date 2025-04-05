#' Create a bar plot
#'
#' Create a bar plots for to visualize the distribution of a categorical variable in a data frame.
#' It ensures the input dataset is valid and that the selected column is a factor before plotting.
#'
#' @param dataset A data frame containing the categorical variable.
#' @param x The name of the column to be visualized (as a string).
#' @param x_name A user-friendly label for the x-axis.
#'
#' @return A ggplot2 object displaying the bar plot.
#'
#' @export
#'
#' @examples
#' # example code
#' barplot <- generate_barplot(dataset = mtcars, x = "gear", x_name = "Gear")
#'
#' \dontrun{
#' barplot <- generate_barplot(dataset = mtcars, x = "gear", x_name = gear)
#'
# Function to create a bar plot for the class distribution
#' }
generate_barplot <- function(dataset, x, x_name) {
    # Check if dataset is a data frame
    if (is.null(dataset) || !is.data.frame(dataset)) {
        stop("Dataset must be a data frame!")
    }

    # Check if x column exists in the dataset
    if (!(x %in% names(dataset))) {
        stop(paste("The column", x, "does not exist in the dataset."))
    }

    dataset[[x]] <- as.factor(dataset[[x]])

    class_distribution <- ggplot2::ggplot(dataset, ggplot2::aes(x = !!rlang::sym(x), fill = !!rlang::sym(x))) +
        ggplot2::geom_bar() +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::labs(
            title = paste("Distribution of", x_name),
            x = x_name,
            y = "Count"
        ) +
        ggplot2::theme(
            axis.text = ggplot2::element_text(size = 12),
            axis.title = ggplot2::element_text(size = 14),
            plot.title = ggplot2::element_text(size = 16, face = "bold")
        )

    return(class_distribution)
}
