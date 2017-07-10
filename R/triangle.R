#' Create a triangle layout ggplot
#'
#' @param elements facetting variable elements. Must equal
#'   \code{\link{fibonacci_sum}(nrow)}.
#' @param user_plot bare name of a user-supplied plotting function. See details.
#' @param nrow number of rows to generate.
#' @param user_scale overall scaling factor and unit, e.g. "2cm". See details.
#' @param gap_size size of the gap between rows and plots in a row (bare
#'   quantity, e.g. 0.2). If missing, this will be taken as 1/4 of
#'   \code{user_scale}.
#' @param point should the point of the triangle be at the top or the bottom?
#'
#' @details
#'
#' The user-supplied function should take a single argument (name is not
#' important, but e.g. letter) which filters the plot to just the relevant
#' letter. It should return the ggplot object. See demo(random_walk).
#'
#' The scale is a text string, e.g. "2cm" which will be separated into a numeric
#' and text component. The numeric component will scale the plot, while the text
#' component sets the units.
#'
#' @return (invisibly) a gtable containing multiple grobs defining the triangle
#'   plot.
#' @export
#'
#' @examples
#' \dontrun{
#' rw <- plot_triangle(letters[1:19], ggshape_random_walk,
#'                     nrow = 5, user_scale = "3cm", gap_size = 0.2,
#'                     point = "bottom")
#' ggsave(rw, filename = "random_triangle_down.png", width = 12, height = 8)
#' rw <- plot_triangle(letters[1:19], ggshape_random_walk,
#'                     nrow = 5, user_scale = "3cm", gap_size = 0.2,
#'                     point = "top")
#' ggsave(rw, filename = "random_triangle_up.png", width = 12, height = 8)
#' lang <- plot_triangle(letters[1:19], ggshape_lang_diffs,
#'                       nrow = 5, user_scale = "3cm", gap_size = 0.2,
#'                       point = "top")
#' ggsave(lang, filename = "languages_triangle_up.png", width = 12, height = 8)
#' eng <- plot_triangle(letters[1:19], ggshape_english_usage,
#'                      nrow = 5, user_scale = "3cm", gap_size = 0.2,
#'                      point = "top")
#' ggsave(eng, filename = "english_triangle_up.png", width = 12, height = 8)
#' nums <- plot_triangle(1:11, ggshape_rainbow_text,
#'                       nrow = 4, user_scale = "3cm", gap_size = 0.2,
#'                       point = "top")
#' ggsave(nums, filename = "rainbow_nums_triangle_up_11.png", width = 12, height = 8)
#' nums <- plot_triangle(1:32, ggshape_rainbow_text,
#'                       nrow = 6, user_scale = "3cm", gap_size = 0.2,
#'                       point = "top")
#' ggsave(nums, filename = "rainbow_nums_triangle_up_32.png", width = 12, height = 8)
#' }
plot_triangle <- function(elements, user_plot, nrow,
                          user_scale = "4cm",
                          gap_size,
                          point = c("top", "bottom")) {

    if (! point %in% c("top", "bottom")) {
        stop("point should be either at the 'top' or 'bottom'")
    }

    element_width <- readr::parse_number(user_scale)
    common_unit <- stringr::str_extract(user_scale, "[^\\d]+")

    ## clean up the users plot
    clean_user_plot <- function(x) {
        user_plot(x) +
            ggplot2::theme_void() +
            ggplot2::guides(fill = FALSE, alpha = FALSE, col = FALSE, size = FALSE, shape = FALSE)
    }

    ## for looping over elements
    j <- 1

    ## number of plots to draw on each row
    n_each_row <- round(fibonacci_binet(seq_len(nrow) + 1))
    if (point == "bottom") n_each_row <- rev(n_each_row)

    ## requires the right number of elements
    if (length(elements) != sum(n_each_row)) {
        stop("number of elements needs to equal fibonacci number corresponding to nrow.
             fibonacci_sum(nrow) to determine how many that is.")
    }

    ## spaces have no labels and are invisible
    # space <- rectGrob(gp = gpar(fill = "grey40", col = "grey60", alpha = 1)) ## debug
    space <- grid::rectGrob(gp = grid::gpar(fill = NULL, col = NULL, alpha = 0))

    rows <- vector("list")
    for (r in seq_along(n_each_row)) {

        ## create enough elements to house the n plots,
        ## (n-1) gaps, and
        ## 2 sides
        ## i.e. 2n + 1 elements
        n_elements <- 2L*n_each_row[r] + 1
        rows[[r]] <- vector("list", length = n_elements)

        ## populate the row
        rows[[r]][[1]] <- space

        for (k in seq(2, n_elements - 1, 2)) {
            rows[[r]][[k]] <- clean_user_plot(elements[j])
            j <- j + 1L
        }
        if (n_elements > 3L) {
            for (k in seq(3, n_elements - 2, 2)) {
                rows[[r]][[k]] <- space
            }
        }

        rows[[r]][[n_elements]] <- space
    }

    nrows <- max(seq_along(n_each_row))
    rowGrobs <- vector("list", length = nrows)
    gap_width <- ifelse(missing(gap_size), 0.25*element_width, gap_size)
    max_width <- max(n_each_row)*element_width + (max(n_each_row) - 1L)*gap_width + 2L*gap_width

    ## calculate spacings
    for (r in seq_along(n_each_row)) {

        side_width <- 0.5*(max_width - n_each_row[r]*element_width - (n_each_row[r] - 1L)*gap_width)

        if (n_each_row[r] > 1) {
            widths <- c(side_width, element_width, rep(c(gap_width, element_width), n_each_row[r] - 1L), side_width)
        } else {
            widths <- c(side_width, element_width, side_width)
        }

        rowGrobs[[r]] <- gridExtra::arrangeGrob(grobs = rows[[r]], nrow = 1,
                                                widths = grid::unit(widths, common_unit),
                                                heights = grid::unit(element_width, common_unit))

    }

    ## inject spaces between rows
    spaceRow <- gridExtra::arrangeGrob(grobs = list(space), nrow = 1, ncol = 1,
                                       widths = grid::unit(max_width, common_unit),
                                       heights = grid::unit(gap_width, common_unit))
    for (s in rev(seq(1, length(n_each_row) - 1L))) {
        rowGrobs <- append(rowGrobs, list(spaceRow), after = s)
    }

    rowGrobs <- append(rowGrobs, list(nrow = 2L*length(n_each_row) - 1L,
                                      ncol = 1L,
                                      heights = grid::unit(
                                          c(rep(c(element_width, gap_width),
                                                length(n_each_row) - 1L),
                                            element_width),
                                          common_unit)))

    p <- do.call(gridExtra::grid.arrange, rowGrobs)
    # grid::grid.draw(p)
    return(invisible(p))
}
