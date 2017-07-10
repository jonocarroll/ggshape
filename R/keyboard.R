#' Create a keyboard layout ggplot
#'
#' @param user_plot bare name of a user-supplied plotting function. See details.
#' @param user_scale overall scaling factor and unit, e.g. "2cm". See details.
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
#' @return (invisibly) a gtable containing multiple grobs defining the keyboard
#'   plot.
#' @export
#'
#' @examples
#' \dontrun{
#' rw <- plot_keyboard(ggshape_random_walk)
#' ggsave(rw, filename = "random.png", width = 16, height = 6)
#'
#' eng <- plot_keyboard(ggshape_english_usage)
#' ggsave(eng, filename = "english.png", width = 16, height = 6)
#'
#' langs <- plot_keyboard(ggshape_lang_diffs)
#' ggsave(langs, filename = "languages.png", width = 16, height = 6)
#' }
plot_keyboard <- function(user_plot, user_scale = "2cm") {

    ## use user_plot to generate the individual letter plots
    ## these should already be facetted by some variable
    letter_key <- function(x) {
        grid::grobTree(
            ggplot2::ggplotGrob(user_plot(x) +
                                    ggplot2::theme_void() +
                                    ggplot2::guides(fill = FALSE, alpha = FALSE, col = FALSE, size = FALSE, shape = FALSE)),
            grid::textGrob(x, gp = grid::gpar(cex = 1.5))
        )
    }

    common_scale <- readr::parse_number(user_scale)
    common_unit <- stringr::str_extract(user_scale, "[^\\d]+")

    ## system keys have no plot, but a label
    sys_key <- function(x) {
        grid::grobTree(
            grid::rectGrob(gp = grid::gpar(fill = "grey90", col = NULL)),
            grid::textGrob(x)
        )
    }

    ## spaces have no labels and are invisible
    space <- grid::rectGrob(gp = grid::gpar(fill = NULL, col = NULL, alpha = 0))

    ## number row
    num_keys <- c(strsplit("~1234567890-=", "")[[1]], "Backspace")
    num_row <- vector("list", 27)
    for (i in seq(1, 27, 2)) {
        num_row[[i]] <- sys_key(num_keys[(i + 1)/2])
    }
    for (i in seq(2, 26, 2)) {
        num_row[[i]] <- space
    }

    ## first row
    q_keys <- strsplit("qwertyuiop", "")[[1]]
    q_row <- vector("list", 27)
    q_row[[1]] <- sys_key("TAB")
    q_row[[23]] <- sys_key("[")
    q_row[[25]] <- sys_key("]")
    q_row[[27]] <- sys_key("\\")
    for (i in c(seq(2, 26, 2))) {
        q_row[[i]] <- space
    }
    for (i in c(seq(3, 21, 2))) {
        q_row[[i]] <- letter_key(q_keys[(i - 1)/2])
    }

    ## home row
    a_keys <- strsplit("asdfghjkl", "")[[1]]
    a_row <- vector("list", 25)
    a_row[[1]] <- sys_key("CAPS")
    a_row[[21]] <- sys_key(";")
    a_row[[23]] <- sys_key("'")
    a_row[[25]] <- sys_key("ENTER")
    for (i in c(seq(2, 24, 2))) {
        a_row[[i]] <- space
    }
    for (i in c(seq(3, 19, 2))) {
        a_row[[i]] <- letter_key(a_keys[(i - 1)/2])
    }

    ## bottom row
    z_keys <- strsplit("zxcvbnm", "")[[1]]
    z_row <- vector("list", 23)
    z_row[[1]] <- sys_key("Shift")
    z_row[[17]] <- sys_key("<")
    z_row[[19]] <- sys_key(">")
    z_row[[21]] <- sys_key("?")
    z_row[[23]] <- sys_key("Shift")
    for (i in c(seq(2, 22, 2))) {
        z_row[[i]] <- space
    }
    for (i in c(seq(3, 15, 2))) {
        z_row[[i]] <- letter_key(z_keys[(i - 1)/2])
    }

    ## space bar row
    space_keys <- c("Ctrl", "Fn", "Win", "Alt", "Space", "Alt", "Menu", "Ctrl")
    space_row <- vector("list", 16)
    for (i in seq(1, 15, 2)) {
        space_row[[i]] <- sys_key(space_keys[(i + 1)/2])
    }
    for (i in seq(2, 14, 2)) {
        space_row[[i]] <- space
    }
    space_row[[16]] <- space

    ## key widths
    num_widths <- c(1.45, rep(c(0.2, 1), 12), 0.2, 2.55)*common_scale
    q_widths <- c(2, rep(c(0.2, 1), 12), 0.2, 2)*common_scale
    a_widths <- c(2.35, rep(c(0.2, 1), 11), 0.2, 2.85)*common_scale
    z_widths <- c(3, rep(c(0.2, 1), 10), 0.2, 3.4)*common_scale
    space_widths <- c(1.8, rep(c(0.2, 1), 3), 0.2, 5.8, rep(c(0.2, 1), 3), 3.6)*common_scale

    ## arrange each row
    num_row_grob   <- gridExtra::arrangeGrob(grobs = num_row,     nrow = 1, widths = grid::unit(num_widths, common_unit))
    q_row_grob     <- gridExtra::arrangeGrob(grobs = q_row,       nrow = 1, widths = grid::unit(q_widths, common_unit))
    a_row_grob     <- gridExtra::arrangeGrob(grobs = a_row,       nrow = 1, widths = grid::unit(a_widths, common_unit))
    z_row_grob     <- gridExtra::arrangeGrob(grobs = z_row,       nrow = 1, widths = grid::unit(z_widths, common_unit))
    space_row_grob <- gridExtra::arrangeGrob(grobs = space_row,   nrow = 1, widths = grid::unit(space_widths, common_unit))
    gap_grob       <- gridExtra::arrangeGrob(grobs = list(space), nrow = 1, widths = grid::unit(sum(q_widths), common_unit))

    ## combine rows into a keyboard
    p <- gridExtra::grid.arrange(num_row_grob, gap_grob,
                                 q_row_grob, gap_grob,
                                 a_row_grob, gap_grob,
                                 z_row_grob, gap_grob,
                                 space_row_grob,
                                 nrow = 9,
                                 heights = grid::unit(c(rep(c(1, 0.2), 4), 1)*common_scale, common_unit))
    # grid::grid.draw(p)
    return(invisible(p))

}
