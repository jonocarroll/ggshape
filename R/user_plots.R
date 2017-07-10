globalVariables("tidy_letters")

#' Example \code{user_plot} functions.
#' @rdname user_plot
#' @param x,letter,text facetting variable
user_plot <- function(x) {
    NULL
}

#' @rdname user_plot
#' @export
ggshape_lang_diffs <- function(letter) {
    if (!exists("tidy_letters", where = parent.frame())) utils::data(tidy_letters, package = "ggshape", envir = environment())
    p <- ggplot2::ggplot(tidy_letters[tidy_letters[["Letter"]] == letter, ]) +
        ggplot2::geom_col(ggplot2::aes_string(x = "Lang", y = "Prop", fill = "Lang")) +
        # ggplot2::geom_text(ggplot2::aes(2.5, 5, label = letter), cex = 8, col = "grey40") +
        ggplot2::coord_cartesian(ylim = c(0, 15))
    return(p)
}

#' @rdname user_plot
#' @export
ggshape_english_usage <- function(letter) {
    # cols <- grDevices::colorRampPalette(c("red", "royalblue"))(101)
    cols <- viridisLite::viridis(101)
    eng_letters <- tidy_letters[tidy_letters[["Lang"]] == "English", ]
    eng_letters[["RelProp"]] <- round(100L*eng_letters[["Prop"]]/max(eng_letters[["Prop"]]))
    this_letter <- eng_letters[eng_letters[["Letter"]] == letter, ]
    p <- ggplot2::ggplot(this_letter) +
        ggplot2::geom_raster(ggplot2::aes(1, 1), fill = cols[this_letter[["RelProp"]] + 1L])
    return(p)
}

#' @rdname user_plot
#' @export
ggshape_random_walk <- function(letter) {
    p <- ggplot2::ggplot(data.frame(x = 1:15, y = stats::runif(15))) +
        ggplot2::geom_line(ggplot2::aes_string("x", "y"), col = "coral")
    return(p)
}

#' @rdname user_plot
#' @export
ggshape_rainbow_text <- function(text) {
    ggplot2::ggplot(data.frame(text = text)) +
        ggplot2::geom_text(ggplot2::aes(x = 1, y = 1),
                           label = text,
                           col = base::sample(grDevices::rainbow(20), 1), cex = 8)
}
