## letter frequencies
# library(rvest)
# library(dplyr)
# library(readr)
# library(magrittr)
#
# letter_freqs <-  "https://en.wikipedia.org/wiki/Letter_frequency" %>%  # take the URL
#     read_html() %>%               # read the HTML source
#     html_nodes('.wikitable') %>%  # extract the nodes with class 'wikitable'
#     extract2(3) %>%               # extract the relative frequencies table
#     html_table() %>%              # convert the table to a data.frame
#     .[1:26, 1:5] %>%              # extract rows for a-z and first 4 languages
#     rename(French = `French [20]`,
#            German = `German [21]`,
#            Spanish = `Spanish [22]`) %>%
#     mutate_at(2:5, readr::parse_number) # remove percent signs
#
# tidy_letters <- tidyr::gather(letter_freqs, Lang, Prop, -Letter)
# save(tidy_letters, file = "tidy_letters.rda")

#' Relative Proportions of Usage of a-z in English, French, German, and Spanish.
#'
#' A tidied dataset comparing the relative frequencies of letters from four languages.
#'
#' @format A \code{data.frame} with 104 rows and 3 variables:
#' \describe{
#'   \item{Letter}{letter of the alphabet, a-z}
#'   \item{Lang}{languge in which the letter is used}
#'   \item{Prop}{relative proportion of use of the letter}
#' }
#' @source \url{https://en.wikipedia.org/wiki/Letter_frequency}
"tidy_letters"
