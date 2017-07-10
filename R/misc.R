#' Estimate the nth Fibonacci number using Binet's Formula
#'
#' @param n which Fibonacci number to estimate
#'
#' @return the nth Fibonacci number
#'
#' @details Binets' Formula estimates the nth Fibonacci number using the Golden Ratio
#'
#' \deqn{F_n = (\phi^n - (-\phi)^{-n})/\sqrt{5}}
#'
#' where \eqn{\phi} is the Golden Ratio
#'
#' \deqn{\phi = (1 + \sqrt{5})/2 ~ 1.618034}
#'
#' @export
#'
#' @examples
#' fibonacci_binet(1:6)
#' # 1 1 2 3 5 8
fibonacci_binet <- function(n) {
    gr <- 0.5*(1 + sqrt(5))
    return((gr^n - (-gr)^-n)/sqrt(5))
}

#' Sum of Fibonacci Terms, Excluding the First
#'
#' @param n how many terms to sum
#'
#' @details This calculates the number of elements arranged in a triangle with
#'   one at the point, and each row containing \eqn{F_n} elements where
#'   \eqn{F_n} is the nth Fibonacci number, calculated using
#'   \code{\link{fibonacci_binet}}.
#'
#' @return the sum of the 2:n Fibonacci terms
#' @export
#'
#' @examples
#' fibonacci_sum(4)
#' # 11
fibonacci_sum <- function(n) {
    return(round(sum(fibonacci_binet(seq_len(n) + 1))))
}
