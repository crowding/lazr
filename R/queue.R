`%:%` <- function(first, rest) environment()
`%'%` <- function(x, y) {x; y; x} #eval in order, return first

#' An impure (modify-by-reference), single-ended queue, implemented using
#' lazy pairs.
#'
#' @return A list with four functions as elements; \code{append},
#' \code{pop}, \push{}, and \code{has}.
#'
#' \itemize{
#' \item{\code{append}}{places \code{v} at the tail of the queue.}
#' \item{\code{pop()}}{removes an element from the head quese and returns it.}
#' \item{\code{push(c)}}{places \code{v} at the head of the queue.}
#' \item{\code{has()}} returns \code{TRUE} if any elements are in the queue.
#' }
#'
#' Removing from the tail of the queue is not supported.
#' (A double-ended queue will be added.)
#'
#' @author Peter Meilstrup
#' @export
refqueue <- function() {
  value <- quote(expr=)
  node <- function() value %:% node()
  head <- node()
  tail <- head
  append <- function(value) {
    #assign a value, and resolve it into the list
    value <<- value
    tail$first #inject value
    tail <<- tail$rest #advance tail
    value <<- quote(expr=)
  }
  #if pop when queue is empty, missing 'value' will throw error
  #and promises remain unresolved

  #urgh, "push" needs a non-lazy pair
  push <- function(value) head <<- list(first=value, rest=head)
  pop <- function() head$first %'% (head <<- head$rest)
  has <- function() !identical(head, tail)
  list(append=append, pop=pop, push=push, has=has)
}
