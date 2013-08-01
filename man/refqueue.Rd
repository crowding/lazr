\name{refqueue}
\alias{refqueue}
\title{An impure (modify-by-reference), single-ended queue, implemented using
lazy pairs.}
\usage{
  refqueue()
}
\value{
  A list with four functions as elements; \code{append},
  \code{pop}, \push{}, and \code{has}.

  \itemize{ \item{\code{append}}{places \code{v} at the
  tail of the queue.} \item{\code{pop()}}{removes an
  element from the head quese and returns it.}
  \item{\code{push(c)}}{places \code{v} at the head of the
  queue.} \item{\code{has()}} returns \code{TRUE} if any
  elements are in the queue. }

  Removing from the tail of the queue is not supported. (A
  double-ended queue will be added.)
}
\description{
  An impure (modify-by-reference), single-ended queue,
  implemented using lazy pairs.
}
\author{
  Peter Meilstrup
}

