#R evaluates function arguments lazily, but tries to hide lazy data
#structures away. What would we do with lazy data structures?

#A simplistic "Lazy cons-cell." Takes two slots, defers their computation.
`%,%` <- function(car, cdr) environment();

x <- {print("first"); 1} %,% {print("next"); 2}
x$car
x$car
x$cdr
x$car
x$cdr

#A sequence of lazy cons-cells makes a lazy sequence.
#Example: This function produces a lazy incrementing sequence.
inc <- function(start, step) {
  force(start); force(step);
  start %,% inc(start+step, step)
}

s <- inc(1,1)
s$car
s$cdr$car
s$cdr$cdr$car

#Get the nth element of a lazy sequence.
nthcdr <- function(seq, n) {
  while(n > 0) {n<-n-1; seq <- seq$cdr}
  seq
}
nth <- function(seq, n) nthcdr(seq, n-1)$car

nth(inc(100, 5), 12)

#Collect the first N elements of a sequence.
s_head <- function(seq, n=10, simplify=TRUE) {
  x <- vector("list", n)
  for (i in 1:n) {
    x[[i]] <- seq$car
    seq <- seq$cdr
  }
  if(simplify) simplify2array(x) else x
}

s_head(inc(37, 11))
s_head(inc(0,1))
s_head(inc(0, pi/2), 4)

##Sequences can be defined recursively. Here's an infinite sequence of
##ones, defined in terms of itself.
ones <- 1 %,% ones
s_head(ones)

##Next is higher order sequence functions.

#Map a sequence through a function. Note arguments need to be forced
s_map <- function(seq, f, ...) {
  force(seq); force(f); list(...)
  f(seq$car, ...) %,% s_map(seq$cdr, f, ...)
}

#sequence of squares using s_map
squares <- s_map(inc(1,1), `^`, 2)
s_head(squares)

#"Zip" combines two sequences through a function. Note op, a and b
#need to be forced
zip <- function(op, a, b) {
  force(a); force(b); x <- op(a$car, b$car);
  x %,% zip(op, a$cdr, b$cdr)
}

#two sequences bound together
s_head(zip(c, inc(0, 10), inc(0, 20)))

#Q: Can 'map', 'zip' be easily (efficiently) generalized into
#a 'mapply' of sequences?

#s_reduce is like 'reduce' for lazy sequences. Note recurrence in definition
s_reduce <- function(seq, f) reduced <- seq$car %,% zip(f, seq$cdr, reduced)

#The harmonic sequence:
harm <- s_reduce(s_map(inc(1,1), function(x) 1/x), `+`)
s_head(harm)

#The Fibbonacci sequence is the sequence (1, 1, ...) added to its
#own delay. It 'creates itself out of thin air'
library(gmp)
fib <- as.bigz(1) %,% (1 %,% s_reduce(fib, `+`))
#Or with 'zip':
fib <- as.bigz(1) %,% (1 %,% zip(`+`, fib, fib$cdr))
#Compare the standard line from Haskell tutorials:
#fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

s_head(s_map(fib, as.integer))

#Lazy sequences build their own memoization;
fib <- as.bigz(1) %,% (1 %,% s_reduce(fib, `+`))
mem.1 <- gc()
system.time(nth(fib, 30000))
system.time(nth(fib, 30000))
#this memoization occupies some memory (rather a lot here, and not
#alll the bignums)
((mem.2 <- gc()) - mem.1)[,1:2]
#If you drop your reference to the head of the sequence,
#that memory will be collected;
fib30000 <- nthcdr(fib, 30000-1)
rm("fib")
((mem.3 <- gc()) - mem.1)[,1:2]
#but the sequence can still continue.
nth(fib30000, 5)

##Here's a sequence of sequences. What does it compute?
pt <- ones %,% s_map(pt, s_reduce, `+`)

# It computes Pascal's triangle (opriented toward the upper left corner, here)
s_head(s_map(pt, s_head))
