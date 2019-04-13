Q <- quote
E <- eval


expr <- Q(x + 1)

with(list(x = 100),
     E(expr))


# defn_ defining a private function?



#' @param times number of times to execute the function
#' @param f function to execute
#' @export
repeatedly <- function(times, f){
        for (i in 1:times){f()}
}



#'
#' @param ... dot-dot-dot
juxt <- function(...){
        fs <- lapply(list(...), match.fun)
        f_names <- as.character(match.call(expand.dots = FALSE)$...)
        # fs <- list()
        # for (i in seq_along(dots)){
        #         fs <- c(fs, eval(dots[[i]]))
        # }

        ## currently don't know how to capture function names inside a list
        # f_names <- list()
        # dot_names <- match.call(expand.dots = FALSE)$...

        ## inspiration from bquote?? .() to expand

        function(...){
                out <- list()
                for(f in fs) {
                        out <- c(out, list(f(...)))
                }
                names(out) <- f_names
                out
        }
}


juxt(mean, sd, summary) (rnorm(100))

#' @description returns x, disregarding any other parameters
#' @param x
#' @param ... dot-dot-dot
#' @export
constantly <- function(x, ...) {
        x
}





#' @description compose, from purrr::compose()
#' @param ... dot-dot-dot
#' @export
comp <- function(...){
        fs <- lapply(list(...), match.fun)
        n <- length(fs)
        last <- fs[[n]]
        rest <- fs[-n]
        function(...) {
                out <- last(...)
                for (f in rev(rest)) {
                        out <- f(out)
                }
                out
        }
}



thread <- function(x, ...){
        ## -> "stabby" in lisp
        fs <- sapply(list(...), match.fun)
        out <- x
        for(f in fs) {
                out <- f(out)
        }
        out
} # add tidyish style, formula for function args.



#' @export
if_ <- `if`


#' @export
when <- function(condition, ...) {



}

#' @export
delay <- Sys.sleep




#' @export
promise <- delayedAssign




deliver




provide <- function(...){
        dots <- match.call(expand.dots = FALSE)$...





}



#' @export
shuffle <- function(x){
        sample(x, length(x), replace = FALSE)
}



rand <- function(n){
        lower <- `if`(n > 0, 0, n)
        higher <- `if`(n > 0, n, 0)
        runif(n = 1, min = lower, max = higher)
}

rand_int <- function(n){
        round(rand(n))
}


# peri

#' @export
defvar <- function(name, value, env = .GlobalEnv){
        name <- deparse(substitute(name))
        if (exists(name, envir = env)) {
                stop(name, " already exists in ", environmentName(env))
        }
        else {
                assign(x = name, value = value, envir = env)
        }
}

#' @export
defn <- function(name, arg, body, note = character(), envir = parent.frame()) {
        fun <- function(){}
        formals(fun) <- as.list(arg)
        body(fun) <- substitute(body)
        attr(fun, "note") <- note
        name <- deparse(substitute(name))
        assign(x = name, value = fun, envir = envir)
}


#' @export
begin <- function(exprs, envir = new.env()){
        for (expr in exprs) {
                eval(expr, envir)
        }
}


# defn(bar, l(a = 1, b, c = 2), p(a + b + c))
# defn(foo,
#       l(a,b),
#       p(def(x, 123),
#         def(y, (`*`(`+`(a, b), 2))),
#         return(x + y)))
#
# foo(20,10)
# bar(b = 1)




# x = 3
#
# let(list(x = 1, y = 2),
#     p(x + y))
#
#
#
# def(exprs, list(
#         qt(p(
#                 def(x,1),
#                 def(y,2)
#         )),
#         qt(def(z, x + y)),
#         qt(print(z))
# ))
#
# prog(exprs)
#
#
#
# `!`({
#         x <- 1
#         b <- 2
#         print(x)
#         print(b)
#
# })
