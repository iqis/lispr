## Package lispr

#' @description constuct a code block, lines sparated by comma
#' @param ... dot dot dot
#' @export
p <- `{`
do <- `{`



#' Construct a list
#'
#' @description construct a list
#' @param ... dot-dot-dot
#' @export
l <- function(..., where = parent.frame()) {
        dots <- as.list(match.call(expand.dots = FALSE)$...)

        # from base::bquote
        unquote <- function(e) if (is.pairlist(e))
                as.pairlist(lapply(e, unquote))
        else if (length(e) <= 1L)
                e
        else if (e[[1L]] == as.name("."))
                eval(e[[2L]], where)
        else as.call(lapply(e, unquote))


        if (length(names(dots)) == 0) {
                for (i in seq_along(dots)) {
                        names(dots)[i] <- as.character(dots[[i]])
                        dots[i] <- alist(k = )
                }
        } else {
                for (i in seq_along(dots)) {
                        if (names(dots)[i] == "") {
                                names(dots)[i] <- as.character(dots[[i]])
                                dots[i] <- alist(k = )
                        }
                }
        }

        for (i in seq_along(dots)) {
                dots[[i]] <- unquote(dots[[i]])
        }

        dots
}


#' @describeIn  an alias for `quote()`
#' @export
qt <- quote


#' @description an alias for `<-`
#' @inherit base::assignOps
#' @export
def <- `<-`


#' Begin something
#'
#' construct a routine as a function, sans side effects
#'
#' @example
#' begin({#define variables
#' x <- 1
#' print(x)
#'
#' begin({# change x to 10, inside this scope
#'         x <- 10
#'         print(x)
#' })()
#'
#' print(x)
#'
#' })()
#'
#' @param ... dot-dot-dot
#' @export
begin <- function(...) {
        exprs <- match.call(expand.dots = FALSE)$...
        exprs <- lapply(exprs, deparse)

        if (exprs[[1]][1] == "{") {
                exprs <- unlist(exprs)
                exprs <- exprs[-1]
                exprs <- exprs[-length(exprs)]
        }

        exprs <- paste(exprs, collapse = ";")
        fun_source <- paste("function(){",
                            exprs,
                            "}")
        fun <- eval(parse(text = fun_source), parent.frame())
        fun
}

#'
#' @description evaluate a block of expression with local binding
#' @example
#' let(l(x = 1, y = 2),
#'     {
#'             z <- x + y
#'             w = rnorm(1, sd = z)
#'             w
#'     }
#' )
#' @export
let <- function(vars, expr) {
        eval(substitute(expr), envir = vars, enclos = parent.frame())
}


#' @description evaluate a block of expressions with local binding, evaluating formulas
#' @export
let_ <- function(vars, expr){
        vars <- lapply(vars, eval)
        eval(substitute(expr), envir = vars, enclos = parent.frame())
}


#' @description evaluate a block of expressions in a new environment, which is returned
#' @export
en <- function(expr){
        env <- new.env()
        eval(substitute(expr), envir = env, enclos = parent.frame())
        env
}

#' Construct a function
#'
#' construct a function with a list for arguments and a code block for body
#'
#' @example
#' def(wow, fn(
#'         l(x = 5, y = 0, z),
#'         p(w <- x + y + rnorm(1) - z, return(w))
#' ))
#' wow(z = 4)
#' @export
fn <- function(arg, body){
        fun <- function(){}
        formals(fun) <- arg
        body(fun) <- substitute(body)
        environment(fun) <- parent.frame()
        fun
}

#' Set the value of a variable
#' @export
set <- function(name, value, env = parent.frame()){
        name <- deparse(substitute(name))
        if (exists(name)) {
                assign(x = name, value = value, envir = env)
        } else {
                stop(name, " does not exist in", environmentName(env))
        }
}


#' @description test for equality of values
#' @example
#' eq(1, 1L)
#' eq(1, "1")
#' @export
eq <- base::`==`


#' @export
equal <- base::identical
