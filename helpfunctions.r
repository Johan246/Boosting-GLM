m_stat_beta <- function (x, data, i.var = 1, n.trees = x$n.trees) 
{
  if (x$interaction.depth < length(i.var)) {
    stop("interaction.depth too low in model call")
  }
  if (all(is.character(i.var))) {
    i <- match(i.var, x$var.names)
    if (any(is.na(i))) {
      stop("Variables given are not used in gbm model fit: ", 
           i.var[is.na(i)])
    }
    else {
      i.var <- i
    }
  }
  if ((min(i.var) < 1) || (max(i.var) > length(x$var.names))) {
    warning("i.var must be between 1 and ", length(x$var.names))
  }
  if (n.trees > x$n.trees) {
    warning(paste("n.trees exceeds the number of trees in the model, ", 
                  x$n.trees, ". Using ", x$n.trees, " trees.", sep = ""))
    n.trees <- x$n.trees
  }
  unique.tab <- function(z, i.var) {
    a <- unique(z[, i.var, drop = FALSE])
    a$n <- table(factor(apply(z[, i.var, drop = FALSE], 
                              1, paste, collapse = "\r"), levels = apply(a, 1, 
                                                                         paste, collapse = "\r")))
    return(a)
  }
  for (j in i.var) {
    if (is.factor(data[, x$var.names[j]])) 
      data[, x$var.names[j]] <- as.numeric(data[, x$var.names[j]]) - 
        1
  }
  a <- apply(expand.grid(rep(list(c(FALSE, TRUE)), length(i.var)))[-1, 
  ], 1, function(x) as.numeric(which(x)))
  FF <- vector("list", length(a))
  for (j in 1:length(a)) {
    FF[[j]]$Z <- data.frame(unique.tab(data, x$var.names[i.var[a[[j]]]]))
    FF[[j]]$n <- as.numeric(FF[[j]]$Z$n)
    FF[[j]]$Z$n <- NULL
    FF[[j]]$f <- .Call("gbm_plot", X = as.double(data.matrix(FF[[j]]$Z)), 
                       cRows = as.integer(nrow(FF[[j]]$Z)), cCols = as.integer(ncol(FF[[j]]$Z)), 
                       n.class = as.integer(x$num.classes), i.var = as.integer(i.var[a[[j]]] - 
                                                                                 1), n.trees = as.integer(n.trees), initF = as.double(x$initF), 
                       trees = x$trees, c.splits = x$c.splits, var.type = as.integer(x$var.type), 
                       PACKAGE = "gbm")
    FF[[j]]$f <- matrix(FF[[j]]$f, ncol = x$num.classes, 
                        byrow = FALSE)
    FF[[j]]$f <- apply(FF[[j]]$f, 2, function(x, w) {
      x * weighted.mean(x, w, na.rm = TRUE) # THis row is changed!
    }, w = FF[[j]]$n)
    FF[[j]]$sign <- ifelse(length(a[[j]])%%2 == length(i.var)%%2, 
                           1, -1)
  }
  H <- FF[[length(a)]]$f
  for (j in 1:(length(a) - 1)) {
    i1 <- apply(FF[[length(a)]]$Z[, a[[j]], drop = FALSE], 
                1, paste, collapse = "\r")
    i2 <- apply(FF[[j]]$Z, 1, paste, collapse = "\r")
    i <- match(i1, i2)
    H <- H + with(FF[[j]], sign * f[i, ])
  }
  w <- matrix(FF[[length(a)]]$n, ncol = 1)
  f <- matrix(FF[[length(a)]]$f^2, ncol = x$num.classes, byrow = FALSE)
  top <- apply(H^2, 2, weighted.mean, w = w, na.rm = TRUE)
  btm <- apply(f, 2, weighted.mean, w = w, na.rm = TRUE)
  H <- top/btm
  if (x$distribution$name == "multinomial") {
    names(H) <- x$classes
  }
  H[H > 1] <- NaN
  return(sqrt(H))
}
