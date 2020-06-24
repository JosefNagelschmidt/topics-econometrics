library("grf")
library("ggplot2")
library("repr")

show_video <- function(filename, mimetype) {
    library(IRdisplay)
    library(base64enc)

    data = base64encode(filename, 'raw')

    display_html(paste0('<video controls src="data:',
         mimetype, ';base64,', data, '">'))
}

plot_func <- function(type) {
    p <- 20
    n <- 1000
    sigma <- sqrt(20)

    mu <- function(x){ log(1 + exp(6 * x)) }
    set.seed(6212)
    X <- matrix(runif(n * p, -1, 1), nrow = n)
    Y <- mu(X[,1]) + sigma * rnorm(n)

    set.seed(40121)
    X.test <- matrix(runif(n * p, -1, 1), nrow = n)
    ticks <- seq(-1, 1, length = n)
    X.test[,1] <- ticks
    truth <- mu(ticks)

    #forest <- regression_forest(X, Y)
    forest <- regression_forest(X, Y, tune.parameters = "all")
    preds.forest <- predict(forest, X.test)$predictions

    df <- data.frame(cbind(ticks, truth, preds.forest))
    options(repr.plot.width = 15, repr.plot.height = 7.5, repr.plot.res = 100)
    g1 <- ggplot(df, aes(ticks)) +
      geom_point(aes(y = preds.forest, color = "Regression Forest"), show.legend = F, size = 0.6) +
      geom_line(aes(y = truth)) +
      geom_line(aes(y = ksmooth(X[,1], Y, "normal", bandwidth = 0.6, x.points= ticks)[[2]] ,color = "NW estimator")) +
      xlab("x") + ylab("y") + theme_bw()

    #ll.forest <- ll_regression_forest(X, Y, enable.ll.split = TRUE, num.trees = 2000, tune.parameters = "all")
    ll.forest <- ll_regression_forest(X, Y, enable.ll.split = TRUE, num.trees = 2000)
    tuned.lambda <- tune_ll_regression_forest(ll.forest, linear.correction.variables = 1)
    preds.llf <- predict(ll.forest, X.test,
      		   linear.correction.variables = 1, ll.lambda= tuned.lambda$lambda.min)$predictions

    df.llf <- data.frame(cbind(ticks, truth, preds.llf))
    options(repr.plot.width = 15, repr.plot.height = 7.5, repr.plot.res = 100)
    g2 <- ggplot(df.llf, aes(ticks)) +
      geom_point(aes(y = preds.forest, color = "Regression Forest"), show.legend = F, size = 0.6) +
      geom_point(aes(y = preds.llf, color = "Local Linear Forest"), show.legend = F, size = 0.6) +
      geom_line(aes(y = truth)) +
      geom_line(aes(y = ksmooth(X[,1], Y, "normal", bandwidth = 0.7, x.points= ticks)[[2]] ,color = "NW estimator")) +
      xlab("x") + ylab("y") + theme_bw()

    if (type == "g1") {
      return(g1)
    } else {
      return(g2)
    }
}
