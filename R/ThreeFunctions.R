#' the first function
#' @title Monte Carlo estimate of the Beta(a,b) cdf
#' @description Write a function to compute a Monte Carlo estimate of the Beta(a,b) cdf
#' @param x the necessary value you need to assign before.
#' @param a the first parameter of beta distribution
#' @param b the second parameter of beta distribution
#' @return a estimate value of the CDF of the beta(a,b)
#' @examples
#' \dontrun{
#' t <- MC.Beta(0.2,3,3)
#' t
#' }
#' @export

MC.Beta<- function(x,a,b){
  m=10000     # The number of random Numbers
  n=length(x)
  y=NULL
  if(x<=0||x>=1) stop("x must be in (0,1)")
  else {
    for (i in 1:n) {
      g=x[i]*dbeta(runif(m,min=0,max=x[i]),a,b)  #Generate MC random Numbers
      y[i]=mean(g) #the  estimated values
    }
  }
  return(y)
}
# end function


#' the second function
#' @title a faster version of chisq.test.
#' @description Make a faster version of chisq.test that only computes the chi-square test statistic when the input is two numeric vectors with no missing values.
#' @param x one of the interger vectors of inputs
#' @param y one of the interger vectors of inputs
#' @return a estimate value of the CDF of the beta(a,b)
#' @examples
#' \dontrun{
#' t <- my.chisq.test(C(1,2,3)),c(4,3,5))
#' t
#' }
#' @export

my.chisq.test <- function(x, y){
  # Input
  if (!is.numeric(x)) {
    stop("x must be numeric")}
  if (!is.numeric(y)) {
    stop("y must be numeric")}
  if (length(x) != length(y)) {
    stop("x and y must have the same length")}
  if (length(x) <= 1) {
    stop("length of x must be greater one")}
  if (any(c(x, y) < 0)) {
    stop("all entries of x and y must be greater or equal zero")}
  if (sum(complete.cases(x, y)) != length(x)) {
    stop("there must be no missing values in x and y")}
  if (any(is.null(c(x, y)))) {
    stop("entries of x and y must not be NULL")}

  # Help variables
  m <- rbind(x, y)
  margin1 <- rowSums(m)
  margin2 <- colSums(m)
  n <- sum(m)
  me <- tcrossprod(margin1, margin2) / n

  # Output
  x_stat = sum((m - me)^2 / me)
  dof <- (length(margin1) - 1) * (length(margin2) - 1) # the degree of freedom
  p <- pchisq(x_stat, df = dof, lower.tail = FALSE)

  return(list(x_stat = x_stat, df = dof, `p-value` = p))
}
# end function



#' the third function
#' @title A function to compute the CDF of the Cauchy distribution
#' @description Write a function to compute the cdf of the Cauchy distribution, which has the specific density.
#' @param eta the known parameter with the specific value
#' @param theta the known positive parameter in the density
#' @param x a value that you need to specified
#' @return estimateF= my.pcauchy(x, eta, theta)
#' @examples
#' \dontrun{
#' estimateF = my.pcauchy(4, 0, 2)
#' estimateF
#' }
#' @export
#'
my.pcauchy = function (x, eta, theta) {
  stopifnot(theta > 0)

  my.dcauchy = function (x, eta, theta) {
    stopifnot(theta > 0)
    return(1/(theta*pi*(1 + ((x - eta)/theta)^2)))
  }

  integral = function (x) {
    my.dcauchy(x, eta, theta)
  }

  return(integrate(integral, lower = -Inf, upper = x)$value)
}
#end function


