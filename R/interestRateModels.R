#### Interest Rate Model ####

#' @name iratemodel
#' @rdname iratemodel
#'
#' @title Interest Rate Model
#'
#' @description The following \code{iratemodel} classes are available:
#' 
#' - \code{ou}: Ornstein-Uhlenbeck process
#' 
#' - \code{gbm}: Geometric Brownian motion
#'
#' Each class can be used to model the force of interest, \eqn{\delta_{s}}.
#'
#' @param params A list with the parameter values to use
#' @param class Either "ou" or "gbm"
#'
#' @details For information on how to use the \code{iratemodel} function see
#' the examples.
#'
#' Define:
#' 
#' \deqn{y(t) = int_{0}^{t} \delta_{s}}
#' \deqn{pv(t) = exp{-y(t)}}
#' 
#' The functions \code{y.ev}, \code{y.var} and \code{y.cov} are available for each class.
#' 
#' @references
#'   Parker, Gary. An application of stochastic interest rate models in
#'   life assurance. Diss. Heriot-Watt University, 1992.
NULL

#' @rdname iratemodel
#' @export
iratemodel <- function(params, class)
{
  class(params) = append(class(params),"iratemodel")
  
  if(class == "ou")
  {
    class(params) = append(class(params),"ou")
    
    return(params)
  }
  else if(class == "gbm")
  {
    class(params) = append(class(params),"gbm")
    
    return(params)
  }
  else 
  {
    stop("unknown class type")
  }
}

#' @rdname iratemodel
#' @export
delta.ev <- function(t, irm)
{
  UseMethod("delta.ev", irm)
}

#' @rdname iratemodel
#' @export
delta.var <- function(t, irm)
{
  UseMethod("delta.var", irm)
}

#' @rdname iratemodel
#' @export
y.ev <- function(t, irm)
{
  UseMethod("y.ev", irm)
}

#' @rdname iratemodel
#' @export
y.var <- function(t, irm)
{
  UseMethod("y.var", irm)
}

#' @rdname iratemodel
#' @export
y.cov <- function(s, t, irm)
{
  UseMethod("y.cov", irm)
}

#' @rdname iratemodel
#' @export
pv.moment <- function(t, moment, irm)
{
  mu = -y.ev(t, irm)
  sigma2 = y.var(t, irm)
  
  exp(mu * moment + 0.5 * sigma2 * moment^2)
}

#' @rdname iratemodel
#' @export
pv.ev <- function(t, irm)
{
  pv.moment(t, moment = 1, irm)
}

#' @rdname iratemodel
#' @export
pv.var <- function(t, irm)
{
  pv.moment(t, moment = 2, irm) - pv.moment(t, moment = 1, irm)^2
}

#' @rdname iratemodel
#' @export
pv.cov <- function(s, t, irm)
{
  mu <- -(y.ev(s, irm) + y.ev(t, irm))
  sigma2 <- y.var(s,irm) + y.var(t,irm) + 2*y.cov(s,t,irm)
  EXY <- exp(mu + 0.5*sigma2)
  EXEY <- pv.ev(s, irm) * pv.ev(t, irm)
  
  EXY - EXEY
}

#### Geometric Brownian Motion ####

#' @name gbmprocess
#' @rdname gbmprocess
#'
#' @title The Geometric Brownian Motion Process
#'
#' @description Expected value, variance and covariance for the Ornstein-Uhlenbeck 
#' process with current force of interest equal to \code{delta0}, ultimate force of
#' interest equal to \code{delta}, friction parameter equal to \code{alpha},
#' and diffusion coefficient equal to \code{sigma}.
#'
#' @details The Geometric Brownian Motion process is defined by
#'
#' \deqn{delta_t = \delta_{0} + \delta_s - \delta) dt + \sigma dW_t}
#'
#' where \eqn{W_t} is a standard Wiener process with parameter \eqn{\sigma = 1}
#' and \eqn{\alpha}, \eqn{\sigma} and \eqn{\delta} are constants with
#' \eqn{\alpha \ge 0} and \eqn{\sigma \ge 0}.
#'
#' Define \eqn{y(t)} as
#'
#' \deqn{y(t) = \int_{0}^{t} \delta_s ds}
#'
#' @return \code{y.ev} gives the expected value, \code{y.var}
#' gives the variance and \code{y.cov} gives the covariance.
#' 
#' @references
#'   Parker, Gary. An application of stochastic interest rate models in
#'   life assurance. Diss. Heriot-Watt University, 1992.
NULL

#' @rdname gbmprocess
#' @export
delta.ev.gbm <- function(t, irm)
{
  irm$delta0 * t^0 # doesn't depend on t
}

#' @rdname gbmprocess
#' @export
delta.var.gbm <- function(t, irm)
{
  irm$sigma^2 * t
}

#' @rdname gbmprocess
#' @export
y.ev.gbm <- function(t, irm)
{
  irm$delta0 * t
}

#' @rdname gbmprocess
#' @export
y.cov.gbm <- function(s, t, irm)
{
  irm$sigma^2 * (min(s,t)^2 * max(s, t) / 2 - min(s,t)^3 / 6)
}

#' @rdname gbmprocess
#' @export
y.var.gbm <- function(t, irm)
{
  irm$sigma^2 * t^3 / 3
}

#### Ornstein-Uhlenbeck Process ####

#' @name ouprocess
#' @rdname ouprocess
#'
#' @title The Ornstein-Uhlenbeck Process
#'
#' @description Expected value, variance and covariance for the Ornstein-Uhlenbeck 
#' process with current force of interest equal to \code{delta0}, ultimate force of
#' interest equal to \code{delta}, friction parameter equal to \code{alpha},
#' and diffusion coefficient equal to \code{sigma}.
#'
#' @details The Ornstein-Uhlenbeck process is defined by
#'
#' \deqn{d\delta_t = -\alpha (\delta_t - \delta) dt + \sigma dW_t}
#'
#' where \eqn{W_t} is a standard Wiener process with parameter \eqn{\sigma = 1}
#' and \eqn{\alpha}, \eqn{\sigma} and \eqn{\delta} are constants with
#' \eqn{\alpha \ge 0} and \eqn{\sigma \ge 0}.
#'
#' Define \eqn{y(t)} as
#'
#' \deqn{y(t) = \int_{0}^{t} \delta_s ds}
#'
#' @return \code{y.ev} gives the expected value, \code{y.var}
#' gives the variance and \code{y.cov} gives the covariance.
#' 
#' @references
#'   Parker, Gary. An application of stochastic interest rate models in
#'   life assurance. Diss. Heriot-Watt University, 1992.
NULL

#' @rdname ouprocess
#' @export
delta.ev.ou <- function(t, irm)
{
  irm$delta + exp(-irm$alpha * t) * (irm$delta0 - irm$delta)
}

#' @rdname ouprocess
#' @export
delta.var.ou <- function(t, irm)
{
  irm$sigma^2 / (2*irm$alpha) * (1 - exp(-2*irm$alpha*t))
}

#' @rdname ouprocess
#' @export
y.ev.ou <- function(t, irm)
{
  irm$delta*t + (irm$delta0 - irm$delta) * (1 - exp(-irm$alpha*t)) / irm$alpha
}

#' @rdname ouprocess
#' @export
y.var.ou <- function(t, irm)
{
  irm$sigma^2/irm$alpha^2*t + irm$sigma^2/(2*irm$alpha^3) * 
    (-3 + 4*exp(-irm$alpha*t) - exp(-2*irm$alpha*t))
}

#' @rdname ouprocess
#' @export
y.cov.ou <- function(s, t, irm)
{
  irm$sigma^2 / irm$alpha^2 * min(s, t) +
    irm$sigma^2 / (2*irm$alpha^3) * (-2 + 2*exp(-irm$alpha*s) + 2*exp(-irm$alpha*t) 
                             - exp(-irm$alpha*(abs(t-s))) - exp(-irm$alpha*(t+s)))
}