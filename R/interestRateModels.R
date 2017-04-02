#### Historical Returns ####

#' @name returns91
#' @title Investment Returns from 1991 to 2017
#' @description Company returns observed on monthly basis from 1991 to 2017
#' @docType data
#' @usage returns91
#' @format A column for month and column for return (annualized). Rates are
#' continuously compounded.
NULL

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
  else if(class == "determ")
  {
    class(params) = append(class(params),"determ")
    
    return(params)
  }
  else if(class == "second")
  {
    class(params) = append(class(params),"second")
    
    return(params)
  }
  else if(class == "ar1")
  {
    class(params) = append(class(params),"ar1")  
    
    return(params)
  }
  else if(class == "arma")
  {
    class(params) = append(class(params),"arma")
    
    return(params)
  }
  else 
  {
    stop("unknown class type")
  }
}

#' @rdname iratemodel
#' @export
iratemodel.convert <- function(from, to, irm, Delta = 1)
{
  if(from == "second")
  {
    if(to == "arma")
    {
      alpha1 <- irm$alpha1
      alpha2 <- irm$alpha2
      sigma2 <- irm$sigma^2
      
      # eigenvalues
      mu1 <- 0.5 * (alpha1 + sqrt(alpha1^2 + 4*alpha2))
      mu2 <- 0.5 * (alpha1 - sqrt(alpha1^2 + 4*alpha2))
      
      lambda1 <- exp(mu1 * Delta)
      lambda2 <- exp(mu2 * Delta)
      
      phi1 <- lambda1 + lambda2
      phi2 <- -lambda1 * lambda2
      
      P = (-mu1*(1+lambda1^2)*(1-lambda2^2) + mu2*(1+lambda2^2)*(1-lambda1^2)) /
        (mu1*lambda1*(1-lambda2^2) - mu2*lambda2*(1-lambda1^2))
      
      theta1 <- Re(polyroot(c(1,P,1)))
      theta1 <- theta1[which(abs(theta1) == min(abs(theta1)))]
      
      sigma2a = sigma2 * (lambda1 - lambda2)^2 / (2*mu1*(mu1^2 - mu2^2) * (lambda1 - theta1)) /
        ((lambda1 - theta1)/(1 - lambda1^2) - (lambda2 - theta1)/(1 - lambda1*lambda2))
      
      armamodel = iratemodel(params = list(phi1 = phi1, phi2 = phi2,
                                           theta1 = theta1,
                                           sigma = sqrt(sigma2a)), "arma")
      
      return(armamodel)
    }
    else
    {
      stop(paste("conversion from", from, "to", to, "not implemented"))
    }
  }
  else if(from == "arma")
  {
    if(to == "second")
    {
      phi1 <- irm$phi1
      phi2 <- irm$phi2
      
      lambda1 <- 0.5 * (phi1 + sqrt(phi1^2 + 4*phi2))
      lambda2 <- 0.5 * (phi1 - sqrt(phi1^2 + 4*phi2))
      
      mu1 <- log(lambda1) / Delta
      mu2 <- log(lambda2) / Delta
      
      alpha1 = mu1 + mu2
      alpha2 = -mu1 * mu2
      
      secondmodel = iratemodel(params = list(alpha1 = alpha1, 
        alpha2 = alpha2, sigma = 0),"second") # sigma arbitrary for now
      
      return(secondmodel)
    }
    else
    {
      stop(paste("conversion from", from, "to", to, "not implemented"))  
    }
  }
  else if(from == "ar1")
  {
    if(to == "ou")
    {
      phi1 = irm$phi1
      sigma2a = irm$sigma^2
      
      alpha = -log(phi1) / Delta
      sigma2 = 2*alpha*sigma2a / (1 - phi1^2)
      
      oumodel = iratemodel(params = list(alpha = alpha, sigma = sqrt(sigma2)),
                        "ou")
    
    return(oumodel)
    }
    else
    {
      stop(paste("conversion from", from, "to", to, "not implemented"))  
    }
  }
  else if(from == "ou")
  {
    if(to == "ar1")
    {
      alpha = irm$alpha
      sigma2 = irm$sigma^2
      
      phi1 = exp(-alpha*Delta)
      sigma2a = sigma2 / (2*alpha) * (1 - phi1^2)
      
      armodel = iratemodel(params = list(coef = phi1, sigma = sqrt(sigma2a)),
                           "ar1")
      
      return(armodel)
    }
    else
    {
      stop(paste("conversion from", from, "to", to, "not implemented"))  
    }
  }
  else
  {
    stop(paste("conversion from", from, "to", to, "not implemented"))
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
delta.cov <- function(s, t, irm)
{
  UseMethod("delta.cov", irm)
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
delta.cov.gbm <- function(s, t, irm)
{
  irm$sigma^2 * min(s, t)
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
delta.cov.ou <- function(s, t, irm)
{
  stop("delta.cov not implemented for ou")
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

#### Deterministic ####

#' @name determ
#' @rdname determ
#'
#' @title Determinstic Interest Rate
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

#' @rdname iratemodel
#' @export
delta.ev.determ <- function(t, irm)
{
  irm$delta * t^0 # constant
}

#' @rdname iratemodel
#' @export
delta.cov.determ <- function(s, t, irm)
{
  0 * s * t # no covariance
}

#' @rdname iratemodel
#' @export
delta.var.determ <- function(t, irm)
{
  0 * t # no variance
}

#' @rdname iratemodel
#' @export
y.ev.determ <- function(t, irm)
{
  irm$delta * t
}

#' @rdname iratemodel
#' @export
y.var.determ <- function(t, irm)
{
  0 * t # no variance
}

#' @rdname iratemodel
#' @export
y.cov.determ <- function(s, t, irm)
{
  0 * t # no covariance
}

#### Second Order Stochcastic Differential Equation ####

#' @name second
#' @rdname second
#'
#' @title Second order stochastic differential equation
#'
#' @description Expected value and variance for a Second order stochastic
#' differential equation with frction parameters \code{alpha1} and
#' \code{alpha2}, derivative of the force of interest at time 0 equal to
#' \code{delta0prime}, current force of interest equal to \code{delta0},
#' ultimate force of interest equal to \code{delta}, and diffusion coefficient
#' equal to \code{sigma}.
#'
#' @details If \code{alpha1}, \code{alpha2}, \code{delta0prime}, \code{delta0},
#' \code{delta}, and \code{sigma} are not specified, they assume the default
#' values of \code{-0.50}, \code{-0.04}, \code{0.05}, \code{0.1} and
#' \code{0.06} respectively.
#'
#' The Second Order Stochastic Differential Equation is defined by
#'
#' \deqn{d(d/dt \delta_t) = \alpha_1 d(\delta_t - \delta) dt +
#'       \alpha_2 (\delta_t - \delta) +
#'       \sigma dW_t}
#'
#' where \eqn{W_t} is a standard Wiener process with parameter \eqn{\sigma = 1}
#' and \eqn{\alpha_1}, \eqn{\alpha_2}, \eqn{\sigma} and \eqn{\delta} are constants.
#'
#' @return \code{sde.evalue} gives the expected value, and \code{sde.var}
#' gives the variance
#'
#' @references
#'   Parker, Gary. An application of stochastic interest rate models in
#'   life assurance. Diss. Heriot-Watt University, 1992.
NULL

#' @rdname second
#' @export
delta.ev.second <- function(t, irm)
{
  f <- function(s)
  {
    lambda1 = (irm$alpha1 - sqrt(irm$alpha1^2 + 4*irm$alpha2)) / 2
    lambda2 = (irm$alpha1 + sqrt(irm$alpha1^2 + 4*irm$alpha2)) / 2
    
    A11 = (lambda2 * exp(lambda2 * s) - lambda1 * exp(lambda1 * s)) / (lambda2 - lambda1)
    A12 = lambda1 * lambda2 * (exp(lambda1 * s) - exp(lambda2 * s)) / (lambda2 - lambda1)
    A21 = (exp(lambda2 * s) - exp(lambda1 * s)) / (lambda2 - lambda1)
    A22 = (lambda2 * exp(lambda1 * s) - lambda1 * exp(lambda2 * s)) / (lambda2 - lambda1)
    A = matrix(c(A11, A21, A12, A22), 2, 2)
    
    out = A %*% matrix(c(irm$delta0prime, irm$delta0 - irm$delta), 2, 1)
    out[2,1] + irm$delta
  }
  
  sapply(t, f)
}

#' @rdname second
#' @export
delta.cov.second <- function(s, t, irm)
{
  stop("delta.cov is not implemented for second order sde")
}

#' @rdname second
#' @export
delta.var.second <- function(t, irm)
{
  f <- function(s)
  {
    lambda1 = (irm$alpha1 - sqrt(irm$alpha1^2 + 4*irm$alpha2)) / 2
    lambda2 = (irm$alpha1 + sqrt(irm$alpha1^2 + 4*irm$alpha2)) / 2
    
    A11 = (lambda2 * exp(lambda2 * s) - lambda1 * exp(lambda1 * s)) / (lambda2 - lambda1)
    A12 = lambda1 * lambda2 * (exp(lambda1 * s) - exp(lambda2 * s)) / (lambda2 - lambda1)
    A21 = (exp(lambda2 * s) - exp(lambda1 * s)) / (lambda2 - lambda1)
    A22 = (lambda2 * exp(lambda1 * s) - lambda1 * exp(lambda2 * s)) / (lambda2 - lambda1)
    A = matrix(c(A11, A21, A12, A22), 2, 2)
    
    I11 = irm$sigma^2 / (lambda2 - lambda1)^2 *
        (-lambda2/2 * (exp(-2*lambda2*s) - 1) +
          2 * lambda1 * lambda2 / (lambda1 + lambda2) * (exp(-(lambda1 + lambda2)*s) - 1) +
          (-lambda1 / 2) * (exp(-2*lambda1*s) - 1))
    
    I12 = irm$sigma^2 / (lambda2 - lambda1)^2 *
      ( -1/2 * (exp(-2*lambda2*s) - 1) +
          (exp(-(lambda1 + lambda2)*s) - 1) +
          (-1/2) * (exp(-2*lambda1*s) - 1)
      )
    
    I21 = I12
    
    I22 = irm$sigma^2 / (lambda2 - lambda1)^2 *
      ( -1/(2*lambda2) * (exp(-2*lambda2*s) - 1) +
          2/(lambda1 + lambda2) * (exp(-(lambda1 + lambda2)*s) - 1) +
          (-1/(2*lambda1)) * (exp(-2*lambda1 * s) - 1)
      )
    
    I = matrix(c(I11, I21, I12, I22), 2, 2)
    
    out = A %*% I %*% t(A)
    out[2,2]
  }
  
  sapply(t, f)
}

#' @rdname second
#' @export
y.ev.second <- function(t, irm)
{
  stop("y.ev is not implemented for second order sde")
}

#' @rdname second
#' @export
y.var.second <- function(t, irm)
{
  stop("y.var is not implemented for second order sde")
}

y.cov.second <- function(s, t, irm)
{
  stop("y.cov is not implemented for second order sde")
}

#### AR1 process ####

#' @name ar1process
#' @rdname ar1process
#'
#' @title The AR(1) process Process
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

#' @rdname ar1process
#' @export
delta.ev.ar1 <- function(t, irm)
{
  irm$phi1^t * (irm$delta0 - irm$delta) + irm$delta
}

#' @rdname ar1process
#' @export
delta.cov.ar1 <- function(s, t, irm)
{
  irm$phi1^(abs(s-t)) * (1 - irm$phi1^(2*min(s,t))) / (1 - irm$phi1^2) * irm$sigma^2
}

#' @rdname ar1process
#' @export
delta.var.ar1 <- function(t, irm)
{
  delta.cov.ar1(t, t, irm)
}

#' @rdname ar1process
#' @export
y.ev.ar1 <- function(t, irm)
{
  sum(delta.ev(0:(t-1), irm))
}

#' @rdname ar1process
#' @export
y.var.ar1 <- function(t, irm)
{
  total <- 0
  for(s in 0:(t-1))
  {
    for(r in 0:(t-1))
    {
      total <- total + delta.cov(s,r,irm)
    }
  }
  total
}

#' @rdname ar1process
#' @export
y.cov.ar1 <- function(s, t, irm)
{
  total <- 0
  for(m in 0:(s-1))
  {
    for(r in 0:(t-1))
    {
      total <- total + delta.cov(m,r,irm)
    }
  }
  total
}

#### ARMA(2,1) process ####

#' @name armaprocess
#' @rdname armaprocess
#'
#' @title The ARMA(2, 1) process Process
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

#' @rdname armaprocess
#' @export
delta.ev.arma <- function(t, irm)
{
  stop("delta.ev not implemented for arma")
}

#' @rdname armaprocess
#' @export
delta.cov.arma <- function(s, t, irm)
{
  stop("delta.cov not implemented for arma")
}

#' @rdname armaprocess
#' @export
delta.var.arma <- function(t, irm)
{
  stop("delta.var not implemented for arma")
}

#' @rdname armaprocess
#' @export
y.ev.arma <- function(t, irm)
{
  stop("y.ev not implemented for arma")
}

#' @rdname armaprocess
#' @export
y.var.arma <- function(t, irm)
{
  stop("y.var not implemented for arma")
}

#' @rdname armaprocess
#' @export
y.cov.arma <- function(s, t, irm)
{
  stop("y.cov not implemented for arma")
}

#### Annuity Functions ####

#' @title Expected value of annuity
#' @description Calculates the expected value of an n-period annuity
#' @param n The length of the annuity
#' @param irm The interest rate model to use
#' @return Present value of annuity
#' @export
ann.ev <- function(n,irm)
{
  total = 0
  
  for(t in 1:n)
  {
    total = total + pv.ev(t,irm)
  }
  
  total
}

#' @title Variance of annuity
#' @description Calculates the variance of an n-period annuity
#' @param n The length of the annuity
#' @param irm The interest rate model to use
#' @return Variance of annuity
#' @export
ann.var <- function(n, irm)
{
  total = 0
  for(i in 1:n)
  {
    for(j in 1:n)
    {
      total = total + pv.cov(i, j, irm)
    }
  }
  total
}