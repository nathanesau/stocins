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
#' @description A class used to describe the interest rate we are modeling. Six
#' types of interest rate models can be used to model the force of interest
#' \eqn{\delta_{s}} (see Parker (1992)):
#' 
#' 1. Deterministic interest rate, i.e. the \link{determ} class. For the
#' determinstic interest rate, the \code{params} argument should be a
#' list with \code{delta} for the interest rate. The class argument should be
#' "determ". See the examples below.
#' 
#' 2. Wiener process, i.e. the \link{wiener} class. For the wiener process, the
#' \code{params} argument should be a list with \code{delta} for the long term
#' mean and \code{sigma} for the local volatility. The class argument should
#' be "wiener". See the examples below.
#' 
#' 3. Ornstein-Uhlenbeck process, i.e. the \link{ou} class. For the Ornstein-Uhlenbeck
#' process, the \code{params} should be a list with \code{delta} for the long 
#' term mean, \code{alpha} for the friction parameter, \code{sigma} for the 
#' local volatility and \code{delta0} for the initial value of the process.
#' The class argument should be "ou". See the examples below.
#' 
#' 4. Second Order Stochastic Differential Equation, i.e. the \link{second} class.
#' For the second order process, the \code{params} should be a list with \code{delta}
#' for the long term mean, \code{alpha1} for the first friction parameter,
#' \code{alpha2} for the second friction parameter, \code{sigma} for the local volatility,
#' \code{delta0prime} for the initial derivative of the process and
#' and \code{delta0} for the initial value of the process. Note that Parker uses
#' the notation of \code{alpha0} instead of \code{alpha2} but 
#' note that we are referring to the same parameter. The class argument should be
#' "second". See the examples below.
#' 
#' 5. AR(1) process, i.e. the \link{ar1} class. For the AR(1) process, the \code{params}
#' should be a list with \code{delta} for the long term mean, \code{delta0} for
#' the initial value of the process, \code{phi1} for the AR(1) coefficient
#' and \code{sigma} for the local volatility. The class argument should be "ar1".
#' See the examples below.
#' 
#' 6. ARMA(2,1) process, i.e. the \link{arma} class. For the ARMA(2,1) process, the
#' \code{params} should be a list with \code{delta} for the long term mean,
#' \code{delta0} for the initial value of the process, \code{phi1} for the AR(1)
#' coefficient, \code{phi2} for the AR(2) coefficient, \code{theta1} for the MA(1)
#' coefficient and \code{sigma} for the local volatility. The class argument should
#' be "arma".
#'
#' @details Define:
#' 
#' \deqn{y(t) = int_{0}^{t} \delta_{s}}
#' \deqn{pv(t) = exp{-y(t)}}
#' 
#' For each class, several functions are available.
#' 
#' The \code{delta.ev} function can be used to calculate the expected value of the
#' interest rate process at time \code{t}, the \code{delta.var} function can be
#' used to calculate of the interest rate process at time \code{t} and 
#' \code{delta.cov} can be used to calculate the covariance of the interest rate
#' process at two times \code{s} and \code{t}.
#' 
#' The \code{y.ev} function can be used to calculate the expected value of the
#' y at time \code{t}, the \code{y.var} function can be
#' used to calculate of variance of y at time \code{t} and 
#' \code{y.cov} can be used to calculate the covariance of
#' y at two times \code{s} and \code{t}.
#' 
#' The \code{pv.moment} can be used to calculate the raw moments of the 
#' present value random variable, \code{pv.ev} can be used to calculate
#' the expected value of the present value random varaible, \code{pv.var} can be
#' used to calculate the variance of the present value random variable and
#' \code{pv.cov} can be used to calculate the covariance of the present value
#' random variable at two times \code{s} and \code{t}.
#' 
#' The \code{ann.ev} function can be used to calculate the expected value of
#' an \code{n}-year annuity and \code{ann.var} can be used to calculate the
#' variance of an \code{n}-year annuity.
#' 
#' The \code{iratemodel.convert} function can be used to convert between
#' these interest rate models using the principle of covariance equivalence.
#' 
#' Refer to the examples below for how to use these functions.
#' 
#' @examples ## 1. Deterministic interest rate
#' determodel = iratemodel(list(delta = 0.06), "determ")
#' delta.ev(5, determodel) # expected value
#' 
#' ## 2. Wiener process
#' wienermodel = iratemodel(list(delta = 0.05, sigma = 0.01), "wiener")
#' pv.ev(10, wienermodel) # expected present value
#' pv.var(10, wienermodel) # variance of present value
#' pv.cov(5,10,wienermodel) # covariance of present value
#' 
#' ## 3. Ornstein-Uhlenbeck process
#' oumodel = iratemodel(list(delta0 = 0.08, delta = 0.05, 
#' alpha = 0.1, sigma = 0.01), "ou")
#' delta.ev(5, oumodel)
#' delta.var(5, oumodel)
#' y.ev(10, oumodel)
#' y.var(10, oumodel)
#' y.cov(5, 10, oumodel)
#' pv.ev(10, oumodel)
#' pv.var(10, oumodel)
#' pv.cov(5, 10, oumodel)
#' 
#' ## 4. Second Order Stochastic Differential Equation
#' secondmodel = iratemodel(params = list(alpha1 = -0.50, alpha2 = -0.04, 
#' delta0prime = 0.05, delta0 = 0.10, delta = 0.06, sigma = 0.01), "second")
#' 
#' ## 5. AR(1) Process
#' ar1model = iratemodel(params = list(delta = 0.05, delta0 = 0.08,
#' phi1 = 0.90, sigma = 0.01), "ar1")
#' delta.ev(5, ar1model)
#' delta.var(5, ar1model)
#' delta.cov(5, 10, ar1model)
#' ann.ev(5, ar1model)
#' ann.var(5, ar1model)
#' 
#' ## 6. ARMA(2,1) Process
#' armamodel = iratemodel(params = list(delta = 0.05, delta0 = 0.08,
#' phi1 = 1.05, phi2 = -0.095, theta1 = -0.05, sigma = 0.01), "arma")
#' 
#' ## Convert from one model to another
#' sdemodel = iratemodel.convert("arma", "second", armamodel, 5)
#' armamodel = iratemodel.convert("second", "arma", sdemodel, 5)
#' 
#' oumodel = iratemodel.convert("ar1", "ou", ar1model, 1/12)
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
  else if(class == "wiener")
  {
    class(params) = append(class(params),"wiener")
    
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

#### Deterministic ####

#' @name determ
#' @rdname determ
#'
#' @title Deterministic interest rate
#'
#' @description A class used to model the deterministic interest rate. 
#' See the \link{iratemodel} class for details and examples.
#' 
#' @references
#'   Parker, Gary. An application of stochastic interest rate models in
#'   life assurance. Diss. Heriot-Watt University, 1992.
NULL

#' @rdname determ
#' @export
delta.ev.determ <- function(t, irm)
{
  irm$delta * t^0 # constant
}

#' @rdname determ
#' @export
delta.cov.determ <- function(s, t, irm)
{
  0 * s * t # no covariance
}

#' @rdname determ
#' @export
delta.var.determ <- function(t, irm)
{
  0 * t # no variance
}

#' @rdname determ
#' @export
y.ev.determ <- function(t, irm)
{
  irm$delta * t
}

#' @rdname determ
#' @export
y.var.determ <- function(t, irm)
{
  0 * t # no variance
}

#' @rdname determ
#' @export
y.cov.determ <- function(s, t, irm)
{
  0 * t # no covariance
}

#### Wiener Process ####

#' @name wiener
#' @rdname wiener
#'
#' @title The Wiener Process
#'
#' @description A class used to model the Wiener process. See the \link{iratemodel}
#' class for details and examples.
#' 
#' @references
#'   Parker, Gary. An application of stochastic interest rate models in
#'   life assurance. Diss. Heriot-Watt University, 1992.
NULL

#' @rdname wiener
#' @export
delta.ev.wiener <- function(t, irm)
{
  irm$delta * t^0 # doesn't depend on t
}

#' @rdname wiener
#' @export
delta.cov.wiener <- function(s, t, irm)
{
  irm$sigma^2 * min(s, t)
}

#' @rdname wiener
#' @export
delta.var.wiener <- function(t, irm)
{
  irm$sigma^2 * t
}

#' @rdname wiener
#' @export
y.ev.wiener <- function(t, irm)
{
  irm$delta * t
}

#' @rdname wiener
#' @export
y.cov.wiener <- function(s, t, irm)
{
  irm$sigma^2 * (min(s,t)^2 * max(s, t) / 2 - min(s,t)^3 / 6)
}

#' @rdname wiener
#' @export
y.var.wiener <- function(t, irm)
{
  irm$sigma^2 * t^3 / 3
}

#### Ornstein-Uhlenbeck Process ####

#' @name ou
#' @rdname ou
#'
#' @title The Ornstein-Uhlenbeck Process
#'
#' @description A class used to model the Ornstein-Uhlenbeck process. See the \link{iratemodel}
#' class for details and examples.
#' 
#' @references
#'   Parker, Gary. An application of stochastic interest rate models in
#'   life assurance. Diss. Heriot-Watt University, 1992.
NULL

#' @rdname ou
#' @export
delta.ev.ou <- function(t, irm)
{
  irm$delta + exp(-irm$alpha * t) * (irm$delta0 - irm$delta)
}

#' @rdname ou
#' @export
delta.cov.ou <- function(s, t, irm)
{
  stop("delta.cov not implemented for ou")
}

#' @rdname ou
#' @export
delta.var.ou <- function(t, irm)
{
  irm$sigma^2 / (2*irm$alpha) * (1 - exp(-2*irm$alpha*t))
}

#' @rdname ou
#' @export
y.ev.ou <- function(t, irm)
{
  irm$delta*t + (irm$delta0 - irm$delta) * (1 - exp(-irm$alpha*t)) / irm$alpha
}

#' @rdname ou
#' @export
y.var.ou <- function(t, irm)
{
  irm$sigma^2/irm$alpha^2*t + irm$sigma^2/(2*irm$alpha^3) * 
    (-3 + 4*exp(-irm$alpha*t) - exp(-2*irm$alpha*t))
}

#' @rdname ou
#' @export
y.cov.ou <- function(s, t, irm)
{
  irm$sigma^2 / irm$alpha^2 * min(s, t) +
    irm$sigma^2 / (2*irm$alpha^3) * (-2 + 2*exp(-irm$alpha*s) + 2*exp(-irm$alpha*t) 
                             - exp(-irm$alpha*(abs(t-s))) - exp(-irm$alpha*(t+s)))
}

#### Second Order Stochcastic Differential Equation ####

#' @name second
#' @rdname second
#'
#' @title Second Order Stochastic Differential Equation
#'
#' @description A class used to model the Second Order Stochastic Differential Equation
#' for the force of interest. See the \link{iratemodel} class for details and examples.
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

#' @name ar1
#' @rdname ar1
#'
#' @title The AR(1) Process
#'
#' @description A class used to model an AR(1) process. See the \link{iratemodel}
#' class for details and examples.
#' 
#' @references
#'   Parker, Gary. An application of stochastic interest rate models in
#'   life assurance. Diss. Heriot-Watt University, 1992.
NULL

#' @rdname ar1
#' @export
delta.ev.ar1 <- function(t, irm)
{
  irm$phi1^t * (irm$delta0 - irm$delta) + irm$delta
}

#' @rdname ar1
#' @export
delta.cov.ar1 <- function(s, t, irm)
{
  irm$phi1^(abs(s-t)) * (1 - irm$phi1^(2*min(s,t))) / (1 - irm$phi1^2) * irm$sigma^2
}

#' @rdname ar1
#' @export
delta.var.ar1 <- function(t, irm)
{
  delta.cov.ar1(t, t, irm)
}

#' @rdname ar1
#' @export
y.ev.ar1 <- function(t, irm)
{
  sum(delta.ev(0:(t-1), irm))
}

#' @rdname ar1
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

#' @rdname ar1
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

#' @name arma
#' @rdname arma
#'
#' @title The ARMA(2,1) Process
#'
#' @description A class used to model the ARMA(2,1) process. See the \link{iratemodel}
#' class for details and examples.
#' 
#' @references
#'   Parker, Gary. An application of stochastic interest rate models in
#'   life assurance. Diss. Heriot-Watt University, 1992.
NULL

#' @rdname arma
#' @export
delta.ev.arma <- function(t, irm)
{
  stop("delta.ev not implemented for arma")
}

#' @rdname arma
#' @export
delta.cov.arma <- function(s, t, irm)
{
  stop("delta.cov not implemented for arma")
}

#' @rdname arma
#' @export
delta.var.arma <- function(t, irm)
{
  stop("delta.var not implemented for arma")
}

#' @rdname arma
#' @export
y.ev.arma <- function(t, irm)
{
  stop("y.ev not implemented for arma")
}

#' @rdname arma
#' @export
y.var.arma <- function(t, irm)
{
  stop("y.var not implemented for arma")
}

#' @rdname arma
#' @export
y.cov.arma <- function(s, t, irm)
{
  stop("y.cov not implemented for arma")
}

#### Annuity Functions ####

#' @title Expected value of annuity
#' @description Calculates the expected value of an n-period annuity.
#' See the \link{iratemodel} class for details and examples.
#' 
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
#' @description Calculates the variance of an n-period annuity. 
#' See the \link{iratemodel} class for details and examples.
#' 
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