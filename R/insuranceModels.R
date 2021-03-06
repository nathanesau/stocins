#### Population Tables ####

#' @name CdnPop2016
#' @title Canadian Population Table for 2016
#' @description Canadian Population for 2016 from Statistics Canada census.
#' @docType data
#' @usage CdnPop2016
#' @format Male and Female population counts and proportions for 
#' different age groups.
#' @source Statistics Canada
NULL

#### Mortality Tables ####

#' @name MaleMort82
#' @title CA Male Mortality rates from 1980-1982
#' @description This data set contains the mortality rates used in Parker (1992).
#' @docType data
#' @usage MaleMort82
#' @format a \code{x} column for the age from 0 to 102 inclusive
#' and a \code{qx} column for the mortality rate.
#' @source Appendix B in Parker (1992).
#' @references
#'   Parker, Gary. An application of stochastic interest rate models in
#'   life assurance. Diss. Heriot-Watt University, 1992.
NULL

#' @name FemaleMort82
#' @title CA Male Mortality rates from 1980-1982 * 0.9
#' @description This data set contains the mortality rates used in Parker (1997). 
#' @docType data
#' @usage FemaleMort82
#' @format a \code{x} column for the age from 0 to 102 inclusive
#' and a \code{qx} column for the mortality rate.
#' @references Parker, G. (1997). Stochastic analysis of the interaction between
#' investment and insurance risks. North American actuarial journal, 1(2), 55–71.
NULL

#' @name MaleMort82Reduced
#' @title CA Male Mortality rates from 1980-1982 * 0.8
#' @description This data set contains the mortality rates used in Parker (1992).
#' @docType data
#' @usage MaleMort82Reduced
#' @format a \code{x} column for the age from 0 to 102 inclusive
#' and a \code{qx} column for the mortality rate.
#' @references Parker, G. (1997). Stochastic analysis of the interaction between
#' investment and insurance risks. North American actuarial journal, 1(2), 55–71.
NULL

#' @name FemaleMort82Reduced
#' @title CA Male Mortality rates from 1980-1982 * 0.75
#' @description This data set contains the mortality rates used in Parker.
#' @docType data
#' @usage FemaleMort82Reduced
#' @format a \code{x} column for the age from 0 to 102 inclusive
#' and a \code{qx} column for the mortality rate.
#' @references Parker, G. (1997). Stochastic analysis of the interaction between
#' investment and insurance risks. North American actuarial journal, 1(2), 55–71.
NULL

#' @name MaleMort91
#' @title CA Male Mortality rates from 1990-1992
#' @description This data set contains the 1991 mortality rates from Statistics Canada.
#' @docType data
#' @usage MaleMort91
#' @format a \code{x} column for the age from 0 to 100 inclusive
#' and a \code{qx} column for the mortality rate.
#' @source Statistics Canada
NULL

#' @name FemaleMort91
#' @title CA Female Mortality rates from 1990-1992
#' @description This data set contains the 1991 mortality rates from Statistics Canada.
#' @docType data
#' @usage FemaleMort91
#' @format a \code{x} column for the age from 0 to 100 inclusive
#' and a \code{qx} column for the mortality rate.
#' @source Statistics Canada
NULL

#### Mortality Assumptions ####

#' @name mortassumptions
#' @rdname mortassumptions
#'
#' @title Mortality Assumptions
#'
#' @description A class which describes the mortality for a policyholder.
#' The \code{params} argument should be a list with \code{x} for the age and
#' \code{table} for the table. See the examples below.
#' 
#' @details The \code{kpx} function can be used to calculate survival
#' probabilities and the \code{kdeferredqx} function can be used to calculate
#' probability of death in a given year. See the examples below.
#' 
#' @examples malemort = mortassumptions(list(x = 50, table = "MaleMort91"))
#' femalemort = mortassumptions(list(x = 30, table = "FemaleMort91"))
#' 
#' # calculate probabilities
#' kpx(5, malemort)
#' kdeferredqx(2, femalemort)
NULL

#' @rdname mortassumptions
#' @export
mortassumptions <- function(params)
{
  # calculate omega from the table
  params$omega = tail(get(params$table), 1)$x + 1
  
  # append class name
  class(params) = append(class(params), "mortassumptions")

  return(params)
}

#' @rdname mortassumptions
#' @export
kpx <- function(k, mort)
{
  UseMethod("kpx", mort)
}

#' @rdname mortassumptions
#' @export
kdeferredqx <- function(k, mort)
{
  UseMethod("kdeferredqx", mort)
}

#' @rdname mortassumptions
#' @export
kpx.mortassumptions <- function(k, mort)
{
  f <- function(k)
  {
    ifelse(k > mort$omega - mort$x, 0, 
    ifelse(k > 0, prod(1 - get(mort$table)$qx[seq(mort$x, mort$x+k-1, 1) + 1]), 1))
  }
  
  sapply(k, f)
}

#' @rdname mortassumptions
#' @export 
kdeferredqx.mortassumptions <- function(k, mort)
{
  f <- function(k)
  {
    ifelse(mort$x + k > mort$omega - 1, 0,
    kpx(k, mort) * get(mort$table)$qx[mort$x+k+1])
  }
  
  sapply(k, f)
}

#### Insurance Product ####

#' @name insurance
#' @rdname insurance
#'
#' @title Insurance product
#'
#' @description A class used to describe the insurance product we are modeling.
#' Three types of insurance policies can be modeled:
#' 
#' 1. Insurance policies issued to a single life, i.e. the \link{isingle} subclasses.
#' For these policies, the \code{params} argument should be a list with \code{n} for the
#' term of the contract, \code{d} for the death benefit and \code{e} for the 
#' survival benefit. If \code{e} is not specified it is assumed to be 0. For a 
#' policy issued to a single life, the class argument should be "isingle"
#' and the subclass argument should be either "term" for a term insurance
#' or "endow" for an endowment insurance. See the examples below.
#' 
#' 2. Identical policies issued to many lives, i.e. the \link{iport} subclasses. For
#' these policies, the \code{params} argument should be a list with \code{single}
#' being an \code{isingle} object and \code{c} being a number specifying how
#' many identical policies are in the portfolio. For a portfolio of policies, the
#' class argument should be "iport" and the subclass argument should either be "term"
#' for a term insurance or "endow" for an endowment insurance. See the examples below.
#' 
#' 3. A group of insurance portfolios, i.e. the \link{igroup} subclass. For a group
#' of policies, the \code{params} argument should be a list of \link{iport} objects
#' and the class argument should be "igroup". The subclass argument is not needed.
#' See the examples below.
#' 
#' @details For each class, several functions are available.
#' 
#' The \code{z.moment} function can be used to calculate the 
#' raw moments of the present value of benefit random variable. 
#' For the \link{isingle} classes all the moments are implemented, 
#' for the \link{iport} classes the first three moments are implemented 
#' and for the \link{igroup} class the first two moments are implemented.
#' The formulas from Parker (1992) were used to implement these moments.
#' 
#' The \code{z.ev} function can be used to calculate the first moment, the 
#' \code{z.sd} function can be used to calculate the standard deviation and
#' the \code{z.sk} function can be used to calculate the skewness of the
#' present value of benefit random variable.
#' 
#' The \code{z.insrisk} function can be used to calculate the insurance risk
#' arising from uncertain mortality and \code{z.invrisk} can be used to calculate
#' the investment risk arising from uncertain investment returns. The formulas
#' from Parker (1997) were used to implement these functions.
#' 
#' The \code{z.pdf} function can be used to calculate the density function
#' of the present value of benefit random variable for the \link{isingle} classes.
#' This function has not been implemented for the \link{iport} and \link{igroup}
#' classes. For details on how this could be done, refer to Parker (1992) and
#' Parker (1997).
#' 
#' Refer to the examples below for how to use these functions.
#' 
#' @examples oumodel = iratemodel(list(delta0 = 0.1, delta = 0.06, 
#' alpha = 0.1, sigma = 0.01), "ou")
#' mort = mortassumptions(list(x = 40, table = "MaleMort91"))
#' mort2 = mortassumptions(list(x = 50, table = "FemaleMort91"))
#' 
#' ## isingle classes
#' termins = insurance(list(n = 10, d = 1), "isingle", "term")
#' endowins = insurance(list(n = 10, e = 1, d = 1), "isingle", "endow")
#' 
#' z.ev(termins, mort, oumodel) # first moment
#' z.moment(2, termins, mort, oumodel) # second moment
#' z.moment(3, termins, mort, oumodel) # third moment
#' z.sd(endowins, mort, oumodel) # standard deviation
#' z.sk(endowins, mort, oumodel) # skewness
#' 
#' plot(function(z) z.pdf(z, termins, mort, oumodel), 0.01, 1.0,
#' ylim = c(0, 0.15), lty = 1, xlab = "z", ylab = "f(z)")
#' 
#' legend('topleft', leg = c(paste0("P(Z=0) = ", round(kpx(1, mort), 5))),
#' lty = 1)
#' 
#' ## iport classes
#' termport = insurance(list(single = termins, c = 1000), "iport", "term")
#' endowport = insurance(list(single = endowins, c = 1000), "iport", "endow")
#' z.moment(1, termport, mort, oumodel) / termport$c # average cost
#' z.sd(termport, mort, oumodel) / termport$c # average standard deviation
#' 
#' ## igroup class
#' groupins = insurance(list(termport, endowport), 
#' "igroup") # 1000 term contracts, 1000 endow contracts
#' groupmort = list(mort, mort2) # term contracts are age 40, endow contracts are age 50
#' z.moment(1, groupins, groupmort, oumodel) / groupins$c # average cost per policy
#' z.insrisk(groupins, groupmort, oumodel) / termport$c^2 # insrisk per policy
#' z.invrisk(groupins, groupmort, oumodel) / termport$c^2 # invrisk per policy
#' z.sd(groupins, groupmort, oumodel) / termport$c # sd per policy
#' 
#' @references
#'   Parker, Gary. An application of stochastic interest rate models in
#'   life assurance. Diss. Heriot-Watt University, 1992.
#'   
#'   Parker, G. (1997). Stochastic analysis of the interaction between
#'   investment and insurance risks. North American actuarial journal, 1(2), 55–71.
NULL

#' @rdname insurance
#' @export
insurance <- function(params, class, subclass = NULL)
{
  class(params) = append(class(params),"insurance")
  
  if(class == "isingle")
  {
    class(params) = append(class(params), "isingle")
    
    if(subclass == "term")
    {
      params$e = 0 # make sure e exists in term for class ``igroup``
      
      class(params) = append(class(params), "termsingle")
      
      return(params)
    }
    else if(subclass == "endow")
    {
      params$e = ifelse(is.null(params$e), 0, params$e)
      
      class(params) = append(class(params), "endowsingle")
      
      return(params)
    }
    else
    {
      stop("unknown subclass type in isingle") 
    }
  }
  else if(class == "iport")
  {
    class(params) = append(class(params), "iport")
    
    if(subclass == "term")
    {
      class(params) = append(class(params), "termport")
      
      return(params)
    }
    else if(subclass == "endow")
    {
      class(params) = append(class(params), "endowport")
      
      return(params)
    }
    else
    {
      stop("unknown subclass type in iport")
    }
  }
  else if(class == "igroup")
  {
    c = sum(unlist(lapply(params, function(x) x$c))) 
    n = max(unlist(lapply(params, function(x) x$single$n)))
    
    params$c = c
    params$n = n
    
    class(params) = append(class(params), "igroup")
    
    return(params)
  }
  else
  {
    stop("unknown class type")
  }
}

#' @rdname insurance
#' @export
z.moment <- function(moment, ins, mort, irm)
{
  UseMethod("z.moment", ins)
}

#' @rdname insurance
#' @export
z.ev <- function(ins, mort, irm)
{
  z.moment(1, ins, mort, irm)
}

#' @rdname insurance
#' @export
z.sd <- function(ins, mort, irm)
{
  sqrt(z.moment(2, ins, mort, irm) - z.moment(1, ins, mort, irm)^2)
}

#' @rdname insurance
#' @export
z.sk <- function(ins, mort, irm)
{
  u1 = z.moment(1, ins, mort, irm)
  u2 = z.moment(2, ins, mort, irm)
  u3 = z.moment(3, ins, mort, irm)

  num = u3 - 3*u2*u1 + 2*u1^3
  den = z.sd(ins, mort, irm)^3

  num / den
}

#' @rdname insurance
#' @export
z.insrisk <- function(ins, mort, irm)
{
  UseMethod("z.insrisk", ins)
}

#' @rdname insurance
#' @export
z.invrisk <- function(ins, mort, irm)
{
  UseMethod("z.invrisk", ins)
}

#' @rdname insurance
#' @export
z.pdf <- function(z, ins, mort, irm)
{
  UseMethod("z.pdf", ins)
}

#### Single Insurance Product ####

#' @name isingle
#' @rdname isingle
#'
#' @title Single insurance product
#'
#' @description A class used to describe insurance policies issued to a single life.
#' The \link{termsingle} and \link{endowsingle} classes can be used to model
#' term and endowment insurance contracts respectively. See the \link{insurance}
#' class for details and examples.
#' 
#' @references
#'   Parker, Gary. An application of stochastic interest rate models in
#'   life assurance. Diss. Heriot-Watt University, 1992.
NULL

#' @rdname isingle
#' @export
z.moment.isingle <- function(moment, ins, mort, irm)
{
  UseMethod("z.moment.isingle", ins)
}

#' @rdname isingle
#' @export
z.insrisk.isingle <- function(ins, mort, irm)
{
  UseMethod("z.insrisk.isingle", ins)
}

#' @rdname isingle
#' @export
z.invrisk.isingle <- function(ins, mort, irm)
{
  UseMethod("z.invrisk.isingle", ins)
}

#' @rdname isingle
#' @export
z.pdf.isingle <- function(z, ins, mort, irm)
{
  UseMethod("z.pdf.isingle", ins)
}

#### Single Term Insurance Product ####

#' @name termsingle
#' @rdname termsingle
#'
#' @title Single term insurance product
#'
#' @description A term insurance pays a benefit of \code{d} if the policyholder
#' dies within the term of the contract and 0 if the policyholder
#' survives the term of the contract.
#' 
#' In addition to the functions described in the \link{insurance} class, other
#' functions are available for the termsingle class.
#' 
#' In particular, 
#' \code{z.ev.two.isingle.termsingle} can be used to calculate \eqn{E[Z_{1}Z_{2}]},
#' \code{z.ev.three.isingle.termsingle} can be used to calculate
#' \eqn{E[Z_{1}Z_{2}Z_{3}]} and \code{z.ev.twoone.isingle.termsingle} 
#' can be used to calculate \eqn{E[Z_{1}^2Z_{2}]} in Parker (1992).
#'
#' @details The \code{z.insrisk} and \code{z.invrisk} functions for the
#' \link{termsingle} class are implemented such that we are conditioning
#' on \eqn{K} not \eqn{y}. See Parker (1997) for details.
#'   
#' @examples oumodel = iratemodel(list(delta0 = 0.1, delta = 0.06, 
#' alpha = 0.1, sigma = 0.01), "ou")
#' mort = mortassumptions(list(x = 40, table = "MaleMort91"))
#' term = insurance(list(n = 10, d = 1), "isingle", "term")
#' 
#' z.ev(term,mort,oumodel) # first moment
#' z.ev.two.isingle.termsingle(term, mort, oumodel)
#' z.ev.twoone.isingle.termsingle(term, mort, oumodel)
#' z.ev.three.isingle.termsingle(term, mort, oumodel)
#' 
#' @references
#'   Parker, Gary. An application of stochastic interest rate models in
#'   life assurance. Diss. Heriot-Watt University, 1992.
#'   
#'   Parker, G. (1997). Stochastic analysis of the interaction between
#'   investment and insurance risks. North American actuarial journal, 1(2), 55–71.
NULL

#' @rdname termsingle
#' @export
z.moment.isingle.termsingle <- function(moment, ins, mort, irm)
{
  total = 0
  
  for(k in seq(0, ins$n - 1, 1))
  {
    total = total + pv.moment(k+1, moment, irm) * ins$d^moment * kdeferredqx(k, mort)
  }
  
  total
}

#' @rdname termsingle
#' @export
z.ev.two.isingle.termsingle <- function(ins, mort, irm)
{
  total = 0
  
  for(i in seq(0, ins$n-1, 1))
  {
    for(j in seq(0, ins$n-1, 1))
    {
      mu = -(y.ev(i+1,irm) + y.ev(j+1,irm))
      sigma2 = y.var(i+1,irm) + y.var(j+1,irm) + 2*y.cov(i+1,j+1,irm)
      EPV = exp(mu + 0.5* sigma2) # EPV not accounting for mortality
      
      total = total + EPV * kdeferredqx(i, mort) * 
        kdeferredqx(j, mort) * ins$d^2
    }
  }
  
  total
}

#' @rdname termsingle
#' @export
z.ev.three.isingle.termsingle <- function(ins, mort, irm)
{
  total = 0
  
  for(i in seq(0, ins$n-1, 1))
  {
    for(j in seq(0, ins$n-1, 1))
    {
      for(k in seq(0, ins$n-1, 1))
      {
        mu = -(y.ev(i+1,irm) + y.ev(j+1,irm) + y.ev(k+1,irm))
        sigma2 = y.var(i+1,irm) + y.var(j+1,irm) + y.var(k+1,irm) +
          2 * y.cov(i+1, j+1, irm) + 2 * y.cov(i+1, k+1, irm) + 2 * y.cov(j+1, k+1, irm)
        
        EPV <- exp(mu + 0.5* sigma2) # EPV not accounting for mortality
        
        total = total + EPV * kdeferredqx(i, mort) * kdeferredqx(j, mort) *
          kdeferredqx(k, mort) * ins$d^3
      }
    }
  }
  
  total
}

# helper  function
#' @rdname termsingle
#' @export
z.ev.twoone.isingle.termsingle <- function(ins, mort, irm)
{
  total = 0
  
  for(i in seq(0, ins$n-1, 1))
  {
    for(j in seq(0, ins$n-1, 1))
    {
      mu <- -(2*y.ev(i+1,irm) + y.ev(j+1,irm))
      sigma2 <- 4*y.var(i+1,irm) + y.var(j+1,irm) + 4*y.cov(i+1,j+1,irm)
      EPV <- exp(mu + 0.5* sigma2) # EPV not accounting for mortality
      
      total = total + EPV * kdeferredqx(i, mort) * kdeferredqx(j, mort) * ins$d^2 * ins$d
    }
  }
  
  total
}

#' @rdname termsingle
#' @export
z.insrisk.isingle.termsingle <- function(ins, mort, irm)
{
  total = 0
  
  for(k in seq(0,ins$n-1,1))
  {
    for(j in seq(0,ins$n-1,1))
    {
      if(k==j)
      {
        total = total + pv.cov(k+1,j+1,irm) * kdeferredqx(k,mort)
      }
    }
  }
  
  total
}

#' @rdname termsingle
#' @export
z.invrisk.isingle.termsingle <- function(ins, mort, irm)
{
  second = 0
  first = 0
  
  for(k in seq(0,ins$n-1,1))
  {
    second = second + pv.moment(k+1,1,irm)^2 * kdeferredqx(k, mort)
    first = first + pv.moment(k+1,1,irm) * kdeferredqx(k, mort)
  }
  
  second - first^2
}

#' @rdname termsingle
#' @export
z.pdf.isingle.termsingle <- function(z, ins, mort, irm)
{
  f <- function(z)
  {
    if(z < 0)
    {
      return(0)
    }
    else if(z == 0)
    {
      return(kpx(k = ins$n, mort))
    }
    else
    {
      if(ins$n > 0)
      {
        k = seq(0, ins$n - 1, 1)
        kqx = kdeferredqx(k, mort)
          
        sum(kqx * dlnorm(z, -y.ev(k + 1, irm), sqrt(y.var(k + 1, irm))))
      }
      else
      {
        return (0)
      }
    }
  }
    
  sapply(z, f)
}

#### Single endowment insurance product ####

#' @name endowsingle
#' @rdname endowsingle
#'
#' @title Single endowment insurance product
#'
#' @description An endowment insurance pays a benefit of \code{d} if the policyholder
#' dies within the term of the contract and \code{e} if the policyholder
#' survives the term of the contract.
#' 
#' In addition to the functions described in the \link{insurance} class, other
#' functions are available for the endowsingle class. 
#' 
#' In particular, 
#' \code{z.ev.two.isingle.endowsingle} can be used to calculate \eqn{E[Z_{1}Z_{2}]},
#' \code{z.ev.three.isingle.endowsingle} can be used to calculate
#' \eqn{E[Z_{1}Z_{2}Z_{3}]} and \code{z.ev.twoone.isingle.endowsingle} 
#' can be used to calculate \eqn{E[Z_{1}^2Z_{2}]} in Parker (1992).
#' 
#' @details The \code{z.insrisk} and \code{z.invrisk} functions for the
#' \code{endowsingle} class are implemented such that we are conditioning
#' on \eqn{K} not \eqn{y}. See Parker (1997) for details.
#' 
#' @examples oumodel = iratemodel(list(delta0 = 0.1, delta = 0.06, 
#' alpha = 0.1, sigma = 0.01), "ou")
#' mort = mortassumptions(list(x = 40, table = "MaleMort91"))
#' endow = insurance(list(n = 10, d = 1, e = 1), "isingle", "endow")
#' 
#' z.ev(endow,mort,oumodel) # first moment
#' z.ev.two.isingle.endowsingle(endow, mort, oumodel)
#' z.ev.twoone.isingle.endowsingle(endow, mort, oumodel)
#' z.ev.three.isingle.endowsingle(endow, mort, oumodel)
#' 
#' @references
#'   Parker, Gary. An application of stochastic interest rate models in
#'   life assurance. Diss. Heriot-Watt University, 1992.
#'   
#'   Parker, G. (1997). Stochastic analysis of the interaction between
#'   investment and insurance risks. North American actuarial journal, 1(2), 55–71.
NULL

#' @rdname endowsingle
#' @export
z.moment.isingle.endowsingle <- function(moment, ins, mort, irm)
{
  total = 0
  
  for(k in seq(0, ins$n - 1, 1))
  {
    total = total + pv.moment(k+1, moment, irm) * ins$d^moment * kdeferredqx(k, mort)
  }
  
  total + kpx(ins$n, mort) * pv.moment(ins$n, moment, irm) * ins$e^moment
}

#' @rdname endowsingle
#' @export
z.ev.two.isingle.endowsingle <- function(ins, mort, irm) 
{
  total = 0
  
  for(i in seq(0, ins$n-1, 1))
  {
    for(j in seq(0, ins$n-1, 1))
    {
      mu <- -(y.ev(i+1, irm) + y.ev(j+1, irm))
      sigma2 <- y.var(i+1,irm) + y.var(j+1, irm) + 2*y.cov(i+1,j+1,irm)
      EPV <- exp(mu + 0.5* sigma2) # EPV not accounting for mortality
      
      total = total + EPV * kdeferredqx(i, mort) * kdeferredqx(j, mort) * ins$d^2
    }
    
    mu <- -(y.ev(i+1, irm) + y.ev(ins$n, irm))
    sigma2 <- y.var(i+1, irm) + y.var(ins$n, irm) + 2*y.cov(i+1, ins$n, irm)
    EPV <- exp(mu + 0.5*sigma2)
    
    total = total + 2 * EPV * kdeferredqx(i, mort) * kpx(ins$n, mort) * ins$d * ins$e
  }
  
  total + pv.moment(ins$n, 2, irm) * kpx(ins$n, mort)^2 * ins$e^2
}

#' @rdname endowsingle
#' @export
z.ev.three.isingle.endowsingle <- function(ins, mort, irm)
{
  total = 0
  
  for(i in seq(0, ins$n-1, 1))
  {
    for(j in seq(0, ins$n-1, 1))
    {
      for(k in seq(0, ins$n-1, 1))
      {
        mu = -(y.ev(i+1, irm) + y.ev(j+1, irm) + y.ev(k+1, irm))
        sigma2 = y.var(i+1, irm) + y.var(j+1, irm) + y.var(k+1, irm) +
          2 * y.cov(i+1, j+1, irm) + 2 * y.cov(i+1, k+1, irm) + 2 * y.cov(j+1, k+1, irm)
        
        EPV <- exp(mu + 0.5* sigma2) # EPV not accounting for mortality
        
        total = total + EPV * kdeferredqx(i, mort) * kdeferredqx(j, mort) * 
          kdeferredqx(k, mort) * ins$d^3
      }
      
      mu <- -(y.ev(i+1, irm) + y.ev(j+1, irm) + y.ev(ins$n, irm))
      sigma2 <- y.var(i+1, irm) + y.var(j+1, irm) + y.var(ins$n, irm) +
        2 * y.cov(i+1, j+1, irm) + 2 * y.cov(i+1, ins$n, irm) + 2 * y.cov(j+1, ins$n, irm)
      EPV <- exp(mu + 0.5* sigma2)
      
      total = total + 3 * EPV * kdeferredqx(i, mort) * kdeferredqx(j, mort) *
        kpx(ins$n, mort) * ins$d^2 * ins$e
    }
    
    mu <- -(y.ev(i+1, irm) + 2*y.ev(ins$n, irm))
    sigma2 <- y.var(i+1, irm) + 4*y.var(ins$n, irm) + 4*y.cov(i+1, ins$n, irm)
    EPV <- exp(mu + 0.5* sigma2)
    
    total = total + 3 * EPV * kdeferredqx(i, mort) * kpx(ins$n, mort)^2 * ins$d * ins$e^2
  }
  
  total + pv.moment(ins$n, 3, irm) * kpx(ins$n, mort)^3 * ins$e^3
}

#' @rdname endowsingle
#' @export
z.ev.twoone.isingle.endowsingle <- function(ins, mort, irm)
{
  total = 0
  
  for(i in seq(0, ins$n-1, 1))
  {
    for(j in seq(0, ins$n-1, 1))
    {
      mu <- -(2*y.ev(i+1, irm) + y.ev(j+1, irm))
      sigma2 <- 4*y.var(i+1, irm) + y.var(j+1, irm) + 4*y.cov(i+1,j+1, irm)
      EPV <- exp(mu + 0.5* sigma2) # EPV not accounting for mortality
      
      total = total + EPV * kdeferredqx(i, mort) * kdeferredqx(j, mort) * ins$d^2 * ins$d
    }
    
    mu <- -(2*y.ev(i+1, irm) + y.ev(ins$n, irm))
    sigma2 <- 4*y.var(i+1, irm) + y.var(ins$n, irm) + 4*y.cov(i+1, ins$n, irm)
    EPV <- exp(mu + 0.5 * sigma2)
    
    total = total + EPV * kdeferredqx(i, mort) * kpx(ins$n, mort) * ins$e * ins$d^2
    
    mu <- -(y.ev(i+1, irm) + 2*y.ev(ins$n, irm))
    sigma2 <- y.var(i+1, irm) + 4*y.var(ins$n, irm) + 4*y.cov(i+1, ins$n, irm)
    EPV <- exp(mu + 0.5 * sigma2)
    
    total = total + EPV * kdeferredqx(i, mort) * kpx(ins$n, mort) * ins$e^2 * ins$d
  }
  
  total + pv.moment(ins$n, 3, irm) * kpx(ins$n, mort)^2 * ins$e^3
}

#' @rdname endowsingle
#' @export
z.insrisk.isingle.endowsingle <- function(ins, mort, irm)
{
  stop("z.insrisk is not implemented for endowsingle class")
}

#' @rdname endowsingle
#' @export
z.invrisk.isingle.endowsingle <- function(ins, mort, irm)
{
  stop("z.invrisk is not implemented for endowsingle class")
}

#' @rdname isingle
#' @export
z.pdf.isingle.endowsingle <- function(z, ins, mort, irm)
{
  f <- function(z)
  {
    if(z <= 0)
    {
      return(0)
    }
    else
    {
      if(ins$n > 0)
      {
        k = seq(0, ins$n - 1, 1)
        kqx = kdeferredqx(k, mort)
        
        sum(kqx * dlnorm(z, -y.ev(k + 1, irm), sqrt(y.var(k + 1, irm)))) + 
        kpx(ins$n, mort) * dlnorm(z, -y.ev(ins$n, irm), sqrt(y.var(ins$n, irm)))
      }
      else
      {
        ifelse(n == 0 && z == 1, 1, 0)
      }
    }
  }
  
  sapply(z, f)
}

#### Insurance Portfolio (identical policies) ####

#' @name iport
#' @rdname iport
#'
#' @title Insurance portfolio (identical policies)
#'
#' @description A portfolio of \code{c} identical term or endowment policies.
#' The \link{termport} and \link{endowport} classes implement the functions for the
#' present value of benefit random variable. See the \link{insurance} class
#' for details and examples on how to use this class.
#' 
#' @references
#'   Parker, Gary. An application of stochastic interest rate models in
#'   life assurance. Diss. Heriot-Watt University, 1992.
NULL

#' @rdname iport
#' @export
z.moment.iport <- function(moment, ins, mort, irm)
{
  UseMethod("z.moment.iport", ins)
}

#' @rdname iport
#' @export
z.insrisk.iport <- function(ins, mort, irm)
{
  stop("z.insrisk not implemented for iport classes")
}

#' @rdname iport
#' @export
z.invrisk.iport <- function(ins, mort, irm)
{
  stop("z.invrisk not implemented for iport classes")
}

#' @rdname iport
z.pdf.iport <- function(z, ins, mort, irm)
{
  stop("z.pdf not implemented for iport classes")
}

#### Term Insurance Portfolio ####

#' @name termport
#' @rdname termport
#'
#' @title Term insurance portfolio (identical policies)
#'
#' @description A portfolio of \code{c} identical term policies. See the 
#' \link{insurance} class for details and examples on how to use this class.
#' 
#' @references
#'   Parker, Gary. An application of stochastic interest rate models in
#'   life assurance. Diss. Heriot-Watt University, 1992.
NULL

#' @rdname termport
#' @export
z.moment.iport.termport <- function(moment, ins, mort, irm)
{
  epv = 0
  
  if(moment == 1)
  {
    epv = z.moment(1, ins$single, mort, irm) * ins$c
  }
  else if(moment == 2)
  {
    epv = ins$c * (ins$c - 1) * z.ev.two.isingle.termsingle(ins$single, mort, irm) + 
      ins$c * z.moment(2, ins$single, mort, irm)
  }
  else if(moment == 3)
  {
    epv = ins$c * (ins$c - 1) * (ins$c - 2) * z.ev.three.isingle.termsingle(ins$single, mort, irm) +
      3 * ins$c * (ins$c - 1) * z.ev.twoone.isingle.termsingle(ins$single, mort, irm) +
      ins$c * z.moment(3, ins$single, mort, irm)
  }
  else
  {
    stop("moment > 3 not implemented for termport")
  }
  
  epv
}

#### Endowment Insurance Portfolio ####

#' @name endowport
#' @rdname endowport
#'
#' @title Endowment insurance portfolio (identical policies)
#'
#' @description A portfolio of \code{c} identical endowment policies. See the 
#' \link{insurance} class for details and examples on how to use this class.
#' 
#' @references
#'   Parker, Gary. An application of stochastic interest rate models in
#'   life assurance. Diss. Heriot-Watt University, 1992.
NULL

#' @rdname endowport
#' @export
z.moment.iport.endowport <- function(moment, ins, mort, irm)
{
  epv = 0
  
  if(moment == 1)
  {
    epv = z.moment(1, ins$single, mort, irm) * ins$c
  }
  else if(moment == 2)
  {
    epv = ins$c * (ins$c - 1) * z.ev.two.isingle.endowsingle(ins$single, mort, irm) + 
      ins$c * z.moment(2, ins$single, mort, irm)
  }
  else if(moment == 3)
  {
    epv = ins$c * (ins$c - 1) * (ins$c - 2) * z.ev.three.isingle.endowsingle(ins$single, mort, irm) +
      3 * ins$c * (ins$c - 1) * z.ev.twoone.isingle.endowsingle(ins$single, mort, irm) +
      ins$c * z.moment(3, ins$single, mort, irm)
  }
  else
  {
    stop("moment > 3 not implemented for endowport")
  }
  
  epv
}

#### Group of Insurance Portfolios ####

#' @name igroup
#' @rdname igroup
#'
#' @title Group of Insurance Portfolios
#'
#' @description A class for calculating the moments, insurance risk and
#' and investment risk for a group of endowment and term insurance portfolios
#' using the formulas described in Parker (1997).
#' 
#' In addition to the functions described in the \link{insurance} class some
#' other functions are available for the \code{igroup} class.
#' 
#' In particular,
#' \code{z.ev.two.igroup} calculates \eqn{E[Z_{1,i}, Z_{i,r}]} for
#' \code{i} = \code{ind1} and \code{r} = \code{ind2}, \code{cashflow.ev}
#' calculates the expected cashflow at time \code{r} and \code{cashflow.cov}
#' calculates the covariance between the cashflows at time \code{r} and time
#' \code{s} as describd in Parker (1997).
#' 
#' For details and examples on how to use the \code{igroup} class
#' see the \link{insurance} class.
#' 
#' @examples oumodel = iratemodel(list(delta0 = 0.1, delta = 0.06, 
#' alpha = 0.1, sigma = 0.01), "ou")
#' mort = mortassumptions(list(x = 40, table = "MaleMort91"))
#' mort2 = mortassumptions(list(x = 50, table = "FemaleMort91"))
#' 
#' termins = insurance(list(n = 10, d = 1), "isingle", "term")
#' endowins = insurance(list(n = 10, e = 1, d = 1), "isingle", "endow")
#' 
#' termport = insurance(list(single = termins, c = 1000), "iport", "term")
#' endowport = insurance(list(single = endowins, c = 1000), "iport", "endow")
#' 
#' groupins = insurance(list(termport, endowport), 
#' "igroup") # 1000 term contracts, 1000 endow contracts
#' groupmort = list(mort, mort2) # term contracts are age 40, endow contracts are age 50
#' z.moment(1, groupins, groupmort, oumodel) / groupins$c # average cost per policy
#' z.ev.two.igroup(1, 2, groupins, groupmort, oumodel)
#' cashflow.ev(5, groupins, groupmort, oumodel)
#' cashflow.cov(3, 5, groupins, groupmort, oumodel)
#' 
#' @references
#'   Parker, G. (1997). Stochastic analysis of the interaction between
#'   investment and insurance risks. North American actuarial journal, 1(2), 55–71.
NULL

#' @rdname igroup
#' @export
z.ev.two.igroup <- function(ind1, ind2, ins, mort, irm)
{
  if(length(ins) != length(mort) + 2)
  {
    stop("length(ins) != length(mort)")
  }
  
  total = 0
  
  for(i in seq(0, ins[[ind1]]$single$n-1, 1))
  {
    for(j in seq(0, ins[[ind2]]$single$n-1, 1))
    {
      mu = -(y.ev(i+1, irm) + y.ev(j+1, irm))
      sigma2 = y.var(i+1, irm) + y.var(j+1, irm) + 2*y.cov(i+1,j+1, irm)
      EPV = exp(mu + 0.5 * sigma2) # EPV not accounting for mortality
      
      total = total + EPV * kdeferredqx(i, mort[[ind1]]) *
        kdeferredqx(j, mort[[ind2]]) * ins[[ind1]]$single$d * ins[[ind2]]$single$d
    }
  }
  
  for(i in seq(0, ins[[ind1]]$single$n-1, 1))
  {
    mu <- -(y.ev(i+1, irm) + y.ev(ins[[ind2]]$single$n, irm))
    sigma2 <- y.var(i+1, irm) + y.var(ins[[ind2]]$single$n, irm) + 
      2*y.cov(i+1, ins[[ind2]]$single$n, irm)
    EPV <- exp(mu + 0.5*sigma2)
    
    total = total + EPV * kdeferredqx(i, mort[[ind1]]) *
      kpx(ins[[ind2]]$single$n, mort[[ind2]]) * ins[[ind1]]$single$d * ins[[ind2]]$single$e
  }
  
  for(i in seq(0, ins[[ind2]]$single$n-1, 1))
  {
    mu <- -(y.ev(i+1, irm) + y.ev(ins[[ind1]]$single$n, irm))
    sigma2 <- y.var(i+1, irm) + y.var(ins[[ind1]]$single$n, irm) + 
      2*y.cov(i+1, ins[[ind1]]$single$n, irm)
    EPV <- exp(mu + 0.5*sigma2)
    
    total = total + EPV * kpx(ins[[ind1]]$single$n, mort[[ind1]]) *
      kdeferredqx(i, mort[[ind2]]) * ins[[ind2]]$single$d * ins[[ind1]]$single$e
  }
  
  mu = -(y.ev(ins[[ind1]]$single$n, irm) + y.ev(ins[[ind2]]$single$n, irm))
  sigma2 = y.var(ins[[ind1]]$single$n, irm) + y.var(ins[[ind2]]$single$n, irm) + 
    2*y.cov(ins[[ind1]]$single$n, ins[[ind2]]$single$n, irm)
  EPV = exp(mu + 0.5*sigma2)
  
  total + EPV * kpx(ins[[ind1]]$single$n, mort[[ind1]]) *
    kpx(ins[[ind2]]$single$n, mort[[ind2]]) * ins[[ind1]]$single$e * ins[[ind2]]$single$e
}

#' @rdname igroup
#' @export
z.moment.igroup <- function(moment, ins, mort, irm)
{
  if(length(ins) != length(mort) + 2)
  {
    stop("length(ins) != length(mort)")
  }
  
  m = length(mort) # number of groups
  
  epv = 0
  
  if(moment == 1)
  {
    for(i in 1:m)
    {
      epv = epv + z.moment(1, ins[[i]], mort[[i]], irm)
    }
  }
  else if(moment == 2)
  {
    for(i in 1:m)
    {
      epv = epv + ins[[i]]$c * z.moment(2, ins[[i]]$single, mort[[i]], irm)
    }
    
    for(i in 1:m)
    {
      epv = epv + ins[[i]]$c * (ins[[i]]$c - 1) * 
        z.ev.two.isingle.endowsingle(ins[[i]]$single, mort[[i]], irm)
    }
    
    for(i in 1:m)
    {
      for(r in i:m)
      {
        if(i != r)
        {
          epv = epv + 2 * ins[[i]]$c * ins[[r]]$c *
            z.ev.two.igroup(i, r, ins, mort, irm)
        }
      }
    }
  }
  
  epv
}

#' @rdname igroup
#' @export
cashflow.ev <- function(r, ins, mort, irm)
{
  if(length(ins) != length(mort) + 2)
  {
    stop("length(ins) != length(mort)")
  }
  
  m = length(mort) # number of groups
  
  epv = 0
  
  I1 <- function(n, r)
  {
    ifelse(n >= r, 1, 0)
  }
  
  I2 <- function(n, r)
  {
    ifelse(n == r, 1, 0)
  }
  
  for(i in 1:m)
  {
    epv = epv + ins[[i]]$single$d * ins[[i]]$c * kdeferredqx(r - 1, mort[[i]]) *
      I1(ins[[i]]$single$n, r)
  }
  
  for(i in 1:m)
  {
    epv = epv + ins[[i]]$single$e * ins[[i]]$c * kpx(ins[[i]]$single$n, mort[[i]]) *
      I2(ins[[i]]$single$n, r)
  }
  
  epv
}

#' @rdname igroup
#' @export
cashflow.cov <- function(s, r, ins, mort, irm)
{
  if(length(ins) != length(mort) + 2)
  {
    stop("length(ins) != length(mort)")
  }
  
  m = length(mort) # number of groups
  
  total = 0
  
  I1 <- function(n, r)
  {
    ifelse(n >= r, 1, 0)
  }
  
  I2 <- function(n, r)
  {
    ifelse(n == r, 1, 0)
  }
  
  if(s == r) # variance
  {
    for(i in 1:m)
    {
      total = total + ins[[i]]$single$d^2 * ins[[i]]$c *
        kdeferredqx(r - 1, mort[[i]]) *
        (1 - kdeferredqx(r - 1, mort[[i]])) * I1(ins[[i]]$single$n, r)
    }
    
    for(i in 1:m)
    {
      total = total + ins[[i]]$single$e^2 * ins[[i]]$c *
        kpx(ins[[i]]$single$n, mort[[i]]) *
        (1 - kpx(ins[[i]]$single$n, mort[[i]])) * I2(ins[[i]]$single$n, r)
    }
    
    for(i in 1:m)
    {
      total = total - 2 * ins[[i]]$single$d * ins[[i]]$single$e * ins[[i]]$c *
        kdeferredqx(r - 1, mort[[i]]) *
        kpx(r, mort[[i]]) * I2(ins[[i]]$single$n, r)
    }
  }
  else
  {
    for(i in 1:m)
    {
      total = total - ins[[i]]$single$d^2 * ins[[i]]$c *
        kdeferredqx(min(s,r) - 1, mort[[i]]) *
        kdeferredqx(max(s,r) - 1, mort[[i]]) * I1(ins[[i]]$single$n, max(s,r))
    }
    
    for(i in 1:m)
    {
      total = total - ins[[i]]$single$d * ins[[i]]$single$e * ins[[i]]$c *
        kdeferredqx(min(s,r) - 1, mort[[i]]) *
        kpx(ins[[i]]$single$n, mort[[i]]) * I2(ins[[i]]$single$n, max(s,r))
    }
  }
  
  total
}

#' @rdname igroup
#' @export
z.insrisk.igroup <- function(ins, mort, irm)
{
  total = 0
  
  for(r in 1:ins$n)
  {
    for(s in 1:ins$n)
    {
      mu <- -(y.ev(r, irm) + y.ev(s, irm))
      sigma2 <- y.var(r, irm) + y.var(s, irm) + 2*y.cov(r, s, irm)
      EPV <- exp(mu + 0.5*sigma2)
      
      total = total +
        EPV * cashflow.cov(r, s, ins, mort, irm)
    }
  }
  
  total
}

#' @rdname igroup
#' @export
z.invrisk.igroup <- function(ins, mort, irm)
{
  total = 0
  
  for(r in 1:ins$n)
  {
    for(s in 1:ins$n)
    {
      total = total + cashflow.ev(r, ins, mort, irm) *
        cashflow.ev(s, ins, mort, irm) * pv.cov(r, s, irm)
    }
  }
  
  total
}