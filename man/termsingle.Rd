% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/insuranceModels.R
\name{termsingle}
\alias{termsingle}
\alias{z.ev.three.isingle.termsingle}
\alias{z.ev.two.isingle.termsingle}
\alias{z.ev.twoone.isingle.termsingle}
\alias{z.insrisk.isingle.termsingle}
\alias{z.invrisk.isingle.termsingle}
\alias{z.moment.isingle.termsingle}
\alias{z.pdf.isingle.termsingle}
\title{Single term insurance product}
\usage{
\method{z.moment}{isingle.termsingle}(moment, ins, mort, irm)

z.ev.two.isingle.termsingle(ins, mort, irm)

z.ev.three.isingle.termsingle(ins, mort, irm)

z.ev.twoone.isingle.termsingle(ins, mort, irm)

\method{z.insrisk}{isingle.termsingle}(ins, mort, irm)

\method{z.invrisk}{isingle.termsingle}(ins, mort, irm)

\method{z.pdf}{isingle.termsingle}(z, ins, mort, irm)
}
\description{
A term insurance pays a benefit of \code{d} if the policyholder
dies within the term of the contract and 0 if the policyholder
survives the term of the contract.

In addition to the functions described in the \link{insurance} class, other
functions are available for the termsingle class.

In particular, 
\code{z.ev.two.isingle.termsingle} can be used to calculate \eqn{E[Z_{1}Z_{2}]},
\code{z.ev.three.isingle.termsingle} can be used to calculate
\eqn{E[Z_{1}Z_{2}Z_{3}]} and \code{z.ev.twoone.isingle.termsingle} 
can be used to calculate \eqn{E[Z_{1}^2Z_{2}]} in Parker (1992).
}
\details{
The \code{z.insrisk} and \code{z.invrisk} functions for the
\link{termsingle} class are implemented such that we are conditioning
on \eqn{K} not \eqn{y}. See Parker (1997) for details.
}
\examples{
oumodel = iratemodel(list(delta0 = 0.1, delta = 0.06, 
alpha = 0.1, sigma = 0.01), "ou")
mort = mortassumptions(list(x = 40, table = "MaleMort91"))
term = insurance(list(n = 10, d = 1), "isingle", "term")

z.ev(term,mort,oumodel) # first moment
z.ev.two.isingle.termsingle(term, mort, oumodel)
z.ev.twoone.isingle.termsingle(term, mort, oumodel)
z.ev.three.isingle.termsingle(term, mort, oumodel)

}
\references{
Parker, Gary. An application of stochastic interest rate models in
  life assurance. Diss. Heriot-Watt University, 1992.
  
  Parker, G. (1997). Stochastic analysis of the interaction between
  investment and insurance risks. North American actuarial journal, 1(2), 55–71.
}

