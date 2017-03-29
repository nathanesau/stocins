library(stocins)

mort = mortassumptions(list(x = 50, table = "MaleMort82"))

oumodel = iratemodel(list(delta0 = 0.08, delta = 0.04,
                          alpha = 0.2, sigma = 0.015), "ou")

## Term Insurance

termins = insurance(params = list(n = 20, d=1000), "isingle", "term")

termport1 = insurance(params = list(single = termins, c = 1), "iport", "term")
termport100 = insurance(params = list(single = termins, c = 100), "iport", "term")
termport1e18 = insurance(params = list(single = termins, c = 1e18), "iport", "term")

z.moment(1, termport1, mort, oumodel) / termport1$c
z.moment(1, termport100, mort, oumodel) / termport100$c
z.moment(1, termport1e18, mort, oumodel) / termport1e18$c

z.sd(termport1, mort, oumodel) / termport1$c
z.sd(termport100, mort, oumodel) / termport100$c
z.sd(termport1e18, mort, oumodel) / termport1e18$c

z.sk(termport1, mort, oumodel)
z.sk(termport100, mort, oumodel)
z.sk(termport1e18, mort, oumodel)

z.moment(3, termins, mort, oumodel)
z.ev.three.isingle.termsingle(termins, mort, oumodel)
z.ev.twoone.isingle.termsingle(termins, mort, oumodel)


## Endowment Insurance

endowins = insurance(params = list(n = 20, d=0, e=1000), "isingle", "endow")

endowport1 = insurance(params = list(single = endowins, c = 1), "iport", "endow")
endowport100 = insurance(params = list(single = endowins, c = 100), "iport", "endow")
endowport1e18 = insurance(params = list(single = endowins, c = 1e18), "iport", "endow")

ratio <- numeric(20)

for(n in 1:20)
{
  endowins = insurance(params = list(n = n, d=1000, e=500), "isingle", "endow")
  endowport1e18 = insurance(params = list(single = endowins, c = 1e18), "iport", "endow")
  
  ratio[n] = z.sd(endowport1e18, mort, oumodel)
}

plot(ratio/endowport1e18$c)

z.moment(1, endowport1, mort, oumodel) / endowport1$c
z.moment(1, endowport100, mort, oumodel) / endowport100$c
z.moment(1, endowport1e18, mort, oumodel) / endowport1e18$c

z.sd(endowport1, mort, oumodel) / endowport1$c
z.sd(endowport100, mort, oumodel) / endowport100$c
z.sd(endowport1e18, mort, oumodel)/ endowport1e18$c


z.sk(endowport1, mort, oumodel)
z.sk(endowport100, mort, oumodel)
z.sk(endowport1e18, mort, oumodel)

z.moment(3, termins, mort, oumodel)
z.ev.three.isingle.termsingle(termins, mort, oumodel)
z.ev.twoone.isingle.termsingle(termins, mort, oumodel)
