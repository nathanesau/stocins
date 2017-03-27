library(stocins)

mort = mortassumptions(list(x = 40, table = "MaleMort82"))

oumodel = iratemodel(list(delta0 = 0.08, delta = 0.06,
                          alpha = 0.1, sigma = 0.01), "ou")

## Term Insurance

termins = insurance(params = list(n = 10, d=100), "isingle", "term")
termport1 = insurance(params = list(single = termins, c = 1), "iport", "term")
termport10 = insurance(params = list(single = termins, c = 10), "iport", "term")
termport1e09 = insurance(params = list(single = termins, c = 1e09), "iport", "term")

# E[Z(c)]/c
z.moment(1, termport1, mort, oumodel) / termport1$c
z.moment(1, termport10, mort, oumodel) / termport10$c
z.moment(1, termport1e09, mort, oumodel)/ termport1e09$c

# E[Z(c)^2]/c
z.moment(2, termport1, mort, oumodel) / termport1$c
z.moment(2, termport10, mort, oumodel) / termport10$c^2
z.moment(2, termport1e09, mort, oumodel) / termport1e09$c^2

# sd[Z(c)]/c
z.sd(termport1, mort, oumodel) / termport1$c
z.sd(termport10, mort, oumodel) / termport10$c
z.sd(termport1e09, mort, oumodel) / termport1e09$c

## Endowment Insurance

endowins = insurance(params = list(n=10, d=100, e=100), "isingle", "endow")
endowport1 = insurance(params = list(single = endowins, c = 1), "iport", "endow")
endowport10 = insurance(params = list(single = endowins, c = 10), "iport", "endow")
endowport1e09 = insurance(params = list(single = endowins, c = 1e09), "iport", "endow")

# E[Z(c)]/c
z.moment(1, endowport1, mort, oumodel) / endowport1$c
z.moment(1, endowport10, mort, oumodel) / endowport10$c
z.moment(1, endowport1e09, mort, oumodel)/ endowport1e09$c

# E[Z(c)^2]/c
z.moment(2, endowport1, mort, oumodel) / endowport1$c
z.moment(2, endowport10, mort, oumodel) / endowport10$c^2
z.moment(2, endowport1e09, mort, oumodel) / endowport1e09$c^2

# sd[Z(c)]/c
z.sd(endowport1, mort, oumodel) / endowport1$c
z.sd(endowport10, mort, oumodel) / endowport10$c
z.sd(endowport1e09, mort, oumodel) / endowport1e09$c