###### What variables have the bigger impact on family income? ######

library(car)
data(catholic, package='wooldridge')
summary(catholic)
# hsgrad has 1460 Nan values

1460/7430* 100
# Impute missing values in the hsgrad variable with the mode
mode_hsgrad <- as.numeric(names(sort(table(catholic$hsgrad), decreasing = TRUE)[1]))
catholic$hsgrad <- ifelse(is.na(catholic$hsgrad), mode_hsgrad, catholic$hsgrad)


# unrestricted model, 1st approach, only with the variables we think make sense for our reserach question

reg_UR = lm(lfaminc ~ fatheduc + motheduc+ read12 + math12 + black + hsgrad, data = catholic  )
summary(reg_UR)


# inicial model, without read12, cause it has a p-value > 0.05 so its not individual significant
reg_inicial = lm(lfaminc ~ fatheduc + motheduc + math12 + black + hsgrad, data = catholic )
summary(reg_inicial)

## Heteroskedasticity ## 
library(lmtest)

#teste breuch-pagan
bptest(reg_inicial)

# Teste White
bptest(reg_inicial, ~ fatheduc+ motheduc + math12 +
         black  + hsgrad + I(fatheduc^2) + I(motheduc^2)+
         I(math12^2) + I(black^2) + I(hsgrad^2) +
         I(fatheduc*motheduc) + I(fatheduc*math12) + 
         I(fatheduc*black) + I(fatheduc*hsgrad) + 
         I(motheduc*math12) + I(motheduc*black)+
         I(motheduc*hsgrad) + I(math12*black) + 
         I(math12*hsgrad) + I(black*hsgrad),data=catholic)

# Teste White simplificado
bptest(reg_inicial, ~ fitted(reg_inicial) + I(fitted(reg_inicial)^2) )

# all the tests have a p-value < 0.05 so we reject the null and we have statistical evidence that we have heteroskedasticity in our model


## Robust Estimation ## 

# Robust estimation techniques, such as robust standard errors, help address issues related to heteroscedasticity and other violations of assumptions
# we must resort to a robust variance-covariance matrix to perform statistical inference.

coeftest(reg_inicial)
robust <- coeftest(reg_inicial, vcov=hccm)
robust

# we can trust the standard errors

## RESET Test ##

RESET <- lm(lfaminc ~ fatheduc + motheduc + math12 + black + hsgrad +I(fitted(reg_inicial)^2)+
                 I(fitted(reg_inicial)^3),data = catholic)

linearHypothesis(RESET, matchCoefs(RESET,"fitted"))

# misspecification

hist(resid(reg_inicial), probability = TRUE , main="Histogram Log-Level")
x <- seq(-8, 4, length.out=100)
y <- dnorm(x, mean(resid(reg_inicial)), sd(resid(reg_inicial)))
lines(x, y, col = "blue")

