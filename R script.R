library(plm)
library(lmtest)
library(ggplot2)


# EXPLORING THE DATASET
dt = prodfn_data_5_3

head(dt)
summary(dt)

# Histogram of the dependent variable
hist(dt$y, main = "Histogram of Output")

# plotting the output and the time identifier
ggplot(dt, aes(x = tvar, y = y)) +
  geom_point() +
  labs(x = "Time identifier", y = "Output", title = "Production over time")

# Scatter plot of l (labour) and k (capital) against y (output)
plot(dt$l, dt$y, main = "Labour vs. Output", xlab = "Labour", ylab = "Output")
plot(dt$k, dt$y, main = "Capital vs. Output", xlab = "Capital", ylab = "Output")

# REGRESSIONS #
dt <- pdata.frame(dt, index=c("ivar", "tvar"))

# Fixed Effect model
fe_model <- plm(y ~ l + k, data = dt, model = "within")
summary(fe_model)

#POLS 
pols <- plm(y ~ k + l, data = dt, model = "pooling")
summary(pols)

# Random Effects model
re_model <- plm(y ~ l + k, data = dt, model = "random")
summary(re_model)

# First-Differenced model
fd_model <- plm(y ~ l + k, data = dt, model = "fd")
summary(fd_model)

# Two-way FE
twoway <- plm(y ~ l + k, data = dt, model = "within", effect = "twoways")
summary(twoway)


# TESTS
# at this point, we will perform some tests to understand which is the best model.

# LM TEST
plmtest(pols, alternative = "individual", type = "bp")
## we prefer RE

# HASUMAN TEST
phtest(fe_model, re_model) # FE better than RE
phtest(fd_model, re_model) # FD better than RE
phtest(twoway, re_model) # Two-Way FE better than RE

# BREUSH-PAGAN TEST
bptest(fe_model)
bptest(re_model)
bptest(fd_model)
bptest(pols)
bptest(twoway)
## no heteroskedasticity

# WOOLDRIDGE TEST
pbgtest(fe_model)
pbgtest(fd_model)
pbgtest(twoway)






