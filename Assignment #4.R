# Alex Nestorov
# Econ 613
# Assignment #4

install.packages("nlme")
library(nlme)
install.packages("nnet")
library(nnet)

######################################
############# Exercise 1 #############
######################################
## I begin by installing the reshape package which will help me manipulate the data set from long 
## to wide format.
install.packages("reshape2")
library(reshape2)

## I have downloaded the Koop-Tobias dataset and load it into my environment.
data <- read.csv("~/Documents/ECON 613/Koop-Tobias.csv")

## I create a wide data table by person, i.e. a row for each PERSONID that has their wage in each
## of the possible time periods from 0 to 14 based on TIMETRND. This produces "na" in many cases
## as we do not have data for each time period for each person. I re-label the column names.
wage_data_wide <- dcast(data, PERSONID ~ TIMETRND, value.var = "LOGWAGE")
colnames(wage_data_wide) <- c("PERSONID", "lwage_time0", "lwage_time1", "lwage_time2", 
                              "lwage_time3", "lwage_time4", "lwage_time5", "lwage_time6", 
                              "lwage_time7", "lwage_time8", "lwage_time9", "lwage_time10", 
                              "lwage_time11", "lwage_time12", "lwage_time13", "lwage_time14")

## I set a seed to have the same random numbers generated each time.
set.seed(15)
## I then create a random sample of 5 individuals.
sample_five <- as.data.frame(sample(wage_data_wide$PERSONID, size = 5))
colnames(sample_five) <- "PERSONID"
## Finally, I merge the wage data for these 5 random individuals into a data table and present it.
random_five <- merge(wage_data_wide, sample_five, by = "PERSONID")
print(random_five)

######################################
############# Exercise 2 #############
######################################
## I can estimate the random effects model under the normality assumption in two ways. I can do
## it using either regular OLS or GLS, but I obtain the same result. These are both good 
## approximations of the random effects model and the results are identical. I choose to go with
## the GLS estimation here.
re_GLS <- gls(LOGWAGE ~ EDUC + POTEXPER, data)
re_coeffs <- as.data.frame(re_GLS$coefficients)
print(re_coeffs)
## These results are slightly different from those found by the regular random effect formula
## but serve as a strong approximation. The standard errors will also be slightly different.

######################################
############# Exercise 3 #############
######################################
## I first have to reshape the data so that it is in wide format for the fixed effect regressions.
## In other words, I need each time's recording of logwage, education, and experience to now be
## a separate column for each person, with "NA" where we do not have the data.
data_wide <- recast(data, PERSONID ~ TIMETRND + variable, measure.var = c("LOGWAGE", "EDUC", 
                                                                          "POTEXPER"))
## I then re-order the data so that all logwage columns are together, all education columns are 
## together, and all experience columns are together for easier view.
data_wide <- data_wide[, c(1, 2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35, 38, 41, 44, 3, 6, 9, 12, 
                           15, 18, 21, 24, 27, 30, 33, 36, 39, 42, 45, 4, 7, 10, 13, 16, 19, 22, 25,
                           28, 31, 34, 37, 40, 43, 46)]

## Between estimator
## I now calculate the average logwage, average education, and average experience that I will need
## for the between estimator.
data_wide$avg_wage <- rowMeans(data_wide[, 2:16], na.rm = TRUE)
data_wide$avg_educ <- rowMeans(data_wide[, 17:31], na.rm = TRUE)
data_wide$avg_exper <- rowMeans(data_wide[, 32:46], na.rm = TRUE)

## I then regress the average wage on the average education and experience and then print the
## coefficients.
btwn_lm <- lm(avg_wage ~ avg_educ + avg_exper, data_wide)
btwn_coeffs <- as.data.frame(btwn_lm$coefficients)
rownames(btwn_coeffs) <- c("Intercept", "EDUC", "POTEXPER")
print(btwn_coeffs)

## Within estimator
## For the within estimator, I have to again reshape the data from wide to long so that we no
## longer have separate columns for each time period for each relevant variable. I also bring in
## the averages that were calculated in the last section as those will be necessary for the 
## calculation of the within estimator.
data_long <- merge(data, data_wide[, c(1, 47:49)], by = "PERSONID")

## I calculate the difference between actual values and averages for logwage, education, and
## experience for each person as these will be used to get the within estimators.
data_long$within_wage <- data_long$LOGWAGE - data_long$avg_wage
data_long$within_educ <- data_long$EDUC - data_long$avg_educ
data_long$within_exper <- data_long$POTEXPER - data_long$avg_exper

## I then use these calculated within columns and regress the within wage on within education and
## experience, adding a 0 so that a constant term is not included. I then save and print the
## coefficients.
within_lm <- lm(within_wage ~ 0 + within_educ + within_exper, data_long)
within_coeffs <- as.data.frame(within_lm$coefficients)
within_coeffs <- rbind("na", within_coeffs)
rownames(within_coeffs) <- c("Intercept", "EDUC", "POTEXPER")
print(within_coeffs)

## First Time Difference Estimator
## Finally, I have to calculate the first difference for each person. I do this for the time trend
## the logwage, education, and experience. I do so for the time trend so that I can capture those
## differences where they are consecutive periods for each person. For example,  if the rows
## contained for each person show data in time period 0, then 7, then 8, then 11, I only want to
## capture the first difference for time period 7 to 8 since those are consecutive periods.
data_long$timetrnd_diff <- unlist(by(data_long$TIMETRND, list(data_long$PERSONID), 
                                     function(i) c(NA,diff(i))))
data_long$wage_diff <- unlist(by(data_long$LOGWAGE, data_long$PERSONID, function(i) c(NA,diff(i))))
data_long$educ_diff <- unlist(by(data_long$EDUC, list(data_long$PERSONID), 
                                 function(i) c(NA,diff(i))))
data_long$exper_diff <- unlist(by(data_long$POTEXPER, list(data_long$PERSONID), 
                                  function(i) c(NA,diff(i))))

## I then regress the first difference of the logwage on that of education and experience, again
## making sure that there is no constant and only regressing when we have consecutive time periods,
## per the description above. I save and print the coefficients.
diff_lm <- lm(wage_diff ~ 0 + educ_diff + exper_diff, subset(data_long, timetrnd_diff == 1))
diff_coeffs <- as.data.frame(diff_lm$coefficients)
diff_coeffs <- rbind("na", diff_coeffs)
rownames(diff_coeffs) <- c("Intercept", "EDUC", "POTEXPER")
print(diff_coeffs)

## Finally, I gather all of the coefficients together in order to compare them.
fe_coeffs <- cbind(btwn_coeffs, within_coeffs, diff_coeffs)
colnames(fe_coeffs) <- c("Between Estimators", "Within Estimators", "First Diff Estimators")
print(fe_coeffs)

## We can see that the estimates differ quite a bit between the different models. This is obvious
## given the estimation processes are different. The values are also interpreted differently among
## the different models. There is a constant in the between model but there are no constants in 
## the within or first difference models. 

######################################
############# Exercise 4 #############
######################################
## To get 100 random individuals, I follow the exact same process as in exercise 1 but I keep the
## long data set rather than just pulling out the wage data for each person.
sample_100 <- as.data.frame(sample(wage_data_wide$PERSONID, size = 100))
colnames(sample_100) <- "PERSONID"
data_long_100 <- merge(data_long, sample_100, by = "PERSONID")
print(data_long_100)

## I now turn to writing and optimizing the likelihood and estimating the individual fixed effect 
## parameters. First, I create X and Y matrices with the X matrix containing the person ID, 
## education, and experience columns and the Y matrix containing the log wage data. I also create
## a parameter vector that will house the 100 alphas, 2 betas (1 each for education and experience)
## and the standard deviation for my maximum likelihood formula. The standard deviation has to be
## <>0, so I pick 1 here to start.
ind_FE_X <- as.matrix(data_long_100[, c(1:2, 4)])
ind_FE_Y <- as.matrix(data_long_100[, c(3)])
parameters <- as.vector(c(rep(0, 102), 1))

## I then create my maximum likelihood formula, with each of the three above as inputs - an X 
## matrix, a Y matrix, and a parameter vector.
ind_FE_mll <- function (X, Y, p) {
  ## I create a new data frame so that I can map my 100 sampled (unique) person IDs to the alphas.
  ## The alphas are based on the parameter vector that is entered at beginning of the function.
  PID_map <- data.frame(PID = c(unique(X[, 1])), Ps = p[1:length(unique(X[, 1]))])
  ## I create an alphas data frame where I can bring together the parameters and the unique PIDs.
  alphas <- PID_map$Ps[match(X[, 1], PID_map$PID)] 
  ## I create the X*beta matrix which multiplies the education and experience columns with the 
  ## two betas in the parameter vector and then adds the alphas above.
  XB <- as.matrix(X[, c(2:3)])%*%p[101:102] + alphas
  ## Finally, I calculate the (negative) log likelihood of the function assuming a standard 
  ## normal distribution of the error terms (Y - XB) and the standard deviation in my parameter
  ## vector.
  -sum(log(dnorm(Y - XB, sd = p[103])))
}

ind_FE_mll(ind_FE_X, ind_FE_Y, parameters)
## I run the maximum likelihood function and obtain 2880.008 for this initial entry.

## Now, I need to optimize the likelihood in order to estimate the individual fixed effect 
## parameters. I use nlm to do so, and begin testing various starting points for the 
## parameter vector that I enter into my function above. I am looking for the lowest 
## minimum produced.
nlm(ind_FE_mll, X=ind_FE_X, Y=ind_FE_Y, rep(-2, 103))$minimum
## The function blows up (min = 1.80e308), so this is obviously not a good place to start.
nlm(ind_FE_mll, X=ind_FE_X, Y=ind_FE_Y, rep(-1, 103))$minimum
## The case is the same here, min = 1.80e308
nlm(ind_FE_mll, X=ind_FE_X, Y=ind_FE_Y, parameters)$minimum
## This shows more promise, min = 245.08.
nlm(ind_FE_mll, X=ind_FE_X, Y=ind_FE_Y, rep(1, 103))$minimum
## Even better, min = 242.6137.
nlm(ind_FE_mll, X=ind_FE_X, Y=ind_FE_Y, rep(1.5, 103))$minimum
## Seems to be slightly better, min = 242.6136.
nlm(ind_FE_mll, X=ind_FE_X, Y=ind_FE_Y, rep(2, 103))$minimum
## Seems to go back up to 242.6137, so we will stop here.

## Given that using 1.5 as the starting point for the parameters seems to give the best
## estimate, this is the model I will use to calculate the estimates of the individual
## fixed effects for each person. I then save them.
ind_FE_coeffs <- nlm(ind_FE_mll, X=ind_FE_X, Y=ind_FE_Y, rep(2, 103))$estimate

## I then bind the alphas into a data frame providing each alpha to the respective person
## ID for 100 rows.
ind_alphas <- cbind.data.frame(unique(ind_FE_X[, 1]), ind_FE_coeffs[1:100])
colnames(ind_alphas) <- c("PERSONID", "alpha_hat")

## Finally, I bring the invariant variables data into the data frame with the alphas so
## that I can calculate the regression.
ind_data <- merge(ind_alphas, data_long_100[!duplicated(data_long_100$PERSONID), 
                                            c(1, 6:10)], by = "PERSONID")

## I run a regression of estimated individual fixed effects on the invariant variables.
## I save and print them as well.
ind_FE_lm <- lm(alpha_hat ~ ABILITY + MOTHERED + FATHERED + BRKNHOME + SIBLINGS, ind_data)
print(ind_FE_lm)

## Finally, we know that standard errors here may be inconsistently estimated. This is
## because any time one has a multi-step regression process, you create errors. When we
## run the first regression to get our alpha_hats, we are estimating these alpha_hats 
## and so these individual fixed effect parameters have an error term associated with
## them. When we then run our second regression of these individual estimated fixed
## effects on the invariant variables, we are now treatubg these alpha_hats as if they
## are actual data without an error, but they are not. Thus, we are ignoring the error
## term that came with estimating them. There is measurement error. In order to avoid 
## this measurement error, we can bootstrap the standard errors, as we have done in
## previous problem sets.

## I start off by creating my bootstrap function which takes as inputs the data frame
## that I will be using and number of repetitions.
bootstrap <- function(data, reps) {
  ## I create a matrix with the number of rows based on number of repetitions of the 
  ## bootstrap and number of columns of invariant variables + one for the intercept.
  ## I rename the columns appropriately.
  beta_boot <- matrix(0, reps, 6)
  colnames(beta_boot) <- c("Intercept", "ABILITY", "MOTHERED", "FATHERED", "BRKNHOME", 
                           "SIBLINGS") 
  ## I then begin my for loop using the number of repetitions again.
  for (i in 1:reps) {
    ## I create a sample data frame that will pull in a unique group of people IDs 
    ## from the subsample of 100 every time that the loop runs and will do so with
    ## replacement.
    boot_sample <- as.data.frame(sample(unique(data$PERSONID), 100, replace = TRUE))
    colnames(boot_sample) <- "PERSONID"
    ## I merge the rest of the relevant data from the 100 subsample into a new data
    ## frame.
    boot_data <- merge(data, boot_sample, by = "PERSONID")
    ## I then need to create dummy variables for each person within this sample of 
    ## the sample in order to be able to run the OLS regression on these dummies.
    sub_boot_data <- with(boot_data[, c(1:4)], data.frame(class.ind(PERSONID), EDUC, 
                                                          LOGWAGE, POTEXPER))
    ## I then run the OLS regression of logwage on these dummy variables and educ
    ## and experience columns.
    boot_OLS <- lm(LOGWAGE ~ ., sub_boot_data) 
    ## I pull out the alphas for the sample of the sample so there is a fixed effect
    ## coefficient for each individual in this smaller sample from the 100 (given 
    ## there are going to be repeats as we selected replacement).
    est_alphas <- as.vector(boot_OLS$coefficients
                            [2:(length(unique(boot_data$PERSONID))+1)]) 
    ## Finally, I pull out the unique rows and relevant invariant data from the 
    ## data frame created above into a smaller one that will have a row for each
    ## person so that I can run the OLS. I also bring in the alphas from the above
    ## data frame so I have them together with the invariant variables.
    sub_boot_data_2 <- unique(boot_data[, c(1, 6:10)])
    sub_boot_data_2$alphas <- est_alphas 
    ## I run the regression and then save the coefficients in the empty matrix I 
    ## created above, and then the for loop runs again until it fills that matrix.
    boot_ind_FE_OLS <- lm(alphas ~ ABILITY + MOTHERED + FATHERED + BRKNHOME + SIBLINGS, 
                          sub_boot_data_2)
    beta_boot[i,] <- t(as.vector(boot_ind_FE_OLS$coefficients))
  }
  ## Finally, I take the standard deviation of all of the coefficients in the now-
  ## filled matrix and save it as another matrix.
  std_err <- apply(beta_boot, 2, sd, na.rm = TRUE)
}

## I run my bootstrap using 99 replications, and I print the standard errors which 
## we can see are similar to the standard errors from the regular process, though
## we are now more convinced that they are correct.
boot_SEs <- bootstrap(data_long_100, 99)
summary(ind_FE_lm)
print(boot_SEs)
