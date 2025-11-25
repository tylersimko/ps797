#----------------------------------------------------
# POLSCI797: Examples of CausalML code in R
#----------------------------------------------------

#------------------------------------------------------------
# Double Machine Learning
# Example based on documentation:
# https://docs.doubleml.org/r/stable/articles/getstarted.html

# remotes::install_github("DoubleML/doubleml-for-r")
library(DoubleML)
library(mlr3)
library(mlr3learners)
set.seed(797)

#----------------------------------------------------------------------
# Step 1: Get data in "correct" format package wants
df = fetch_bonus(return_type = "data.table")
head(df)

# Helper function to get data into right format
dml_data = DoubleMLData$new(
  df,
  y_col = "inuidur1",
  # outcome
  d_cols = "tg",
  # treatment
  x_cols = c(
    "female",
    "black",
    "othrace",
    "dep1",
    "dep2",
    "q2",
    "q3",
    "q4",
    "q5",
    "q6",
    "agelt35",
    "agegt54",
    "durable",
    "lusd",
    "husd"
  )
)
print(dml_data)
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# 2. Define your "learner" models
# Here, use random forest for both functions—could be anything
# See: https://mlr3.mlr-org.com/

# Remember tidymodels? This line is similar: sets "recipe" for model
learner = lrn(
  "regr.ranger",
  num.trees = 500,
  max.depth = 5,
  min.node.size = 2
)
ml_l = learner$clone() # outcome model
ml_m = learner$clone() # treatment model
#----------------------------------------------------------------------
# 3. Fit DoubleML model for your functional form

# Notice here we use PLR: "partially linear" regression
# See other options: e.g. DoubleMLIRM
dml_fit = DoubleMLPLR$new(dml_data, ml_l = ml_l, ml_m = ml_m)
dml_fit$fit()
print(dml_fit)

names(dml_fit)
#----------------------------------------------------------------------
# CausalForest
# Example adapted from Athey/Wager
# https://www.youtube.com/watch?v=gaX_HP0tdRE&list=PLxq_lXOUlvQAoWZEqhRqHNezS30lI49G-&index=15

#----------------------------------------------------------------------
# 1. Simulate some fake data
n = 4000
p = 10
X = matrix(rnorm(n * p), n, p)
W = rbinom(n, 1, 1 / (1 + exp(-X[, 3])))
TAU = 1
Y = 2 * pmax(X[, 1] + X[, 2] + X[, 3], 0) + W * TAU + rnorm(n)

train_idx <- 1:3000
test_idx <- 3001:4000
Xtrain = X[train_idx, ]
Xtest = X[test_idx, ]
#----------------------------------------------------------------------
# 2. Fit causal forest
cf = grf::causal_forest(Xtrain, Y[train_idx], W[train_idx], tune.parameters = "all")
tau_pred = predict(cf, newdata = Xtest)
length(tau_pred$predictions)

plot(Xtest[, 3], tau_pred$predictions, ylim = c(-1, 3))
abline(a = 1, b = 0, col = "red")

hist(tau_pred$predictions)

#-----------------------------------------------------------------------
