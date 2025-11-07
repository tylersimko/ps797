#-----------------------------------------------------------------
# Data to write for class
# Goal: predict total turnout at 9pm
nyc <- read_csv("~/Documents/teaching/p797/slides/class_18/nyc_elec.csv")

# Recode timestamps to numeric
nyc <- nyc |>
  mutate(
    time = case_when(
      time == "9am" ~ 3,
      time == "12pm" ~ 6,
      time == "3pm" ~ 9,
      time == "6pm" ~ 12,
      time == "9pm" ~ 15
    )
  )

# Pivot so each row is year x time x borough
train <- nyc |>
  pivot_longer(cols = Manhattan:`Staten Island`)

# We want to predict turnout at 9pm (15 hours since polls opened) in each borough
test <- tibble(
  year = 2025,
  time = 15,
  name = c("Brooklyn", "Manhattan", "Queens", "Bronx", "Staten Island")
)

# Fit a GAM: time control, with interaction between year*borough*time
# e.g. time trend can depend on borough and year
gam_fit <- mgcv::gam(
  value ~ s(time, k = 5) + as.factor(year) * name * time,
  data = train
)
summary(gam_fit)

# Predict turnout for each borough
(yhat_b <- predict(gam_fit, newdata = test))

# Sum to get citywide turnout
sum(yhat_b)

#-----------------------------------------------------------------
# https://enr.boenyc.gov/CD27286ADI0.html
# Results as of 11/6. About 94% complete
tibble(
  value = c(658199, 521767, 503989, 223042, 148924),
  name = c("Brooklyn", "Manhattan", "Queens", "Bronx", "Staten Island")
) |>
  pull(value) |>
  sum() /
  0.94
#-----------------------------------------------------------------
