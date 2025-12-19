library(mirai)
library(purrr)


default_param <- 0.5

delay <- function(secs = default_param) {
  Sys.sleep(secs)
}

slow_lm <- function(formula, data) {
  delay()
  lm(formula, data)
}

# Example of a 'crate' returned by in_parallel(). The object print method
# shows the size of the crate and any objects contained within:
crate <- in_parallel(
  \(df) slow_lm(mpg ~ disp, data = df),
  slow_lm = slow_lm,
  delay = delay,
  default_param = default_param
)
crate

# Use mirai::mirai() to test that a crate is self-contained
# by running it in a daemon and collecting its return value:
mirai::mirai(crate(mtcars), crate = crate) |> mirai::collect_mirai()
