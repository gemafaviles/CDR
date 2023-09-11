## -----------------------------------------------------------------------------
# to pretty-print all columns in the output tibble
options(
  tibble.width = Inf,
  pillar.bold = TRUE,
  pillar.neg = TRUE,
  pillar.subtle_num = TRUE,
  pillar.min_chars = Inf
)

knitr::opts_chunk$set(
  collapse = TRUE,
  dpi = 300,
  warning = FALSE,
  message = FALSE,
  out.width = "100%",
  comment = "#>"
)

# needed libraries
library(dplyr)
library(statsExpressions)
library(ggplot2)

# for reproducibility
set.seed(123)

## -----------------------------------------------------------------------------
citation("statsExpressions")

## -----------------------------------------------------------------------------
mtcars %>% oneway_anova(cyl, wt, type = "nonparametric")

mtcars %>% oneway_anova(cyl, wt, type = "robust")

## -----------------------------------------------------------------------------
# running one-sample proportion test for all levels of `cyl`
mtcars %>%
  group_by(cyl) %>%
  group_modify(~ contingency_table(.x, am), .keep = TRUE) %>%
  ungroup()

## -----------------------------------------------------------------------------
knitr::include_graphics("../man/figures/stats_reporting_format.png")

## -----------------------------------------------------------------------------
# needed libraries
library(statsExpressions)
library(ggplot2)

# creating a dataframe
res <- oneway_anova(iris, Species, Sepal.Length, type = "nonparametric")

# create a ridgeplot using `ggridges` package
ggplot(iris, aes(x = Sepal.Length, y = Species)) +
  geom_boxplot() + # use 'expression' column to display results in the subtitle
  labs(
    x = "Penguin Species",
    y = "Body mass (in grams)",
    title = "Kruskal-Wallis Rank Sum Test",
    subtitle = res$expression[[1]]
  )

