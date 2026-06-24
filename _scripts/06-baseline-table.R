# Real baselinr output on the bundled (simulated) tutoring dataset.
suppressMessages({library(baselinr); library(ggplot2)})
data(tutoring)
covs <- c("pretest", "attendance", "age", "female", "frpl", "ell")
res <- baseline_equivalence(tutoring, treatment = "treat", covariates = covs)
print(res[, c("covariate", "type", "mean_treatment", "mean_comparison",
              "effect_size", "wwc_category")], digits = 3)
ggsave("writing/posts/01-baseline-table/fig-loveplot.png",
       love_plot(res), width = 7, height = 3, dpi = 150)
cat("figure written\n")
