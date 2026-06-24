# Simulated demo for "More controls != less bias".
# Shows: adjusting for a pre-treatment confounder is good; additionally
# adjusting for a POST-treatment mediator over-controls and biases the
# estimated TOTAL program effect toward zero. All data are simulated.
suppressMessages(library(ggplot2))
set.seed(2026)
n <- 4000
C <- rnorm(n)                                   # pre-treatment trait (confounder)
treat <- rbinom(n, 1, plogis(C))                # selection: high-C more likely treated
M <- 1.5 * treat + 0.5 * C + rnorm(n)           # post-treatment mediator (e.g. motivation)
Y <- 4 * treat + 2 * C + 3 * M + rnorm(n, 0, 2) # outcome
# True TOTAL effect of treat on Y = direct 4 + indirect (1.5*3) = 8.5
truth <- 4 + 1.5 * 3

est <- c(
  "Naive (Y ~ treat)"                       = coef(lm(Y ~ treat))["treat"],
  "+ confounder (Y ~ treat + C)"            = coef(lm(Y ~ treat + C))["treat"],
  "+ mediator too (Y ~ treat + C + M)"      = coef(lm(Y ~ treat + C + M))["treat"]
)
cat(sprintf("TRUE total effect: %.2f\n", truth))
for (nm in names(est)) cat(sprintf("%-38s %.2f\n", nm, est[[nm]]))

df <- data.frame(model = factor(names(est), levels = names(est)), estimate = as.numeric(est))
p <- ggplot(df, aes(x = estimate, y = model)) +
  geom_vline(xintercept = truth, linetype = "dashed", colour = "grey50") +
  geom_point(size = 4, colour = "#d95f02") +
  labs(x = "Estimated treatment effect", y = NULL,
       title = "Adding the wrong control moves the estimate the wrong way",
       subtitle = sprintf("Simulated data; dashed line = true total effect (%.1f)", truth)) +
  theme_minimal(base_size = 12)
ggsave("/Users/yuxialiang/bridge-year/writing/posts/controls-collider/fig-overcontrol.png",
       p, width = 7, height = 2.8, dpi = 150)
cat("figure written\n")
