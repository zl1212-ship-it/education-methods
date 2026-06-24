# Simulated demo for "Students aren't independent".
# School-level treatment, NO true effect, strong within-school correlation.
# Naive student-level SEs understate uncertainty -> inflated false positives;
# cluster-robust SEs fix it. All data simulated.
suppressMessages({library(ggplot2); library(sandwich)})

sim_study <- function(n_school = 60, per = 50, tau = 0.6, sigma = 1, effect = 0) {
  school <- rep(seq_len(n_school), each = per)
  u <- rnorm(n_school, 0, tau)[school]              # school random effect (clustering)
  treat <- rep(rbinom(n_school, 1, 0.5), each = per) # treatment assigned at school level
  y <- effect * treat + u + rnorm(n_school * per, 0, sigma)
  data.frame(y, treat, school)
}

icc <- 0.6^2 / (0.6^2 + 1^2)
cat(sprintf("ICC = %.2f | design effect (per=50) = %.1f\n", icc, 1 + (50 - 1) * icc))

# --- one illustrative null draw where naive is "significant" but clustering is not
pick <- NA
for (s in 1:60) {
  set.seed(s); d <- sim_study()
  m <- lm(y ~ treat, d)
  se_n <- sqrt(diag(vcov(m)))["treat"]
  se_c <- sqrt(diag(vcovCL(m, cluster = ~school)))["treat"]
  p_n <- 2 * pnorm(-abs(coef(m)["treat"] / se_n))
  p_c <- 2 * pnorm(-abs(coef(m)["treat"] / se_c))
  if (p_n < 0.01 && p_c > 0.10) { pick <- s; break }
}
set.seed(pick); d <- sim_study(); m <- lm(y ~ treat, d)
se_n <- sqrt(diag(vcov(m)))["treat"]; se_c <- sqrt(diag(vcovCL(m, cluster = ~school)))["treat"]
b <- coef(m)["treat"]
cat(sprintf("\nExample null draw (true effect = 0):\n"))
cat(sprintf("  estimate = %.3f\n", b))
cat(sprintf("  naive SE = %.3f -> p = %.4f\n", se_n, 2 * pnorm(-abs(b / se_n))))
cat(sprintf("  cluster-robust SE = %.3f -> p = %.3f\n", se_c, 2 * pnorm(-abs(b / se_c))))

# --- false-positive rate across many null studies
set.seed(1)
R <- 600
fp <- t(sapply(seq_len(R), function(i) {
  d <- sim_study()
  m <- lm(y ~ treat, d)
  b <- unname(coef(m)["treat"])
  pn <- 2 * pnorm(-abs(b / unname(sqrt(diag(vcov(m)))["treat"])))
  pc <- 2 * pnorm(-abs(b / unname(sqrt(diag(vcovCL(m, cluster = ~school)))["treat"])))
  c(naive = pn < 0.05, cluster = pc < 0.05)
}))
rate <- colMeans(fp)
cat(sprintf("\nFalse-positive rate over %d null studies: naive %.0f%% | cluster-robust %.0f%%\n",
            R, 100 * rate["naive"], 100 * rate["cluster"]))

df <- data.frame(method = c("Naive\n(student-level)", "Cluster-robust"),
                 rate = 100 * as.numeric(rate))
p <- ggplot(df, aes(x = rate, y = method)) +
  geom_vline(xintercept = 5, linetype = "dashed", colour = "grey50") +
  geom_col(fill = "#d95f02", width = 0.5) +
  labs(x = "% of NULL studies declared significant (p < .05)", y = NULL,
       title = "Ignoring clustering manufactures false positives",
       subtitle = "Simulated null studies; dashed line = the 5% you signed up for") +
  theme_minimal(base_size = 12)
ggsave("writing/posts/clustering-independence/fig-falsepos.png", p, width = 7, height = 2.6, dpi = 150)
cat("figure written\n")
