# Simulated demo for "Your classifier is 85% accurate...".
# True effect of a text-derived feature on an outcome = 1.0.
# (1) random misclassification attenuates the estimate; (2) differential
# misclassification (worse for one group that also affects the outcome) biases
# it further / non-trivially. All data simulated.
suppressMessages(library(ggplot2))
set.seed(2026)
n <- 6000
G <- rbinom(n, 1, 0.5)                 # teacher type (e.g., serves many multilingual learners)
Z <- rbinom(n, 1, 0.5)                 # TRUE label: feedback is "specific"
Y <- 1.0 * Z + 0.7 * G + rnorm(n)      # student progress; true effect of Z = 1.0

flip <- function(z, p) ifelse(runif(length(z)) < p, 1 - z, z)
Zhat_nd <- flip(Z, 0.15)                          # 85% accurate, non-differential
p_diff  <- ifelse(G == 1, 0.23, 0.07)             # worse for G==1
Zhat_d  <- flip(Z, p_diff)                         # ~85% overall, differential

acc <- function(zhat) mean(zhat == Z)
cat(sprintf("overall accuracy: non-diff %.2f | differential %.2f\n", acc(Zhat_nd), acc(Zhat_d)))
cat(sprintf("differential accuracy by group: G=0 %.2f | G=1 %.2f\n",
            mean(Zhat_d[G == 0] == Z[G == 0]), mean(Zhat_d[G == 1] == Z[G == 1])))

est <- c(
  "True label (Y ~ Z)"                 = coef(lm(Y ~ Z))["Z"],
  "Predicted, non-differential error"  = coef(lm(Y ~ Zhat_nd))["Zhat_nd"],
  "Predicted, differential error"      = coef(lm(Y ~ Zhat_d))["Zhat_d"]
)
for (nm in names(est)) cat(sprintf("%-36s %.2f\n", nm, est[[nm]]))

df <- data.frame(model = factor(names(est), levels = rev(names(est))), estimate = as.numeric(est))
p <- ggplot(df, aes(x = estimate, y = model)) +
  geom_vline(xintercept = 1.0, linetype = "dashed", colour = "grey50") +
  geom_point(size = 4, colour = "#7570b3") +
  xlim(0, 1.2) +
  labs(x = "Estimated effect of the (text-derived) feature", y = NULL,
       title = "An 85%-accurate classifier does not give you an unbiased estimate",
       subtitle = "Simulated data; dashed line = true effect (1.0)") +
  theme_minimal(base_size = 12)
ggsave("writing/posts/text-classifier-error/fig-attenuation.png", p, width = 7, height = 2.8, dpi = 150)
cat("figure written\n")
