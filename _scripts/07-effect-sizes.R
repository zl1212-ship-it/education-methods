# Simulated demo for "Effect sizes for education".
# True standardized effect = 0.5. Cohen's d (uncorrected) is biased upward in
# small samples; Hedges' g (with the WWC correction) is ~unbiased. Simulated.
suppressMessages({library(ggplot2); library(baselinr)})
set.seed(2026)
true_d <- 0.5
ns <- c(3, 5, 10, 20, 50, 100)
reps <- 4000

one <- function(n) {
  a <- rnorm(n, true_d); b <- rnorm(n, 0)
  sp <- sqrt(((n - 1) * var(a) + (n - 1) * var(b)) / (2 * n - 2))
  d <- (mean(a) - mean(b)) / sp
  c(d = d, g = d * (1 - 3 / (4 * (2 * n) - 9)))
}
res <- do.call(rbind, lapply(ns, function(n) {
  m <- rowMeans(replicate(reps, one(n)))
  data.frame(n = n, Cohens_d = m["d"], Hedges_g = m["g"])
}))
print(round(res, 3), row.names = FALSE)

# sanity: baselinr::hedges_g reproduces the formula
chk <- hedges_g(c(rnorm(5, .5), rnorm(5)), rep(c(1, 0), each = 5))
cat(sprintf("\n(baselinr::hedges_g sanity value: %.3f)\n", chk))

long <- rbind(
  data.frame(n = res$n, estimate = res$Cohens_d, measure = "Cohen's d (uncorrected)"),
  data.frame(n = res$n, estimate = res$Hedges_g, measure = "Hedges' g (corrected)")
)
p <- ggplot(long, aes(n, estimate, colour = measure)) +
  geom_hline(yintercept = true_d, linetype = "dashed", colour = "grey50") +
  geom_line(linewidth = 1) + geom_point(size = 2.5) +
  scale_x_log10(breaks = ns) +
  scale_colour_manual(values = c("Cohen's d (uncorrected)" = "#d7191c",
                                 "Hedges' g (corrected)" = "#1b9e77")) +
  labs(x = "Per-group sample size (log scale)", y = "Average estimated effect",
       colour = NULL, title = "Cohen's d overstates the effect in small samples",
       subtitle = "Simulated; true effect = 0.5 (dashed). Hedges' g stays honest.") +
  theme_minimal(base_size = 12) + theme(legend.position = "top")
ggsave("writing/posts/02-effect-sizes-wwc/fig-bias.png", p, width = 7, height = 3.4, dpi = 150)
cat("figure written\n")
