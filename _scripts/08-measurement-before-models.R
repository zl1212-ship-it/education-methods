# Simulated demo for "Measurement before models".
# A real program effect (true standardized = 0.5) measured with decreasing
# reliability: the observed effect shrinks (~ true * sqrt(reliability)) and
# power collapses. Simulated.
suppressMessages({library(ggplot2); library(baselinr)})
set.seed(2026)
true_d <- 0.5; n <- 50; reps <- 3000
rels <- c(1.0, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4)

one <- function(rho) {
  yt <- rnorm(n, true_d); yc <- rnorm(n, 0)            # latent outcome
  esd <- sqrt((1 - rho) / rho)                         # measurement-error SD
  yt <- yt + rnorm(n, 0, esd); yc <- yc + rnorm(n, 0, esd)
  g <- hedges_g(c(yt, yc), rep(c(1, 0), each = n))
  p <- t.test(yt, yc)$p.value
  c(g = g, sig = p < 0.05)
}
res <- do.call(rbind, lapply(rels, function(rho) {
  m <- rowMeans(replicate(reps, one(rho)))
  data.frame(reliability = rho, observed_g = m["g"], power = m["sig"])
}))
print(round(res, 3), row.names = FALSE)

p <- ggplot(res, aes(reliability, observed_g)) +
  geom_hline(yintercept = true_d, linetype = "dashed", colour = "grey50") +
  geom_line(linewidth = 1, colour = "#1b9e77") + geom_point(size = 3, colour = "#1b9e77") +
  scale_x_reverse(breaks = rels) + ylim(0, 0.55) +
  labs(x = "Outcome reliability (worse -->)", y = "Observed standardized effect",
       title = "A noisy outcome shrinks the effect you can report",
       subtitle = "Simulated; true effect = 0.5 (dashed). Same program, worse ruler.") +
  theme_minimal(base_size = 12)
ggsave("writing/posts/03-measurement-before-models/fig-attenuation.png", p, width = 7, height = 3.2, dpi = 150)
cat("figure written\n")
