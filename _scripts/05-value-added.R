# Simulated demo for "What a teacher-effectiveness score measures".
# Teachers have a STABLE true effect, but value-added estimates are noisy
# (small classes, test error). Year-to-year rankings swing on noise alone.
# All data simulated.
suppressMessages(library(ggplot2))
set.seed(2026)
nT <- 300
mu <- rnorm(nT, 0, 1)                    # TRUE, stable teacher effect
sd_noise <- sqrt(1.5)                    # tuned for a realistic ~0.4 reliability
vam1 <- mu + rnorm(nT, 0, sd_noise)      # year 1 estimate
vam2 <- mu + rnorm(nT, 0, sd_noise)      # year 2 estimate (same true mu)

rel <- cor(vam1, vam2)
pct1 <- rank(vam1) / nT * 100
pct2 <- rank(vam2) / nT * 100
cat(sprintf("year-to-year reliability (correlation of estimates): %.2f\n", rel))

# churn: of teachers in the bottom quartile in year 1, how many are above the
# median in year 2?
bottomY1 <- pct1 <= 25
cat(sprintf("of bottom-quartile (Y1) teachers, %.0f%% land above the median in Y2\n",
            100 * mean(pct2[bottomY1] > 50)))

# an example teacher who jumps from ~40th to ~70th on noise alone
i <- which.min(abs(pct1 - 40) + abs(pct2 - 70))
cat(sprintf("example teacher: %.0fth percentile (Y1) -> %.0fth percentile (Y2), same true skill\n",
            pct1[i], pct2[i]))

df <- data.frame(pct1, pct2)
p <- ggplot(df, aes(pct1, pct2)) +
  geom_abline(slope = 1, intercept = 0, colour = "grey80") +
  geom_point(alpha = 0.4, colour = "#7570b3") +
  geom_point(data = df[i, ], size = 4, colour = "#d95f02") +
  annotate("text", x = pct1[i], y = pct2[i], label = "  same teacher",
           hjust = 0, vjust = 0.2, size = 3.4, colour = "#d95f02") +
  labs(x = "Year 1 percentile rank", y = "Year 2 percentile rank",
       title = "Year 1 barely predicts year 2",
       subtitle = sprintf("Simulated; true skill is unchanged. Year-to-year reliability = %.2f", rel)) +
  theme_minimal(base_size = 12)
ggsave("writing/posts/value-added-scores/fig-vam-churn.png", p, width = 6.5, height = 4, dpi = 150)
cat("figure written\n")
