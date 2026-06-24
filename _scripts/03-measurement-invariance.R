# Simulated demo for "Before you compare two groups' survey scores".
# Two groups have EQUAL true latent engagement, but one item functions
# differently across groups (DIF). The observed scale mean shows a spurious
# difference. All data simulated.
suppressMessages({library(ggplot2); library(baselinr)})
set.seed(2026)
nA <- nB <- 1500
group <- c(rep(0L, nA), rep(1L, nB))     # 0 = group A, 1 = group B
theta <- rnorm(nA + nB)                   # TRUE latent engagement: identical distribution
items <- sapply(1:5, function(j) theta + rnorm(nA + nB, 0, 0.8))
items[, 5] <- items[, 5] - 1.2 * (group == 1)   # DIF: item 5 reads lower for group B net of theta
colnames(items) <- paste0("item", 1:5)

scale_full <- rowMeans(items)
scale_inv  <- rowMeans(items[, 1:4])      # invariant items only

cat(sprintf("TRUE latent diff (Hedges' g on theta): %.3f\n", hedges_g(theta, group)))
cat(sprintf("Observed FULL-scale diff (Hedges' g):  %.3f\n", hedges_g(scale_full, group)))
cat(sprintf("Invariant-items-only diff (Hedges' g): %.3f\n", hedges_g(scale_inv, group)))

diff_by_item <- colMeans(items[group == 1, ]) - colMeans(items[group == 0, ])
print(round(diff_by_item, 2))

df <- data.frame(item = factor(colnames(items), levels = colnames(items)),
                 diff = as.numeric(diff_by_item))
p <- ggplot(df, aes(x = diff, y = item)) +
  geom_vline(xintercept = 0, colour = "grey60") +
  geom_point(size = 4, colour = "#1b9e77") +
  labs(x = "Observed group difference (B - A), item mean", y = NULL,
       title = "One item is doing all the work",
       subtitle = "Simulated; groups have equal true engagement. Only item 5 differs (DIF).") +
  theme_minimal(base_size = 12)
ggsave("writing/posts/measurement-invariance/fig-dif.png", p, width = 7, height = 2.8, dpi = 150)
cat("figure written\n")
