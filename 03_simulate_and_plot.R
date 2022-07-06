# Written by Thijs

n <- 1000

found <- matrix(nrow = n, ncol = 48)
pb <- txtProgressBar(max = n, style = 3)
for (r in 1:n) {
  # Generate random values of tau (speciation duration in Myr)
  tau <- 2 # runif(1, 0, 10)

  # Pick constant, reasonable speciation and extinction rates
  b1 <- 0.1
  b2 <- rnorm(n = 1, mean = b1, sd = 0.02)
  mu1 <- mu2 <- 0

  # Note: they are equal for good (#1) and incipient (#2) species

  # Deduce speciation completion rate from speciation duration
  lambda <- b2 / (exp(tau * b2) - 1)

  focal_tree <- pbd_sim(pars = c(b1, lambda, b2, mu1, mu2), age = 40)

  stats <- treestats::calc_all_stats(focal_tree$stree_random, normalize = TRUE)
  stats <- unlist(stats)

  to_add <- c(lambda, b2 / b1, stats)
  found[r, ] <- to_add

  setTxtProgressBar(pb, r)
}

test_tree <- ape::rphylo(n = 10, 1, 0)
test_stats <- treestats::calc_all_stats(test_tree)
colnames(found) <- c("tau", "spec_2", names(test_stats))

found <- as_tibble(found)
plot1 <- found %>%
  gather(key = "statistic", value = "val", -c(tau, spec_2)) %>%
  ggplot(aes(x = spec_2, y = val)) +
    geom_point() +
    stat_smooth() +
    facet_wrap(~statistic, scales = "free")

ggsave("summary_b2.png", width = 20, height = 20, dpi = 300)

