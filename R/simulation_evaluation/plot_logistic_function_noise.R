library(ggplot2)

logistic_function_nonoise <- function(m, Mimpact, b) {
  c <- Mimpact / 2
  k <- 20 / (Mimpact + 1)
  r <- k * c - log((1 / b) - 1)
  return(1 / (1 + exp(-k * (m - c) - r)))
}


logistic_function_noise <- function(m, Mimpact, b) {
  c <- Mimpact / 2
  k <- 20 / (Mimpact + 1)
  r <- k * c - log((1 / b) - 1)
  return((1 - b) / (1 + exp(-k * (m - c) - r)))
}


generate_combined_data <- function(Mtot_percents, Nmut) {
  df_list <- list()

  for (Mtot_percent in Mtot_percents) {
    Mimpact <- Mtot_percent * Nmut
    Mtot <- Mtot_percent

    m_values1 <- seq(0, Mimpact, length.out = 100)
    prob_values1 <- sapply(m_values1, logistic_function_nonoise, Mimpact = Mimpact, b = 0.01)

    df1 <- data.frame(
      m = m_values1,
      probability = prob_values1,
      Mtot = factor(paste0(Mtot_percent * 100, "%"), levels = c("1%", "10%", "50%")),
      noise = "no"
    )

    m_values2 <- seq(0, Mimpact, length.out = 100)
    prob_values2 <- sapply(m_values2, logistic_function_noise, Mimpact = Mimpact, b = 0.2)

    df2 <- data.frame(
      m = m_values2,
      probability = prob_values2,
      Mtot = factor(paste0(Mtot_percent * 100, "%"), levels = c("1%", "10%", "50%")),
      noise = "yes"
    )

    df_list[[paste0("Mtot_", Mtot_percent)]] <- rbind(df1, df2)
  }

  return(do.call(rbind, df_list))
}


Mtot_percents <- c(0.01, 0.10, 0.50)
Nmut <- 100


data <- generate_combined_data(Mtot_percents, Nmut)


ggplot(data, aes(x = m, y = probability, color = Mtot, linetype = noise)) +
  geom_line(size = 1.2) +
  labs(
    x = "Number of causal mutations (m)",
    y = "P(severe | m)",
    color = "Mtot",
    linetype = "Noise"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")


####
