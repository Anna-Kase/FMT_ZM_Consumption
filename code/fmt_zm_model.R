

#Call up necessary packages
library(tidyverse)
library(brms)


#Read in data file
data <- read.csv("C:/Users/annak/OneDrive/Documents/ZM_Project/ZM_Vanessa_Poster_Stats/ZM_FMT_Data.csv", header=T)


# Total_ZM ~ Site

# y ~ dnorm(mu, sigma)
# mu  = a + bx
# a ~ ?
# b ~ ?
# sigma ~ ?

N = 100  # number of simulations (change as needed)

# simulate priors
priors <- tibble(a = rnorm(N, 2, 1),
                 b = rnorm(N, 0, 1),
                 sigma = rexp(N, 1),
                 sim = 1:N)

# data (only the x values, since we're simulating y and mu and pretending we don't have them yet)
x <- data$Total_Number_ZM_Consmed_After_24_Hours - mean(data$Total_Number_ZM_Consmed_After_24_Hours)

# combine and simulate
prior_and_x <- priors %>% expand_grid(x = x) %>%    # combine priors and x's
  mutate(mu = a + b*x,                              # simulate regressions
         y = rnorm(nrow(.), mu, sigma))             # simulate data (e.g., y_rep)

# plot the priors
prior_and_x %>% 
  ggplot(aes(x = x, y = mu, group = sim)) + 
  geom_line() +
  geom_point(aes(y = y)) +
  labs(y = "sim")



zm_brm <- brm(Total_Number_ZM_Consmed_After_24_Hours ~ Site,
              family = poisson(link="log"),
              data = data %>% mutate(total_c = (Total_Number_ZM_Consmed_After_24_Hours - mean(Total_Number_ZM_Consmed_After_24_Hours))/sd(Total_Number_ZM_Consmed_After_24_Hours)),
              prior = c(prior(normal(2,1), class="Intercept"),
                        prior(normal(0,1), class="b")),
              cores = 4, chains = 1, iter=1000)


zm_brm

conditional_effects(zm_brm)

pp_check(zm_brm, type="stat_grouped", group = 'Site')
pp_check(zm_brm, type="freqpoly_grouped", group='Site')

simulated_mr <- rpois(25, (exp(1.79)))
simulated_fc <- rpois(25, (exp(1.79-0.09)))

simulated_data <- as.data.frame(cbind(simulated_mr, simulated_fc))
simulated_data <- gather(simulated_data, "site", "zm")
simulated_data$zm <- as.factor(simulated_data$zm)

plot(simulated_data$site, col=simulated_data$zm, pch=16)
plot(data$Total_Number_ZM_Consmed_After_24_Hours, col=data$Site, pch=16)



