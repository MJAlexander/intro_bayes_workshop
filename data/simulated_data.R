# simulate some Gompertz data

mean_log_alpha <- -7
sd_log_alpha <- 0.5
mean_beta <- 0.1
sd_beta <- 0.05

n_areas <- 5
pop_size <- 5000

set.seed(109)
log_alphas <- rnorm(n_areas, mean = mean_log_alpha, sd = sd_log_alpha)
betas <- rnorm(n_areas, mean = mean_beta, sd = sd_beta)

set.seed(098)
df <- tibble(age = rep(40:59, times = n_areas), 
             area = rep(1:n_areas, each = length(40:59)),
             log_alpha = log_alphas[area],
             beta = betas[area],
             log_mx_true = log_alpha + beta*(age-40)) |> 
  rowwise() |> 
  mutate(deaths_sim = rpois(1, exp(log_mx_true)*pop_size)) |> 
  mutate(mx_sim = deaths_sim/pop_size) |> 
  mutate(qx_sim = 1-exp(-1*mx_sim)) |> 
  mutate(px_sim = 1-qx_sim) |> 
  group_by(area) |> 
  mutate(cprod = 1-cumprod(px_sim))

q40 <- df$cprod[nrow(df)]
write_rds(q40, "data/q40.rds")
# remove last area and save the rest as data

df |> 
  filter(area!=n_areas) |> 
  select(-(qx_sim:cprod), -(log_alpha:log_mx_true), -mx_sim) |> 
  mutate(pop = pop_size) |> 
  rename(deaths = deaths_sim) |> 
  write_rds("data/sim.rds")

true_params <- tibble(are = 1:5, log_alpha = log_alphas,
                      beta = betas) |> 
  write_rds("data/true_params.rds")
