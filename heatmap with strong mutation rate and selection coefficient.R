
init_wd <- 300
init_wb <- 100
init_sd <- 300
init_sb <- 100
m_rate_wm <- 0.01
m_rate_sm <- 0.05
decay_rate_wm <- 0.1
decay_rate_sm <- 0.25
s_wm <- 0.2
s_sm <- 0.4
max_gen <- 1000
# determine how often to run the Simulation for each set of Parameters
no_replicates <- 100


# set Parameters to vary
s_values_wm <- c(0.10,0.15,0.2)
s_values_sm <- c(0.2,0.205,0.210)
m_values_wm <- c(0.001,0.003,0.005)
m_values_sm <- c(0.006,0.008,0.010)


# initialize Data Table - where to collect the Results
data_table <- c()
data_table_wd<- c()
data_table_wb<- c()
data_table_sd<- c()
data_table_sb<- c()
# empty list to save rescue results
rescue_results <- data.frame(
  s_sm = numeric(),
  m_rate_sm = numeric(),
  replicate = integer(),
  rescued_sd = integer(),
  rescued_sb = integer()
)

# define total runs and rescue count
total_runs<-0
rescue_count<-0
# run the Simulation across all chosen parameters
# loop over mutation rates
for(mval_sm in m_values_sm){
  for(sval_sm in s_values_sm){
    for(rep in 1:no_replicates){
      
      # Run your simulation
      one_run <- simulate_pop(init_wd, init_wb, init_sd, init_sb,
                              decay_rate_wm, decay_rate_sm,
                              sval_sm, s_wm, mval_sm, m_rate_wm, max_gen)
      
      # Final population sizes for wm
      N_end_sd <- one_run[nrow(one_run), "sd"]
      N_end_sb <- one_run[nrow(one_run), "sb"]
      
      # Determine rescue outcomes
      rescued_sd <- as.integer(N_end_sd > 0)
      rescued_sb <- as.integer(N_end_sb > 0)
      
      # Append results
      rescue_results <- rbind(rescue_results,
                              data.frame(
                                s_sm = sval_sm,
                                m_rate_sm = mval_sm,
                                replicate = rep,
                                rescued_sd = rescued_sd,
                                rescued_sb = rescued_sb
                              ))
    }
  }
}

library(dplyr)

library(dplyr)

rescue_summary <- rescue_results %>%
  group_by(s_sm, m_rate_sm) %>%
  summarise(
    rescue_prob_sd = mean(rescued_sd),
    rescue_prob_sb = mean(rescued_sb),
    .groups = "drop"
  )
library(tidyr)
library(tidyr)

rescue_long <- rescue_summary %>%
  pivot_longer(cols = starts_with("rescue_prob_"),
               names_to = "mutant",
               values_to = "rescue_prob") %>%
  mutate(
    mutant = gsub("rescue_prob_", "", mutant)
  )


ggplot(rescue_summary, aes(x=factor(s_sm), y=factor(m_rate_sm), fill=rescue_prob_sd)) +
  geom_tile() +
  scale_fill_gradientn(colors=c("blue","white","red"), name="Rescue Prob sd") +
  labs(title="Heatmap rescue probability of strong deleterious mutation",
       x="selection coefficients strong mutation",
       y="mutation rates strong mutation") +
  theme_minimal()


ggplot(rescue_summary, aes(x=factor(s_sm), y=factor(m_rate_sm), fill=rescue_prob_sb)) +
  geom_tile() +
  scale_fill_gradientn(colors=c("blue","white","red"), name="Rescue Prob sb") +
  labs(title="Heatmap rescue probability of strong beneficial mutation",
       x="selection coefficients strong mutation",
       y="mutation rates strong mutation") +
  theme_minimal()
