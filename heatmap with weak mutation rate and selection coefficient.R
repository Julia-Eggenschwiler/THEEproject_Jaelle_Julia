
init_wd <- 300
init_wb <- 100
init_sd <- 300
init_sb <- 100
m_rate_wm <- 0.01
m_rate_sm <- 0.05
decay_rate_wm <- 0.2
decay_rate_sm <- 0.4
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
  s_wm = numeric(),
  m_rate_wm = numeric(),
  replicate = integer(),
  rescued_wd = integer(),
  rescued_wb = integer()
)

# define total runs and rescue count
total_runs<-0
rescue_count<-0
# run the Simulation across all chosen parameters
# loop over mutation rates
for(mval_wm in m_values_wm){
  for(sval_wm in s_values_wm){
    for(rep in 1:no_replicates){
      
      # Run your simulation
      one_run <- simulate_pop(init_wd, init_wb, init_sd, init_sb,
                              decay_rate_wm, decay_rate_sm,
                              sval_wm, s_sm, mval_wm, m_rate_sm, max_gen)
      
      # Final population sizes for wm
      N_end_wd <- one_run[nrow(one_run), "wd"]
      N_end_wb <- one_run[nrow(one_run), "wb"]
      
      # Determine rescue outcomes
      rescued_wd <- as.integer(N_end_wd > 0)
      rescued_wb <- as.integer(N_end_wb > 0)
      
      # Append results
      rescue_results <- rbind(rescue_results,
                              data.frame(
                                s_wm = sval_wm,
                                m_rate_wm = mval_wm,
                                replicate = rep,
                                rescued_wd = rescued_wd,
                                rescued_wb = rescued_wb
                              ))
    }
  }
}

library(dplyr)

library(dplyr)

rescue_summary <- rescue_results %>%
  group_by(s_wm, m_rate_wm) %>%
  summarise(
    rescue_prob_wd = mean(rescued_wd),
    rescue_prob_wb = mean(rescued_wb),
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


ggplot(rescue_summary, aes(x=factor(s_wm), y=factor(m_rate_wm), fill=rescue_prob_wd)) +
  geom_tile() +
  scale_fill_gradientn(colors=c("blue","white","red"), name="Rescue Prob wd") +
  labs(title="Heatmap of wd Rescue Probability",
       x="s_wm", y="m_rate_wm") +
  theme_minimal()

ggplot(rescue_summary, aes(x=factor(s_wm), y=factor(m_rate_wm), fill=rescue_prob_wb)) +
  geom_tile() +
  scale_fill_gradientn(colors=c("blue","white","red"), name="Rescue Prob wb") +
  labs(title="Heatmap of wb Rescue Probability",
       x="s_wm", y="m_rate_wm") +
  theme_minimal()


