# code for plotting the result of different populations in one plot
init_wd <- 30
init_wb <- 30
init_sd <- 30
init_sb <- 30
m_rate_wm <- 0.003 # approximately 0.4
m_rate_sm <- 0.005 # approximately 0.6
decay_rate_wm <- 0.1
decay_rate_sm <- 0.2
s_wm <- 0.1
s_sm <- 0.2
max_gen <- 1000
replicates <- 30


# defining Data Frame to save one Row (Population Size for multiple Generations until max_gen) after one Loop
temp_output <- data.frame(array(NA, dim = c(0,max_gen)))
# defining Data Frame to save all Results from Loop
final_output <- data.frame(array(NA, dim = c(replicates,max_gen)))
# defining a Set of colors in order to color each Line differently
lines_colors <- rainbow(replicates)


# simulating a population multiple times
for (i in 1:replicates) {
  temp_output <- simulate_pop(
    init_wd, init_wb, init_sd, init_sb,
    decay_rate_wm, decay_rate_sm,
    s_wm, s_sm,
    m_rate_wm, m_rate_sm,
    max_gen = max_gen)
  # extract Pop Matrix
  pop_matrix <- temp_output$population
  
  # calculate Row Sums
  temp_output <- rowSums(pop_matrix[, c("wd", "wb", "sd", "sb")])
  # adding to Result Table
  final_output[i, 1:length(temp_output)]<-temp_output
}

# defining end_gen which is as long as the last Generation where the pop size is an actual Number and not NA yet
end_gen<-tail(which(!is.na(final_output[1,])), 1)
# Plot one Line / one Population
plot(x=1:max_gen,y=final_output[1,1:max_gen],type='l',ylim=c(0,max(1400, na.rm = TRUE)),xlab =  "Generation",ylab = "Population size", col = lines_colors[1])
# Loop for plotting each Row of Data Table / Population as a Line in the Plot
for (i in 1:replicates) {
  
  end_gen<-tail(which(!is.na(final_output[i,])), 1)
  
  if (end_gen < max_gen){
    line_col <- "grey"
  } else {
    line_col <- lines_colors[i]
  }
  lines(x=1:max_gen,y=final_output[i,1:max_gen], col=line_col)
  
  
}


# code for plotting the result of different populations in one plot
init_wd <- 30
init_wb <- 30
init_sd <- 30
init_sb <- 30
m_rate_wm <- 0.002 # approximately 0.2
m_rate_sm <- 0.003 # approximately 0.3
decay_rate_wm <- 0.1
decay_rate_sm <- 0.2
s_wm <- 0.1
s_sm <- 0.2
max_gen <- 1000
replicates <- 30


# defining Data Frame to save one Row (Population Size for multiple Generations until max_gen) after one Loop
temp_output <- data.frame(array(NA, dim = c(0,max_gen)))
# defining Data Frame to save all Results from Loop
final_output <- data.frame(array(NA, dim = c(replicates,max_gen)))
# defining a Set of colors in order to color each Line differently
lines_colors <- rainbow(replicates)

# simulating a population multiple times
for (i in 1:replicates) {
  temp_output <- simulate_pop(
    init_wd, init_wb, init_sd, init_sb,
    decay_rate_wm, decay_rate_sm,
    s_wm, s_sm,
    m_rate_wm, m_rate_sm,
    max_gen = max_gen)
  # extract Pop Matrix
  pop_matrix <- temp_output$population
  
  # calculate Row Sums
  temp_output <- rowSums(pop_matrix[, c("wd", "wb", "sd", "sb")])
  # adding to Result Table
  final_output[i, 1:length(temp_output)]<-temp_output
}

# defining end_gen which is as long as the last Generation where the pop size is an actual Number and not NA yet
end_gen<-tail(which(!is.na(final_output[1,])), 1)
# Plot one Line / one Population
plot(x=1:max_gen,y=final_output[1,1:max_gen],type='l',ylim=c(0,max(final_output, na.rm = TRUE)),xlab =  "Generation",ylab = "Population size", col = lines_colors[1])
# Loop for plotting each Row of Data Table / Population as a Line in the Plot
for (i in 2:replicates) {
  
  end_gen<-tail(which(!is.na(final_output[i,])), 1)
  lines(x=1:max_gen,y=final_output[i,1:max_gen], col=lines_colors[i])
  
  
}

#init_wd <- 50 => stochastic but sometimes one population survives
#init_wb <- 50
#init_sd <- 50
#init_sb <- 50
#m_rate_wm <- 0.005
#m_rate_sm <- 0.007
#decay_rate_wm <- 0.1
#decay_rate_sm <- 0.2
#s_wm <- 0.1
#s_sm <- 0.2
#max_gen <- 1000
#replicates <- 30

#init_wd <- 100 => mostly survival
#init_wb <- 100
#init_sd <- 100
#init_sb <- 100
#m_rate_wm <- 0.002
#m_rate_sm <- 0.003
#decay_rate_wm <- 0.1
#decay_rate_sm <- 0.2
##s_wm <- 0.1
#s_sm <- 0.2
#max_gen <- 1000
#replicates <- 30


output <- simulate_pop(init_wd=30, init_wb=30,  init_sd=30, init_sb=30, decay_rate_wm=0.1, decay_rate_sm=0.2, s_wm=0.1, s_sm=0.2, m_rate_wm=0.003, m_rate_sm=0.005,max_gen)

# show the last few Lines of the Data Table

print(tail(output))

# plot the Output 

# population-Matrix extrahieren
pop_matrix <- output$population

# Prozentuale Häufigkeiten der Mutationswechsel
switch_stats <- output$switch_stats
print(switch_stats)

# Plot der Populationsgrößen über die Generationen
x_range <- 0:(nrow(pop_matrix)-1)

# Gesamtpopulation
plot(x_range, pop_matrix[,1] + pop_matrix[,2] + pop_matrix[,3] + pop_matrix[,4],
     type='l', ylim=c(0,max(pop_matrix)), xlab="Generation", ylab="Population size",
     col="black", lwd=1.5)

# Einzelne Mutanten hinzufügen
lines(x_range, pop_matrix[,1], col="#77b5fe", lwd=1.5) # wd
lines(x_range, pop_matrix[,2], col="navyblue", lwd=1.5) # wb
lines(x_range, pop_matrix[,3], col="#D55E00", lwd=1.5) # sd
lines(x_range, pop_matrix[,4], col="darkred", lwd=1.5) # sb

# Legende
legend("topleft",
       legend=c("Total","wd","wb","sd","sb"),
       col=c("black","#77b5fe","navyblue","#D55E00","darkred"),
       lty=1, lwd=1.5)

# Beispiel: Ausgabe der Prozentzahlen für Mutationswechsel
cat("\nProzentuale Häufigkeiten der Wechsel:\n")
print(switch_stats)


output <- simulate_pop(init_wd=50, init_wb=50,  init_sd=50, init_sb=50, decay_rate_wm=0.1, decay_rate_sm=0.2, s_wm=0.1, s_sm=0.2, m_rate_wm=0.002, m_rate_sm=0.003,max_gen)

# show the last few Lines of the Data Table

print(tail(output))

# extractin Population-Matrix 
pop_matrix <- output$population

# assign frequencies of mutation changes
switch_stats <- output$switch_stats
switch_stats_all

# determine x Axis Range 
x_range <- 0:(nrow(pop_matrix)-1)

# plot the Output
# this plots the total Population Size-black line visible in the plot
plot(x_range, pop_matrix[,1] + pop_matrix[,2] + pop_matrix[,3] + pop_matrix[,4],
     type='l', xlim = c(0,max(1000, na.rm = TRUE)), ylim=c(0,max(500, na.rm = TRUE)), xlab="Generation", ylab="Population size",
     col="black", lwd=1.5)

# adding each mutant
lines(x_range, pop_matrix[,1], col="#77b5fe", lwd=1.5) # wd
lines(x_range, pop_matrix[,2], col="navyblue", lwd=1.5) # wb
lines(x_range, pop_matrix[,3], col="#D55E00", lwd=1.5) # sd
lines(x_range, pop_matrix[,4], col="darkred", lwd=1.5) # sb

# Legend
legend("topleft",
       legend=c("Total","wd","wb","sd","sb"),
       col=c("black","#77b5fe","navyblue","#D55E00","darkred"),
       lty=1, lwd=1.5)


