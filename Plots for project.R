# code for plotting the result of different populations in one plot
init_wd <- 500
init_wb <- 50
init_sd <- 500
init_sb <- 50
m_rate_wm <- 0.0007
m_rate_sm <- 0.001
decay_rate_wm <- 0.1
decay_rate_sm <- 0.2
s_wm <- 0.1
s_sm <- 0.2
max_gen <- 1000
replicates <- 50



temp_output <- data.frame(array(NA, dim = c(0,max_gen)))

final_output <- data.frame(array(NA, dim = c(replicates,max_gen)))
lines_colors <- rainbow(replicates)
# simulating a population multiple times
for (i in 1:replicates) {
  
  
  
  temp_output <- rowSums(simulate_pop(
    init_wd, init_wb, init_sd, init_sb,
    decay_rate_wm, decay_rate_sm,
    s_wm, s_sm,
    m_rate_wm, m_rate_sm,
    max_gen = max_gen)[, c("wd", "wb", "sd", "sb")])
  
  final_output[i, 1:length(temp_output)]<-temp_output
}


end_gen<-tail(which(!is.na(final_output[1,])), 1)
plot(x=1:end_gen,y=final_output[1,1:end_gen],type='l',ylim=c(0,max(final_output, na.rm = TRUE)),xlab =  "Generation",ylab = "Population size", col = lines_colors[1])

for (i in 2:replicates) {
  
  end_gen<-tail(which(!is.na(final_output[i,])), 1)
  lines(x=1:end_gen,y=final_output[i,1:end_gen], col=lines_colors[i])
  
  
}
final_output1 <- rbind(final_output, c(1:1000))
final_output2 <- data.frame(t(final_output1))
dev.off()
dev.off()
dev.off()
plot(final_output2$X1~final_output2$X11, type = "l") 
for (i in 2:replicates) {
  lines(final_output2[,i]~final_output2$X11, col="red",na.rm=TRUE)
  
}

x_range <- 0:(length(output[,1])-1)

# this plots the total Population Size-black line visible in the plot

plot(x_range,output[,1]+output[,2]+output[ ,3]+output[ ,4],type='l',ylim=c(0,max(output[,1]+output[,2]+output[ ,3]+output[ ,4])),xlab =  "Generation",ylab = "Population size")

# add Number of wd Individuals
lines(x_range,output[,1], col="#77b5fe")

# add Number of wb Individuals
lines(x_range,output[,2], col="navyblue")

# add Number of sd Individuals
lines(x_range,output[,3], col="#D55E00")

# add Number of sb Individuals
lines(x_range,output[,4], col="darkred")

# add Legend in the top-left corner
legend("topleft",
       legend=c("Weak deleterious","Weak beneficial","Strong deleterious","Strong beneficial"),
       col=c("#77b5fe","navyblue","#D55E00","darkred"), lty=1, lwd=1.5)
