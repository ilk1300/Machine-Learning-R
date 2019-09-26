##Question3f

source("hw1item3e.R", local = TRUE)

cost_y_R_normal <- R_Hbf_normal_pdf$y *5  #take y values from responder conditional pdf and multiply by 5

plot(R_Hbf_normal_pdf$x, cost_y_R_normal, type = 'l', xlim=c(0,25), ylim=c(0.00, 0.60), col='red', xlab="%Hbf", ylab="Density", main = "Conditional Probability Estiamtion with Error Penalties")

par(new=TRUE)

plot(NR_Hbf_normal_pdf, xlim=c(0,25), ylim=c(0.00, 0.60), col='purple', xlab="%Hbf", main = "Conditional Probability Estiamtion with Error Penalties")

legend('topright', legend = c("Responders", "Non-responders"), fill = c("purple", "red"))