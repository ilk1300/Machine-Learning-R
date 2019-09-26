##Question2d

source("hw1item2b.R", local = TRUE)

 cost_y_R <- ipdRatio_pdf$y *5  #take y values from responder conditional pdf and multiply by 5
 
 plot(ipdRatio_pdf$x, cost_y_R, type = 'l', ylim=c(0.00, 8), xlim=c(0,15), col='red', xlab="ipdRatio", ylab="Density", main = "Conditional Probability Estiamtion with Error Penalties")
 
 par(new=TRUE)
 
 plot(ipdRatio_pdf1, xlim=c(0,15), ylim=c(0.00, 8), col='purple', xlab="ipdRatio", main = "Conditional Probability Estiamtion with Error Penalties")
 
 legend('topright', legend = c("unmythelated", "mythelated"), fill = c("purple", "red"))

