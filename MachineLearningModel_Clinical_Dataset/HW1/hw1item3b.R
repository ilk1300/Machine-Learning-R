##Question3b

#get Hbf values
HbF_table <- read.delim("hu.dat")
start_Hbf <- HbF_table$X.HbF

#get FHbf values
hemo_table <- read.csv("hemo.csv", header = TRUE)
final_Hbf <- hemo_table$FHbF

#plot the pdf for Hbf values given it's a responder
R_Hbf <- start_Hbf[which(final_Hbf >= 15)]
#hist(R_Hbf, freq = FALSE) #use density histogram to help decide bandwidth, comment out after chosen bw
R_Hbf_pdf <- density(R_Hbf, bw=2.0, kernel = "rectangular", from = 0, to = 25)
plot(R_Hbf_pdf, xlim=c(0,25), ylim=c(0.00, 0.14), col='green', xlab = "%Hbf", main = "Conditional Probability Estiamtion\n bw=2.0, uniform kernels")

par(new=TRUE)

#plot the pdf for Hbf values given it's a non-responder
NR_Hbf <- start_Hbf[which(final_Hbf < 15)]
#hist(NR_Hbf, freq = FALSE) #use density histogram to help decide bandwidth, comment out after chosen bw
NR_Hbf_pdf <- density(NR_Hbf, bw=2.0, kernel = "rectangular", from = 0, to = 25)
plot(NR_Hbf_pdf, xlim=c(0,25), ylim=c(0.00, 0.14), col='orange', xlab = "%Hbf", main = "Conditional Probability Estiamtion\n bw=2.0, uniform kernels")

legend('topright', legend = c("Responders", "Non-responders"), fill = c("green", "orange"), cex = 0.75)
