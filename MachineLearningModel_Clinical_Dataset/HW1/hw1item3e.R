##Question3e

#get Hbf values
HbF_table <- read.delim("hu.dat")
start_Hbf <- HbF_table$X.HbF

#get FHbf values
hemo_table <- read.csv("hemo.csv", header = TRUE)
final_Hbf <- hemo_table$FHbF

#plot the pdf for Hbf values given it's a responder
R_Hbf <- start_Hbf[which(final_Hbf >= 15)]
#hist(R_Hbf, freq = FALSE)  #plot probability density histgram as a reference to choose bw
R_Hbf_normal_pdf <- density(R_Hbf, bw=1.5, kernel = "gaussian", from = 0, to=25)
plot(R_Hbf_normal_pdf, ylim=c(0.00, 0.18), col='seagreen', xlab = "%Hbf", main = "Conditional Probability Estiamtion\n bw=1.5, normal kernels")

par(new=TRUE)

#plot the pdf for Hbf values given it's a non-responder
NR_Hbf <- start_Hbf[which(final_Hbf < 15)]
#hist(NR_Hbf, freq = FALSE)  #plot probability density histgram as a reference to choose bw
NR_Hbf_normal_pdf <- density(NR_Hbf, bw=1.5, kernel = "gaussian", from = 0, to=25)
plot(NR_Hbf_normal_pdf, ylim=c(0.00, 0.18), col='maroon', xlab = "%Hbf", main = "Conditional Probability Estiamtion\n bw=1.5, normal kernels")

legend("topright", legend = c("Responders", "Non-responders"), fill = c("seagreen", "maroon"), cex=0.75)
