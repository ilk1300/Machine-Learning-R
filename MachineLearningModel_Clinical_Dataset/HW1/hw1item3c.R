##Question3c

source("hw1item3b.R", local = TRUE)

#get intersction point of these two pdfs
intersection <- NR_Hbf_pdf$x[which(diff((NR_Hbf_pdf$y - R_Hbf_pdf$y) >0) !=0) +1]
#since %Hbf is within range (0,20.7), only the first value make sense
threshold <- intersection[1] 
#draw threshold line
abline(v=threshold, col='red')

legend('topright', legend = c("Responders", "Non-responders", "Threshold"), fill = c("green", "orange", "red"), cex = 0.75)
