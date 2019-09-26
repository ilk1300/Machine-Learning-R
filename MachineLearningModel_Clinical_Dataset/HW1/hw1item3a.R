##Question3a

#read in data and get initial Hbf column
hu_dat <- read.delim("hu.dat")
initial_HbF <- hu_dat$X.HbF

#use density function to plot pdf
initial_HbF_pdf <- density(initial_HbF, bw = 1.5, kernel = "rectangular")

#plot a density histgram which has the true distribution to help decide bandwidth
#but comment out after chosen bandwidth
#hist(initial_HbF, freq = FALSE) 

#plot pdf, specify x label and main title
plot(initial_HbF_pdf, xlim=c(0,25), xlab = "%Hbf", main="Parzen Density Estimation\n  bw=1.5, unifrom kernels")
