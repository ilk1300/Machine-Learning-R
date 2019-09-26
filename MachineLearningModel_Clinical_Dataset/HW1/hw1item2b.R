##Question2b
source("hw1item2a.R", local = TRUE)

#plot U and N with uniform kernels when bw=0.01
U_u_w1 <- density(U, bw = 0.01, kernel = "rectangular")
N_u_w1 <- density(N, bw = 0.01, kernel = "rectangular")
plot(U_u_w1, xlim=c(0.0,1.0), ylim=c(0.0,8.0), col='red', xlab = "Randomly Generated Variables", main="Parzen Density Estimation\n  bw=0.01, uniform kernels")
par(new=TRUE)
plot(N_u_w1, xlim=c(0.0,1.0), ylim=c(0.0,8.0), col='green', xlab = "Randomly Generated Variables", main="Parzen Density Estimation\n  bw=0.01, uniform kernels")
legend('topright', legend = c('uniformly distributed', 'normally distributed'), fill = c('red', 'green'), cex = 0.75)

#plot U and N with uniform kernels when bw=0.05
U_u_w2 <- density(U, bw = 0.05, kernel = "rectangular")
N_u_w2 <- density(N, bw = 0.05, kernel = "rectangular")
plot(U_u_w2, xlim=c(0.0,1.0), ylim=c(0.0,6.0), col='red', xlab = "Randomly Generated Variables", main="Parzen Density Estimation\n  bw=0.05, uniform kernels")
par(new=TRUE)
plot(N_u_w2, xlim=c(0.0,1.0), ylim=c(0.0,6.0), col='green', xlab = "Randomly Generated Variables", main="Parzen Density Estimation\n  bw=0.05, uniform kernels")
legend('topright', legend = c('uniformly distributed', 'normally distributed'), fill = c('red', 'green'), cex = 0.75)

#plot U and N with uniform kernels when bw=0.1
U_u_w3 <- density(U, bw = 0.1, kernel = "rectangular")
N_u_w3 <- density(N, bw = 0.1, kernel = "rectangular")
plot(U_u_w3, xlim=c(0.0,1.0), ylim=c(0.0,3.0), col='red', xlab = "Randomly Generated Variables", main="Parzen Density Estimation\n  bw=0.1, uniform kernels")
par(new=TRUE)
plot(N_u_w3, xlim=c(0.0,1.0), ylim=c(0.0,3.0), col='green', xlab = "Randomly Generated Variables", main="Parzen Density Estimation\n  bw=0.1, uniform kernels")
legend('topright', legend = c('uniformly distributed', 'normally distributed'), fill = c('red', 'green'), cex = 0.75)