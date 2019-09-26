##Question2c
source("hw1item2a.R", local = TRUE)

#plot U and N with normal kernels when bw=0.01
U_n_w1 <- density(U, bw = 0.01, kernel = "gaussian")
N_n_w1 <- density(N, bw = 0.01, kernel = "gaussian")
plot(U_n_w1, xlim=c(0.0,1.0), ylim=c(0.0,8.0), col='red', xlab = "Randomly Generated Variables", main="Parzen Density Estimation\n  bw=0.01, normal kernels")
par(new=TRUE)
plot(N_n_w1, xlim=c(0.0,1.0), ylim=c(0.0,8.0), col='green', xlab = "Randomly Generated Variables", main="Parzen Density Estimation\n  bw=0.01, normal kernels")
legend('topright', legend = c('uniformly distributed', 'normally distributed'), fill = c('red', 'green'), cex = 0.75)

#plot U and N with normal kernels when bw=0.05
U_n_w2 <- density(U, bw = 0.05, kernel = "gaussian")
N_n_w2 <- density(N, bw = 0.05, kernel = "gaussian")
plot(U_n_w2, xlim=c(0.0,1.0), ylim=c(0.0,6.0), col='red', xlab = "Randomly Generated Variables", main="Parzen Density Estimation\n  bw=0.05, normal kernels")
par(new=TRUE)
plot(N_n_w2, xlim=c(0.0,1.0), ylim=c(0.0,6.0), col='green', xlab = "Randomly Generated Variables", main="Parzen Density Estimation\n  bw=0.05, normal kernels")
legend('topright', legend = c('uniformly distributed', 'normally distributed'), fill = c('red', 'green'), cex = 0.75)

#plot U and N with normal kernels when bw=0.1
U_n_w3 <- density(U, bw = 0.1, kernel = "gaussian")
N_n_w3 <- density(N, bw = 0.1, kernel = "gaussian")
plot(U_n_w3, xlim=c(0.0,1.0), ylim=c(0.0,3.5), col='red', xlab = "Randomly Generated Variables", main="Parzen Density Estimation\n  bw=0.1, normal kernels")
par(new=TRUE)
plot(N_n_w3, xlim=c(0.0,1.0), ylim=c(0.0,3.5), col='green', xlab = "Randomly Generated Variables", main="Parzen Density Estimation\n  bw=0.1, normal kernels")
legend('topright', legend = c('uniformly distributed', 'normally distributed'), fill = c('red', 'green'), cex = 0.75)