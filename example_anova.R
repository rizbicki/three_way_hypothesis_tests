library(tidyverse)
library(lubridate)
library(ggrepel)

hpd_prob <- 0.5

statistics <- data.frame(rep(NA,3))
statistics$mean=c(0.15,-0.13,-0.38)
statistics$var=c(1.09,0.5,0.79)^2
statistics$n=5
#statistics$n=10

prior_mean <- c(2,2,2)
prior_var <- rep(50,3)

posterior_mean <- 1/(1/prior_var+statistics$n/statistics$var)*(prior_mean/prior_var+
                                                         statistics$n*statistics$mean/statistics$var)
posterior_var <- 1/(1/prior_var+statistics$n/statistics$var)

# Monte Carlo Sampling
set.seed(0)
B <- 100000
sample_1 <- rnorm(B,posterior_mean[1],sqrt(posterior_var[1]))
sample_2 <- rnorm(B,posterior_mean[2],sqrt(posterior_var[2]))
sample_3 <- rnorm(B,posterior_mean[3],sqrt(posterior_var[3]))

p_1_greater_2 <- mean(sample_1>sample_2)
p_2_greater_3 <- mean(sample_2>sample_3)
p_1_greater_3 <- mean(sample_1>sample_3)
p_3_greater_2 <- mean(sample_3>sample_2)
p_1_greater_2_greater_3 <- mean(sample_1>sample_2&sample_2>sample_3)
p_1_greater_3_greater_2 <- mean(sample_1>sample_3&sample_3>sample_2)

p_1_greater_3
p_3_greater_2
p_1_greater_3_greater_2

p_1_greater_2
p_2_greater_3
p_1_greater_2_greater_3

library(rgl)
options(rgl.printRglwidget = TRUE)
Sigma <- diag(c(posterior_var))
Mean <- posterior_mean
x <- MASS::mvrnorm(1000, Mean, Sigma)

open3d()
plot3d( ellipse3d(Sigma, centre = Mean,level=hpd_prob), col = "lightblue", 
        alpha = 1,xlim=c(-1.5,1.5),ylim=c(-1.5,1.5),zlim=c(-1.5,1.5),
        xlab='μ1', ylab='μ2', zlab='μ3')

rgl.planes(1,-1, 0.00000000000001, 0, alpha=0.8, color = "red")
rgl.planes(0.00000000000001, 1, -1,0, alpha=0.8, color = "blue")
