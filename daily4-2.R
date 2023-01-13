library(ggplot2)
set.seed(05202001)


arr <- data.frame()
for(i in 1:100){
  a <- rchisq(7,4)
  samp_mean <- mean(a)
  B<-10000
  boot_samp <- replicate(B, sample(a, replace=T))
  boot_means <- apply(boot_samp,2,mean)
  mean(boot_means)
  boot_err <- boot_means - samp_mean
  boot_err_sort <- sort(boot_err)
  p2.5 <- B*0.025
  p97.5 <- B*0.975
  boot_ci <- samp_mean - boot_err_sort[c(p97.5,p2.5)]
  arr[i, "lower"]<-boot_ci[1]
  arr[i, "upper"]<- boot_ci[2]
  arr[i, "samp.mean"] <- samp_mean
}

arr[,'lower']<arr[,"samp.mean"]

