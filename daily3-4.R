# 2
Xdata2 <- data.frame(X=c(-3,3))
dist2 <- ggplot(Xdata2, aes(x=X))
dist2 + stat_function(fun=dnorm)

temp <- as.data.frame(rnorm(1000))
ggplot(temp, aes(x=`rnorm(1000)`))+geom_density()
# FALSE?

#3
Xdata2 <- data.frame(X=c(-3,3))
dist2 <- ggplot(Xdata2, aes(x=X))
dist2 + stat_function(fun=dnorm)

temp <- as.data.frame(rnorm(1000))
ggplot(temp, aes(x=`rnorm(1000)`))+geom_line(stat='density')
# FALSE

#4
faithful <- faithful
faithful <- faithful[which(faithful$waiting>65),]
ggplot(faithful, aes(sample = waiting)) + stat_qq() + stat_qq_line()
# TRUE

#5
# false? there is only one 45 degree reference line on a qq plot

# 7
# false? can change the specific labels of a legend (male/female) but
# can only change legend title with labs

#8
Davis <- Davis
DavisScatter2 <- ggplot(Davis, aes(x=weight, y=repwt, color=sex)) + 
  geom_point()
DavisScatter2+ scale_color_discrete(limits=c("M","F"), 
                                    labels=c("Female","Male")) 
# false; male female should be flipped

#9
DavisScatter2+ scale_color_discrete(limits=c("M","F"), 
                                    labels=c("Male","Female"))+
  scale_color_manual(values=c("green", "pink"))


?scale_color_manual

#11
new <- ggplot(Davis, aes(x=weight, y=repwt))+geom_point()+theme_bw()

new+theme(panel.border=element_blank())
