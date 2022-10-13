library(distributions3)
library(ggplot2)
library(gridExtra)

b1 = 0.5
sigma = 0.2

xDist <- Uniform(a=0.8, b=0.9)
x_list <- random(xDist, 1000)
x_list
yProj <- function(x, y){
  x*y
}
simData <- function(n, rng, sigma, b1, randX){
  errorDist <- Normal(mu = 0, sigma = sigma)
  error_list <- random(errorDist, n)
  if(randX == TRUE){
    x_list <- random(xDist, n)
  } else {
    x_list = rng    
  }

  real_y <- function(y, i, b1){
    y <- append(y, x_list[i]*b1 + error_list[i])
  }
  y <- c()
  for(i in rng){
    y <- append(y, x_list[i]*b1 + error_list[i])
  }
  y
  
  simulation <- data.frame(y=y, x=x_list)
  return(c(simulation, x_list))
}
getSample <- function(simulation, ns){
  df <- data.frame(y=simulation[1], x=simulation[2])
  sampleX <- sample(df$x, size = ns, replace = FALSE)
  sampleXY <- df[df$x %in% sampleX, ]
  #return(sampleX)
  return(sampleXY)
}
graphPlots <- function(n, rng, sd, slope, ns){
simulacaoTotal <- simData(n, rng, sd, slope, TRUE)

sample1 <- getSample(simulacaoTotal, ns)
sample2 <- getSample(simulacaoTotal, ns)
sample3 <- getSample(simulacaoTotal, ns)
sample4 <- getSample(simulacaoTotal, ns)

m1 <- lm(sample1$y ~ 0+sample1$x)
m2 <- lm(sample2$y ~ 0+sample2$x)
m3 <- lm(sample3$y ~ 0+sample3$x)
m4 <- lm(sample4$y ~ 0+sample4$x)

#p <- ggplot(data=simulacaoTotal, aes(x=simulacaoTotal[2], y=simulacaoTotal[1])) + geom_point() + geom_smooth(method="lm")
p11 <- ggplot(data=sample1, aes(x=x, y=y)) + geom_point() + geom_abline(slope=m1$coefficients[1]) + 
                                                           geom_abline(slope=m1$coefficients[1], intercept =  2*sd(sample1$y), color = "red") +
                                                           geom_abline(slope=m1$coefficients[1], intercept =  2*sd, color = "blue") + 
                                                           geom_abline(slope=m1$coefficients[1], intercept =  -2*sd(sample1$y), color = "red") +
                                                           geom_abline(slope=m1$coefficients[1], intercept =  -2*sd, color = "blue")
#p2 <- ggplot(data=sample2, aes(x=x, y=y)) + geom_point() + geom_abline(slope=m2$coefficients[1]) + 
#  geom_abline(slope=m2$coefficients[1], intercept =  2*sd(sample2$y), color = "red") +
#  geom_abline(slope=m2$coefficients[1], intercept =  2*sd, color = "blue") + 
#  geom_abline(slope=m2$coefficients[1], intercept =  -2*sd(sample2$y), color = "red") +
#  geom_abline(slope=m2$coefficients[1], intercept =  -2*sd, color = "blue")
#p3 <- ggplot(data=sample3, aes(x=x, y=y)) + geom_point() + geom_abline(slope=m3$coefficients[1]) + 
#  geom_abline(slope=m3$coefficients[1], intercept =  2*sd(sample3$y), color = "red") +
#  geom_abline(slope=m3$coefficients[1], intercept =  2*sd, color = "blue") + 
#  geom_abline(slope=m3$coefficients[1], intercept =  -2*sd(sample3$y), color = "red") +
#  geom_abline(slope=m3$coefficients[1], intercept =  -2*sd, color = "blue")
#p4 <- ggplot(data=sample4, aes(x=x, y=y)) + geom_point() + geom_abline(slope=m4$coefficients[1]) + 
#  geom_abline(slope=m4$coefficients[1], intercept =  2*sd(sample4$y), color = "red") +
#  geom_abline(slope=m4$coefficients[1], intercept =  2*sd, color = "blue") + 
#  geom_abline(slope=m4$coefficients[1], intercept =  -2*sd(sample4$y), color = "red") +
#  geom_abline(slope=m4$coefficients[1], intercept =  -2*sd, color = "blue")
y_proj_list <- sapply(sample1$x, yProj, y=summary(m1)$coefficients[, "Estimate"])
y_proj_list
p12 <- ggplot(data=sample1) + geom_line(aes(x=1:ns, y=y), color="blue") + geom_line(aes(y=y_proj_list, x=1:ns), color="red") + 
                              geom_ribbon(aes(ymin=y_proj_list + sd(y_proj_list), ymax=y_proj_list - sd(y_proj_list), x=1:ns), alpha=0.3)+ 
                              geom_ribbon(aes(ymin=y_proj_list + sd, ymax=y_proj_list - sd, x=1:ns), alpha=0.5, fill="red")

#grid.arrange(p11, p12, ncol=2)
p12
}
graphPlots(1000, 1:1000, 0.005, 0.87, 44)
