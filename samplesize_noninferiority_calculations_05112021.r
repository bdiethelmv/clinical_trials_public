## Date: 05 Nov 2021
## Author: BDV
## Non-inferiority trial sample size calculator

# Manual calculation using HR
# Source: http://powerandsamplesize.com/Calculators/Test-Time-To-Event-Data/Cox-PH-1-Sided-non-inferiority-superiority
hr=0.5
hr0=1
pE=0.5
pA=0.5
alpha=0.025
beta=0.10

s <- (1/(pA*(1-pA)*pE))*(((qnorm(1-alpha)+qnorm(1-beta))/(log(hr)-log(hr0)))^2)
s/2

(n=((qnorm(1-alpha)+qnorm(1-beta))/(log(hr)-log(hr0)))^2/(pA*(1-pA)*pE))

 # Calculations for various VEs

ves <- c(0.1,0.2,0.25,0.3,0.4,0.5,0.6,0.7,0.8,0.90,0.95)
rrs <- 1-ves
rrs

p_0.05 <- vector()
pE=0.05
for (item in rrs){
  sampsize <- (1/(pA*(1-pA)*pE))*(((qnorm(1-alpha)+qnorm(1-beta))/(log(item)-log(hr0)))^2)/2
  p_0.05 <- c(p_0.05,sampsize)
}

p_0.1 <- vector()
pE=0.1
for (item in rrs){
  sampsize <- (1/(pA*(1-pA)*pE))*(((qnorm(1-alpha)+qnorm(1-beta))/(log(item)-log(hr0)))^2)/2
  p_0.1 <- c(p_0.1,sampsize)
}

p_0.2 <- vector()
pE=0.2
for (item in rrs){
  sampsize <- (1/(pA*(1-pA)*pE))*(((qnorm(1-alpha)+qnorm(1-beta))/(log(item)-log(hr0)))^2)/2
  p_0.2 <- c(p_0.2,sampsize)
}


p_0.3 <- vector()
pE=0.3
for (item in rrs){
  sampsize <- (1/(pA*(1-pA)*pE))*(((qnorm(1-alpha)+qnorm(1-beta))/(log(item)-log(hr0)))^2)/2
  p_0.3 <- c(p_0.3,sampsize)
}

p_0.4 <- vector()
pE=0.4
for (item in rrs){
  sampsize <- (1/(pA*(1-pA)*pE))*(((qnorm(1-alpha)+qnorm(1-beta))/(log(item)-log(hr0)))^2)/2
  p_0.4 <- c(p_0.4,sampsize)
}

p_0.5 <- vector()
pE=0.5
for (item in rrs){
  sampsize <- (1/(pA*(1-pA)*pE))*(((qnorm(1-alpha)+qnorm(1-beta))/(log(item)-log(hr0)))^2)/2
  p_0.5 <- c(p_0.5,sampsize)
}

p_0.6 <- vector()
pE=0.6
for (item in rrs){
  sampsize <- (1/(pA*(1-pA)*pE))*(((qnorm(1-alpha)+qnorm(1-beta))/(log(item)-log(hr0)))^2)/2
  p_0.6 <- c(p_0.6,sampsize)
}


p_0.7 <- vector()
pE=0.7
for (item in rrs){
  sampsize <- (1/(pA*(1-pA)*pE))*(((qnorm(1-alpha)+qnorm(1-beta))/(log(item)-log(hr0)))^2)/2
  p_0.7 <- c(p_0.7,sampsize)
}


p_0.8 <- vector()
pE=0.8
for (item in rrs){
  sampsize <- (1/(pA*(1-pA)*pE))*(((qnorm(1-alpha)+qnorm(1-beta))/(log(item)-log(hr0)))^2)/2
  p_0.8 <- c(p_0.8,sampsize)
}

p_0.9 <- vector()
pE=0.9
for (item in rrs){
  sampsize <- (1/(pA*(1-pA)*pE))*(((qnorm(1-alpha)+qnorm(1-beta))/(log(item)-log(hr0)))^2)/2
  p_0.9 <- c(p_0.9,sampsize)
}


p_1 <- vector()
pE=1
for (item in rrs){
  sampsize <- (1/(pA*(1-pA)*pE))*(((qnorm(1-alpha)+qnorm(1-beta))/(log(item)-log(hr0)))^2)/2
  p_1 <- c(p_1,sampsize)
}

sampsize <- data.frame(ves, p_0.05, p_0.1, p_0.2, p_0.3, p_0.4, p_0.5, p_0.6,
                        p_0.7, p_0.8, p_0.9, p_1)

# Visualization

n_probs_ve0.5 <- as.numeric(as.vector(sampsize[6,]))
indices <- c(1)
n_probs_ve0.5 <- n_probs_ve0.5[-indices]
n_probs_ve0.5
probs <- c(0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)

efficacy <- ggplot() + geom_point(alpha=0.75, data=sampsize, aes(x=ves, y=p_0.5), 
                      size=I(4), col='salmon')+
  labs(x='Eficacia de la vacuna en estudio (1-HR)', 
       y='Tamaño de muestra (participantes por grupo)',
       title='Tamaño de muestra según eficacia de la vacuna, asumiendo 50% de probabilidad de infección') +
  theme_linedraw()+
  theme(axis.title.x=element_text(size=13),
        axis.title.y=element_text(size=13),
        axis.text.y=element_text(size=12),
        axis.text.x=element_text(size=12),
        plot.title=element_text(size=15),
        text=element_text(family='Arial'))+ 
  scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
  scale_y_continuous(breaks = seq(0, 40000, by = 200))


probability <- ggplot() + geom_point(alpha=0.75, aes(x=probs, y=n_probs_ve0.5),
                      size=I(4), col='royalblue')+
  labs(x='Probabilidad de experimentar infección durante el curso del estudio',
       y='Tamaño de muestra (participantes por grupo)',
       title='Tamaño de muestra según probabilidad acumulada de infección, asumiendo vacuna con 50% de eficacia')+
  theme_linedraw()+
  theme(axis.title.x=element_text(size=13),
        axis.title.y=element_text(size=13),
        axis.text.y=element_text(size=12),
        axis.text.x=element_text(size=12),
        plot.title=element_text(size=15),
        text=element_text(family='Arial'))+ 
  scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
  scale_y_continuous(breaks = seq(0, 1700, by = 100))

library(ggpubr)
figure <- ggarrange(efficacy, probability,
                    labels = c("A", "B"),
                    ncol = 1, nrow = 2)
figure
