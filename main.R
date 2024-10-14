# IPoWDM traffic simulator example



# We assume a horseshoe ring with N Access COs connected to 2 Regional COs Naco.

Naco = 7; 

a = 2

simulateE2Edelay <- function(Naco) {
  
  Peakmax = 100; Peakmin = Peakmax/4; 
  
  nexp = 1e3
  tt = matrix(NA,nrow=nexp,ncol=Naco)
  for (kk in c(1:nrow(tt))) {
    Peak_traff = runif(ncol(tt), Peakmin, Peakmax)
    Peak_sd = runif(ncol(tt), 0.3, 0.7)
    #traff_aco = NA*rep(1,Naco);
    for (ii in c(1:ncol(tt))) {
      tt[kk,ii] = rnorm(1,Peak_traff[ii],Peak_sd[ii])
    }
    #tt[kk] = sum(traff_aco/2)  
    
  }
  
  print(summary(tt))
  
  
  rho = cumsum(tt[1,]/2)/400
  
  EX = 8*1500/400e9
  
  
  ss_delay_ring = NA*c(1:nexp)
  for (kk in c(1:nexp)) {
    sim_delay = NA*rho;
    for (jj in c(1:length(rho))) {
      sim_delay[jj] = rexp(1,rate = (1-rho[jj])/EX)
    }
    ss_delay_ring[kk] = sum(sim_delay)
  }
  
  return(ss_delay_ring)
  
}



simulateE2Edelay(3)


data <- data.frame(ACO3 = simulateE2Edelay(3)*1e6, 
                   ACO5 = simulateE2Edelay(5)*1e6, 
                   ACO7 = simulateE2Edelay(7)*1e6,
                   ACO9 = simulateE2Edelay(9)*1e6)
                    
                    
boxplot(data,ylab = "End2End Latency (us)","title = Horshoe Access Ring")


  
library(ggplot2)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

xs = c(1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048)

fun.1 <- function(x) { 1 / ( 0.01 + x) }
fun.2 <- function(x) { 1 / ( 0.1 + x) }
fun.3 <- function(x) { 1 / ( 0.3 + x) }

ggplot(data.frame(x = xs), aes(x=x)) + 
  lapply(1:3, function(i) {
    fun_name <- paste0("fun.", i)
    fun <- get(fun_name)
    line <- geom_function(fun = fun,
                          aes(colour = fun_name))
    points <- geom_point(aes(y = fun(xs), colour = fun_name))
    list(line, points)
  }) +
  scale_x_log10(breaks = xs, name = "Input") +
  scale_y_continuous(name = "Growth") +
  theme_classic() +
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )


base <- ggplot(data.frame(x = xs), aes(x=as.factor(x)))
#Plot 2
base + theme_classic() +
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  ) +
  stat_function(fun = fun.1, aes(colour = "fun.1"),geom = 'point',size=1) + 
  stat_function(fun = fun.1, aes(colour = "fun.1"),geom = 'line') + 
  stat_function(fun = fun.2, aes(colour = "fun.2"),geom='point',size=1) +
  stat_function(fun = fun.2, aes(colour = "fun.2"),geom = 'line') +
  stat_function(fun = fun.3, aes(colour = "fun.3"),geom = 'point',size=1) +
  stat_function(fun = fun.3, aes(colour = "fun.3"),geom = 'line') +
  scale_x_discrete(name = "Input",
                   breaks = xs) +
  scale_y_discrete(name = "Growth")+
  scale_color_manual(values = c('red','blue','yellow'))





# set seed
set.seed(1234)

# create dataframe
df <- data.frame(category=factor(rep(c("3 hops", "5 hops", "7 hops", "9 hops"), each=nrow(data))),
                 Latency_us=c(data$ACO3,data$ACO5,data$ACO7,data$ACO9))

# load library ggplot2 package

library(ggplot2)




# Basic density plot with custom color
ggplot(df, aes(x=Latency_us, color=category)) + 
  
  # color property for changing color of plot
  # geom_density() function plots the density plot
  geom_density()






