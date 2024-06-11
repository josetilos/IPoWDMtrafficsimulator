# IPoWDM traffic simulator example



# We assume a horshoe ring with N Access COs connected to 2 Regional COs.

Naco = 7;

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


  















