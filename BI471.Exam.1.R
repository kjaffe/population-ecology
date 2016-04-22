log.growth <- function(t, y, p) {
  N <- y[1]
  with(as.list(p), {
    dN.dt <- r * N *(1- (N/K)^theta)
    return(list(dN.dt))
  })
}

p.t <- c('r' = .2, 'K' = 1.05, 'theta' = 1.05)
y0 <- c('N' = .01)
t <- 1:100

p.g <- c('r' = .28, 'K' = .75, 'theta' = 1.2)
p.p <- c('r' = .15, 'K' = 1, 'theta' = 1)

sim.t <- ode(y = y0, times = t, func = log.growth, parms = p.t, method = 'lsoda')
sim.t <- as.data.frame(sim.t)

sim.g <- ode(y = y0, times = t, func = log.growth, parms = p.g, method = 'lsoda')
sim.g <- as.data.frame(sim.g)

sim.p <- ode(y = y0, times = t, func = log.growth, parms = p.p, method = 'lsoda')
sim.p <- as.data.frame(sim.p)

sim.t$pop.growth.rate <- c(diff(sim.t$N), NA)
plot(pop.growth.rate ~ N, data = sim.t, type = 'l', ylim= c(0,.06), col = 'red', bty = 'l')

sim.g$pop.growth.rate <- c(diff(sim.g$N), NA)
points(pop.growth.rate ~ N, data = sim.g, type = 'l', col = 'purple', bty = 'l')

sim.p$pop.growth.rate <- c(diff(sim.p$N), NA)
points(pop.growth.rate ~ N, data = sim.p, type = 'l', col = 'pink', bty = 'l')

N.t.at.max.growth <- sim.t$N[which(sim.t$pop.growth.rate == max(sim.t$pop.growth.rate, na.rm = TRUE))]

N.g.at.max.growth <- sim.g$N[which(sim.g$pop.growth.rate == max(sim.g$pop.growth.rate, na.rm = TRUE))]

N.p.at.max.growth <- sim.p$N[which(sim.p$pop.growth.rate == max(sim.p$pop.growth.rate, na.rm = TRUE))]

N.t.at.max.growth
N.g.at.max.growth
N.p.at.max.growth







