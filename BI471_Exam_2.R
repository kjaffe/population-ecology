library('deSolve')

#Question 1c
pred.prey <- function(t, y, p) {
  H <- y[1]
  Z <- y[2]
  with(as.list(p), {
    dH.dt <- (r * H) * (1-(H/K)) - b * H * Z
    dZ.dt <- c * H * Z - m * Z
    return(list(c(dH.dt, dZ.dt)))
  })
}

t <- 1:100
y0 <- c('H' = 1, 'Z' = .1)
p <- c('r' = 1, 'b' = 1, 'K' = 1,
       'c' = 1, 'm' = .1)

sim <- ode(y = y0, times = t, func = pred.prey, parms = p,
           method = 'lsoda')
sim <- as.data.frame(sim)

plot(H ~time, type = 'l', col = 'blue', bty = 'l', data = sim, ylim = c(0,2))
points(Z ~time, type = 'l', lty = 2, col = 'green', bty = 'l', data = sim)

#Question 2
pred.prey.biocontrol <- function(t, y, p) {
  H <- y[1]
  Z <- y[2]
  P <- y[3]
  with(as.list(p), {
    dH.dt <- (r * H) * (1-(H/K)) - b * H * Z
    dZ.dt <- c * H * Z - m * Z - d * Z * P
    dP.dt <- e * Z * P - n * P
    return(list(c(dH.dt, dZ.dt, dP.dt)))
  })
}

t <- 1:100
y0 <- c('H' = 1, 'Z' = .1, 'P' = .1)
p <- c('r' = 1, 'b' = 1, 'K' = 1,
       'c' = 1, 'm' = .1, 'd' = 1,
       'n' = .1, 'e' = 1)

sim.biocontrol <- ode(y = y0, times = t, func = pred.prey.biocontrol, parms = p,
           method = 'lsoda')
sim.biocontrol <- as.data.frame(sim.biocontrol)

plot(H ~time, type = 'l', col = 'blue', bty = 'l', data = sim.biocontrol, ylim = c(0,2))
points(Z ~time, type = 'l', lty = 2, col = 'green', bty = 'l', data = sim.biocontrol)
points(P ~time, type = 'l', lty = 3, col = 'purple', bty = 'l', data = sim.biocontrol)



