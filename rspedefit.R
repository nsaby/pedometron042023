fit <- inlabru:: bru(components = cmp,
                     data = meuse,
                     family = "sn",
                     domain = list(coordinates = mesh),
                     options = list(
                       control.inla = list(int.strategy = "eb"),
                       verbose = FALSE)
)

rSPDE fit

add this here ?
  
  {r}
#| eval: false

library(rSPDE)
rspde_model <- rspde.matern(mesh = mesh)
cmp <- om ~ 
  field(coordinates, model = rspde_model ) + 
  Intercept(1) + 
  dist(dist, model = 'linear' )     
fit2 <- inlabru:: bru(components = cmp,
                      data = meuse,
                      family = "gaussian",
                      domain = list(coordinates = mesh),
                      options = list(
                        control.inla = list(int.strategy = "eb"),
                        verbose = FALSE)
)
result_fit <- rspde.result(fit2, "field", rspde_model)
summary(result_fit)

posterior_df_fit <- gg_df(result_fit)

ggplot(posterior_df_fit) + geom_line(aes(x = x, y = y)) + 
  facet_wrap(~parameter, scales = "free") + labs(y = "Density")

pred <- predict(
  fit,
  meuse.grid,
  ~ field + Intercept + dist ,
  num.threads = 2
)
pred$q0.025[pred$q0.025<0] = 0 
tm_shape(pred) +
  tm_raster(
    c("q0.025","median","q0.975")
  )




