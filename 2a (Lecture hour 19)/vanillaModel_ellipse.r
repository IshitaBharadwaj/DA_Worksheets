dragon_neurons <- read.csv('dragon_neurons.csv')
head(dragon_neurons)

ols.lm <- lm(formula = conduction_velocity ~ axon_diameter, data = dragon_neurons)
ols.lm

library(ggplot2)

ggplot(dragon_neurons, aes(x=axon_diameter, y=conduction_velocity)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)

ols.res <- resid(ols.lm)

plot(dragon_neurons$axon_diameter, ols.res, ylab='Residuals', xlab='Axon diameter',
     main='Residual  plot')
abline(0, 0)

van_model <- dragon_neurons[c('axon_diameter' ,'conduction_velocity')]

# Find the center and covariance
van_model.center <- colMeans(van_model)
van_model.cov <- cov(van_model)

# Find the radius of the ellipse
van_model.rad <- sqrt(qchisq(p=0.95, df=ncol(van_model)))

# Find the ellipse coordinates
ellipse <- car::ellipse(center=van_model.center, shape=van_model.cov, radius=log_model.rad, segments=150, draw=FALSE)

#Plot the ellipse
ellipse <- as.data.frame(ellipse)
colnames(ellipse) <- colnames(van_model)

ggplot(van_model , aes(x=axon_diameter, y=conduction_velocity)) +
  geom_point(size = 2) +
  geom_polygon(data=ellipse , fill="yellow", color="yellow", alpha=0.5) +
  geom_point(aes(van_model.center[1] , van_model.center[2]) , size=5 , color="magenta") +
  geom_text(aes(label=row.names(van_model)), hjust=1, vjust=-1.5, size=2.5)

