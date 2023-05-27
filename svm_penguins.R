library("e1071")
library(ggplot2)

library(palmerpenguins)

data(package = 'palmerpenguins')

p <- subset(penguins, select=c("species","body_mass_g", "flipper_length_mm" ))
p <- na.omit(p)
p <- p[p$species != 'Chinstrap',]
p$species <- droplevels(p$species)

### scatter plot #1
###ggplot(p, aes(x=body_mass_g, y=flipper_length_mm, color=species)) + geom_point(size=4)

### default scatter plot
###plot(body_mass_g ~ flipper_length_mm, data=p, pch=16, col=p$species)
### quick svm_model
svm_model <- svm(species~.,data=p, type='C-classification', kernel='linear')
plot(svm_model,p, col = c("#FFFFFF","#FFFFFF"), grid = 250, svSymbol="●", dataSymbol="●")



### Polynomial Kernel

## Step 1 Cross validate
tune_poly=tune(svm ,species~.,data=p ,kernel ="poly", type='C-classification',#degree=4,#cost=1,
               ranges =list(cost=seq(from=0.1, to=5, by=0.1),
                            degree=seq(from=1, to=3, by=1),
                            gamma=2^(1:5),
                            coef0=seq(from=-10, to=-5, by=1)
                            )
               )
print(tune_poly$best.parameters)
svm_model <- tune_poly$best.model

### default svm plot with all points
plot(svm_model,p,color.palette = terrain.colors, title = "Poly", grid = 250, svSymbol="●", dataSymbol="○")

### plot with only SVM
plot(svm_model,p,color.palette = terrain.colors, title = "Poly", grid = 250, svSymbol="●", dataSymbol="")


### Radial Kernel
tune_radial <- tune(svm ,species~.,data=p ,kernel ="radial", type='C-classification',
               ranges =list(cost=seq(from=0.05,to=4,by=0.05),
                            gamma=seq(from=2^(0:4))
                            )
               )
print(tune_radial$best.parameters)
svm_model <- tune_radial$best.model

plot(svm_model,p, col = c("#CCFFFF","#65CCFF"), grid = 250, svSymbol="●", dataSymbol="○")
plot(svm_model,p, col = c("#CCFFFF","#65CCFF"), grid = 250, svSymbol="●", dataSymbol="")



### Linear Kernel
tune_linear=tune(svm ,species~.,data=p ,kernel ="linear", type='C-classification',
               ranges =list(cost=c(0.001,0.01,0.1,1,10,20), #seq(from=0.01,to=2,by=0.01),
                            coef0=seq(from=-10000,to=0,by=200)
                            )
               )
print(tune_linear$best.parameters)
svm_model <- tune_linear$best.model

#svm_model <- svm(species ~., data=p, type='C-classification', kernel='linear')
plot(svm_model,p, col= c("#FFFFEB", "#FFD699"), grid = 250, svSymbol="●", dataSymbol="○")
plot(svm_model,p, col= c("#FFFFEB", "#FFD699"), grid = 250, svSymbol="●", dataSymbol="")



# Using SVM with massively overlapping data

p <- subset(penguins, select=c("species","body_mass_g", "flipper_length_mm" ))
p <- na.omit(p)
###p <- p[p$species != 'Chinstrap',]
p <- p[p$species != 'Gentoo',]
plot(body_mass_g ~ flipper_length_mm, data=p, pch=16, col=p$species)


plot(body_mass_g ~ flipper_length_mm, data=p, pch=16, col=p$species)


tune_poly=tune(svm ,species~.,data=p ,kernel ="poly", type='C-classification',
               ranges =list(cost=c(0.1,1,2,5),
                            degree=c(1,2,3,4),
                            coef0=c(0,1,3)
                            )
               )
svm_model <- tune_poly$best.model
plot(svm_model,p,color.palette = terrain.colors, title = "Poly", grid = 250, svSymbol="●", dataSymbol="")


svm_model <- svm(species~.,data=p, type='C-classification', kernel='linear')




tune_sigmoid <- tune(svm ,species~.,data=p ,kernel ="sigmoid", type='C-classification',
               ranges =list(cost=seq(from=0.05,to=4,by=0.05),
                            gamma=seq(from=2^(0:4)),
                            coef=seq(from=-10, to=10, by=2)
                            )
               )
print(tune_sigmoid$best.parameters)
svm_model <- tune_sigmoid$best.model

plot(svm_model,p, col = c("#CCFFFF","#65CCFF"), grid = 250, svSymbol="●", dataSymbol="○")
