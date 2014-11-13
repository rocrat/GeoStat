library(sp)
data(meuse)
?meuse
coords<-cbind(meuse$x, meuse$y)
meuse.df<-SpatialPointsDataFrame(coords,meuse)
bubble(meuse.df,"zinc")

data(meuse.grid)
meuse.grid$ffreq<-as.factor(meuse.grid$ffreq)
m.grid.df<-SpatialPixelsDataFrame(points=meuse.grid[c("x","y")],data=meuse.grid)
pts<-list("sp.points",meuse.df,pch=4,col="white")
spplot(m.grid.df,"ffreq",col.regions=1:3,sp.layout=list(pts))

library(gstat)
#variogram cloud
cvgm<-variogram(zinc~1,data=meuse.df, width=100, cutoff=1000, cloud=T)
plot(cvgm)
#agregated
cvgm<-variogram(zinc ~ 1, data=meuse.df, width=100, cutoff=1000) 
plot(cvgm)

#fit the observation to the theoretical model
#Use exponential model 
vgm.md<-vgm(psill=1, model='Exp', range =100, nugget=1) 
efitted<-fit.variogram(cvgm, model=vgm.md) 
efitted 
plot(cvgm, model=efitted) 

#Using the fitted variogram conduct ordinary Kriging 
#fit ordinary kriging 
OK_fit<-gstat(id='OK_fit', formula=zinc ~ 1, data=meuse.df, model=efitted) 
#cross-validation 
pe<-gstat.cv(OK_fit, nfold=155, debug.level=0, random=FALSE)$residual 
round(sqrt(mean(pe^2)),2) 
z<-predict(OK_fit, newdata=m.grid.df, debug.level=0) 
m.grid.df$OK_pred <-z$OK_fit.pred #Kriging prediction 
m.grid.df$OK_se<-sqrt(z$OK_fit.var) #Kriging standard error 

#set up the plotting environment
library(maptools) 
bluepal<-colorRampPalette(c('azure1','steelblue4')) 
brks<-c(0, 130, 155, 195, 250, 330, 450, 630, 890, 1270, 1850) 
cols<-bluepal(length(brks)-1) 
sepal<-colorRampPalette(c('peachpuff1', 'tomato3')) 
brks.se<-c(0, 240, 250, 260, 270, 280, 290, 300, 350, 400, 1000) 
cols.se<-sepal(length(brks.se)-1) 
scols<-c('green', 'red') 

#Plot the Kriging prediction 
image (m.grid.df, 'OK_pred', breaks=brks, col=cols) 
symbols(coordinates(meuse.df), circles=sqrt(abs(pe)), fg='black', 
        bg=scols[(pe<0)+1], inches=FALSE, add=TRUE) 
legend('topleft',fill=cols, legend=leglabs(brks), bty ='n', cex=0.8) 

#Plot the Kriging standard errors 
image (m.grid.df, 'OK_se', breaks=brks.se, col=cols.se) 
symbols(coordinates(meuse.df), circles=sqrt(abs(pe)), fg='black', 
        bg=scols[(pe<0)+1], inches=FALSE, add=TRUE) 
legend('topleft', fill=cols.se, legend=leglabs(brks.se), bty='n', cex=0.8) 


