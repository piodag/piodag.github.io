#########################################################################
#
#
# Copyright 2021-2024: all rights reserved
#
# Author: Dr Giorgio Pioda
# email: giorgio,pioda@spse.ch, giorgio.pioda@edu.ti.ch, gfwp@ticino.com
#
# 
########################################################################


library(nlstools)
library(sfsmisc)

cs.gauss<-function(xmin,xmax,n=40,slope=1,intercept=0,sigmax=1,sigmay=1){
  xr<-runif(n,xmin,xmax)
  yr<-slope*xr+intercept
  xe<-rnorm(1:length(xr),mean=0,sd=sigmax)
  ye<-rnorm(1:length(yr),mean=0,sd=sigmay)
  detmin<-0.1
  X<-ifelse((xr+xe) > detmin, xr+xe,detmin/2)
  Y<-ifelse((yr+ye) > detmin, yr+ye,detmin/2)
  return(data.frame(cbind(X=X,Y=Y)))
}

cs.het<-function(xmin,xmax,n=40,slope=1,intercept=0,sigmax=1,sigmay=1){
  xr<-runif(n,xmin,xmax)
  yr<-slope*xr+intercept
  xe<-xr/mean(xr)*rnorm(1:length(xr),mean=0,sd=sigmax)
  ye<-yr/mean(yr)*rnorm(1:length(yr),mean=0,sd=sigmay)
  detmin<-0.1
  X<-ifelse((xr+xe) > detmin, xr+xe,detmin/2)
  Y<-ifelse((yr+ye) > detmin, yr+ye,detmin/2)
  
  return(data.frame(cbind(X=X,Y=Y)))
}

cs.mixhet<-function(xmin,xmax,n=40,slope=1,intercept=0,sigmax=1,sigmay=1){
  xr<-runif(n,xmin,xmax)
  yr<-slope*xr+intercept
  ranx<-rnorm(1:length(xr),mean=0,sd=sigmax)
  rany<-rnorm(1:length(yr),mean=0,sd=sigmay)
  xe<-xr/mean(xr)*ranx/2+ranx/2
  ye<-yr/mean(yr)*rany/2+rany/2
  detmin<-0.1
  X<-ifelse((xr+xe) > detmin, xr+xe,detmin/2)
  Y<-ifelse((yr+ye) > detmin, yr+ye,detmin/2)
  
  return(data.frame(cbind(X=X,Y=Y)))
}

cs.st<-function(xmin,xmax,n=40,slope=1,intercept=0,sigmax=1,sigmay=1){
  xr<-runif(n,xmin,xmax)
  yr<-slope*xr+intercept
  xe<-rnorm(1:length(xr),mean=0,sd=sigmax)
  ye<-rnorm(1:length(yr),mean=0,sd=sigmay)
  detmin<-0.1
  X<-ifelse((xr+xe) > detmin, signif(xr+xe,2),detmin/2)
  Y<-ifelse((yr+ye) > detmin, signif(yr+ye,2),detmin/2)
  return(data.frame(cbind(X=X,Y=Y)))
}



p.pdmcomp2<-function(res,titolo="Comp",sigma.st=0.01,scale=0.05,pch=1,col="black",add=F,
                    lty=1,lwd=1,xmin=0.85,xmax=1.15){
  
  p.plotcompleto(res,"slope","d.tot",sigma.st = 0.025,main=titolo,lwd=1,lty=1,xmin=xmin,xmax=xmax,pch=1)
  p.plotcompleto(res,"slope","dmcd.e100",sigma.st = 0.015,lwd=1,lty=2,add=T,pch=1)
  p.plotcompleto(res,"slope","p.tot",sigma.st = 0.025,lwd=1,lty=1,add=T,pch=16,col="blue")
  p.plotcompleto(res,"slope","pmcd.e100",sigma.st = 0.015,lwd=1,lty=2,col="blue",add=T,pch=16)
  p.plotcompleto(res,"slope","m.tot",sigma.st = 0.025,lwd=1,lty=1,add=T,pch=17,col="red")
  p.plotcompleto(res,"slope","mmcd.e100",sigma.st = 0.015,lwd=1,lty=2,col="red",add=T,pch=17)
  
  
  legend("topright",legend=c("CI5% Dem","JE.MCD1% Dem",
                             "CI5% PaBa","JE.MCD1% PaBa",
                             "CI5% MDem","JE.MCD1% MDem"),
         title="Methods",
         col=rep(c("black","blue","red"),each=2),
         pch=c(1,1,16,16,17,17),
         lty=c(1,2,1,2,1,2))
  
}

p.pdmcomp3<-function(res,titolo="Comp",sigma.st=0.01,scale=0.05,pch=1,col="black",add=F,
                     lty=1,lwd=1,xmin=0.85,xmax=1.15){
  
  p.plotcompleto(res,"slope","d.tot",sigma.st = 0.025,main=titolo,lwd=1,lty=1,xmin=xmin,xmax=xmax,pch=1)
  p.plotcompleto(res,"slope","dmcd.e100",sigma.st = 0.015,lwd=1,lty=2,add=T,pch=1)
  p.plotcompleto(res,"slope","p.tot",sigma.st = 0.025,lwd=1,lty=1,add=T,pch=16,col="blue")
  p.plotcompleto(res,"slope","pmcd.e100",sigma.st = 0.015,lwd=1,lty=2,col="blue",add=T,pch=16)
  p.plotcompleto(res,"slope","m.tot",sigma.st = 0.025,lwd=1,lty=1,add=T,pch=17,col="red")
  p.plotcompleto(res,"slope","mmcd.e100",sigma.st = 0.015,lwd=1,lty=2,col="red",add=T,pch=17)
  p.plotcompleto(res,"slope","mm.tot",sigma.st = 0.025,lwd=1,lty=1,add=T,pch=2,col="purple")
  p.plotcompleto(res,"slope","mmmcd.e100",sigma.st = 0.015,lwd=1,lty=2,col="purple",add=T,pch=2)
  p.plotcompleto(res,"slope","w.tot",sigma.st = 0.025,lwd=1,lty=1,add=T,pch=3,col="orange")
  p.plotcompleto(res,"slope","wmcd.e100",sigma.st = 0.025,lwd=1,lty=2,col="orange",add=T,pch=3)
  
  
  legend("topright",legend=c("CI5% Dem","JE.MCD1% Dem",
                             "CI5% PaBa","JE.MCD1% PaBa",
                             "CI5% MDem","JE.MCD1% MDem",
                             "CI5% MMDem","JE.MCD1% MMDem",
                             "CI5% WDem","JE.MCD1% WDem"),
         title="Methods",
         col=rep(c("black","blue","red","purple","orange"),each=2),
         pch=c(1,1,16,16,17,17,2,2,3,3),
         lty=c(1,2,1,2,1,2,1,2,1,2))
  
}

p.pdmHet<-function(res1,res2,res3,col1="m.tot",col2="mmcd.e100",titolo="Comp",sigma.st=0.01,scale=0.05,pch=1,col="black",add=F,
                     lty=1,lwd=1,xmin=0.85,xmax=1.15){
  
  p.plotcompleto(res1,"slope",col1,sigma.st = 0.025,main=titolo,lwd=1,lty=1,xmin=xmin,xmax=xmax,pch=1)
  p.plotcompleto(res1,"slope",col2,sigma.st=ifelse(col2=="wmcd.e100",0.03,0.015),lwd=1,lty=1,add=T,pch=1,col="purple")
  p.plotcompleto(res2,"slope",col1,sigma.st = 0.025,lwd=1,lty=2,add=T,pch=16)
  p.plotcompleto(res2,"slope",col2,sigma.st=ifelse(col2=="wmcd.e100",0.03,0.015),lwd=1,lty=2,col="purple",add=T,pch=16)
  p.plotcompleto(res3,"slope",col1,sigma.st = 0.025,lwd=1,lty=3,add=T,pch=17)
  p.plotcompleto(res3,"slope",col2,sigma.st=ifelse(col2=="wmcd.e100",0.03,0.015),lwd=1,lty=3,col="purple",add=T,pch=17)
  
  legend("topright",legend=c("CI5% hom","JE.MCD1% hom",
                             "CI5% mix","JE.MCD1% mix",
                             "CI5% het","JE.MCD1% het"),
         title="Methods",
         col=rep(c("black","purple"),3),
         pch=c(1,1,16,16,17,17),
         lty=c(1,1,2,2,3,3))
  
}

fit.nls<-function(res,x,y,sigma.st=0.01){
  
  form<-paste(y," ~ scale*beta*exp(-(abs(",x,"-mu)/sigma)^beta)/(2*sigma*gamma(1/beta))",sep="")
  form<-as.formula(form)
  t.f<-nls(form,data=res,
           start=list(scale=0.05,mu=1,sigma=sigma.st,beta=2))
}


fit.int<-function(res,x,y,sigma.st=0.2,scale=1){
  
  form<-paste(y," ~ scale*beta*exp(-(abs(",x,"-mu)/sigma)^beta)/(2*sigma*gamma(1/beta))",sep="")
  form<-as.formula(form)
  t.f<-nls(form,data=res,
           start=list(scale=scale,mu=0,sigma=sigma.st,beta=2))
}

pow.estim<-function(t.f,p.pow=0.2){
  t.cov<-summary(t.f)$cov.unscaled
  t.s2<-summary(t.f)$sigma^2
  t.theta<-coef(t.f)
  
  (f.a0<-deriv(p.tot ~ scal*bet*exp(-((slope-mu)/sig)^bet)/(2*sig*gamma(1/bet)),
              c("scal","mu","sig","bet"),
              function(scal,mu,sig,bet,slope){}))
  
  r.x<-c((t.theta[2]+0.000000000001),seq(1.002,1.3,0.0001))
  r.pred <- predict(t.f,newdata=data.frame(slope=r.x))
  t.a0<-f.a0(t.theta[1],t.theta[2],t.theta[3],t.theta[4],r.x)
  t.a0<-as.matrix(attr(t.a0, "gradient"))
  
  t.s2.x0 <- t.s2 * diag(t.a0 %*% t.cov %*% t(t.a0))
  r.limit <- sqrt(t.s2 + t.s2.x0) * qt(0.975, summary(t.f)$df[2])
  pow.es<-f.calib(r.x, r.pred, r.limit, p.pow)
  return(pow.es)
  
}

pow.intestim<-function(t.f,p.pow=0.2){
  t.cov<-summary(t.f)$cov.unscaled
  t.s2<-summary(t.f)$sigma^2
  t.theta<-coef(t.f)
  
  f.a0<-deriv(p.tot ~ scal*bet*exp(-((intercept-mu)/sig)^bet)/(2*sig*gamma(1/bet)),
              c("scal","mu","sig","bet"),
              function(scal,mu,sig,bet,intercept){})
  
  r.x<-seq(t.theta[2]+0.001,8,0.001)
  r.pred <- predict(t.f,newdata=data.frame(intercept=r.x))
  t.a0<-f.a0(t.theta[1],t.theta[2],t.theta[3],t.theta[4],r.x)
  t.a0<-as.matrix(attr(t.a0, "gradient"))
  
  t.s2.x0 <- t.s2 * diag(t.a0 %*% t.cov %*% t(t.a0))
  r.limit <- sqrt(t.s2 + t.s2.x0) * qt(0.975, summary(t.f)$df[2])
  pow.es<-f.calib(r.x, r.pred, r.limit, 0.2)
  return(pow.es)
  
}


approx.center<-function(t.f){
  t.cov<-summary(t.f)$cov.unscaled
  t.s2<-summary(t.f)$sigma^2
  t.theta<-coef(t.f)
  f.a0<-deriv(p.tot ~ scal*bet*exp(-((slope-mu)/sig)^bet)/(2*sig*gamma(1/bet)),
              c("scal","mu","sig","bet"),
              function(scal,mu,sig,bet,slope){})
  
  center<-c((t.theta[2]+0.000000001))
  #r.pred <- predict(t.f,newdata=data.frame(slope=r.x))
  r.pred <- predict(t.f,newdata=data.frame(slope=center))
  t.a0<-f.a0(t.theta[1],t.theta[2],t.theta[3],t.theta[4],center)
  t.a0<-as.matrix(attr(t.a0, "gradient"))
  
  t.s2.x0 <- t.s2 * diag(t.a0 %*% t.cov %*% t(t.a0))
  pred.val<-predict(t.f,newdata=list(slope=t.theta[2]))
  r.limit <- sqrt(t.s2 + t.s2.x0) * qt(0.975, summary(t.f)$df[2])
  pred.lim<-pred.val + r.limit * c(-1,1)
  t.pr<-c(pred.lim[1],pred.val,pred.lim[2])
  names(t.pr)<-c("LCI","peak.est","UCI")
  return(t.pr)
}

approx.intcenter<-function(t.f){
  t.cov<-summary(t.f)$cov.unscaled
  t.s2<-summary(t.f)$sigma^2
  t.theta<-coef(t.f)
  f.a0<-deriv(p.tot ~ scal*bet*exp(-((intercept-mu)/sig)^bet)/(2*sig*gamma(1/bet)),
              c("scal","mu","sig","bet"),
              function(scal,mu,sig,bet,intercept){})
  
  center<-c((t.theta[2]+0.001))
  #r.pred <- predict(t.f,newdata=data.frame(slope=r.x))
  #r.pred <- predict(t.f,newdata=data.frame(intercept=center))
  t.a0<-f.a0(t.theta[1],t.theta[2],t.theta[3],t.theta[4],center)
  t.a0<-as.matrix(attr(t.a0, "gradient"))
  
  t.s2.x0 <- t.s2 * diag(t.a0 %*% t.cov %*% t(t.a0))
  pred.val<-predict(t.f,newdata=list(intercept=t.theta[2]))
  r.limit <- sqrt(t.s2 + t.s2.x0) * qt(0.975, summary(t.f)$df[2])
  pred.lim<-pred.val + r.limit * c(-1,1)
  t.pr<-c(pred.lim[1],pred.val,pred.lim[2])
  names(t.pr)<-c("LCI","peak.est","UCI")
  return(t.pr)
}


p.slopecurve<-function(fit.nls){
  r.x<-seq(0.85,1.15,by=0.001)
  r.y<-predict(fit.nls,newdata=data.frame(slope=r.x))
  plot(r.x,r.y,ylim=c(0,1),xlim=c(0.85,1.15),type="l")
}


p.plotcompleto<-function(res,x,y,sigma.st=0.01,scale=0.05,pch=1,col="black",add=F,
                         lty=1,lwd=1,main="title",xmin=0.85,xmax=1.15){
  form<-paste(y," ~ scale*beta*exp(-(abs(",x,"-mu)/sigma)^beta)/(2*sigma*gamma(1/beta))",sep="")
  form<-as.formula(form)
  t.f<-nls(form,data=res,
           start=list(scale=scale,mu=1,sigma=sigma.st,beta=2))
  r.x<-seq(xmin,xmax,by=(xmax-xmin)/300)
  r.y<-predict(t.f,newdata=data.frame(slope=r.x))
  form.p<-as.formula(paste(y, "~",x))
  if (add == F) {
      plot(form.p,data=res,col=col,pch=pch,
           ylim=c(0,1),xlim=c(xmin,xmax),
           #add=add,
           ylab="Empirical type II error",
           main=main)
      lines(r.x,r.y,col=col,lty=lty,lwd=lwd)
      grid()
      abline(h=0.2,lwd=2,lty=2,col="green")
      abline(h=0.95,lwd=2,lty=2,col="green")
      } else {
      points(form.p,data=res,col=col,pch=pch,lty=lty,lwd=lwd,
             ylim=c(0,1),xlim=c(0.85,1.15),add=add)
      lines(r.x,r.y,col=col,lty=lty,lwd=lwd)
  }
}



p.ptotale3<-function(res,titolo="main",xmin=0.85,xmax=1.15){
  pal<-colorRampPalette(c("orange","red","purple"))
  colpal<-pal(3)
  p.plotcompleto(res,"slope","p.tot",sigma.st = 0.03,main=titolo,lwd=1,lty=1,xmin=xmin,xmax=xmax)
  p.plotcompleto(res,"slope","pr.e500",sigma.st = 0.01,col=colpal[1],add=T,lty=1)
  p.plotcompleto(res,"slope","pmcd.e500",sigma.st = 0.01,col=colpal[2],add=T,lty=1)
  p.plotcompleto(res,"slope","pc.e500",sigma.st = 0.01,lwd=1,lty=1,col=colpal[3],add=T)
  p.plotcompleto(res,"slope","pr.e100",sigma.st = 0.01,col=colpal[1],add=T,lty=2)
  p.plotcompleto(res,"slope","pmcd.e100",sigma.st = 0.01,col=colpal[2],add=T,lty=2)
  p.plotcompleto(res,"slope","pc.e100",sigma.st = 0.01,lwd=1,col=colpal[3],add=T,lty=2)
  legend("topright",legend=c("CI5%","JE.SDe5%","JE.MCD5%","JE.Clas5%",
                             "JE.SDe1%","JE.MCD1%","JE.Clas1%"),
         title="Methods",
         pch=rep(1,7),
         col=c("black",rep(colpal[1:3],2)),
         lty=c(1,1,1,1,2,2,2),
         lwd=c(1,1,1,1,1,1,1))
  
}

p.mtotale3<-function(res,titolo="main",xmin=0.85,xmax=1.15){
  pal<-colorRampPalette(c("orange","red","purple"))
  colpal<-pal(3)
  p.plotcompleto(res,"slope","m.tot",sigma.st = 0.03,main=titolo,lwd=1,lty=1,xmin=xmin,xmax=xmax)
  p.plotcompleto(res,"slope","mr.e500",sigma.st = 0.01,col=colpal[1],add=T,lty=1)
  p.plotcompleto(res,"slope","mmcd.e500",sigma.st = 0.01,col=colpal[2],add=T,lty=1)
  p.plotcompleto(res,"slope","mc.e500",sigma.st = 0.01,lwd=1,lty=1,col=colpal[3],add=T)
  p.plotcompleto(res,"slope","mr.e100",sigma.st = 0.01,col=colpal[1],add=T,lty=2)
  p.plotcompleto(res,"slope","mmcd.e100",sigma.st = 0.01,col=colpal[2],add=T,lty=2)
  p.plotcompleto(res,"slope","mc.e100",sigma.st = 0.01,lwd=1,col=colpal[3],add=T,lty=2)
  legend("topright",legend=c("CI5%","JE.SDe5%","JE.MCD5%","JE.Clas5%",
                             "JE.SDe1%","JE.MCD1%","JE.Clas1%"),
         title="Methods",
         pch=rep(1,7),
         col=c("black",rep(colpal[1:3],2)),
         lty=c(1,1,1,1,2,2,2),
         lwd=c(1,1,1,1,1,1,1))
  
}

p.dtotale3<-function(res,titolo="main",xmin=0.85,xmax=1.15){
  pal<-colorRampPalette(c("orange","red","purple"))
  colpal<-pal(3)
  p.plotcompleto(res,"slope","d.tot",sigma.st = 0.03,main=titolo,lwd=1,lty=1,xmin=xmin,xmax=xmax)
  p.plotcompleto(res,"slope","dr.e500",sigma.st = 0.01,col=colpal[1],add=T,lty=1)
  p.plotcompleto(res,"slope","dmcd.e500",sigma.st = 0.01,col=colpal[2],add=T,lty=1)
  p.plotcompleto(res,"slope","dc.e500",sigma.st = 0.01,lwd=1,lty=1,col=colpal[3],add=T)
  p.plotcompleto(res,"slope","dr.e100",sigma.st = 0.01,col=colpal[1],add=T,lty=2)
  p.plotcompleto(res,"slope","dmcd.e100",sigma.st = 0.01,col=colpal[2],add=T,lty=2)
  p.plotcompleto(res,"slope","dc.e100",sigma.st = 0.01,lwd=1,col=colpal[3],add=T,lty=2)
  legend("topright",legend=c("CI5%","JE.SDe5%","JE.MCD5%","JE.Clas5%",
                             "JE.SDe1%","JE.MCD1%","JE.Clas1%"),
         title="Methods",
         pch=rep(1,7),
         col=c("black",rep(colpal[1:3],2)),
         lty=c(1,1,1,1,2,2,2),
         lwd=c(1,1,1,1,1,1,1))
  
}

p.wtotale3<-function(res,titolo="main",xmin=0.85,xmax=1.15){
  pal<-colorRampPalette(c("orange","red","purple"))
  colpal<-pal(3)
  p.plotcompleto(res,"slope","w.tot",sigma.st = 0.03,main=titolo,lwd=1,lty=1,xmin=xmin,xmax=xmax)
  p.plotcompleto(res,"slope","wr.e500",sigma.st = 0.02,col=colpal[1],add=T,lty=1)
  p.plotcompleto(res,"slope","wmcd.e500",sigma.st = 0.02,col=colpal[2],add=T,lty=1)
  p.plotcompleto(res,"slope","wc.e500",sigma.st = 0.02,lwd=1,lty=1,col=colpal[3],add=T)
  p.plotcompleto(res,"slope","wr.e100",sigma.st = 0.02,col=colpal[1],add=T,lty=2)
  p.plotcompleto(res,"slope","wmcd.e100",sigma.st = 0.02,col=colpal[2],add=T,lty=2)
  p.plotcompleto(res,"slope","wc.e100",sigma.st = 0.02,lwd=1,col=colpal[3],add=T,lty=2)
  legend("topright",legend=c("CI5%","JE.SDe5%","JE.MCD5%","JE.Clas5%",
                             "JE.SDe1%","JE.MCD1%","JE.Clas1%"),
         title="Methods",
         pch=rep(1,7),
         col=c("black",rep(colpal[1:3],2)),
         lty=c(1,1,1,1,2,2,2),
         lwd=c(1,1,1,1,1,1,1))
  
}

p.mmtotale3<-function(res,titolo="main",xmin=0.85,xmax=1.15){
  pal<-colorRampPalette(c("orange","red","purple"))
  colpal<-pal(3)
  p.plotcompleto(res,"slope","mm.tot",sigma.st = 0.03,main=titolo,lwd=1,lty=1,xmin=xmin,xmax=xmax)
  p.plotcompleto(res,"slope","mmr.e500",sigma.st = 0.01,col=colpal[1],add=T,lty=1)
  p.plotcompleto(res,"slope","mmmcd.e500",sigma.st = 0.01,col=colpal[2],add=T,lty=1)
  p.plotcompleto(res,"slope","mmc.e500",sigma.st = 0.01,lwd=1,lty=1,col=colpal[3],add=T)
  p.plotcompleto(res,"slope","mmr.e100",sigma.st = 0.01,col=colpal[1],add=T,lty=2)
  p.plotcompleto(res,"slope","mmmcd.e100",sigma.st = 0.01,col=colpal[2],add=T,lty=2)
  p.plotcompleto(res,"slope","mmc.e100",sigma.st = 0.01,lwd=1,col=colpal[3],add=T,lty=2)
  legend("topright",legend=c("CI5%","JE.SDe5%","JE.MCD5%","JE.Clas5%",
                             "JE.SDe1%","JE.MCD1%","JE.Clas1%"),
         title="Methods",
         pch=rep(1,7),
         col=c("black",rep(colpal[1:3],2)),
         lty=c(1,1,1,1,2,2,2),
         lwd=c(1,1,1,1,1,1,1))
  
}



p.intcompleto<-function(res,x,y,sigma.st=0.1, scale=0.5,pch=1,col="black", add=F,
                        lty=1,lwd=1,main="title",xmin=-1.2,xmax=1.2) {
  form<-paste(y," ~ scale*beta*exp(-(abs(",x,"-mu)/sigma)^beta)/(2*sigma*gamma(1/beta))",sep="")
  form<-as.formula(form)
  t.f<-nls(form,data=res,
           start=list(scale=scale,mu=0,sigma=sigma.st,beta=2))
  
  r.x<-seq(xmin,xmax,by=(xmax-xmin)/300)
  r.y<-predict(t.f,newdata=data.frame(intercept=r.x))
  form.p<-as.formula(paste(y, "~",x))
  if (add == F) {
    plot(form.p,data=res,col=col,pch=pch,
         ylim=c(0,1),xlim=c(xmin,xmax),add=add,
         ylab="Empirical type II error",
         main=main)
    lines(r.x,r.y,col=col,lty=lty,lwd=lwd)
    grid()
    abline(h=0.2,lwd=2,lty=2,col="green")
    abline(h=0.95,lwd=2,lty=2,col="green")
  } else {
    points(form.p,data=res,col=col,pch=pch,lty=lty,lwd=lwd,
           ylim=c(0,1),xlim=c(0.85,1.15),add=add)
    lines(r.x,r.y,col=col,lty=lty,lwd=lwd)
  }
}



p.intshortTer<-function(res,titolo="main",xmin=-1.2,xmax=1.2){
  pal<-colorRampPalette(c("orange","red","purple"))
  #colpal<-pal(6)
  p.intcompleto(res,"intercept","d.tot",sigma.st = 0.3,scale=0.5,main=titolo,xmin=xmin,xmax=xmax,lty=1)
  p.intcompleto(res,"intercept","dmcd.e100",sigma.st = 0.1,add=T,lty=2,lwd=1)
  p.intcompleto(res,"intercept","p.tot",sigma.st = 0.3,scale=0.5,col="blue",add=T,lty=1,lwd=1,pch=16)
  p.intcompleto(res,"intercept","pmcd.e100",sigma.st = 0.15,col="blue",add=T,lty=2,lwd=1,pch=16)
  p.intcompleto(res,"intercept","m.tot",sigma.st = 0.3,scale=0.5,col="red",add=T,lty=1,lwd=1,pch=17)
  p.intcompleto(res,"intercept","mmcd.e100",sigma.st = 0.15,col="red",add=T,lty=2,lwd=1,pch=17)
  p.intcompleto(res,"intercept","mm.tot",sigma.st = 0.3,scale=0.5,col="purple",add=T,lty=1,lwd=1,pch=2)
  p.intcompleto(res,"intercept","mmmcd.e100",sigma.st = 0.2,col="purple",add=T,lty=2,lwd=1,pch=2)
  p.intcompleto(res,"intercept","w.tot",sigma.st = 0.3,scale=0.5,col="orange",add=T,lty=1,lwd=1,pch=3)
  p.intcompleto(res,"intercept","wmcd.e100",sigma.st = 0.15,scale=0.5,col="orange",add=T,lty=2,lwd=1,pch=3)
  
  legend("topright",legend=c("CI5% Dem","JE.MCD1% Dem",
                             "CI5% PaBa","JE.MCD1% PaBa",
                             "CI5% MDem","JE.MCD1% MDem",
                             "CI5% MMDem","JE.MCD1% MMDem",
                             "CI5% WDem","JE.MCD1% WDem"),
         title="Methods",
         pch=c(1,1,16,16,17,17,2,2,3,3),
         col=rep(c("black","blue","red","purple","orange"),each=2),
         lty=c(1,2,1,2,1,2,1,2,1,2))
  
}



p.intlongTer<-function(res,titolo="main",xmin=-5,xmax=5){
  
  p.intcompleto(res,"intercept","d.tot",sigma.st = 2,scale=2,main=titolo,xmin=xmin,xmax=xmax,lty=1,lwd=1)
  p.intcompleto(res,"intercept","dmcd.e100",sigma.st = 1.5,scale=2,add=T,xmin=xmin,xmax=xmax,lty=2,lwd=1)
  p.intcompleto(res,"intercept","p.tot",sigma.st = 2,scale=2,col="blue",add=T,xmin=xmin,xmax=xmax,lty=1,lwd=1,pch=16)
  p.intcompleto(res,"intercept","pmcd.e100",sigma.st = 2,scale=2,col="blue",add=T,xmin=xmin,xmax=xmax,lwd=1,lty=2,pch=16)
  p.intcompleto(res,"intercept","m.tot",sigma.st = 2,scale=2,col="red",add=T,xmin=xmin,xmax=xmax,lty=1,lwd=1,pch=17)
  p.intcompleto(res,"intercept","mmcd.e100",sigma.st = 1.5,scale=2,col="red",add=T,xmin=xmin,xmax=xmax,lwd=1,lty=2,pch=17)
  p.intcompleto(res,"intercept","mm.tot",sigma.st = 2,scale=2,col="purple",add=T,xmin=xmin,xmax=xmax,lty=1,lwd=1,pch=2)
  p.intcompleto(res,"intercept","mmmcd.e100",sigma.st = 2,scale=2,col="purple",add=T,xmin=xmin,xmax=xmax,lwd=1,lty=2,pch=2)
  p.intcompleto(res,"intercept","w.tot",sigma.st = 2,scale=2,col="orange",add=T,xmin=xmin,xmax=xmax,lty=1,lwd=1,pch=3)
  p.intcompleto(res,"intercept","wmcd.e100",sigma.st = 2,scale=2,col="orange",add=T,xmin=xmin,xmax=xmax,lwd=1,lty=2,pch=3)
  
  legend("topright",legend=c("CI5% Dem","JE.MCD1% Dem",
                             "CI5% PaBa","JE.MCD1% PaBa",
                             "CI5% MDem","JE.MCD1% MDem",
                             "CI5% MMDem","JE.MCD1% MMDem",
                             "CI5% WDem","JE.MCD1% WDem"),
         title="Methods",
         pch=c(1,1,16,16,17,17,2,2,3,3),
         col=rep(c("black","blue","red","purple","orange"),each=2),
         lty=c(1,2,1,2,1,2,1,2,1,2))
  
}


f.powdatQuad<-function(res,col1="m.tot",col2="mr.e100",col3="mmcd.e100",col4="mc.e100"){
  
  f.tot<-fit.nls(res,"slope",col1,sigma.st=0.03)
  f.e100r<-fit.nls(res,"slope",col2,sigma.st=0.015)
  f.totmcd<-fit.nls(res,"slope",col3,sigma.st=0.015)
  f.e100cl<-fit.nls(res,"slope",col4,sigma.st=0.015)
  
  t.tot.max<-approx.center(f.tot)
  t.e100r.max<-approx.center(f.e100r)
  t.totmcd.max<-approx.center(f.totmcd)
  t.e100cl.max<-approx.center(f.e100cl)
  
  t.tot.p20<-pow.estim(f.tot)
  t.e100r.p20<-pow.estim(f.e100r)
  t.totmcd.p20<-pow.estim(f.totmcd)
  t.e100cl.p20<-pow.estim(f.e100cl)
  
  m.powdat<-data.frame(NA,ncol=3,nrow=4)
  m.powdat[1,]<-t.tot.p20
  m.powdat[2,]<-t.e100r.p20
  m.powdat[3,]<-t.totmcd.p20
  m.powdat[4,]<-t.e100cl.p20
  
  m.maxdat<-data.frame(NA,ncol=3,nrow=4)
  m.maxdat[1,]<-t.tot.max
  m.maxdat[2,]<-t.e100r.max
  m.maxdat[3,]<-t.totmcd.max
  m.maxdat[4,]<-t.e100cl.max
  
  f.maxdat<-cbind(m.powdat,m.maxdat)
  
  colnames(f.maxdat)<-c("p80% LCI","p80% est","p80% UCI","T-I LCI","T-I est","T-I UCI")
  #rownames(f.maxdat)<-c("CI5%","JE.SDe1%","JE.MCD1%","JE.Clas1%")
  f.maxdat<-cbind(Method=c("CI5%","JE.SDe1%","JE.MCD1%","JE.Clas1%"),f.maxdat)
  return(f.maxdat)
  
}


f.powdatPenta<-function(res,col1="m.tot",col2="mr.e100",col3="mmcd.e100",col4="mc.e100"){
  
  f.tot<-fit.nls(res,"slope",col1,sigma.st=0.03)
  f.e100r<-fit.nls(res,"slope",col2,sigma.st=ifelse(col2=="wr.e100",0.03,0.015))
  f.totmcd<-fit.nls(res,"slope",col3,sigma.st=ifelse(col3=="wmcd.e100",0.03,0.015))
  f.e100cl<-fit.nls(res,"slope",col4,sigma.st=ifelse(col4=="wc.e100",0.03,0.015))
  
  t.tot.max<-approx.center(f.tot)
  t.e100r.max<-approx.center(f.e100r)
  t.totmcd.max<-approx.center(f.totmcd)
  t.e100cl.max<-approx.center(f.e100cl)
  
  t.tot.p20<-pow.estim(f.tot)
  t.e100r.p20<-pow.estim(f.e100r)
  t.totmcd.p20<-pow.estim(f.totmcd)
  t.e100cl.p20<-pow.estim(f.e100cl)
  
  m.powdat<-data.frame(NA,ncol=3,nrow=4)
  m.powdat[1,]<-t.tot.p20
  m.powdat[2,]<-t.e100r.p20
  m.powdat[3,]<-t.totmcd.p20
  m.powdat[4,]<-t.e100cl.p20
  
  m.maxdat<-data.frame(NA,ncol=3,nrow=4)
  m.maxdat[1,]<-t.tot.max
  m.maxdat[2,]<-t.e100r.max
  m.maxdat[3,]<-t.totmcd.max
  m.maxdat[4,]<-t.e100cl.max
  
  f.maxdat<-cbind(m.powdat,m.maxdat)
  
  colnames(f.maxdat)<-c("p80% LCI","p80% est","p80% UCI","T-I LCI","T-I est","T-I UCI")
  #rownames(f.maxdat)<-c("CI5%","JE.SDe1%","JE.MCD1%","JE.Clas1%")
  f.maxdat<-cbind(Method=c("CI5%","JE.SDe1%","JE.MCD1%","JE.Clas1%"),f.maxdat)
  return(f.maxdat)
  
}


f.powintshortQuad<-function(res,col1="m.tot",col2="mmcd.e100"){
  
  f.tot<-fit.int(res,"intercept",col1,sigma.st=0.3,scale=0.5)
  fmcd.e100<-fit.int(res,"intercept",col2,sigma.st=0.15,scale=0.3)
  
  t.tot.max<-approx.intcenter(f.tot)
  tmcd.e100.max<-approx.intcenter(fmcd.e100)
 
  t.tot.p20<-pow.intestim(f.tot)
  tmcd.e100.p20<-pow.intestim(fmcd.e100)
  
  m.powdat<-data.frame(NA,ncol=3,nrow=2)
  m.powdat[1,]<-t.tot.p20
  m.powdat[2,]<-tmcd.e100.p20
  
  m.maxdat<-data.frame(NA,ncol=3,nrow=2)
  m.maxdat[1,]<-t.tot.max
  m.maxdat[2,]<-tmcd.e100.max
 
  f.maxdat<-cbind(m.powdat,m.maxdat)
  
  colnames(f.maxdat)<-c("p80% LCI","p80% est","p80% UCI","T-I LCI","T-I est.","T-I UCI")
  rownames(f.maxdat)<-c("CI -5%","e100.MCD")
  return(f.maxdat)
  
}

f.powintshortPenta<-function(res,col1="m.tot",col2="mr.e100",col3="mmcd.e100",col4="mc.e100"){
  
  f.tot<-fit.int(res,"intercept",col1,sigma.st=0.3,scale=0.5)
  fsde.e100<-fit.int(res,"intercept",col2,sigma.st=0.15,scale=0.3)
  fmcd.e100<-fit.int(res,"intercept",col2,sigma.st=0.15,scale=0.3)
  fcl.e100<-fit.int(res,"intercept",col2,sigma.st=0.15,scale=0.3)
  
  t.tot.max<-approx.intcenter(f.tot)
  tsde.e100.max<-approx.intcenter(fsde.e100)
  tmcd.e100.max<-approx.intcenter(fmcd.e100)
  tcl.e100.max<-approx.intcenter(fcl.e100)
  
  t.tot.p20<-pow.intestim(f.tot)
  tsde.e100.p20<-pow.intestim(fsde.e100)
  tmcd.e100.p20<-pow.intestim(fmcd.e100)
  tcl.e100.p20<-pow.intestim(fcl.e100)
  
  m.powdat<-data.frame(NA,ncol=3,nrow=4)
  m.powdat[1,]<-t.tot.p20
  m.powdat[2,]<-tsde.e100.p20
  m.powdat[3,]<-tmcd.e100.p20
  m.powdat[4,]<-tcl.e100.p20
  
  m.maxdat<-data.frame(NA,ncol=3,nrow=2)
  m.maxdat[1,]<-t.tot.max
  m.maxdat[2,]<-tsde.e100.max
  m.maxdat[3,]<-tmcd.e100.max
  m.maxdat[4,]<-tcl.e100.max
  
  f.maxdat<-cbind(m.powdat,m.maxdat)
  
  colnames(f.maxdat)<-c("p80% LCI","p80% est","p80% UCI","T-I LCI","T-I est.","T-I UCI")
  #rownames(f.maxdat)<-c("CI -5%","e100.SDe","e100.MCD","e100.Cl")
  f.maxdat<-cbind(Method=c("CI5%","JE.SDe1%","JE.MCD1%","JE.Clas1%"),f.maxdat)
  return(f.maxdat)
  
}

f.powintlongPenta<-function(res,col1="m.tot",col2="mr.e100",col3="mmcd.e100",col4="mc.e100"){
  
  f.tot<-fit.int(res,"intercept",col1,sigma.st=1,scale=1)
  fsde.e100<-fit.int(res,"intercept",col2,sigma.st=1,scale=1)
  fmcd.e100<-fit.int(res,"intercept",col2,sigma.st=1,scale=1)
  fcl.e100<-fit.int(res,"intercept",col2,sigma.st=1,scale=1)
  
  t.tot.max<-approx.intcenter(f.tot)
  tsde.e100.max<-approx.intcenter(fsde.e100)
  tmcd.e100.max<-approx.intcenter(fmcd.e100)
  tcl.e100.max<-approx.intcenter(fcl.e100)
  
  t.tot.p20<-pow.intestim(f.tot)
  tsde.e100.p20<-pow.intestim(fsde.e100)
  tmcd.e100.p20<-pow.intestim(fmcd.e100)
  tcl.e100.p20<-pow.intestim(fcl.e100)
  
  m.powdat<-data.frame(NA,ncol=3,nrow=4)
  m.powdat[1,]<-t.tot.p20
  m.powdat[2,]<-tsde.e100.p20
  m.powdat[3,]<-tmcd.e100.p20
  m.powdat[4,]<-tcl.e100.p20
  
  m.maxdat<-data.frame(NA,ncol=3,nrow=2)
  m.maxdat[1,]<-t.tot.max
  m.maxdat[2,]<-tsde.e100.max
  m.maxdat[3,]<-tmcd.e100.max
  m.maxdat[4,]<-tcl.e100.max
  
  f.maxdat<-cbind(m.powdat,m.maxdat)
  
  colnames(f.maxdat)<-c("p80% LCI","p80% est","p80% UCI","T-I LCI","T-I est.","T-I UCI")
  #rownames(f.maxdat)<-c("CI -5%","e100.SDe","e100.MCD","e100.Cl")
  f.maxdat<-cbind(Method=c("CI5%","JE.SDe1%","JE.MCD1%","JE.Clas1%"),f.maxdat)
  return(f.maxdat)
  
}

f.powintlongQuad<-function(res,col1="m.tot",col2="mmcd.e100"){
  
  f.tot<-fit.int(res,"intercept",col1,sigma.st=1,scale=1)
  fmcd.e100<-fit.int(res,"intercept",col2,sigma.st=1,scale=1)
  
  t.tot.max<-approx.intcenter(f.tot)
  tmcd.e100.max<-approx.intcenter(fmcd.e100)
  
  t.tot.p20<-pow.intestim(f.tot)
  tmcd.e100.p20<-pow.intestim(fmcd.e100)
  
  m.powdat<-data.frame(NA,ncol=3,nrow=2)
  m.powdat[1,]<-t.tot.p20
  m.powdat[2,]<-tmcd.e100.p20
  
  m.maxdat<-data.frame(NA,ncol=3,nrow=2)
  m.maxdat[1,]<-t.tot.max
  m.maxdat[2,]<-tmcd.e100.max
  
  f.maxdat<-cbind(m.powdat,m.maxdat)
  
  colnames(f.maxdat)<-c("p80% LCI","p80% est","p80% UCI","T-I LCI","T-I est.","T-I UCI")
  rownames(f.maxdat)<-c("CI -5%","e100.MCD")
  return(f.maxdat)
  
}


f.powdath<-function(res){
  f.ptot<-fit.nls(res,"slope","p.tot",sigma.st=0.03)
  f.pe100<-fit.nls(res,"slope","p.e100",sigma.st=0.02)
  f.dtot<-fit.nls(res,"slope","d.tot",sigma.st=0.03)
  f.de100<-fit.nls(res,"slope","d.e100",sigma.st=0.02)
  f.mtot<-fit.nls(res,"slope","m.tot",sigma.st=0.03)
  f.me100<-fit.nls(res,"slope","m.e100",sigma.st=0.02)
  f.mmtot<-fit.nls(res,"slope","mm.tot",sigma.st=0.03)
  f.mme100<-fit.nls(res,"slope","mm.e100",sigma.st=0.02)
  f.wtot<-fit.nls(res,"slope","w.tot",sigma.st=0.03)
  f.we100<-fit.nls(res,"slope","w.e100",sigma.st=0.02)

  
  t.ptot.max<-approx.center(f.ptot)
  t.pe100.max<-approx.center(f.pe100)
  t.dtot.max<-approx.center(f.dtot)
  t.de100.max<-approx.center(f.de100)
  t.mtot.max<-approx.center(f.mtot)
  t.me100.max<-approx.center(f.me100)
  t.mmtot.max<-approx.center(f.mmtot)
  t.mme100.max<-approx.center(f.mme100)
  t.wtot.max<-approx.center(f.wtot)
  t.we100.max<-approx.center(f.we100)
  
  t.ptot.p20<-pow.estim(f.ptot)
  t.pe100.p20<-pow.estim(f.pe100)
  t.dtot.p20<-pow.estim(f.dtot)
  t.de100.p20<-pow.estim(f.de100)
  t.mtot.p20<-pow.estim(f.mtot)
  t.me100.p20<-pow.estim(f.me100)
  t.mmtot.p20<-pow.estim(f.mmtot)
  t.mme100.p20<-pow.estim(f.mme100)
  t.wtot.p20<-pow.estim(f.wtot)
  t.we100.p20<-pow.estim(f.we100)
  
  m.powdat<-data.frame(NA,ncol=3,nrow=10)
  m.powdat[1,]<-t.ptot.p20
  m.powdat[2,]<-t.pe100.p20
  m.powdat[3,]<-t.dtot.p20
  m.powdat[4,]<-t.de100.p20
  m.powdat[5,]<-t.mtot.p20
  m.powdat[6,]<-t.me100.p20
  m.powdat[7,]<-t.mmtot.p20
  m.powdat[8,]<-t.mme100.p20
  m.powdat[9,]<-t.wtot.p20
  m.powdat[10,]<-t.we100.p20
  
  m.maxdat<-data.frame(NA,ncol=3,nrow=10)
  m.maxdat[1,]<-t.ptot.max
  m.maxdat[2,]<-t.pe100.max
  m.maxdat[3,]<-t.dtot.max
  m.maxdat[4,]<-t.de100.max
  m.maxdat[5,]<-t.mtot.max
  m.maxdat[6,]<-t.me100.max
  m.maxdat[7,]<-t.mmtot.max
  m.maxdat[8,]<-t.mme100.max
  m.maxdat[9,]<-t.wtot.max
  m.maxdat[10,]<-t.we100.max
  
  f.maxdat<-cbind(m.powdat,m.maxdat)
  
  colnames(f.maxdat)<-c("p. 80% LCI","p. 80% est","p. 80% UCI","T I err LCI","T I err est.","T I err UCI")
  rownames(f.maxdat)<-c("CI - Paba","e100 PaBa",
                        "CI Dem","e100 Dem",
                        "CI MDem","e100 MDem",
                        "CI MMDem","e100 MMDem",
                        "CI WDem", "e100 WDem")
  return(f.maxdat)
  
}

f.powdathSec<-function(res){
  f.ptot<-fit.nls(res,"slope","p.tot",sigma.st=0.03)
  f.pe100<-fit.nls(res,"slope","pr.e100",sigma.st=0.02)
  f.dtot<-fit.nls(res,"slope","d.tot",sigma.st=0.03)
  f.de100<-fit.nls(res,"slope","dr.e100",sigma.st=0.02)
  f.mtot<-fit.nls(res,"slope","m.tot",sigma.st=0.03)
  f.me100<-fit.nls(res,"slope","mr.e100",sigma.st=0.02)
  f.mmtot<-fit.nls(res,"slope","mm.tot",sigma.st=0.03)
  f.mme100<-fit.nls(res,"slope","mmr.e100",sigma.st=0.02)
  f.wtot<-fit.nls(res,"slope","w.tot",sigma.st=0.03)
  f.we100<-fit.nls(res,"slope","wr.e100",sigma.st=0.02)
  
  
  t.ptot.max<-approx.center(f.ptot)
  t.pe100.max<-approx.center(f.pe100)
  t.dtot.max<-approx.center(f.dtot)
  t.de100.max<-approx.center(f.de100)
  t.mtot.max<-approx.center(f.mtot)
  t.me100.max<-approx.center(f.me100)
  t.mmtot.max<-approx.center(f.mmtot)
  t.mme100.max<-approx.center(f.mme100)
  t.wtot.max<-approx.center(f.wtot)
  t.we100.max<-approx.center(f.we100)
  
  t.ptot.p20<-pow.estim(f.ptot)
  t.pe100.p20<-pow.estim(f.pe100)
  t.dtot.p20<-pow.estim(f.dtot)
  t.de100.p20<-pow.estim(f.de100)
  t.mtot.p20<-pow.estim(f.mtot)
  t.me100.p20<-pow.estim(f.me100)
  t.mmtot.p20<-pow.estim(f.mmtot)
  t.mme100.p20<-pow.estim(f.mme100)
  t.wtot.p20<-pow.estim(f.wtot)
  t.we100.p20<-pow.estim(f.we100)
  
  m.powdat<-data.frame(NA,ncol=3,nrow=10)
  m.powdat[1,]<-t.ptot.p20
  m.powdat[2,]<-t.pe100.p20
  m.powdat[3,]<-t.dtot.p20
  m.powdat[4,]<-t.de100.p20
  m.powdat[5,]<-t.mtot.p20
  m.powdat[6,]<-t.me100.p20
  m.powdat[7,]<-t.mmtot.p20
  m.powdat[8,]<-t.mme100.p20
  m.powdat[9,]<-t.wtot.p20
  m.powdat[10,]<-t.we100.p20
  
  m.maxdat<-data.frame(NA,ncol=3,nrow=10)
  m.maxdat[1,]<-t.ptot.max
  m.maxdat[2,]<-t.pe100.max
  m.maxdat[3,]<-t.dtot.max
  m.maxdat[4,]<-t.de100.max
  m.maxdat[5,]<-t.mtot.max
  m.maxdat[6,]<-t.me100.max
  m.maxdat[7,]<-t.mmtot.max
  m.maxdat[8,]<-t.mme100.max
  m.maxdat[9,]<-t.wtot.max
  m.maxdat[10,]<-t.we100.max
  
  f.maxdat<-cbind(m.powdat,m.maxdat)
  
  colnames(f.maxdat)<-c("p. 80% LCI","p. 80% est","p. 80% UCI","T I err LCI","T I err est.","T I err UCI")
  rownames(f.maxdat)<-c("CI - Paba","e100 PaBa",
                        "CI Dem","e100 Dem",
                        "CI MDem","e100 MDem",
                        "CI MMDem","e100 MMDem",
                        "CI WDem", "e100 WDem")
  return(f.maxdat)
  
}

plotBoxEllipses<-function(.Object, robust.cov = T,...)
{
  par(mfrow=c(1,1))
  if(robust.cov==T){
    t.mcd<-rrcov::CovRobust(cbind(.Object@B0,.Object@B1))
    text.label<-"robust SDe"
  }else{
    t.mcd<-rrcov::CovClassic(cbind(.Object@B0,.Object@B1))
    text.label<-"classical"
  }
  
  t.md<-mahalanobis(c(0,1),center=t.mcd$center,cov=t.mcd$cov)
  res.p<-pchisq(t.md,df=2,lower.tail=F)
  names(res.p)<-"Chisq global p-value"
  plot.default(.Object@B0,.Object@B1,xlab="intercept",ylab="slope",pch=16,col=rgb(0,0,0,alpha=0.2),...)
  points(0,1,pch=4,cex=2,col="red",lwd=3)
  grid()
  res.cx<-.Object@para[1,1:4]
  res.cy<-.Object@para[2,1:4]
  rect(res.cx[3],res.cy[3],res.cx[4],res.cy[4],
       border="purple",lty=3,lwd=2)
  
  points(res.cx[1],res.cy[1],col="purple",pch=3,cex=2,lwd=3)
  
  #mixtools::ellipse(mu=t.mcd$center,sigma=t.mcd$cov,alpha=0.05,
                    #npoints=250,newplot=FALSE,
                    #draw=TRUE, col="blue",lwd=1,lty=2)
  
  #mixtools::ellipse(mu=t.mcd$center,sigma=t.mcd$cov,alpha=0.01,
                    #npoints=250,newplot=FALSE,
                    #draw=TRUE, col="blue",lwd=1,lty=1)
  
  points(t.mcd$center[1],t.mcd$center[2],col="blue",pch=8,cex=2,lwd=3)
  
  mtext(paste("Chi sq. p-value with 2 d.f.: ",signif(res.p,4)),side=1, line=-2,adj=0.1,font=1)
  
  legend("topright",legend=c("JE5%","JE1%","CI5%"),title="Alpha val.",
         lty=c(2,1,3),lwd=c(1,1,2),col=c("blue","blue","purple"))
  
  title(paste("Box & ellipses (",text.label,"covariance ) of the",.Object@regmeth,"bootstrapped samples estimates"))
  
  
}

grPPcompFs<-function(dat40,dat100,xmax=1){
  #data prep
  y.varslt40<-p.ck(dat40,x.vert)
  #str(y.varsl)
  colnames(y.varslt40)<-c("paba.mcd","paba.sde","paba.cl",
                        "deming.mcd","deming.sde","deming.cl",
                        "wdeming.mcd","wdeming.sde","wdeming.cl",
                        "mdeming.mcd","mdeming.sde","mdeming.cl",
                        "mmdeming.mcd","mmdeming.sde","mmdeming.cl")
  rej.tablt40<-data.frame(cbind(x.vert,y.varslt40))
  
  y.varslt100<-p.ck(dat100,x.vert)
  #str(y.varsl)
  colnames(y.varslt100)<-c("paba.mcd","paba.sde","paba.cl",
                        "deming.mcd","deming.sde","deming.cl",
                        "wdeming.mcd","wdeming.sde","wdeming.cl",
                        "mdeming.mcd","mdeming.sde","mdeming.cl",
                        "mmdeming.mcd","mmdeming.sde","mmdeming.cl")
  rej.tablt100<-data.frame(cbind(x.vert,y.varslt100))
  
#PaBa
  par(mfrow=c(5,2))
  plot(rej.tablt40$x.vert, 1-rej.tablt40$paba.mcd,type="l",
       main="PaBa-short range (40 sample)",
       xlab="Theor. alpha error",
       ylab="Empyrical type I error",
       xlim=c(0,xmax),
       ylim=c(0,xmax),
       col=1)
  
  lines(rej.tablt40$x.vert, 1-rej.tablt40$paba.sde,col=1,lty=2)
  lines(rej.tablt40$x.vert, 1-rej.tablt40$paba.cl,col=1,lty=3)
  legend("bottomright",legend=c("PaBa.MCD","PaBa.SDe","PaBa.Clas"),
         lty=1:3)
  grid()
  abline(a=0,b=1,col="red",lty=2,lwd=2)
  abline(h=0.05,col="red",lty=3)
  abline(v=0.01,col="red",lty=3)
  
  plot(rej.tablt100$x.vert, 1-rej.tablt100$paba.mcd,type="l",
       main="PaBa-short range (100 sample)",
       xlab="Theor. alpha error",
       ylab="Empyrical type I error",
       xlim=c(0,xmax),
       ylim=c(0,xmax),
       col=1)
  
  lines(rej.tablt100$x.vert, 1-rej.tablt100$paba.sde,col=1,lty=2)
  lines(rej.tablt100$x.vert, 1-rej.tablt100$paba.cl,col=1,lty=3)
  legend("bottomright",legend=c("PaBa.MCD","PaBa.SDe","PaBa.Clas"),
         lty=1:3)
  grid()
  abline(a=0,b=1,col="red",lty=2,lwd=2)
  abline(h=0.05,col="red",lty=3)
  abline(v=0.01,col="red",lty=3)
  
  #Dem
  
  plot(rej.tablt40$x.vert, 1-rej.tablt40$deming.mcd,type="l",
       main="Dem-short range (40 sample)",
       xlab="Theor. alpha error",
       ylab="Empyrical type I error",
       xlim=c(0,xmax),
       ylim=c(0,xmax),
       col=1)
  
  lines(rej.tablt40$x.vert, 1-rej.tablt40$deming.sde,col=1,lty=2)
  lines(rej.tablt40$x.vert, 1-rej.tablt40$deming.cl,col=1,lty=3)
  legend("bottomright",legend=c("Dem.MCD","Dem.SDe","Dem.Clas"),
         lty=1:3)
  grid()
  abline(a=0,b=1,col="red",lty=2,lwd=2)
  abline(h=0.05,col="red",lty=3)
  abline(v=0.01,col="red",lty=3)
  
  plot(rej.tablt100$x.vert, 1-rej.tablt100$deming.mcd,type="l",
       main="Dem - short range (100 sample)",
       xlab="Theor. alpha error",
       ylab="Empyrical type I error",
       xlim=c(0,xmax),
       ylim=c(0,xmax),
       col=1)
  
  lines(rej.tablt100$x.vert, 1-rej.tablt100$deming.sde,col=1,lty=2)
  lines(rej.tablt100$x.vert, 1-rej.tablt100$deming.cl,col=1,lty=3)
  legend("bottomright",legend=c("Dem.MCD","Dem.SDe","Dem.Clas"),
         lty=1:3)
  grid()
  abline(a=0,b=1,col="red",lty=2,lwd=2)
  abline(h=0.05,col="red",lty=3)
  abline(v=0.01,col="red",lty=3)
  
  #MDem
  
  plot(rej.tablt40$x.vert, 1-rej.tablt40$mdeming.mcd,type="l",
       main="MDem-short range (40 sample)",
       xlab="Theor. alpha error",
       ylab="Empyrical type I error",
       xlim=c(0,xmax),
       ylim=c(0,xmax),
       col=1)
  lines(rej.tablt40$x.vert, 1-rej.tablt40$mdeming.sde,col=1,lty=2)
  lines(rej.tablt40$x.vert, 1-rej.tablt40$mdeming.cl,col=1,lty=3)
  legend("bottomright",legend=c("MDem.MCD","MDem.SDe","MDem.Clas"),
         lty=1:3)
  grid()
  abline(a=0,b=1,col="red",lty=2,lwd=2)
  abline(h=0.05,col="red",lty=3)
  abline(v=0.01,col="red",lty=3)
  
  plot(rej.tablt100$x.vert, 1-rej.tablt100$mdeming.mcd,type="l",
       main="MDem - short range (40 sample)",
       xlab="Theor. alpha error",
       ylab="Empyrical type I error",
       xlim=c(0,xmax),
       ylim=c(0,xmax),
       col=1)
  lines(rej.tablt100$x.vert, 1-rej.tablt100$mdeming.sde,col=1,lty=2)
  lines(rej.tablt100$x.vert, 1-rej.tablt100$mdeming.cl,col=1,lty=3)
  legend("bottomright",legend=c("MDem.MCD","MDem.SDe","MDem.Clas"),
         lty=1:3)
  grid()
  abline(a=0,b=1,col="red",lty=2,lwd=2)
  abline(h=0.05,col="red",lty=3)
  abline(v=0.01,col="red",lty=3)
  
  #MMDem
  plot(rej.tablt40$x.vert, 1-rej.tablt40$mmdeming.mcd,type="l",
       main="MMDem - short range (40 sample)",
       xlab="Theor. alpha error",
       ylab="Empyrical type I error",
       xlim=c(0,xmax),
       ylim=c(0,xmax),
       col=1)
  lines(rej.tablt40$x.vert, 1-rej.tablt40$mmdeming.sde,col=1,lty=2)
  lines(rej.tablt40$x.vert, 1-rej.tablt40$mmdeming.cl,col=1,lty=3)
  legend("bottomright",legend=c("MMDem.MCD","MMDem.SDe","MMDem.Clas"),
         lty=1:3)
  grid()
  abline(a=0,b=1,col="red",lty=2,lwd=2)
  abline(h=0.05,col="red",lty=3)
  abline(v=0.01,col="red",lty=3)
  
  plot(rej.tablt100$x.vert, 1-rej.tablt100$mmdeming.mcd,type="l",
       main="MMDem-short range (100 sample)",
       xlab="Theor. alpha error",
       ylab="Empyrical type I error",
       xlim=c(0,xmax),
       ylim=c(0,xmax),
       col=1)
  lines(rej.tablt100$x.vert, 1-rej.tablt100$mmdeming.sde,col=1,lty=2)
  lines(rej.tablt100$x.vert, 1-rej.tablt100$mmdeming.cl,col=1,lty=3)
  legend("bottomright",legend=c("MMDem.MCD","MMDem.SDe","MMDem.Clas"),
         lty=1:3)
  grid()
  abline(a=0,b=1,col="red",lty=2,lwd=2)
  abline(h=0.05,col="red",lty=3)
  abline(v=0.01,col="red",lty=3)
  
  #WDem
  
  plot(rej.tablt40$x.vert, 1-rej.tablt40$wdeming.mcd,type="l",
       main="WDem-short range (40 sample)",
       xlab="Theor. alpha error",
       ylab="Empyrical type I error",
       xlim=c(0,xmax),
       ylim=c(0,xmax),
       col=1)
  lines(rej.tablt40$x.vert, 1-rej.tablt40$wdeming.sde,col=1,lty=2)
  lines(rej.tablt40$x.vert, 1-rej.tablt40$wdeming.cl,col=1,lty=3)
  legend("bottomright",legend=c("WDem.MCD","WDem.SDe","WDem.Clas"),
         lty=1:3)
  grid()
  abline(a=0,b=1,col="red",lty=2,lwd=2)
  abline(h=0.05,col="red",lty=3)
  abline(v=0.01,col="red",lty=3)
  
  plot(rej.tablt100$x.vert, 1-rej.tablt100$wdeming.mcd,type="l",
       main="WDem - short range (100 sample)",
       xlab="Theor. alpha error",
       ylab="Empyrical type I error",
       xlim=c(0,xmax),
       ylim=c(0,xmax),
       col=1)
  lines(rej.tablt100$x.vert, 1-rej.tablt100$wdeming.sde,col=1,lty=2)
  lines(rej.tablt100$x.vert, 1-rej.tablt100$wdeming.cl,col=1,lty=3)
  legend("bottomright",legend=c("WDem.MCD","WDem.SDe","WDem.Clas"),
         lty=1:3)
  grid()
  abline(a=0,b=1,col="red",lty=2,lwd=2)
  abline(h=0.05,col="red",lty=3)
  abline(v=0.01,col="red",lty=3)
  
  par(mfrow=c(1,1))
  
}

grPPcompFl<-function(dat40,dat100,xmax=1){
  #data prep
  y.varslt40<-p.ck(dat40,x.vert)
  #str(y.varsl)
  colnames(y.varslt40)<-c("paba.mcd","paba.sde","paba.cl",
                          "deming.mcd","deming.sde","deming.cl",
                          "wdeming.mcd","wdeming.sde","wdeming.cl",
                          "mdeming.mcd","mdeming.sde","mdeming.cl",
                          "mmdeming.mcd","mmdeming.sde","mmdeming.cl")
  rej.tablt40<-data.frame(cbind(x.vert,y.varslt40))
  
  y.varslt100<-p.ck(dat100,x.vert)
  #str(y.varsl)
  colnames(y.varslt100)<-c("paba.mcd","paba.sde","paba.cl",
                           "deming.mcd","deming.sde","deming.cl",
                           "wdeming.mcd","wdeming.sde","wdeming.cl",
                           "mdeming.mcd","mdeming.sde","mdeming.cl",
                           "mmdeming.mcd","mmdeming.sde","mmdeming.cl")
  rej.tablt100<-data.frame(cbind(x.vert,y.varslt100))
  
  #PaBa
  par(mfrow=c(5,2))
  plot(rej.tablt40$x.vert, 1-rej.tablt40$paba.mcd,type="l",
       main="PaBa-long range (40 sample)",
       xlab="Theor. alpha error",
       ylab="Empyrical type I error",
       xlim=c(0,xmax),
       ylim=c(0,xmax),
       col=1)
  
  lines(rej.tablt40$x.vert, 1-rej.tablt40$paba.sde,col=1,lty=2)
  lines(rej.tablt40$x.vert, 1-rej.tablt40$paba.cl,col=1,lty=3)
  legend("bottomright",legend=c("PaBa.MCD","PaBa.SDe","PaBa.Clas"),
         lty=1:3)
  grid()
  abline(a=0,b=1,col="red",lty=2,lwd=2)
  abline(h=0.05,col="red",lty=3)
  abline(v=0.01,col="red",lty=3)
  
  plot(rej.tablt100$x.vert, 1-rej.tablt100$paba.mcd,type="l",
       main="PaBa-long range (100 sample)",
       xlab="Theor. alpha error",
       ylab="Empyrical type I error",
       xlim=c(0,xmax),
       ylim=c(0,xmax),
       col=1)
  
  lines(rej.tablt100$x.vert, 1-rej.tablt100$paba.sde,col=1,lty=2)
  lines(rej.tablt100$x.vert, 1-rej.tablt100$paba.cl,col=1,lty=3)
  legend("bottomright",legend=c("PaBa.MCD","PaBa.SDe","PaBa.Clas"),
         lty=1:3)
  grid()
  abline(a=0,b=1,col="red",lty=2,lwd=2)
  abline(h=0.05,col="red",lty=3)
  abline(v=0.01,col="red",lty=3)
  
  #Dem
  
  plot(rej.tablt40$x.vert, 1-rej.tablt40$deming.mcd,type="l",
       main="Dem-long range (40 sample)",
       xlab="Theor. alpha error",
       ylab="Empyrical type I error",
       xlim=c(0,xmax),
       ylim=c(0,xmax),
       col=1)
  
  lines(rej.tablt40$x.vert, 1-rej.tablt40$deming.sde,col=1,lty=2)
  lines(rej.tablt40$x.vert, 1-rej.tablt40$deming.cl,col=1,lty=3)
  legend("bottomright",legend=c("Dem.MCD","Dem.SDe","Dem.Clas"),
         lty=1:3)
  grid()
  abline(a=0,b=1,col="red",lty=2,lwd=2)
  abline(h=0.05,col="red",lty=3)
  abline(v=0.01,col="red",lty=3)
  
  plot(rej.tablt100$x.vert, 1-rej.tablt100$deming.mcd,type="l",
       main="Dem - long range (100 sample)",
       xlab="Theor. alpha error",
       ylab="Empyrical type I error",
       xlim=c(0,xmax),
       ylim=c(0,xmax),
       col=1)
  
  lines(rej.tablt100$x.vert, 1-rej.tablt100$deming.sde,col=1,lty=2)
  lines(rej.tablt100$x.vert, 1-rej.tablt100$deming.cl,col=1,lty=3)
  legend("bottomright",legend=c("Dem.MCD","Dem.SDe","Dem.Clas"),
         lty=1:3)
  grid()
  abline(a=0,b=1,col="red",lty=2,lwd=2)
  abline(h=0.05,col="red",lty=3)
  abline(v=0.01,col="red",lty=3)
  
  #MDem
  
  plot(rej.tablt40$x.vert, 1-rej.tablt40$mdeming.mcd,type="l",
       main="MDem-long range (40 sample)",
       xlab="Theor. alpha error",
       ylab="Empyrical type I error",
       xlim=c(0,xmax),
       ylim=c(0,xmax),
       col=1)
  lines(rej.tablt40$x.vert, 1-rej.tablt40$mdeming.sde,col=1,lty=2)
  lines(rej.tablt40$x.vert, 1-rej.tablt40$mdeming.cl,col=1,lty=3)
  legend("bottomright",legend=c("MDem.MCD","MDem.SDe","MDem.Clas"),
         lty=1:3)
  grid()
  abline(a=0,b=1,col="red",lty=2,lwd=2)
  abline(h=0.05,col="red",lty=3)
  abline(v=0.01,col="red",lty=3)
  
  plot(rej.tablt100$x.vert, 1-rej.tablt100$mdeming.mcd,type="l",
       main="MDem - long range (40 sample)",
       xlab="Theor. alpha error",
       ylab="Empyrical type I error",
       xlim=c(0,xmax),
       ylim=c(0,xmax),
       col=1)
  lines(rej.tablt100$x.vert, 1-rej.tablt100$mdeming.sde,col=1,lty=2)
  lines(rej.tablt100$x.vert, 1-rej.tablt100$mdeming.cl,col=1,lty=3)
  legend("bottomright",legend=c("MDem.MCD","MDem.SDe","MDem.Clas"),
         lty=1:3)
  grid()
  abline(a=0,b=1,col="red",lty=2,lwd=2)
  abline(h=0.05,col="red",lty=3)
  abline(v=0.01,col="red",lty=3)
  
  #MMDem
  plot(rej.tablt40$x.vert, 1-rej.tablt40$mmdeming.mcd,type="l",
       main="MMDem - long range (40 sample)",
       xlab="Theor. alpha error",
       ylab="Empyrical type I error",
       xlim=c(0,xmax),
       ylim=c(0,xmax),
       col=1)
  lines(rej.tablt40$x.vert, 1-rej.tablt40$mmdeming.sde,col=1,lty=2)
  lines(rej.tablt40$x.vert, 1-rej.tablt40$mmdeming.cl,col=1,lty=3)
  legend("bottomright",legend=c("MMDem.MCD","MMDem.SDe","MMDem.Clas"),
         lty=1:3)
  grid()
  abline(a=0,b=1,col="red",lty=2,lwd=2)
  abline(h=0.05,col="red",lty=3)
  abline(v=0.01,col="red",lty=3)
  
  plot(rej.tablt100$x.vert, 1-rej.tablt100$mmdeming.mcd,type="l",
       main="MMDem-long range (100 sample)",
       xlab="Theor. alpha error",
       ylab="Empyrical type I error",
       xlim=c(0,xmax),
       ylim=c(0,xmax),
       col=1)
  lines(rej.tablt100$x.vert, 1-rej.tablt100$mmdeming.sde,col=1,lty=2)
  lines(rej.tablt100$x.vert, 1-rej.tablt100$mmdeming.cl,col=1,lty=3)
  legend("bottomright",legend=c("MMDem.MCD","MMDem.SDe","MMDem.Clas"),
         lty=1:3)
  grid()
  abline(a=0,b=1,col="red",lty=2,lwd=2)
  abline(h=0.05,col="red",lty=3)
  abline(v=0.01,col="red",lty=3)
  
  #WDem
  
  plot(rej.tablt40$x.vert, 1-rej.tablt40$wdeming.mcd,type="l",
       main="WDem-long range (40 sample)",
       xlab="Theor. alpha error",
       ylab="Empyrical type I error",
       xlim=c(0,xmax),
       ylim=c(0,xmax),
       col=1)
  lines(rej.tablt40$x.vert, 1-rej.tablt40$wdeming.sde,col=1,lty=2)
  lines(rej.tablt40$x.vert, 1-rej.tablt40$wdeming.cl,col=1,lty=3)
  legend("bottomright",legend=c("WDem.MCD","WDem.SDe","WDem.Clas"),
         lty=1:3)
  grid()
  abline(a=0,b=1,col="red",lty=2,lwd=2)
  abline(h=0.05,col="red",lty=3)
  abline(v=0.01,col="red",lty=3)
  
  plot(rej.tablt100$x.vert, 1-rej.tablt100$wdeming.mcd,type="l",
       main="WDem - long range (100 sample)",
       xlab="Theor. alpha error",
       ylab="Empyrical type I error",
       xlim=c(0,xmax),
       ylim=c(0,xmax),
       col=1)
  lines(rej.tablt100$x.vert, 1-rej.tablt100$wdeming.sde,col=1,lty=2)
  lines(rej.tablt100$x.vert, 1-rej.tablt100$wdeming.cl,col=1,lty=3)
  legend("bottomright",legend=c("WDem.MCD","WDem.SDe","WDem.Clas"),
         lty=1:3)
  grid()
  abline(a=0,b=1,col="red",lty=2,lwd=2)
  abline(h=0.05,col="red",lty=3)
  abline(v=0.01,col="red",lty=3)
  
  par(mfrow=c(1,1))
  
}


grPPcompFsl<-function(dat40,dat100,xmax=1){
  #data prep
  y.varslt40<-p.ck(dat40,x.vert)
  #str(y.varsl)
  colnames(y.varslt40)<-c("paba.mcd","paba.sde","paba.cl",
                          "deming.mcd","deming.sde","deming.cl",
                          "wdeming.mcd","wdeming.sde","wdeming.cl",
                          "mdeming.mcd","mdeming.sde","mdeming.cl",
                          "mmdeming.mcd","mmdeming.sde","mmdeming.cl")
  rej.tablt40<-data.frame(cbind(x.vert,y.varslt40))
  
  y.varslt100<-p.ck(dat100,x.vert)
  #str(y.varsl)
  colnames(y.varslt100)<-c("paba.mcd","paba.sde","paba.cl",
                           "deming.mcd","deming.sde","deming.cl",
                           "wdeming.mcd","wdeming.sde","wdeming.cl",
                           "mdeming.mcd","mdeming.sde","mdeming.cl",
                           "mmdeming.mcd","mmdeming.sde","mmdeming.cl")
  rej.tablt100<-data.frame(cbind(x.vert,y.varslt100))
  
  #PaBa
  par(mfrow=c(5,2))
  plot(rej.tablt40$x.vert, 1-rej.tablt40$paba.mcd,type="l",
       main="PaBa-short range (40 sample)",
       xlab="Theor. alpha error",
       ylab="Empyrical type I error",
       xlim=c(0,xmax),
       ylim=c(0,xmax),
       col=1)
  
  lines(rej.tablt40$x.vert, 1-rej.tablt40$paba.sde,col=1,lty=2)
  lines(rej.tablt40$x.vert, 1-rej.tablt40$paba.cl,col=1,lty=3)
  legend("bottomright",legend=c("PaBa.MCD","PaBa.SDe","PaBa.Clas"),
         lty=1:3)
  grid()
  abline(a=0,b=1,col="red",lty=2,lwd=2)
  abline(h=0.05,col="red",lty=3)
  abline(v=0.01,col="red",lty=3)
  
  plot(rej.tablt100$x.vert, 1-rej.tablt100$paba.mcd,type="l",
       main="PaBa-long range (40 sample)",
       xlab="Theor. alpha error",
       ylab="Empyrical type I error",
       xlim=c(0,xmax),
       ylim=c(0,xmax),
       col=1)
  
  lines(rej.tablt100$x.vert, 1-rej.tablt100$paba.sde,col=1,lty=2)
  lines(rej.tablt100$x.vert, 1-rej.tablt100$paba.cl,col=1,lty=3)
  legend("bottomright",legend=c("PaBa.MCD","PaBa.SDe","PaBa.Clas"),
         lty=1:3)
  grid()
  abline(a=0,b=1,col="red",lty=2,lwd=2)
  abline(h=0.05,col="red",lty=3)
  abline(v=0.01,col="red",lty=3)
  
  #Dem
  
  plot(rej.tablt40$x.vert, 1-rej.tablt40$deming.mcd,type="l",
       main="Dem-short range (40 sample)",
       xlab="Theor. alpha error",
       ylab="Empyrical type I error",
       xlim=c(0,xmax),
       ylim=c(0,xmax),
       col=1)
  
  lines(rej.tablt40$x.vert, 1-rej.tablt40$deming.sde,col=1,lty=2)
  lines(rej.tablt40$x.vert, 1-rej.tablt40$deming.cl,col=1,lty=3)
  legend("bottomright",legend=c("Dem.MCD","Dem.SDe","Dem.Clas"),
         lty=1:3)
  grid()
  abline(a=0,b=1,col="red",lty=2,lwd=2)
  abline(h=0.05,col="red",lty=3)
  abline(v=0.01,col="red",lty=3)
  
  plot(rej.tablt100$x.vert, 1-rej.tablt100$deming.mcd,type="l",
       main="Dem - long range (40 sample)",
       xlab="Theor. alpha error",
       ylab="Empyrical type I error",
       xlim=c(0,xmax),
       ylim=c(0,xmax),
       col=1)
  
  lines(rej.tablt100$x.vert, 1-rej.tablt100$deming.sde,col=1,lty=2)
  lines(rej.tablt100$x.vert, 1-rej.tablt100$deming.cl,col=1,lty=3)
  legend("bottomright",legend=c("Dem.MCD","Dem.SDe","Dem.Clas"),
         lty=1:3)
  grid()
  abline(a=0,b=1,col="red",lty=2,lwd=2)
  abline(h=0.05,col="red",lty=3)
  abline(v=0.01,col="red",lty=3)
  
  #MDem
  
  plot(rej.tablt40$x.vert, 1-rej.tablt40$mdeming.mcd,type="l",
       main="MDem-short range (40 sample)",
       xlab="Theor. alpha error",
       ylab="Empyrical type I error",
       xlim=c(0,xmax),
       ylim=c(0,xmax),
       col=1)
  lines(rej.tablt40$x.vert, 1-rej.tablt40$mdeming.sde,col=1,lty=2)
  lines(rej.tablt40$x.vert, 1-rej.tablt40$mdeming.cl,col=1,lty=3)
  legend("bottomright",legend=c("MDem.MCD","MDem.SDe","MDem.Clas"),
         lty=1:3)
  grid()
  abline(a=0,b=1,col="red",lty=2,lwd=2)
  abline(h=0.05,col="red",lty=3)
  abline(v=0.01,col="red",lty=3)
  
  plot(rej.tablt100$x.vert, 1-rej.tablt100$mdeming.mcd,type="l",
       main="MDem - long range (40 sample)",
       xlab="Theor. alpha error",
       ylab="Empyrical type I error",
       xlim=c(0,xmax),
       ylim=c(0,xmax),
       col=1)
  lines(rej.tablt100$x.vert, 1-rej.tablt100$mdeming.sde,col=1,lty=2)
  lines(rej.tablt100$x.vert, 1-rej.tablt100$mdeming.cl,col=1,lty=3)
  legend("bottomright",legend=c("MDem.MCD","MDem.SDe","MDem.Clas"),
         lty=1:3)
  grid()
  abline(a=0,b=1,col="red",lty=2,lwd=2)
  abline(h=0.05,col="red",lty=3)
  abline(v=0.01,col="red",lty=3)
  
  #MMDem
  plot(rej.tablt40$x.vert, 1-rej.tablt40$mmdeming.mcd,type="l",
       main="MMDem - short range (40 sample)",
       xlab="Theor. alpha error",
       ylab="Empyrical type I error",
       xlim=c(0,xmax),
       ylim=c(0,xmax),
       col=1)
  lines(rej.tablt40$x.vert, 1-rej.tablt40$mmdeming.sde,col=1,lty=2)
  lines(rej.tablt40$x.vert, 1-rej.tablt40$mmdeming.cl,col=1,lty=3)
  legend("bottomright",legend=c("MMDem.MCD","MMDem.SDe","MMDem.Clas"),
         lty=1:3)
  grid()
  abline(a=0,b=1,col="red",lty=2,lwd=2)
  abline(h=0.05,col="red",lty=3)
  abline(v=0.01,col="red",lty=3)
  
  plot(rej.tablt100$x.vert, 1-rej.tablt100$mmdeming.mcd,type="l",
       main="MMDem-long range (40 sample)",
       xlab="Theor. alpha error",
       ylab="Empyrical type I error",
       xlim=c(0,xmax),
       ylim=c(0,xmax),
       col=1)
  lines(rej.tablt100$x.vert, 1-rej.tablt100$mmdeming.sde,col=1,lty=2)
  lines(rej.tablt100$x.vert, 1-rej.tablt100$mmdeming.cl,col=1,lty=3)
  legend("bottomright",legend=c("MMDem.MCD","MMDem.SDe","MMDem.Clas"),
         lty=1:3)
  grid()
  abline(a=0,b=1,col="red",lty=2,lwd=2)
  abline(h=0.05,col="red",lty=3)
  abline(v=0.01,col="red",lty=3)
  
  #WDem
  
  plot(rej.tablt40$x.vert, 1-rej.tablt40$wdeming.mcd,type="l",
       main="WDem-short range (40 sample)",
       xlab="Theor. alpha error",
       ylab="Empyrical type I error",
       xlim=c(0,xmax),
       ylim=c(0,xmax),
       col=1)
  lines(rej.tablt40$x.vert, 1-rej.tablt40$wdeming.sde,col=1,lty=2)
  lines(rej.tablt40$x.vert, 1-rej.tablt40$wdeming.cl,col=1,lty=3)
  legend("bottomright",legend=c("WDem.MCD","WDem.SDe","WDem.Clas"),
         lty=1:3)
  grid()
  abline(a=0,b=1,col="red",lty=2,lwd=2)
  abline(h=0.05,col="red",lty=3)
  abline(v=0.01,col="red",lty=3)
  
  plot(rej.tablt100$x.vert, 1-rej.tablt100$wdeming.mcd,type="l",
       main="WDem - long range (40 sample)",
       xlab="Theor. alpha error",
       ylab="Empyrical type I error",
       xlim=c(0,xmax),
       ylim=c(0,xmax),
       col=1)
  lines(rej.tablt100$x.vert, 1-rej.tablt100$wdeming.sde,col=1,lty=2)
  lines(rej.tablt100$x.vert, 1-rej.tablt100$wdeming.cl,col=1,lty=3)
  legend("bottomright",legend=c("WDem.MCD","WDem.SDe","WDem.Clas"),
         lty=1:3)
  grid()
  abline(a=0,b=1,col="red",lty=2,lwd=2)
  abline(h=0.05,col="red",lty=3)
  abline(v=0.01,col="red",lty=3)
  
  par(mfrow=c(1,1))
  
}


p.ck<-function(sim.res,x.ver){
  t.y<-matrix(NA,ncol=15,nrow=length(x.ver))
  for(i in 1:length(x.ver)){
    t.y[i,]<-c((mean(sim.res[,7] >= x.ver[i])),(mean(sim.res[,8] >= x.ver[i])),(mean(sim.res[,9] >= x.ver[i])),
               (mean(sim.res[,16] >= x.ver[i])),(mean(sim.res[,17] >= x.ver[i])),(mean(sim.res[,18] >= x.ver[i])),
               (mean(sim.res[,25] >= x.ver[i])),(mean(sim.res[,26] >= x.ver[i])),(mean(sim.res[,27] >= x.ver[i])),
               (mean(sim.res[,34] >= x.ver[i])),(mean(sim.res[,35] >= x.ver[i])),(mean(sim.res[,36] >= x.ver[i])),
               (mean(sim.res[,43] >= x.ver[i])),(mean(sim.res[,44] >= x.ver[i])),(mean(sim.res[,45] >= x.ver[i])))
  }
  return(t.y)
}

