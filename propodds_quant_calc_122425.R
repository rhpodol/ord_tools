library(VGAM)

orm_quantile_calc<-function(gcdf,xvals,ilink=NULL,quant=0.5,outlab="median") {
  if (is.null(ilink)) ilink<-function(yval) exp(yval)/(exp(yval)+1)
  tcdf<-ilink(gcdf)
  tcdfdiff<-quant-tcdf
  if (max(tcdfdiff)>0) {
    ibounds<-which(tcdfdiff%in%c(max(tcdfdiff[tcdfdiff<0]),
                                 min(tcdfdiff[tcdfdiff>0])))
    medhat<-ifelse(length(ibounds)!=2,NA,
                   xvals[ibounds[1]]+(0.5-tcdf[ibounds[1]])/diff(tcdf[ibounds])*diff(xvals[ibounds]))
    names(medhat)<-outlab
  } else { medhat<-min(xvals) }
  return(medhat)
}

po_grpmed_calc<-function(modobj,yvals,lmat) {
  bvec<-coefficients(modobj)
  lest<-t(lmat)%*%bvec
  lest_se<-sqrt(diag(t(lmat)%*%vcov(modobj)%*%lmat))
  lest_lcl<-lest-lest_se*qnorm(0.975)  
  lest_ucl<-lest+lest_se*qnorm(0.975)  
  l_med<-orm_quantile_calc(lest,yvals)
  l_med_lcl<-orm_quantile_calc(lest_lcl,yvals)
  l_med_ucl<-orm_quantile_calc(lest_ucl,yvals)
  return(data.frame(median=l_med,median_lcl=l_med_lcl,median_ucl=l_med_ucl))
}

po_grpmed_calc2<-function(modobj,yvals,lmat) {
  bvec<-coefficients(modobj)
  lest<-t(lmat)%*%bvec
  lcdfest<-exp(lest)/(exp(lest)+1)
  lcdfsd<-sapply(1:length(lest),function(ri) {
      lgvec<-lest[ri]*lmat[,ri] 
      lgvec<-exp(lgvec)/(exp(lgvec)+1)^2
      tcdfsd<-sqrt(t(lgvec)%*%vcov(modobj)%*%lgvec)
      return(tcdfsd)
  })
  l_med<-orm_quantile_calc(lcdfest,yvals,ilink = function(x) x)
  l_med_lcl<-orm_quantile_calc(lcdfest-qnorm(0.975)*lcdfsd,yvals,ilink = function(x) x)
  l_med_ucl<-orm_quantile_calc(lcdfest+qnorm(0.975)*lcdfsd,yvals,ilink = function(x) x)
  return(data.frame(median=l_med,median_lcl=l_med_lcl,median_ucl=l_med_ucl))
}

nb<-2500
nsubj<-c(145,86)
bvec<-c(-1.31,-0.295,1.44,2.25,-0.76)
cptab<-cbind(exp(bvec[1:4])/(exp(bvec[1:4])+1),exp(bvec[1:4]+bvec[5])/(exp(bvec[1:4]+bvec[5])+1))
ptab<-matrix(NA,ncol=2,nrow=5)
ptab[1,]<-cptab[1,]
for (ri in 2:4) ptab[ri,]<-cptab[ri,]-cptab[ri-1,]
ptab[5,]<-1-cptab[4,]
etavec<-c(orm_quantile_calc(bvec[1:4],0:4),orm_quantile_calc(bvec[1:4]+bvec[5],0:4))
  
yvec<-rep(1:5,2)
grp<-factor(rep(1:2,each=5),labels=c("Grp1","Grp2"))
posimdat<-cbind(t(rmultinom(nb,nsubj[1],ptab[,1])),t(rmultinom(nb,nsubj[2],ptab[,2])))

lmat4_istate<-matrix(c(1,0,0,0,0,
                        0,1,0,0,0,
                        0,0,1,0,0,
                        0,0,0,1,0),ncol=4,byrow = F)

lmat4_ostate<-matrix(c(1,0,0,0,1,
                        0,1,0,0,1,
                        0,0,1,0,1,
                        0,0,0,1,1),ncol=4,byrow = F)

posimresult<-apply(posimdat,1,function(simyvec){
  tsmod<-vglm(yvec~grp,cumulative(link = "logitlink",parallel = T),weights=simyvec)
  tdf<-data.frame(po_grpmed_calc(modobj=tsmod,yvals=0:4,lmat=lmat4_istate),
                  po_grpmed_calc(modobj=tsmod,yvals=0:4,lmat=lmat4_ostate))
  names(tdf)<-c("Grp1_med","Grp1_med_ucl","Grp1_med_lcl","Grp2_med","Grp2_med_ucl","Grp2_med_lcl")
  return(tdf)
})
posimresult<-do.call(rbind,posimresult)

table(etavec[1]>=posimresult$Grp1_med_lcl & etavec[1]<=posimresult$Grp1_med_ucl)/nb
table(etavec[2]>=posimresult$Grp2_med_lcl & etavec[2]<=posimresult$Grp2_med_ucl)/nb
