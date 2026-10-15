isColor <- function(x)
{
  res <- try(col2rgb(x),silent=TRUE)
  return(!"try-error"%in%class(res))
}

grpbarplot<-function(datadf,meansdf,yvar,grp,meanvar,lclvar,uclvar,ylab,pointsize=4,barwidth=0.25,
                     ebarwidth=0.25,elinewidth=1.5,datcol="gray30",meancolor="red",barlinewidth=1.5,
                     barcolor="white",barlcol="black",jitterwidth=0.25) {
   lp<-ggplot(datadf,aes(x=!!as.name(grp),y=!!as.name(yvar))) + 
           geom_bar(data=meansdf,aes(y=!!as.name(meanvar),x=!!as.name(grp)),stat = "identity",alpha=0.3,
                    width=barwidth,fill=barcolor,color=barlcol,linewidth=barlinewidth) +
           geom_jitter(size=pointsize,width=jitterwidth,color=datcol) +
           geom_errorbar(data=meansdf,aes(x=!!as.name(grp),y=!!as.name(meanvar),ymin=!!as.name(lclvar),
                                          ymax=!!as.name(uclvar)),
                         color=meancolor,width=ebarwidth,linewidth=elinewidth) +
           scale_color_nejm() +
           ylab(ylab) +
           theme_classic() +
           theme(panel.background = element_blank(),
                 axis.line = element_line(linewidth = 1),
                 axis.title.y = element_text(size=12,face="bold"),
                 axis.title.x = element_blank(),
                 axis.text.y=element_text(size=10,face="bold",color="black"),
                 axis.text.x=element_text(size=10,face="bold",color="black")) 
           
   return(lp)
}

twofactbarplot<-function(datadf,meansdf,yvar,grp,meanvar,lclvar,uclvar,ylab,pointsize=4,barwidth=0.5,ebarwidth=0.25,elinewidth=1.5,dodgewidth=1,meancolor,barlinewidth=1.5,
                     barcolor,barlcol="black",jitterwidth=0.4) {
   lp<-ggplot(datadf,aes(x=!!as.name(grp),y=!!as.name(yvar),fill=!!as.name(barcolor))) + 
           geom_point(size=pointsize,position=position_jitterdodge(dodge.width=dodgewidth,jitter.width = jitterwidth)) +
           geom_col(data=meansdf,aes(y=!!as.name(meanvar),x=!!as.name(grp)),alpha=0.3,width=barwidth,color=barlcol,
                    linewidth=barlinewidth,position=position_dodge(width=dodgewidth)) +
           geom_errorbar(data=meansdf,aes(x=!!as.name(grp),y=!!as.name(meanvar),ymin=!!as.name(lclvar),ymax=!!as.name(uclvar),color=!!as.name(meancolor)),width=ebarwidth,
                         linewidth=elinewidth,position=position_dodge(width=dodgewidth)) +
           scale_color_nejm() + scale_fill_nejm() +
           ylab(ylab) +
           theme_classic() +
           theme(panel.background = element_blank(),
		         legend.title=element_blank(),
                 axis.line = element_line(linewidth = 1),
                 axis.title.y = element_text(size=12,face="bold"),
                 axis.title.x = element_blank(),
                 axis.text.y=element_text(size=10,face="bold",color="black"),
                 axis.text.x=element_text(size=10,face="bold",color="black")) 
           
   return(lp)
}

regplotgen<-function(datadf,fitdf,yvar,xvar,ylab,xlab,grp=NULL,lpos=NULL){
  if (is.null(grp)) {
      lp<- ggplot(data=datadf,aes(x=!!as.name(xvar),y=!!as.name(yvar))) +  
                                geom_point() + 
                                geom_line(data=fitdf,aes(x=!!as.name(xvar),y=Estimate)) + 
                                geom_ribbon(data=fitdf,aes(x=!!as.name(xvar),y=Estimate,ymin=Lower,ymax=Upper),alpha=0.3) 
      lpos<-"none"
  }
  if (!is.null(grp)) {
      lp <- ggplot(data=datadf,aes(x=!!as.name(xvar),y=!!as.name(yvar),color=!!as.name(grp))) +  
                                geom_point() + 
                                geom_line(data=fitdf,aes(x=!!as.name(xvar),y=Estimate,color=!!as.name(grp))) +
                                geom_ribbon(data=fitdf,aes(x=!!as.name(xvar),y=Estimate,ymin=Lower,ymax=Upper,
                                                           color=!!as.name(grp),fill=!!as.name(grp)),alpha=0.3)
  }
  lp<-lp + xlab(xlab) + ylab(ylab) + 
           scale_color_nejm() + scale_fill_nejm() +
           theme_classic() +
           theme(legend.position=lpos,
                 legend.title = element_blank(),
                 legend.text = element_text(size=10,face="bold"),
                 panel.background = element_blank(),
                 axis.line = element_line(linewidth = 1),
                 axis.title.y = element_text(size=12,face="bold"),
                 axis.title.x = element_text(size=12,face="bold",color="black"),
                 axis.text.y=element_text(size=10,face="bold",color="black"),
                 axis.text.x=element_text(size=10,face="bold",color="black")) 
  return(lp)
}

grpdotplot<-function(datadf,meansdf,yvar,grp,ylab,dotsize=0.5,pointsize=4,ebarwidth=0.25,elinewidth=1.5,meancolor="red") {
   lp<-ggplot(datadf,aes(x=!!as.name(grp),y=!!as.name(yvar))) + 
           geom_dotplot(binaxis = "y", stackdir = "center",dotsize=dotsize) + 
           geom_point(data=meansdf,aes(y=Estimate,x=!!as.name(grp)),color=meancolor,size=pointsize) +
           geom_errorbar(data=pmdat,aes(x=!!as.name(grp),y=Estimate,ymin=Lower,ymax=Upper),color=meancolor,width=ebarwidth,
                         linewidth=elinewidth) +
           scale_color_nejm() +
           ylab(ylab) +
           theme_classic() +
           theme(panel.background = element_blank(),
                 axis.line = element_line(linewidth = 1),
                 axis.title.y = element_text(size=12,face="bold"),
                 axis.title.x = element_blank(),
                 axis.text.y=element_text(size=10,face="bold",color="black"),
                 axis.text.x=element_text(size=10,face="bold",color="black")) 
           
   return(lp)
}

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