\name{consensusRDA}
\alias{consensusRDA}
\title{
Calculates a consensus RDA
}
\description{
Calculates a consensus RDA. That is a consensus of a series of RDA performed on the same data using different association coefficients.
}
\usage{
consensusRDA(ordires, ordisigniaxis, resp.var, expl.var, pval = 0.05, scaling = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ordires}{
 A list of \code{\link[vegan]{rda}} or \code{\link[vegan]{capscale}} result object that includes a series of RDA or distance-based RDA (dbRDA) performed with different association coefficients on the same data.
}
  \item{ordisigniaxis}{
 A list of \code{\link[vegan]{anova.cca}} object where each axis was tested for each RDA (or dbRDA) in \code{ordires}. This argument can also be a vector defining the number of significant axes in the RDA. See details. 
}
  \item{resp.var}{
 A matrix of the response variables (species) used to construct all the RDA (or dbRDA) in \code{ordires}.
}
  \item{expl.var}{
 A matrix of the explanatory variables used to construct all the RDA (or dbRDA) in \code{ordires}.
}
  \item{pval}{
 Numeric. P-value threshold to select the number of axes to use. This argument is only active if a list of \code{\link[vegan]{anova.cca}} object is given for the argument \code{ordisigniaxis}, otherwise it is not considered. Default is 0.05.
}
  \item{scaling}{
 Type of scaling used to project the results. A distance scaling (scaling 1) or a correlation scaling (scaling 2) can be used. Default is scaling 1. See details for more information.
}
}
\details{
For the argument \code{ordisigniaxis}, if a vector of number of significant axes is given for each RDA, it is assumed that the significant axes are selected in sequential order from the first axis.
Although it is possible for the scaling to be 3 (it is available in the vegan package), this scaling should only be used for canonical correspondence analysis (CCA), it does not make any sense to use in the RDA framework.
}
\value{
 \item{value}{A vector of eigenvalues associated to the axes of the consensus RDA}
 \item{siteConsensus}{A matrix of consensus sites scores}
 \item{spConsensus}{A matrix of consensus species scores}
 \item{descConsensus}{A matrix of consensus canonical coefficient}
}
\author{
F. Guillaume Blanchet
}

\examples{
###########################################################################
### This example reproduces Figure 7c of Blanchet et al. (in press)
### 
### However, for illustration purposes RDA axes were tested using anova.cca
### This only has minor influence on the triplot. 
###########################################################################
data(beetle)
data(beetle.expl)

### Construct results object
ndis<-9
ordiRes<-vector("list",length=ndis)

### RDA species profile
sp<-beetle/apply(beetle,1,sum)
ordiRes[[1]]<-rda(sp~.,data=beetle.expl)
	
### RDA chord
chord<-beetle/sqrt(apply(beetle^2,1,sum))
ordiRes[[2]]<-rda(chord~.,data=beetle.expl)
	
### RDA Hellinger
hell<-decostand(beetle,method="hellinger")
ordiRes[[3]]<-rda(hell~.,data=beetle.expl)

### db-RDA Bray-Curtis
ordiRes[[4]]<-capscale(sqrt(vegdist(beetle,method="bray"))~.,data=beetle.expl,comm=beetle)
	
### db-RDA square-root Bray-Curtis
ordiRes[[5]]<-capscale(sqrt(vegdist(beetle^0.5,method="bray"))~.,data=beetle.expl,comm=beetle^0.5)
	
### db-RDA fourth-root Bray-Curtis
ordiRes[[6]]<-capscale(sqrt(vegdist(beetle^0.25,method="bray"))~.,data=beetle.expl,comm=beetle^0.25)
	
### db-RDA modified Gower log 2
ordiRes[[7]]<-capscale(vegdist(decostand(beetle, "log",logbase=2), "altGower")~.,data=beetle.expl,comm=decostand(beetle, "log",logbase=2)) ### Warning message stem from log transformation of 0
	
### db-RDA modified Gower log 5
ordiRes[[8]]<-capscale(vegdist(decostand(beetle, "log",logbase=5), "altGower")~.,data=beetle.expl,comm=decostand(beetle, "log",logbase=5)) ### Warning message stem from log transformation of 0
	
### db-RDA modified Gower log 10
ordiRes[[9]]<-capscale(vegdist(decostand(beetle, "log",logbase=10), "altGower")~.,data=beetle.expl,comm=decostand(beetle, "log",logbase=10)) ### Warning message stem from log transformation of 0

#----------------
### Test RDA axis 
#----------------
ordiResTest<-vector("list",length=ndis)

for(i in 1:ndis){
	ordiResTest[[i]]<-anova.cca(ordiRes[[i]],by="axis",cutoff=0.1)
}

### Consensus RDA
consRDA<-consensusRDA(ordiRes,ordiResTest,beetle,beetle.expl)
summary(consRDA)
axisLabels<-c(paste("Axis 1 - ",round(consRDA$values[1]/sum(consRDA$values),4)*100,sep=""),paste("Axis 2 - ",round(consRDA$values[2]/sum(consRDA$values),4)*100,sep=""))

plot(consRDA$siteConsensus[,1:2],pch=19,xlab=axisLabels[1],ylab=axisLabels[2],las=1)
abline(h=0,v=0,lty=3)

arrows(0,0,consRDA$spConsensus[,1]*10,consRDA$spConsensus[,2]*10,col="red",length=0.1,angle=12)
text(consRDA$spConsensus[,1:2]*10,labels=rownames(consRDA$spConsensus),col="red")

arrows(0,0,consRDA$descConsensus[,1]*0.4,consRDA$descConsensus[,2]*0.4,col="blue",length=0.1,angle=12)
text(consRDA$descConsensus[,1:2]*0.4,labels=rownames(consRDA$descConsensus),col="blue")

}

\keyword{ multivariate }
