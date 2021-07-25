# GCTabs.R 2019-Feb-10 ViperMCS@gmail.com
# ------------------------------------------------------------------------------------------------------------------------ #
# Load Reference Tables
require(RODBC)
DTABS = "X:\\Tabs\\"
if(TRUE){
x=odbcConnectAccess(paste(DTABS,"GCTabs.MDB",sep=""))
#W6NI=sqlQuery(x,"SELECT * FROM WHO2006;",believeNRows=FALSE,rows_at_time=1)
W6NA=sqlQuery(x,"SELECT * FROM WHO2006NA;",believeNRows=FALSE,rows_at_time=1)
QTPR=sqlQuery(x,"SELECT * FROM QTPRENDA;",believeNRows=FALSE,rows_at_time=1)
odbcClose(x)
l=c("wei","len","bmi","hc","ac","ss","ts","wfl","wfh")
for(i in 1:length(l)){
	x=read.table(paste0(DTABS,"WHOANTHRO\\",l[i],"anthro.txt"),header=TRUE,sep="",skip=0)
	if(!"loh"%in%names(x)&!"lorh"%in%names(x)){x$loh="X"}else if("lorh"%in%names(x)){names(x)[which(names(x)=="lorh")]="loh"}
	if("age"%in%names(x)){names(x)[which(names(x)=="age")]="icoox"}
	if("length"%in%names(x)){names(x)[which(names(x)=="length")]="icoox";x$icoox=10*x$icoox}
	if("height"%in%names(x)){names(x)[which(names(x)=="height")]="icoox";x$icoox=10*x$icoox}
	x$loh=with(x,as.integer(ifelse(loh=="L",1,ifelse(loh=="H",2,0))))
	x$icoox=as.integer(x$icoox)
	x$tab=paste0(substr(l[i],1,1),substr(l[i],nchar(l[i]),nchar(l[i])))
	if(i==1){d=x}else{d=rbind(d,x)}
}
names(d)=c("ISEX","ICOOX","LPAR","MPAR","SPAR","IRECUMB","ITAB")
d$ICOOXI=d$ICOOX
d$ITAB=toupper(d$ITAB)
d$ITAB=with(d,ifelse(ITAB=="BI","BM",ifelse(ITAB=="LN","HA",ifelse(ITAB=="WI","WA",ifelse(ITAB=="WL","WH",ITAB)))))
W6NI=d
}
# ------------------------------------------------------------------------------------------------------------------------ #
# Anthropometric indices, children and adolescents
WNAzcm=function(w,h,s,m){
	# computes raw WHO 2006 Children-Adolescent anthro scores
	WFAZ=NA;WFAC=NA;WFAP=NA
	HFAZ=NA;HFAC=NA;HFAP=NA
	BFAZ=NA;BFAC=NA;BFAP=NA
	l=W6NA[W6NA$Sex==s&W6NA$Month==as.integer(m),]
	if(nrow(l)==1){
		#print(l)
		WFAP=w/l$MWFA
		WFAZ=ifelse(l$LWFA==0,log(WFAP)/SWFA,(WFAP^l$LWFA-1)/l$SWFA/l$LWFA)
		WFAC=pnorm(WFAZ)
		HFAP=h/l$MHFA
		HFAZ=ifelse(l$LHFA==0,log(HFAP)/SHFA,(HFAP^l$LHFA-1)/l$SHFA/l$LHFA)
		HFAC=pnorm(HFAZ)
		BFAP=(w/(h/100)^2)/l$MBMI
		BFAZ=ifelse(l$LBMI==0,log(BFAP)/SBFA,(BFAP^l$LBMI-1)/l$SBMI/l$LBMI)
		BFAC=pnorm(BFAZ)
	}
	v=c(WFAZ,WFAC,WFAP,HFAZ,HFAC,HFAP,BFAZ,BFAC,BFAP)
	names(v)=c("WFAZ","WFAC","WFAP","HFAZ","HFAC","HFAP","BFAZ","BFAC","BFAP")
	return(v)
}
# ------------------------------------------------------------------------------------------------------------------------ #
# Anthropometric indices, infants
WINzcm=function(w,h,s,d,f){
	# computes raw WHO 2006 Infants anthro scores
	WFAZ=NA;WFAC=NA;WFAP=NA
	HFAZ=NA;HFAC=NA;HFAP=NA
	WFHZ=NA;WFHC=NA;WFHP=NA
	BMIZ=NA;BMIC=NA;BMIP=NA
	l=W6NI[W6NI$ITAB=="WA"&W6NI$ISEX==s&W6NI$ICOOXI==as.integer(d),]
	if(nrow(l)==1){
		#print(l)
		WFAP=w/l$MPAR
		WFAZ=ifelse(l$LPAR==0,log(WFAP)/SPAR,(WFAP^l$LPAR-1)/l$SPAR/l$LPAR)
		WFAC=pnorm(WFAZ)
	}
	l=W6NI[W6NI$ITAB=="HA"&W6NI$ISEX==s&W6NI$ICOOXI==as.integer(d)&W6NI$IRECUMB==f,]
	if(nrow(l)==1){
		#print(l)
		HFAP=h/l$MPAR
		HFAZ=ifelse(l$LPAR==0,log(HFAP)/SPAR,(HFAP^l$LPAR-1)/l$SPAR/l$LPAR)
		HFAC=pnorm(HFAZ)
	}
	l=W6NI[W6NI$ITAB=="WH"&W6NI$ISEX==s&W6NI$ICOOXI==round(h*10)&W6NI$IRECUMB==f,]
	if(nrow(l)==1){
		#print(l)
		WFHP=w/l$MPAR
		WFHZ=ifelse(l$LPAR==0,log(WFHP)/SPAR,(WFHP^l$LPAR-1)/l$SPAR/l$LPAR)
		WFHC=pnorm(WFHZ)
	}
	l=W6NI[W6NI$ITAB=="BM"&W6NI$ISEX==s&W6NI$ICOOXI==as.integer(d),]
	if(nrow(l)==1){
		#print(l)
		BMIP=(w/(h/100)^2)/l$MPAR
		BMIZ=ifelse(l$LPAR==0,log(BMIP)/SPAR,(BMIP^l$LPAR-1)/l$SPAR/l$LPAR)
		BMIC=pnorm(BMIZ)
	}
	v=c(WFAZ,WFAC,WFAP,HFAZ,HFAC,HFAP,WFHZ,WFHC,WFHP,BMIZ,BMIC,BMIP)
	names(v)=c("WFAZ","WFAC","WFAP","HFAZ","HFAC","HFAP","WFHZ","WFHC","WFHP","BFAZ","BFAC","BFAP")
	return(v)
}
vWINzcm=Vectorize(WINzcm)
# ------------------------------------------------------------------------------------------------------------------------ #
# Storage
if(FALSE){
save(
	W6NA,QTPR,W6NI,
	WNAzcm,WINzcm,
	vWINzcm,
	file="GCTabs.rda"
)
# load(file="GCTabs.rda")
}
# ------------------------------------------------------------------------------------------------------------------------ #
# Inactive Code
if(FALSE){
# Tests
#WNAzcm(100,10,1,60)
#WNAzcm(100,10,2,70)
#WNAzcm(15,100,2,70)
#WINzcm(10,75,1,11*30,1)
# Pending
# interpolation option
# vectorize
}
# Documentation
if(FALSE){
# References
# Norms
# http://www.who.int/mediacentre/factsheets/fs311/en/
# Programming
# https://stackoverflow.com/questions/12460938/r-reading-in-a-zip-data-file-without-unzipping-it
# https://stackoverflow.com/questions/5448128/recoding-character-variables
# http://dplyr.tidyverse.org/reference/recode.html
}
# ------------------------------------------------------------------------------------------------------------------------ #
