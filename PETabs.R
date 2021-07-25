# PETabs.R 2021-Jul-09 ViperMCS@gmail.com
# ------------------------------------------------------------------------------------------------------------------------ #
# Load Reference Tables
require(tidyverse)
require(RODBC)
DTABS = "X:\\Tabs\\"
# ------------------------------------------------------------------------------------------------------------------------ #
# Tablas de Referencia - Mapas de Pobreza
if(TRUE){
# ------------------------------------------------------------------------------------------------------------------------ #
# Mapas de pobreza
x=odbcConnectAccess(paste(DTABS,"PETabs.MDB",sep=""))
Y09=sqlQuery(x,"SELECT UbiGeo,FGT0 FROM MaPo09 WHERE RIGHT(UbiGeo,2)<>'00' ORDER BY UbiGeo;",believeNRows=FALSE,rows_at_time=1)
Y13=sqlQuery(x,"SELECT UBIGEO,PP15,PTLI95,PTLS95 FROM MaPo13;",believeNRows=FALSE,rows_at_time=1)
odbcClose(x)
Y09$Ubi=Y09$UbiGeo
Y13$Ubi=Y13$UBIGEO
Y13$NPI95=Y13$PTLI95*Y13$PP15/100
Y13$NPS95=Y13$PTLS95*Y13$PP15/100
x=	Y13 %>% 
	group_by(Ubi) %>% 
	summarise(
		NROWS=n(),
		TPP15=sum(PP15,na.rm=TRUE),
		NPI95=sum(NPI95,na.rm=TRUE),
		NPS95=sum(NPS95,na.rm=TRUE) 
	) %>%
	mutate(
		PTLI95=NPI95/TPP15,
		PTLS95=NPS95/TPP15,
		PTLW95=(NPI95+NPS95)/TPP15/2,
		NTLW95=(NPI95+NPS95)/2
	) %>%
	arrange(desc(PTLW95)) %>%
	mutate(
		STLW95=cumsum(TPP15)/sum(TPP15),
		GCP05=as.integer((STLW95-min(STLW95))/0.20)+1,
		GCP10=as.integer((STLW95-min(STLW95))/0.10)+1
	) %>%
	arrange(desc(NTLW95)) %>%
	mutate(
		SNLW95=cumsum(NTLW95)/sum(NTLW95),
		GPP05=as.integer((SNLW95-min(SNLW95))/0.20)+1,
		GPP10=as.integer((SNLW95-min(SNLW95))/0.10)+1
	) %>%
	arrange(desc(Ubi))
#y=Y13
#x[x$NROWS==2,]
#y[y$Ubi%in%x$Ubi[x$NROWS==2],]
MP09=Y09	# MP9 en versiones anteriores
MP13=x
#x=merge(x,MP9,by=c("Ubi"))
#with(x,plot(PTLW95,FGT0,col="gray",cex=0.5))
#with(x[x$NROWS>1,],points(PTLW95,FGT0,col="red",cex=0.5,pch=16))
#abline(a=0,b=100,col="pink")
x=0;y=0;rm(Y09,Y13)
# pendiente: pensar en deciles, quintiles y pareto
}
# ------------------------------------------------------------------------------------------------------------------------ #
# Tablas de Referencia - Proyecciones Demográficas Edad (a) 0:80 vs Tiempo 1950:2050
if(TRUE){	# XPreadpppdf=function()
#require(pdftools)
X=pdftools::pdf_text(pdf="E:\\Lib\\Pub\\Peru\\INEI\\BibINEI\\DEMO\\INEI2009LibroBE17proy.pdf")
U=list()
for(i in 33:66){
	y=X[i]%>%readr::read_lines()
	y=y[!grepl("ESTIMACIONES",toupper(y))&!grepl("NACIONAL",toupper(y))&!grepl("CUADRO",y)&!grepl("CALENDARIO",y)&!grepl("Continúa",y)&!grepl("Conclusión",y)&!grepl("-",y)]
	y=gsub("80 y más","    80  ",y)
	y=gsub("Total","1000",y)
	y=gsub("\\s1\\s(\\d+)\\s(\\d+)\\s"," \\1\\2\\3\\4 ",y)	# para millones
	y=gsub("(\\d+)\\s(\\d+)\\s(\\d+)\\s(\\d+)","\\1 \\2   \\3 \\4",y)	# para columnas muy pegadas
	u=(do.call(rbind,strsplit(trimws(gsub("(\\d)\\s(\\d)","\\1\\2",y)),"\\s+")))	# para espacios únicos dentro de cifras
	v=as.data.frame(matrix(sapply(u[2:nrow(u),],as.numeric),c(nrow(u)-1,ncol(u))),stringsAsFactors=FALSE)
	names(v)=c("EDAD",paste0("A",u[1,2:ncol(u)]))
	U[[i]]=v
}
for(i in 1:17){
	U[[i]]=rbind(U[[(i-1)*2+33]],U[[(i-1)*2+34]])
}
z=do.call(cbind,U[1:17]);names(z)[1]="EDADS";z=z[names(z)!="EDAD"]
TPPBE17=z
}
# ------------------------------------------------------------------------------------------------------------------------ #
# Tablas de Referencia - Proyecciones Demográficas Departamento, Edad, Sexo vs Tiempo 1950:2050
if(TRUE){
X=pdftools::pdf_text(pdf="E:\\Lib\\Pub\\Peru\\INEI\\BibINEI\\DEMO\\INEI2009LibroBAD37proydpto.pdf")
U=list();k=58;S=seq(1995,2025,5)
for(i in 1:21){
	print(i+k-1)	#;readline(prompt="Next")
	y=X[i+k-1]%>%readr::read_lines()
	L1=y[grepl(" 3.",y)];L1=strsplit(trimws(L1)," ")[[1]];L1=L1[length(L1)]
	L2=case_when(any(grepl(" MASCU",y))~"M",any(grepl(" FEMEN",y))~"F",TRUE~"T")
	l=str_locate(y[grepl("Amazonas",y)],"Amazonas")[1]
	y=trimws(y[nchar(trimws(substr(y,l,l)))>0])
	for(j in 1:2){y=gsub("(\\d+)\\s(\\d{3})","\\1\\2",y)}	# números precediendo tríadas
	y=gsub("Prov. Const. ","",y)
	y=gsub("La Libertad","La_Libertad",y)
	y=gsub("Madre de Dios","Madre_de_Dios",y)
	y=gsub("San Martín","San_Martín",y)
	if(i==10){	# parche de secuencias específicas
		y=gsub("804729623443","804729 623443",y)
		y=gsub("336472287918","336472 287918",y)
	}
	u=(do.call(rbind,strsplit(y,"\\s+")))
	v=as.data.frame(u)
	names(v)=c("DPTO","TOTAL",paste0("A",sprintf("%02d",seq(0,80,5))))
	U[[i]]=v%>%mutate(GYR=S[(i-1)%/%3+1],GSX=L2,TAB=L1)
}
z=as_tibble(do.call(rbind,U))%>%mutate(across(TOTAL:GYR,as.numeric))
TPPBE37=z
}
# ------------------------------------------------------------------------------------------------------------------------ #
# Tablas de Referencia - Demografía Básica Distrital censo 2017
if(TRUE){
d=
	readxl::read_excel(
		path="X:/Tabs/INEICPV/INEIreporteCPV1721030820.xlsx",
		col_names=c("V1","GRPO","GEDA","NMAS","NFEM","NTOT"),
		col_types=c("skip","text","text","numeric","numeric","numeric")
	)%>%
	filter(
		grepl("AREA #",GRPO)|
		grepl("RESUMEN",GRPO)|
		grepl("Urbano",GRPO)|
		grepl("Rural",GRPO)|
		grepl("Total",GRPO)|
		!is.na(NTOT)
	)%>%
	mutate(
		AUR=ifelse(grepl("Urbano",GRPO),"U",ifelse(grepl("Rural",GRPO),"R",ifelse(grepl("Total",GRPO),"T",ifelse(grepl("RESUMEN",GRPO),"O",NA)))),
		UBI=ifelse(grepl("AREA #",GRPO),substr(GRPO,8,13),ifelse(grepl("RESUMEN",GRPO),"000000",NA)),
		DPD=ifelse(grepl("AREA #",GRPO),GEDA,ifelse(grepl("RESUMEN",GRPO),"Perú",NA))
	)%>%
	fill(AUR,UBI,DPD)%>%
	filter(!is.na(NTOT))
TDPD2017=
	d%>%
	filter(GEDA=="Total"&AUR=="T"&UBI!="000000")%>%
	mutate(DPD=gsub("del Callao,","del Callao, Callao,",DPD))%>%
	separate(col=DPD,into=c("DPTO","PROV","DIST"),sep=",")%>%
	mutate(
		PROV=trimws(PROV),
		DIST=gsub("distrito: ","",trimws(DIST))
	)%>%
	mutate(
		DIST=
			case_when(
				DIST=="Victor Larco Herrera"~"Víctor Larco Herrera",
				DIST=="Monsefu"~"Monsefú",
				DIST=="Tuman"~"Tumán",
				DIST=="Veintiseis de Octubre"~"Veintiséis de Octubre",
				DIST=="El Algarrobal"~"Algarrobal",
				DIST=="Coronel Gregorio Albarracin Lanchipa"~"Coronel Gregorio Albarracín Lanchipa",
				DIST=="José Luis Bustamante Y Rivero"~"José Luis Bustamante y Rivero",
				DIST=="San Sebastian"~"San Sebastián",
				DIST=="Calleria"~"Callería",
				DIST=="Simon Bolívar"~"Simón Bolívar",
				TRUE~DIST
			)
	)%>%
	select(NMAS,NFEM,NTOT,UBI,DPTO,PROV,DIST)
TDPD2017ESA=
	d%>%
	mutate(GED=trimws(substr(GEDA,4,5)))%>%
	mutate(GED=ifelse(nchar(GED)==1,paste0("0",GED),ifelse(GED=="al","TO",GED)))%>%
	select(GED,AUR,NMAS,NFEM,NTOT,UBI)
}
# ------------------------------------------------------------------------------------------------------------------------ #
# Tablas de Referencia - Principales Ciudades (excepto Lima) 2020
if(TRUE){
X=pdftools::pdf_text(pdf="E:\\Lib\\Pub\\Biblio\\Biblio2008\\INEI2020Libro1747planosgc.pdf")
U=list();k=3
Y1=X[1+k-1]%>%readr::read_lines()
Y2=X[2+k-1]%>%readr::read_lines()
#save(Y1,Y2,file="X2021030808.rda")
#load(file="X2021030808.rda")
x=
	c(
		substr(Y1,  1, 60),
		substr(Y1,114,170),
		substr(Y1,223,280),
		substr(Y2,376,430),
		substr(Y2,482,540),
		substr(Y2,587,620)
	)
x=trimws(gsub("\\.","",x[grepl("Región",x)|grepl("Distrito",x)|grepl("Ciudad",x)]))
z=
	tibble(GRU="",REG="",CIU="",DST="",UBI="",DESC="",ORIG=x)%>%
	mutate(
		DESC=
			gsub("Distrito de ","",
			gsub("Ciudad de ","",
			gsub("Región ","",
			gsub("Ciudades de la ","",ORIG))))
	)%>%
	mutate(
		GRU=ifelse(grepl("Ciudades de la ",ORIG),DESC,NA),
		REG=ifelse(grepl("Región ",ORIG),DESC,NA),
		CIU=ifelse(grepl("Ciudad de ",ORIG),DESC,NA),
		DST=ifelse(grepl("Distrito de ",ORIG),DESC,NA)
	)%>%
	fill(GRU,REG,CIU)%>%
	filter(!is.na(DST))
INEIGC20=
	z%>%
	mutate(
		DST=
			case_when(
				DST=="San Ramon"~"San Ramón",
				DST=="Mariano Damaso"~"Mariano Damaso Beraun",
				DST=="San Juan Bautis"~"San Juan Bautista",
				DST=="Simón Bolivar"~"Simón Bolívar",
				DST=="Juanjui"~"Juanjuí",
				DST=="La Banda de Shi"~"La Banda de Shilcayo",
				DST=="Pillcomarca"~"Pillco Marca",
				TRUE~DST
			)
	)%>%
	left_join(
		y=
			TDPD2017%>%
			rename(REG=DPTO,DST=DIST,UBIGEO=UBI)%>%
			select(REG,DST,UBIGEO),
		by=c("REG","DST")
	)%>%
	mutate(UBI=UBIGEO)%>%
	select(-UBIGEO)
}
# ------------------------------------------------------------------------------------------------------------------------ #
# Tablas de Referencia - Autoridades
if(TRUE){
x="
'AFF','1995-07-28','2000-07-28','orange'
'AFF','2000-07-28','2000-11-21','orange'
'VPC','2000-11-21','2001-07-28','black'
'ATM','2001-07-28','2006-07-28','darkgreen'
'AGP','2006-07-28','2011-07-28','red'
'OHT','2011-07-28','2016-07-28','pink'
'PKG','2016-07-28','2018-03-23','lightblue'
'MVC','2018-03-23','2020-11-10','blue'
'MML','2020-11-10','2020-11-17','gray'
'FSH','2020-11-17','2021-07-28','violet'
"
PEPRE=read_delim(file=x,col_names=c("PRESI","DESDE","HASTA","COLOR"),col_types="cDDc",delim=",",quote="'")
x="
'AAR','1999-04-15','2000-11-25'
'EPZ','2000-11-25','2001-07-28'
'LSD','2001-07-28','2002-01-18'
'FCC','2002-01-18','2003-06-28'
'AVR','2003-06-28','2004-02-16'
'PMZ','2004-02-16','2006-07-28'
'CVS','2006-07-28','2007-12-20'
'HGL','2007-12-20','2008-10-14'
'OUU','2008-10-14','2011-07-28'
'ATN','2011-07-28','2012-07-23'
'MHR','2012-07-23','2014-11-05'
'AVV','2014-11-05','2016-07-28'
'PGF','2016-07-28','2017-09-17'
'FAI','2017-09-17','2018-01-09'
'ASR','2018-01-09','2018-04-09'
'SPE','2018-04-09','2019-01-07'
'ZTG','2019-01-07','2020-11-18'
'EHP','2019-11-18','2020-03-20'
'VZM','2020-03-20','2020-07-15'
'PMS','2020-07-15','2020-11-11'
'ASL','2020-11-11','2021-11-18'
'PMS','2020-11-18','2021-02-13'
'OUU','2021-02-13','2021-07-28'
"
PEMSA=read_delim(file=x,col_names=c("MINSA","DESDE","HASTA"),col_types="cDD",delim=",",quote="'")
x="
'CTA','2011-10-21','2013-07-22'
'MRG','2013-07-22','2014-02-24'
'PBS','2014-02-24','2016-07-28'
'CAG','2016-07-28','2017-07-27'
'FMA','2017-07-27','2018-01-09'
'JMC','2018-01-09','2018-03-23'
'JMC','2018-03-23','2018-04-02'
'LLH','2018-04-02','2019-03-19'
'PBS','2019-03-11','2019-10-03'
'LMC','2019-10-03','2019-10-29'
'ALF','2019-10-29','2020-07-15'
'PDP','2020-07-15','2020-11-15'
'FTH','2020-11-15','2020-11-18'
'SVW','2020-11-18','2021-07-28'
"
PEMDI=read_delim(file=x,col_names=c("MIDIS","DESDE","HASTA"),col_types="cDD",delim=",",quote="'")
#sink("X201809171036.txt");print(U);print(O);sink()
#gsub("(\\d+)\\s(\\d+)\\s(\\d+)\\s(\\d+)","\\1\\2 \\3\\4","12345   123 456 654 321   6789  ")
#for(i in 33:65){print(head(U[[i]]))}
#	if((i%%2)==1){
#		u=gsub(" ","",sapply(1:length(y),function(k)substring(y[k],c(1,seq(18,93,15)),c(19,seq(18,93,15)+15-1)),simplify=TRUE))
#		v=as.data.frame(matrix(sapply(u[2:7,],as.numeric),c(nrow(u)-1,ncol(u))),stringsAsFactors=FALSE)
#		names(v)=c("AÑO","TOTAL",paste0("E",sprintf("%03d",as.numeric(u[1,3:dim(u)[2]]))))
#	}else{
#		u=gsub(" ","",sapply(1:length(y),function(k)substring(y[k],c(1,20,46,60,75,87,95),c(19,45,59,74,86,94,116)),simplify=TRUE))
#		v=as.data.frame(matrix(sapply(u[2:7,],as.numeric),c(nrow(u)-1,ncol(u))),stringsAsFactors=FALSE)
#		names(v)=c("AÑO",paste0("E",sprintf("%03d",as.numeric(u[1,2:dim(u)[2]]))))
#	}
#	v=array(unlist(u),dim=c(length(unlist(u))/length(y),length(y)))
#O=
#	left_join(
#		do.call(rbind,U[sapply(U,function(x)"E000"%in%names(x))]),
#		do.call(rbind,U[sapply(U,function(x)"E041"%in%names(x))]) 
#	)
#print(c(i, names(v)[1:6]))
}
# ------------------------------------------------------------------------------------------------------------------------ #
# Informes - Presupuesto por Resultados
if(FALSE){
# Parámetros Generales
S1=c("OO","VE","DS","LI","LS","CV","NP","NS")
S2=c("skip","text","text","skip","skip","skip","text","text")
F1=function(f,h,r,s,l,fDEB=FALSE){
d=
	readxl::read_excel(
		path=f,sheet=h,range=r,
		col_names=c(
			"Categ",
			s,
			"X01","DIFE94","DIFE98","X02","TEST94","TEST98"
		),
		col_types=c(
			"text",
			l,
			"skip","numeric","numeric","skip","text","text"
		)
	)
if(fDEB){print(d)}
x=
	d%>%
	filter(!is.na(Categ))%>%
	select(-one_of("DIFE94","DIFE98","TEST94","TEST98"))%>%
	pivot_longer(cols=!Categ)%>%
	mutate(Tramo=substr(name,3,6),name=substr(name,1,2))%>%
	pivot_wider(id_cols=c(Categ,Tramo))%>%
	mutate(
		VE=(gsub(",",".",gsub("\\)","",gsub("\\(","",gsub("-","",gsub("n.d.","",VE)))))),
		DS=(gsub(",",".",gsub("\\)","",gsub("\\(","",gsub("-","",gsub("n.d.","",DS)))))),
		NP=(gsub("n.d.","",NP)),
		NS=(gsub("-","",(gsub("n.d.","",NS))))
	)
if(fDEB){print(x)}
y=
	x%>%
	mutate(
		VE=as.numeric(VE),
		DS=as.numeric(DS),
		NP=as.numeric(NP),
		NS=as.numeric(NS)
	)%>%
	filter(!is.na(VE))
if(fDEB){print(y)}
if(fDEB){return(list(d,x,y))}else{return(y)}
}
# Desnutrición OMS NN 2005-2020
S3=
	c(
		"2005","2007","2008","2009",
		"2010","2011","2012","2013","2014","2015",
		"2016","2017","2X18","2018","2019"
	)
L3=length(S3)
s="";for(i in 1:L3){s=c(s,paste0(S1,S3[i]))};s=s[2:length(s)]
l=rep(S2,L3)
k=which(s=="OO2011")	# entre 10 y 11 hay dos columnas blancas
s=c(s[1:k],"Y01",s[(k+1):length(s)])
l=c(l[1:k],"skip",l[(k+1):length(l)])
dDN1=
	F1(
		f="X:/Tabs/PPR/ppr_2014_2019/A I -  Articulado Nutricional 2019.xlsx",
		h="(2)Desnutrición Cronica OMS NN",r="A7:DX52",
		s=s,l=l
	)%>%
	mutate(
		Grupo=
			case_when(
				grepl("Total",Categ)~"Total",
				grepl("36",Categ)~"Edad",
				grepl("Urbana",Categ)|grepl("Rural",Categ)~"Área",
				grepl("Lima",Categ)|grepl("Costa",Categ)|grepl("Sierra",Categ)|grepl("Selva",Categ)~"Dominio",
				grepl("Primaria",Categ)|grepl("Secundaria",Categ)|grepl("Superior",Categ)~"Educación",
				grepl("quintil",tolower(Categ))~"Quintil",
				grepl("JUNTOS",Categ)~"Juntos",
				TRUE~"Agua"
			),
		Indic="DTE06"
	)
# Desnutrición OMS D 2005-2019
S3=
	c(
		"2X07","2B09","2B10","2B11","2B12","2B13",
		"2015","2016","2017","2018","2019"
	)
L3=length(S3)
s="";for(i in 1:L3){s=c(s,paste0(S1,S3[i]))};s=s[2:length(s)]
l=rep(S2,L3)
s=s[2:length(s)];l=l[2:length(l)]	# sin primera columna de espacio
dDN2=
	F1(
		f="X:/Tabs/PPR/ppr_2014_2019/A I -  Articulado Nutricional 2019.xlsx",
		h="(2A)Desnutrición Cronica OMS D ",r="A7:CP37",
		s=s,l=l
	)%>%
	mutate(
		Grupo=ifelse(grepl("Total",Categ),"Total","RegionP"),
		Indic="DTE06"
	)%>%
	filter(!(Grupo=="Total"&Tramo%in%as.character(2015:2019)))
# table(x[[2]]$VE[which(is.na(as.numeric(x[[2]]$VE)))])
EPPR=
	bind_rows(
		dDN1,dDN2
	)%>%
	mutate(
		Categ=trimws(gsub("  "," ",Categ)),
		Perio=as.numeric(paste0(substr(Tramo,1,1),"0",substr(Tramo,3,4)))
	)%>%
	mutate(
		Ciclo=
			case_when(
				Perio==2000~1,
				Perio%in%c(2005:2008)~2,
				Perio%in%c(2009:2011)~3,
				Perio%in%c(2012:2014)~4,
				Perio%in%c(2015:2017)~5,
				Perio%in%c(2018:2020)~6,
				TRUE~as.numeric(NA)
			),
		Punto=
			Perio+
			case_when(
				substr(Tramo,2,2)=="B"~1.00,
				substr(Tramo,2,2)=="X"~0.25,
				TRUE~0.50
			)
	)%>%
	arrange(Indic,Grupo,Categ,Perio,Punto)
# Desnutrición OMS 2020
S3=
	c(
		"2005","2007","2008","2009",
		"2010","2011","2012","2013","2014","2015",
		"2016","2017","2X18","2018","2019","2020"
	)
L3=length(S3)
s="";for(i in 1:L3){s=c(s,paste0(S1,S3[i]))};s=s[2:length(s)]
l=rep(S2,L3)
k=which(s=="OO2011")	# entre 10 y 11 hay dos columnas blancas
s=c(s[1:k],"Y01",s[(k+1):length(s)])
l=c(l[1:k],"skip",l[(k+1):length(l)])
dDN1=
	F1(
		f="X:/Tabs/PPR/ppr_2015_2020/A I Articulado Nutricional 2020.xlsx",
		h="(1)Desnutrición Cronica OMS NN",r="A7:EF52",
		s=s,l=l
	)%>%
	mutate(
		Grupo=
			case_when(
				grepl("Total",Categ)~"Total",
				grepl("36",Categ)~"Edad",
				grepl("Urbana",Categ)|grepl("Rural",Categ)~"Área",
				grepl("Lima",Categ)|grepl("Costa",Categ)|grepl("Sierra",Categ)|grepl("Selva",Categ)~"Dominio",
				grepl("Primaria",Categ)|grepl("Secundaria",Categ)|grepl("Superior",Categ)~"Educación",
				grepl("quintil",tolower(Categ))~"Quintil",
				grepl("JUNTOS",Categ)~"Juntos",
				TRUE~"Agua"
			),
		Indic="DTE06"
	)
S3=
	c(
		"2X07","2B09","2B10","2B11","2B12","2B13",
		"2015","2016","2017","2018","2019","2020"
	)
L3=length(S3)
s="";for(i in 1:L3){s=c(s,paste0(S1,S3[i]))};s=s[2:length(s)]
l=rep(S2,L3)
s=s[2:length(s)];l=l[2:length(l)]	# sin primera columna de espacio
dDN2=
	F1(
		f="X:/Tabs/PPR/ppr_2015_2020/A I Articulado Nutricional 2020.xlsx",
		h="(1A)Desnutrición Cronica OMS D ",r="A7:CX37",
		s=s,l=l
	)%>%
	mutate(
		Grupo=ifelse(grepl("Total",Categ),"Total","RegionP"),
		Indic="DTE06"
	)%>%
	filter(!(Grupo=="Total"&Tramo%in%as.character(2015:2020)))
d=
	bind_rows(dDN1,dDN2)%>%
	filter(Tramo%in%c("2020"))%>%
	mutate(
		Categ=trimws(gsub("  "," ",Categ)),
		Perio=as.numeric(paste0(substr(Tramo,1,1),"0",substr(Tramo,3,4)))
	)%>%
	mutate(
		Ciclo=
			case_when(
				Perio%in%c(2018:2020)~6,
				TRUE~as.numeric(NA)
			),
		Punto=
			Perio+
			case_when(
				substr(Tramo,2,2)=="B"~1.00,
				substr(Tramo,2,2)=="X"~0.25,
				TRUE~0.50
			)
	)
EPPR=
	bind_rows(EPPR,d)%>%
	arrange(Indic,Grupo,Categ,Perio,Punto)
# Anemia
# Suplementación
# CRED
# Informes PPR antes de 2014,2000
# Informes posteriores a 2020
}
# ------------------------------------------------------------------------------------------------------------------------ #
# Storage
if(FALSE){
save(
	TPPBE17,TPPBE37,
	INEIGC20,TDPD2017,TDPD2017ESA,
	PEPRE,PEMSA,PEMDI,
	MP09,MP13,
	EPPR,
	file=paste0(DTABS,"PETabs.rda")
)
load(file=paste0(DTABS,"PETabs.rda"))
}
# ------------------------------------------------------------------------------------------------------------------------ #
