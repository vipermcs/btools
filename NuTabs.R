# NuTabs.R 2020-Sep-27 ViperMCS@gmail.com

# ------------------------------------------------------------------------------------------------------------------------ #
# Parameters and Libraries
require(RODBC)
require(dplyr)
require(haven)
DTABS="X:\\Tabs\\"
NTABS=paste0(DTABS,"NuTabsSRC\\")
WKDIR="X:\\Work\\"
setwd(WKDIR)
# load(file=paste0(DTABS,"NuTabs.rda"))
# ------------------------------------------------------------------------------------------------------------------------ #
# Functions
# Complementary Functions
# ------------------------------------------------------------------------------------------------------------------------ #
# Load Reference Tables
RPCP=function(s,l=6){return(ifelse(is.na(s),NA,substr(paste0(trimws(s),"          "),1,l)))}
# ------------------------------------------------------------------------------------------------------------------------ #
# Food Composition and Auxilliary Tables
if(TRUE){
# reads ANDREA from source MDB
x=odbcConnectAccess(paste(NTABS,"Consumo.MDB",sep=""),pwd="cenan")
ANUT=sqlQuery(x,"SELECT * FROM NUTRIENTE;",believeNRows=FALSE,rows_at_time=1,stringsAsFactors=FALSE)
AAYP=sqlQuery(x,"SELECT * FROM ALIMENTO_PREPARACION;",believeNRows=FALSE,rows_at_time=1,stringsAsFactors=FALSE)
AAXN=sqlQuery(x,"SELECT * FROM ALIMENTOXNUTRIENTE;",believeNRows=FALSE,rows_at_time=1,stringsAsFactors=FALSE)
odbcClose(x)
TAN3=
	AAYP%>%
	rename(
		CORAP=CORRELATIVO_ALIM_PREP,
		CTP=CODIGO_TIPO,
		PCOM=PARTE_COMESTIBLE,
	)%>%
	mutate(
		NumAlim=paste0('M',sprintf('%04d',CORAP)),
		DSC=
			paste0(
				trimws(NOMBRE), 
				ifelse(DESCRIPCION!='-1',paste0(' /',trimws(DESCRIPCION)),''), 
				ifelse(RAZON!='-1',paste0(' /',trimws(RAZON)),'')
			),
		COPR=
			case_when(
				CORAP%in%c(1852,1853,1854,1855,1856,1857,1858,1859,1860,1861,1862,1864,1865,1866,1867,1868,1869,1870,1871,1872,1873,1874)~'04',
				substr(CTP,1,1)=="P"~'07',
				TRUE~sprintf('%02d',CONDICION)
			)
	)%>%
	dplyr::select(NumAlim,CORAP,CTP,DSC,COPR,PCOM)%>%
	left_join(
		y=
			AAXN%>%
			rename(CORAP=CORRELATIVO_ALIM_PREP)%>%
			group_by(CORAP)%>%
			summarize(
				ENERGC =sum(ifelse(CORRELATIVO_NUTRIENTE==2 ,APORTE_NUTRITIVO,0)),
				PROTG  =sum(ifelse(CORRELATIVO_NUTRIENTE==4 ,APORTE_NUTRITIVO,0)),
				FATG   =sum(ifelse(CORRELATIVO_NUTRIENTE==6 ,APORTE_NUTRITIVO,0)),
				CHOG   =sum(ifelse(CORRELATIVO_NUTRIENTE==5 ,APORTE_NUTRITIVO,0)),
				FIBERDG=sum(ifelse(CORRELATIVO_NUTRIENTE==8 ,APORTE_NUTRITIVO,0)),
				CAMG   =sum(ifelse(CORRELATIVO_NUTRIENTE==10,APORTE_NUTRITIVO,0)),
				FEMG   =sum(ifelse(CORRELATIVO_NUTRIENTE==12,APORTE_NUTRITIVO,0)),
				CADUG  =sum(ifelse(CORRELATIVO_NUTRIENTE==18,APORTE_NUTRITIVO,0)),
				BCAUG  =sum(ifelse(CORRELATIVO_NUTRIENTE==17,APORTE_NUTRITIVO,0)),
				CARUG  =sum(ifelse(CORRELATIVO_NUTRIENTE==16,APORTE_NUTRITIVO,0)),
				RETUG  =sum(ifelse(CORRELATIVO_NUTRIENTE==15,APORTE_NUTRITIVO,0)),
				VAREIU =sum(ifelse(CORRELATIVO_NUTRIENTE==14,APORTE_NUTRITIVO,0)),
				VAREUG =sum(ifelse(CORRELATIVO_NUTRIENTE==13,APORTE_NUTRITIVO,0)),
				VITCMG =sum(ifelse(CORRELATIVO_NUTRIENTE==22,APORTE_NUTRITIVO,0)),
				B01THMG=sum(ifelse(CORRELATIVO_NUTRIENTE==19,APORTE_NUTRITIVO,0)),
				B02RBMG=sum(ifelse(CORRELATIVO_NUTRIENTE==20,APORTE_NUTRITIVO,0)),
				B03NIMG=sum(ifelse(CORRELATIVO_NUTRIENTE==21,APORTE_NUTRITIVO,0)),
				B06PXMG=sum(ifelse(CORRELATIVO_NUTRIENTE==23,APORTE_NUTRITIVO,0)),
				B09FOMG=sum(ifelse(CORRELATIVO_NUTRIENTE==25,APORTE_NUTRITIVO,0)),
				B12COUG=sum(ifelse(CORRELATIVO_NUTRIENTE==24,APORTE_NUTRITIVO,0)),
				ZNMG   =sum(ifelse(CORRELATIVO_NUTRIENTE==34,APORTE_NUTRITIVO,0)) 
			),
		by="CORAP"
	)
#	filter(!is.na(CODIGO_TIPO))
# reads CENAN et al from consolidated MDB
x=odbcConnectAccess(paste(DTABS,"NuTabsWK.MDB",sep=""))
cEC8=sqlQuery(x,"SELECT * FROM TCENAN20XX;",believeNRows=FALSE,rows_at_time=1,stringsAsFactors=FALSE)
xGCP=sqlQuery(x,"SELECT * FROM GCPPOOL;",believeNRows=FALSE,rows_at_time=1,stringsAsFactors=FALSE)
xCPC=sqlQuery(x,"SELECT * FROM TCMPCOM10;",believeNRows=FALSE,rows_at_time=1,stringsAsFactors=FALSE)
xABP=sqlQuery(x,"SELECT * FROM ABNPOOL;",believeNRows=FALSE,rows_at_time=1,stringsAsFactors=FALSE)
xAXM=sqlQuery(x,"SELECT * FROM TAXM;",believeNRows=FALSE,rows_at_time=1,stringsAsFactors=FALSE)
xN16=sqlQuery(x,"SELECT * FROM NM20161012;",believeNRows=FALSE,rows_at_time=1,stringsAsFactors=FALSE)
xN20=sqlQuery(x,"SELECT * FROM MM20200116;",believeNRows=FALSE,rows_at_time=1,stringsAsFactors=FALSE)
x14c=sqlQuery(x,"SELECT * FROM CNMCA14;",believeNRows=FALSE,rows_at_time=1,stringsAsFactors=FALSE)
x14n=sqlQuery(x,"SELECT * FROM CNMNA14;",believeNRows=FALSE,rows_at_time=1,stringsAsFactors=FALSE)
xE16=sqlQuery(x,"SELECT * FROM TCEVET2016;",believeNRows=FALSE,rows_at_time=1,stringsAsFactors=FALSE)
odbcClose(x)
# reads consolidated MDB (Built by XQuest with NuTabs1.sql, NuTabs3.sql, NuTabsWK.txt, NuTabsWK.bat)
x=odbcConnectAccess(paste(DTABS,"NuTabsWK.MDB",sep=""))	# "V15r.MDB", "NuTabs.MDB", "EVCTR.MDB"
TWAE=sqlQuery(x,"SELECT CodMC, MAX(Gramos) AS GramosWW FROM TMCxA WHERE NUMALIM='M1028' GROUP BY CodMC ORDER BY CodMC;",believeNRows=FALSE,rows_at_time=1,stringsAsFactors=FALSE)
TFCC=sqlQuery(x,"SELECT NUMALIM,COND AS CONDF,FCRaCO,NEqs FROM TFCC ORDER BY NumAlim,COND;",believeNRows=FALSE,rows_at_time=1,stringsAsFactors=FALSE)
TDEN=sqlQuery(x,"SELECT * FROM TDENSAL ORDER BY NUMALIM;",believeNRows=FALSE,rows_at_time=1,stringsAsFactors=FALSE)
TMXA=sqlQuery(x,"SELECT * FROM TMCxA;",believeNRows=FALSE,rows_at_time=1,stringsAsFactors=FALSE)
TASI=sqlQuery(x,"SELECT * FROM TAlimSin;",believeNRows=FALSE,rows_at_time=1,stringsAsFactors=FALSE)
TE10=sqlQuery(x,"SELECT * FROM TCMEQUA10;",believeNRows=FALSE,rows_at_time=1,stringsAsFactors=FALSE)
TM14=sqlQuery(x,"SELECT * FROM TMCXA14;",believeNRows=FALSE,rows_at_time=1,stringsAsFactors=FALSE)
TGRA=sqlQuery(x,"SELECT * FROM TGrupoA;",believeNRows=FALSE,rows_at_time=1,stringsAsFactors=FALSE)
TGRP=sqlQuery(x,"SELECT * FROM TGrupoP;",believeNRows=FALSE,rows_at_time=1,stringsAsFactors=FALSE)
TPPA=sqlQuery(x,"SELECT * FROM TPresPA;",believeNRows=FALSE,rows_at_time=1,stringsAsFactors=FALSE)
TM3T=sqlQuery(x,"SELECT * FROM MA3TipoA;",believeNRows=FALSE,rows_at_time=1,stringsAsFactors=FALSE)
TMCA=sqlQuery(x,"SELECT * FROM TMedCas;",believeNRows=FALSE,rows_at_time=1,stringsAsFactors=FALSE)
TMAQ=sqlQuery(x,"SELECT * FROM EVISMaq;",believeNRows=FALSE,rows_at_time=1,stringsAsFactors=FALSE)
TN14=sqlQuery(x,"SELECT * FROM NMGDA14;",believeNRows=FALSE,rows_at_time=1,stringsAsFactors=FALSE)
#TCAP=sqlQuery(x,"SELECT * FROM TCAPE ORDER BY NumAlim;",believeNRows=FALSE,rows_at_time=1,stringsAsFactors=FALSE)
#TMCW=sqlQuery(x,"SELECT * FROM TMCxAg ORDER BY NumAlim,CodMC;",believeNRows=FALSE,rows_at_time=1)
#TAN3=sqlQuery(x,"SELECT * FROM TANDREA2003 ORDER BY NUMALIM;",believeNRows=FALSE,rows_at_time=1,stringsAsFactors=FALSE)
odbcClose(x)
TWAE=TWAE%>%mutate(CodMC=RPCP(CodMC))
TMXA=TMXA%>%mutate(CodMC=RPCP(CodMC))
TE10=TE10%>%mutate(CODMC=RPCP(CODMC))
TM14=TM14%>%mutate(CODMC=RPCP(CODMC),UEQ=RPCP(ifelse(trimws(UEQ)=="NA",NA,UEQ)))
TMCA=TMCA%>%mutate(CodMC=RPCP(CodMC))
TFCC=TFCC%>%mutate(CONDF=as.numeric(CONDF))
# read mappings
xMA1=read_excel(path=paste0(NTABS,"NuTabsMaps.xlsx"),sheet="Padres")
P1=xMA1%>%filter(is.na(NUMALIM2)|NUMALIM2==NUMALIM1)%>%rename(NUMALIM=NUMALIM1)%>%mutate(NUMALIMP="")	# list of not-synonyms=parents
f=function(NumA){
x=NA
if(!is.na(NumA)){
	s=xMA1%>%filter(NUMALIM1==NumA)
	if(nrow(s)==1){
		y=s$NUMALIM2[1]
		if(NumA%in%P1$NUMALIM){
			x=NumA
		}else{
			x=f(y)
		}
	}
}
return(x)
}
g=Vectorize(f)
xMA1=xMA1%>%mutate(NUMALIMP=g(NUMALIM1))%>%rename(NUMALIM=NUMALIM1)
# builds TCAPE, consolidated working food composition table
# pools TCENAN20XX, TANDREA2003, synonims TAlimSin from TCENAN20XX, added CNMCA14 and TCEVET2016
TCAP=
	bind_rows(
			cEC8%>%
			rename(
				ZNMG=ZincZNmg,
				TFTE=FTEC,
				Alimento=NOMALIM
			)%>%
			left_join(y=xCPC%>%rename(CPCOM=PCOM),by="NUMALIM")%>%
			left_join(y=xGCP%>%rename(GCTAL=CTAL,GCOND=COND,GPCOM=PCOM),by="NUMALIM")%>%
			left_join(y=xN16,by="NUMALIM")%>%
			left_join(y=xN20,by="NUMALIM")%>%
			mutate(
				CTALIM=ifelse(!is.na(GCTAL),GCTAL,CTAL),
				COND=ifelse(!is.na(COND20),COND20,COND),
				PCOM=
					case_when(
						!is.na(PCOM16)~PCOM16,
						is.na(PCOM16)&!is.na(GPCOM)~as.integer(GPCOM),
						is.na(PCOM16)&is.na(GPCOM)&!is.na(CPCOM)~as.integer(CPCOM*100),
						TRUE~as.integer(100)
					),
				Retinolug=ifelse(VITAmg>((CARTBQmg/6)+Retinolug),VITAmg,((CARTBQmg/6)+Retinolug)),
				RetinolRAE=ifelse(VITAmg>((CARTBQmg/6)+Retinolug),VITAmg,((CARTBQmg/12)+Retinolug)), 
				PYRmg=NA,
				COBug=NA,
				VIENED=NUMALIM
			)%>%
			dplyr::select(
				NUMALIM,CTALIM,COND,PCOM,
				ENERC,PROCNTg,FATg,CHOCDFg,FIBTGg,
				HierroFEmg,Retinolug,RetinolRAE,ZNMG,CalcCAmg,VITCmg,THIAmg,RIBFmg,NIAmg,AFolug,PYRmg,COBug,
				VIENED,TFTE,Alimento 
			)
		,
			TAN3%>%
			rename(
				NUMALIM   =NumAlim,
				ENERC     =ENERGC,
				PROCNTg   =PROTG,
				FATg      =FATG,
				CHOCDFg   =CHOG,
				FIBTGg    =FIBERDG,
				HierroFEmg=FEMG,
				CalcCAmg  =CAMG,
				VITCmg    =VITCMG,
				THIAmg    =B01THMG,
				RIBFmg    =B02RBMG,
				NIAmg     =B03NIMG,
				AFolug    =B09FOMG,
				PYRmg     =B06PXMG,
				COBug     =B12COUG
			)%>%
			left_join(y=xCPC%>%rename(CPCOM=PCOM),by="NUMALIM")%>%
			left_join(y=xGCP%>%rename(GCTAL=CTAL,GCOND=COND,GPCOM=PCOM),by="NUMALIM")%>%
			mutate(
				CTALIM=trimws(ifelse(!is.na(GCTAL),GCTAL,CTP)),
				COND=ifelse(!is.na(GCOND),GCOND,as.integer(COPR)),
				PCOM=
					case_when(
						!is.na(GPCOM)~as.integer(GPCOM),
						is.na(GPCOM)&!is.na(CPCOM)~as.integer(CPCOM*100),
						is.na(GPCOM)&is.na(CPCOM)&PCOM!=(-1)~as.integer(PCOM),
						TRUE~as.integer(100)
					),
				Retinolug=
					case_when(
						!is.na(VAREUG)~VAREUG,
						is.na(VAREUG)&!is.na(VAREIU)~VAREIU*0.3,
						is.na(VAREUG)&is.na(VAREIU)~RETUG+0.167*BCAUG+0.084*(CADUG+CARUG),
						TRUE~as.numeric(NA)
					),
				RetinolRAE=
					case_when(
						!is.na(VAREUG)~VAREUG,
						is.na(VAREUG)&!is.na(VAREIU)~VAREIU,
						is.na(VAREUG)&is.na(VAREIU)~RETUG+BCAUG/12+(CADUG+CARUG)/24,
						TRUE~as.numeric(NA)
					),
				TFTE='AN03',
				VIENED=NUMALIM,
				Alimento=trimws(substr(DSC,1,100))
			)%>%
			dplyr::select(
				NUMALIM,CTALIM,COND,PCOM,
				ENERC,PROCNTg,FATg,CHOCDFg,FIBTGg,
				HierroFEmg,Retinolug,RetinolRAE,ZNMG,CalcCAmg,VITCmg,THIAmg,RIBFmg,NIAmg,AFolug,PYRmg,COBug,
				VIENED,TFTE,Alimento 
			)
		,
			cEC8%>%
			rename(
				NUMALIMD=NUMALIM,
				CTALIM=CTAL,
				ZNMG=ZincZNmg,
				Alimento=NOMALIM
			)%>%
			inner_join(y=TASI%>%filter(ParaComp=='1'),by="NUMALIMD")%>%
			left_join(y=xCPC%>%rename(CPCOM=PCOM,NUMALIMD=NUMALIM),by="NUMALIMD")%>%
			mutate(
				NUMALIM=NUMALIMO,
				PCOM=ifelse(!is.na(CPCOM),CPCOM*100,100),
				Retinolug=ifelse(VITAmg>((CARTBQmg/6)+Retinolug),VITAmg,((CARTBQmg/6)+Retinolug)),
				RetinolRAE=ifelse(VITAmg>((CARTBQmg/6)+Retinolug),VITAmg,((CARTBQmg/12)+Retinolug)), 
				PYRmg=NA,
				COBug=NA,
				TFTE='TS01',
				VIENED=NUMALIMD
			)%>%
			dplyr::select(
				NUMALIM,CTALIM,COND,PCOM,
				ENERC,PROCNTg,FATg,CHOCDFg,FIBTGg,
				HierroFEmg,Retinolug,RetinolRAE,ZNMG,CalcCAmg,VITCmg,THIAmg,RIBFmg,NIAmg,AFolug,PYRmg,COBug,
				VIENED,TFTE,Alimento 
			)
		,
			x14c%>%
			rename(
				CTALIM=CTAL,
				CHOCDFg=CHOg,
				HierroFEmg=FEmg,
				ZNMG=ZNmg,
				Retinolug =Retug,
				RetinolRAE=RetRAE
			)%>%
			left_join(y=x14n%>%rename(NUMALIM=CODALIM),by="NUMALIM")%>%
			mutate(
				NUMALIM=as.character(NUMALIM),
				CalcCAmg=NA, 
				VITCmg=NA, 
				THIAmg=NA, 
				RIBFmg=NA, 
				NIAmg=NA, 
				AFolug=NA, 
				PYRmg=NA, 
				COBug=NA, 
				TFTE='NUE4',
				VIENED=as.character(NUMALIM),
				Alimento=trimws(substr(NOMBRE,1,100))
			)%>%
			dplyr::select(
				NUMALIM,CTALIM,COND,PCOM,
				ENERC,PROCNTg,FATg,CHOCDFg,FIBTGg,
				HierroFEmg,Retinolug,RetinolRAE,ZNMG,CalcCAmg,VITCmg,THIAmg,RIBFmg,NIAmg,AFolug,PYRmg,COBug,
				VIENED,TFTE,Alimento 
			)
		,
			xE16%>%
			rename(
				CTALIM    =CTAL,
				ENERC     =ENERKC,
				PROCNTg   =PROTGR,
				FATg      =GRASGR,
				CHOCDFg   =CARBGR,
				FIBTGg    =FIBRGR,
				HierroFEmg=HIERMG,
				ZNMG      =ZINCMG
			)%>%
			mutate(
				COND=4,
				PCOM=100,
				Retinolug=case_when(!is.na(VITAUG)~VITAUG,is.na(VITAUG)&!is.na(VITAUI)~VITAUI*0.3,TRUE~as.numeric(NA)),
				RetinolRAE=case_when(!is.na(VITAUG)~VITAUG,is.na(VITAUG)&!is.na(VITAUI)~VITAUI*0.3,TRUE~as.numeric(NA)),
				CalcCAmg=NA, 
				VITCmg=NA, 
				THIAmg=NA, 
				RIBFmg=NA, 
				NIAmg=NA, 
				AFolug=NA, 
				PYRmg=NA, 
				COBug=NA, 
				TFTE='EV16',
				VIENED=NUMALIM,
				Alimento=trimws(substr(NOMALIM,1,100))
			)%>%
			dplyr::select(
				NUMALIM,CTALIM,COND,PCOM,
				ENERC,PROCNTg,FATg,CHOCDFg,FIBTGg,
				HierroFEmg,Retinolug,RetinolRAE,ZNMG,CalcCAmg,VITCmg,THIAmg,RIBFmg,NIAmg,AFolug,PYRmg,COBug,
				VIENED,TFTE,Alimento 
			)
	)%>%
	mutate(
		VITCmg=ifelse(VITCmg<0,NA,VITCmg),
		CTALIM=
			case_when(
				NUMALIM%in%c("P0024","P0025","P0027","P0028","P0031")~"A",
				NUMALIM%in%c("P0012","P0013","P0030","P0043","P0049","P0050","P0051")~"B",
				NUMALIM%in%c("P0009","P0046","P0047")~"C",
				NUMALIM%in%c("L0039")~"D",
				NUMALIM%in%c("P0004","P0005")~"E",
				NUMALIM%in%c("P0003","P0016","P0029","P0040","P0044","P0045","P0052")~"F",
				NUMALIM%in%c("L0005")~"H",
				NUMALIM%in%c("P0014")~"L",
				NUMALIM%in%c("P0011","P0018","P0019","P0020","P0021","P0022","P0023","P0035","P0036")~"T",
				NUMALIM%in%
					c(
						"P0010","P0001","P0002","P0006","P0007","P0008","P0015","P0017","P0026",
						"P0032","P0033","P0034","P0037","P0038","P0039","P0041","P0042","P0048"
					)~"U",
				TRUE~CTALIM
			)
	)%>%
	mutate(CTALIM=trimws(CTALIM))
# 2018-Dec-17 rectifying old P class
#TCAP$NUMALIM=as.character(TCAP$NUMALIM)
#TFCC$NUMALIM=as.character(TFCC$NUMALIM)
#TAN3$NumAlim=as.character(TAN3$NumAlim)
#TMXA$NUMALIM=as.character(TMXA$NUMALIM)
#TE10$NUMALIM=as.character(TE10$NUMALIM)
#TM14$NUMALIM=as.character(TM14$NUMALIM)
#TWAE$CodMC=as.character(TWAE$CodMC)
#TMXA$CodMC=as.character(TMXA$CodMC)
#TE10$CODMC=as.character(TE10$CODMC)
#TM14$CODMC=as.character(TM14$CODMC)
#TASI$NUMALIMO=as.character(TASI$NUMALIMO)
#TASI$NUMALIMD=as.character(TASI$NUMALIMD)
#TM14$EQDEN=as.character(TM14$EQDEN)
#TMCW$NumAlim=as.character(TMCW$NumAlim)
#TMCW$CodMC=as.character(TMCW$CodMC)
}
# ------------------------------------------------------------------------------------------------------------------------ #
# Nutritional Requirements Tables
if(TRUE){
# FAO/WHO/UNU (FWU) Tables
# ------------------------------------------------------------------------------------------------------------------------ #
# Energy
FET0502=
read.table(text="
1  0  3  162 0.249 -0.127 0.292 59.512 -30.4  70
1  3 10  338 0.095  2.110 0.280 22.706 504.3  67
1 10 18  734 0.074  2.754 0.441 17.686 658.2 105
1 18 30 2879 0.063  2.896 0.641 15.057 692.2 153
1 30 60  646 0.048  3.653 0.700 11.472 873.1 167
1 60 99   50 0.049  2.459 0.686 11.711 587.7 164
2  0  3  137 0.244 -0.130 0.246 58.317 -31.1  59
2  3 10  413 0.085  2.033 0.292 20.315 485.9  70
2 10 18  575 0.056  2.898 0.466 13.384 692.6 111
2 18 30  829 0.062  2.036 0.497 14.818 486.6 119
2 30 60  372 0.034  3.538 0.465  8.126 845.6 111
2 60 99   38 0.038  2.755 0.451  9.082 658.5 108
",
col.names=c("SEXG","AGEB","AGEF","NSAM","BJD","AJD","SJD","BCD","ACD","SCD")
)	# WHO 2001 Tab 5.2
FET0503=
read.table(text="
1.70,1.99,'Active or moderately active'
1.40,1.69,'Sedentary or light activity'
2.00,2.40,'Vigorous or vigorously active'
",
sep=",",col.names=c("PALL","PALU","DSC"),stringsAsFactors=FALSE
)	# WHO 2001 Tab 5.3
FET0503$PAL=c(2,1,3)
FET0503$PALM=c(1.85,1.55,2.20)	#(FET0503$PALL+FET0503$PALU)/2
# Protein
FPT31X=data.frame(
	AGM=c(1:4,6),
	ARQ=c(1.41,1.23,1.13,1.07,0.98),
	SRQ=c(1.77,1.5,1.36,1.24,1.14)
)	# WHO 2007 TR935 T31 under 6 months
#x=c(1.41,1.77,1.23,1.50,1.13,1.36,1.07,1.24,0.98,1.14)
FPT33A=data.frame(
	AGY=c(0.5,1,1.5,2,3,4,5,6,7,8,9,10),
	ARQ=c(1.12,0.95,0.85,0.79,0.73,0.69,0.69,0.72,0.74,0.75,0.75,0.75),
	SRQ=c(1.31,1.14,1.03,0.97,0.9,0.86,0.85,0.89,0.91,0.92,0.92,0.91)
)	# WHO 2007 TR935 T33a up to 10 years
FPT33B=data.frame(
	SEX=c(rep(1,8),rep(2,8)),
	AGY=c(11:18,11:18),
	ARQ=c(0.73,0.72,0.71,0.7,0.69,0.68,0.67,0.66,0.75,0.74,0.73,0.72,0.72,0.71,0.7,0.69),
	SRQ=c(0.9,1.89,1.88,0.87,0.85,0.84,0.83,0.82,0.91,0.9,0.9,0.89,0.88,0.87,0.86,0.85)
)	# WHO 2007 TR935 T33b adolescents
FPT46=data.frame(
	WKG=seq(40,80,5),
	RXD=c(33,37,42,46,50,54,58,62,66)
)	# WHO 2007 TR935 T46 adults, not loaded
FPT46$RXK=FPT46$WKG*0.83
FPT48=data.frame(
	GRP=c("T1","T2","T3","L1","L2"),
	XRT=c(1,10,31,19,13),
	AJR=c(375,1200,1950,2800,1925),
	PER=c(0.04,0.11,0.23,0.11,0.11)
)	# WHO 2007 TR935 T48 pregnant and lactating
# . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  #
# Iron
FHT135=data.frame(
	GRP=c("NI","NI","NI","NI","M","M","M","PREMQ","POSTMQ","POSTMQ","POSTMQ","POSTMP","LACT"),
	AGB=c(0.5,1,4,7,11,15,18,11,11,15,18,0,0),
	AGE=c(1,3,6,10,14,17,64,14,14,17,64,99,99),
	MBW=c(9,13,19,28,45,64,75,46,46,56,62,62,62),
	B15=c(6.2,3.9,4.2,5.9,9.7,12.5,9.1,9.3,21.8,20.7,19.6,7.5,10),
	B12=c(7.7,4.8,5.3,7.4,12.2,15.7,11.4,11.7,27.7,25.8,24.5,9.4,12.5),
	B10=c(9.3,5.8,6.3,8.9,14.6,18.8,13.7,14,32.7,31,29.4,11.3,15),
	B05=c(18.6,11.6,12.6,17.8,29.2,37.6,27.4,28,65.4,62,58.8,22.6,30),
	T50=c(0.72,0.46,0.50,0.71,1.17,1.50,1.05,1.20,1.68,1.62,1.46,0.87,1.15),
	T95=c(0.93,0.58,0.63,0.89,1.46,1.88,1.37,1.40,3.27,3.10,2.94,1.13,1.50)
)	# WHO 2004 9241546123 T13.1 & T13.5
# Zinc
C12T1203=
read.table(text="
2    0    2  0 175 457 1067
1    0    2  0 200 514 1200
0    3    5  0  79 204  477
0    6   11  1  66 311  621
0    6   11  2 186 311  621
0   12   35  0 138 230  459
0   36   71  0 114 190  380
0   72  119  0  90 149  299
2  120  143  0  68 113  227
1  120  143  0  80 133  267
2  144  179  0  64 107  215
1  144  179  0  76 126  253
2  180  215  0  56 93   187
1  180  215  0  61 102  205
2  216 1200  0  36 59   119
1  216 1200  0  43 72   144
",
col.names=c("SEXG","AGEBM","AGEFM","BREASTF","AZNMGKH","AZNMGKM","AZNMGKL")
)	# WHO 2004 Tab 12.3
C12T1204=
read.table(text="
0    0    6  0 0    6 1.1  2.8  6.6
0    7   12  0 1    9 0.8  4.1  8.4
0    7   12  0 2    9 2.5  4.1  8.4
0   12   47  0 0   12 2.4  4.1  8.3
0   48   83  0 0   17 2.9  4.8  9.6
0   84  119  0 0   25 3.3  5.6 11.2
2  120  227  0 0   47 4.3  7.2 14.4
1  120  227  0 0   49 5.1  8.6 17.1
2  228  779  0 0   55 3.0  4.9  9.8
1  228  779  0 0   65 4.2  7.0 14.0
2  780 1200  0 0   55 3.0  4.9  9.8
1  780 1200  0 0   65 4.2  7.0 14.0
2    0    2  1 0    0 3.4  5.5 11.0
2    3    5  1 0    0 4.2  7.0 14.0
2    6   10  1 0    0 6.0 10.0 20.0
2    0    2  2 0    0 5.8  9.5 19.0
2    3    5  2 0    0 5.3  8.8 17.5
2    6   12  2 0    0 4.3  7.2 14.4
",
col.names=c("SEXG","TIMBM","TIMFM","PRGLAC12","BREASTF","PESOKG","RZNMGH","RZNMGM","RZNMGL")
)	# WHO 2004 Tab 12.4
# Calcium
C04T0402=
read.table(text="
0   0    6 1 0 0  300
0   0    6 2 0 0  400
0   7   11 0 0 0  400
0  12   47 0 0 0  500
0  48   83 0 0 0  600
0  84  119 0 0 0  700
0 120  227 0 0 0 1300
2 228 1200 0 2 0 1000
2 228 1200 0 1 0 1300
1 228  779 0 0 0 1000
1 780 1200 0 0 0 1300
2   0    5 0 0 1 1000
2   6   10 0 0 1 1200
2   0 1200 0 0 2 1000
",
col.names=c("SEXG","AGEBM","AGEFM","BREASTF","POSTME","PRGLAC12","RDACAMG")
)	# WHO 2004 Tab 4.2
# . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  #
# Vitamin A
C02T0204=
read.table(text="
0   0    6 0 180 375
0   7   11 0 190 400
0  12   47 0 200 400
0  48   83 0 200 450
0  84  119 0 250 500
0 120  227 0 365 600
2 228  779 0 270 500
2 780 1200 0 300 600
1 228  779 0 300 600
1 780 1200 0 300 600
2 228 1200 1 370 800
2 228 1200 2 450 850
",
col.names=c("SEXG","AGEBM","AGEFM","PRGLAC12","MRRETUG","SARETUG")
)	# WHO 2004 Tab 2.4
# adolescents 10–18 years 330–400 600
# Vitamin C
C07T0701=
read.table(text="
0   0    6 0  25
0   7   11 0  30
0  12   47 0  30
0  48   83 0  30
0  84  119 0  35
0 120  227 0  40
0 228  779 0  45
0 780 1200 0  45
2 228 1200 1  55
2 228 1200 2  70
",
col.names=c("SEXG","AGEBM","AGEFM","PRGLAC12","RNIVCAMG")
)	# WHO 2004 Tab 7.1
# Vitamins B: Thiamine, Riboflavine, Niacine, Pyridoxine
C09T09XX=
read.table(text="
0    0    6  0  0.2 0.3  2 0.1
0    7   11  0  0.3 0.4  4 0.3
0   12   47  0  0.5 0.5  6 0.5
0   48   83  0  0.6 0.6  8 0.6
0   84  119  0  0.9 0.9 12 1.0
2  120  227  0  1.1 1.0 16 1.2
1  120  227  0  1.2 1.3 16 1.3
2  228  611  0  1.1 1.1 14 1.3
1  228  611  0  1.2 1.3 16 1.3
2  612 1200  0  1.1 1.1 14 1.5
1  612 1200  0  1.2 1.3 16 1.7
2  228 1200  1  1.4 1.4 18 1.9
2  228 1200  2  1.5 1.6 17 2.0
",
col.names=c("SEXG","AGEBM","AGEFM","PRGLAC12","RB01MG","RB02MG","RB03MG","RB06MG")
)	# WHO 2004 Tab 9.2, 9.3, 9.4, 9.5
# Vitamins B: Folic Acid & Cobalamine
C1XT1415=
read.table(text="
0    0    6  0   65  80 0.3 0.4
0    7   11  0   65  80 0.6 0.7
0   12   47  0  120 150 0.7 0.9
0   48   83  0  160 200 1.0 1.2
0   84  119  0  250 300 1.5 1.8
0  120  227  0  330 400 2.0 2.4
0  228  779  0  320 400 2.0 2.4
0  780 1200  0  320 400 2.0 2.4
2  228 1200  1  520 600 2.2 2.6
2  228 1200  2  450 500 2.4 2.8
",
col.names=c("SEXG","AGEBM","AGEFM","PRGLAC12","EARB09MG","RNIB09MG","EARB12UG","RNIB12UG")
)	# WHO 2004 Tab 15.1, 14.1
C1XT1415$UPPB09MG=1000
# ------------------------------------------------------------------------------------------------------------------------ #
# DRI Tables ### pending
# ------------------------------------------------------------------------------------------------------------------------ #
# Food Condition Codes (from MONIN0.SQL, DicCodigo.pgta='PESCO')
# 1  , 'neto'
# 2  , 'bruto'
# Food Preparation Codes (from MONIN0.SQL, DicCodigo.pgta='PRECO')
# 2  , 'crudo'
# 8  , 'sancochado'
# 1  , 'asado'
# 3  , 'frito'
# 4  , 'industrial'
# 5  , 'horn/brasa'
# 6  , 'natural'
# 7  , 'preparado'
# 9  , 'tostado'
# 10 , 'remojado'
# 11 , 'pasado'
# ------------------------------------------------------------------------------------------------------------------------ #
}
# ------------------------------------------------------------------------------------------------------------------------ #
# Nutritional Requirements Functions
RQFWUE=function(s,e,w,b=0,p=2,u="y"){	# Energy FWU
	# Input:
	# 	u: unit of age (y, m)
	# 	e: age, in the specified units, can be fractional
	# 	s: sex (1 male, 2 female)
	# 	w: body weight in Kg (use actual or ideal as per your norms)
	#	b: currently breast-feeding (1 yes 0 no)
	#	p: physical activity level (FWU 1 lo 2 mid 3 hi)
	# Output:
	# 	R: energy requirement in kcal/d
	R=NA
	if(!(is.na(s))&!is.na(e)&!is.na(w)){
		if(u=="m"){e=e/12}
		if(e<18){
			m=e*12
			JXG04 = 
				ifelse(m>=0&m<12,
					ifelse(s==1,34.11911 - 6.44484*m + 0.25404*m^2 + 0.01462*m^3,
					ifelse(s==2,29.84784 - 1.73696*m - 0.54623*m^2 + 0.05044*m^3,NA))
				,NA)
			GXD04 = 
				ifelse(m>=0&m<12,
					ifelse(s==1,39.3731000 - 7.6805000*m + 0.6214000*m^2 - 0.0166000*m^3,
					ifelse(s==2,30.7807782 - 4.2441646*m + 0.1903513*m^2 - 0.0004533*m^3,NA))
				,ifelse(m>=12&m<216,
					ifelse(s==1,6.712 - 0.05364*e - 0.1467*e^2 + 0.02070*e^3 + 0.0003983*e^4 - 0.00006237*e^5,
					ifelse(s==2,7.188 + 0.57320*e - 0.8486*e^2 + 0.19070*e^3 - 0.0138600*e^4 + 0.00031710*e^5,NA))
				,NA))
			EDF04 = 
				ifelse(m>=0&m<12,
					( JXG04 / 1000 ) * GXD04
				,ifelse(m>=12&m<216,
					GXD04 * 8.6 / 1000.0
				,NA))
			TEF04 = 
				ifelse(w>0,
					ifelse(m>=0&m<12,
						ifelse(b==1,
							( 0.388 * w ) - 0.635
						,ifelse(b==0,
							( 0.346 * w ) - 0.122
						,
							( 0.371 * w ) - 0.416
						))
					,ifelse(m>=12&m<216,
						ifelse(s==1,
							1.298 + 0.265*w - 0.0011*w^2
						,ifelse(s==2,
							1.102 + 0.273*w - 0.0019*w^2
						,NA))
					,NA))
				,NA)
			TEF04 = TEF04 * ifelse(m>=12&m<24,( 1.0 - 0.07 ),1.0)
			R=(EDF04+TEF04)*239.006
		}else{
			x=with(FET0502,which(SEXG==s&AGEB<=e&e<AGEF))
			if(length(x)==1){
				R=(FET0502$AJD[x]+w*FET0502$BJD[x])*FET0503$PALM[FET0503$PAL==p]*239.006
			}
		}
	}
	return(R)
}
vRQFWUE=Vectorize(RQFWUE)
RQFWUP=function(m,s,w,g="NO"){	# Protein FWU
	# Input:
	# 	m: months of age, can be fractional
	# 	s: sex (1 male, 2 female)
	# 	w: body weight in Kg (use actual or ideal as per your norms)
	#	g: women status, NO not P/L, Tx pregnant trimester x, Lx lactating semester (2 is 2+)
	#	o: output 1: requirement, 2: limit, 3, both
	# Output:
	# 	Vector of one or two elements: requirement and upper safety limit
	A=NA;S=NA;X=0
	if(!(is.na(m))&!is.na(s)&!is.na(w)){
	if(m<6){
		x=findInterval(m,FPT31X$AGM)
		A=with(FPT31X,ARQ[x]+(ARQ[x+1]-ARQ[x])*(m-AGM[x])/(AGM[x+1]-AGM[x]))
		S=with(FPT31X,SRQ[x]+(SRQ[x+1]-SRQ[x])*(m-AGM[x])/(AGM[x+1]-AGM[x]))
	}else if(m>=6&m<120){
		a=m/12
		x=findInterval(a,FPT33A$AGY)
		A=with(FPT33A,ARQ[x]+(ARQ[x+1]-ARQ[x])*(a-AGY[x])/(AGY[x+1]-AGY[x]))
		S=with(FPT33A,SRQ[x]+(SRQ[x+1]-SRQ[x])*(a-AGY[x])/(AGY[x+1]-AGY[x]))
	}else if(m>=120&m<216){
		a=m/12
		u=rbind(
			FPT33A[FPT33A$AGY==10,],
			FPT33B[FPT33B$SEX==s,!names(FPT33B)=="SEX"]
		)
		x=findInterval(a,u$AGY)
		A=with(u,ARQ[x]+(ARQ[x+1]-ARQ[x])*(a-AGY[x])/(AGY[x+1]-AGY[x]))
		S=with(u,SRQ[x]+(SRQ[x+1]-SRQ[x])*(a-AGY[x])/(AGY[x+1]-AGY[x]))
	}else if(m>=216){
		A=0.831
	}
	if(s==2&!is.na(g)&g%in%c("T1","T2","T3","L1","L2")){
		X=FPT48$XRT[g==FPT48$GRP]
	}
	}
	return(c(w*A+X,w*S))
}
R1FWUP=function(m,s,w,g="NO"){return(RQFWUP(m,s,w,g="NO")[1])}
vRQFWUP=Vectorize(RQFWUP)
vR1FWUP=Vectorize(R1FWUP)
RQFWUH=function(a,s,g,D=NA,M="T",I="N"){ # Iron FWU 
	# Input:
	# 	a: years of age, can be fractional
	# 	s: sex (1 male, 2 female)
	# 	g: population group ("NI" child,"M" male,"PREMQ" girl,"POSTMQ" woman,"POSTMP" menopausal,"LACT" lactating)
	#	D: diet bioavailability, proportion between 0 and 1 if computed, integer in (5,10,12,15) if table
	#	M: mode for bioavailability (T: as reading table (only those D in table), Q: computed for any D)
	# Output:
	# 	Requirement value
	# Source: FWU 2004 (Workshop 1998) Tables 13.5 and 13.1
	B=NA
	if(!(is.na(a))&!is.na(s)&!is.na(g)){
	if(I=="N"){
		if(a>=0.5&a<11){
			x=FHT135[FHT135$GRP=="NI"&FHT135$AGB<=a&a<(FHT135$AGE+ifelse(FHT135$AGB==0.5,0,1)),]
		}else if(s==1){
			x=FHT135[FHT135$GRP=="M"&FHT135$AGB<=a&a<(FHT135$AGE+1),]
		}else{
			x=FHT135[FHT135$GRP==g&FHT135$AGB<=a&a<(FHT135$AGE+1),]
		}
	}else{
	}
	if(nrow(x)==1){
		if(!is.na(D)&M=="Q"&D>0&D<=1){B=x$T95/D}
		else if(!is.na(D)&M=="T"&D%in%c(5,10,12,15)){B=x[1,paste0("B",sprintf("%02d",D))]}
		else if(is.na(D)){B=x$B15}
	}
	}
	return(B)
	# pending: interpolation option
	# pending: fitted polynomial option
}
vRQFWUH=Vectorize(RQFWUH)
fREQFWU=function(WFILE=NA,XAGEM,XSXMF,XWTKG,XRWKG,XWABF,XWEBF,XENKC=NA,IDKEY){	# FAO/WHO/UNU 2001 Requirements, children
	# Vitamins and Minerals FAO/WHO 2004
	# Fat: FAO/WHO 2011 Tab 2.2 approximate segmented fit
	xWFILE=eval(parse(text=WFILE))
	xXAGEM=rlang::sym(XAGEM)	# age in months and fraction
	xXSXMF=rlang::sym(XSXMF)	# "M" male, "F" female
	xXWTKG=rlang::sym(XWTKG)	# actual body weight in Kg
	xXRWKG=rlang::sym(XRWKG)	# ideal body weight for height in Kg
	xXWABF=rlang::sym(XWABF)	# 1 any BF, 0 none
	xXWEBF=rlang::sym(XWEBF)	# 1 exclusive BF, 0 no
	#xXENKC=rlang::sym(XENKC)
	xIDKEY=rlang::syms(IDKEY)	# vector of primary key variables
	# file setting
	w=	xWFILE %>%
		filter(!(is.na(!!xXAGEM))&!(is.na(!!xXSXMF))&!(is.na(!!xXWTKG)))%>%
		select(one_of(c(IDKEY,XAGEM,XSXMF,XWTKG,XRWKG,XWABF,XWEBF)))%>%
		mutate(EDADY=(!!xXAGEM)*30.4375/365.25)
	# intermediate variables
	w=	w %>%
		mutate(
			JXG04 = 
				ifelse((!!xXAGEM)>=0&(!!xXAGEM)<12,
					ifelse((!!xXSXMF)=="M",34.11911 - 6.44484*(!!xXAGEM) + 0.25404*(!!xXAGEM)^2 + 0.01462*(!!xXAGEM)^3,
					ifelse((!!xXSXMF)=="F",29.84784 - 1.73696*(!!xXAGEM) - 0.54623*(!!xXAGEM)^2 + 0.05044*(!!xXAGEM)^3,NA))
				,NA),
			GXD04 = 
				ifelse((!!xXAGEM)>=0&(!!xXAGEM)<12,
					ifelse((!!xXSXMF)=="M",39.3731000 - 7.6805000*(!!xXAGEM) + 0.6214000*(!!xXAGEM)^2 - 0.0166000*(!!xXAGEM)^3,
					ifelse((!!xXSXMF)=="F",30.7807782 - 4.2441646*(!!xXAGEM) + 0.1903513*(!!xXAGEM)^2 - 0.0004533*(!!xXAGEM)^3,NA))
				,ifelse((!!xXAGEM)>=12&(!!xXAGEM)<216,
					ifelse((!!xXSXMF)=="M",6.712 - 0.05364*EDADY - 0.1467*EDADY^2 + 0.02070*EDADY^3 + 0.0003983*EDADY^4 - 0.00006237*EDADY^5,
					ifelse((!!xXSXMF)=="F",7.188 + 0.57320*EDADY - 0.8486*EDADY^2 + 0.19070*EDADY^3 - 0.0138600*EDADY^4 + 0.00031710*EDADY^5,NA))
				,NA))
		) %>%
		mutate(
			EDF04 = 
				ifelse((!!xXAGEM)>=0&(!!xXAGEM)<12,
					( JXG04 / 1000 ) * GXD04
				,ifelse((!!xXAGEM)>=12&(!!xXAGEM)<216,
					GXD04 * 8.6 / 1000.0
				,NA)),
			TEF04 = 
				ifelse((!!xXWTKG)>0,
					ifelse((!!xXAGEM)>=0&(!!xXAGEM)<12,
						ifelse((!!xXWEBF)==1,
							( 0.388 * (!!xXWTKG) ) - 0.635
						,ifelse((!!xXWABF)==0,
							( 0.346 * (!!xXWTKG) ) - 0.122
						,
							( 0.371 * (!!xXWTKG) ) - 0.416
						))
					,ifelse((!!xXAGEM)>=12&(!!xXAGEM)<216,
						ifelse((!!xXSXMF)=="M",
							1.298 + 0.265*(!!xXWTKG) - 0.0011*(!!xXWTKG)^2
						,ifelse((!!xXSXMF)=="F",
							1.102 + 0.273*(!!xXWTKG) - 0.0019*(!!xXWTKG)^2
						,NA))
					,NA))
				,NA),
			TEF04 = TEF04 * ifelse((!!xXAGEM)>=12&(!!xXAGEM)<24,( 1.0 - 0.07 ),1.0),
			EDF04 = EDF04 * 239.0,
			TEF04 = TEF04 * 239.0,
			TEFI4 = 
				ifelse((!!xXWTKG)>0,
					ifelse((!!xXAGEM)>=0&(!!xXAGEM)<12,
						ifelse((!!xXWEBF)==1,
							( 0.388 * (!!xXRWKG) ) - 0.635
						,ifelse((!!xXWABF)==0,
							( 0.346 * (!!xXRWKG) ) - 0.122
						,
							( 0.371 * (!!xXRWKG) ) - 0.416
						))
					,ifelse((!!xXAGEM)>=12&(!!xXAGEM)<216,
						ifelse((!!xXSXMF)=="M",
							1.298 + 0.265*(!!xXRWKG) - 0.0011*(!!xXRWKG)^2
						,ifelse((!!xXSXMF)=="F",
							1.102 + 0.273*(!!xXRWKG) - 0.0019*(!!xXRWKG)^2
						,NA))
					,NA))
				,NA),
			TEFI4 = TEFI4 * ifelse((!!xXAGEM)>=12&(!!xXAGEM)<24,( 1.0 - 0.07 ),1.0),
			TEFI4 = TEFI4 * 239.0
		)
	# requirements
	w=
		w %>%
		mutate(
			RDENER=EDF04+TEF04,
			RDENID=EDF04+TEFI4,
			RDPROT=NA,
			RDHIER=NA,
			RDREUG=
				ifelse((!!xXAGEM)>=  0&(!!xXAGEM)<  6,180,
				ifelse((!!xXAGEM)>=  6&(!!xXAGEM)< 12,190,
				ifelse((!!xXAGEM)>= 12&(!!xXAGEM)< 48,200,
				ifelse((!!xXAGEM)>= 48&(!!xXAGEM)< 84,200,
				ifelse((!!xXAGEM)>= 84&(!!xXAGEM)<120,250,
				ifelse((!!xXAGEM)>=120&(!!xXAGEM)<216,365,
				NA)))))),
			RSPROT=NA,
			RSHIER=NA,
			RSREUG=
				ifelse((!!xXAGEM)>=  0&(!!xXAGEM)<  6,375,
				ifelse((!!xXAGEM)>=  6&(!!xXAGEM)< 12,400,
				ifelse((!!xXAGEM)>= 12&(!!xXAGEM)< 48,400,
				ifelse((!!xXAGEM)>= 48&(!!xXAGEM)< 84,450,
				ifelse((!!xXAGEM)>= 84&(!!xXAGEM)<120,500,
				ifelse((!!xXAGEM)>=120&(!!xXAGEM)<216,600,
				NA)))))),
			RDZINC=(!!xXWTKG)*
					ifelse((!!xXSXMF)=="M"&(!!xXAGEM)>=0&(!!xXAGEM)<3,0.514,
					ifelse((!!xXSXMF)=="F"&(!!xXAGEM)>=0&(!!xXAGEM)<3,0.457,
					ifelse((!!xXAGEM)>=3&(!!xXAGEM)<6,0.204,
					ifelse((!!xXAGEM)>=6&(!!xXAGEM)<12,0.311,
					ifelse((!!xXAGEM)>=12&(!!xXAGEM)<36,0.230,
					ifelse((!!xXAGEM)>=36&(!!xXAGEM)<72,0.190,
					NA)))))),
			RSZINC=
					ifelse((!!xXAGEM)>=0&(!!xXAGEM)<6,2.8,
					ifelse((!!xXAGEM)>=6&(!!xXAGEM)<12,4.1,
					ifelse((!!xXAGEM)>=12&(!!xXAGEM)<48,4.1,
					ifelse((!!xXAGEM)>=48&(!!xXAGEM)<72,4.8,
					NA)))),
			RDIODO=ifelse((!!xXAGEM)>=0&(!!xXAGEM)<60,6.0*(!!xXWTKG),NA),
			RSIODO=
					ifelse((!!xXAGEM)>=0&(!!xXAGEM)<7,15.0,
					ifelse((!!xXAGEM)>=7&(!!xXAGEM)<12,15.0,
					ifelse((!!xXAGEM)>=12&(!!xXAGEM)<84,6.0,
					NA)))
		)%>%
		mutate(
			RDGRA3=(0.55-0.15*ifelse((!!xXAGEM)<24,(!!xXAGEM)/18,0.30))*RDENER/9.0,
		)
	for(i in 1:nrow(w)){
		a=as.numeric(w[i,XAGEM])
		s=as.character(w[i,XSXMF]);s=as.numeric(ifelse(s=="M",1,ifelse(s=="F",2,NA)))
		k=as.numeric(w[i,XWTKG])
		w$RDPROT[i]=with(w,RQFWUP(a,s,k)[1])
		w$RSPROT[i]=with(w,RQFWUP(a,s,k)[2])
		w$RDHIER[i]=with(w,RQFWUH(a/12,s,ifelse(s==1,"M",ifelse(a<180,"PREMQ","POSTMQ"))))
	}
	w$RSHIER=w$RDHIER
	w=w%>%select(-one_of(c(XAGEM,XSXMF,XWTKG,XRWKG,XWABF,XWEBF,"EDADY","JXG04","GXD04","EDF04","TEF04","TEFI4")))
	#		RDGRA3=(0.55-0.15*ifelse((!!xXAGEM)<24,(!!xXAGEM)/18,0.30))*(!!xXENKC)/9.0,
	#RSHIER = with(eV,ifelse(EdadM>=6,PesoKg * ( 1.2107+EdadM*((-0.03623)+EdadM*(0.0005829+EdadM*(-0.000002535))) ),NA))
	#		RDPROT=
	#			ifelse(IPCONLM==1&EdadM>=0&EdadM<6&PesoKg>0,
	#				(0.58+0.9558+EdadM*((-0.2807)+EdadM*(0.059+EdadM*(-0.0048))))*PesoKg
	#			,ifelse(((IPCONLM==0&EdadM>=0)|EdadM>=6)&EdadM<60&PesoKg>0,
	#				(0.66+0.7264+EdadM*((-0.05576)+EdadM*(0.002111+EdadM*(-0.00004621+EdadM*(0.0000005766+
	#				EdadM*((-0.000000003698)+EdadM*0.000000000009365))))))*PesoKg
	#			,NA)),
	#		RDHIER=ifelse(EdadM>=6&EdadM<12,6.2,ifelse(EdadM>=12&EdadM<48,3.9,ifelse(EdadM>=48&EdadM<84,4.2,NA))),
	#		RSPROT=RDPROT,
	#		RSPROT=PesoKg*(RSPROT*(0.5896*RSPROT+0.0184)+0.5715),
	return(w)
}
fREQDRI=function(WFILE=NA,XAGEM,XSXMF,XWTKG,XHTCM,IDKEY){	# USDA DRI 2005 Requirements, children
	#xXAGEM=rlang::sym(XAGEM)
	#xXSXMF=rlang::sym(XSXMF)
	#xXWTKG=rlang::sym(XWTKG)
	#xXHTCM=rlang::sym(XHTCM)
	#xIDKEY=rlang::syms(IDKEY)
	w=eval(expr=parse(text=WFILE))
	w=eval(expr=parse(text=paste0("w%>%rename(xXAGEM=",XAGEM,",xXSXMF=",XSXMF,",xXWTKG=",XWTKG,",xXHTCM=",XHTCM,",xIDKEY=",IDKEY,")")))
	# file setting
	w=
		w%>%
		mutate(EDADY=xXAGEM*30.4375/365.25)%>%
		na.omit()
	# intermediate variables
	w=
		w %>%
		mutate(
			RTEE = 
					ifelse(xXWTKG>0&xXAGEM>=0  &xXAGEM<36,89.0 * xXWTKG - 100.0,
					ifelse(xXWTKG>0&xXSXMF=="M"&xXAGEM>=36&xXAGEM<108, 88.5 - 61.9 * EDADY + 1.13 * ( 26.7*xXWTKG + 9.03 * xXHTCM ),
					ifelse(xXWTKG>0&xXSXMF=="F"&xXAGEM>=36&xXAGEM<108,135.3 - 30.8 * EDADY + 1.16 * ( 10.0*xXWTKG + 9.34 * xXHTCM ),NA))),
			REDEP = 
					ifelse(xXSXMF=="M"&xXAGEM>=2&xXAGEM<15,286.82288 - 61.56805*xXAGEM + 4.35284*xXAGEM^2 - 0.09531*xXAGEM^3,
					ifelse(xXSXMF=="F"&xXAGEM>=2&xXAGEM<15,251.82318 - 51.62261*xXAGEM + 3.64699*xXAGEM^2 - 0.08193*xXAGEM^3,
					ifelse(xXAGEM>=15 &xXAGEM<36,20.0,
					ifelse(xXAGEM>=36 &xXAGEM<108,20.0,NA))))
		)
	# requirements
	w=
		w %>%
		mutate(
			REDRI = RTEE + REDEP,
			RPDRI = xXWTKG*ifelse(xXAGEM>=0&xXAGEM<6,1.53,ifelse(xXAGEM>=6&xXAGEM<12,1.00,ifelse(xXAGEM>=12&xXAGEM<48,0.87,ifelse(xXAGEM>=48&xXAGEM<108,0.76,NA)))),
			UPDRI = xXWTKG*ifelse(xXAGEM>=7&xXAGEM<12,1.20,ifelse(xXAGEM>=12&xXAGEM<48,1.05,ifelse(xXAGEM>=48&xXAGEM<108,0.95,NA))),
			RGDRI = ifelse(xXAGEM>=0&xXAGEM<6,31.0,ifelse(xXAGEM>=6&xXAGEM<12,30.0,NA)),
			RFDRI = ifelse(xXAGEM>=0&xXAGEM<6,0.27,ifelse(xXAGEM>=6&xXAGEM<12,6.90,ifelse(xXAGEM>=12&xXAGEM<48,3.00,ifelse(xXAGEM>=48&xXAGEM<108,4.10,NA)))),
			UFDRI = ifelse(xXAGEM>=12&xXAGEM<48,7.0,ifelse(xXAGEM>=48&xXAGEM<108,10.0,NA)),
			RADRI = ifelse(xXAGEM>=0&xXAGEM<6,400.0,ifelse(xXAGEM>=6&xXAGEM<12,500.0,ifelse(xXAGEM>=12&xXAGEM<48,210.0,ifelse(xXAGEM>=48&xXAGEM<108,275.0,NA)))),
			UADRI = ifelse(xXAGEM>=12&xXAGEM<48,300.0,ifelse(xXAGEM>=48&xXAGEM<108,400.0,NA)),
			RZDRI = ifelse(xXAGEM>=0&xXAGEM<7,2.0,ifelse(xXAGEM>=7&xXAGEM<48,2.5,ifelse(xXAGEM>=48&xXAGEM<108,400.0,NA))),
			UZDRI = ifelse(xXAGEM>=7&xXAGEM<48,3.0,ifelse(xXAGEM>=48&xXAGEM<108,5.0,NA)),
			RIDRI = ifelse(xXAGEM>=0&xXAGEM<7,110.0,ifelse(xXAGEM>=7&xXAGEM<12,130.0,ifelse(xXAGEM>=12&xXAGEM<108,65.0,NA))),
			UIDRI = ifelse(xXAGEM>=12&xXAGEM<108,90.0,NA)
		)
	w=
		w %>%
		mutate(
			SPDRI=
				xXWTKG*sqrt((0.12*0.688)^2+case_when(
					xXAGEM>=0&xXAGEM<12~
						(0.43*1.72*0.182)^2,
					xXAGEM>=12&xXAGEM<(14*12)~
						(0.43*1.72*ifelse(xXAGEM>=12&xXAGEM<48,0.104,ifelse(xXAGEM>=48&xXAGEM<108,0.046,0.043)))^2,
					TRUE~as.numeric(NA)
				))
		)
	w=w%>%select(xIDKEY,REDRI,RPDRI,UPDRI,RGDRI,RFDRI,UFDRI,RADRI,UADRI,RZDRI,UZDRI,RIDRI,UIDRI,SPDRI)
	w=eval(expr=parse(text=paste0("w%>%rename(",IDKEY,"=xIDKEY)")))
	return(w)
}
# ------------------------------------------------------------------------------------------------------------------------ #
# Nutrient Intake (MONIN0.SQL:MakeVCons)
fTMCW=function(vQPrepar=NA,vQIngred=NA,vQConsum=NA,VIDNAL="NUMALIM",VIDMCA="CodMC",VIDKEY="",fDEB=FALSE){	# TMCW: TMCxAg
	# Purpose:
	# 	Build consolidated reference table of food (NUMALIM) x household measure (CODMC) combinations 
	# 	(default reference tables plus existing in optional dataset plus NUMALIM='M1028' water) with their weight equivalent.
	# Input:
	# 	All are optional. 
	# 	If data.frames and field names are specified, existing data food-measure combinations are added and fille at least with water-average equivalent.
	# 	If they are not specified, only reference tables are included in the consolidated output.
	# 	vQPrepar,vQIngred,vQConsum
	# 		Names of data frames for observed Recipes, Ingredients and Individual Intakes.
	# 		If anyone is null or empty string only reference tables included.
	# 	VIDKEY
	# 		Names of foreign key identifiers for ingredients to recipes (example: household, subject and family recipe).
	# 	VIDNAL, VIDMCA
	# 		see gQInCons function.
	# 	fDEB
	# 		If FALSE output is single data frame with consolidated table of food-measures, this is the default
	# 		If TRUE output is a list of two elements, first the consolidated data frame and second the union of partial sources
	# 	Input Environment as set by NuTabs.R. 
	# Output:
	# 	Data Frame (Tibble) with columns:
	# 	Key: Food and Measure Codes as defined by VIDNAL and VIDMCA.
	# 	Data: 
	# 		Gramos1	weight for food in net condition (without non-edible part)
	# 		Gramos2	weight for food in raw condition (with non-edible part)
	# 		Gramos3	weight for food in unspecified condition
	# 		GramosW	weight for average measure, only if low SD, regardless of condition 
	# 		Gramos 	weight for average measure, any SD, with edible-part correction for raw food
	# 	Count: NTMCW, number of items considered
	# Algorithm:
	# 	If all three data sources are specified, build a list d of all pairs {Food Code,Measure Code} in data as JD Source
	# 	Bind (Union) into a pooled o all pairs {Food Code,Measure Code} and their Condition and Weight from
	# 		. AN Source: Original ANDREA Food-Measures Tables TMXA 
	# 		. S1 Source: Foods of Equivalent Density(First List) TASI with their corresponding ANDREA data
	# 		. X1 Source: Equivalent Foods-Measures (Second List) TE10
	# 		. GR Source: All Foods (TCAP) with "GRAMOS" Measure Code and Equivalence 1
	# 		. S4 Source: Equivalent Foods (Third List) TM14, equivalent density for all measures, with their corresponding ANDREA data
	# 		. S5 Source: Equivalent Foods (Third List) TM14, available weights
	# 		. S6 Source: Equivalent Foods (Third List) TM14, equivalent density for specific measures, with their corresponding ANDREA data
	# 	If there is d, bind it to o, keep current o as v for debugging output
	# 	To each item in pooled o match the corresponding Edible Fraction for the Food-Measure from Food Table TCAP
	# 	To each item in pooled o match the corresponding Water Weigth for the Measure from a pooled-averaged weight from original ANDREA table
	# 		(NuTabs.mdb query TMCxA in "SELECT 'M1028' AS NUMALIM, CodMC, '0' AS TPESO, AvgPESO AS Gramos WHERE (SDPESO/AvgPESO)<0.35;"
	# 		and query APTMCcA which groups query APTMCxA a name transformation of original ANDREA table)
	# 	Group all items in pooled o by Food-Measure Codes and summarize available weights
	# Comments:
	# 	TPESO is 1 raw 2 net in ANDREA and in o; just the reverse of MONIN, EVAR and EVIS coding
	xIDNAL=rlang::sym(VIDNAL)
	xIDMCA=rlang::sym(VIDMCA)
	fd=!is.na(vQPrepar)&!is.na(vQIngred)&!is.na(vQConsum)
	if(fd){fd=fd&nchar(vQPrepar)>0&nchar(vQIngred)>0&nchar(vQConsum)>0}
	if(fd){
		xQPrepar=eval(parse(text=vQPrepar),envir=parent.frame())
		xQIngred=eval(parse(text=vQIngred),envir=parent.frame())
		xQConsum=eval(parse(text=vQConsum),envir=parent.frame())
		X1=xQConsum%>%filter((!!xIDNAL)!="00000"&nchar(!!xIDNAL)==5&(!!xIDMCA)!="000000"&!is.na((!!xIDNAL))&!is.na((!!xIDMCA)))%>%select(one_of(c(VIDNAL,VIDMCA)))
		X2=xQIngred%>%filter((!!xIDNAL)!="00000"&nchar(!!xIDNAL)==5&(!!xIDMCA)!="000000"&!is.na((!!xIDNAL))&!is.na((!!xIDMCA)))%>%select(one_of(c(VIDNAL,VIDMCA)))
		X3=xQConsum%>%inner_join(xQPrepar%>%select(one_of(VIDKEY)),VIDKEY)%>%
					  filter((!!xIDNAL)!="00000"&nchar(!!xIDNAL)==5&(!!xIDMCA)!="000000"&!is.na((!!xIDNAL))&!is.na((!!xIDMCA)))%>%select(one_of(c(VIDNAL,VIDMCA)))
		d=
			bind_rows(X1,X2,X3) %>%
			group_by(!!xIDNAL,!!xIDMCA) %>%
			mutate(TPESO=0,GRAMOS=NA,Fuente="JD")
		if(VIDNAL!="NUMALIM"){d=d%>%rename(NUMALIM=!!xIDNAL)}
		if(VIDMCA!="CodMC")  {d=d%>%rename(CodMC=!!xIDMCA)}
	}
	o=
		bind_rows(
			TMXA %>% mutate(GRAMOS=Gramos,Fuente="AN") %>% filter(NUMALIM!="M1028") %>% mutate(MNALIM=NUMALIM) %>% select(one_of(c("NUMALIM","CodMC","TPESO","GRAMOS","Fuente"))),
			TMXA %>% mutate(GRAMOS=Gramos,Fuente="S1") %>% select(one_of(c("NUMALIM","CodMC","TPESO","GRAMOS","Fuente"))) %>% rename(NUMALIMD=NUMALIM) %>%
				inner_join(TASI%>%filter(ParaMedC=="1")%>%select(one_of(c("NUMALIMO","NUMALIMD"))),c("NUMALIMD")) %>%
				rename(NUMALIM=NUMALIMO) %>% select(one_of(c("NUMALIM","CodMC","TPESO","GRAMOS","Fuente"))),
			TE10 %>% rename(CodMC=CODMC) %>% mutate(Fuente="X1") %>% select(one_of(c("NUMALIM","CodMC","TPESO","GRAMOS","Fuente"))),
			TCAP %>% mutate(CodMC="GRAMOS",TPESO=0,GRAMOS=1.0,Fuente="GR") %>% select(one_of(c("NUMALIM","CodMC","TPESO","GRAMOS","Fuente"))),
			TM14 %>% filter(!is.na(EQDEN)&is.na(UEQ)) %>% rename(CodMC=CODMC) %>% select(one_of(c("EQDEN","CodMC","NUMALIM"))) %>%
				inner_join(TMXA %>% rename(EQDEN=NUMALIM),c("EQDEN","CodMC")) %>%
				mutate(GRAMOS=Gramos,Fuente="S4") %>% select(one_of(c("NUMALIM","CodMC","TPESO","GRAMOS","Fuente"))),
			TM14 %>% filter(!is.na(GRAMOS)) %>% rename(CodMC=CODMC) %>% mutate(TPESO=0,Fuente="S5") %>% select(one_of(c("NUMALIM","CodMC","TPESO","GRAMOS","Fuente"))),
			TM14 %>% filter(!is.na(UEQ)) %>% rename(CodMC=CODMC) %>% select(one_of(c("EQDEN","UEQ","CodMC","NUMALIM"))) %>%
				inner_join(TMXA %>% rename(EQDEN=NUMALIM,UEQ=CodMC),c("EQDEN","UEQ")) %>%
				mutate(GRAMOS=Gramos,Fuente="S7") %>% select(one_of(c("NUMALIM","CodMC","TPESO","GRAMOS","Fuente"))) 
		)
	#		TMXA %>% mutate(GRAMOS=Gramos,Fuente="S6") %>% select(one_of(c("NUMALIM","CodMC","TPESO","GRAMOS","Fuente"))) %>% 
	#			inner_join(TM14%>%filter(!is.na(UEQ))%>%group_by(NUMALIM,CODMC)%>%summarise(MUEQ=max(as.character(UEQ),na.rm=TRUE)),c("NUMALIM","CodMC"="MUEQ")) %>%
	#			select(one_of(c("NUMALIM","CodMC","TPESO","GRAMOS","Fuente"))) 
	if(fd){o=bind_rows(o,d)}
	v=o
	o=
		o %>%
		left_join(TCAP%>%select(one_of(c("NUMALIM","PCOM"))),c("NUMALIM")) %>%
		left_join(
			TMXA %>%
			filter(NUMALIM=="M1028"&!is.na(CodMC)) %>%
			rename(GramosW=Gramos) %>%
			select(one_of(c("CodMC","GramosW"))),
			c("CodMC")
		) %>%
		group_by(NUMALIM,CodMC) %>%
		summarise(
			Gramos1=mean(ifelse(TPESO==2,GRAMOS,NA),na.rm=TRUE),
			Gramos2=mean(ifelse(TPESO==1,GRAMOS,NA),na.rm=TRUE),
			Gramos3=mean(ifelse(TPESO==0,GRAMOS,NA),na.rm=TRUE),
			GramosW=mean(GramosW,na.rm=TRUE),
			Gramos =mean(GRAMOS*ifelse(TPESO==1,PCOM/100,1.0),na.rm=TRUE),
			NTMCW  =n()
		)
	#	left_join(TAN3%>%select(one_of(c("NumAlim","PCOM"))),c("NUMALIM"="NumAlim")) %>%
	if(VIDNAL!="NUMALIM"){o=o%>%rename(!!xIDNAL:=NUMALIM)}
	if(VIDMCA!="CodMC")  {o=o%>%rename(!!xIDMCA:=CodMC)}
	if(!fDEB){return(o)}else{return(list(o,v))}
# %>%mutate(NumAlim=gsub(" ","",NumAlim))
#		TMXA %>% mutate(GRAMOS=Gramos,Fuente="S4") %>% select(one_of(c("NUMALIM","CodMC","TPESO","GRAMOS","Fuente"))) %>% 
#			inner_join(TM14%>%filter(!is.na(EQDEN))%>%group_by(NUMALIM,CODMC)%>%summarise(EQUIVD=max(EQDEN,na.rm=TRUE)),c("NUMALIM"="EQUIVD","CodMC"="CODMC")),
}
fQIiConS=function(vQCons0P,VIDNAL,VIDEQP,VIDEQA,VIDPRE,VCPRES,VCTPES,VCPESC,VIDMCO,VIDMCS,VCCANO,VCCANS,VCMOPO,VCMOPS,VIDKEY,FCCPC=1){	# QIiConS (QIndiConS, VQICS) Intakes that are Recipes, with computed quantities, also # QIiConC (QIndiConC, VQICC)
	# Purpose:
	# 	For a given intake dataset (individual intakes, ingredients or any subset or match) computes
	# 	the weights for household measures and applies adjustments for net/raw and cooked/raw status.
	# Input:
	# 	vQCons0P: Name of intake data frame.
	# 	VIDNAL to VCMOPS: see gQInCons function.
	# 	VIDKEY: Names of key identifiers for input data frame. It may include extra variables to keep in output.
	# 	FCCPC: If 1, apply no correction (i.e. 1) for cooked/raw status, 0 otherwise
	# 	Input Environment as set by NuTabs.R. 
	# Output:
	# 	Data Frame (Tibble) with columns:
	# 	Key: as specified by VIDKEY.
	# 	VIDPRE,VIDNAL,VIDMCO,VCTPES see gQInCons function.
	# 	Unids:  	net intake (offered-leftover) in household measures (which might be GRAMOS)
	# 	FCRaCO: 	raw-to-cooked conversion factor, as gotten from referral tables for the food code
	# 	PaCom:  	edible-fraction (raw-to-net conversion factor), as gotten from referral tables for the food code
	# 	Peso:   	net weight intake, in grams
	# 	QIItem: 	constant "00", to be deprecated
	# 	NumAlimO:	original food code (VIDNAL is replaced by equivalence used)
	# 	NumAlimEC:	referral food code, form TCAP composition table
	# Comments:
	# 	pending check if substraction is only about leftover, or offered has to be taken into account, port to MONIN0 (would affect EVAR, MONIN, VIN)
	xQCons0P=eval(parse(text=vQCons0P),envir=parent.frame())
	xVIDNAL=rlang::sym(VIDNAL)
	xVIDEQP=rlang::sym(VIDEQP)
	xVIDEQA=rlang::sym(VIDEQA)
	xVIDPRE=rlang::sym(VIDPRE)
	xVCPRES=rlang::sym(VCPRES)
	xVCTPES=rlang::sym(VCTPES)
	xVCPESC=rlang::sym(VCPESC)
	xVIDMCO=rlang::sym(VIDMCO)
	xVIDMCS=rlang::sym(VIDMCS)
	xVCCANO=rlang::sym(VCCANO)
	xVCCANS=rlang::sym(VCCANS)
	xVCMOPO=rlang::sym(VCMOPO)
	xVCMOPS=rlang::sym(VCMOPS)
	o=
		xQCons0P %>%
		left_join(
			TMCW%>%rename(TXGramos1=Gramos1,TXGramos2=Gramos2,TXGramos3=Gramos3,TXGramosW=GramosW,TXGramos=Gramos),
			setNames(c(VIDNAL,VIDMCO),c(VIDEQP,VIDMCO))
		) %>%
		left_join(
			TMCW%>%rename(TSGramos1=Gramos1,TSGramos2=Gramos2,TSGramos3=Gramos3,TSGramosW=GramosW,TSGramos=Gramos),
			setNames(c(VIDNAL,VIDMCO),c(VIDEQP,VIDMCS))
		) %>%
		left_join(TWAE,setNames("CodMC",VIDMCO)) %>%
		left_join(TCAP,setNames("NUMALIM",VIDEQA)) %>%
		left_join(TFCC,setNames(c("NUMALIM","CONDF"),c(VIDEQA,VCPRES))) %>%
		left_join(TDEN,setNames("NUMALIM",VIDEQA)) %>%
		mutate(
			Unids=(!!xVCCANO)-ifelse(is.na(!!xVCCANS),0,(!!xVCCANS)),
			FCRaCO=ifelse(FCCPC==1&rep(TRUE,nrow(xQCons0P)),1.0,ifelse(!is.na(FCRaCO)&FCRaCO>0,FCRaCO,1.0)),
			PaCom=ifelse(FCCPC==1&rep(TRUE,nrow(xQCons0P)),1.0,
				ifelse(PCOM>0&((!is.na((!!xVCPESC))&(!!xVCTPES)==1)|(is.na((!!xVCPESC))&(!!xVCTPES)==2&!is.na(TXGramos1)&!is.na(TXGramos2))),PCOM/100,1.0) *
				ifelse(PCOM>0&(is.na((!!xVCPESC))&(!!xVCTPES)==1&is.na(TXGramos1)&!is.na(TXGramos2)),100/PCOM,1.0)
			),
			TipPeso=!!xVCTPES,QIItem="00",
			NumAlimO=!!xVIDNAL,NumAlimEC=VIENED 
		) %>%
		mutate(
			!!xVIDNAL:=ifelse(is.na(!!xVIDEQA),(!!xVIDNAL),(!!xVIDEQA))
		)%>%
		mutate(
			Peso=
				(
				ifelse(!is.na((!!xVCPESC)),(!!xVCPESC),0) +
				((ifelse(is.na((!!xVCPESC))&!is.na((!!xVCCANO))&(is.na((!!xVIDMCO))|(!!xVIDMCO)=='000000'|(!!xVIDMCO)=='GRAMO '|(!!xVCMOPO)%in%c(1,3)),(!!xVCCANO),0)) *
				 ifelse((!!xVIDNAL)%in%c('G0012','M0848'),76.0,1.0))*
				 ifelse(!is.na(DENGXML)&(!!xVCMOPO)==3,DENGXML,1.0) +
				((ifelse(is.na((!!xVCPESC))&!is.na((!!xVCCANO))&!is.na((!!xVIDMCO))&(!!xVIDMCO)>'000000'&(!!xVIDMCO)!='GRAMO '&!(!!xVCMOPO)%in%c(1,3),(!!xVCCANO),0)) *
				(ifelse(!is.na(TXGramos1)&is.na(TXGramos2),TXGramos1,0) +
				ifelse(is.na(TXGramos1)&!is.na(TXGramos2),TXGramos2,0) +
				ifelse(!is.na(TXGramos1)&!is.na(TXGramos2)&(!!xVCTPES)==1,TXGramos1,0) +
				ifelse(!is.na(TXGramos1)&!is.na(TXGramos2)&(!!xVCTPES)==2,TXGramos2,0) +
				ifelse(is.na(TXGramos1)&is.na(TXGramos2)&!is.na(TXGramos3)&!(!!xVIDNAL)%in%c('G0012','M0848'),TXGramos3,0) +
				ifelse(is.na(TXGramos1)&is.na(TXGramos2)&is.na(TXGramos3)&!is.na(GramosWW)&!(!!xVIDNAL)%in%c('G0012','M0848'),GramosWW,0)
				))
				) - 
				((
				(ifelse(is.na((!!xVCPESC))&!is.na((!!xVCCANS))&!is.na((!!xVIDMCS))&((!!xVIDMCS)=='GRAMO '|(!!xVCMOPS)%in%c(1,3))&!is.na((!!xVCCANS))&(!!xVCCANS)>0,(!!xVCCANS),0))*
				  ifelse(!is.na(DENGXML)&(!!xVCMOPS)==3,DENGXML,1.0)+
				(ifelse(is.na((!!xVCPESC))&!is.na((!!xVCCANS))&!is.na((!!xVIDMCS))&(!!xVIDMCS)>'000000'&(!!xVIDMCS)!='GRAMO '&!(!!xVCMOPS)%in%c(1,3)&!is.na((!!xVCCANS))&(!!xVCCANS)>0,(!!xVCCANS),0)) *
				(ifelse(!is.na(TSGramos1)&is.na(TSGramos2),TSGramos1,0) +
				ifelse(is.na(TSGramos1)&!is.na(TSGramos2),TSGramos2,0) +
				ifelse(!is.na(TSGramos1)&!is.na(TSGramos2)&(!!xVCTPES)==1,TSGramos1,0) +
				ifelse(!is.na(TSGramos1)&!is.na(TSGramos2)&(!!xVCTPES)==2,TSGramos2,0) +
				ifelse(is.na(TSGramos1)&is.na(TSGramos2)&!is.na(TSGramos3)&!(!!xVIDNAL)%in%c('G0012','M0848'),TSGramos3,0) +
				ifelse(is.na(TSGramos1)&is.na(TSGramos2)&is.na(TSGramos3)&!is.na(GramosWW)&!(!!xVIDNAL)%in%c('G0012','M0848'),GramosWW,0)
				)
				))
		) %>%
		select(one_of(c(
			VIDKEY,VIDPRE,VIDNAL,VIDMCO,VCTPES,"Unids","FCRaCO","PaCom","Peso","QIItem","NumAlimO","NumAlimEC"
		)))
	return(o)
	#VCCONS,
	#xVCCONS=rlang::sym(VCCONS)
	#"QLugar","Consist","Presen","Tiempo"
	#Consist=!!xVCCONS,Presen=!!xVCPRES,
}
fQPrepss=function(vQPrepar=NA,vQIngPrp=NA,VIDNAL,VCVOTP,VCPEIN,VIDKEY=NA){	# QPrepss (QPreps) Recipes, with their total volumes
	xQPrepar=eval(parse(text=vQPrepar),envir=parent.frame())
	xQIngPrp=eval(parse(text=vQIngPrp),envir=parent.frame())
	xVIDNAL=rlang::sym(VIDNAL)
	xVCVOTP=rlang::sym(VCVOTP)
	xVCPEIN=rlang::sym(VCPEIN)
	xVIDKEY=rlang::syms(VIDKEY)
	o=
		xQPrepar %>%
		left_join(
			xQIngPrp %>% 
			group_by(!!!xVIDKEY) %>% 
			summarise(PTotal=sum((!!xVCPEIN),na.rm=TRUE),NIngOK=n()),
			c(VIDKEY)
		)%>%
		mutate(
			PesGR=PTotal,
			PTotal=ifelse(!is.na((!!xVCVOTP))&(!!xVCVOTP)>0,(!!xVCVOTP),PTotal)
		) %>%
		mutate(
			!!xVIDNAL:=as.character(ifelse(!is.na(!!xVIDNAL)&!(!!xVIDNAL)%in%c("00000","     "),(!!xVIDNAL),"M1028"))
		)%>%
		select(one_of(c(names(xQPrepar),"PTotal","NIngOK","PesGR")))
	return(o)
}
fQIngPrT=function(vQIngPrp=NA,vQPrepss=NA,VIDNAL,VIDKEY=NA){	# QIngPrT (QIngPrepT) Non-Excluded Ingredients of all recipes, with equivalences, set for Recipp Aggregation
	xQIngPrp=eval(parse(text=vQIngPrp),envir=parent.frame())
	xQPrepss=eval(parse(text=vQPrepss),envir=parent.frame())
	xVIDNAL=rlang::sym(VIDNAL)
	xVIDKEY=rlang::syms(VIDKEY)
	o=
		xQIngPrp%>%mutate(PesoI=Peso) %>%
		inner_join(
			xQPrepss %>%
			mutate(QPNumAlim=!!xVIDNAL) %>%
			select(-one_of(c(VIDNAL))),
			c(VIDKEY)
		) %>%
		select(one_of(c(names(xQIngPrp),"PesoI","PTotal","QPNumAlim")))
	return(o)
}
fQCons0P=function(vQConsu0=NA,vQPrepss=NA,VIDNAL,VIDPRE,VCPESC,VCCANO,VIDKEY=NA){	# QCons0P (QConsu0OPP) Intakes that are Recipes, with equivalences
	xQConsu0=eval(parse(text=vQConsu0),envir=parent.frame())
	xQPrepss=eval(parse(text=vQPrepss),envir=parent.frame())
	xVIDNAL=rlang::sym(VIDNAL)
	xVIDPRE=rlang::sym(VIDPRE)
	xVCPESC=rlang::sym(VCPESC)
	xVCCANO=rlang::sym(VCCANO)
	o=
		xQConsu0 %>%
		mutate(Peso=1) %>%
		inner_join(
			xQPrepss %>% mutate(EquivP=!!xVIDNAL) %>% select(one_of(c(VIDKEY,"EquivP"))),
			c(VIDKEY)
		) %>%
		select(one_of(names(xQConsu0),"EquivP"))
	return(o)
	#	filter(!is.na(!!xVIDPRE)&(is.na(!!xVIDNAL)|(!!xVIDNAL)%in%c('00000','     '))&((!!xVCPESC)>0|(!!xVCCANO)>0)) %>%
}
fQIiConP=function(vQIiConS,vQIngPrT,VIDITM,VCTPES,VIDNAL,VIDMCO,VIDPRE,VIDKEY,VIDOUT){	# QIiConP (QIndiConP, VQICP) Ingredient Expansion
	xQIiConS=eval(parse(text=vQIiConS),envir=parent.frame())
	xQIngPrT=eval(parse(text=vQIngPrT),envir=parent.frame())
	xVIDITM=rlang::sym(VIDITM)
	o=
		xQIiConS %>% select(-one_of(c(VIDNAL,"NumAlimO","NumAlimEC"))) %>%
		inner_join(
			xQIngPrT %>% select(one_of(c(VIDKEY,VIDNAL,VIDITM,"PesoI","PTotal","NumAlimO","NumAlimEC"))),
			VIDKEY
		) %>%
		mutate(
			FCRaCO=1.0,
			PaCom=1.0,
			QIPeso=as.numeric(PesoI),
			QPTotal=as.numeric(PTotal),
			QIItem=sprintf("%02d",(!!xVIDITM))
		) %>%
		select(one_of(c(
			VIDOUT,VIDPRE,VIDNAL,VIDMCO,VCTPES,"Unids","FCRaCO","PaCom","Peso","QIItem","NumAlimO","NumAlimEC","QIPeso","QPTotal"
		)))
	return(o)
	#		VIDKEY,VIDPRE,VIDNAL,VIDMCO,VCTPES,"Unids","FCRaCO","PaCom","Peso","QIItem","NumAlimO","NumAlimEC"
	#"QLugar","Tiempo","Consist","Presen"
}
fQInCons=function(vQIiConC=NA,vQIiConP=NA,VIDPRE=NA,VIDOUT=NA){	# QInCons (QIngCons) Consolidated Foods Taken
	xQIiConC=eval(parse(text=vQIiConC),envir=parent.frame())
	xQIiConP=eval(parse(text=vQIiConP),envir=parent.frame())
	xVIDPRE=rlang::sym(VIDPRE)
	o=
		union(
			xQIiConC %>%
			mutate(
				Peso=Peso*PaCom*FCRaCO,
				!!xVIDPRE:=ifelse((!!xVIDPRE)>'00',(!!xVIDPRE),'00'),
				Tipo="CON"
			) %>%
			select(one_of(VIDOUT))
			,
			xQIiConP %>%
			mutate(
				Peso=Peso*QIPeso/QPTotal,
				Tipo="PRE"
			) %>%
			select(one_of(VIDOUT))
		)
	return(o)
}
gQInCons=function(
	gDING=NA,	# Ingredients: data
	gCEXCL=NA,	# Ingredients: exclusion column (1 excluded)
	gCPRES=NA,	# Ingredients: presentation type of process (1 ... 2...) 
	gCTPES=NA,	# Ingredients: condition type of weight (1 net 2 raw)
	gCPESC=NA,	# Ingredients: quantity in grams
	gCCANO=NA,	# Ingredients: quantity in household units
	gCMOPO=NA,	# Ingredients: type of measure procedure (1 weight 2 household unit)
	gCITEM=NA,	# Ingredients: Row Id, should be part of pk of gDING
	gDPRP=NA,	# Recipes: data
	gPPRP=NA,	# Recipes: primary key
	gCVOTP=NA,	# Recipes: total volume of recipe
	gDCON=NA,	# Intakes: data
	gDCOP=NA,	# Intakes: data, recipes only
	gDCNP=NA,	# Intakes: data, non-recipes only
	gPCON=NA,	# Intakes: primary key
	gIPRES=NA,	# Intakes: presentation type of process (1 ... 2...)
	gITPES=NA,	# Intakes: condition type of weight (1 net 2 raw)
	gIPESO=NA,	# Intakes: (offered) quantity in grams
	gICANO=NA,	# Intakes: (offered) quantity in household units
	gIMOPO=NA,	# Intakes: (offered) type of measure procedure (1 weight 2 household unit)
	gICANS=NA,	# Intakes: (leftover) quantity in household units
	gIMOPS=NA,	# Intakes: (leftover) type of measure procedure (1 weight 2 household unit)
	gIDMCS=NA,	# Intakes: (leftover) Household Measurement Unit Code, Right Padded 6b
	gIDNAL=NA,	# Food Code, same name in all data files
	gIDPRE=NA,	# Recipe Code, same name in all data files
	gIDEQA=NA,	# Equivalent Density Food Code of an Ingredient or Food
	gIDEQP=NA,	# Equivalent Density Food Code of a Recipe
	gIDMCO=NA,	# Household Measurement Unit Code, Right Padded 6b, same name in all data files, used for offered
	gFDEB=FALSE	# output only final QIngCons file
){
	QIngPrp=NA;QPrepss=NA;QIngPrT=NA;QCons0P=NA;QIiConS=NA;QIiConP=NA;QIiConC=NA;QInCons=NA
	#QIngPrp=fQIngPrp(vQIngred=gDING,VIDNAL=gIDNAL,VIDEQA=gIDEQA,
	#	VCPRES=gCPRES,VCTPES=gCTPES,VCPESC=gCPESC,VIDMCO=gIDMCO,VCCANO=gCCANO,VCMOPO=gCMOPO,VCEXCL=gCEXCL )
	QIngPrp=fQIiConS(
		vQCons0P=paste0(
			gDING,"%>%mutate(",gIMOPS,"=",gCMOPO,",",gICANS,"=0)%>%",
			"filter((",gCEXCL,")!=1&(",gIDEQA,")!='00000'&((",gCPESC,")>0|(",gCCANO,")>0))"
		),
		VIDNAL=gIDNAL,VIDEQP=gIDEQA,VIDEQA=gIDEQA,VIDPRE=gIDPRE,VCPRES=gCPRES,VCTPES=gCTPES,VCPESC=gCPESC,
		VIDMCO=gIDMCO,VIDMCS=gIDMCO,VCCANO=gCCANO,VCCANS=gICANS,VCMOPO=gCMOPO,VCMOPS=gIMOPS,
		VIDKEY=eval(parse(text=paste0("names(",gDING,")","[!","names(",gDING,")","%in%c('",gIDPRE,"','",gIDNAL,"','",gIDMCO,"','",gCTPES,"')]"))),FCCPC=0
	)
	QPrepss=fQPrepss(vQPrepar=gDPRP,vQIngPrp="QIngPrp",VIDNAL=gIDNAL,VCVOTP=gCVOTP,VCPEIN="Peso",VIDKEY=gPPRP)
	QIngPrT=fQIngPrT(vQIngPrp="QIngPrp",vQPrepss="QPrepss",VIDNAL=gIDNAL,VIDKEY=gPPRP)
	QCons0P=fQCons0P(vQConsu0=gDCOP,vQPrepss="QPrepss",VIDNAL=gIDNAL,VIDPRE=gIDPRE,VCPESC=gIPESO,VCCANO=gICANO,VIDKEY=gPPRP)
	QIiConS=fQIiConS(
		vQCons0P="QCons0P",VIDNAL=gIDNAL,VIDEQP=gIDEQP,VIDEQA=gIDEQP,VIDPRE=gIDPRE,VCPRES=gIPRES,VCTPES=gITPES,VCPESC=gIPESO,
		VIDMCO=gIDMCO,VIDMCS=gIDMCS,VCCANO=gICANO,VCCANS=gICANS,VCMOPO=gIMOPO,VCMOPS=gIMOPS,VIDKEY=gPCON,FCCPC=1
	)
	QIiConP=fQIiConP(vQIiConS="QIiConS",vQIngPrT="QIngPrT",VIDITM=gCITEM,VCTPES=gITPES,VIDNAL=gIDNAL,VIDMCO=gIDMCO,VIDPRE=gIDPRE,VIDKEY=gPPRP,VIDOUT=gPCON)
	QIiConC=fQIiConS(
		vQCons0P=gDCNP,VIDNAL=gIDNAL,VIDEQP=gIDEQA,VIDEQA=gIDEQA,VIDPRE=gIDPRE,VCPRES=gIPRES,VCTPES=gITPES,VCPESC=gIPESO,
		VIDMCO=gIDMCO,VIDMCS=gIDMCS,VCCANO=gICANO,VCCANS=gICANS,VCMOPO=gIMOPO,VCMOPS=gIMOPS,VIDKEY=gPCON,FCCPC=0
	)%>%mutate(QIPeso=1.0,QPTotal=1.0)
	QInCons=fQInCons(
		vQIiConC="QIiConC",vQIiConP="QIiConP",VIDPRE=gIDPRE,
		VIDOUT=c(gPCON,gIDNAL,gIDPRE,"QIItem","Unids","Peso","Tipo","NumAlimO","NumAlimEC")
	)	# "CPOF","QIItem","Tiempo","Consist","Presen","QLugar",
	if(gFDEB){
		o=list(QIngPrp,QPrepss,QIngPrT,QCons0P,QIiConS,QIiConP,QIiConC,QInCons)
		names(o)=c("QIngPrp","QPrepss","QIngPrT","QCons0P","QIiConS","QIiConP","QIiConC","QInCons")
	}else{
		o=QInCons
	}
	return(o)
	#gCCONS=NA,	# Consistency (1 ... 2... ...), same name in all data files
}
# pending: check var exclusions and other 'fixed' arguments, watch case variation
# pending: check retinol,carb
# ------------------------------------------------------------------------------------------------------------------------ #
# Storage
if(FALSE){
save(
	RPCP,
	TCAP,TWAE,TFCC,TDEN,TAN3,TMXA,TASI,TE10,TM14,TGRA,TGRP,TM3T,TMCA,TMAQ,TN14,TPPA,
	TCAPd,
	FET0502,FET0503,
	FPT31X,FPT33A,FPT33B,FHT135,FPT48,
	C12T1203,C12T1204,C04T0402,C02T0204,C07T0701,C09T09XX,C1XT1415,
	RQFWUP,RQFWUP,RQFWUH,R1FWUP,
	fREQFWU,fREQDRI,fTMCW,fQIiConS,fQPrepss,fQIngPrT,fQCons0P,fQIiConP,fQInCons,gQInCons,
	vRQFWUE,vRQFWUH,vRQFWUP,vR1FWUP,
	file=paste0(DTABS,"NuTabs.rda")
)
# load(file=paste0(DTABS,"NuTabs.rda"))
}
# ------------------------------------------------------------------------------------------------------------------------ #
# Reports
if(FALSE){
f=function(i,d,h,C,n){
	openxlsx::addWorksheet(b,n)
	openxlsx::writeData(b,i,d,withFilter=TRUE,headerStyle=h)
	openxlsx::freezePane(b,sheet=i,firstActiveRow=2,firstActiveCol=C)
	openxlsx::setColWidths(b,sheet=i,cols=1:ncol(d),widths="auto")
}
TCAPd=
	TCAP%>%
	left_join(y=TPPA%>%rename(COND=PRES),by="COND")%>%
	left_join(
		y=
			bind_rows(
				TM3T%>%left_join(x=TGRA,by="CODGA")%>%rename(CTALIM=CTP,AGRUPO=GRUPO)%>%mutate(CTALIM=trimws(CTALIM)),
				TGRA%>%rename(AGRUPO=GRUPO)%>%mutate(CTALIM=trimws(CTAL),CODTP=NA)
			),
		by="CTALIM"
	)%>%
	left_join(y=xABP,by="NUMALIM")%>%
	left_join(y=xMA1,by="NUMALIM")%>%
	rename(
		PRES=COND
	)%>%
	mutate(
		NUMALIMP=ifelse(is.na(NUMALIMP),NUMALIM,NUMALIMP),
		DNPRES=DPRES,
		DNALIM=ifelse(!is.na(NABALIM),NABALIM,toupper(Alimento)),
		DSALIM=Alimento,
		DNGRUP=toupper(AGRUPO)
	)%>%
	filter(!TFTE=="TS01")%>%
	dplyr::select(
		NUMALIM, CTAL, CODGA, CTALIM, PRES, PCOM, TFTE, NUMALIMP, 
		ENERC, PROCNTg, FATg, CHOCDFg, FIBTGg, HierroFEmg, Retinolug, RetinolRAE, 
		ZNMG, CalcCAmg, VITCmg, THIAmg, RIBFmg, NIAmg, AFolug, PYRmg, COBug, 
		VIENED, DNALIM, DNPRES, DNGRUP 
	)
#	left_join(y=TGRA%>%rename(CTALIM=CTAL,GGRUPO=GRUPO)%>%mutate(CTALIM=trimws(CTALIM),GGRUPO=trimws(GGRUPO)),by="CTALIM")%>%
#		DNGRUP=toupper(ifelse(!is.na(GGRUPO),GGRUPO,ifelse(!is.na(AGRUPO),AGRUPO,NA)))
s="NuTabsDSC.xlsx"
h=openxlsx::createStyle(textDecoration="Bold")
b=openxlsx::createWorkbook(s)
f(i=1,d=TCAPd,h=h,C=2,n="TCAPEdsc")
f(i=2,d=TCAP, h=h,C=2,n="TCAPE")
f(i=3,d=TMCA, h=h,C=2,n="TMC")
f(i=4,d=xAXM, h=h,C=8,n="TAXM")
openxlsx::saveWorkbook(b,file=s,overwrite=TRUE)
}
# ------------------------------------------------------------------------------------------------------------------------ #
# Not in Use
fQEquiv0=function(vQInFile=NA,vQObFile=NA,VIDHOG="",VIDSUJ="",VT=NA){	# Foods with equivalent food codes, if they exist as comments
	# Purpose:
	#	Build equivalent column from structured comment (EVAR structure).
	# Output:
	# 	Data frame equal to vQInFile plus new extra column EquivA
	# 	which has the Equivalent food code as specified by the observation
	# 	or the original food code in the input file (maybe null in the origin).
	xQInFile=eval(parse(text=vQInFile),envir=parent.frame())
	xQObFile=eval(parse(text=vQObFile),envir=parent.frame())
	if(VT=="In"){
		xQInFile=xQInFile%>%mutate(Clave1=CVis,Clave2=paste(idItem,CodPrep,sep=""))
		NT="Ingredientes"
	}else if(VT=="Co"){
		xQInFile=xQInFile%>%mutate(Clave1=CVis,Clave2=IdItem)
		NT="Consumos"
	}
	o=
		xQInFile %>% 
		left_join(
			filter(xQObFile,nomTab==NT&nomCol=="EquivA"),
			c(VIDHOG,VIDSUJ,"Clave1","Clave2")
		) %>%
		mutate(EquivA=ifelse(!is.na(Obs),substr(Obs,1,5),NumAlim)) %>%
		select(-one_of(c("Clave1","Clave2","usuario","fIns","fRegis","nomTab","nomCol","Obs")))
	return(o)
}
fQIngPrp=function(vQIngred=NA,VIDNAL,VIDEQA,VCPRES,VCTPES,VCPESC,VIDMCO,VCCANO,VCMOPO,VCEXCL){	# QIngPrp (QIngPrep) Non-Excluded Ingredients with estimated equivalent quantities
	# Retired, ingredient count is now done by fQIiConS
	xQIngred=eval(parse(text=vQIngred),envir=parent.frame())
	xVIDNAL=rlang::sym(VIDNAL)
	xVIDEQA=rlang::sym(VIDEQA)
	xVCPRES=rlang::sym(VCPRES)
	xVCTPES=rlang::sym(VCTPES)
	xVCPESC=rlang::sym(VCPESC)
	xVIDMCO=rlang::sym(VIDMCO)
	xVCCANO=rlang::sym(VCCANO)
	xVCMOPO=rlang::sym(VCMOPO)
	xVCEXCL=rlang::sym(VCEXCL)
	#xVIDDAT=rlang::syms(VIDDAT)
	#xVIDKEY=rlang::syms(VIDKEY)
	#l=c(VIDKEY,VIDNAL,"NumAlimO","NumAlimEC","Peso",VCTPES,VCPRES)
	#if(!any(is.na(VIDDAT))){l=c(l,VIDDAT)}
	o=
		xQIngred %>%
		filter((!!xVCEXCL)!=1&(!!xVIDEQA)!='00000'&((!!xVCPESC)>0|(!!xVCCANO)>0)) %>%
		left_join(TMCW,setNames(c(VIDNAL,VIDMCO),c(VIDEQA,VIDMCO))) %>%
		left_join(TWAE,setNames("CodMC",VIDMCO)) %>%
		left_join(TCAP,setNames("NUMALIM",VIDEQA)) %>%
		left_join(TFCC,setNames(c("NUMALIM","CONDF"),c(VIDEQA,VCPRES))) %>%
		left_join(TDEN,setNames("NUMALIM",VIDEQA)) %>%
		mutate(
			NumAlimO=(!!xVIDNAL),
			!!xVIDNAL:=(!!xVIDEQA),
			NumAlimEC=VIENED,
			Peso=
				ifelse(!is.na(FCRaCO)&FCRaCO>0,FCRaCO,1.0) * 
				ifelse(PCOM>0&((!is.na((!!xVCPESC))&(!!xVCTPES)==1)|(is.na((!!xVCPESC))&(!!xVCTPES)==2&!is.na(Gramos1)&is.na(Gramos2))),PCOM/100,1.0) * 
				ifelse(PCOM>0&(is.na((!!xVCPESC))&(!!xVCTPES)==1&is.na(Gramos1)&!is.na(Gramos2)),100/PCOM,1.0) * 
				ifelse(!is.na(DENGXML)&(!!xVCMOPO)==3,DENGXML,1.0) * 
				(	ifelse(!is.na((!!xVCPESC)),(!!xVCPESC),0) +
					ifelse(is.na((!!xVCPESC))&!is.na((!!xVCCANO))&(!!xVIDMCO)=='GRAMO',(!!xVCCANO),0) +
					ifelse(is.na((!!xVCPESC))&!is.na((!!xVCCANO))&(!!xVIDMCO)>'000000'&(!!xVIDMCO)!='GRAMO',(!!xVCCANO),0)*
					(
						ifelse(!is.na(Gramos1)& is.na(Gramos2),Gramos1,0) +
						ifelse( is.na(Gramos1)&!is.na(Gramos2),Gramos2,0) +
						ifelse(!is.na(Gramos1)&!is.na(Gramos2)&(!!xVCTPES)==1,Gramos1,0) +
						ifelse(!is.na(Gramos1)&!is.na(Gramos2)&(!!xVCTPES)==2,Gramos2,0) +
						ifelse( is.na(Gramos1)& is.na(Gramos2)&!is.na(Gramos3),Gramos3,0) +
						ifelse( is.na(Gramos1)& is.na(Gramos2)&is.na(Gramos3)&!is.na(GramosWW)&!(!!xVIDNAL)%in%c('G0012','M0848'),GramosWW,0)
					)
				)
		)%>%
		select(one_of(c(names(xQIngred),"Peso","NumAlimO","NumAlimEC")))
	return(o)
}
# ------------------------------------------------------------------------------------------------------------------------ #
# Documentation
if(FALSE){
# References
# nutrition
# http://www.who.int/nutrition/publications/nutrient/en/
# programming
# https://stackoverflow.com/questions/26470465/enter-new-column-names-as-string-in-dplyrs-rename-function
# https://stackoverflow.com/questions/30382908/r-dplyr-rename-variables-using-string-functions
# https://stackoverflow.com/questions/3171426/compare-two-data-frames-to-find-the-rows-in-data-frame-1-that-are-not-present-in
# https://stackoverflow.com/questions/10978895/how-to-compare-two-dataframes
# https://stackoverflow.com/questions/19720488/faster-way-to-compare-rows-in-a-data-frame
# https://rappster.wordpress.com/2011/10/12/identifying-records-in-data-frame-a-that-are-not-contained-in-fata-frame-b-a-comparison/
# http://www.cookbook-r.com/Manipulating_data/Comparing_data_frames/
# http://jason.bryer.org/posts/2013-01-24/Comparing_Two_Data_Frames.html
# https://stackoverflow.com/questions/21208801/group-by-multiple-columns-in-dplyr-using-string-vector-input
# https://stackoverflow.com/questions/27688193/dplyrgroup-by-with-character-string-input-of-several-variable-names
# https://github.com/tidyverse/rlang/issues/116
# https://stackoverflow.com/questions/42612417/how-to-pass-multiple-column-names-as-input-to-group-by-in-dplyr
# https://stackoverflow.com/questions/21208801/group-by-multiple-columns-in-dplyr-using-string-vector-input
# https://stackoverflow.com/questions/44169505/grouping-on-multiple-programmatically-specified-vars-in-dplyr-0-6
# https://stackoverflow.com/questions/43415475/how-to-parametrize-function-calls-in-dplyr-0-7
# https://stackoverflow.com/questions/28399065/dplyr-join-on-by-a-b-where-a-and-b-are-variables-containing-strings
# https://stackoverflow.com/questions/21888910/how-to-specify-names-of-columns-for-x-and-y-when-joining-in-dplyr
# https://stackoverflow.com/questions/28125816/r-standard-evalation-for-join-dplyr
# https://stackoverflow.com/questions/38503960/dplyr-join-two-tables-within-a-function-where-one-variable-name-is-an-argument-t
# https://www.r-bloggers.com/data-frame-columns-as-arguments-to-dplyr-functions/
# https://stackoverflow.com/questions/7535412/create-a-numeric-vector-with-names-in-one-statement
# https://stackoverflow.com/questions/24459752/can-dplyr-package-be-used-for-conditional-mutating
# https://stackoverflow.com/questions/1335830/why-cant-rs-ifelse-statements-return-vectors
# http://tidyr.tidyverse.org/
# https://stackoverflow.com/questions/39066811/long-to-wide-with-dplyr
# https://stackoverflow.com/questions/38505035/r-converting-wide-format-to-long-format-with-multiple-3-time-period-variables
# https://stackoverflow.com/questions/10589693/convert-data-from-long-format-to-wide-format-with-multiple-measure-columns
# 
# Map
#          QPrepar         QIngred         QObserv         QConsum
#          |               |               |               |
#          |               +---------------+---------------+
#          |               |                               |
#          |               QIngre0                         QConsu0
#          |               |                               |
#
#          |               |                               |
#          |               QIngPrp                         |
#          |               |                               |
#          +---------------x                               |
#          |               |                               |
#          QPrepss         |                               |
#          |               |                               |
#          +---------------+                               |
#                          |                               |
#                          QIngPrT         +---------------#---------------+
#                          |               |                               |
#                          |               QCons0P                         |
#                          |               |                               |
#                          |               QIiConS                         |
#                          |               |                               |
#                          +-------X-------+                               |
#                                  |                                       |
#                                  QIiConP                                 QIiConC
#                                  |                                       |
#                                  +-------------------U-------------------+
#                                                      |
#                                                      QInCons
# 
}
# ------------------------------------------------------------------------------------------------------------------------ #

# ======================================================================================================================== #
# . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  #
# %&$@/\¿?()!¡^+-*/[]{}´;:.,áéíóúñ|~
# ------------------------------------------------------------------------------------------------------------------------ #
