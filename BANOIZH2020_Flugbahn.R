setwd("C:/Users/andyf/Desktop/DataR")   ###select where to save txt files

for(i in 1:nrow(X20190912_Dep_RWY28_LV95_AttTable)){    #loop that runs through every line of CSV file
#for(i in 1:3){
 
  
###create a file for every flight and add standard text to file. Total 327 files###
  
  standardtext="Trajektorie für sonAIR
----------------------------------------------------------														
Flugbahn BA NOIZH Bisang und Gschwend													
Variablen-Informationen:														
Time [s]: Lokalzeit seit Mitternacht	X [m]: Metrische X-Koordinate (Rechtswert)	Y [m]: Metrische Y-Koordinate (Hochwert)	Z [m]: Höhe über MSL	V [m/s]: Bahngeschwindigkeit	Track-Angle [°]: Bahnazimuth	Climb Angle [°]: Bahnneigungswinkel	BankAngle [°]: Rollwinkel	Ma: Machzahl	Density [kg/m^3]: Dichte	N1[%]: Drehzahl Niederdruckwelle	Flaps: Klappenstellung (Flaps und Slats)	Gears: Fahrwerk ausgefahren (1)	Speedbrakes: Störklappen aktiv (1)	Flight phase: Flugphase
----------------------------------------------------------"
  
  write.table(standardtext, file = paste0("flight", i,".txt"), sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
 
   
###add second section to existing txt files###
  
  BID="BID:	---"
  write.table(BID, file = paste0("flight", i,".txt"),append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
  
  ZeitTDAB="Zeit TDAB:	---"
  write.table(ZeitTDAB, file = paste0("flight", i,".txt"),append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
  
  Flugzeugtyp =paste0("Flugzeugtyp (ICAO):	",X20190912_Dep_RWY28_LV95_AttTable$ICT[i])
  write.table(Flugzeugtyp, file = paste0("flight", i,".txt"),append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE) 
  
  Triebwerkstyp ="Triebwerkstyp:	---"
  write.table(Triebwerkstyp, file = paste0("flight", i,".txt"),append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE) 
  
  IMM ="IMM:	---"
  write.table(IMM, file = paste0("flight", i,".txt"),append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
  
  IcaoFlugnr =paste0("ICAO Flugnr:	",X20190912_Dep_RWY28_LV95_AttTable$CSG[i])
  write.table(IcaoFlugnr, file = paste0("flight", i,".txt"),append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE) 
  
  Flughafen ="Flughafen:	LSZH"
  write.table(Flughafen, file = paste0("flight", i,".txt"),append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE) 
  
  Destination =paste0("Destination:	",X20190912_Dep_RWY28_LV95_AttTable$IORIDS[i])
  write.table(Destination, file = paste0("flight", i,".txt"),append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
  
  Procedure =paste0("Procedure:	",X20190912_Dep_RWY28_LV95_AttTable$Procedure[i])
  write.table(Procedure, file = paste0("flight", i,".txt"),append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
  
  RWY =paste0("RWY:	",X20190912_Dep_RWY28_LV95_AttTable$RWY[i])
  write.table(RWY, file = paste0("flight", i,".txt"),append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
  
  Route =paste0("Route:	",X20190912_Dep_RWY28_LV95_AttTable$SID[i])
  write.table(Route, file = paste0("flight", i,".txt"),append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
  
  Subroute ="Subroute:	---"
  write.table(Subroute, file = paste0("flight", i,".txt"),append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE) 
  
  MTOW ="MTOW:	---"
  write.table(MTOW, file = paste0("flight", i,".txt"),append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE) 
  
  ATOW ="ATOW:	---"
  write.table(ATOW, file = paste0("flight", i,".txt"),append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE) 
  
  Zabs ="Zabs:	1"
  write.table(Zabs, file = paste0("flight", i,".txt"),append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE) 
  
  
###Add header for matrix in third section
  
  Head="
Time	X	Y	Z	V	TrackAngle	ClimbAngle	BankAngle	Ma	Density	N1	Flaps	Gears	Speedbrakes	FlightPhase"
  write.table(Head, file = paste0("flight", i,".txt"),append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
  
  
###create matrix with t,X,Y,Z data and add to existing file   
  
  line=X20190912_Dep_RWY28_LV95_AttTable$WKT[i] #           save all content from row i in CSV to line
  
  cleanline=substr(line,16,nchar(line)-1) #                 get rid of unneeded characters in string "Linestring ZM" and ()
  
  m1=unlist(strsplit(cleanline,split=",")) #                split string by breaking at comma
  
  n1=unlist(strsplit(m1,split = " ")) #                     split string by breaking at space
  
  matrix1=matrix(unlist(n1), ncol = 4, byrow = TRUE) #      create matrix by adding X,Y,Z,t row by row
  
  sortedmatrix1=matrix1[,c(4,1,2,3)] #                      rearrange columns to t,X,Y,Z
  
  prematrix=mapply(sortedmatrix1, FUN = as.numeric) #                   make values in matrix numeric
  
  mat=matrix(prematrix,length(prematrix)/4,4,byrow = FALSE) #           arrange to matrix again
  
###mat is now the matrix with all data from FZAG.
  
###Add missing columns based on calculations with given values t,X,Y,Z###

    
###Formula for V in 3-d Space
  
  Vvec=c(0:(nrow(mat)-1))
  
  for(j in 2:length(Vvec))
    Vvec[j]=(sqrt((mat[j,2]-mat[(j-1),2])^2+(mat[j,3]-mat[(j-1),3])^2+(mat[j,4]-mat[(j-1),4])^2))/(mat[j,1]-mat[(j-1),1])
  
  Vmatrix=cbind(mat,Vvec)
  
  
###According to formula for angle between vectors in 2D. A-vector always points North, A=(0,1), B=(Xnow-before, Ynow-Ybefore), If AC Westbound then 360°-alpha, if AC eastbound then alpha
  
  TrackANGLEvec=c(0:(nrow(mat)-1))
  
  for(j in 2:length(TrackANGLEvec))
    TrackANGLEvec[j]=ifelse(mat[j,2]-mat[(j-1),2]<=0,360-(acos((mat[j,3]-mat[(j-1),3])/sqrt((mat[j,2]-mat[(j-1),2])^2+(mat[j,3]-mat[(j-1),3])^2))*180/pi),(acos((mat[j,3]-mat[(j-1),3])/sqrt((mat[j,2]-mat[(j-1),2])^2+(mat[j,3]-mat[(j-1),3])^2))*180/pi))
  
  TRACKANGLEmatrix=cbind(Vmatrix,TrackANGLEvec)
  
  
###According to formula for angle between vectors in 3D. Az always remains 0 and therefore parallel to ground. Ax,Ay,Bx,By,Bz calculated by subtracting previous from current position
  
  CLIMBANGLEvec=c(0:(nrow(mat)-1))
  
  for(j in 2:length(CLIMBANGLEvec))
    CLIMBANGLEvec[j]=ifelse((mat[j,4]-mat[(j-1),4])==0,0,180/pi*acos(((mat[j,2]-mat[(j-1),2])^2+(mat[j,3]-mat[(j-1),3])^2)/(sqrt((mat[j,2]-mat[(j-1),2])^2+(mat[j,3]-mat[(j-1),3])^2)*sqrt((mat[j,2]-mat[(j-1),2])^2+(mat[j,3]-mat[(j-1),3])^2+(mat[j,4]-mat[(j-1),4])^2))))

  CLIMBANGLEmatrix=cbind(TRACKANGLEmatrix,CLIMBANGLEvec)
  
  
###No feasible solution found so far. Major effort for minor benefit. Therefore aircraft does not bank.
  
  BANKANGLEmatrix=cbind(CLIMBANGLEmatrix,0)
  
  
###M=u/c. u=velocity AC, c=sqrt(gamma*R*T), gamma=1.4, R=287.05, T from lapse rate -6.5K/1000m
  
  MAvec=c(1:nrow(mat))
  
  for(j in 1:nrow(mat))
    MAvec[j]=Vmatrix[j,5]/(sqrt(1.4*287.05*(288.15-(0.0065*mat[j,4]))))
  
  MAmatrix=cbind(BANKANGLEmatrix,MAvec)
  
  
### rho=P/(R*T) in ISA, P from barometric formula, T from lapse rate -6.5K/1000m, R constant
  
  DENSITYvec=c(1:nrow(mat))
  
  for(j in 1:nrow(mat))
    DENSITYvec[j]=((101325)*(1-((0.0065*mat[j,4])/(288.15)))^5.255)/((287.05)*(288.15-(0.0065*mat[j,4])))
  
  DENSITYmatrix=cbind(MAmatrix,DENSITYvec)
  
  
###Initial 500m of climb @ flex rpm, after reduced rpm
  
  N1vec=c(1:nrow(mat))
  
  for(j in 1:nrow(mat))
    N1vec[j]=ifelse(mat[j,4]-mat[1,4]>=500,85,90)
  
  N1matrix=cbind(DENSITYmatrix,N1vec)
  
  
###at T/O always remain retracted. 0=retracted, 1=extended
  
  FLAPSmatrix=cbind(N1matrix,0)
  
  
###initial 100m of climb with gear down, then gear up. 0=GearUP, 1=GearDOWN
  
  GEARSvec=c(1:nrow(mat))
  
  for(j in 1:nrow(mat))
    GEARSvec[j]=ifelse(mat[j,4]-mat[1,4]<100,1,0)
  
  GEARSmatrix=cbind(FLAPSmatrix,GEARSvec)
  
  
###at T/O always remain inactive. 0=inactive, 1=active
  
  SPEEDBREAKESmatrix=cbind(GEARSmatrix,0)
  
  
###if V smaller than 100m/s then 0, when faster it is 1
  
  FLIGHTPHASEvec=c(1:nrow(mat))
  
  for(j in 1:nrow(mat))
    FLIGHTPHASEvec[j]=ifelse(Vmatrix[j,5]>=100,1,0)
  
  FLIGHTPHASEmatrix=cbind(SPEEDBREAKESmatrix,FLIGHTPHASEvec)
  
  
###final matrix with all data and calculations is assembled
  
  
###round final matrix and append to existing file
  
  finalmatrix=format(round(FLIGHTPHASEmatrix,digits = 4))
  
  write.table(finalmatrix, file = paste0("flight", i,".txt"), append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
  
}


