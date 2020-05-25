setwd("C:/Users/andyf/Desktop/DataR")   ###select where to save txt files

for(i in 1:nrow(X20190912_Dep_RWY28_LV95_AttTable)){    #loop that runs through every line of CSV file
#for(i in 1:3){
 
  
###create a file for every flight and add standard text to it ,total 327 files###
  
  standardtext="----------------------------------------------------------
Flugprofil BA NOIZH Bisang und Gschwend
----------------------------------------------------------
BEGIN_PROFILE"
  
  write.table(standardtext, file = paste0("Flugprofil flight", i,".txt"), sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
 
   
###add second chapter to existing txt files###
  
  
  ProfileID =paste0("ProfileID:	Flight",X20190912_Dep_RWY28_LV95_AttTable$OBJECTID[i],"_",X20190912_Dep_RWY28_LV95_AttTable$SID[i],"_",X20190912_Dep_RWY28_LV95_AttTable$ICT[i])
  write.table(ProfileID, file = paste0("Flugprofil flight", i,".txt"),append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
  
  
  Procedure =paste0("Procedure:	",X20190912_Dep_RWY28_LV95_AttTable$Procedure[i])
  write.table(Procedure, file = paste0("Flugprofil flight", i,".txt"),append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE) 
  
  Offset="Offset:	A"
  write.table(Offset, file = paste0("Flugprofil flight", i,".txt"),append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
  
  Offsetweight="Offsetweight:	---"
  write.table(Offsetweight, file = paste0("Flugprofil flight", i,".txt"),append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
  
  AircraftType =paste0("AircraftType:	",X20190912_Dep_RWY28_LV95_AttTable$ICT[i])
  write.table(AircraftType, file = paste0("Flugprofil flight", i,".txt"),append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE) 
  
  Route =paste0("Route:	",X20190912_Dep_RWY28_LV95_AttTable$SID[i])
  write.table(Route, file = paste0("Flugprofil flight", i,".txt"),append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)  
  
  Runway =paste0("Runway:	",X20190912_Dep_RWY28_LV95_AttTable$RWY[i])
  write.table(Runway, file = paste0("Flugprofil flight", i,".txt"),append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)  
  
  
  
###Add heading for matrix in third chapter
  
  Head="disxy	z	v	rho	c	n1	flaps	gears	sb	phase"
  write.table(Head, file = paste0("Flugprofil flight", i,".txt"),append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
  
  
###create matrix with t,X,Y,Z data and add to existing file   
  
  line=X20190912_Dep_RWY28_LV95_AttTable$WKT[i] #           save full excel line from flight i to line
  
  cleanline=substr(line,16,nchar(line)-1) #                 get rid of unneeded charecters in string "Linestring ZM" and ()
  
  m1=unlist(strsplit(cleanline,split=",")) #                split string by breaking at comma
  
  n1=unlist(strsplit(m1,split = " ")) #                     split string by breaking at space
  
  matrix1=matrix(unlist(n1), ncol = 4, byrow = TRUE) #      create matrix by adding X,Y,T,t row by row
  
  sortedmatrix1=matrix1[,c(4,1,2,3)] #                      rearange columns to t,X,Y,Z
  
  prematrix=mapply(sortedmatrix1, FUN = as.numeric) #                   make values in matrix numeric
  
  mat=matrix(prematrix,length(prematrix)/4,4,byrow = FALSE) #           arrange to matrix again =)
  
###mat is now the matrix with all data from FZAG### Now we need to add calcualtions based on t,X,Y,Z  ###
    
  
###Add missing columns based on calcualtions from t,X,Y,Z###
  
  
###Disxy calcualted by sqrt(x^2+y^2)and add to line before
  
  disxyvec=c(0:(nrow(mat)-1))
  
  for(j in 2:length(disxyvec))
    disxyvec[j]=sqrt((mat[j,2]-mat[j-1,2])^2+(mat[j,3]-mat[j-1,3])^2)+disxyvec[j-1]
  
  disxymatrix=cbind(mat,disxyvec)
  

###Create z column by copying from mat and deduct RWY elevation
  
  zvec=c(1:nrow(mat))
  
  for(j in 1:length(zvec))
    zvec[j]=mat[j,4]-424
  
  zmatrix=cbind(disxymatrix,zvec)
  
  
###Formula for V in 3-d Space
  
  Vvec=c(0:(nrow(mat)-1))
  
  for(j in 2:length(Vvec))
    Vvec[j]=(sqrt((mat[j,2]-mat[(j-1),2])^2+(mat[j,3]-mat[(j-1),3])^2+(mat[j,4]-mat[(j-1),4])^2))/(mat[j,1]-mat[(j-1),1])
  
  Vmatrix=cbind(zmatrix,Vvec)
  
  
  ### rho=P/(R*T) in ISA, P from barometric formula, T from lapse rate -6.5K/1000m, R constant
  
  rhovec=c(1:nrow(mat))
  
  for(j in 1:length(rhovec))
    rhovec[j]=((101325)*(1-((0.0065*mat[j,4])/(288.15)))^5.255)/((287.05)*(288.15-(0.0065*mat[j,4])))
  
  rhomatrix=cbind(Vmatrix,rhovec)  
  
  
  ###c=sqrt(gamma*R*T), gamma=1.4, R=287.05, T from lapse rate -6.5K/1000m
  
  cvec=c(1:nrow(mat))
  
  for(j in 1:length(cvec))
    cvec[j]=sqrt(1.4*287.05*(288.15-(0.0065*mat[j,4])))
  
  cmatrix=cbind(rhomatrix,cvec)

    
  ###Inital 500m of climb @ flex rpm, after reduced rpm
  
  N1vec=c(1:nrow(mat))
  
  for(j in 1:length(N1vec))
    N1vec[j]=ifelse(mat[j,4]-mat[1,4]>=500,85,90)
  
  N1matrix=cbind(cmatrix,N1vec)  
  
  
  ###at T/O always remain retracted. 0=retracted, 1=extended
  
  FLAPSmatrix=cbind(N1matrix,-99) 
  
  
  ###inital 100m of climb with gear down, then gear up. 0=GearUP, 1=GearDOWN
  
  #GEARSvec=c(1:nrow(mat))
  
  #for(j in 1:nrow(mat))
    #GEARSvec[j]=ifelse(mat[j,4]-mat[1,4]<100,-99,0)
  
  GEARSmatrix=cbind(FLAPSmatrix,-99)
  
  
  ###at T/O always remain inactive. 0=inactive, 1=active
  
  SPEEDBREAKESmatrix=cbind(GEARSmatrix,-99)  
  
  
  ###if V smaller than 100m/s then 0, when faster it is 1
  
  #FLIGHTPHASEvec=c(1:nrow(mat))
  
  #for(j in 1:nrow(mat))
    #FLIGHTPHASEvec[j]=ifelse(Vmatrix[j,7]>=100,0,10)
  
  FLIGHTPHASEmatrix=cbind(SPEEDBREAKESmatrix,-99)  
  
  
  
###final matrix with all data and calculations is assembled
  
###cut the first 3 columns from existing matrix
  
  cutmatrix=FLIGHTPHASEmatrix[,-c(1,2,3,4)]
  
  
###round final matrix and append to existing file
  
  finalmatrix=cutmatrix
  
  write.table(finalmatrix, file = paste0("Flugprofil flight", i,".txt"), append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
  
  
  
  EndProfile="End_Profile"
  write.table(EndProfile, file = paste0("Flugprofil flight", i,".txt"),append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)  
  
}


