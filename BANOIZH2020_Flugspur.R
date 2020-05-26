setwd("C:/Users/andyf/Desktop/DataR")   ###select where to save txt files

for(i in 1:nrow(X20190912_Dep_RWY28_LV95_AttTable)){    #loop that runs through every line of CSV file
#for(i in 1:3){
 
  
###create a file for every flight and add standard text to file. Total 327 files###
  
  standardtext="----------------------------------------------------------
Flugspur BA NOIZH 2020 Bisang und Gschwend
----------------------------------------------------------
BEGIN_TRACK"
  
  write.table(standardtext, file = paste0("Flugspur flight", i,".txt"), sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
 
   
###add second section to existing txt files###
  
  Track =paste0("Track:	Flight",X20190912_Dep_RWY28_LV95_AttTable$OBJECTID[i],"_",X20190912_Dep_RWY28_LV95_AttTable$SID[i],"_",X20190912_Dep_RWY28_LV95_AttTable$ICT[i])
  write.table(Track, file = paste0("Flugspur flight", i,".txt"),append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
  
  Procedure =paste0("Procedure:	",X20190912_Dep_RWY28_LV95_AttTable$Procedure[i])
  write.table(Procedure, file = paste0("Flugspur flight", i,".txt"),append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
  
  Offset="Offset:	A"
  write.table(Offset, file = paste0("Flugspur flight", i,".txt"),append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
  
  OffsetWeight="OffsetWeight:	---"
  write.table(OffsetWeight, file = paste0("Flugspur flight", i,".txt"),append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
  
  OffsetIndex ="OffsetIndex:	---"
  write.table(OffsetIndex, file = paste0("Flugspur flight", i,".txt"),append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
  
  Route =paste0("Route:	",X20190912_Dep_RWY28_LV95_AttTable$SID[i])
  write.table(Route, file = paste0("Flugspur flight", i,".txt"),append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)  
  
  Runway =paste0("Runway:	",X20190912_Dep_RWY28_LV95_AttTable$RWY[i])
  write.table(Runway, file = paste0("Flugspur flight", i,".txt"),append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)  
  
  
  
###Add header for matrix in third section###
  
  Head="x	y"
  write.table(Head, file = paste0("Flugspur flight", i,".txt"),append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
  
  
###create matrix with X,Y data and add to existing file   
  
  line=X20190912_Dep_RWY28_LV95_AttTable$WKT[i] #           save all content from row i in CSV to line
  
  cleanline=substr(line,16,nchar(line)-1) #                 get rid of unneeded characters in string "Linestring ZM" and ()
  
  m1=unlist(strsplit(cleanline,split=",")) #                split string by breaking at comma
  
  n1=unlist(strsplit(m1,split = " ")) #                     split string by breaking at space
  
  matrix1=matrix(unlist(n1), ncol = 4, byrow = TRUE) #      create matrix by adding X,Y,Z,t row by row
  
  sortedmatrix1=matrix1[,-c(3,4)] #                         rearrange and reduce columns to X,Y
  
 
  
###append final matrix to existing file
  
  finalmatrix=sortedmatrix1
  
  write.table(finalmatrix, file = paste0("Flugspur flight", i,".txt"), append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
  
  
###append end_track to last row
  
  EndTrack="End_Track"
  write.table(EndTrack, file = paste0("Flugspur flight", i,".txt"),append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)  
  
}


