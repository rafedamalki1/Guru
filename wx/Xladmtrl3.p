MESSAGE "nu".
INPUT FROM /guru/mtrl3.d convert target "iso8859-1" source
"iso8859-1". 
REPEAT: 
   CREATE MTRL.  
   ASSIGN.     
   IMPORT MTRL.  
END.
INPUT CLOSE.
