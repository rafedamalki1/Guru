/* XobMISV.p*/

INPUT FROM c:\obtab.d convert target "iso8859-1" source "iso8859-1".
REPEAT:  
     
   CREATE  obtab.  
   ASSIGN.
   IMPORT obtab.
END.
INPUT FROM c:\obavtab.d convert target "iso8859-1" source "iso8859-1".
REPEAT:  
     
   CREATE  obavtab.  
   ASSIGN.
   IMPORT obavtab.
END.