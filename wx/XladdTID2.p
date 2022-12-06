/*XLADTID2.P*/
/*LADDAR TID FRÅN .D FILER*/
DEFINE INPUT PARAMETER globforetag LIKE FORETAG.FORETAG NO-UNDO.

   {muswait.i}
   IF  
   globforetag = "VAST" OR  
    globforetag = "VNAT" THEN DO:
      INPUT FROM /guru/tidregit.d convert target "iso8859-1" source "iso8859-1". 
   END.    
   REPEAT: 
      CREATE TIDREGITAB.  
      ASSIGN.     
      IMPORT TIDREGITAB.  
   END.
   INPUT CLOSE.
   
   
   

      
