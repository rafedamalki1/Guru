/*LADDKON.p LADDAR konstruktion FRÅN .D FILER*/
DEFINE INPUT PARAMETER globforetag LIKE FORETAG.FORETAG NO-UNDO.
{AMERICANEUROPEAN.I}
   {muswait.i}
   IF  
   globforetag = "VAST"   THEN DO:
      INPUT FROM e:\delad\pro9\guru\konstval.d convert target "iso8859-1" source "iso8859-1". 
   END.   
   ELSE DO:
      INPUT FROM C:\Protemp11\konstval.d convert target "iso8859-1" source "iso8859-1".
   END.   
   REPEAT: 
      CREATE KONSTVAL.  
      ASSIGN.     
      IMPORT KONSTVAL.  
   END.
   INPUT CLOSE.                 

   IF  globforetag = "VAST"  THEN DO:
      INPUT FROM e:\delad\pro9\guru\mtrlber.d convert target "iso8859-1" source "iso8859-1". 
   END.   
   ELSE DO:
      INPUT FROM C:\Protemp11\mtrlber.d convert target "iso8859-1" source "iso8859-1".
   END.   
   REPEAT: 
      CREATE MTRLBER.  
      ASSIGN.     
      IMPORT MTRLBER.  
   END.
   INPUT CLOSE.
   {EUROPEANAMERICAN.I}   
