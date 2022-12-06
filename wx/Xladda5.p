/*XLADDA5.P MARKVARDERING*/
{muswait.i} 
INPUT FROM /pro7/guru/kindex.d convert target "iso8859-1" source "iso8859-1" NO-ECHO.
REPEAT:
   CREATE KINDEX.
   ASSIGN.
   IMPORT KINDEX.
END.            
INPUT FROM /pro7/guru/markdiv.d convert target "iso8859-1" source "iso8859-1" NO-ECHO.
REPEAT:
   CREATE MARKDIV.
   ASSIGN.
   IMPORT MARKDIV.
END.      
INPUT FROM /pro7/guru/pakerreg.d convert target "iso8859-1" source "iso8859-1" NO-ECHO.
REPEAT:
   CREATE PAKERREG.
   ASSIGN.
   IMPORT PAKERREG.
END.     
INPUT FROM /pro7/guru/prodaker.d convert target "iso8859-1" source "iso8859-1" NO-ECHO.
REPEAT:
   CREATE PRODAKER.
   ASSIGN.
   IMPORT PRODAKER.
END.     
INPUT FROM /pro7/guru/vardaker.d convert target "iso8859-1" source "iso8859-1" NO-ECHO.
REPEAT:
   CREATE VARDAKER.
   ASSIGN.
   IMPORT VARDAKER.
END.     
INPUT FROM /pro7/guru/vardskog.d convert target "iso8859-1" source "iso8859-1" NO-ECHO.
REPEAT:
   CREATE VARDSKOG.
   ASSIGN.
   IMPORT VARDSKOG.
END.     
INPUT FROM /pro7/guru/volkr.d convert target "iso8859-1" source "iso8859-1" NO-ECHO.
REPEAT:
   CREATE VOLKR.
   ASSIGN.
   IMPORT VOLKR.
END.     
INPUT FROM /pro7/guru/volymber.d convert target "iso8859-1" source "iso8859-1" NO-ECHO.
REPEAT:
   CREATE VOLYMBER.
   ASSIGN.
   IMPORT VOLYMBER.
END.     
INPUT FROM /pro7/guru/vskogreg.d convert target "iso8859-1" source "iso8859-1" NO-ECHO.
REPEAT:
   CREATE VSKOGREG.
   ASSIGN.
   IMPORT VSKOGREG.
END.     
MESSAGE "ALLT KLART" VIEW-AS ALERT-BOX.
{musarrow.i}
