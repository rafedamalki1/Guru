
/* XAVTALJSBF.P las ut ett visst avtal*/
{AMERICANEUROPEAN.I}
/*INPUT FROM C:\LENA\ny2.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE veckoarbetid.
   ASSIGN.
   IMPORT veckoarbetid.          
END.
INPUT CLOSE.

INPUT FROM C:\LENA\arbetsti.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE arbetsti.
   ASSIGN.
   IMPORT arbetsti.          
END.
INPUT CLOSE.*/

/*ANSTFORM = T2*/

INPUT FROM C:\LENA\ansform.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE anstform.
   ASSIGN.
   IMPORT anstform .          
END.
INPUT CLOSE.
FOR EACH anstform :
   IF ANSTFORMTAB.KOD = "T2" THEN.
   ELSE IF ANSTFORMTAB.KOD = "T1" THEN.
   ELSE IF ANSTFORMTAB.KOD = "H" THEN.
   ELSE IF ANSTFORMTAB.KOD = "E" THEN.
   ELSE  DELETE ANSTFORMTAB.
END.   

INPUT FROM C:\LENA\ersattnu.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE ersattning.
   ASSIGN.
   IMPORT ersattning.          
END.
INPUT CLOSE.

INPUT FROM C:\LENA\lontill.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE lontill.
   ASSIGN.
   IMPORT lontill .          
END.
INPUT CLOSE.
INPUT FROM C:\LENA\obavtab.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE OBAVTAB.
   ASSIGN.
   IMPORT OBAVTAB.          
END.
INPUT CLOSE.

INPUT FROM C:\LENA\obtab.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE OBTAB.
   ASSIGN.
   IMPORT OBTAB.          
END.
INPUT CLOSE.

INPUT FROM C:\LENA\ordarb.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE ORDARB.
   ASSIGN.
   IMPORT ORDARB.          
END.
INPUT CLOSE.
INPUT FROM C:\LENA\overavta.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE overavta .
   ASSIGN.
   IMPORT overavta.          
END.
INPUT CLOSE.

INPUT FROM C:\LENA\overkod.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE overkod .
   ASSIGN.
   IMPORT overkod.          
END.
INPUT CLOSE.

INPUT FROM C:\LENA\overtidt.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE overtidt.
   ASSIGN.
   IMPORT overtidt .          
END.
INPUT CLOSE.

INPUT FROM C:\LENA\restidta.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE restidta.
   ASSIGN.
   IMPORT restidta .          
END.
INPUT CLOSE.

INPUT FROM C:\LENA\utrtab.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE utrtab .
   ASSIGN.
   IMPORT utrtab .          
END.
INPUT CLOSE.

INPUT FROM C:\LENA\utryckni.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE utryckni.
   ASSIGN.
   IMPORT utryckni.          
END.
INPUT CLOSE.

/*BERTAB = T*/

INPUT FROM C:\LENA\bertab.d  convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE bertab.
   ASSIGN.
   IMPORT bertab.          
END.
INPUT CLOSE.
FOR EACH bertab :
   IF bertab.BEREDSKAPSAVTAL = "T" THEN.
   ELSE IF bertab.BEREDSKAPSAVTAL = "T2" THEN.   
   ELSE  DELETE bertab.
END.
INPUT FROM C:\LENA\beredsk1.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE beredskapav.
   ASSIGN.
   IMPORT beredskapav.          
END.
INPUT CLOSE.


INPUT FROM C:\LENA\\beredsk5.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE beredskapstart.
   ASSIGN.
   IMPORT beredskapstart .          
END.
INPUT CLOSE.

INPUT FROM C:\LENA\beredska.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE beredskaptab.
   ASSIGN.
   IMPORT beredskaptab.          
END.
INPUT CLOSE.

INPUT FROM C:\LENA\berkod.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE berkod.
   ASSIGN.
   IMPORT berkod.          
END.
INPUT CLOSE.

/*TRAAVTAB = TS*/
INPUT FROM C:\LENA\traavtab.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE traavtab .
   ASSIGN.
   IMPORT traavtab.          
END.
INPUT CLOSE.

INPUT FROM C:\LENA\avdragma.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE avdragma.
   ASSIGN.
   IMPORT avdragma.          
END.
INPUT CLOSE.


INPUT FROM C:\LENA\kforman.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE kforman.
   ASSIGN.
   IMPORT kforman.          
END.
INPUT CLOSE.

INPUT FROM C:\LENA\lonfler.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE lonfler.
   ASSIGN.
   IMPORT lonfler.          
END.
INPUT CLOSE.

INPUT FROM C:\LENA\maltab.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE maltab.
   ASSIGN.
   IMPORT maltab.          
END.
INPUT CLOSE.


INPUT FROM C:\LENA\traktast.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE traktast.
   ASSIGN.
   IMPORT traktast.          
END.
INPUT CLOSE.

INPUT FROM C:\LENA\traktata.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE traktata.
   ASSIGN.
   IMPORT traktata.          
END.
INPUT CLOSE.

INPUT FROM C:\LENA\traktfle.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE traktfle.
   ASSIGN.
   IMPORT traktfle.          
END.
INPUT CLOSE.

INPUT FROM C:\LENA\traktreg.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE traktreg.
   ASSIGN.
   IMPORT traktreg.          
END.
INPUT CLOSE.


/*DIVERSE*/

INPUT FROM C:\LENA\berhojn.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE BERHOJN.
   ASSIGN.
   IMPORT BERHOJN.          
END.
INPUT CLOSE.


INPUT FROM C:\LENA\bhoj.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE BHOJ.
   ASSIGN.
   IMPORT BHOJ.          
END.
INPUT CLOSE.


INPUT FROM C:\LENA\bilforar.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE BILFORARE.
   ASSIGN.
   IMPORT BILFORARE.          
END.
INPUT CLOSE.

INPUT FROM C:\LENA\flexreg.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE FLEXREG.
   ASSIGN.
   IMPORT FLEXREG.          
END.
INPUT CLOSE.

INPUT FROM C:\LENA\fvaro.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE FVARO.
   ASSIGN.
   IMPORT FVARO.          
END.
INPUT CLOSE.

INPUT FROM C:\LENA\lagbas.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE LAGBAS.
   ASSIGN.
   IMPORT LAGBAS.          
END.
INPUT CLOSE.

INPUT FROM C:\LENA\land.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE LAND.
   ASSIGN.
   IMPORT LAND.          
END.
INPUT CLOSE.

INPUT FROM C:\LENA\nfall.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE NFALL.
   ASSIGN.
   IMPORT NFALL.          
END.
INPUT CLOSE.




{EUROPEANAMERICAN.I}
=======
/* XAVTALJSBF.P las ut ett visst avtal*/
{AMERICANEUROPEAN.I}
/*INPUT FROM C:\LENA\ny2.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE veckoarbetid.
   ASSIGN.
   IMPORT veckoarbetid.          
END.
INPUT CLOSE.

INPUT FROM C:\LENA\arbetsti.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE arbetsti.
   ASSIGN.
   IMPORT arbetsti.          
END.
INPUT CLOSE.*/

/*ANSTFORM = T2*/

INPUT FROM C:\LENA\ansform.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE anstform.
   ASSIGN.
   IMPORT anstform .          
END.
INPUT CLOSE.
FOR EACH anstform :
   IF ANSTFORMTAB.KOD = "T2" THEN.
   ELSE IF ANSTFORMTAB.KOD = "T1" THEN.
   ELSE IF ANSTFORMTAB.KOD = "H" THEN.
   ELSE IF ANSTFORMTAB.KOD = "E" THEN.
   ELSE  DELETE ANSTFORMTAB.
END.   

INPUT FROM C:\LENA\ersattnu.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE ersattning.
   ASSIGN.
   IMPORT ersattning.          
END.
INPUT CLOSE.

INPUT FROM C:\LENA\lontill.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE lontill.
   ASSIGN.
   IMPORT lontill .          
END.
INPUT CLOSE.
INPUT FROM C:\LENA\obavtab.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE OBAVTAB.
   ASSIGN.
   IMPORT OBAVTAB.          
END.
INPUT CLOSE.

INPUT FROM C:\LENA\obtab.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE OBTAB.
   ASSIGN.
   IMPORT OBTAB.          
END.
INPUT CLOSE.

INPUT FROM C:\LENA\ordarb.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE ORDARB.
   ASSIGN.
   IMPORT ORDARB.          
END.
INPUT CLOSE.
INPUT FROM C:\LENA\overavta.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE overavta .
   ASSIGN.
   IMPORT overavta.          
END.
INPUT CLOSE.

INPUT FROM C:\LENA\overkod.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE overkod .
   ASSIGN.
   IMPORT overkod.          
END.
INPUT CLOSE.

INPUT FROM C:\LENA\overtidt.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE overtidt.
   ASSIGN.
   IMPORT overtidt .          
END.
INPUT CLOSE.

INPUT FROM C:\LENA\restidta.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE restidta.
   ASSIGN.
   IMPORT restidta .          
END.
INPUT CLOSE.

INPUT FROM C:\LENA\utrtab.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE utrtab .
   ASSIGN.
   IMPORT utrtab .          
END.
INPUT CLOSE.

INPUT FROM C:\LENA\utryckni.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE utryckni.
   ASSIGN.
   IMPORT utryckni.          
END.
INPUT CLOSE.

/*BERTAB = T*/

INPUT FROM C:\LENA\bertab.d  convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE bertab.
   ASSIGN.
   IMPORT bertab.          
END.
INPUT CLOSE.
FOR EACH bertab :
   IF bertab.BEREDSKAPSAVTAL = "T" THEN.
   ELSE IF bertab.BEREDSKAPSAVTAL = "T2" THEN.   
   ELSE  DELETE bertab.
END.
INPUT FROM C:\LENA\beredsk1.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE beredskapav.
   ASSIGN.
   IMPORT beredskapav.          
END.
INPUT CLOSE.


INPUT FROM C:\LENA\\beredsk5.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE beredskapstart.
   ASSIGN.
   IMPORT beredskapstart .          
END.
INPUT CLOSE.

INPUT FROM C:\LENA\beredska.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE beredskaptab.
   ASSIGN.
   IMPORT beredskaptab.          
END.
INPUT CLOSE.

INPUT FROM C:\LENA\berkod.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE berkod.
   ASSIGN.
   IMPORT berkod.          
END.
INPUT CLOSE.

/*TRAAVTAB = TS*/
INPUT FROM C:\LENA\traavtab.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE traavtab .
   ASSIGN.
   IMPORT traavtab.          
END.
INPUT CLOSE.

INPUT FROM C:\LENA\avdragma.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE avdragma.
   ASSIGN.
   IMPORT avdragma.          
END.
INPUT CLOSE.


INPUT FROM C:\LENA\kforman.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE kforman.
   ASSIGN.
   IMPORT kforman.          
END.
INPUT CLOSE.

INPUT FROM C:\LENA\lonfler.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE lonfler.
   ASSIGN.
   IMPORT lonfler.          
END.
INPUT CLOSE.

INPUT FROM C:\LENA\maltab.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE maltab.
   ASSIGN.
   IMPORT maltab.          
END.
INPUT CLOSE.


INPUT FROM C:\LENA\traktast.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE traktast.
   ASSIGN.
   IMPORT traktast.          
END.
INPUT CLOSE.

INPUT FROM C:\LENA\traktata.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE traktata.
   ASSIGN.
   IMPORT traktata.          
END.
INPUT CLOSE.

INPUT FROM C:\LENA\traktfle.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE traktfle.
   ASSIGN.
   IMPORT traktfle.          
END.
INPUT CLOSE.

INPUT FROM C:\LENA\traktreg.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE traktreg.
   ASSIGN.
   IMPORT traktreg.          
END.
INPUT CLOSE.


/*DIVERSE*/

INPUT FROM C:\LENA\berhojn.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE BERHOJN.
   ASSIGN.
   IMPORT BERHOJN.          
END.
INPUT CLOSE.


INPUT FROM C:\LENA\bhoj.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE BHOJ.
   ASSIGN.
   IMPORT BHOJ.          
END.
INPUT CLOSE.


INPUT FROM C:\LENA\bilforar.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE BILFORARE.
   ASSIGN.
   IMPORT BILFORARE.          
END.
INPUT CLOSE.

INPUT FROM C:\LENA\flexreg.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE FLEXREG.
   ASSIGN.
   IMPORT FLEXREG.          
END.
INPUT CLOSE.

INPUT FROM C:\LENA\fvaro.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE FVARO.
   ASSIGN.
   IMPORT FVARO.          
END.
INPUT CLOSE.

INPUT FROM C:\LENA\lagbas.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE LAGBAS.
   ASSIGN.
   IMPORT LAGBAS.          
END.
INPUT CLOSE.

INPUT FROM C:\LENA\land.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE LAND.
   ASSIGN.
   IMPORT LAND.          
END.
INPUT CLOSE.

INPUT FROM C:\LENA\nfall.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE NFALL.
   ASSIGN.
   IMPORT NFALL.          
END.
INPUT CLOSE.




{EUROPEANAMERICAN.I}
>>>>>>> branch 'master' of file:///\\server05\delad\REMOTEGURU\GuruRemote.git
