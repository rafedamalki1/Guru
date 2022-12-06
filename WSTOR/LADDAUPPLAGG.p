/*LADDAUPPLAGG.P*/
DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE forevar AS CHARACTER NO-UNDO.
{AMERICANEUROPEAN.I}
FIND FIRST FORETAG NO-LOCK NO-ERROR.
forevar = FORETAG.FORETAG.

IF forevar = "GKAL" THEN DO:
   filnamn = "D:\DELAD\KLIENT\PRO9\". 
END.
IF forevar = "GRAN" OR forevar = "GRIT" THEN DO:
   filnamn = "d:\elpool\delad\pro9\wrk\". 
END.

ELSE IF forevar = "SUND" OR forevar = "SNAT" THEN DO:
   filnamn = "D:\DELAD\KLIENT\PRO9\GURU\WTID\". 
END.
ELSE DO:
   filnamn = "".
END.

INPUT FROM VALUE(filnamn + "ANLAGDEL.D") convert target "iso8859-1" source "iso8859-1".
REPEAT:
   DO TRANSACTION:      
      CREATE ANLAGGNINGSDEL.      
      IMPORT ANLAGGNINGSDEL.   
   END.
END.   
INPUT CLOSE.        

INPUT FROM VALUE(filnamn + "ANLAGGN1.D") convert target "iso8859-1" source "iso8859-1".
REPEAT:
   DO TRANSACTION:      
      CREATE ANLAGGNINGSTYP.      
      IMPORT ANLAGGNINGSTYP.   
   END.
END.   
INPUT CLOSE.        

INPUT FROM VALUE(filnamn + "BRYTORGA.D") convert target "iso8859-1" source "iso8859-1".
REPEAT:
   DO TRANSACTION:      
      CREATE BRYTORGAN.      
      IMPORT BRYTORGAN.   
   END.
END.   
INPUT CLOSE.        

INPUT FROM VALUE(filnamn + "FELORSAK.D") convert target "iso8859-1" source "iso8859-1".
REPEAT:
   DO TRANSACTION:      
      CREATE FELORSAK.      
      IMPORT FELORSAK.   
   END.
END.   
INPUT CLOSE.        

INPUT FROM VALUE(filnamn + "FELYTTRI.D") convert target "iso8859-1" source "iso8859-1".
REPEAT:
   DO TRANSACTION:      
      CREATE FELYTTRING.      
      IMPORT FELYTTRING.   
   END.
END.   
INPUT CLOSE.        

INPUT FROM VALUE(filnamn + "GRUNDFEL.D") convert target "iso8859-1" source "iso8859-1".
REPEAT:
   DO TRANSACTION:      
      CREATE GRUNDFELTYP.      
      IMPORT GRUNDFELTYP.   
   END.
END.   
INPUT CLOSE.        

INPUT FROM VALUE(filnamn + "GRUNDNAT.D") convert target "iso8859-1" source "iso8859-1".
REPEAT:
   DO TRANSACTION:      
      CREATE GRUNDNATSTOR.      
      IMPORT GRUNDNATSTOR.   
   END.
END.   
INPUT CLOSE.        

INPUT FROM VALUE(filnamn + "INLASTAB.D") convert target "iso8859-1" source "iso8859-1".
REPEAT:
   DO TRANSACTION:      
      CREATE INLASTAB.      
      IMPORT INLASTAB.   
   END.
END.   
INPUT CLOSE.        

INPUT FROM VALUE(filnamn + "NATSTOR.D") convert target "iso8859-1" source "iso8859-1".
REPEAT:
   DO TRANSACTION:      
      CREATE NATSTOR.      
      IMPORT NATSTOR.   
   END.
END.   
INPUT CLOSE.        

INPUT FROM VALUE(filnamn + "NATSTRUK.D") convert target "iso8859-1" source "iso8859-1".
REPEAT:
   DO TRANSACTION:      
      CREATE NATSTRUKTUR.      
      IMPORT NATSTRUKTUR.   
   END.
END.   
INPUT CLOSE.        

INPUT FROM VALUE(filnamn + "NATTYP.D") convert target "iso8859-1" source "iso8859-1".
REPEAT:
   DO TRANSACTION:      
      CREATE NATTYP.      
      IMPORT NATTYP.   
   END.
END.   
INPUT CLOSE.       

INPUT FROM VALUE(filnamn + "NATUPPKO.D") convert target "iso8859-1" source "iso8859-1".
REPEAT:
   DO TRANSACTION:      
      CREATE NATUPPKOPP.      
      IMPORT NATUPPKOPP.   
   END.
END.   
INPUT CLOSE.        

INPUT FROM VALUE(filnamn + "NATUPPL1.D") convert target "iso8859-1" source "iso8859-1".
REPEAT:
   DO TRANSACTION:      
      CREATE NATUPPLAGG1.      
      IMPORT NATUPPLAGG1.   
   END.
END.   
INPUT CLOSE.        

INPUT FROM VALUE(filnamn + "NATUPPL2.D") convert target "iso8859-1" source "iso8859-1".
REPEAT:
   DO TRANSACTION:      
      CREATE NATUPPLAGG2.      
      IMPORT NATUPPLAGG2.   
   END.
END.   
INPUT CLOSE.        

INPUT FROM VALUE(filnamn + "NATUPPL3.D") convert target "iso8859-1" source "iso8859-1".
REPEAT:
   DO TRANSACTION:      
      CREATE NATUPPLAGG3.      
      IMPORT NATUPPLAGG3.   
   END.
END.   
INPUT CLOSE.        

INPUT FROM VALUE(filnamn + "NATUPPL4.D") convert target "iso8859-1" source "iso8859-1".
REPEAT:
   DO TRANSACTION:      
      CREATE NATUPPLAGG4.      
      IMPORT NATUPPLAGG4.   
   END.
END.   
INPUT CLOSE.        

INPUT FROM VALUE(filnamn + "RELAINDI.D") convert target "iso8859-1" source "iso8859-1".
REPEAT:
   DO TRANSACTION:      
      CREATE RELAINDIKERING.      
      IMPORT RELAINDIKERING.   
   END.
END.   
INPUT CLOSE.        

INPUT FROM VALUE(filnamn + "RESERVKR.D") convert target "iso8859-1" source "iso8859-1".
REPEAT:
   DO TRANSACTION:      
      CREATE RESERVKRAFTMETOD.      
      IMPORT RESERVKRAFTMETOD.   
   END.
END.   
INPUT CLOSE.        

INPUT FROM VALUE(filnamn + "SAKRINGS.D") convert target "iso8859-1" source "iso8859-1".
REPEAT:
   DO TRANSACTION:      
      CREATE SAKRINGSTYP.      
      IMPORT SAKRINGSTYP.   
   END.
END.   
INPUT CLOSE.        

INPUT FROM VALUE(filnamn + "SEKTIONE.D") convert target "iso8859-1" source "iso8859-1".
REPEAT:
   DO TRANSACTION:      
      CREATE SEKTIONERING.      
      IMPORT SEKTIONERING.   
   END.
END.   
INPUT CLOSE.        

INPUT FROM VALUE(filnamn + "SPANNING.D") convert target "iso8859-1" source "iso8859-1".
REPEAT:
   DO TRANSACTION:      
      CREATE SPANNINGSNIV.      
      IMPORT SPANNINGSNIV.   
   END.
END.   
INPUT CLOSE.        

INPUT FROM VALUE(filnamn + "STORDRIF.D") convert target "iso8859-1" source "iso8859-1".
REPEAT:
   DO TRANSACTION:      
      CREATE STORDRIFTOMR.      
      IMPORT STORDRIFTOMR.   
   END.
END.   
INPUT CLOSE.        

INPUT FROM VALUE(filnamn + "STORNING.D") convert target "iso8859-1" source "iso8859-1".
REPEAT:
   DO TRANSACTION:      
      CREATE STORNINGSTYP.      
      IMPORT STORNINGSTYP.   
   END.
END.   
INPUT CLOSE.        

INPUT FROM VALUE(filnamn + "UTLOSNIN.D") convert target "iso8859-1" source "iso8859-1".
REPEAT:
   DO TRANSACTION:      
      CREATE UTLOSNINGSKYDD.      
      IMPORT UTLOSNINGSKYDD.   
   END.
END.   
INPUT CLOSE.        
{EUROPEANAMERICAN.I}