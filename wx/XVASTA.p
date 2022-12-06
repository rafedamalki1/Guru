/*XVASTA.P.   MANUELL KORNING VATTENFALL*/
/*DEFINE NEW SHARED VARIABLE vkdatum AS DATE NO-UNDO.*/
DEFINE NEW SHARED VARIABLE globanv LIKE ANVANDARE.ANVANDARE NO-UNDO.
DEFINE VARIABLE manval AS LOGICAL NO-UNDO.

DEFINE new SHARED VARIABLE appcon AS LOGICAL NO-UNDO.

DEFINE NEW SHARED VARIABLE korlage AS CHARACTER FORMAT "X(50)" NO-UNDO.
DEFINE NEW SHARED VARIABLE globforetag LIKE FORETAG.FORETAG NO-UNDO.
FIND FIRST FORETAG NO-LOCK NO-ERROR.
ASSIGN globforetag = foretag.foretag.
ASSIGN Guru.Konstanter:globanv = "elpa".
IF globforetag = "ELPA" THEN DO:
    CREATE SERVER Guru.Konstanter:apphand.
    Guru.Konstanter:appcon = Guru.Konstanter:apphand:CONNECT("-S appelpool -H NTSERVER2 -N TCP",CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79),"KAGGEN","") NO-ERROR.
END.
ELSE DO:
  CREATE SERVER Guru.Konstanter:apphand.
  Guru.Konstanter:appcon = Guru.Konstanter:apphand:CONNECT("-S appvatt -H thnuguru -N TCP",CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79),"KAGGEN","") NO-ERROR.
END.  
  RUN XVATVE1.P  ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT Guru.Konstanter:globanv).
      korlage = "Nu startar frånvarosammanställning " + STRING(TIME,"HH:MM:SS").
      DISPLAY  korlage VIEW-AS TEXT  NO-LABELS.
      PAUSE 0.     
      RUN XVATFR1.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT Guru.Konstanter:globanv).
      korlage = "Nu startar ekonomisammanställning " + STRING(TIME,"HH:MM:SS").
      DISPLAY  korlage VIEW-AS TEXT NO-LABELS.
      PAUSE 0.

manval = FALSE.    
      
      
            MESSAGE "Vill du starta skapa lönefil direkt efter ekonomi- och lönesammanställningen ?" SKIP      
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE valv AS LOGICAL.
            CASE valv:
               WHEN TRUE THEN DO:
                  manval = TRUE.              
               END.
               WHEN FALSE THEN manval = FALSE.
            END CASE.
      
      
IF manval = TRUE THEN DO:         
         korlage = "Nu skapas lönefilen " + STRING(TIME,"HH:MM:SS").
         DISPLAY korlage VIEW-AS TEXT  NO-LABELS.
         PAUSE 0.
         RUN VATTMAN.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT.    
      END.      
