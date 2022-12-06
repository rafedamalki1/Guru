/*xGKor.p*/
DEFINE NEW SHARED VARIABLE globforetag LIKE FORETAG.FORETAG NO-UNDO.
DEFINE NEW SHARED VARIABLE vkdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE gvisatidpermanad AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE man AS INTEGER FORMAT "99" NO-UNDO.
DEFINE NEW SHARED VARIABLE globanv LIKE ANVANDARE.ANVANDARE NO-UNDO.
 
DEFINE  VARIABLE appcon AS LOGICAL NO-UNDO.
DEFINE VARIABLE manval AS LOGICAL NO-UNDO.
DEFINE VARIABLE samvar AS CHARACTER NO-UNDO.
DEFINE NEW SHARED TEMP-TABLE tidut
   FIELD UT AS CHARACTER FORMAT "X(132)".
FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR.
globforetag = FORETAG.FORETAG.
{CONAPP.I}
vkdatum = 04/30/2004.
gvisatidpermanad = TRUE.
globanv = "elpa".
IF globforetag = "elpa" THEN samvar = "\\pc012\d\delad\pro9s\elLESAMM.TXT".
   IF globforetag = "GRAN" THEN samvar = "\\granguru\guru_ser\server\pro9s\GRLESAMM.TXT".
   IF globforetag = "GRIT" THEN samvar = "\\granguru\guru_ser\server\pro9s\ITLESAMM.TXT". 
   IF globforetag = "GADM" THEN samvar = "\\granguru\guru_ser\server\pro9s\ADLESAMM.TXT".
   IF globforetag = "GKAL" THEN samvar = "\\granguru\guru_ser\server\pro9s\KALESAMM.TXT".
   IF globforetag = "SUND" THEN samvar = "\\BEREDNING1\DELAD\SERVER\PRO9S\SULESAMM.TXT".        
   IF globforetag = "GKRVA" THEN samvar = "\\granguru\guru_ser\server\pro9s\KRVASAMM.TXT".
   RUN LESAMMAN.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
   (INPUT 1,INPUT samvar,OUTPUT TABLE tidut).
IF globforetag = 'GRAN' OR globforetag = 'GADM'  OR globforetag = "GKAL"  THEN DO:      
      manval = FALSE.          
      IF MONTH(vkdatum) NE MONTH(vkdatum + 1) THEN DO:            
        MESSAGE "Vill du starta skapa lönefil direkt efter ekonomi- och lönesammanställningen ?" SKIP      
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE valg AS LOGICAL.
        CASE valg:
           WHEN TRUE THEN DO:
              manval = TRUE.              
           END.
           WHEN FALSE THEN manval = FALSE.
        END CASE.
      END.   
      
      MESSAGE "Nu startar lönesammanställning " + STRING(TIME,"HH:MM").        
      RUN GRANVE1.P  ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT samvar,INPUT vkdatum, INPUT gvisatidpermanad, INPUT globforetag).     
      MESSAGE "Nu startar frånvarosammanställning " + STRING(TIME,"HH:MM").      
      RUN GRANFR1.P  ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT samvar,INPUT vkdatum, INPUT gvisatidpermanad, INPUT globforetag).      
      IF manval = TRUE THEN DO:
         IF globforetag = 'GRAN' OR globforetag = 'GSYD' OR globforetag = 'GADM'
         OR globforetag = 'GRIT' OR globforetag = "GKAL" OR globforetag = "celpa"  THEN DO: 
            MESSAGE "Nu skapas lönefilen " + STRING(TIME,"HH:MM").            
            RUN GRANMAN.P  ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
            (INPUT samvar,INPUT vkdatum, INPUT gvisatidpermanad, INPUT globforetag,INPUT man).           
            RUN GRANFR2.P  ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
            (INPUT samvar,INPUT vkdatum, INPUT gvisatidpermanad, INPUT globforetag,INPUT man).           
         END.
         /*RUN GRANFLYTTA.P  ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT samvar,INPUT globforetag).                                 */
      END.
      /*IF globforetag = "GKAL" THEN DO:
         RUN GKALFL.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT samvar,INPUT vkdatum, INPUT Guru.Konstanter:globanv).
      END.*/
   END.  

IF Guru.Konstanter:appcon THEN Guru.Konstanter:appcon = Guru.Konstanter:apphand:DISCONNECT().
         DELETE OBJECT Guru.Konstanter:apphand.

