/*APKTARB.p*/
DEFINE NEW SHARED VARIABLE fnytid AS DECIMAL FORMAT "-99.99" NO-UNDO.
DEFINE NEW SHARED VARIABLE gvisatidpermanad AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE tidtabrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE persrec2 AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.   
DEFINE NEW SHARED VARIABLE regstart LIKE TIDREGITAB.START NO-UNDO. 
DEFINE NEW SHARED VARIABLE regslut LIKE TIDREGITAB.SLUT NO-UNDO.
DEFINE NEW SHARED VARIABLE regvnr AS INTEGER FORMAT "999" NO-UNDO.
DEFINE NEW SHARED VARIABLE regdagnamn AS CHARACTER FORMAT "X(3)" NO-UNDO.        
DEFINE NEW SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE regmnr AS INTEGER FORMAT "99" NO-UNDO.
DEFINE NEW SHARED VARIABLE regmannamn AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE regar AS INTEGER FORMAT "99" NO-UNDO.
DEFINE NEW SHARED VARIABLE regtotalt LIKE TIDREGITAB.TOTALT NO-UNDO.      
DEFINE NEW SHARED VARIABLE bdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE avdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE NEW SHARED VARIABLE sekunder AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE NEW SHARED VARIABLE regstartsek AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE regslutsek AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE frustarten AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE fruslutet AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE kaffestart AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE kaffeslut AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE lunchstarten AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE lunchslutet AS DECIMAL NO-UNDO.

DEFINE VARIABLE periodtot AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE VARIABLE pekodtot AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE VARIABLE pertot AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE pkodtot AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE difftot AS DECIMAL FORMAT "-99.99" NO-UNDO.
&Scoped-define NEW NEW
{TIDPERS.I}

DEFINE TEMP-TABLE invartemp   
   FIELD GA LIKE ANVANDARE.ANVANDARE 
   FIELD GM AS LOGICAL 
   FIELD SK AS LOGICAL 
   FIELD TI AS RECID 
   FIELD PER AS RECID 
   FIELD PER2 AS RECID 
   FIELD MU AS LOGICAL    
   FIELD REGST LIKE TIDREGITAB.START 
   FIELD REGSU LIKE TIDREGITAB.SLUT 
   FIELD RV AS INTEGER FORMAT "999" 
   FIELD RDAG AS CHARACTER FORMAT "X(3)"         
   FIELD RD AS DATE 
   FIELD RM AS INTEGER FORMAT "99" 
   FIELD RMN AS CHARACTER  
   FIELD REGA AS INTEGER FORMAT "99" 
   FIELD RT LIKE TIDREGITAB.TOTALT       
   FIELD BD AS DATE 
   FIELD AD AS DATE 
   FIELD NY AS DECIMAL 
   FIELD SEK AS INTEGER FORMAT "-9999999" 
   FIELD RSEK AS INTEGER 
   FIELD REGS AS INTEGER 
   FIELD GL LIKE FORETAG.FORETAG. 

DEFINE VARIABLE str AS CHARACTER FORMAT "X(90)" NO-UNDO.
{TIDUTTTNEW.I}
DEFINE QUERY tidq FOR TIDREGITAB.

DEFINE INPUT PARAMETER TABLE FOR invartemp.
DEFINE INPUT PARAMETER TABLE FOR tidpers.
DEFINE OUTPUT PARAMETER TABLE FOR tidut.
ASSIGN   str=                                                                              
"===========================================================================================================".   

FIND FIRST invartemp NO-ERROR.
ASSIGN
 
gvisatidpermanad = invartemp.GM 
skrivut = invartemp.SK   
tidtabrec = invartemp.TI   
persrec = invartemp.PER   
persrec2 = invartemp.PER2  
musz = invartemp.MU      
regstart = invartemp.REGST   
regslut = invartemp.REGSU  
regvnr = invartemp.RV   
regdagnamn = invartemp.RDAG          
regdatum = invartemp.RD   
regmnr = invartemp.RM  
regmannamn = invartemp.RMN  
regar = invartemp.REGA  
regtotalt = invartemp.RT        
bdatum = invartemp.BD  
avdatum = invartemp.AD  
nytid = invartemp.NY 
sekunder = invartemp.SEK 
regstartsek = invartemp.RSEK  
regslutsek = invartemp.REGS 
Guru.Konstanter:globforetag = invartemp.GL.

RUN huvud_UI.
PROCEDURE huvud_UI :
   CREATE tidut.
   ASSIGN 
   SUBSTRING(tidut.UT,4) = "Arbetad tid"
   SUBSTRING(tidut.UT,16) = "from:"  
   SUBSTRING(tidut.UT,22) = STRING(bdatum)
   SUBSTRING(tidut.UT,32) = "tom:"  
   SUBSTRING(tidut.UT,37) = STRING(avdatum)
   SUBSTRING(tidut.UT,70) = STRING(TODAY)
   SUBSTRING(tidut.UT,80) = STRING(TIME,"HH:MM:SS").
   CREATE tidut.                        
   ASSIGN
   SUBSTRING(tidut.UT,1) = "ENHET/SIGN"
   SUBSTRING(tidut.UT,7) = "FÖRNAMN"
   SUBSTRING(tidut.UT,23) = "EFTERNAMN".  
   CREATE tidut.  
   ASSIGN tidut.UT = str.   
   FOR EACH tidpers USE-INDEX PERSONALKOD NO-LOCK: 
      ASSIGN
      persrec = tidpers.TIDPERSREC
      regdatum = bdatum. 
      FIND PERSONALTAB WHERE RECID(PERSONALTAB) = persrec NO-LOCK NO-ERROR. 
      FIND FIRST ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING = PERSONALTAB.ANSTALLNING
      USE-INDEX ANSTF NO-LOCK NO-ERROR.                 
      musz = FALSE.      
      DO:
         musz = FALSE.
         pertot = 0.
         pkodtot = 0.
         NYDAG2:
         REPEAT:                          
            RUN REGDAG.P.
            RUN REGVEC.P.
            RUN SLUTARB.P.
            IF regstart = regslut THEN DO:
               regdatum = regdatum + 1.
               IF regdatum > avdatum THEN DO:
               
                  sekunder = pertot.
                  RUN FSEKTIM.P.
                  periodtot = fnytid.
                  sekunder = pkodtot.
                  RUN FSEKTIM.P.
                  pekodtot = fnytid.
                  IF periodtot > pekodtot THEN DO:
                     CREATE tidut.      
                     ASSIGN         
                     SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
                     SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
                     SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)    
                     SUBSTRING(tidut.UT,44) = "Period:"
                     SUBSTRING(tidut.UT,52) = STRING(periodtot)              
                     SUBSTRING(tidut.UT,57) =  "Tid"
                     SUBSTRING(tidut.UT,63) = STRING(pekodtot).             
                  END.   
                  LEAVE NYDAG2.
               END.   
               ELSE NEXT NYDAG2.
            END.                                   
            FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND 
            TIDREGITAB.DATUM = regdatum AND TIDREGITAB.VECKOKORD NE "" 
            USE-INDEX PVKORD NO-LOCK NO-ERROR.
            IF AVAILABLE TIDREGITAB THEN DO:
               regdatum = regdatum + 1.
               IF regdatum > avdatum THEN DO:
                  sekunder = pertot.
                  RUN FSEKTIM.P.
                  periodtot = fnytid.
                  sekunder = pkodtot.
                  RUN FSEKTIM.P.
                  pekodtot = fnytid.
                  IF periodtot > pekodtot THEN DO:
                     CREATE tidut.      
                     ASSIGN         
                     SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
                     SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
                     SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)    
                     SUBSTRING(tidut.UT,44) = "Period:"
                     SUBSTRING(tidut.UT,52) = STRING(periodtot)              
                     SUBSTRING(tidut.UT,57) =  "Tid:"
                     SUBSTRING(tidut.UT,63) = STRING(pekodtot).
                  END.   
                  LEAVE NYDAG2.
               END.   
               ELSE NEXT NYDAG2.
            END.            
            nytid = regtotalt.
            RUN TIMSEK.P.
            pertot = pertot + sekunder.
            OPEN QUERY ttq FOR EACH TIDREGITAB WHERE 
            TIDREGITAB.PERSONAL = tidpers.PERSONALKOD AND
            TIDREGITAB.DATUM = regdatum AND TIDREGITAB.TIDLOG = TRUE  USE-INDEX PSTART NO-LOCK.
            GET FIRST ttq NO-LOCK.
            DO WHILE AVAILABLE(TIDREGITAB): 
               musz = FALSE.             
               IF TIDREGITAB.START GE regstart AND TIDREGITAB.SLUT LE regslut THEN musz = TRUE.
               IF TIDREGITAB.LONTILLAGG NE "" AND TIDREGITAB.LONTILLAGG NE "108"  THEN musz = FALSE. /*VECKOVILA ESMA*/
               IF musz = TRUE THEN DO:
                  musz = FALSE.
                  nytid = TIDREGITAB.TOTALT.
                  RUN TIMSEK.P.
                  pkodtot = pkodtot + sekunder.
               END.                  
               GET NEXT ttq NO-LOCK.
            END.                 
            regdatum = regdatum + 1.
            IF regdatum > avdatum THEN DO:               
               sekunder = pertot.
               RUN FSEKTIM.P.
               periodtot = fnytid.
               sekunder = pkodtot.
               RUN FSEKTIM.P.
               pekodtot = fnytid.
               IF periodtot > pekodtot THEN DO:
                  CREATE tidut.      
                  ASSIGN         
                  SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
                  SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
                  SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)    
                  SUBSTRING(tidut.UT,44) = "Period:"
                  SUBSTRING(tidut.UT,52) = STRING(periodtot)              
                  SUBSTRING(tidut.UT,57) =  "Tid:"
                  SUBSTRING(tidut.UT,63) = STRING(pekodtot).
                  LEAVE NYDAG2.
               END.   
            END.   
         END.                     
      END.   
   END.      
   FIND LAST tidut NO-LOCK NO-ERROR.   
END PROCEDURE.
