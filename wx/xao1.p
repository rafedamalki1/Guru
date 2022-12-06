/*ESAOIN1.P AONUMMER FRÅN ES TILL GURU*/       
DEFINE VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE chdatvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE indatvar AS INTEGER NO-UNDO.
DEFINE VARIABLE rad AS INTEGER NO-UNDO.
DEFINE VARIABLE progrest AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE prognamnold AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE prognamndat AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE prognamnque AS CHARACTER FORMAT "X(20)" NO-UNDO.                
DEFINE VARIABLE words AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE kommando AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE kommandoprog AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE TEMP-TABLE tidin
   /*    
           BOLAG DATABAS  FÖRKL 
   ENHET   408 ESNORD   NS GAMLA TAS BORT 
           409=ETA      ETA
           414=ETA      ETA TRAFIKSYSTEM 
           905=ESMAL    ES MÄLARDALEN
           401=ESADM    ES AB ADMIN  
           402=ESADM    ESNORD NYA
              */        
   FIELD ENHET              AS INTEGER FORMAT "999"             
   FIELD AONR               AS CHARACTER FORMAT "X(8)" 
   FIELD AOBENAMNING        AS CHARACTER FORMAT "X(25)" 
   FIELD UPPLDATUM          AS CHARACTER FORMAT "9999999999999" 
   FIELD REDOVISNINGSTYP    AS INTEGER FORMAT "9" 
   FIELD GILTDATUMFROM      AS CHARACTER FORMAT "9999999999999" /*PLAN START*/
   FIELD GILTDATUMTOM       AS CHARACTER FORMAT "9999999999999" 
   FIELD EKAVSLDATUM        AS CHARACTER FORMAT "9999999999999" 
   FIELD TEKNAVSLDATUM      AS CHARACTER FORMAT "9999999999999" /*AVSLUT*/
   FIELD AOTYP              AS CHARACTER FORMAT "X(1)" 
   FIELD ANDRDATUM          AS CHARACTER FORMAT "9999999999999" 
   FIELD HUVUDAONR          AS CHARACTER FORMAT "X(6)" 
   FIELD PRODUKT            AS CHARACTER FORMAT "X(2)" 
   FIELD PROJEKTLEDARE      AS INTEGER FORMAT "9999"  /*OM 8 TA EJ DENNA*/
   FIELD ASTATUS            AS CHARACTER FORMAT "X(1)" 
   FIELD KATEGORI           AS CHARACTER FORMAT "X(2)" 
   FIELD EKAVRAKNTIDPUNKT   AS CHARACTER FORMAT "9999999999999" 
   FIELD PROJEKT            AS CHARACTER FORMAT "X(3)" 
   FIELD AKTIVITET          AS CHARACTER FORMAT "X(1)" 
   FIELD HUVUDKUNDNR        AS INTEGER FORMAT "999999" 
   FIELD ANSVAR             AS INTEGER FORMAT "9999" 
   FIELD INTERNKOD          AS CHARACTER FORMAT "X(3)" 
   FIELD FORLUSTPROJ        AS INTEGER FORMAT "9"
   FIELD TEKNAVSLTIDPUNKT   AS CHARACTER FORMAT "9999999999999" /*AVSLUT*/
   FIELD ANLAGGNING         AS CHARACTER FORMAT "X(40)" 
   FIELD OBJEKT             AS CHARACTER FORMAT "X(40)" 
   FIELD OBJEKTDEL          AS CHARACTER FORMAT "X(40)" 
   FIELD ARBETSPLATS        AS INTEGER FORMAT "9999" 
   FIELD BESTALLARE         AS CHARACTER FORMAT "X(20)" 
   FIELD KUNDEREF           AS CHARACTER FORMAT "X(20)" 
   FIELD KUNDNAMN           AS CHARACTER FORMAT "X(25)" 
   FIELD KUNDADR1           AS CHARACTER FORMAT "X(25)" 
   FIELD KUNDADR2           AS CHARACTER FORMAT "X(25)" 
   FIELD KUNDADR3           AS CHARACTER FORMAT "X(25)" 
   FIELD POSTNR             AS INTEGER FORMAT "99999" 
   FIELD POSTADR            AS CHARACTER FORMAT "X(13)" 
   FIELD AOARBPLATS         AS CHARACTER FORMAT "X(25)" 
   FIELD ARBOMFATT          AS CHARACTER VIEW-AS EDITOR SCROLLBAR-VERTICAL SIZE 68 BY 6     
   FIELD GROSSISTPRISLISTA  AS INTEGER FORMAT "9" 
   FIELD KUNDPRISLISTA      AS INTEGER FORMAT "9"
   FIELD ANBUDSSUMMA        AS INTEGER FORMAT "999999999" 
   FIELD KUNDTYP            AS INTEGER FORMAT "9"
   INDEX AONR IS PRIMARY AONR
   INDEX ENHET ENHET.

DEFINE TEMP-TABLE infil
   FIELD PROGNAMN AS CHARACTER FORMAT "X(78)" 
   INDEX PRO IS PRIMARY PROGNAMN.
DEFINE TEMP-TABLE intid
   FIELD TIN AS CHARACTER FORMAT "X(78)" .

FOR EACH infil:
   DELETE infil.
END.
/*VAR IFRÅN SKALL FILERNA TAS*/ 
IF OPSYS = "UNIX" THEN DO: 
   kommandoprog = "/u10/guru/import/aoin.txt".
   kommando = "ls /u10/guru/import/ao*.dat > /u10/guru/import/aoin.txt".   
END.    
ELSE DO: 
   kommandoprog = "C:\delad\pro8\guru\komp\aoin.txt".
   kommando = "DIR/a:-d /b C:\delad\pro8\guru\komp\ao*.old > C:\delad\pro8\guru\komp\aoin.txt".
   
END. 
OS-DELETE VALUE(kommandoprog).
OS-COMMAND SILENT VALUE(kommando) . 
/*VILKA FILER*/
INPUT FROM VALUE(kommandoprog) NO-ECHO.
REPEAT:
   DO TRANSACTION: 
      CREATE infil.
      ASSIGN.
      IMPORT infil   NO-ERROR.
   END.
END.
INPUT CLOSE.
FOR EACH infil:   
   IF INDEX(infil.PROGNAMN,".old") = 0 THEN DO:       
      DELETE infil.
      NEXT.
   END.
   infil.PROGNAMN = SUBSTRING(infil.PROGNAMN,1,INDEX(infil.PROGNAMN,".old") - 1).   
END.
FOR EACH infil:  
   IF OPSYS = "UNIX" THEN DO:          
      progrest = "/u10/guru/import/ao0rest.dat".
      prognamnold = infil.PROGNAMN + ".dat".                
      prognamndat = infil.PROGNAMN + ".old".
      prognamnque = infil.PROGNAMN + ".q".
   END. 
   ELSE DO:     
      prognamnold = "C:\delad\pro8\guru\komp\" + infil.PROGNAMN + ".dat".             
      prognamndat = "C:\delad\pro8\guru\komp\" + infil.PROGNAMN + ".old".
      prognamnque = "C:\delad\pro8\guru\komp\" + infil.PROGNAMN + ".q".
   END.   
   RUN in_UI.
   OS-RENAME VALUE(prognamndat) VALUE(prognamnold).  
   OS-DELETE VALUE(prognamnque). 
END.

PROCEDURE in_UI:  
   FOR EACH intid:
      DELETE intid.
   END.
   FOR EACH tidin:
      DELETE tidin.
   END.   
   IF OPSYS = "UNIX" THEN DO:
      kommando = SEARCH("quoter").
      IF kommando = ? THEN DO:          
         NEXT.       
      END.   
   END.
   ELSE DO:      
      kommando = SEARCH("quoter.exe").
      IF kommando = ? THEN NEXT.       
   END.      
   OS-COMMAND SILENT VALUE(kommando) VALUE(prognamndat) > VALUE(prognamnque).
   IF OS-ERROR > 0 THEN DO:         
      RETURN.
   END.   
   /*LÄS IN RADER FRÅN VARJE FIL*/
   INPUT FROM VALUE(prognamnque) NO-ECHO.
   REPEAT:
      DO TRANSACTION: 
         SET words VIEW-AS EDITOR INNER-CHARS 78 INNER-LINES 80 WITH FRAME DDD WIDTH 80.   
         CREATE intid.   
         ASSIGN intid.TIN = words.   
      END.
   END.
   INPUT CLOSE.  
   /*DELA UPP RADER PÅ RÄTT SÄTT*/
   OUTPUT TO VALUE(prognamnque).
   FOR EACH intid:      
      IF LENGTH(intid.TIN) = 0 THEN DO:
         PUT UNFORMATTED intid.TIN.
      END.
      ELSE IF SUBSTRING(intid.TIN,LENGTH(intid.TIN),1) = "ü" THEN DO:
         PUT UNFORMATTED intid.TIN SKIP.
      END.
      ELSE DO:
         PUT UNFORMATTED intid.TIN.
      END.   
   END.
   OUTPUT CLOSE.
   /*SKAPA POSTER*/
   INPUT FROM VALUE(prognamnque) NO-ECHO.
   REPEAT:
      DO TRANSACTION: 
         CREATE tidin.
         ASSIGN.
         IMPORT DELIMITER "ü" tidin   NO-ERROR.
      END.               
   END.
   INPUT CLOSE.         
   FOR EACH tidin WHERE tidin.AONR = "":
      DELETE tidin.
   END.
   FOR EACH tidin :
      IF INTEGER(tidin.AONR) < 10000000 THEN DO:
         DELETE tidin.
         NEXT.         
      END.
      IF tidin.ENHET = 401 OR tidin.ENHET = 402 OR tidin.ENHET = 409 OR
      tidin.ENHET = 414 OR tidin.ENHET = 905 THEN musz = musz.
      ELSE DELETE tidin.
   END.               
   musz = FALSE.   
   /*DELA UPP POSERT TILL OLIKA FILER FÖR OLIKA DATABASER*/
   OUTPUT TO C:\delad\pro8\guru\komp\401.dat APPEND.
   FOR EACH tidin WHERE tidin.ENHET = 401:
      EXPORT tidin.
   END.
   OUTPUT CLOSE.
   OUTPUT TO C:\delad\pro8\guru\komp\402.dat APPEND.
   FOR EACH tidin WHERE tidin.ENHET = 402:
      EXPORT tidin.
   END.
   OUTPUT CLOSE.
   OUTPUT TO C:\delad\pro8\guru\komp\409.dat APPEND.
   FOR EACH tidin WHERE tidin.ENHET = 409:
      EXPORT tidin.
   END.
   OUTPUT CLOSE.
   OUTPUT TO C:\delad\pro8\guru\komp\414.dat APPEND.
   FOR EACH tidin WHERE tidin.ENHET = 414:
      EXPORT tidin.
   END.
   OUTPUT CLOSE.
   OUTPUT TO C:\delad\pro8\guru\komp\905.dat APPEND.
   FOR EACH tidin WHERE tidin.ENHET = 905:
      EXPORT tidin.
   END.
   OUTPUT CLOSE.
END PROCEDURE.        
/*
PROCEDURE skapaao_UI:   
      /*     
         BOLAG DATABAS  FÖRKL 
   ENHET   408 ESNORD   NS GAMLA TAS BORT 
           409=ETA      ETA
           414=ETA      ETA TRAFIKSYSTEM 
           905=ESMAL    ES MÄLARDALEN
           401=ESADM    ES AB ADMIN  
           402=ESADM    ESNORD NYA
            
           */
   musz = FALSE.
   /*TA IN RÄTT POSTER TILL RÄTT DATABAS*/
   FOR EACH tidin:
      DELETE tidin.
   END.   
   kommando = SEARCH("/u10/guru/import/401.dat").
   IF kommando NE ? THEN DO:          
      INPUT FROM "/u10/guru/import/401.dat".
      REPEAT:
         DO TRANSACTION: 
            CREATE tidin.
            ASSIGN.
            IMPORT tidin NO-ERROR.
         END.               
      END.
      INPUT CLOSE.
      FIND FIRST tidin WHERE tidin.ENHET = 401 NO-LOCK NO-ERROR.
      IF AVAILABLE tidin THEN DO:
         IF NOT CONNECTED("esadm") THEN DO :               
            CONNECT -db esadm -S esadmd8 -H elpaso.sydkraft.se -N tcp NO-ERROR. 
            IF CONNECTED("esadm") THEN DO:
               CREATE ALIAS RT8 FOR DATABASE esadm NO-ERROR.
               RUN ESSKAPAO.P (INPUT 401,INPUT TABLE tidin).  
               FOR EACH tidin WHERE tidin.ENHET = 401:
                  DELETE tidin.
               END.    
               OS-DELETE "/u10/guru/import/401.dat".
               DELETE ALIAS RT8.
               DISCONNECT esadm NO-ERROR.
            END.
            ELSE musz = TRUE. 
         END.
      END.
      ELSE OS-DELETE "/u10/guru/import/401.dat".
   END.
   FOR EACH tidin:
      DELETE tidin.
   END.   
   kommando = SEARCH("/u10/guru/import/402.dat").
   IF kommando NE ? THEN DO:          
      INPUT FROM "/u10/guru/import/402.dat".
      REPEAT:
         DO TRANSACTION: 
            CREATE tidin.
            ASSIGN.
            IMPORT tidin NO-ERROR.
         END.               
      END.
      INPUT CLOSE.   
      FIND FIRST tidin WHERE tidin.ENHET = 402 NO-LOCK NO-ERROR.
      IF AVAILABLE tidin THEN DO:
         IF NOT CONNECTED("esadm") THEN DO :               
            CONNECT -db esadm -S esadmd8 -H elpaso.sydkraft.se -N tcp NO-ERROR. 
            IF CONNECTED("esadm") THEN DO:
               CREATE ALIAS RT8 FOR DATABASE esadm NO-ERROR.
               RUN ESSKAPAO.P (INPUT 402,INPUT TABLE tidin).  
               FOR EACH tidin WHERE tidin.ENHET = 402:
                  DELETE tidin.
               END.
               OS-DELETE "/u10/guru/import/402.dat".
               DELETE ALIAS RT8.
               DISCONNECT esadm NO-ERROR.
            END.
            ELSE musz = TRUE. 
         END.
      END.
      ELSE OS-DELETE "/u10/guru/import/402.dat".
   END.
   FOR EACH tidin:
      DELETE tidin.
   END.   
   kommando = SEARCH("/u10/guru/import/409.dat").
   IF kommando NE ? THEN DO:          
      INPUT FROM "/u10/guru/import/409.dat".
      REPEAT:
         DO TRANSACTION: 
            CREATE tidin.
            ASSIGN.
            IMPORT tidin NO-ERROR.
         END.               
      END.
      INPUT CLOSE.      
      FIND FIRST tidin WHERE tidin.ENHET = 409 NO-LOCK NO-ERROR.
      IF AVAILABLE tidin THEN DO:
         IF NOT CONNECTED("eta") THEN DO :               
            CONNECT -db eta -S eseta8 -H elpaso.sydkraft.se -N tcp NO-ERROR. 
            IF CONNECTED("eta") THEN DO:
               CREATE ALIAS RT8 FOR DATABASE eta NO-ERROR.
               RUN ESSKAPAO.P (INPUT 409,INPUT TABLE tidin).
               FOR EACH tidin WHERE tidin.ENHET = 409:
                  DELETE tidin.
               END.  
               OS-DELETE "/u10/guru/import/409.dat".
               DELETE ALIAS RT8.
               DISCONNECT eta NO-ERROR.
            END. 
            ELSE musz = TRUE.
         END.
      END.
      ELSE OS-DELETE "/u10/guru/import/409.dat".
   END.
   FOR EACH tidin:
      DELETE tidin.
   END.      
   kommando = SEARCH("/u10/guru/import/414.dat").
   IF kommando NE ? THEN DO:          
      INPUT FROM "/u10/guru/import/414.dat".
      REPEAT:
         DO TRANSACTION: 
            CREATE tidin.
            ASSIGN.
            IMPORT tidin NO-ERROR.
         END.               
      END.
      INPUT CLOSE.         
      FIND FIRST tidin WHERE tidin.ENHET = 414 NO-LOCK NO-ERROR.
      IF AVAILABLE tidin THEN DO:
         IF NOT CONNECTED("eta") THEN DO :               
            CONNECT -db eta -S eseta8 -H elpaso.sydkraft.se -N tcp NO-ERROR. 
            IF CONNECTED("eta") THEN DO:
               CREATE ALIAS RT8 FOR DATABASE eta NO-ERROR.
               RUN ESSKAPAO.P (INPUT 414,INPUT TABLE tidin).  
               FOR EACH tidin WHERE tidin.ENHET = 414:
                  DELETE tidin.
               END.
               OS-DELETE "/u10/guru/import/414.dat".
               DELETE ALIAS RT8.
               DISCONNECT eta NO-ERROR.
            END. 
            ELSE musz = TRUE.
         END.
      END.
      ELSE OS-DELETE "/u10/guru/import/414.dat".
   END.
   FOR EACH tidin:
      DELETE tidin.
   END.      
   kommando = SEARCH("/u10/guru/import/905.dat").
   IF kommando NE ? THEN DO:          
      INPUT FROM "/u10/guru/import/905.dat".
      REPEAT:
         DO TRANSACTION: 
            CREATE tidin.
            ASSIGN.
            IMPORT tidin NO-ERROR.
         END.               
      END.
      INPUT CLOSE.
      FIND FIRST tidin WHERE tidin.ENHET = 905 NO-LOCK NO-ERROR.
      IF AVAILABLE tidin THEN DO:
         IF NOT CONNECTED("esmal") THEN DO :               
            CONNECT -db esmal -S esmal8 -H elpaso.sydkraft.se -N tcp NO-ERROR. 
            IF CONNECTED("esmal") THEN DO:
               CREATE ALIAS RT8 FOR DATABASE esmal NO-ERROR.
               RUN ESSKAPAO.P (INPUT 905,INPUT TABLE tidin).  
               FOR EACH tidin WHERE tidin.ENHET = 905:
                  DELETE tidin.
               END.
               OS-DELETE "/u10/guru/import/905.dat".
               DELETE ALIAS RT8.
               DISCONNECT esmal NO-ERROR.
            END. 
            ELSE musz = TRUE.
         END.
      END.
      ELSE OS-DELETE "/u10/guru/import/905.dat".
   END.   
   musz = FALSE.
END PROCEDURE.   

  */              
