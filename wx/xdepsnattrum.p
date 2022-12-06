
   
   /*xdepsnattrum.p*/       
DEFINE NEW SHARED VARIABLE quotervar AS CHARACTER FORMAT "X(256)" NO-UNDO.



DEFINE VARIABLE gurubilder AS CHARACTER NO-UNDO.
/*{PROVAG.I} */
DEFINE VARIABLE musz AS LOGICAL NO-UNDO.

DEFINE VARIABLE rad AS INTEGER NO-UNDO.
DEFINE VARIABLE prognamn AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE prognamndat AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE prognamnque AS CHARACTER FORMAT "X(20)" NO-UNDO.                
DEFINE VARIABLE words AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE kommando AS CHARACTER  NO-UNDO.
DEFINE VARIABLE kommando2 AS CHARACTER  NO-UNDO.
DEFINE VARIABLE kommandoprog AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE satsvar AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE enrvar AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE melvar AS INTEGER NO-UNDO.
DEFINE VARIABLE melvar2 AS INTEGER NO-UNDO.
DEFINE VARIABLE langd AS INTEGER NO-UNDO.
DEFINE VARIABLE pos1 AS INTEGER NO-UNDO. 
DEFINE VARIABLE hjenr AS CHARACTER NO-UNDO.
DEFINE VARIABLE vald_depa  AS INTEGER NO-UNDO.
DEFINE VARIABLE timehjalp AS INTEGER NO-UNDO.


DEFINE BUFFER mtrlbuff FOR MTRL.
DEFINE BUFFER mtrldepbuff FOR MTRLDEP.



DEFINE TEMP-TABLE tidinah
   FIELD ENR                AS CHARACTER    
   /*FIELD ENHET              AS CHARACTER*/
   FIELD BENAMNING          AS CHARACTER   
   FIELD NPRIS              AS DECIMAL   
   FIELD ANTAL              AS INTEGER   
   FIELD BESTPUNKT              AS INTEGER
   FIELD BESTKV              AS INTEGER
   FIELD indat AS date 
   INDEX ENR IS PRIMARY ENR.
   

DEFINE TEMP-TABLE infil
   FIELD PROGNAMN AS CHARACTER FORMAT "X(78)" 
   INDEX PRO IS PRIMARY PROGNAMN.
DEFINE TEMP-TABLE intid
   FIELD TIN AS CHARACTER FORMAT "X(78)" .
   
DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO.   
DEFINE VARIABLE leverant LIKE LEVERANTOR.LEVKOD NO-UNDO.
{EXTRADATA.I}
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
EMPTY TEMP-TABLE inextradatatemp NO-ERROR.
FIND FIRST FORETAG NO-LOCK NO-ERROR.
{muswait.i} 


   ASSIGN 
   vald_depa = 2
   leverant = "12"
   filnamn = "D:\delad\PRO10S\DEPÅ\INLÄS ELNÄT.SKV".
   OUTPUT TO D:\delad\PRO10S\DEPÅ\feldep2.txt.
   /*filnamn = "\\SERVER05\d\elpool\elplo\Sundsvall Elnät\Depå\Kabel i depå 20200122\FIX saldo Fifo\INLÄS ELNÄT.SKV".
   OUTPUT TO C:\feldep2.txt.*/
   RUN in_UI.
   OUTPUT CLOSE.
   /*ASSIGN 
   vald_depa = 2
   leverant = "12".
   OUTPUT TO D:\delad\PRO10S\DEPÅ\feldep22.txt.
   filnamn = "D:\delad\PRO10S\DEPÅ\FixInventerat returlager Sundsvall 20200107.skv".
   /*OUTPUT TO C:\feldep22.txt.
   filnamn = "\\SERVER05\d\elpool\elplo\Sundsvall Elnät\Depå\2-Elnät returlager 2020114\FixInventerat returlager Sundsvall 20200107.skv".*/
   RUN in_UI.
   OUTPUT CLOSE.*/
   
   ASSIGN
   vald_depa = 3
   leverant = "12"   
   filnamn = "D:\delad\PRO10S\DEPÅ\Inläs Hsand fiber.skv".
   OUTPUT TO D:\delad\PRO10S\DEPÅ\feldep3.txt.
   /*filnamn = "\\SERVER05\d\elpool\elplo\Sundsvall Elnät\Depå\Kabel i depå 20200122\FIX saldo Fifo\Inläs Hsand fiber.skv".
   OUTPUT TO C:\feldep3.txt.*/ 
   RUN in_UI.
   OUTPUT CLOSE.
   
   assign 
   vald_depa = 2
   leverant = "12"
   filnamn = "D:\delad\PRO10S\DEPÅ\Inläs elnät fiber.skv".
   OUTPUT TO D:\delad\PRO10S\DEPÅ\feldep2f.txt.
   /*filnamn = "\\SERVER05\d\elpool\elplo\Sundsvall Elnät\Depå\Kabel i depå 20200122\FIX saldo Fifo\Inläs elnät fiber.skv".
   OUTPUT TO C:\feldep2f.txt.*/
   RUN in_UI.
   ASSIGN  
   vald_depa = 5
   leverant = "12"
   filnamn = "D:\delad\PRO10S\DEPÅ\Inläs Strömsund fiber.skv".
   OUTPUT TO D:\delad\PRO10S\DEPÅ\feldep5.txt.
   /*filnamn = "\\SERVER05\d\elpool\elplo\Sundsvall Elnät\Depå\Kabel i depå 20200122\FIX saldo Fifo\Inläs Strömsund fiber.skv".
   OUTPUT TO C:\feldep5.txt.*/ 
   RUN in_UI.
   OUTPUT CLOSE.
   ASSIGN 
   vald_depa = 6
   leverant = "12"
   filnamn = "D:\delad\PRO10S\DEPÅ\Inläs Ånge fiber.skv".
   OUTPUT TO D:\delad\PRO10S\DEPÅ\feldep6.txt.
   /*filnamn = "\\SERVER05\d\elpool\elplo\Sundsvall Elnät\Depå\Kabel i depå 20200122\FIX saldo Fifo\Inläs Ånge fiber.skv".
   OUTPUT TO C:\feldep6.txt.*/    
   RUN in_UI.   
   OUTPUT CLOSE.
   
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?.
   
PROCEDURE in_UI: 
   EMPTY TEMP-TABLE intid NO-ERROR.
   EMPTY TEMP-TABLE tidinah NO-ERROR.
   
   INPUT FROM VALUE(filnamn) NO-ECHO.
   REPEAT:
      DO TRANSACTION: 
         CREATE tidinah.
         ASSIGN.
         IMPORT DELIMITER ";" tidinah   NO-ERROR.
      END.               
   END.

   FOR EACH tidinah WHERE tidinah.ENR = "":
      DELETE tidinah.
   END.     
   

   RUN skapaenr_UI.           

   
END PROCEDURE.

PROCEDURE skapaenr_UI:   

   EMPTY TEMP-TABLE inextradatatemp NO-ERROR.
   /*måste vara unik på enr datum och tid. Det går för fort med inläsning= sama tid pluusa på 1*/
   timehjalp = TIME.
   FOR EACH tidinah NO-LOCK:                                   
      DO TRANSACTION: 
         hjenr = tidinah.ENR.
         
         
         IF FORETAG.FORETAG = "SNAT" THEN DO: 
            IF SUBSTRING(hjenr,1,1) NE  "E" THEN hjenr = "E" + hjenr.
         END.      
         FIND FIRST mtrlbuff WHERE mtrlbuff.ENR =  hjenr AND
         mtrlbuff.LEVKOD = leverant AND mtrlbuff.KALKNR = 0 USE-INDEX LEV
         EXCLUSIVE-LOCK NO-ERROR.   
         IF AVAILABLE mtrlbuff THEN DO:
            FIND FIRST MTRLDEP WHERE MTRLDEP.DEPNR = vald_depa AND MTRLDEP.ENR = hjenr AND MTRLDEP.IBDATUM = ? EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAILABLE MTRLDEP THEN DO:            
               CREATE mtrldepbuff.
               ASSIGN 
               mtrldepbuff.DEPNR = vald_depa
               mtrldepbuff.ENR = hjenr
               mtrldepbuff.LEVKOD = leverant
               mtrldepbuff.BENAMNING = mtrlbuff.BENAMNING
               mtrldepbuff.ENHET = mtrlbuff.ENHET
               mtrldepbuff.BESTPUNKT = tidinah.BESTPUNKT
               mtrldepbuff.BESTKV = tidinah.BESTKV
               mtrldepbuff.FACKID = ""
               mtrldepbuff.OMSATT = 0
               mtrldepbuff.NPRIS = tidinah.NPRIS / 100
               mtrldepbuff.BPRIS = tidinah.NPRIS / 100 
               mtrldepbuff.LAGER = TRUE
               mtrldepbuff.IBDATUM = ?         
               mtrldepbuff.INVDATUM = TODAY
               mtrldepbuff.SALDO = tidinah.ANTAL.
               IF mtrldepbuff.NPRIS = 0 OR mtrldepbuff.NPRIS = ? THEN DO:
                   mtrldepbuff.NPRIS = mtrlbuff.NPRIS.
                   mtrldepbuff.BPRIS = mtrlbuff.NPRIS.
               END.    
               timehjalp = timehjalp + 1. 
               CREATE inextradatatemp.          
               ASSIGN
               inextradatatemp.PROGRAM = "FIFO"                                  
               inextradatatemp.HUVUDINT =  mtrldepbuff.DEPNR
               inextradatatemp.HUVUDCH =  mtrldepbuff.ENR
               inextradatatemp.SOKCHAR[2] =  mtrldepbuff.BENAMNING
               inextradatatemp.SOKCHAR[3] =  mtrldepbuff.ENHET
               inextradatatemp.SOKDEC[1] =  mtrldepbuff.NPRIS
               inextradatatemp.SOKINT[1] =  mtrldepbuff.SALDO
               inextradatatemp.SOKDATE[1] =  tidinah.INDAT
               inextradatatemp.SOKINT[2] =  timehjalp.                                    
            END.
            ELSE DO:
               ASSIGN MTRLDEP.SALDO = MTRLDEP.SALDO + tidinah.ANTAL.
               timehjalp = timehjalp + 1. 
               CREATE inextradatatemp.          
               ASSIGN
               inextradatatemp.PROGRAM = "FIFO"                                  
               inextradatatemp.HUVUDINT =  MTRLDEP.DEPNR
               inextradatatemp.HUVUDCH =  MTRLDEP.ENR
               inextradatatemp.SOKCHAR[2] =  MTRLDEP.BENAMNING
               inextradatatemp.SOKCHAR[3] =  MTRLDEP.ENHET
               
               inextradatatemp.SOKINT[1] = tidinah.ANTAL
               inextradatatemp.SOKDATE[1] =  tidinah.INDAT
               inextradatatemp.SOKINT[2] =  timehjalp.
               IF tidinah.NPRIS = 0 OR tidinah.NPRIS = ? THEN DO:
                  inextradatatemp.SOKDEC[1] =  mtrlbuff.NPRIS.
               END.
               ELSE inextradatatemp.SOKDEC[1] =  tidinah.NPRIS / 100.   
                                 
            END.
            
                       
                     
         END.
         ELSE DO:                               
         
            PUT UNFORMATTED tidinah.ENR "  "  tidinah.benamning " " vald_depa SKIP.
            
         END.   
      END.            
   END.
   RUN extraspar_UI IN edataapph (INPUT TABLE inextradatatemp).
              

/*   OS-DELETE VALUE(kommando).   
    OUTPUT CLOSE. */
END PROCEDURE.   

                

   

=======
   
   /*xdepsnattrum.p*/       
DEFINE NEW SHARED VARIABLE quotervar AS CHARACTER FORMAT "X(256)" NO-UNDO.



DEFINE VARIABLE gurubilder AS CHARACTER NO-UNDO.
/*{PROVAG.I} */
DEFINE VARIABLE musz AS LOGICAL NO-UNDO.

DEFINE VARIABLE rad AS INTEGER NO-UNDO.
DEFINE VARIABLE prognamn AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE prognamndat AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE prognamnque AS CHARACTER FORMAT "X(20)" NO-UNDO.                
DEFINE VARIABLE words AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE kommando AS CHARACTER  NO-UNDO.
DEFINE VARIABLE kommando2 AS CHARACTER  NO-UNDO.
DEFINE VARIABLE kommandoprog AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE satsvar AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE enrvar AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE melvar AS INTEGER NO-UNDO.
DEFINE VARIABLE melvar2 AS INTEGER NO-UNDO.
DEFINE VARIABLE langd AS INTEGER NO-UNDO.
DEFINE VARIABLE pos1 AS INTEGER NO-UNDO. 
DEFINE VARIABLE hjenr AS CHARACTER NO-UNDO.
DEFINE VARIABLE vald_depa  AS INTEGER NO-UNDO.
DEFINE VARIABLE timehjalp AS INTEGER NO-UNDO.


DEFINE BUFFER mtrlbuff FOR MTRL.
DEFINE BUFFER mtrldepbuff FOR MTRLDEP.



DEFINE TEMP-TABLE tidinah
   FIELD ENR                AS CHARACTER    
   /*FIELD ENHET              AS CHARACTER*/
   FIELD BENAMNING          AS CHARACTER   
   FIELD NPRIS              AS DECIMAL   
   FIELD ANTAL              AS INTEGER   
   FIELD BESTPUNKT              AS INTEGER
   FIELD BESTKV              AS INTEGER
   FIELD indat AS date 
   INDEX ENR IS PRIMARY ENR.
   

DEFINE TEMP-TABLE infil
   FIELD PROGNAMN AS CHARACTER FORMAT "X(78)" 
   INDEX PRO IS PRIMARY PROGNAMN.
DEFINE TEMP-TABLE intid
   FIELD TIN AS CHARACTER FORMAT "X(78)" .
   
DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO.   
DEFINE VARIABLE leverant LIKE LEVERANTOR.LEVKOD NO-UNDO.
{EXTRADATA.I}
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
EMPTY TEMP-TABLE inextradatatemp NO-ERROR.
FIND FIRST FORETAG NO-LOCK NO-ERROR.
{muswait.i} 


   ASSIGN 
   vald_depa = 2
   leverant = "12"
   filnamn = "D:\delad\PRO10S\DEPÅ\INLÄS ELNÄT.SKV".
   OUTPUT TO D:\delad\PRO10S\DEPÅ\feldep2.txt.
   /*filnamn = "\\SERVER05\d\elpool\elplo\Sundsvall Elnät\Depå\Kabel i depå 20200122\FIX saldo Fifo\INLÄS ELNÄT.SKV".
   OUTPUT TO C:\feldep2.txt.*/
   RUN in_UI.
   OUTPUT CLOSE.
   /*ASSIGN 
   vald_depa = 2
   leverant = "12".
   OUTPUT TO D:\delad\PRO10S\DEPÅ\feldep22.txt.
   filnamn = "D:\delad\PRO10S\DEPÅ\FixInventerat returlager Sundsvall 20200107.skv".
   /*OUTPUT TO C:\feldep22.txt.
   filnamn = "\\SERVER05\d\elpool\elplo\Sundsvall Elnät\Depå\2-Elnät returlager 2020114\FixInventerat returlager Sundsvall 20200107.skv".*/
   RUN in_UI.
   OUTPUT CLOSE.*/
   
   ASSIGN
   vald_depa = 3
   leverant = "12"   
   filnamn = "D:\delad\PRO10S\DEPÅ\Inläs Hsand fiber.skv".
   OUTPUT TO D:\delad\PRO10S\DEPÅ\feldep3.txt.
   /*filnamn = "\\SERVER05\d\elpool\elplo\Sundsvall Elnät\Depå\Kabel i depå 20200122\FIX saldo Fifo\Inläs Hsand fiber.skv".
   OUTPUT TO C:\feldep3.txt.*/ 
   RUN in_UI.
   OUTPUT CLOSE.
   
   assign 
   vald_depa = 2
   leverant = "12"
   filnamn = "D:\delad\PRO10S\DEPÅ\Inläs elnät fiber.skv".
   OUTPUT TO D:\delad\PRO10S\DEPÅ\feldep2f.txt.
   /*filnamn = "\\SERVER05\d\elpool\elplo\Sundsvall Elnät\Depå\Kabel i depå 20200122\FIX saldo Fifo\Inläs elnät fiber.skv".
   OUTPUT TO C:\feldep2f.txt.*/
   RUN in_UI.
   ASSIGN  
   vald_depa = 5
   leverant = "12"
   filnamn = "D:\delad\PRO10S\DEPÅ\Inläs Strömsund fiber.skv".
   OUTPUT TO D:\delad\PRO10S\DEPÅ\feldep5.txt.
   /*filnamn = "\\SERVER05\d\elpool\elplo\Sundsvall Elnät\Depå\Kabel i depå 20200122\FIX saldo Fifo\Inläs Strömsund fiber.skv".
   OUTPUT TO C:\feldep5.txt.*/ 
   RUN in_UI.
   OUTPUT CLOSE.
   ASSIGN 
   vald_depa = 6
   leverant = "12"
   filnamn = "D:\delad\PRO10S\DEPÅ\Inläs Ånge fiber.skv".
   OUTPUT TO D:\delad\PRO10S\DEPÅ\feldep6.txt.
   /*filnamn = "\\SERVER05\d\elpool\elplo\Sundsvall Elnät\Depå\Kabel i depå 20200122\FIX saldo Fifo\Inläs Ånge fiber.skv".
   OUTPUT TO C:\feldep6.txt.*/    
   RUN in_UI.   
   OUTPUT CLOSE.
   
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?.
   
PROCEDURE in_UI: 
   EMPTY TEMP-TABLE intid NO-ERROR.
   EMPTY TEMP-TABLE tidinah NO-ERROR.
   
   INPUT FROM VALUE(filnamn) NO-ECHO.
   REPEAT:
      DO TRANSACTION: 
         CREATE tidinah.
         ASSIGN.
         IMPORT DELIMITER ";" tidinah   NO-ERROR.
      END.               
   END.

   FOR EACH tidinah WHERE tidinah.ENR = "":
      DELETE tidinah.
   END.     
   

   RUN skapaenr_UI.           

   
END PROCEDURE.

PROCEDURE skapaenr_UI:   

   EMPTY TEMP-TABLE inextradatatemp NO-ERROR.
   /*måste vara unik på enr datum och tid. Det går för fort med inläsning= sama tid pluusa på 1*/
   timehjalp = TIME.
   FOR EACH tidinah NO-LOCK:                                   
      DO TRANSACTION: 
         hjenr = tidinah.ENR.
         
         
         IF FORETAG.FORETAG = "SNAT" THEN DO: 
            IF SUBSTRING(hjenr,1,1) NE  "E" THEN hjenr = "E" + hjenr.
         END.      
         FIND FIRST mtrlbuff WHERE mtrlbuff.ENR =  hjenr AND
         mtrlbuff.LEVKOD = leverant AND mtrlbuff.KALKNR = 0 USE-INDEX LEV
         EXCLUSIVE-LOCK NO-ERROR.   
         IF AVAILABLE mtrlbuff THEN DO:
            FIND FIRST MTRLDEP WHERE MTRLDEP.DEPNR = vald_depa AND MTRLDEP.ENR = hjenr AND MTRLDEP.IBDATUM = ? EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAILABLE MTRLDEP THEN DO:            
               CREATE mtrldepbuff.
               ASSIGN 
               mtrldepbuff.DEPNR = vald_depa
               mtrldepbuff.ENR = hjenr
               mtrldepbuff.LEVKOD = leverant
               mtrldepbuff.BENAMNING = mtrlbuff.BENAMNING
               mtrldepbuff.ENHET = mtrlbuff.ENHET
               mtrldepbuff.BESTPUNKT = tidinah.BESTPUNKT
               mtrldepbuff.BESTKV = tidinah.BESTKV
               mtrldepbuff.FACKID = ""
               mtrldepbuff.OMSATT = 0
               mtrldepbuff.NPRIS = tidinah.NPRIS / 100
               mtrldepbuff.BPRIS = tidinah.NPRIS / 100 
               mtrldepbuff.LAGER = TRUE
               mtrldepbuff.IBDATUM = ?         
               mtrldepbuff.INVDATUM = TODAY
               mtrldepbuff.SALDO = tidinah.ANTAL.
               IF mtrldepbuff.NPRIS = 0 OR mtrldepbuff.NPRIS = ? THEN DO:
                   mtrldepbuff.NPRIS = mtrlbuff.NPRIS.
                   mtrldepbuff.BPRIS = mtrlbuff.NPRIS.
               END.    
               timehjalp = timehjalp + 1. 
               CREATE inextradatatemp.          
               ASSIGN
               inextradatatemp.PROGRAM = "FIFO"                                  
               inextradatatemp.HUVUDINT =  mtrldepbuff.DEPNR
               inextradatatemp.HUVUDCH =  mtrldepbuff.ENR
               inextradatatemp.SOKCHAR[2] =  mtrldepbuff.BENAMNING
               inextradatatemp.SOKCHAR[3] =  mtrldepbuff.ENHET
               inextradatatemp.SOKDEC[1] =  mtrldepbuff.NPRIS
               inextradatatemp.SOKINT[1] =  mtrldepbuff.SALDO
               inextradatatemp.SOKDATE[1] =  tidinah.INDAT
               inextradatatemp.SOKINT[2] =  timehjalp.                                    
            END.
            ELSE DO:
               ASSIGN MTRLDEP.SALDO = MTRLDEP.SALDO + tidinah.ANTAL.
               timehjalp = timehjalp + 1. 
               CREATE inextradatatemp.          
               ASSIGN
               inextradatatemp.PROGRAM = "FIFO"                                  
               inextradatatemp.HUVUDINT =  MTRLDEP.DEPNR
               inextradatatemp.HUVUDCH =  MTRLDEP.ENR
               inextradatatemp.SOKCHAR[2] =  MTRLDEP.BENAMNING
               inextradatatemp.SOKCHAR[3] =  MTRLDEP.ENHET
               
               inextradatatemp.SOKINT[1] = tidinah.ANTAL
               inextradatatemp.SOKDATE[1] =  tidinah.INDAT
               inextradatatemp.SOKINT[2] =  timehjalp.
               IF tidinah.NPRIS = 0 OR tidinah.NPRIS = ? THEN DO:
                  inextradatatemp.SOKDEC[1] =  mtrlbuff.NPRIS.
               END.
               ELSE inextradatatemp.SOKDEC[1] =  tidinah.NPRIS / 100.   
                                 
            END.
            
                       
                     
         END.
         ELSE DO:                               
         
            PUT UNFORMATTED tidinah.ENR "  "  tidinah.benamning " " vald_depa SKIP.
            
         END.   
      END.            
   END.
   RUN extraspar_UI IN edataapph (INPUT TABLE inextradatatemp).
              

/*   OS-DELETE VALUE(kommando).   
    OUTPUT CLOSE. */
END PROCEDURE.   

                

   

>>>>>>> branch 'master' of file:///\\server05\delad\REMOTEGURU\GuruRemote.git
