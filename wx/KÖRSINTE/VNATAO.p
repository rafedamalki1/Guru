/*VNATAO.P*/
DEFINE NEW SHARED VARIABLE regdagnamn AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE NEW SHARED VARIABLE regvnr AS INTEGER FORMAT "999" NO-UNDO.
DEFINE NEW SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE chdatvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE aorec AS RECID NO-UNDO.
DEFINE BUFFER fakbuff FOR FAKTPLAN.
DEFINE BUFFER aobuff FOR AONRTAB.

DEFINE TEMP-TABLE tidin
   FIELD AONR               AS CHARACTER     
   FIELD BENAMNING          AS CHARACTER
   FIELD OMRADE             AS CHARACTER
   INDEX AONR IS PRIMARY AONR.
   
DEFINE TEMP-TABLE feltemp
   FIELD AONR               AS CHARACTER     
   FIELD BENAMNING          AS CHARACTER
   FIELD OMRADE             AS CHARACTER
   FIELD FEL                AS CHARACTER
   INDEX AONR IS PRIMARY AONR.   

DEFINE INPUT PARAMETER TABLE FOR tidin.
EMPTY TEMP-TABLE feltemp NO-ERROR. 

FOR EACH tidin USE-INDEX AONR NO-LOCK:               
   DO TRANSACTION:                        
      FIND FIRST AONRTAB WHERE AONRTAB.AONR = tidin.AONR AND 
      AONRTAB.DELNR = 0    
      USE-INDEX AONR EXCLUSIVE-LOCK NO-ERROR.
      IF NOT AVAILABLE AONRTAB THEN DO:
         FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = tidin.OMRADE
         NO-LOCK NO-ERROR.
         IF AVAILABLE OMRADETAB THEN DO:
            CREATE AONRTAB.
            ASSIGN      
            AONRTAB.AONR = tidin.AONR
            AONRTAB.DELNR = 0        
            AONRTAB.ORT = tidin.BENAMNING 
            AONRTAB.OMRADE = tidin.OMRADE
            AONRTAB.BESTID = tidin.OMRADE
            AONRTAB.ARBARTKOD = 0
            AONRTAB.ARBBESKED = FALSE
            AONRTAB.ARTAL = ?            
            AONRTAB.BETNR = 0
            AONRTAB.ELVOMRKOD = 0
            AONRTAB.FAKTNR = 0
            AONRTAB.FASTKALK = FALSE
            AONRTAB.KALKNR = ?
            AONRTAB.PKOD = 0
            AONRTAB.PLANNR = ?
            AONRTAB.PRISTYP = "TOT.PRIS."
            AONRTAB.STARTDATUM = ?
            AONRTAB.TRAKTAMENTE = 0
            AONRTAB.UTFARDAT = ""
            AONRTAB.UTRYCKNING = FALSE
            AONRTAB.AUTOREG = TRUE
            AONRTAB.AONRAVDATUM = 01/01/91
            AONRTAB.FASTAAONR = FALSE
            regdatum = DATE(01,01,YEAR(TODAY)).
            RUN REGDAG.P.
            RUN REGVEC.P.
            ASSIGN
            AONRTAB.STARTDAG = regdagnamn       
            AONRTAB.STARTVNR = regvnr
            regdatum = DATE(12,28,YEAR(TODAY)).
            RUN REGDAG.P.
            RUN REGVEC.P.         
            ASSIGN
            AONRTAB.SLUTDAG = regdagnamn       
            AONRTAB.SLUTVNR = regvnr.
         END.
         ELSE DO:
            CREATE feltemp.
            ASSIGN
            feltemp.AONR = tidin.AONR
            feltemp.BENAMNING = tidin.BENAMNING
            feltemp.OMRADE = tidin.OMRADE
            feltemp.FEL = "Område saknas".
         END.   
      END.   
      ELSE DO:
         IF AONRTAB.OMRADE = tidin.OMRADE THEN DO:
            AONRTAB.ORT = tidin.BENAMNING.
         END.
         ELSE DO:
            CREATE feltemp.
            ASSIGN
            feltemp.AONR = tidin.AONR
            feltemp.BENAMNING = tidin.BENAMNING
            feltemp.OMRADE = tidin.OMRADE
            feltemp.FEL = "Område har ändrats".
         END.
      END.
   END. 
END.
FIND FIRST feltemp NO-LOCK NO-ERROR.
IF AVAILABLE feltemp THEN DO:
   RUN medd_UI.
END.

PROCEDURE medd_UI.
   DEFINE VARIABLE str AS CHARACTER NO-UNDO.
   FIND FIRST MEDDELANDE WHERE MEDDELANDE.SANDARE = CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79) AND 
   MEDDELANDE.MOTTAGARE = "HJSS" AND LENGTH(MEDDELANDE.MEDD,"CHARACTER") < 30000
   EXCLUSIVE-LOCK NO-ERROR.
   IF NOT AVAILABLE MEDDELANDE THEN DO:
      CREATE MEDDELANDE.
      ASSIGN               
      MEDDELANDE.SANDARE = CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79)
      MEDDELANDE.EMOTAGET = FALSE
      MEDDELANDE.MOTTAGARE = "HJSS"
      MEDDELANDE.SDATUM = TODAY.
      MEDDELANDE.MEDD = "FELAKTIGA AONR" + CHR(10).        
   END.     
   FOR EACH feltemp:      
      ASSIGN  
      str = ""
      str = str + feltemp.AONR + " "
      str = str + SUBSTRING(feltemp.BENAMNING,1,20) + " "
      str = str + feltemp.OMRADE + " "
      str = str + feltemp.FEL. 
      ASSIGN
      MEDDELANDE.MED = MEDDELANDE.MED + str + CHR(10).     
   END.   
END PROCEDURE.
