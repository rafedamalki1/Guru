 /*LOSENKOLL.P*/
 /*Anders Olsson Elpool i Ume� AB  23 nov 2017 17:05:11 
 anv�nds inte 
 */
DEFINE INPUT PARAMETER typkoll AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER globanv AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER globlos AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER globallao AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER globallpers AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER globavd AS INTEGER NO-UNDO. 
DEFINE OUTPUT PARAMETER globomr AS CHARACTER NO-UNDO. 
DEFINE OUTPUT PARAMETER globstorb AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER globstorh AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER globniv AS INTEGER NO-UNDO. 
DEFINE OUTPUT PARAMETER globpersnamn AS CHARACTER NO-UNDO. 
DEFINE OUTPUT PARAMETER globanvpkod AS CHARACTER NO-UNDO. 
DEFINE OUTPUT PARAMETER globanvavdnr AS INTEGER NO-UNDO. 
DEFINE OUTPUT PARAMETER musz AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER globjid AS CHARACTER NO-UNDO.
DEFINE VARIABLE restuserh AS HANDLE NO-UNDO.
DEFINE TEMP-TABLE felmeddtemp 
   FIELD FELMEDD AS CHARACTER
   FIELD VAL     AS INTEGER.  
/*FINNS ANV�NDAREN OCH �R DEN KOPPLAD TII PERSONALTAB*/
IF typkoll = 99 THEN DO:
   FIND FIRST ANVANDARE WHERE ANVANDARE.ANVANDARE = globanv  
   USE-INDEX ANDV NO-LOCK NO-ERROR.
   IF AVAILABLE ANVANDARE THEN DO:
      globanvpkod = ANVANDARE.PERSONALKOD.
   END.     
   RETURN.
END.   
 
/*KONTROLL OM R�TT L�SEN*/
IF typkoll = 1 THEN DO:
   FIND FIRST ANVANDARE WHERE ANVANDARE.ANVANDARE = globanv AND 
   ANVANDARE.AV-LOSEN = globlos USE-INDEX ANDV NO-LOCK NO-ERROR.    
END.

IF typkoll = 2 THEN DO:
   FIND FIRST ANVANDARE WHERE ANVANDARE.ANVANDARE = globanv 
   USE-INDEX ANDV NO-LOCK NO-ERROR.
END.
/*BYT L�SEN*/
IF typkoll = 3 THEN DO TRANSACTION:
   FIND FIRST ANVANDARE WHERE ANVANDARE.ANVANDARE = globanv 
   USE-INDEX ANDV EXCLUSIVE-LOCK NO-ERROR.
   ASSIGN ANVANDARE.AV-LOSEN = globlos.
   RUN RESTUSERS.P PERSISTENT SET restuserh. 
   RUN restBytLosenord_UI IN restuserh(INPUT ANVANDARE.PERSONALKOD,INPUT ANVANDARE.AV-LOSEN, OUTPUT TABLE felmeddtemp). 
   RUN avslutarestUsr_UI IN restuserh.
   DELETE PROCEDURE restuserh NO-ERROR.
   restuserh = ?.
END.
IF NOT AVAILABLE ANVANDARE THEN DO:
   musz = TRUE.      
   RETURN.
END.
ASSIGN 
globanvavdnr = ANVANDARE.AVDELNINGNR
globanvpkod = ANVANDARE.PERSONALKOD
globlos = ANVANDARE.AV-LOSEN
globstorb = ANVANDARE.SIDL      /*bredd*/
globstorh = ANVANDARE.SIDS     /*h�jd*/
globniv = ANVANDARE.AV-LEVEL
globallpers = ANVANDARE.ALLPERS
globallao = ANVANDARE.ALLAONR
globpersnamn = ANVANDARE.AV-NAMN   .     
IF ANVANDARE.PERSONALKOD NE "" THEN DO:
    FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = ANVANDARE.PERSONALKOD
    USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
    IF AVAILABLE PERSONALTAB THEN DO:
       globomr = PERSONALTAB.OMRADE.
       FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE =  PERSONALTAB.OMRADE NO-LOCK NO-ERROR.
       IF AVAILABLE OMRADETAB THEN DO:
          FIND FIRST AVDELNING WHERE AVDELNING.AVDELNINGNR  = OMRADETAB.AVDELNINGNR NO-LOCK NO-ERROR.
          IF AVAILABLE AVDELNING THEN DO:
             FIND FIRST JURPERS WHERE JURPERS.JUDID = AVDELNING.POSTANST NO-LOCK NO-ERROR.
             IF AVAILABLE JURPERS THEN DO:
                globjid = JURPERS.JUDID.               
             END.   
          END.   
      END.
   END.
END.
IF Guru.Konstanter:globanv = CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79) THEN globjid = "".


