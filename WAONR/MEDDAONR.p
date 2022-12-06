/*MEDDAONR.P*/
DEFINE INPUT PARAMETER aonrrec AS RECID NO-UNDO.
DEFINE INPUT PARAMETER tabortskap AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER servervar AS CHARACTER LABEL "Smtp Server" NO-UNDO.
DEFINE INPUT PARAMETER tillvar   AS CHARACTER LABEL "To" NO-UNDO.
DEFINE INPUT PARAMETER franvar   AS CHARACTER LABEL "From" NO-UNDO.
DEFINE OUTPUT PARAMETER skick  AS LOGICAL.        /* Email status  */
DEFINE OUTPUT PARAMETER efel AS CHARACTER.      /* Status txt  */

DEFINE VARIABLE str AS CHARACTER NO-UNDO.
 
 
IF Guru.Konstanter:globforetag = Guru.Konstanter:globforetag THEN RETURN. 
{SMTPDEF3.I}

FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR.
Guru.Konstanter:globforetag = FORETAG.FORETAG.
   

IF tabortskap = 1 THEN DO:
   FIND AONRTAB WHERE RECID(AONRTAB) = aonrrec NO-LOCK NO-ERROR.
   IF AONRTAB.DELNR NE 0 THEN RETURN.
   RUN medd_UI (INPUT "").   
END.
IF tabortskap = 2 THEN DO:
   FIND AONRTAB WHERE RECID(AONRTAB) = aonrrec NO-LOCK NO-ERROR.
   IF AONRTAB.DELNR NE 0 THEN RETURN.
   RUN bortmedd_UI.
END.
IF tabortskap = 3 THEN RUN emedd_UI.
/*(INPUT "NatRedovisning@graninge.se")*/
PROCEDURE bortmedd_UI:
   OPEN QUERY mq FOR EACH MEDDELANDE WHERE MEDDELANDE.SANDARE = "SUCCEL" AND 
   MEDDELANDE.MOTTAGARE = "SUCCEL" NO-LOCK.
   GET FIRST mq NO-LOCK.
   DO WHILE AVAILABLE(MEDDELANDE):
      REPEAT:
         IF INDEX(AONRTAB.AONR,MEDDELANDE.MEDD) > 0 THEN DO TRANSACTION:
            GET CURRENT mq EXCLUSIVE-LOCK.
            MEDDELANDE.MEDD = REPLACE(MEDDELANDE.MEDD,AONRTAB.AONR,"").
            MEDDELANDE.MEDD = REPLACE(MEDDELANDE.MEDD,SUBSTRING(AONRTAB.ORT,1,27),"").
            MEDDELANDE.MEDD = REPLACE(MEDDELANDE.MEDD,AONRTAB.OMRADE,"").
         END.
         ELSE LEAVE.
      END.
      GET NEXT mq NO-LOCK.                
   END.     
END PROCEDURE.
PROCEDURE medd_UI:
   DEFINE INPUT PARAMETER medpers LIKE  MEDDELANDE.MOTTAGARE.
   FIND FIRST MEDDELANDE WHERE MEDDELANDE.SANDARE = "SUCCEL" AND 
   MEDDELANDE.MOTTAGARE = "SUCCEL" AND LENGTH(MEDDELANDE.MEDD,"CHARACTER") < 30000
   EXCLUSIVE-LOCK NO-ERROR.
   IF NOT AVAILABLE MEDDELANDE THEN DO:
      ASSIGN  
      SUBSTRING(str,1,6) = CAPS(Guru.Konstanter:gaok)
      SUBSTRING(str,8,9) = "BENÄMNING"
      SUBSTRING(str,37,6) = CAPS(Guru.Konstanter:gomrk).  
      CREATE MEDDELANDE.
      ASSIGN               
      MEDDELANDE.SANDARE = "SUCCEL"
      MEDDELANDE.EMOTAGET = FALSE
      MEDDELANDE.MOTTAGARE = "SUCCEL"
      MEDDELANDE.SDATUM = TODAY.
      MEDDELANDE.MEDD = "NYA " + CAPS(Guru.Konstanter:gaok) + " FRÅN GURU" + CHR(10) +
      str + CHR(10).        
   END.  
   str = "".    
   ASSIGN  
   SUBSTRING(str,1,6) = AONRTAB.AONR
   SUBSTRING(str,8,27) = SUBSTRING(AONRTAB.ORT,1,27)
   SUBSTRING(str,37,6) = AONRTAB.OMRADE.             
   ASSIGN
   MEDDELANDE.MED = MEDDELANDE.MED + str + CHR(10).     
END PROCEDURE.
PROCEDURE emedd_UI.
   DEFINE VARIABLE ctillvar   AS CHARACTER LABEL "Kopia" NO-UNDO.
  
   Ctillvar =  "".
   OPEN QUERY mq FOR EACH MEDDELANDE WHERE MEDDELANDE.SANDARE = "SUCCEL" AND 
   MEDDELANDE.MOTTAGARE = "SUCCEL" NO-LOCK.
   GET FIRST mq NO-LOCK.
   DO WHILE AVAILABLE(MEDDELANDE):
      ASSIGN 
      mailhub             = servervar     
      EmailTo             = tillvar 
      EmailFrom           = ""
      EmailCC             = Ctillvar
      Attachmentstyp      = ""
      LocalFiles          = ""
      Subject             = "Nya " + LC(Guru.Konstanter:gaok) + " från Guru"
      Bodysmtp            = MEDDELANDE.MEDD
      MIMEHeader          = "type=text/html:charset=iso-8859-1:filetype=ascii"
      BodyType            = "".
      IF Guru.Konstanter:globforetag = "sund" THEN EmailFrom = "webguru@sundsvallenergi.se".
      IF Guru.Konstanter:globforetag = "SNAT" THEN EmailFrom = "@guru.sundsvallelnat.se".
      IF Guru.Konstanter:globforetag = "MISV" THEN EmailFrom = "webguru@mittsverigevatten.se".
      RUN smtpmail_UI (INPUT FALSE).
      IF oSuccessful = TRUE THEN DO TRANSACTION:
         oSuccessful = FALSE.      
         GET CURRENT mq SHARE-LOCK.
         DELETE MEDDELANDE.
      END.      
      GET NEXT mq NO-LOCK.                
   END.     
END PROCEDURE.  
