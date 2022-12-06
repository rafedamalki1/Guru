/*EPOSTN2.P*/
DEFINE TEMP-TABLE tidut
   FIELD UT AS CHARACTER FORMAT "X(90)".
 
DEFINE TEMP-TABLE Body1
   FIELD UT AS CHARACTER.
   
DEFINE TEMP-TABLE Body2
   FIELD UT AS CHARACTER.
   
DEFINE TEMP-TABLE Body3
   FIELD UT AS CHARACTER.

DEFINE TEMP-TABLE Body4
   FIELD UT AS CHARACTER.
   
DEFINE TEMP-TABLE Body5
   FIELD UT AS CHARACTER.      

DEFINE VAR Address  AS CHARACTER.      /* Email address of recipient */
DEFINE VAR Subject  AS CHARACTER.      /* Subject of email */
DEFINE VAR Filpath  AS CHARACTER.      /* Name of file to attach */
DEFINE VAR Filnamn  AS CHARACTER.      /* Name only  */
DEFINE VAR globanv AS CHARACTER NO-UNDO. 
DEFINE VAR globforetag AS CHARACTER NO-UNDO.
DEFINE VAR Ereslut  AS LOGICAL.        /* Email status  */
DEFINE VARIABLE skickadir AS INTEGER NO-UNDO.
DEFINE VARIABLE Body     AS CHARACTER.
DEFINE VARIABLE avsandare  AS CHARACTER NO-UNDO.
DEFINE VAR objSession AS COM-HANDLE.
&Scoped-define NEW   
&Scoped-define SHARED
{ANVPERS.I}
/*CREATE "MAPI.SESSION" objSession.*/
    DEF VAR I AS INTEGER.
REPEAT:
   I = I + 1.
   IF I > 650 THEN LEAVE.
   CREATE TIDUT.
   TIDUT.UT = "WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW".
END.
FIND FIRST tidut NO-LOCK NO-ERROR.
IF AVAILABLE tidut THEN DO:
   CREATE Body1.
   B1:
   FOR EACH tidut:      
      IF LENGTH(Body1.UT,"CHARACTER") < 30000 THEN DO:
         IF globforetag = "SUND" THEN DO:
            Body1.UT = Body1.UT  + TRIM(tidut.UT) + CHR(13) + CHR(10).
         END.
         ELSE DO:
            Body1.UT = Body1.UT  + TRIM(tidut.UT) + CHR(10).
         END.   
         DELETE tidut. 
      END.
      ELSE LEAVE B1.      
   END.
END.         
FIND FIRST tidut NO-LOCK NO-ERROR.
IF AVAILABLE tidut THEN DO:
   CREATE Body2.
   B2:
   FOR EACH tidut:    
      IF LENGTH(Body2.UT,"CHARACTER") < 30000 THEN DO:
         IF globforetag = "SUND" THEN DO:
            Body2.UT = Body2.UT  + TRIM(tidut.UT) + CHR(13) + CHR(10).
         END.
         ELSE DO:
            Body2.UT = Body2.UT  + TRIM(tidut.UT) + CHR(10).
         END.   
         DELETE tidut.
      END. 
      ELSE LEAVE B2.
   END.
END.    
FIND FIRST tidut NO-LOCK NO-ERROR.
IF AVAILABLE tidut THEN DO:
   CREATE Body3.
   B3:
   FOR EACH tidut:    
      IF LENGTH(Body3.UT,"CHARACTER") < 30000 THEN DO:
         IF globforetag = "SUND" THEN DO:
            Body3.UT = Body3.UT  + TRIM(tidut.UT) + CHR(13) + CHR(10).
         END.
         ELSE DO:
            Body3.UT = Body3.UT  + TRIM(tidut.UT) + CHR(10).
         END.   
         DELETE tidut.
      END. 
      ELSE LEAVE B3.
   END.
END.    
FIND FIRST tidut NO-LOCK NO-ERROR.
IF AVAILABLE tidut THEN DO:
   MESSAGE "Ett fel har uppstått kontakta Niklas Johnson Elpool 090/184540. Ange EDI-fel"
   VIEW-AS ALERT-BOX TITLE "Medelande".
   RETURN.
END.

{windows.i}
body = "".
IF Address = "" THEN DO:
   Address = "Till vem".
   skickadir = 11.
END.
ELSE skickadir = 0.
RUN mapi 
   ( avsandare,                 /*avsändare*/
     Address,
     Subject,
     Body,
     Filpath).
PROCEDURE MAPI :
   DEFINE INPUT PARAMETER OriginName   AS CHARACTER.
   DEFINE INPUT PARAMETER RecipName    AS CHARACTER.
   DEFINE INPUT PARAMETER Subject      AS CHARACTER.
   DEFINE INPUT PARAMETER Bodytext     AS CHARACTER.
   DEFINE INPUT PARAMETER FilePathName AS CHARACTER. 
   DEFINE VARIABLE SubjPtr AS MEMPTR.
   SET-SIZE(SubjPtr) = LENGTH(Subject) + 1. /* maximum = 255 */ 
   PUT-STRING(SubjPtr,1) = Subject.
   DEFINE VARIABLE TextPtr AS MEMPTR.
   SET-SIZE(TextPtr) = 90000. 
   /*SET-SIZE(TextPtr) = 60000. */
   FIND FIRST Body1 NO-LOCK NO-ERROR.
   FIND FIRST Body2 NO-LOCK NO-ERROR.
   IF NOT AVAILABLE Body2 THEN DO:
      CREATE Body2.
   END.
   
   FIND FIRST Body3 NO-LOCK NO-ERROR.
   IF NOT AVAILABLE Body3 THEN DO:
      CREATE Body3.
   END. 
   PUT-STRING(TextPtr,1) = Body1.UT + Body2.UT + Body3.UT + (IF FilePathName = "":U 
                                    THEN "":U 
                                    ELSE CHR(10) + CHR(10) + " ":U).   
    /* if file attached place at end of Bodytext with line skip */
 
   /* ---------------- Build Originator details ------------------------ */
   DEFINE VARIABLE OriginNamePtr AS MEMPTR.
   SET-SIZE(OriginNamePtr) = LENGTH(OriginName) + 1.  /* maximum = 255 */
   PUT-STRING(OriginNamePtr,1) = OriginName.  /* Originator name */
 
   DEFINE VARIABLE OriginDescPtr AS MEMPTR.
   SET-SIZE(OriginDescPtr) = 24.
   PUT-LONG(OriginDescPtr,1) = 0. /* Reserved */ 
   PUT-LONG(OriginDescPtr,5) = 0. /* RecipClass 0 = MAPI_ORIG */ 
   PUT-LONG(OriginDescPtr,9) = GET-POINTER-VALUE(OriginNamePtr).  /* Name */
   PUT-LONG(OriginDescPtr,13) = 0. /* Address */ 
   PUT-LONG(OriginDescPtr,17) = 0. /* EID Size */ 
   PUT-LONG(OriginDescPtr,21) = 0. /* Entry ID */
    
   /* ----------------Build Recipient details -------------------------- */
   DEFINE VARIABLE RecipNamePtr AS MEMPTR.
   SET-SIZE(RecipNamePtr) = LENGTH(RecipName) + 1. /* maximum = 255 */ 
   PUT-STRING(RecipNamePtr,1) = RecipName. /* Recipient name */
   DEFINE VARIABLE RecipDescPtr AS MEMPTR.
   SET-SIZE(RecipDescPtr) = 24.
   PUT-LONG(RecipDescPtr,1) = 0. /* Reserved */ 
   PUT-LONG(RecipDescPtr,5) = 1. /* RecipClass 1 = MAPI_TO */ 
   PUT-LONG(RecipDescPtr,9) = GET-POINTER-VALUE(RecipNamePtr).  /* Name */
   PUT-LONG(RecipDescPtr,13) = 0. /* Address */ 
   PUT-LONG(RecipDescPtr,17) = 0. /* EID Size */ 
   PUT-LONG(RecipDescPtr,21) = 0. /* Entry ID */
    
   /* --------------- Build File Details ------------------- */
   IF FilePathName <> "":U THEN DO:
      DEFINE VARIABLE FilePathNamePtr AS MEMPTR.
      SET-SIZE(FilePathNamePtr) = LENGTH(FilePathName) + 1.  /* maximum = 255 */
      PUT-STRING(FilePathNamePtr,1) = FilePathName.  /* File pathname */
    
      DEFINE VARIABLE FILENAME AS CHARACTER NO-UNDO.
      FILENAME = SUBSTRING(FilePathName,R-INDEX(FilePathName,"\":U) + 1).
      /* extract filename (starting on last \) from filefullname */
      FILENAME = "     ":U + FILENAME.
      /* for some strange reason the first five chars disappear */
    
      DEFINE VARIABLE FileNamePtr AS MEMPTR.
      SET-SIZE(FileNamePtr) = LENGTH(FILENAME) + 1. /* maximum = 255 */ 
      PUT-STRING(FileNamePtr,1) = FILENAME. /* File name */
    
      DEFINE VARIABLE FileDescPtr AS MEMPTR.
      SET-SIZE(FileDescPtr) = 24.
      PUT-LONG(FileDescPtr,1) = 0. /* Reserved */ 
      PUT-LONG(FileDescPtr,5) = 0. /* Flags 0 = data file */
      PUT-LONG(FileDescPtr,9) = LENGTH(Body1.UT + Body2.UT + Body3.UT) + 2.  /* Position */
      PUT-LONG(FileDescPtr,13) = GET-POINTER-VALUE(FilePathNamePtr).  /* PathName */
      PUT-LONG(FileDescPtr,17) = GET-POINTER-VALUE(FileNamePtr). /* FileName */ 
      PUT-LONG(FileDescPtr,21) = 0. /* FileType */
   END.
   /* ---------- Build Message Details ------------------- */
   DEFINE VARIABLE MessageDescPtr AS MEMPTR.
   SET-SIZE(MessageDescPtr) = 48.
   PUT-LONG(MessageDescPtr,1) = 0.  /* Reserved */
   PUT-LONG(MessageDescPtr,5) = GET-POINTER-VALUE(SubjPtr).  /* Subject */
   PUT-LONG(MessageDescPtr,9) = GET-POINTER-VALUE(TextPtr).  /* Text */
   PUT-LONG(MessageDescPtr,13) = 0. /* MessageType */ 
   PUT-LONG(MessageDescPtr,17) = 0. /* DateReceived */ 
   PUT-LONG(MessageDescPtr,21) = 0. /* ConversationID */ 
   PUT-LONG(MessageDescPtr,25) = 1.  /* Flags */
   PUT-LONG(MessageDescPtr,29) = GET-POINTER-VALUE(OriginDescPtr).  /* Originator */
   PUT-LONG(MessageDescPtr,33) = 1.  /* RecipCount */
   PUT-LONG(MessageDescPtr,37) = GET-POINTER-VALUE(RecipDescPtr).  /* Recips */
   PUT-LONG(MessageDescPtr,41) = (IF FilePathName = "":U 
                                     THEN 0 
                                     ELSE 1).  /* FileCount */
   PUT-LONG(MessageDescPtr,45) = (IF FilePathName = "":U 
                                     THEN 0 
                                     ELSE GET-POINTER-VALUE(FileDescPtr)). /* Files */
   /* EO Build Message Details */
    
   /* -------- Send Message ------------ */
   DEFINE VARIABLE ResultInt AS INTEGER NO-UNDO.
   IF skickadir = 0  THEN DO:
      RUN MAPISendMail IN Guru.Konstanter:hpApi
      (INPUT 0,       /* mapi session handle */
       INPUT 0,       /* parent window handle */   
       INPUT GET-POINTER-VALUE(MessageDescPtr),
       INPUT 1, /* flags */ /* 1 = MAPI_LOGON_UI + 2 = MAPI_NEW_SESSION + 8 = MAPI_DIALOG */
       INPUT 0,     /* reserved, must be 0 */   
       OUTPUT ResultInt). /* error status */           
   END.
   ELSE DO:
      RUN MAPISendMail IN Guru.Konstanter:hpApi
      (INPUT 0,       /* mapi session handle */
       INPUT 0,       /* parent window handle */   
       INPUT GET-POINTER-VALUE(MessageDescPtr),
       INPUT 11, /* flags */ /* 1 = MAPI_LOGON_UI + 2 = MAPI_NEW_SESSION + 8 = MAPI_DIALOG */
       INPUT 0,     /* reserved, must be 0 */   
       OUTPUT ResultInt). /* error status */           
   END.
   RUN MapiReturnCode (ResultInt).   
   /* ------- Free memory ------------ */
   SET-SIZE(SubjPtr)         = 0.
   SET-SIZE(TextPtr)         = 0. 
   SET-SIZE(OriginNamePtr)   = 0.
   SET-SIZE(OriginDescPtr)   = 0.
   SET-SIZE(RecipNamePtr)    = 0.
   SET-SIZE(RecipDescPtr)    = 0.
   SET-SIZE(FilePathNamePtr) = 0.
   SET-SIZE(FileNamePtr)     = 0.
 
END PROCEDURE.

PROCEDURE MapiReturnCode  :
   DEFINE  INPUT PARAMETER ResultInt AS INTEGER. /* result from MAPISendMail */
   DEFINE VARIABLE RESULT AS CHARACTER NO-UNDO.
   IF ResultInt <> 0 THEN DO:  /* 0 = Success */ 
      CASE ResultInt:
         WHEN  1 THEN RESULT = "User Abort".
         WHEN  2 THEN RESULT = "Failure".
         WHEN  3 THEN RESULT = "Login Failure".
         WHEN  4 THEN RESULT = "Disk Full".
         WHEN  5 THEN RESULT = "Insufficient Memory".
         WHEN  6 THEN RESULT = "Blk Too Small".
         WHEN  8 THEN RESULT = "Too Many Sessions".
         WHEN  9 THEN RESULT = "Too Many Files".
         WHEN 10 THEN RESULT = "Too Many Recipients".
         WHEN 11 THEN RESULT = "Attachment Not Found".
         WHEN 12 THEN RESULT = "Attachment Open Failure".
         WHEN 13 THEN RESULT = "Attachment Write Failure".
         WHEN 14 THEN RESULT = "Unknown Recipient".
         WHEN 15 THEN RESULT = "Bad Recipient type".
         WHEN 16 THEN RESULT = "No Messages".
         WHEN 17 THEN RESULT = "Invalid Message".
         WHEN 18 THEN RESULT = "Bodytext Too Large".
         WHEN 19 THEN RESULT = "Invalid Session".
         WHEN 20 THEN RESULT = "Type Not Supported".
         WHEN 21 THEN RESULT = "Ambiguous Recipient".
         WHEN 22 THEN RESULT = "Message in use".
         WHEN 23 THEN RESULT = "Network failure".
         WHEN 24 THEN RESULT = "Invalid edit fields".
         WHEN 25 THEN RESULT = "Invalid recipients".
         WHEN 26 THEN RESULT = "Feature not supported".
         OTHERWISE RESULT    = "Unknown error".
      END CASE.
      
   END.
   ASSIGN
   Ereslut = FALSE.                                    
       /*
       Emailtxt = "E-posten är nu skickad till utkorgen". 
   */
END PROCEDURE.
 
