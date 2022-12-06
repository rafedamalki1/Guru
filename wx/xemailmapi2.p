This solution is made by Johann van der Merwe.
It allows one attachement. 

{windows.i}
 
RUN mapi ( "you@work.com",
           "yourwife@home.com",
           "late for dinner",
           "something came up..",
           "c:\images\flowers.bmp"
          ).
 
 
/* ========================================================================== */
 
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
SET-SIZE(TextPtr) = 16000. 
PUT-STRING(TextPtr,1) = Bodytext + (IF FilePathName = "":U 
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
SET-SIZE(RecipNamePtr) = LENGTH(RecipName) + 1./* maximum = 255 */ 
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
   PUT-LONG(FileDescPtr,9) = LENGTH(Bodytext) + 2.  /* Position */
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
RUN MAPISendMail IN Guru.Konstanter:hpApi
 (INPUT 0,
  INPUT 0,
  INPUT GET-POINTER-VALUE(MessageDescPtr),
  INPUT 11, /* 1 = MAPI_LOGON_UI + 2 = MAPI_NEW_SESSION + 8 = MAPI_DIALOG */
  INPUT 0,     
  OUTPUT ResultInt). 
 
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

 
