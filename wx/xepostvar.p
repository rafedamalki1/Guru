/***********************************************************************
  Program:   mapi16.p
  Author :   Todd Nist/Dmitri Levin
  Date   :   10/07/1998
  Purpose:   Program to send e-mail from Progress 8.1 and below

 for example

 run c:\temp\mapi16.p(
    'yourname@host.com', /* from */
    'yourname@host.com', /* to */
    'This is a test of MAPI with DLL calls from Progress ' + proversion, /* subject */
    'This is test of mail from Progress ' + proversion,  /* body */
    'c:\config.sys', /* attachment */
     output any-logical-var /* Success or failure */ ).
  
****************************** PARAMETERS          ****************************/

define input parameter  from-e-mail-addr as character no-undo.
define input parameter  to-e-mail-addr   as character no-undo.
define input parameter  subject-text     as character no-undo.
define input parameter  message-text     as character no-undo.
define input parameter  attach-file-name as character no-undo.
define output parameter bResult          as logical   no-undo.

/* ********************  Preprocessor and variables Definitions  ******************** */

{mapi16.i} 
define variable iRetCode  as  integer no-undo.
define variable iSession  as  integer no-undo.

/* ***********************  Main block  ********************** */

run logon(input '':U,
          input '':U,
          output iSession).
                   
if iSession <> 0 then /* valid session handle */
do:
    run SendMail(input from-e-mail-addr,    /* from */
                 input to-e-mail-addr,      /* to */
                 input subject-text,        /* Subject */
                 input message-text,        /* Body */
                 input attach-file-name).   /* Attachment */
end.
  
/* ************************  Procedure Implementations ***************** */

procedure GetMapiError: 

 define input parameter piRetCode as int no-undo.

 define variable cResult as char no-undo.

 if piRetCode <> 0 then  /* 0 = Success */    
 do:
  case piRetCode:
    when  1 then cResult = "User Abort".     
    when  2 then cResult = "Failure".
    when  3 then cResult = "Login Failure".
    when  4 then cResult = "Disk Full".
    when  5 then cResult = "Insufficient Memory".
    when  6 then cResult = "Blk Too Small".
    when  8 then cResult = "Too Many Sessions".
    when  9 then cResult = "Too Many Files".
    when 10 then cResult = "Too Many Recipients".
    when 11 then cResult = "Attachment Not Found".
    when 12 then cResult = "Attachment Open Failure".
    when 13 then cResult = "Attachment Write Failure".
    when 14 then cResult = "Unknown Recipient".
    when 15 then cResult = "Bad Recipient type".
    when 16 then cResult = "No Messages".
    when 17 then cResult = "Invalid Message".
    when 18 then cResult = "Bodytext Too Large".
    when 19 then cResult = "Invalid Session".
    when 20 then cResult = "Type Not Supported".
    when 21 then cResult = "Ambiguous Recipient".
    when 22 then cResult = "Message in use".
    when 23 then cResult = "Network failure".
    when 24 then cResult = "Invalid edit fields".
    when 25 then cResult = "Invalid recipients".
    when 26 then cResult = "Feature not supported".
    otherwise cResult    = "Unknown error".
  end case.  

  message substitute('&1, MAPI Error (&2) encountered.',
                      cResult,
                      piRetCode).  
 end.

end procedure. /* GetMapiError */

procedure logoff :

    run MAPILogoff(input iSession,
                   input 0, /* UIParam */
                   input 0, /* Flags */
                   input 0,  /* Reserved */
                   output iRetCode).
end procedure.

procedure logon :

  define input  parameter pcProfileName as  character no-undo.
  define input  parameter pcPassword    as  character no-undo.
  define output parameter iSession      as  integer   no-undo.
     
  run MAPILogon(input  0,
                input  pcProfileName,  /* user id */
                input  pcPassword,  /* password */
                input  1,   /* flags */
                input  0,   /* reserved */
                output iSession,
                output iRetCode). 

  run GetMapiError(iRetCode).

  iSession = if iRetCode = {&SUCCESS_SUCCESS} THEN
                iSession
             else 0.   /* Function return value. */
end procedure.

procedure SendMail:

  define input parameter pcOriginator  as character no-undo.
  define input parameter pcRecipient   as character no-undo.
  define input parameter pcSubject     as character no-undo.
  define input parameter pcBody        as character no-undo.
  define input parameter pcAttachment  as character no-undo.

  define var lpMessage          as  memptr  no-undo.
  define var lpOriginator       as  memptr  no-undo.
  define var lpOrigDesc         as  memptr  no-undo.
  define var lpRecipient        as  memptr  no-undo.
  define var lpRecipDesc        as  memptr  no-undo.
  define var lpSubject          as  memptr  no-undo.
  define var lpBody             as  memptr  no-undo.
  define var lpFilePathName     as  memptr  no-undo.
  define var lpFileName         as  memptr  no-undo.
  define var lpFilePath         as  memptr  no-undo.
  define var lpFileDesc         as  memptr  no-undo.
  define var cFileName          as  char    no-undo.
  define var cFilePath          as  char    no-undo.
  define var iOffset            as  int     no-undo.
    
  
  /*------------------- Build Originator structure ----------------------------*/
  
  assign
      set-size(lpOriginator)             = length(pcOriginator) + 1
      put-string(lpOriginator,1)         = pcOriginator
      set-size(lpOrigDesc)               = 25
      put-long(lpOrigDesc,1)             = 0  /* Reserved */
      put-long(lpOrigDesc,5)             = 0  /* Recip Class MAPI_ORIG */
      put-long(lpOrigDesc,9)             = get-pointer-value(lpOriginator) /* Names */
      put-long(lpOrigDesc,13)            = 0  /* Address */
      put-long(lpOrigDesc,17)            = 0  /* EID Size */
      put-long(lpOrigDesc,21)            = 0.
  
  /*------------------- Build Recipient structure ----------------------------*/
  
  assign  
      set-size(lpRecipient)              = length(pcRecipient) + 1
      put-string(lpRecipient,1)          = pcRecipient
      set-size(lpRecipDesc)              = 25
      put-long(lpRecipDesc,1)            = 0  /* Reserved */
      put-long(lpRecipDesc,5)            = 1  /* Recip Class MAPI_TO */
      put-long(lpRecipDesc,9)            = get-pointer-value(lpRecipient) /* Names */
      put-long(lpRecipDesc,13)           = 0  /* Address */
      put-long(lpRecipDesc,17)           = 0  /* EID Size */
      put-long(lpRecipDesc,21)           = 0. /* Entry Id */
  
  /*---------------------- Build Subject structure ----------------------------*/
  
  assign
      set-size(lpSubject)                = length(pcSubject) + 1
      put-string(lpSubject,1)            = pcSubject.
  
  /*------------------- Build Message Body structure --------------------------*/
  
  assign
      set-size(lpBody)                 = 16000
      put-string(lpBody,1)             = pcBody + (if pcAttachment = '':U then '':U
                                                   else chr(10) + chr(10) + ' ':U).

  /*------------------- Build Attachments structure ---------------------------*/
  
  if pcAttachment <> "" then
  do:
    assign
        cFileName = entry(num-entries(pcAttachment, '\':U),pcAttachment, '\':U)
        cFilePath = replace(pcAttachment,cFilename,'':U)

      /*----------------- Set the full path spec -----------------*/
        set-size(lpFilePathName)        = length(pcAttachment) + 1
        put-string(lpFilePathName,1)  = pcAttachment

      /*----------------- Set the only file spec -----------------*/
        set-size(lpFileName)            = length(cFileName) + 1
        put-string(lpFileName, 1)       = cFileName

      /*----------------- Set up attachment structure -----------------*/
        set-size(lpFileDesc)            = 25
        put-long(lpFileDesc,1)          = 0 /* reserved */
        put-long(lpFileDesc,5)          = 0 /* flags */
        put-long(lpFileDesc,9)          = length(pcBody) + 2 /* position */
        put-long(lpFileDesc,13)         = get-pointer-value(lpFilePathName)
        put-long(lpFileDesc,17)         = get-pointer-value(lpFileName)
        put-long(lpFileDesc,21)         = 0
        .
  end.
  
  /*---------------------- Build Message structure ----------------------------*/
  
  assign
      set-size(lpMessage)            = 49
      put-long(lpMessage,1)          = 0  /* Reserved */
      put-long(lpMessage,5)          = get-pointer-value(lpSubject) /* subject */
      put-long(lpMessage,9)          = get-pointer-value(lpBody)    /* body */
      put-long(lpMessage,13)         = 0  /* Message type */
      put-long(lpMessage,17)         = 0  /* Date Received */
      put-long(lpMessage,21)         = 0  /* ConversationId */
      put-long(lpMessage,25)         = 1  /* Flags */
      put-long(lpMessage,29)         = get-pointer-value(lpOrigDesc) /* Originator */
      put-long(lpMessage,33)         = 1  /* Recipient Count */
      put-long(lpMessage,37)         = get-pointer-value(lpRecipDesc) /* Recipient */
      put-long(lpMessage,41)         = (if pcAttachment = '':U then 0
                                        else 1)  /* Number of attachments */
      put-long(lpMessage,45)         = (if pcAttachment = '':U then 0
                                        else get-pointer-value(lpFileDesc)).


  /*---------------------------- Send Message now -----------------------------*/

  run MAPISendMail(input 0,  /* current session */
                   input 0,         /* UIParam */
                   input lpMessage,     /* pointer to message structure */
                   input 0, /* 11, /* flags, 1 = MAPI_LOGON_UI + 2 = MAPI_NEW_SESSION + 8 = MAPI_DIALOG */ */
                   input 0, /* reserved */
                   output iRetCode).
 
  run GetMapiError(iRetCode).

  bResult = if iRetCode eq 0 then true else false.
    
  /*---------------------------- Deallocate memory ----------------------------*/
    
  assign
      set-size(lpMessage)           = 0
      set-size(lpOriginator)        = 0
      set-size(lpOrigDesc)          = 0
      set-size(lpRecipient)         = 0
      set-size(lpRecipDesc)         = 0
      set-size(lpSubject)           = 0
      set-size(lpBody)              = 0
      set-size(lpFilePathName)      = 0
      set-size(lpFileName)          = 0
      set-size(lpFileDesc)          = 0
      set-size(lpFilePath)          = 0
      .
    
end procedure. /* SendMail */
  
