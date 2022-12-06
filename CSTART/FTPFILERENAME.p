/*FTPFILERENAME.p*/
&SCOPE  MAX_PATH 260
&SCOPE FILE_ATTRIBUTE_NORMAL  128

/* Internet constants */

&SCOPE INTERNET_OPEN_TYPE_PRECONFIG    0
/* indicates to use config information from registry */
&SCOPE INTERNET_FLAG_EXISITING_CONNECT 536870912
/* used for ftp connections */
&SCOPE INTERNET_FLAG_PASSIVE           134217728

/* Flags for FTP transfer mode */
&SCOPE  FTP_TRANSFER_TYPE_ASCII  1   /* 0x00000001 */
&SCOPE  FTP_TRANSFER_TYPE_BINARY 2   /* 0x00000002 */


&SCOPE INTERNET_DEFAULT_FTP_PORT     21
&SCOPE INTERNET_DEFAULT_GOPHER_PORT  70
&SCOPE INTERNET_DEFAULT_HTTP_PORT    80
&SCOPE INTERNET_DEFAULT_HTTPS_PORT  443
&SCOPE INTERNET_DEFAULT_SOCKS_PORT 1080

/* Type of service to access */
&SCOPE INTERNET_SERVICE_FTP    1
&SCOPE INTERNET_SERVICE_GOPHER 2
&SCOPE INTERNET_SERVICE_HTTP   3
&Scoped-define NEW NEW 
DEFINE {&NEW} SHARED TEMP-TABLE felmeddtemp 
  FIELD FELMEDD AS CHARACTER
  FIELD VAL AS INTEGER.
DEFINE INPUT PARAMETER remotLogin  AS CHARACTER NO-UNDO.  /*login*/
DEFINE INPUT PARAMETER remotpasswd AS CHARACTER NO-UNDO.  /*lösen*/
DEFINE INPUT PARAMETER skicka      AS LOGICAL NO-UNDO.    /*skicka = true hämta = false*/
DEFINE INPUT PARAMETER ascibin     AS INTEGER NO-UNDO.    /*1=ascii 2 = bin*/
DEFINE INPUT PARAMETER cLocalFile  AS CHARACTER NO-UNDO.   /*fil på local pc*/ 

DEFINE INPUT PARAMETER cRemoteFile AS CHARACTER NO-UNDO.   /*fil på connectad maskin*/
DEFINE INPUT PARAMETER cReNameFile AS CHARACTER NO-UNDO.   /*fil på connectad maskin*/
DEFINE INPUT PARAMETER motagandemaskin AS CHARACTER NO-UNDO.  /*ftp maskin */
DEFINE OUTPUT PARAMETER TABLE FOR felmeddtemp.

DEFINE VARIABLE portremot AS INTEGER NO-UNDO.

portremot =  {&INTERNET_DEFAULT_FTP_PORT}. 
IF INDEX(motagandemaskin,":") > 0 THEN DO:
   portremot = INTEGER(SUBSTRING(motagandemaskin,INDEX(motagandemaskin,":") + 1)).
   motagandemaskin = SUBSTRING(motagandemaskin,1,INDEX(motagandemaskin,":") - 1).
END.   
DEFINE VARIABLE antali AS INTEGER.
DEFINE TEMP-TABLE provag
   FIELD VAGNR AS INTEGER
   FIELD VAG AS CHARACTER
   INDEX VAGNR IS PRIMARY VAGNR.
DEFINE VARIABLE hInternetSession   AS INTEGER  NO-UNDO.
/* handle to the ftp session inside the internet connection */
DEFINE VARIABLE hFTPSession        AS INTEGER  NO-UNDO.
/* current directory which we are processing */
DEFINE VARIABLE cCurrentDir        AS CHARACTER NO-UNDO.
/*FUNKTIONER*/

FUNCTION CloseInternetConnection RETURNS LOGICAL
  ( input phInternetSession as integer ) :
  define variable iRetCode      as  integer no-undo.

  run InternetCloseHandle(input  phInternetSession,
                          output iRetCode).     
  RETURN iRetCode > 0.   /* Function return value. */

END FUNCTION.
FUNCTION InternetGetLastResponseInfo RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
  define var cBuffer            as  char no-undo.
  define var iBufferSz          as  int init 4096 no-undo.
  define var iResultCode        as  int no-undo.
  define var iTemp              as  int no-undo.
  
  /* allocate for the buffer */
  assign
  cBuffer = fill(' ', iBufferSz).

  run InternetGetLastResponseInfoA (output iResultCode,
                                    output cBuffer,
                                    input-output iBufferSz,
                                    output iTemp).
/*     message substitute('Error (&1):  &2',                            */
/*                      iResultCode,                                    */
/*                      substr(cBuffer,1,iBufferSz)) view-as alert-box. */
  CREATE felmeddtemp.
  felmeddtemp.FELMEDD = substitute('Error (&1):  &2',iResultCode,substr(cBuffer,1,iBufferSz)).
                       
  RETURN "".   /* Function return value. */

END FUNCTION.

FUNCTION ConnectWinInet RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
  run InternetOpenA(input  'WebBasedAgent',
                    input  {&INTERNET_OPEN_TYPE_PRECONFIG},
                    input  '',
                    input  '',
                    input  0,
                    output hInternetSession).
  
  RETURN hInternetSession <> 0. /* Function return value. */

END FUNCTION.

FUNCTION FTPConnect RETURNS LOGICAL
  ( input pcURL as char):
  def var iError as int no-undo.
  
  run InternetConnectA(input  hInternetSession,
                       input  pcURL,
                       /*
                       input  {&INTERNET_DEFAULT_FTP_PORT},
                       */
                       input  portremot,
                       input  remotLogin,
                       input  remotpasswd,
                       input  {&INTERNET_SERVICE_FTP},
                       input  0,
                       input  0,
                       output hFTPSession).
  

  IF hFTPSession = 0 THEN DO:
    run GetLastError(output iError).
    CREATE felmeddtemp.
    felmeddtemp.FELMEDD = "InternetConnectA Failed:  " + STRING(iError).
   
    InternetGetLastResponseInfo().
    RETURN FALSE.
  end.

  RETURN TRUE.   /* Function return value. */

END FUNCTION.
FUNCTION FtpGetFile RETURNS CHARACTER
  ( input pcFilename as char ) :
/*------------------------------------------------------------------------------
  Purpose:  Retrieves a file from the FTP Server and stores it under the 
            specified file name, creating a new local file in the process.
    Notes:  
------------------------------------------------------------------------------*/

  define var lpRemoteFile   as  memptr  no-undo.
  define var lpNewFile      as  memptr  no-undo.
  define var fOverwirte     as  log     no-undo.
  define var iRetCode       as  integer no-undo.
 /* define var cRemoteFile    as  char    no-undo.*/
  
 cRemoteFile = REPLACE(cRemoteFile,"/","\").
 
  /* remove the file size from the file name */
/*  cRemoteFile = trim(entry(1,slFiles, '(' ) )*/
 /* 
 REPEAT antali = 1 TO NUM-ENTRIES(cRemoteFile,"\"):
     CREATE provag.
     ASSIGN
     provag.VAGNR = antali
     provag.VAG = STRING(ENTRY(antali,cRemoteFile,"\"),"x(78)").
  END.
  FIND LAST provag NO-LOCK NO-ERROR.
  IF AVAILABLE provag THEN DO:  
     cRemoteFile = provag.VAG.         
  END.
  */
  assign
  set-size(lpRemoteFile)     = length(cRemoteFile) + 1
  put-string(lpRemoteFile,1) = cRemoteFile
  set-size(lpNewFile)        = length(pcFilename) + 1
  put-string(lpNewFile,1)    = pcFileName.   
  Session:Set-Wait-State('General':U).
  
  run FtpGetFileA(input hFtpSession,
                 input get-pointer-value(lpRemoteFile),
                 input get-pointer-value(lpNewFile),
                 input 0, /* 1 - fail if file exists, 0 - overwrite */
                 input {&FILE_ATTRIBUTE_NORMAL},
                 input ascibin,
                 input 0,
                 output iRetCode).
                 
  Session:Set-Wait-State('':U).
  
  if iRetCode = 0 then
    InternetGetLastResponseInfo().
  ELSE do: 
     CREATE felmeddtemp.
     felmeddtemp.FELMEDD = 'Fil mottagen...'.
  END.
  assign
  set-size(lpRemoteFile)     = 0
  set-size(lpNewFile)        = 0.

  RETURN "".   /* Function return value. */

END FUNCTION.
FUNCTION FtpPutFile RETURNS CHARACTER
  ( input pcLocalFile as char,
    input pcRemoteFile as char ) :
/*------------------------------------------------------------------------------
  Purpose:  Sends a file to the FTP Server and stores it under the 
            specified file name, creating a new remote file in the process
            if you have the appropriate permissions.  If not you will be told
            so via InternetGetLastResponse.
    Notes:  
------------------------------------------------------------------------------*/

  define var lpLocalFile        as  memptr  no-undo.
  define var lpNewRemoteFile    as  memptr  no-undo.
  define var fOverwirte         as  log     no-undo.
  define var iRetCode           as  integer no-undo.
  
  assign
  /* remove the file size from the file name */
  set-size(lpNewRemoteFile)     = length(pcRemoteFile) + 1
  put-string(lpNewRemoteFile,1) = pcRemoteFile
  set-size(lpLocalFile)         = length(pcLocalFile) + 1
  put-string(lpLocalFile,1)     = pcLocalFile.
  
  Session:Set-Wait-State('General':U).

  run FtpPutFileA(input hFtpSession,
                  input get-pointer-value(lpLocalFile),
                  input get-pointer-value(lpNewRemoteFile),
                  input ascibin,
                  input 0,
                  output iRetCode).
                 
  Session:Set-Wait-State('':U).
  
  if iRetCode = 0 then
    InternetGetLastResponseInfo().
  else DO:
     CREATE felmeddtemp.
     felmeddtemp.FELMEDD = 'Fil skickad...'.
  END.
    
  assign
  set-size(lpNewRemoteFile)     = 0
  set-size(lpLocalFile)         = 0.
 


  RETURN "".   /* Function return value. */

END FUNCTION.
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FtpDeleteFile C-Win 
FUNCTION FtpDeleteFile RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  Deletes a file from the FTP Server if you have permissions.
    Notes:  
------------------------------------------------------------------------------*/

  define var lpRemoteFile   as  memptr  no-undo.
  define var iRetCode       as  integer no-undo.
  
  assign
  /* remove the file size from the file name */
  
  set-size(lpRemoteFile)     = length(cRemoteFile) + 1
  put-string(lpRemoteFile,1) = cRemoteFile.
  
  Session:Set-Wait-State('General':U).
  run FtpDeleteFileA(input hFtpSession,
                     input get-pointer-value(lpRemoteFile),
                     output iRetCode).
                 
  Session:Set-Wait-State('':U).
  
  if iRetCode = 0 then
    InternetGetLastResponseInfo().
  else DO:
     CREATE felmeddtemp.
     felmeddtemp.FELMEDD = 'File Deleted...'.
  END.
  assign
  set-size(lpRemoteFile)     = 0.

  RETURN "".   /* Function return value. */

END FUNCTION.

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FtpRenameFile C-Win 
FUNCTION FtpRenameFile RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  Deletes a file from the FTP Server if you have permissions.
    Notes:  
------------------------------------------------------------------------------*/

  define var lpExisting   as  memptr  no-undo.
  define var lpNewFile   as  memptr  no-undo.
  define var iRetCode       as  integer no-undo.
    
  
  assign
  /* remove the file size from the file name */
  
  set-size(lpExisting)     = length(cRemoteFile) + 1
  put-string(lpExisting,1) = cRemoteFile.
  assign
  set-size(lpNewFile)     = length(cReNameFile) + 1
  put-string(lpNewFile,1) = cReNameFile.
  Session:Set-Wait-State('General':U).
  run FtpRenameFileA(input hFtpSession,
                     input get-pointer-value(lpExisting),
                     input get-pointer-value(lpNewFile),
                     output iRetCode).
                 
  Session:Set-Wait-State('':U).
  
  if iRetCode = 0 then
    InternetGetLastResponseInfo().
  else DO:
     CREATE felmeddtemp.
     felmeddtemp.FELMEDD = 'File Renamed...'.
  END.
  assign
  set-size(lpExisting)     = 0.
  assign
  set-size(lpNewFile)     = 0.
  RETURN "".   /* Function return value. */

END FUNCTION.

/*CONNEKTA*/
IF NOT ConnectWinInet() THEN DO:
   CREATE felmeddtemp.
   felmeddtemp.FELMEDD = "Det gick inte att nå mottagaren.".
   RETURN.
END.
ELSE DO:
   if FTPConnect(motagandemaskin) THEN DO:  
      /*SKICKA*/
      IF skicka = TRUE THEN DO:
         FtpPutFile(input cLocalFile,input cRemoteFile).
      END. 
      ELSE IF skicka = FALSE THEN DO:
         FtpGetFile(cLocalFile).  
      END.
      ELSE DO:
         FtpDeleteFile().
      END.
      IF  cReNameFile NE "" THEN FtpRenameFile().
   END.
END.
/*STÄNGA*/
CloseInternetConnection(hInternetSession).
/*ta bort skickadfil*/

PROCEDURE InternetConnectA EXTERNAL "wininet.dll" PERSISTENT:
  define input parameter  hInternetSession  as  long.
  define input parameter  lpszServerName    as  char.
  define input parameter  nServerPort       as  long.
  define input parameter  lpszUserName      as  char.
  define input parameter  lpszPassword      as  char.
  define input parameter  dwService         as  long.
  define input parameter  dwFlags           as  long.
  define input parameter  dwContext         as  long.
  define return parameter hInternetConnect  as  long.
END.

PROCEDURE InternetGetLastResponseInfoA EXTERNAL "wininet.dll" PERSISTENT:
  define output parameter lpdwError          as  long.
  define output parameter lpszBuffer         as  char.
  define input-output  parameter lpdwBufferLength   as  long.
  define return parameter iResultCode       as  long.
END.

PROCEDURE InternetOpenUrlA EXTERNAL "wininet.dll" PERSISTENT:
  define input parameter  hInternetSession  as  long.
  define input parameter  lpszUrl           as  char.
  define input parameter  lpszHeaders       as  char.
  define input parameter  dwHeadersLength   as  long.
  define input parameter  dwFlags           as  long.
  define input parameter  dwContext         as  long.
  define return parameter iResultCode       as  long.
END.

PROCEDURE InternetOpenA EXTERNAL "wininet.dll" PERSISTENT:
  define input parameter  sAgent            as  char.
  define input parameter  lAccessType       as  long.
  define input parameter  sProxyName        as  char.
  define input parameter  sProxyBypass      as  char.
  define input parameter  lFlags            as  long.
  define return parameter iResultCode       as  long.
END.

PROCEDURE InternetReadFile EXTERNAL "wininet.dll" PERSISTENT:
  define input  parameter  hFile            as  long.
  define output parameter  sBuffer          as  char.
  define input  parameter  lNumBytesToRead  as  long.
  define output parameter  lNumOfBytesRead  as  long.
  define return parameter  iResultCode      as  long.
END.

PROCEDURE InternetCloseHandle EXTERNAL "wininet.dll" PERSISTENT:
  define input parameter  hInet             as  long.
  define return parameter iResultCode       as  long.
END.

PROCEDURE FtpFindFirstFileA EXTERNAL "wininet.dll" PERSISTENT :
    define input parameter  hFtpSession as  long.
    define input parameter  lpFileName as char.
    define input parameter  lpFindFileData as memptr.
    define input parameter  dwFlags        as long.
    define input parameter  dwContext      as long.
    define return parameter hSearch as long.
END PROCEDURE.    


PROCEDURE InternetFindNextFileA EXTERNAL "wininet.dll" PERSISTENT:
    define input parameter  hSearch as long.
    define input parameter  lpFindFileData as memptr.
    define return parameter found as long.
END PROCEDURE.


PROCEDURE FtpGetCurrentDirectoryA EXTERNAL "wininet.dll" PERSISTENT:
    define input parameter  hFtpSession as long.
    define input parameter  lpszCurrentDirectory as long.
    define input-output parameter lpdwCurrentDirectory as long.
    define return parameter iRetCode as long.
END PROCEDURE.

PROCEDURE FtpSetCurrentDirectoryA EXTERNAL "wininet.dll" PERSISTENT:
    define input parameter  hFtpSession as long.
    define input parameter  lpszDirectory as long.
    define return parameter iRetCode as long.
END PROCEDURE.

PROCEDURE FtpOpenFileA EXTERNAL "wininet.dll" PERSISTENT:
    define input parameter  hFtpSession  as long.
    define input parameter  lpszFileName as long.
    define input parameter  dwAccess     as long.
    define input parameter  dwFlags      as long.
    define input parameter  dwContext    as long.
    define return parameter iRetCode as long.
END PROCEDURE.

PROCEDURE FtpPutFileA EXTERNAL "wininet.dll" PERSISTENT:
    define input parameter  hFtpSession       as long.
    define input parameter  lpszLocalFile     as long.
    define input parameter  lpszNewRemoteFile as long.
    define input parameter  dwFlags           as long.
    define input parameter  dwContext         as long.
    define return parameter iRetCode          as long.
END PROCEDURE.

PROCEDURE FtpGetFileA EXTERNAL "wininet.dll" PERSISTENT:
    define input parameter  hFtpSession          as long.
    define input parameter  lpszRemoteFile       as long.
    define input parameter  lpszNewFile          as long.
    define input parameter  fFailIfExists        as long.
    define input parameter  dwFlagsAndAttributes as long.
    define input parameter  dwFlags              as long.
    define input parameter  dwContext            as long.
    define return parameter iRetCode             as long.
END PROCEDURE.

PROCEDURE FtpDeleteFileA EXTERNAL "wininet.dll" PERSISTENT:
    define input parameter  hFtpSession          as long.
    define input parameter  lpszRemoteFile       as long.
    define return parameter iRetCode             as long.
END PROCEDURE.
PROCEDURE FtpRenameFileA EXTERNAL "wininet.dll" PERSISTENT:
    define input parameter  hFtpSession          as long.
    define input parameter  lpszExisting       as long.
    define input parameter  lpszNewFile       as long.
    define return parameter iRetCode             as long.
END PROCEDURE.

PROCEDURE GetLastError external "kernel32.dll" :
  define return parameter dwMessageID as long. 
END PROCEDURE.

