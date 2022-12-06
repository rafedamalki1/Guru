
/*FTPDIR.P*/
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
DEFINE TEMP-TABLE provag
   FIELD VAGNR AS INTEGER
   FIELD VAG AS CHARACTER
   INDEX VAGNR IS PRIMARY VAGNR.
/*DEFINE TEMP-TABLE provag
   FIELD VAGNR AS INTEGER
   FIELD VAG AS CHARACTER
   INDEX VAGNR IS PRIMARY VAGNR.

*/

DEFINE INPUT PARAMETER remotLogin  AS CHARACTER NO-UNDO.  /*login*/               
DEFINE INPUT PARAMETER remotpasswd AS CHARACTER NO-UNDO.  /*lösen*/
DEFINE INPUT PARAMETER sokdir  AS CHARACTER NO-UNDO.   /*fil på local pc*/ 
DEFINE INPUT PARAMETER filtyp AS CHARACTER NO-UNDO.   /*fil på connectad maskin*/
DEFINE INPUT PARAMETER motagandemaskin AS CHARACTER NO-UNDO.  /*ftp maskin */
DEFINE OUTPUT PARAMETER TABLE FOR felmeddtemp.
DEFINE OUTPUT PARAMETER TABLE FOR provag.
DEFINE VARIABLE antali AS INTEGER.
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
                       input  {&INTERNET_DEFAULT_FTP_PORT},
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

FUNCTION FTPListDir RETURNS INTEGER
  (INPUT cSearchDir      as CHAR,
   INPUT cSearchFileSpec as CHAR,
   INPUT hFTPSession     as INT,
   INPUT cProgCallBack   as CHAR,
   INPUT hCallProc       as HANDLE):
/*------------------------------------------------------------------------

  Function:    DirList

  Description: Returns integer corresponding to the error:
                   0 - if there were no errors.
                   1 - file list buffer is too small
                   2 - file list buffer is too big
                   3 - invalid path given
                   5 - directory on drive is invaild

               
               cSearchDir:
                 Directory to search in, can be relative, use /../../, etc.                 
               
               cSearchFileSpec:
                 comma delimited list of file types, can have trailing 
                 comma e.g: " foo?ar.*  ,  *.p, "
                 
               hFTPSession - handle to an open ftp session.

               cProgCallBack:
                 Program to be called if a file is found passing in
                 to input parameters, the directory being searched, 
                 and the filename found.
                 
               hCallProc
                 Handle to the calling program where call back process
                 is to execute.

  Notes:       FtpFindFirstFile can only occur once within a given FTP
               session.  To issue another one, a call must be made
               InternetCloseHandle.

  History: 
          
------------------------------------------------------------------------*/
  def var lpFindData   as memptr    no-undo.
  def var hSearch      as integer   no-undo.
  def var iFound       as integer   no-undo initial 1.
  def var iFileSpec    as integer   no-undo.
  def var cFileList    as char      no-undo.
  def var iRetCode     as integer   no-undo.

  &SCOPE  FIND_DATA-SIZE 4           /* dwFileAttributes       */~
                       + 8           /* ftCreationTime         */~
                       + 8           /* ftLastAccessTime       */~
                       + 8           /* ftLastWriteTime        */~
                       + 4           /* nFileSizeHigh          */~
                       + 4           /* nFileSizeLow           */~
                       + 4           /* dwReserved0            */~
                       + 4           /* dwReserved1            */~
                       + {&MAX_PATH} /* cFileName[MAX_PATH]    */~
                       + 14          /* cAlternateFileName[14] */
  
  /* allocate the memory for the find_data structure */
  assign
  set-size(lpFindData) = {&FIND_DATA-SIZE}.

  do iFileSpec = 1 to num-entries(cSearchFileSpec):

    iFound = 1.
    run FtpFindFirstFileA (input  hFtpSession,
                           input  cSearchDir + '/' + 
                                  entry(iFileSpec, cSearchFileSpec), 
                           input  lpFindData,
                           input  {&INTERNET_FLAG_EXISITING_CONNECT},
                           input  0,
                           output hSearch).
        
    if hSearch <> -1 then 
    repeat while iFound <> 0:
       CREATE provag.
     ASSIGN
     provag.VAGNR = antali
     provag.VAG = get-string(lpFindData,45).            
     /*
      run value(cProgCallBack) in hCallProc
            (input lpFindData,
             input  cSearchDir).                 /* current directory */
      */   
      run InternetFindNextFileA (input  hSearch, 
                                 input  lpFindData,
                                 output iFound).
        
    end. /* repeat while ifound <> 0... */

    /* set error for invalid file specification */
    else 
      iRetCode = 5.
 
  end. /* do iFileSpec = 1 to ... */
    
  set-size(lpFindData) = 0.

  /* close file handle now so we can do a find again */
  run InternetCloseHandle (input hSearch, OUTPUT iRetCode).

  return iRetCode.

END FUNCTION.



/*CONNEKTA*/
IF NOT ConnectWinInet() THEN DO:
   CREATE felmeddtemp.
   felmeddtemp.FELMEDD = "Det gick inte att nå motagaren.".
   RETURN.
END.
ELSE DO:
   if FTPConnect(motagandemaskin) THEN DO:  
      
      FTPListDir(INPUT sokdir,
                   INPUT filtyp,
                   INPUT hFTPSession,
                   INPUT 'CreateFileList',
                   INPUT THIS-PROCEDURE).
      
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

PROCEDURE GetLastError external "kernel32.dll" :
  define return parameter dwMessageID as long. 
END PROCEDURE.


