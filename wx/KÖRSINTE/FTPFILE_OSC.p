
/*FTPFILE_OSC.p*/
DEFINE TEMP-TABLE felmeddtemp 
  FIELD FELMEDD AS CHARACTER
  FIELD VAL AS INTEGER.
  
DEFINE INPUT PARAMETER motagandemaskin AS CHARACTER NO-UNDO.  /*ftp maskin */
DEFINE INPUT PARAMETER portremot AS INTEGER NO-UNDO.  /*ftp maskin */
DEFINE INPUT PARAMETER remotLogin  AS CHARACTER NO-UNDO.  /*login*/               
DEFINE INPUT PARAMETER remotpasswd AS CHARACTER NO-UNDO.  /*lösen*/
DEFINE INPUT PARAMETER motagandekat AS CHARACTER NO-UNDO.  /*underkatalog */
DEFINE INPUT PARAMETER motagandeextra AS CHARACTER NO-UNDO.  /*extra kommandon som ska köras, tex browsa ner i strukturen */
DEFINE INPUT PARAMETER ftpcommando_filnamn  AS CHARACTER NO-UNDO.  /* namn på ftpfil */ 
DEFINE INPUT PARAMETER filurval AS CHARACTER NO-UNDO. /* Om man vill filtrera ut filer att lista */
DEFINE INPUT PARAMETER binardata AS LOGICAL NO-UNDO. /* Binärdata */
DEFINE INPUT PARAMETER localpath  AS CHARACTER NO-UNDO.  /* Var ska hämtade filer läggas? */
DEFINE INPUT PARAMETER localfilename  AS CHARACTER NO-UNDO.  /* Ska filen döpas om efter hämtning? */

DEFINE OUTPUT PARAMETER TABLE FOR felmeddtemp.

DEFINE STREAM SS.
DEFINE VARIABLE vl_command_line AS CHAR NO-UNDO.
DEFINE VARIABLE cnt AS INTEGER NO-UNDO.
DEFINE VARIABLE vl_filename AS CHARACTER NO-UNDO.
DEFINE VARIABLE vl_slask AS CHARACTER NO-UNDO.
DEFINE VARIABLE vl_tag AS CHARACTER NO-UNDO.
DEFINE VARIABLE vl_status AS INTEGER NO-UNDO.
DEFINE VARIABLE antali AS INTEGER.

IF INDEX(motagandemaskin,":") > 0 THEN DO:
   portremot = INTEGER(SUBSTRING(motagandemaskin,INDEX(motagandemaskin,":") + 1)).
   motagandemaskin = SUBSTRING(motagandemaskin,1,INDEX(motagandemaskin,":") - 1).
END. 
  
IF filurval = "" THEN
   filurval = "*.*".
   
IF localfilename = "" THEN
DO:
    IF INDEX(filurval,"*") <= 0 THEN localfilename = filurval. /* Man kan bara döpa om filen om man hämtat med exakt filnamn */
END.

/* Get */
OUTPUT STREAM SS TO VALUE(ftpcommando_filnamn + ".ftp").

PUT STREAM SS UNFORMATTED "open " motagandemaskin + " " + STRING(portremot) SKIP.
PUT STREAM SS UNFORMATTED remotLogin skip
                       remotpasswd skip.
                       
IF motagandekat <> "" THEN
   PUT STREAM SS UNFORMATTED "cd " + motagandekat SKIP.
   
IF motagandeextra <> "" THEN DO:
   DO cnt = 1 TO NUM-ENTRIES(motagandeextra):
      PUT STREAM SS UNFORMATTED ENTRY(cnt,motagandeextra) SKIP.
   END. 
END.

IF binardata THEN
   PUT STREAM SS UNFORMATTED "binary" SKIP.   
   
PUT STREAM SS UNFORMATTED "get " filurval " " localpath + localfilename SKIP.
PUT STREAM SS UNFORMATTED "bye" SKIP.

OUTPUT STREAM SS CLOSE.

vl_command_line = "ftp -s:" + ftpcommando_filnamn + ".ftp -i >" + ftpcommando_filnamn + ".out".  
/*** Kör skriptet som du nyss byggt ihop ***/

OS-COMMAND  VALUE(vl_command_line).

/*RUN FTPCHECK_OSC.p (vl_mall, FALSE, OUTPUT vl_status).
RUN FTPLOG_OSC.p (vl_mall, "GET", vl_status).*/

/* Gick det bra?
CREATE felmeddtemp.
felmeddtemp.FELMEDD = 'Fil mottagen...'.

*/


/*
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
/* för nyare server os 2016 osv
PROCEDURE FtpPutFileA EXTERNAL "wininet.dll" PERSISTENT:
    define input parameter  hFtpSession       as long.
    define input parameter  lpszLocalFile     as CHAR.
    define input parameter  lpszNewRemoteFile as CHAR.
    define input parameter  dwFlags           as long.
    define input parameter  dwContext         as long.
    define return parameter iRetCode          as long.
END PROCEDURE.
*/
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

*/

/*

-1

I managed to work out code for sftp. I also tried code ftp which Tom provided.

Here's the code if anyone is interested (substitute info for serverName, remoteFile, usrName, usrPass)

/******************/
/* Ftpbob         */
/* ftp command    */
/* Author: 4gl    */
/******************/
/* upload using sftp (1)
curl  -k "sftp://83.46.38.23:22/CurlPutTest/" --user "testuser:testpassword" -T "C:\test\testfile.xml" --ftp-create-dirs
*/
/* upload using ftp (2)
os-command value(substitute('curl -p --insecure  "ftp://&1:&2" --user "&3:&4" -T "&5" --ftp-create-dirs', serverName, remoteFile, usrName, usrPass, localFile )).
*/

        Def var lvCommand  as char no-undo.
        Def var serverName as char no-undo initial "serv1". /* 92.25.04.69 */
        Def var usrName    as char no-undo initial "bob".
        Def var usrPass    as char no-undo initial "sumpass1".
        Def var remoteFile as char no-undo initial "/remoteDir/abc.csv".
        Def var localFile  as char no-undo initial "/localDir/123.csv".

        update
          serverName format "x(25)"
          usrName    format "x(25)"
          usrPass    format "x(25)"
          remoteFile format "x(25)"
          localFile  format "x(25)"
        with 1 col side-labels.

        /************************* FTP ***********************
        lvCommand = substitute
        ('curl -p --insecure "ftp://&1:&2" --user "&3:&4" -T "&5" --ftp-create-dirs',
              trim(serverName), 
              trim(remoteFile), 
              trim(usrName), 
              trim(usrPass),
              trim(localFile)
            ). 
        ******************************************************/

        /*** SFTP ***/
        lvCommand = substitute
        ('curl -k "sftp://&1:&2" --user "&3:&4" -T "&5" --ftp-create-dirs',
              trim(serverName), 
              trim(remoteFile), 
              trim(usrName), 
              trim(usrPass),
              trim(localFile)
            ). 

        message lvCommand
        view-as alert-box.

        os-command value(lvCommand).

Share
Improve this answer
Follow
edited Sep 10 '19 at 15:31
answered Sep 10 '19 at 8:18
bb_man
333 bronze badges
A
*/
