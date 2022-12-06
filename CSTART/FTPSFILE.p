/*FTPSFILE.p*/

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
E:\delad\curl-7.77.0-win64-mingw\bin
*/

        
        Def var serverName as char no-undo initial "31.216.227.29". /* 92.25.04.69 */ 
        Def var usrName    as char no-undo initial "elplo".
        Def var usrPass    as char no-undo initial "Jaggillarskidor&5419".
        
        Def var localFile  as char no-undo initial "X:\vatten\Elpool.dll".

*/
DEFINE INPUT  PARAMETER varskicka AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER curlvar AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER serverName AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER usrName AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER usrPass AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER localFile AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER remoteFile AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER remotdir AS CHARACTER NO-UNDO.

DEFINE VARIABLE  lvCommand  AS  CHARACTER  NO-UNDO.
IF varskicka = "AESorder" THEN serverName = serverName + remotdir + remoteFile + ":" + remoteFile. 
ELSE serverName = serverName + remotdir + remoteFile. 
  
        /************************* FTP ***********************
        
              update
          serverName format "x(25)"
          usrName    format "x(25)"
          usrPass    format "x(25)"
          remoteFile format "x(25)"
          localFile  format "x(25)"
        with 1 col side-labels.
        
        lvCommand = substitute
        ('curl -p --insecure "ftp://&1:&2" --user "&3:&4" -T "&5" --ftp-create-dirs',
              trim(serverName), 
              trim(remoteFile), 
              trim(usrName), 
              trim(usrPass),
              trim(localFile)
              
              lvCommand = SUBSTITUTE 
        (curlvar + 'curl -k "sftp://&1:&2" --user "&3:&4" -T "&5" --ftp-create-dirs',
              trim(serverName), 
              trim(remoteFile), 
              trim(usrName), 
              trim(usrPass),
              trim(localFile)
            ).
              
            ). 
        ******************************************************/

        
        lvCommand = curlvar + 'curl -k' + ' "sftp://'  + serverName + '"' +  ' --user "' + usrName + ':' + usrPass + '"' + ' -T "' + localFile + '"' +  ' --ftp-create-dirs'.
        DEBUGGER:SET-BREAK().  
        OUTPUT TO "SFTPLOG.txt" APPEND. 
        PUT UNFORMATTED TODAY " " STRING(TIME,"HH:MM:SS") " " localFile SKIP.       
        OUTPUT CLOSE.
        OS-COMMAND SILENT  VALUE(lvCommand).


