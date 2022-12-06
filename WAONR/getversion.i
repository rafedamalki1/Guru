/*getversion.i*/
FUNCTION RunningWindows95 RETURNS LOGICAL () :
  /* returns TRUE if you are running Windows 95 */
 
  DEF VAR Win95         AS LOGICAL NO-UNDO.
  DEF VAR lpVersionInfo AS MEMPTR.
  DEF VAR dwPlatformID  AS INTEGER NO-UNDO.
  DEF VAR MinorVersion  AS INTEGER NO-UNDO.
  DEF VAR ReturnValue   AS INTEGER NO-UNDO.
 
  SET-SIZE(lpVersionInfo)   = 148.
  PUT-LONG(lpVersionInfo,1) = 148.
  RUN GetVersionExA ( GET-POINTER-VALUE(lpVersionInfo), 
                              OUTPUT ReturnValue).
  dwPlatformID = GET-LONG(lpVersionInfo,17).
  MinorVersion = GET-BYTE(lpVersionInfo,15).
 
  Win95 = (dwPlatformId=1 AND MinorVersion=0).
 
  SET-SIZE(lpVersionInfo) = 0.
  RETURN Win95.
 
END FUNCTION.
 
FUNCTION RunningWindows98 RETURNS LOGICAL () :
  /* returns TRUE if you are running Windows 98 */
 
  DEF VAR Win98         AS LOGICAL NO-UNDO.
  DEF VAR lpVersionInfo AS MEMPTR.
  DEF VAR dwPlatformID  AS INTEGER NO-UNDO.
  DEF VAR MinorVersion  AS INTEGER NO-UNDO.
  DEF VAR ReturnValue   AS INTEGER NO-UNDO.
 
  SET-SIZE(lpVersionInfo)   = 148.
  PUT-LONG(lpVersionInfo,1) = 148.
  RUN GetVersionExA ( GET-POINTER-VALUE(lpVersionInfo), 
                              OUTPUT ReturnValue).
  dwPlatformID = GET-LONG(lpVersionInfo,17).
  MinorVersion = GET-BYTE(lpVersionInfo,15).
 
  Win98 = (dwPlatformId=1 AND MinorVersion=10).
 
  SET-SIZE(lpVersionInfo) = 0.
  RETURN Win98.
 
END FUNCTION.
 
FUNCTION RunningWindowsNT4 RETURNS LOGICAL () :
  /* returns TRUE if you are running Windows NT4.
     I have not had a chance to test this yet */
 
  DEF VAR NT4           AS LOGICAL NO-UNDO.
  DEF VAR lpVersionInfo AS MEMPTR.
  DEF VAR dwPlatformID  AS INTEGER NO-UNDO.
  DEF VAR MajorVersion  AS INTEGER NO-UNDO.
  DEF VAR ReturnValue   AS INTEGER NO-UNDO.
 
  SET-SIZE(lpVersionInfo)   = 148.
  PUT-LONG(lpVersionInfo,1) = 148.
  RUN GetVersionExA ( GET-POINTER-VALUE(lpVersionInfo), 
                              OUTPUT ReturnValue).
 
  dwPlatformID = GET-LONG(lpVersionInfo,17).
  MajorVersion = GET-BYTE(lpVersionInfo, 5).
 
  NT4 = (dwPlatformId=2 AND MajorVersion=4).
 
  SET-SIZE(lpVersionInfo) = 0.
  RETURN NT4.
 
END FUNCTION.
 
FUNCTION RunningWindows2000 RETURNS LOGICAL () :
  /* returns TRUE if you are running Windows 2000 */
 
  DEF VAR Win2000       AS LOGICAL NO-UNDO.
  DEF VAR lpVersionInfo AS MEMPTR.
  DEF VAR dwPlatformID  AS INTEGER NO-UNDO.
  DEF VAR MajorVersion  AS INTEGER NO-UNDO.
  DEF VAR ReturnValue   AS INTEGER NO-UNDO.
 
  SET-SIZE(lpVersionInfo)   = 148.
  PUT-LONG(lpVersionInfo,1) = 148.
  RUN GetVersionExA ( GET-POINTER-VALUE(lpVersionInfo), 
                              OUTPUT ReturnValue).
 
  dwPlatformID = GET-LONG(lpVersionInfo,17).
  MajorVersion = GET-BYTE(lpVersionInfo, 5).
 
  Win2000 = (dwPlatformId=2 AND MajorVersion=5).
 
  SET-SIZE(lpVersionInfo) = 0.
  RETURN Win2000.
 
END FUNCTION.

