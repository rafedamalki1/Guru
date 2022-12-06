DEFINE NEW GLOBAL SHARED VARIABLE hpApi AS HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE hpWinFunc AS HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE retvalkoll AS LOGICAL NO-UNDO.

{windows.i}
   DEFINE VARIABLE OSsvar AS LOGICAL NO-UNDO.
   DEF VAR  dwPlatformID  AS INTEGER NO-UNDO.
   DEF VAR MajorVersion  AS INTEGER NO-UNDO.
   DEF VAR ReturnValue   AS INTEGER NO-UNDO.
   RUN RunningWindows_UI IN Guru.Konstanter:hpApi (OUTPUT dwPlatformID,
                                   OUTPUT MajorVersion,
                                   OUTPUT ReturnValue   ). 
   DISP dwPlatformID
        MajorVersion
        ReturnValue. 

   DISP OS-GETENV("WINDIR") FORMAT "X(12)".
