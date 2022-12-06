/*OPENDOCVAL.p  DU VÄLJER VILKET PROGRAM DU VILL ÖPPNA MED*/
DEFINE VARIABLE hInstance AS INTEGER NO-UNDO.
DEFINE VARIABLE cWorkDirectory AS CHARACTER NO-UNDO.

DEFINE INPUT PARAMETER filnamn AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER cParams AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER cDirectory AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER lPrint AS LOGICAL NO-UNDO.


ASSIGN 
FILE-INFO:FILE-NAME = cDirectory.
IF FILE-INFO:FULL-PATHNAME > "" THEN cWorkDirectory = FILE-INFO:FULL-PATHNAME.
 
 
   /* Ignore cParams because cFileName is a document.
      cParams is only valid with executables */
   RUN ShellExecuteA (INPUT 0,
                      INPUT "open":u,
                      INPUT "rundll32.exe":u,
                      INPUT "shell32.dll,OpenAs_RunDLL ":u + filnamn,
                      INPUT cWorkDirectory,
                      INPUT 1,
                      OUTPUT hInstance).
/* test for error: */
 
RUN TestErrorCode(hInstance).
IF RETURN-VALUE > "" THEN
  MESSAGE RETURN-VALUE
    VIEW-AS ALERT-BOX ERROR BUTTON OK.
 
/****************************************************************************/
 
PROCEDURE ShellExecuteA EXTERNAL "shell32":U :
  DEFINE INPUT PARAMETER HWND         AS LONG.
  DEFINE INPUT PARAMETER lpOperation  AS CHAR.
  DEFINE INPUT PARAMETER lpFile       AS CHAR.
  DEFINE INPUT PARAMETER lpParameters AS CHAR.
  DEFINE INPUT PARAMETER lpDirectory  AS CHAR.
  DEFINE INPUT PARAMETER nShowCmd     AS LONG.
  DEFINE RETURN PARAMETER hInstance   AS LONG.
END PROCEDURE.
 

 
PROCEDURE TestErrorCode :
DEFINE INPUT PARAMETER iCode AS INTEGER.
DEF VAR cTxt AS CHAR NO-UNDO.
 
IF iCode < 0 OR iCode > 32 THEN RETURN "". /* no error */
 
CASE iCode :
  WHEN  0 THEN cTxt = "Operativsystemet har för lite minne eller resurser.":T132.
  WHEN  2 THEN cTxt = "Filen hittades inte.":T132.
  WHEN  3 THEN cTxt = "Sökvägen hittades inte.":T132.
  WHEN  5 THEN cTxt = "Operativsystemet medgav ej access till filen.":T132.
  WHEN  8 THEN cTxt = "Det var för lite minne för att slutföra processen.":T132.
  WHEN 10 THEN cTxt = "Felaktig Windows-version":T132.
  WHEN 11 THEN cTxt = "EXE filen är felaktig (non-Win32 .EXE or error in .EXE image).":T132.
  WHEN 12 THEN cTxt = "Applikationen är framtagen för ett annat operativ system.":T132.
  WHEN 13 THEN cTxt = "Applikationen är anpassad för MS-DOS 4.0.":T132.
  WHEN 15 THEN cTxt = "Attempt to load a real-mode program.":T132.
  WHEN 16 THEN cTxt = "Attempt to load a second instance of an application with non-readonly data segments.":T132.
  WHEN 19 THEN cTxt = "Attempt to load a compressed application file.":T132.
  WHEN 20 THEN cTxt = "Dynamic-link library (DLL) file failure.":T132.
  WHEN 26 THEN cTxt = "A sharing violation occurred.":T132.
  WHEN 27 THEN cTxt = "The filename association is incomplete or invalid.":T132.
  WHEN 28 THEN cTxt = "The DDE transaction could not be completed because the request timed out.":T132.
  WHEN 29 THEN cTxt = "The DDE transaction failed.":T132.
  WHEN 30 THEN cTxt = "The DDE transaction could not be completed because other DDE transactions were being processed.":T132.
  WHEN 31 THEN cTxt = "There is no application associated with the given filename extension.":T132.
  WHEN 32 THEN cTxt = "The specified dynamic-link library was not found.":T132.
  OTHERWISE    cTxt = "Undocumented error code returned":T132.
END.
 
RETURN cTxt.
 
END PROCEDURE.
