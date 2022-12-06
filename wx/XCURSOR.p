/*
     Filename: XCURSOR.P
      Created: 03.03.0027 08:30ELPAO     
     Modified: 
*/
{windows.i}
/* PROCEDURE SetSystemCursor EXTERNAL "user32" :        */
/*    DEFINE INPUT  PARAMETER hcur          AS LONG.    */
/*    DEFINE INPUT  PARAMETER id            AS LONG.    */
/*    DEFINE RETURN PARAMETER ReturnValue   AS {&BOOL}. */
/* END PROCEDURE.                                       */
/*                                                                       */
/* FUNCTION SetSystemCursor     /* wrapper for the big API definition */ */
/*          RETURNS INTEGER   /* = if success then hProcess else 0  */   */
/*          (INPUT CommandLine as CHAR,                                  */
/*           INPUT CurrentDir  as CHAR,                                  */
/*           INPUT wShowWindow as INTEGER).                              */
/*                                                                       */
/* PROCEDURE GetLastError EXTERNAL {&KERNEL} :                           */
/*   DEFINE RETURN PARAMETER dwMessageID AS {&INT}.                      */
/* END PROCEDURE.                                                        */
/*                                                                       */
/* FUNCTION GetLastError      /* 1:1 implementation of API */            */
/*          RETURNS INTEGER   /* = dwErrorID */                          */
/*          ()                                                           */
/*          IN hpWinFunc.                                                */
/*                                                                       */
/* FUNCTION GetLastError RETURNS INTEGER :                               */
/*   def var dwMessageID as integer no-undo.                             */
/*   run GetLastError IN Guru.Konstanter:hpApi (output dwMessageID).                     */
/*   RETURN (dwMessageID).                                               */
/* END FUNCTION.                                                         */

/* PROCEDURE LoadCursorA EXTERNAL "user32" :          */
/*    DEFINE INPUT  PARAMETER hInstance      AS LONG. */
/*    DEFINE INPUT  PARAMETER lpCursorName   AS LONG. */
/*    DEFINE RETURN PARAMETER hcur           AS LONG. */
/* END PROCEDURE.                                     */
/*                                                    */
/* PROCEDURE DestroyCursor EXTERNAL "user32" :        */
/*    DEFINE INPUT  PARAMETER hCursorName    AS LONG. */
/*    DEFINE RETURN PARAMETER retValue       AS LONG. */
/* END PROCEDURE.                                     */
/*                                                    */
/* PROCEDURE GetTickCount EXTERNAL "kernel32" :       */
/*    DEFINE RETURN PARAMETER retValue       AS LONG. */
/* END PROCEDURE.                                     */
/*                                                    */
/* PROCEDURE Sleep EXTERNAL "kernel32" :              */
/*    DEFINE INPUT PARAMETER dwMilliseconds  AS LONG. */
/* END PROCEDURE.                                     */

/* &GLOB  IDC_APPSTARTING  32650 */
/* &GLOB  IDC_ARROW        32512 */
/* &GLOB  IDC_CROSS        32515 */
/* &GLOB  IDC_IBEAM        32513 */
/* &GLOB  IDC_ICON         32641 */
/* &GLOB  IDC_NO           32648 */
/* &GLOB  IDC_SIZE         32640 */
/* &GLOB  IDC_SIZEALL      32646 */
/* &GLOB  IDC_SIZENESW     32643 */
/* &GLOB  IDC_SIZEWE       32644 */
/* &GLOB  IDC_UPARROW      32516 */
/* &GLOB  IDC_WAIT         32514 */

DEFINE VARIABLE hcur AS INTEGER NO-UNDO.
DEFINE VARIABLE hcur2 AS INTEGER NO-UNDO.
DEFINE VARIABLE retval AS INTEGER NO-UNDO.
DEFINE VARIABLE retval2 AS INTEGER NO-UNDO.

/* RUN Sleep (INPUT 2000).           */
/* MESSAGE retval VIEW-AS ALERT-BOX. */

RUN LoadCursorA IN Guru.Konstanter:hpApi (INPUT 0, INPUT {&IDC_APPSTARTING}, OUTPUT hcur).
RUN LoadCursorA IN Guru.Konstanter:hpApi (INPUT 0, INPUT {&IDC_ARROW}, OUTPUT hcur2).
MESSAGE "Cursor value:" hcur hcur2 VIEW-AS ALERT-BOX.
RUN SetSystemCursor IN Guru.Konstanter:hpApi (INPUT hcur, INPUT {&IDC_ARROW}, OUTPUT retval).
/* RUN SetSystemCursor IN Guru.Konstanter:hpApi (INPUT hcur2, INPUT {&IDC_APPSTARTING}, OUTPUT retval2).  */
MESSAGE retval VIEW-AS ALERT-BOX.
RUN DestroyCursor IN Guru.Konstanter:hpApi (INPUT hcur, OUTPUT retval).
RUN DestroyCursor IN Guru.Konstanter:hpApi (INPUT hcur2, OUTPUT retval2).
/* MESSAGE "Cursor value:" hcur " Return value(Destroy):" retval VIEW-AS ALERT-BOX. */
MESSAGE "Return value(Destroy):" retval retval2 VIEW-AS ALERT-BOX.

/* BOOL SetSystemCursor(    */
/*     HCURSOR hcur,        */
/*     DWORD id             */
/* );                       */
/*                          */
/* HCURSOR LoadCursor(      */
/*     HINSTANCE hInstance, */
/*     LPCTSTR lpCursorName */
/* );                       */



/* pCursorName                                                                 */
/*     IDC_APPSTARTING                                                         */
/*     Standard arrow and small hourglass                                      */
/*     IDC_ARROW                                                               */
/*     Standard arrow                                                          */
/*     IDC_CROSS                                                               */
/*     Crosshair                                                               */
/*     IDC_HAND                                                                */
/*     Windows 98/Me, Windows 2000/XP: Hand                                    */
/*     IDC_HELP                                                                */
/*     Arrow and question mark                                                 */
/*     IDC_IBEAM                                                               */
/*     I-beam                                                                  */
/*     IDC_ICON                                                                */
/*     Obsolete for applications marked version 4.0 or later.                  */
/*     IDC_NO                                                                  */
/*     Slashed circle                                                          */
/*     IDC_SIZE                                                                */
/*     Obsolete for applications marked version 4.0 or later. Use IDC_SIZEALL. */
/*     IDC_SIZEALL                                                             */
/*     Four-pointed arrow pointing north, south, east, and west                */
/*     IDC_SIZENESW                                                            */
/*     Double-pointed arrow pointing northeast and southwest                   */
/*     IDC_SIZENS                                                              */
/*     Double-pointed arrow pointing north and south                           */
/*     IDC_SIZENWSE                                                            */
/*     Double-pointed arrow pointing northwest and southeast                   */
/*     IDC_SIZEWE                                                              */
/*     Double-pointed arrow pointing west and east                             */
/*     IDC_UPARROW                                                             */
/*     Vertical arrow                                                          */
/*     IDC_WAIT                                                                */
/*     Hourglass                                                               */
