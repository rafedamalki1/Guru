/*
     Filename: SETARROWCURSOR.I
      Created: 2003.12.18 09:06ELPAO     
     Modified: 
*/

PROCEDURE DestroyCursor EXTERNAL "user32":
   DEFINE INPUT  PARAMETER hCursorName    AS LONG.
   DEFINE RETURN PARAMETER retValue       AS LONG.  
END PROCEDURE.

PROCEDURE SetSystemCursor EXTERNAL "user32":
   DEFINE INPUT  PARAMETER hcur          AS LONG.
   DEFINE INPUT  PARAMETER id            AS LONG.
   DEFINE RETURN PARAMETER ReturnValue   AS LONG.  
END PROCEDURE.

PROCEDURE LoadCursorA EXTERNAL "user32":
   DEFINE INPUT  PARAMETER hInstance      AS LONG.
   DEFINE INPUT  PARAMETER lpCursorName   AS LONG.
   DEFINE RETURN PARAMETER hcur           AS LONG.  
END PROCEDURE.                             

DEFINE VARIABLE hcur AS INTEGER NO-UNDO.                                       
DEFINE VARIABLE retval AS INTEGER NO-UNDO.  
RUN LoadCursorA (INPUT 0, INPUT 32650, OUTPUT hcur).  /* IDC_APPSTARTING */
RUN SetSystemCursor (INPUT hcur, INPUT 32512, OUTPUT retval). /* IDC_ARROW */ 
RUN DestroyCursor (INPUT hcur, OUTPUT retval). 
