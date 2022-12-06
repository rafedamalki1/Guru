/*
               KSV Editor
    Copyright: (C) 2000-2001 Serguey Klimoff (bulkl0DD)
     Filename: C:\DELAD\PRO9S\WX\XBORTFRÅNMULTISTART.P
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 2009.08.17 10:47 ELPAO   
     Modified: 
*/

/*
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE uttcopy_UI C-Win 
PROCEDURE uttcopy_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER vad AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER TABLE-HANDLE tthandle.
   DEFINE INPUT PARAMETER ttkopia AS CHARACTER NO-UNDO.
   DEFINE VARIABLE ttcopyh AS HANDLE NO-UNDO.
   DEFINE VARIABLE komcop AS CHARACTER NO-UNDO.
   DEFINE VARIABLE ttqh AS HANDLE NO-UNDO.
   DEFINE VARIABLE ttbuffh AS HANDLE NO-UNDO.
   /*
   CREATE TEMP-TABLE ttcopyh. 
   ttcopyh:ADD-FIELDS-FROM(tthandle:NAME).                                
   */
   ttcopyh:CREATE-LIKE(tthandle:NAME).
   ttcopyh:TEMP-TABLE-PREPARE(ttkopia). 
   ttbuffcopyh = ttcopyh:DEFAULT-BUFFER-HANDLE.
   ttbuffcopyh:EMPTY-TEMP-TABLE() NO-ERROR.
   ttbuffh = tthandle:DEFAULT-BUFFER-HANDLE.  
   komcop = "FOR EACH " + ttbuffh:TABLE + " NO-LOCK.".
   CREATE QUERY ttqh.
   ttqh:SET-BUFFERS(ttbuffh).
   ttqh:QUERY-PREPARE(komcop).
   ttqh:QUERY-OPEN().
   ttqh:GET-FIRST(NO-LOCK).
   DO WHILE ttqh:QUERY-OFF-END = FALSE:
      ttbuffcopyh:BUFFER-CREATE().
      ttbuffcopyh:BUFFER-COPY(ttbuffh).
      ttqh:GET-NEXT(NO-LOCK).
   END.
   ttqh:QUERY-CLOSE.
   DELETE OBJECT ttqh NO-ERROR.     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE utthmt_UI C-Win 
PROCEDURE utthmt_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT-OUTPUT PARAMETER favbuffh AS HANDLE.
   DEFINE VARIABLE compsave AS LOGICAL NO-UNDO.
   DEFINE VARIABLE komfav AS CHARACTER NO-UNDO.
   DEFINE VARIABLE komcop AS CHARACTER NO-UNDO.
   DEFINE VARIABLE ttcopqh AS HANDLE NO-UNDO.
   DEFINE VARIABLE ttfavqh AS HANDLE NO-UNDO.
   komcop = "FOR EACH " + ttbuffcopyh:TABLE + " NO-LOCK.".
   komfav = "FOR EACH " + favbuffh:TABLE + " NO-LOCK.".
   CREATE QUERY ttcopqh.
   ttcopqh:SET-BUFFERS(ttbuffcopyh).
   ttcopqh:QUERY-PREPARE(komcop).
   ttcopqh:QUERY-OPEN().
   CREATE QUERY ttfavqh.
   ttfavqh:SET-BUFFERS(favbuffh).
   ttfavqh:QUERY-PREPARE(komfav).
   ttfavqh:QUERY-OPEN().
   ttcopqh:GET-FIRST(NO-LOCK).
   DO WHILE ttcopqh:QUERY-OFF-END = FALSE:
      ttfavqh:GET-FIRST(NO-LOCK).
      compsave = FALSE.
      DO WHILE ttfavqh:QUERY-OFF-END = FALSE:
         IF favbuffh:BUFFER-COMPARE(ttbuffcopyh) THEN DO:
            compsave = TRUE.       
            ttfavqh:GET-LAST(NO-LOCK).
            ttfavqh:GET-NEXT(NO-LOCK).
         END.
         ttfavqh:GET-NEXT(NO-LOCK).
      END.                  
      IF compsave = TRUE THEN DO:
         ttbuffcopyh:BUFFER-DELETE().  
      END.
      ELSE DO:
         favbuffh:BUFFER-CREATE().
         favbuffh:BUFFER-COPY(ttbuffcopyh).
      END.
      ttcopqh:GET-NEXT(NO-LOCK).
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
*/
