/*xdynex.p*/

DEF VAR select-1 AS CHAR
    VIEW-AS SELECTION-LIST INNER-CHARS 20 INNER-LINES 5 
    SCROLLBAR-VERTICAL LABEL "Table".
DEF VAR select-2 AS CHAR
    VIEW-AS SELECTION-LIST INNER-CHARS 20 INNER-LINES 5 
    SCROLLBAR-VERTICAL MULTIPLE LABEL "Fields".
DEF BUTTON b-first LABEL "First".
DEF BUTTON b-prev LABEL "Prev".
DEF BUTTON b-next LABEL "Next".
DEF BUTTON b-last LABEL "Last".
DEF BUTTON b-use-selected-fields LABEL "Use Selected fields".

DEF VAR qh AS HANDLE.
DEF VAR bh AS HANDLE.
DEF VAR bf AS HANDLE.

FORM 
  select-1
  HELP "Choose one table in this selection list."
  select-2
  HELP "Select one field or use ctrl-click to select more than one."
  SKIP
  b-first b-prev b-next b-last b-use-selected-fields AT 40
  WITH FRAME FRAME-A SIDE-LABELS WIDTH 80 THREE-D.
VIEW FRAME FRAME-A.
FORM WITH FRAME FRAME-B SIDE-LABELS WIDTH 120 17 DOWN 
     THREE-D ROW 6.
VIEW FRAME FRAME-B.

ON CHOOSE OF b-first DO:
   IF NOT VALID-HANDLE(qh) THEN LEAVE.
   qh:GET-FIRST().
   RUN display_fields.   
END.
ON CHOOSE OF b-prev DO:
   IF NOT VALID-HANDLE(qh) THEN LEAVE.
   qh:GET-PREV().
   RUN display_fields.   
END.
ON CHOOSE OF b-next DO:
   IF NOT VALID-HANDLE(qh) THEN LEAVE.
   qh:GET-NEXT().
   RUN display_fields.   
END.
ON CHOOSE OF b-last DO:
   IF NOT VALID-HANDLE(qh) THEN LEAVE.
   qh:GET-LAST().
   RUN display_fields.   
END.
ON CHOOSE OF b-use-selected-fields DO:
   RUN create_selected_fields.
   RUN display_fields.
END.

ON VALUE-CHANGED OF select-1 DO:
   DEF VAR I AS INT.
   CREATE BUFFER bh FOR TABLE SELF:SCREEN-VALUE.
   RUN create_fields.

   CREATE QUERY qh.
   qh:SET-BUFFERS(bh).
   qh:QUERY-PREPARE("FOR EACH " + SELF:SCREEN-VALUE).
   qh:QUERY-OPEN().
   qh:GET-FIRST().

   RUN display_fields.

   select-2:LIST-ITEMS = "".
   DO I = 1 TO bh:NUM-FIELDS:
      bf = bh:BUFFER-FIELD(I).
      select-2:ADD-LAST(bf:NAME).      
   END.
END.

DELETE WIDGET-POOL "fields" NO-ERROR.
CREATE WIDGET-POOL "fields" PERSISTENT.

FOR EACH _file WHERE NOT _FROZEN NO-LOCK:
    select-1:ADD-LAST(_file-name).
END.
ENABLE ALL WITH FRAME FRAME-B.
ENABLE ALL WITH FRAME FRAME-A.
WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.
DELETE WIDGET-POOL "fields".

PROCEDURE create_fields:
  DEF VAR h  AS HANDLE NO-UNDO.
  DEF VAR I  AS INT NO-UNDO.
  DEF VAR wf AS HANDLE NO-UNDO.

  DELETE WIDGET-POOL "fields".
  CREATE WIDGET-POOL "fields" PERSISTENT.
  DO I = 1 TO bh:NUM-FIELDS:
     bf = bh:BUFFER-FIELD(I).
     IF bf:EXTENT = 0 THEN
        RUN create_field(I, bf, 0).
     ELSE DO:
        DEF VAR J AS INT NO-UNDO.
        DO J = 1 TO bf:EXTENT:
            RUN create_field(I + J - 1, bf, J).
        END.
     END.   
  END.
END.

PROCEDURE display_fields:
  DEF VAR h  AS HANDLE NO-UNDO.

  IF NOT bh:AVAILABLE THEN DO:
     MESSAGE "Record is not available.".
     RETURN.
  END.
  h = FRAME FRAME-B:FIRST-CHILD.
  h = h:FIRST-CHILD.
  DO WHILE VALID-HANDLE(h):
     IF h:TYPE = "FILL-IN" AND h:PRIVATE-DATA <> ? THEN DO:
        IF NUM-ENTRIES(h:PRIVATE-DATA) = 1 THEN DO:
        bf = bh:BUFFER-FIELD(h:PRIVATE-DATA).
             ASSIGN h:SCREEN-VALUE = bf:STRING-VALUE.
        END.
        ELSE DO:
             bf = bh:BUFFER-FIELD(ENTRY(1,h:PRIVATE-DATA)).
             ASSIGN h:SCREEN-VALUE =
                    bf:STRING-VALUE(INTEGER(ENTRY(2,h:PRIVATE-DATA))).
        END.
        ASSIGN h:VISIBLE = YES NO-ERROR.
     END.
     h = h:NEXT-SIBLING.
  END.
END.

PROCEDURE create_selected_fields:
  DEF VAR I  AS INT NO-UNDO.

  DELETE WIDGET-POOL "fields".
  CREATE WIDGET-POOL "fields" PERSISTENT.
  DO I = 1 TO NUM-ENTRIES(select-2:SCREEN-VALUE 
     IN FRAME FRAME-A):
     bf = bh:BUFFER-FIELD(ENTRY(I,select-2:SCREEN-VALUE)).
     IF bf:EXTENT = 0 THEN
        RUN create_field(I, bf, 0).
     ELSE DO:
        DEF VAR J AS INT NO-UNDO.
        DO J = 1 TO bf:EXTENT:
            RUN create_field(I + J - 1, bf, J).
        END.
     END.   
  END.
END.

PROCEDURE create_field:
  DEF INPUT PARAMETER position AS INT NO-UNDO.
  DEF INPUT PARAMETER buffer-field AS HANDLE NO-UNDO.
  DEF INPUT PARAMETER extent AS INT NO-UNDO.

  DEF VAR h  AS HANDLE NO-UNDO.  
  DEF VAR wf AS HANDLE NO-UNDO.

  CREATE TEXT h IN WIDGET-POOL "fields"
         ASSIGN FRAME = FRAME FRAME-B:HANDLE
                ROW = position
                COL = 1
                FORMAT = "X(17):"
                SCREEN-VALUE = buffer-field:LABEL + 
                               (IF extent > 0 THEN 
                                   STRING(extent," [99]")
                                ELSE "")
                VISIBLE = TRUE.
  CREATE FILL-IN wf IN WIDGET-POOL "fields"
         ASSIGN FRAME = FRAME FRAME-B:HANDLE
                ROW = position          
                COL = 20
                SIDE-LABEL-HANDLE = h                 
                DATA-TYPE = bf:DATA-TYPE
                PRIVATE-DATA = bf:NAME +
                               (IF extent > 0 THEN
                                   STRING(extent,",99")
                                ELSE "")
                SENSITIVE = TRUE.
                ASSIGN wf:VISIBLE = FALSE
                wf:FORMAT = bf:FORMAT
                wf:HELP = bf:HELP NO-ERROR.
END.


