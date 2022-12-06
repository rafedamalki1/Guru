DEFINE {1} SHARED FRAME  marker
  DICTDB._File._File-name FORMAT "x(32)" LABEL "Table" COLON 11   
  DICTDB._field._Field-name FORMAT "x(32)":u LABEL "Field" COLON 11 SKIP
  DICTDB._Index._Index-name FORMAT "x(32)":u LABEL "Index" COLON 11 SKIP
  HEADER 
    " Dumping definitions.  Press" +
    KBLABEL("STOP") + " to terminate the dump process. " format "x(70)"
  WITH  OVERLAY ROW 4 CENTERED SIDE-LABELS ATTR-SPACE USE-TEXT.

COLOR DISPLAY MESSAGES 
                       DICTDB._File._File-name 
                       DICTDB._Field._Field-name
                       DICTDB._Index._Index-name
              WITH FRAME marker.         