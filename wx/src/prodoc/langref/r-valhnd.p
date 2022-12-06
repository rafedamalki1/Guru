DEFINE VARIABLE mywin AS WIDGET-HANDLE.

DEFINE BUTTON mkwin   LABEL "New Window".

ENABLE mkwin.

ON CHOOSE OF mkwin
   DO:
      CREATE WINDOW mywin
          ASSIGN VISIBLE = TRUE
                 TITLE = "Second Window"
                 MAX-WIDTH-CHARS = 40
                 MAX-HEIGHT-CHARS = 10.
     SELF:SENSITIVE = FALSE.
   END.


ON WINDOW-CLOSE OF DEFAULT-WINDOW
   DO:
      IF VALID-HANDLE(mywin)
      THEN DELETE WIDGET mywin.
   END.
   
WAIT-FOR WINDOW-CLOSE OF DEFAULT-WINDOW.
