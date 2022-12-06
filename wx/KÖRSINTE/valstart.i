/*VALSTART.I
används inte
*/
DEFINE VARIABLE nyaprog AS LOGICAL NO-UNDO.
BTN_AVB:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      {WCONAPP.I}
{&WINDOW-NAME}:HIDDEN = TRUE.
 DEFAULT-WINDOW:HIDDEN = TRUE.
{&WINDOW-NAME}:ALWAYS-ON-TOP = FALSE.       
      {&WINDOW-NAME}:HIDDEN = TRUE.      
      DISABLE BRW_VDB BTN_START WITH FRAME {&FRAME-NAME}.      
     
      Guru.SharedVariable:singel = FALSE.     
      nyaprog = TRUE.
      REPEAT:
         RUN WSTART.W (INPUT valdbtemp.GFORETAG,INPUT valdbtemp.DBCACHE,INPUT-OUTPUT nyaprog).
         IF nyaprog = FALSE THEN LEAVE.
      END.
      IF Guru.Konstanter:appcon THEN Guru.Konstanter:appcon = Guru.Konstanter:apphand:DISCONNECT().
      IF Guru.Konstanter:apphand NE ? THEN DELETE OBJECT Guru.Konstanter:apphand NO-ERROR.
      
      ENABLE BRW_VDB BTN_START WITH FRAME {&FRAME-NAME}.
      BTN_AVB:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      {&WINDOW-NAME}:HIDDEN = FALSE.
      {&WINDOW-NAME}:HIDDEN = FALSE.
{&WINDOW-NAME}:MOVE-TO-TOP ().
{musarrow.i}
