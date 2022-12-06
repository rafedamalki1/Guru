/*WIN_M_SLUT.I*/
/*{center.i}*/
DEFINE VARIABLE movetotopp AS LOGICAL NO-UNDO.
         
FOR EACH widgettemp WHERE widgettemp.WIDGETTYP = "BUTTON" NO-LOCK:
   IF widgettemp.WIDGETHAND:HEIGHT = 2.33 AND widgettemp.WIDGETHAND:WIDTH >= 7.25 THEN DO:
      IF widgettemp.WIDGETHAND:WIDTH = 7.5 THEN widgettemp.WIDGETHAND:WIDTH = 7.25.
      widgettemp.WIDGETHAND:HEIGHT-CHARS = widgettemp.WIDGETHAND:HEIGHT-CHARS + 0.07.
      widgettemp.WIDGETHAND:WIDTH-CHARS = widgettemp.WIDGETHAND:WIDTH-CHARS + 0.1.
   END.
END.
  

IF VALID-HANDLE(framesizeh) THEN RUN nydefault_UI IN framesizeh.

IF VALID-HANDLE(framesizeh) THEN RUN alltidmax_UI IN framesizeh (INPUT Guru.Konstanter:alltidmax).
RUN FRONTSLUT.P (INPUT TABLE widgettemp).
RUN CenterWindow IN Guru.Konstanter:hpApi (INPUT CURRENT-WINDOW).
/*
{&WINDOW-NAME}:HIDDEN = FALSE.
{&WINDOW-NAME}:ALWAYS-ON-TOP = TRUE.
movetotopp =  {&WINDOW-NAME}:MOVE-TO-TOP().

DEFINE VARIABLE status-musdia AS LOGICAL NO-UNDO. 
status-musdia = SESSION:SET-WAIT-STATE("").
{&WINDOW-NAME}:ALWAYS-ON-TOP = FALSE.
*/
DEFINE VARIABLE status-musdia AS LOGICAL NO-UNDO. 
Guru.GlobalaVariabler:windowsizeh = {&WINDOW-NAME}:HANDLE.

Guru.Konstanter:BtnOkAvbFlytt(). 
IF Guru.GlobalaVariabler:retvalkoll = TRUE THEN DO:
   IF VALID-HANDLE(Guru.Konstanter:hpApi) THEN RUN SetDefaultCursors IN Guru.Konstanter:hpApi.
   Guru.GlobalaVariabler:retvalkoll = FALSE.
END.
IF VALID-HANDLE(framesizeh) THEN RUN winresized IN framesizeh.
IF Guru.GlobalaVariabler:fonstercol > 0 THEN CURRENT-WINDOW:X = Guru.GlobalaVariabler:fonstercol.
IF Guru.GlobalaVariabler:fonsterrow > 0 THEN CURRENT-WINDOW:Y = Guru.GlobalaVariabler:fonsterrow.
{&WINDOW-NAME}:HIDDEN = FALSE.
{&WINDOW-NAME}:ALWAYS-ON-TOP = TRUE.
movetotopp =  {&WINDOW-NAME}:MOVE-TO-TOP().
status-musdia = SESSION:SET-WAIT-STATE("").
{&WINDOW-NAME}:ALWAYS-ON-TOP = FALSE.

/*
RUN CenterWindow IN Guru.Konstanter:hpApi (INPUT CURRENT-WINDOW).
*/
