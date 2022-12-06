/*WIDGETDEF.I*/

FIND FIRST widgettemp WHERE widgettemp.WIDGETHAND = vh NO-LOCK NO-ERROR.
IF NOT AVAILABLE widgettemp THEN DO:
   RUN WIDGETDEF.P (INPUT vh).
   CREATE widgettemp. 
   ASSIGN
   widgettemp.WIDGETHAND = vh
   widgettemp.WIDGETNAMN = vh:NAME
   widgettemp.WIDGETTYP = vh:TYPE.
END.

      

