/*AVIKDAGTID.I*/
DEFINE VARIABLE korweekday AS INTEGER NO-UNDO.
FIND FIRST ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING = PERSONALTAB.ANSTALLNING
USE-INDEX ANSTF NO-LOCK NO-ERROR.   
FIND FIRST OVERAVTAB WHERE OVERAVTAB.DATUM = regdatum AND 
OVERAVTAB.KOD = ANSTFORMTAB.KOD USE-INDEX ODATUM NO-LOCK NO-ERROR.
IF AVAILABLE OVERAVTAB AND OVERAVTAB.EQDAG = 1  THEN dagnr = OVERAVTAB.EQDAG.
ELSE IF AVAILABLE OVERAVTAB AND OVERAVTAB.EQDAG = 7  THEN dagnr = OVERAVTAB.EQDAG.
ELSE dagnr = WEEKDAY(regdatum).

IF Guru.Konstanter:globforetag = "GKAL" THEN DO:   
  /*S?vsj? jobbar ej r?da dagar .Om innevarade veckas arbetstid dag7 har arbetstid f?resl?s denna f?r r?d dag.FEL!
  Detta till?gg g?r att den f?resl?r INGEN ARBETSTID dag1 och dag7*/
  IF ANSTFORMTAB.KOD = "K4" THEN DO:
     IF AVAILABLE OVERAVTAB THEN DO:
        IF dagnr = 1 OR dagnr = 7 THEN DO:
           vschemat = 1.
        END.
     END.
  END.
END.
FIND FIRST VECKOARBETID WHERE VECKOARBETID.VECKOSCHEMA = vschemat
USE-INDEX VECKOSCHEMA NO-LOCK NO-ERROR.
FIND FIRST EXTRADATA WHERE EXTRADATA.PROGRAM = "ARBTID" AND 
EXTRADATA.HUVUDCH = PERSONALTAB.PERSONALKOD AND
EXTRADATA.HUVUDINT = arregvnr NO-LOCK NO-ERROR.
IF AVAILABLE EXTRADATA THEN DO:
   korweekday = WEEKDAY(regdatum).
   IF korweekday = 1 THEN korweekday = 7.
   ELSE korweekday = korweekday - 1.
   IF EXTRADATA.SOKINT[korweekday] = -1 THEN dagnr = dagnr.
   ELSE DO:
      FIND FIRST ARBETSTIDTAB WHERE ARBETSTIDTAB.ARBTIDKOD = EXTRADATA.SOKINT[korweekday]
      USE-INDEX ARBTIDKOD NO-LOCK NO-ERROR.
      dagnr = 0.
   END.
   
END.

   
