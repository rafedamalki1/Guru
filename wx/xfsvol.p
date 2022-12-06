OUTPUT TO E:\delad\fsvol.txt.
for each extradata where extradata.program = "fsvol" BY extradata.huvudint:
FIND FIRST AOVARD WHERE AOVARD.VARDNR = extradata.HUVUDINT AND AOVARD.AONR NE ? NO-LOCK NO-ERROR.
IF AVAILABLE AOVARD THEN DO:
  FIND FIRST AONRTIDLAGE WHERE AONRTIDLAGE.AONR = AOVARD.AONR AND AONRTIDLAGE.IDTIDLAG = "AOUPPLAGT" NO-LOCK NO-ERROR.
  IF AVAILABLE AONRTIDLAGE THEN DO:
   
    disp extradata.huvudint extradata.huvudch extradata.sokint[1] extradata.sokint[2] AONRTIDLAGE.DATUM1 AONRTIDLAGE.DATUM2.
  END.
END.
END.    
