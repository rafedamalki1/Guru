DEFINE VARIABLE street-number AS INTEGER LABEL "Street Number".

FOR EACH customer:
   ASSIGN street-number = INTEGER(ENTRY(1, address, " ")) NO-ERROR.
   IF ERROR-STATUS:ERROR
   THEN MESSAGE "Could not get street number of" address.
   ELSE DISPLAY cust-num address street-number.
END.
