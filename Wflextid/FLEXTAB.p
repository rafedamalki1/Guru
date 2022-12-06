/*FLEXTAB.P*/
&Scoped-define NEW    
&Scoped-define SHARED 
{FLEXTAB.I}
DEFINE INPUT PARAMETER vadgora AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER vem AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ansttemp.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR flexregtemp.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR flexavttemp.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR flexsaldotemp.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR utryckningtemp.

IF vadgora = 1 OR vadgora = 4 THEN DO:
   IF vem = ""  THEN DO:
      EMPTY TEMP-TABLE ansttemp NO-ERROR. 
      OPEN QUERY aq FOR EACH ANSTFORMTAB NO-LOCK.
      GET FIRST aq NO-LOCK.
      DO WHILE AVAILABLE(ANSTFORMTAB):         
         CREATE ansttemp.
         BUFFER-COPY ANSTFORMTAB TO ansttemp.
         GET NEXT aq NO-LOCK.
      END.
   END.
   ELSE DO:
      FIND FIRST ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING = vem NO-LOCK NO-ERROR.
      IF AVAILABLE ANSTFORMTAB THEN DO:
         FIND FIRST ansttemp WHERE ansttemp.ANSTALLNING = vem NO-ERROR.
         IF NOT AVAILABLE ansttemp THEN CREATE ansttemp.
         BUFFER-COPY ANSTFORMTAB TO ansttemp.
      END.
   END.
END.
IF vadgora = 2 OR vadgora = 4 THEN DO:
   IF vem = ""  THEN DO:
      EMPTY TEMP-TABLE flexregtemp NO-ERROR. 
      OPEN QUERY pq FOR EACH FLEXREG NO-LOCK.
      GET FIRST pq NO-LOCK.
      DO WHILE AVAILABLE(FLEXREG):
         CREATE flexregtemp.
         BUFFER-COPY FLEXREG TO flexregtemp.
         GET NEXT pq NO-LOCK.
      END.
   END.
   ELSE DO:
      FIND FIRST FLEXREG WHERE FLEXREG.KOD = vem NO-LOCK NO-ERROR.
      IF AVAILABLE FLEXREG THEN DO:
         FIND FIRST flexregtemp WHERE flexregtemp.KOD = vem NO-ERROR.
         IF NOT AVAILABLE flexregtemp THEN CREATE flexregtemp.
         BUFFER-COPY FLEXREG TO flexregtemp.
      END.
   END.
END.
IF vadgora = 3 OR vadgora = 4 THEN DO:
   IF vem = ""  THEN DO:
      EMPTY TEMP-TABLE flexavttemp NO-ERROR. 
      OPEN QUERY fq FOR EACH FLEXAVT NO-LOCK.
      GET FIRST fq NO-LOCK.
      DO WHILE AVAILABLE(FLEXAVT):
         CREATE flexavttemp.
         BUFFER-COPY FLEXAVT TO flexavttemp.
         GET NEXT fq NO-LOCK.
      END.
   END.
   ELSE DO:
      FIND FIRST FLEXAVT WHERE FLEXAVT.PERSONALKOD = vem NO-LOCK NO-ERROR.
      IF AVAILABLE FLEXAVT THEN DO:
         FIND FIRST flexavttemp WHERE flexavttemp.PERSONALKOD = vem NO-ERROR.
         IF NOT AVAILABLE flexavttemp THEN CREATE flexavttemp.
         BUFFER-COPY FLEXAVT TO flexavttemp.
      END.
   END.
END.
IF vadgora = 5 THEN DO:
   IF vem = ""  THEN DO:
      EMPTY TEMP-TABLE flexsaldotemp NO-ERROR. 
   END.
   ELSE DO:
      FIND FIRST FLEXSALDO WHERE FLEXSALDO.PERSONALKOD = vem NO-LOCK NO-ERROR.
      IF AVAILABLE FLEXSALDO THEN DO:
         FIND FIRST flexsaldotemp WHERE flexsaldotemp.PERSONALKOD = vem NO-ERROR.
         IF NOT AVAILABLE flexsaldotemp THEN CREATE flexsaldotemp.
         BUFFER-COPY FLEXSALDO TO flexsaldotemp.
      END.
   END.
END.
IF vadgora = 6 OR vadgora = 4 THEN DO:
   IF vem = ""  THEN DO:
      EMPTY TEMP-TABLE utryckningtemp NO-ERROR. 
      OPEN QUERY uq FOR EACH UTRYCKNING NO-LOCK.
      GET FIRST uq NO-LOCK.
      DO WHILE AVAILABLE(UTRYCKNING):         
         CREATE utryckningtemp.
         BUFFER-COPY UTRYCKNING TO utryckningtemp.
         GET NEXT uq NO-LOCK.
      END.
   END.
   ELSE DO:
      FIND FIRST UTRYCKNING WHERE UTRYCKNING.KOD = vem NO-LOCK NO-ERROR.
      IF AVAILABLE UTRYCKNING THEN DO:
         FIND FIRST utryckningtemp WHERE utryckningtemp.KOD = vem NO-ERROR.
         IF NOT AVAILABLE utryckningtemp THEN CREATE utryckningtemp.
         BUFFER-COPY UTRYCKNING TO utryckningtemp.
      END.
   END.
END.
