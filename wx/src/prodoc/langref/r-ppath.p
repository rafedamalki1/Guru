/* r-ppath.p  */

DEFINE VARIABLE menu     AS CHARACTER EXTENT 4 FORMAT "X(20)"
   INITIAL ["1. Sales","2. Acctg","3. Personnel","4. Exit"].

DEFINE VARIABLE proglist AS CHARACTER EXTENT 4 FORMAT "X(8)"
   INITIAL ["sales.p","acctg.p","per.p","exit.p"].

DEFINE VARIABLE ppath    AS CHARACTER EXTENT 4
   INITIAL ["sales/s-procs","acctg/a-procs","per/p-procs",","].

REPEAT:
  DISPLAY menu WITH TITLE " M A I N   M E N U " CENTERED
     1 COLUMN 1 DOWN NO-LABELS ROW 8 ATTR-SPACE.
  CHOOSE FIELD menu AUTO-RETURN.
  HIDE.
  PROPATH = ppath[FRAME-INDEX].
  RUN VALUE(proglist[FRAME-INDEX]).
END.
