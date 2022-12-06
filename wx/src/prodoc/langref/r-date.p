/* r-date.p */

DEFINE VARIABLE cnum AS CHARACTER FORMAT "x(3)".
DEFINE VARIABLE cdate AS CHARACTER FORMAT "x(6)".
DEFINE VARIABLE iday AS INTEGER.
DEFINE VARIABLE imon AS INTEGER.
DEFINE VARIABLE iyr AS INTEGER.
DEFINE VARIABLE ddate AS DATE.

INPUT FROM VALUE(SEARCH("r-date.dat")).

REPEAT:
  SET cnum cdate.
  imon = INTEGER(SUBSTR(cdate,1,2)).
  iday = INTEGER(SUBSTR(cdate,3,2)).
  iyr  = INTEGER(SUBSTR(cdate,5,2)).
  IF (iyr < 50)   /*WORKS FOR 2-DIGIT YEARS WITHIN 50 of 2000*/
  THEN
      iyr = iyr + 2000.
  ELSE
      iyr = iyr + 1900.
  ddate = DATE(imon,iday,iyr).
  DISPLAY ddate.
END.

INPUT CLOSE.

/* 
SEE ALSO: 
  SESSION:YEAR-OFFSET and -yy parm, SESSION:DATE-FORMAT and -d parm
*/
