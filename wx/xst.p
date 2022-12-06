def var a as integer.
def var b as integer.
def var c as integer.

b = etime.
open query tq for each AFIL no-lock.
get first tq exclusive-lock.
do while available(AFIL):
  a = a + 1.
  delete afil .
  get next tq exclusive-lock.
end.


b = etime - b.
display a b.
/*
REPEAT :
C = C + 1.
MESSAGE C.
CREATE AFIL.
IF C > 100000 THEN LEAVE.
END.
*/
