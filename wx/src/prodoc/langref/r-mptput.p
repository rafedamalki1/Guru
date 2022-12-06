/* r-mptput.p */

DEFINE VARIABLE mptr AS MEMPTR.

SET-SIZE(mptr) = LENGTH("Bill") + 1.
PUT-BYTE(mptr,1) = ASC('B').
PUT-BYTE(mptr,2) = ASC('i').
PUT-BYTE(mptr,3) = ASC('l').
PUT-BYTE(mptr,4) = ASC('l').
PUT-BYTE(mptr,5) = 0.

DISPLAY GET-STRING(mptr,1).

