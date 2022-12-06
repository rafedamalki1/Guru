/* r-tstnm.p */
DEF VAR testnm AS CHAR.
SET testnm.
IF LDBNAME (testnm) = testnm
    THEN MESSAGE testnm " is a true logical database name.".
    ELSE
	IF LDBNAME (testnm) = ?
	  THEN
	  MESSAGE testnm "is not the name or alias of any connected database.".
	  ELSE MESSAGE testnm " is an ALIAS for database" LDBNAME(testnm).
