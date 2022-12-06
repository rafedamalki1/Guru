OUTPUT TO \\GRANGURU\guru_ser\server\PRO8S\sumlon.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  sumlon WHERE SUMLON.DATUM < 01/01/2000:
   EXPORT sumlon.
END.
OUTPUT TO \\GRANGURU\guru_ser\server\PRO8S\sumEJon.d  convert target "iso8859-1" source "iso8859-1".
FOR EACH  sumEJlon WHERE SUMEJLON.DATUM < 01/01/2000:
   EXPORT sumEJlon.
END.

OUTPUT TO \\GRANGURU\guru_ser\server\PRO8S\sumtidda.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  sumtiddaG WHERE SUMTIDDAG.DATUM < 01/01/2000:
   EXPORT sumtiddaG.
END.
OUTPUT TO \\GRANGURU\guru_ser\server\PRO8S\sumtid.d  convert target "iso8859-1" source "iso8859-1".
FOR EACH  sumtid WHERE SUMTID.DATUM < 01/01/2000:
   EXPORT sumtid.
END.
OUTPUT TO \\GRANGURU\guru_ser\server\PRO8S\sumtrakt.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  sumtrakt WHERE SUMTRAKT.DATUM < 01/01/2000:
   EXPORT sumtrakt.
END.
OUTPUT TO \\GRANGURU\guru_ser\server\PRO8S\GODKOLL.d  convert target "iso8859-1" source "iso8859-1".
FOR EACH  GODKOLL WHERE GODKOLL.DATUM < 01/01/2000:
   EXPORT GODKOLL.
END.

OUTPUT TO \\GRANGURU\guru_ser\server\PRO8S\tidregit.d  convert target "iso8859-1" source "iso8859-1".
FOR EACH  tidregit WHERE TIDREGITAB.DATUM < 01/01/2000:
   EXPORT tidregit.
END.
