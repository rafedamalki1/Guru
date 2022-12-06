OUTPUT TO \\GRANGURU\guru_ser\server\PRO8S\KOLL.d APPEND.
PUT "1" skip.
OUTPUT CLOSE. 
INPUT FROM \\GRANGURU\guru_ser\server\PRO8S\sumlon.d convert target "iso8859-1" source "iso8859-1".
REPEAT:
   CREATE sumlon.
   ASSIGN.
   IMPORT sumlon.
 
END.
OUTPUT TO \\GRANGURU\guru_ser\server\PRO8S\KOLL.d APPEND.
PUT "2" skip.
OUTPUT CLOSE. 

INPUT FROM \\GRANGURU\guru_ser\server\PRO8S\sumEJon.d  convert target "iso8859-1" source "iso8859-1".
REPEAT:
   CREATE sumEJlon.
   ASSIGN.
   IMPORT sumEJlon.
END.
OUTPUT TO \\GRANGURU\guru_ser\server\PRO8S\KOLL.d APPEND.
PUT "3" skip.
OUTPUT CLOSE. 

INPUT FROM \\GRANGURU\guru_ser\server\PRO8S\sumtidda.d convert target "iso8859-1" source "iso8859-1".
REPEAT:
   CREATE sumtiddaG.
   ASSIGN.
   IMPORT sumtiddaG.
END.
OUTPUT TO \\GRANGURU\guru_ser\server\PRO8S\KOLL.d APPEND.
PUT "4" skip.
OUTPUT CLOSE. 

INPUT FROM \\GRANGURU\guru_ser\server\PRO8S\sumtid.d  convert target "iso8859-1" source "iso8859-1".
REPEAT:
   CREATE sumtid.
   ASSIGN.
   IMPORT sumtid.
END.
OUTPUT TO \\GRANGURU\guru_ser\server\PRO8S\KOLL.d APPEND.
PUT "5" skip.
OUTPUT CLOSE. 

INPUT FROM \\GRANGURU\guru_ser\server\PRO8S\sumtrakt.d convert target "iso8859-1" source "iso8859-1".
REPEAT:
   CREATE sumtrakt.
   ASSIGN.
   IMPORT sumtrakt.
END.
OUTPUT TO \\GRANGURU\guru_ser\server\PRO8S\KOLL.d APPEND.
PUT "6" skip.
OUTPUT CLOSE. 

INPUT FROM \\GRANGURU\guru_ser\server\PRO8S\GODKOLL.d  convert target "iso8859-1" source "iso8859-1".
REPEAT:
   CREATE GODKOLL.
   ASSIGN.
   IMPORT GODKOLL.
END.
OUTPUT TO \\GRANGURU\guru_ser\server\PRO8S\KOLL.d APPEND.
PUT "7" skip.
OUTPUT CLOSE. 

INPUT FROM \\GRANGURU\guru_ser\server\PRO8S\tidregit.d  convert target "iso8859-1" source "iso8859-1".
REPEAT:
   CREATE tidregit.
   ASSIGN.
   IMPORT tidregit.
END.
OUTPUT TO \\GRANGURU\guru_ser\server\PRO8S\KOLL.d APPEND.
PUT "8" skip.
OUTPUT CLOSE. 
