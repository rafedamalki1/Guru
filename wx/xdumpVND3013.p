DEFINE VARIABLE valar LIKE P1.KATAR NO-UNDO.
   
   ASSIGN
   valar = 2016.
   
   
   OUTPUT TO C:\temp\p2.d convert target "iso8859-1" source "iso8859-1" append.
   OPEN QUERY kq2 FOR EACH P2 WHERE P2.KATAR = valar AND p2.arbkod = "VND" NO-LOCK.
   GET FIRST kq2 NO-LOCK.
   DO WHILE AVAILABLE(P2):
      EXPORT P2.
      GET NEXT kq2 NO-LOCK.
   END.
   CLOSE QUERY kq2.
   
   
   OUTPUT TO C:\temp\lop2.d convert target "iso8859-1" source "iso8859-1" append.
   OPEN QUERY kq5 FOR EACH LOP2 WHERE LOP2.KATAR = valar AND lop2.arbkod = "VND" NO-LOCK.
   GET FIRST kq5 NO-LOCK.
   DO WHILE AVAILABLE(LOP2):
      EXPORT LOP2.
      GET NEXT kq5 NO-LOCK.
   END.
   CLOSE QUERY kq5.
      
   