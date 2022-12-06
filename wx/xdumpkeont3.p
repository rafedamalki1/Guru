DEFINE VARIABLE valar LIKE P1.KATAR NO-UNDO.
   
   ASSIGN
   valar = 2017.
   
   
   OUTPUT TO \\SERVER05\d\elpool\elplo\kalk\Eon2016\p2.d convert target "iso8859-1" source "iso8859-1" append.
   /*OUTPUT TO e:\delad\pro9\p2.d convert target "iso8859-1" source "iso8859-1" append.*/
   OPEN QUERY kq2 FOR EACH P2 WHERE P2.KATAR = valar AND p2.arbkod = "EJK" NO-LOCK.
   GET FIRST kq2 NO-LOCK.
   DO WHILE AVAILABLE(P2):
      EXPORT P2.
      GET NEXT kq2 NO-LOCK.
   END.
   CLOSE QUERY kq2.
   OPEN QUERY kq2 FOR EACH P2 WHERE P2.KATAR = valar AND p2.arbkod = "ELL" NO-LOCK.
   GET FIRST kq2 NO-LOCK.
   DO WHILE AVAILABLE(P2):
      EXPORT P2.
      GET NEXT kq2 NO-LOCK.
   END.
   CLOSE QUERY kq2.
   OPEN QUERY kq2 FOR EACH P2 WHERE P2.KATAR = valar AND p2.arbkod = "EUH" NO-LOCK.
   GET FIRST kq2 NO-LOCK.
   DO WHILE AVAILABLE(P2):
      EXPORT P2.
      GET NEXT kq2 NO-LOCK.
   END.
   CLOSE QUERY kq2.
   OPEN QUERY kq2 FOR EACH P2 WHERE P2.KATAR = valar AND p2.arbkod = "EAF" NO-LOCK.
   GET FIRST kq2 NO-LOCK.
   DO WHILE AVAILABLE(P2):
      EXPORT P2.
      GET NEXT kq2 NO-LOCK.
   END.
   CLOSE QUERY kq2.
   OUTPUT CLOSE.
   
   OUTPUT TO \\SERVER05\d\elpool\elplo\kalk\Eon2016\lop2.d convert target "iso8859-1" source "iso8859-1" append.
   /*OUTPUT TO e:\delad\pro9\lop2.d convert target "iso8859-1" source "iso8859-1" append.*/
   OPEN QUERY kq5 FOR EACH LOP2 WHERE LOP2.KATAR = valar AND lop2.arbkod = "EJK" NO-LOCK.
   GET FIRST kq5 NO-LOCK.
   DO WHILE AVAILABLE(LOP2):
      EXPORT LOP2.
      GET NEXT kq5 NO-LOCK.
   END.
   CLOSE QUERY kq5.
   OPEN QUERY kq5 FOR EACH LOP2 WHERE LOP2.KATAR = valar AND lop2.arbkod = "ELL" NO-LOCK.
   GET FIRST kq5 NO-LOCK.
   DO WHILE AVAILABLE(LOP2):
      EXPORT LOP2.
      GET NEXT kq5 NO-LOCK.
   END.
   CLOSE QUERY kq5.
   OPEN QUERY kq5 FOR EACH LOP2 WHERE LOP2.KATAR = valar AND lop2.arbkod = "EUH" NO-LOCK.
   GET FIRST kq5 NO-LOCK.
   DO WHILE AVAILABLE(LOP2):
      EXPORT LOP2.
      GET NEXT kq5 NO-LOCK.
   END.
   CLOSE QUERY kq5.
   OPEN QUERY kq5 FOR EACH LOP2 WHERE LOP2.KATAR = valar AND lop2.arbkod = "EAF" NO-LOCK.
   GET FIRST kq5 NO-LOCK.
   DO WHILE AVAILABLE(LOP2):
      EXPORT LOP2.
      GET NEXT kq5 NO-LOCK.
   END.
   CLOSE QUERY kq5.
   OUTPUT CLOSE.

   
   
   