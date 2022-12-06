DEFINE VARIABLE valar LIKE P1.KATAR NO-UNDO.
   
   ASSIGN
   valar = 2011.
   
   /*OUTPUT TO F:\elpool\elpnj\kalk\2010\skjskl\p2.d convert target "iso8859-1" source "iso8859-1" append.
   OUTPUT TO F:\elpool\elpnj\kalk\2010\skjrönvitufr\p2.d convert target "iso8859-1" source "iso8859-1" append.*/
   OUTPUT TO F:\elpool\elpnj\kalk\2010\SKJSKL2011\p2.d convert target "iso8859-1" source "iso8859-1" append.
   OPEN QUERY kq2 FOR EACH P2 WHERE P2.KATAR = valar AND p2.arbkod = "SKJ" NO-LOCK.
   GET FIRST kq2 NO-LOCK.
   DO WHILE AVAILABLE(P2):
      EXPORT P2.
      GET NEXT kq2 NO-LOCK.
   END.
   CLOSE QUERY kq2.
   OUTPUT CLOSE.
   /*OUTPUT TO F:\elpool\elpnj\kalk\2010\skjskl\lop2.d convert target "iso8859-1" source "iso8859-1" append.
   OUTPUT TO F:\elpool\elpnj\kalk\2010\skjrönvitufr\lop2.d convert target "iso8859-1" source "iso8859-1" append.*/
   OUTPUT TO F:\elpool\elpnj\kalk\2010\SKJSKL2011\lop2.d convert target "iso8859-1" source "iso8859-1" append.
   OPEN QUERY kq5 FOR EACH LOP2 WHERE LOP2.KATAR = valar AND lop2.arbkod = "SKJ" NO-LOCK.
   GET FIRST kq5 NO-LOCK.
   DO WHILE AVAILABLE(LOP2):
      EXPORT LOP2.
      GET NEXT kq5 NO-LOCK.
   END.
   CLOSE QUERY kq5.
   OUTPUT CLOSE.

   /*OUTPUT TO F:\elpool\elpnj\kalk\2010\skjskl\p2.d convert target "iso8859-1" source "iso8859-1" append.
   OUTPUT TO F:\elpool\elpnj\kalk\2010\skjrönvitufr\p2.d convert target "iso8859-1" source "iso8859-1" append.*/
   OUTPUT TO F:\elpool\elpnj\kalk\2010\SKJSKL2011\p2.d convert target "iso8859-1" source "iso8859-1" append.
   OPEN QUERY kq2 FOR EACH P2 WHERE P2.KATAR = valar AND p2.arbkod = "SKL" NO-LOCK.
   GET FIRST kq2 NO-LOCK.
   DO WHILE AVAILABLE(P2):
      EXPORT P2.
      GET NEXT kq2 NO-LOCK.
   END.
   CLOSE QUERY kq2.
   OUTPUT CLOSE.
   /*OUTPUT TO F:\elpool\elpnj\kalk\2010\skjskl\lop2.d convert target "iso8859-1" source "iso8859-1" append. 
   OUTPUT TO F:\elpool\elpnj\kalk\2010\skjrönvitufr\lop2.d convert target "iso8859-1" source "iso8859-1" append.*/
   OUTPUT TO F:\elpool\elpnj\kalk\2010\SKJSKL2011\lop2.d convert target "iso8859-1" source "iso8859-1" append.

   
   OPEN QUERY kq5 FOR EACH LOP2 WHERE LOP2.KATAR = valar AND lop2.arbkod = "SKL" NO-LOCK.
   GET FIRST kq5 NO-LOCK.
   DO WHILE AVAILABLE(LOP2):
      EXPORT LOP2.
      GET NEXT kq5 NO-LOCK.
   END.
   CLOSE QUERY kq5.
   OUTPUT CLOSE.

   /*OUTPUT TO F:\elpool\elpnj\kalk\2010\skjskl\p2.d convert target "iso8859-1" source "iso8859-1" append.*/
   /*OUTPUT TO F:\elpool\elpnj\kalk\FORTUM\2010\p2.d convert target "iso8859-1" source "iso8859-1" append.
   OPEN QUERY kq2 FOR EACH P2 WHERE P2.KATAR = valar AND p2.arbkod = "TKOD" NO-LOCK.
   GET FIRST kq2 NO-LOCK.
   DO WHILE AVAILABLE(P2):
      EXPORT P2.
      GET NEXT kq2 NO-LOCK.
   END.
   CLOSE QUERY kq2.
   OUTPUT CLOSE.
   /*OUTPUT TO F:\elpool\elpnj\kalk\2010\skjskl\lop2.d convert target "iso8859-1" source "iso8859-1" append.*/
   OUTPUT TO F:\elpool\elpnj\kalk\FORTUM\2010\lop2.d convert target "iso8859-1" source "iso8859-1" append.
   OPEN QUERY kq5 FOR EACH LOP2 WHERE LOP2.KATAR = valar AND lop2.arbkod = "TKOD" NO-LOCK.
   GET FIRST kq5 NO-LOCK.
   DO WHILE AVAILABLE(LOP2):
      EXPORT LOP2.
      GET NEXT kq5 NO-LOCK.
   END.
   CLOSE QUERY kq5.
   OUTPUT CLOSE.*/


