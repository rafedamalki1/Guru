OUTPUT TO C:\Pro10\overkod.d convert target "iso8859-1" source "iso8859-1" append.
   OPEN QUERY kq FOR EACH overkod WHERE  overkod.kod = "t2" NO-LOCK.
   GET FIRST kq NO-LOCK.
   DO WHILE AVAILABLE(overkod):
      EXPORT overkod.
      GET NEXT kq NO-LOCK.
   END.
   CLOSE QUERY kq.
   OUTPUT CLOSE.

   OUTPUT TO C:\Pro10\overtidta.d convert target "iso8859-1" source "iso8859-1" append.
   OPEN QUERY kq2 FOR EACH overtidtab WHERE overtidtab.kod = "t2" NO-LOCK.
   GET FIRST kq2 NO-LOCK.
   DO WHILE AVAILABLE(overtidtab):
      EXPORT overtidtab.
      GET NEXT kq2 NO-LOCK.
   END.
   CLOSE QUERY kq2.
   OUTPUT CLOSE.
