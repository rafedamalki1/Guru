/* Xavtal.p las ut ett visst avtal*/
OUTPUT TO ny2.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  veckoarbetid:
   EXPORT veckoarbetid.
END.
OUTPUT TO arbetsti.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  arbetsti:
   EXPORT arbetsti.
END.
OUTPUT TO a:\ansform.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  anstform where anstform.kod = "TE" :
    EXPORT anstform.
END.

OUTPUT TO a:\ersattnu.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  ersattning where ersattning.kod = "TE" :
   EXPORT ersattning.
END.

OUTPUT TO a:\lontill.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  lontill where lontill.kod = "TE" :
   EXPORT lontill.
END.

OUTPUT TO a:\overavta.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  overavta where overavta.kod = "TE" :
   EXPORT overavta.
END.
OUTPUT TO a:\overkod.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  overkod where overkod.kod = "TE" :
   EXPORT overkod.
END.
OUTPUT TO a:\overtidt.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  overtidt where overtidt.kod = "TE" :
   EXPORT overtidt.
END.
OUTPUT TO a:\restidta.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  restidta where restidta.kod = "TE" :
   EXPORT restidta.
END.

OUTPUT TO a:\utrtab.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  utrtab where utrtab.kod = "TE" :
   EXPORT utrtab.
END.
OUTPUT TO a:\utryckni.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  utryckni where utryckni.kod = "TE" :
   EXPORT utryckni.
END.

OUTPUT TO a:\beredsk1.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  beredskapav where beredskapav.beredskapsavtal = "KE":
   EXPORT beredskapav.
END.
OUTPUT TO a:\beredsk5.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  beredskapstart where beredskapstart.beredskapsavtal = "KE":
   EXPORT beredskapstart.
END.
OUTPUT TO a:\beredska.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  beredskaptab where beredskaptab.beredskapsavtal = "KE":
   EXPORT beredskaptab.
END.
OUTPUT TO a:\berkod.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  berkod where berkod.beredskapsavtal = "KE":
   EXPORT berkod.
END.
OUTPUT TO a:\bertab.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  bertab where bertab.beredskapsavtal = "KE":
   EXPORT bertab.
END.


OUTPUT TO a:\avdragma.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  avdragma where avdragma.traavtal = "KV":
   EXPORT avdragma.
END.

OUTPUT TO a:\kforman.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  kforman where kforman.traavtal = "KV":
   EXPORT kforman.
END.
OUTPUT TO a:\lonfler.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  lonfler where lonfler.traavtal = "KV":
   EXPORT lonfler.
END.

OUTPUT TO a:\maltab.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  maltab where maltab.traavtal = "KV" :
   EXPORT maltab.
END.

OUTPUT TO a:\traavtab.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  traavtab where traavtab.traavtal = "KV":
   EXPORT traavtab.
END.
OUTPUT TO a:\traktast.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  traktast where traktast.traavtal = "KV" :
   EXPORT traktast.
END.
OUTPUT TO a:\traktata.d convert target "iso8859-1" source "iso8859-1".
FOR EACH traktata where traktata.traavtal = "KV":
   EXPORT traktata.
END.
OUTPUT TO a:\traktfle.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  traktfle where traktfle.traavtal = "KV":
   EXPORT traktfle.
END.
OUTPUT TO a:\traktreg.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  traktreg where traktreg.traavtal = "KV":
   EXPORT traktreg.
END.

/*
OUTPUT TO .d convert target "iso8859-1" source "iso8859-1".
FOR EACH  :
   EXPORT .
END.
OUTPUT TO .d convert target "iso8859-1" source "iso8859-1".
FOR EACH  :
   EXPORT .
END.
OUTPUT TO .d convert target "iso8859-1" source "iso8859-1".
FOR EACH  :
   EXPORT .
END.
OUTPUT TO .d convert target "iso8859-1" source "iso8859-1".
FOR EACH  :
   EXPORT .
END.
OUTPUT TO .d convert target "iso8859-1" source "iso8859-1".
FOR EACH  :
   EXPORT .
END.
OUTPUT TO .d convert target "iso8859-1" source "iso8859-1".
FOR EACH  :
   EXPORT .
END.
OUTPUT TO .d convert target "iso8859-1" source "iso8859-1".
FOR EACH  :
   EXPORT .
END.
OUTPUT TO .d convert target "iso8859-1" source "iso8859-1".
FOR EACH  :
   EXPORT .
END.
OUTPUT TO .d convert target "iso8859-1" source "iso8859-1".
FOR EACH  :
   EXPORT .
END.
OUTPUT TO .d convert target "iso8859-1" source "iso8859-1".
FOR EACH  :
   EXPORT .
END.
OUTPUT TO .d convert target "iso8859-1" source "iso8859-1".
FOR EACH  :
   EXPORT .
END.
OUTPUT TO .d convert target "iso8859-1" source "iso8859-1".
FOR EACH  :
   EXPORT .
END.
OUTPUT TO .d convert target "iso8859-1" source "iso8859-1".
FOR EACH  :
   EXPORT .
END.
OUTPUT TO .d convert target "iso8859-1" source "iso8859-1".
FOR EACH  :
   EXPORT .
END.
OUTPUT TO .d convert target "iso8859-1" source "iso8859-1".
FOR EACH  :
   EXPORT .
END.
OUTPUT TO .d convert target "iso8859-1" source "iso8859-1".
FOR EACH  :
   EXPORT .
END.
OUTPUT TO .d convert target "iso8859-1" source "iso8859-1".
FOR EACH  :
   EXPORT .
END.
OUTPUT TO .d convert target "iso8859-1" source "iso8859-1".
FOR EACH  :
   EXPORT .
END.

*/
