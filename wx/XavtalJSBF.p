
/* XAVTALJSBF.P las ut ett visst avtal*/
/*OUTPUT TO C:\LENA\ny2.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  veckoarbetid:
   EXPORT veckoarbetid.
END.
OUTPUT TO C:\LENA\arbetsti.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  arbetsti:
   EXPORT arbetsti.
END.*/
OUTPUT TO C:\LENA\ansform.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  anstform /*where anstform.kod = "TE" */:
    EXPORT anstform.
END.

OUTPUT TO C:\LENA\ersattnu.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  ersattning /*where ersattning.kod = "TE" */:
   EXPORT ersattning.
END.

OUTPUT TO C:\LENA\lontill.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  lontill /*where lontill.kod = "TE" */:
   EXPORT lontill.
END.

OUTPUT TO C:\LENA\overavta.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  overavta /*where overavta.kod = "TE"*/ :
   EXPORT overavta.
END.
OUTPUT TO C:\LENA\overkod.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  overkod /*where overkod.kod = "TE" */:
   EXPORT overkod.
END.
OUTPUT TO C:\LENA\overtidt.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  overtidt /*where overtidt.kod = "TE"*/ :
   EXPORT overtidt.
END.
OUTPUT TO C:\LENA\restidta.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  restidta /*where restidta.kod = "TE" */:
   EXPORT restidta.
END.

OUTPUT TO C:\LENA\utrtab.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  utrtab /*where utrtab.kod = "TE"*/ :
   EXPORT utrtab.
END.
OUTPUT TO C:\LENA\utryckni.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  utryckni /*where utryckni.kod = "TE" */:
   EXPORT utryckni.
END.

OUTPUT TO C:\LENA\beredsk1.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  beredskapav /*where beredskapav.beredskapsavtal = "KE"*/:
   EXPORT beredskapav.
END.
OUTPUT TO C:\LENA\beredsk5.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  beredskapstart /*where beredskapstart.beredskapsavtal = "KE"*/:
   EXPORT beredskapstart.
END.
OUTPUT TO C:\LENA\beredska.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  beredskaptab /*where beredskaptab.beredskapsavtal = "KE"*/:
   EXPORT beredskaptab.
END.
OUTPUT TO C:\LENA\berkod.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  berkod where berkod.beredskapsavtal = "KE":
   EXPORT berkod.
END.
OUTPUT TO C:\LENA\bertab.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  bertab /*where bertab.beredskapsavtal = "KE"*/:
   EXPORT bertab.
END.


OUTPUT TO C:\LENA\avdragma.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  avdragma /*where avdragma.traavtal = "KV"*/:
   EXPORT avdragma.
END.

OUTPUT TO C:\LENA\kforman.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  kforman /*where kforman.traavtal = "KV"*/:
   EXPORT kforman.
END.
OUTPUT TO C:\LENA\lonfler.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  lonfler /*where lonfler.traavtal = "KV"*/:
   EXPORT lonfler.
END.

OUTPUT TO C:\LENA\maltab.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  maltab /*where maltab.traavtal = "KV" */:
   EXPORT maltab.
END.

OUTPUT TO C:\LENA\traavtab.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  traavtab /*where traavtab.traavtal = "KV"*/:
   EXPORT traavtab.
END.
OUTPUT TO C:\LENA\traktast.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  traktast /*where traktast.traavtal = "KV" */:
   EXPORT traktast.
END.
OUTPUT TO C:\LENA\traktata.d convert target "iso8859-1" source "iso8859-1".
FOR EACH traktata /*where traktata.traavtal = "KV"*/:
   EXPORT traktata.
END.
OUTPUT TO C:\LENA\traktfle.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  traktfle /*where traktfle.traavtal = "KV"*/:
   EXPORT traktfle.
END.
OUTPUT TO C:\LENA\traktreg.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  traktreg /*where traktreg.traavtal = "KV"*/:
   EXPORT traktreg.
END.


OUTPUT TO C:\LENA\berhojn.d convert target "iso8859-1" source "iso8859-1".
FOR EACH BERHOJN  :
   EXPORT BERHOJN.
END.

OUTPUT TO C:\LENA\bhoj.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  BHOJ:
   EXPORT BHOJ.
END.
OUTPUT TO C:\LENA\bilforar.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  BILFORARE:
   EXPORT BILFORARE.
END.
OUTPUT TO C:\LENA\flexreg.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  FLEXREG:
   EXPORT FLEXREG.
END.
OUTPUT TO C:\LENA\fvaro.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  FVARO:
   EXPORT FVARO.
END.
OUTPUT TO C:\LENA\lagbas.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  LAGBAS:
   EXPORT LAGBAS.
END.
OUTPUT TO C:\LENA\land.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  LAND:
   EXPORT LAND.
END.
OUTPUT TO C:\LENA\nfall.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  NFALL:
   EXPORT NFALL.
END.
OUTPUT TO C:\LENA\obavtab.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  OBAVTAB:
   EXPORT OBAVTAB.
END.
OUTPUT TO C:\LENA\obtab.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  OBTAB:
   EXPORT OBTAB.
END.
OUTPUT TO C:\LENA\ordarb.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  ORDARB:
   EXPORT ORDARB.
END.
/*OUTPUT TO .d convert target "iso8859-1" source "iso8859-1".
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
=======
/* XAVTALJSBF.P las ut ett visst avtal*/
/*OUTPUT TO C:\LENA\ny2.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  veckoarbetid:
   EXPORT veckoarbetid.
END.
OUTPUT TO C:\LENA\arbetsti.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  arbetsti:
   EXPORT arbetsti.
END.*/
OUTPUT TO C:\LENA\ansform.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  anstform /*where anstform.kod = "TE" */:
    EXPORT anstform.
END.

OUTPUT TO C:\LENA\ersattnu.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  ersattning /*where ersattning.kod = "TE" */:
   EXPORT ersattning.
END.

OUTPUT TO C:\LENA\lontill.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  lontill /*where lontill.kod = "TE" */:
   EXPORT lontill.
END.

OUTPUT TO C:\LENA\overavta.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  overavta /*where overavta.kod = "TE"*/ :
   EXPORT overavta.
END.
OUTPUT TO C:\LENA\overkod.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  overkod /*where overkod.kod = "TE" */:
   EXPORT overkod.
END.
OUTPUT TO C:\LENA\overtidt.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  overtidt /*where overtidt.kod = "TE"*/ :
   EXPORT overtidt.
END.
OUTPUT TO C:\LENA\restidta.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  restidta /*where restidta.kod = "TE" */:
   EXPORT restidta.
END.

OUTPUT TO C:\LENA\utrtab.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  utrtab /*where utrtab.kod = "TE"*/ :
   EXPORT utrtab.
END.
OUTPUT TO C:\LENA\utryckni.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  utryckni /*where utryckni.kod = "TE" */:
   EXPORT utryckni.
END.

OUTPUT TO C:\LENA\beredsk1.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  beredskapav /*where beredskapav.beredskapsavtal = "KE"*/:
   EXPORT beredskapav.
END.
OUTPUT TO C:\LENA\beredsk5.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  beredskapstart /*where beredskapstart.beredskapsavtal = "KE"*/:
   EXPORT beredskapstart.
END.
OUTPUT TO C:\LENA\beredska.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  beredskaptab /*where beredskaptab.beredskapsavtal = "KE"*/:
   EXPORT beredskaptab.
END.
OUTPUT TO C:\LENA\berkod.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  berkod where berkod.beredskapsavtal = "KE":
   EXPORT berkod.
END.
OUTPUT TO C:\LENA\bertab.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  bertab /*where bertab.beredskapsavtal = "KE"*/:
   EXPORT bertab.
END.


OUTPUT TO C:\LENA\avdragma.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  avdragma /*where avdragma.traavtal = "KV"*/:
   EXPORT avdragma.
END.

OUTPUT TO C:\LENA\kforman.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  kforman /*where kforman.traavtal = "KV"*/:
   EXPORT kforman.
END.
OUTPUT TO C:\LENA\lonfler.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  lonfler /*where lonfler.traavtal = "KV"*/:
   EXPORT lonfler.
END.

OUTPUT TO C:\LENA\maltab.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  maltab /*where maltab.traavtal = "KV" */:
   EXPORT maltab.
END.

OUTPUT TO C:\LENA\traavtab.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  traavtab /*where traavtab.traavtal = "KV"*/:
   EXPORT traavtab.
END.
OUTPUT TO C:\LENA\traktast.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  traktast /*where traktast.traavtal = "KV" */:
   EXPORT traktast.
END.
OUTPUT TO C:\LENA\traktata.d convert target "iso8859-1" source "iso8859-1".
FOR EACH traktata /*where traktata.traavtal = "KV"*/:
   EXPORT traktata.
END.
OUTPUT TO C:\LENA\traktfle.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  traktfle /*where traktfle.traavtal = "KV"*/:
   EXPORT traktfle.
END.
OUTPUT TO C:\LENA\traktreg.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  traktreg /*where traktreg.traavtal = "KV"*/:
   EXPORT traktreg.
END.


OUTPUT TO C:\LENA\berhojn.d convert target "iso8859-1" source "iso8859-1".
FOR EACH BERHOJN  :
   EXPORT BERHOJN.
END.

OUTPUT TO C:\LENA\bhoj.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  BHOJ:
   EXPORT BHOJ.
END.
OUTPUT TO C:\LENA\bilforar.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  BILFORARE:
   EXPORT BILFORARE.
END.
OUTPUT TO C:\LENA\flexreg.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  FLEXREG:
   EXPORT FLEXREG.
END.
OUTPUT TO C:\LENA\fvaro.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  FVARO:
   EXPORT FVARO.
END.
OUTPUT TO C:\LENA\lagbas.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  LAGBAS:
   EXPORT LAGBAS.
END.
OUTPUT TO C:\LENA\land.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  LAND:
   EXPORT LAND.
END.
OUTPUT TO C:\LENA\nfall.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  NFALL:
   EXPORT NFALL.
END.
OUTPUT TO C:\LENA\obavtab.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  OBAVTAB:
   EXPORT OBAVTAB.
END.
OUTPUT TO C:\LENA\obtab.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  OBTAB:
   EXPORT OBTAB.
END.
OUTPUT TO C:\LENA\ordarb.d convert target "iso8859-1" source "iso8859-1".
FOR EACH  ORDARB:
   EXPORT ORDARB.
END.
/*OUTPUT TO .d convert target "iso8859-1" source "iso8859-1".
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
>>>>>>> branch 'master' of file:///\\server05\delad\REMOTEGURU\GuruRemote.git
