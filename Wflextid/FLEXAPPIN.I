/*FLEXAPPIN.I*/
   FIND FIRST flexapp NO-ERROR.
   IF NOT AVAILABLE flexapp THEN CREATE flexapp.
   ASSIGN   
   flexapp.ANVANDARE = Guru.Konstanter:globanv
   flexapp.AONR    = onr
   flexapp.DELNR   = dnr
   flexapp.TRAKT   = tra
   flexapp.OVERUT  = ovut
   flexapp.PRISTYP = typ
   flexapp.PRIS    = tpris
   flexapp.V3AONR   = aonr3
   flexapp.V3DELNR   = dnr3
   flexapp.V3PRISTYP = typ3
   flexapp.V3PRIS    = tpris3
   flexapp.TIDEJ   = ejtid
   flexapp.ORSAK  = florsak
   flexapp.TID    = tid5
   flexapp.DATUM   = regdatum
   flexapp.STARTREG = regstart
   flexapp.SLUTREG = regslut
   flexapp.KOLLA   = kollen
   flexapp.KOLLA2   = kollen2
   flexapp.KOLLA3   = kollen3
   flexapp.KOLLFL   =  flkoll
   flexapp.MOSTFLEX = flexmost
   flexapp.MOSLFLEX = flexmosl
   flexapp.KVSL     =  flexkvsl.
