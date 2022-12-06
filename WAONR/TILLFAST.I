   /*TILLFAST.I*/     
   status-ok = RAD_FAST:DELETE("Tillfälliga aonr").
   status-ok = RAD_FAST:DELETE("Fasta aonr").
   RAD_FAST:ADD-LAST(Guru.Konstanter:gtillk + " " + LC(Guru.Konstanter:gaok), no).
   RAD_FAST:ADD-LAST(Guru.Konstanter:gfastl + " " + LC(Guru.Konstanter:gaok), yes). 
   /*
   RAD_FAST:ADD-LAST("MINA NUMMER", "?"). 
     */
