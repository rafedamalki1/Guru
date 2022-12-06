   /*TILLFAST2.I*/     
   status-ok = RAD_FAST:DELETE("Tillfälliga aonr").
   status-ok = RAD_FAST:DELETE("Fasta aonr").
   status-ok = RAD_FAST:DELETE("Favorit aonr").
   RAD_FAST:ADD-LAST(Guru.Konstanter:gtillk + " " + LC(Guru.Konstanter:gaok), 1).
   RAD_FAST:ADD-LAST(Guru.Konstanter:gfastl + " " + LC(Guru.Konstanter:gaok), 2). 
   RAD_FAST:ADD-LAST("Favorit" + " " + LC(Guru.Konstanter:gaok), 3). 
   /*
   RAD_FAST:ADD-LAST("MINA NUMMER", "?"). 
     */
