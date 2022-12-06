   /*TILFSTPL.I*/     
   status-ok = RAD_FAST:DELETE("Tillfälliga plannr").
   status-ok = RAD_FAST:DELETE("Fasta plannr").
   RAD_FAST:ADD-LAST(Guru.Konstanter:gtillk + " " + LC(Guru.Konstanter:gplk), no).
   RAD_FAST:ADD-LAST(Guru.Konstanter:gfastl + " " + LC(Guru.Konstanter:gpll), yes). 
