DEFINE DATASET SprakDS FOR spraktemp,sprakstrangtemp
   DATA-RELATION SprakStrangDR FOR spraktemp, sprakstrangtemp RELATION-FIELDS (spraktemp.ID, sprakstrangtemp.SPRAKID).  