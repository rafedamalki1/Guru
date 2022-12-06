/*SkapaSprak.p*/
FOR EACH SPRAK_STRANG :
    DELETE SPRAK_STRANG.
END.

FOR EACH SPRAK:
    DELETE SPRAK.
END.

CREATE SPRAK.
SPRAK.ID = 0.
SPRAK.BENAMNING = "Svenska".


DEFINE VARIABLE asokchar AS CHARACTER EXTENT 41 NO-UNDO.
DEFINE VARIABLE aben AS CHARACTER EXTENT 41 NO-UNDO.

asokchar[1] = "gavdl". /*AVDELNING LÅNG*/
aben[1] = "Avdelning". 

asokchar[2] = "gavdk". /*AVDELNING KORT*/ 
aben[2] = "Avd.".

asokchar[3] = "gomrl". /*OMRÅDE LÅNG*/ 
aben[3] = "Område".

asokchar[4] = "gomrk". /*OMRÅDE KORT*/ 
aben[4] = "OMRADETEST".

asokchar[5] = "gaol". /*AONR LÅNG*/ 
aben[5] = "Projektnummer".

asokchar[6] = "gaok".  /*AONR KORT*/  
aben[6] = "P-nr.".

asokchar[7] = "gpll". /*PLANNR LÅNG*/
aben[7] = "Plannummer".

asokchar[8] = "gplk". /*PLANNR KORT*/
aben[8] = "Pl-nr.".

asokchar[9] = "genl". /*E-NUMMER LÅNG*/ 
aben[9] = "E-Nummer".

asokchar[10] = "genk". /*E-NUMMER KORT*/
aben[10] = "E-nr".

asokchar[11] = "gjul". /*JURIDISK LÅNG*/ 
aben[11] = "Juridisk".

asokchar[12] = "gjuk". /*JURIDISK KORT*/
aben[12] = "Jur.".

asokchar[13] = "gfastl". /*FASTA LÅNG*/ 
aben[13] = "Fasta".

asokchar[14] = "gfastk".  /*FASTA KORT*/
aben[14] = "Fst.".

asokchar[15] = "gtilll". /*TILLFÄLIGA LÅNG*/
aben[15] = "Tillfälliga".

asokchar[16] = "gtillk".  /*TILLFÄLIGA KORT*/
aben[16] = "Tlfg.".

asokchar[17] = "gberel". /*BEREDARE LÅNG*/
aben[17] = "Beredare".

asokchar[18] = "gberek". /*BEREDARE KORT*/
aben[18] = "Brd.".

asokchar[19] = "gprojl".  /*PROJEKTÖR LÅNG*/ 
aben[19] = "Projektör".

asokchar[20] = "gprojk". /*PROJEKTÖR KORT*/
aben[20] = "Prj.ör".

asokchar[21] = "garbal". /*ARBETSANSVARIG LÅNG*/
aben[21] = "Arbetsansvarig".

asokchar[22] = "garbak".  /*ARBETSANSVARIG KORT*/
aben[22] = "Arb.ansvr.".

asokchar[23] = "gutfk". /*Utförande KORT*/
aben[23] = "Utf.".

asokchar[24] = "gutfl". /*Utförande LÅNG*/ 
aben[24] = "Utförande".

asokchar[25] = "gbestk".  /*Beställare/Kund KORT*/
aben[25] = "Bst/Knd.".

asokchar[26] = "gbestl". /*Beställare/Kund LÅNG*/ 
aben[26] = "Beställare/Kund".

asokchar[27] = "gdebk".  /*Debiterigstyp KORT*/
aben[27] = "Dbt.".

asokchar[28] = "gdebl". /*Debiterigstyp LÅNG*/ 
aben[28] = "Debiteringstyp".

asokchar[29] = "gtidlk". /*tidläge KORT*/
aben[29] = "Tidl.".

asokchar[30] = "gtidll". /*tidläge LÅNG*/ 
aben[30] = "Tidläge".

asokchar[31] = "gutfardk".  /*utfärdat av KORT*/
aben[31] = "Utf. Av.".

asokchar[32] = "gutfardl". /*utfärdat av LÅNG*/ 
aben[32] = "Utfärdat av".

asokchar[33] = "grefbefk". /*Ref.nr beställare KORT*/
aben[33] = "Beställarreferens".

asokchar[34] = "grefbefl". /*Ref.nr beställare  LÅNG*/ 
aben[34] = "Best. ref.".

asokchar[35] = "gpriok".  /*Prioritet KORT*/
aben[35] = "Prio".

asokchar[36] = "gpriol".   /*Prioritet  LÅNG*/
aben[36] = "Prioritet".

asokchar[37] = "gartk". /*Arbetsart KORT*/
aben[37] = "Arb.art".

asokchar[38] = "gartl".  /*Arbetsart  LÅNG*/
aben[38] = "Arbetsart".

asokchar[39] = "gaonamnk".  /*Ort/Benämning aonr KORT*/
aben[39] = "Ort.Ben ao".

asokchar[40] = "gaonamnl".  /*Ort/Benämning aonr  LÅNG*/
aben[40] = "Ort/Benämning aonr".

asokchar[41] = "bufferfill".  /*Ort/Benämning aonr  LÅNG*/
aben[41] = "Finns den här kan den tas bort".

DEFINE VARIABLE counter AS INTEGER INITIAL 1 NO-UNDO.
DO WHILE counter < 41:

   DEFINE VARIABLE ch AS INTEGER NO-UNDO.
   FIND LAST SPRAK_STRANG NO-ERROR.
   IF NOT AVAILABLE(SPRAK_STRANG) THEN ch = 0.
   ELSE ch = SPRAK_STRANG.ID + 1.


   CREATE SPRAK_STRANG.
   SPRAK_STRANG.ID = ch.
   SPRAK_STRANG.SPRAKID = 0.
   SPRAK_STRANG.SOKID = counter.
   SPRAK_STRANG.BENAMNING = aben[counter].
   SPRAK_STRANG.SOKCHAR = asokchar[counter].
   counter = counter + 1.
END.

CREATE SPRAK_STRANG.
   SPRAK_STRANG.ID = 40.
   SPRAK_STRANG.SPRAKID = 0.
   SPRAK_STRANG.SOKID = 41.
   SPRAK_STRANG.BENAMNING = "Skaopa neu!".
   SPRAK_STRANG.SOKCHAR = "".
