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

asokchar[1] = "gavdl". /*AVDELNING L�NG*/
aben[1] = "Avdelning". 

asokchar[2] = "gavdk". /*AVDELNING KORT*/ 
aben[2] = "Avd.".

asokchar[3] = "gomrl". /*OMR�DE L�NG*/ 
aben[3] = "Omr�de".

asokchar[4] = "gomrk". /*OMR�DE KORT*/ 
aben[4] = "OMRADETEST".

asokchar[5] = "gaol". /*AONR L�NG*/ 
aben[5] = "Projektnummer".

asokchar[6] = "gaok".  /*AONR KORT*/  
aben[6] = "P-nr.".

asokchar[7] = "gpll". /*PLANNR L�NG*/
aben[7] = "Plannummer".

asokchar[8] = "gplk". /*PLANNR KORT*/
aben[8] = "Pl-nr.".

asokchar[9] = "genl". /*E-NUMMER L�NG*/ 
aben[9] = "E-Nummer".

asokchar[10] = "genk". /*E-NUMMER KORT*/
aben[10] = "E-nr".

asokchar[11] = "gjul". /*JURIDISK L�NG*/ 
aben[11] = "Juridisk".

asokchar[12] = "gjuk". /*JURIDISK KORT*/
aben[12] = "Jur.".

asokchar[13] = "gfastl". /*FASTA L�NG*/ 
aben[13] = "Fasta".

asokchar[14] = "gfastk".  /*FASTA KORT*/
aben[14] = "Fst.".

asokchar[15] = "gtilll". /*TILLF�LIGA L�NG*/
aben[15] = "Tillf�lliga".

asokchar[16] = "gtillk".  /*TILLF�LIGA KORT*/
aben[16] = "Tlfg.".

asokchar[17] = "gberel". /*BEREDARE L�NG*/
aben[17] = "Beredare".

asokchar[18] = "gberek". /*BEREDARE KORT*/
aben[18] = "Brd.".

asokchar[19] = "gprojl".  /*PROJEKT�R L�NG*/ 
aben[19] = "Projekt�r".

asokchar[20] = "gprojk". /*PROJEKT�R KORT*/
aben[20] = "Prj.�r".

asokchar[21] = "garbal". /*ARBETSANSVARIG L�NG*/
aben[21] = "Arbetsansvarig".

asokchar[22] = "garbak".  /*ARBETSANSVARIG KORT*/
aben[22] = "Arb.ansvr.".

asokchar[23] = "gutfk". /*Utf�rande KORT*/
aben[23] = "Utf.".

asokchar[24] = "gutfl". /*Utf�rande L�NG*/ 
aben[24] = "Utf�rande".

asokchar[25] = "gbestk".  /*Best�llare/Kund KORT*/
aben[25] = "Bst/Knd.".

asokchar[26] = "gbestl". /*Best�llare/Kund L�NG*/ 
aben[26] = "Best�llare/Kund".

asokchar[27] = "gdebk".  /*Debiterigstyp KORT*/
aben[27] = "Dbt.".

asokchar[28] = "gdebl". /*Debiterigstyp L�NG*/ 
aben[28] = "Debiteringstyp".

asokchar[29] = "gtidlk". /*tidl�ge KORT*/
aben[29] = "Tidl.".

asokchar[30] = "gtidll". /*tidl�ge L�NG*/ 
aben[30] = "Tidl�ge".

asokchar[31] = "gutfardk".  /*utf�rdat av KORT*/
aben[31] = "Utf. Av.".

asokchar[32] = "gutfardl". /*utf�rdat av L�NG*/ 
aben[32] = "Utf�rdat av".

asokchar[33] = "grefbefk". /*Ref.nr best�llare KORT*/
aben[33] = "Best�llarreferens".

asokchar[34] = "grefbefl". /*Ref.nr best�llare  L�NG*/ 
aben[34] = "Best. ref.".

asokchar[35] = "gpriok".  /*Prioritet KORT*/
aben[35] = "Prio".

asokchar[36] = "gpriol".   /*Prioritet  L�NG*/
aben[36] = "Prioritet".

asokchar[37] = "gartk". /*Arbetsart KORT*/
aben[37] = "Arb.art".

asokchar[38] = "gartl".  /*Arbetsart  L�NG*/
aben[38] = "Arbetsart".

asokchar[39] = "gaonamnk".  /*Ort/Ben�mning aonr KORT*/
aben[39] = "Ort.Ben ao".

asokchar[40] = "gaonamnl".  /*Ort/Ben�mning aonr  L�NG*/
aben[40] = "Ort/Ben�mning aonr".

asokchar[41] = "bufferfill".  /*Ort/Ben�mning aonr  L�NG*/
aben[41] = "Finns den h�r kan den tas bort".

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
