/*KALKYLPRODATABORT.i*/
DEFINE DATASET KalkylbortDS FOR kalkhuvtt,kalknumtt,kalknumsubtt,kalkaonrTT,kalkfaktorertt,kalkegnaprisertt,kalktmtrlTT
DATA-RELATION KalkhuvNumsDR FOR kalkhuvtt, kalknumtt RELATION-FIELDS (kalkhuvtt.KALKNR,kalknumtt.KALKNR,kalkhuvtt.OMRADE,kalknumtt.OMRADE)
DATA-RELATION KalknumSubsDR FOR kalknumtt, kalknumsubtt RELATION-FIELDS (kalknumtt.KALKNR,kalknumsubtt.KALKNR,kalknumtt.OMRADE,kalknumsubtt.OMRADE,kalknumtt.NUM,kalknumsubtt.NUM)
DATA-RELATION KalkhuvAonrDR FOR kalkhuvtt, kalkaonrTT RELATION-FIELDS (kalkhuvtt.KALKNR,kalkaonrTT.KALKNR,kalkhuvtt.OMRADE,kalkaonrTT.OMRADE)
DATA-RELATION KalkhuvFaktDR FOR kalkhuvtt, kalkfaktorertt RELATION-FIELDS (kalkhuvtt.KALKNR,kalkfaktorertt.KALKNR,kalkhuvtt.OMRADE,kalkfaktorertt.OMRADE)
DATA-RELATION KalkhuvEgnaDR FOR kalkhuvtt, kalkegnaprisertt RELATION-FIELDS (kalkhuvtt.KALKNR,kalkegnaprisertt.KALKNR,kalkhuvtt.OMRADE,kalkegnaprisertt.OMRADE)
DATA-RELATION KalkhuvMtrlDR FOR kalkhuvtt, kalktmtrlTT RELATION-FIELDS (kalkhuvtt.KALKNR,kalktmtrlTT.KALKNR,kalkhuvtt.OMRADE,kalktmtrlTT.OMRADE).

/*
DEFINE DATASET KalkylbortDS FOR kalkhuvtt,GuruDefaultsTT,kalknumtt,kalknumsubtt,kalkaonrTT,kalkfaktorertt,kalkegnaprisertt,kalktmtrlTT
DATA-RELATION KalkhuvDefaultDR FOR kalkhuvtt, GuruDefaultsTT RELATION-FIELDS (kalkhuvtt.KALKNR,GuruDefaultsTT.HUVUDINT,kalkhuvtt.OMRADE,GuruDefaultsTT.HUVUDCHAR)
DATA-RELATION KalkhuvNumsDR FOR kalkhuvtt, kalknumtt RELATION-FIELDS (kalkhuvtt.KALKNR,kalknumtt.KALKNR,kalkhuvtt.OMRADE,kalknumtt.OMRADE)
DATA-RELATION KalknumSubsDR FOR kalknumtt, kalknumsubtt RELATION-FIELDS (kalknumtt.KALKNR,kalknumsubtt.KALKNR,kalknumtt.OMRADE,kalknumsubtt.OMRADE,kalknumtt.NUM,kalknumsubtt.NUM)
DATA-RELATION KalkhuvAonrDR FOR kalkhuvtt, kalkaonrTT RELATION-FIELDS (kalkhuvtt.KALKNR,kalkaonrTT.KALKNR,kalkhuvtt.OMRADE,kalkaonrTT.OMRADE)
DATA-RELATION KalkhuvFaktDR FOR kalkhuvtt, kalkfaktorertt RELATION-FIELDS (kalkhuvtt.KALKNR,kalkfaktorertt.KALKNR,kalkhuvtt.OMRADE,kalkfaktorertt.OMRADE)
DATA-RELATION KalkhuvEgnaDR FOR kalkhuvtt, kalkegnaprisertt RELATION-FIELDS (kalkhuvtt.KALKNR,kalkegnaprisertt.KALKNR,kalkhuvtt.OMRADE,kalkegnaprisertt.OMRADE)
DATA-RELATION KalkhuvMtrlDR FOR kalkhuvtt, kalktmtrlTT RELATION-FIELDS (kalkhuvtt.KALKNR,kalktmtrlTT.KALKNR,kalkhuvtt.OMRADE,kalktmtrlTT.OMRADE).




















DEFINE DATASET KalkylDS FOR kalkhuvtt,GuruDefaultsTT,kalknumtt,kalknumsubtt,kalkaonrTT,kalkfaktorertt,kalkegnaprisertt,kalktmtrlTT
DATA-RELATION KalkhuvDefaultDR FOR kalkhuvtt, GuruDefaultsTT RELATION-FIELDS (kalkhuvtt.KALKNR,GuruDefaultsTT.HUVUDINT,kalkhuvtt.OMRADE,GuruDefaultsTT.HUVUDCHAR)
DATA-RELATION KalkhuvNumsDR FOR kalkhuvtt, kalknumtt RELATION-FIELDS (kalkhuvtt.KALKNR,kalknumtt.KALKNR,kalkhuvtt.OMRADE,kalknumtt.OMRADE)
DATA-RELATION KalknumSubsDR FOR kalknumtt, kalknumsubtt RELATION-FIELDS (kalknumtt.KALKNR,kalknumsubtt.KALKNR,kalknumtt.OMRADE,kalknumsubtt.OMRADE,kalknumtt.NUM,kalknumsubtt.NUM)
DATA-RELATION KalkhuvAonrDR FOR kalkhuvtt, kalkaonrTT RELATION-FIELDS (kalkhuvtt.KALKNR,kalkaonrTT.KALKNR,kalkhuvtt.OMRADE,kalkaonrTT.OMRADE)
DATA-RELATION KalkhuvFaktDR FOR kalkhuvtt, kalkfaktorertt RELATION-FIELDS (kalkhuvtt.KALKNR,kalkfaktorertt.KALKNR,kalkhuvtt.OMRADE,kalkfaktorertt.OMRADE)
DATA-RELATION KalkhuvEgnaDR FOR kalkhuvtt, kalkegnaprisertt RELATION-FIELDS (kalkhuvtt.KALKNR,kalkegnaprisertt.KALKNR,kalkhuvtt.OMRADE,kalkegnaprisertt.OMRADE)
DATA-RELATION KalkhuvMtrlDR FOR kalkhuvtt, kalktmtrlTT RELATION-FIELDS (kalkhuvtt.KALKNR,kalktmtrlTT.KALKNR,kalkhuvtt.OMRADE,kalktmtrlTT.OMRADE).



DEFINE DATASET KalkylDS FOR GuruDefaultsTT,kalkhuvtt,kalknumtt,kalknumsubtt,kalkaonrTT,kalkfaktorertt,kalkegnaprisertt,kalktmtrlTT
DATA-RELATION KalkhuvtDR FOR GuruDefaultsTT,kalkhuvtt RELATION-FIELDS (GuruDefaultsTT.HUVUDINT,kalkhuvtt.KALKNR,GuruDefaultsTT.HUVUDCHAR,kalkhuvtt.OMRADE)
DATA-RELATION KalkhuvNumsDR FOR kalkhuvtt, kalknumtt RELATION-FIELDS (kalkhuvtt.KALKNR,kalknumtt.KALKNR,kalkhuvtt.OMRADE,kalknumtt.OMRADE)
DATA-RELATION KalknumSubsDR FOR kalknumtt, kalknumsubtt RELATION-FIELDS (kalknumtt.KALKNR,kalknumsubtt.KALKNR,kalknumtt.OMRADE,kalknumsubtt.OMRADE,kalknumtt.NUM,kalknumsubtt.NUM)
DATA-RELATION KalkhuvAonrDR FOR kalkhuvtt, kalkaonrTT RELATION-FIELDS (kalkhuvtt.KALKNR,kalkaonrTT.KALKNR,kalkhuvtt.OMRADE,kalkaonrTT.OMRADE)
DATA-RELATION KalkhuvFaktDR FOR kalkhuvtt, kalkfaktorertt RELATION-FIELDS (kalkhuvtt.KALKNR,kalkfaktorertt.KALKNR,kalkhuvtt.OMRADE,kalkfaktorertt.OMRADE)
DATA-RELATION KalkhuvEgnaDR FOR kalkhuvtt, kalkegnaprisertt RELATION-FIELDS (kalkhuvtt.KALKNR,kalkegnaprisertt.KALKNR,kalkhuvtt.OMRADE,kalkegnaprisertt.OMRADE)
DATA-RELATION KalkhuvMtrlDR FOR kalkhuvtt, kalktmtrlTT RELATION-FIELDS (kalkhuvtt.KALKNR,kalktmtrlTT.KALKNR,kalkhuvtt.OMRADE,kalktmtrlTT.OMRADE).
*/

