/*KALKYLPRODATA.i*/
DEFINE DATASET KalkylDS FOR kalkhuvtt,kalknumtt,kalknumsubtt,kalkaonrTT,kalkfaktorertt,kalkegnaprisertt,kalktmtrlTT,kalkttidlageTT
DATA-RELATION huvNumsDR FOR kalkhuvtt, kalknumtt RELATION-FIELDS (kalkhuvtt.KALKNR,kalknumtt.KALKNR,kalkhuvtt.OMRADE,kalknumtt.OMRADE)
DATA-RELATION numSubsDR FOR kalknumtt, kalknumsubtt RELATION-FIELDS (kalknumtt.KALKNR,kalknumsubtt.KALKNR,kalknumtt.OMRADE,kalknumsubtt.OMRADE,kalknumtt.NUM,kalknumsubtt.NUM)
DATA-RELATION huvAonrDR FOR kalkhuvtt, kalkaonrTT RELATION-FIELDS (kalkhuvtt.KALKNR,kalkaonrTT.KALKNR,kalkhuvtt.OMRADE,kalkaonrTT.OMRADE)
 /*EXTRA KLOGSUBID*/
DATA-RELATION huvFaktDR FOR kalkhuvtt, kalkfaktorertt RELATION-FIELDS (kalkhuvtt.KALKNR,kalkfaktorertt.KALKNR,kalkhuvtt.OMRADE,kalkfaktorertt.OMRADE)
 /*EXTRA KLOGSUBID*/
DATA-RELATION huvEgnaDR FOR kalkhuvtt, kalkegnaprisertt RELATION-FIELDS (kalkhuvtt.KALKNR,kalkegnaprisertt.KALKNR,kalkhuvtt.OMRADE,kalkegnaprisertt.OMRADE)
DATA-RELATION huvMtrlDR FOR kalkhuvtt, kalktmtrlTT RELATION-FIELDS (kalkhuvtt.KALKNR,kalktmtrlTT.KALKNR,kalkhuvtt.OMRADE,kalktmtrlTT.OMRADE)
DATA-RELATION huvTidlDR FOR kalkhuvtt, kalkttidlageTT RELATION-FIELDS (kalkhuvtt.KALKNR,kalkttidlageTT.KALKNR,kalkhuvtt.OMRADE,kalkttidlageTT.OMRADE).

/*
DEFINE DATASET KalkylDS FOR kalkhuvtt,kalkaonrTT,kalkfaktorertt,kalkegnaprisertt,kalktmtrlTT,kalkttidlageTT
DATA-RELATION huvAonrDR FOR kalkhuvtt, kalkaonrTT RELATION-FIELDS (kalkhuvtt.KALKNR,kalkaonrTT.KALKNR,kalkhuvtt.OMRADE,kalkaonrTT.OMRADE)
 /*EXTRA KLOGSUBID*/
DATA-RELATION huvFaktDR FOR kalkhuvtt, kalkfaktorertt RELATION-FIELDS (kalkhuvtt.KALKNR,kalkfaktorertt.KALKNR,kalkhuvtt.OMRADE,kalkfaktorertt.OMRADE)
 /*EXTRA KLOGSUBID*/
DATA-RELATION huvEgnaDR FOR kalkhuvtt, kalkegnaprisertt RELATION-FIELDS (kalkhuvtt.KALKNR,kalkegnaprisertt.KALKNR,kalkhuvtt.OMRADE,kalkegnaprisertt.OMRADE)
DATA-RELATION huvMtrlDR FOR kalkhuvtt, kalktmtrlTT RELATION-FIELDS (kalkhuvtt.KALKNR,kalktmtrlTT.KALKNR,kalkhuvtt.OMRADE,kalktmtrlTT.OMRADE)
DATA-RELATION huvTidlDR FOR kalkhuvtt, kalkttidlageTT RELATION-FIELDS (kalkhuvtt.KALKNR,kalkttidlageTT.KALKNR,kalkhuvtt.OMRADE,kalkttidlageTT.OMRADE).
*/


/*
DEFINE DATASET KalkylDS FOR kalkhuvtt,kalknumtt,kalknumsubtt,kalkaonrTT,kalkfaktorertt,kalkegnaprisertt,kalktmtrlTT,kalkttidlageTT
DATA-RELATION huvNumsDR FOR kalkhuvtt, kalknumtt RELATION-FIELDS (kalkhuvtt.KALKNR,kalknumtt.KALKNR,kalkhuvtt.OMRADE,kalknumtt.OMRADE)
DATA-RELATION numSubsDR FOR kalknumtt, kalknumsubtt RELATION-FIELDS (kalknumtt.KALKNR,kalknumsubtt.KALKNR,kalknumtt.OMRADE,kalknumsubtt.OMRADE,kalknumtt.NUM,kalknumsubtt.NUM)
DATA-RELATION huvAonrDR FOR kalkhuvtt, kalkaonrTT RELATION-FIELDS (kalkhuvtt.KALKNR,kalkaonrTT.KALKNR,kalkhuvtt.OMRADE,kalkaonrTT.OMRADE)
 /*EXTRA KLOGSUBID*/
DATA-RELATION huvFaktDR FOR kalkhuvtt, kalkfaktorertt RELATION-FIELDS (kalkhuvtt.KALKNR,kalkfaktorertt.KALKNR,kalkhuvtt.OMRADE,kalkfaktorertt.OMRADE)
 /*EXTRA KLOGSUBID*/
DATA-RELATION huvEgnaDR FOR kalkhuvtt, kalkegnaprisertt RELATION-FIELDS (kalkhuvtt.KALKNR,kalkegnaprisertt.KALKNR,kalkhuvtt.OMRADE,kalkegnaprisertt.OMRADE)
DATA-RELATION huvMtrlDR FOR kalkhuvtt, kalktmtrlTT RELATION-FIELDS (kalkhuvtt.KALKNR,kalktmtrlTT.KALKNR,kalkhuvtt.OMRADE,kalktmtrlTT.OMRADE)
DATA-RELATION huvTidlDR FOR kalkhuvtt, kalkttidlageTT RELATION-FIELDS (kalkhuvtt.KALKNR,kalkttidlageTT.KALKNR,kalkhuvtt.OMRADE,kalkttidlageTT.OMRADE).

/*
PREFER-DATASET 
DEFINE DATA-SOURCE
*/
*/ 