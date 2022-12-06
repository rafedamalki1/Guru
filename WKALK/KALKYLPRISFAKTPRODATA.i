/*KALKYLPRISFAKTPRODATA.i*/
DEFINE DATASET PrisFaktDS FOR kalkhuvtt,kalkfaktorertt,kalkegnaprisertt
DATA-RELATION PFFaktDR FOR kalkhuvtt, kalkfaktorertt RELATION-FIELDS (kalkhuvtt.KALKNR,kalkfaktorertt.KALKNR,kalkhuvtt.OMRADE,kalkfaktorertt.OMRADE)
DATA-RELATION PFEgnaDR FOR kalkhuvtt, kalkegnaprisertt RELATION-FIELDS (kalkhuvtt.KALKNR,kalkegnaprisertt.KALKNR,kalkhuvtt.OMRADE,kalkegnaprisertt.OMRADE).

