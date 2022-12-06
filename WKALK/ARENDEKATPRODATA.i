/*ARENDEKATPRODATA.i*/

DEFINE DATASET KalkylMallarDS FOR KalkmallHuvudtt,KalkmallKodertt
DATA-RELATION LopSubsDR FOR KalkmallHuvudtt, KalkmallKodertt RELATION-FIELDS (KalkmallHuvudtt.MALLNR,KalkmallKodertt.MALLNR).