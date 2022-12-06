/*KALKYLKATPRODATA.i*/
DEFINE DATASET KalkylKatDS FOR kalkylkatalogtt,kalkylkatalogsubtt,kalkylprisertt,kalkvisningtt,kalkanvtt
DATA-RELATION AnvDR FOR kalkylkatalogtt, kalkanvtt RELATION-FIELDS (kalkylkatalogtt.KLOGID,kalkanvtt.KLOGID).

DEFINE DATASET KalkylKoderDS FOR kalkylarbkodertt.

DEFINE DATASET KalkylLoparDS FOR kalkylloppostertt,kalkyllopsubtt
DATA-RELATION LopSubsDR FOR kalkylloppostertt, kalkyllopsubtt RELATION-FIELDS (kalkylloppostertt.KLOGSUBID,kalkyllopsubtt.KLOGSUBID,kalkylloppostertt.ARBKOD,kalkyllopsubtt.ARBKOD,kalkylloppostertt.LOPNR,kalkyllopsubtt.LOPNR).

DEFINE DATASET KalkylFrekDS FOR frekvenstemp.

DEFINE DATASET KalkylMallarDS FOR KalkmallHuvudtt,KalkmallKodertt
DATA-RELATION LopSubsDR FOR KalkmallHuvudtt, KalkmallKodertt RELATION-FIELDS (KalkmallHuvudtt.MALLNR,KalkmallKodertt.MALLNR).


/*
DATA-RELATION KatDR FOR kalkylkatalogtt, kalkylkatalogsubtt RELATION-FIELDS (kalkylkatalogtt.KLOGID,kalkylkatalogsubtt.KLOGID)
*/
/*
DATA-RELATION SubsDR FOR kalkylkatalogsubtt, kalkylprisertt RELATION-FIELDS (kalkylkatalogsubtt.KLOGSUBID,kalkylprisertt.KLOGSUBID)
*/
/*
DATA-RELATION PrisDR FOR kalkylprisertt, kalkvisningtt RELATION-FIELDS (kalkylprisertt.KVID,kalkvisningtt.KVID)
*/

/*KalkylKoderDS
DEFINE DATASET KalkylKoderDS FOR kalkylarbkodertt,kalkylloppostertt,kalkyllopsubtt
DATA-RELATION KoderDR FOR kalkylarbkodertt, kalkylloppostertt RELATION-FIELDS (kalkylarbkodertt.KLOGSUBID,kalkylloppostertt.KLOGSUBID,kalkylarbkodertt.ARBKOD,kalkylloppostertt.ARBKOD)
DATA-RELATION LopSubsDR FOR kalkylloppostertt, kalkyllopsubtt RELATION-FIELDS (kalkylloppostertt.KLOGSUBID,kalkyllopsubtt.KLOGSUBID,kalkylloppostertt.ARBKOD,kalkyllopsubtt.ARBKOD,kalkylloppostertt.LOPNR,kalkyllopsubtt.LOPNR).
*/