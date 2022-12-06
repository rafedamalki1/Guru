/* TIMSEK.P Omvandlar tiden i timmar och minuter till sekunder */
DEFINE SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE SHARED VARIABLE sekunder AS INTEGER FORMAT "-9999999" NO-UNDO. 

sekunder = TRUNCATE(nytid,0) * 3600 + (nytid - TRUNCATE(nytid,0)) * 100 * 60.
