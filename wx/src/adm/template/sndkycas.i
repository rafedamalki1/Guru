/* sndkycas.i - CASE statement for KEY link support */

    WHEN "{1}" THEN 
      pc_key-value = IF AVAILABLE {2} THEN STRING({2}.{3}) ELSE ?.
            
            
