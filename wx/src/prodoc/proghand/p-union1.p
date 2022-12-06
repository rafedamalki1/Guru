/* p-union1.p */

SELECT name, state FROM customer WHERE state = "MA"
 UNION
SELECT rep-name, region FROM salesrep WHERE region = "East".
