/* r-setsiz.p */

DEFINE VARIABLE ElipRegion AS MEMPTR.

SET-SIZE(ElipRegion) = 8.
PUT-SHORT(ElipRegion, 1) = 10.
PUT-SHORT(ElipRegion, 3) = 10.
PUT-SHORT(ElipRegion, 5) = 200.
PUT-SHORT(ElipRegion, 7) = 50.

