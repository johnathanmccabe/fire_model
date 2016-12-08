SELECT
	dbo_accgrp.ACCGRPID
	, dbo_accgrp.ACCGRPNUM
	, dbo_accgrp.ACCGRPNAME
	, dbo_Property.LOCID
	, dbo_Property.LOCNUM
	, dbo_Property.LOCNAME
	, dbo_Loc.STREETNAME
	, dbo_Loc.POSTALCODE
	, dbo_Loc.CITY
	, dbo_Loc.COUNTY
	, dbo_Loc.STATECODE
	, dbo_Loc.CNTRYSCHEME
	, dbo_Loc.CNTRYCODE
	, dbo_Loc.BLDGSCHEME
	, dbo_Loc.BLDGCLASS
	, dbo_Loc.OCCSCHEME
	, dbo_Loc.OCCTYPE
	, dbo_Loc.YEARBUILT
	, dbo_Loc.NUMBLDGS
	, dbo_Loc.USERID2 as PROTCLASS
	, dbo_FrDet.SPRINKLER
	, dbo_LocTIV.TIV
	, Sum(CASE dbo_loccvg.LOSSTYPE WHEN 1 THEN dbo_loccvg.VALUEAMT ELSE 0 END) AS BUILDINGS
	, Sum(CASE dbo_loccvg.LOSSTYPE WHEN 2 THEN dbo_loccvg.VALUEAMT ELSE 0 END) AS CONTENTS
	, Sum(CASE dbo_loccvg.LOSSTYPE WHEN 3 THEN dbo_loccvg.VALUEAMT ELSE 0 END) AS BI
	, dbo_loccvg.VALUECUR AS [CURRENCY]
FROM [&db&].[dbo].[accgrp] dbo_accgrp
INNER JOIN [&db&].[dbo].[Property] dbo_Property ON dbo_accgrp.ACCGRPID = dbo_Property.ACCGRPID
INNER JOIN [&db&].[dbo].[Loc] dbo_Loc ON dbo_Property.LOCID = dbo_Loc.LOCID
INNER JOIN [&db&].[dbo].[loccvg] dbo_loccvg ON dbo_Property.LOCID = dbo_loccvg.LOCID
INNER JOIN [&db&].[dbo].[LocTIV] dbo_LocTIV ON dbo_Property.LOCID = dbo_LocTIV.LocId
INNER JOIN [&db&].[dbo].[frdet] dbo_FrDet on dbo_FrDet.LOCID = dbo_Loc.LOCID
WHERE dbo_accgrp.ACCGRPNUM ='&account&' AND dbo_loccvg.PERIL=&peril&
GROUP BY
	dbo_accgrp.ACCGRPID
	, dbo_accgrp.ACCGRPNUM
	, dbo_accgrp.ACCGRPNAME
	, dbo_Property.LOCID
	, dbo_Property.LOCNUM
	, dbo_Property.LOCNAME
	, dbo_Loc.STREETNAME
	, dbo_Loc.POSTALCODE
	, dbo_Loc.CITY
	, dbo_Loc.COUNTY
	, dbo_Loc.STATECODE
	, dbo_Loc.CNTRYSCHEME
	, dbo_Loc.CNTRYCODE
	, dbo_Loc.BLDGSCHEME
	, dbo_Loc.BLDGCLASS
	, dbo_Loc.OCCSCHEME
	, dbo_Loc.OCCTYPE
	, dbo_Loc.YEARBUILT
	, dbo_Loc.NUMBLDGS
	, dbo_Loc.USERID2
	, dbo_FrDet.SPRINKLER
	, dbo_LocTIV.TIV
	, dbo_loccvg.VALUECUR
	, dbo_loccvg.PERIL
