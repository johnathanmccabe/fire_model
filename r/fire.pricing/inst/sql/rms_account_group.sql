SELECT
dbo_accgrp.ACCGRPID
, dbo_accgrp.ACCGRPNUM
, dbo_accgrp.ACCGRPNAME

FROM [&db&].[dbo].[accgrp] dbo_accgrp

WHERE dbo_accgrp.ACCGRPNUM LIKE '%_&sequel_id&_%'
