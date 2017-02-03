use [fire_model]

GO
IF OBJECT_ID('[curve_value]', 'U') IS NOT NULL 
DROP TABLE  [curve_value]

GO
IF OBJECT_ID('[curve_instance]', 'U') IS NOT NULL 
DROP TABLE  [curve_instance]

GO
IF OBJECT_ID('[curve]', 'U') IS NOT NULL 
DROP TABLE [curve]


-- Locations
GO
IF OBJECT_ID('[location_factor]', 'U') IS NOT NULL 
DROP TABLE  [location_factor]

GO
IF OBJECT_ID('[location]', 'U') IS NOT NULL 
DROP TABLE  [location]

-- Occupancies
GO
IF OBJECT_ID('[occupancy_type]', 'U') IS NOT NULL 
DROP TABLE  [occupancy_type]

GO
IF OBJECT_ID('[occupancy_factor]', 'U') IS NOT NULL 
DROP TABLE  [occupancy_factor]

GO
IF OBJECT_ID('[occupancy_scheme]', 'U') IS NOT NULL 
DROP TABLE  [occupancy_scheme]

GO
IF OBJECT_ID('[occupancy]', 'U') IS NOT NULL 
DROP TABLE  [occupancy]

GO
IF OBJECT_ID('[base_rate]', 'U') IS NOT NULL 
DROP TABLE  [base_rate]

GO
IF OBJECT_ID('[sir_adjustment]', 'U') IS NOT NULL 
DROP TABLE  [sir_adjustment]

GO
IF OBJECT_ID('[coverage]', 'U') IS NOT NULL 
DROP TABLE  [coverage]

GO
IF OBJECT_ID('[sprinkler]', 'U') IS NOT NULL 
DROP TABLE  [sprinkler]

-- this should be the last drop item
GO
IF OBJECT_ID('[assumption_set]', 'U') IS NOT NULL 
DROP TABLE  [assumption_set]
