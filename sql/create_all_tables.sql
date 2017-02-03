use [fire_model]

CREATE TABLE [curve] (
	curve_id integer NOT NULL IDENTITY(1,1),
	curve_name varchar(255) NOT NULL,
	display_name varchar(255) NOT NULL,
	created_dtm datetime NOT NULL,
	audit_dtm datetime NOT NULL,
	is_deleted varchar(1) NOT NULL,
	audit_credential varchar(40) NOT NULL,
	created_credential varchar(40) NOT NULL,
  CONSTRAINT [PK_CURVE] PRIMARY KEY CLUSTERED
  (
  [curve_id] ASC
  ) WITH (IGNORE_DUP_KEY = OFF)

)
GO
CREATE TABLE [curve_instance] (
	curve_instance_id integer NOT NULL IDENTITY(1,1),
	valid_fom_date datetime NOT NULL,
	valid_to_date datetime NOT NULL,
	is_superseded varchar NOT NULL,
	is_deleted varchar NOT NULL,
	curve_id integer NOT NULL,
	audit_dtm datetime NOT NULL,
	audit_credential varchar(40) NOT NULL,
  CONSTRAINT [PK_CURVE_INSTANCE] PRIMARY KEY CLUSTERED
  (
  [curve_instance_id] ASC
  ) WITH (IGNORE_DUP_KEY = OFF)

)
GO
CREATE TABLE [curve_value] (
	curve_value_id integer NOT NULL IDENTITY(1,1),
	curve_instance_id integer NOT NULL,
	tiv_percentage float NOT NULL,
	lev_percentage float NOT NULL,
	audit_dtm datetime NOT NULL,
	audit_credential varchar(40) NOT NULL,
  CONSTRAINT [PK_CURVE_VALUE] PRIMARY KEY CLUSTERED
  (
  [curve_value_id] ASC
  ) WITH (IGNORE_DUP_KEY = OFF)

)
GO
CREATE TABLE [location_factor] (
	location_factor_id integer NOT NULL IDENTITY(1,1),
	location_code varchar(25) NOT NULL,
	factor float NOT NULL,
	audit_dtm datetime NOT NULL,
	audit_credential varchar(40) NOT NULL,
	coverage_code varchar(255) NOT NULL,
	assumption_set_id integer NOT NULL,
  CONSTRAINT [PK_LOCATION_FACTOR] PRIMARY KEY CLUSTERED
  (
  [location_factor_id] ASC
  ) WITH (IGNORE_DUP_KEY = OFF)

)
GO
CREATE TABLE [occupancy_factor] (
	occupancy_factor_id integer NOT NULL IDENTITY(1,1),
	occupancy_factor float NOT NULL,
	sprinkler_factor float NOT NULL,
	audit_dtm datetime NOT NULL,
	audit_credential varchar(40) NOT NULL,
	assumption_set_id integer NOT NULL,
	atc_code varchar(255) NOT NULL,
	coverage_code varchar(255) NOT NULL,
  CONSTRAINT [PK_OCCUPANCY_FACTOR] PRIMARY KEY CLUSTERED
  (
  [occupancy_factor_id] ASC
  ) WITH (IGNORE_DUP_KEY = OFF)

)
GO
CREATE TABLE [base_rate] (
	base_rate_id integer NOT NULL IDENTITY(1,1),
	base_rate float NOT NULL,
	coverage_code varchar(255) NOT NULL,
	audit_dtm datetime NOT NULL,
	audit_credential varchar(40) NOT NULL,
	assumption_set_id integer NOT NULL,
  CONSTRAINT [PK_BASE_RATE] PRIMARY KEY CLUSTERED
  (
  [base_rate_id] ASC
  ) WITH (IGNORE_DUP_KEY = OFF)

)
GO
CREATE TABLE [sir_adjustment] (
	sir_adjustment_id integer NOT NULL IDENTITY(1,1),
	tiv float NOT NULL,
	sir float NOT NULL,
	adjustment_factor float NOT NULL,
	audit_dtm datetime NOT NULL,
	audit_credential varchar(40) NOT NULL,
	assumption_set_id integer NOT NULL,
  CONSTRAINT [PK_SIR_ADJUSTMENT] PRIMARY KEY CLUSTERED
  (
  [sir_adjustment_id] ASC
  ) WITH (IGNORE_DUP_KEY = OFF)

)
GO
CREATE TABLE [occupancy] (
	is_residential varchar(1) NOT NULL,
	audit_dtm datetime NOT NULL,
	audit_credential varchar(40) NOT NULL,
	atc_code varchar(255) NOT NULL,
	occupancy_description varchar(255) NOT NULL,
	occupancy_group varchar(255) NOT NULL
  CONSTRAINT [PK_OCCUPANCY] PRIMARY KEY CLUSTERED
  (
  [atc_code] ASC
  ) WITH (IGNORE_DUP_KEY = OFF)

)
GO
CREATE TABLE [coverage] (
	coverage_code varchar(255) NOT NULL,
	description varchar(255) NOT NULL,
	audit_dtm datetime NOT NULL,
	audit_credential varchar(40) NOT NULL,
  CONSTRAINT [PK_COVERAGE] PRIMARY KEY CLUSTERED
  (
  [coverage_code] ASC
  ) WITH (IGNORE_DUP_KEY = OFF)

)
GO
CREATE TABLE [assumption_set] (
	assumption_set_id integer NOT NULL IDENTITY(1,1),
	description varchar(255) NOT NULL,
	audit_dtm datetime NOT NULL,
	audit_credential varchar(255) NOT NULL,
  CONSTRAINT [PK_ASSUMPTION_SET] PRIMARY KEY CLUSTERED
  (
  [assumption_set_id] ASC
  ) WITH (IGNORE_DUP_KEY = OFF)

)
GO
CREATE TABLE [location] (
	location_code varchar(25) NOT NULL,
	location_description varchar(255) NOT NULL,
	audit_dtm datetime NOT NULL,
	audit_credential varchar(40) NOT NULL,
  CONSTRAINT [PK_LOCATION] PRIMARY KEY CLUSTERED
  (
  [location_code] ASC
  ) WITH (IGNORE_DUP_KEY = OFF)

)
GO
CREATE TABLE [sprinkler] (
	sprinkler_id integer NOT NULL IDENTITY(1,1),
	sprinkler_description varchar(255) NOT NULL,
  CONSTRAINT [PK_SPRINKLER] PRIMARY KEY CLUSTERED
  (
  [sprinkler_id] ASC
  ) WITH (IGNORE_DUP_KEY = OFF)

)
GO
CREATE TABLE [occupancy_scheme] (
	occupancy_scheme varchar(255) NOT NULL,
	occupancy_code varchar(255) NOT NULL,
	atc_code varchar(255) NOT NULL,
	audit_dtm datetime NOT NULL,
	audit_credential varchar(40) NOT NULL
)
GO
CREATE TABLE [construction] (
	iso_code varchar(255) NOT NULL,
	description varchar(255) NOT NULL,
	audit_dtm datetime NOT NULL,
	audit_credential varchar(40) NOT NULL,
  CONSTRAINT [PK_CONSTRUCTION] PRIMARY KEY CLUSTERED
  (
  [iso_code] ASC
  ) WITH (IGNORE_DUP_KEY = OFF)

)
GO
CREATE TABLE [construction_scheme] (
	construction_scheme varchar(255) NOT NULL,
	construction_code varchar(255) NOT NULL,
	iso_code varchar(255) NOT NULL,
	audit_dtm datetime NOT NULL,
	audit_credential varchar(40) NOT NULL
)
GO
CREATE TABLE [construction_factor] (
	construction_factor float NOT NULL,
	iso_code varchar(255) NOT NULL,
	coverage_code varchar(255) NOT NULL,
	assumption_set_id integer NOT NULL,
	construction_factor_id integer NOT NULL IDENTITY(1,1),
	audit_dtm datetime NOT NULL,
	audit_credential varchar(255) NOT NULL,
  CONSTRAINT [PK_CONSTRUCTION_FACTOR] PRIMARY KEY CLUSTERED
  (
  [construction_factor_id] ASC
  ) WITH (IGNORE_DUP_KEY = OFF)

)

GO

ALTER TABLE [curve_instance] WITH CHECK ADD CONSTRAINT [curve_instance_fk0] FOREIGN KEY ([curve_id]) REFERENCES [curve]([curve_id])
ON UPDATE CASCADE ON DELETE CASCADE
GO
ALTER TABLE [curve_instance] CHECK CONSTRAINT [curve_instance_fk0]
GO

ALTER TABLE [curve_value] WITH CHECK ADD CONSTRAINT [curve_value_fk0] FOREIGN KEY ([curve_instance_id]) REFERENCES [curve_instance]([curve_instance_id])
ON UPDATE CASCADE ON DELETE CASCADE
GO
ALTER TABLE [curve_value] CHECK CONSTRAINT [curve_value_fk0]
GO

ALTER TABLE [location_factor] WITH CHECK ADD CONSTRAINT [location_factor_fk0] FOREIGN KEY ([location_code]) REFERENCES [location]([location_code])
ON UPDATE CASCADE ON DELETE CASCADE
GO
ALTER TABLE [location_factor] CHECK CONSTRAINT [location_factor_fk0]
GO
ALTER TABLE [location_factor] WITH CHECK ADD CONSTRAINT [location_factor_fk1] FOREIGN KEY ([coverage_code]) REFERENCES [coverage]([coverage_code])
ON UPDATE CASCADE ON DELETE CASCADE
GO
ALTER TABLE [location_factor] CHECK CONSTRAINT [location_factor_fk1]
GO
ALTER TABLE [location_factor] WITH CHECK ADD CONSTRAINT [location_factor_fk2] FOREIGN KEY ([assumption_set_id]) REFERENCES [assumption_set]([assumption_set_id])
ON UPDATE CASCADE ON DELETE CASCADE
GO
ALTER TABLE [location_factor] CHECK CONSTRAINT [location_factor_fk2]
GO

ALTER TABLE [occupancy_factor] WITH CHECK ADD CONSTRAINT [occupancy_factor_fk0] FOREIGN KEY ([assumption_set_id]) REFERENCES [assumption_set]([assumption_set_id])
ON UPDATE CASCADE ON DELETE CASCADE
GO
ALTER TABLE [occupancy_factor] CHECK CONSTRAINT [occupancy_factor_fk0]
GO
ALTER TABLE [occupancy_factor] WITH CHECK ADD CONSTRAINT [occupancy_factor_fk1] FOREIGN KEY ([atc_code]) REFERENCES [occupancy]([atc_code])
ON UPDATE CASCADE ON DELETE CASCADE
GO
ALTER TABLE [occupancy_factor] CHECK CONSTRAINT [occupancy_factor_fk1]
GO
ALTER TABLE [occupancy_factor] WITH CHECK ADD CONSTRAINT [occupancy_factor_fk2] FOREIGN KEY ([coverage_code]) REFERENCES [coverage]([coverage_code])
ON UPDATE CASCADE ON DELETE CASCADE
GO
ALTER TABLE [occupancy_factor] CHECK CONSTRAINT [occupancy_factor_fk2]
GO

ALTER TABLE [base_rate] WITH CHECK ADD CONSTRAINT [base_rate_fk0] FOREIGN KEY ([coverage_code]) REFERENCES [coverage]([coverage_code])
ON UPDATE CASCADE ON DELETE CASCADE
GO
ALTER TABLE [base_rate] CHECK CONSTRAINT [base_rate_fk0]
GO
ALTER TABLE [base_rate] WITH CHECK ADD CONSTRAINT [base_rate_fk1] FOREIGN KEY ([assumption_set_id]) REFERENCES [assumption_set]([assumption_set_id])
ON UPDATE CASCADE ON DELETE CASCADE
GO
ALTER TABLE [base_rate] CHECK CONSTRAINT [base_rate_fk1]
GO

ALTER TABLE [sir_adjustment] WITH CHECK ADD CONSTRAINT [sir_adjustment_fk0] FOREIGN KEY ([assumption_set_id]) REFERENCES [assumption_set]([assumption_set_id])
ON UPDATE CASCADE ON DELETE CASCADE
GO
ALTER TABLE [sir_adjustment] CHECK CONSTRAINT [sir_adjustment_fk0]
GO






ALTER TABLE [occupancy_scheme] WITH CHECK ADD CONSTRAINT [occupancy_scheme_fk0] FOREIGN KEY ([atc_code]) REFERENCES [occupancy]([atc_code])
ON UPDATE CASCADE ON DELETE CASCADE
GO
ALTER TABLE [occupancy_scheme] CHECK CONSTRAINT [occupancy_scheme_fk0]
GO

ALTER TABLE [construction_scheme] WITH CHECK ADD CONSTRAINT [construction_scheme_fk0] FOREIGN KEY ([iso_code]) REFERENCES [construction]([iso_code])
ON UPDATE CASCADE
GO
ALTER TABLE [construction_scheme] CHECK CONSTRAINT [construction_scheme_fk0]
GO

-- construction tables foreign keys
ALTER TABLE [construction_factor] WITH CHECK ADD CONSTRAINT [construction_factor_fk0] FOREIGN KEY ([iso_code]) REFERENCES [construction]([iso_code])
ON UPDATE CASCADE
GO
ALTER TABLE [construction_factor] CHECK CONSTRAINT [construction_factor_fk0]
GO
ALTER TABLE [construction_factor] WITH CHECK ADD CONSTRAINT [construction_factor_fk1] FOREIGN KEY ([coverage_code]) REFERENCES [coverage]([coverage_code])
ON UPDATE CASCADE
GO
ALTER TABLE [construction_factor] CHECK CONSTRAINT [construction_factor_fk1]
GO
ALTER TABLE [construction_factor] WITH CHECK ADD CONSTRAINT [construction_factor_fk2] FOREIGN KEY ([assumption_set_id]) REFERENCES [assumption_set]([assumption_set_id])
ON UPDATE CASCADE
GO
ALTER TABLE [construction_factor] CHECK CONSTRAINT [construction_factor_fk2]
GO



-- Load coverage codes

INSERT INTO dbo.coverage (coverage_code, description, audit_dtm, audit_credential) 
	VALUES ('b', 'Buildings', GETDATE(), 'jmccabe')
INSERT INTO dbo.coverage (coverage_code, description, audit_dtm, audit_credential) 
	VALUES ('c', 'Contents', GETDATE(), 'jmccabe')
INSERT INTO dbo.coverage (coverage_code, description, audit_dtm, audit_credential) 
	VALUES ('bi', 'Business Interruption', GETDATE(), 'jmccabe')