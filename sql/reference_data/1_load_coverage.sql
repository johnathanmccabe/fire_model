--#load coverage codes
use [fire_model]

INSERT INTO dbo.coverage (coverage_code, description, audit_dtm, audit_credential) 
	VALUES ('b', 'Buildings', GETDATE(), 'jmccabe')
INSERT INTO dbo.coverage (coverage_code, description, audit_dtm, audit_credential) 
	VALUES ('c', 'Contents', GETDATE(), 'jmccabe')
INSERT INTO dbo.coverage (coverage_code, description, audit_dtm, audit_credential) 
	VALUES ('bi', 'Business Interruption', GETDATE(), 'jmccabe')