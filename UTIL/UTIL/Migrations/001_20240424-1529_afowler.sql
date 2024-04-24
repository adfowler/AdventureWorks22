-- <Migration ID="53ce4c50-3ef1-449c-ad48-436610430cac" />
GO

IF NOT EXISTS (SELECT * FROM master.dbo.syslogins WHERE loginname = N'MSRA\Everyone')
CREATE LOGIN [MSRA\Everyone] FROM WINDOWS
GO
CREATE USER [Everyone] FOR LOGIN [MSRA\Everyone]
GO
DECLARE @associate bit
SELECT @associate = CASE SERVERPROPERTY('EngineEdition') WHEN 5 THEN 1 ELSE 0 END
IF @associate = 0 EXEC sp_executesql N'SELECT @count = COUNT(*) FROM master.dbo.syslogins WHERE loginname = N''ImpervaAcct''', N'@count bit OUT', @associate OUT
IF @associate = 1
BEGIN
    PRINT N'Creating user [ImpervaAcct] and mapping to the login [ImpervaAcct]'
    CREATE USER [ImpervaAcct] FOR LOGIN [ImpervaAcct]
END
ELSE
BEGIN
    PRINT N'Creating user [ImpervaAcct] without login'
    CREATE USER [ImpervaAcct] WITHOUT LOGIN
END
GO
CREATE USER [IntranetHRIS_PRMLogin] WITHOUT LOGIN
GO
IF NOT EXISTS (SELECT * FROM master.dbo.syslogins WHERE loginname = N'MSRA\IS')
CREATE LOGIN [MSRA\IS] FROM WINDOWS
GO
CREATE USER [MSRA\IS] FOR LOGIN [MSRA\IS]
GO
IF NOT EXISTS (SELECT * FROM master.dbo.syslogins WHERE loginname = N'MSRA\sqlservice')
CREATE LOGIN [MSRA\sqlservice] FROM WINDOWS
GO
CREATE USER [MSRA\sqlservice] FOR LOGIN [MSRA\sqlservice]
GO
DECLARE @associate bit
SELECT @associate = CASE SERVERPROPERTY('EngineEdition') WHEN 5 THEN 1 ELSE 0 END
IF @associate = 0 EXEC sp_executesql N'SELECT @count = COUNT(*) FROM master.dbo.syslogins WHERE loginname = N''mpaslinkservice''', N'@count bit OUT', @associate OUT
IF @associate = 1
BEGIN
    PRINT N'Creating user [mpaslinkservice] and mapping to the login [mpaslinkservice]'
    CREATE USER [mpaslinkservice] FOR LOGIN [mpaslinkservice]
END
ELSE
BEGIN
    PRINT N'Creating user [mpaslinkservice] without login'
    CREATE USER [mpaslinkservice] WITHOUT LOGIN
END
GO
PRINT N'Creating role AllUsers'
GO
CREATE ROLE [AllUsers]
AUTHORIZATION [dbo]
GO
PRINT N'Creating role ExternalUsers'
GO
CREATE ROLE [ExternalUsers]
AUTHORIZATION [dbo]
GO
PRINT N'Creating role LocalUsers'
GO
CREATE ROLE [LocalUsers]
AUTHORIZATION [dbo]
GO
PRINT N'Altering members of role ExternalUsers'
GO
EXEC sp_addrolemember N'ExternalUsers', N'mpaslinkservice'
GO
PRINT N'Altering members of role LocalUsers'
GO
EXEC sp_addrolemember N'LocalUsers', N'mpaslinkservice'
GO
EXEC sp_addrolemember N'LocalUsers', N'MSRA\IS'
GO
PRINT N'Creating schemas'
GO
CREATE SCHEMA [Everyone]
AUTHORIZATION [Everyone]
GO
PRINT N'Creating [dbo].[IsLeapYear]'
GO
SET QUOTED_IDENTIFIER OFF
GO
SET ANSI_NULLS OFF
GO
/*
* ==============================================================================

Syntax

Arguments

Return Types
* ==============================================================================
*/
CREATE FUNCTION [dbo].[IsLeapYear] (@pDate    VARCHAR(4) )
RETURNS BIT
AS
BEGIN

    IF  @pDate % 4 = 0 AND @pDate % 100 != 0 OR @pDate  % 400 = 0
        RETURN 1

    RETURN 0

END
GO
PRINT N'Creating [dbo].[DecimalDateTimeToDateTime]'
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_NULLS ON
GO
/*
* ==============================================================================

Syntax

Arguments

Return Types
* ==============================================================================
*/
CREATE FUNCTION [dbo].[DecimalDateTimeToDateTime] (@inDate DECIMAL, @inTime DECIMAL)  
RETURNS DATETIME AS  
BEGIN 
DECLARE @tempval VARCHAR(20)
DECLARE @return DATETIME

DECLARE @day VARCHAR(2)
DECLARE @month VARCHAR(2)
DECLARE @year VARCHAR(4)

DECLARE @hours VARCHAR(2)
DECLARE @mins VARCHAR(2)
DECLARE @secs VARCHAR(2)

IF LEN (@inTime) >= 5
BEGIN
	SET @hours = LEFT(@inTime, LEN(@inTime) - 4)
	SET @mins = SUBSTRING(RIGHT(@inTime, 4), 1, 2)
	SET @secs= RIGHT(@inTime, 2)
END

ELSE
BEGIN
	SET @hours = '12'
	SET @mins = '30'
	SET @secs= '00'

END


SET @day = SUBSTRING(cast(@inDate as varchar), 7,2) 
SET @month = SUBSTRING(cast(@inDate as varchar), 5,2) 
SET @year = SUBSTRING(cast(@inDate as varchar), 1,4) 

IF @month > 12 	SET @month = 12
IF @month < 1	SET @month = 1

IF @day > 31 SET @day = 28
IF @day < 1 SET @day = 1
IF dbo.IsLeapYear (@year) = 0  AND @month = 2 AND @day > 28 SET @day = 28

IF LEN(@year) < 4 SET @year = 2000
IF @year > 2005 SET @year = 2000


SELECT @tempval =  @month  + '/' + @day + '/'+ @year  +'  ' + @hours  + ':' + @mins + ':'+ @secs

--SELECT @tempval =  SUBSTRING(cast(@inDate as varchar), 5,2) + '/' + SUBSTRING(cast(@inDate as varchar), 7,2) + '/'+ SUBSTRING(cast(@inDate as varchar), 1,4) 

SELECT @Return = CAST(@tempval AS DATETIME)

RETURN @Return
END
GO
PRINT N'Creating [dbo].[DecimalToDate]'
GO
SET QUOTED_IDENTIFIER OFF
GO
/*
* ==============================================================================

Syntax

Arguments

Return Types
* ==============================================================================
*/
CREATE FUNCTION [dbo].[DecimalToDate] (@inDate DECIMAL)  
RETURNS DATETIME AS  
BEGIN 
DECLARE @tempval VARCHAR(10)
DECLARE @return DATETIME

DECLARE @day VARCHAR(2)
DECLARE @month VARCHAR(2)
DECLARE @year VARCHAR(4)

SET @day = SUBSTRING(cast(@inDate as varchar), 7,2) 
SET @month = SUBSTRING(cast(@inDate as varchar), 5,2) 
SET @year = SUBSTRING(cast(@inDate as varchar), 1,4) 

IF @month > 12 	SET @month = 12
IF @month < 1	SET @month = 1

IF @day > 31 SET @day = 28
IF @day < 1 SET @day = 1
IF dbo.IsLeapYear (@year) = 0  AND @month = 2 AND @day > 28 SET @day = 28

IF LEN(@year) < 4 SET @year = 2000
IF @year > 2005 SET @year = 2000


SELECT @tempval =  @month  + '/' + @day + '/'+ @year

--SELECT @tempval =  SUBSTRING(cast(@inDate as varchar), 5,2) + '/' + SUBSTRING(cast(@inDate as varchar), 7,2) + '/'+ SUBSTRING(cast(@inDate as varchar), 1,4) 

SELECT @Return = CAST(@tempval AS DATETIME)

RETURN @Return
END
GO
PRINT N'Creating [dbo].[FixDDMMYYToDate]'
GO
SET QUOTED_IDENTIFIER ON
GO
/*
* ==============================================================================

Syntax

Arguments

Return Types
* ==============================================================================
*/
CREATE FUNCTION [dbo].[FixDDMMYYToDate] (@inDate VARCHAR(8))  
RETURNS VARCHAR(8) AS  
BEGIN 
DECLARE @tempval VARCHAR(10)
DECLARE @return VARCHAR(8)

IF LEN(@inDate) > 0
BEGIN
	DECLARE @day VARCHAR(2)
	DECLARE @month VARCHAR(2)
	DECLARE @year VARCHAR(4)

	SET @month = SUBSTRING(cast(@inDate as varchar), 0,3) 
	SET @day = SUBSTRING(cast(@inDate as varchar), 3,2) 
	SET @year = SUBSTRING(cast(@inDate as varchar), 5,2) 

	IF @month > 12 	SET @month = 12
	IF @month < 1	SET @month = 1

	IF @day > 31 SET @day = 28
	IF @day < 1 SET @day = 1
	IF dbo.IsLeapYear (@year) = 0  AND @month = 2 AND @day > 28 SET @day = 28

	SET @year = SUBSTRING(cast(@inDate as varchar), 5,2) 

	--IF @year <= 50 SET @year = '19'+ @year
	SET @tempval =  @month  + '/' + @day + '/'+ @year
END
ELSE
BEGIN
	SET @tempval =  NULL
END

--SELECT @month +'/'+ @day +'/'+ @year 

--SET @Return = CAST(@tempval AS DATETIME)

--RETURN @Return
return @tempval
END
GO
PRINT N'Creating [dbo].[FixDDMMYYToStringDate]'
GO
/*
* ==============================================================================

Syntax

Arguments

Return Types
* ==============================================================================
*/
CREATE FUNCTION [dbo].[FixDDMMYYToStringDate] (@inDate VARCHAR(8))  
RETURNS VARCHAR(10) AS  
BEGIN 
DECLARE @tempval VARCHAR(10)
DECLARE @return VARCHAR(10)

IF LEN(@inDate) > 0
BEGIN
	DECLARE @day VARCHAR(2)
	DECLARE @month VARCHAR(2)
	DECLARE @year VARCHAR(4)

	SET @month = SUBSTRING(cast(@inDate as varchar), 0,3) 
	SET @day = SUBSTRING(cast(@inDate as varchar), 3,2) 
	SET @year = SUBSTRING(cast(@inDate as varchar), 5,2) 

	IF @month > 12 	SET @month = 12
	IF @month < 1	SET @month = 1

	IF @day > 31 SET @day = 28
	IF @day < 1 SET @day = 1
	IF dbo.IsLeapYear (@year) = 0  AND @month = 2 AND @day > 28 SET @day = 28

	SET @year = SUBSTRING(cast(@inDate as varchar), 5,2) 

	IF @year > 5 SET @year = '19'+ @year
	SET @Return =  @month  + '/' + @day + '/'+ @year
END
ELSE
BEGIN
	SET @Return =  NULL
END

IF @inDate = '000000'
	BEGIN
		SET @Return =  NULL
	END

--SELECT @month +'/'+ @day +'/'+ @year 
--SET @Return = CAST(@tempval AS DATETIME)
--return @tempval

RETURN @Return
END
GO
PRINT N'Creating [dbo].[FixDDMMYYToStringDate19]'
GO
/*
* ==============================================================================

Syntax

Arguments

Return Types
* ==============================================================================
*/
CREATE FUNCTION [dbo].[FixDDMMYYToStringDate19] (@inDate VARCHAR(8))  
RETURNS VARCHAR(10) AS  
BEGIN 
DECLARE @tempval VARCHAR(10)
DECLARE @return VARCHAR(10)

IF LEN(@inDate) > 0
BEGIN
	DECLARE @day VARCHAR(2)
	DECLARE @month VARCHAR(2)
	DECLARE @year VARCHAR(4)

	SET @month = SUBSTRING(cast(@inDate as varchar), 0,3) 
	SET @day = SUBSTRING(cast(@inDate as varchar), 3,2) 
	SET @year = SUBSTRING(cast(@inDate as varchar), 5,2) 

	IF @month > 12 	SET @month = 12
	IF @month < 1	SET @month = 1

	IF @day > 31 SET @day = 28
	IF @day < 1 SET @day = 1
	IF dbo.IsLeapYear (@year) = 0  AND @month = 2 AND @day > 28 SET @day = 28

	SET @year = SUBSTRING(cast(@inDate as varchar), 5,2) 

	--IF @year > 5 SET @year = '19'+ @year
	SET @year = '19'+ @year
	SET @Return =  @month  + '/' + @day + '/'+ @year
END
ELSE
BEGIN
	SET @Return =  NULL
END

IF @inDate = '000000'
	BEGIN
		SET @Return =  NULL
	END

--SELECT @month +'/'+ @day +'/'+ @year 
--SET @Return = CAST(@tempval AS DATETIME)
--return @tempval

RETURN @Return
END
GO
PRINT N'Creating [dbo].[FixYYMMDDToStringDate]'
GO
/*
* ==============================================================================

Syntax

Arguments

Return Types
* ==============================================================================
*/
CREATE FUNCTION [dbo].[FixYYMMDDToStringDate] (@inDate VARCHAR(8))  
RETURNS VARCHAR(10) AS  
BEGIN 
DECLARE @tempval VARCHAR(10)
DECLARE @return VARCHAR(10)

IF LEN(@inDate) > 0
BEGIN
	DECLARE @day VARCHAR(2)
	DECLARE @month VARCHAR(2)
	DECLARE @year VARCHAR(4)

	SET @year = SUBSTRING(cast(@inDate as varchar), 0,3) 
	SET @month = SUBSTRING(cast(@inDate as varchar), 3,2) 
	SET @day = SUBSTRING(cast(@inDate as varchar), 5,2) 

	IF @month > 12 	SET @month = 12
	IF @month < 1	SET @month = 1

	IF @day > 31 SET @day = 28
	IF @day < 1 SET @day = 1
	IF dbo.IsLeapYear (@year) = 0  AND @month = 2 AND @day > 28 SET @day = 28

	--SET @year = SUBSTRING(cast(@inDate as varchar), 5,2) 

	--IF @year > 5 SET @year = '19'+ @year
	SET @Return =  @month  + '/' + @day + '/'+ @year
END
ELSE
BEGIN
	SET @Return =  NULL
END

IF @inDate = '000000'
	BEGIN
		SET @Return =  NULL
	END

--SELECT @month +'/'+ @day +'/'+ @year 
--SET @Return = CAST(@tempval AS DATETIME)
--return @tempval

RETURN @Return
END
GO
PRINT N'Creating [dbo].[tblAdminEMail]'
GO
CREATE TABLE [dbo].[tblAdminEMail]
(
[ID] [int] NOT NULL,
[AdminEmail] [varchar] (128) NOT NULL
)
GO
PRINT N'Creating primary key [PK_tblAdminEMail] on [dbo].[tblAdminEMail]'
GO
ALTER TABLE [dbo].[tblAdminEMail] ADD CONSTRAINT [PK_tblAdminEMail] PRIMARY KEY CLUSTERED  ([ID])
GO
PRINT N'Creating [dbo].[spr_DBAdmin_Script]'
GO
CREATE PROCEDURE [dbo].[spr_DBAdmin_Script]

AS
	BEGIN	-- Start of the spr_DBAdmin_Script Stored Procedure.
		SET NOCOUNT ON;
		
		DECLARE @Local_Exec												nvarchar(4000),
				@Local_ErrorMessage										nvarchar(4000),
				@Local_ErrorSeverity									int,
				@Local_ErrorState										int,
				@Local_ListOfDatabasesDBAdmin_DatabaseName				nvarchar(256),
				@Local_PrimaryKeyID										int,
				@Local_Email_To_Send_To									varchar(8000),
				@Local_EMailBody										nvarchar(512),
				@LOCAL_PROFILE_NAME										varchar(255),
				@Local_start_time_of_sp									datetime,
				@Local_end_time_of_sp									datetime
	
	SET @Local_start_time_of_sp = getdate() -- Timing the start of the SP.

	-- Populate GLOBAL variables.	
	SET @LOCAL_PROFILE_NAME = 'SQLMail'
	SELECT @Local_Email_To_Send_To = COALESCE(@Local_Email_To_Send_To + ';', '') + AdminEmail 
	FROM UTIL.dbo.tblAdminEMail	


		-- Dropping Temp Table.
	IF EXISTS (
		SELECT  *
		FROM 	tempdb.dbo.sysobjects o
		WHERE
			o.xtype in ('U')	and
			o.id = object_id( N'tempdb..'+'#ListOfDatabasesDBAdmin')
		)
		BEGIN
			DROP TABLE #ListOfDatabasesFullBackup
		END
	-- Creating the Temp Table.
	CREATE TABLE #ListOfDatabasesDBAdmin(
			PrimaryKeyID							int identity (1, 1),			
			ListOfDatabasesDBAdmin_DatabaseName		nvarchar(256)
		)


	-- Populating the #ListOfDatabases temp table with a list
	-- of databases.
	INSERT INTO #ListOfDatabasesDBAdmin(ListOfDatabasesDBAdmin_DatabaseName)	
	
	SELECT name FROM master.sys.databases
	WHERE	
			state_desc = 'ONLINE' AND
			is_read_only = 0 AND
			--name  IN('MSRA_DROP')
	name NOT IN('tempdb', 'model', 'msdb', 'master','SharePoint_AdminContent_13d878f2-85b5-4256-9986-25be315c5173', 'SharePoint_AdminContent_87ff83e3-b85a-48e6-afc1-54df36806b03')
	ORDER BY name ASC		

	SELECT @Local_PrimaryKeyID = MIN(PrimaryKeyID)
	FROM #ListOfDatabasesDBAdmin
	WHILE @Local_PrimaryKeyID IS NOT NULL

	BEGIN -- WHILE @Local_PrimaryKeyID IS NOT NULL
		SELECT	@Local_ListOfDatabasesDBAdmin_DatabaseName		= NULL

		SELECT 	@Local_ListOfDatabasesDBAdmin_DatabaseName	= ListOfDatabasesDBAdmin_DatabaseName
				
		FROM #ListOfDatabasesDBAdmin
		WHERE PrimaryKeyID = @Local_PrimaryKeyID
		
		
		BEGIN TRY				
				Print 'Now Processing: ' +  @Local_ListOfDatabasesDBAdmin_DatabaseName +  CHAR(10) + CHAR(13) +  CHAR(10) + CHAR(13)

				--SELECT @Local_Exec = 'USE ' + @Local_ListOfDatabasesDBAdmin_DatabaseName + ' DBCC CHECKDB'
				SELECT @Local_Exec = 'USE ' + @Local_ListOfDatabasesDBAdmin_DatabaseName + ' DBCC CHECKDB(' + @Local_ListOfDatabasesDBAdmin_DatabaseName + ') WITH ALL_ERRORMSGS, NO_INFOMSGS'
				
				Print 'Starting CHECKDB' + CHAR(10) + CHAR(13)
				Exec sp_executesql @Local_Exec -- Run the Command.
				Print CHAR(10) + CHAR(13) + 'Finished CHECKDB' + CHAR(10) + CHAR(13) + CHAR(10) + CHAR(13)
				
				Print 'Starting CHECKCATALOG' + CHAR(10) + CHAR(13)
				SELECT @Local_Exec = 'USE ' + @Local_ListOfDatabasesDBAdmin_DatabaseName + ' DBCC CHECKCATALOG '
				Exec sp_executesql @Local_Exec -- Run the Command.
				Print CHAR(10) + CHAR(13) + 'Finished CHECKCATALOG' + CHAR(10) + CHAR(13) + CHAR(10) + CHAR(13)

				Print 'Starting CHECKALLOC' + CHAR(10) + CHAR(13)
				SELECT @Local_Exec = 'USE ' + @Local_ListOfDatabasesDBAdmin_DatabaseName + ' DBCC CHECKALLOC WITH ALL_ERRORMSGS, NO_INFOMSGS '
				Exec sp_executesql @Local_Exec -- Run the Command.
				Print CHAR(10) + CHAR(13) + 'Finished CHECKALLOC' + CHAR(10) + CHAR(13) + CHAR(10) + CHAR(13)

				Print 'Starting CHECKCONSTRAINTS' + CHAR(10) + CHAR(13)
				SELECT @Local_Exec = 'USE ' + @Local_ListOfDatabasesDBAdmin_DatabaseName + ' DBCC CHECKCONSTRAINTS WITH ALL_ERRORMSGS, NO_INFOMSGS '
				Exec sp_executesql @Local_Exec -- Run the Command.
				Print CHAR(10) + CHAR(13) + 'Finished CHECKCONSTRAINTS' + CHAR(10) + CHAR(13) + CHAR(10) + CHAR(13)

				Print 'Starting CHECKFILEGROUP' + CHAR(10) + CHAR(13)
				SELECT @Local_Exec = 'USE ' + @Local_ListOfDatabasesDBAdmin_DatabaseName + ' DBCC CHECKFILEGROUP WITH ALL_ERRORMSGS, NO_INFOMSGS '
				Exec sp_executesql @Local_Exec -- Run the Command.
				Print CHAR(10) + CHAR(13) + 'Finished CHECKFILEGROUP' + CHAR(10) + CHAR(13) + CHAR(10) + CHAR(13)

				Print 'Starting UPDATEUSAGE' + CHAR(10) + CHAR(13)
				SELECT @Local_Exec = 'USE ' + @Local_ListOfDatabasesDBAdmin_DatabaseName + ' DBCC UPDATEUSAGE (0) WITH COUNT_ROWS '
				Exec sp_executesql @Local_Exec -- Run the Command. 
				Print CHAR(10) + CHAR(13) + 'Finished UPDATEUSAGE' + CHAR(10) + CHAR(13) + CHAR(10) + CHAR(13)

				Print 'Starting sp_updatestats' + CHAR(10) + CHAR(13)
				SELECT @Local_Exec = 'USE ' + @Local_ListOfDatabasesDBAdmin_DatabaseName + ' Exec sp_updatestats'
				Exec sp_executesql @Local_Exec -- Run the Command.
				Print CHAR(10) + CHAR(13) + 'Finished sp_updatestats' + CHAR(10) + CHAR(13) + CHAR(10) + CHAR(13)
				
				Print 'Starting DBCC CHECKIDENT' + CHAR(10) + CHAR(13)
				SELECT @Local_Exec = 'USE ' + @Local_ListOfDatabasesDBAdmin_DatabaseName + 
						' DECLARE @TableNameCheckIdent sysname
						DECLARE cur_reindexchecktable CURSOR FOR						
						SELECT table_name
						FROM information_schema.tables
						WHERE table_type = ''base table'' AND  OBJECTPROPERTY(object_id(table_name), ''TableHasIdentity'') = 1 AND
						table_name NOT IN(''sysdiagrams'',''dtproperties'')
						OPEN cur_reindexchecktable
						FETCH NEXT FROM cur_reindexchecktable INTO @TableNameCheckIdent
						WHILE @@FETCH_STATUS = 0
						BEGIN						 
						 DBCC CHECKIDENT (@TableNameCheckIdent, NORESEED) WITH NO_INFOMSGS
						  FETCH NEXT FROM cur_reindexchecktable INTO @TableNameCheckIdent
						END
						CLOSE cur_reindexchecktable
						DEALLOCATE cur_reindexchecktable '
				Exec sp_executesql @Local_Exec -- Run the Command. 
				Print CHAR(10) + CHAR(13) + 'Finished DBCC CHECKIDENT' + CHAR(10) + CHAR(13) + CHAR(10) + CHAR(13)


				Print 'Starting DBCC DBREINDEX' + CHAR(10) + CHAR(13)
				SELECT @Local_Exec = 'USE ' + @Local_ListOfDatabasesDBAdmin_DatabaseName + 
						' DECLARE @TableName sysname
						DECLARE cur_reindex CURSOR FOR
						SELECT TABLE_SCHEMA + ''.'' + table_name
						  FROM information_schema.tables
						  WHERE table_type = ''base table''
						OPEN cur_reindex
						FETCH NEXT FROM cur_reindex INTO @TableName
						WHILE @@FETCH_STATUS = 0
						BEGIN
						  
						 DBCC DBREINDEX (@TableName, '' '', 0)						
						  FETCH NEXT FROM cur_reindex INTO @TableName
						END
						CLOSE cur_reindex
						DEALLOCATE cur_reindex '
				Exec sp_executesql @Local_Exec -- Run the Command. 
				Print CHAR(10) + CHAR(13) + 'Finished DBCC DBREINDEX' + CHAR(10) + CHAR(13) + CHAR(10) + CHAR(13)
				 
				Print 'Starting DBCC INDEXDEFRAG' + CHAR(10) + CHAR(13)
				SELECT @Local_Exec = 'USE ' + @Local_ListOfDatabasesDBAdmin_DatabaseName + 
				' DECLARE @tablename varchar(255)
				DECLARE @execstr   varchar(400)
				DECLARE @objectid  int
				DECLARE @indexid   int
				DECLARE @frag      decimal
				DECLARE @maxfrag   decimal

				-- Decide on the maximum fragmentation to allow for.
				SELECT @maxfrag = 1.0

				-- Declare a cursor.
				DECLARE tables CURSOR FOR
				   SELECT TABLE_SCHEMA + ''.'' + TABLE_NAME
				   FROM INFORMATION_SCHEMA.TABLES
				   WHERE TABLE_TYPE = ''BASE TABLE''

				-- Create the table.
				CREATE TABLE #fraglist (
				   ObjectName char(255),
				   ObjectId int,
				   IndexName char(255),
				   IndexId int,
				   Lvl int,
				   CountPages int,
				   CountRows int,
				   MinRecSize int,
				   MaxRecSize int,
				   AvgRecSize int,
				   ForRecCount int,
				   Extents int,
				   ExtentSwitches int,
				   AvgFreeBytes int,
				   AvgPageDensity int,
				   ScanDensity decimal,
				   BestCount int,
				   ActualCount int,
				   LogicalFrag decimal,
				   ExtentFrag decimal);

				-- Open the cursor.
				OPEN tables

				-- Loop through all the tables in the database.
				FETCH NEXT
				   FROM tables
				   INTO @tablename

				WHILE @@FETCH_STATUS = 0
				BEGIN;
				-- Do the showcontig of all indexes of the table
				   INSERT INTO #fraglist 
				   EXEC (''DBCC SHOWCONTIG ('''''' + @tablename + '''''') 
					  WITH FAST, TABLERESULTS, ALL_INDEXES, NO_INFOMSGS'');
				   FETCH NEXT
					  FROM tables
					  INTO @tablename;
				END;

				-- Close and deallocate the cursor.
				CLOSE tables
				DEALLOCATE tables

				-- Declare the cursor for the list of indexes to be defragged.
				DECLARE indexes CURSOR FOR
				   SELECT ObjectName, ObjectId, IndexId, LogicalFrag
				   FROM #fraglist
				   WHERE LogicalFrag >= @maxfrag
					  AND INDEXPROPERTY (ObjectId, IndexName, ''IndexDepth'') > 0;

				-- Open the cursor.
				OPEN indexes;

				-- Loop through the indexes.
				FETCH NEXT
				   FROM indexes
				   INTO @tablename, @objectid, @indexid, @frag;

				WHILE @@FETCH_STATUS = 0
				BEGIN

				   SELECT @execstr = ''DBCC INDEXDEFRAG (0, '' + RTRIM(@objectid) + '',
					   '' + RTRIM(@indexid) + '') WITH NO_INFOMSGS'';
					--Print @execstr
				   EXEC (@execstr);

				   FETCH NEXT
					  FROM indexes
					  INTO @tablename, @objectid, @indexid, @frag;
				END

				-- Close and deallocate the cursor.
				CLOSE indexes
				DEALLOCATE indexes

				-- Delete the temporary table.
				DROP TABLE #fraglist '
				Exec sp_executesql @Local_Exec -- Run the Command.
				Print CHAR(10) + CHAR(13) + 'Finished DBCC INDEXDEFRAG' + CHAR(10) + CHAR(13) + CHAR(10) + CHAR(13)
		
				Print   CHAR(10) + CHAR(13) + CHAR(10) + CHAR(13) + 'Finished Processing: ' +  @Local_ListOfDatabasesDBAdmin_DatabaseName +  CHAR(10) + CHAR(13) + CHAR(10) + CHAR(13)
		END TRY
		BEGIN CATCH
			
			 SELECT 
				@Local_ErrorMessage = ERROR_MESSAGE(),
				@Local_ErrorSeverity = ERROR_SEVERITY(),
				@Local_ErrorState = ERROR_STATE();
			
			RAISERROR (@Local_ErrorMessage, -- Message text.
               @Local_ErrorSeverity, -- Severity.
               @Local_ErrorState -- State.
               );
		END CATCH
	
	 SELECT @Local_PrimaryKeyID = MIN(PrimaryKeyID)
			FROM #ListOfDatabasesDBAdmin
			WHERE PrimaryKeyID > @Local_PrimaryKeyID

	END -- WHILE @Local_PrimaryKeyID IS NOT NULL

	SET @Local_end_time_of_sp = getdate()

	SET @Local_EMailBody = 'Time taken for spr_DBAdmin_Script to execute is: ' + CONVERT(varchar(20), DATEDIFF(second, @Local_start_time_of_sp, @Local_end_time_of_sp)) + ' second(s).'
		
		-- Send the email.
	/*EXECUTE msdb.dbo.sp_send_dbmail
				@profile_name	= @LOCAL_PROFILE_NAME,
				@recipients		= @Local_Email_To_Send_To,
				@subject		= 'SQL2: spr_DBAdmin_Script Stored Procedure Succeeded. ',
				@body_format	= 'HTML',
				@body			= @Local_EMailBody
	*/
		
END		-- End of the spr_DBAdmin_Script Stored Procedure.
GO
PRINT N'Creating [dbo].[spr_DBAdmin_Script_DRSQL1]'
GO
CREATE PROCEDURE [dbo].[spr_DBAdmin_Script_DRSQL1]

AS
	BEGIN	-- Start of the spr_DBAdmin_Script Stored Procedure.
		SET NOCOUNT ON;
		
		DECLARE @Local_Exec												nvarchar(4000),
				@Local_ErrorMessage										nvarchar(4000),
				@Local_ErrorSeverity									int,
				@Local_ErrorState										int,
				@Local_ListOfDatabasesDBAdmin_DatabaseName				nvarchar(256),
				@Local_PrimaryKeyID										int,
				@Local_Email_To_Send_To									varchar(8000),
				@Local_EMailBody										nvarchar(512),
				@LOCAL_PROFILE_NAME										varchar(255),
				@Local_start_time_of_sp									datetime,
				@Local_end_time_of_sp									datetime
	
	SET @Local_start_time_of_sp = getdate() -- Timing the start of the SP.

	-- Populate GLOBAL variables.	
	SET @LOCAL_PROFILE_NAME = 'SQLMail'
	SELECT @Local_Email_To_Send_To = COALESCE(@Local_Email_To_Send_To + ';', '') + AdminEmail 
	FROM UTIL.dbo.tblAdminEMail	


		-- Dropping Temp Table.
	IF EXISTS (
		SELECT  *
		FROM 	tempdb.dbo.sysobjects o
		WHERE
			o.xtype in ('U')	and
			o.id = object_id( N'tempdb..'+'#ListOfDatabasesDBAdmin')
		)
		BEGIN
			DROP TABLE #ListOfDatabasesFullBackup
		END
	-- Creating the Temp Table.
	CREATE TABLE #ListOfDatabasesDBAdmin(
			PrimaryKeyID							int identity (1, 1),			
			ListOfDatabasesDBAdmin_DatabaseName		nvarchar(256)
		)


	-- Populating the #ListOfDatabases temp table with a list
	-- of databases.
	INSERT INTO #ListOfDatabasesDBAdmin(ListOfDatabasesDBAdmin_DatabaseName)	
	
	SELECT name FROM master.sys.databases
	WHERE	
			state_desc = 'ONLINE' AND
			is_read_only = 0 AND
	name   IN('VRDB')
	ORDER BY name ASC
	
	SELECT * FROM #ListOfDatabasesDBAdmin
	

	SELECT @Local_PrimaryKeyID = MIN(PrimaryKeyID)
	FROM #ListOfDatabasesDBAdmin
	WHILE @Local_PrimaryKeyID IS NOT NULL

	BEGIN -- WHILE @Local_PrimaryKeyID IS NOT NULL
		SELECT	@Local_ListOfDatabasesDBAdmin_DatabaseName		= NULL

		SELECT 	@Local_ListOfDatabasesDBAdmin_DatabaseName	= ListOfDatabasesDBAdmin_DatabaseName
				
		FROM #ListOfDatabasesDBAdmin
		WHERE PrimaryKeyID = @Local_PrimaryKeyID
		
		
		BEGIN TRY				
				Print 'Now Processing: ' +  @Local_ListOfDatabasesDBAdmin_DatabaseName +  CHAR(10) + CHAR(13) +  CHAR(10) + CHAR(13)

				--SELECT @Local_Exec = 'USE ' + @Local_ListOfDatabasesDBAdmin_DatabaseName + ' DBCC CHECKDB'
				SELECT @Local_Exec = 'USE ' + @Local_ListOfDatabasesDBAdmin_DatabaseName + ' DBCC CHECKDB(' + @Local_ListOfDatabasesDBAdmin_DatabaseName + ') WITH ALL_ERRORMSGS, NO_INFOMSGS'
				
				Print 'Starting CHECKDB' + CHAR(10) + CHAR(13)
				Exec sp_executesql @Local_Exec -- Run the Command.
				Print CHAR(10) + CHAR(13) + 'Finished CHECKDB' + CHAR(10) + CHAR(13) + CHAR(10) + CHAR(13)
				
				Print 'Starting CHECKCATALOG' + CHAR(10) + CHAR(13)
				SELECT @Local_Exec = 'USE ' + @Local_ListOfDatabasesDBAdmin_DatabaseName + ' DBCC CHECKCATALOG '
				Exec sp_executesql @Local_Exec -- Run the Command.
				Print CHAR(10) + CHAR(13) + 'Finished CHECKCATALOG' + CHAR(10) + CHAR(13) + CHAR(10) + CHAR(13)

				Print 'Starting CHECKALLOC' + CHAR(10) + CHAR(13)
				SELECT @Local_Exec = 'USE ' + @Local_ListOfDatabasesDBAdmin_DatabaseName + ' DBCC CHECKALLOC WITH ALL_ERRORMSGS, NO_INFOMSGS '
				Exec sp_executesql @Local_Exec -- Run the Command.
				Print CHAR(10) + CHAR(13) + 'Finished CHECKALLOC' + CHAR(10) + CHAR(13) + CHAR(10) + CHAR(13)

				Print 'Starting CHECKCONSTRAINTS' + CHAR(10) + CHAR(13)
				SELECT @Local_Exec = 'USE ' + @Local_ListOfDatabasesDBAdmin_DatabaseName + ' DBCC CHECKCONSTRAINTS WITH ALL_ERRORMSGS, NO_INFOMSGS '
				Exec sp_executesql @Local_Exec -- Run the Command.
				Print CHAR(10) + CHAR(13) + 'Finished CHECKCONSTRAINTS' + CHAR(10) + CHAR(13) + CHAR(10) + CHAR(13)

				Print 'Starting CHECKFILEGROUP' + CHAR(10) + CHAR(13)
				SELECT @Local_Exec = 'USE ' + @Local_ListOfDatabasesDBAdmin_DatabaseName + ' DBCC CHECKFILEGROUP WITH ALL_ERRORMSGS, NO_INFOMSGS '
				Exec sp_executesql @Local_Exec -- Run the Command.
				Print CHAR(10) + CHAR(13) + 'Finished CHECKFILEGROUP' + CHAR(10) + CHAR(13) + CHAR(10) + CHAR(13)

				Print 'Starting UPDATEUSAGE' + CHAR(10) + CHAR(13)
				SELECT @Local_Exec = 'USE ' + @Local_ListOfDatabasesDBAdmin_DatabaseName + ' DBCC UPDATEUSAGE (0) WITH COUNT_ROWS '
				Exec sp_executesql @Local_Exec -- Run the Command. 
				Print CHAR(10) + CHAR(13) + 'Finished UPDATEUSAGE' + CHAR(10) + CHAR(13) + CHAR(10) + CHAR(13)

				Print 'Starting sp_updatestats' + CHAR(10) + CHAR(13)
				SELECT @Local_Exec = 'USE ' + @Local_ListOfDatabasesDBAdmin_DatabaseName + ' Exec sp_updatestats'
				Exec sp_executesql @Local_Exec -- Run the Command.
				Print CHAR(10) + CHAR(13) + 'Finished sp_updatestats' + CHAR(10) + CHAR(13) + CHAR(10) + CHAR(13)
				
				Print 'Starting DBCC CHECKIDENT' + CHAR(10) + CHAR(13)
				SELECT @Local_Exec = 'USE ' + @Local_ListOfDatabasesDBAdmin_DatabaseName + 
						' DECLARE @TableNameCheckIdent sysname
						DECLARE cur_reindexchecktable CURSOR FOR						
						SELECT table_name
						FROM information_schema.tables
						WHERE table_type = ''base table'' AND  OBJECTPROPERTY(object_id(table_name), ''TableHasIdentity'') = 1 AND
						table_name NOT IN(''sysdiagrams'',''dtproperties'')
						OPEN cur_reindexchecktable
						FETCH NEXT FROM cur_reindexchecktable INTO @TableNameCheckIdent
						WHILE @@FETCH_STATUS = 0
						BEGIN						 
						 DBCC CHECKIDENT (@TableNameCheckIdent, NORESEED) WITH NO_INFOMSGS
						  FETCH NEXT FROM cur_reindexchecktable INTO @TableNameCheckIdent
						END
						CLOSE cur_reindexchecktable
						DEALLOCATE cur_reindexchecktable '
				Exec sp_executesql @Local_Exec -- Run the Command. 
				Print CHAR(10) + CHAR(13) + 'Finished DBCC CHECKIDENT' + CHAR(10) + CHAR(13) + CHAR(10) + CHAR(13)


				Print 'Starting DBCC DBREINDEX' + CHAR(10) + CHAR(13)
				SELECT @Local_Exec = 'USE ' + @Local_ListOfDatabasesDBAdmin_DatabaseName + 
						' DECLARE @TableName sysname
						DECLARE cur_reindex CURSOR FOR
						SELECT table_name
						  FROM information_schema.tables
						  WHERE table_type = ''base table''
						OPEN cur_reindex
						FETCH NEXT FROM cur_reindex INTO @TableName
						WHILE @@FETCH_STATUS = 0
						BEGIN
						  
						 DBCC DBREINDEX (@TableName, '' '', 0)						
						  FETCH NEXT FROM cur_reindex INTO @TableName
						END
						CLOSE cur_reindex
						DEALLOCATE cur_reindex '
				Exec sp_executesql @Local_Exec -- Run the Command. 
				Print CHAR(10) + CHAR(13) + 'Finished DBCC DBREINDEX' + CHAR(10) + CHAR(13) + CHAR(10) + CHAR(13)
				 
				Print 'Starting DBCC INDEXDEFRAG' + CHAR(10) + CHAR(13)
				SELECT @Local_Exec = 'USE ' + @Local_ListOfDatabasesDBAdmin_DatabaseName + 
				' DECLARE @tablename varchar(255)
				DECLARE @execstr   varchar(400)
				DECLARE @objectid  int
				DECLARE @indexid   int
				DECLARE @frag      decimal
				DECLARE @maxfrag   decimal

				-- Decide on the maximum fragmentation to allow for.
				SELECT @maxfrag = 1.0

				-- Declare a cursor.
				DECLARE tables CURSOR FOR
				   SELECT TABLE_SCHEMA + ''.'' + TABLE_NAME
				   FROM INFORMATION_SCHEMA.TABLES
				   WHERE TABLE_TYPE = ''BASE TABLE''

				-- Create the table.
				CREATE TABLE #fraglist (
				   ObjectName char(255),
				   ObjectId int,
				   IndexName char(255),
				   IndexId int,
				   Lvl int,
				   CountPages int,
				   CountRows int,
				   MinRecSize int,
				   MaxRecSize int,
				   AvgRecSize int,
				   ForRecCount int,
				   Extents int,
				   ExtentSwitches int,
				   AvgFreeBytes int,
				   AvgPageDensity int,
				   ScanDensity decimal,
				   BestCount int,
				   ActualCount int,
				   LogicalFrag decimal,
				   ExtentFrag decimal);

				-- Open the cursor.
				OPEN tables

				-- Loop through all the tables in the database.
				FETCH NEXT
				   FROM tables
				   INTO @tablename

				WHILE @@FETCH_STATUS = 0
				BEGIN;
				-- Do the showcontig of all indexes of the table
				   INSERT INTO #fraglist 
				   EXEC (''DBCC SHOWCONTIG ('''''' + @tablename + '''''') 
					  WITH FAST, TABLERESULTS, ALL_INDEXES, NO_INFOMSGS'');
				   FETCH NEXT
					  FROM tables
					  INTO @tablename;
				END;

				-- Close and deallocate the cursor.
				CLOSE tables
				DEALLOCATE tables

				-- Declare the cursor for the list of indexes to be defragged.
				DECLARE indexes CURSOR FOR
				   SELECT ObjectName, ObjectId, IndexId, LogicalFrag
				   FROM #fraglist
				   WHERE LogicalFrag >= @maxfrag
					  AND INDEXPROPERTY (ObjectId, IndexName, ''IndexDepth'') > 0;

				-- Open the cursor.
				OPEN indexes;

				-- Loop through the indexes.
				FETCH NEXT
				   FROM indexes
				   INTO @tablename, @objectid, @indexid, @frag;

				WHILE @@FETCH_STATUS = 0
				BEGIN

				   SELECT @execstr = ''DBCC INDEXDEFRAG (0, '' + RTRIM(@objectid) + '',
					   '' + RTRIM(@indexid) + '') WITH NO_INFOMSGS'';
					--Print @execstr
				   EXEC (@execstr);

				   FETCH NEXT
					  FROM indexes
					  INTO @tablename, @objectid, @indexid, @frag;
				END

				-- Close and deallocate the cursor.
				CLOSE indexes
				DEALLOCATE indexes

				-- Delete the temporary table.
				DROP TABLE #fraglist '
				Exec sp_executesql @Local_Exec -- Run the Command.
				Print CHAR(10) + CHAR(13) + 'Finished DBCC INDEXDEFRAG' + CHAR(10) + CHAR(13) + CHAR(10) + CHAR(13)
		
				Print   CHAR(10) + CHAR(13) + CHAR(10) + CHAR(13) + 'Finished Processing: ' +  @Local_ListOfDatabasesDBAdmin_DatabaseName +  CHAR(10) + CHAR(13) + CHAR(10) + CHAR(13)
		END TRY
		BEGIN CATCH
			
			 SELECT 
				@Local_ErrorMessage = ERROR_MESSAGE(),
				@Local_ErrorSeverity = ERROR_SEVERITY(),
				@Local_ErrorState = ERROR_STATE();
			
			RAISERROR (@Local_ErrorMessage, -- Message text.
               @Local_ErrorSeverity, -- Severity.
               @Local_ErrorState -- State.
               );
		END CATCH
	
	 SELECT @Local_PrimaryKeyID = MIN(PrimaryKeyID)
			FROM #ListOfDatabasesDBAdmin
			WHERE PrimaryKeyID > @Local_PrimaryKeyID

	END -- WHILE @Local_PrimaryKeyID IS NOT NULL

	SET @Local_end_time_of_sp = getdate()

	SET @Local_EMailBody = 'Time taken for spr_DBAdmin_Script to execute is: ' + CONVERT(varchar(20), DATEDIFF(second, @Local_start_time_of_sp, @Local_end_time_of_sp)) + ' second(s).'
		
		-- Send the email.
	EXECUTE msdb.dbo.sp_send_dbmail
				@profile_name	= @LOCAL_PROFILE_NAME,
				@recipients		= @Local_Email_To_Send_To,
				@subject		= 'SQL2: spr_DBAdmin_Script Stored Procedure Succeeded. ',
				@body_format	= 'HTML',
				@body			= @Local_EMailBody

		
END		-- End of the spr_DBAdmin_Script Stored Procedure.
GO
PRINT N'Creating [dbo].[spr_DBAdmin_Script_Misc]'
GO
CREATE PROCEDURE [dbo].[spr_DBAdmin_Script_Misc]

AS
	BEGIN	-- Start of the spr_DBAdmin_Script Stored Procedure.
		SET NOCOUNT ON;
		
		DECLARE @Local_Exec												nvarchar(4000),
				@Local_ErrorMessage										nvarchar(4000),
				@Local_ErrorSeverity									int,
				@Local_ErrorState										int,
				@Local_ListOfDatabasesDBAdmin_DatabaseName				nvarchar(256),
				@Local_PrimaryKeyID										int,
				@Local_Email_To_Send_To									varchar(8000),
				@Local_EMailBody										nvarchar(512),
				@LOCAL_PROFILE_NAME										varchar(255),
				@Local_start_time_of_sp									datetime,
				@Local_end_time_of_sp									datetime
	
	SET @Local_start_time_of_sp = getdate() -- Timing the start of the SP.

	-- Populate GLOBAL variables.	
	SET @LOCAL_PROFILE_NAME = 'SQLMail'
	SELECT @Local_Email_To_Send_To = COALESCE(@Local_Email_To_Send_To + ';', '') + AdminEmail 
	FROM UTIL.dbo.tblAdminEMail	


		-- Dropping Temp Table.
	IF EXISTS (
		SELECT  *
		FROM 	tempdb.dbo.sysobjects o
		WHERE
			o.xtype in ('U')	and
			o.id = object_id( N'tempdb..'+'#ListOfDatabasesDBAdmin')
		)
		BEGIN
			DROP TABLE #ListOfDatabasesFullBackup
		END
	-- Creating the Temp Table.
	CREATE TABLE #ListOfDatabasesDBAdmin(
			PrimaryKeyID							int identity (1, 1),			
			ListOfDatabasesDBAdmin_DatabaseName		nvarchar(256)
		)


	-- Populating the #ListOfDatabases temp table with a list
	-- of databases.
	INSERT INTO #ListOfDatabasesDBAdmin(ListOfDatabasesDBAdmin_DatabaseName)	
	
	SELECT name FROM master.sys.databases
	WHERE	
			state = 0 AND
			is_read_only = 0 AND
			[database_id] > 6
	ORDER BY name ASC		

	SELECT @Local_PrimaryKeyID = MIN(PrimaryKeyID)
	FROM #ListOfDatabasesDBAdmin
	WHILE @Local_PrimaryKeyID IS NOT NULL

	BEGIN -- WHILE @Local_PrimaryKeyID IS NOT NULL
		SELECT	@Local_ListOfDatabasesDBAdmin_DatabaseName		= NULL

		SELECT 	@Local_ListOfDatabasesDBAdmin_DatabaseName	= ListOfDatabasesDBAdmin_DatabaseName
				
		FROM #ListOfDatabasesDBAdmin
		WHERE PrimaryKeyID = @Local_PrimaryKeyID
		
		
		BEGIN TRY				
				Print 'Now Processing: ' +  @Local_ListOfDatabasesDBAdmin_DatabaseName +  CHAR(10) + CHAR(13) +  CHAR(10) + CHAR(13)

				--SELECT @Local_Exec = 'USE ' + @Local_ListOfDatabasesDBAdmin_DatabaseName + ' DBCC CHECKDB'
				SELECT @Local_Exec = 'USE ' + @Local_ListOfDatabasesDBAdmin_DatabaseName + ' DBCC CHECKDB(' + @Local_ListOfDatabasesDBAdmin_DatabaseName + ') WITH ALL_ERRORMSGS, NO_INFOMSGS'
				
				Print 'Starting CHECKDB' + CHAR(10) + CHAR(13)
				Exec sp_executesql @Local_Exec -- Run the Command.
				Print CHAR(10) + CHAR(13) + 'Finished CHECKDB' + CHAR(10) + CHAR(13) + CHAR(10) + CHAR(13)
				
				Print 'Starting CHECKCATALOG' + CHAR(10) + CHAR(13)
				SELECT @Local_Exec = 'USE ' + @Local_ListOfDatabasesDBAdmin_DatabaseName + ' DBCC CHECKCATALOG '
				Exec sp_executesql @Local_Exec -- Run the Command.
				Print CHAR(10) + CHAR(13) + 'Finished CHECKCATALOG' + CHAR(10) + CHAR(13) + CHAR(10) + CHAR(13)

				Print 'Starting CHECKALLOC' + CHAR(10) + CHAR(13)
				SELECT @Local_Exec = 'USE ' + @Local_ListOfDatabasesDBAdmin_DatabaseName + ' DBCC CHECKALLOC WITH ALL_ERRORMSGS, NO_INFOMSGS '
				Exec sp_executesql @Local_Exec -- Run the Command.
				Print CHAR(10) + CHAR(13) + 'Finished CHECKALLOC' + CHAR(10) + CHAR(13) + CHAR(10) + CHAR(13)

				Print 'Starting CHECKCONSTRAINTS' + CHAR(10) + CHAR(13)
				SELECT @Local_Exec = 'USE ' + @Local_ListOfDatabasesDBAdmin_DatabaseName + ' DBCC CHECKCONSTRAINTS WITH ALL_ERRORMSGS, NO_INFOMSGS '
				Exec sp_executesql @Local_Exec -- Run the Command.
				Print CHAR(10) + CHAR(13) + 'Finished CHECKCONSTRAINTS' + CHAR(10) + CHAR(13) + CHAR(10) + CHAR(13)

				Print 'Starting CHECKFILEGROUP' + CHAR(10) + CHAR(13)
				SELECT @Local_Exec = 'USE ' + @Local_ListOfDatabasesDBAdmin_DatabaseName + ' DBCC CHECKFILEGROUP WITH ALL_ERRORMSGS, NO_INFOMSGS '
				Exec sp_executesql @Local_Exec -- Run the Command.
				Print CHAR(10) + CHAR(13) + 'Finished CHECKFILEGROUP' + CHAR(10) + CHAR(13) + CHAR(10) + CHAR(13)

				Print 'Starting UPDATEUSAGE' + CHAR(10) + CHAR(13)
				SELECT @Local_Exec = 'USE ' + @Local_ListOfDatabasesDBAdmin_DatabaseName + ' DBCC UPDATEUSAGE (0) WITH COUNT_ROWS '
				Exec sp_executesql @Local_Exec -- Run the Command. 
				Print CHAR(10) + CHAR(13) + 'Finished UPDATEUSAGE' + CHAR(10) + CHAR(13) + CHAR(10) + CHAR(13)

				Print 'Starting sp_updatestats' + CHAR(10) + CHAR(13)
				SELECT @Local_Exec = 'USE ' + @Local_ListOfDatabasesDBAdmin_DatabaseName + ' Exec sp_updatestats'
				Exec sp_executesql @Local_Exec -- Run the Command.
				Print CHAR(10) + CHAR(13) + 'Finished sp_updatestats' + CHAR(10) + CHAR(13) + CHAR(10) + CHAR(13)
				
				Print 'Starting DBCC CHECKIDENT' + CHAR(10) + CHAR(13)
				SELECT @Local_Exec = 'USE ' + @Local_ListOfDatabasesDBAdmin_DatabaseName + 
						' DECLARE @TableNameCheckIdent sysname
						DECLARE cur_reindexchecktable CURSOR FOR						
						SELECT table_name
						FROM information_schema.tables
						WHERE table_type = ''base table'' AND  OBJECTPROPERTY(object_id(table_name), ''TableHasIdentity'') = 1 AND
						table_name NOT IN(''sysdiagrams'',''dtproperties'')
						OPEN cur_reindexchecktable
						FETCH NEXT FROM cur_reindexchecktable INTO @TableNameCheckIdent
						WHILE @@FETCH_STATUS = 0
						BEGIN						 
						 DBCC CHECKIDENT (@TableNameCheckIdent, NORESEED) WITH NO_INFOMSGS
						  FETCH NEXT FROM cur_reindexchecktable INTO @TableNameCheckIdent
						END
						CLOSE cur_reindexchecktable
						DEALLOCATE cur_reindexchecktable '
				Exec sp_executesql @Local_Exec -- Run the Command. 
				Print CHAR(10) + CHAR(13) + 'Finished DBCC CHECKIDENT' + CHAR(10) + CHAR(13) + CHAR(10) + CHAR(13)


				Print 'Starting DBCC DBREINDEX' + CHAR(10) + CHAR(13)
				SELECT @Local_Exec = 'USE ' + @Local_ListOfDatabasesDBAdmin_DatabaseName + 
						' DECLARE @TableName sysname
						DECLARE cur_reindex CURSOR FOR
						SELECT TABLE_SCHEMA + ''.'' + table_name
						  FROM information_schema.tables
						  WHERE table_type = ''base table''
						OPEN cur_reindex
						FETCH NEXT FROM cur_reindex INTO @TableName
						WHILE @@FETCH_STATUS = 0
						BEGIN
						  
						 DBCC DBREINDEX (@TableName, '' '', 0)						
						  FETCH NEXT FROM cur_reindex INTO @TableName
						END
						CLOSE cur_reindex
						DEALLOCATE cur_reindex '
				Exec sp_executesql @Local_Exec -- Run the Command. 
				Print CHAR(10) + CHAR(13) + 'Finished DBCC DBREINDEX' + CHAR(10) + CHAR(13) + CHAR(10) + CHAR(13)
				 
				Print 'Starting DBCC INDEXDEFRAG' + CHAR(10) + CHAR(13)
				SELECT @Local_Exec = 'USE ' + @Local_ListOfDatabasesDBAdmin_DatabaseName + 
				' DECLARE @tablename varchar(255)
				DECLARE @execstr   varchar(400)
				DECLARE @objectid  int
				DECLARE @indexid   int
				DECLARE @frag      decimal
				DECLARE @maxfrag   decimal

				-- Decide on the maximum fragmentation to allow for.
				SELECT @maxfrag = 1.0

				-- Declare a cursor.
				DECLARE tables CURSOR FOR
				   SELECT TABLE_SCHEMA + ''.'' + TABLE_NAME
				   FROM INFORMATION_SCHEMA.TABLES
				   WHERE TABLE_TYPE = ''BASE TABLE''

				-- Create the table.
				CREATE TABLE #fraglist (
				   ObjectName char(255),
				   ObjectId int,
				   IndexName char(255),
				   IndexId int,
				   Lvl int,
				   CountPages int,
				   CountRows int,
				   MinRecSize int,
				   MaxRecSize int,
				   AvgRecSize int,
				   ForRecCount int,
				   Extents int,
				   ExtentSwitches int,
				   AvgFreeBytes int,
				   AvgPageDensity int,
				   ScanDensity decimal,
				   BestCount int,
				   ActualCount int,
				   LogicalFrag decimal,
				   ExtentFrag decimal);

				-- Open the cursor.
				OPEN tables

				-- Loop through all the tables in the database.
				FETCH NEXT
				   FROM tables
				   INTO @tablename

				WHILE @@FETCH_STATUS = 0
				BEGIN;
				-- Do the showcontig of all indexes of the table
				   INSERT INTO #fraglist 
				   EXEC (''DBCC SHOWCONTIG ('''''' + @tablename + '''''') 
					  WITH FAST, TABLERESULTS, ALL_INDEXES, NO_INFOMSGS'');
				   FETCH NEXT
					  FROM tables
					  INTO @tablename;
				END;

				-- Close and deallocate the cursor.
				CLOSE tables
				DEALLOCATE tables

				-- Declare the cursor for the list of indexes to be defragged.
				DECLARE indexes CURSOR FOR
				   SELECT ObjectName, ObjectId, IndexId, LogicalFrag
				   FROM #fraglist
				   WHERE LogicalFrag >= @maxfrag
					  AND INDEXPROPERTY (ObjectId, IndexName, ''IndexDepth'') > 0;

				-- Open the cursor.
				OPEN indexes;

				-- Loop through the indexes.
				FETCH NEXT
				   FROM indexes
				   INTO @tablename, @objectid, @indexid, @frag;

				WHILE @@FETCH_STATUS = 0
				BEGIN

				   SELECT @execstr = ''DBCC INDEXDEFRAG (0, '' + RTRIM(@objectid) + '',
					   '' + RTRIM(@indexid) + '') WITH NO_INFOMSGS'';
					--Print @execstr
				   EXEC (@execstr);

				   FETCH NEXT
					  FROM indexes
					  INTO @tablename, @objectid, @indexid, @frag;
				END

				-- Close and deallocate the cursor.
				CLOSE indexes
				DEALLOCATE indexes

				-- Delete the temporary table.
				DROP TABLE #fraglist '
				Exec sp_executesql @Local_Exec -- Run the Command.
				Print CHAR(10) + CHAR(13) + 'Finished DBCC INDEXDEFRAG' + CHAR(10) + CHAR(13) + CHAR(10) + CHAR(13)
		
				Print   CHAR(10) + CHAR(13) + CHAR(10) + CHAR(13) + 'Finished Processing: ' +  @Local_ListOfDatabasesDBAdmin_DatabaseName +  CHAR(10) + CHAR(13) + CHAR(10) + CHAR(13)
		END TRY
		BEGIN CATCH
			
			 SELECT 
				@Local_ErrorMessage = ERROR_MESSAGE(),
				@Local_ErrorSeverity = ERROR_SEVERITY(),
				@Local_ErrorState = ERROR_STATE();
			
			RAISERROR (@Local_ErrorMessage, -- Message text.
               @Local_ErrorSeverity, -- Severity.
               @Local_ErrorState -- State.
               );
		END CATCH
	
	 SELECT @Local_PrimaryKeyID = MIN(PrimaryKeyID)
			FROM #ListOfDatabasesDBAdmin
			WHERE PrimaryKeyID > @Local_PrimaryKeyID

	END -- WHILE @Local_PrimaryKeyID IS NOT NULL

	SET @Local_end_time_of_sp = getdate()

	SET @Local_EMailBody = 'Time taken for spr_DBAdmin_Script to execute is: ' + CONVERT(varchar(20), DATEDIFF(second, @Local_start_time_of_sp, @Local_end_time_of_sp)) + ' second(s).'
		
		-- Send the email.
	EXECUTE msdb.dbo.sp_send_dbmail
				@profile_name	= @LOCAL_PROFILE_NAME,
				@recipients		= @Local_Email_To_Send_To,
				@subject		= 'SQL2: spr_DBAdmin_Script Stored Procedure Succeeded. ',
				@body_format	= 'HTML',
				@body			= @Local_EMailBody

		
END		-- End of the spr_DBAdmin_Script Stored Procedure.
GO
PRINT N'Creating [dbo].[tblFullBackupAndDR]'
GO
CREATE TABLE [dbo].[tblFullBackupAndDR]
(
[DatabaseID] [int] NOT NULL,
[DatabaseName] [nvarchar] (256) NOT NULL,
[FullBackup] [bit] NOT NULL CONSTRAINT [DF_tblFullBackupAndDR_FullBackup] DEFAULT ((1)),
[FullBackupPathLocalServer] [nvarchar] (256) NULL,
[FullDatabaseBackupName] [nvarchar] (256) NULL,
[DR] [bit] NOT NULL CONSTRAINT [DF_tblFullBackupAndDR_DR] DEFAULT ((0)),
[DRPathLocalServer] [nvarchar] (256) NULL,
[DRCopyPath] [nvarchar] (256) NULL
)
GO
PRINT N'Creating primary key [PK_tblFullBackupAndDR] on [dbo].[tblFullBackupAndDR]'
GO
ALTER TABLE [dbo].[tblFullBackupAndDR] ADD CONSTRAINT [PK_tblFullBackupAndDR] PRIMARY KEY CLUSTERED  ([DatabaseID])
GO
PRINT N'Adding constraints to [dbo].[tblFullBackupAndDR]'
GO
ALTER TABLE [dbo].[tblFullBackupAndDR] ADD CONSTRAINT [constraint_DatabaseName] UNIQUE NONCLUSTERED  ([DatabaseName])
GO
PRINT N'Creating [dbo].[SRA_DeleteOldTranLog_DataDomain]'
GO
SET QUOTED_IDENTIFIER OFF
GO
CREATE PROCEDURE [dbo].[SRA_DeleteOldTranLog_DataDomain]

	
AS
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	-- interfering with SELECT statements.
	
	--2014-05-21 dlong updated to in-line enable/disable xp_cmdshell
	
	--temporarily enable xp_cmdhshell
	EXEC sp_configure 'xp_cmdshell', '1';
	RECONFIGURE WITH OVERRIDE;
	
	SET NOCOUNT ON;
	DECLARE	@Local_PrimaryKeyID										int,
			@Local_ListOfDatabasesTL_DatabaseName					nvarchar(256),
			@Local_TranLogName										nvarchar(4000),
			@Local_BackupTransactionLogCommand						nvarchar(4000),
			@Local_ListOfDatabasesTL_DRPathLocalServer				nvarchar(256),
			@Local_ListOfDatabases_DRCopyPath						nvarchar(256),
			@Local_ErrorMessage										nvarchar(4000),
			@Local_ErrorSeverity									int,
			@Local_ErrorState										int,
			@Local_xp_cmdshell										nvarchar(4000),
			@Local_Email_To_Send_To									varchar(8000),
			@LOCAL_PROFILE_NAME										varchar(255)

	-- Populate GLOBAL variables.	
	SET @LOCAL_PROFILE_NAME = 'SQLMail'
	SELECT @Local_Email_To_Send_To = COALESCE(@Local_Email_To_Send_To + ';', '') + AdminEmail 
	FROM UTIL.dbo.tblAdminEMail	

	IF EXISTS (
		SELECT  *
		FROM 	tempdb.dbo.sysobjects o
		WHERE
			o.xtype in ('U')	and
			o.id = object_id( N'tempdb..'+'#ListOfDatabasesOldTLDelete')
		)
		BEGIN
			DROP TABLE #ListOfDatabasesOldTLDelete
		END
	-- Creating the Temp Table.
	CREATE TABLE #ListOfDatabasesOldTLDelete(
			PrimaryKeyID								int identity (1, 1),			
			ListOfDatabasesTL_DRPathLocalServer			nvarchar(256)
		)
	
	INSERT INTO #ListOfDatabasesOldTLDelete(ListOfDatabasesTL_DRPathLocalServer)							

	SELECT	DRPathLocalServer			
	FROM UTIL.dbo.tblFullBackupAndDR
	WHERE DR = 1
	ORDER BY DatabaseName ASC


	SELECT @Local_PrimaryKeyID = MIN(PrimaryKeyID)
	FROM #ListOfDatabasesOldTLDelete
	WHILE @Local_PrimaryKeyID IS NOT NULL
		BEGIN -- WHILE @Local_PrimaryKeyID IS NOT NULL
			SELECT	@Local_ListOfDatabasesTL_DRPathLocalServer				= NULL

			SELECT	@Local_ListOfDatabasesTL_DRPathLocalServer = ListOfDatabasesTL_DRPathLocalServer					
			FROM #ListOfDatabasesOldTLDelete
			WHERE PrimaryKeyID = @Local_PrimaryKeyID
					

		BEGIN TRY						
					-- Deleting existing bak files from the @Local_Path folder.
					EXECUTE master.dbo.xp_delete_file 0,@Local_ListOfDatabasesTL_DRPathLocalServer,N'trn'
					--Print @Local_ListOfDatabasesTL_DRPathLocalServer
		END TRY
		BEGIN CATCH
			
			 SELECT 
				@Local_ErrorMessage = 'OLD BACKUP LOG DELETE failed. The DELETE path is: ' + @Local_ListOfDatabasesTL_DRPathLocalServer + ' ' + ERROR_MESSAGE(),
				@Local_ErrorSeverity = ERROR_SEVERITY(),
				@Local_ErrorState = ERROR_STATE();
			
			RAISERROR (@Local_ErrorMessage, -- Message text.
               @Local_ErrorSeverity, -- Severity.
               @Local_ErrorState -- State.
               );			
			/*EXECUTE msdb.dbo.sp_send_dbmail
						@profile_name	= @LOCAL_PROFILE_NAME,
						@recipients		= @Local_Email_To_Send_To,
						@subject		= 'SQL2: SRA_DeleteOldTranLog_DataDomain Stored Procedure failed. ',
						@body_format	= 'HTML',
						@body			= @Local_ErrorMessage */
		END CATCH
		--Print '-------------------------------------------------------------------------'
				

		
		SELECT @Local_PrimaryKeyID = MIN(PrimaryKeyID)
			FROM #ListOfDatabasesOldTLDelete
			WHERE PrimaryKeyID > @Local_PrimaryKeyID
	END -- WHILE @Local_PrimaryKeyID IS NOT NULL

	IF EXISTS (
		SELECT  *
		FROM 	tempdb.dbo.sysobjects o
		WHERE
			o.xtype in ('U')	and
			o.id = object_id( N'tempdb..'+'#ListOfDatabasesOldTLDelete')
		)
		BEGIN
			DROP TABLE #ListOfDatabasesOldTLDelete
		END
	--disable xp_cmdhshell
	EXEC sp_configure 'xp_cmdshell', '0';
	RECONFIGURE WITH OVERRIDE;
END
GO
PRINT N'Creating [dbo].[SRA_FullDBBackup]'
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE PROCEDURE [dbo].[SRA_FullDBBackup]
	
AS
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	-- interfering with SELECT statements.
	SET NOCOUNT ON;

	DECLARE @Local_BackupDataCommand									nvarchar(2048),
			@Local_VerifyBackupCommand									nvarchar(2048),
			@Local_ListOfDatabasesFullBackup_DatabaseName				nvarchar(256),
			@Local_ListOfDatabasesFullBackup_FullBackupPathLocalServer	nvarchar(256),
			@Local_ListOfDatabasesFullBackup_FullDatabaseBackupName		nvarchar(256),
			@Local_PrimaryKeyID											int,
			@Local_DeleteFlag											bit,
			@Local_ErrorMessage											nvarchar(4000),
			@Local_ErrorSeverity										int,
			@Local_ErrorState											int,
			@Local_start_time_of_sp										datetime,
			@Local_end_time_of_sp										datetime,
			@Local_EMailBody											nvarchar(512),
			@Local_Email_To_Send_To										varchar(8000),
			@LOCAL_PROFILE_NAME											varchar(255),
			@Local_FileExists											int,
			@Local_FilePathAndName										nvarchar(2048)


	SET @Local_start_time_of_sp = getdate() -- Timing the start of the SP.
	SET @Local_DeleteFlag = 0

	-- Populate GLOBAL variables.	
	SET @LOCAL_PROFILE_NAME = 'SQLMail'
	SELECT @Local_Email_To_Send_To = COALESCE(@Local_Email_To_Send_To + ';', '') + AdminEmail 
	FROM UTIL.dbo.tblAdminEMail		

	IF EXISTS (SELECT 1 FROM msdb..suspect_pages)
	
		-- Make sure that msdb..suspect_pages has no rows.
		BEGIN
				EXECUTE msdb.dbo.sp_send_dbmail
						@profile_name	= @LOCAL_PROFILE_NAME,
						@recipients		= @Local_Email_To_Send_To,
						@subject		= 'SQL2: SRA_FullDBBackup Stored Procedure failed. ',
						@body_format	= 'HTML',
						@body			= 'The msdb..suspect_pages table has rows.'
		END

		SET @Local_FilePathAndName = 'H:\Microsoft SQL Server\MSSQL.1\MSSQL\LOG\SQLDump0030.txt'
		EXEC master.dbo.xp_fileexist @Local_FilePathAndName, @Local_FileExists OUTPUT
		-- Here we are finding out whether the SQLDump File exists.
			IF @Local_FileExists = 1
				BEGIN -- IF @Local_FileExists = 1
					EXECUTE msdb.dbo.sp_send_dbmail
						@profile_name	= @LOCAL_PROFILE_NAME,
						@recipients		= @Local_Email_To_Send_To,
						@subject		= 'SQL2: SRA_FullDBBackup Stored Procedure failed. ',
						@body_format	= 'HTML',
						@body			= 'The SQLDump0030.txt file exists in the H:\Microsoft SQL Server\MSSQL.1\MSSQL\LOG\ path.'
				END

	-- Dropping Temp Table.
	IF EXISTS (
		SELECT  *
		FROM 	tempdb.dbo.sysobjects o
		WHERE
			o.xtype in ('U')	and
			o.id = object_id( N'tempdb..'+'#ListOfDatabasesFullBackup')
		)
		BEGIN
			DROP TABLE #ListOfDatabasesFullBackup
		END
	-- Creating the Temp Table.
	CREATE TABLE #ListOfDatabasesFullBackup(
			PrimaryKeyID										int identity (1, 1),			
			ListOfDatabasesFullBackup_DatabaseName				nvarchar(256),
			ListOfDatabasesFullBackup_FullBackupPathLocalServer	nvarchar(256),
			ListOfDatabasesFullBackup_FullDatabaseBackupName	nvarchar(256)
		)


	-- Populating the #ListOfDatabases temp table with a list
	-- of databases.
	INSERT INTO #ListOfDatabasesFullBackup(ListOfDatabasesFullBackup_DatabaseName,
					ListOfDatabasesFullBackup_FullBackupPathLocalServer,
					ListOfDatabasesFullBackup_FullDatabaseBackupName)
	
	SELECT	DatabaseName,
			FullBackupPathLocalServer,
			FullDatabaseBackupName
	FROM UTIL.dbo.tblFullBackupAndDR
	WHERE FullBackup = 1			
	ORDER BY DatabaseName ASC
	
	-- Special Case. For SLFileAuditor. Need to copy only on Friday's.
	/*IF ( datename(dw,getdate())) <> 'Friday'
	BEGIN
		DELETE FROM #ListOfDatabasesFullBackup WHERE ListOfDatabasesFullBackup_DatabaseName IN('SLFileAuditor')
	END*/

	-- Looping through the databases and backing up the database.	

	SELECT @Local_PrimaryKeyID = MIN(PrimaryKeyID)
	FROM #ListOfDatabasesFullBackup
	WHILE @Local_PrimaryKeyID IS NOT NULL
	BEGIN -- WHILE @Local_PrimaryKeyID IS NOT NULL
		SELECT	@Local_ListOfDatabasesFullBackup_DatabaseName				= NULL,
				@Local_ListOfDatabasesFullBackup_FullBackupPathLocalServer	= NULL,
				@Local_ListOfDatabasesFullBackup_FullDatabaseBackupName		= NULL,
				@Local_BackupDataCommand									= NULL,
				@Local_VerifyBackupCommand									= NULL

		SELECT 	@Local_ListOfDatabasesFullBackup_DatabaseName	= ListOfDatabasesFullBackup_DatabaseName,
				@Local_ListOfDatabasesFullBackup_FullBackupPathLocalServer = ListOfDatabasesFullBackup_FullBackupPathLocalServer,
				@Local_ListOfDatabasesFullBackup_FullDatabaseBackupName = ListOfDatabasesFullBackup_FullDatabaseBackupName
		FROM #ListOfDatabasesFullBackup
		WHERE PrimaryKeyID = @Local_PrimaryKeyID

		SET @Local_BackupDataCommand = 'BACKUP DATABASE [' + @Local_ListOfDatabasesFullBackup_DatabaseName + '] TO DISK = ' + '''' + @Local_ListOfDatabasesFullBackup_FullBackupPathLocalServer + @Local_ListOfDatabasesFullBackup_FullDatabaseBackupName + ''''

		
		BEGIN TRY
				IF @Local_DeleteFlag = 0
					BEGIN
							-- Deleting existing bak files from the @Local_Path folder.
							EXECUTE master.dbo.xp_delete_file 0,@Local_ListOfDatabasesFullBackup_FullBackupPathLocalServer,N'bak'
							SET @Local_DeleteFlag = 1
					END
		
				IF DB_ID(@Local_ListOfDatabasesFullBackup_DatabaseName) IS NOT NULL
						BEGIN
							Exec sp_executesql @Local_BackupDataCommand -- Backup the database.
						END								
		END TRY

		BEGIN CATCH
			
			 SELECT 
				@Local_ErrorMessage = 'Database name is: ' + @Local_ListOfDatabasesFullBackup_DatabaseName + '. ' + ERROR_MESSAGE(),
				@Local_ErrorSeverity = ERROR_SEVERITY(),
				@Local_ErrorState = ERROR_STATE();
			
				EXECUTE msdb.dbo.sp_send_dbmail
					@profile_name	= @LOCAL_PROFILE_NAME,
					@recipients		= @Local_Email_To_Send_To,
					@subject		= 'SQL2: SRA_FullDBBackup Stored Procedure failed. Backup Error. ',
					@body_format	= 'HTML',
					@body			= @Local_ErrorMessage

			RAISERROR (@Local_ErrorMessage, -- Message text.
               @Local_ErrorSeverity, -- Severity.
               @Local_ErrorState -- State.
               );
		END CATCH
		
		-- We want a 2 minute delay while the FINQ database is being backed up.
		-- This will give it enough time to back up.
		IF @Local_ListOfDatabasesFullBackup_FullDatabaseBackupName = 'FINQ.bak'
			BEGIN
					WAITFOR DELAY '000:02:00'
			END
		SET @Local_VerifyBackupCommand = 'RESTORE VERIFYONLY FROM DISK = ' + '''' + @Local_ListOfDatabasesFullBackup_FullBackupPathLocalServer + @Local_ListOfDatabasesFullBackup_FullDatabaseBackupName + ''''
		
		-- We also don't want to verify FINQ database backup.
		BEGIN TRY
				IF ( (DB_ID(@Local_ListOfDatabasesFullBackup_DatabaseName) IS NOT NULL) AND (@Local_ListOfDatabasesFullBackup_FullDatabaseBackupName <> 'FINQ.bak'))
					BEGIN
						Exec sp_executesql @Local_VerifyBackupCommand -- Verify the database backup.
					END
		END TRY

		BEGIN CATCH
			SELECT 
				@Local_ErrorMessage = 'Database name is: ' + @Local_ListOfDatabasesFullBackup_DatabaseName +  '. ' + ERROR_MESSAGE(),
				@Local_ErrorSeverity = ERROR_SEVERITY(),
				@Local_ErrorState = ERROR_STATE();
			
				EXECUTE msdb.dbo.sp_send_dbmail
					@profile_name	= @LOCAL_PROFILE_NAME,
					@recipients		= @Local_Email_To_Send_To,
					@subject		= 'SQL2: SRA_FullDBBackup Stored Procedure failed. Verifying restore. ',
					@body_format	= 'HTML',
					@body			= @Local_ErrorMessage

			RAISERROR (@Local_ErrorMessage, -- Message text.
               @Local_ErrorSeverity, -- Severity.
               @Local_ErrorState -- State.
               );
		END CATCH
		
	 SELECT @Local_PrimaryKeyID = MIN(PrimaryKeyID)
			FROM #ListOfDatabasesFullBackup
			WHERE PrimaryKeyID > @Local_PrimaryKeyID
	END -- WHILE @Local_PrimaryKeyID IS NOT NULL

	-- Dropping Temp Table.
	IF EXISTS (
		SELECT  *
		FROM 	tempdb.dbo.sysobjects o
		WHERE
			o.xtype in ('U')	and
			o.id = object_id( N'tempdb..'+'#ListOfDatabasesFullBackup')
		)
		BEGIN
			DROP TABLE #ListOfDatabasesFullBackup
		END

		SET @Local_end_time_of_sp = getdate()

		SET @Local_EMailBody = 'Time taken for SRA_FullDBBackup to execute is: ' + CONVERT(varchar(20), DATEDIFF(second, @Local_start_time_of_sp, @Local_end_time_of_sp)) + ' second(s).'
		
		-- Send the email.
		EXECUTE msdb.dbo.sp_send_dbmail
					@profile_name	= @LOCAL_PROFILE_NAME,
					@recipients		= @Local_Email_To_Send_To,
					@subject		= 'SQL2: SRA_FullDBBackup Stored Procedure Succeeded. ',
					@body_format	= 'HTML',
					@body			= @Local_EMailBody
		

END
GO
PRINT N'Creating [dbo].[SRA_FullDBBackup_DataDomain]'
GO

CREATE PROCEDURE [dbo].[SRA_FullDBBackup_DataDomain]
	
AS
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	-- interfering with SELECT statements.
	--2014-05-21 dlong updated to comment out xp_cmdshell as the directory doesn't exist and we do this another way.
	SET NOCOUNT ON;

	DECLARE @Local_BackupDataCommand									nvarchar(2048),
			@Local_VerifyBackupCommand									nvarchar(2048),
			@Local_ListOfDatabasesFullBackup_DatabaseName				nvarchar(256),
			@Local_ListOfDatabasesFullBackup_FullBackupPathLocalServer	nvarchar(256),
			@Local_ListOfDatabasesFullBackup_FullDatabaseBackupName		nvarchar(256),
			@Local_PrimaryKeyID											int,
			@Local_DeleteFlag											bit,
			@Local_ErrorMessage											nvarchar(4000),
			@Local_ErrorSeverity										int,
			@Local_ErrorState											int,
			@Local_start_time_of_sp										datetime,
			@Local_end_time_of_sp										datetime,
			@Local_EMailBody											nvarchar(512),
			@Local_Email_To_Send_To										varchar(8000),
			@LOCAL_PROFILE_NAME											varchar(255),
			@Local_FileExists											int,
			@Local_FilePathAndName										nvarchar(2048),
			@Local_rc													int


	SET @Local_start_time_of_sp = getdate() -- Timing the start of the SP.
	SET @Local_DeleteFlag = 0

	-- Populate GLOBAL variables.	
	SET @LOCAL_PROFILE_NAME = 'SQLMail'
	SELECT @Local_Email_To_Send_To = COALESCE(@Local_Email_To_Send_To + ';', '') + AdminEmail 
	FROM UTIL.dbo.tblAdminEMail		

	IF EXISTS (SELECT 1 FROM msdb..suspect_pages)
	
		-- Make sure that msdb..suspect_pages has no rows.
		BEGIN
				EXECUTE msdb.dbo.sp_send_dbmail
						@profile_name	= @LOCAL_PROFILE_NAME,
						@recipients		= @Local_Email_To_Send_To,
						@subject		= 'SQL2: SRA_FullDBBackup_DataDomain Stored Procedure failed. ',
						@body_format	= 'HTML',
						@body			= 'The msdb..suspect_pages table has rows.'
		END

		SET @Local_FilePathAndName = 'H:\Microsoft SQL Server\MSSQL.1\MSSQL\LOG\SQLDump0099.txt'
		EXEC master.dbo.xp_fileexist @Local_FilePathAndName, @Local_FileExists OUTPUT
		-- Here we are finding out whether the SQLDump File exists.
			IF @Local_FileExists = 1
				BEGIN -- IF @Local_FileExists = 1
					EXECUTE msdb.dbo.sp_send_dbmail
						@profile_name	= @LOCAL_PROFILE_NAME,
						@recipients		= @Local_Email_To_Send_To,
						@subject		= 'SQL2: SRA_FullDBBackup_DataDomain Stored Procedure failed. ',
						@body_format	= 'HTML',
						@body			= 'The SQLDump0099.txt file exists in the H:\Microsoft SQL Server\MSSQL.1\MSSQL\LOG\ path.'
				END

	-- Dropping Temp Table.
	IF EXISTS (
		SELECT  *
		FROM 	tempdb.dbo.sysobjects o
		WHERE
			o.xtype in ('U')	and
			o.id = object_id( N'tempdb..'+'#ListOfDatabasesFullBackup')
		)
		BEGIN
			DROP TABLE #ListOfDatabasesFullBackup
		END
	-- Creating the Temp Table.
	CREATE TABLE #ListOfDatabasesFullBackup(
			PrimaryKeyID										int identity (1, 1),			
			ListOfDatabasesFullBackup_DatabaseName				nvarchar(256),
			ListOfDatabasesFullBackup_FullBackupPathLocalServer	nvarchar(256),
			ListOfDatabasesFullBackup_FullDatabaseBackupName	nvarchar(256)
		)


	-- Populating the #ListOfDatabases temp table with a list
	-- of databases.
	INSERT INTO #ListOfDatabasesFullBackup(ListOfDatabasesFullBackup_DatabaseName,
					ListOfDatabasesFullBackup_FullBackupPathLocalServer,
					ListOfDatabasesFullBackup_FullDatabaseBackupName)
	
	SELECT	DatabaseName,
			FullBackupPathLocalServer,
			FullDatabaseBackupName
	FROM UTIL.dbo.tblFullBackupAndDR
	WHERE FullBackup = 1			
	ORDER BY DatabaseName ASC
	


	-- Looping through the databases and backing up the database.	

	SELECT @Local_PrimaryKeyID = MIN(PrimaryKeyID)
	FROM #ListOfDatabasesFullBackup
	WHILE @Local_PrimaryKeyID IS NOT NULL
	BEGIN -- WHILE @Local_PrimaryKeyID IS NOT NULL
		SELECT	@Local_ListOfDatabasesFullBackup_DatabaseName				= NULL,
				@Local_ListOfDatabasesFullBackup_FullBackupPathLocalServer	= NULL,
				@Local_ListOfDatabasesFullBackup_FullDatabaseBackupName		= NULL,
				@Local_BackupDataCommand									= NULL,
				@Local_VerifyBackupCommand									= NULL

		SELECT 	@Local_ListOfDatabasesFullBackup_DatabaseName	= ListOfDatabasesFullBackup_DatabaseName,
				@Local_ListOfDatabasesFullBackup_FullBackupPathLocalServer = ListOfDatabasesFullBackup_FullBackupPathLocalServer,
				@Local_ListOfDatabasesFullBackup_FullDatabaseBackupName = ListOfDatabasesFullBackup_FullDatabaseBackupName
		FROM #ListOfDatabasesFullBackup
		WHERE PrimaryKeyID = @Local_PrimaryKeyID

		SET @Local_BackupDataCommand = 'BACKUP DATABASE [' + @Local_ListOfDatabasesFullBackup_DatabaseName + '] TO DISK = ' + '''' + @Local_ListOfDatabasesFullBackup_FullBackupPathLocalServer + @Local_ListOfDatabasesFullBackup_FullDatabaseBackupName + ''''
		--Print @Local_BackupDataCommand
		
		BEGIN TRY
				IF @Local_DeleteFlag = 0
					BEGIN
							-- Deleting existing bak files from the @Local_Path folder.
							EXECUTE master.dbo.xp_delete_file 0,@Local_ListOfDatabasesFullBackup_FullBackupPathLocalServer,N'bak'							
							--SET @Local_DeleteFlag = 1
					END
		
				IF DB_ID(@Local_ListOfDatabasesFullBackup_DatabaseName) IS NOT NULL
						BEGIN
							Exec sp_executesql @Local_BackupDataCommand -- Backup the database.
							-- Special Logic for MSRA_DROP database restore.
							--IF LTRIM(RTRIM(@Local_ListOfDatabasesFullBackup_DatabaseName)) = 'MSRA_DROP'
							--	BEGIN
							--		-- Step 1. Delete existing bak file on SRADEVSQL.
							--		EXECUTE master.dbo.xp_delete_file 0,'\\sradevsql\h$\COPIED_FROM_SQL2_MSRA_DROP\',N'bak'
									
							--		-- Step 2. Copy the MSRA_DROP.bak file from Data Domain to SRADEVSQL.
							--		EXEC @Local_rc = master.dbo.xp_cmdshell 'copy \\srabaldd1\backup\SRA\SQL2\MSRA_DROP\MSRA_DROP.bak \\sradevsql\h$\COPIED_FROM_SQL2_MSRA_DROP\MSRA_DROP.bak'
							--	END
						END								
		END TRY

		BEGIN CATCH
			
			 SELECT 
				@Local_ErrorMessage = 'Database name is: ' + @Local_ListOfDatabasesFullBackup_DatabaseName + '. ' + ERROR_MESSAGE(),
				@Local_ErrorSeverity = ERROR_SEVERITY(),
				@Local_ErrorState = ERROR_STATE();
			
				EXECUTE msdb.dbo.sp_send_dbmail
					@profile_name	= @LOCAL_PROFILE_NAME,
					@recipients		= @Local_Email_To_Send_To,
					@subject		= 'SQL2: SRA_FullDBBackup_DataDomain Stored Procedure failed. Backup Error. ',
					@body_format	= 'HTML',
					@body			= @Local_ErrorMessage

			RAISERROR (@Local_ErrorMessage, -- Message text.
               @Local_ErrorSeverity, -- Severity.
               @Local_ErrorState -- State.
               );
		END CATCH
				
	 SELECT @Local_PrimaryKeyID = MIN(PrimaryKeyID)
			FROM #ListOfDatabasesFullBackup
			WHERE PrimaryKeyID > @Local_PrimaryKeyID
	END -- WHILE @Local_PrimaryKeyID IS NOT NULL

	-- Dropping Temp Table.
	IF EXISTS (
		SELECT  *
		FROM 	tempdb.dbo.sysobjects o
		WHERE
			o.xtype in ('U')	and
			o.id = object_id( N'tempdb..'+'#ListOfDatabasesFullBackup')
		)
		BEGIN
			DROP TABLE #ListOfDatabasesFullBackup
		END

		SET @Local_end_time_of_sp = getdate()

		SET @Local_EMailBody = 'Time taken for SRA_FullDBBackup_DataDomain to execute is: ' + CONVERT(varchar(20), DATEDIFF(second, @Local_start_time_of_sp, @Local_end_time_of_sp)) + ' second(s).'
		
		-- Send the email.
		/*EXECUTE msdb.dbo.sp_send_dbmail
					@profile_name	= @LOCAL_PROFILE_NAME,
					@recipients		= @Local_Email_To_Send_To,
					@subject		= 'SQL2: SRA_FullDBBackup_DataDomain Stored Procedure Succeeded. ',
					@body_format	= 'HTML',
					@body			= @Local_EMailBody
		*/

END
GO
PRINT N'Creating [dbo].[SRA_FullDBBackup_DataDomain_NoDelete]'
GO
CREATE PROCEDURE [dbo].[SRA_FullDBBackup_DataDomain_NoDelete]
	
AS

BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	-- interfering with SELECT statements.
	--2014-05-27 dlong updated to in-line enable/disable xp_cmdshell
	SET NOCOUNT ON;

	DECLARE @Local_BackupDataCommand									nvarchar(2048),
			@Local_VerifyBackupCommand									nvarchar(2048),
			@Local_ListOfDatabasesFullBackup_DatabaseName				nvarchar(256),
			@Local_ListOfDatabasesFullBackup_FullBackupPathLocalServer	nvarchar(256),
			@Local_ListOfDatabasesFullBackup_FullDatabaseBackupName		nvarchar(256),
			@Local_PrimaryKeyID											int,
			@Local_DeleteFlag											bit,
			@Local_ErrorMessage											nvarchar(4000),
			@Local_ErrorSeverity										int,
			@Local_ErrorState											int,
			@Local_start_time_of_sp										datetime,
			@Local_end_time_of_sp										datetime,
			@Local_EMailBody											nvarchar(512),
			@Local_Email_To_Send_To										varchar(8000),
			@LOCAL_PROFILE_NAME											varchar(255),
			@Local_FileExists											int,
			@Local_FilePathAndName										nvarchar(2048),
			@Local_rc													int


	SET @Local_start_time_of_sp = getdate() -- Timing the start of the SP.
	SET @Local_DeleteFlag = 0

	-- Populate GLOBAL variables.	
	SET @LOCAL_PROFILE_NAME = 'SQLMail'
	SELECT @Local_Email_To_Send_To = COALESCE(@Local_Email_To_Send_To + ';', '') + AdminEmail 
	FROM UTIL.dbo.tblAdminEMail		
/*
	IF EXISTS (SELECT 1 FROM msdb..suspect_pages)
	
		-- Make sure that msdb..suspect_pages has no rows.
		BEGIN
				EXECUTE msdb.dbo.sp_send_dbmail
						@profile_name	= @LOCAL_PROFILE_NAME,
						@recipients		= @Local_Email_To_Send_To,
						@subject		= 'SQL2: SRA_FullDBBackup_DataDomain Stored Procedure failed. ',
						@body_format	= 'HTML',
						@body			= 'The msdb..suspect_pages table has rows.'
		END

		SET @Local_FilePathAndName = 'H:\Microsoft SQL Server\MSSQL.1\MSSQL\LOG\SQLDump0099.txt'
		EXEC master.dbo.xp_fileexist @Local_FilePathAndName, @Local_FileExists OUTPUT
		-- Here we are finding out whether the SQLDump File exists.
			IF @Local_FileExists = 1
				BEGIN -- IF @Local_FileExists = 1
					EXECUTE msdb.dbo.sp_send_dbmail
						@profile_name	= @LOCAL_PROFILE_NAME,
						@recipients		= @Local_Email_To_Send_To,
						@subject		= 'SQL2: SRA_FullDBBackup_DataDomain Stored Procedure failed. ',
						@body_format	= 'HTML',
						@body			= 'The SQLDump0099.txt file exists in the H:\Microsoft SQL Server\MSSQL.1\MSSQL\LOG\ path.'
				END
*/
	-- Dropping Temp Table.
	IF EXISTS (
		SELECT  *
		FROM 	tempdb.dbo.sysobjects o
		WHERE
			o.xtype in ('U')	and
			o.id = object_id( N'tempdb..'+'#ListOfDatabasesFullBackup')
		)
		BEGIN
			DROP TABLE #ListOfDatabasesFullBackup
		END
	-- Creating the Temp Table.
	CREATE TABLE #ListOfDatabasesFullBackup(
			PrimaryKeyID										int identity (1, 1),			
			ListOfDatabasesFullBackup_DatabaseName				nvarchar(256),
			ListOfDatabasesFullBackup_FullBackupPathLocalServer	nvarchar(256),
			ListOfDatabasesFullBackup_FullDatabaseBackupName	nvarchar(256)
		)


	-- Populating the #ListOfDatabases temp table with a list
	-- of databases.
	INSERT INTO #ListOfDatabasesFullBackup(ListOfDatabasesFullBackup_DatabaseName,
					ListOfDatabasesFullBackup_FullBackupPathLocalServer,
					ListOfDatabasesFullBackup_FullDatabaseBackupName)
	
	SELECT	DatabaseName,
			FullBackupPathLocalServer,
			FullDatabaseBackupName
	FROM UTIL.dbo.tblFullBackupAndDR
	WHERE FullBackup = 1			
	ORDER BY DatabaseName ASC
	


	-- Looping through the databases and backing up the database.	

	SELECT @Local_PrimaryKeyID = MIN(PrimaryKeyID)
	FROM #ListOfDatabasesFullBackup
	
	--temporarily enable xp_cmdhshell
	EXEC sp_configure 'xp_cmdshell', '1';
	RECONFIGURE WITH OVERRIDE;
	
	WHILE @Local_PrimaryKeyID IS NOT NULL
	BEGIN -- WHILE @Local_PrimaryKeyID IS NOT NULL
		SELECT	@Local_ListOfDatabasesFullBackup_DatabaseName				= NULL,
				@Local_ListOfDatabasesFullBackup_FullBackupPathLocalServer	= NULL,
				@Local_ListOfDatabasesFullBackup_FullDatabaseBackupName		= NULL,
				@Local_BackupDataCommand									= NULL,
				@Local_VerifyBackupCommand									= NULL

		SELECT 	@Local_ListOfDatabasesFullBackup_DatabaseName	= ListOfDatabasesFullBackup_DatabaseName,
				@Local_ListOfDatabasesFullBackup_FullBackupPathLocalServer = ListOfDatabasesFullBackup_FullBackupPathLocalServer,
				@Local_ListOfDatabasesFullBackup_FullDatabaseBackupName = ListOfDatabasesFullBackup_FullDatabaseBackupName
		FROM #ListOfDatabasesFullBackup
		WHERE PrimaryKeyID = @Local_PrimaryKeyID

		SET @Local_BackupDataCommand = 'BACKUP DATABASE [' + @Local_ListOfDatabasesFullBackup_DatabaseName + '] TO DISK = ' + '''' + @Local_ListOfDatabasesFullBackup_FullBackupPathLocalServer + @Local_ListOfDatabasesFullBackup_FullDatabaseBackupName + ''''
		--Print @Local_BackupDataCommand
		
		BEGIN TRY
				/*IF @Local_DeleteFlag = 0
					BEGIN
							-- Deleting existing bak files from the @Local_Path folder.
							EXECUTE master.dbo.xp_delete_file 0,@Local_ListOfDatabasesFullBackup_FullBackupPathLocalServer,N'bak'							
							--SET @Local_DeleteFlag = 1
					END
		*/
				IF DB_ID(@Local_ListOfDatabasesFullBackup_DatabaseName) IS NOT NULL
						BEGIN
							Exec sp_executesql @Local_BackupDataCommand -- Backup the database.
							-- Special Logic for MSRA_DROP database restore.
							IF LTRIM(RTRIM(@Local_ListOfDatabasesFullBackup_DatabaseName)) = 'MSRA_DROP'
								BEGIN
									-- Step 1. Delete existing bak file on SRADEVSQL.
									--EXECUTE master.dbo.xp_delete_file 0,'\\sradevsql\h$\COPIED_FROM_SQL2_MSRA_DROP\',N'bak'
									
									-- Step 2. Copy the MSRA_DROP.bak file from Data Domain to SRADEVSQL.
									EXEC @Local_rc = master.dbo.xp_cmdshell 'copy \\srabaldd1\backup\SRA\SQL2\MSRA_DROP\MSRA_DROP.bak \\sradevsql\h$\COPIED_FROM_SQL2_MSRA_DROP\MSRA_DROP.bak'
								END
						END								
		END TRY

		BEGIN CATCH
			
			 SELECT 
				@Local_ErrorMessage = 'Database name is: ' + @Local_ListOfDatabasesFullBackup_DatabaseName + '. ' + ERROR_MESSAGE(),
				@Local_ErrorSeverity = ERROR_SEVERITY(),
				@Local_ErrorState = ERROR_STATE();
			
				EXECUTE msdb.dbo.sp_send_dbmail
					@profile_name	= @LOCAL_PROFILE_NAME,
					@recipients		= @Local_Email_To_Send_To,
					@subject		= 'SQL2: SRA_FullDBBackup_DataDomain Stored Procedure failed. Backup Error. ',
					@body_format	= 'HTML',
					@body			= @Local_ErrorMessage

			RAISERROR (@Local_ErrorMessage, -- Message text.
               @Local_ErrorSeverity, -- Severity.
               @Local_ErrorState -- State.
               );
		END CATCH
				
	 SELECT @Local_PrimaryKeyID = MIN(PrimaryKeyID)
			FROM #ListOfDatabasesFullBackup
			WHERE PrimaryKeyID > @Local_PrimaryKeyID
	END -- WHILE @Local_PrimaryKeyID IS NOT NULL
	
	--disable xp_cmdhshell
	EXEC sp_configure 'xp_cmdshell', '0';
	RECONFIGURE WITH OVERRIDE;

	-- Dropping Temp Table.
	IF EXISTS (
		SELECT  *
		FROM 	tempdb.dbo.sysobjects o
		WHERE
			o.xtype in ('U')	and
			o.id = object_id( N'tempdb..'+'#ListOfDatabasesFullBackup')
		)
		BEGIN
			DROP TABLE #ListOfDatabasesFullBackup
		END

		SET @Local_end_time_of_sp = getdate()

		SET @Local_EMailBody = 'Time taken for SRA_FullDBBackup_DataDomain to execute is: ' + CONVERT(varchar(20), DATEDIFF(second, @Local_start_time_of_sp, @Local_end_time_of_sp)) + ' second(s).'
		
		-- Send the email.
		/*EXECUTE msdb.dbo.sp_send_dbmail
					@profile_name	= @LOCAL_PROFILE_NAME,
					@recipients		= @Local_Email_To_Send_To,
					@subject		= 'SQL2: SRA_FullDBBackup_DataDomain Stored Procedure Succeeded. ',
					@body_format	= 'HTML',
					@body			= @Local_EMailBody
		*/

END
GO
PRINT N'Creating [dbo].[SRA_GenerateTranLog]'
GO
SET QUOTED_IDENTIFIER OFF
GO
CREATE PROCEDURE [dbo].[SRA_GenerateTranLog]

	
AS
BEGIN
	--2014-05-27 dlong updated xp_cmdShell to use in-line enable/disable but this is not in use anyway
	-- SET NOCOUNT ON added to prevent extra result sets from
	-- interfering with SELECT statements.
	SET NOCOUNT ON;
	DECLARE	@Local_PrimaryKeyID										int,
			@Local_ListOfDatabasesTL_DatabaseName					nvarchar(256),
			@Local_TranLogName										nvarchar(4000),
			@Local_BackupTransactionLogCommand						nvarchar(4000),
			@Local_ListOfDatabasesTL_DRPathLocalServer				nvarchar(256),
			@Local_ListOfDatabases_DRCopyPath						nvarchar(256),
			@Local_ErrorMessage										nvarchar(4000),
			@Local_ErrorSeverity									int,
			@Local_ErrorState										int,
			@Local_xp_cmdshell										nvarchar(4000),
			@Local_Email_To_Send_To									varchar(8000),
			@LOCAL_PROFILE_NAME										varchar(255)

	-- Populate GLOBAL variables.	
	SET @LOCAL_PROFILE_NAME = 'SQLMail'
	SELECT @Local_Email_To_Send_To = COALESCE(@Local_Email_To_Send_To + ';', '') + AdminEmail 
	FROM UTIL.dbo.tblAdminEMail	

	IF EXISTS (
		SELECT  *
		FROM 	tempdb.dbo.sysobjects o
		WHERE
			o.xtype in ('U')	and
			o.id = object_id( N'tempdb..'+'#ListOfDatabasesFullBackup')
		)
		BEGIN
			DROP TABLE #ListOfDatabasesFullBackup
		END
	-- Creating the Temp Table.
	CREATE TABLE #ListOfDatabasesTL(
			PrimaryKeyID								int identity (1, 1),			
			ListOfDatabasesTL_DatabaseName				nvarchar(256),
			ListOfDatabasesTL_DRPathLocalServer			nvarchar(256),
			ListOfDatabases_DRCopyPath					nvarchar(256)
		)
	
	INSERT INTO #ListOfDatabasesTL(	ListOfDatabasesTL_DatabaseName,
									ListOfDatabasesTL_DRPathLocalServer,
									ListOfDatabases_DRCopyPath
							)
	SELECT	DatabaseName,
			DRPathLocalServer,
			DRCopyPath
	FROM UTIL.dbo.tblFullBackupAndDR
	WHERE DR = 1
	ORDER BY DatabaseName ASC


	SELECT @Local_PrimaryKeyID = MIN(PrimaryKeyID)
	FROM #ListOfDatabasesTL
	WHILE @Local_PrimaryKeyID IS NOT NULL
		BEGIN -- WHILE @Local_PrimaryKeyID IS NOT NULL
			SELECT	@Local_ListOfDatabasesTL_DatabaseName					= NULL,	
					@Local_ListOfDatabasesTL_DRPathLocalServer				= NULL,				
					@Local_BackupTransactionLogCommand						= NULL,
					@Local_ListOfDatabases_DRCopyPath						= NULL,
					@Local_xp_cmdshell										= NULL

			SELECT	@Local_ListOfDatabasesTL_DatabaseName	= ListOfDatabasesTL_DatabaseName,
					@Local_ListOfDatabasesTL_DRPathLocalServer = ListOfDatabasesTL_DRPathLocalServer,
					@Local_ListOfDatabases_DRCopyPath	= ListOfDatabases_DRCopyPath
			FROM #ListOfDatabasesTL
			WHERE PrimaryKeyID = @Local_PrimaryKeyID
		
		SELECT @Local_TranLogName  =  @Local_ListOfDatabasesTL_DatabaseName + '-' 
		+ DATENAME(month, getdate()) + '-' 
		+ DATENAME(day, getdate()) + '-' 
		+ DATENAME(year, getdate()) + '-'
		+ DATENAME(weekday, getdate()) + '-'	
		+ DATENAME(hour, getdate()) + '-'
		+ DATENAME(minute, getdate()) + '-'
		+ DATENAME(second, getdate()) + '-'
		+ RIGHT(CONVERT(CHAR(19),getdate()) , 2) + '-'
		+ DATENAME(millisecond, getdate())
		+ '_Log.trn'
		--Print @Local_TranLogName

		SET @Local_BackupTransactionLogCommand = 'BACKUP LOG [' + @Local_ListOfDatabasesTL_DatabaseName + '] TO DISK = ' + '''' + @Local_ListOfDatabasesTL_DRPathLocalServer + @Local_TranLogName + '''' + + ' WITH CHECKSUM'	

		BEGIN TRY
			IF DB_ID(@Local_ListOfDatabasesTL_DatabaseName) IS NOT NULL
				BEGIN
					EXECUTE sp_executesql @Local_BackupTransactionLogCommand
				END			
		END TRY
		BEGIN CATCH
			
			 SELECT 
				@Local_ErrorMessage = 'BACKUP LOG failed. The BACKUP command is: ' + @Local_BackupTransactionLogCommand + ' ' + ERROR_MESSAGE(),
				@Local_ErrorSeverity = ERROR_SEVERITY(),
				@Local_ErrorState = ERROR_STATE();
			
			RAISERROR (@Local_ErrorMessage, -- Message text.
               @Local_ErrorSeverity, -- Severity.
               @Local_ErrorState -- State.
               );			
			EXECUTE msdb.dbo.sp_send_dbmail
						@profile_name	= @LOCAL_PROFILE_NAME,
						@recipients		= @Local_Email_To_Send_To,
						@subject		= 'SQL2: SRA_GenerateTranLog Stored Procedure failed. ',
						@body_format	= 'HTML',
						@body			= @Local_ErrorMessage
		END CATCH
		--Print '-------------------------------------------------------------------------'
		
		BEGIN TRY
			IF DB_ID(@Local_ListOfDatabasesTL_DatabaseName) IS NOT NULL
				BEGIN		
					--temporarily enable xp_cmdhshell
					EXEC sp_configure 'xp_cmdshell', '1';
					RECONFIGURE WITH OVERRIDE;
					SET @Local_xp_cmdshell = 'xcopy ' +  @Local_ListOfDatabasesTL_DRPathLocalServer + @Local_TranLogName +  @Local_ListOfDatabases_DRCopyPath
					EXECUTE master..xp_cmdshell @Local_xp_cmdshell
					--disable xp_cmdhshell
					EXEC sp_configure 'xp_cmdshell', '0';
					RECONFIGURE WITH OVERRIDE;


					-- Deleting existing trn files from the @Local_ListOfDatabasesTL_DRPathLocalServer folder.
					EXECUTE master.dbo.xp_delete_file 0,@Local_ListOfDatabasesTL_DRPathLocalServer,N'trn'
				END
		END TRY

		BEGIN CATCH
			SELECT 
				@Local_ErrorMessage = 'Either xcopy of xp_delete_file failed. ' + ERROR_MESSAGE(),
				@Local_ErrorSeverity = ERROR_SEVERITY(),
				@Local_ErrorState = ERROR_STATE();
			
			RAISERROR (@Local_ErrorMessage, -- Message text.
               @Local_ErrorSeverity, -- Severity.
               @Local_ErrorState -- State.
               );
			EXECUTE msdb.dbo.sp_send_dbmail
						@profile_name	= @LOCAL_PROFILE_NAME,
						@recipients		= @Local_Email_To_Send_To,
						@subject		= 'SQL2: SRA_GenerateTranLog Stored Procedure failed. ',
						@body_format	= 'HTML',
						@body			= @Local_ErrorMessage
		END CATCH

		
		SELECT @Local_PrimaryKeyID = MIN(PrimaryKeyID)
			FROM #ListOfDatabasesTL
			WHERE PrimaryKeyID > @Local_PrimaryKeyID
	END -- WHILE @Local_PrimaryKeyID IS NOT NULL

	IF EXISTS (
		SELECT  *
		FROM 	tempdb.dbo.sysobjects o
		WHERE
			o.xtype in ('U')	and
			o.id = object_id( N'tempdb..'+'#ListOfDatabasesFullBackup')
		)
		BEGIN
			DROP TABLE #ListOfDatabasesFullBackup
		END
END
GO
PRINT N'Creating [dbo].[SRA_GenerateTranLog_DataDomain]'
GO
CREATE PROCEDURE [dbo].[SRA_GenerateTranLog_DataDomain]

	
AS
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	-- interfering with SELECT statements.
	SET NOCOUNT ON;
	DECLARE	@Local_PrimaryKeyID										int,
			@Local_ListOfDatabasesTL_DatabaseName					nvarchar(256),
			@Local_TranLogName										nvarchar(4000),
			@Local_BackupTransactionLogCommand						nvarchar(4000),
			@Local_ListOfDatabasesTL_DRPathLocalServer				nvarchar(256),
			@Local_ListOfDatabases_DRCopyPath						nvarchar(256),
			@Local_ErrorMessage										nvarchar(4000),
			@Local_ErrorSeverity									int,
			@Local_ErrorState										int,
			@Local_Email_To_Send_To									varchar(8000),
			@Local_Hour												int,
			@LOCAL_PROFILE_NAME										varchar(255)

	-- Populate GLOBAL variables.	
	SET @LOCAL_PROFILE_NAME = 'SQLMail'
	SELECT @Local_Email_To_Send_To = COALESCE(@Local_Email_To_Send_To + ';', '') + AdminEmail 
	FROM UTIL.dbo.tblAdminEMail	

	IF EXISTS (
		SELECT  *
		FROM 	tempdb.dbo.sysobjects o
		WHERE
			o.xtype in ('U')	and
			o.id = object_id( N'tempdb..'+'#ListOfDatabasesTL')
		)
		BEGIN
			DROP TABLE #ListOfDatabasesTL
		END
	-- Creating the Temp Table.
	CREATE TABLE #ListOfDatabasesTL(
			PrimaryKeyID								int identity (1, 1),			
			ListOfDatabasesTL_DatabaseName				nvarchar(256),
			ListOfDatabasesTL_DRPathLocalServer			nvarchar(256),
			ListOfDatabases_DRCopyPath					nvarchar(256)
		)
	
	INSERT INTO #ListOfDatabasesTL(	ListOfDatabasesTL_DatabaseName,
									ListOfDatabasesTL_DRPathLocalServer,
									ListOfDatabases_DRCopyPath
							)
	SELECT	DatabaseName,
			DRPathLocalServer,
			DRCopyPath
	FROM UTIL.dbo.tblFullBackupAndDR
	WHERE DR = 1
	ORDER BY DatabaseName ASC


	SELECT @Local_PrimaryKeyID = MIN(PrimaryKeyID)
	FROM #ListOfDatabasesTL
	WHILE @Local_PrimaryKeyID IS NOT NULL
		BEGIN -- WHILE @Local_PrimaryKeyID IS NOT NULL
			SELECT	@Local_ListOfDatabasesTL_DatabaseName					= NULL,	
					@Local_ListOfDatabasesTL_DRPathLocalServer				= NULL,				
					@Local_BackupTransactionLogCommand						= NULL,
					@Local_ListOfDatabases_DRCopyPath						= NULL					

			SELECT	@Local_ListOfDatabasesTL_DatabaseName	= ListOfDatabasesTL_DatabaseName,
					@Local_ListOfDatabasesTL_DRPathLocalServer = ListOfDatabasesTL_DRPathLocalServer,
					@Local_ListOfDatabases_DRCopyPath	= ListOfDatabases_DRCopyPath
			FROM #ListOfDatabasesTL
			WHERE PrimaryKeyID = @Local_PrimaryKeyID
		
		SELECT @Local_TranLogName  =  @Local_ListOfDatabasesTL_DatabaseName + '-' 
		+ DATENAME(month, getdate()) + '-' 
		+ DATENAME(day, getdate()) + '-' 
		+ DATENAME(year, getdate()) + '-'
		+ DATENAME(weekday, getdate()) + '-'	
		+ DATENAME(hour, getdate()) + '-'
		+ DATENAME(minute, getdate()) + '-'
		+ DATENAME(second, getdate()) + '-'
		+ RIGHT(CONVERT(CHAR(19),getdate()) , 2) + '-'
		+ DATENAME(millisecond, getdate())
		+ '_Log.trn'
		--Print @Local_TranLogName

		SET @Local_BackupTransactionLogCommand = 'BACKUP LOG [' + @Local_ListOfDatabasesTL_DatabaseName + '] TO DISK = ' + '''' + @Local_ListOfDatabasesTL_DRPathLocalServer + @Local_TranLogName + '''' + + ' WITH CHECKSUM'	
		
		
		BEGIN TRY
				
			IF DB_ID(@Local_ListOfDatabasesTL_DatabaseName) IS NOT NULL
				BEGIN
					EXECUTE sp_executesql @Local_BackupTransactionLogCommand
				END

		
			--Print @Local_BackupTransactionLogCommand
		END TRY
		BEGIN CATCH
			
			 SELECT 
				@Local_ErrorMessage = 'BACKUP LOG failed. The BACKUP command is: ' + @Local_BackupTransactionLogCommand + ' ' + ERROR_MESSAGE(),
				@Local_ErrorSeverity = ERROR_SEVERITY(),
				@Local_ErrorState = ERROR_STATE();
			
			RAISERROR (@Local_ErrorMessage, -- Message text.
               @Local_ErrorSeverity, -- Severity.
               @Local_ErrorState -- State.
               );			
			EXECUTE msdb.dbo.sp_send_dbmail
						@profile_name	= @LOCAL_PROFILE_NAME,
						@recipients		= @Local_Email_To_Send_To,
						@subject		= 'SQL2: SRA_GenerateTranLog_DataDomain Stored Procedure failed. ',
						@body_format	= 'HTML',
						@body			= @Local_ErrorMessage
		END CATCH
		--Print '-------------------------------------------------------------------------'
				
		
		SELECT @Local_PrimaryKeyID = MIN(PrimaryKeyID)
			FROM #ListOfDatabasesTL
			WHERE PrimaryKeyID > @Local_PrimaryKeyID
	END -- WHILE @Local_PrimaryKeyID IS NOT NULL

	IF EXISTS (
		SELECT  *
		FROM 	tempdb.dbo.sysobjects o
		WHERE
			o.xtype in ('U')	and
			o.id = object_id( N'tempdb..'+'#ListOfDatabasesTL')
		)
		BEGIN
			DROP TABLE #ListOfDatabasesTL
		END
END
GO
PRINT N'Creating [dbo].[SRA_ShrinkTransactionLogNew]'
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE PROCEDURE [dbo].[SRA_ShrinkTransactionLogNew]
	
AS
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	-- interfering with SELECT statements.
	SET NOCOUNT ON;

	DECLARE @Local_BackupTransactionLogCommand		nvarchar(1255),
			@Local_AlterDatabaseCommand				nvarchar(1255),
			@Local_ShrinkFileCommand				nvarchar(1255),
			@Local_DatabaseName						nvarchar(1255),
			@Local_SysFilesString					nvarchar(1255),
			@Local_LogFileName						nvarchar(1255),
			@Local_PrimaryKeyID						int,
			@Local_Counter							int,
			@Local_ErrorMessage						nvarchar(4000),
			@Local_ErrorSeverity					int,
			@Local_ErrorState						int,
			@Local_EMailBody						nvarchar(512),
			@Local_Email_To_Send_To					varchar(8000),
			@LOCAL_PROFILE_NAME						varchar(255)


	-- Populate GLOBAL variables.	
	SET @LOCAL_PROFILE_NAME = 'SQLMail'
	SELECT @Local_Email_To_Send_To = COALESCE(@Local_Email_To_Send_To + ';', '') + AdminEmail 
	FROM UTIL.dbo.tblAdminEMail		

	-- Dropping Temp Table.
	IF EXISTS (
		SELECT  *
		FROM 	tempdb.dbo.sysobjects o
		WHERE
			o.xtype in ('U')	and
			o.id = object_id( N'tempdb..'+'#ListOfDatabases')
		)
		BEGIN
			DROP TABLE #ListOfDatabases
		END
	-- Creating the Temp Table.
	CREATE TABLE #ListOfDatabases(
			PrimaryKeyID					Int Identity (1, 1),			
			ListOfDatabases_DatabaseName	nvarchar(1255)
		)

	DECLARE @shrinktablevar TABLE(
				shrinktablevar_logfilename nvarchar(1255)
			)

	
	-- Deleting existing bak and trn files from the @Local_Path folder.
	--EXECUTE master.dbo.xp_delete_file 0,@Local_Path,N'bak'
	--EXECUTE master.dbo.xp_delete_file 0,@Local_Path,N'trn'

	-- Populating the #ListOfDatabases temp table with a list
	-- of databases.
	INSERT INTO #ListOfDatabases(ListOfDatabases_DatabaseName)
	
	SELECT sysdb.name
	FROM master.sys.databases sysdb
	INNER JOIN sys.database_recovery_status sysrecstatus ON sysdb.database_id = sysrecstatus.database_id
	WHERE	sysdb.recovery_model = 1 AND 
			sysdb.state_desc = 'ONLINE' AND
			sysdb.is_read_only = 0 AND
			sysdb.name NOT IN('tempdb')	AND
			
			sysrecstatus.last_log_backup_lsn IS NOT NULL
	ORDER BY sysdb.name ASC
	DELETE FROM #ListOfDatabases
	WHERE ListOfDatabases_DatabaseName IN(	SELECT DatabaseName 
											FROM UTIL.dbo.tblFullBackupAndDR
											WHERE DR  = 1
										)
		
	--SELECT * FROM #ListOfDatabases
	--RETURN
	
	-- Looping through the databases and backing up the data and transaction log
	-- and shrinking the transaction log.
	SELECT @Local_PrimaryKeyID = MIN(PrimaryKeyID)
	FROM #ListOfDatabases
	WHILE @Local_PrimaryKeyID IS NOT NULL
	BEGIN -- WHILE @Local_PrimaryKeyID IS NOT NULL
		BEGIN TRY
			SELECT	@Local_BackupTransactionLogCommand	= NULL,
					@Local_SysFilesString				= NULL,
					@Local_DatabaseName					= NULL,
					@Local_AlterDatabaseCommand			= NULL

			SELECT 	@Local_DatabaseName	= ListOfDatabases_DatabaseName
			FROM #ListOfDatabases
			WHERE PrimaryKeyID = @Local_PrimaryKeyID

			SET @Local_BackupTransactionLogCommand = 'BACKUP LOG [' + @Local_DatabaseName + '] TO DISK = ' + '''' + 'NUL:' + ''''
					
			SET @Local_SysFilesString = 'USE [' + @Local_DatabaseName + '] SELECT name FROM sysfiles WHERE fileid = 2'
			DELETE FROM @shrinktablevar
			INSERT into @shrinktablevar
			Exec (@Local_SysFilesString)
			SELECT @Local_LogFileName = shrinktablevar_logfilename FROM @shrinktablevar
			SET @Local_ShrinkFileCommand = 'USE [' + @Local_DatabaseName + '] DBCC SHRINKFILE ([' + @Local_LogFileName + '])'
					
			Print 'Now Processing: ' + @Local_DatabaseName
			Exec (@Local_BackupTransactionLogCommand)	-- Backup the transacton log.
			Exec (@Local_ShrinkFileCommand)				-- Shrink the transaction log.
			Exec (@Local_ShrinkFileCommand)				-- Shrink the transaction log.
			Exec (@Local_ShrinkFileCommand)				-- Shrink the transaction log.
			 

			-- Deleting existing bak and trn files from the @Local_Path folder.
			--EXECUTE master.dbo.xp_delete_file 0,@Local_Path,N'bak'
			--EXECUTE master.dbo.xp_delete_file 0,@Local_Path,N'trn'

			IF  @Local_DatabaseName IN ('SharedServices3_Search_DB', 'SharePoint_Config')
				BEGIN				
						IF (SELECT recovery_model_desc from sys.databases WHERE name = @Local_DatabaseName) = 'FULL'
							BEGIN								
								SET @Local_AlterDatabaseCommand = 'ALTER DATABASE [' + @Local_DatabaseName + '] ' + 'SET RECOVERY SIMPLE'
								Exec (@Local_AlterDatabaseCommand)	-- Set the Recovery Mode to Simple.
								Exec (@Local_ShrinkFileCommand)		-- Shrink the transaction log.
								Exec (@Local_ShrinkFileCommand)		-- Shrink the transaction log.
								SET @Local_AlterDatabaseCommand = 'ALTER DATABASE [' + @Local_DatabaseName + '] ' + 'SET RECOVERY FULL'
								Exec (@Local_AlterDatabaseCommand)	-- Set the Recovery Mode to Full.
							END										
				END
		END TRY

		BEGIN CATCH
			SELECT 
				@Local_ErrorMessage = 'Database name is: ' + @Local_DatabaseName +  '. ' + ERROR_MESSAGE(),
				@Local_ErrorSeverity = ERROR_SEVERITY(),
				@Local_ErrorState = ERROR_STATE();
			
				EXECUTE msdb.dbo.sp_send_dbmail
					@profile_name	= @LOCAL_PROFILE_NAME,
					@recipients		= @Local_Email_To_Send_To,
					@subject		= 'SQL2: SRA_ShrinkTransactionLogNew Stored Procedure failed. ',
					@body_format	= 'HTML',
					@body			= @Local_ErrorMessage

			RAISERROR (@Local_ErrorMessage, -- Message text.
               @Local_ErrorSeverity, -- Severity.
               @Local_ErrorState -- State.
               );
		END CATCH

	 SELECT @Local_PrimaryKeyID = MIN(PrimaryKeyID)
			FROM #ListOfDatabases
			WHERE PrimaryKeyID > @Local_PrimaryKeyID
	END -- WHILE @Local_PrimaryKeyID IS NOT NULL


END
GO
PRINT N'Creating [dbo].[spr_ImportMembers]'
GO
SET QUOTED_IDENTIFIER OFF
GO
SET ANSI_NULLS OFF
GO
CREATE PROCEDURE [dbo].[spr_ImportMembers] (@source VARCHAR(20) = NULL)
AS

SET NOCOUNT ON

DECLARE @ssn varchar(11), @fname varchar(20), @lname varchar(40),
   @message varchar(80), @title varchar(80)

IF @source = 'IVR'
BEGIN
	DECLARE members_cursor CURSOR FOR 
	SELECT DISTINCT SSN, FNAME, LNAME
	FROM         IVRDATA.dbo.vw_MemberInfo
	WHERE SSN NOT IN (SELECT     SSN  FROM          FINQ.dbo.vw_MEMBERS)
END

IF @source = 'AS400'
BEGIN
	DECLARE members_cursor CURSOR FOR 
	SELECT DISTINCT SSN, FNAME, LNAME
	FROM        vw_DB2_PRIBTEMP
	WHERE SSN NOT IN (SELECT     SSN  FROM          FINQ.dbo.vw_MEMBERS)
--where ssn = '066-60-8327'
END

IF @source = 'STAGING'
BEGIN
	DECLARE members_cursor CURSOR FOR 
	SELECT ssn, fname, lname
	FROM vw_SSNs_ALL
END


IF @source IS NOT NULL AND (@source = 'STAGING'  OR @source = 'AS400' OR @source = 'IVR')
BEGIN
OPEN members_cursor

FETCH NEXT FROM members_cursor 
INTO @ssn, @fname, @lname

WHILE @@FETCH_STATUS = 0
BEGIN
--   PRINT ' '
   SELECT @message = 'Attempting to insert  '  +    @fname + '  ' + @lname  +   ' (' + @ssn + ')'
   PRINT @message

   EXEC FINQ.DBO.spr_AddMember @ssn, @fname,  @lname

   -- Declare an inner cursor based   
   -- on ssn from the outer cursor.
   
   -- Get the next member.
   FETCH NEXT FROM members_cursor 
   INTO @ssn, @fname, @lname
END

CLOSE members_cursor
DEALLOCATE members_cursor

END
GO
PRINT N'Creating [dbo].[spr_AddNewMembers]'
GO
CREATE PROCEDURE [dbo].[spr_AddNewMembers] AS

EXEC spr_ImportMembers 'IVR'
--EXEC spr_ImportMembers 'AS400'
--EXEC spr_ImportMembers 'STAGING'



/*
-- From IVR Database
INSERT INTO  FINQ.dbo.vw_MEMBERS
SELECT DISTINCT SSN, FNAME, LNAME
FROM         IVRDATA.dbo.vw_MemberInfo
WHERE SSN NOT IN (SELECT     SSN  FROM          FINQ.dbo.vw_MEMBERS)

-- From DB2 PRIBTEMP
INSERT INTO  FINQ.dbo.vw_MEMBERS
SELECT DISTINCT SSN, FNAME, LNAME
FROM        vw_DB2_PRIBTEMP
WHERE SSN NOT IN (SELECT     SSN  FROM          FINQ.dbo.vw_MEMBERS)


INSERT INTO  FINQ.dbo.vw_MEMBERS
SELECT    DISTINCT  *
FROM         vw_SSNs_NEW



INSERT INTO  FINQ.dbo.vw_MEMBERS
SELECT DISTINCT SSN, NULL as FNAME, NULL as LNAME
FROM         IMAGE_TRANSFER
WHERE SSN NOT IN (SELECT     SSN  FROM          FINQ.dbo.vw_MEMBERS)


INSERT INTO  FINQ.dbo.vw_MEMBERS
SELECT DISTINCT SSN, NULL as FNAME, NULL as LNAME
FROM         ALL_SSNS
WHERE SSN NOT IN (SELECT     SSN   FROM          FINQ.dbo.vw_MEMBERS)
*/
GO
PRINT N'Creating [dbo].[DB_RESTORE]'
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_NULLS ON
GO
CREATE TABLE [dbo].[DB_RESTORE]
(
[id] [int] NOT NULL IDENTITY(1, 1),
[backup_file] [nvarchar] (150) NULL,
[database] [nvarchar] (50) NULL,
[file_extension] [nvarchar] (10) NULL,
[size] [int] NULL,
[modified_date] [datetime] NULL,
[isActive] [bit] NULL CONSTRAINT [DF_DB_RESTORE_isActive] DEFAULT ((1)),
[isRestored] [bit] NULL CONSTRAINT [DF_DB_RESTORE_isRestored] DEFAULT ((0)),
[error_message] [varchar] (max) NULL
)
GO
PRINT N'Creating [dbo].[ADDWorkingDays]'
GO
/*
* ==============================================================================
AddWorkingDays
Works like DATEADD, but adds the working days.

Syntax
AddWorkingDays ( StartDate, WorkDays )

Arguments
StartDate - the DATETIME value (start date).
WorkDays - the integer value (number of working days).

Return Types
DATETIME
* ==============================================================================
*/
CREATE FUNCTION [dbo].[ADDWorkingDays]
  ( @StartDate DATETIME,
    @WorkDays INT )
RETURNS DATETIME
AS
BEGIN
  DECLARE @TotalDays INT, @FirstPart INT
  DECLARE @EndDate DATETIME
  DECLARE @LastNum INT, @LastPart INT

  IF @WorkDays < 0
     BEGIN
       SELECT @FirstPart = CASE DATENAME(weekday, @StartDate)
                 WHEN 'Sunday' THEN 0
                 WHEN 'Monday' THEN 1
                 WHEN 'Tuesday' THEN 2
                 WHEN 'Wednesday' THEN 3
                 WHEN 'Thursday' THEN 4
                 WHEN 'Friday' THEN 5
                 WHEN 'Saturday' THEN 6
               END
       IF ABS(@WorkDays) < @FirstPart
          SELECT @EndDate = DATEADD(dd, @WorkDays, @StartDate)
       ELSE
         BEGIN
           SELECT @TotalDays = (ABS(@WorkDays) - @FirstPart) / 5
           SELECT @LastPart = (ABS(@WorkDays) - @FirstPart) % 7
           SELECT @LastNum = CASE
             WHEN (@LastPart < 7) AND (@LastPart > 0) THEN @LastPart - 1
             ELSE 0
           END
           SELECT @TotalDays = - 2 * (@TotalDays + 1) + @WorkDays
           SELECT @EndDate = DATEADD(dd, @TotalDays, @StartDate)
         END
     END

  ELSE

     BEGIN
       SELECT @FirstPart = CASE DATENAME(weekday, @StartDate)
                 WHEN 'Sunday' THEN 6
                 WHEN 'Monday' THEN 5
                 WHEN 'Tuesday' THEN 4
                 WHEN 'Wednesday' THEN 3
                 WHEN 'Thursday' THEN 2
                 WHEN 'Friday' THEN 1
                 WHEN 'Saturday' THEN 0
               END
       IF @WorkDays < @FirstPart
          SELECT @EndDate = DATEADD(dd, @WorkDays, @StartDate)
       ELSE
         BEGIN
           SELECT @TotalDays = (@WorkDays - @FirstPart) / 5
           SELECT @LastPart = (@WorkDays - @FirstPart) % 7
           SELECT @LastNum = CASE
             WHEN (@LastPart < 7) AND (@LastPart > 0) THEN @LastPart - 1
             ELSE 0
           END
           SELECT @TotalDays = 2 * (@TotalDays + 1) + @WorkDays
           SELECT @EndDate = DATEADD(dd, @TotalDays, @StartDate)
         END
     END

  RETURN ( @EndDate )

END
GO
PRINT N'Creating [dbo].[DateNoTimePart]'
GO
SET ANSI_NULLS OFF
GO
/*
* ==============================================================================
DateNoTimePart
Syntax

Arguments

Return Types
* ==============================================================================
*/
CREATE FUNCTION [dbo].[DateNoTimePart] (@inDate datetime)  
RETURNS DATETIME AS  
BEGIN 
DECLARE @tempval VARCHAR(10)
DECLARE @return DATETIME

DECLARE @day VARCHAR(2)
DECLARE @month VARCHAR(2)
DECLARE @year VARCHAR(4)

SET @day = DATEPART(day, @inDate)
SET @month = DATEPART(month, @inDate)
SET @year = DATEPART(year, @inDate)

SELECT @tempval =  @month  + '/' + @day + '/'+ @year

SELECT @Return = CAST(@tempval AS DATETIME)

RETURN @Return 

END
GO
PRINT N'Creating [dbo].[DatePart]'
GO
SET ANSI_NULLS ON
GO
/*
* ==============================================================================
Syntax
DatePart ( datepart )

Arguments
datepart - DATETIME value.

Return Types
VARCHAR
* ==============================================================================
*/
CREATE FUNCTION [dbo].[DatePart]
  ( @fDate DATETIME )
RETURNS VARCHAR(10)
AS
BEGIN
  RETURN ( CONVERT(VARCHAR(10),@fDate,101) )
END
GO
PRINT N'Creating [dbo].[DecimalToTime]'
GO
SET QUOTED_IDENTIFIER OFF
GO
/*
* ==============================================================================

Syntax

Arguments

Return Types
* ==============================================================================
*/
CREATE FUNCTION [dbo].[DecimalToTime](@inTime DECIMAL)  
RETURNS DATETIME AS  
BEGIN 
DECLARE @tempval VARCHAR(20)
DECLARE @return DATETIME

DECLARE @hours VARCHAR(2)
DECLARE @mins VARCHAR(2)
DECLARE @secs VARCHAR(4)

SET @hours = LEFT(@inTime, LEN(@inTime) - 4)
SET @mins = SUBSTRING(RIGHT(@inTime, 4), 1, 2)
SET @secs= RIGHT(@inTime, 2)


SELECT @tempval =  '01/01/01 ' + @hours  + ':' + @mins + ':'+ @secs

SELECT @Return = CAST(@tempval AS DATETIME)

RETURN @Return 
END
GO
PRINT N'Creating [dbo].[FirstMonthDay]'
GO
SET QUOTED_IDENTIFIER ON
GO
/*
* ==============================================================================
FirstMonthDay
Returns the first day of the month for the given date.

Syntax
FirstMonthDay ( date )

Arguments
date - DATETIME value.

Return Types
DATETIME
* ==============================================================================
*/
CREATE FUNCTION [dbo].[FirstMonthDay]
  ( @Date DATETIME )
RETURNS DATETIME
AS
BEGIN
RETURN (CAST(STR(MONTH(@Date)) + '/' + STR(01) + '/' + STR(YEAR(@Date)) AS DATETIME))
END
GO
PRINT N'Creating [dbo].[FiscalYearStartDate]'
GO

CREATE FUNCTION [dbo].[FiscalYearStartDate]
	(
	@statementDate DATETIME
	)
RETURNS VARCHAR(10)

AS
	BEGIN
	DECLARE @fyStartDate VARCHAR(10)
	
		IF MONTH(@statementDate)>=7
				SET @fyStartDate=CONVERT(VARCHAR(4),YEAR(@statementDate))+'-07-01'
		ELSE
				SET @fyStartDate=CONVERT(VARCHAR(4),YEAR(@statementDate)-1)+'-07-01'
		RETURN @fyStartDate
	END
GO
PRINT N'Creating [dbo].[FixSSN]'
GO
SET QUOTED_IDENTIFIER OFF
GO
CREATE FUNCTION [dbo].[FixSSN] (@inSSN VARCHAR(11) )
RETURNS VARCHAR(11) AS  
BEGIN 
DECLARE @tempval VARCHAR(11)

DECLARE @first_part VARCHAR(3)
DECLARE @second_part VARCHAR(2)
DECLARE @third_part VARCHAR(4)

IF PATINDEX('%-%', @inSSN) > 0 OR LEN(RTRIM(LTRIM(@inSSN))) < 9
	SELECT @tempval =  @inSSN
ELSE
	BEGIN
		SET @first_part  = SUBSTRING(@inSSN, 1,3) 
		SET @second_part  = SUBSTRING(@inSSN, 4,2) 
		SET @third_part  = SUBSTRING(@inSSN, 6,4) 
	
	SELECT @tempval =  @first_part   + '-' + @second_part  + '-'+  @third_part 
	END

RETURN @tempval

END
GO
PRINT N'Creating [dbo].[GetMonthYearNumber]'
GO
SET ANSI_NULLS OFF
GO
CREATE FUNCTION [dbo].[GetMonthYearNumber] (@inDate as DATETIME)  
RETURNS VARCHAR(20)
AS  
BEGIN 
	DECLARE @tempint AS INT
	DECLARE @temptxt AS VARCHAR(2)

	SET @tempint = MONTH(@inDATE)

	IF @tempint < 10
	BEGIN
		SET @temptxt = '0' + CAST(@tempint AS VARCHAR) 
	END
	ELSE
	BEGIN
		SET @temptxt = CAST(@tempint AS VARCHAR) 
	END

RETURN @temptxt  + '/'+ CAST(YEAR(@inDATE) AS VARCHAR)
END
GO
PRINT N'Creating [dbo].[GetMonthYear]'
GO
CREATE FUNCTION [dbo].[GetMonthYear] (@inDate as DATETIME)  
RETURNS VARCHAR(20)
AS  
BEGIN 
--DECLARE @temp AS VARCHAR(20)
RETURN DATENAME(MONTH, @inDATE) + ' '+ CAST(YEAR(@inDATE) AS VARCHAR)
END
GO
PRINT N'Creating [dbo].[GetRate]'
GO
SET ANSI_NULLS ON
GO
/*
* ==============================================================================

Syntax

Arguments

Return Types
* ==============================================================================
*/
CREATE FUNCTION [dbo].[GetRate] (@Param1 float, @Param2 float)
RETURNS varchar(40) AS  
BEGIN 
declare @Return varchar(40)
--SELECT @Return = CAST(CAST(((@Param1/(@Param2*100.00) AS DECIMAL(8,2)) AS VARCHAR(12)) + '%' 

IF @Param2 > 0
	BEGIN
		SELECT @Return = CAST(CAST((@Param1/@Param2*1.00) AS DECIMAL(38,2)) AS VARCHAR(255)) 
	END
ELSE
		SELECT @Return = 0
--SELECT @Return = CAST((@Param1/@Param2*100.00) AS VARCHAR(12))

--SELECT @Return = @Param1/@Param2*100.00

return @Return 
END
GO
PRINT N'Creating [dbo].[GetWorkingDays]'
GO
SET QUOTED_IDENTIFIER ON
GO
/*
* ==============================================================================
GetWorkingDays
Returns the number of working days between two dates (not including these dates).

Syntax
GetWorkingDays ( StartDate, EndDate )

Arguments
StartDate - the DATETIME value (start date).
EndDate - the DATETIME value (end date).

Return Types
INT
* ==============================================================================
*/
CREATE FUNCTION [dbo].[GetWorkingDays]
  ( @StartDate DATETIME,
    @EndDate DATETIME )
RETURNS INT
AS

BEGIN
  DECLARE @WorkDays INT, @FirstPart INT
  DECLARE @FirstNum INT, @TotalDays INT
  DECLARE @LastNum INT, @LastPart INT
  IF (DATEDIFF(day, @StartDate, @EndDate) < 2)
    BEGIN
      RETURN ( 0 )
    END
  SELECT
   @TotalDays = DATEDIFF(day, @StartDate, @EndDate) - 1,
   @FirstPart = CASE DATENAME(weekday, @StartDate)
                 WHEN 'Sunday' THEN 6
                 WHEN 'Monday' THEN 5
                 WHEN 'Tuesday' THEN 4
                 WHEN 'Wednesday' THEN 3
                 WHEN 'Thursday' THEN 2
                 WHEN 'Friday' THEN 1
                 WHEN 'Saturday' THEN 0
               END,
   @FirstNum = CASE DATENAME(weekday, @StartDate)
                 WHEN 'Sunday' THEN 5
                 WHEN 'Monday' THEN 4
                 WHEN 'Tuesday' THEN 3
                 WHEN 'Wednesday' THEN 2
                 WHEN 'Thursday' THEN 1
                 WHEN 'Friday' THEN 0
                 WHEN 'Saturday' THEN 0
               END
  IF (@TotalDays < @FirstPart)
     BEGIN
       SELECT @WorkDays = @TotalDays
     END
  ELSE
     BEGIN
       SELECT @WorkDays = (@TotalDays - @FirstPart) / 7
       SELECT @LastPart = (@TotalDays - @FirstPart) % 7
       SELECT @LastNum = CASE
         WHEN (@LastPart < 7) AND (@LastPart > 0) THEN @LastPart - 1
         ELSE 0
       END
       SELECT @WorkDays = @WorkDays * 5 + @FirstNum + @LastNum
     END
  RETURN ( @WorkDays )
END
GO
PRINT N'Creating [dbo].[GetYearMonthNumber]'
GO
SET QUOTED_IDENTIFIER OFF
GO
SET ANSI_NULLS OFF
GO
/*
select dbo.GetYearMonthNumber('11/01/2006')
select dbo.GetYearMonthNumber('01/31/2007')
*/
CREATE FUNCTION [dbo].[GetYearMonthNumber] (@inDate as DATETIME)  
RETURNS VARCHAR(20)
AS  
BEGIN 
	DECLARE @tempint AS INT
	DECLARE @temptxt AS VARCHAR(2)

	SET @tempint = MONTH(@inDATE)

	IF @tempint < 10
	BEGIN
		SET @temptxt = '0' + CAST(@tempint AS VARCHAR) 
	END
	ELSE
	BEGIN
		SET @temptxt = CAST(@tempint AS VARCHAR) 
	END

RETURN   CAST(YEAR(@inDATE)  AS VARCHAR)+ '' + @temptxt
END
GO
PRINT N'Creating [dbo].[INITCAPS]'
GO
CREATE   FUNCTION [dbo].[INITCAPS] (@inString VARCHAR(5000) ) 
/*  INITCAP returns char, with the first letter of each word capitalized  */
RETURNS VARCHAR(5000)
as
BEGIN
DECLARE @i INT, @c CHAR(1), @result VARCHAR(255)
SET @result=LOWER(@inString)
SET @i=2
SET @result=STUFF(@result,1,1,UPPER(SUBSTRING(@inString,1,1)))
WHILE @i<=LEN(@inString)
 BEGIN
 SET @c=SUBSTRING(@inString,@i,1)
 IF (@c=' ') OR (@c=';') OR (@c=':') OR (@c='!') OR (@c='?') OR (@c=',')OR (@c='.')OR (@c='_')
  IF @i<LEN(@inString)
   BEGIN
   SET @i=@i+1
   SET @result=STUFF(@result,@i,1,UPPER(SUBSTRING(@inString,@i,1)))
   END
 SET @i=@i+1
 END
RETURN  @result
END
GO
PRINT N'Creating [dbo].[IsSaturday]'
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_NULLS ON
GO

CREATE FUNCTION [dbo].[IsSaturday]
  ( @fDate DATETIME )
RETURNS bit
AS
BEGIN
	IF DATEPART(dw, @fDate)= 7
		RETURN 1
	
	RETURN 0
END
GO
PRINT N'Creating [dbo].[IsTuesday]'
GO

CREATE FUNCTION [dbo].[IsTuesday]
  ( @fDate DATETIME )
RETURNS varchar (10)
AS
BEGIN
	IF DATEPART(dw, @fDate)= 3
		RETURN 'TRUE'
	
	RETURN 'FALSE'
END
GO
PRINT N'Creating [dbo].[LastMonthDay]'
GO
/*
* ==============================================================================
LastMonthDay
Returns the last day of the month for the given date.

Syntax
LastMonthDay ( date )

Arguments
date - DATETIME value.

Return Types
DATETIME
* ==============================================================================
*/
CREATE FUNCTION [dbo].[LastMonthDay]
  ( @Date DATETIME )
RETURNS DATETIME
AS
BEGIN
RETURN (CASE WHEN MONTH(@Date)= 12
THEN DATEADD(day,-1,CAST('01/01/'+STR(YEAR(@Date)+1) AS DATETIME))
ELSE DATEADD(day,-1,CAST(STR(MONTH(@Date)+1)+'/01/'+STR(YEAR(@Date)) AS DATETIME))
END)
END
GO
PRINT N'Creating [dbo].[LoginName]'
GO
SET QUOTED_IDENTIFIER OFF
GO
/*
* ==============================================================================
* Function:  getLoginName
*
* Returns:   Name of the logged-in user. If Windows-Authentication is used,
*            the domain-part is truncated (e.g. \\SERVER\msrauser ==> msrauser)
* ==============================================================================
*/

CREATE FUNCTION [dbo].[LoginName]()
RETURNS VARCHAR(15)
AS
BEGIN
--    RETURN SYSTEM_USER
    RETURN RIGHT(SYSTEM_USER,(LEN(SYSTEM_USER)-CHARINDEX('\',SYSTEM_USER)))
END
GO
PRINT N'Creating [dbo].[MonthYearText]'
GO
SET QUOTED_IDENTIFIER ON
GO
/*
* ==============================================================================

Syntax

Arguments

Return Types
* ==============================================================================
*/
CREATE FUNCTION [dbo].[MonthYearText] (@inDate as DATETIME)  
RETURNS VARCHAR(20)
AS  
BEGIN 
--DECLARE @temp AS VARCHAR(20)
RETURN DATENAME(MONTH, @inDATE) + ' '+ CAST(YEAR(@inDATE) AS VARCHAR)
END
GO
PRINT N'Creating [dbo].[RemoveTimePartToStringDate]'
GO
CREATE FUNCTION [dbo].[RemoveTimePartToStringDate] (@inDate datetime)  
RETURNS VARCHAR(10) AS  
BEGIN 
DECLARE @tempval VARCHAR(10)
DECLARE @return VARCHAR(10)

DECLARE @day VARCHAR(2)
DECLARE @month VARCHAR(2)
DECLARE @year VARCHAR(4)

SET @day = DATEPART(day, @inDate)
SET @month = DATEPART(month, @inDate)
SET @year = DATEPART(year, @inDate)

SELECT @return =  @month  + '/' + @day + '/'+ @year

RETURN @return

END
GO
PRINT N'Creating [dbo].[RemoveTimePart]'
GO
SET QUOTED_IDENTIFIER OFF
GO
SET ANSI_NULLS OFF
GO
CREATE FUNCTION [dbo].[RemoveTimePart] (@inDate datetime)  
RETURNS DATETIME AS  
BEGIN 
DECLARE @tempval VARCHAR(10)
DECLARE @return DATETIME

DECLARE @day VARCHAR(2)
DECLARE @month VARCHAR(2)
DECLARE @year VARCHAR(4)

SET @day = DATEPART(day, @inDate)
SET @month = DATEPART(month, @inDate)
SET @year = DATEPART(year, @inDate)

SELECT @tempval =  @month  + '/' + @day + '/'+ @year

SELECT @Return = CAST(@tempval AS DATETIME)

RETURN @Return 

END
GO
PRINT N'Creating [dbo].[ReplaceStr]'
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_NULLS ON
GO
CREATE FUNCTION [dbo].[ReplaceStr] (@inStr VARCHAR(50), @searchStr VARCHAR(50), @replaceStr VARCHAR(50))  
RETURNS VARCHAR(50) AS  
BEGIN 
DECLARE @return VARCHAR(50)


IF @inStr = @searchStr 
	SET @Return = @replaceStr
ELSE
	SET @Return = @inStr 


RETURN @Return 

END
GO
PRINT N'Creating [dbo].[Sat]'
GO

CREATE FUNCTION [dbo].[Sat]
  ( @fDate DATETIME )
RETURNS bit
AS
BEGIN
	IF DATEPART(dw, @fDate)= 7
		RETURN 1
	
	RETURN 0
END
GO
PRINT N'Creating [dbo].[ShortDateSlashes]'
GO
SET QUOTED_IDENTIFIER OFF
GO
SET ANSI_NULLS OFF
GO
CREATE FUNCTION [dbo].[ShortDateSlashes] (@inDate DATETIME)  
RETURNS char(10) AS  
BEGIN 
DECLARE @return char(10)
SELECT @return = CONVERT(char(10), @inDate, 101)
RETURN @return
END
GO
PRINT N'Creating [dbo].[ShortDate]'
GO
SET ANSI_NULLS ON
GO
/*
* ==============================================================================

Syntax

Arguments

Return Types
* ==============================================================================
*/
CREATE FUNCTION [dbo].[ShortDate] (@inDate DATETIME)  
RETURNS SMALLDATETIME AS  
BEGIN 

DECLARE @return SMALLDATETIME
DECLARE @temp char(12)

SELECT @temp = CONVERT(char(12), @inDate, 10)

SELECT @return = CAST(@temp AS SMALLDATETIME)

RETURN @return

END
GO
PRINT N'Creating [dbo].[ShortLoginName]'
GO
/*
* ==============================================================================
* Function:  getShortLoginName
*
* Returns:   Name of the logged-in user. If Windows-Authentication is used,
*            the domain-part is truncated (e.g. \\SERVER\msrauser ==> msrauser)
* ==============================================================================
*/
CREATE FUNCTION [dbo].[ShortLoginName]()
RETURNS VARCHAR(15)
AS
BEGIN
--    RETURN SYSTEM_USER
    RETURN RIGHT(SYSTEM_USER,(LEN(SYSTEM_USER)-CHARINDEX('\',SYSTEM_USER)))
END
GO
PRINT N'Creating [dbo].[StringShortDate]'
GO
SET ANSI_NULLS OFF
GO
/*
* ==============================================================================

Syntax

Arguments

Return Types
* ==============================================================================
*/
CREATE FUNCTION [dbo].[StringShortDate] (@inDate DATETIME)  
RETURNS VARCHAR(10) AS  
BEGIN 

DECLARE @return VARCHAR(10)
DECLARE @temp char(10)

SELECT @temp = CONVERT(char(12), @inDate, 101)

--SELECT @return = CAST(@temp AS SMALLDATETIME)

RETURN @temp

END
GO
PRINT N'Creating [dbo].[TimePart]'
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_NULLS ON
GO
/*
* ==============================================================================
TimePart
Returns the time part of the DATETIME value.

Syntax
TimePart ( datepart )

Arguments
datepart - DATETIME value.

Return Types
VARCHAR
* ==============================================================================
*/
CREATE FUNCTION [dbo].[TimePart]
  ( @fDate DATETIME )
RETURNS VARCHAR(10)
AS
BEGIN
  RETURN ( CONVERT(VARCHAR(7),right(@fDate,7),101) )
END
GO
PRINT N'Creating [dbo].[Tues]'
GO

CREATE FUNCTION [dbo].[Tues]
  ( @fDate DATETIME )
RETURNS bit
AS
BEGIN
	IF DATEPART(dw, @fDate)= 3
		RETURN 1
	
	RETURN 0
END
GO
PRINT N'Creating [dbo].[WeekDay]'
GO
SET ANSI_NULLS OFF
GO
/*
* ==============================================================================
WeekDay
Returns the Weekday of a date.

Syntax
WeekDay( datepart )

Arguments
datepart - DATETIME value.

Return Types
VARCHAR
* ==============================================================================
*/
CREATE FUNCTION [dbo].[WeekDay]
  ( @fDate DATETIME )
RETURNS VARCHAR(9)
AS
BEGIN
  RETURN DATENAME(Dw,  @fDate)
END
GO
PRINT N'Creating [dbo].[getCustomStringDate]'
GO
SET QUOTED_IDENTIFIER OFF
GO
CREATE FUNCTION [dbo].[getCustomStringDate] (@inDate DATETIME)  
RETURNS varchar(30) AS  
BEGIN 
DECLARE @return varchar(30)
DECLARE @day varchar(2)
DECLARE @month  varchar(2)
SET @day = CAST(DAY(@inDate) AS VARCHAR)
SET @month = CAST(MONTH(@inDate) AS VARCHAR)

IF @day < 10 
SET @day = '0' + @day

IF @month < 10 
SET @month = '0' + @month


--SELECT @Return =  CAST(YEAR(@inDate) AS VARCHAR) + '' + CAST(MONTH(@inDate) AS VARCHAR) + '' + CAST(DAY(@inDate) AS VARCHAR)

SELECT @Return =  CAST(YEAR(@inDate) AS VARCHAR) + '' + @month + '' + @day

--SELECT @return = CONVERT(varchar(30), @inDate, 120)
RETURN @return
END
GO
PRINT N'Creating [dbo].[getCustomStringYearMonth]'
GO
CREATE FUNCTION [dbo].[getCustomStringYearMonth] (@inDate DATETIME)  
RETURNS varchar(30) AS  
BEGIN 
DECLARE @return varchar(30)
DECLARE @day varchar(2)
DECLARE @month  varchar(2)
SET @day = CAST(DAY(@inDate) AS VARCHAR)
SET @month = CAST(MONTH(@inDate) AS VARCHAR)

IF @day < 10 
SET @day = '0' + @day

IF @month < 10 
SET @month = '0' + @month


--SELECT @Return =  CAST(YEAR(@inDate) AS VARCHAR) + '' + CAST(MONTH(@inDate) AS VARCHAR) + '' + CAST(DAY(@inDate) AS VARCHAR)

SELECT @Return =  CAST(YEAR(@inDate) AS VARCHAR) + '' + @month + '' 

--SELECT @return = CONVERT(varchar(30), @inDate, 120)
RETURN @return
END
GO
PRINT N'Creating [dbo].[getLastScanDate]'
GO
CREATE FUNCTION [dbo].[getLastScanDate] ()  
RETURNS varchar(30) AS  
BEGIN 
DECLARE @return varchar(30)

--SELECT LAST_SCAN_DATE INTO @return FROM vw_LastScanDate
SET @return  = (SELECT LAST_SCAN_DATE FROM vw_LastScanDate)

--SELECT @return = CONVERT(varchar(30), @inDate, 120)
RETURN @return

--RETURN (SELECT LAST_SCAN_DATE FROM vw_LastScanDate)
END
GO
PRINT N'Creating [dbo].[getLoginName]'
GO
/*
* ==============================================================================
* Function:  getLoginName
*
* Returns:   Name of the logged-in user. If Windows-Authentication is used,
*            the domain-part is truncated (e.g. \\SERVER\msrauser ==> msrauser)
* ==============================================================================
*/
CREATE FUNCTION [dbo].[getLoginName]()
RETURNS VARCHAR(15)
AS
BEGIN
--    RETURN SYSTEM_USER
    RETURN RIGHT(SYSTEM_USER,(LEN(SYSTEM_USER)-CHARINDEX('\',SYSTEM_USER)))
END
GO
PRINT N'Creating [dbo].[toDecimal]'
GO
/*
* ==============================================================================

Syntax

Arguments

Return Types
* ==============================================================================
*/
CREATE FUNCTION [dbo].[toDecimal] (@Param1 varchar(30)) RETURNS DECIMAL(38,2) AS  
BEGIN 
declare @Return varchar(40)
--SELECT @Return = CAST(CAST(((@Param1/(@Param2*100.00) AS DECIMAL(8,2)) AS VARCHAR(12)) + '%' 


SELECT @Return = CAST(@Param1 AS DECIMAL(38,2))

return @Return 
END
GO
PRINT N'Creating [dbo].[udf_LocalToUTCDatetime]'
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_NULLS ON
GO
-- =============================================
-- Author:		<Author,,Name>
-- Create date: <Create Date, ,>
-- Description:	<Description, ,>
-- =============================================
CREATE FUNCTION [dbo].[udf_LocalToUTCDatetime]
(
	-- Add the parameters for the function here
	@LocalDate DATETIME
)
RETURNS DATETIME  
AS
BEGIN

	-- Add the T-SQL statements to compute the return value here
	RETURN CAST (DATEADD(MILLISECOND,DATEDIFF(MILLISECOND,GETDATE(),GETUTCDATE()),@LocalDate) AS DATETIME)


END
GO
PRINT N'Creating [dbo].[udf_USCenturyZeroFilledMonthDatetime]'
GO


-- =============================================
-- Author:		<Author,,Name>
-- Create date: <Create Date, ,>
-- Description:	<Description, ,>
-- =============================================
CREATE FUNCTION [dbo].[udf_USCenturyZeroFilledMonthDatetime]
(
	-- Add the parameters for the function here
	@USCenturyDatetime DATETIME
)
RETURNS  VARCHAR(30)  
AS
BEGIN

	DECLARE @AM_PM VARCHAR(20) = (SELECT CONCAT(CONVERT(VARCHAR(12), @USCenturyDatetime,101), RIGHT(CONVERT(VARCHAR(20), @USCenturyDatetime, 22),12))) 

		-- Add the T-SQL statements to compute the return value here
	--RETURN CONCAT(CONVERT(VARCHAR(12), @USCenturyDatetime,101),' ', RIGHT(CONVERT(VARCHAR(20), @USCenturyDatetime, 120),8),' ',RIGHT(CONVERT(VARCHAR(20), @USCenturyDatetime, 22),2))
	RETURN CASE WHEN SUBSTRING(@AM_PM,11,4) LIKE '  [0-9]:' THEN REPLACE(@AM_PM,'  ', ' 0') ELSE @AM_PM END

END
GO
PRINT N'Creating [dbo].[udf_UTCDatetimeToLocal]'
GO
-- =============================================
-- Author:		<Author,,Name>
-- Create date: <Create Date, ,>
-- Description:	<Description, ,>
-- =============================================
CREATE FUNCTION [dbo].[udf_UTCDatetimeToLocal]
(
	-- Add the parameters for the function here
	@UTCDate DATETIME
)
RETURNS DATETIME  
AS
BEGIN

	-- Add the T-SQL statements to compute the return value here
	RETURN CAST (DATEADD(MILLISECOND,DATEDIFF(MILLISECOND,GETUTCDATE(),GETDATE()),@UTCDate) AS DATETIME)
	
END
GO
PRINT N'Creating [dbo].[udf_getCustomMessage]'
GO
/*
* ==============================================================================

Syntax

Arguments

Return Types
* ==============================================================================
*/
CREATE FUNCTION [dbo].[udf_getCustomMessage] (@message_id INT)  
RETURNS VARCHAR(400) AS  
BEGIN 
DECLARE @return VARCHAR(400)

SELECT @return = text FROM sys.messages  WHERE message_id = @message_id

Return @return
END
GO
PRINT N'Creating [dbo].[MigrationDBs]'
GO
CREATE TABLE [dbo].[MigrationDBs]
(
[name] [sys].[sysname] NOT NULL,
[dbid] [smallint] NULL,
[ToBackUp] [varchar] (1) NOT NULL,
[Restored] [varchar] (1) NOT NULL,
[BackupCode] [nvarchar] (584) NOT NULL
)
GO
PRINT N'Creating [dbo].[trackdownCDCissue]'
GO
CREATE TABLE [dbo].[trackdownCDCissue]
(
[RowNumber] [int] NOT NULL IDENTITY(0, 1),
[EventClass] [int] NULL,
[TextData] [ntext] NULL,
[DatabaseID] [int] NULL,
[DatabaseName] [nvarchar] (128) NULL,
[ObjectID] [int] NULL,
[ObjectName] [nvarchar] (128) NULL,
[ServerName] [nvarchar] (128) NULL,
[BinaryData] [image] NULL,
[SPID] [int] NULL,
[StartTime] [datetime] NULL,
[TextData_22] [nvarchar] (max) NULL,
[BinaryData_22] [varbinary] (max) NULL
)
GO
PRINT N'Creating primary key [PK__trackdow__AAAC09D89C7C6E13] on [dbo].[trackdownCDCissue]'
GO
ALTER TABLE [dbo].[trackdownCDCissue] ADD CONSTRAINT [PK__trackdow__AAAC09D89C7C6E13] PRIMARY KEY CLUSTERED  ([RowNumber])
GO
PRINT N'Creating [dbo].[SRA_Backup_MdTimeDB]'
GO
CREATE PROCEDURE [dbo].[SRA_Backup_MdTimeDB]
	
AS
BEGIN

	DECLARE @Local_DBBackupName			nvarchar(2000),
			@Local_BackupDataCommand	nvarchar(4000)


	SELECT @Local_DBBackupName  =  'MdTime' + '-' 
			+ DATENAME(month, getdate()) + '-' 
			+ DATENAME(day, getdate()) + '-' 
			+ DATENAME(year, getdate()) + '-'
			+ DATENAME(weekday, getdate()) + '-'	
			+ DATENAME(hour, getdate()) + '-'
			+ DATENAME(minute, getdate()) + '-'
			+ DATENAME(second, getdate()) + '-'
			+ RIGHT(CONVERT(CHAR(19),getdate()) , 2) + '-'
			+ DATENAME(millisecond, getdate())
			+ '.bak'

	SET @Local_BackupDataCommand = 'BACKUP DATABASE [' + 'MdTime' + '] TO DISK = ' + '''' + N'H:\MdTimeBackup\LatestBackup\' + @Local_DBBackupName + ''''
	Exec sp_executesql @Local_BackupDataCommand -- Backup the database.

END
GO
PRINT N'Creating [dbo].[SRA_JobBackup_MSRA_DROP]'
GO
CREATE PROCEDURE [dbo].[SRA_JobBackup_MSRA_DROP]
	
AS
BEGIN

	DECLARE @Local_DBBackupName			nvarchar(2000),
			@Local_BackupDataCommand	nvarchar(4000)

	WAITFOR DELAY '00:00:05';
	SELECT @Local_DBBackupName  =  'MSRA_DROP' + '-' 
			+ DATENAME(month, getdate()) + '-' 
			+ DATENAME(day, getdate()) + '-' 
			+ DATENAME(year, getdate()) + '-'
			+ DATENAME(weekday, getdate()) + '-'	
			+ DATENAME(hour, getdate()) + '-'
			+ DATENAME(minute, getdate()) + '-'
			+ DATENAME(second, getdate()) + '-'
			+ RIGHT(CONVERT(CHAR(19),getdate()) , 2) + '-'
			+ DATENAME(millisecond, getdate())
			+ '.bak'

	SET @Local_BackupDataCommand = 'BACKUP DATABASE [' + 'MSRA_DROP' + '] TO DISK = ' + '''' + N'\\srabaldd1\backup\SRA\SQL2\MSRA_DROP_JobBackup\' + @Local_DBBackupName + '''' + ' WITH COPY_ONLY'
	Exec sp_executesql @Local_BackupDataCommand -- Backup the database.


	/*RESTORE DATABASE [MSRA_DROP_FromProd_March15_2011]
	FROM  DISK = N'H:\COPIED_FROM_SQL2\MSRA_DROP-March-15-2011-Tuesday-14-25-6-PM-283.bak' WITH  FILE = 1,
	MOVE N'MSRA_DROP_Data' TO N'H:\SQLServerData\MSRA_DROP_March15.mdf',
	MOVE N'MSRA_DROP_Log' TO N'F:\SQLServerDatabaseLog\MSRA_DROP_March15.ldf',
	NOUNLOAD,  STATS = 10
	GO*/
END
GO
PRINT N'Creating [dbo].[SRA_SendDiskDriveSpace]'
GO
CREATE PROCEDURE [dbo].[SRA_SendDiskDriveSpace]
	
AS
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	-- interfering with SELECT statements.
	SET NOCOUNT ON;

	DECLARE @Local_EMail_Body			varchar(8000),
			@Local_DriveLetter			varchar(1255),
			@Local_DriveSpaceInMegs		bigint,
			@Local_DriveSpaceInGigs		decimal(9,2),
			@Local_PrimaryKeyID			int,
			@Local_EMail_Subject		varchar(255),
			@LOCAL_THRESHOLD			int,			
			@Local_Email_To_Send_To		varchar(8000),
			@LOCAL_PROFILE_NAME			varchar(255)

	-- Populate GLOBAL variables.
	SET @LOCAL_THRESHOLD = 3000	
	SET @LOCAL_PROFILE_NAME = 'SQLMail'
	--SELECT @Local_Email_To_Send_To = COALESCE(@Local_Email_To_Send_To + ';', '') + AdminEmail 
	--FROM UTIL.dbo.tblAdminEMail	
	SET @Local_Email_To_Send_To = 'databaseadmins@sra.state.md.us'

	-- Dropping Temp Table.
	IF EXISTS (
		SELECT  *
		FROM 	tempdb.dbo.sysobjects o
		WHERE
			o.xtype in ('U')	and
			o.id = object_id( N'tempdb..'+'#DiskDriveSpace')
		)
		BEGIN
			DROP TABLE #DiskDriveSpace
		END
	-- Creating the Temp Table.
	CREATE TABLE #DiskDriveSpace(
			ListOfDatabases_DriveLetter			varchar(1255),		
			ListOfDatabases_DriveSpaceInMegs	bigint,
			ListOfDatabases_DriveSpaceInGigs	decimal(9,2)
		)

	-- Populate the #DiskDriveSpace variable.
	INSERT INTO #DiskDriveSpace(ListOfDatabases_DriveLetter,
								ListOfDatabases_DriveSpaceInMegs)
	Exec master..xp_fixeddrives
	
	UPDATE #DiskDriveSpace
	SET ListOfDatabases_DriveSpaceInGigs = ( CONVERT(decimal(9,2),ISNULL(ListOfDatabases_DriveSpaceInMegs, 0))/1024)

	-- Populate the @Local_EMail_Body variable.
	SET @Local_EMail_Body = 'Database Server name: ' + @@Servername + Char(13) + Char(10) + 'Space Left on '
	SELECT @Local_EMail_Body =	CASE @Local_EMail_Body 
								WHEN '' THEN ListOfDatabases_DriveLetter 
								ELSE	@Local_EMail_Body + Char(13) + Char(10) + 
										ListOfDatabases_DriveLetter + ' drive: ' + 
										CONVERT(varchar(255), ISNULL(ListOfDatabases_DriveSpaceInMegs, '0')) + ' Megs / ' +
										CONVERT(varchar(255), ISNULL(ListOfDatabases_DriveSpaceInGigs, '0')) + ' Gigs.'
							END
	FROM #DiskDriveSpace

	-- Populate the @Local_EMail_Subject variable based on @LOCAL_THRESHOLD.
	IF EXISTS (SELECT 1 FROM #DiskDriveSpace WHERE ListOfDatabases_DriveSpaceInMegs <= @LOCAL_THRESHOLD)
		BEGIN
			SET @Local_EMail_Subject = @@SERVERNAME + '_Emergency_Space_Stats'			
		END
	ELSE
		BEGIN
			SET @Local_EMail_Subject = @@SERVERNAME + '_Regular_Space_Stats'			
		END

	-- Send the actual email.		
	EXEC msdb.dbo.sp_send_dbmail
    @profile_name	= @LOCAL_PROFILE_NAME,
    @recipients		= @Local_Email_To_Send_To,
    @body			= @Local_EMail_Body,
    @subject		= @Local_EMail_Subject
	
END
GO
PRINT N'Creating [dbo].[SRA_ShrinkTransactionLog]'
GO
CREATE PROCEDURE [dbo].[SRA_ShrinkTransactionLog]
	
AS
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	-- interfering with SELECT statements.
	SET NOCOUNT ON;

	DECLARE @Local_Path								nvarchar(1255),
			@Local_BackupDataCommand				nvarchar(1255),
			@Local_BackupTransactionLogCommand		nvarchar(1255),
			@Local_ShrinkFileCommand				nvarchar(1255),
			@Local_DatabaseName						nvarchar(1255),
			@Local_SysFilesString					nvarchar(1255),
			@Local_LogFileName						nvarchar(1255),
			@Local_ServerName						nvarchar(1255),
			@Local_PrimaryKeyID						int,
			@Local_Counter							int

	-- Dropping Temp Table.
	IF EXISTS (
		SELECT  *
		FROM 	tempdb.dbo.sysobjects o
		WHERE
			o.xtype in ('U')	and
			o.id = object_id( N'tempdb..'+'#ListOfDatabases')
		)
		BEGIN
			DROP TABLE #ListOfDatabases
		END
	-- Creating the Temp Table.
	CREATE TABLE #ListOfDatabases(
			PrimaryKeyID					Int Identity (1, 1),			
			ListOfDatabases_DatabaseName	nvarchar(1255)
		)

	DECLARE @shrinktablevar TABLE(
				shrinktablevar_logfilename nvarchar(1255)
			)

	-- Populating the @Local_ServerName and @Local_Path variables.
	SET @Local_ServerName = @@SERVERNAME
	IF @Local_ServerName IN('DEV3','TESTSQL1','SQL2','SRABALSQL2')
		BEGIN
			SET @Local_Path = 'H:\ShrinkTransaction\'
		END
	ELSE IF @Local_ServerName IN('INETSQL1')
		BEGIN
			SET @Local_Path = 'E:\ShrinkTransaction\'
		END

	IF ((@Local_Path IS NULL) OR (@Local_Path = ''))
		BEGIN
			RAISERROR('@Local_Path IS NULL', 16, 1)
			RETURN
		END

	-- Deleting existing bak and trn files from the @Local_Path folder.
	EXECUTE master.dbo.xp_delete_file 0,@Local_Path,N'bak'
	EXECUTE master.dbo.xp_delete_file 0,@Local_Path,N'trn'

	-- Populating the #ListOfDatabases temp table with a list
	-- of databases.
	INSERT INTO #ListOfDatabases(ListOfDatabases_DatabaseName)
	
	SELECT name FROM master.sys.databases
	WHERE	recovery_model = 1 AND 
			state_desc = 'ONLINE' AND
			is_read_only = 0 AND
	name NOT IN('tempdb')				
	ORDER BY name ASC

	/*DELETE FROM #ListOfDatabases
	WHERE ListOfDatabases_DatabaseName IN(	SELECT DatabaseName 
											FROM UTIL.dbo.tblFullBackupAndDR
											WHERE DR  = 1
										)*/

	-- Looping through the databases and backing up the data and transaction log
	-- and shrinking the transaction log.
	SELECT @Local_PrimaryKeyID = MIN(PrimaryKeyID)
	FROM #ListOfDatabases
	WHILE @Local_PrimaryKeyID IS NOT NULL
	BEGIN -- WHILE @Local_PrimaryKeyID IS NOT NULL
		SELECT	@Local_BackupDataCommand			= NULL,
				@Local_BackupTransactionLogCommand	= NULL,
				@Local_SysFilesString				= NULL,
				@Local_DatabaseName					= NULL

		SELECT 	@Local_DatabaseName	= ListOfDatabases_DatabaseName
		FROM #ListOfDatabases
		WHERE PrimaryKeyID = @Local_PrimaryKeyID

		SET @Local_BackupDataCommand = 'BACKUP DATABASE [' + @Local_DatabaseName + '] TO DISK = ' + '''' + @Local_Path + @Local_DatabaseName + '_Data.bak' + ''''
		SET @Local_BackupTransactionLogCommand = 'BACKUP LOG [' + @Local_DatabaseName + '] TO DISK = ' + '''' + @Local_Path + @Local_DatabaseName + '_Log.trn' + ''''
		
		SET @Local_SysFilesString = 'USE [' + @Local_DatabaseName + '] SELECT name FROM sysfiles WHERE fileid = 2'
		DELETE FROM @shrinktablevar
		INSERT into @shrinktablevar
		Exec (@Local_SysFilesString)
		SELECT @Local_LogFileName = shrinktablevar_logfilename FROM @shrinktablevar
		SET @Local_ShrinkFileCommand = 'USE [' + @Local_DatabaseName + '] DBCC SHRINKFILE ([' + @Local_LogFileName + '])'
		
		Exec (@Local_BackupDataCommand)				-- Backup the database.
		Exec (@Local_BackupTransactionLogCommand)	-- Backup the transacton log.
		Exec (@Local_ShrinkFileCommand)				-- Shrink the transaction log.
		Exec (@Local_ShrinkFileCommand)				-- Shrink the transaction log.
		Exec (@Local_ShrinkFileCommand)				-- Shrink the transaction log.


		-- Deleting existing bak and trn files from the @Local_Path folder.
		EXECUTE master.dbo.xp_delete_file 0,@Local_Path,N'bak'
		EXECUTE master.dbo.xp_delete_file 0,@Local_Path,N'trn'

		IF  @Local_DatabaseName IN ('SharePoint_Config',
									'MSRAPGUINBOUND',
									'SLFileAuditor',
									'SharePoint_WSS_Search_APP1',
									'FINQ',
									'VRDB',
									'SharedServices3_Search_DB')
			BEGIN
				Exec (@Local_BackupDataCommand)				-- Backup the database.
				Exec (@Local_BackupTransactionLogCommand)	-- Backup the transacton log.
				Exec (@Local_ShrinkFileCommand)				-- Shrink the transaction log.
				Exec (@Local_ShrinkFileCommand)				-- Shrink the transaction log.
				Exec (@Local_ShrinkFileCommand)				-- Shrink the transaction log.
			END

		IF  @Local_DatabaseName IN ('SharedServices3_Search_DB')
			BEGIN
				SET @Local_Counter = 1

				WHILE @Local_Counter <= 4
					BEGIN
						-- Deleting existing bak and trn files from the @Local_Path folder.
						EXECUTE master.dbo.xp_delete_file 0,@Local_Path,N'bak'
						EXECUTE master.dbo.xp_delete_file 0,@Local_Path,N'trn'

						Exec (@Local_BackupDataCommand)				-- Backup the database.
						Exec (@Local_BackupTransactionLogCommand)	-- Backup the transacton log.
						Exec (@Local_ShrinkFileCommand)				-- Shrink the transaction log.
						Exec (@Local_ShrinkFileCommand)				-- Shrink the transaction log.
						Exec (@Local_ShrinkFileCommand)				-- Shrink the transaction log.

						SET @Local_Counter = @Local_Counter + 1
					END	

				-- Just shrink two more times to make sure..
				Exec (@Local_ShrinkFileCommand)				-- Shrink the transaction log.
				Exec (@Local_ShrinkFileCommand)				-- Shrink the transaction log.			
			END

	 SELECT @Local_PrimaryKeyID = MIN(PrimaryKeyID)
			FROM #ListOfDatabases
			WHERE PrimaryKeyID > @Local_PrimaryKeyID
	END -- WHILE @Local_PrimaryKeyID IS NOT NULL


END
GO
PRINT N'Creating [dbo].[ShowOrphanUsers]'
GO
/* 
use DeceasedTracking
EXEC sp_change_users_login 'Auto_Fix', 'login'
EXEC sp_change_users_login 'Update_One', 'existing_login', 'new_login'

USE UTIL
exec dbo.ShowOrphanUsers
*/

--USE UTIL
CREATE PROC [dbo].[ShowOrphanUsers]
AS
BEGIN
	CREATE TABLE #Results
	(
		[Database Name] sysname COLLATE Latin1_General_CI_AS, 
		[Orphaned User] sysname COLLATE Latin1_General_CI_AS
	)

	SET NOCOUNT ON	

	DECLARE @DBName sysname, @Qry nvarchar(4000)

	SET @Qry = ''
	SET @DBName = ''

	WHILE @DBName IS NOT NULL
	BEGIN
		SET @DBName = 
				(
					SELECT MIN(name) 
					FROM master..sysdatabases 
					WHERE 	name NOT IN 
						(
						 'master', 'model', 'tempdb', 'msdb', 
						 'distribution', 'pubs', 'northwind'
						)
						AND DATABASEPROPERTY(name, 'IsOffline') = 0 
						AND DATABASEPROPERTY(name, 'IsSuspect') = 0 
						AND name > @DBName
				)
		
		IF @DBName IS NULL BREAK

		SET @Qry = '	SELECT ''' + @DBName + ''' AS [Database Name], 
				CAST(name AS sysname) COLLATE Latin1_General_CI_AS  AS [Orphaned User]
				FROM ' + QUOTENAME(@DBName) + '..sysusers su
				WHERE su.islogin = 1
				AND su.name <> ''guest''
				AND NOT EXISTS
				(
					SELECT 1
					FROM master..syslogins sl
					WHERE su.sid = sl.sid
				)'

		INSERT INTO #Results EXEC (@Qry)
	END


	SELECT * 
	FROM #Results 
	ORDER BY [Database Name], [Orphaned User]
END
GO
PRINT N'Creating [dbo].[sp_MSRA_send_dbmail]'
GO

CREATE PROCEDURE [dbo].[sp_MSRA_send_dbmail]
   @profile_name               sysname    = NULL,        
   @recipients                 VARCHAR(MAX)  = NULL, 
   @copy_recipients            VARCHAR(MAX)  = NULL,
   @blind_copy_recipients      VARCHAR(MAX)  = NULL,
   @subject                    NVARCHAR(255) = NULL,
   @body                       NVARCHAR(MAX) = NULL, 
   @body_format                VARCHAR(20)   = NULL, 
   @importance                 VARCHAR(6)    = 'NORMAL',
   @sensitivity                VARCHAR(12)   = 'NORMAL',
   @file_attachments           NVARCHAR(MAX) = NULL,  
   @query                      NVARCHAR(MAX) = NULL,
   @execute_query_database     sysname       = NULL,  
   @attach_query_result_as_file BIT          = 0,
   @query_attachment_filename  NVARCHAR(260) = NULL,  
   @query_result_header        BIT           = 1,
   @query_result_width         INT           = 256,            
   @query_result_separator     CHAR(1)       = ' ',
   @exclude_query_output       BIT           = 0,
   @append_query_error         BIT           = 0,
   @query_no_truncate          BIT           = 0,
   @query_result_no_padding    BIT           = 0,
   @mailitem_id               INT            = NULL OUTPUT,
   @from_address               VARCHAR(max)  = NULL,
   @reply_to                   VARCHAR(max)  = NULL
   
 AS 
 
EXECUTE AS LOGIN = 'MSRA\sqlservice';

EXECUTE msdb.dbo.sp_send_dbmail
   @profile_name,        
   @recipients, 
   @copy_recipients,
   @blind_copy_recipients,
   @subject,
   @body, 
   @body_format, 
   @importance,
   @sensitivity,
   @file_attachments,  
   @query,
   @execute_query_database,  
   @attach_query_result_as_file,
   @query_attachment_filename,  
   @query_result_header,
   @query_result_width,            
   @query_result_separator,
   @exclude_query_output,
   @append_query_error,
   @query_no_truncate,
   @query_result_no_padding,
   @mailitem_id,
   @from_address,
   @reply_to
   
 REVERT;
GO
PRINT N'Altering permissions on  [dbo].[INITCAPS]'
GO
GRANT EXECUTE ON  [dbo].[INITCAPS] TO [IntranetHRIS_PRMLogin]
GO
PRINT N'Altering permissions on  [dbo].[sp_MSRA_send_dbmail]'
GO
GRANT EXECUTE ON  [dbo].[sp_MSRA_send_dbmail] TO [IntranetHRIS_PRMLogin]
GO
GRANT EXECUTE ON  [dbo].[sp_MSRA_send_dbmail] TO [MSRA\sqlservice]
GO
GRANT SELECT TO [ImpervaAcct]
