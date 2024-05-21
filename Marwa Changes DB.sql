CREATE OR ALTER  PROCEDURE [dbo].[SP_Supervisor_CloneOperationSchedule]
    @Schedule_Id INT,
    @New_From_Date DATE,
    @New_To_Date DATE,
    @Created_By INT,
    @Result int OUTPUT,
    @Error_Message NVARCHAR(4000) OUTPUT
AS
BEGIN
    SET NOCOUNT ON;

    DECLARE @New_Schedule_Id INT;
    DECLARE @Schedule_Details_Id INT;
    DECLARE @New_Schedule_Details_Id INT;
    DECLARE @User_Id INT;
    DECLARE @Attendance_Type INT;

    BEGIN TRANSACTION;

    BEGIN TRY
        -- Clone the Operation_Schedule (Master Table)
        INSERT INTO [dbo].[Operation_Schedule]
        (
            Station_Id,
            From_Date,
            To_Date,
            Status,
            Created_By,
            Created_Date
           
        )
        SELECT
            Station_Id,
            @New_From_Date,
            @New_To_Date,
            Status,
            @Created_By,
            GETDATE()
          from dbo.Operation_Schedule
        WHERE Schedule_Id = @Schedule_Id;

        -- Get the new Schedule_Id
        SET @New_Schedule_Id = SCOPE_IDENTITY();

        -- Clone the Operation_schedule_details (Dtl for first table)
        DECLARE detail_cursor CURSOR FOR
        SELECT 
            Schedule_Details_Id,
            User_Id,
            Attendance_Type
        FROM [dbo].[Operation_schedule_details]
        WHERE Schedule_Id = @Schedule_Id;

        OPEN detail_cursor;

        FETCH NEXT FROM detail_cursor INTO @Schedule_Details_Id, @User_Id, @Attendance_Type;

        WHILE @@FETCH_STATUS = 0
        BEGIN
            INSERT INTO [dbo].[Operation_schedule_details]
            (
                Schedule_Id,
                User_Id,
                From_Time,
                To_Time,
                Attendance_Type,
                Created_By,
                Created_Date
             
            )
            SELECT
                @New_Schedule_Id,
                User_Id,
                From_Time,
                To_Time,
                Attendance_Type,
                @Created_By,
                GETDATE()
              
            FROM [dbo].[Operation_schedule_details]
            WHERE Schedule_Details_Id = @Schedule_Details_Id;

            -- Get the new Schedule_Details_Id
            SET @New_Schedule_Details_Id = SCOPE_IDENTITY();

            -- Clone the Operation_Schedule_Assignment (Dtl of second)
            INSERT INTO [dbo].[Operation_Schedule_Assignment]
            (
                Schedule_Details_Id,
                Schedule_Id,
                User_Id,
                Lane_Id,
                Section_Id,
                Created_By,
                Created_Date
                
            )
            SELECT
                @New_Schedule_Details_Id,
                @New_Schedule_Id,
                User_Id,
                Lane_Id,
                Section_Id,
                @Created_By,
                GETDATE()
             
            FROM [dbo].[Operation_Schedule_Assignment]
            WHERE Schedule_Details_Id = @Schedule_Details_Id;

            FETCH NEXT FROM detail_cursor INTO @Schedule_Details_Id, @User_Id, @Attendance_Type;
        END

        CLOSE detail_cursor;
        DEALLOCATE detail_cursor;

        COMMIT TRANSACTION;

        -- Set success output
        SET @Result = 1;
    END TRY
    BEGIN CATCH
        ROLLBACK TRANSACTION;
   DECLARE @ErrorNumber INT = ERROR_NUMBER();

        -- Call the function to get the custom error message
        SET @Error_Message = dbo.fn_Core_GetCustomErrorMessage(@ErrorNumber);
        SET @Result = -1; -- Indicate failure
    END CATCH;
END;
GO

/////////////////////////////////////////////////////////
CREATE OR ALTER    PROCEDURE [dbo].[SP_supervisor_GetScheduleOperationDetails] 
	@stationId int,
	@ScheduleDate date,
	@Result int out,
	@Error_message NVARCHAR(MAX) out
AS
BEGIN
	-- SELECT 1 33 333
	SET NOCOUNT ON;
	BEGIN TRY
	 SELECT
		os.Schedule_Id AS ScheduleId, os.Station_Id AS StationId, os.From_Date AS FromDate, os.To_Date AS ToDate,
		CASE WHEN os.Status =1 THEN 'Active' ELSE 'Inactive' END AS Status,osd.schedule_Details_Id AS scheduleDetailsId, 
		osd.User_Id AS UserId, osd.From_Time AS FromTime, osd.To_Time AS ToTime, 
		CASE WHEN osd.Attendance_Type =1 THEN 'Reliver' 
		WHEN osd.Attendance_Type =2 THEN 'ON CALL' 
		WHEN osd.Attendance_Type =3 THEN 'BREAK'
		ELSE 'Default' END AS AttendanceType,
		osd.Attendance_Type AS AttendanceTypeId,
		osa.Lane_Id AS LaneId, osa.Section_Id AS SectionId,osa.Assignment_Id AS AssignmentId,
		usr.User_Full_Name AS FullName, lan.lane_Name AS LaneName, sec.Section_Name AS SectionName

	FROM Operation_Schedule os
		LEFT JOIN Operation_schedule_details osd ON os.Schedule_Id = osd.Schedule_Id
		LEFT JOIN Operation_Schedule_Assignment osa ON osa.schedule_Details_Id = osd.schedule_Details_Id
		LEFT JOIN users usr ON usr.User_Id = osd.User_Id
		LEFT JOIN Lanes_Def lan ON lan.Lane_Id = osa.Lane_Id
		LEFT JOIN Section_Def sec ON sec.Section_Id = osa.Section_Id
	WHERE 
		os.Station_Id = @stationId and @ScheduleDate BETWEEN os.From_Date and os.To_Date

	ORDER BY osd.From_Time
	SET @Result = @@ROWCOUNT;
	SET @Error_message = null;
  END TRY
  BEGIN CATCH
	SET @Error_message = ERROR_MESSAGE();
	SET @Result = -1; -- Set @Result to -1 to indicate an error
  END CATCH;
END
GO

///////////////////////////////////////////////////////////////////////////////////////////////////

CREATE OR ALTER  PROCEDURE [dbo].[Sp_SupervisOR_OperationScheduleAssignmentDML]
	@OperationType NVARCHAR(10), -- 'INSERT', 'UPDATE', OR 'DELETE'
	@Assignment_Id int = null,
	@Schedule_Details_Id int,
	@Schedule_Id int,
	@User_Id int,
	@Lane_Id int,
	@Section_Id int,
	@Created_By int = null,
	@Updated_By int = null,
	@Result INT OUTPUT,
    @ErrOR_Message NVARCHAR(MAX) OUTPUT
AS
BEGIN
	SET NOCOUNT ON;
	BEGIN TRY
		IF @OperationType = 'INSERT'
		BEGIN
		
			INSERT INTO Operation_Schedule_Assignment
			(Schedule_Details_Id,Schedule_Id,User_Id,Lane_Id,Section_Id,Created_By,Updated_By,Created_Date,Updated_Date )
			VALUES
			(@Schedule_Details_Id,@Schedule_Id,@User_Id,@Lane_Id,@Section_Id,@Created_By,@Updated_By,GETDATE(),GETDATE() )

			SET @Result = SCOPE_IDENTITY();
		END
		ELSE IF @OperationType = 'UPDATE'
		BEGIN
			UPDATE Operation_Schedule_Assignment
			SET 
		    Schedule_Details_Id = @Schedule_Details_Id,
            Schedule_Id = @Schedule_Id,
            User_Id = @User_Id,
            Lane_Id = @Lane_Id,
            Section_Id = @Section_Id,
            Updated_By = @Updated_By,
            Updated_Date = GETDATE()
        WHERE Assignment_Id = @Assignment_Id;

		SET @Result = @Assignment_Id;
    END

		
		-- Add DELETE logic

	END TRY
    BEGIN CATCH
        DECLARE @ErrorNumber INT = ERROR_NUMBER();

        -- Call the function to get the custom error message
        SET @Error_Message = dbo.fn_Core_GetCustomErrorMessage(@ErrorNumber);
        SET @Result = -1; -- Indicate failure
        RETURN;
    END CATCH;
END
Go

/////////////////////////////////////////////////////////////////

CREATE OR ALTER  PROCEDURE [dbo].[Sp_SupervisOR_OperationScheduleDetailsDML] 
    @OperationType NVARCHAR(10), -- 'INSERT', 'UPDATE', OR 'DELETE'
    @schedule_Details_Id INT = NULL,
	@Schedule_Id INT = NULL,
	@User_Id INT = NULL,
	@From_Time Time = NULL,
	@To_Time Time = NULL,
	@Attendance_Type INT = NULL,
	@Created_By INT = NULL,
	@Updated_By INT = NULL,
    @Result INT OUTPUT,
    @Error_Message NVARCHAR(MAX) OUTPUT
AS
BEGIN
    SET NOCOUNT ON;

    BEGIN TRY
        IF @OperationType = 'INSERT'
        BEGIN
            INSERT INTO Operation_schedule_details 
            (Schedule_Id, User_Id, From_Time, To_Time, Attendance_Type, Created_By, Created_Date, Updated_By, Updated_Date)
            VALUES(
				@Schedule_Id, 
                @User_Id, 
                @From_Time, 
                @To_Time, 
                @Attendance_Type, 
                @Created_By, 
                GETDATE(), 
                @Updated_By, 
                GETDATE()
			)
                
            
            -- Assuming single insert and SCOPE_IDENTITY() to return the last inserted ID.
            SET @Result = SCOPE_IDENTITY();
        END
        ELSE IF @OperationType = 'UPDATE'
        BEGIN
            UPDATE Operation_schedule_details
            SET 
                Schedule_Id = @Schedule_Id,
                User_Id = @User_Id,
                From_Time = @From_Time,
                To_Time = @To_Time,
                Attendance_Type = @Attendance_Type,
                Updated_By = @Updated_By,
                Updated_Date = GETDATE()
            WHERE schedule_Details_Id = @schedule_Details_Id;

            -- Assuming single update and returning the schedule_Details_Id of the updated record
            SET @Result =  @schedule_Details_Id;
        END
        ELSE IF @OperationType = 'DELETE'
        BEGIN
            DELETE FROM Operation_schedule_details 
            WHERE schedule_Details_Id = @schedule_Details_Id;

            -- Assuming single delete and returning the schedule_Details_Id of the deleted record
            SET @Result =  @schedule_Details_Id;
        END
    END TRY
    BEGIN CATCH
        DECLARE @ErrorNumber INT = ERROR_NUMBER();

        -- Call the function to get the custom error message
        SET @Error_Message = dbo.fn_Core_GetCustomErrorMessage(@ErrorNumber);
        SET @Result = -1; -- Indicate failure
        RETURN;
    END CATCH;
END
Go;

/////////////////////////////////////////////////////////////////////////////////////

CREATE OR ALTER   PROCEDURE [dbo].[Sp_SupervisOR_OperationScheduleDML]
	@OperationType NVARCHAR(10), -- 'INSERT', 'UPDATE', OR 'DELETE'
	@Schedule_Id int = null,
	@Station_Id int,
	@From_Date date,
	@To_Date date,
	@Status int,
	@Created_By int = null,
	@Updated_By int = null,
	@Result INT OUTPUT,
    @ErrOR_Message NVARCHAR(MAX) OUTPUT
AS
BEGIN
	SET NOCOUNT ON;
	BEGIN TRY
		IF @OperationType = 'INSERT'
		BEGIN
			DECLARE @exisitingId int;
			SELECT @exisitingId = Schedule_Id 
			FROM Operation_Schedule 
			WHERE (From_Date =@From_Date OR 
				To_Date =@To_Date OR 
				(@From_Date between From_Date and To_Date) OR
				(@To_Date between From_Date and To_Date) )
				AND Station_Id = @Station_Id

			IF @exisitingId IS NULL
			BEGIN
				INSERT INTO Operation_Schedule 
				(Station_Id,From_Date,To_Date,Status,Created_By,Created_Date,Updated_By,Updated_Date)
				VALUES (@Station_Id,@From_Date,@To_Date,@Status,@Created_By,GETDATE(),@Updated_By,GETDATE())
				
				SET @Result = SCOPE_IDENTITY();
			END
			ELSE
			BEGIN
				SET @Result = @exisitingId;
				SET @Error_Message='Schedule is already Exists';
			END
		END
		ELSE IF @OperationType = 'UPDATE'
		BEGIN
			UPDATE Operation_Schedule
			SET 
				Station_Id = @Station_Id,
				From_Date = @From_Date,
				To_Date = @To_Date,
				Status = @Status,
				Updated_By = @Updated_By,
				Updated_Date = GETDATE()
			WHERE Schedule_Id = @Schedule_Id;

			SET @Result = @Schedule_Id;
		END
		-- Add DELETE logic

	END TRY
    BEGIN CATCH
       DECLARE @ErrorNumber INT = ERROR_NUMBER();

        -- Call the function to get the custom error message
        SET @Error_Message = dbo.fn_Core_GetCustomErrorMessage(@ErrorNumber);
        SET @Result = -1; -- Indicate failure
        RETURN;
    END CATCH;
END
GO

///////////////////////////////////////////////////////////////////////

IF NOT EXISTS (
    SELECT 1
    FROM INFORMATION_SCHEMA.COLUMNS
    WHERE TABLE_NAME = 'Credit_Card_Type' AND COLUMN_NAME = 'Percentage'
) 
BEGIN
Alter Credit_Card_Type
ADD Percentage decimal(18,3)

END
GO