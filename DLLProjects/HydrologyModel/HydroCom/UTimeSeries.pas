//
// Contains : Class TTimeSeries.
// Author   : Grant Nyland
//
unit UTimeSeries;


interface


//
// Interface dependencies
//
uses

  // Delphi
  Classes,

  // WRYM
  HydrologyCom_TLB,
  UAbstractObject,
  UNumberLists;


//
// Implementation dependencies
//
const
  NullDouble = -1.0E+300;


//
// A class that time series data.
//
type
  TTimeSeries = class(TAbstractObject, ITimeSeries)
  protected
    FData           : TDoubleList;     // Contains the period data.
    FYears          : TIntegerList;    // Contains the actual year values.
    FYearAverages   : TDoubleList;
    FYearTotals     : TDoubleList;     // Contains values for each year accross all periods.
    FPeriodAverages : TDoubleList;
    FPeriodTotals   : TDoubleList;     // Contains values for each period accross all years.
    FPeriodsPerYear : integer;         // Contains the number of periods per year - normally 12 months per year.
    FOverallAverage : Double;
    FOverallTotal   : double;          // Overall summary values.
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    // Introduced in this class.

    function Get_PeriodsPerYear: Integer; safecall;
    function Get_IntervalCount: Integer; safecall;
    function Get_YearCount: Integer; safecall;
    function Get_StartYear: Integer; safecall;
    function Get_EndYear: Integer; safecall;
    function Get_YearTotal(AYearIndex: Integer): Double; safecall;
    function Get_YearAverage(AYearIndex: Integer): Double; safecall;
    function Get_PeriodTotal(APeriodIndex: Integer): Double; safecall;
    function Get_PeriodAverage(APeriodIndex: Integer): Double; safecall;
    function Get_Year(AYearIndex: Integer): Integer; safecall;
    function Get_Data(AIntervalIndex: Integer): Double; safecall;
    function Get_DataByYearMonth(AYear: Integer; AMonth: Integer): Double; safecall;
    function Get_OverallAverage: Double; safecall;
    function Get_OverallTotal: Double; safecall;
  public

    // Construction and destruction.
    constructor Create(APeriodsPerYear: integer);
    destructor Destroy; override;

    // Introduced in this class.
    procedure Clear;
    function LoadFromFile(AFileName: string): boolean;
    function LoadFromFlaggedFile (AFileName  : string;
                                  AFirstYear : Integer;
                                  ALastYear  : Integer): boolean;
    function LoadFromSeries(ADates, AValues: TDoubleList): boolean;
    function SaveToFile(AFileName, AYearFormat, ADataFormat, AYearAverageFormat, AYearTotalFormat,
               AGrandAverageLabel, AGrandTotalLabel: string; AHeadingLines : TStringList): boolean;
    function SaveToStringList(AStringList: TStringList; AYearFormat, ADataFormat, AYearAverageFormat,
               AYearTotalFormat, AGrandAverageLabel, AGrandTotalLabel: string; AHeadingLines: TStringList): boolean;
    procedure CalculateTotals;
    function GetIntervalIndexGregorian(AGregorianYear, APeriodNumber: integer): integer;
    function GetIntervalIndex(AYearIndex, APeriodIndex: integer): integer;
    procedure AddPeriodValue(AYearGregorian: integer; AValue: double);

    // Properties
    property PeriodsPerYear: Integer read Get_PeriodsPerYear;
    property IntervalCount: Integer read Get_IntervalCount;
    property YearCount: Integer read Get_YearCount;
    property StartYear: Integer read Get_StartYear;
    property EndYear: Integer read Get_EndYear;
    property YearTotal[AYearIndex: Integer]: Double read Get_YearTotal;
    property YearAverage[AYearIndex: Integer]: Double read Get_YearAverage;
    property PeriodTotal[APeriodIndex: Integer]: Double read Get_PeriodTotal;
    property PeriodAverage[APerionIndex: Integer]: Double read Get_PeriodAverage;
    property Year[AYearIndex: Integer]: Integer read Get_Year;
    property Data[AIntervalIndex: Integer]: Double read Get_Data; default;
    property DataByYearMonth[AYear: Integer; AMonth: Integer]: Double read Get_DataByYearMonth;
    property OverallAverage: Double read Get_OverallAverage;
    property OverallTotal: Double read Get_OverallTotal;

  end;


implementation


//
// Implementation dependencies
//
uses

  // Delphi
  SysUtils,

  // Alborak
  UFileReader,
  UErrorHandlingOperations;


function TTimeSeries._AddRef: Integer;
const OPNAME = 'TTimeSeries._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TTimeSeries._Release: Integer;
const OPNAME = 'TTimeSeries._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

//
// Calls the ancestor and creates the container.
//
constructor TTimeSeries.Create(APeriodsPerYear: integer);
const OPNAME = 'TTimeSeries.Create';
begin
  try
    inherited Create;
    FPeriodsPerYear := APeriodsPerYear;
    FData := TDoubleList.Create;
    FYears := TIntegerList.Create;
    FYearAverages := TDoubleList.Create;
    FYearTotals := TDoubleList.Create;
    FPeriodAverages := TDoubleList.Create;
    FPeriodTotals := TDoubleList.Create;
    Clear;

  // Handle all exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;


//
// Destructor.
//
destructor TTimeSeries.Destroy;
const OPNAME = 'TTimeSeries.Destroy';
begin
  try
    Clear;
    FreeAndNil(FPeriodTotals);
    FreeAndNil(FPeriodAverages);
    FreeAndNil(FYearTotals);
    FreeAndNil(FYearAverages);
    FreeAndNil(FYears);
    FreeAndNil(FData);
    inherited Destroy;

  // Handle all exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;


//
// Calls the ancestor and creates the container.
//
procedure TTimeSeries.Clear;
const OPNAME = 'TTimeSeries.Clear';
begin
  try
    FData.Clear;
    FYears.Clear;
    FYearAverages.Clear;
    FYearTotals.Clear;
    FPeriodAverages.Clear;
    FPeriodTotals.Clear;
    FOverallAverage := 0.0;
    FOverallTotal := 0.0;

  // Handle all exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTimeSeries.Get_PeriodsPerYear: Integer;
begin
  Result := FPeriodsPerYear;
end;

function TTimeSeries.Get_OverallAverage: Double;
begin
  Result := FOverallAverage;
end;

function TTimeSeries.Get_OverallTotal: Double;
begin
  Result := FOverallTotal;
end;

//
// Returns the total number of elements in the data list.
//
function TTimeSeries.Get_IntervalCount: integer;
const OPNAME = 'TTimeSeries.Get_IntervalCount';
begin
  Result := 0;
  try
    Result := FData.Count;

  // Handle all exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;


//
// Returns the total number of elements in the data list.
//
function TTimeSeries.Get_YearCount: integer;
const OPNAME = 'TTimeSeries.Get_YearCount';
begin
  Result := 0;
  try
    Result := FYears.Count;

  // Handle all exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;


//
// Returns the first year value.
//
function TTimeSeries.Get_StartYear: integer;
const OPNAME = 'TTimeSeries.Get_StartYear';
begin
  Result := -1;
  try
    if (FYears.Count > 0) then
      Result := FYears[0];

  // Handle all exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;


//
// Returns the first year value.
//
function TTimeSeries.Get_EndYear: integer;
const OPNAME = 'TTimeSeries.Get_EndYear';
begin
  Result := -1;
  try
    if (FYears.Count > 0) then
      Result := FYears[FYears.Count - 1];

  // Handle all exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;


//
// Returns the item index from the year and period.
// The year is actual gregorian year. The period index is one based.
//
function TTimeSeries.GetIntervalIndexGregorian(AGregorianYear, APeriodNumber: integer): integer;
const OPNAME = 'TTimeSeries.GetIntervalIndexGregorian';
var LYearIndex, LPeriodIndex: integer;
begin
  Result := -1;
  try
    LYearIndex := (AGregorianYear - StartYear);
    LPeriodIndex := (APeriodNumber - 1);
    Result := GetIntervalIndex(LYearIndex, LPeriodIndex);

  // Handle all exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;


//
// Returns the item index from the year and period indexes.
// Indexes are zero based.
//
function TTimeSeries.GetIntervalIndex(AYearIndex, APeriodIndex: integer): integer;
const OPNAME = 'TTimeSeries.GetIntervalIndex';
begin
  Result := -1;
  try
    Result := (AYearIndex * FPeriodsPerYear) + APeriodIndex;

  // Handle all exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;


//
// Returns the data value from the list.
// Has no error management so that caller catches exception.
//
function TTimeSeries.Get_Year(AYearIndex: integer): integer;
begin
  Result := FYears[AYearIndex];
end;

function TTimeSeries.Get_Data(AIntervalIndex: integer): double;
begin
  Result := FData[AIntervalIndex];
end;

function TTimeSeries.Get_DataByYearMonth (AYear : integer; AMonth : integer): double;
begin
  Result := FData[GetIntervalIndexGregorian(AYear, AMonth)];
end;

function TTimeSeries.Get_YearAverage(AYearIndex: integer): double;
begin
  Result := FYearAverages[AYearIndex];
end;

function TTimeSeries.Get_YearTotal(AYearIndex: integer): double;
begin
  Result := FYearTotals[AYearIndex];
end;

function TTimeSeries.Get_PeriodAverage(APeriodIndex: integer): double;
begin
  Result := FPeriodAverages[APeriodIndex];
end;

function TTimeSeries.Get_PeriodTotal(APeriodIndex: integer): double;
begin
  Result := FPeriodTotals[APeriodIndex];
end;


//
// Recalculates the annual and monthly values.
//
procedure TTimeSeries.CalculateTotals;
const OPNAME = 'TTimeSeries.CalculateTotals';
var
  LYearCount, LYearIndex, LPeriodIndex, LIntervalIndex, LPeriodsThisYear, LPeriodCount: integer;
  LDataValue, LYearTotal: double;
  LPeriodTotals: array of double;
  LAverage : Double;
begin
  try

    // Initialise.
    FYearAverages.Clear;
    FYearTotals.Clear;
    FPeriodAverages.Clear;
    FPeriodTotals.Clear;
    FOverallAverage := 0.0;
    FOverallTotal := 0.0;
    LIntervalIndex := 0;
    LPeriodCount := 0;
    LYearCount := YearCount;
    SetLength(LPeriodTotals, FPeriodsPerYear);

    // Loop for all the years.
    for LYearIndex := 0 to LYearCount - 1 do
    begin
      LYearTotal := 0.0;
      LPeriodsThisYear := 0;
      for LPeriodIndex := 0 to FPeriodsPerYear - 1 do
      begin
        LDataValue := FData[LIntervalIndex];
        if (LDataValue > NullDouble) then
        begin
          LYearTotal := LYearTotal + LDataValue;
          LPeriodTotals[LPeriodIndex] := LPeriodTotals[LPeriodIndex] + LDataValue;
          Inc(LPeriodsThisYear);
        end;
        Inc(LIntervalIndex);
      end;
      if (LPeriodsThisYear <= 0) then
      begin
        FYearTotals.Add(0.0);
        FYearAverages.Add(0.0);
      end
      else
      begin
        LPeriodCount := LPeriodCount + LPeriodsThisYear;
        FYearTotals.Add(LYearTotal);
        if (LPeriodsThisYear > 0) then
          LAverage := LYearTotal / LPeriodsThisYear
        else
          LAverage := 0;
        FYearAverages.Add(LAverage);
        FOverallTotal := FOverallTotal + LYearTotal;
      end;
    end;
    if (LPeriodCount > 0) then
      FOverallAverage := FOverallTotal / LPeriodCount
    else
      FOverallAverage := 0;

    // Loop for all periods.
    for LPeriodIndex := 0 to FPeriodsPerYear - 1 do
    begin
      FPeriodTotals.Add(LPeriodTotals[LPeriodIndex]);
      if (LYearCount > 0) then
        LAverage := LPeriodTotals[LPeriodIndex] / LYearCount
      else
        LAverage := 0;
      FPeriodAverages.Add(LAverage);
    end;

  // Handle all exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;


//
// Adds this value as if it is the next number. No sequential checking.
//
procedure TTimeSeries.AddPeriodValue(AYearGregorian: integer; AValue: double);
const OPNAME = 'TTimeSeries.AddPeriodValue';
begin
  try
    if ((FData.Count mod FPeriodsPerYear) = 0) then
      FYears.Add(AYearGregorian);
    FData.Add(AValue);

  // Handle all exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;


//
// Loads the numbers from a time series file into the number list.
//
function TTimeSeries.LoadFromFile(AFileName: string): boolean;
const OPNAME = 'TTimeSeries.LoadFromFile';
var
  LTimeSeriesFile: TFileReader;
  LPreviousYear, LYear, LPeriodIndex: integer;
  LNumber: double;
begin
  Result := False;
  try

    // Clear any existing data.
    Clear;

    // Make sure that the file exists.
    if (Trim(AFileName) = '') then
    begin
      raise Exception.Create('No time series file name specified. ');
    end else begin

      // Make sure that the file exists.
      if (not FileExists(AFileName)) then
      begin
        raise Exception.CreateFmt('Could not find the time series file [%s] to read. ', [AFileName]);
      end else begin

        // Load the file.
        LTimeSeriesFile := TFileReader.Create;
        try
          LTimeSeriesFile.LoadFromFile(AFileName);

          // Make sure that the file has data.
          if (not LTimeSeriesFile.HasData) then
          begin
            raise Exception.CreateFmt('The time series file [%s] does not contain any data to read. ', [AFileName]);
          end else begin

            // Loop until there is no more data.
            LYear := -1;
            LPreviousYear := -1;
            while (LTimeSeriesFile.CurrentLineLength >= (13 * 8)) do
            begin
              LTimeSeriesFile.Read(LYear, 'Year');

              // Make sure that the years are continuous.
              if (LPreviousYear > 0) then
                if (LYear <> (StartYear + YearCount)) then
                  raise Exception.CreateFmt('The years in time series file [%s] do not follow sequentually. ' +
                                            'Check between years [%d] and [%d]. ', [AFileName, LPreviousYear, LYear]);

              // Add the year.
              FYears.Add(LYear);
              LPreviousYear := LYear;

              // Loop for all periods.
              for LPeriodIndex := 1 to FPeriodsPerYear do
              begin
                LTimeSeriesFile.Read(LNumber, 'Year=%d Month=%d', [LYear, LPeriodIndex]);
                FData.Add(LNumber);
              end;

              // Next year.
              LTimeSeriesFile.NextLine;
            end;

            // Done.
            Result := True;
          end;
        finally
          LTimeSeriesFile.Free;
        end;

        // Recalculate all the summary values.
        CalculateTotals;
      end;
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTimeSeries.LoadFromFlaggedFile (AFileName  : string;
                                          AFirstYear : Integer;
                                          ALastYear  : Integer): boolean;
const OPNAME = 'TTimeSeries.LoadFromFlaggedFile';
var
  LTimeSeriesFile : TFileReader;
  LPreviousYear   : integer;
  LYear           : integer;
  LPeriodIndex    : integer;
  LNumber         : double;
  LFlag           : string;
begin
  Result := False;
  try

    // Clear any existing data.
    Clear;

    // Make sure that the file exists.
    if (Trim(AFileName) = '') then
    begin
      raise Exception.Create('No time series file name specified. ');
    end
    else
    begin

      // Make sure that the file exists.
      if (not FileExists(AFileName)) then
      begin
        raise Exception.CreateFmt('Could not find the time series file [%s] to read. ', [AFileName]);
      end
      else
      begin

        // Load the file.
        LTimeSeriesFile := TFileReader.Create;
        try
          LTimeSeriesFile.LoadFromFile(AFileName);

          // Make sure that the file has data.
          if (not LTimeSeriesFile.HasData) then
          begin
            raise Exception.CreateFmt('The time series file [%s] does not contain any data to read. ', [AFileName]);
          end
          else
          begin

            // Loop until there is no more data.
            LYear := -1;
            LPreviousYear := -1;
            while (LTimeSeriesFile.CurrentLineLength >= (13 * 8)) do
            begin
              LTimeSeriesFile.Read(LYear, 'Year');
              if (((AFirstYear = 0) AND (ALastYear = 0)) OR
                  ((LYear >= AFirstYear) AND (LYear <= ALastYear))) then
              begin
                // Make sure that the years are continuous.
                if (LPreviousYear > 0) then
                  if (LYear <> (StartYear + YearCount)) then
                    raise Exception.CreateFmt('The years in time series file [%s] do not follow sequentually. ' +
                                              'Check between years [%d] and [%d]. ', [AFileName, LPreviousYear, LYear]);

                // Add the year.
                FYears.Add(LYear);
                LPreviousYear := LYear;

                // Loop for all periods.
                for LPeriodIndex := 1 to FPeriodsPerYear do
                begin
  //                LTimeSeriesFile.Read(LNumber, 'Year=%d Month=%d', [LYear, LPeriodIndex]);
                  LTimeSeriesFile.ReadFlagged(LNumber, LFlag, 'Year=%d Month=%d', [LYear, LPeriodIndex]);
                  FData.Add(LNumber);
                end;
              end;
              // Next year.
              LTimeSeriesFile.NextLine;
            end;

            // Done.
            Result := True;
          end;
        finally
          LTimeSeriesFile.Free;
        end;

        // Recalculate all the summary values.
        CalculateTotals;
      end;
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;


//
// Loads the numbers from a time series file into the number list.
// The series are assumed to be monthly time series lists.
//
function TTimeSeries.LoadFromSeries(ADates, AValues: TDoubleList): boolean;
const OPNAME = 'TTimeSeries.LoadFromSeries';
var
  LYearIndex, LMonthIndex: integer;
  LStartYear, LStartMonth, LEndYear, LEndMonth, LDay: word;
begin
  Result := False;
  try

    // Clear any existing data.
    Clear;

    // Set the start year.
    if (ADates.Count > 0) then
    begin
      DecodeDate(ADates[0], LStartYear, LStartMonth, LDay);
      IncAMonth(LStartYear, LStartMonth, LDay, 3);
      DecodeDate(ADates[ADates.Count - 1], LEndYear, LEndMonth, LDay);
      IncAMonth(LEndYear, LEndMonth, LDay, 3);

      // Populate the years.
      for LYearIndex := LStartYear to LEndYear do
        FYears.Add(LYearIndex);

      // The first year may be incomplete so add dummy values.
      for LMonthIndex := 1 to integer(LStartMonth) - 1 do
        FData.Add(NullDouble - 1.0);

      // Dump the data in as is.
      FData.AddNumberList(AValues);

      // The last year may be incomplete so add dummy values.
      for LMonthIndex := LEndMonth + 1 to 12 do
        FData.Add(NullDouble - 1.0);

      // Recalculate all the summary values.
      CalculateTotals;
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;


//
// Writes the data to a file. Leave a format string empty if that value is not required.
//
function TTimeSeries.SaveToFile(AFileName, AYearFormat, ADataFormat, AYearAverageFormat, AYearTotalFormat, AGrandAverageLabel, AGrandTotalLabel: string;
                                AHeadingLines : TStringList): boolean;
const OPNAME = 'TTimeSeries.SaveToFile';
var LTimeSeriesFile: TStringList;
begin
  Result := False;
  try

    // Make sure that the file exists.
    if (Trim(AFileName) = '') then
    begin
      raise Exception.Create('No time series file name specified for saving. ');
    end else begin

      // Make sure that the folder exists.
      if (not DirectoryExists(ExtractFilePath(AFileName))) then
        ForceDirectories(ExtractFilePath(AFileName));
      if (not DirectoryExists(ExtractFilePath(AFileName))) then
      begin
        raise Exception.CreateFmt('Could not create the output directory for the time series file [%s]. ', [AFileName]);
      end else begin

        // Make sure that the file will get some data.
        if (FData.Count <= 0) then
        begin
          SysUtils.DeleteFile(AFileName);
        end else begin

          // Load the file.
          LTimeSeriesFile := TStringList.Create;
          try
            if SaveToStringList(LTimeSeriesFile, AYearFormat, ADataFormat, AYearAverageFormat,
                             AYearTotalFormat, AGrandAverageLabel, AGrandTotalLabel, AHeadingLines) then
            begin

              // Attempt to save the file.
              LTimeSeriesFile.SaveToFile(AFileName);

              // Done.
              Result := True;
            end;
          finally
            LTimeSeriesFile.Free;
          end;
        end;
      end;
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;


//
// Writes the data to a string list. Leave a format string empty if that value is not required.
//
function TTimeSeries.SaveToStringList(AStringList: TStringList;
      AYearFormat, ADataFormat, AYearAverageFormat, AYearTotalFormat, AGrandAverageLabel, AGrandTotalLabel: string;
      AHeadingLines : TStringList): boolean;
const OPNAME = 'TTimeSeries.SaveToStringList';
var
  LYearIndex, LPeriodIndex, LIntervalIndex: integer;
  LLine, LValue: string;
  LTemp : Double;
begin
  Result := False;
  try

    // Add heading lines.
    if Assigned(AHeadingLines) then
      if (AHeadingLines.Count > 0) then
        AStringList.AddStrings(AHeadingLines);

    // Loop for all years.
    LIntervalIndex := 0;
    for LYearIndex := 0 to YearCount - 1 do
    begin
      LLine := '';

      // Add the year.
      if (AYearFormat <> '') then
        LLine := Format(AYearFormat, [Year[LYearIndex]]);

      // Loop for all periods.
      if (ADataFormat <> '') then
      begin
        for LPeriodIndex := 0 to FPeriodsPerYear - 1 do
        begin
          LValue := ',';
          if (Data[LIntervalIndex] > NullDouble) then
            LValue := Format(ADataFormat, [Data[LIntervalIndex]]);
          LLine := LLine + LValue;
          Inc(LIntervalIndex);
        end;
      end;

      // Add year totals.
      if (AYearTotalFormat <> '') then
        LLine := LLine + Format(AYearTotalFormat, [YearTotal[LYearIndex]]);

      // Add year average.
      if (AYearAverageFormat <> '') then
        LLine := LLine + Format(AYearAverageFormat, [YearAverage[LYearIndex]]);

      // Add the line to the list.
      AStringList.Add(LLine);
    end;

    // Write a blank line if one of the file summary lines is required.
    if (AGrandAverageLabel <> '') or (AGrandTotalLabel <> '') then
      AStringList.Add('');

    // Write the total line if required.
    if (AGrandTotalLabel <> '') then
    begin
      LLine := AGrandTotalLabel;

      // Loop for all periods.
      if (ADataFormat <> '') then
        for LPeriodIndex := 0 to FPeriodsPerYear - 1 do
          LLine := LLine + Format(ADataFormat, [PeriodTotal[LPeriodIndex]]);

      // Add total.
      if (AYearTotalFormat <> '') then
        LLine := LLine + Format(AYearTotalFormat, [OverallTotal]);

      // Add average.
      if (AYearAverageFormat <> '') then
        LLine := LLine + Format(AYearAverageFormat, [OverallTotal / 12]);

      // Add the line to the list.
      AStringList.Add(LLine);
    end;

    // Write the average line if required.
    if (AGrandAverageLabel <> '') then
    begin
      LTemp := 0;
      LLine := AGrandAverageLabel;

      // Loop for all periods.
      if (ADataFormat <> '') then
      begin
        for LPeriodIndex := 0 to FPeriodsPerYear - 1 do
        begin
          LLine := LLine + Format(ADataFormat, [PeriodAverage[LPeriodIndex]]);
          LTemp := LTemp + PeriodAverage[LPeriodIndex];
        end;
      end;

      // Add total.
      if (AYearTotalFormat <> '') then
        LLine := LLine + Format(AYearTotalFormat, [LTemp]);

      // Add average.
      if (AYearAverageFormat <> '') then
        LLine := LLine + Format(AYearAverageFormat, [OverallAverage]);

      // Add the line to the list.
      AStringList.Add(LLine);
    end;

    // Done.
    Result := True;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.
