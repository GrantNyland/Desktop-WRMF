unit UDataObjectRainfallLoadAgent;

interface
uses
  Classes,
  Contnrs,
  SysUtils,
  UAbstractObject,
  UYearlyStationData;
type
  TDataObjectRainfallLoadAgent = class(TAbstractAppObject)
  protected
    function LoadProjectGauges(AStationDataList : TObjectList): boolean;
    function LoadGaugeInfo(AStationData : TStationData) : boolean;
    function LoadSplitData(AStationData : TStationData) : boolean;
    function LoadWRCData(AStationData : TStationData) : boolean;
    function LoadPatchdata(AStationData : TStationData) : boolean;
    function LoadMonthlyData(AStationData : TStationData) : boolean;
    function LoadMonthlyRAWData(AStationData : TStationData) : boolean;
  public
    function LoadData(AStationDataList : TObjectList) : WordBool;
end;

implementation
uses
  DB,
  Math,
  //ZLibEx,
  System.ZLib,
  VCL.Graphics,
  VCL.Forms,
  UConstants,
  UDataSetType,
  UErrorHandlingOperations,
  DateUtils;

function TDataObjectRainfallLoadAgent.LoadProjectGauges(AStationDataList : TObjectList): boolean;
const OPNAME = 'TDataObjectRainfallLoadAgent.LoadProjectGauges';
var
  LDataset   : TAbstractModelDataset;
  LErrorFree : boolean;
  LStationData   : TStationData;
  LSQL       : string;
begin
  Result := False;
  try
    LErrorFree := FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataset);
    try
      if LErrorFree then
      begin
        LDataset.DataSet.Close;
        lSQL := '  SELECT * FROM RainfallProjectGauges                         ' +
                '  WHERE                                                       ' +
                '    RainfallProjectGauges.Model = :AModel                     ' +
                '    AND RainfallProjectGauges.StudyAreaName = :AStudyAreaName ' +
                '    AND RainfallProjectGauges.SubArea = :ASubArea             ' +
                '    AND RainfallProjectGauges.Scenario = :AScenario           ';
        LDataset.SetSQL(lSQL);
        LDataset.SetParams(['AModel','AStudyAreaName','ASubArea','AScenario'],
          [FAppModules.StudyArea.ModelCode,
           FAppModules.StudyArea.StudyAreaCode,
           FAppModules.StudyArea.SubAreaCode,
           UAbstractObject.CProjectGauges]);
        LDataset.Dataset.Open();
        while not LDataset.DataSet.Eof do
        begin
          LStationData   := TStationData.Create(FAppModules);
          LStationData.Initialise;
          LStationData.RainfallData.StationID := LDataset.DataSet.FieldByName('StationID').AsInteger;
          AStationDataList.Add(LStationData);
          LoadGaugeInfo(LStationData);
          LDataset.DataSet.Next;
        end;
      end;
    finally
      if Assigned(LDataset) then
      begin
        LDataset.Dataset.Close;
        FreeAndNil(LDataset);
      end;
    end;
    Result := True;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TDataObjectRainfallLoadAgent.LoadSplitData(AStationData : TStationData) : boolean;
const OPNAME = 'TDataObjectRainfallLoadAgent.LoadSplitData';
var
  LDataset   : TAbstractModelDataset;
  LErrorFree : boolean;
  LSQL       : string;
  LEndYear,
  LStartYear : integer;
begin
  Result := False;
  try
    LErrorFree := FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataset);
    try
      if (LErrorFree) and (AStationData.RainfallData <> nil) then
      begin
        AStationData.CreateAndPopulateSplit(0, 0);
        LDataset.DataSet.Close;
        LSQL := '  SELECT * FROM RainfallRAWSplits ' +
                '  WHERE                           ' +
                ' StationID =                      ' + IntToStr(AStationData.RainfallData.StationID)+
                ' ORDER BY HydroStartYear';
        LDataset.SetSQL(LSQL);
        LDataset.Dataset.Open();
        while (not lDataset.DataSet.EOF) do
        begin
          LStartYear := lDataset.DataSet.FieldByName('HydroStartYear').AsInteger;
          LEndYear   := lDataset.DataSet.FieldByName('HydroEndYear').AsInteger;
          AStationData.CreateAndPopulateSplit(LStartYear, LEndYear);
          LDataset.DataSet.Next;
        end;

        LDataset.DataSet.Close;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataObjectRainfallLoadAgent.LoadWRCData(AStationData : TStationData) : boolean;
const OPNAME = 'TDataObjectRainfallLoadAgent.LoadWRCData';
begin
  Result := False;
  try
    AStationData.LoadMonthlyWRCData;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDataObjectRainfallLoadAgent.LoadPatchdata(AStationData : TStationData) : boolean;
const OPNAME = 'TDataObjectRainfallLoadAgent.LoadPatchdata';
begin
  Result := False;
  try
    AStationData.LoadMonthlyPatchData;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;
function TDataObjectRainfallLoadAgent.LoadMonthlyRAWData(AStationData : TStationData) : boolean;
const OPNAME = 'TDataObjectRainfallLoadAgent.LoadMonthlyRAWData';
begin
  Result := False;
  try
    AStationData.LoadMonthlyRAWData;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TDataObjectRainfallLoadAgent.LoadMonthlyData(AStationData : TStationData) : boolean;
const OPNAME = 'TDataObjectRainfallLoadAgent.LoadMonthlyData';
var
  LDataset          : TAbstractModelDataset;
  LSQL              : string;
  LYearlyData       : TYearlyData;
  LHydroYear,
  LYear             : integer;
  LCounter          : integer;
  LMonth            : integer;
  LValue            : double;
begin
  Result := False;
  try
    AStationData.CastRainfallData.HydroYearlyData.Clear;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if (Assigned(lDataset)) then
      begin
        LDataset.DataSet.Close;
        if (AStationData.RainfallData.StationID < 100000) then
          LSQL := 'SELECT * FROM RainfallMonthlyRAWData WHERE StationID = ' +
                  IntToStr(AStationData.RainfallData.StationID) +
                  ' ORDER BY Year'
        else
          LSQL := 'SELECT * FROM RainfallUserMonthlyData WHERE StationID = ' +
                  IntToStr(AStationData.RainfallData.StationID) +
                  ' ORDER BY Year';
        LDataset.SetSQL(lSQL);
        LDataset.DataSet.Open;
        LDataset.DataSet.First;
        LDataset.DataSet.Last;
        LDataset.DataSet.First;
        if LDataset.DataSet.RecordCount > 0 then
        begin
          LYearlyData   := TYearlyData.Create(FAppModules);
          LYearlyData.Initialise;
          LYearlyData.Year := LDataset.DataSet.FieldByName('Year').AsInteger-1;
          AStationData.CastRainfallData.AddYearlyData(LYearlyData);
          AStationData.CastRainfallData.YearlyData.Add(LYearlyData);
          LYearlyData.HydroYear := IntToStr(LDataset.DataSet.FieldByName('Year').AsInteger-1) + '/' + Copy(IntToStr(LDataset.DataSet.FieldByName('Year').AsInteger), 3, 2);
          for LMonth := 10 to 12 do
            lYearlyData.MonthlyRainfall[LMonth] := NullFloat;
        end;
        LCounter := 0;
        while (not LDataset.DataSet.EOF) do
        begin
          inc(LCounter);
          LYear := LDataset.DataSet.FieldByName('Year').AsInteger;
          for LMonth := 1 to 12 do
          begin
            if LMonth < 10 then
              LHydroYear := LYear-1
            else
              LHydroYear := LYear;
            LYearlyData := AStationData.CastRainfallData.CastHydroYearDataByYear(LHydroYear);
            if LYearlyData = nil then
            begin
              LYearlyData   := TYearlyData.Create(FAppModules);
              LYearlyData.Initialise;
              LYearlyData.Year := LHydroYear;
              AStationData.CastRainfallData.AddYearlyData(LYearlyData);
              AStationData.CastRainfallData.YearlyData.Add(LYearlyData);
              LYearlyData.HydroYear := IntToStr(LHydroYear) + '/' + Copy(IntToStr(LHydroYear+1), 3, 2);
            end;
            if (lDataset.DataSet.FieldByName(Format('Value%2.2d',[LMonth])).IsNull) then
            begin
              if (LCounter = 1) and (LMonth < 10) then
                lYearlyData.MonthlyRainfall[LMonth + 3] := NullFloat
              else
                lYearlyData.MonthlyRainfall[LMonth] := NullFloat;
            end
            else
            begin
              LValue := lDataset.DataSet.FieldByName(Format('Value%2.2d',[LMonth])).AsFloat;
              LValue := (Round(lValue * 10)) / 10;
              if (LCounter = 1) and (LMonth < 10) then
                lYearlyData.MonthlyRainfall[LMonth + 3] := LValue
              else
              if (LCounter = 1) and (LMonth > 9) then
                LYearlyData.MonthlyRainfall[LMonth-9] := LValue;

             if (LCounter > 1) and (LMonth < 10) then
                lYearlyData.MonthlyRainfall[LMonth + 3] := LValue
              else
              if (LCounter > 1) and (LMonth > 9) then
                LYearlyData.MonthlyRainfall[LMonth-9] := LValue;

            end;
            if (LCounter = 1) and (LMonth < 10) then
              LYearlyData.MonthlyPatchSign[LMonth+3] := Trim(lDataset.DataSet.FieldByName(Format('Flag%2.2d',[LMonth])).AsString)
            else
            if (LCounter = 1) and (LMonth > 9) then
              LYearlyData.MonthlyPatchSign[LMonth-9] := Trim(lDataset.DataSet.FieldByName(Format('Flag%2.2d',[LMonth])).AsString);

            if (LCounter > 1) and (LMonth < 10) then
              LYearlyData.MonthlyPatchSign[LMonth+3] := Trim(lDataset.DataSet.FieldByName(Format('Flag%2.2d',[LMonth])).AsString)
            else
            if (LCounter > 1) and (LMonth > 9) then
              LYearlyData.MonthlyPatchSign[LMonth-9] := Trim(lDataset.DataSet.FieldByName(Format('Flag%2.2d',[LMonth])).AsString);

          end;
          LDataset.DataSet.Next;
        end;
        LDataset.DataSet.Close;
      end;
    finally
      lDataset.Free;
    end;

    if (AStationData.CastRainfallData.HydroYearlyData.Count > 0) then
    begin
      AStationData.CastRainfallData.HydroStartYear := TYearlyData(AStationData.CastRainfallData.HydroYearlyData.Items[0]).Year;
      AStationData.CastRainfallData.StartYear      := AStationData.CastRainfallData.HydroStartYear;
      AStationData.CastRainfallData.HydroEndYear   := TYearlyData(AStationData.CastRainfallData.HydroYearlyData.Items[AStationData.CastRainfallData.HydroYearlyData.Count - 1]).Year;
      AStationData.CastRainfallData.EndYear        := AStationData.CastRainfallData.HydroEndYear;
    end;
    AStationData.CastRainfallData.CalculateStats;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDataObjectRainfallLoadAgent.LoadData(AStationDataList : TObjectList): WordBool;
const OPNAME = 'TDataObjectRainfallLoadAgent.LoadData';
begin
  Result := True;
  try
    LoadProjectGauges(AStationDataList);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDataObjectRainfallLoadAgent.LoadGaugeInfo(AStationData: TStationData): boolean;
const OPNAME = 'TDataObjectRainfallLoadAgent.LoadGaugeInfo';
var
  LDataset : TAbstractModelDataset;
  LSQL : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    if (Assigned(LDataset)) then
    begin
      if (AStationData.RainfallData.StationID > 100000) then
        LSQL := 'SELECT * FROM RainfallUserStations WHERE StationID = ' +
                 IntToStr(AStationData.RainfallData.StationID)
      else
        LSQL := 'SELECT * FROM RainfallStations WHERE StationID = ' +
                IntToStr(AStationData.RainfallData.StationID);
      LDataset.DataSet.Close;
      LDataset.SetSQL(lSQL);
      LDataset.DataSet.Open;
      if not (LDataset.DataSet.Eof) then
      begin
        AStationData.RainfallData.StationNumber := Trim(LDataset.DataSet.FieldByName('StationNumber').AsString);;
        AStationData.StationName   := Trim(LDataset.DataSet.FieldByName('StationName').AsString);
        AStationData.Latitude      := LDataset.DataSet.FieldByName('StationLatitude').AsInteger;
        AStationData.Longitude     := LDataset.DataSet.FieldByName('StationLongitude').AsInteger;
        if (LDataset.DataSet.FieldByName('StationHeight').IsNull) then
          AStationData.Height      := NullInteger
        else
          AStationData.Height      := LDataset.DataSet.FieldByName('StationHeight').AsInteger;
        AStationData.StationType   := Trim(LDataset.DataSet.FieldByName('StationType').AsString);
        if (AStationData.RainfallData.StationID < 100000) then
          AStationData.IsInWR90 := (Trim(lDataset.DataSet.FieldByName('WR90').AsString) = 'Y')
        else
          AStationData.IsInWR90 := FALSE;
        LoadMonthlyRAWData(AStationData);
        LoadSplitData(AStationData);
        if (AStationData.RainfallData.StationID < 100000) then
          LoadWRCData(AStationData);
        LoadPatchdata(AStationData);

      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.






