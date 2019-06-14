//
//
//  UNIT      : Contains TRWHDataSQLAgent Class
//  AUTHOR    : Dziedzi Ramulondi (arivia.kom)
//  DATE      : 09/01/2007
//  COPYRIGHT : Copyright © 2007 DWAF
//
//

unit URWHDataSQLAgent;

interface
uses
  Classes,
  VCL.Controls,
  URWHDataObject,
  UAbstractObject;
type
  TRWHDataSQLAgent = class(TAbstractSQLAgent)
  private
  protected
    function GetScenarioWhereClause: string;
    function GetMaxIdentifierSQL                    : string;
    procedure GetStationDailyData(AStationID: Integer; AData: TStringList);
    procedure GetStationRawDailyData(AStationID : Integer; AData : TStringList);

  public
    function AddSelectedRainfallStation(ARainfallStation : TRainfallStation): boolean;overload;
    function RemoveRainfallStationFromSelection(ARainfallStation : TRainfallStation): boolean;overload;

    function RemoveAllRainfallStationFromSelection : boolean;

    function AddRunConfigurationData(ARunConfig :TRWHRunConfig;AUpdate:Boolean=False): boolean;
    function UpdateRunConfigurationData(ARunConfig :TRWHRunConfig): boolean;
    function DeleteRunConfigurationData(ARunConfig :TRWHRunConfig): boolean;

    function GetDailyData( AStationNumber, APatchName: WideString): WideString;
    function GetRAWDailyData(AStationNumber : WideString; AData : TStrings): Boolean;
    function GetMonthlyData(AStationNumber, APatchName: WideString): WideString;
  end;

implementation
uses
  Math,
  //ZLibEx,
  System.ZLib,
  SysUtils,
  UConstants,
  UDataSetType,
  VCL.Dialogs,
  UYearlyStationData,
  UErrorHandlingOperations, DB;
  { TRWHDataSQLAgent }

function TRWHDataSQLAgent.GetScenarioWhereClause: string;
const OPNAME = 'TRWHDataSQLAgent.GetScenarioWhereClause';
begin
  Result := '';
  try
    Result :=
      ' (Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode) + ') AND ' +
      ' (StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
      ' (SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
      ' (Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRWHDataSQLAgent.GetMonthlyData (AStationNumber : WideString;  APatchName     : WideString) : WideString;
const OPNAME = 'TRWHDataSQLAgent.GetMonthlyData';
var
  lSQL           : string;
  lDataset       : TAbstractModelDataset;
  lStationID     : integer;
  lStation       : TStationData;
  lPatch         : TPatchData;
  lTitle         : string;
begin
  Result := '';
  try
    if (AStationNumber <> '') then
    begin
      FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
      try
        if (Assigned(lDataset)) then
        begin
          lSQL := 'SELECT * FROM RainfallStations WHERE StationNumber = ' +
                  QuotedStr(AStationNumber);
          lDataset.DataSet.Close;
          lDataset.SetSQL(lSQL);
          lDataset.DataSet.Open;
          if (NOT lDataset.DataSet.Eof) then
          begin
            lStationID := lDataset.DataSet.FieldByName('StationID').AsInteger;
            lStation   := TStationData.Create(FAppModules);
            try
              lStation.Initialise;
              lStation.RainfallData.StationNumber := AStationNumber;
              lStation.RainfallData.StationID     := lStationID;
              lStation.StationName   := Trim(lDataset.DataSet.FieldByName('StationName').AsString);
              lStation.Latitude      := lDataset.DataSet.FieldByName('StationLatitude').AsInteger;
              lStation.Longitude     := lDataset.DataSet.FieldByName('StationLongitude').AsInteger;
              if (lDataset.DataSet.FieldByName('StationHeight').IsNull) then
                lStation.Height      := NullInteger
              else
                lStation.Height      := lDataset.DataSet.FieldByName('StationHeight').AsInteger;
              lStation.StationType   := Trim(lDataset.DataSet.FieldByName('StationType').AsString);
              lStation.IsInWR90      := (Trim(lDataset.DataSet.FieldByName('WR90').AsString) = 'Y');
              lStation.LoadMonthlyData;
              if (APatchName <> '') then
              begin
                lPatch := lStation.CastPatchWithName(APatchName);
                if (lPatch <> nil) then
                begin
                  lTitle := lPatch.RainfallData.StationNumber + ' (' + lPatch.PatchName + ')';
                  Result := lPatch.CastRainfallData.StreamOut(lTitle);
                end;
              end
              else
              begin
                lTitle := lStation.RainfallData.StationNumber + ' (RAW)';
                Result := lStation.CastRainfallData.StreamOut(lTitle);
              end;
            finally
              FreeAndNil(lStation);
            end;
          end;
        end;
      finally
        if Assigned(lDataset) then
        begin
          lDataset.Dataset.Close;
          FreeAndNil(lDataset);
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRWHDataSQLAgent.GetDailyData (AStationNumber : WideString; APatchName     : WideString) : WideString;
const OPNAME = 'TRWHDataSQLAgent.GetDailyData';
var
  lSQL       : string;
  lDataset   : TAbstractModelDataset;
  lStationID : integer;
  lDailyData : TStringlist;
  lIndex     : integer;
  lTempStr   : string;
begin
  Result := '';
  try
    if(APatchName = '') then
      APatchName := 'WRC';

    if (AStationNumber <> '') AND (APatchName = 'WRC') then
    begin
      FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
      try
        if (Assigned(lDataset)) then
        begin
          lSQL := 'SELECT * FROM RainfallStations WHERE StationNumber = ' +
                  QuotedStr(AStationNumber);
          lDataset.SetSQL(lSQL);
          lDataset.DataSet.Open;
          if (NOT lDataset.DataSet.Eof) then
          begin
            lStationID := lDataset.DataSet.FieldByName('StationID').AsInteger;
            lDailyData := TStringlist.Create;
            try
              GetStationDailyData(lStationID, lDailyData);
              for lIndex := 0 to lDailyData.Count - 1 do
              begin
                lTempStr := lDailyData[lIndex];
                Result := Result + lTempStr + #13#10
              end;
            finally
              FreeAndNil(lDailyData);
            end;
          end;
        end;
      finally
        if Assigned(lDataset) then
        begin
          lDataset.Dataset.Close;
          FreeAndNil(lDataset);
        end;
      end;
    end;

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TRWHDataSQLAgent.GetRAWDailyData(AStationNumber : WideString; AData : TStrings): Boolean;
const OPNAME = 'TRWHDataSQLAgent.GetRAWDailyData';
var
  lSQL       : string;
  lDataset   : TAbstractModelDataset;
  lStationID : integer;
  lDailyData : TStringlist;
  lIndex     : integer;
  lTempStr   : TStringlist;
  LReadInteger,
  LErrorCode : Integer;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if (Assigned(lDataset)) and (AData <> nil) then
      begin
        LSQL := 'SELECT * FROM RainfallStations WHERE StationNumber = ' +
                QuotedStr(AStationNumber);
        LDataset.SetSQL(LSQL);
        LDataset.DataSet.Open;
        if (NOT lDataset.DataSet.Eof) then
        begin
          LStationID := LDataset.DataSet.FieldByName('StationID').AsInteger;
          LDailyData := TStringlist.Create;
          lTempStr   := TStringlist.Create;
          try
            GetStationRawDailyData(LStationID, LDailyData);
            for lIndex := 0 to LDailyData.Count - 1 do
            begin
              lTempStr.Clear;
              LTempStr.CommaText := LDailyData[lIndex];
              if lTempStr.Count>0 then
              begin
                if (Trim(lTempStr[0]) <> '') then
                begin

                  Val(lTempStr[0],LReadInteger,LErrorCode);
                  if (LErrorCode = 0) then
                  begin
                    LTempStr[0] := FormatDateTime('yyyy/mm/dd', EncodeDate(StrToIntDef(Copy(lTempStr[0],1,4),0),
                                                                           StrToIntDef(Copy(lTempStr[0],5,2),0),
                                                                           StrToIntDef(Copy(lTempStr[0],7,2),0)));
                    AData.Add(lTempStr.CommaText);
                    lTempStr.Add(IntToStr(LReadInteger));
                  end;
                end;
              end;
            end;
          finally
            FreeAndNil(LDailyData);
            FreeAndNil(lTempStr);
          end;
        end;
      end;
    finally
      if Assigned(LDataset) then
      begin
        lDataset.Dataset.Close;
        FreeAndNil(LDataset);
      end;
    end;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRWHDataSQLAgent.GetStationDailyData (AStationID : Integer; AData      : TStringList);
const OPNAME = 'TRWHDataSQLAgent.GetStationDailyData';
var
  LDecompressionStream : TZDecompressionStream;
  LBlobStream          : TStream;
  lDataset             : TAbstractModelDataset;
begin
  try
    AData.Clear;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if (Assigned(lDataset)) then
      begin
        lDataset.SetSQL('SELECT StationID, RainfallData FROM RainfallDailyData ' +
                        'WHERE StationID = :AStationID' );

        lDataset.SetParams(['AStationID'], [IntToStr(AStationID)]);
        lDataset.Dataset.Open;

        if not lDataset.DataSet.Eof then
        begin
          LBlobStream := lDataset.Dataset.CreateBlobStream(lDataset.Dataset.FieldByName('RainfallData'), bmRead);
          LDecompressionStream := TZDecompressionStream.Create(LBlobStream);
          try
            AData.LoadFromStream(LDecompressionStream);
          finally
            FreeAndNil(LBlobStream);
            FreeAndNil(LDecompressionStream);
          end;
        end;
      end;
    finally
      if Assigned(lDataset) then
      begin
        lDataset.Dataset.Close;
        FreeAndNil(lDataset);
      end;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRWHDataSQLAgent.GetStationRawDailyData(AStationID : Integer; AData      : TStringList);
const OPNAME = 'TRWHDataSQLAgent.GetStationDailyData';
var
  LDecompressionStream : TZDecompressionStream;
  LBlobStream          : TStream;
  lDataset             : TAbstractModelDataset;
begin
  try
    AData.Clear;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if (Assigned(lDataset)) then
      begin
        lDataset.SetSQL('SELECT StationID, RainfallData FROM RainfallRawDailyData ' +
                        'WHERE StationID = :AStationID' );

        lDataset.SetParams(['AStationID'], [IntToStr(AStationID)]);
        lDataset.Dataset.Open;

        if not lDataset.DataSet.Eof then
        begin
          LBlobStream := lDataset.Dataset.CreateBlobStream(lDataset.Dataset.FieldByName('RainfallData'), bmRead);
          LDecompressionStream := TZDecompressionStream.Create(LBlobStream);
          try
            AData.LoadFromStream(LDecompressionStream);
          finally
            FreeAndNil(LBlobStream);
            FreeAndNil(LDecompressionStream);
          end;
        end;
      end;
    finally
      if Assigned(lDataset) then
      begin
        lDataset.Dataset.Close;
        FreeAndNil(lDataset);
      end;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRWHDataSQLAgent.AddSelectedRainfallStation(ARainfallStation: TRainfallStation): boolean;
const OPNAME = 'TRWHDataSQLAgent.AddSelectedRainfallStation';
var
  lDataset : TAbstractModelDataset;
  LSQL     : string;
begin
  Result := False;
  try
    RemoveRainfallStationFromSelection(ARainfallStation);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if (Assigned(lDataset)) then
      begin
        LSQL := 'INSERT INTO RWHSelectedStations'+
                ' (Model,StudyAreaName,SubArea,Scenario,StationID,StationNumber,StationName)'+
                ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:StationID,:StationNumber,:StationName)';
        lDataset.SetSQL(LSQL);
        LDataSet.SetParams(
        ['Model', 'StudyAreaName', 'SubArea', 'Scenario'],
        [FAppModules.StudyArea.ModelCode, FAppModules.StudyArea.StudyAreaCode,
         FAppModules.StudyArea.SubAreaCode, FAppModules.StudyArea.ScenarioCode]);

        lDataset.SetParams(['StationID'], [IntToStr(ARainfallStation.StationID)]);
        lDataset.SetParams(['StationNumber'], [ARainfallStation.StationNumber]);
        lDataset.SetParams(['StationName'], [ARainfallStation.StationName]);
        LDataSet.ExecSQL;
        Result := True;
      end;
    finally
      FreeAndNil(lDataset);
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRWHDataSQLAgent.RemoveRainfallStationFromSelection(ARainfallStation: TRainfallStation): boolean;
const OPNAME = 'TRWHDataSQLAgent.RemoveRainfallStationFromSelection';
var
  lDataset : TAbstractModelDataset;
  LSQL     : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if (Assigned(lDataset)) then
      begin
        lSQL := 'DELETE FROM RWHSelectedStations WHERE ' + GetScenarioWhereClause + ' AND StationID = ' + IntToStr(ARainfallStation.StationID);
        lDataset.SetSQL(lSQL);
        LDataSet.ExecSQL;
        Result := True;
      end;
    finally
      FreeAndNil(lDataset);
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRWHDataSQLAgent.RemoveAllRainfallStationFromSelection : boolean;
const OPNAME = 'TRWHDataSQLAgent.RemoveRainfallStationFromSelection';
var
  lDataset : TAbstractModelDataset;
  LSQL     : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if (Assigned(lDataset)) then
      begin
        lSQL := 'DELETE * FROM RWHSelectedStations WHERE ' + GetScenarioWhereClause;
        LDataSet.SetSQL(lSQL);
        LDataSet.ExecSQL;
        Result := True;
      end;
    finally
      FreeAndNil(lDataset);
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;


function TRWHDataSQLAgent.GetMaxIdentifierSQL: string;
const OPNAME = 'TRWHDataSQLAgent.GetMaxIdentifierSQL';
begin
  Result := '';
  try
    Result := 'SELECT MAX(Identifier) AS MaxIdentifier FROM RWHRunConfig A WHERE ' +
              GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRWHDataSQLAgent.AddRunConfigurationData(ARunConfig: TRWHRunConfig;AUpdate:Boolean=False): boolean;
const OPNAME = 'TRWHDataSQLAgent.AddRunConfigurationData';
var
  lDataset : TAbstractModelDataset;
  LSQL     : string;
begin
  Result := False;
  try
    if(ARunConfig.Identifier <> NullInteger) then
      DeleteRunConfigurationData(ARunConfig);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if (Assigned(lDataset)) then
      begin
        if not AUpdate then
        begin
          lDataset.SetSQL(GetMaxIdentifierSQL);
          lDataset.DataSet.Open;
          ARunConfig.Identifier := lDataset.DataSet.FieldByName('MaxIdentifier').AsInteger+1;
          lDataset.DataSet.Close;
        end;
        LSQL := 'INSERT INTO RWHRunConfig'+
                ' (Model,StudyAreaName,SubArea,Scenario,Identifier,RunName,PeriodStartDate,PeriodEndDate,'+
                '  RunTypeID,RunStartVolume,RunStartLevel,RunStopLevel,RoofArea,RoofRunoffCoef,HouseHoldNumber,'+
                '  HouseHoldMembers,HouseHoldDemandPP,TankSize01,TankSize02,TankSize03,TankSize04,TankSize05,'+
                '  TankSize06,TankSize07,TankSize08,TankSize09,TankSize10)'+
                ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:Identifier,:RunName,:PeriodStartDate,:PeriodEndDate,'+
                '  :RunTypeID,:RunStartVolume,:RunStartLevel,:RunStopLevel,:RoofArea,:RoofRunoffCoef,:HouseHoldNumber,'+
                '  :HouseHoldMembers,:HouseHoldDemandPP,:TankSize01,:TankSize02,:TankSize03,:TankSize04,:TankSize05,'+
                '  :TankSize06,:TankSize07,:TankSize08,:TankSize09,:TankSize10)';
        lDataset.SetSQL(LSQL);
        LDataSet.ClearQueryParams(prAll);
        LDataSet.SetParams(
        ['Model', 'StudyAreaName', 'SubArea', 'Scenario'],
        [FAppModules.StudyArea.ModelCode, FAppModules.StudyArea.StudyAreaCode,
         FAppModules.StudyArea.SubAreaCode, FAppModules.StudyArea.ScenarioCode]);
        lDataset.SetParams(['Identifier'], [IntToStr(ARunConfig.Identifier)]);

        if(ARunConfig.RunName               <> '') then
          lDataset.SetParams(['RunName']             , [ARunConfig.RunName]);
        if(ARunConfig.PeriodStartDate       <> NullDateTime) then
          lDataset.SetParams(['PeriodStartDate']     ,[DateTimeToStr(ARunConfig.PeriodStartDate)]);
        if(ARunConfig.PeriodEndDate         <> NullDateTime) then
          lDataset.SetParams(['PeriodEndDate']       ,[DateTimeToStr(ARunConfig.PeriodEndDate)]);
        if(ARunConfig.RunTypeID             <> NullInteger) then
          lDataset.SetParams(['RunTypeID']           ,[IntToStr(ARunConfig.RunTypeID)]);
        if(ARunConfig.RunStartVolume        <> NullFloat) then
          lDataset.SetParams(['RunStartVolume']      ,[FloatTostr(ARunConfig.RunStartVolume)]);
        if(ARunConfig.RunStartLevel         <> NullFloat) then
          lDataset.SetParams(['RunStartLevel']       ,[FloatTostr(ARunConfig.RunStartLevel)]);
        if(ARunConfig.RunStopLevel          <> NullFloat) then
          lDataset.SetParams(['RunStopLevel']        ,[FloatTostr(ARunConfig.RunStopLevel)]);
        if(ARunConfig.RoofArea              <> NullFloat) then
          lDataset.SetParams(['RoofArea']            ,[FloatTostr(ARunConfig.RoofArea)]);
        if(ARunConfig.RoofRunoffCoef        <> NullFloat) then
          lDataset.SetParams(['RoofRunoffCoef']      ,[FloatTostr(ARunConfig.RoofRunoffCoef)]);
        if(ARunConfig.HouseHoldNumber       <> NullInteger) then
          lDataset.SetParams(['HouseHoldNumber']     ,[IntToStr(ARunConfig.HouseHoldNumber)]);
        if(ARunConfig.HouseHoldMembers      <> NullInteger) then
          lDataset.SetParams(['HouseHoldMembers']    ,[IntToStr(ARunConfig.HouseHoldMembers)]);
        if(ARunConfig.HouseHoldDemandPP     <> NullFloat) then
          lDataset.SetParams(['HouseHoldDemandPP']   ,[FloatTostr(ARunConfig.HouseHoldDemandPP)]);
        if(ARunConfig.TankSize01            <> NullFloat) then
          lDataset.SetParams(['TankSize01']          ,[FloatTostr(ARunConfig.TankSize01)]);
        if(ARunConfig.TankSize02            <> NullFloat) then
          lDataset.SetParams(['TankSize02']          ,[FloatTostr(ARunConfig.TankSize02)]);
        if(ARunConfig.TankSize03            <> NullFloat) then
          lDataset.SetParams(['TankSize03']          ,[FloatTostr(ARunConfig.TankSize03)]);
        if(ARunConfig.TankSize04            <> NullFloat) then
          lDataset.SetParams(['TankSize04']          ,[FloatTostr(ARunConfig.TankSize04)]);
        if(ARunConfig.TankSize05            <> NullFloat) then
          lDataset.SetParams(['TankSize05']          ,[FloatTostr(ARunConfig.TankSize05)]);
        if(ARunConfig.TankSize06            <> NullFloat) then
          lDataset.SetParams(['TankSize06']          ,[FloatTostr(ARunConfig.TankSize06)]);
        if(ARunConfig.TankSize07            <> NullFloat) then
          lDataset.SetParams(['TankSize07']          ,[FloatTostr(ARunConfig.TankSize07)]);
        if(ARunConfig.TankSize08            <> NullFloat) then
          lDataset.SetParams(['TankSize08']          ,[FloatTostr(ARunConfig.TankSize08)]);
        if(ARunConfig.TankSize09            <> NullFloat) then
          lDataset.SetParams(['TankSize09']          ,[FloatTostr(ARunConfig.TankSize09)]);
        if(ARunConfig.TankSize10            <> NullFloat) then
          lDataset.SetParams(['TankSize10']          ,[FloatTostr(ARunConfig.TankSize10)]);


        LDataSet.ExecSQL;
        ARunConfig.SavedInDB  := True;
        ARunConfig.Changed    := False;
        ARunConfig.Populated  := True;
        Result := True;
      end;
    finally
      FreeAndNil(lDataset);
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRWHDataSQLAgent.UpdateRunConfigurationData(ARunConfig: TRWHRunConfig): boolean;
const OPNAME = 'TRWHDataSQLAgent.RemoveRainfallStationFromSelection';
begin
  Result := False;
  try
    Result := AddRunConfigurationData(ARunConfig,True);
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRWHDataSQLAgent.DeleteRunConfigurationData(ARunConfig: TRWHRunConfig): boolean;
const OPNAME = 'TRWHDataSQLAgent.RemoveRainfallStationFromSelection';
var
  lDataset : TAbstractModelDataset;
  LSQL     : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if (Assigned(lDataset)) then
      begin
        lSQL := 'DELETE FROM RWHRunConfig WHERE ' + GetScenarioWhereClause + ' AND Identifier = ' + IntToStr(ARunConfig.Identifier);
        LDataSet.SetSQL(lSQL);
        LDataSet.ExecSQL;
        Result := True;
      end;
    finally
      FreeAndNil(lDataset);
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

end.
