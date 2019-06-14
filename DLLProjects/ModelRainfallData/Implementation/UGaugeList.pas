{******************************************************************************}
{*  UNIT      : Contains the class TRainGaugeList.                            *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2005/04/01                                                    *}
{*  COPYRIGHT : Copyright © 2005 DWAF                                         *}
{******************************************************************************}

unit UGaugeList;

interface
                                                
uses
  Contnrs,
  Classes,
  Windows,
  VCL.Dialogs,
  UGauge,
  RainfallCom_TLB,
  UAbstractObject;

type

  TRainGaugeList = class(TAbstractAppObject, IRainGaugeList)
  private
    FGaugeList   : TStringList;
    FFileLoaded  : boolean;
    procedure ClearLists;
    procedure ClearUserGauges;
  protected
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function CastGaugeByIndex(AIndex: Integer): TRainGauge;
  public
    function LoadFromDatabase(AProgressFunction: TNotifyEvent = nil): Boolean;
    function LoadUserGaugesFromDatabase: Boolean;
    function TotalCount: Integer; safecall;
    function SelectedCount: Integer; safecall;
    function GetSelectedGauges: WideString; safecall;
    procedure DeSelectAll; safecall;
    procedure SaveToDB; safecall;
    procedure SelectByDistance(ALatitude: Double; ALongitude: Double; ADistance: Double;
                               AReplaceSelection: WordBool); safecall;
    procedure SelectByRectangle(ALeft: Integer; ATop: Integer; ABottom: Integer; ARight: Integer;
                                AReplaceSelection: WordBool); safecall;
    procedure SelectByName(const AName: WideString; AReplaceSelection: WordBool); safecall;
    procedure SelectByNumber(const ANumber: WideString; AReplaceSelection: WordBool); safecall;
    procedure SelectByStationID(AGaugeID: Integer; AReplaceSelection: WordBool); safecall;
    function GetGaugeByIndex(AIndex: Integer): IRainGauge; safecall;
    function GetGaugeByNumber(const ANumber: WideString): IRainGauge; safecall;

    property FileLoaded : boolean read FFileLoaded write FFileLoaded;
  end;

implementation

uses
  SysUtils,
  Math,
  UUtils,
  UDataSetType,
  UErrorHandlingOperations,
  Types;

{* TRainGaugeList *************************************************************}

procedure TRainGaugeList.CreateMemberObjects;
const OPNAME = 'TRainGaugeList.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FGaugeList := TStringList.Create;
    FGaugeList.Sorted := TRUE;

    FileLoaded := FALSE;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRainGaugeList.DestroyMemberObjects;
const OPNAME = 'TRainGaugeList.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    ClearLists;
    FreeAndNil(FGaugeList);
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRainGaugeList._AddRef: Integer;
const OPNAME = 'TRainGaugeList._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainGaugeList._Release: Integer;
const OPNAME = 'TRainGaugeList._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainGaugeList.LoadFromDatabase (AProgressFunction : TNotifyEvent): Boolean;
const OPNAME = 'TRainGaugeList.LoadFromDatabase';
var
  LGaugeNumber : string;
  LLatitude    : string;
  LLongitude   : string;
  LGauge       : TRainGauge;
  LDataset     : TAbstractModelDataset;
  lSQL         : string;
  LErrorFree   : boolean;
  lIndex       : integer;
begin
  Result := False;
  try
    LErrorFree := FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataset);
    try
      if LErrorFree then
      begin
        ClearLists;

        lSQL := 'SELECT * FROM RainfallStations ORDER BY StationNumber';
        LDataset.DataSet.Close;
        LDataset.SetSQL(lSQL);
        LDataset.DataSet.Open;

        while (NOT LDataset.DataSet.Eof) do
        begin
          LGaugeNumber := Trim(LDataset.Dataset.FieldByName('StationNumber').AsString);
          LLatitude    := Trim(LDataset.Dataset.FieldByName('StationLatitude').AsString);
          LLongitude   := Trim(LDataset.Dataset.FieldByName('StationLongitude').AsString);

          LGauge := TRainGauge.Create(FAppModules);
          lIndex := FGaugeList.AddObject(LGaugeNumber, LGauge);
          LGauge.Populate(lIndex,
                          Trim(LDataset.Dataset.FieldByName('StationName').AsString),
                          LDataset.Dataset.FieldByName('StationID').AsInteger,
                          LGaugeNumber,
                          Copy(LGaugeNumber, 1, 4),
                          StrToInt(LLatitude),
                          StrToInt(LLongitude),
                          Trim(LDataset.Dataset.FieldByName('WR90').AsString) = 'Y');
          if Assigned(AProgressFunction) then
            AProgressFunction(nil);
          LDataset.DataSet.Next;
        end;
        FileLoaded := TRUE;
        Result := FileLoaded;
      end;
    finally
      if Assigned(LDataset) then
      begin
        LDataset.DataSet.Close;
        FreeAndNil(LDataset);
      end;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRainGaugeList.LoadUserGaugesFromDatabase : Boolean;
const OPNAME = 'TRainGaugeList.LoadUserGaugesFromDatabase';
var
  lGaugeNumber : string;
  lLatitude    : string;
  lLongitude   : string;
  lGauge       : TRainGauge;
  lDataset     : TAbstractModelDataset;
  lSQL         : string;
  lIndex       : integer;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if (Assigned(lDataset)) then
      begin
        ClearUserGauges;

        lSQL := 'SELECT * FROM RainfallUserStations ' +
                ' ORDER BY Source, StationNumber';
        LDataset.DataSet.Close;
        LDataset.SetSQL(lSQL);
        LDataset.DataSet.Open;

        while (NOT LDataset.DataSet.Eof) do
        begin
          LGaugeNumber := Trim(LDataset.Dataset.FieldByName('StationNumber').AsString);
          LLatitude    := Trim(LDataset.Dataset.FieldByName('StationLatitude').AsString);
          LLongitude   := Trim(LDataset.Dataset.FieldByName('StationLongitude').AsString);

          LGauge := TRainGauge.Create(FAppModules);
          lIndex := FGaugeList.AddObject(LGaugeNumber, LGauge);
          LGauge.Populate(lIndex,
                          Trim(LDataset.Dataset.FieldByName('StationName').AsString),
                          LDataset.Dataset.FieldByName('StationID').AsInteger,
                          LGaugeNumber,
                          Trim(LDataset.Dataset.FieldByName('Source').AsString),
                          StrToInt(LLatitude),
                          StrToInt(LLongitude),
                          FALSE);
          LDataset.DataSet.Next;
        end;

        FileLoaded := TRUE;
        Result := FileLoaded;
      end;
    finally
      if Assigned(LDataset) then
      begin
        LDataset.DataSet.Close;
        FreeAndNil(LDataset);
      end;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRainGaugeList.SaveToDB;
const OPNAME = 'TRainGaugeList.SaveToDB';
var
  LDataset   : TAbstractModelDataset;
  lSQL       : string;
  LIndex     : integer;
begin
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataset);
    try
      if (Assigned(LDataset)) then
      begin
        lDataset.DataSet.Close;
        lSQL := 'DELETE FROM RainfallProjectGauges ' +
                ' WHERE Model = '       + QuotedStr(FAppModules.StudyArea.ModelCode) +
                ' AND StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +
                ' AND SubArea = '       + QuotedStr(FAppModules.StudyArea.SubAreaCode) +
                ' AND Scenario = '      + QuotedStr(FAppModules.StudyArea.ScenarioCode);
        LDataset.DataSet.Close;
        LDataset.SetSQL(lSQL);
        LDataset.ExecSQL();

        lSQL := 'INSERT INTO RainfallProjectGauges (Model, StudyAreaName, SubArea, Scenario, StationID) ' +
                'VALUES (:AModel,:AStudyAreaName,:ASubArea,:AScenario,:AStationID) ;';
        LDataset.DataSet.Close;
        LDataset.SetSQL(lSQL);

        for LIndex := 0 to FGaugeList.Count - 1 do
        begin
          if TRainGauge(FGaugeList.Objects[LIndex]).Selected then
          begin
            LDataset.SetParams(['AModel','AStudyAreaName','ASubArea','AScenario','AStationID'],
              [FAppModules.StudyArea.ModelCode,
               FAppModules.StudyArea.StudyAreaCode,
               FAppModules.StudyArea.SubAreaCode,
               FAppModules.StudyArea.ScenarioCode,
               IntToStr(TRainGauge(FGaugeList.Objects[LIndex]).GaugeID)]);
            LDataset.ExecSQL();
          end;
        end;
      end;
    finally
      if Assigned(LDataset) then
      begin
        LDataset.Dataset.Close;
        FreeAndNil(LDataset);
      end;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRainGaugeList.ClearLists;
const OPNAME = 'TRainGaugeList.ClearLists';
var
  LIndex : integer;
begin
  try
    for LIndex := 0 to FGaugeList.Count - 1 do
    begin
      FGaugeList.Objects[LIndex].Free;
      FGaugeList.Objects[LIndex] := nil;
    end;
    FGaugeList.Clear;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRainGaugeList.ClearUserGauges;
const OPNAME = 'TRainGaugeList.ClearUserGauges';
var
  lIndex : integer;
  lGauge : TRainGauge;
begin
  try
    lIndex := 0;
    while (lIndex < FGaugeList.Count) do
    begin
      lGauge := CastGaugeByIndex(LIndex);
      if (lGauge.GaugeID > 100000) then
      begin
        FGaugeList.Objects[LIndex].Free;
        FGaugeList.Delete(LIndex);
      end
      else
        lIndex := lIndex + 1;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRainGaugeList.TotalCount : integer;
const OPNAME = 'TRainGaugeList.TotalCount';
begin
  Result := 0;
  try
    Result := FGaugeList.Count;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRainGaugeList.DeSelectAll;
const OPNAME = 'TRainGaugeList.DeSelectAll';
var
  lIndex : integer;
  lGauge : IRainGauge;
begin
  try
    for lIndex := 0 to FGaugeList.Count - 1 do
    begin
      lGauge := GetGaugeByIndex(lIndex);
      lGauge.Selected := FALSE;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRainGaugeList.SelectedCount : integer;
const OPNAME = 'TRainGaugeList.SelectedCount';
var
  lIndex : integer;
  lCount : integer;
  lGauge : IRainGauge;
begin
  Result := 0;
  try
    lCount := 0;
    for lIndex := 0 to FGaugeList.Count - 1 do
    begin
      lGauge := GetGaugeByIndex(lIndex);
      if (lGauge.Selected) then
       lCount := lCount + 1;
    end;
    Result := lCount;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRainGaugeList.SelectByRectangle (ALeft             : Integer;
                                            ATop              : Integer;
                                            ABottom           : Integer;
                                            ARight            : Integer;
                                            AReplaceSelection : WordBool);
const OPNAME = 'TRainGaugeList.SelectByRectangle';
var
  LSwapTemp  : integer;
  lIndex     : integer;
  lGauge     : IRainGauge;
begin
  try
    if ALeft > ARight then
    begin
      LSwapTemp := ARight;
      ARight    := ALeft;
      ALeft     := LSwapTemp;
    end;
    if ABottom > ATop then
    begin
      LSwapTemp := ATop;
      ATop      := ABottom;
      ABottom   := LSwapTemp;
    end;
    for lIndex := 0 to FGaugeList.Count - 1 do
    begin
      lGauge := GetGaugeByIndex(lIndex);
      if ((lGauge.Latitude >= ABottom) AND (lGauge.Latitude <= ATop) AND
          (lGauge.Longitude >= ALeft) AND (lGauge.Longitude <= ARight)) then
        lGauge.Selected := TRUE
      else
      if AReplaceSelection then
        lGauge.Selected := FALSE;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRainGaugeList.GetGaugeByIndex (AIndex : integer) : IRainGauge;
const OPNAME = 'TRainGaugeList.GetGaugeByIndex';
begin
  Result := nil;
  try
    if (AIndex < FGaugeList.Count) then
      Result := TRainGauge(FGaugeList.Objects[AIndex]);
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRainGaugeList.CastGaugeByIndex (AIndex : integer) : TRainGauge;
const OPNAME = 'TRainGaugeList.CastGaugeByIndex';
begin
  Result := nil;
  try
    if (AIndex < FGaugeList.Count) then
      Result := TRainGauge(FGaugeList.Objects[AIndex]);
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRainGaugeList.GetGaugeByNumber (const ANumber : WideString) : IRainGauge;
const OPNAME = 'TRainGaugeList.GetGaugeByNumber';
var
  LIndex : integer;
begin
  Result := nil;
  try
    LIndex := FGaugeList.IndexOf(ANumber);
    if (LIndex >= 0) then
      Result := TRainGauge(FGaugeList.Objects[LIndex]);
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRainGaugeList.SelectByStationID (AGaugeID          : integer;
                                            AReplaceSelection : WordBool);
const OPNAME = 'TRainGaugeList.SelectByStationID';
var
  lIndex : integer;
  lGauge : IRainGauge;
begin
  try
    for lIndex := 0 to FGaugeList.Count - 1 do
    begin
      lGauge := GetGaugeByIndex(lIndex);
      if (lGauge.GaugeID = AGaugeID) then
        lGauge.Selected := TRUE
      else
      if AReplaceSelection then
        lGauge.Selected := FALSE;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRainGaugeList.SelectByName (const AName       : WideString;
                                       AReplaceSelection : WordBool);
const OPNAME = 'TRainGaugeList.SelectByName';
var
  lIndex : integer;
  lGauge : IRainGauge;
  lType  : integer;
  lMatch : boolean;
  lName  : string;
begin
  try
    lName := AName;
    if CriteriaHasOnlyOneStarInTheMiddle(lName) then
    begin
      ShowMessage( lName + FAppModules.Language.GetString('Message.InvalidSearchCriteria'));
      Exit;
    end;
    if (Copy(lName, 1, 11) = '<Pos></Pos>') then
    begin
      lType := 1;
      lName := Copy(lName, 12, Length(lName) - 11);
    end
    else
    if (Pos('*', lName) > 0) or (Pos('?', lName) > 0) then
    begin
      lType := 2;
      if Length(lName) < 4 then
      begin
        showMessage(lName + FAppModules.Language.GetString('Message.EnterCharacters'));
        Exit;
      end;
    end
    else
      lType := 3;
    for lIndex := 0 to FGaugeList.Count - 1 do
    begin
      lGauge := GetGaugeByIndex(lIndex);
      lMatch := FALSE;
      case lType of
        1 : lMatch := Pos(Uppercase(lName), Uppercase(lGauge.GaugeName)) > 0;
        2 : lMatch := WildCardCompare(lName, lGauge.GaugeName);
        3 : lMatch := Trim(Uppercase(lGauge.GaugeName)) = Trim(UpperCase(lName));
      else
      end;
      if (lMatch) then
        lGauge.Selected := TRUE
      else
      if AReplaceSelection then
        lGauge.Selected := FALSE;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRainGaugeList.SelectByNumber (const ANumber     : WideString;
                                         AReplaceSelection : WordBool);
const OPNAME = 'TRainGaugeList.SelectByNumber';
var
  lIndex  : integer;
  lGauge  : IRainGauge;
  lType   : integer;
  lMatch  : boolean;
  LGaugeNumber : string;
begin
  try
    if CriteriaHasOnlyOneStarInTheMiddle(ANumber) then
    begin
      ShowMessage( ANumber + FAppModules.Language.GetString('Message.InvalidCriteria'));
      Exit;
    end;
    if (Pos('*', ANumber) > 0) or (Pos('?', ANumber) > 0) then
      lType := 1
    else
      lType := 2;
    for lIndex := 0 to FGaugeList.Count - 1 do
    begin
      lGauge := GetGaugeByIndex(lIndex);
      LGaugeNumber := lGauge.GaugeNumber;
      lMatch := FALSE;
      case lType of
        1 : lMatch := WildCardCompare(ANumber, lGauge.GaugeNumber);
        2 : lMatch := Trim(WithoutSpaces(LGaugeNumber)) = Trim(WithoutSpaces(ANumber));
      else
      end;
      if (lMatch) then
        lGauge.Selected := TRUE
      else
      if AReplaceSelection then
        lGauge.Selected := FALSE;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRainGaugeList.SelectByDistance (ALatitude         : double;
                                           ALongitude        : double;
                                           ADistance         : double;
                                           AReplaceSelection : WordBool);
const OPNAME = 'TRainGaugeList.SelectByDistance';
var
  lIndex     : integer;
  lGauge     : IRainGauge;
  lGaugeLat  : double;
  lGaugeLong : double;
  lGaugeDist : double;
  lTemp      : double;
  lRadLat    : double;
  lRadLong   : double;
begin
  try
    lRadLat  := DegToRad(ALatitude);
    lRadLong := DegToRad(ALongitude);
    for lIndex := 0 to FGaugeList.Count - 1 do
    begin
      lGauge   := GetGaugeByIndex(lIndex);
      lGaugeLat  := DegToRad(lGauge.Latitude / 60);
      lGaugeLong := DegToRad(lGauge.Longitude / 60);
      lTemp      := lGaugeLong - lRadLong;
      lGaugeDist := 1.852 * 180 * 60 / pi *
                    ArcCos(Sin(lRadLat) * Sin(lGaugeLat) +
                           Cos(lRadLat) * Cos(lGaugeLat) * Cos(lTemp));
      if (lGaugeDist <= ADistance) then
        lGauge.Selected := TRUE
      else
      if (AReplaceSelection) then
        lGauge.Selected := FALSE;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRainGaugeList.GetSelectedGauges : WideString;
const OPNAME = 'TRainGaugeList.GetSelectedGauges';
var
  LIndex   : integer;
  lGaugeNr : string;
  lGauge   : IRainGauge;
  lStrList : TStringList;
begin
  try
    lStrList := TStringList.Create;
    try
      for LIndex := 0 to FGaugeList.Count - 1 do
      begin
        lGauge := GetGaugeByIndex(LIndex);
        if (lGauge.Selected) then
        begin
          lGaugeNr := lGauge.GaugeNumber;
          if (lGauge.IsInWR90) then
            lGaugeNr := lGaugeNr + ' *';
          lStrList.Add(lGaugeNr);
        end;
      end;
      Result := lStrList.CommaText;
    finally
      FreeAndNil(lStrList);
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

end.
