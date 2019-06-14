{******************************************************************************}
{*  UNIT      : Contains the class TWeatherEvents                             *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2005/04/04                                                    *}
{*  COPYRIGHT : Copyright © 2005 DWAF                                         *}
{******************************************************************************}

unit UWeatherEvents;

interface

uses
  Classes,
  VCL.ComCtrls,
  UAbstractObject,
  VoaimsCom_TLB;

type
  TWeatherEvents = class(TAbstractAppObject, IWeatherEvents)
  protected
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    function FindWeatherEvents (AStartDateTime : TDateTime;
                                AEndDateTime   : TDateTime;
                                const AArea    : WideString): WideString; safecall;
    function GetAreas : WideString; safecall;
    function EarliestDate : TDateTime; safecall;
    function LatestDate : TDateTime; safecall;
  end;

implementation

uses
  VCL.Controls,
  SysUtils,
  UDataSetType,
  UErrorHandlingOperations;

{ TWeatherEvents }

function TWeatherEvents._AddRef: Integer;
const OPNAME = 'TWeatherEvents._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWeatherEvents._Release: Integer;
const OPNAME = 'TWeatherEvents._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWeatherEvents.FindWeatherEvents (AStartDateTime : TDateTime;
                                           AEndDateTime   : TDateTime;
                                           const AArea    : WideString): WideString;
const OPNAME = 'TWeatherEvents.FindWeatherEvents';
var
  lDataSet  : TAbstractModelDataset;
  lSQL      : string;
  lStartStr : string;
  lEndStr   : string;
  lTempStr  : string;
  lTempList : TStringList;
  lAndFlag  : Boolean;
begin
  Result := '';
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if (Assigned(lDataSet)) then
      begin
        lAndFlag  := FALSE;
        lSQL      := 'SELECT * FROM WeatherEvents ';
        if (AStartDateTime <> 0) then
        begin
          lStartStr := FormatDateTime('yyyymmddhhnnsszz', AStartDateTime);
          if (lAndFlag) then
            lSQL := lSQL + ' AND StartDateTime >= ' + QuotedStr(lStartStr)
          else
            lSQL := lSQL + ' WHERE StartDateTime >= ' + QuotedStr(lStartStr);
          lAndFlag := TRUE;
        end;
        if (AEndDateTime <> 0) then
        begin
          lEndStr   := FormatDateTime('yyyymmddhhnnsszz', AEndDateTime);
          if (lAndFlag) then
            lSQL := lSQL + ' AND EndDateTime <= ' + QuotedStr(lEndStr)
          else
            lSQL := lSQL + ' WHERE EndDateTime <= ' + QuotedStr(lEndStr);
          lAndFlag := TRUE;
        end;
        if (Trim(AArea) <> '') then
        begin
          if (lAndFlag) then
            lSQL := lSQL + ' AND Area = ' + QuotedStr(AArea)
          else
            lSQL := lSQL + ' WHERE Area = ' + QuotedStr(AArea);
        end;
        lSQL := lSQL + ' ORDER BY StartDateTime';

        lTempList := TStringList.Create;
        try
          lDataSet.SetSQL(lSQL);
          lDataset.DataSet.Open;
          lDataSet.DataSet.First;
          while (NOT lDataset.DataSet.EOF) do
          begin
            lTempStr := Trim(lDataSet.DataSet.FieldByName('StartDateTime').AsString) + '|' +
                        Trim(lDataSet.DataSet.FieldByName('EndDateTime').AsString)   + '|' +
                        Trim(lDataSet.DataSet.FieldByName('Area').AsString)          + '|' +
                        Trim(lDataSet.DataSet.FieldByName('Event').AsString)         + '|' +
                        Trim(lDataSet.DataSet.FieldByName('Comments').AsString);
            lTempList.Add(lTempStr);            
            lDataSet.DataSet.Next;
          end;
          Result := lTempList.CommaText;
        finally
          FreeAndNil(lTempList);
        end;
      end;
    finally
      lDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWeatherEvents.GetAreas : WideString;
const OPNAME = 'TWeatherEvents.GetAreas';
var
  lDataSet  : TAbstractModelDataset;
  lSQL      : string;
  lTempStr  : string;
  lTempList : TStringList;
begin
  Result := '';
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if (Assigned(lDataSet)) then
      begin
        lTempList := TStringList.Create;
        try
          lSQL := 'SELECT DISTINCT (Area) AS AreaName FROM WeatherEvents ORDER BY Area';
          lDataSet.SetSQL(lSQL);
          lDataset.DataSet.Open;
          lDataSet.DataSet.First;
          while (NOT lDataset.DataSet.EOF) do
          begin
            lTempStr := Trim(lDataSet.DataSet.FieldByName('AreaName').AsString);
            lTempList.Add(lTempStr);
            lDataSet.DataSet.Next;
          end;
          Result := lTempList.CommaText;
        finally
          FreeAndNil(lTempList);
        end;
      end;
    finally
      lDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWeatherEvents.EarliestDate : TDateTime;
const OPNAME = 'TWeatherEvents.EarliestDate';
var
  lDataSet  : TAbstractModelDataset;
  lSQL      : string;
  lTempStr  : string;
begin
  Result := Date;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if (Assigned(lDataSet)) then
      begin
        lSQL := 'SELECT MIN(StartDateTime) AS StartDate FROM WeatherEvents';
        lDataSet.SetSQL(lSQL);
        lDataset.DataSet.Open;
        lDataSet.DataSet.First;
        if (NOT lDataset.DataSet.EOF) then
        begin
          lTempStr := Trim(lDataSet.DataSet.FieldByName('StartDate').AsString);
          try
            Result := EncodeDate(StrToInt(Copy(lTempStr, 1, 4)),
                                 StrToInt(Copy(lTempStr, 5, 2)),
                                 StrToInt(Copy(lTempStr, 7, 2)));
          except
          end;
        end;
      end;
    finally
      lDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWeatherEvents.LatestDate : TDateTime;
const OPNAME = 'TWeatherEvents.LatestDate';
var
  lDataSet  : TAbstractModelDataset;
  lSQL      : string;
  lTempStr  : string;
begin
  Result := Date;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if (Assigned(lDataSet)) then
      begin
        lSQL := 'SELECT MAX(EndDateTime) AS EndDate FROM WeatherEvents';
        lDataSet.SetSQL(lSQL);
        lDataset.DataSet.Open;
        lDataSet.DataSet.First;
        if (NOT lDataset.DataSet.EOF) then
        begin
          lTempStr := Trim(lDataSet.DataSet.FieldByName('EndDate').AsString);
          try
            Result := EncodeDate(StrToInt(Copy(lTempStr, 1, 4)),
                                 StrToInt(Copy(lTempStr, 5, 2)),
                                 StrToInt(Copy(lTempStr, 7, 2)));
          except
          end;
        end;
      end;
    finally
      lDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
