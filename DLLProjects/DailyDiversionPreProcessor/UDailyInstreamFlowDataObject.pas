//
//
//  UNIT      : Contains TDailyInstreamFlowDataObject Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 20/08/2006
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UDailyInstreamFlowDataObject;

interface
uses
  Classes,
  sysutils,
  contnrs,
  VCL.Controls,

  //  DWAF VCL
  UAbstractDataObject,
  UBasicObjects,
  UConstants;
type
  TDailyInstreamFlowData = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FIdentifier  : TInteger;
    FInstreamDate : TString;
    FAvgFlow     : TDouble;
    FQualityCode : TInteger;
    procedure Reset;override;
    function Initialise: boolean;override;
  end;

  TDailyInstreamFlowDataObject = class(TAbstractDataObject)
  protected
    FDailyInstreamFlowDataList : TObjectList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FFileHeader : TString;
    FExtraLines: TStringlist;
    FStationID : integer;
    FStartDate : TString;
    FEndDate : TString;
    function DailyFlowDataCount : integer;
    function AddDailyInstreamFlowData : TDailyInstreamFlowData;
    function GetDailyInstreamFlowDataByIndex(AIndex : integer) : TDailyInstreamFlowData;
    function Initialise: Boolean; override;
    procedure Reset;override;

end;

implementation
uses
  UErrorHandlingOperations;
{ TDailyInstreamFlowData }

procedure TDailyInstreamFlowData.CreateMemberObjects;
const OPNAME = 'TDailyInstreamFlowData.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FIdentifier  := TInteger.Create;
    FInstreamDate := TString.Create;
    FAvgFlow     := TDouble.Create;
    FQualityCode := TInteger.Create;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyInstreamFlowData.DestroyMemberObjects;
const OPNAME = 'TDailyInstreamFlowData.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
    FIdentifier.Free;
    FInstreamDate.Free;
    FAvgFlow.Free;
    FQualityCode.Free;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyInstreamFlowData.Initialise: boolean;
const OPNAME = 'TDailyInstreamFlowData.Initialise';
begin
  Result := False;
  try
    FIdentifier.FData := 0;
    FIdentifier.FInitalised := False;
    FIdentifier.FLength := 5;
    FIdentifier.FDecimal := 0;
    FIdentifier.FDefaultPadding := True;

    FInstreamDate.FData := '';
    FInstreamDate.FInitalised := False;
    FInstreamDate.FLength := 8;
    FInstreamDate.FDecimal := 0;
    FInstreamDate.FDefaultPadding := True;

    FAvgFlow.FData := 0;
    FAvgFlow.FInitalised := False;
    FAvgFlow.FLength := 9;
    FIdentifier.FDecimal := 3;
    FAvgFlow.FDefaultPadding := True;
    FAvgFlow.ShowDecimalPoint := True;

    FQualityCode.FData := 0;
    FQualityCode.FInitalised := False;
    FQualityCode.FLength := 4;
    FQualityCode.FDecimal := 0;
    FQualityCode.FDefaultPadding := True;
    
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyInstreamFlowData.Reset;
const OPNAME = 'TDailyInstreamFlowData.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TDailyInstreamFlowDataObject }

function TDailyInstreamFlowDataObject.AddDailyInstreamFlowData: TDailyInstreamFlowData;
const OPNAME = 'TDailyInstreamFlowDataObject.AddDailyInstreamFlowData';
begin
  Result := nil;
  try
    Result := TDailyInstreamFlowData.Create;
    FDailyInstreamFlowDataList.Add(Result);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyInstreamFlowDataObject.CreateMemberObjects;
const OPNAME = 'TDailyInstreamFlowDataObject.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FDailyInstreamFlowDataList := TObjectList.Create;
    FExtraLines  := TStringlist.Create;
    FFileHeader  := TString.Create;
    FStartDate := TString.Create;
    FEndDate := TString.Create;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyInstreamFlowDataObject.DailyFlowDataCount: integer;
const OPNAME = 'TDailyInstreamFlowDataObject.DailyFlowDataCount';
begin
  Result := 0;
  try
    Result := FDailyInstreamFlowDataList.Count;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyInstreamFlowDataObject.DestroyMemberObjects;
const OPNAME = 'TDailyInstreamFlowDataObject.DestroyMemberObjects';
begin
  try
    FreeAndNil(FDailyInstreamFlowDataList);
    FExtraLines.Free;
    FFileHeader.Free;
    FEndDate.Free;
    FStartDate.Free;
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyInstreamFlowDataObject.GetDailyInstreamFlowDataByIndex(AIndex: integer): TDailyInstreamFlowData;
const OPNAME = 'TDailyInstreamFlowDataObject.GetDailyInstreamFlowDataByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) and ((FDailyInstreamFlowDataList.Count-1) >= AIndex) then
      Result := TDailyInstreamFlowData(FDailyInstreamFlowDataList.Items[AIndex]);   
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyInstreamFlowDataObject.Initialise: Boolean;
const OPNAME = 'TDailyInstreamFlowDataObject.Initialise';
begin
  Result := False;
  try
    FExtraLines.Clear;
    FFileHeader.FData := '';
    FFileHeader.FInitalised := False;
    FFileHeader.FLength := 50;
    FFileHeader.FDecimal := 0;
    FFileHeader.FDefaultPadding := True;
    FDailyInstreamFlowDataList.Clear;
    
    FStartDate.FData := '';
    FStartDate.FInitalised := False;
    FStartDate.FLength := 8;
    FStartDate.FDecimal := 0;
    FStartDate.FDefaultPadding := True;

    FEndDate.FData := '';
    FEndDate.FInitalised := False;
    FEndDate.FLength := 8;
    FEndDate.FDecimal := 0;
    FEndDate.FDefaultPadding := True;

    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyInstreamFlowDataObject.Reset;
const OPNAME = 'TDailyInstreamFlowDataObject.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
