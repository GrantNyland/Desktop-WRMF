//
//
//  UNIT      : Contains TDailyFlowDataObject Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 20/08/2006
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UDailyFlowDataObject;

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
  TDailyFlowData = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FIdentifier  : TInteger;
    FDiversionDate : TString;
    FAvgFlow     : TDouble;
    FQualityCode : TInteger;
    procedure Reset;override;
    function Initialise: boolean;override;
  end;

  TDailyFlowDataObject = class(TAbstractDataObject)
  protected
    FDailyFlowDataList : TObjectList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FFileHeader : TString;
    FExtraLines: TStringlist;
    FStationID : integer;
    FStartDate : TString;
    FEndDate : TString;
    function DailyFlowDataCount : integer;
    function AddDailyFlowData : TDailyFlowData;
    function GetDailyFlowDataByIndex(AIndex : integer) : TDailyFlowData;
    function Initialise: Boolean; override;
    procedure Reset;override;

end;

implementation
uses
  UErrorHandlingOperations;
{ TDailyFlowData }

procedure TDailyFlowData.CreateMemberObjects;
const OPNAME = 'TDailyFlowData.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FIdentifier  := TInteger.Create;
    FDiversionDate := TString.Create;
    FAvgFlow     := TDouble.Create;
    FQualityCode := TInteger.Create;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyFlowData.DestroyMemberObjects;
const OPNAME = 'TDailyFlowData.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
    FIdentifier.Free;
    FDiversionDate.Free;
    FAvgFlow.Free;
    FQualityCode.Free;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyFlowData.Initialise: boolean;
const OPNAME = 'TDailyFlowData.Initialise';
begin
  Result := False;
  try
    FIdentifier.FData := 0;
    FIdentifier.FInitalised := False;
    FIdentifier.FLength := 5;
    FIdentifier.FDecimal := 0;
    FIdentifier.FDefaultPadding := True;

    FDiversionDate.FData := '';
    FDiversionDate.FInitalised := False;
    FDiversionDate.FLength := 8;
    FDiversionDate.FDecimal := 0;
    FDiversionDate.FDefaultPadding := True;

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

procedure TDailyFlowData.Reset;
const OPNAME = 'TDailyFlowData.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TDailyFlowDataObject }

function TDailyFlowDataObject.AddDailyFlowData: TDailyFlowData;
const OPNAME = 'TDailyFlowDataObject.AddDailyFlowData';
begin
  Result := nil;
  try
    Result := TDailyFlowData.Create;
    FDailyFlowDataList.Add(Result);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyFlowDataObject.CreateMemberObjects;
const OPNAME = 'TDailyFlowDataObject.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FDailyFlowDataList := TObjectList.Create;
    FExtraLines  := TStringlist.Create;
    FFileHeader  := TString.Create;
    FStartDate := TString.Create;
    FEndDate := TString.Create;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyFlowDataObject.DailyFlowDataCount: integer;
const OPNAME = 'TDailyFlowDataObject.DailyFlowDataCount';
begin
  Result := 0;
  try
    Result := FDailyFlowDataList.Count;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyFlowDataObject.DestroyMemberObjects;
const OPNAME = 'TDailyFlowDataObject.DestroyMemberObjects';
begin
  try
    FreeAndNil(FDailyFlowDataList);
    FExtraLines.Free;
    FFileHeader.Free;
    FEndDate.Free;
    FStartDate.Free;
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyFlowDataObject.GetDailyFlowDataByIndex(AIndex: integer): TDailyFlowData;
const OPNAME = 'TDailyFlowDataObject.GetDailyFlowDataByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) and ((FDailyFlowDataList.Count-1) >= AIndex) then
      Result := TDailyFlowData(FDailyFlowDataList.Items[AIndex]);   
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyFlowDataObject.Initialise: Boolean;
const OPNAME = 'TDailyFlowDataObject.Initialise';
begin
  Result := False;
  try
    FExtraLines.Clear;
    FFileHeader.FData := '';
    FFileHeader.FInitalised := False;
    FFileHeader.FLength := 50;
    FFileHeader.FDecimal := 0;
    FFileHeader.FDefaultPadding := True;
    FDailyFlowDataList.Clear;

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

procedure TDailyFlowDataObject.Reset;
const OPNAME = 'TDailyFlowDataObject.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
