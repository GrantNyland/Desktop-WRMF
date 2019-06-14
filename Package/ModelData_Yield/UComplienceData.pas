unit UComplienceData;

interface
uses
  Classes,
  Contnrs;
type
  TBarValues = class(TObject)
  protected
    FYearValue : Integer;
    FXValue    : Double;
    FMinYValue : Double;
    FMaxYValue : Double;
  public
    procedure Initialise;
    procedure Populate(AYears : integer;AXValue,AMinYValue,AMaxYValue : double);
    property YearValue : Integer read FYearValue write FYearValue;
    property XValue    : Double read FXValue write FXValue;
    property MinYValue : Double read FMinYValue write FMinYValue;
    property MaxYValue : Double read FMaxYValue write FMaxYValue;
  end;

  TLineValues = class(TObject)
  protected
    FXValue : Double;                                         
    FYValue : Double;
  public
    property XValue : Double read FXValue write FXValue;
    property YValue : Double read FYValue write FYValue;
  end;

  TComplianceGraphObject = class(TObject)
  protected
    FBarValuesContainer    : TObjectList;
    FLineValuesContainer   : TObjectList;
    function GetLineValuesContainerCount  : integer;
    function GetBarValuesContainerCount   : integer;
    function AddBarValues                 : TBarValues;
    function AddLineValues                : TLineValues;
  public
    procedure Initialise;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function GetBarValuesByIndex (AIndex : integer) : TBarValues;
    function GetLineValuesByIndex (AIndex : integer) : TLineValues;
    function Populate(AData: TStrings): boolean;
    property LineValuesContainerCount : Integer read GetLineValuesContainerCount;
    property BarValuesContainerCount : Integer read GetBarValuesContainerCount;
  end;

implementation

uses
  SysUtils,
  UErrorHandlingOperations;
{ TBarValues }

procedure TBarValues.Populate(AYears : integer; AXValue, AMinYValue,AMaxYValue : double);
const OPNAME = 'TBarValues.Populate';
begin
  try
    FYearValue  := AYears;
    FXValue     := AXValue;
    FMinYValue  := AMinYValue;
    FMaxYValue  := AMaxYValue;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TBarValues.Initialise;
const OPNAME = 'TBarValues.Initialise';
begin
  try
    FYearValue := -1;
    FXValue    := -1;
    FMinYValue := -1;
    FMaxYValue := -1;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TComplianceGraphObject }

function TComplianceGraphObject.AddBarValues : TBarValues;
const OPNAME = 'TComplianceGraphObject.AddBarValues';
begin
  Result := nil;
  try
    Result := TBarValues.Create;
    FBarValuesContainer.Add(Result);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TComplianceGraphObject.AfterConstruction;
const OPNAME = 'TComplianceGraphObject.AfterConstruction';
begin
  try
    inherited AfterConstruction;
    FBarValuesContainer     := TObjectList.Create(True);
    FLineValuesContainer    := TObjectList.Create(True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TComplianceGraphObject.BeforeDestruction;
const OPNAME = 'TComplianceGraphObject.BeforeDestruction';
begin
  try
    inherited BeforeDestruction;
    FBarValuesContainer.Free;
    FLineValuesContainer.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TComplianceGraphObject.GetBarValuesByIndex(AIndex : integer) : TBarValues;
const OPNAME = 'TComplianceGraphObject.GetBarValuesByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FBarValuesContainer.Count) then
      Result := TBarValues(FBarValuesContainer.Items[AIndex])
    else
      Result := nil;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TComplianceGraphObject.Initialise;
const OPNAME = 'TComplianceGraphObject.Initialise';
begin
  try
    FBarValuesContainer.Clear;
    FLineValuesContainer.Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TComplianceGraphObject.GetBarValuesContainerCount : integer;
const OPNAME = 'TComplianceGraphObject.GetBarValuesContainerCount';
begin
  Result := -1;
  try
    result := FBarValuesContainer.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TComplianceGraphObject.AddLineValues : TLineValues;
const OPNAME = 'TComplianceGraphObject.AddLineValues';
begin
  Result := nil;
  try
    Result := TLineValues.Create;
    FLineValuesContainer.Add(Result);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TComplianceGraphObject.GetLineValuesContainerCount : integer;
const OPNAME = 'TComplianceGraphObject.GetLineValuesContainerCount';
begin
  Result := -1;
  try
    result := FLineValuesContainer.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TComplianceGraphObject.GetLineValuesByIndex(AIndex : integer) : TLineValues;
const OPNAME = 'TComplianceGraphObject.GetLineValuesByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FLineValuesContainer.Count) then
       Result := TLineValues(FLineValuesContainer.Items[AIndex])
     else
       Result := nil;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TComplianceGraphObject.Populate(AData: TStrings): boolean;
const OPNAME = 'TComplianceGraphObject.Populate';
var
  LCount          : Integer;
  LStringTemp     : String;
  LBarValues      : TBarValues;
  LLineValues     : TLineValues;
begin
  Result := False;
  try
    for LCount := 2 to AData.Count - 1 do
    begin

      LStringTemp := AData[LCount];
      if (LStringTemp <> '') then
      begin
        if(Length(LStringTemp) >= 22) then
        begin
          LLineValues := AddLineValues;
          LLineValues.XValue     := StrToFloat(Trim(Copy(LStringTemp, 12, 3)));
          LLineValues.YValue     := StrToFloat(Trim(Copy(LStringTemp, 22, 3)));
        end;

        if(Length(LStringTemp) >= 80) then
        begin
          LBarValues  := AddBarValues;
          LBarValues.XValue      := StrToFloat(Trim(Copy(LStringTemp, 46, 3)));
          LBarValues.MinYValue   := StrToFloat(Trim(Copy(LStringTemp, 64, 3)));
          LBarValues.MaxYValue   := StrToFloat(Trim(Copy(LStringTemp, 72, 3)));
          LBarValues.YearValue   := StrToInt(Trim(Copy(LStringTemp, 80, 3)));
        end;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

