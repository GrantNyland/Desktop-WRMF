//
//
//  UNIT      : Contains TBoxPlotData Classes
//  AUTHOR    : Dziedzi  Ramulondi (Arivia)
//  DATE      : 2006/03/23
//  COPYRIGHT : Copyright © 2006 DWAF
//
//
unit UBoxPlotData;

interface
uses
  Classes,
  SysUtils,
  Contnrs,
  UAbstractObject;
type
  TBoxPlotData = class(TObject)
  protected
    FUseDateValue : boolean;
    FXValueDate   : TDateTime;
    FXValueDouble : double;
    FYValues      : TOneDimensionDoubleArray;
  public
    procedure AfterConstruction; override;
    function Populate(ACommaTextData: string) : Boolean; overload;
    function Populate(AYear: integer;AData: TIntegerArray) : Boolean; overload;
    function Populate(AColIndex : integer; AValues : TTwoDimensionIntegerArray) : Boolean; overload;
    function AddValue(AValue:double) : Boolean; 
    function Populated: boolean;
    function RemoveZeroValues: boolean;
    function PercValue(APerc:double):double;
    procedure SortData;
    property XValueDate        : TDateTime       read FXValueDate;
    property XValueDouble      : double          read FXValueDouble;
    property YValues           : TOneDimensionDoubleArray    read FYValues;
    property UseDateValue      : boolean         read FUseDateValue write FUseDateValue;
  end;

  TBoxPlotDataList = class(TObject)
  protected
    FSorted    : boolean;
    FContainer : TStringList;
    function Get_BoxPlotDataByIndex(AIndex : integer): TBoxPlotData;
    function Get_BoxPlotDataByXValue(AXValue : double): TBoxPlotData;
    function FormatXValue(AXValue : double): string;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function Count : integer;
    function CreateBoxPlotData(AXValue: double): TBoxPlotData;
    property BoxPlotDataByIndex[AIndex : integer]: TBoxPlotData read Get_BoxPlotDataByIndex;
    property BoxPlotDataByXValue[AXValue : double]: TBoxPlotData read Get_BoxPlotDataByXValue;
  end;
implementation

uses
  Math,
  UConstants,
  UUtilities,
  UErrorHandlingOperations;

{ TBoxPlotData }

procedure TBoxPlotData.AfterConstruction;
const OPNAME = 'TBoxPlotData.AfterConstruction';
begin
  inherited;
  try
    FUseDateValue := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TBoxPlotData.Populated: boolean;
const OPNAME = 'TBoxPlotData.Populated';
begin
  Result := False;
  try
    Result := Length(FYValues) > 0;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TBoxPlotData.Populate(ACommaTextData: string): Boolean;
const OPNAME = 'TBoxPlotData.Populate';
var
  LDate           : TDateTime;
  LIndex          : Integer;
  LStringValues   : TStringList;
  //LSortedValues   : TStringList;
begin
  Result := False;
  try
    if(Trim(ACommaTextData) = '') then Exit;
    FXValueDouble := 0.0;
    FXValueDate := 0.0;
    Finalize(FYValues);
    LStringValues   := TStringList.Create;
    //LSortedValues   := TStringList.Create;
    try
      LStringValues.CommaText := ACommaTextData;
      if FUseDateValue  then
      begin
        LDate       := StrToDate(LStringValues[0]);
        FXValueDate := Trunc(LDate);
      end
      else
      begin
        FXValueDouble  := StrToFloat(LStringValues[0]);
      end;

      SetLength(FYValues,LStringValues.Count - 1);
      for LIndex := 1 to LStringValues.Count - 1 do
         FYValues[LIndex-1] := StrToFloat(LStringValues[LIndex]);
      SortFloatArray(FYValues,soAscending);
      {LSortedValues.Sorted := True;
      LSortedValues.Duplicates := dupAccept;
      for LIndex := 1 to LStringValues.Count - 1 do
         LSortedValues.Add(FormatFloat('0000000000000000.000',StrToFloat(LStringValues[LIndex])));
      for LIndex := 0 to LSortedValues.Count - 1 do
         FYValues[LIndex] := StrToFloat(LSortedValues[LIndex]);
      }
    finally
      LStringValues.Free;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TBoxPlotData.Populate(AYear: integer;AData: TIntegerArray): Boolean;
const OPNAME = 'TBoxPlotData.Populate';
var
  LIndex          : Integer;
begin
  Result := False;
  try
    if(Length(AData) = 0) then Exit;
    FXValueDouble := AYear;
    FXValueDate := AYear;
    Finalize(FYValues);
    SetLength(FYValues,Length(AData));
    for LIndex := Low(AData) to High(AData) do
      FYValues[LIndex] := AData[LIndex];
    SortFloatArray(FYValues,soAscending);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TBoxPlotData.Populate(AColIndex: integer;AValues: TTwoDimensionIntegerArray): Boolean;
const OPNAME = 'TBoxPlotData.Populate';
var
  LIndex          : Integer;
begin
  Result := False;
  try
    if(AColIndex < Low(AValues)) or (AColIndex > High(AValues)) then Exit;

    FXValueDouble := AColIndex;
    FXValueDate := AColIndex;
    Finalize(FYValues);
    SetLength(FYValues,Length(AValues[AColIndex]));
    for LIndex := Low(AValues[AColIndex]) to High(AValues[AColIndex]) do
      FYValues[LIndex] := AValues[AColIndex,LIndex];
    SortFloatArray(FYValues,soAscending);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TBoxPlotData.PercValue(APerc: double): double;
const OPNAME = 'TBoxPlotData.PercValue';
var
  LIndex     : integer;
  //LLength    : integer;
  LPosition  : double;
  LRemainder : double;
  //LValue1,
  //LValue2: double;
begin
  Result := NullFloat;
  try
    if Populated then
    begin
      if(Length(FYValues) = 1) then
        Result := FYValues[0]
      else
      if(APerc <= 0.0) then
        Result := FYValues[0]
      else
      if(APerc >= 100.0) then
        Result := FYValues[High(FYValues)]
      else
      begin
        LPosition     := (Aperc/100.0)*(Length(FYValues)+1);
        LRemainder    := Frac(LPosition);
        if(LRemainder < 0.01) then
          LIndex        := Trunc(LPosition)
        else
          LIndex        := Trunc(LPosition)+1;
        Result := FYValues[LIndex];

        {Aperc   := Aperc/100.0;
        LLength := Length(FYValues)-1;
        LIndex := Round(Aperc * LLength);
        Result := FYValues[LIndex];
        }
        {
        LPosition     := (Aperc/100.0)*(Length(FYValues)+1);
        LRemainder    := Frac(LPosition);
        LIndex        := Trunc(LPosition)-1;
        if(LIndex < 0) then
          LIndex      := 0;
        if (LRemainder < 0.00001) then
        begin
          Result := FYValues[LIndex];
        end
        else
        begin
          LValue1       := 0.0;
          LValue2       := 0.0;
          if ((LIndex) < High(FYValues)) then
          begin
            LValue1 := FYValues[LIndex];
            LValue2 := FYValues[LIndex+1];
          end
          else
          if (LIndex = High(FYValues)) then
          begin
            LValue1 := FYValues[LIndex - 1];
            LValue2 := FYValues[LIndex];
          end;
          Result := LValue1 + (LValue2 - LValue1)*LRemainder;
        end;
        }
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TBoxPlotData.RemoveZeroValues: boolean;
const OPNAME = 'TBoxPlotData.RemoveZeroValues';
var
  LCount,
  LIndex: integer;
begin
  Result := False;
  try
    if Populated then
    begin
      LCount := 0;
      while (LCount <= High(FYValues)) and (Length(FYValues) > 1) do
      begin
         if(FYValues[LCount] = 0.0) then
         begin
           for LIndex := LCount to High(FYValues)-1 do
               FYValues[LIndex] := FYValues[LIndex+1];
           SetLength(FYValues,Length(FYValues)-1);
         end
         else
           LCount := LCount + 1;
      end;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TBoxPlotData.AddValue(AValue: double): Boolean;
const OPNAME = 'TBoxPlotData.AddValue';
begin
  Result := False;
  try
    SetLength(FYValues,Length(FYValues)+1);
    FYValues[High(FYValues)] := AValue;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TBoxPlotData.SortData;
const OPNAME = 'TBoxPlotData.SortData';
begin
  try
    SortFloatArray(FYValues,soAscending);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TBoxPlotDataList }

procedure TBoxPlotDataList.AfterConstruction;
const OPNAME = 'TBoxPlotDataList.AfterConstruction';
begin
  inherited;
  try
    FSorted    := True;
    FContainer   := TStringList.Create;
    FContainer.Sorted := True;
    FContainer.Duplicates := dupIgnore;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TBoxPlotDataList.BeforeDestruction;
const OPNAME = 'TBoxPlotDataList.BeforeDestruction';
var
  LIndex : integer;
begin
  inherited;
  try
    for LIndex := 0 to FContainer.Count-1 do
    begin
      if(FContainer.Objects[LIndex] <> nil) then
         FContainer.Objects[LIndex].Free;
    end;
    FreeAndNil(FContainer);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TBoxPlotDataList.Count: integer;
const OPNAME = 'TBoxPlotDataList.CreateBoxPlotData';
begin
  Result := 0;
  try
    Result := FContainer.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TBoxPlotDataList.CreateBoxPlotData(AXValue: double): TBoxPlotData;
const OPNAME = 'TBoxPlotDataList.CreateBoxPlotData';
begin
  Result := nil;
  try
    Result := Get_BoxPlotDataByXValue(AXValue);
    if(Result = nil) then
    begin
      Result := TBoxPlotData.Create;
      Result.FXValueDouble := AXValue;
      Result.FXValueDate := AXValue;
      FContainer.AddObject(FormatXValue(AXValue),Result);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TBoxPlotDataList.FormatXValue(AXValue: double): string;
const OPNAME = 'TBoxPlotDataList.CreateBoxPlotData';
begin
  Result := '';
  try
    Result := FormatFloat('0000000000',AXValue);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TBoxPlotDataList.Get_BoxPlotDataByIndex(AIndex: integer): TBoxPlotData;
const OPNAME = 'TBoxPlotDataList.Get_BoxPlotDataByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FContainer.Count) then
    Result := TBoxPlotData(FContainer.Objects[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TBoxPlotDataList.Get_BoxPlotDataByXValue(AXValue: double): TBoxPlotData;
const OPNAME = 'TBoxPlotDataList.Get_BoxPlotDataByXValue';
var
  LIndex : integer;
begin
  Result := nil;
  try
    LIndex := FContainer.IndexOf(FormatXValue(AXValue));
    if(LIndex >= 0) then
      Result := TBoxPlotData(FContainer.Objects[LIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
