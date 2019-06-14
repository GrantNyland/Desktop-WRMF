//
//
//  UNIT      : Contains TDivChannelDemandObject Class
//  AUTHOR    : Titi Ngubane(Arivia)
//  DATE      : 10/03/2003
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UDivChannelDemandObject;

interface

uses
  Classes, sysutils,contnrs,

  //  DWAF VCL
  UConstants,
  UBasicObjects,
  UAbstractDataObject;

type
    //File F10.dat

  TLineArray = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FLineType: TInteger;
    FLineValues : array[1..12] Of TDouble;
    procedure Reset;override;
    function Initialise: boolean;override;
  end;

  TFlowLine = class(TLineArray)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FFlowValue: TDouble;
    procedure Reset;override;
    function Initialise: boolean;override;
  end;

  TDiversionChannelHeading = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FDivChannelName : TString;
    FDivChannelNumber : TInteger;
    FChannelType : TInteger;
    procedure Reset;override;
    function Initialise: boolean;override;
  end;

  //Line 4
  TDiversionChannelProportionCounts = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FNodeNumber : TInteger;
    FResLevelsCount : TInteger;
    FRefFlowsCount :TInteger;
    procedure Reset;override;
    function Initialise: boolean;override;
  end;

  TDiversionChannelProportion = class(TAbstractDataObject)
  protected
    FPropotionContainer: TObjectList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetFlowProportion(AIndex: integer): TFlowLine;
  public
    procedure Reset;override;
    function Initialise: boolean;override;
    function AddProportionFlowArray:TFlowLine;
    function ProportionCount: integer;
    property FlowProportion[AIndex: integer]:TFlowLine read GetFlowProportion;
  end;

  //Channel type 1,2
  TDiversionChannelData12 = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    //Line 1
    FHeading:TDiversionChannelHeading;
    //Line 2
    FLine1Array: TLineArray;
    //Line 3
    FLine2Array: TLineArray;
    procedure Reset;override;
    function Initialise: boolean;override;
  end;

  //Channel type 3
  TDiversionChannelData3 = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    //Line 1
    FHeading:TDiversionChannelHeading;
    //Line 4
    FProportionCounts:TDiversionChannelProportionCounts;
    //Line 5
    FLineArray: TLineArray;
    //Line 6
    FChannelProportion:TDiversionChannelProportion;
    procedure Reset;override;
    function Initialise: boolean;override;
  end;

  TDivChannelDemandObject = class(TAbstractDataObject)
  protected
    FDiversionChannelContainer: TObjectList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetDiversionChannelData12(AIndex: integer): TDiversionChannelData12;
    function GetDiversionChannelData3(AIndex: integer): TDiversionChannelData3;
  public
    FF10ExtraLines: TStringList;
    procedure Reset;override;
    function Initialise: boolean;override;
    function DiversionChannelCount: integer;
    function IsDiversionChannelData12(AIndex: integer): boolean;
    function IsDiversionChannelData3(AIndex: integer): boolean;
    function AddDiversionChannelData12:TDiversionChannelData12;
    function AddDiversionChannelData3:TDiversionChannelData3;
    function GetDiversionChanell12(AChannelNumber: integer):TDiversionChannelData12;
    function GetDiversionChanell3(AChannelNumber: integer):TDiversionChannelData3;

    property DiversionChannelData12[AIndex: integer]:TDiversionChannelData12 read GetDiversionChannelData12;
    property DiversionChannelData3[AIndex: integer]:TDiversionChannelData3 read GetDiversionChannelData3;
  end;

implementation


uses UErrorHandlingOperations;

{ TLineArray }

procedure TLineArray.CreateMemberObjects;
const OPNAME = 'TLineArray.CreateMemberObjects';
var
  LCount: integer;
  LDouble : TDouble;
begin
  inherited;
  try
    FLineType := TInteger.Create;
    for LCount := 1 to 12 do
    begin
      LDouble := TDouble.Create;
      FLineValues[LCount] := LDouble;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TLineArray.DestroyMemberObjects;
const OPNAME = 'TLineArray.DestroyMemberObjects';
var
  LCount: integer;
begin
  inherited;
  try
    FLineType.Free;
    for LCount := 1 to 12 do
    begin
      FLineValues[LCount].Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TLineArray.Initialise: boolean;
const OPNAME = 'TLineArray.Initialise';
var
  LCount: integer;
begin
  Result := inherited Initialise;
  try
    FLineType.FData := 0;
    for LCount := 1 to 12 do
    begin
      FLineValues[LCount].FData := 0.0;
      FLineValues[LCount].FInitalised := False;
      FLineValues[LCount].FLength := 6;
      FLineValues[LCount].FDecimal := 2;
      FLineValues[LCount].FDefaultPadding := True;
      FLineValues[LCount].ShowDecimalPoint := False;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TLineArray.Reset;
const OPNAME = 'TLineArray.Reset';
begin
  inherited;
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TFlowLine }

procedure TFlowLine.CreateMemberObjects;
const OPNAME = 'TFlowLine.CreateMemberObjects';
begin
  inherited;
  try
    FFlowValue := TDouble.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFlowLine.DestroyMemberObjects;
const OPNAME = 'TFlowLine.DestroyMemberObjects';
begin
  inherited;
  try
    FFlowValue.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFlowLine.Initialise: boolean;
const OPNAME = 'TFlowLine.Initialise';
begin
  Result := inherited Initialise;
  try
    FFlowValue.FData := 0.0;
    FFlowValue.FInitalised := False;
    FFlowValue.FLength := 6;
    FFlowValue.FDecimal := 2;
    FFlowValue.FDefaultPadding := True;
    FFlowValue.ShowDecimalPoint := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFlowLine.Reset;
const OPNAME = 'TFlowLine.Reset';
begin
  inherited;
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TDiversionChannelHeading }

procedure TDiversionChannelHeading.CreateMemberObjects;
const OPNAME = 'TDiversionChannelHeading.CreateMemberObjects';
begin
  inherited;
  try
    FDivChannelName := TString.Create;
    FDivChannelNumber := TInteger.Create;
    FChannelType := TInteger.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionChannelHeading.DestroyMemberObjects;
const OPNAME = 'TDiversionChannelHeading.DestroyMemberObjects';
begin
  inherited;
  try
    FDivChannelName.Free;
    FDivChannelNumber.Free;
    FChannelType.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDiversionChannelHeading.Initialise: boolean;
const OPNAME = 'TDiversionChannelHeading.Initialise';
begin
  Result := inherited Initialise;
  try
    FDivChannelName.FData := '';
    FDivChannelName.FInitalised := False;
    FDivChannelName.FLength := 36;
    FDivChannelName.FDecimal := 0;
    FDivChannelName.FDefaultPadding := True;

    FDivChannelNumber.FInitalised := False;
    FDivChannelNumber.FData := 0;
    FDivChannelNumber.FLength := 6;
    FDivChannelNumber.FDecimal := 0;
    FDivChannelNumber.FDefaultPadding := True;

    FChannelType.FInitalised := False;
    FChannelType.FData := 0;
    FChannelType.FLength := 5;
    FChannelType.FDecimal := 0;
    FChannelType.FDefaultPadding := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionChannelHeading.Reset;
const OPNAME = 'TDiversionChannelHeading.Reset';
begin
  inherited;
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TDiversionChannelProportionCounts }

procedure TDiversionChannelProportionCounts.CreateMemberObjects;
const OPNAME = 'TDiversionChannelProportionCounts.CreateMemberObjects';
begin
  inherited;
  try
    FNodeNumber := TInteger.Create;
    FResLevelsCount := TInteger.Create;
    FRefFlowsCount := TInteger.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionChannelProportionCounts.DestroyMemberObjects;
const OPNAME = 'TDiversionChannelProportionCounts.DestroyMemberObjects';
begin
  inherited;
  try
    FNodeNumber.Free;
    FResLevelsCount.Free;
    FRefFlowsCount.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDiversionChannelProportionCounts.Initialise: boolean;
const OPNAME = 'TDiversionChannelProportionCounts.Initialise';
begin
  Result := inherited Initialise;
  try
    FNodeNumber.FInitalised := False;
    FNodeNumber.FData := 0;
    FNodeNumber.FLength := 6;
    FNodeNumber.FDecimal := 0;
    FNodeNumber.FDefaultPadding := True;

    FResLevelsCount.FInitalised := False;
    FResLevelsCount.FData := 0;
    FResLevelsCount.FLength := 6;
    FResLevelsCount.FDecimal := 0;
    FResLevelsCount.FDefaultPadding := True;

    FRefFlowsCount.FInitalised := False;
    FRefFlowsCount.FData := 0;
    FRefFlowsCount.FLength := 6;
    FRefFlowsCount.FDecimal := 0;
    FRefFlowsCount.FDefaultPadding := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionChannelProportionCounts.Reset;
const OPNAME = 'TDiversionChannelProportionCounts.Reset';
begin
  inherited;
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TDiversionChannelProportion }

procedure TDiversionChannelProportion.CreateMemberObjects;
const OPNAME = 'TDiversionChannelProportion.CreateMemberObjects';
begin
  inherited;
  try
    FPropotionContainer    := TObjectList.Create(True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionChannelProportion.DestroyMemberObjects;
const OPNAME = 'TDiversionChannelProportion.DestroyMemberObjects';
begin
  inherited;
  try
    FPropotionContainer.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDiversionChannelProportion.GetFlowProportion(AIndex: integer): TFlowLine;
const OPNAME = 'TDiversionChannelProportion.GetFlowProportion';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FPropotionContainer.Count) then
      Result := TFlowLine(FPropotionContainer.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDiversionChannelProportion.Initialise: boolean;
const OPNAME = 'TDiversionChannelProportion.Initialise';
begin
  Result := inherited Initialise;
  try
    FPropotionContainer.Clear;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionChannelProportion.Reset;
const OPNAME = 'TDiversionChannelProportion.Reset';
begin
  inherited;
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDiversionChannelProportion.ProportionCount: integer;
const OPNAME = 'TDiversionChannelProportion.ProportionCount';
begin
  Result := 0;
  try
    Result := FPropotionContainer.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDiversionChannelProportion.AddProportionFlowArray: TFlowLine;
const OPNAME = 'TDiversionChannelProportion.AddProportionFlowArray';
var
  LFlowLine:TFlowLine;
begin
  Result := nil;
  try
    LFlowLine := TFlowLine.Create;
    LFlowLine.Initialise;
    FPropotionContainer.Add(LFlowLine);
    Result := LFlowLine;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TDiversionChannelData12 }

procedure TDiversionChannelData12.CreateMemberObjects;
const OPNAME = 'TDiversionChannelData12.CreateMemberObjects';
begin
  inherited;
  try
    FHeading := TDiversionChannelHeading.Create;
    FLine1Array := TLineArray.Create;
    FLine2Array := TLineArray.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionChannelData12.DestroyMemberObjects;
const OPNAME = 'TDiversionChannelData12.DestroyMemberObjects';
begin
  inherited;
  try
    FHeading.Free;
    FLine1Array.Free;
    FLine2Array.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDiversionChannelData12.Initialise: boolean;
const OPNAME = 'TDiversionChannelData12.Initialise';
begin
  Result := inherited Initialise;
  try
    FHeading.Initialise;
    FLine1Array.Initialise;
    FLine2Array.Initialise;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionChannelData12.Reset;
const OPNAME = 'TDiversionChannelData12.Reset';
begin
  inherited Reset;
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TDivChannelDemandObject }

procedure TDivChannelDemandObject.CreateMemberObjects;
const OPNAME = 'TDivChannelDemandObject.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FDiversionChannelContainer := TObjectList.Create(True);
    FF10ExtraLines            := TStringList.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDivChannelDemandObject.DestroyMemberObjects;
const OPNAME = 'TDivChannelDemandObject.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FDiversionChannelContainer.Free;
    FF10ExtraLines.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDivChannelDemandObject.Initialise: boolean;
const OPNAME = 'TDivChannelDemandObject.Initialise';
begin
  Result := inherited Initialise;
  try
    FDiversionChannelContainer.Clear;
    FF10ExtraLines.Clear;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDivChannelDemandObject.Reset;
const OPNAME = 'TDivChannelDemandObject.Reset';
begin
  inherited Initialise;
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDivChannelDemandObject.DiversionChannelCount: integer;
const OPNAME = 'TDivChannelDemandObject.DiversionChannelCount';
begin
  Result := 0;
  try
    Result := FDiversionChannelContainer.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDivChannelDemandObject.AddDiversionChannelData12: TDiversionChannelData12;
const OPNAME = 'TDivChannelDemandObject.AddDiversionChannelData12';
var
  LDiversionChannelData12: TDiversionChannelData12;
begin
  Result := nil;
  try
    LDiversionChannelData12 := TDiversionChannelData12.Create;
    LDiversionChannelData12.Initialise;
    FDiversionChannelContainer.Add(LDiversionChannelData12);
    Result := LDiversionChannelData12;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDivChannelDemandObject.AddDiversionChannelData3: TDiversionChannelData3;
const OPNAME = 'TDivChannelDemandObject.AddDiversionChannelData3';
var
  LDiversionChannelData3: TDiversionChannelData3;
begin
  Result := nil;
  try
    LDiversionChannelData3 := TDiversionChannelData3.Create;
    LDiversionChannelData3.Initialise;
    FDiversionChannelContainer.Add(LDiversionChannelData3);
    Result := LDiversionChannelData3;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDivChannelDemandObject.GetDiversionChannelData12(AIndex: integer): TDiversionChannelData12;
const OPNAME = 'TDivChannelDemandObject.GetDiversionChannelData12';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FDiversionChannelContainer.Count) then
    begin
      if IsDiversionChannelData12(AIndex) then
      begin
        Result := TDiversionChannelData12(FDiversionChannelContainer.Items[AIndex]);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDivChannelDemandObject.GetDiversionChannelData3(AIndex: integer): TDiversionChannelData3;
const OPNAME = 'TDivChannelDemandObject.GetDiversionChannelData3';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FDiversionChannelContainer.Count) then
    begin
      if IsDiversionChannelData3(AIndex) then
      begin
        Result := TDiversionChannelData3(FDiversionChannelContainer.Items[AIndex]);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDivChannelDemandObject.IsDiversionChannelData12(AIndex: integer): boolean;
const OPNAME = 'TDivChannelDemandObject.IsDiversionChannelData12';
begin
  Result := False;
  try
    if(AIndex >= 0) and (AIndex < FDiversionChannelContainer.Count) then
    begin
      Result := FDiversionChannelContainer.Items[AIndex].ClassName = 'TDiversionChannelData12';
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDivChannelDemandObject.IsDiversionChannelData3(AIndex: integer): boolean;
const OPNAME = 'TDivChannelDemandObject.IsDiversionChannelData3';
begin
  Result := False;
  try
    if(AIndex >= 0) and (AIndex < FDiversionChannelContainer.Count) then
    begin
      Result := FDiversionChannelContainer.Items[AIndex].ClassName = 'TDiversionChannelData3';
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDivChannelDemandObject.GetDiversionChanell12(AChannelNumber: integer): TDiversionChannelData12;
const OPNAME = 'TDivChannelDemandObject.GetDiversionChanell12';
var
  LCount: integer;
begin
  Result := nil;
  try
     for LCount := 0 to FDiversionChannelContainer.Count - 1 do
     begin
       if IsDiversionChannelData12(LCount) then
       begin
         if(DiversionChannelData12[LCount].FHeading.FDivChannelNumber.FData = AChannelNumber) then
         begin
           Result := DiversionChannelData12[LCount];
           Break;
         end;
       end;
     end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDivChannelDemandObject.GetDiversionChanell3(AChannelNumber: integer): TDiversionChannelData3;
const OPNAME = 'TDivChannelDemandObject.GetDiversionChanell3';
var
  LCount: integer;
begin
  Result := nil;
  try
     for LCount := 0 to FDiversionChannelContainer.Count - 1 do
     begin
       if IsDiversionChannelData3(LCount) then
       begin
         if(DiversionChannelData3[LCount].FHeading.FDivChannelNumber.FData = AChannelNumber) then
         begin
           Result := DiversionChannelData3[LCount];
           Break;
         end;
       end;
     end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TDiversionChannelData3 }

procedure TDiversionChannelData3.CreateMemberObjects;
const OPNAME = 'TDiversionChannelData3.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FHeading := TDiversionChannelHeading.Create;
    FProportionCounts :=TDiversionChannelProportionCounts.Create;
    FLineArray := TLineArray.Create;
    FChannelProportion := TDiversionChannelProportion.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionChannelData3.DestroyMemberObjects;
const OPNAME = 'TDiversionChannelData3.DestroyMemberObjects';
begin
  inherited;
  try
    FHeading.Free;
    FProportionCounts.Free;
    FLineArray.Free;
    FChannelProportion.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDiversionChannelData3.Initialise: boolean;
const OPNAME = 'TDiversionChannelData3.Initialise';
begin
  Result := inherited Initialise;
  try
    FHeading.Initialise;
    FProportionCounts.Initialise;
    FLineArray.Initialise;
    FChannelProportion.Initialise;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionChannelData3.Reset;
const OPNAME = 'TDiversionChannelData3.Reset';
begin
  inherited;
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
