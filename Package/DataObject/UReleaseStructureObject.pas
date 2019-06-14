//
//
//  UNIT      : Contains TReleaseStructureObject Class
//  AUTHOR    : Titi Ngubane(Arivia)
//  DATE      : 15/03/2003
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UReleaseStructureObject;

interface

uses
  Classes, sysutils,contnrs,

  //  DWAF VCL
  UConstants,
  UBasicObjects,
  UAbstractDataObject;
const
  MaxRefNodeNumber             = 500;
  MaxMonthlyControlStructCount = 500;
  MaxAnnualControlStructCount  = 500;
type
  TReleaseControlStructureData = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    //File F14.dat
    //Line 2
    FIFRChannelNumber : TInteger;
    FReferenceNodeCount : TInteger;
    FLagInMonthsCount      : TInteger;
    FPointsCount :TInteger;
    FIFRLoss :TInteger;
    FIFRUnknown :TInteger;

    //Line 3
    FRefNodeNumber  : TStringList;
    FMonthlyIFRLoss : TStringList;

    procedure Reset;override;
    function Initialise: boolean;override;
  end;

  TInflowVariableLine  = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public

    //Line 4 type
    FInflowVariable  : Array[MinReleaseStructure..MaxReleaseStructure] Of TDouble;
    FReleaseVariable : Array[MinReleaseStructure..MaxReleaseStructure] Of TDouble;

     procedure Reset;override;
    function Initialise: boolean;override;
  end;

  TReleaseControlStructureDetails = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    //Line 2..3
    FControlStructureData : TReleaseControlStructureData;
    //Line 4
    FInflowVariableLine : TObjectList;
     procedure Reset;override;
    function Initialise: boolean;override;
    function AddInflowVariableLine : TInflowVariableLine;
  end;

  TReferenceChannel = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    //File F14.dat
    //Line 6
    FChannelNumber   : TInteger;
    FNodeCount       : TInteger;
    FClassCount      : TInteger;
    FCalculateOption : TDouble;
    procedure Reset;override;
    function Initialise: boolean;override;
  end;

  TReferenceRelease = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    //File F14.dat
    //Line 8
    FAnualInflow   : TDouble;
    FMonthlyRelease : Array[MinReleaseStructure..MaxReleaseStructure] Of TDouble;
    procedure Reset;override;
    function Initialise: boolean;override;
  end;

  TReference = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    //Line 6
    FReferenceChannel   : TReferenceChannel;

    //Line 7
    FReferenceNodeNumbers: TStringList;

    //Line 8
    FReferenceReleaseList:TObjectList;

    procedure Reset;override;
    function AddReferenceRelease:TReferenceRelease;
    function ReferenceReleaseByIndex(AIndex: integer):TReferenceRelease;
    function Initialise: boolean;override;
  end;


  TReleaseControlStructureObject = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    //Line 1
    FControlStructCount : TInteger;
    FInflowOption       : TInteger;

    //Line 2..4
    FReleaseControlStructureDetails : TObjectList;

    //Line 5
    FReferenceChannelsCount: TInteger;
    //Line 6..8
    FReferenceDetails : TObjectList;

    FF14ExtraLines: TStringList;
    function AddReleaseControlStructureDetails : TReleaseControlStructureDetails;
    function AddReference : TReference;
    function ReferenceByIndex(AIndex: integer):TReference;
    procedure Reset;override;
    function Initialise: boolean;override;
  end;

implementation


uses UErrorHandlingOperations;

{TReleaseControlStructureData}

procedure TReleaseControlStructureData.CreateMemberObjects;
const OPNAME = 'TReleaseControlStructureData.CreateMemberObjects';
begin
  try
    //File F14.dat
    FIFRChannelNumber    := TInteger.Create;
    FReferenceNodeCount  := TInteger.Create;
    FLagInMonthsCount    := TInteger.Create;
    FPointsCount         := TInteger.Create;
    FIFRLoss                := TInteger.Create;
    FIFRUnknown             := TInteger.Create;
    FMonthlyIFRLoss      := TStringList.Create;
    FRefNodeNumber       := TStringList.Create;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReleaseControlStructureData.DestroyMemberObjects;
const OPNAME = 'TReleaseControlStructureData.DestroyMemberObjects';
begin
  try
    //File F14.dat
    FIFRChannelNumber.Free;
    FReferenceNodeCount.Free;
    FLagInMonthsCount.Free;
    FPointsCount.Free;
    FIFRLoss.Free;
    FIFRUnknown.Free;
    FMonthlyIFRLoss.Free;
    FRefNodeNumber.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReleaseControlStructureData.Initialise: boolean;
const OPNAME = 'TReleaseControlStructureData.Initialise';
begin
  Result := False;
  try
    FIFRChannelNumber.FData := 0;
    FIFRChannelNumber.FInitalised := False;
    FIFRChannelNumber.FLength := 6;
    FIFRChannelNumber.FDecimal := 0;
    FIFRChannelNumber.FDefaultPadding := True;

    FReferenceNodeCount.FData := 0;
    FReferenceNodeCount.FInitalised := False;
    FReferenceNodeCount.FLength := 6;
    FReferenceNodeCount.FDecimal := 0;
    FReferenceNodeCount.FDefaultPadding := True;

    FLagInMonthsCount.FData := 0;
    FLagInMonthsCount.FInitalised := False;
    FLagInMonthsCount.FLength := 6;
    FLagInMonthsCount.FDecimal := 0;
    FLagInMonthsCount.FDefaultPadding := True;

    FPointsCount.FData := 0;
    FPointsCount.FInitalised := False;
    FPointsCount.FLength := 6;
    FPointsCount.FDecimal := 0;
    FPointsCount.FDefaultPadding := False;

    FIFRLoss.FData := 0;
    FIFRLoss.FInitalised := False;
    FIFRLoss.FLength := 6;
    FIFRLoss.FDecimal := 0;
    FIFRLoss.FDefaultPadding := False;

    FIFRUnknown.FData := 0;
    FIFRUnknown.FInitalised := False;
    FIFRUnknown.FLength := 6;
    FIFRUnknown.FDecimal := 0;
    FIFRUnknown.FDefaultPadding := False;

    FMonthlyIFRLoss.Clear;
    FRefNodeNumber.Clear;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReleaseControlStructureData.Reset;
const OPNAME = 'TReleaseControlStructureData.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{TInflowVariableLine}

procedure TInflowVariableLine.CreateMemberObjects;
const OPNAME = 'TInflowVariableLine.CreateMemberObjects';
var
  LCount: integer;
  LDouble : TDouble;
begin
  try
    //line 4
    for LCount := MinReleaseStructure to MaxReleaseStructure do
    begin
      LDouble := TDouble.Create;
      FReleaseVariable[LCount] := LDouble;
      LDouble := TDouble.Create;
      FInflowVariable[LCount] := LDouble;
     end;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TInflowVariableLine.DestroyMemberObjects;
const OPNAME = 'TInflowVariableLine.DestroyMemberObjects';
var
  LCount: integer;
begin
  try
    //line 4
    for LCount := MinReleaseStructure to MaxReleaseStructure do
    begin
      FReleaseVariable[LCount].Free;
      FInflowVariable[LCount].Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TInflowVariableLine.Initialise: boolean;
const OPNAME = 'TInflowVariableLine.Initialise';
var
  LCount: integer;
begin
  Result := False;
  try
    for LCount := MinReleaseStructure to MaxReleaseStructure do
    begin
      FReleaseVariable[LCount].FData := 0;
      FReleaseVariable[LCount].FInitalised := False;
      FReleaseVariable[LCount].FLength := 10;
      FReleaseVariable[LCount].FDecimal := 3;
      FReleaseVariable[LCount].FDefaultPadding := True;
      FReleaseVariable[LCount].ShowDecimalPoint := False;

      FInflowVariable[LCount].FData := 0;
      FInflowVariable[LCount].FInitalised := False;
      FInflowVariable[LCount].FLength := 10;
      FInflowVariable[LCount].FDecimal := 3;
      FInflowVariable[LCount].FDefaultPadding := True;
      FInflowVariable[LCount].ShowDecimalPoint := False;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TInflowVariableLine.Reset;
const OPNAME = 'TInflowVariableLine.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{TReleaseControlStructureDetails}

procedure TReleaseControlStructureDetails.CreateMemberObjects;
const OPNAME = 'TReleaseControlStructureDetails.CreateMemberObjects';
begin
  try
    //File F14.dat
    FControlStructureData := TReleaseControlStructureData.Create;
    FInflowVariableLine         := TObjectList.Create(True);
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReleaseControlStructureDetails.DestroyMemberObjects;
const OPNAME = 'TReleaseControlStructureDetails.DestroyMemberObjects';
begin
  try
    //File F14.dat
    FControlStructureData.Free;
    FInflowVariableLine.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReleaseControlStructureDetails.Initialise: boolean;
const OPNAME = 'TReleaseControlStructureDetails.Initialise';
begin
  Result := False;
  try
    FControlStructureData.Initialise;
    FInflowVariableLine.Clear;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReleaseControlStructureDetails.Reset;
const OPNAME = 'TReleaseControlStructureDetails.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReleaseControlStructureDetails.AddInflowVariableLine : TInflowVariableLine;
const OPNAME = 'TReleaseControlStructureDetails.AddInflowVariableLine';
var
  LInflowVariableLine :TInflowVariableLine;
Begin
  Result := nil;
  try
    LInflowVariableLine := TInflowVariableLine.Create;
    FInflowVariableLine.Add(LInflowVariableLine);
    Result := LInflowVariableLine;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TReleaseControlStructureObject}

procedure TReleaseControlStructureObject.CreateMemberObjects;
const OPNAME = 'TReleaseControlStructureObject.CreateMemberObjects';
begin
  try
    //File F14.dat
    FControlStructCount             := TInteger.Create;
    FInflowOption                   := TInteger.Create;
    FReleaseControlStructureDetails := TObjectList.Create(True);
    FReferenceChannelsCount         := TInteger.Create;
    FReferenceDetails               := TObjectList.Create(True);
    FF14ExtraLines                  := TStringList.Create;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReleaseControlStructureObject.DestroyMemberObjects;
const OPNAME = 'TReleaseControlStructureObject.DestroyMemberObjects';
begin
  try
    //File F14.dat
    FControlStructCount.Free;
    FInflowOption.Free;
    FReleaseControlStructureDetails.Free;
    FReferenceChannelsCount.Free;
    FReferenceDetails.Free;
    FF14ExtraLines.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReleaseControlStructureObject.Initialise: boolean;
const OPNAME = 'TReleaseControlStructureObject.Initialise';
Begin
  Result := False;
  try
    FControlStructCount.FData := 0;
    FControlStructCount.FInitalised := False;
    FControlStructCount.FLength := 6;
    FControlStructCount.FDecimal := 0;
    FControlStructCount.FDefaultPadding := False;

    FInflowOption.FData := 0;
    FInflowOption.FInitalised := False;
    FInflowOption.FLength := 6;
    FInflowOption.FDecimal := 0;
    FInflowOption.FDefaultPadding := False;

    FReleaseControlStructureDetails.Clear;

    FReferenceChannelsCount.FData := 0;
    FReferenceChannelsCount.FInitalised := False;
    FReferenceChannelsCount.FLength := 6;
    FReferenceChannelsCount.FDecimal := 0;
    FReferenceChannelsCount.FDefaultPadding := False;
    FReferenceDetails.Clear;

    FF14ExtraLines.Clear;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReleaseControlStructureObject.Reset;
const OPNAME = 'TReleaseControlStructureObject.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReleaseControlStructureObject.AddReleaseControlStructureDetails: TReleaseControlStructureDetails;
const OPNAME = 'TReleaseControlStructureObject.AddReleaseControlStructureDetails';
var
  LReleaseControlStructureDetails :TReleaseControlStructureDetails;
Begin
  Result := nil;
  try
    LReleaseControlStructureDetails := TReleaseControlStructureDetails.Create;
    FReleaseControlStructureDetails.Add(LReleaseControlStructureDetails);
    Result := LReleaseControlStructureDetails;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReleaseControlStructureObject.AddReference : TReference;
const OPNAME = 'TReleaseControlStructureObject.AddReference';
var
  LReference:TReference;
begin
  Result := nil;
  try
    LReference := TReference.Create;
    FReferenceDetails.Add(LReference);
    Result := LReference;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReleaseControlStructureObject.ReferenceByIndex(AIndex: integer): TReference;
const OPNAME = 'TReleaseControlStructureObject.ReferenceByIndex';
begin
  Result := Nil;
  try
    if(AIndex >= 0) and (AIndex < FReferenceDetails.Count) then
     Result := TReference(FReferenceDetails[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TReferenceChannel }

procedure TReferenceChannel.CreateMemberObjects;
const OPNAME = 'TReferenceChannel.CreateMemberObjects';
begin
  inherited;
  try
    FChannelNumber    := TInteger.Create;
    FNodeCount        := TInteger.Create;
    FClassCount       := TInteger.Create;
    FCalculateOption  := TDouble.Create;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReferenceChannel.DestroyMemberObjects;
const OPNAME = 'TReferenceChannel.DestroyMemberObjects';
begin
  inherited;
  try
    FChannelNumber.Free;
    FNodeCount.Free;
    FClassCount.Free;
    FCalculateOption.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReferenceChannel.Initialise: boolean;
const OPNAME = 'TReferenceChannel.Initialise';
Begin
  Result := False;
  try
    FChannelNumber.FData := 0;
    FChannelNumber.FInitalised := False;
    FChannelNumber.FLength := 6;
    FChannelNumber.FDecimal := 0;
    FChannelNumber.FDefaultPadding := True;

    FNodeCount.FData := 0;
    FNodeCount.FInitalised := False;
    FNodeCount.FLength := 6;
    FNodeCount.FDecimal := 0;
    FNodeCount.FDefaultPadding := True;

    FClassCount.FData := 0;
    FClassCount.FInitalised := False;
    FClassCount.FLength := 6;
    FClassCount.FDecimal := 0;
    FClassCount.FDefaultPadding := True;

    FCalculateOption.FData := 0;
    FCalculateOption.FInitalised := False;
    FCalculateOption.FLength := 6;
    FCalculateOption.FDecimal := 3;
    FCalculateOption.FDefaultPadding := False;
    FCalculateOption.ShowDecimalPoint := True;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReferenceChannel.Reset;
const OPNAME = 'TReferenceChannel.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TReferenceRelease }

procedure TReferenceRelease.CreateMemberObjects;
const OPNAME = 'TReferenceRelease.CreateMemberObjects';
var
  LCount: integer;
  LField: TDouble;
begin
  inherited;
  try
    FAnualInflow    := TDouble.Create;
    for LCount := MinReleaseStructure to MaxReleaseStructure do
    begin
      LField := TDouble.Create;
      FMonthlyRelease[LCount] := LField;
    end;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReferenceRelease.DestroyMemberObjects;
const OPNAME = 'TReferenceRelease.DestroyMemberObjects';
var
  LCount : integer;
begin
  inherited;
  try
    FAnualInflow.Free;
    for LCount := MinReleaseStructure to MaxReleaseStructure do
      FMonthlyRelease[LCount].Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReferenceRelease.Initialise: boolean;
const OPNAME = 'TReferenceRelease.Initialise';
var
  LCount: integer;
  LField: TDouble;
begin
  Result := False;
  try
    FAnualInflow.FData := 0;
    FAnualInflow.FInitalised := False;
    FAnualInflow.FLength := 10;
    FAnualInflow.FDecimal := 3;
    FAnualInflow.FDefaultPadding := False;
    FAnualInflow.ShowDecimalPoint := True;

    for LCount := MinReleaseStructure to MaxReleaseStructure do
    begin
      LField := FMonthlyRelease[LCount];
      LField.FData := 0;
      LField.FInitalised := False;
      LField.FLength := 10;
      LField.FDecimal := 3;
      LField.FDefaultPadding := False;
      LField.ShowDecimalPoint := True;
    end;
    Result := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReferenceRelease.Reset;
const OPNAME = 'TReferenceRelease.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TReference }


procedure TReference.CreateMemberObjects;
const OPNAME = 'TReference.CreateMemberObjects';
begin
  inherited;
  try
    FReferenceChannel     := TReferenceChannel.Create;
    FReferenceNodeNumbers := TStringList.Create;
    FReferenceReleaseList := TObjectList.Create(True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReference.DestroyMemberObjects;
const OPNAME = 'TReference.DestroyMemberObjects';
begin
  inherited;
  try
    FReferenceChannel.Free;
    FReferenceNodeNumbers.Free;
    FReferenceReleaseList.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReference.Initialise: boolean;
const OPNAME = 'TReference.Initialise';
begin
  Result := False;
  try
    FReferenceChannel.Initialise;
    FReferenceNodeNumbers.Clear;
    FReferenceReleaseList.Clear;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReference.AddReferenceRelease: TReferenceRelease;
const OPNAME = 'TReference.AddReferenceRelease';
var
  LReferenceRelease: TReferenceRelease;
begin
  Result := nil;
  try
    LReferenceRelease := TReferenceRelease.Create;
    FReferenceReleaseList.Add(LReferenceRelease);
    Result := LReferenceRelease;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReference.ReferenceReleaseByIndex( AIndex: integer): TReferenceRelease;
const OPNAME = 'TReference.ReferenceReleaseByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FReferenceReleaseList.Count) then
      Result := TReferenceRelease(FReferenceReleaseList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReference.Reset;
const OPNAME = 'TReference.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
