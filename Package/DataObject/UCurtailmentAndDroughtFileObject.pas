//
//
//  UNIT      : Contains TCurtailmentAndDroughtFileObject Class
//  AUTHOR    : Presley Mudau(Cornastone)
//  DATE      : 17/07/2007
//  COPYRIGHT : Copyright © 2007 DWAF
//
//
unit UCurtailmentAndDroughtFileObject;

interface

uses
  Classes,
  sysutils,
  contnrs,
  UAbstractDataObject,
  UBasicObjects,
  UConstants;

type
  TMonthlyDouble          = array[MinMonths..MaxMonths] of TDouble;
  TStartMonthArray        = array[MinCurtailmentPeriod..MaxCurtailmentPeriod] of TInteger;
  TStorageVolumesArray    = array[MinStorageVolumes..MaxStorageVolumes] of TDouble;
  TAllocationFactorsArray = array[MinAllocationFactors..MaxAllocationFactors] of TDouble;

  TCurtailedChannelFileObject = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects;  override;
    procedure DestroyMemberObjects; override;

  public
    FIdentifier         : TInteger;
    FChannelNumber      : TInteger;
    FAllocationFactors  : TAllocationFactorsArray;

    procedure Reset;override;
    function Initialise: boolean;override;
  end;

  TCurtailmentFileObject = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects;  override;
    procedure DestroyMemberObjects; override;
  public
    FCurtailmentPeriodCount : TInteger;
    FStartMonth             : TStartMonthArray;
    procedure Reset;override;
    function Initialise: boolean;override;
  end;

  TDroughtRestrictionFileObject = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects;  override;
    procedure DestroyMemberObjects; override;
  public
    FIdentifier        : TInteger;
    FName              : TString;
    FChannelNumbers    : TString;
    FReservoirNumbers  : TString;
    FStorageVolumes    : TStorageVolumesArray;
    FAllocationFactors : TStorageVolumesArray;
    procedure Reset;override;
    function Initialise: boolean;override;
  end;

  TCurtailmentAndDroughtFileObject = class(TAbstractDataObject)
  protected
    FCurtailmentList        : TObjectList;
    FCurtailedChannelList   : TObjectList;
    FDroughtRestrictionList : TObjectList;
    FComment                : TStringlist;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetCurtailmentByIndex(AIndex: integer): TCurtailmentFileObject;
    function GetCurtailedChannelByIndex(AIndex: integer): TCurtailedChannelFileObject;
    function GetDroughtRestrictionByIndex(AIndex: integer): TDroughtRestrictionFileObject;
  public
    procedure Reset;override;
    function Initialise: boolean;override;
    function CurtailedChannelCount: integer;
    function CurtailmentCount: integer;
    function DroughtRestrictionCount: integer;
    function AddCurtailment:TCurtailmentFileObject;
    function AddCurtailedChannel:TCurtailedChannelFileObject;
    function AddDroughtRestriction:TDroughtRestrictionFileObject;
    function NewCurtailmentAndDroughtFileObject: TCurtailmentAndDroughtFileObject;


    property CurtailmentByIndex[AIndex: integer]        : TCurtailmentFileObject        read GetCurtailmentByIndex;
    property CurtailedChannelByIndex[AIndex: integer]   : TCurtailedChannelFileObject   read GetCurtailedChannelByIndex;
    property DroughtRestrictionByIndex[AIndex: integer] : TDroughtRestrictionFileObject read GetDroughtRestrictionByIndex;
    property Comment:TStringlist read FComment;
  end;

implementation

uses UErrorHandlingOperations;

{ TCurtailedChannelFileObject }

procedure TCurtailedChannelFileObject.CreateMemberObjects;
const OPNAME = 'TCurtailedChannelFileObject.CreateMemberObjects';
var
  LIndex : integer;
begin
  try
    inherited;
    FIdentifier    := TInteger.Create;
    FChannelNumber := TInteger.Create;

    for LIndex := Low(FAllocationFactors) to High(FAllocationFactors) do
    begin
      FAllocationFactors[LIndex] := TDouble.Create;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCurtailedChannelFileObject.DestroyMemberObjects;
const OPNAME = 'TCurtailedChannelFileObject.DestroyMemberObjects';
var
  LIndex : integer;
begin
  inherited;
  try
    FIdentifier.Free;
    FChannelNumber.Free;
    for LIndex := Low(FAllocationFactors) to High(FAllocationFactors) do
    begin
      FAllocationFactors[LIndex].Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TCurtailedChannelFileObject.Initialise: boolean;
const OPNAME = 'TCurtailedChannelFileObject.Initialise';
var
  LIndex : integer;
begin
  Result := False;
  try
    FIdentifier.FData := 0;
    FIdentifier.FInitalised := False;
    FIdentifier.FLength := 10;
    FIdentifier.FDecimal := 2;
    FIdentifier.FDefaultPadding := True;

    FChannelNumber.FData := 0;
    FChannelNumber.FInitalised := False;
    FChannelNumber.FLength := 10;
    FChannelNumber.FDecimal := 2;
    FChannelNumber.FDefaultPadding := True;

    for LIndex := Low(FAllocationFactors) to High(FAllocationFactors) do
    begin
      FAllocationFactors[LIndex].FData := 0.0;
      FAllocationFactors[LIndex].FInitalised := False;
      FAllocationFactors[LIndex].FLength := 10;
      FAllocationFactors[LIndex].FDecimal := 2;
      FAllocationFactors[LIndex].FDefaultPadding := True;
      FAllocationFactors[LIndex].ShowDecimalPoint := True;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCurtailedChannelFileObject.Reset;
const OPNAME = 'TCurtailedChannelFileObject.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TDroughtRestrictionFileObject }

procedure TDroughtRestrictionFileObject.CreateMemberObjects;
const OPNAME = 'TDroughtRestrictionFileObject.CreateMemberObjects';
var
  LIndex : integer;
begin
  try
    inherited;
    FIdentifier       := TInteger.Create;
    FName             := TString.Create;
    FChannelNumbers   := TString.Create;
    FReservoirNumbers := TString.Create;

    for LIndex := Low(FStorageVolumes) to High(FStorageVolumes) do
    begin
      FStorageVolumes[LIndex] := TDouble.Create;
    end;

    for LIndex := Low(FAllocationFactors) to High(FAllocationFactors) do
    begin
      FAllocationFactors[LIndex] := TDouble.Create;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDroughtRestrictionFileObject.DestroyMemberObjects;
const OPNAME = 'TDroughtRestrictionFileObject.DestroyMemberObjects';
var
  LIndex : integer;
begin
  inherited;
  try
    FIdentifier.Free;
    FName.Free;
    FChannelNumbers.Free;
    FReservoirNumbers.Free;

    for LIndex := Low(FStorageVolumes) to High(FStorageVolumes) do
    begin
      FStorageVolumes[LIndex].Free;
    end;
    for LIndex := Low(FAllocationFactors) to High(FAllocationFactors) do
    begin
      FAllocationFactors[LIndex].Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDroughtRestrictionFileObject.Initialise: boolean;
const OPNAME = 'TDroughtRestrictionFileObject.Initialise';
var
  LIndex : integer;
begin
  Result := False;
  try
    FIdentifier.FData := 0;
    FIdentifier.FInitalised := False;
    FIdentifier.FLength := 10;
    FIdentifier.FDecimal := 2;
    FIdentifier.FDefaultPadding := True;

    FName.FData := '';
    FName.FInitalised := False;
    FName.FLength := 255;
    FName.FDecimal := 0;
    FName.FDefaultPadding := True;

    FChannelNumbers.FData := '';
    FChannelNumbers.FInitalised := False;
    FChannelNumbers.FLength := 255;
    FChannelNumbers.FDecimal := 0;
    FChannelNumbers.FDefaultPadding := True;

    FReservoirNumbers.FData := '';
    FReservoirNumbers.FInitalised := False;
    FReservoirNumbers.FLength := 255;
    FReservoirNumbers.FDecimal := 0;
    FReservoirNumbers.FDefaultPadding := True;

    for LIndex := Low(FStorageVolumes) to High(FStorageVolumes) do
    begin
      FStorageVolumes[LIndex].FData := 0.0;
      FStorageVolumes[LIndex].FInitalised := False;
      FStorageVolumes[LIndex].FLength := 10;
      FStorageVolumes[LIndex].FDecimal := 2;
      FStorageVolumes[LIndex].FDefaultPadding := True;
      FStorageVolumes[LIndex].ShowDecimalPoint := True;
    end;

    for LIndex := Low(FAllocationFactors) to High(FAllocationFactors) do
    begin
      FAllocationFactors[LIndex].FData := 0.0;
      FAllocationFactors[LIndex].FInitalised := False;
      FAllocationFactors[LIndex].FLength := 10;
      FAllocationFactors[LIndex].FDecimal := 2;
      FAllocationFactors[LIndex].FDefaultPadding := True;
      FAllocationFactors[LIndex].ShowDecimalPoint := True;
    end;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDroughtRestrictionFileObject.Reset;
const OPNAME = 'TDroughtRestrictionFileObject.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TCurtailmentAndDroughtFileObject }

function TCurtailmentAndDroughtFileObject.AddCurtailment: TCurtailmentFileObject;
const OPNAME = 'TCurtailmentAndDroughtFileObject.AddCurtailment';
begin
  Result := nil;
  try
    Result := TCurtailmentFileObject.Create;
    FCurtailmentList.Add(Result);
    Result.Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TCurtailmentAndDroughtFileObject.AddCurtailedChannel: TCurtailedChannelFileObject;
const OPNAME = 'TCurtailmentAndDroughtFileObject.AddCurtailedChannel';
begin
  Result := nil;
  try
    Result := TCurtailedChannelFileObject.Create;
    FCurtailedChannelList.Add(Result);
    Result.Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TCurtailmentAndDroughtFileObject.AddDroughtRestriction: TDroughtRestrictionFileObject;
const OPNAME = 'TCurtailmentAndDroughtFileObject.AddDroughtRestriction';
begin
  Result := nil;
  try
    Result := TDroughtRestrictionFileObject.Create;
    FDroughtRestrictionList.Add(Result);
    Result.Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TCurtailmentAndDroughtFileObject.NewCurtailmentAndDroughtFileObject: TCurtailmentAndDroughtFileObject;
const OPNAME = 'TCurtailmentAndDroughtFileObject.NewCurtailmentAndDroughtFileObject';
begin
  Result := nil;
  try
    Result := TCurtailmentAndDroughtFileObject.Create;
    //Result.Initialise;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


procedure TCurtailmentAndDroughtFileObject.CreateMemberObjects;
const OPNAME = 'TCurtailmentAndDroughtFileObject.CreateMemberObjects';
begin
  try
    inherited;

    FCurtailedChannelList   := TObjectList.Create(True);
    FDroughtRestrictionList := TObjectList.Create(True);
    FCurtailmentList        := TObjectList.Create(True);
    FComment                := TStringlist.Create;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TCurtailmentAndDroughtFileObject.CurtailedChannelCount: integer;
const OPNAME = 'TCurtailmentAndDroughtFileObject.CurtailedChannelCount';
begin
  Result := 0;
  try
    Result := FCurtailedChannelList.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TCurtailmentAndDroughtFileObject.CurtailmentCount: integer;
const OPNAME = 'TCurtailmentAndDroughtFileObject.CurtailmentCount';
begin
  Result := 0;
  try
    Result := FCurtailmentList.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TCurtailmentAndDroughtFileObject.DestroyMemberObjects;
const OPNAME = 'TCurtailmentAndDroughtFileObject.DestroyMemberObjects';
begin
  try
    inherited;
    FCurtailedChannelList.Clear;
    FDroughtRestrictionList.Free;
    FCurtailmentList.Free;
    FComment.Clear;
    FComment.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TCurtailmentAndDroughtFileObject.DroughtRestrictionCount: integer;
const OPNAME = 'TCurtailmentAndDroughtFileObject.DroughtRestrictionCount';
begin
  Result := 0;
  try
    Result := FDroughtRestrictionList.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TCurtailmentAndDroughtFileObject.GetCurtailedChannelByIndex(AIndex: integer): TCurtailedChannelFileObject;
const OPNAME = 'TCurtailmentAndDroughtFileObject.GetCurtailedChannelByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FCurtailedChannelList.Count) then
      Result := TCurtailedChannelFileObject(FCurtailedChannelList[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TCurtailmentAndDroughtFileObject.GetDroughtRestrictionByIndex(AIndex: integer): TDroughtRestrictionFileObject;
const OPNAME = 'TCurtailmentAndDroughtFileObject.GetDroughtRestrictionByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FDroughtRestrictionList.Count) then
      Result := TDroughtRestrictionFileObject(FDroughtRestrictionList[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TCurtailmentAndDroughtFileObject.GetCurtailmentByIndex(AIndex: integer): TCurtailmentFileObject;
const OPNAME = 'TCurtailmentAndDroughtFileObject.GetCurtailmentByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FCurtailmentList.Count) then
      Result := TCurtailmentFileObject(FCurtailmentList[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCurtailmentAndDroughtFileObject.Reset;
const OPNAME = 'TCurtailmentAndDroughtFileObject.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TCurtailmentAndDroughtFileObject.Initialise: boolean;
const OPNAME = 'TCurtailmentAndDroughtFileObject.Initialise';
begin
  Result := False;
  try
    FCurtailedChannelList.Clear;
    FDroughtRestrictionList.Clear;
    FCurtailmentList.Clear;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TCurtailmentFileObject }

procedure TCurtailmentFileObject.CreateMemberObjects;
const OPNAME = 'TCurtailmentFileObject.CreateMemberObjects';
var
  LIndex : integer;
begin
  inherited;
  try
    FCurtailmentPeriodCount := TInteger.Create;
    for LIndex := Low(FStartMonth) to High(FStartMonth) do
    begin
      FStartMonth[LIndex] := TInteger.Create;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCurtailmentFileObject.DestroyMemberObjects;
const OPNAME = 'TCurtailmentFileObject.DestroyMemberObjects';
var
  LIndex : integer;
begin
  inherited;
  try
    FCurtailmentPeriodCount.Free;
    for LIndex := Low(FStartMonth) to High(FStartMonth) do
    begin
      FStartMonth[LIndex].Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TCurtailmentFileObject.Initialise: boolean;
const OPNAME = 'TCurtailmentFileObject.Initialise';
var
  LIndex : integer;
begin
  Result := False;
  try
    FCurtailmentPeriodCount.FData := 0;
    FCurtailmentPeriodCount.FInitalised := False;
    FCurtailmentPeriodCount.FLength := 4;
    FCurtailmentPeriodCount.FDecimal := 0;

    for LIndex := Low(FStartMonth) to High(FStartMonth) do
    begin
      FStartMonth[LIndex].FData := 1;
      FStartMonth[LIndex].FInitalised := False;
      FStartMonth[LIndex].FLength := 10;
      FStartMonth[LIndex].FDecimal := 2;
      FStartMonth[LIndex].FDefaultPadding := True;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCurtailmentFileObject.Reset;
const OPNAME = 'TCurtailmentFileObject.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
