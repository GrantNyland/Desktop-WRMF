//
//
//  UNIT      : Contains TGrowthFactorFileObject Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 03/05/2006
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UGrowthFactorFileObject;

interface
uses
  Classes, sysutils,contnrs,

  //  DWAF VCL
  UAbstractDataObject,
  UBasicObjects,
  UConstants;
type
  TGrowthFactorDemandCentresFileObject = class(TAbstractDataObject)
  protected
    FDemandChannelNumber : TInteger;
    FGrowthFactorsForEachYear : TString;
    FComment : TString;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure Reset;override;
    function Initialise: boolean;override;
    property DemandChannelNumber : TInteger read FDemandChannelNumber;
    property Comment : TString read FComment;
    property GrowthFactorsForEachYear : TString read FGrowthFactorsForEachYear;
  end;

  TGrowthFactorMinMaxChannelFileObject = class(TAbstractDataObject)
  protected
    FChannelNumber : TInteger;
    FArcNumber : TInteger;
    FComment : TString;
    FGrowthFactorsForEachYear : TString;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure Reset;override;
    function Initialise: boolean;override;
    property ChannelNumber : TInteger read FChannelNumber;
    property ArcNumber : TInteger read FArcNumber;
    property Comment : TString read FComment;
    property GrowthFactorsForEachYear : TString read FGrowthFactorsForEachYear;
  end;

  TGrowthFactorHydroDataFileObject = class(TAbstractDataObject)
  protected
    FGaugeNumber : TInteger;
    FComment : TString;
    FGrowthFactorsForAFF : TString;
    FGrowthFactorsForIRR : TString;
    FGrowthFactorsForURB : TString;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure Reset;override;
    function Initialise: boolean;override;
    property GaugeNumber : TInteger read FGaugeNumber;
    property Comment : TString read FComment;
    property GrowthFactorsForAFF : TString read FGrowthFactorsForAFF;
    property GrowthFactorsForIRR : TString read FGrowthFactorsForIRR;
    property GrowthFactorsForURB : TString read FGrowthFactorsForURB;
  end;

  TGrowthFactorFileObject  = class(TAbstractDataObject)
  protected

    FNumberOfYears : TInteger;
    FNumberOfSupply : TInteger;
    FGFDemandCentresObjectContainer : TObjectList;
    FGFMinMaxChannelObjectContainer : TObjectList;
    FGFHydroDataObjectContainer : TObjectList;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    function GetDemandGrowthFactorByIndex(AIndex: integer): TGrowthFactorDemandCentresFileObject;
    function GetMinMaxChannelGrowthFactorByIndex(AIndex: integer): TGrowthFactorMinMaxChannelFileObject;
    function GetHydroDataGrowthFactorByIndex(AIndex: integer): TGrowthFactorHydroDataFileObject;

  public
    FExtraLines: TStringlist;
    procedure Reset;override;
    function Initialise: boolean;override;
    function AddDemandCentresGrowthFactorFileObject:TGrowthFactorDemandCentresFileObject;
    function AddMinMaxChannelGrowthFactorFileObject:TGrowthFactorMinMaxChannelFileObject;
    function AddHydroDataGrowthFactorFileObject:TGrowthFactorHydroDataFileObject;
    function DemandCentresGrowthFactorsCount : integer;
    function MinMaxChannelGrowthFactorsCount : integer;
    function HydrologyGrowthFactorsCount     : integer;

    property DemandCentresGrowthFactorObjectByIndex[AIndex : integer] : TGrowthFactorDemandCentresFileObject
             read GetDemandGrowthFactorByIndex;
    property  MinMaxChannelGrowthFactorByIndex[AMinMaxChannelNumber: integer] : TGrowthFactorMinMaxChannelFileObject
              read GetMinMaxChannelGrowthFactorByIndex;
    property  HydroDataGrowthFactorByIndex[AGaugeNumber: integer] : TGrowthFactorHydroDataFileObject
              read GetHydroDataGrowthFactorByIndex;
    property NumberOfYears : TInteger read FNumberOfYears;
    property NumberOfSupply : TInteger read FNumberOfSupply;
  end;

implementation
uses
  UErrorHandlingOperations;


{ TGrowthFactorDemandCentresFileObject }

procedure TGrowthFactorDemandCentresFileObject.CreateMemberObjects;
const OPNAME = 'TGrowthFactorDemandCentresFileObject.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FDemandChannelNumber := TInteger.Create;
    FGrowthFactorsForEachYear := TString.Create;
    FComment := TString.Create;
    Initialise;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TGrowthFactorDemandCentresFileObject.DestroyMemberObjects;
const OPNAME = 'TGrowthFactorDemandCentresFileObject.DestroyMemberObjects';
begin
  try
    FreeAndNil(FDemandChannelNumber);
    FreeAndNil(FComment);
    FreeAndNil(FGrowthFactorsForEachYear);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactorDemandCentresFileObject.Initialise: boolean;
const OPNAME = 'TGrowthFactorDemandCentresFileObject.Initialise';
begin
  Result := False;
  try
    FDemandChannelNumber.FData := 0;
    FDemandChannelNumber.FInitalised := False;
    FDemandChannelNumber.FLength := 5;
    FDemandChannelNumber.FDefaultPadding := True;

    FGrowthFactorsForEachYear.FData := '';
    FGrowthFactorsForEachYear.FInitalised := False;
    FGrowthFactorsForEachYear.FLength := 0;
    FGrowthFactorsForEachYear.FDefaultPadding := True;

    FComment.FData := '';
    FComment.FInitalised := False;
    FComment.FLength := 0;
    FComment.FDefaultPadding := True;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TGrowthFactorDemandCentresFileObject.Reset;
const OPNAME = 'TGrowthFactorDemandCentresFileObject.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{ TGrowthFactorFileObject }

function TGrowthFactorFileObject.AddDemandCentresGrowthFactorFileObject:TGrowthFactorDemandCentresFileObject;
const OPNAME = 'TGrowthFactorFileObject.AddDemandCentresGrowthFactorFileObject';
begin
  Result := nil;
  try
    Result := TGrowthFactorDemandCentresFileObject.Create;
    FGFDemandCentresObjectContainer.Add(Result);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactorFileObject.AddMinMaxChannelGrowthFactorFileObject: TGrowthFactorMinMaxChannelFileObject;
const OPNAME = 'TGrowthFactorFileObject.AddMinMaxChannelGrowthFactorFileObject';
begin
  Result := nil;
  try
    Result := TGrowthFactorMinMaxChannelFileObject.Create;
    FGFMinMaxChannelObjectContainer.Add(Result);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactorFileObject.AddHydroDataGrowthFactorFileObject: TGrowthFactorHydroDataFileObject;
const OPNAME = 'TGrowthFactorFileObject.AddHydroDataGrowthFactorFileObject';
begin
  Result := nil;
  try
    Result := TGrowthFactorHydroDataFileObject.Create;
    FGFHydroDataObjectContainer.Add(Result);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TGrowthFactorFileObject.CreateMemberObjects;
const OPNAME = 'TGrowthFactorFileObject.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FNumberOfYears := TInteger.Create;
    FNumberOfSupply := TInteger.Create;
    FGFDemandCentresObjectContainer := TObjectList.Create;
    FGFMinMaxChannelObjectContainer := TObjectList.Create;
    FGFHydroDataObjectContainer := TObjectList.Create;
    FExtraLines:= TStringlist.Create;
    Initialise;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TGrowthFactorFileObject.DestroyMemberObjects;
const OPNAME = 'TGrowthFactorFileObject.DestroyMemberObjects';
begin
  try
    FreeAndNil(FNumberOfYears);
    FreeAndNil(FNumberOfSupply);
    FreeAndNil(FGFDemandCentresObjectContainer);
    FreeAndNil(FGFMinMaxChannelObjectContainer);
    FreeAndNil(FGFHydroDataObjectContainer);
    FreeAndNil(FExtraLines);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactorFileObject.Initialise: boolean;
const OPNAME = 'TGrowthFactorFileObject.Initialise';
begin
  Result := False;
  try
    FNumberOfYears.FData := 0;
    FNumberOfYears.FInitalised := False;
    FNumberOfYears.FLength := 5;
    FNumberOfYears.FDefaultPadding := True;

    FNumberOfSupply.FData := 0;
    FNumberOfSupply.FInitalised := False;
    FNumberOfSupply.FLength := 5;
    FNumberOfSupply.FDefaultPadding := True;

    FGFDemandCentresObjectContainer.Clear;
    FGFMinMaxChannelObjectContainer.Clear;
    FGFHydroDataObjectContainer.Clear;
    FExtraLines.Clear;

    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactorFileObject.DemandCentresGrowthFactorsCount: integer;
const OPNAME = 'TGrowthFactorFileObject.DemandCentresGrowthFactorsCount';
begin
  Result := 0;
  try
    Result := FGFDemandCentresObjectContainer.Count;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactorFileObject.HydrologyGrowthFactorsCount: integer;
const OPNAME = 'TGrowthFactorFileObject.HydrologyGrowthFactorsCount';
begin
  Result := 0;
  try
    Result := FGFHydroDataObjectContainer.Count;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactorFileObject.MinMaxChannelGrowthFactorsCount: integer;
const OPNAME = 'TGrowthFactorFileObject.MinMaxChannelGrowthFactorsCount';
begin
  Result := 0;
  try
    Result := FGFMinMaxChannelObjectContainer.Count;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TGrowthFactorFileObject.Reset;
const OPNAME = 'TGrowthFactorFileObject.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactorFileObject.GetMinMaxChannelGrowthFactorByIndex(AIndex: integer): TGrowthFactorMinMaxChannelFileObject;
const OPNAME = 'TGrowthFactorFileObject.GetMinMaxChannelGrowthFactorByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FGFMinMaxChannelObjectContainer.Count) then
      Result :=  TGrowthFactorMinMaxChannelFileObject(FGFMinMaxChannelObjectContainer[AIndex]);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactorFileObject.GetHydroDataGrowthFactorByIndex(AIndex: integer): TGrowthFactorHydroDataFileObject;
const OPNAME = 'TGrowthFactorFileObject.GetHydroDataGrowthFactorByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FGFHydroDataObjectContainer.Count) then
      Result :=  TGrowthFactorHydroDataFileObject(FGFHydroDataObjectContainer[AIndex]);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactorFileObject.GetDemandGrowthFactorByIndex(AIndex: integer): TGrowthFactorDemandCentresFileObject;
const OPNAME = 'TGrowthFactorFileObject.GetDemandGrowthFactorByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FGFDemandCentresObjectContainer.Count) then
      Result :=  TGrowthFactorDemandCentresFileObject(FGFDemandCentresObjectContainer[AIndex]);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{ TGrowthFactorHydroDataFileObject }

procedure TGrowthFactorHydroDataFileObject.CreateMemberObjects;
const OPNAME = 'TGrowthFactorHydroDataFileObject.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FGaugeNumber := TInteger.Create;
    FComment     := TString.Create;
    FGrowthFactorsForAFF := TString.Create;
    FGrowthFactorsForIRR := TString.Create;
    FGrowthFactorsForURB := TString.Create;
    Initialise;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TGrowthFactorHydroDataFileObject.DestroyMemberObjects;
const OPNAME = 'TGrowthFactorHydroDataFileObject.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FreeAndNil(FGaugeNumber);
    FreeAndNil(FComment);
    FreeAndNil(FGrowthFactorsForAFF);
    FreeAndNil(FGrowthFactorsForIRR);
    FreeAndNil(FGrowthFactorsForURB);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactorHydroDataFileObject.Initialise: boolean;
const OPNAME = 'TGrowthFactorHydroDataFileObject.Initialise';
begin
  Result := False;
  try
    FGaugeNumber.FData := 0;
    FGaugeNumber.FInitalised := False;
    FGaugeNumber.FLength := 5;
    FGaugeNumber.FDefaultPadding := True;

    FComment.FData := '';
    FComment.FInitalised := False;
    FComment.FLength := 0;
    FComment.FDefaultPadding := True;

    FGrowthFactorsForAFF.FData := '';
    FGrowthFactorsForAFF.FInitalised := False;
    FGrowthFactorsForAFF.FLength := 0;
    FGrowthFactorsForAFF.FDefaultPadding := True;

    FGrowthFactorsForIRR.FData := '';
    FGrowthFactorsForIRR.FInitalised := False;
    FGrowthFactorsForIRR.FLength := 0;
    FGrowthFactorsForIRR.FDefaultPadding := True;

    FGrowthFactorsForURB.FData := '';
    FGrowthFactorsForURB.FInitalised := False;
    FGrowthFactorsForURB.FLength := 0;
    FGrowthFactorsForURB.FDefaultPadding := True;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TGrowthFactorHydroDataFileObject.Reset;
const OPNAME = 'TGrowthFactorHydroDataFileObject.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{ TGrowthFactorMinMaxChannelFileObject }

procedure TGrowthFactorMinMaxChannelFileObject.CreateMemberObjects;
const OPNAME = 'TGrowthFactorMinMaxChannelFileObject.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FChannelNumber := TInteger.Create;
    FArcNumber     := TInteger.Create;
    FComment       := TString.Create;
    FGrowthFactorsForEachYear := TString.Create;
    Initialise;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TGrowthFactorMinMaxChannelFileObject.DestroyMemberObjects;
const OPNAME = 'TGrowthFactorMinMaxChannelFileObject.DestroyMemberObjects';
begin
  try
    FreeAndNil(FChannelNumber);
    FreeAndNil(FArcNumber);
    FreeAndNil(FComment);
    FreeAndNil(FGrowthFactorsForEachYear);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactorMinMaxChannelFileObject.Initialise: boolean;
const OPNAME = 'TGrowthFactorMinMaxChannelFileObject.Initialise';
begin
  Result := False;
  try
    FChannelNumber.FData := 0;
    FChannelNumber.FInitalised := False;
    FChannelNumber.FLength := 5;
    FChannelNumber.FDefaultPadding := True;

    FArcNumber.FData := 0;
    FArcNumber.FInitalised := False;
    FArcNumber.FLength := 5;
    FArcNumber.FDefaultPadding := True;

    FComment.FData := '';
    FComment.FInitalised := False;
    FComment.FLength := 0;
    FComment.FDefaultPadding := True;

    FGrowthFactorsForEachYear.FData := '';
    FGrowthFactorsForEachYear.FInitalised := False;
    FGrowthFactorsForEachYear.FLength := 0;
    FGrowthFactorsForEachYear.FDefaultPadding := True;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TGrowthFactorMinMaxChannelFileObject.Reset;
const OPNAME = 'TGrowthFactorMinMaxChannelFileObject.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
