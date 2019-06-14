unit UTariffCalculationFileDataObject;

interface
uses
  Classes, sysutils,contnrs,
  UBasicObjects,
  UAbstractDataObject,
  UFileNames,
  UAbstractObject,
  UConstants;

type
  //File TAR.DAT
  TChannelTariffData = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    //Line 2
    FChannelNumber           : TInteger;
    FChannelTariff           : TDouble;

    //Line 3
    FEscalationFactors       : TString;

    procedure Reset;override;
    function Initialise: boolean;override;
  end;

  TTariffCalculationFileDataObject  = class(TAbstractDataObject)
  protected
    //Line 1
    FDataYears : TInteger;

    // Line 2,3
    FChannelTariffList   : TObjectList;

    //Line 4... : Extra useless lines
    FHDextraLines : TStringList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetChannelTariffByIndex(AIndex: integer):TChannelTariffData;
  public
    procedure Reset;override;
    function Initialise: boolean;override;
    function AddChannelTariff:TChannelTariffData;

    property DataYears         : TInteger  read FDataYears;
    function ChannelTariffCount: integer;
    property ChannelTariffByIndex[AIndex: integer] : TChannelTariffData read GetChannelTariffByIndex;
    property HDextraLines : TStringList read FHDextraLines;

  end;

implementation
uses
  UErrorHandlingOperations;

{ TChannelTariffData }

procedure TChannelTariffData.CreateMemberObjects;
const OPNAME = 'TChannelTariffData.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    //Line 2
    FChannelNumber           := TInteger.Create;
    FChannelTariff           := TDouble.Create;

    //Line 3
    FEscalationFactors       := TString.Create;

    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelTariffData.DestroyMemberObjects;
const OPNAME = 'TChannelTariffData.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    // Line 2
    FChannelNumber.Free;
    FChannelTariff.Free;

    // Line 3
    FEscalationFactors.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelTariffData.Initialise: boolean;
const OPNAME = 'TChannelTariffData.Initialise';
begin
  Result := False;
  try
    FChannelNumber.FData := 0;
    FChannelNumber.FInitalised := False;
    FChannelNumber.FLength := 5;
    FChannelNumber.FDecimal := 0;
    FChannelNumber.FDefaultPadding := True;

    FChannelTariff.FData := 0;
    FChannelTariff.FInitalised := False;
    FChannelTariff.FLength := 6;
    FChannelTariff.FDecimal := 2;
    FChannelTariff.FDefaultPadding := True;


    FEscalationFactors.FData := '';
    FEscalationFactors.FInitalised := False;
    FEscalationFactors.FLength := 255;
    FEscalationFactors.FDecimal := 0;
    FEscalationFactors.FDefaultPadding := True;

    Result :=  True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelTariffData.Reset;
const OPNAME = 'TChannelTariffData.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{TTariffCalculationFileDataObject}

procedure TTariffCalculationFileDataObject.CreateMemberObjects;
CONST OPNAME = 'TTariffCalculationFileDataObject.CreateMemberObjects';
begin
  try
    FDataYears         := TInteger.Create;
    FChannelTariffList := TObjectList.Create(True);
    FHDextraLines      := TStringList.Create;
    Initialise;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TTariffCalculationFileDataObject.DestroyMemberObjects;
CONST OPNAME = 'TTariffCalculationFileDataObject.DestroyMemberObjects';
begin
  inherited;
  try
    FreeAndNil(FDataYears);
    FreeAndNil(FChannelTariffList);
    FreeAndNil(FHDextraLines);
  except on E: Exception do HandleError ( E, OPNAME ) end;

end;

function TTariffCalculationFileDataObject.Initialise: Boolean;
const OPNAME = 'TTariffCalculationFileDataObject.Initialise';
begin
  Result := False;
  try
    FDataYears.FData := 0;
    FDataYears.FInitalised := False;
    FDataYears.FLength := 6;
    FDataYears.FDecimal := 0;
    FDataYears.FDefaultPadding := True;

    FChannelTariffList.Clear;
    FHDextraLines.Clear;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;

end;

procedure TTariffCalculationFileDataObject.Reset;
const OPNAME = 'TTariffCalculationFileDataObject.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTariffCalculationFileDataObject.AddChannelTariff: TChannelTariffData;
const OPNAME = 'TTariffCalculationFileDataObject.AddChannelTariff';
begin
  Result := nil;
  try
    Result := TChannelTariffData.Create;
    FChannelTariffList.Add(Result);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTariffCalculationFileDataObject.ChannelTariffCount: integer;
const OPNAME = 'TTariffCalculationFileDataObject.Reset';
begin
  Result := 0;
  try
    Result := FChannelTariffList.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTariffCalculationFileDataObject.GetChannelTariffByIndex(AIndex: integer): TChannelTariffData;
const OPNAME = 'TTariffCalculationFileDataObject.GetChannelTariffByIndex';
begin
  Result := nil;
  try
   if(AIndex >= 0) and (AIndex < FChannelTariffList.Count) then
      Result := TChannelTariffData(FChannelTariffList.Items[AIndex])
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

