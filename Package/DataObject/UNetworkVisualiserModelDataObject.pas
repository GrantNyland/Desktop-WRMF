//
//
//  UNIT      : Contains TNetworkVisualiserModelDataObject Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 23/05/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
unit UNetworkVisualiserModelDataObject;

interface

uses
  Classes, sysutils,contnrs,

  //  DWAF VCL
  UAbstractDataObject,
  UBasicObjects,
  UAbstractObject,
  UConstants;

type
  TNetworkElement = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FModel          :TString;
    FStudyAreaName  :TString;
    FSubArea        :TString;
    FScenario       :TString;
    FElementID      :TInteger;
    FIdentifier     :TInteger;
    FModelElementID :TInteger;
    FElementType    :TInteger;
    FChannelType    :TInteger;
    FStartElementID :TInteger;
    FEndElementID   :TInteger;
    FIDString       :TString;
    FCaption        :TString;
    FHeight         :TInteger;
    FWidth          :TInteger;
    FPenaltyCount   :TInteger;
    FPenaltyStruct  : TInteger;
    FAssociatedFile :TString;
    FPenaltyValue   : array[MinPenaltyZone..MaxPenaltyZone] of TDouble;
    FPenaltyDescr   : array[MinPenaltyZone..MaxPenaltyZone] of TString;
    FPenaltyNames   : array[MinPenaltyZone..MaxPenaltyZone] of TString;
    procedure Reset;override;
    function AssignFrom(ANetworkElement: TNetworkElement): Boolean;
    function Initialise: boolean;override;
  end;

  TNetworkVisualiserModelDataObject = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FNetworkNodes : TObjectList;
    FNetworkChannels : TObjectList;
    procedure Reset;override;
    function Initialise: boolean;override;
    function AddNetworkNode(ANetworkElement: TNetworkElement): boolean;
    function AddNetworkChannel(ANetworkElement: TNetworkElement): boolean;
    function NetworkNodeExists(ANetworkElement: TNetworkElement): boolean;
    function NetworkChannelExists(ANetworkElement: TNetworkElement): boolean;
    function NetworkNodeExistsInDB(ANetworkElement: TNetworkElement): boolean;
    function NetworkChannelExistsInDB(ANetworkElement: TNetworkElement): boolean;
    function AddNode(ANodeNumber: Integer;AAppModules: TAppModules):boolean;
    function FindNode(ANodeNumber: Integer):TNetworkElement;
  end;

implementation

uses UErrorHandlingOperations;

{ TNetworkElement }

function TNetworkElement.AssignFrom(ANetworkElement: TNetworkElement): Boolean;
const OPNAME = 'TNetworkElement.AssignFrom';
var
  LCount: integer;
begin
  Result := False;
  try
    if Assigned(ANetworkElement) then
    begin
      FModel.FData          := ANetworkElement.FModel.FData;
      FStudyAreaName.FData  := ANetworkElement.FStudyAreaName.FData;
      FSubArea.FData        := ANetworkElement.FSubArea.FData;
      FScenario.FData       := ANetworkElement.FScenario.FData;
      FElementID.FData      := ANetworkElement.FElementID.FData;
      FIdentifier.FData     := ANetworkElement.FIdentifier.FData;
      FModelElementID.FData := ANetworkElement.FModelElementID.FData;
      FElementType.FData    := ANetworkElement.FElementType.FData;
      FChannelType.FData    := ANetworkElement.FChannelType.FData;
      FStartElementID.FData := ANetworkElement.FStartElementID.FData;
      FEndElementID.FData   := ANetworkElement.FEndElementID.FData;
      FIDString.FData       := ANetworkElement.FIDString.FData;
      FCaption.FData        := ANetworkElement.FCaption.FData;
      FHeight.FData         := ANetworkElement.FHeight.FData;
      FWidth.FData          := ANetworkElement.FWidth.FData;
      FPenaltyCount.FData   := ANetworkElement.FPenaltyCount.FData;
      FPenaltyStruct.FData  := ANetworkElement.FPenaltyStruct.FData;
      FAssociatedFile.FData := ANetworkElement.FAssociatedFile.FData;
      for LCount := MinPenaltyZone to MaxPenaltyZone do
      begin
        FPenaltyValue[LCount].FData  := ANetworkElement.FPenaltyValue[LCount].FData;
        FPenaltyDescr[LCount].FData  := ANetworkElement.FPenaltyDescr[LCount].FData;
        FPenaltyNames[LCount].FData  := ANetworkElement.FPenaltyNames[LCount].FData;
      end;

      FModel.FInitalised          := ANetworkElement.FModel.FInitalised;
      FStudyAreaName.FInitalised  := ANetworkElement.FStudyAreaName.FInitalised;
      FSubArea.FInitalised        := ANetworkElement.FSubArea.FInitalised;
      FScenario.FInitalised       := ANetworkElement.FScenario.FInitalised;
      FElementID.FInitalised      := ANetworkElement.FElementID.FInitalised;
      FIdentifier.FInitalised     := ANetworkElement.FIdentifier.FInitalised;
      FModelElementID.FInitalised := ANetworkElement.FModelElementID.FInitalised;
      FElementType.FInitalised    := ANetworkElement.FElementType.FInitalised;
      FChannelType.FInitalised    := ANetworkElement.FChannelType.FInitalised;
      FStartElementID.FInitalised := ANetworkElement.FStartElementID.FInitalised;
      FEndElementID.FInitalised   := ANetworkElement.FEndElementID.FInitalised;
      FIDString.FInitalised       := ANetworkElement.FIDString.FInitalised;
      FCaption.FInitalised        := ANetworkElement.FCaption.FInitalised;
      FHeight.FInitalised         := ANetworkElement.FHeight.FInitalised;
      FWidth.FInitalised          := ANetworkElement.FWidth.FInitalised;
      FPenaltyCount.FInitalised   := ANetworkElement.FPenaltyCount.FInitalised;
      FPenaltyStruct.FInitalised  := ANetworkElement.FPenaltyStruct.FInitalised;
      FAssociatedFile.FInitalised := ANetworkElement.FAssociatedFile.FInitalised;

      for LCount := MinPenaltyZone to MaxPenaltyZone do
      begin
        FPenaltyValue[LCount].FInitalised  := ANetworkElement.FPenaltyValue[LCount].FInitalised;
        FPenaltyDescr[LCount].FInitalised  := ANetworkElement.FPenaltyDescr[LCount].FInitalised;
        FPenaltyNames[LCount].FInitalised  := ANetworkElement.FPenaltyNames[LCount].FInitalised;
      end;

      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkElement.CreateMemberObjects;
const OPNAME = 'TNetworkElement.CreateMemberObjects';
var
  LCount: integer;
begin
  try
    FModel          := TString.Create;
    FStudyAreaName  := TString.Create;
    FSubArea        := TString.Create;
    FScenario       := TString.Create;
    FElementID      := TInteger.Create;
    FIdentifier     := TInteger.Create;
    FModelElementID := TInteger.Create;
    FElementType    := TInteger.Create;
    FChannelType    := TInteger.Create;
    FStartElementID := TInteger.Create;
    FEndElementID   := TInteger.Create;
    FIDString       := TString.Create;
    FCaption        := TString.Create;
    FHeight         := TInteger.Create;
    FWidth          := TInteger.Create;
    FPenaltyCount   := TInteger.Create;
    FPenaltyStruct  := TInteger.Create;
    FAssociatedFile  := TString.Create;

    for LCount := MinPenaltyZone to MaxPenaltyZone do
    begin
      FPenaltyValue[LCount] := TDouble.Create;
      FPenaltyDescr[LCount] := TString.Create;
      FPenaltyNames[LCount] := TString.Create;
    end;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkElement.DestroyMemberObjects;
const OPNAME = 'TNetworkElement.DestroyMemberObjects';
var
  LCount: integer;
begin
  try
    FModel.Free;
    FStudyAreaName.Free;
    FSubArea.Free;
    FScenario.Free;
    FElementID.Free;
    FIdentifier.Free;
    FModelElementID.Free;
    FElementType.Free;
    FChannelType.Free;
    FStartElementID.Free;
    FEndElementID.Free;
    FIDString.Free;
    FCaption.Free;
    FHeight.Free;
    FWidth.Free;
    FPenaltyCount.Free;
    FPenaltyStruct.Free;
    FAssociatedFile.Free;

    for LCount := MinPenaltyZone to MaxPenaltyZone do
    begin
      FPenaltyValue[LCount].Free;
      FPenaltyDescr[LCount].Free;
      FPenaltyNames[LCount].Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkElement.Initialise: boolean;
const OPNAME = 'TNetworkElement.Initialise';
var
  LCount: integer;
Begin
  Result := False;
  try
    FModel.FData := '';
    FModel.FInitalised := False;
    FModel.FLength := 0;
    FModel.FDecimal := 0;
    FModel.FDefaultPadding := True;

    FStudyAreaName.FData := '';
    FStudyAreaName.FInitalised := False;
    FStudyAreaName.FLength := 0;
    FStudyAreaName.FDecimal := 0;
    FStudyAreaName.FDefaultPadding := True;

    FSubArea.FData := '';
    FSubArea.FInitalised := False;
    FSubArea.FLength := 0;
    FSubArea.FDecimal := 0;
    FSubArea.FDefaultPadding := True;

    FScenario.FData := '';
    FScenario.FInitalised := False;
    FScenario.FLength := 0;
    FScenario.FDecimal := 0;
    FScenario.FDefaultPadding := True;

    FElementID.FData := 0;
    FElementID.FInitalised := False;
    FElementID.FLength := 0;
    FElementID.FDecimal := 0;
    FElementID.FDefaultPadding := True;

    FIdentifier.FData := 0;
    FIdentifier.FInitalised := False;
    FIdentifier.FLength := 0;
    FIdentifier.FDecimal := 0;
    FIdentifier.FDefaultPadding := True;

    FModelElementID.FData := 0;
    FModelElementID.FInitalised := False;
    FModelElementID.FLength := 0;
    FModelElementID.FDecimal := 0;
    FModelElementID.FDefaultPadding := True;

    FElementType.FData := 0;
    FElementType.FInitalised := False;
    FElementType.FLength := 0;
    FElementType.FDecimal := 0;
    FElementType.FDefaultPadding := True;

    FChannelType.FData := 0;
    FChannelType.FInitalised := False;
    FChannelType.FLength := 0;
    FChannelType.FDecimal := 0;
    FChannelType.FDefaultPadding := True;

    FStartElementID.FData := 0;
    FStartElementID.FInitalised := False;
    FStartElementID.FLength := 0;
    FStartElementID.FDecimal := 0;
    FStartElementID.FDefaultPadding := True;

    FEndElementID.FData := 0;
    FEndElementID.FInitalised := False;
    FEndElementID.FLength := 0;
    FEndElementID.FDecimal := 0;
    FEndElementID.FDefaultPadding := True;
    FIDString.FData := '';
    FIDString.FInitalised := False;
    FIDString.FLength := 0;
    FIDString.FDecimal := 0;
    FIDString.FDefaultPadding := True;

    FCaption.FData := '';
    FCaption.FInitalised := False;
    FCaption.FLength := 0;
    FCaption.FDecimal := 0;
    FCaption.FDefaultPadding := True;

    FHeight.FData := 0;
    FHeight.FInitalised := False;
    FHeight.FLength := 0;
    FHeight.FDecimal := 0;
    FHeight.FDefaultPadding := True;

    FWidth.FData := 0;
    FWidth.FInitalised := False;
    FWidth.FLength := 0;
    FWidth.FDecimal := 0;
    FWidth.FDefaultPadding := True;

    FPenaltyCount.FData := 0;
    FPenaltyCount.FInitalised := False;
    FPenaltyCount.FLength := 0;
    FPenaltyCount.FDecimal := 0;
    FPenaltyCount.FDefaultPadding := True;

    FPenaltyStruct.FData := 0;
    FPenaltyStruct.FInitalised := False;
    FPenaltyStruct.FLength := 0;
    FPenaltyStruct.FDecimal := 0;
    FPenaltyStruct.FDefaultPadding := True;

    FAssociatedFile.FData := '';
    FAssociatedFile.FInitalised := False;
    FAssociatedFile.FLength := 0;
    FAssociatedFile.FDecimal := 0;
    FAssociatedFile.FDefaultPadding := True;

    for LCount := MinPenaltyZone to MaxPenaltyZone do
    begin
      FPenaltyValue[LCount].FData := 0.0;
      FPenaltyValue[LCount].FInitalised := False;
      FPenaltyValue[LCount].FLength := 0;
      FPenaltyValue[LCount].FDecimal := 0;
      FPenaltyValue[LCount].FDefaultPadding := True;

      FPenaltyDescr[LCount].FData := '';
      FPenaltyDescr[LCount].FInitalised := False;
      FPenaltyDescr[LCount].FLength := 0;
      FPenaltyDescr[LCount].FDecimal := 0;
      FPenaltyDescr[LCount].FDefaultPadding := True;

      FPenaltyNames[LCount].FData := '';
      FPenaltyNames[LCount].FInitalised := False;
      FPenaltyNames[LCount].FLength := 0;
      FPenaltyNames[LCount].FDecimal := 0;
      FPenaltyNames[LCount].FDefaultPadding := True;
    end;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TNetworkElement.Reset;
const OPNAME = 'TNetworkElement.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


{ TNetworkVisualiserModelDataObject }

procedure TNetworkVisualiserModelDataObject.CreateMemberObjects;
const OPNAME = 'TNetworkVisualiserModelDataObject.CreateMemberObjects';
begin
  try
    FNetworkNodes  := TObjectList.Create(True);
    FNetworkChannels  := TObjectList.Create(True);
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkVisualiserModelDataObject.DestroyMemberObjects;
const OPNAME = 'TNetworkVisualiserModelDataObject.DestroyMemberObjects';
begin
  try
    FNetworkNodes.Free;
    FNetworkChannels.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkVisualiserModelDataObject.Initialise: boolean;
const OPNAME = 'TNetworkVisualiserModelDataObject.Initialise';
Begin
  Result := False;
  try
    Reset;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkVisualiserModelDataObject.Reset;
const OPNAME = 'TNetworkVisualiserModelDataObject.Reset';
Begin
  try
    FNetworkNodes.Clear;
    FNetworkChannels.Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkVisualiserModelDataObject.AddNetworkNode(ANetworkElement: TNetworkElement): boolean;
const OPNAME = 'TNetworkVisualiserModelDataObject.AddNetworkNode';
var
  LNetworkElement :TNetworkElement;
Begin
  Result := False;
  try
    LNetworkElement := TNetworkElement.Create;
    LNetworkElement.AssignFrom(ANetworkElement);
    FNetworkNodes.Add(LNetworkElement);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkVisualiserModelDataObject.AddNetworkChannel(ANetworkElement: TNetworkElement): boolean;
const OPNAME = 'TNetworkVisualiserModelDataObject.AddNetworkChannel';
var
  LNetworkElement :TNetworkElement;
Begin
  Result := False;
  try
    LNetworkElement := TNetworkElement.Create;
    LNetworkElement.AssignFrom(ANetworkElement);
    FNetworkChannels.Add(LNetworkElement);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkVisualiserModelDataObject.NetworkNodeExists(ANetworkElement: TNetworkElement): boolean;
const OPNAME = 'TNetworkVisualiserModelDataObject.NetworkNodeExists';
var
  LCount: integer;
  LNetworkElement: TNetworkElement;
begin
  Result := False;
  try
     for LCount := 0 to FNetworkNodes.Count - 1 do
     begin
       LNetworkElement := TNetworkElement(FNetworkNodes[LCount]);
       if(LNetworkElement.FModel.FData          = ANetworkElement.FModel.FData) and
         (LNetworkElement.FStudyAreaName.FData  = ANetworkElement.FStudyAreaName.FData) and
         (LNetworkElement.FSubArea.FData        = ANetworkElement.FSubArea.FData) and
         (LNetworkElement.FModelElementID.FData = ANetworkElement.FModelElementID.FData) and
         (LNetworkElement.FElementType.FData    = ANetworkElement.FElementType.FData) then
       begin
         Result := True;
         Break;
       end;
     end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkVisualiserModelDataObject.NetworkChannelExists(ANetworkElement: TNetworkElement): boolean;
const OPNAME = 'TNetworkVisualiserModelDataObject.NetworkChannelExists';
var
  LCount: integer;
  LNetworkElement: TNetworkElement;
begin
  Result := False;
  try
     for LCount := 0 to FNetworkChannels.Count - 1 do
     begin
       LNetworkElement := TNetworkElement(FNetworkChannels[LCount]);
       if(LNetworkElement.FModel.FData          = ANetworkElement.FModel.FData) and
         (LNetworkElement.FStudyAreaName.FData  = ANetworkElement.FStudyAreaName.FData) and
         (LNetworkElement.FSubArea.FData        = ANetworkElement.FSubArea.FData) and
         (LNetworkElement.FModelElementID.FData = ANetworkElement.FModelElementID.FData) and
         (LNetworkElement.FElementType.FData    = ANetworkElement.FElementType.FData) then
       begin
         Result := True;
         Break;
       end;
     end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkVisualiserModelDataObject.NetworkNodeExistsInDB(ANetworkElement: TNetworkElement): boolean;
const OPNAME = 'TNetworkVisualiserModelDataObject.NetworkNodeExistsInDB';
var
  LCount: integer;
  LNetworkElement: TNetworkElement;
begin
  Result := False;
  try
     for LCount := 0 to FNetworkNodes.Count - 1 do
     begin
       LNetworkElement := TNetworkElement(FNetworkNodes[LCount]);
       if(LNetworkElement.FModel.FData          = ANetworkElement.FModel.FData) and
         (LNetworkElement.FStudyAreaName.FData  = ANetworkElement.FStudyAreaName.FData) and
         (LNetworkElement.FSubArea.FData        = ANetworkElement.FSubArea.FData) and
         (LNetworkElement.FElementID.FData      <> 0) and
         (LNetworkElement.FModelElementID.FData = ANetworkElement.FModelElementID.FData) and
         (LNetworkElement.FElementType.FData    = ANetworkElement.FElementType.FData) then
       begin
         Result := True;
         Break;
       end;
     end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkVisualiserModelDataObject.NetworkChannelExistsInDB(ANetworkElement: TNetworkElement): boolean;
const OPNAME = 'TNetworkVisualiserModelDataObject.NetworkChannelExistsInDB';
var
  LCount: integer;
  LNetworkElement: TNetworkElement;
begin
  Result := False;
  try
     for LCount := 0 to FNetworkChannels.Count - 1 do
     begin
       LNetworkElement := TNetworkElement(FNetworkChannels[LCount]);
       if(LNetworkElement.FModel.FData          = ANetworkElement.FModel.FData) and
         (LNetworkElement.FStudyAreaName.FData  = ANetworkElement.FStudyAreaName.FData) and
         (LNetworkElement.FSubArea.FData        = ANetworkElement.FSubArea.FData) and
         (LNetworkElement.FElementID.FData      <> 0) and
         (LNetworkElement.FModelElementID.FData = ANetworkElement.FModelElementID.FData) and
         (LNetworkElement.FElementType.FData    = ANetworkElement.FElementType.FData) then
       begin
         Result := True;
         Break;
       end;
     end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkVisualiserModelDataObject.FindNode(ANodeNumber: Integer): TNetworkElement;
const OPNAME = 'TNetworkVisualiserModelDataObject.FindNode';
var
  LCount: integer;
  LNetworkElement: TNetworkElement;
begin
  Result := nil;
  try
     for LCount := 0 to FNetworkNodes.Count - 1 do
     begin
       LNetworkElement := TNetworkElement(FNetworkNodes[LCount]);
       if (LNetworkElement.FModelElementID.FData = ANodeNumber) then
       begin
         Result := LNetworkElement;
         Break;
       end;
     end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkVisualiserModelDataObject.AddNode(ANodeNumber: Integer;AAppModules: TAppModules): boolean;
const OPNAME = 'TNetworkVisualiserModelDataObject.AddNode';
var
  LNetworkElement: TNetworkElement;
begin
  Result := False;
  try
    if Assigned(AAppModules) then
    begin
      LNetworkElement := TNetworkElement.Create;

      LNetworkElement.FModel.FData := AAppModules.StudyArea.ModelCode;
      LNetworkElement.FModel.FInitalised := True;

      LNetworkElement.FStudyAreaName.FData := AAppModules.StudyArea.StudyAreaCode;
      LNetworkElement.FStudyAreaName.FInitalised := True;

      LNetworkElement.FSubArea.FData := AAppModules.StudyArea.SubAreaCode;
      LNetworkElement.FSubArea.FInitalised := True;

      LNetworkElement.FScenario.FData := AAppModules.StudyArea.ScenarioCode;
      LNetworkElement.FScenario.FInitalised := True;

      //LNetworkElement.FElementID.FData := 0;
      LNetworkElement.FElementID.FInitalised := False;

      LNetworkElement.FModelElementID.FData := ANodeNumber;
      LNetworkElement.FModelElementID.FInitalised := True;

      LNetworkElement.FElementType.FData := 120;
      LNetworkElement.FElementType.FInitalised := True;

      LNetworkElement.FIDString.FData := IntToStr(ANodeNumber);
      LNetworkElement.FIDString.FInitalised := True;

      LNetworkElement.FCaption.FData := IntToStr(ANodeNumber);
      LNetworkElement.FCaption.FInitalised := True;

      LNetworkElement.FHeight.FData := 32;
      LNetworkElement.FHeight.FInitalised := True;

      LNetworkElement.FWidth.FData := 32;
      LNetworkElement.FWidth.FInitalised := True;

      Result := AddNetworkNode(LNetworkElement);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
