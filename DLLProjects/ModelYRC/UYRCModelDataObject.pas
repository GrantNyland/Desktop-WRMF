//
//
//  UNIT      : Contains  TYRCModelDataObject Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 28/01/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UYRCModelDataObject;

interface

uses
  Classes, SysUtils, Contnrs,

  UAbstractObject,
  UAbstractYRCModelDataObject,
  UAbstractFileNamesObject,
  UFileNames,
  UFilesLineTypeObject,
  UViewModelDataObject,
  UAbstractYRCData,
  UYRCGraphDataObject;

type


  TYRCModelDataObject = class(TAbstractYRCModelDataObject)
  protected
    FFileNamesObject     : TModelFileNames;
    FFilesLineTypes      : TFilesLineTypes;
    FYRCGraphDataObject    : TYRCGraphDataObject;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    function GetFileNamesObject: TAbstractModelFileNameList; override;
    function GetFilesLineTypes: TAbstractFilesLineTypes; override;
    function GetYRCGraphDataObject: TAbstractYRCGraphDataObject; override;

  public
    procedure Reset;
    function Initialise: boolean; override;
    function GetViewDataItems(AViewId: string; AItemsList:TViewModelDataItemsList; var AHandled:boolean): boolean;override;
    property CastYRCGraphDataObject    : TYRCGraphDataObject read FYRCGraphDataObject;
    property CastModelFileNames    : TModelFileNames read FFileNamesObject;
  end;

implementation

uses
  UDemandHydrologyDataSQLAgent,
  UViewModelDataValidator,
  UErrorHandlingOperations;

procedure TYRCModelDataObject.CreateMemberObjects;
const OPNAME = 'TYRCModelDataObject.CreateMemberObjects';
begin
  try
    FFileNamesObject        := TModelFileNames.Create(FAppModules);
    FFilesLineTypes         := TFilesLineTypes.Create;
    FYRCGraphDataObject     := TYRCGraphDataObject.Create(FAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCModelDataObject.DestroyMemberObjects;
const OPNAME = 'TYRCModelDataObject.DestroyMemberObjects';
begin
  try
    FreeAndNil(FFileNamesObject);
    FreeAndNil(FFilesLineTypes);
    FreeAndNil(FYRCGraphDataObject);
  except on E: Exception do HandleError(E, OPNAME) end;
end;
{
function TYRCModelDataObject.GetCastStudyConfigurationData: TStudyConfigurationData;
const OPNAME = 'TYRCModelDataObject.GetCastStudyConfigurationData';
begin
  Result := nil;
  try
    Result := FStudyConfigurationData;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
}

function TYRCModelDataObject.GetFilesLineTypes: TAbstractFilesLineTypes;
const OPNAME = 'TYRCModelDataObject.GetFilesLineTypes';
begin
  Result := nil;
  try
    Result := FFilesLineTypes;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCModelDataObject.GetFileNamesObject: TAbstractModelFileNameList;
const OPNAME = 'TYRCModelDataObject.GetFileNamesObject';
begin
  Result := nil;
  try
    Result := FFileNamesObject;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
{
function TYieldModelDataObject.GetFilesLineTypes: TAbstractFilesLineTypes;
const OPNAME = 'TYieldModelDataObject.GetFilesLineTypes';
begin
  Result := nil;
  try
    Result := FFilesLineTypes;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataObject.GetStudyConfigurationData: TAbstracTStudyConfigurationData;
const OPNAME = 'TYieldModelDataObject.GetStudyConfigurationData';
begin
  Result := nil;
  try
    Result := FStudyConfigurationData;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
}
function TYRCModelDataObject.GetViewDataItems(AViewId: string; AItemsList: TViewModelDataItemsList; var AHandled: boolean): boolean;
const OPNAME = 'TYRCModelDataObject.GetViewDataItems';
var
  LUpperViewId: string;
begin
  Result := False;
  try
    AHandled := False;
    if (Trim(AViewId) <> '') and Assigned(AItemsList) then
    begin
      LUpperViewId := UpperCase(Trim(AViewId));
{      if (Pos('NODENAMESWITHINFLOW',LUpperViewId) = 1) then
        AHandled := GetNodesWithInflowViewDataItems(AViewId,AItemsList,FNetworkElementData.ReservoirList)
      else
      if (Pos('NODENAMESWITHOUTINFLOW',LUpperViewId) = 1) then
        AHandled := GetNodesWithoutInflowViewDataItems(AViewId,AItemsList,FNetworkElementData.ReservoirList)
      else
      if(Pos('CHANNELNAMES',LUpperViewId) = 1) then
        AHandled := GetChannelViewDataItems(AViewId,AItemsList,FNetworkElementData.ChannelList)
      else
      if(Pos('DEMANDFILENUMBERS',LUpperViewId) = 1) then
        AHandled := GetDemandFilesViewDataItems(AViewId,AItemsList,FFileNamesObject)
      else
      if(Pos('HYDROLOGYFILENAMES',LUpperViewId) = 1) then
        AHandled := GetHydrologyFilesViewDataItems(AViewId,AItemsList,FFileNamesObject)
      else
      if(Pos('POWERPLANTS',LUpperViewId) = 1) then
        AHandled := GetPowerPlantsViewDataItems(AViewId,AItemsList,FNetworkFeaturesData.CastPowerPlantList)
      else
      if(Pos('IRRIGATIONAREAS',LUpperViewId) = 1) then
        AHandled := GetIrrigationAreasViewDataItems(AViewId,AItemsList,FNetworkFeaturesData.CastIrrigationAreaList);
}
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TYRCModelDataObject.GetYRCGraphDataObject: TAbstractYRCGraphDataObject;
const OPNAME = 'TYRCModelDataObject.GetYRCGraphDataObject';
begin
  Result := nil;
  try
    Result := FYRCGraphDataObject;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCModelDataObject.Initialise: boolean;
const OPNAME = 'TYRCModelDataObject.Initialise';
begin
  Result := False;
  try
    Result := FFileNamesObject.Initialise;
    Result := Result and FYRCGraphDataObject.Initialise;//was commented
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCModelDataObject.Reset;
const OPNAME = 'TYRCModelDataObject.Reset';
begin
  try
    FFileNamesObject.Initialise;
    FYRCGraphDataObject.Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
{
function TYRCModelDataObject.GetParamSetup: TAbstractParamSetup;
const OPNAME = 'TYRCModelDataObject.GetParamSetup';
begin
  Result := nil;
  try
    Result := FParamSetup;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataObject.GetCastParameterData: TParamSetup;
const OPNAME = 'TYieldModelDataObject.GetCastParameterData';
begin
  Result := nil;
  try
    Result := FParamSetup;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataObject.GetHydrologyFilesForCatchment(ACatchmentRef: integer;AFilesNamesContainer: TStringList): boolean;
const OPNAME = 'TYieldModelDataObject.GetHydrologyFilesForCatchment';
var
  LParamReference : TAbstractParamReference;
begin
  Result := False;
  try
    if not Assigned(AFilesNamesContainer) then Exit;
    AFilesNamesContainer.Clear;

    LParamReference := FParamSetup.ReferenceDataByCatchNumber[ACatchmentRef];
    Result := Assigned(LParamReference);
    Result := Result and FFileNamesObject.HydrologyFileNames.FindFilesFromPrefix(
                         LParamReference.FileReference,AFilesNamesContainer);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCModelDataObject.GetDatasetFileDataSet(ADemandFileName: string; ADataSet: TAbstractModelDataset): boolean;
const OPNAME = 'TYRCModelDataObject.GetDatasetFileDataSet';
var
  LSQLAgent: TDemandHydrologyDataSQLAgent;
begin
  Result := False;
  try
    if (Trim(ADemandFileName) <> '') and Assigned(ADataSet) then
    begin
      LSQLAgent := TDemandHydrologyDataSQLAgent.Create(FAppModules);
      try
        ADataSet.SetSQL(LSQLAgent.GetDemandDataSQL(ADemandFileName));
        ADataSet.DataSet.Open;
      finally
        LSQLAgent.free;
      end;
      Result := True;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCModelDataObject.GetHydrologyFileDataSet(AHydrologyFileName: string; ADataSet: TAbstractModelDataset): boolean;
const OPNAME = 'TYRCModelDataObject.GetHydrologyFileDataSet';
var
  LSQLAgent: TDemandHydrologyDataSQLAgent;
begin
  Result := False;
  try
    if (Trim(AHydrologyFileName) <> '') and Assigned(ADataSet) then
    begin
      LSQLAgent := TDemandHydrologyDataSQLAgent.Create(FAppModules);
      try
        ADataSet.SetSQL(LSQLAgent.GetHydrologyDataSQL(AHydrologyFileName));
        ADataSet.DataSet.Open;
      finally
        LSQLAgent.free;
      end;
      Result := True;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataObject.GetNetworkElementData: TAbstractNetworkElementData;
const OPNAME = 'TYieldModelDataObject.GetNetworkElementData';
begin
  Result := nil;
  try
    Result := FNetworkElementData;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataObject.GetNetworkFeaturesData: TAbstractNetworkFeaturesData;
const OPNAME = 'TYieldModelDataObject.GetNetworkFeaturesData';
begin
  Result := nil;
  try
    Result := FNetworkFeaturesData;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataObject.GetModelCalendar: AbstractModelCalendar;
const OPNAME = 'TYieldModelDataObject.GetModelCalendar';
begin
  Result := nil;
  try
    Result := FYieldModelCalendar;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataObject.GetYieldModelCapability: TAbstractYieldModelCapability;
const OPNAME = 'TYieldModelDataObject.GetYieldModelCapability';
begin
  Result := nil;
  try
    Result := FYieldModelCapability;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
}
end.
