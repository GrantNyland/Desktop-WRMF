//
//
//  UNIT      : Contains  TYieldModelDataObject Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 28/01/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UYieldModelDataObject;

interface

uses
  Classes, SysUtils, Contnrs,


  UAbstractObject,
  VoaimsCom_TLB,
  UFileNames,
  UFilesLineTypeObject,
  UAbstractFileNamesObject,
  URunConfigurationData,
  UNetworkElementData,
  UNetworkFeaturesData,
  UDataFilePaths,
  UViewModelDataObject,
  UYRCGraphDataObject,
  UAbstractYRCData,
  UStringListOfStringLists,
  UYRCModelDataObject,
  UAbstractModelData,
  UYieldModelCalendar,
  UParameterData,
  UOutputData,
  UOutputComparisonData,
  UModelViews,
  UImplementedNetworkFeatures,
  UYieldModelCapability,
  UStudyMetaData;

type

  TYieldModelDataObject = class(TAbstractModelData, IYieldModelData)
  protected
    FNetworkElementData         : TNetworkElementData;
    FNetworkFeaturesData        : TNetworkFeaturesData;
    FYieldModelCalendar         : TYieldModelCalendar;
    FParamSetup                 : TParamSetup;
    FRunConfigurationData       : TRunConfigurationData;
    FYRCGraphDataObject         : TYRCGraphDataObject;
    FYieldModelViews            : TModelViews;
    FDataFilePaths              : TDataFilePaths;
    FYieldModelCapability       : TYieldModelCapability;
    FOutputData                 : TOutputData;
    FOutputComparisonData       : TOutputComparisonList;
    FImplementedNetworkFeatures : TImplementedNetworkFeatures;
    FStudyMetaDataList          : TStudyMetaDataList;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    function GetYRCGraphDataObject: TAbstractYRCGraphDataObject;
    function GetYieldModelCapability:IYieldModelCapability;
    function Get_ImplementedNetworkFeatures: IImplementedNetworkFeatures; safecall;

    function GetCastRunConfigurationData: TRunConfigurationData;
    function GetCastParameterData: TParamSetup;
    function GetCastFilesLineTypes: TFilesLineTypes;
    function GetCastFileNamesObject: TModelFileNames;
    function GetScenarioWhereClause: string;
  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function ResetState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;

    function GetRunConfigurationData: TRunConfigurationData;
    function GetViewDataItems(AViewId: string; AItemsList:TViewModelDataItemsList; var AHandled:boolean): boolean; override;
    function GetHydrologyFilesForCatchment(ACatchmentRef: integer;AFilesNamesContainer: TStringList): boolean;
    function GetHydrologyFileDataSet(AHydrologyFileName: string;ADataSet:TAbstractModelDataset): boolean;
    function GetDatasetFileDataSet(ADemandFileName: string;ADataSet:TAbstractModelDataset): boolean;
    function GetModelViewItems(AItems: TStringListOfStringLists): boolean;

    //Com getters
    function Get_RunConfigurationData: IRunConfigurationData; safecall;
    function Get_NetworkElementData: INetworkElementData; safecall;
    function Get_NetworkFeaturesData: INetworkFeaturesData; safecall;
    function Get_DataFilePaths: IDataFilePaths; safecall;
    function Get_YieldModelCapability: IYieldModelCapability; safecall;
    function Get_ModelCalendar: IModelCalendar;safecall;
    function Get_ParamSetup: IParamSetup; safecall;
    function Get_HydrologyFileData(const AFileName: WideString) : WideString; safecall;
    function Get_DemandFileData(const AFileName: WideString) : WideString; safecall;

    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    procedure HydrologyFilesForCatchment(ACatchmentRef: Integer;
                                         var AFilesNamesContainer: WideString); safecall;
    function Get_OutputData : IOutputData; safecall;

    property RunConfigurationData: IRunConfigurationData read Get_RunConfigurationData;
    property NetworkElementData: INetworkElementData read Get_NetworkElementData;
    property NetworkFeaturesData: INetworkFeaturesData read Get_NetworkFeaturesData;
    property DataFilePath : IDataFilePaths read Get_DataFilePaths;
    property YieldModelCapability: IYieldModelCapability read Get_YieldModelCapability;
    property ParamSetup: IParamSetup read Get_ParamSetup;
    property ModelCalendar: IModelCalendar read Get_ModelCalendar;
    property HydrologyFileData[const AFileName: WideString]: WideString read Get_HydrologyFileData;
    property DemandFileData[const AFileName: WideString]: WideString read Get_DemandFileData;

    property CastNetworkElementData     : TNetworkElementData         read FNetworkElementData;
    property CastRunConfigurationData   : TRunConfigurationData       read GetCastRunConfigurationData;
    property CastDataFilePaths          : TDataFilePaths              read FDataFilePaths;
    property CastCastParameterData      : TParamSetup                 read GetCastParameterData;
    property CastNetworkFeaturesData    : TNetworkFeaturesData        read FNetworkFeaturesData;
    property CastYRCGraphDataObject     : TYRCGraphDataObject         read FYRCGraphDataObject;
    property CastFilesLineTypes         : TFilesLineTypes             read GetCastFilesLineTypes;
    property CastFileNamesObject        : TModelFileNames             read GetCastFileNamesObject;
    property OutputData                 : TOutputData                 read FOutputData;
    property OutputComparisonData       : TOutputComparisonList       read FOutputComparisonData;
    property ImplementedNetworkFeatures : TImplementedNetworkFeatures read FImplementedNetworkFeatures;
    property StudyMetaDataList          : TStudyMetaDataList          read FStudyMetaDataList;

  end;

implementation

uses
  UDataSetType,
  UDemandHydrologyDataSQLAgent,
  UViewModelDataValidator,
  UErrorHandlingOperations;

procedure TYieldModelDataObject.CreateMemberObjects;
const OPNAME = 'TYieldModelDataObject.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FNetworkFeaturesData        := TNetworkFeaturesData.Create(FAppModules);
    FNetworkElementData         := TNetworkElementData.Create(FAppModules);
    FDataFilePaths              := TDataFilePaths.Create(FAppModules);
    FYRCGraphDataObject         := TYRCGraphDataObject.Create(FAppModules);
    FParamSetup                 := TParamSetup.Create(FAppModules);
    FRunConfigurationData       := TRunConfigurationData.Create(FAppModules);
    FYieldModelCalendar         := TYieldModelCalendar.Create(FAppModules);
    FYieldModelCapability       := TYieldModelCapability.Create(FAppModules);
    FYieldModelViews            := TModelViews.Create(FAppModules);
    FOutputData                 := TOutputData.Create(FAppModules);
    FOutputComparisonData       := TOutputComparisonList.Create(FAppModules);
    FImplementedNetworkFeatures := TImplementedNetworkFeatures.Create(FAppModules);
    FStudyMetaDataList          := TStudyMetaDataList.Create(FAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelDataObject.DestroyMemberObjects;
const OPNAME = 'TYieldModelDataObject.DestroyMemberObjects';
begin
  try
    FreeAndNil(FNetworkElementData);
    FreeAndNil(FNetworkFeaturesData);
    FreeAndNil(FDataFilePaths);
    FreeAndNil(FYRCGraphDataObject);
    FreeAndNil(FParamSetup);
    FreeAndNil(FRunConfigurationData);
    FreeAndNil(FYieldModelCalendar);
    FreeAndNil(FYieldModelCapability);
    FreeAndNil(FYieldModelViews);
    FreeAndNil(FOutputData);
    FreeAndNil(FOutputComparisonData);
    FreeAndNil(FImplementedNetworkFeatures);
    FreeAndNil(FStudyMetaDataList);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataObject._AddRef: Integer;
const OPNAME = 'TYieldModelDataObject._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelDataObject._Release: Integer;
const OPNAME = 'TYieldModelDataObject._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelDataObject.GetCastRunConfigurationData: TRunConfigurationData;
const OPNAME = 'TYieldModelDataObject.GetCastRunConfigurationData';
begin
  Result := nil;
  try
    Result := FRunConfigurationData;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataObject.GetRunConfigurationData: TRunConfigurationData;
const OPNAME = 'TYieldModelDataObject.GetRunConfigurationData';
begin
  Result := nil;
  try
    Result := FRunConfigurationData;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataObject.GetViewDataItems(AViewId: string; AItemsList: TViewModelDataItemsList; var AHandled: boolean): boolean;
const OPNAME = 'TYieldModelDataObject.GetViewDataItems';
var
  LUpperViewId: string;
begin
  Result := False;
  try
    AHandled := False;
    if (Trim(AViewId) <> '') and Assigned(AItemsList) then
    begin
      LUpperViewId := UpperCase(Trim(AViewId));
      if(Pos('RESERVOIRNAMES',LUpperViewId) = 1) then
        AHandled := GetReservoirViewDataItems(AViewId,AItemsList,FNetworkElementData.CastReservoirList)
      else
      if (Pos('NODENAMESWITHINFLOW',LUpperViewId) = 1) then
        AHandled := GetNodesWithInflowViewDataItems(AViewId,AItemsList,FNetworkElementData.CastReservoirList)
      else
      if (Pos('NODENAMESWITHOUTINFLOW',LUpperViewId) = 1) then
        AHandled := GetNodesWithoutInflowViewDataItems(AViewId,AItemsList,FNetworkElementData.CastReservoirList)
      else
      if(Pos('CHANNELNAMES',LUpperViewId) = 1) then
        AHandled := GetChannelViewDataItems(AViewId,AItemsList,FNetworkElementData.CastChannelList)
      else
      if(Pos('DEMANDFILENUMBERS',LUpperViewId) = 1) then
        AHandled := GetDemandFilesViewDataItems(AViewId,AItemsList, CastFileNamesObject)
      else
      if(Pos('HYDROLOGYFILENAMES',LUpperViewId) = 1) then
        AHandled := GetHydrologyFilesViewDataItems(AViewId, AItemsList, CastFileNamesObject)
      else
      if(Pos('POWERPLANTS',LUpperViewId) = 1) then
        AHandled := GetPowerPlantsViewDataItems(AViewId,AItemsList,FNetworkFeaturesData.CastPowerPlantList)
      else
      if(Pos('IRRIGATIONAREAS',LUpperViewId) = 1) then
        AHandled := GetIrrigationAreasViewDataItems(AViewId,AItemsList,FNetworkFeaturesData.CastIrrigationAreaList)
      else
      if(Pos('IRRIGATIONBLOCK',LUpperViewId) = 1) then
      begin
        if(FAppModules.StudyArea.ModelVersion <> '7') then
          AHandled := True
        else
        AHandled := GetIrrigationBlockViewDataItems(AViewId,AItemsList,FNetworkFeaturesData.CastIrrigationBlockList)
      end
      else
      if(Pos('WETLAND',LUpperViewId) = 1) then
      begin
        if(FAppModules.StudyArea.ModelVersion <> '7') then
          AHandled := True
        else
        AHandled := GetWetlandViewDataItems(AViewId,AItemsList,FNetworkFeaturesData.CastWetlandList);
      end
      else
      if(Pos('YMDEMANDCENTRE',LUpperViewId) = 1) then
      begin
        if(FAppModules.StudyArea.ModelVersion <> '7') then
          AHandled := True
        else
        AHandled := GetYMDemandCentreViewDataItems(AViewId,AItemsList,FNetworkFeaturesData.CastYMDemandCentreList);
      end
      else
      if(Pos('STREAMFLOWREDUCTIONNAMES',LUpperViewId) = 1) then
      begin
        if(FAppModules.StudyArea.ModelVersion <> '7') then
          AHandled := True
        else
        AHandled := GetStreamFlowReductionsViewDataItems(AViewId,AItemsList,FNetworkFeaturesData.CastStreamFlowReductionList);
      end
      else
      if(Pos('MINENAMES',LUpperViewId) = 1) then
      begin
        if(FAppModules.StudyArea.ModelVersion <> '7') then
          AHandled := True
        else
        AHandled := GetMineViewDataItems(AViewId,AItemsList,FNetworkFeaturesData.CastMineList)
      end;

      if(Pos('CHANNELAREAS',LUpperViewId) = 1) then
      begin
        AHandled := GetChannelAreaViewDataItems(AViewId,AItemsList,FNetworkFeaturesData.CastChannelAreaList)
      end;

      if(Pos('DROUGHTRESTRICTION',LUpperViewId) = 1) then
      begin
        if(FAppModules.StudyArea.ModelVersion <> '7') then
          AHandled := True
        else
        AHandled := GetDroughtRestrictionViewDataItems(AViewId,AItemsList,FNetworkFeaturesData.CastCurtailmentAndDrought)
      end;

      if(Pos('GROUNDWATER',LUpperViewId) = 1) then
      begin
        if(FAppModules.StudyArea.ModelVersion <> '7') then
          AHandled := True
        else
        AHandled := GetGroundWaterViewDataItems(AViewId,AItemsList,FNetworkFeaturesData.CastGroundWaterList)
      end;


      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataObject.GetYRCGraphDataObject: TAbstractYRCGraphDataObject;
const OPNAME = 'TYieldModelDataObject.GetYRCGraphDataObject';
begin
  Result := nil;
  try
    Result := FYRCGraphDataObject;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataObject.Get_ParamSetup: IParamSetup;
const OPNAME = 'TYieldModelDataObject.Get_ParamSetup';
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

function TYieldModelDataObject.GetCastFilesLineTypes: TFilesLineTypes;
const OPNAME = 'TYieldModelDataObject.GetCastFilesLineTypes';
begin
  Result := nil;
  try
    Result := TFilesLineTypes(FFilesLineTypes);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataObject.GetCastFileNamesObject: TModelFileNames;
const OPNAME = 'TYieldModelDataObject.GetCastFileNamesObject';
begin
  Result := nil;
  try
    Result := TModelFileNames(FFileNamesObject);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataObject.Get_OutputData : IOutputData;
const OPNAME = 'TYieldModelDataObject.Get_OutputData';
begin
  Result := nil;
  try
    Result := FOutputData;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataObject.GetHydrologyFilesForCatchment(ACatchmentRef: integer;AFilesNamesContainer: TStringList): boolean;
const OPNAME = 'TYieldModelDataObject.GetHydrologyFilesForCatchment';
var
  LParamReference : IParamReference;
begin
  Result := False;
  try
    if not Assigned(AFilesNamesContainer) then Exit;
    AFilesNamesContainer.Clear;

    LParamReference := FParamSetup.ReferenceDataByCatchNumber[ACatchmentRef];
    Result := Assigned(LParamReference);
    Result := Result and FFileNamesObject.HydrologyFileNames.FindFilesFromPrefix(
                         LParamReference.FileReference,'',AFilesNamesContainer);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataObject.GetDatasetFileDataSet(ADemandFileName: string; ADataSet: TAbstractModelDataset): boolean;
const OPNAME = 'TYieldModelDataObject.GetDatasetFileDataSet';
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

function TYieldModelDataObject.GetHydrologyFileDataSet(AHydrologyFileName: string; ADataSet: TAbstractModelDataset): boolean;
const OPNAME = 'TYieldModelDataObject.GetHydrologyFileDataSet';
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

function TYieldModelDataObject.Get_NetworkElementData: INetworkElementData;
const OPNAME = 'TYieldModelDataObject.Get_NetworkElementData';
begin
  Result := nil;
  try
    Result := FNetworkElementData;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataObject.Get_NetworkFeaturesData: INetworkFeaturesData;
const OPNAME = 'TYieldModelDataObject.Get_NetworkFeaturesData';
begin
  Result := nil;
  try
    Result := FNetworkFeaturesData;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataObject.Get_DataFilePaths: IDataFilePaths;
const OPNAME = 'TYieldModelDataObject.Get_DataFilePaths';
begin
  Result := nil;
  try
    Result := FDataFilePaths;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataObject.Get_ModelCalendar: IModelCalendar;
const OPNAME = 'TYieldModelDataObject.Get_ModelCalendar';
begin
  Result := nil;
  try
    Result := FYieldModelCalendar;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataObject.GetYieldModelCapability: IYieldModelCapability;
const OPNAME = 'TYieldModelDataObject.GetYieldModelCapability';
begin
  Result := nil;
  try
    Result := FYieldModelCapability;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataObject.Get_RunConfigurationData: IRunConfigurationData;
const OPNAME = 'TYieldModelDataObject.Get_RunConfigurationData';
begin
  Result := nil;
  try
    Result := FRunConfigurationData;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataObject.Get_YieldModelCapability: IYieldModelCapability;
const OPNAME = 'TYieldModelDataObject.Get_YieldModelCapability';
begin
  Result := nil;
  try
    Result := FYieldModelCapability;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataObject.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TYieldModelDataObject.Validate';
begin
  Result := True;
  try
    if not FNetworkElementData.Validate(AErrors,AContext) then
      Result := False;
    if(not Result) and FAppModules.GlobalData.StopOnFirstErr then
      Exit;
    if not FNetworkFeaturesData.Validate(AErrors,AContext) then
      Result := False;
    if(not Result) and FAppModules.GlobalData.StopOnFirstErr then
      Exit;
    {if not FYieldModelCalendar.Validate(AErrors,AContext) then
      Result := False;
    if(not Result) and FAppModules.GlobalData.StopOnFirstErr then
      Exit;}
    if not FRunConfigurationData.Validate(AErrors,AContext) then
      Result := False;
    if(not Result) and FAppModules.GlobalData.StopOnFirstErr then
      Exit;
    if not FDataFilePaths.Validate(AErrors,AContext) then
      Result := False;
    if(not Result) and FAppModules.GlobalData.StopOnFirstErr then
      Exit;
    if not FParamSetup.Validate(AErrors,AContext) then
      Result := False;
    if(not Result) and FAppModules.GlobalData.StopOnFirstErr then
      Exit;
    {if not FYieldModelCapability.Validate(AErrors,AContext) then
      Result := False;
    if(not Result) and FAppModules.GlobalData.StopOnFirstErr then
      Exit;}
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYieldModelDataObject.HydrologyFilesForCatchment(ACatchmentRef: Integer; var AFilesNamesContainer: WideString);
const OPNAME = 'TYieldModelDataObject.HydrologyFilesForCatchment';
var
  LFiles: TStringList;
begin
  AFilesNamesContainer := '';
  try
    LFiles := TStringList.Create;
    try
      if GetHydrologyFilesForCatchment(ACatchmentRef,LFiles) then
        AFilesNamesContainer := LFiles.Text;
    finally
      FreeAndNil(LFiles);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataObject.GetModelViewItems(AItems: TStringListOfStringLists): boolean;
const OPNAME = 'TYieldModelDataObject.GetModelViewItems';
begin
  Result := False;
  try
    Result := FYieldModelViews.GetModelViewItems(AItems);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataObject.Get_DemandFileData(const AFileName: WideString): WideString;
const OPNAME = 'TYieldModelDataObject.Get_DemandFileData';
var
  LCol : integer;
  LDataSet : TAbstractModelDataset;
  LRowData,
  LFileData: TStringList;
  LData,
  LFieldName,
  LSQL: string;
  LFileNameObject: TAbstractModelFileName;
begin
  Result := '';
  try

    LFileNameObject :=  FFileNamesObject.DemandFileNames.FindFile(AFileName);
    if(LFileNameObject = nil) then Exit;

    LRowData  := TStringList.Create;
    LFileData := TStringList.Create;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LSQL := 'SELECT DemandYearValue,'+
              'DemandMonthValue01 as Value01,'+
              'DemandMonthValue02 as Value02,'+
              'DemandMonthValue03 as Value03,'+
              'DemandMonthValue04 as Value04,'+
              'DemandMonthValue05 as Value05,'+
              'DemandMonthValue06 as Value06,'+
              'DemandMonthValue07 as Value07,'+
              'DemandMonthValue08 as Value08,'+
              'DemandMonthValue09 as Value09,'+
              'DemandMonthValue10 as Value10,'+
              'DemandMonthValue11 as Value11,'+
              'DemandMonthValue12 as Value12,'+
              'DemandTotalValue   as Value13'+
              ' FROM DemandFileData A WHERE ' +
               GetScenarioWhereClause +
              //' AND FileType = '+IntToStr(LFileNameObject.FileGroup)+
              ' AND FileNumber = '+IntToStr(LFileNameObject.FileNumber)+
              ' ORDER BY DemandYearValue';
      LDataSet.DataSet.Close;
      LDataSet.SetSQL(LSQL);
      LDataSet.DataSet.Open;
      while not LDataSet.DataSet.Eof do
      begin
        LRowData.Clear;
        LData    := IntToStr(LDataSet.DataSet.FieldByName('DemandYearValue').AsInteger);
        LRowData.Add(LData);
        for LCol:= 1 to 12 do
        begin
          LFieldName := Format('%s%2.2d',['Value',LCol]);
          LData     := FormatFloat('##0.00000',LDataSet.DataSet.FieldByName(LFieldName).AsFloat);
          LRowData.Add(LData);
        end;
        LFileData.Add(LRowData.CommaText);
        LDataSet.DataSet.Next;
      end;
      Result := LFileData.CommaText;
      LDataSet.DataSet.Close;
    finally
      LRowData.Free;
      LFileData.Free;
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataObject.Get_HydrologyFileData(const AFileName: WideString): WideString;
const OPNAME = 'TYieldModelDataObject.Get_HydrologyFileData';
var
  LYear : integer;
  LStartYear : integer;
  LEndYear : integer;
  LCol : integer;
  LDataSet : TAbstractModelDataset;
  LRowData,
  LFileData: TStringList;
  LData,
  LFieldName,
  LSQL: string;
begin
  Result := '';
  try
    LRowData  := TStringList.Create;
    LFileData := TStringList.Create;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LSQL := 'SELECT StudyAreaName, FileName,Identifier,   '+
              ' HydroYearValue,      '+
              ' HydroMonthValue01 as Value01,'+
              ' HydroMonthValue02 as Value02,'+
              ' HydroMonthValue03 as Value03,'+
              ' HydroMonthValue04 as Value04,'+
              ' HydroMonthValue05 as Value05,'+
              ' HydroMonthValue06 as Value06,'+
              ' HydroMonthValue07 as Value07,'+
              ' HydroMonthValue08 as Value08,'+
              ' HydroMonthValue09 as Value09,'+
              ' HydroMonthValue10 as Value10,'+
              ' HydroMonthValue11 as Value11,'+
              ' HydroMonthValue12 as Value12,'+
              ' HydroTotalValue   as Value13 '+
              ' FROM HydrologyFileData       '+
              ' WHERE FileName = '+QuotedStr(ExtractFileName(AFileName))+
              ' AND StudyAreaName = '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND Identifier  IS NOT NULL'+
              ' ORDER BY FileName,Identifier';
      LDataSet.DataSet.Close;
      LDataSet.SetSQL(LSQL);
      LDataSet.DataSet.Open;
      LStartYear := FRunConfigurationData.Get_StartYearOther;
      LEndYear   := LStartYear + FRunConfigurationData.Get_YearsInAnalysis -1;

      while not LDataSet.DataSet.Eof do
      begin
        LRowData.Clear;
        LYear    := LDataSet.DataSet.FieldByName('HydroYearValue').AsInteger;
        if(LYear >= LStartYear) and (LYear <= LEndYear) then
        begin
          LData    := IntToStr(LYear);
          LRowData.Add(LData);
          for LCol:= 1 to 12 do
          begin
            LFieldName := Format('%s%2.2d',['Value',LCol]);
            LData     := FormatFloat('##0.00000',LDataSet.DataSet.FieldByName(LFieldName).AsFloat);
            LRowData.Add(LData);
          end;
          if(LRowData.Count > 0) then
            LFileData.Add(LRowData.CommaText);
        end;
        LDataSet.DataSet.Next;
      end;
      Result := LFileData.CommaText;
      LDataSet.DataSet.Close;
    finally
      LRowData.Free;
      LFileData.Free;
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataObject.GetScenarioWhereClause: string;
const OPNAME = 'TYieldModelDataObject.GetScenarioWhereClause';
begin
  Result := '';
  try
    Result :=
      ' (A.Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND ' +
      ' (A.StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
      ' (A.SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
      ' (A.Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataObject.Get_ImplementedNetworkFeatures: IImplementedNetworkFeatures;
const OPNAME = 'TYieldModelDataObject.Get_ImplementedNetworkFeatures';
begin
  Result := nil;
  try
    Result := FImplementedNetworkFeatures;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataObject.Initialise: boolean;
const OPNAME = 'TYieldModelDataObject.Initialise';
begin
  Result := False;
  try
    if (inherited Initialise) then
    begin
      FYRCGraphDataObject.Reset;
      Result := FNetworkElementData.Initialise;
      Result := Result AND FNetworkFeaturesData.Initialise;
      Result := Result and FDataFilePaths.Initialise;
      Result := Result and FParamSetup.Initialise;
      Result := Result and FOutputData.Initialise;
      Result := Result and FOutputComparisonData.Initialise;
      Result := Result and FImplementedNetworkFeatures.Initialise;
      Result := Result and FStudyMetaDataList.Initialise;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataObject.LanguageHasChanged: boolean;
const OPNAME = 'TYieldModelDataObject.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FNetworkElementData.LanguageHasChanged;
    FNetworkFeaturesData.LanguageHasChanged;
    FYieldModelCalendar.LanguageHasChanged;
    FParamSetup.LanguageHasChanged;
    FFileNamesObject.LanguageHasChanged;
    FRunConfigurationData.LanguageHasChanged;
    FYRCGraphDataObject.LanguageHasChanged;
    FYieldModelViews.LanguageHasChanged;
    FDataFilePaths.LanguageHasChanged;
    FYieldModelCapability.LanguageHasChanged;
    FOutputData.LanguageHasChanged;
    FOutputComparisonData.LanguageHasChanged;
    FImplementedNetworkFeatures.LanguageHasChanged;
    FStudyMetaDataList.LanguageHasChanged;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataObject.ResetState: boolean;
const OPNAME = 'TYieldModelDataObject.ResetState';
begin
  Result := inherited ResetState;
  try
    FNetworkElementData.ResetState;
    FNetworkFeaturesData.ResetState;
    FYieldModelCalendar.ResetState;
    FParamSetup.ResetState;
    FFileNamesObject.ResetState;
    FRunConfigurationData.ResetState;
    FYRCGraphDataObject.ResetState;
    FYieldModelViews.ResetState;
    FDataFilePaths.ResetState;
    FYieldModelCapability.ResetState;
    FOutputData.ResetState;
    FOutputComparisonData.ResetState;
    FImplementedNetworkFeatures.ResetState;
    FStudyMetaDataList.ResetState;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataObject.SaveState: boolean;
const OPNAME = 'TYieldModelDataObject.SaveState';
begin
  Result := inherited SaveState;
  try
    FNetworkElementData.SaveState;
    FNetworkFeaturesData.SaveState;
    FYieldModelCalendar.SaveState;
    FParamSetup.SaveState;
    FFileNamesObject.SaveState;
    FRunConfigurationData.SaveState;
    FYRCGraphDataObject.SaveState;
    FYieldModelViews.SaveState;
    FDataFilePaths.SaveState;
    FYieldModelCapability.SaveState;
    FOutputData.SaveState;
    FOutputComparisonData.SaveState;
    FImplementedNetworkFeatures.SaveState;
    FStudyMetaDataList.SaveState;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataObject.StudyDataHasChanged(AContext: TChangeContext; AFieldName, AOldValue,ANewValue: string): boolean;
const OPNAME = 'TYieldModelDataObject.StudyDataHasChanged';
begin
  Result := True;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataObject.StudyHasChanged: boolean;
const OPNAME = 'TYieldModelDataObject.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    FNetworkElementData.StudyHasChanged;
    FNetworkFeaturesData.StudyHasChanged;
    FYieldModelCalendar.StudyHasChanged;
    FParamSetup.StudyHasChanged;
    FFileNamesObject.StudyHasChanged;
    FRunConfigurationData.StudyHasChanged;
    FYRCGraphDataObject.StudyHasChanged;
    FYieldModelViews.StudyHasChanged;
    FDataFilePaths.StudyHasChanged;
    FYieldModelCapability.StudyHasChanged;
    FOutputData.StudyHasChanged;
    FOutputComparisonData.StudyHasChanged;
    FImplementedNetworkFeatures.StudyHasChanged;
    FStudyMetaDataList.StudyHasChanged;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
