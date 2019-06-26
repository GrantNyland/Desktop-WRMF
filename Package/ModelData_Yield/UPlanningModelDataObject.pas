//
//
//  UNIT      : Contains  TPlanningModelDataObject Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 04/01/2006
//  COPYRIGHT : Copyright © 2006 DWAF
//
//
unit UPlanningModelDataObject;

interface

uses
  Classes, SysUtils, Contnrs,

  UAbstractObject,
  UGrowthFactorData,
  UFilesLineTypeObject,
  UGrowthFactorsExcelData,
  UYieldModelDataObject,
  UViewModelDataObject,
  UDisbenefitFunctionData,
  UAllocationDefinitionData,
  USwitchDefinition,
  UTariffCalculationData,
  UReturnFlowChannelData,
  UWQConstraintData,
  UWRPMPostProcessorData,
  UMultiResChannelCurtailmentData,
  UAbstractFileNamesObject,
  UPlanningMineData,
  VoaimsCom_TLB;

type
  TPlanningModelDataObject = class(TYieldModelDataObject, IPlanningModelData)
  protected
    FAllocationDefinitionsList    : TAllocationDefinitionsList;
    FSwitchDefinitionsList        : TSwitchDefinitionsList;
    FGrowthFactors                : TGrowthFactors;
    FExelGrowthFactors            : TExelGrowthFactors;
    FDisbenefitFunction           : TDisbenefitFunctionData;
    FReturnFlowChannel            : TReturnFlowChannelData;
    FTariffCalculationData        : TTariffCalculationData;
    FWQConstriantData             : TWQConstriantData;
    FWRPMPostProcessorData        : TWRPMPostProcessorData;
    FReservoirsWithOutputList     : TStringList;
    FChannelsWithOutputList       : TStringList;
    FSubSystemWithOutputList      : TStringList;
    FOutputFilesListsLoaded       : boolean;
    FMultiCurtailList             : TMultiResChannelCurtailmentList;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    function GetMasterControlViewDataItems(AViewID : string;AItemsList : TViewModelDataItemsList): boolean;
    function GetSubSystemStorageViewDataItems(AViewID : string;AItemsList : TViewModelDataItemsList): boolean;
    function GetTotalSystemStorageViewDataItems(AViewID : string;AItemsList : TViewModelDataItemsList): boolean;
    function GetSubSystemCurtailmentViewDataItems(AViewID : string;AItemsList : TViewModelDataItemsList): boolean;
    function GetTotalSystemCurtailmentViewDataItems(AViewID : string;AItemsList : TViewModelDataItemsList): boolean;

    function GetAllocationControlViewDataItems(AViewID : string;AItemsList : TViewModelDataItemsList): boolean;
    function GetSwitchControlViewDataItems(AViewID : string;AItemsList : TViewModelDataItemsList): boolean;
    function GetDemandSupplyViewDataItems(AViewID : string;AItemsList : TViewModelDataItemsList): boolean;
    function GetReviewInterBasinSupportChannelsViewDataItems(AViewID : string;AItemsList : TViewModelDataItemsList): boolean;
    function Get_AllocationDefinitionsList : IAllocationDefinitionsList; safecall;
    function Get_SwitchDefinitionsList     : ISwitchDefinitionsList; safecall;
    function Get_TariffCalculationData: ITariffCalculationData; safecall;
    procedure LoadOutputFilesLists;

  public
    procedure Reset;
    function Initialise: boolean; override;
    function StudyHasChanged: boolean; override;
    function GetViewDataItems(AViewId : string; AItemsList : TViewModelDataItemsList; var AHandled : boolean): boolean; override;
    function ViewDataItemExist(AItemType, AItemName : string): boolean; override;
    property AllocationDefinitionsList        : IAllocationDefinitionsList        read Get_AllocationDefinitionsList;
    property SwitchDefinitionsList            : ISwitchDefinitionsList            read Get_SwitchDefinitionsList;
    property CastAllocationDefinitionsList    : TAllocationDefinitionsList        read FAllocationDefinitionsList;
    property CastSwitchDefinitionsList        : TSwitchDefinitionsList            read FSwitchDefinitionsList;
    property CastGrowthFactors                : TGrowthFactors                    read FGrowthFactors;
    property CastExelGrowthFactors            : TExelGrowthFactors                read FExelGrowthFactors;
    property CastDisbenefitFunction           : TDisbenefitFunctionData           read FDisbenefitFunction;
    property CastReturnFlowChannel            : TReturnFlowChannelData            read FReturnFlowChannel;
    property CastTariffCalculationData        : TTariffCalculationData            read FTariffCalculationData;
    property CastWRPMPostProcessorData        : TWRPMPostProcessorData            read FWRPMPostProcessorData;
    property CastWQConstriantData             : TWQConstriantData                 read FWQConstriantData;
    property CastMultiRestrictionData         : TMultiResChannelCurtailmentList   read FMultiCurtailList;
  end;

implementation

uses
  UConstants,
  UChannelDataSQLAgent,
  UWRPMPmpFileManager,
  UWRPMPltFileManager,
  UErrorHandlingOperations;


procedure TPlanningModelDataObject.CreateMemberObjects;
const OPNAME = 'TPlanningModelDataObject.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FAllocationDefinitionsList     := TAllocationDefinitionsList.Create(FAppModules);
    FSwitchDefinitionsList         := TSwitchDefinitionsList.Create(FAppModules);
    FGrowthFactors                 := TGrowthFactors.Create(FAppModules);
    FExelGrowthFactors             := TExelGrowthFactors.Create(FAppModules);
    FDisbenefitFunction            := TDisbenefitFunctionData.Create(FAppModules);
    FReturnFlowChannel             := TReturnFlowChannelData.Create(FAppModules);
    FTariffCalculationData         := TTariffCalculationData.Create(FAppModules);
    FWRPMPostProcessorData         := TWRPMPostProcessorData.Create(FAppModules);
    FWQConstriantData              := TWQConstriantData.Create(FAppModules);
    FMultiCurtailList              := TMultiResChannelCurtailmentList.Create(FAppModules);
    FReservoirsWithOutputList      := TStringList.Create;
    FChannelsWithOutputList        := TStringList.Create;
    FSubSystemWithOutputList       := TStringList.Create;
    FReservoirsWithOutputList.CaseSensitive := False;
    FChannelsWithOutputList.CaseSensitive   := False;
    FSubSystemWithOutputList.CaseSensitive  := False;
    FOutputFilesListsLoaded                 := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningModelDataObject.DestroyMemberObjects;
const OPNAME = 'TPlanningModelDataObject.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FreeAndNil(FAllocationDefinitionsList);
    FreeAndNil(FSwitchDefinitionsList);
    FreeAndNil(FGrowthFactors);
    FreeAndNil(FExelGrowthFactors);
    FreeAndNil(FDisbenefitFunction);
    FreeAndNil(FReturnFlowChannel);
    FreeAndNil(FTariffCalculationData);
    FreeAndNil(FWRPMPostProcessorData);
    FreeAndNil(FReservoirsWithOutputList);
    FreeAndNil(FChannelsWithOutputList);
    FreeAndNil(FWQConstriantData);
    FreeAndNil(FMultiCurtailList);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningModelDataObject._AddRef: Integer;
const OPNAME = 'TPlanningModelDataObject._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPlanningModelDataObject._Release: Integer;
const OPNAME = 'TPlanningModelDataObject._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPlanningModelDataObject.Reset;
const OPNAME = 'TPlanningModelDataObject.Reset';
begin
  try
    FFileNamesObject.Reset;
    FYRCGraphDataObject.Reset;
    FGrowthFactors.Initialise;
    FExelGrowthFactors.Initialise;
    FDisbenefitFunction.Initialise;
    FNetworkFeaturesData.Initialise;
    FNetworkElementData.Initialise;
    FReturnFlowChannel.Initialise;
    FTariffCalculationData.Initialise;
    FWQConstriantData.Initialise;
    FParamSetup.Initialise;
    FOutputData.Initialise;
    FWRPMPostProcessorData.Initialise;
    FMultiCurtailList.Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningModelDataObject.Initialise: boolean;
const OPNAME = 'TPlanningModelDataObject.Initialise';
begin
  inherited Initialise;
  Result := False;
  try
    Reset;
    Result := FNetworkElementData.Initialise;
    Result := Result AND FNetworkFeaturesData.Initialise;
    Result := Result and FParamSetup.Initialise;
    Result := Result and FOutputData.Initialise;
    Result := Result and FGrowthFactors.Initialise;
    Result := Result and FExelGrowthFactors.Initialise;
    Result := Result and FReturnFlowChannel.Initialise;
    Result := Result and FTariffCalculationData.Initialise;
    Result := Result and FWQConstriantData.Initialise;
    Result := Result and FWRPMPostProcessorData.Initialise;

    FReservoirsWithOutputList.Clear;
    FChannelsWithOutputList.Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningModelDataObject.StudyHasChanged: boolean;
const OPNAME = 'TPlanningModelDataObject.StudyHasChanged';
begin
  Result := False;
  try
    FOutputFilesListsLoaded  := False;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningModelDataObject.Get_AllocationDefinitionsList : IAllocationDefinitionsList;
const OPNAME = 'TPlanningModelDataObject.Get_AllocationDefinitionsList';
begin
  Result := nil;
  try
    Result := FAllocationDefinitionsList;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPlanningModelDataObject.Get_SwitchDefinitionsList : ISwitchDefinitionsList;
const OPNAME = 'TPlanningModelDataObject.Get_SwitchDefinitionsList';
begin
  Result := nil;
  try
    Result := FSwitchDefinitionsList;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPlanningModelDataObject.Get_TariffCalculationData: ITariffCalculationData;
const OPNAME = 'TPlanningModelDataObject.Get_SwitchDefinitionsList';
begin
  Result := nil;
  try
    Result := FTariffCalculationData;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPlanningModelDataObject.GetViewDataItems (AViewId     : string;
                                                    AItemsList   : TViewModelDataItemsList;
                                                    var AHandled : boolean): boolean;
const OPNAME = 'TPlanningModelDataObject.GetViewDataItems';
var
  LUpperViewId: string;
begin
  Result := False;
  try
    if not FOutputFilesListsLoaded then
       LoadOutputFilesLists;

    AItemsList.Reset;
    if (Trim(AViewId) <> '') and Assigned(AItemsList) then
    begin
      LUpperViewId := UpperCase(Trim(AViewId));

      if (Pos('MASTERCONTROLNAMES',LUpperViewId) = 1) then
        AHandled := GetMasterControlViewDataItems(AViewId,AItemsList)
      else if (Pos('SUBSYSTEMSTORAGENAMES',LUpperViewId) = 1) then
        AHandled := GetSubSystemStorageViewDataItems(AViewId,AItemsList)
      else if (Pos('TOTALSYSTEMSTORAGENAMES',LUpperViewId) = 1) then
        AHandled := GetTotalSystemStorageViewDataItems(AViewId,AItemsList)
      else if (Pos('SUBSYSTEMCURTAILMENTNAMES',LUpperViewId) = 1) then
        AHandled := GetSubSystemCurtailmentViewDataItems(AViewId,AItemsList)
      else if (Pos('TOTALSYSTEMCURTAILMENTNAMES',LUpperViewId) = 1) then
        AHandled := GetTotalSystemCurtailmentViewDataItems(AViewId,AItemsList)
      else if (Pos('DEMANDSUPPLYNAMES',LUpperViewId) = 1) then
        AHandled := GetDemandSupplyViewDataItems(AViewId,AItemsList)
      else if (Pos('INTERBASINSUPPORTNAMES',LUpperViewId) = 1) then
        AHandled := GetReviewInterBasinSupportChannelsViewDataItems(AViewId,AItemsList)

      else if (Pos('ALLOCATIONDEFINITIONNAMES',LUpperViewId) = 1) then
        AHandled := GetAllocationControlViewDataItems(AViewId,AItemsList)
      else if (Pos('SWITCHDEFINITIONNAMES',LUpperViewId) = 1) then
        AHandled := GetSwitchControlViewDataItems(AViewId,AItemsList);

      Result := TRUE;
    end;
    if (not AHandled) then
      Result := inherited GetViewDataItems(AViewId, AItemsList, AHandled);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningModelDataObject.GetMasterControlViewDataItems(AViewID: string; AItemsList: TViewModelDataItemsList): boolean;
const OPNAME = 'TPlanningModelDataObject.GetMasterControlViewDataItems';
var
  LIndex                : integer;
  LChannelData          : IGeneralFlowChannel;
  LViewModelDataItem    : TViewModelDataItem;
Begin
  Result := False;
  try
    if(AViewID <> '') AND Assigned(AItemsList) then
    begin
      for LIndex := 0 to FNetworkElementData.CastChannelList.ChannelCount - 1 do
      begin
        LChannelData := FNetworkElementData.CastChannelList.ChannelByIndex[LIndex];
        if(LChannelData.ChannelType = 2) then
        begin
          LViewModelDataItem := AItemsList.AddViewModelDataItem;
          if Assigned(LViewModelDataItem) then
          begin
            LViewModelDataItem.Weighting   := LChannelData.ChannelNumber;
            LViewModelDataItem.Caption     := Trim(LChannelData.ChannelName);
            LViewModelDataItem.ParamNames  :=  'AChannelNumber' ;
            LViewModelDataItem.ParamValues := IntToStr(LChannelData.ChannelNumber);
            LViewModelDataItem.DataType    := 'CHANNELNUMBER';
          end;
        end;
      end;
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningModelDataObject.GetSubSystemStorageViewDataItems(AViewID: string; AItemsList: TViewModelDataItemsList): boolean;
const OPNAME = 'TPlanningModelDataObject.GetSubSystemStorageViewDataItems';
var
  LIndex                : integer;
  LCount                : integer;
  LSubSystem            : ISubSystem;
  LViewModelDataItem    : TViewModelDataItem;
  LAllocationDefinition : IAllocationDefinition;
Begin
  Result := False;
  try
    if(AViewID <> '') AND Assigned(AItemsList) then
    begin
      for LIndex := 0 to FAllocationDefinitionsList.AllocationDefinitionCount - 1 do
      begin
        LAllocationDefinition := FAllocationDefinitionsList.AllocationDefinitionByIndex[LIndex];
        if Assigned(LAllocationDefinition) then
        begin
          for LCount := 0 to LAllocationDefinition.NrOfSubSystems-1 do
          begin
            LSubSystem := LAllocationDefinition.SubSystemByIndex[LCount];
            if Assigned(LSubSystem) then
            begin
              LViewModelDataItem := AItemsList.AddViewModelDataItem;
              if Assigned(LViewModelDataItem) then
              begin
                LViewModelDataItem.Caption     := Trim(LSubSystem.Name);
                LViewModelDataItem.Weighting   := LAllocationDefinition.AllocationDefinitionID+LSubSystem.SubSystemID;
                LViewModelDataItem.ParamNames  := 'Identifier' ;
                LViewModelDataItem.ParamValues := IntToStr(LAllocationDefinition.AllocationDefinitionID) +','+IntToStr(LSubSystem.SubSystemID);
                LViewModelDataItem.DataType    := 'ALLOCATIONDEFINITIONID_SUBSYSTEMID';
              end;
            end;
          end;
        end;
      end;
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningModelDataObject.GetTotalSystemStorageViewDataItems(AViewID: string; AItemsList: TViewModelDataItemsList): boolean;
const OPNAME = 'TPlanningModelDataObject.GetTotalSystemStorageViewDataItems';
var
  LIndex                : integer;
  LViewModelDataItem    : TViewModelDataItem;
  LFileName             : TAbstractModelFileName;
  LName                 : string;
  LSysPrefix            : string;
Begin
  Result := False;
  try
    if(AViewID <> '') AND Assigned(AItemsList) then
    begin
      LSysPrefix := UpperCase(FDataFilePaths.DataFilePrefix)+ 'SYS';
      for LIndex := 0 to CastFileNamesObject.CastOutputFileNames.Count - 1 do
      begin
        LFileName := CastFileNamesObject.CastOutputFileNames.CastFileObject[LIndex];
        if Assigned(LFileName) and FileExists(LFileName.FileName) then
        begin
          LName := UpperCase(ExtractFileName(LFileName.ShortName));
          if(Pos(LSysPrefix,LName) > 0) then
          begin
            LViewModelDataItem := AItemsList.AddViewModelDataItem;
            if Assigned(LViewModelDataItem) then
            begin
              LViewModelDataItem.Caption     := Trim(LFileName.ShortName);
              LViewModelDataItem.Weighting   := LIndex+1;
              LViewModelDataItem.ParamNames  := 'FileName' ;
              LViewModelDataItem.ParamValues := LFileName.FileName;
              LViewModelDataItem.DataType    := 'TOTALSYSTEMSTORAGE';
            end;
          end;
        end;
      end;
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningModelDataObject.GetSubSystemCurtailmentViewDataItems(AViewID: string; AItemsList: TViewModelDataItemsList): boolean;
const OPNAME = 'TPlanningModelDataObject.GetSubSystemCurtailmentViewDataItems';
var
  LIndex                : integer;
  LCount                : integer;
  LSubSystem            : ISubSystem;
  LViewModelDataItem    : TViewModelDataItem;
  LAllocationDefinition : IAllocationDefinition;
Begin
  Result := False;
  try
    if(AViewID <> '') AND Assigned(AItemsList) then
    begin
      for LIndex := 0 to FAllocationDefinitionsList.AllocationDefinitionCount - 1 do
      begin
        LAllocationDefinition := FAllocationDefinitionsList.AllocationDefinitionByIndex[LIndex];
        if Assigned(LAllocationDefinition) then
        begin
          for LCount := 0 to LAllocationDefinition.NrOfSubSystems-1 do
          begin
            LSubSystem := LAllocationDefinition.SubSystemByIndex[LCount];
            if Assigned(LSubSystem) then
            begin
              LViewModelDataItem := AItemsList.AddViewModelDataItem;
              if Assigned(LViewModelDataItem) then
              begin
                LViewModelDataItem.Caption     := Trim(LSubSystem.Name);
                LViewModelDataItem.Weighting   := LAllocationDefinition.AllocationDefinitionID+LSubSystem.SubSystemID;
                LViewModelDataItem.ParamNames  := 'Identifier' ;
                LViewModelDataItem.ParamValues := IntToStr(LAllocationDefinition.AllocationDefinitionID) +','+IntToStr(LSubSystem.SubSystemID);
                LViewModelDataItem.DataType    := 'CURTAILMENT';
              end;
            end;
          end;
        end;
      end;
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningModelDataObject.GetTotalSystemCurtailmentViewDataItems(AViewID: string; AItemsList: TViewModelDataItemsList): boolean;
const OPNAME = 'TPlanningModelDataObject.GetTotalSystemStorageViewDataItems';
var
  LIndex                : integer;
  LViewModelDataItem    : TViewModelDataItem;
  LFileName             : TAbstractModelFileName;
  LName                 : string;
  LSysPrefix            : string;
Begin
  Result := False;
  try
    if(AViewID <> '') AND Assigned(AItemsList) then
    begin
      LSysPrefix := UpperCase(FDataFilePaths.DataFilePrefix)+ 'SYS';
      for LIndex := 0 to CastFileNamesObject.CastOutputFileNames.Count - 1 do
      begin
        LFileName := CastFileNamesObject.CastOutputFileNames.CastFileObject[LIndex];
        if Assigned(LFileName) and FileExists(LFileName.FileName) then
        begin
          LName := UpperCase(ExtractFileName(LFileName.ShortName));
          if(Pos(LSysPrefix,LName) > 0) then
          begin
            LViewModelDataItem := AItemsList.AddViewModelDataItem;
            if Assigned(LViewModelDataItem) then
            begin
              LViewModelDataItem.Caption     := Trim(LFileName.ShortName);
              LViewModelDataItem.Weighting   := LIndex+1;
              LViewModelDataItem.ParamNames  := 'FileName' ;
              LViewModelDataItem.ParamValues := LFileName.FileName;
              LViewModelDataItem.DataType    := 'TOTALSYSTEMCURTAILMENT';
            end;
          end;
        end;
      end;
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningModelDataObject.GetDemandSupplyViewDataItems(AViewID: string; AItemsList: TViewModelDataItemsList): boolean;
const OPNAME = 'TPlanningModelDataObject.GetDemandSupplyViewDataItems';
var
  LIndex                : integer;
  LViewModelDataItem    : TViewModelDataItem;
  LFileName             : TAbstractModelFileName;
  LName                 : string;
  LSysPrefix            : string;
Begin
  Result := False;
  try
    if(AViewID <> '') AND Assigned(AItemsList) then
    begin
      LSysPrefix := UpperCase(FDataFilePaths.DataFilePrefix)+ 'RES';
      for LIndex := 0 to CastFileNamesObject.CastOutputFileNames.Count - 1 do
      begin
        LFileName := CastFileNamesObject.CastOutputFileNames.CastFileObject[LIndex];
        if Assigned(LFileName) and FileExists(LFileName.FileName) then
        begin
          LName := UpperCase(ExtractFileName(LFileName.ShortName));
          if(Pos(LSysPrefix,LName) > 0) then
          begin
            LViewModelDataItem := AItemsList.AddViewModelDataItem;
            if Assigned(LViewModelDataItem) then
            begin
              LViewModelDataItem.Caption     := Trim(LFileName.ShortName);
              LViewModelDataItem.Weighting   := LIndex+1;
              LViewModelDataItem.ParamNames  := 'FileName' ;
              LViewModelDataItem.ParamValues := LFileName.FileName;
              LViewModelDataItem.DataType    := 'REVIEWDEMANDSUPPLY';
            end;
          end;
        end;
      end;
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningModelDataObject.GetReviewInterBasinSupportChannelsViewDataItems(AViewID: string; AItemsList: TViewModelDataItemsList): boolean;
const OPNAME = 'TPlanningModelDataObject.GetReviewInterBasinSupportChannelsViewDataItems';
var
  LIndex             : integer;
  LChannelNumber     : integer;
  LSQLAgent          : TChannelDataSQLAgent;
  LChannelNumbers    : TStringList;
  LChannelData       : IGeneralFlowChannel;
  LViewModelDataItem : TViewModelDataItem;
begin
  Result := False;
  try
    if(AViewID <> '') AND Assigned(AItemsList) then
    begin
      LSQLAgent       := TChannelDataSQLAgent.Create(FAppModules);
      LChannelNumbers := TStringList.Create;
      try
        LChannelNumbers.CommaText := LSQLAgent.GetInterBasinSupportChannelNumbersCommaText;
        for LIndex := 0 to LChannelNumbers.Count-1 do
        begin
          LChannelNumber := StrToIntDef(LChannelNumbers[LIndex],0);
          if(LChannelNumber > 0) then
          begin
            LChannelData := FNetworkElementData.CastChannelList.ChannelByChannelNumber[LChannelNumber];
            if(LChannelData <> nil) then
            begin
              LViewModelDataItem := AItemsList.AddViewModelDataItem;
              if Assigned(LViewModelDataItem) then
              begin
                LViewModelDataItem.Caption     := LChannelData.ChannelName;
                LViewModelDataItem.Weighting   := LChannelNumber;
                LViewModelDataItem.ParamNames  := 'ChannelName' ;
                LViewModelDataItem.ParamValues := LChannelData.ChannelName;
                LViewModelDataItem.DataType    := 'REVIEWINTERBASINSUPPORT';
              end;
            end;
          end;
        end;
        Result := TRUE;
      finally
        LChannelNumbers.Free;
        LSQLAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPlanningModelDataObject.GetAllocationControlViewDataItems (AViewID    : string; AItemsList : TViewModelDataItemsList): boolean;
const OPNAME = 'TPlanningModelDataObject.GetAllocationControlViewDataItems';
Var
  LIndex             : integer;
  lYear              : integer;
  lMonth             : integer;
  lViewModelDataItem : TViewModelDataItem;
  lAllocDef          : TAllocationDefinition;
  LDataContainer     : TStringList;
  LStartDate         : string;
Begin
  Result := False;
  try
    if(AViewID <> '') AND Assigned(AItemsList) then
    begin
      LDataContainer := TStringList.Create;
      try
        LDataContainer.Sorted := True;
        LDataContainer.Duplicates := dupAccept;
        for LIndex := 0 to FAllocationDefinitionsList.AllocationDefinitionCount - 1 do
        begin
          lAllocDef := FAllocationDefinitionsList.CastAllocationDefinitionByIndex(LIndex);
          if Assigned(lAllocDef) then
          begin
            if(lAllocDef.StartYear = NullInteger) then
              lYear := 1900
            else
              lYear := lAllocDef.StartYear;

            if(lAllocDef.StartMonth = NullInteger) then
              lMonth := 1
            else
              lMonth := lAllocDef.StartMonth;

            LStartDate := FormatFloat('0000',lYear) + FormatFloat('00',lMonth);
            LDataContainer.AddObject(LStartDate,lAllocDef)
          end;
        end;

        for LIndex := 0 to LDataContainer.Count - 1 do
        begin
          lAllocDef := TAllocationDefinition(LDataContainer.Objects[LIndex]);
          if Assigned(lAllocDef) then
          begin
            LViewModelDataItem := AItemsList.AddViewModelDataItem;
            if Assigned(LViewModelDataItem) then
            begin
              LViewModelDataItem.Caption     := Trim(lAllocDef.Name);
              LViewModelDataItem.Weighting   := lAllocDef.AllocationDefinitionID;
              LViewModelDataItem.ParamNames  :=  'Identifier' ;
              LViewModelDataItem.ParamValues := IntToStr(lAllocDef.AllocationDefinitionID);
              LViewModelDataItem.DataType    := 'ALLOCATIONDEFINITION';
            end;
          end;
        end;
       finally
         LDataContainer.Free;
       end;

      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningModelDataObject.GetSwitchControlViewDataItems (AViewID    : string;
                                                                 AItemsList : TViewModelDataItemsList): boolean;
const OPNAME = 'TPlanningModelDataObject.GetSwitchControlViewDataItems';
Var
  LIndex             : integer;
  lViewModelDataItem : TViewModelDataItem;
  lSwitchDef         : ISwitchDefinition;
Begin
  Result := False;
  try
    if(AViewID <> '') AND Assigned(AItemsList) then
    begin
      for LIndex := 0 to FSwitchDefinitionsList.SwitchDefinitionCount - 1 do
      begin
        lSwitchDef := FSwitchDefinitionsList.SwitchDefinitionByIndex[LIndex];
        if Assigned(lSwitchDef) then
        begin
          LViewModelDataItem := AItemsList.AddViewModelDataItem;
          if Assigned(LViewModelDataItem) then
          begin
            LViewModelDataItem.Caption     := Trim(lSwitchDef.SwitchDefFileName);
            LViewModelDataItem.Weighting   := lSwitchDef.SwitchDefID;
            LViewModelDataItem.ParamNames  :=  'Identifier' ;
            LViewModelDataItem.ParamValues := IntToStr(lSwitchDef.SwitchDefID);
            LViewModelDataItem.DataType    := 'SWITCHDEFINITION';
          end;
        end;
      end;
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TPlanningModelDataObject.LoadOutputFilesLists;
const OPNAME = 'TPlanningModelDataObject.LoadOutputFilesLists';
var
  LPltFileManager : TWRPMPltFileManager;
  LPmpFileManager : TWRPMPmpFileManager;
  LFileName       : string;
  LIndex          : integer;
begin
  try
    FReservoirsWithOutputList.Clear;
    FChannelsWithOutputList.Clear;
    FSubSystemWithOutputList.Clear;

    LPltFileManager := TWRPMPltFileManager.Create(FAppModules);
    LPmpFileManager := TWRPMPmpFileManager.Create(FAppModules);
    try
      LFileName     := FWRPMPostProcessorData.GetOutputPlotFileName;
      if LPltFileManager.ReadHeaderData(LFileName) then
      begin
        for LIndex := 0 to LPltFileManager.HeaderData.ReservoirsList.Count-1 do
          FReservoirsWithOutputList.Add(LPltFileManager.HeaderData.ReservoirsList[LIndex]);
        for LIndex := 0 to LPltFileManager.HeaderData.DemandChannelsList.Count-1 do
          FChannelsWithOutputList.Add(LPltFileManager.HeaderData.DemandChannelsList[LIndex]);
        for LIndex := 0 to LPltFileManager.HeaderData.SubSystemsList.Count-1 do
          FSubSystemWithOutputList.Add(LPltFileManager.HeaderData.SubSystemsList[LIndex]);
      end;

      LFileName     := FWRPMPostProcessorData.GetOutputPumpFileName;
      if LPmpFileManager.ReadHeaderData(LFileName) then
      begin
        for LIndex := 0 to LPmpFileManager.HeaderData.PumpingChannelsList.Count-1 do
          FChannelsWithOutputList.Add(LPmpFileManager.HeaderData.PumpingChannelsList[LIndex]);
      end;
      FOutputFilesListsLoaded := True;
    finally
      LPltFileManager.Free;
      LPmpFileManager.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningModelDataObject.ViewDataItemExist(AItemType,AItemName: string): boolean;
const OPNAME = 'TPlanningModelDataObject.ViewDataItemExist';
begin
  Result := False;
  try
    if(UpperCase(AItemType) = 'RESERVOIRNAMES') then
      Result := (FReservoirsWithOutputList.IndexOf(AItemName) >= 0);

    if(UpperCase(AItemType) = 'CHANNELNAMES') then
      Result := (FChannelsWithOutputList.IndexOf(AItemName) >= 0);

    if(UpperCase(AItemType) = 'SUBSYSTEMNAMES') then
      Result := (FSubSystemWithOutputList.IndexOf(AItemName) >= 0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
