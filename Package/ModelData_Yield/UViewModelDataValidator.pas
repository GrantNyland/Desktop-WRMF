unit UViewModelDataValidator;

interface
uses
  //  Delphi VCL
  Classes,sysutils,

  //  DWAF VCL
  UAbstractFileNamesObject,
  UFileNames,
  UViewModelDataObject,
  UYRCGraphDataObject,
  VoaimsCom_TLB,
  UReservoirData,
  UFilesLineTypeObject,
  UReservoirPenaltyStructureData,
  UReservoirZoneElevationData,
  UWetland,
  UYMDemandCentre,
  UStreamFlowReduction,
  UMiningData,
  UCurtailmentAndDrought,
  UChannelAreas,
  UGroundWater;

function GetHydrologyFilesViewDataItems(AViewId: string; AItemsList:TViewModelDataItemsList;AFileNamesObject: TModelFileNames): boolean;
function GetDemandFilesViewDataItems(AViewId: string; AItemsList:TViewModelDataItemsList;AFileNamesObject: TModelFileNames): boolean;
function GetReservoirViewDataItems(AViewId: string; AItemsList:TViewModelDataItemsList;AReservoirData: TReservoirDataList): boolean;
function GetNodesWithInflowViewDataItems (AViewId    : string;
                                          AItemsList : TViewModelDataItemsList;
                                          ANodeData  : TReservoirDataList) : boolean;
function GetNodesWithoutInflowViewDataItems (AViewId    : string;
                                             AItemsList : TViewModelDataItemsList;
                                             ANodeData  : TReservoirDataList) : boolean;
function GetChannelViewDataItems(AViewId: string; AItemsList:TViewModelDataItemsList;AChannelDataList: IChannelList): boolean;
function GetPowerPlantsViewDataItems(AViewId         : string;
                                     AItemsList      : TViewModelDataItemsList;
                                     APowerPlantList : IPowerPlantList): boolean;
function GetIrrigationAreasViewDataItems(AViewId             : string;
                                         AItemsList          : TViewModelDataItemsList;
                                         AIrrigationAreaList : IIrrigationAreaList): boolean;
function GetIrrigationBlockViewDataItems(AViewId              : string;
                                         AItemsList           : TViewModelDataItemsList;
                                         AIrrigationBlockList : IIrrigationBlockList): Boolean;
function GetWetlandViewDataItems(AViewID      : string;
                                 AItemsList   : TViewModelDataItemsList;
                                 AWetlandList : IWetlandList): Boolean;
function GetYMDemandCentreViewDataItems(AViewID      : string;
                                 AItemsList   : TViewModelDataItemsList;
                                 AYMDemandCentreList : IYMDemandCentreList): Boolean; //TODO: Must change to interface
function GetStreamFlowReductionsViewDataItems(AViewID      : string;
                                 AItemsList   : TViewModelDataItemsList;
                                 AStreamFlowReductionList : TStreamFlowReductionList): Boolean;
function GetMineViewDataItems(AViewID        : string;
                              AItemsList     : TViewModelDataItemsList;
                              AMineList      : TMineList): Boolean;
function GetChannelAreaViewDataItems(AViewID : string;
                              AItemsList     : TViewModelDataItemsList;
                              AChannelAreaList : TChannelAreaList): Boolean;
function GetDroughtRestrictionViewDataItems(AViewID : string;
                              AItemsList     : TViewModelDataItemsList;
                              ACurtailmentAndDrought: TCurtailmentAndDrought): Boolean;

function GetGroundWaterViewDataItems(AViewID : string; AItemsList : TViewModelDataItemsList;
                                     AGroundWaterList : TGroundWaterList): Boolean;
implementation

uses
  UErrorHandlingOperations;

function GetHydrologyFilesViewDataItems(AViewId: string; AItemsList:TViewModelDataItemsList;AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'UViewModelDataValidator.GetHyDrologyFilesViewDataItems';
Var
  LFileExt,
  LFileExtIn: string;
  LIndex: integer;
  LViewModelDataItem : TViewModelDataItem;
  LFileNameObject: TAbstractModelFileName;
Begin
  Result := False;
  try
    AItemsList.Reset;
    AViewId := Trim(AViewId);
    if (AViewId <> '') and Assigned(AItemsList) and Assigned(AFileNamesObject) then
    begin
      if(Length(AViewId) > 3) then
      begin
        LFileExtIn := '.' + UpperCase(Copy(AViewId,Length(AViewId)-2,3));
        for LIndex := 0 to AFileNamesObject.HydrologyFileNames.FilesCount - 1 do
        begin
           LFileNameObject := AFileNamesObject.HydrologyFileNames.FileNameObject[LIndex];
           if Assigned(LFileNameObject) and LFileNameObject.SavedInDB then
           begin
             LFileExt := UpperCase(ExtractFileExt(LFileNameObject.FileName));
             if(LFileExt = LFileExtIn) then
             begin
               LViewModelDataItem := AItemsList.AddViewModelDataItem;
               if Assigned(LViewModelDataItem) then
               begin
                 LViewModelDataItem.Weighting   := 10000;
                 LViewModelDataItem.Caption     := ExtractFileName(LFileNameObject.FileName);
                 LViewModelDataItem.ParamNames  := 'AFileName';
                 LViewModelDataItem.ParamValues := ExtractFileName(LFileNameObject.FileName);
               end;
             end;
           end;
        end;
        Result := True;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function GetDemandFilesViewDataItems(AViewId: string; AItemsList:TViewModelDataItemsList;AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'UViewModelDataValidator.GetDemandFilesViewDataItems';
Var
  LFileExt,
  LFileExtIn: string;
  LIndex: integer;
  LViewModelDataItem : TViewModelDataItem;
  LFileNameObject: TAbstractModelFileName;
Begin
  Result := False;
  try
    AItemsList.Reset;
    AViewId := Trim(AViewId);
    if (AViewId <> '') and Assigned(AItemsList) and Assigned(AFileNamesObject) then
    begin
      if(Length(AViewId) > 3) then
      begin
        LFileExtIn := '.' + UpperCase(Copy(AViewId,Length(AViewId)-2,3));
        for LIndex := 0 to AFileNamesObject.DemandFileNames.FilesCount - 1 do
        begin
           LFileNameObject := AFileNamesObject.DemandFileNames.FileNameObject[LIndex];
           if Assigned(LFileNameObject) and LFileNameObject.SavedInDB then
           begin
             LFileExt := UpperCase(ExtractFileExt(LFileNameObject.FileName));
             if(LFileExt = LFileExtIn) then
             begin
               LViewModelDataItem := AItemsList.AddViewModelDataItem;
               if Assigned(LViewModelDataItem) then
               begin
                 {LViewModelDataItem.Weighting   := LFileNameObject.FileNumber;
                 LViewModelDataItem.Caption     := IntToStr(LFileNameObject.FileNumber);
                 LViewModelDataItem.ParamNames  := 'AFileName';
                 LViewModelDataItem.ParamValues := IntToStr(LFileNameObject.FileNumber);
                 }
                 LViewModelDataItem.Weighting   := 10000;
                 LViewModelDataItem.Caption     := LFileNameObject.FileName;
                 LViewModelDataItem.ParamNames  := 'AFileNumber';
                 LViewModelDataItem.ParamValues := IntToStr(LFileNameObject.FileNumber);
               end;
             end;
           end;
        end;
        Result := True;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function GetReservoirViewDataItems(AViewId: string; AItemsList:TViewModelDataItemsList;AReservoirData: TReservoirDataList): boolean;
const OPNAME = 'UViewModelDataValidator.GetReservoirViewDataItems';
Var
  LIndex: integer;
  LViewModelDataItem : TViewModelDataItem;
  LReservoirData    : IReservoirData;
Begin
  Result := False;
  try
    AItemsList.Reset;
    AViewId := UpperCase(Trim(AViewId));
    if (Trim(AViewId) <> '') and Assigned(AItemsList) and Assigned(AReservoirData) then
    begin
      for LIndex := 0 to AReservoirData.ReservoirCount-1 do
      begin
        LReservoirData := AReservoirData.ReservoirByIndex[LIndex];
        if Assigned(LReservoirData) and (LReservoirData.ReservoirConfigurationData.NodeType = ntReservoir) then
        begin
          LViewModelDataItem := AItemsList.AddViewModelDataItem;
          if Assigned(LViewModelDataItem) then
          begin
            LViewModelDataItem.Weighting   := LReservoirData.ReservoirConfigurationData.ReservoirIdentifier;
            LViewModelDataItem.Caption     := Trim(LReservoirData.ReservoirConfigurationData.ReservoirName);
            LViewModelDataItem.ParamNames  := 'ANodeCount' ;
            LViewModelDataItem.ParamValues := IntToStr(LReservoirData.ReservoirConfigurationData.ReservoirIdentifier);
            LViewModelDataItem.DataType    := 'RESERVOIR';
          end;
        end;
      end;
      AItemsList.Sort;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function GetNodesWithInflowViewDataItems (AViewId    : string;
                                          AItemsList : TViewModelDataItemsList;
                                          ANodeData  : TReservoirDataList) : boolean;
const OPNAME = 'UViewModelDataValidator.GetNodesWithInflowViewDataItems';
Var
  LIndex             : integer;
  LViewModelDataItem : TViewModelDataItem;
  LNodeData          : IReservoirData;
Begin
  Result := False;
  try
    AItemsList.Reset;
    AViewId := UpperCase(Trim(AViewId));
    if (Trim(AViewId) <> '') and Assigned(AItemsList) and Assigned(ANodeData) then
    begin
      for LIndex := 0 to ANodeData.NodesWithInflowCount - 1 do
      begin
        LNodeData := ANodeData.NodeWithInflowByIndex[LIndex];
        if Assigned(LNodeData) and (LNodeData.ReservoirConfigurationData.NodeType = ntNodeWithInflow) then
        begin
          LViewModelDataItem := AItemsList.AddViewModelDataItem;
          if Assigned(LViewModelDataItem) then
          begin
            LViewModelDataItem.Weighting   := LNodeData.ReservoirConfigurationData.ReservoirIdentifier;
            LViewModelDataItem.Caption     := Trim(LNodeData.ReservoirConfigurationData.ReservoirName);
            LViewModelDataItem.ParamNames  := 'ANodeCount' ;
            LViewModelDataItem.ParamValues := IntToStr(LNodeData.ReservoirConfigurationData.ReservoirIdentifier);
            LViewModelDataItem.DataType    := 'NODEWITHINFLOW';
          end;
        end;
      end;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function GetNodesWithoutInflowViewDataItems (AViewId    : string;
                                             AItemsList : TViewModelDataItemsList;
                                             ANodeData  : TReservoirDataList) : boolean;
const OPNAME = 'UViewModelDataValidator.GetNodesWithoutInflowViewDataItems';
Var
  LIndex             : integer;
  LViewModelDataItem : TViewModelDataItem;
  LNodeData          : IReservoirData;
Begin
  Result := False;
  try
    AItemsList.Reset;
    AViewId := UpperCase(Trim(AViewId));
    if (Trim(AViewId) <> '') and Assigned(AItemsList) and Assigned(ANodeData) then
    begin
      for LIndex := 0 to ANodeData.NodesWithoutInflowCount - 1 do
      begin
        LNodeData := ANodeData.NodeWithoutInflowByIndex[LIndex];
        if Assigned(LNodeData) and (LNodeData.ReservoirConfigurationData.NodeType = ntNodeWithoutInflow) then
        begin
          LViewModelDataItem := AItemsList.AddViewModelDataItem;
          if Assigned(LViewModelDataItem) then
          begin
            LViewModelDataItem.Weighting   := LNodeData.ReservoirConfigurationData.ReservoirIdentifier;
            LViewModelDataItem.Caption     := Trim(LNodeData.ReservoirConfigurationData.ReservoirName);
            LViewModelDataItem.ParamNames  := 'ANodeCount' ;
            LViewModelDataItem.ParamValues := IntToStr(LNodeData.ReservoirConfigurationData.ReservoirIdentifier);
            LViewModelDataItem.DataType    := 'NODEWITHOUTINFLOW';
          end;
        end;
      end;
      {for LIndex := 0 to ANodeData.IrrigationNodesCount - 1 do
      begin
        LNodeData := ANodeData.IrrigationNodeByIndex[LIndex];
        if Assigned(LNodeData) then
        begin
          LViewModelDataItem := AItemsList.AddViewModelDataItem;
          if Assigned(LViewModelDataItem) then
          begin
            LViewModelDataItem.Weighting   := LNodeData.ReservoirConfigurationData.ReservoirIdentifier;
            LViewModelDataItem.Caption     := Trim(LNodeData.ReservoirConfigurationData.ReservoirName);
            LViewModelDataItem.ParamNames  := 'ANodeCount' ;
            LViewModelDataItem.ParamValues := IntToStr(LNodeData.ReservoirConfigurationData.ReservoirIdentifier);
            LViewModelDataItem.DataType    := 'IRRIGATIONNODE';
          end;
        end;
      end;}
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function GetChannelViewDataItems(AViewId: string; AItemsList:TViewModelDataItemsList;AChannelDataList: IChannelList): boolean;
const OPNAME = 'UViewModelDataValidator.GetChannelViewDataItems';
Var
  LIndex: integer;
  LViewModelDataItem : TViewModelDataItem;
  LChannelData       : IGeneralFlowChannel;
  LAdd               : boolean;
Begin
  Result := False;
  try
    AItemsList.Reset;
    AViewId := UpperCase(Trim(AViewId));
    if (Trim(AViewId) <> '') and Assigned(AItemsList) and Assigned(AChannelDataList) then
    begin
      for LIndex := 0 to AChannelDataList.ChannelCount-1 do
      begin
        try
          LChannelData := AChannelDataList.ChannelByIndex[LIndex];
          if Assigned(LChannelData)  then
          begin
            LAdd := False;
            if(AViewId = 'CHANNELNAMES18') then
            begin
              if(LChannelData.ChannelType = 08) and (LChannelData.IFRFeature <> nil) then
              begin
                LAdd := True;
              end;
            end
            else
            if(AViewId = 'CHANNELNAMES19') then
            begin
              if(LChannelData.ChannelType = 08) and (LChannelData.PhysicalFlowConstraint <> nil) then
              begin
                LAdd := True;
              end;
            end
            else
            if(AViewId = 'CHANNELNAMES8') then
            begin
              if(LChannelData.ChannelType = 08) and (LChannelData.IFRFeature = nil) and (LChannelData.PhysicalFlowConstraint = nil) then
              begin
                LAdd := True;
              end;
            end
            else
            if((LChannelData.ChannelType = 02) and (AViewId = 'CHANNELNAMES2')) or
              ((LChannelData.ChannelType = 05) and (AViewId = 'CHANNELNAMES5')) or
              ((LChannelData.ChannelType = 06) and (AViewId = 'CHANNELNAMES6')) or
              ((LChannelData.ChannelType = 07) and (AViewId = 'CHANNELNAMES7')) or
              ((LChannelData.ChannelType = 09) and (AViewId = 'CHANNELNAMES9')) or
              ((LChannelData.ChannelType = 10) and (AViewId = 'CHANNELNAMES10')) or
              ((LChannelData.ChannelType = 11) and (AViewId = 'CHANNELNAMES11')) or
              ((LChannelData.ChannelType = 12) and (AViewId = 'CHANNELNAMES12')) or
              ((LChannelData.ChannelType = 20) and (AViewId = 'CHANNELNAMES20')) then
  //            ((LChannelData.ChannelType = 19) and (AViewId = 'CHANNELNAMES19')) then
            begin
              LAdd := True;
            end;

            if LAdd then
            begin
              LViewModelDataItem := AItemsList.AddViewModelDataItem;
              if Assigned(LViewModelDataItem) then
              begin
                LViewModelDataItem.Weighting   := LChannelData.ChannelNumber;
                LViewModelDataItem.Caption     := Trim(LChannelData.ChannelName);
                LViewModelDataItem.ParamNames  :=  'AChannelNumber' ;
                LViewModelDataItem.ParamValues := IntToStr(LChannelData.ChannelNumber);
                LViewModelDataItem.DataType    := 'CHANNEL';
              end;
            end;
          end;
        finally
          LChannelData := nil;
        end;
      end;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function GetPowerPlantsViewDataItems (AViewId         : string;
                                      AItemsList      : TViewModelDataItemsList;
                                      APowerPlantList : IPowerPlantList): boolean;
const OPNAME = 'UViewModelDataValidator.GetPowerPlantsViewDataItems';
Var
  LIndex             : integer;
  LViewModelDataItem : TViewModelDataItem;
  LPowerPlant        : IPowerPlant;
Begin
  Result := False;
  try
    AItemsList.Reset;
    AViewId := UpperCase(Trim(AViewId));
    if (Trim(AViewId) <> '') and Assigned(AItemsList) and Assigned(APowerPlantList) then
    begin
      for LIndex := 0 to APowerPlantList.PowerPlantCount-1 do
      begin
        try
          LPowerPlant := APowerPlantList.PowerPlantByIndex[LIndex];
          if Assigned(LPowerPlant) then
          begin
            LViewModelDataItem := AItemsList.AddViewModelDataItem;
            if Assigned(LViewModelDataItem) then
            begin
              LViewModelDataItem.Caption     := Trim(LPowerPlant.FeatureName);
              LViewModelDataItem.Weighting   := LPowerPlant.FeatureID;
              LViewModelDataItem.ParamNames  :=  'Identifier' ;
              LViewModelDataItem.ParamValues := IntToStr(LPowerPlant.FeatureID);
              LViewModelDataItem.DataType    := 'POWERPLANT';
            end;
          end;
        finally
          LPowerPlant := nil;
        end;
      end;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function GetIrrigationAreasViewDataItems(AViewId             : string;
                                         AItemsList          : TViewModelDataItemsList;
                                         AIrrigationAreaList : IIrrigationAreaList): boolean;
const OPNAME = 'UViewModelDataValidator.GetIrrigationAreasViewDataItems';
Var
  LIndex             : integer;
  LViewModelDataItem : TViewModelDataItem;
  LIrrigationArea    : IIrrigationArea;
Begin
  Result := False;
  try
    AItemsList.Reset;
    AViewId := UpperCase(Trim(AViewId));
    if (Trim(AViewId) <> '') and Assigned(AItemsList) and Assigned(AIrrigationAreaList) then
    begin
      for LIndex := 0 to AIrrigationAreaList.IrrigationAreaCount-1 do
      begin
        try
          LIrrigationArea := AIrrigationAreaList.IrrigationAreaByIndex[LIndex];
          if Assigned(LIrrigationArea) then
          begin
            LViewModelDataItem := AItemsList.AddViewModelDataItem;
            if Assigned(LViewModelDataItem) then
            begin
              LViewModelDataItem.Caption     := Trim(LIrrigationArea.FeatureName);
              LViewModelDataItem.Weighting   := LIrrigationArea.IrrigationNodeNumber;
              LViewModelDataItem.ParamNames  :=  'Identifier' ;
              LViewModelDataItem.ParamValues := IntToStr(LIrrigationArea.IrrigationNodeNumber);
              LViewModelDataItem.DataType    := 'IRRIGATIONAREA';
            end;
          end;
        finally
          LIrrigationArea := nil;
        end;
      end;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function GetIrrigationBlockViewDataItems(AViewId              : string;
                                         AItemsList           : TViewModelDataItemsList;
                                         AIrrigationBlockList : IIrrigationBlockList): Boolean;
const OPNAME = 'UViewModelDataValidator.GetIrrigationBlockViewDataItems';
Var
  LIndex              : integer;
  LViewModelDataItem  : TViewModelDataItem;
  LIrrigationBlock    : IIrrigationBlock;
Begin
  Result := False;
  try
    AItemsList.Reset;
    AViewId := UpperCase(Trim(AViewId));
    if (Trim(AViewId) <> '') and Assigned(AItemsList) and Assigned(AIrrigationBlockList) then
    begin
      for LIndex := 0 to AIrrigationBlockList.IrrigationBlockCount-1 do
      begin
        try
          LIrrigationBlock := AIrrigationBlockList.IrrigationBlockByIndex[LIndex];
          if Assigned(LIrrigationBlock) then
          begin
            LViewModelDataItem := AItemsList.AddViewModelDataItem;
            if Assigned(LViewModelDataItem) then
            begin
              LViewModelDataItem.Caption     := Trim(LIrrigationBlock.BlockName);
              LViewModelDataItem.Weighting   := LIrrigationBlock.BlockNodeNumber;
              LViewModelDataItem.ParamNames  :=  'Identifier' ;
              LViewModelDataItem.ParamValues := IntToStr(LIrrigationBlock.Identifier);
              LViewModelDataItem.DataType    := 'IRRIGATIONBLOCK';
            end;
          end;
        finally
          LIrrigationBlock := nil;
        end;
      end;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;

end;                                         

function GetWetlandViewDataItems(AViewID      : string;
                                 AItemsList   : TViewModelDataItemsList;
                                 AWetlandList : IWetlandList): Boolean;
const OPNAME = 'UViewModelDataValidator.GetWetlandViewDataItems';
var
  LIndex              : Integer;
  LViewModelDataItem  : TViewModelDataItem;
  LWetland            : iWetland;
Begin
  Result := False;
  try
    AItemsList.Reset;
    AViewId := UpperCase(Trim(AViewId));
    if (Trim(AViewId) <> '') and Assigned(AItemsList) and Assigned(AWetlandList) then
    begin
      for LIndex := 0 to AWetlandList.WetLandCount-1 do
      begin
        try
          LWetland := AWetlandList.WetlandByIndex[LIndex];
          if Assigned(LWetland) then
          begin
            LViewModelDataItem := AItemsList.AddViewModelDataItem;
            if Assigned(LViewModelDataItem) then
            begin
              LViewModelDataItem.Caption     := Trim(LWetland.Name);
              LViewModelDataItem.Weighting   := LWetland.NodeNumber;
              LViewModelDataItem.ParamNames  :=  'Identifier' ;
              LViewModelDataItem.ParamValues := IntToStr(LWetland.Identifier);
              LViewModelDataItem.DataType    := 'WETLAND';
            end;
          end;
        finally
          LWetland := nil;
        end;
      end;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function GetYMDemandCentreViewDataItems(AViewID      : string;
                                 AItemsList   : TViewModelDataItemsList;
                                 AYMDemandCentreList : IYMDemandCentreList): Boolean; //TODO: Must change to interface
const OPNAME = 'UViewModelDataValidator.GetYMDemandCentreViewDataItems';
var
  LIndex              : Integer;
  LViewModelDataItem  : TViewModelDataItem;
  LYMDemandCentre     : IYMDemandCentre;
Begin
  Result := False;
  try
    AItemsList.Reset;
    AViewId := UpperCase(Trim(AViewId));
    if (Trim(AViewId) <> '') and Assigned(AItemsList) and Assigned(AYMDemandCentreList) then
    begin
      for LIndex := 0 to AYMDemandCentreList.YMDemandCentreCount-1 do
      begin
        try
          LYMDemandCentre := AYMDemandCentreList.YMDemandCentreByIndex[LIndex];
          if Assigned(LYMDemandCentre) then
          begin
            LViewModelDataItem := AItemsList.AddViewModelDataItem;
            if Assigned(LViewModelDataItem) then
            begin
              LViewModelDataItem.Caption     := Trim(LYMDemandCentre.Name);
              LViewModelDataItem.Weighting   := LYMDemandCentre.NodeNumber;
              LViewModelDataItem.ParamNames  :=  'Identifier' ;
              LViewModelDataItem.ParamValues := IntToStr(LYMDemandCentre.Identifier);
              LViewModelDataItem.DataType    := 'YMDEMANDCENTRE';
            end;
          end;
        finally
          //TODO:LYMDemandCentre := nil;
        end;
      end;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function GetStreamFlowReductionsViewDataItems(AViewID      : string;AItemsList   : TViewModelDataItemsList;
         AStreamFlowReductionList : TStreamFlowReductionList): Boolean;
const OPNAME = 'UViewModelDataValidator.GetStreamFlowReductionsViewDataItems';
var
  LIndex              : Integer;
  LViewModelDataItem  : TViewModelDataItem;
  LStreamFlowReduction            : IStreamFlowReduction;
Begin
  Result := False;
  try
    AItemsList.Reset;
    AViewId := UpperCase(Trim(AViewId));
    if (Trim(AViewId) <> '') and Assigned(AItemsList) and Assigned(AStreamFlowReductionList) then
    begin
      for LIndex := 0 to AStreamFlowReductionList.StreamFlowReductionCount-1 do
      begin
        LStreamFlowReduction := AStreamFlowReductionList.StreamFlowReductionByIndex[LIndex];
        if Assigned(LStreamFlowReduction) then
        begin
          LViewModelDataItem := AItemsList.AddViewModelDataItem;
          if Assigned(LViewModelDataItem) then
          begin
            LViewModelDataItem.Caption     := Trim(LStreamFlowReduction.SFRName);
            LViewModelDataItem.Weighting   := LStreamFlowReduction.Identifier;
            LViewModelDataItem.ParamNames  :=  'Identifier' ;
            LViewModelDataItem.ParamValues := IntToStr(LStreamFlowReduction.Identifier);
            LViewModelDataItem.DataType    := 'STREAMFLOWREDUCTION';
          end;
        end;
      end;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function GetMineViewDataItems(AViewID : string; AItemsList   : TViewModelDataItemsList; AMineList    : TMineList): Boolean;
const OPNAME = 'GetMineViewDataItems';
Var
  //LCount,
  LIndex: integer;
  LViewModelDataItem : TViewModelDataItem;
  LMine              : TMine;
  //LUnderground       : IUnderground;
  //LReservoir         : IReservoirData;
Begin
  Result := False;
  try
    AItemsList.Reset;
    AViewId := UpperCase(Trim(AViewId));
    if (Trim(AViewId) <> '') and Assigned(AItemsList) and Assigned(AMineList) then
    begin
      for LIndex := 0 to AMineList.MineCount-1 do
      begin
        LMine := AMineList.CastMineByIndex[LIndex];
        if Assigned(LMine)  then
        begin
          LViewModelDataItem := AItemsList.AddViewModelDataItem;
          if Assigned(LViewModelDataItem) then
          begin
            LViewModelDataItem.Weighting   := LMine.NodeNumber;
            LViewModelDataItem.Caption     := Trim(LMine.MineName);
            LViewModelDataItem.ParamNames  := 'Identifier' ;
            LViewModelDataItem.ParamValues := IntToStr(LMine.Identifier);
            LViewModelDataItem.DataType    := 'MINE';
          end;
        end;
      end;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function GetChannelAreaViewDataItems(AViewID : string; AItemsList : TViewModelDataItemsList;
                                     AChannelAreaList : TChannelAreaList): Boolean;
const OPNAME = 'UViewModelDataValidator.GetChannelAreaViewDataItems';
Var
  LIndex             : integer;
  LViewModelDataItem : TViewModelDataItem;
  LChannelArea       : TChannelArea;
Begin
  Result := False;
  try
    AItemsList.Reset;
    AViewId := UpperCase(Trim(AViewId));
    if (Trim(AViewId) <> '') and Assigned(AItemsList) and Assigned(AChannelAreaList) then
    begin
      for LIndex := 0 to AChannelAreaList.AreaCount-1 do
      begin
        LChannelArea := AChannelAreaList.CastChannelAreaByIndex[LIndex];
        if Assigned(LChannelArea)  then
        begin
          LViewModelDataItem := AItemsList.AddViewModelDataItem;
          if Assigned(LViewModelDataItem) then
          begin
            LViewModelDataItem.Weighting   := LChannelArea.AreaID;
            LViewModelDataItem.Caption     := Trim(LChannelArea.AreaName);
            LViewModelDataItem.ParamNames  := 'AreaID' ;
            LViewModelDataItem.ParamValues := IntToStr(LChannelArea.AreaID);
            LViewModelDataItem.DataType    := 'CHANNELAREA';
          end;
        end;
      end;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function GetGroundWaterViewDataItems(AViewID : string; AItemsList : TViewModelDataItemsList;
                                            AGroundWaterList : TGroundWaterList): Boolean;
const OPNAME = 'UViewModelDataValidator.GetGroundWaterViewDataItems';
Var
  LIndex              : integer;
  LViewModelDataItem  : TViewModelDataItem;
  LGroundWater        : TGroundWater;
Begin
  Result := False;
  try
    AItemsList.Reset;
    AViewId := UpperCase(Trim(AViewId));
    if (Trim(AViewId) <> '') and Assigned(AItemsList) and Assigned(AGroundWaterList) then
    begin
      for LIndex := 0 to AGroundWaterList.GroundWaterCount-1 do
      begin
        LGroundWater := AGroundWaterList.CastGroundWaterByIndex[LIndex];
        if Assigned(LGroundWater)  then
        begin
          LViewModelDataItem := AItemsList.AddViewModelDataItem;
          if Assigned(LViewModelDataItem) then
          begin
            LViewModelDataItem.Weighting   := LGroundWater.Identifier;
            LViewModelDataItem.Caption     := Trim(LGroundWater.Name);
            LViewModelDataItem.ParamNames  := 'Identifier' ;
            LViewModelDataItem.ParamValues := IntToStr(LGroundWater.Identifier);
            LViewModelDataItem.DataType    := 'GROUNDWATER';
          end;
        end;
      end;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function GetDroughtRestrictionViewDataItems(AViewID : string; AItemsList : TViewModelDataItemsList;
                                            ACurtailmentAndDrought : TCurtailmentAndDrought): Boolean;
const OPNAME = 'UViewModelDataValidator.GetDroughtRestrictionViewDataItems';
Var
  LIndex              : integer;
  LViewModelDataItem  : TViewModelDataItem;
  LDroughtRestriction : TDroughtRestriction;
Begin
  Result := False;
  try
    AItemsList.Reset;
    AViewId := UpperCase(Trim(AViewId));
    if (Trim(AViewId) <> '') and Assigned(AItemsList) and Assigned(ACurtailmentAndDrought) then
    begin
      for LIndex := 0 to ACurtailmentAndDrought.DroughtRestrictionCount-1 do
      begin
        LDroughtRestriction := ACurtailmentAndDrought.CastDroughtRestrictionByIndex[LIndex];
        if Assigned(LDroughtRestriction)  then
        begin
          LViewModelDataItem := AItemsList.AddViewModelDataItem;
          if Assigned(LViewModelDataItem) then
          begin
            LViewModelDataItem.Weighting   := LDroughtRestriction.Identifier;
            LViewModelDataItem.Caption     := Trim(LDroughtRestriction.DroughtRestrictionName);
            LViewModelDataItem.ParamNames  := 'Identifier' ;
            LViewModelDataItem.ParamValues := IntToStr(LDroughtRestriction.Identifier);
            LViewModelDataItem.DataType    := 'DROUGHTRESTRICTIONS';
          end;
        end;
      end;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;




end.
