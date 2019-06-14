//
//
//  UNIT      : Contains TPlanningFileDataObjects Class
//  AUTHOR    : Dziedzi Ramulondi(Aravia)
//  DATE      : 02/04/2003
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UPlanningFileDataObjects;

interface

uses
  Classes, sysutils,contnrs,

  //  DWAF VCL
  UBasicObjects,
  UAbstractDataObject,
  UFileNames,
  UChannelDescriptionObject,
  UChannelSwitchControlFileDataObjects,
  UAllocationDefinitionFileDataObjects,
  UReservoirImplementationFileDataObjects,
  UPumpingChannelControlFileDataObjects,
  UGeneralChannelControlFileDataObjects,
  UGrowthFactorFileObject,
  UDisbenefitFileDataObjects,
  UReturnFlowChannelFileDataObjects,
  UHydroPowerAllocationControlFileDataObject,
  UAllocationChannelControlFileDataObject,
  UControlledReleaseStructureFileDataObject,
  UTariffCalculationFileDataObject,
  UMonthlyWaterRequirementFileDataObjects,
  UReclamationPlantFileDataObjects,
  UCurtailObject,
  USaltWashOffObject,
  UMIMMFileObject,
  UWRPMPostProcessorData,
  UDataFileObjects,
  VoaimsCom_TLB,
  UAbstractObject;

type

  TPlanningFileDataObjects = class(TDataFileObjects)
  protected
    //File SW*.dat
    FChannelSwitchControlFiles : TObjectList;
    //File FM*.dat
    FAllocationDefinitionFiles : TObjectList;
    //File DAM.dat
    FReservoirAndFilesImplementation   : TReservoirAndFilesImplementationObject;
    //File PMP.dat
    FPumpingChannelControlFileData     : TPumpingChannelControlFileDataObject;
    //File PUR.dat
    FGeneralChannelControlFileData     : TGeneralChannelControlFileDataObject;
    // Annual Growth Factors (GTH.dat)
    FGrowthFactorFileObject : TGrowthFactorFileObject;    
    // Disbenefit (DBF.dat)
    FDisbenefitFileDataObject : TDisbenefitFileDataObject;
    // Return Flow Channel (RET.dat)
    FReturnFlowChannelFileObject : TReturnFlowChannelFileObject;
    // Allocation Channel Control (ALO.dat)
    FAllocationChannelObject : TAllocationChannelControlFileDataObject;
    // Hydrology Allocation (HYD.dat)
    FHydroAllocationDataObject : THydroPowerAllocationControlFileDataObject;
    // AControlled Release Structure (Rel.dat)
    FControlReleaseStructure : TControlledReleaseStructureFileDataObject;
    // Tarrif calculations (Tar.dat)
    FTariffCalculationData : TTariffCalculationFileDataObject;
    // Monthly Water Requirement (Hst.dat)
    FMonthlyWaterRequirementFileDataObjects:TMonthlyWaterRequirementFileDataObjects;
    // ReclamationPlant Control (Rec.dat)
    FReclamationPlantFileDataObjects:TReclamationPlantFileDataObjects;
    // Multi Reservoir Multi Channel Restriction File...(cur.dat)
    FCurtailDataObject : TCurtailDataObject;
    // Salt Wash Off File (MISW...Dat)
    FSaltWashOffObject : TObjectList;
  // Mine ( MIMM.dat)
    FMIMMFileObjectList : TObjectList;
   // FMIMMSubCatchmentListFileObject : TMIMMSubCatchmentListFileObject;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetChannelSwitchControlFileDataObjectByIndex(AIndex: integer):TChannelSwitchControlFileDataObject;
    function GetChannelSwitchControlFileDataObjectByFileNumber(AFileNumber: integer):TChannelSwitchControlFileDataObject;
    function GetAllocationDefinitionFileDataObjectByIndex(AIndex: integer):TAllocationDefinitionFileDataObject;
    function GetAllocationDefinitionFileDataObjectByFileNumber(AFileNumber: integer):TAllocationDefinitionFileDataObject;
    function GetMIMMListFileObjectByFileNumber(AFileNumber: integer): TMIMMListFileObject;
    function GetMIMMListFileObjectByIndex(AIndex: integer): TMIMMListFileObject;
    function GetSaltWashOffObjectByFileNumber(AFileNumber: integer): TSaltWashOffObject;
    function GetSaltWashOffObjectByIndex(AIndex: integer): TSaltWashOffObject;


    function ValidateDisbenefitFunctionChannel(AMasterControlChannelList : TStringList;AAppModules:TAppModules;AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;
    function ValidatePumpingChannelControl(APumpingChannelList : TStringList;AAppModules:TAppModules;AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;
    function ValidatePurificationChannelControl(AMasterControlChannelList,APumpingChannelList : TStringList;AAppModules:TAppModules;AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;


  public
    function Initialise: boolean;override;
    procedure Reset;override;
    procedure UpdateChannelSwitchStartDates;
    procedure AddPlanningNodeWitoutInflow;
    procedure UpdatePlanningNetworkElementsType;
    function ValidateFileData(AAppModules:TAppModules;AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;override;
    function AddChannelSwitchControlFileDataObject(AFileNumber: integer):TChannelSwitchControlFileDataObject;
    function AddAllocationDefinitionFileDataObject(AFileNumber: integer):TAllocationDefinitionFileDataObject;

    function AddMIMMFileObject(AFileNumber: integer):TMIMMListFileObject;
    function AddSaltWashOffObject(AFileNumber: integer):TSaltWashOffObject;

    property ChannelSwitchControlFileDataObjectByIndex[AIndex: integer]: TChannelSwitchControlFileDataObject read GetChannelSwitchControlFileDataObjectByIndex;
    property ChannelSwitchControlFileDataObjectByFileNumber[AFileNumber: integer]: TChannelSwitchControlFileDataObject read GetChannelSwitchControlFileDataObjectByFileNumber;
    property AllocationDefinitionFileDataObjectByIndex[AIndex: integer]: TAllocationDefinitionFileDataObject read GetAllocationDefinitionFileDataObjectByIndex;
    property AllocationDefinitionFileDataObjectByFileNumber[AFileNumber: integer]: TAllocationDefinitionFileDataObject read GetAllocationDefinitionFileDataObjectByFileNumber;
    property ReservoirAndFilesImplementation : TReservoirAndFilesImplementationObject read FReservoirAndFilesImplementation;
    property PumpingChannelControlFileData   : TPumpingChannelControlFileDataObject read FPumpingChannelControlFileData;
    property GeneralChannelControlFileData   : TGeneralChannelControlFileDataObject read FGeneralChannelControlFileData;
    property GrowthFactorFileObject          : TGrowthFactorFileObject              read FGrowthFactorFileObject;
    property DisbenefitFileDataObject        : TDisbenefitFileDataObject            read FDisbenefitFileDataObject;
    property ReturnFlowChannelFileObject     : TReturnFlowChannelFileObject         read FReturnFlowChannelFileObject;
    property HydroAllocationDataObject       : THydroPowerAllocationControlFileDataObject read FHydroAllocationDataObject;
    property AllocationChannelObject         : TAllocationChannelControlFileDataObject    read FAllocationChannelObject;
    property ControlReleaseStructure         : TControlledReleaseStructureFileDataObject  read FControlReleaseStructure;
    property TariffCalculationData           : TTariffCalculationFileDataObject           read FTariffCalculationData;
    property MonthlyWaterRequirementFileDataObjects:TMonthlyWaterRequirementFileDataObjects read FMonthlyWaterRequirementFileDataObjects;
    property ReclamationPlantFileDataObjects:TReclamationPlantFileDataObjects read FReclamationPlantFileDataObjects;
    property CurtailDataObject              : TCurtailDataObject              read FCurtailDataObject;
    property MIMMFileObjectByFileNumber[AFileNumber: integer]   : TMIMMListFileObject             read GetMIMMListFileObjectByFileNumber;
    property MIMMFileObjectByIndex[AIndex: integer]        : TMIMMListFileObject                  read GetMIMMListFileObjectByIndex;
    property SaltWashOffObjectByFileNumber[AFileNumber: integer]: TSaltWashOffObject        read GetSaltWashOffObjectByFileNumber;
    property SaltWashOffObjectByIndex[AIndex: integer]          : TSaltWashOffObject        read GetSaltWashOffObjectByIndex;

  end;
implementation


{ TPlanningFileDataObjects }
uses
  UMineFileObject,
  UReservoirObject,
  UErrorHandlingOperations;

procedure TPlanningFileDataObjects.CreateMemberObjects;
const OPNAME = 'TPlanningFileDataObjects.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    //File SW*.dat
    FChannelSwitchControlFiles := TObjectList.Create(True);
    //File FM*.dat
    FAllocationDefinitionFiles := TObjectList.Create(True);
    //File DAM.dat
    FReservoirAndFilesImplementation := TReservoirAndFilesImplementationObject.Create;
    //File PMP.dat
    FPumpingChannelControlFileData   := TPumpingChannelControlFileDataObject.Create;
    //File PMP.dat
    FGeneralChannelControlFileData   := TGeneralChannelControlFileDataObject.Create;
    // Annual Growth Factors (GTH.dat)
    FGrowthFactorFileObject := TGrowthFactorFileObject.Create;
    // Disbenefit (DBF.dat)
    FDisbenefitFileDataObject := TDisbenefitFileDataObject.Create;
    // Return Flow Channel (RET.dat)
    FReturnFlowChannelFileObject := TReturnFlowChannelFileObject.Create;
    // Allocation Channel Control (ALO.dat)
    FAllocationChannelObject := TAllocationChannelControlFileDataObject.Create;
    // Hydrology Allocation (HYD.dat)
    FHydroAllocationDataObject := THydroPowerAllocationControlFileDataObject.Create;
    // AControlled Release Structure (Rel.dat)
    FControlReleaseStructure := TControlledReleaseStructureFileDataObject.Create;
    // Tarrif calculations (Tar.dat)
    FTariffCalculationData := TTariffCalculationFileDataObject.Create;
    // Monthly Water Requirement (Hst.dat)
    FMonthlyWaterRequirementFileDataObjects := TMonthlyWaterRequirementFileDataObjects.Create;
    // ReclamationPlant Control (Rec.dat)
    FReclamationPlantFileDataObjects := TReclamationPlantFileDataObjects.Create;
    // Multi-Reservoir Restriction (Cur.dat)
    FCurtailDataObject := TCurtailDataObject.Create;
    // Salt Wash Off (MISW...dat)
    FSaltWashOffObject := TObjectList.Create(True);
    // Mine (MIMM.dat)
    FMIMMFileObjectList := TObjectList.Create(True);
  //  FMIMMSubCatchmentListFileObject := TMIMMSubCatchmentListFileObject.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningFileDataObjects.DestroyMemberObjects;
const OPNAME = 'TPlanningFileDataObjects.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    //File SW*.dat
    FChannelSwitchControlFiles.Clear;
    FChannelSwitchControlFiles.Free;
    //File FM*.dat
    FAllocationDefinitionFiles.Clear;
    FAllocationDefinitionFiles.Free;
    //File DAM.dat
    FReservoirAndFilesImplementation.Free;
    //File PMP.dat
    FPumpingChannelControlFileData.Free;
    //File PUR.dat
    FGeneralChannelControlFileData.Free;
    // Annual Growth Factors (GTH.dat)
    FGrowthFactorFileObject.Free;
    // Disbenefit (DBF.dat)
    FDisbenefitFileDataObject.Free;
    // Return Flow Channel (RET.dat)
    FReturnFlowChannelFileObject.Free;
    // Allocation Channel Control (ALO.dat)
    FAllocationChannelObject.Free;
    // Hydrology Allocation (HYD.dat)
    FHydroAllocationDataObject.Free;
    // AControlled Release Structure (Rel.dat)
    FControlReleaseStructure.Free;
    // Tarrif calculations (Tar.dat)
    FTariffCalculationData.Free;
    // Monthly Water Requirement (Hst.dat)
    FreeAndNil(FMonthlyWaterRequirementFileDataObjects);
    // ReclamationPlant Control (Rec.dat)
    FreeAndNil(FReclamationPlantFileDataObjects);
    //FreeAndNil(FCurtailDataObject)

    FSaltWashOffObject.Clear;
    FSaltWashOffObject.Free;

    // Mine

     FMIMMFileObjectList.Clear;
     FMIMMFileObjectList.Free;

    // FreeAndNil(FMIMMSubCatchmentListFileObject);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningFileDataObjects.Initialise: boolean;
const OPNAME = 'TPlanningFileDataObjects.Initialise';
begin
  Result := inherited Initialise;
  try
    //File SW*.dat
    FChannelSwitchControlFiles.Clear;
    //File FM*.dat
    FAllocationDefinitionFiles.Clear;
    //File DAM.dat
    FReservoirAndFilesImplementation.Initialise;
    //File PMP.dat
    FPumpingChannelControlFileData.Initialise;
    //File PUR.dat
    FGeneralChannelControlFileData.Initialise;
    // Annual Growth Factors (GTH.dat)
    FGrowthFactorFileObject.Initialise;
    // Disbenefit (DBF.dat)
    FDisbenefitFileDataObject.Initialise;
    // Return Flow Channel (RET.dat)
    FReturnFlowChannelFileObject.Initialise;
    // Allocation Channel Control (ALO.dat)
    FAllocationChannelObject.Initialise;
    // Hydrology Allocation (HYD.dat)
    FHydroAllocationDataObject.Initialise;
    // AControlled Release Structure (Rel.dat)
    FControlReleaseStructure.Initialise;
    // Tarrif calculations (Tar.dat)
    FTariffCalculationData.Initialise;
    // Monthly Water Requirement (Hst.dat)
    FMonthlyWaterRequirementFileDataObjects.Initialise;
    // ReclamationPlant Control (Rec.dat)
    FReclamationPlantFileDataObjects.Initialise;
    // Multi-Reservoir Restriction (Cur.dat)
    FCurtailDataObject.Initialise;
    // Salt Wash Off (MISW...dat)
    FSaltWashOffObject.Clear;
    // Mine
    FMIMMFileObjectList.Clear;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningFileDataObjects.Reset;
const OPNAME = 'TPlanningFileDataObjects.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningFileDataObjects.UpdateChannelSwitchStartDates;
const OPNAME = 'TPlanningFileDataObjects.Reset';
var
  LIndex                 : integer;
  LFileName              : string;
  LChannelSwitch         : TChannelSwitchFileObject;
  LSwitchChannelFile     : TChannelSwitchControlFileDataObject;
begin
  try
    for LIndex := 0 to FChannelSwitchControlFiles.Count-1 do
    begin
      LSwitchChannelFile  := TChannelSwitchControlFileDataObject(FChannelSwitchControlFiles[LIndex]);
      LFileName           := LSwitchChannelFile.SwitchDefFileName.FData;
      LChannelSwitch      := FReservoirAndFilesImplementation.GetChannelSwitchByFileName(LFileName);
      if(LChannelSwitch <> nil) then
      begin
        LSwitchChannelFile.SwitchDefStartYear.FData       := LChannelSwitch.YearFileActive.FData;
        LSwitchChannelFile.SwitchDefStartYear.FInitalised := True;
        LSwitchChannelFile.SwitchDefStartMonth.FData       := LChannelSwitch.MonthFileActive.FData;
        LSwitchChannelFile.SwitchDefStartMonth.FInitalised := True;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningFileDataObjects.ValidateDisbenefitFunctionChannel(AMasterControlChannelList : TStringList;AAppModules: TAppModules; AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TPlanningFileDataObjects.ValidateDisbenefitFunctionChannel';
var
  LIndex                    : integer;
  LChannelNumber            : string;
  LMessage                  : string;
  LStop                     : boolean;
  LDisbenefit               : TDisbenefitFileObject;
begin
  Result := False;
  try
    for LIndex := 0 to FDisbenefitFileDataObject.DisbenefitCount -1 do
    begin
      LDisbenefit := FDisbenefitFileDataObject.DisbenefitByIndex[LIndex];
      if LDisbenefit.ChannelNumber.FInitalised and (LDisbenefit.ChannelNumber.FData > 0) then
      begin
        LChannelNumber := IntToStr(LDisbenefit.ChannelNumber.FData);
        if(AMasterControlChannelList.IndexOf(LChannelNumber) < 0) then
        begin
          LMessage := AAppModules.Language.GetString('TDataFileObjects.strDBFChannelNotMasterControl');
          LMessage := Format(LMessage,[LChannelNumber]);
          if Assigned(AProgressUpdateFuntion) then
            AProgressUpdateFuntion(LMessage,ptError,LStop);
        end;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningFileDataObjects.ValidatePumpingChannelControl(APumpingChannelList : TStringList;AAppModules: TAppModules; AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TPlanningFileDataObjects.ValidatePumpingChannelControl';
var
  LIndex                    : integer;
  LChannelNumber            : string;
  LMessage                  : string;
  LStop                     : boolean;
  LPumpingChannel           : TPumpingChannelControlObject;
begin
  Result := False;
  try
    for LIndex := 0 to FPumpingChannelControlFileData.PumpingChannelControlCount -1 do
    begin
      LPumpingChannel := FPumpingChannelControlFileData.PumpingChannelControlByIndex[LIndex];
      if LPumpingChannel.ChannelNumber.FInitalised and (LPumpingChannel.ChannelNumber.FData > 0)then
      begin
        LChannelNumber := IntToStr(LPumpingChannel.ChannelNumber.FData);
        if(APumpingChannelList.IndexOf(LChannelNumber) < 0) then
        begin
          LMessage := AAppModules.Language.GetString('TDataFileObjects.strPMPChannelNotPumping');
          LMessage := Format(LMessage,[LChannelNumber]);
          if Assigned(AProgressUpdateFuntion) then
            AProgressUpdateFuntion(LMessage,ptError,LStop);
        end;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningFileDataObjects.ValidatePurificationChannelControl(AMasterControlChannelList,APumpingChannelList : TStringList;AAppModules: TAppModules; AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TPlanningFileDataObjects.ValidatePurificationChannelControl';
var
  LIndex                    : integer;
  LChannelNumber            : string;
  LMessage                  : string;
  LStop                     : boolean;
  LGeneralChannelControl    : TGeneralChannelControlObject;
begin
  Result := False;
  try
    for LIndex := 0 to FGeneralChannelControlFileData.GeneralChannelControlCount -1 do
    begin
      LGeneralChannelControl := FGeneralChannelControlFileData.GeneralChannelControlByIndex[LIndex];
      if LGeneralChannelControl.ChannelNumber.FInitalised and (LGeneralChannelControl.ChannelNumber.FData > 0)then
      begin
        LChannelNumber := IntToStr(LGeneralChannelControl.ChannelNumber.FData);
        if(AMasterControlChannelList.IndexOf(LChannelNumber) >= 0) then
        begin
          LMessage := AAppModules.Language.GetString('TDataFileObjects.strPURChannelMasterControl');
          LMessage := Format(LMessage,[LChannelNumber]);
          if Assigned(AProgressUpdateFuntion) then
            AProgressUpdateFuntion(LMessage,ptError,LStop);
        end;

        if(APumpingChannelList.IndexOf(LChannelNumber) >= 0) then
        begin
          LMessage := AAppModules.Language.GetString('TDataFileObjects.strPURChannelNotPumping');
          LMessage := Format(LMessage,[LChannelNumber]);
          if Assigned(AProgressUpdateFuntion) then
            AProgressUpdateFuntion(LMessage,ptError,LStop);
        end;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningFileDataObjects.ValidateFileData(AAppModules: TAppModules;  AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TPlanningFileDataObjects.ValidateFileData';
var
  LIndex                    : integer;
  LChannelNumber            : integer;
  LMasterControlChannelList : TStringList;
  LPumpingChannelList       : TStringList;
begin
  Result := inherited ValidateFileData(AAppModules,AProgressUpdateFuntion);
  try
    LMasterControlChannelList := TStringList.Create;
    LPumpingChannelList       := TStringList.Create;
    try
      for LIndex := 0 to FChannelDescrObject.FMasterChannelList.Count -1 do
      begin
        LChannelNumber := TMasterChannelObject(FChannelDescrObject.FMasterChannelList[LIndex]).FChannelNumber.FData;
        LMasterControlChannelList.AddObject(IntToStr(LChannelNumber),FChannelDescrObject.FMasterChannelList[LIndex]);
      end;

      for LIndex := 0 to FChannelDescrObject.FPumpingChannelList.Count -1 do
      begin
        LChannelNumber := TPumpingChannelObject(FChannelDescrObject.FPumpingChannelList[LIndex]).FChannelNumber.FData;
        LPumpingChannelList.AddObject(IntToStr(LChannelNumber),FChannelDescrObject.FPumpingChannelList[LIndex]);
      end;

      ValidateDisbenefitFunctionChannel(LMasterControlChannelList,AAppModules,AProgressUpdateFuntion);
      ValidatePumpingChannelControl(LPumpingChannelList,AAppModules,AProgressUpdateFuntion);
      ValidatePurificationChannelControl(LMasterControlChannelList,LPumpingChannelList,AAppModules,AProgressUpdateFuntion);

      Result := True;
    finally
      LMasterControlChannelList.Free;
      LPumpingChannelList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningFileDataObjects.AddAllocationDefinitionFileDataObject(AFileNumber: integer): TAllocationDefinitionFileDataObject;
const OPNAME = 'TPlanningFileDataObjects.AddAllocationDefinitionFileDataObject';
begin
  Result  := AllocationDefinitionFileDataObjectByFileNumber[AFileNumber];
  try
    if(Result = nil) then
    begin
      Result  := TAllocationDefinitionFileDataObject.Create(AFileNumber);
      FAllocationDefinitionFiles.Add(Result);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TPlanningFileDataObjects.AddMIMMFileObject(AFileNumber: integer): TMIMMListFileObject;
const OPNAME = 'TPlanningFileDataObjects.AddMIMMFileObject';
begin
  Result  := MIMMFileObjectByFileNumber[AFileNumber];
  try
    if(Result = nil) then
    begin
      Result  := TMIMMListFileObject.Create(AFileNumber);
      FMIMMFileObjectList.Add(Result);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TPlanningFileDataObjects.AddPlanningNodeWitoutInflow;
const OPNAME = 'TPlanningFileDataObjects.AddPlanningNodeWitoutInflow';
var
  LIndex,
  LCount : integer;
  LMineList  :  TMIMMListFileObject;
  LMine :  TMIMMFileObject;
begin
  try

    for LCount := 0 to FMIMMFileObjectList.Count-1 do
    begin
      LMineList  := MIMMFileObjectByFileNumber[LCount+1];
      if LMineList <> nil then
      begin
      for LIndex := 0 to LMineList.MineFileObjectCount-1 do
      begin
        LMine := TMIMMFileObject(LMineList.MineFileObjectByIndex[LIndex]);
        if LMine <> nil then
          if LMine.NodeNumber.FInitalised then
            FReservoirObject.AddNodeWithoutInflow(LMine.NodeNumber.FData,ntMineNode);
        end;
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningFileDataObjects.UpdatePlanningNetworkElementsType;
const OPNAME = 'TPlanningFileDataObjects.UpdatePlanningNetworkElementsType';
var
  LCount, LUGIndex,
  LIndex: integer;
  LMineList  :  TMIMMListFileObject;
  LMine :  TMIMMFileObject;
  LReservoir : TReservoir;
  LUndegroundMine : TMIMMUndegroundFileObject;
  LMinMaxChannel  : TMultiPurposeChannelObject;
begin
  try
    for LCount := 0 to FMIMMFileObjectList.Count-1 do
    begin

      LMineList  := MIMMFileObjectByFileNumber[LCount+1];
      if LMineList <> nil then
      begin
        for LIndex := 0 to LMineList.MineFileObjectCount-1 do
        begin
          LMine := TMIMMFileObject(LMineList.MineFileObjectByIndex[LIndex]);

          if(LMine <> nil) and (LMine.PCDChannelNumber.FInitalised) then
          begin
            LMinMaxChannel := FChannelDescrObject.FindMultiPurposeChannel(LMine.PCDChannelNumber.FData);
            if(LMinMaxChannel <> nil) then
            begin
              LMinMaxChannel.FChannelType := ctMineToPCDChannel;
              if(LMinMaxChannel.FDownNodeNumber.FInitalised) then
              begin
                LReservoir := FReservoirObject.ReservoirByReservoirNumber(LMinMaxChannel.FDownNodeNumber.FData);
                if(LReservoir <> nil) and (LReservoir.FNodeType.FData = ntReservoir) then
                begin
                  LReservoir.FNodeType.FData := ntMinePolutionControlDam;
                  LReservoir.FNodeType.FInitalised := True;
                end;
              end;
            end;

            LMinMaxChannel := FChannelDescrObject.FindMultiPurposeChannel(LMine.RiverChannelNumber.FData);
            if(LMinMaxChannel <> nil) then
            begin
              LMinMaxChannel.FChannelType := ctMineToRiverDChannel;
            end;
          end;



          for LUGIndex := 0 to LMine.UndegroundCount-1 do
          begin
            LUndegroundMine := TMIMMUndegroundFileObject(LMine.UndegroundByIndex[LUGIndex]);
            if(LUndegroundMine <> nil) then
            begin
              LMinMaxChannel := FChannelDescrObject.FindMultiPurposeChannel(LUndegroundMine.ChannelNumberToUGDam.FData);
              if(LMinMaxChannel <> nil) then
              begin
                LMinMaxChannel.FChannelType := ctMineToUndergroundChannel;

                if(LMinMaxChannel.FDownNodeNumber.FInitalised) then
                begin
                  LReservoir := FReservoirObject.ReservoirByReservoirNumber(LMinMaxChannel.FDownNodeNumber.FData);
                  if(LReservoir <> nil) and (LReservoir.FNodeType.FData = ntReservoir) then
                  begin
                    LReservoir.FNodeType.FData := ntMineUndergroundDam;
                    LReservoir.FNodeType.FInitalised := True;
                  end;
                end;
              end;
            end;
          end;

        end;

      end;

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TPlanningFileDataObjects.GetAllocationDefinitionFileDataObjectByIndex(AIndex: integer): TAllocationDefinitionFileDataObject;
const OPNAME = 'TPlanningFileDataObjects.GetAllocationDefinitionFileDataObjectByIndex';
begin
  Result  := nil;
  try
    if(AIndex >= 0) and (AIndex < FAllocationDefinitionFiles.Count) then
      Result := TAllocationDefinitionFileDataObject(FAllocationDefinitionFiles[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningFileDataObjects.GetAllocationDefinitionFileDataObjectByFileNumber(AFileNumber: integer): TAllocationDefinitionFileDataObject;
const OPNAME = 'TPlanningFileDataObjects.GetAllocationDefinitionFileDataObjectByFileNumber';
var
  LIndex: integer;
begin
  Result  := nil;
  try
    for LIndex := 0 to FAllocationDefinitionFiles.Count-1 do
    begin
      if(TAllocationDefinitionFileDataObject(FAllocationDefinitionFiles[LIndex]).FileNumber = AFileNumber) then
      begin
        Result  := TAllocationDefinitionFileDataObject(FAllocationDefinitionFiles[LIndex]);
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningFileDataObjects.AddChannelSwitchControlFileDataObject(AFileNumber: integer): TChannelSwitchControlFileDataObject;
const OPNAME = 'TPlanningFileDataObjects.AddChannelSwitchControlFileDataObject';
begin
  Result  := ChannelSwitchControlFileDataObjectByFileNumber[AFileNumber];
  try
    if(Result = nil) then
    begin
      Result  := TChannelSwitchControlFileDataObject.Create(AFileNumber);
      FChannelSwitchControlFiles.Add(Result);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningFileDataObjects.GetChannelSwitchControlFileDataObjectByFileNumber(AFileNumber: integer): TChannelSwitchControlFileDataObject;
const OPNAME = 'TPlanningFileDataObjects.GetChannelSwitchControlFileDataObjectByFileNumber';
var
  LIndex: integer;
begin
  Result  := nil;
  try
    for LIndex := 0 to FChannelSwitchControlFiles.Count-1 do
    begin
      if(TChannelSwitchControlFileDataObject(FChannelSwitchControlFiles[LIndex]).FileNumber = AFileNumber) then
      begin
        Result  := TChannelSwitchControlFileDataObject(FChannelSwitchControlFiles[LIndex]);
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningFileDataObjects.GetChannelSwitchControlFileDataObjectByIndex(AIndex: integer): TChannelSwitchControlFileDataObject;
const OPNAME = 'TPlanningFileDataObjects.GetChannelSwitchControlFileDataObjectByIndex';
begin
  Result  := nil;
  try
    if(AIndex >= 0) and (AIndex < FChannelSwitchControlFiles.Count) then
      Result := TChannelSwitchControlFileDataObject(FChannelSwitchControlFiles[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningFileDataObjects.GetMIMMListFileObjectByFileNumber(AFileNumber: integer): TMIMMListFileObject;
const OPNAME = 'TPlanningFileDataObjects.GetMIMMListFileObjectByFileNumber';
var
  LIndex: integer;
begin
  Result  := nil;
  try
    for LIndex := 0 to FMIMMFileObjectList.Count-1 do
    begin
      if(TMIMMListFileObject(FMIMMFileObjectList[LIndex]).FileNumber = AFileNumber) then
      begin
        Result  := TMIMMListFileObject(FMIMMFileObjectList[LIndex]);
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningFileDataObjects.GetMIMMListFileObjectByIndex(AIndex: integer): TMIMMListFileObject;
const OPNAME = 'TPlanningFileDataObjects.GetAllocationDefinitionFileDataObjectByIndex';
begin
  Result  := nil;
  try
    if(AIndex >= 0) and (AIndex < FMIMMFileObjectList.Count) then
      Result := TMIMMListFileObject(FMIMMFileObjectList[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningFileDataObjects.GetSaltWashOffObjectByFileNumber(AFileNumber: integer): TSaltWashOffObject;
const OPNAME = 'TPlanningFileDataObjects.GetSaltWashOffObjectByFileNumber';
var
  LIndex: integer;
begin
  Result  := nil;
  try
    for LIndex := 0 to FSaltWashOffObject.Count-1 do
    begin
      if(TSaltWashOffObject(FSaltWashOffObject[LIndex]).FileNumber = AFileNumber) then
      begin
        Result  := TSaltWashOffObject(FSaltWashOffObject[LIndex]);
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningFileDataObjects.GetSaltWashOffObjectByIndex(AIndex: integer): TSaltWashOffObject;
const OPNAME = 'TPlanningFileDataObjects.GetSaltWashOffObjectByIndex';
begin
  Result  := nil;
  try
    if(AIndex >= 0) and (AIndex < FSaltWashOffObject.Count) then
      Result := TSaltWashOffObject(FSaltWashOffObject[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningFileDataObjects.AddSaltWashOffObject(AFileNumber: integer): TSaltWashOffObject;
const OPNAME = 'TPlanningFileDataObjects.AddSaltWashOffObject';
begin
  Result  := SaltWashOffObjectByFileNumber[AFileNumber];
  try
    if(Result = nil) then
    begin
      Result  := TSaltWashOffObject.Create(AFileNumber);
      FSaltWashOffObject.Add(Result);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.


