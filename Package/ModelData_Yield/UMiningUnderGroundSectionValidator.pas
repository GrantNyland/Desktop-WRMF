{******************************************************************************}
//  UNIT      : Contains the class TMiningUnderGroundSectionValidator.
//  AUTHOR    : Presley Mudau
//  DATE      : 2007/03/14
//  COPYRIGHT : Copyright © 2007 DWAF
{******************************************************************************}

unit UMiningUnderGroundSectionValidator;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  VCL.Grids,
  VCL.Dialogs,

  // arivia.kom
  VoaimsCom_TLB,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent,
  UMiningUnderGroundSectionDialog,
  UAbstractYieldDataDialogValidator,
  UYieldContextValidationType;
type
  TMiningUnderGroundSectionValidator = class(TAbstractYieldDataDialogValidator)
  protected
    FIdentifier         : integer;
    FSelectUGIdentifier : integer;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnStringGridCellDataHasChanged (ASender: TObject; ACol, ARow: integer); override;
    procedure OnGridSelectCell(ASender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure OnMonthlyRechargeFactorsClick(Sender: TObject; ACol, ARow: Longint);
    procedure OnInsertUndergroundSection(Sender: TObject);
    procedure OnDeleteUndergroundSection(Sender: TObject);
    procedure PopulateUnderGroundMiningGrid;
    procedure UpdateUndergroundSectionName(ARowIndex : integer;
                                           AValue    : string);
    procedure UpdateUpstreamCatchmentArea(ARowIndex : integer;
                                          AValue    : string);
    procedure UpdateBoardPillarCatchmentArea(ARowIndex : integer;
                                             AValue    : string);
    procedure UpdateHighExtractionCatchmentArea(ARowIndex : integer;
                                                AValue    : string);
    procedure UpdateHighExtractionAreaRunoffFactor(ARowIndex : integer;
                                                   AValue    : string);
     procedure RePopulateDataViewer;

  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;

    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;

    function MiningUnderGroundSectionDialog: TMiningUnderGroundSectionDialog;
    property Identifier : integer read FIdentifier write FIdentifier;
//    procedure DoContextValidation (AValidationType : TDialogValidationType); override;
    function DetermineWizardStatus (ASequence : integer = 0) : integer; override;
  end;

implementation

uses
  SysUtils,
  VCL.Graphics,
  UFileNames,
  UConstants,
  UAbstractFileNamesObject,
  UMonthlyDamLevelsObject,
  UReservoirPenaltyValidator,
  UYieldModelDataGUIForm,
  UReservoirPenaltyDialog,
  UMineMonthlyDataValidator,
  UReservoirPenaltyStructureData,
  UYieldModelDataObject,
  UErrorHandlingOperations, Math;

{ TMiningValidator }

procedure TMiningUnderGroundSectionValidator.CreateMemberObjects;
const OPNAME = 'TMiningUnderGroundSectionValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FIdentifier := -1;
    FPanel := TMiningUnderGroundSectionDialog.Create(FPanelOwner,FAppModules);
    with MiningUnderGroundSectionDialog do
    begin
      NrOfUnderGroundMiningEdt.FieldProperty    := FAppModules.FieldProperties.FieldProperty('NrOfUnderGroundMining');
      NrOfUnderGroundMiningEdt.OnEnter          := OnEditControlEnter;
      NrOfUnderGroundMiningEdt.OnExit           := OnEditControltExit;

      InsertUndergroundBtn.OnClick              := OnInsertUndergroundSection;
      DeleteUndergroundSlurryBtn.OnClick        := OnDeleteUndergroundSection;

      UnderGroundMiningGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('NrOfUnderGroundMining'));
      UnderGroundMiningGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ChannelNumberToUGDam'));
      UnderGroundMiningGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('UndergroundSectionName'));
      UnderGroundMiningGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('UpstreamCatchmentArea'));
      UnderGroundMiningGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('BoardPillarCatchmentArea'));
      UnderGroundMiningGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('HighExtractionCatchmentArea'));
      UnderGroundMiningGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('HighExtractionAreaRunoffFactor'));
      UnderGroundMiningGrid.OnBeforeCellChange   := OnStringGridCellDataHasChanged;
      UnderGroundMiningGrid.OnSelectCell         := OnGridSelectCell;
      UnderGroundMiningGrid.OnColEnter           := OnStringGridColEnter;
      UnderGroundMiningGrid.OnExit               := OnEditControltExit;
      UnderGroundMiningGrid.OnEnter              := OnEditControlEnter;

      UnderGroundMiningGrid.ButtonColumn[7] := True;
      UnderGroundMiningGrid.ButtonColumnCaption[7] := FAppModules.Language.GetString('LabelText.ThreeDots');
      UnderGroundMiningGrid.ButtonColumnOnClick[7] := OnMonthlyRechargeFactorsClick;

      UnderGroundMiningGrid.ButtonColumn[8] := True;
      UnderGroundMiningGrid.ButtonColumnCaption[8] := FAppModules.Language.GetString('LabelText.ThreeDots');
      UnderGroundMiningGrid.ButtonColumnOnClick[8] := OnMonthlyRechargeFactorsClick;

      UnderGroundMiningGrid.ButtonColumn[9] := True;
      UnderGroundMiningGrid.ButtonColumnCaption[9] := FAppModules.Language.GetString('LabelText.ThreeDots');
      UnderGroundMiningGrid.ButtonColumnOnClick[9] := OnMonthlyRechargeFactorsClick;

    end;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningUnderGroundSectionValidator.DestroyMemberObjects;
const OPNAME = 'TMiningUnderGroundSectionValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMiningUnderGroundSectionValidator.Initialise: boolean;
const OPNAME = 'TMiningUnderGroundSectionValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMiningUnderGroundSectionValidator.LanguageHasChanged: boolean;
const OPNAME = 'TMiningUnderGroundSectionValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := 'UnderGround Section';
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningUnderGroundSectionValidator.ClearDataViewer;
const OPNAME = 'TMiningUnderGroundSectionValidator.ClearDataViewer';
begin
  inherited ClearDataViewer;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningUnderGroundSectionValidator.RePopulateDataViewer;
const OPNAME = 'TMiningUnderGroundSectionValidator.RePopulateDataViewer';
var
  LMine      : IMine;
  LCanSelect : Boolean;
begin
  try
    if(FIdentifier >= 0) then
    begin
      LMine := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
                       .CastMineList.MineByNodeNumber[FIdentifier];
      if (LMine <> nil) then
      begin
        with MiningUnderGroundSectionDialog do
        begin
          NrOfUnderGroundMiningEdt.SetFieldValue(LMine.UndergroundCount);
          PopulateUnderGroundMiningGrid;

          LCanSelect := True;
          if(LMine.UndergroundCount > 0) then
            OnGridSelectCell(UnderGroundMiningGrid,1,1,LCanSelect);
        end;
        MiningUnderGroundSectionDialog.ResetButtonState(LMine.UndergroundCount);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMiningUnderGroundSectionValidator.SaveState: boolean;
const OPNAME = 'TMiningUnderGroundSectionValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningUnderGroundSectionValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TMiningUnderGroundSectionValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningUnderGroundSectionValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TMiningUnderGroundSectionValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    if ((Sender =  MiningUnderGroundSectionDialog.NrOfUnderGroundMiningEdt) AND
       (MiningUnderGroundSectionDialog.NrOfUnderGroundMiningEdt.HasValueChanged)) then
       PopulateUnderGroundMiningGrid
   { else
    if ((Sender =  ReservoirPropertiesDialog.ReservoirExistsChkBox) AND
       (ReservoirPropertiesDialog.ReservoirExistsChkBox.HasValueChanged)) then
       UpdateReservoirExists
    else
    if ((Sender =  ReservoirPropertiesDialog.ReservoirNameEdit) AND
       (ReservoirPropertiesDialog.ReservoirNameEdit.HasValueChanged)) then
       UpdateResevoirName
    else
    if ((Sender =  ReservoirPropertiesDialog.PenaltyStructureEdit) AND
       (ReservoirPropertiesDialog.PenaltyStructureEdit.HasValueChanged))then
       UpdateResevoirPenaltyStructure
    else
    if ((Sender =  ReservoirPropertiesDialog.PriorityEdit) AND
       (ReservoirPropertiesDialog.PriorityEdit.HasValueChanged)) then
       UpdateResevoirPriority
    else
    if ((sender = ReservoirPropertiesDialog.RainCoeffEdit) AND
       (ReservoirPropertiesDialog.RainCoeffEdit.HasValueChanged)) then
       UpdateRainCoef
    else
    if ((sender = ReservoirPropertiesDialog.ReservoirXCoordEdit) AND
       (ReservoirPropertiesDialog.ReservoirXCoordEdit.HasValueChanged)) then
       UpdateXCoord
    else
    if ((sender = ReservoirPropertiesDialog.ReservoirYCoordEdit) AND
       (ReservoirPropertiesDialog.ReservoirYCoordEdit.HasValueChanged)) then
       UpdateYCoord
    else
    if ((sender = ReservoirPropertiesDialog.DamLevelsFileNameCbx) AND
       (ReservoirPropertiesDialog.DamLevelsFileNameCbx.HasValueChanged)) then
    begin
      UpdateDamLevelFileName;
      RePopulateDataViewer;
    end;                    }
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMiningUnderGroundSectionValidator.MiningUnderGroundSectionDialog :TMiningUnderGroundSectionDialog;
const OPNAME = 'TMiningUnderGroundSectionValidator.MiningUnderGroundSectionDialog';
begin
  Result := Nil;
  try
    if Assigned(FPanel) then
      Result := TMiningUnderGroundSectionDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMiningUnderGroundSectionValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TMiningUnderGroundSectionValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if (AFieldName = 'Underground') then
      RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMiningUnderGroundSectionValidator.StudyHasChanged: boolean;
const OPNAME = 'TMiningUnderGroundSectionValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{procedure TMiningUnderGroundSectionValidator.UpdateResevoirName;
const OPNAME = 'TMiningUnderGroundSectionValidator.UpdateResevoirName';
var
  LReservoirObject: IReservoirData;
  LErrorMessage: string;
begin
  try
    if ReservoirPropertiesDialog.ReservoirNameEdit.HasValueChanged then
    begin
      LReservoirObject := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier];
      if (LReservoirObject <> nil) then
      begin
        with ReservoirPropertiesDialog do
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              ReservoirNameEdit.FieldProperty.FieldName,
              ReservoirNameEdit.Text,LErrorMessage)) then
          begin
            LReservoirObject.ReservoirConfigurationData.ReservoirName := Trim(ReservoirNameEdit.Text);
            ReservoirNameEdit.SetFieldValue(LReservoirObject.ReservoirConfigurationData.ReservoirName);
            DoContextValidation(dvtResPropReservoirName);
          end;
          ReservoirNameEdit.ContextValidationError := LErrorMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningUnderGroundSectionValidator.UpdateResevoirPenaltyStructure;
const OPNAME = 'TMiningUnderGroundSectionValidator.UpdateResevoirPenaltyStructure';
var
  LReservoirObject      : IReservoirData;
  LPenaltyStructureList : IReservoirPenaltyList;
  LPenaltyCountsData    : IReservoirPenaltyCounts;
  LErrorMessage         : string;
begin
  try
    if(ReservoirPropertiesDialog.PenaltyStructureEdit.HasValueChanged) then
    begin
      LReservoirObject := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier];
      if (LReservoirObject <> nil) then
      begin
        if FAppModules.FieldProperties.ValidateFieldProperty(
           ReservoirPropertiesDialog.PenaltyStructureEdit.FieldProperty.FieldName,
           ReservoirPropertiesDialog.PenaltyStructureEdit.Text,LErrorMessage) then
        begin
          LPenaltyStructureList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirPenaltyStructureList;
          if (LPenaltyStructureList <> nil) then
          begin
            LPenaltyCountsData := LPenaltyStructureList.ReservoirPenaltyCounts;
            if (LPenaltyCountsData <> nil) then
            begin
              if  (StrToInt(ReservoirPropertiesDialog.PenaltyStructureEdit.Text) >  LPenaltyCountsData.PenaltyStructureCount) then
                ReservoirPropertiesDialog.PenaltyStructureEdit.FieldValidationError := 'invalid penalty structure number'
              else
              begin
                ReservoirPropertiesDialog.PenaltyStructureEdit.FieldValidationError :=  LErrorMessage;
                LReservoirObject.ReservoirConfigurationData.PenaltyStructIdentifier := StrToInt(ReservoirPropertiesDialog.PenaltyStructureEdit.Text);
                ReservoirPropertiesDialog.PenaltyStructureEdit.SetFieldValue(ReservoirPropertiesDialog.PenaltyStructureEdit.Text);
                DoContextValidation(dvtReservoirPenalty);
              end;
            end;
          end;
        end
        else
        begin
          ReservoirPropertiesDialog.PenaltyStructureEdit.FieldValidationError :=  LErrorMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningUnderGroundSectionValidator.UpdateResevoirPriority;
const OPNAME = 'TMiningUnderGroundSectionValidator.UpdateResevoirPriority';
var
  lErrorMessage    : string;
  LReservoirObject : IReservoirData;
begin
  try
    if ReservoirPropertiesDialog.PriorityEdit.HasValueChanged then
    begin
      LReservoirObject := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier];
      if (LReservoirObject <> nil) then
      begin
        with ReservoirPropertiesDialog do
        begin
          if FAppModules.FieldProperties.ValidateFieldProperty(
           'ReservoirPriority',PriorityEdit.Text, lErrorMessage) then
          begin
            PriorityEdit.FieldValidationError := LErrorMessage;
            LReservoirObject.ReservoirConfigurationData.Priority := StrToFloat(PriorityEdit.Text);
            PriorityEdit.SetFieldValue(LReservoirObject.ReservoirConfigurationData.Priority);
            DoContextValidation(dvtResPropPriority);
          end
          else
            PriorityEdit.FieldValidationError := lErrorMessage;
        end
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningUnderGroundSectionValidator.UpdateReservoirExists;
const OPNAME = 'TMiningUnderGroundSectionValidator.UpdateReservoirExists';
var
  LReservoirObject: IReservoirData;
  LReservoirExists : integer;
  LErrorMessage: string;

begin
  try
    LReservoirObject := CurrentReservoir;
    if (LReservoirObject <> nil) then
    begin
      LReservoirExists := LReservoirObject.ReservoirConfigurationData.StatusIndicator;

      if((LReservoirExists = 1) and ( not ReservoirPropertiesDialog.ReservoirExistsChkBox.Checked) or
         (LReservoirExists <> 1) and (ReservoirPropertiesDialog.ReservoirExistsChkBox.Checked)) then
      begin
        if ReservoirPropertiesDialog.ReservoirExistsChkBox.Checked then
          LReservoirExists := 1
        else
          LReservoirExists := 0;

      if FAppModules.FieldProperties.ValidateFieldProperty(
         ReservoirPropertiesDialog.ReservoirExistsChkBox.FieldProperty.FieldName,
         IntToStr(LReservoirExists),LErrorMessage) then
        begin
          LReservoirObject.ReservoirConfigurationData.StatusIndicator := LReservoirExists;
          ReservoirPropertiesDialog.ReservoirExistsChkBox.Checked     :=
            (LReservoirObject.ReservoirConfigurationData.StatusIndicator = 1);
        end
        else
        begin
          ReservoirPropertiesDialog.ShowError(LErrorMessage);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningUnderGroundSectionValidator.UpdateResevoirIncludeSummary;
const OPNAME = 'TMiningUnderGroundSectionValidator.UpdateResevoirIncludeSummary';
var
  LReservoirObject: IReservoirData;
  LIncludeSummary : string;
  LErrorMessage: string;
begin
  try
    LReservoirObject := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier];
    if (LReservoirObject <> nil) then
    begin
      LIncludeSummary := UpperCase(Trim(LReservoirObject.ReservoirConfigurationData.IncludeSummary));

      if((LIncludeSummary = 'Y') and ( not ReservoirPropertiesDialog.SummaryIncludeChkBox.Checked) or
         (LIncludeSummary <> 'Y') and (ReservoirPropertiesDialog.SummaryIncludeChkBox.Checked)) then
      begin
        if ReservoirPropertiesDialog.SummaryIncludeChkBox.Checked then
          LIncludeSummary := 'Y'
        else
          LIncludeSummary := 'N';

        if FAppModules.FieldProperties.ValidateFieldProperty(
         ReservoirPropertiesDialog.SummaryIncludeChkBox.FieldProperty.FieldName,
         LIncludeSummary,LErrorMessage) then
        begin
          LReservoirObject.ReservoirConfigurationData.IncludeSummary := LIncludeSummary;
          ReservoirPropertiesDialog.SummaryIncludeChkBox.Checked     :=
            (LReservoirObject.ReservoirConfigurationData.IncludeSummary = 'Y');
        end
        else
        begin
          ReservoirPropertiesDialog.ShowError(LErrorMessage);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningUnderGroundSectionValidator.UpdateRainCoef;
const OPNAME = 'TMiningUnderGroundSectionValidator.UpdateRainCoef';
var
  lErrorMessage    : string;
  LReservoirObject : IReservoirData;
begin
  try
    if ReservoirPropertiesDialog.RainCoeffEdit.HasValueChanged then
    begin
      LReservoirObject := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier];
      if (LReservoirObject <> nil) then
      begin
        with ReservoirPropertiesDialog do
        begin
          if FAppModules.FieldProperties.ValidateFieldProperty('RainCoef',RainCoeffEdit.Text, lErrorMessage) then
          begin
            RainCoeffEdit.FieldValidationError := lErrorMessage;
            LReservoirObject.ReservoirConfigurationData.RainCoef := StrToFloat(RainCoeffEdit.Text);
            RainCoeffEdit.SetFieldValue(LReservoirObject.ReservoirConfigurationData.RainCoef);
            DoContextValidation(dvtResPropRainCoef);
          end
          else
            RainCoeffEdit.FieldValidationError := lErrorMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningUnderGroundSectionValidator.UpdateXCoord;
const OPNAME = 'TMiningUnderGroundSectionValidator.UpdateXCoord';
var
  lErrorMessage    : string;
  LReservoirObject : IReservoirData;
begin
  try
    if ReservoirPropertiesDialog.ReservoirXCoordEdit.HasValueChanged then
    begin
      LReservoirObject := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier];
      if (LReservoirObject <> nil) then
      begin
        with ReservoirPropertiesDialog do
        begin
          if FAppModules.FieldProperties.ValidateFieldProperty('XCoord',ReservoirXCoordEdit.Text, lErrorMessage) then
          begin
            ReservoirXCoordEdit.FieldValidationError := lErrorMessage;
            LReservoirObject.ReservoirConfigurationData.XCoord := StrToFloat(ReservoirXCoordEdit.Text);
            ReservoirXCoordEdit.SetFieldValue(LReservoirObject.ReservoirConfigurationData.XCoord);
          end
          else
            ReservoirXCoordEdit.FieldValidationError := lErrorMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningUnderGroundSectionValidator.UpdateYCoord;
const OPNAME = 'TMiningUnderGroundSectionValidator.UpdateYCoord';
var
  lErrorMessage    : string;
  LReservoirObject : IReservoirData;
begin
  try
    if ReservoirPropertiesDialog.ReservoirYCoordEdit.HasValueChanged then
    begin
      LReservoirObject := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier];
      if (LReservoirObject <> nil) then
      begin
        with ReservoirPropertiesDialog do
        begin
          if FAppModules.FieldProperties.ValidateFieldProperty('YCoord',ReservoirYCoordEdit.Text, lErrorMessage) then
          begin
            ReservoirYCoordEdit.FieldValidationError := lErrorMessage;
            LReservoirObject.ReservoirConfigurationData.YCoord := StrToFloat(ReservoirYCoordEdit.Text);
            ReservoirYCoordEdit.SetFieldValue(LReservoirObject.ReservoirConfigurationData.YCoord);
          end
          else
            ReservoirYCoordEdit.FieldValidationError := lErrorMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;    }


procedure TMiningUnderGroundSectionValidator.PopulateDataViewer;
const OPNAME = 'TMiningUnderGroundSectionValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtResPropAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{procedure TMiningUnderGroundSectionValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TMiningUnderGroundSectionValidator.DoContextValidation';
var
  lReservoir     : IReservoirData;
  lReservoirList : IReservoirDataList;
begin
  try
    FAllErrorMessages.Clear;
    if (FIdentifier >= 0) then
    begin
      try
        lReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).
                          NetworkElementData.ReservoirList;
        lReservoir     := lReservoirList.ReservoirByIdentifier[FIdentifier];
        if (lReservoir <> nil) then
        begin
           if (AValidationType in [dvtResPropAll, dvtResPropReservoirName]) then
              ValidateReservoirName(lReservoir);

           if (AValidationType in [dvtResPropAll, dvtReservoirPenalty]) then
              ValidateReservoirPenaltyStructure(lReservoir);

           if (AValidationType in [dvtResPropAll, dvtResPropReservoirNumber]) then
              ValidateReservoirNumber(lReservoir);

           if (AValidationType in [dvtResPropAll, dvtResPropPriority]) then
              ValidateReservoirPriority(lReservoir);
           
           if (AValidationType in [dvtResPropAll, dvtResPropRainCoef]) then
              ValidateRainCoef(lReservoir.ReservoirConfigurationData);

           if (AValidationType in [dvtResPropAll, dvtDamlevelFileName]) then
              ValidateDamLevelFileName(lReservoir);
        end;
      finally
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;    }

function TMiningUnderGroundSectionValidator.DetermineWizardStatus (ASequence : integer = 0) : integer;
const OPNAME = 'TMiningUnderGroundSectionValidator.DetermineWizardStatus';
var
  lReservoir     : IReservoirData;
  lReservoirList : IReservoirDataList;
begin
  Result := inherited DetermineWizardStatus(ASequence);
  try
    if (FIdentifier >= 0) then
    begin
      try
        lReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).
                            NetworkElementData.ReservoirList;
        lReservoir     := lReservoirList.ReservoirOrNodeByIdentifier[FIdentifier];
        if (lReservoir <> nil) then
        begin
          DoContextValidation(dvtResPropAll);
          if (lReservoir.ReservoirConfigurationData.PenaltyStructIdentifier <> 0) then
          begin
            Result := 1;
            if (FAllErrorMessages.Count = 0) then
              Result := 2;
          end;    
        end;
      finally
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{procedure TMiningUnderGroundSectionValidator.ValidateReservoirName(AReservoir: IReservoirData);
const OPNAME = 'TMiningUnderGroundSectionValidator.ValidateReservoirName';
begin
  try
    with ReservoirPropertiesDialog do
    begin
      FErrorMessage := '';
      ReservoirNameEdit.ContextValidationError := FErrorMessage;
      if (NOT AReservoir.ReservoirConfigurationData.Validate(FErrorMessage ,'ReservoirName')) then
        FAllErrorMessages.Add(FErrorMessage);
      ReservoirNameEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningUnderGroundSectionValidator.ValidateReservoirPenaltyStructure(AReservoir: IReservoirData);
const OPNAME = 'TMiningUnderGroundSectionValidator.ValidateReservoirPenaltyStructure';
begin
  try
    with ReservoirPropertiesDialog do
    begin
      FErrorMessage := '';
      PenaltyStructureEdit.ContextValidationError := FErrorMessage;
      if (NOT AReservoir.ReservoirConfigurationData.Validate(FErrorMessage,'ReservoirPenaltyStructure')) then
        FAllErrorMessages.Add(FErrorMessage);
      PenaltyStructureEdit.FieldValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningUnderGroundSectionValidator.ValidateReservoirNumber(AReservoir: IReservoirData);
const OPNAME = 'TMiningUnderGroundSectionValidator.ValidateReservoirNumber';
begin
  try
    with ReservoirPropertiesDialog do
    begin
      FErrorMessage := '';
      ReservoirNumberEdit.ContextValidationError := FErrorMessage;
      if (NOT AReservoir.ReservoirConfigurationData.Validate(FErrorMessage,'ReservoirNumber')) then
        FAllErrorMessages.Add(FErrorMessage);
      ReservoirNumberEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningUnderGroundSectionValidator.ValidateReservoirPriority(AReservoir: IReservoirData);
const OPNAME = 'TMiningUnderGroundSectionValidator.ValidateReservoirPriority';
begin
  try
    with ReservoirPropertiesDialog do
    begin
      FErrorMessage := '';
      PriorityEdit.ContextValidationError := FErrorMessage;
      if (NOT AReservoir.ReservoirConfigurationData.Validate(FErrorMessage, 'ReservoirPriority')) then
        FAllErrorMessages.Add(FErrorMessage);
      PriorityEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningUnderGroundSectionValidator.ValidateRainCoef(AReservoir: IReservoirConfigurationData);
const OPNAME = 'TMiningUnderGroundSectionValidator.ValidateRainCoef';
begin
  try
    with ReservoirPropertiesDialog do
    begin
      FErrorMessage := '';
      RainCoeffEdit.ContextValidationError := FErrorMessage;
      if (NOT AReservoir.Validate(FErrorMessage, 'RainCoef')) then
        FAllErrorMessages.Add(FErrorMessage);
      RainCoeffEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMiningUnderGroundSectionValidator.ProcessMetaDataEvent: boolean;
const OPNAME = 'TMiningUnderGroundSectionValidator.ProcessMetaDataEvent';
var
  lFieldIndex    : string;
  lKeyValues     : string;
  lReservoir     : IReservoirData;
  lFieldProperty : TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    if (FPanel.Visible AND (FActiveControl <> nil)) then
    begin
      lReservoir := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier];
      if (lReservoir <> nil) then
      begin
        with ReservoirPropertiesDialog do
        begin
          lFieldIndex         := '';
          lFieldProperty      := nil;
          if (FActiveControl  = ReservoirNameEdit) then
            lFieldProperty    := ReservoirNameEdit.FieldProperty;
          if (FActiveControl  = PenaltyStructureEdit) then
            lFieldProperty    := PenaltyStructureEdit.FieldProperty;
          if (FActiveControl  = PriorityEdit) then
            lFieldProperty    := PriorityEdit.FieldProperty;
          if (FActiveControl  = RainCoeffEdit) then
            lFieldProperty    := RainCoeffEdit.FieldProperty;
          if (FActiveControl  = SummaryIncludeChkBox) then
            lFieldProperty    := SummaryIncludeChkBox.FieldProperty;
          if (FActiveControl  = ReservoirExistsChkBox) then
            lFieldProperty    := ReservoirExistsChkBox.FieldProperty;

          if (lFieldProperty <> nil) then
          begin
            lKeyValues := lReservoir.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
            FAppModules.MetaData.ShowMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex);
            RePopulateDataViewer;
            Result := TRUE;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMiningUnderGroundSectionValidator.OnSelectHistoricWaterlevelClick(Sender: TObject);
const OPNAME = 'TMiningUnderGroundSectionValidator.OnSelectHistoricWaterlevelClick';
var
  LFileSelector : TOpenDialog;
  LFileName     : string;
  lReservoir    : IReservoirData;
  lMessage      : string;
begin
  try
    LFileSelector := TOpenDialog.Create(nil);
    try
      LFileSelector.Filter  := 'All Files|*.*| (*.TXT)| *.txt|(*.ABS)|*.abs|(*.CIR)|*.cir|(*.DEM)|*.dem|(*.IRR)|*.IRR|(*.IRD)|*.ird|(*.URB)|*.urb';
      LFileSelector.Options := [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing];
      if LFileSelector.Execute then
      begin
        LFileName := LFileSelector.FileName;
        if(UpperCase(LFileName) <> UpperCase( ReservoirPropertiesDialog.DamLevelsFileNameCbx.Text)) then
        begin
          lReservoir := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier];
          if (lReservoir <> nil) then
          begin
            with ReservoirPropertiesDialog do
            begin
              if (FAppModules.FieldProperties.ValidateFieldProperty(
                  DamLevelsFileNameCbx.FieldProperty.FieldName,
                  LFileName,lMessage)) then
              begin
                lReservoir.ReservoirConfigurationData.DamLevelsFileName := LFileName;
                RepopulateHistoricWaterLevels;
                DamLevelsFileNameCbx.ItemIndex := DamLevelsFileNameCbx.Items.IndexOf(lReservoir.ReservoirConfigurationData.DamLevelsFileName);
                DoContextValidation(dvtDamlevelFileName);
                SetGridGraphBtnState(lReservoir);
              end
              else
                DamLevelsFileNameCbx.ValidationError := lMessage;
            end;
          end;
        end;
      end;
    finally
      FreeAndNil(LFileSelector);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningUnderGroundSectionValidator.RepopulateHistoricWaterLevels;
const OPNAME = 'TMiningUnderGroundSectionValidator.RepopulateHistoricWaterLevels';
var
  LFileNamesList : TFileNamesList;
  LFileName      : string;
  LIndex         : integer;
  lReservoir    : IReservoirData;
begin
  try
    with ReservoirPropertiesDialog do
    begin
      DamLevelsFileNameCbx.Items.Clear;
     LFileNamesList := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.CastDamLevelsFileNames;
      for LIndex:= 0 to LFileNamesList.FilesCount-1 do
      begin
        LFileName := LFileNamesList.FileNameObject[LIndex].FileName;
        DamLevelsFileNameCbx.Items.Add(LFileName);
      end;
      lReservoir := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier];
      if (lReservoir <> nil)  and (lReservoir.ReservoirConfigurationData.DamLevelsFileName <> '')then
      DamLevelsFileNameCbx.ItemIndex := DamLevelsFileNameCbx.Items.IndexOf(lReservoir.ReservoirConfigurationData.DamLevelsFileName);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMiningUnderGroundSectionValidator.UpdateDamLevelFileName;
const OPNAME = 'TMiningUnderGroundSectionValidator.UpdateDamLevelFileName';
var
  lReservoir : IReservoirData;
  lMessage   : string;
begin
  try
    lReservoir := TYieldModelDataObject(FAppModules.Model.ModelData).
                  NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier];
    if (lReservoir <> nil) then
    begin
      with ReservoirPropertiesDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            DamLevelsFileNameCbx.FieldProperty.FieldName,
            DamLevelsFileNameCbx.Text,lMessage)) then
        begin
          lReservoir.ReservoirConfigurationData.DamLevelsFileName := DamLevelsFileNameCbx.Text;
          DamLevelsFileNameCbx.SetFieldIndex(DamLevelsFileNameCbx.Items.IndexOf(lReservoir.ReservoirConfigurationData.DamLevelsFileName));
          DoContextValidation(dvtDamlevelFileName);
        end
        else
          DamLevelsFileNameCbx.ValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningUnderGroundSectionValidator.OnViewGridClick(Sender: TObject);
const OPNAME = 'TMiningUnderGroundSectionValidator.OnViewGridClick';
var
  LContextDataList: TStringList;
begin
  try
    LContextDataList := TStringList.Create;
    try
      LContextDataList.Add('VIEWNAME=INPUTVIEWDATAGRID');
      LContextDataList.Add(Format('MODELELEMENTID=%d',[FIdentifier]));
      LContextDataList.Add('MODELELEMENTTYPE=DAMLEVELS');
      FAppModules.Model.ViewInputDialog(nil, LContextDataList.CommaText, nil);
    finally
      LContextDataList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningUnderGroundSectionValidator.OnViewGraphClick(Sender: TObject);
const OPNAME = 'TMiningUnderGroundSectionValidator.OnViewGraphClick';
var
  LContextDataList: TStringList;
begin
  try
    LContextDataList := TStringList.Create;
    try
      LContextDataList.Add('VIEWNAME=INPUTVIEWDATAGRAPH');
      LContextDataList.Add(Format('MODELELEMENTID=%d',[FIdentifier]));
      LContextDataList.Add('MODELELEMENTTYPE=DAMLEVELS');
      FAppModules.Model.ViewInputDialog(nil, LContextDataList.CommaText, nil);
    finally
      LContextDataList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningUnderGroundSectionValidator.SetGridGraphBtnState(AReservoir: IReservoirData);
const OPNAME = 'TMiningUnderGroundSectionValidator.SetGridGraphBtnState';
var
  LFileNamesList : TFileNamesList;
  LFileNameObject: TAbstractModelFileName;
  LIndex         : integer;
begin
  try
    ReservoirPropertiesDialog.DamLevelsFileNameGridBtn.Enabled := False;
    ReservoirPropertiesDialog.DamLevelsFileNameGraphBtn.Enabled := False;
    if(AReservoir <> nil) and (AReservoir.ReservoirConfigurationData.DamLevelsFileName <> '') then
    begin
      LFileNameObject := nil;
      LFileNamesList := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.CastDamLevelsFileNames;
      for LIndex:= 0 to LFileNamesList.FilesCount-1 do
      begin
        if(UpperCase(AReservoir.ReservoirConfigurationData.DamLevelsFileName) = UpperCase(LFileNamesList.FileNameObject[LIndex].ShortName)) then
        begin
          LFileNameObject := LFileNamesList.FileNameObject[LIndex];
          Break;
        end;
      end;
      if(LFileNameObject <> nil) then
      begin
        ReservoirPropertiesDialog.DamLevelsFileNameGridBtn.Enabled  := LFileNameObject.SavedInDB;
        ReservoirPropertiesDialog.DamLevelsFileNameGraphBtn.Enabled := LFileNameObject.SavedInDB;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningUnderGroundSectionValidator.ValidateDamLevelFileName(AReservoir: IReservoirData);
const OPNAME = 'TMiningUnderGroundSectionValidator.ValidateDamLevelFileName';
begin
  try
    with ReservoirPropertiesDialog do
    begin
      FErrorMessage := '';
      if (AReservoir.ReservoirConfigurationData.Validate(FErrorMessage, 'DamLevelsFileName')) then
      begin
        DamLevelsFileNameCbx.InValidationError := FALSE;
        DamLevelsFileNameCbx.ValidationError := '';
        DamLevelsFileNameCbx.ShowErrorState(FALSE);
      end
      else
      begin
        DamLevelsFileNameCbx.InValidationError := TRUE;
        DamLevelsFileNameCbx.ValidationError := FErrorMessage;
        DamLevelsFileNameCbx.ShowErrorState(TRUE);
        FAllErrorMessages.Add(Trim(FErrorMessage));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;    }

procedure TMiningUnderGroundSectionValidator.PopulateUnderGroundMiningGrid;
const OPNAME = 'TMiningUnderGroundSectionValidator.PopulateUnderGroundMiningGrid';
var
  LRow              : integer;
  LMine             : IMine;
  LUndergroundMine  : IUnderground;
begin
  try
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList
             .MineByNodeNumber[FIdentifier];
    with MiningUnderGroundSectionDialog do
    begin

      UnderGroundMiningGrid.ColCount := 10;
      UnderGroundMiningGrid.RowCount := Max((UnderGroundMiningGrid.FixedRows + 1),(1 + LMine.UndergroundCount));;

      if (LMine.UndergroundCount > 0) then
      begin
        for lRow := 1 to LMine.UndergroundCount do
        begin
          LUndergroundMine := LMine.UnderGroundByIndex[lRow-1];
          UnderGroundMiningGrid.SetFieldValue(0, lRow, LUndergroundMine.Identifier);
          UnderGroundMiningGrid.SetFieldValue(1, lRow, LUndergroundMine.ChannelNumberToUGDam);
          UnderGroundMiningGrid.SetFieldValue(2, lRow, LUndergroundMine.UndergroundSectionName);
          UnderGroundMiningGrid.SetFieldValue(3, lRow, LUndergroundMine.UpstreamCatchmentArea);
          UnderGroundMiningGrid.SetFieldValue(4, lRow, LUndergroundMine.BoardPillarCatchmentArea);
          UnderGroundMiningGrid.SetFieldValue(5, lRow, LUndergroundMine.HighExtractionCatchmentArea);
          UnderGroundMiningGrid.SetFieldValue(6, lRow, LUndergroundMine.HighExtractionAreaRunoffFactor);
          UnderGroundMiningGrid.Options := UnderGroundMiningGrid.Options + [goEditing];
        end;
      end  
      else
      begin
        UnderGroundMiningGrid.Cells[0, 1] := '';
        UnderGroundMiningGrid.Cells[1, 1] := '';
        UnderGroundMiningGrid.Cells[2, 1] := '';
        UnderGroundMiningGrid.Cells[3, 1] := '';
        UnderGroundMiningGrid.Cells[4, 1] := '';
        UnderGroundMiningGrid.Cells[5, 1] := '';
        UnderGroundMiningGrid.Cells[6, 1] := '';

        UnderGroundMiningGrid.Options := UnderGroundMiningGrid.Options - [goEditing];
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningUnderGroundSectionValidator.UpdateUndergroundSectionName(ARowIndex: integer;
                                                                          AValue: string);
const OPNAME = 'TMiningUnderGroundSectionValidator.UpdateUndergroundSectionName';
var
  LMine            : IMine;
  lValue           : string;
  lMessage         : string;
  LUndergroundID   : integer;
  LUndergroundMine : IUnderground;
begin
  try
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).
                     CastNetworkFeaturesData.CastMineList.MineByNodeNumber[FIdentifier];
    if (LMine <> nil) then
    begin
      with MiningUnderGroundSectionDialog do
      begin
         LUndergroundID := StrToInt(UnderGroundMiningGrid.Cells[0,ARowIndex]);
         LUndergroundMine := LMine.UnderGroundByIdentifier[LUndergroundID];
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'UndergroundSectionName', AValue, lMessage, 2, ARowIndex)) then
        begin
          lValue := AValue;
          LUndergroundMine.UndergroundSectionName := lValue;
          RePopulateDataViewer;
//          DoContextValidation(dvtDivFeatureType3Proportions);}
        end
        else
          UnderGroundMiningGrid.ValidationError[2, ARowIndex, gveCellField] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningUnderGroundSectionValidator.UpdateUpstreamCatchmentArea(ARowIndex: integer;
                                                                         AValue: string);
const OPNAME = 'TMiningUnderGroundSectionValidator.UpdateUpstreamCatchmentArea';
var
  LMine            : IMine;
  lValue           : string;
  lMessage         : string;
  LUndergroundID   : integer;
  LUndergroundMine : IUnderground;
begin
  try
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).
                     CastNetworkFeaturesData.CastMineList.MineByNodeNumber[FIdentifier];
    if (LMine <> nil) then
    begin
      with MiningUnderGroundSectionDialog do
      begin
         LUndergroundID := StrToInt(UnderGroundMiningGrid.Cells[0,ARowIndex]);
         LUndergroundMine := LMine.UnderGroundByIdentifier[LUndergroundID];
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'UpstreamCatchmentArea', AValue, lMessage, 3, ARowIndex)) then
        begin
          lValue := AValue;
          LUndergroundMine.UpstreamCatchmentArea := StrToFloat(lValue);
          RePopulateDataViewer;
//          DoContextValidation(dvtDivFeatureType3Proportions);}
        end
        else
          UnderGroundMiningGrid.ValidationError[3, ARowIndex, gveCellField] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningUnderGroundSectionValidator.UpdateBoardPillarCatchmentArea(ARowIndex: integer;
                                                                            AValue: string);
const OPNAME = 'TMiningUnderGroundSectionValidator.UpdateBoardPillarCatchmentArea';
var
  LMine            : IMine;
  lValue           : string;
  lMessage         : string;
  LUndergroundID   : integer;
  LUndergroundMine : IUnderground;
begin
  try
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).
                     CastNetworkFeaturesData.CastMineList.MineByNodeNumber[FIdentifier];
    if (LMine <> nil) then
    begin
      with MiningUnderGroundSectionDialog do
      begin
         LUndergroundID := StrToInt(UnderGroundMiningGrid.Cells[0,ARowIndex]);
         LUndergroundMine := LMine.UnderGroundByIdentifier[LUndergroundID];
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'BoardPillarCatchmentArea', AValue, lMessage, 4, ARowIndex)) then
        begin
          lValue := AValue;
          LUndergroundMine.BoardPillarCatchmentArea := StrToFloat(lValue);
          RePopulateDataViewer;
//          DoContextValidation(dvtDivFeatureType3Proportions);}
        end
        else
          UnderGroundMiningGrid.ValidationError[4, ARowIndex, gveCellField] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningUnderGroundSectionValidator.UpdateHighExtractionCatchmentArea(ARowIndex: integer;
                                                                               AValue: string);
const OPNAME = 'TMiningUnderGroundSectionValidator.UpdateHighExtractionCatchmentArea';
var
  LMine            : IMine;
  lValue           : string;
  lMessage         : string;
  LUndergroundID   : integer;
  LUndergroundMine : IUnderground;
begin
  try
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).
                     CastNetworkFeaturesData.CastMineList.MineByNodeNumber[FIdentifier];
    if (LMine <> nil) then
    begin
      with MiningUnderGroundSectionDialog do
      begin
         LUndergroundID := StrToInt(UnderGroundMiningGrid.Cells[0,ARowIndex]);
         LUndergroundMine := LMine.UnderGroundByIdentifier[LUndergroundID];
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'HighExtractionCatchmentArea', AValue, lMessage, 5, ARowIndex)) then
        begin
          lValue := AValue;
          LUndergroundMine.HighExtractionCatchmentArea := StrToFloat(lValue);
          RePopulateDataViewer;
//          DoContextValidation(dvtDivFeatureType3Proportions);}
        end
        else
          UnderGroundMiningGrid.ValidationError[5, ARowIndex, gveCellField] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningUnderGroundSectionValidator.UpdateHighExtractionAreaRunoffFactor(ARowIndex: integer;
                                                                                  AValue: string);
const OPNAME = 'TMiningUnderGroundSectionValidator.UpdateHighExtractionAreaRunoffFactor';
var
  LMine            : IMine;
  lValue           : string;
  lMessage         : string;
  LUndergroundID   : integer;
  LUndergroundMine : IUnderground;
begin
  try
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).
                     CastNetworkFeaturesData.CastMineList.MineByNodeNumber[FIdentifier];
    if (LMine <> nil) then
    begin
      with MiningUnderGroundSectionDialog do
      begin
         LUndergroundID := StrToInt(UnderGroundMiningGrid.Cells[0,ARowIndex]);
         LUndergroundMine := LMine.UnderGroundByIdentifier[LUndergroundID];
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'HighExtractionAreaRunoffFactor', AValue, lMessage, 6, ARowIndex)) then
        begin
          lValue := AValue;
          LUndergroundMine.HighExtractionAreaRunoffFactor := StrToFloat(lValue);
          RePopulateDataViewer;
//          DoContextValidation(dvtDivFeatureType3Proportions);}
        end
        else
          UnderGroundMiningGrid.ValidationError[6, ARowIndex, gveCellField] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningUnderGroundSectionValidator.OnMonthlyRechargeFactorsClick(Sender: TObject; ACol,
                                                                           ARow: Longint);
const OPNAME = 'TMiningUnderGroundSectionValidator.OnMonthlyRechargeFactorsClick';
var
  LForm              : TYieldModelDataGUIForm;
  LDialogValidator   : TMineMonthlyDataValidator;
  LMine              : IMine;
  LUndergroundMineID : integer;
begin
  try
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
             .CastMineList.MineByNodeNumber[FIdentifier];
    if (LMine <> nil) then
    begin
      if (LMine.UndergroundCount > 0) then
      begin
        LForm   := TYieldModelDataGUIForm.CreateWithoutDFM(nil,FAppModules);
        try
          LForm.Initialise;
          LForm.BtnCancel.Visible := False;
          LForm.LanguageHasChanged;
          LDialogValidator := TMineMonthlyDataValidator.Create(LForm,FAppModules);
          try
            LDialogValidator.Identifier := FIdentifier;
            if (ACol = 7) then
              LDialogValidator.RechargeFactorType := rftUnderGround_UpstreamRunoffPortion
            else
            if (ACol = 8) then
              LDialogValidator.RechargeFactorType := rftUnderGround_BoardAndPilarRechargeFactor
            else
            if (ACol = 9) then
              LDialogValidator.RechargeFactorType := rftUnderGround_HighExtractionRechargeFactor;

            LUndergroundMineID := StrToInt(MiningUnderGroundSectionDialog.UnderGroundMiningGrid.Cells[0,ARow]);
            LDialogValidator.ParentIdentifier := LUndergroundMineID;

            LForm.AddModelDataPanel(LDialogValidator.Panel);
            LDialogValidator.Initialise;
            LDialogValidator.PopulateDataViewer;
            LDialogValidator.LanguageHasChanged;

            LForm.ShowModal;
            if (LForm.ModalResult = mrOk) then
            begin
            end;
          finally
            LDialogValidator.Free;
          end;
        finally
          LForm.Free;
        end;
      end;
    end;    
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningUnderGroundSectionValidator.OnStringGridCellDataHasChanged(ASender: TObject;
                                                                            ACol, ARow: integer);
const OPNAME = 'TMiningUnderGroundSectionValidator.OnStringGridCellDataHasChanged';
var
  LMine : IMine;
begin
  inherited OnStringGridCellDataHasChanged(ASender, ACol, ARow);
  try
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
             .CastMineList.MineByNodeNumber[FIdentifier];
    if (LMine <> nil) then
    begin
      if (LMine.UndergroundCount > 0)  then
      begin
        with MiningUnderGroundSectionDialog do
        begin
          if ((UnderGroundMiningGrid = ASender) AND (ACol = 2)) then
            UpdateUndergroundSectionName(ARow, Trim(UnderGroundMiningGrid.Cells[2, ARow]))
          else
          if ((UnderGroundMiningGrid = ASender) AND (ACol = 3)) then
            UpdateUpstreamCatchmentArea(ARow, Trim(UnderGroundMiningGrid.Cells[3, ARow]))
          else
          if ((UnderGroundMiningGrid = ASender) AND (ACol = 4)) then
            UpdateBoardPillarCatchmentArea(ARow, Trim(UnderGroundMiningGrid.Cells[4, ARow]))
          else
          if ((UnderGroundMiningGrid = ASender) AND (ACol = 5)) then
            UpdateHighExtractionCatchmentArea(ARow, Trim(UnderGroundMiningGrid.Cells[5, ARow]))
          else
          if ((UnderGroundMiningGrid = ASender) AND (ACol = 6)) then
            UpdateHighExtractionAreaRunoffFactor(ARow, Trim(UnderGroundMiningGrid.Cells[6, ARow]))
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningUnderGroundSectionValidator.OnDeleteUndergroundSection(Sender: TObject);
const OPNAME = 'TMiningUnderGroundSectionValidator.OnDeleteUndergroundSection';
begin
  try
    if(FAppModules.Model as IYieldModel).DoDeleteUnderGround(FIdentifier,FSelectUGIdentifier) then
    begin
      RePopulateDataViewer;
      DoContextValidation(dvtMineAll);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningUnderGroundSectionValidator.OnInsertUndergroundSection(Sender: TObject);
const OPNAME = 'TMiningUnderGroundSectionValidator.OnInsertUndergroundSection';
begin
  try
    if((FAppModules.Model as IYieldModel).DoCreateUnderGround(FIdentifier) <> nil)then
    begin
      RePopulateDataViewer;
      DoContextValidation(dvtMineAll);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningUnderGroundSectionValidator.OnGridSelectCell(ASender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
const OPNAME = 'TMiningUnderGroundSectionValidator.OnGridSelectCell';
var
  LMine : IMine;
begin
  try
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
             .CastMineList.MineByNodeNumber[FIdentifier];
    if (LMine <> nil) then
    begin
      FSelectUGIdentifier := -1;
      if ASender = MiningUnderGroundSectionDialog.UnderGroundMiningGrid then
      begin
        if (LMine.UndergroundCount > 0) then
        begin
          FSelectUGIdentifier := StrToInt(MiningUnderGroundSectionDialog.UnderGroundMiningGrid.Cells[0,ARow]);
        end;
      end;
    end;
    MiningUnderGroundSectionDialog.ResetButtonState(LMine.UndergroundCount);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.


