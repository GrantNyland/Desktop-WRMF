{******************************************************************************}
{*  UNIT      : Contains the class TMineSubcatchmentValidator.                *}
{*  AUTHOR    : Presley Mudau                                                 *}
{*  DATE      : 2007/11/09                                                    *}
{*  COPYRIGHT : Copyright © 2007 DWAF                                         *}
{******************************************************************************}

unit UMineSubcatchmentValidator;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent,
  VoaimsCom_TLB,
  UParameterData,
  Contnrs,
  UMineSubcatchmentDialog,
  UAbstractYieldDataDialogValidator,
  UYieldContextValidationType;

type

  TMineSubcatchmentValidator = class(TAbstractYieldDataDialogValidator)
  private
  protected
    FSelectedCatchmentIndex : integer;
    FSelectedCatchmentRef   : integer;
    FSelectedID             : integer;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnMineSubCatchmentFactorsClick(Sender: TObject; ACol, ARow: Longint);
    procedure OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer); override;
    procedure OnMineSubcatchmentGridSelectCell(ASender: TObject;ACol, ARow: Longint;
                                          var CanSelect: Boolean);
    procedure UpdateProportionAntecedentFlow(ARowIndex : integer;
                                             AValue    : string);
    procedure UpdateGroundwaterFlowVolume(ARowIndex : integer;
                                             AValue    : string);
    procedure UpdateAntecedentRunoffDecayFactor(ARowIndex : integer;
                                             AValue    : string);
    procedure RePopulateDataViewer;
    procedure RePopulateMinimunGroundwaterFlowVolumeGrid;
    procedure RePopulateReservoirListBox;

    procedure ValidateNodeRefNumber(AMineSubCatchment: IMineSubCatchment);
    procedure ValidateProportionAntecedentFlow(AMineSubCatchment: IMineSubCatchment);
    procedure ValidateGroundwaterFlowVolume(AMineSubCatchment: IMineSubCatchment);

  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    procedure ClearDataViewer; override;
    procedure DoContextValidation (AValidationType : TDialogValidationType); override;
    procedure PopulateDataViewer; override;
    function DetermineWizardStatus (ASequence : integer = 0) : integer; override;
    function MineSubcatchmentDialog: TMineSubcatchmentDialog;
  end;

implementation

uses
  VCL.Grids,
  SysUtils,
  VCL.Graphics,
  UConstants,
  UUtilities,
  UYieldModelDataGUIForm,
  UYieldModelDataObject,
  UErrorHandlingOperations,
  USelectChannelDialog,
  UNetworkElementData,
  Math,
  DateUtils,
  UNetworkFeaturesData,
  UMineSubCatchmentFlowFactorsValidator,
  UMineSubCatchmentFlowFactorsDialog;

{******************************************************************************}
{* TMineSubcatchmentValidator                                                 *}
{******************************************************************************}

procedure TMineSubcatchmentValidator.CreateMemberObjects;
const OPNAME = 'TMineSubcatchmentValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FSelectedCatchmentIndex := 0;
    FPanel := TMineSubcatchmentDialog.Create(FPanelOwner,FAppModules);

    with MineSubcatchmentDialog do
    begin
      CatchmentRefNrEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('NrOfUnderGroundMining');
      CatchmentRefNrEdit.OnEnter       := OnEditControlEnter;
      CatchmentRefNrEdit.OnExit        :=  OnEditControltExit;

      MinimunGroundwaterFlowVolumeGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('MineCatchmentReferenceName'));
      MinimunGroundwaterFlowVolumeGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ProportionAntecedentFlow'));
      MinimunGroundwaterFlowVolumeGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('GroundwaterFlowVolume'));
      MinimunGroundwaterFlowVolumeGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('AntecedentRunoffDecayFactor'));

      MinimunGroundwaterFlowVolumeGrid.OnBeforeCellChange := OnStringGridCellDataHasChanged;
      MinimunGroundwaterFlowVolumeGrid.OnSelectCell       := OnMineSubcatchmentGridSelectCell;
      MinimunGroundwaterFlowVolumeGrid.OnColEnter         := OnStringGridColEnter;
      MinimunGroundwaterFlowVolumeGrid.OnEnter            := OnEditControlEnter;

      MinimunGroundwaterFlowVolumeGrid.ButtonColumn[4] := True;
      MinimunGroundwaterFlowVolumeGrid.ButtonColumnCaption[4] := FAppModules.Language.GetString('LabelText.ThreeDots');
      MinimunGroundwaterFlowVolumeGrid.ButtonColumnOnClick[4] := OnMineSubCatchmentFactorsClick;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSubcatchmentValidator.DestroyMemberObjects;
const OPNAME = 'TMineSubcatchmentValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSubcatchmentValidator.Initialise: boolean;
const OPNAME = 'TMineSubcatchmentValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSubcatchmentValidator.LanguageHasChanged: boolean;
const OPNAME = 'TMineSubcatchmentValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('ViewData.MineSubcatchment');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSubcatchmentValidator.ClearDataViewer;
const OPNAME = 'TMineSubcatchmentValidator.ClearDataViewer';
var
  lpPanel : TMineSubcatchmentDialog;
begin
  inherited ClearDataViewer;
  try
    lpPanel := MineSubcatchmentDialog;
    with lpPanel do
    begin
      CatchmentRefNrEdit.Text          := '-1';
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSubcatchmentValidator.PopulateDataViewer;
const OPNAME = 'TMineSubcatchmentValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtMineAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSubcatchmentValidator.MineSubcatchmentDialog : TMineSubcatchmentDialog;
const OPNAME = 'TMineSubcatchmentValidator.MineSubcatchmentDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TMineSubcatchmentDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSubcatchmentValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TMineSubcatchmentValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSubcatchmentValidator.StudyHasChanged: boolean;
const OPNAME = 'TMineSubcatchmentValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSubcatchmentValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TMineSubcatchmentValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSubcatchmentValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TMineSubcatchmentValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with MineSubcatchmentDialog do
    begin
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSubcatchmentValidator.OnMineSubCatchmentFactorsClick(Sender: TObject; ACol, ARow: Longint);
const OPNAME = 'TMineSubcatchmentValidator.OnMineSubCatchmentFactorsClick';
var
  LForm            : TYieldModelDataGUIForm;
  LDialogValidator : TMineSubCatchmentFlowFactorsValidator;
begin
  try
    LForm   := TYieldModelDataGUIForm.CreateWithoutDFM(nil,FAppModules);
    try
      LForm.Initialise;
      LForm.BtnCancel.Visible := False;
      LForm.LanguageHasChanged;
      LDialogValidator := TMineSubCatchmentFlowFactorsValidator.Create(LForm,FAppModules);
      try

        LDialogValidator.Identifier := FSelectedID;
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
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSubcatchmentValidator.DetermineWizardStatus (ASequence : integer = 0) : integer;
const OPNAME = 'TMineSubcatchmentValidator.DetermineWizardStatus';
begin
  Result := inherited DetermineWizardStatus(ASequence);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSubcatchmentValidator.RePopulateDataViewer;
const OPNAME = 'TMineSubcatchmentValidator.RePopulateDataViewer';
var
  LMineSubCatchmentList : IMineSubCatchmentList;
begin
  try
    LMineSubCatchmentList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData
                           .MineSubCatchmentList;
    if (LMineSubCatchmentList <> nil )then
    begin
      MineSubcatchmentDialog.CatchmentRefNrEdit.SetFieldValue(LMineSubCatchmentList.MineSubCatchmentCount);
      RePopulateMinimunGroundwaterFlowVolumeGrid;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSubcatchmentValidator.RePopulateMinimunGroundwaterFlowVolumeGrid;
const OPNAME = 'TMineSubcatchmentValidator.RePopulateMinimunGroundwaterFlowVolumeGrid';
var
  LIndex                : Integer;
  LMineSubCatchment     : IMineSubCatchment;
  LMineSubCatchmentList : IMineSubCatchmentList;
begin
  try
    LMineSubCatchmentList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData
                             .MineSubCatchmentList;
    if (LMineSubCatchmentList <> nil) then
    begin
      for LIndex := 0 to LMineSubCatchmentList.MineSubCatchmentCount -1 do
      begin
        LMineSubCatchment := LMineSubCatchmentList.MineSubCatchmentByIndex[LIndex];
        if (LMineSubCatchment <> nil) then
        begin
          MineSubcatchmentDialog.MinimunGroundwaterFlowVolumeGrid.ColCount := 5;
          MineSubcatchmentDialog.MinimunGroundwaterFlowVolumeGrid.RowCount :=
                                 Max((MineSubcatchmentDialog.MinimunGroundwaterFlowVolumeGrid.FixedRows + 1),
                                     (1 + LMineSubCatchmentList.MineSubCatchmentCount));

          MineSubcatchmentDialog.MinimunGroundwaterFlowVolumeGrid.Cells[0,LIndex+1] := LMineSubCatchment.CatchmentRefName;
          MineSubcatchmentDialog.MinimunGroundwaterFlowVolumeGrid.Cells[1,LIndex+1] := FloatToStr(LMineSubCatchment.ProportionAntecedentFlows);
          MineSubcatchmentDialog.MinimunGroundwaterFlowVolumeGrid.Cells[2,LIndex+1] := FloatToStr(LMineSubCatchment.GroundwaterFlowVolume);
          MineSubcatchmentDialog.MinimunGroundwaterFlowVolumeGrid.Cells[3,LIndex+1] := FloatToStr(LMineSubCatchment.AntecedentRunoffDecayFactor);
        end;
      end;

      if ((FSelectedCatchmentIndex = 0) AND (LMineSubCatchmentList.MineSubCatchmentCount > 0)) then
      begin
        FSelectedCatchmentIndex  := 0;
        FSelectedCatchmentRef    := LMineSubCatchmentList.MineSubCatchmentByIndex[0].CatchmentReferenceNr;
       end;
       RepopulateReservoirListBox;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSubcatchmentValidator.OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer);
const OPNAME = 'TMineSubcatchmentValidator.OnStringGridCellDataHasChanged';
var
  LMineSubcatchment : IMineSubCatchment;
begin
  inherited OnStringGridCellDataHasChanged(ASender, ACol, ARow);
  try
    LMineSubcatchment := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData
             .MineSubCatchmentList.MineSubCatchmentByIdentifier[FSelectedID];
    if (LMineSubcatchment <> nil) then
    begin
      with MineSubcatchmentDialog do
      begin
        if ((MinimunGroundwaterFlowVolumeGrid = ASender) AND (ACol = 1)) then
          UpdateProportionAntecedentFlow(ARow, Trim(MinimunGroundwaterFlowVolumeGrid.Cells[1, ARow]))
        else
        if ((MinimunGroundwaterFlowVolumeGrid = ASender) AND (ACol = 2)) then
          UpdateGroundwaterFlowVolume(ARow, Trim(MinimunGroundwaterFlowVolumeGrid.Cells[2, ARow]))
        else
        if ((MinimunGroundwaterFlowVolumeGrid = ASender) AND (ACol = 3)) then
          UpdateAntecedentRunoffDecayFactor(ARow, Trim(MinimunGroundwaterFlowVolumeGrid.Cells[3, ARow]))
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSubcatchmentValidator.OnMineSubcatchmentGridSelectCell(ASender: TObject; ACol, ARow: Integer;
                                                            var CanSelect: Boolean);
const OPNAME = 'TMineSubcatchmentValidator.OnMineSubcatchmentGridSelectCell';
var
  LMineSubCatchment     : IMineSubCatchment;
  LMineSubCatchmentList : IMineSubCatchmentList;
begin
  try
    CanSelect := True;
    LMineSubCatchmentList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData
                    .MineSubCatchmentList;
    with MineSubcatchmentDialog do
    begin
      if (ARow <> FSelectedCatchmentIndex) then
      begin
        FSelectedCatchmentIndex := ARow - 1;
        LMineSubCatchment := LMineSubCatchmentList.MineSubCatchmentByIndex[FSelectedCatchmentIndex];
        if LMineSubCatchment <> nil then
        begin
          FSelectedCatchmentRef   := LMineSubCatchment.CatchmentReferenceNr;
          FSelectedID             := LMineSubCatchment.Identifier;
        end;
        RePopulateReservoirListBox;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSubcatchmentValidator.SaveState: boolean;
const OPNAME = 'TMineSubcatchmentValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSubcatchmentValidator.UpdateProportionAntecedentFlow(ARowIndex : integer;
                                                                    AValue    : string);
const OPNAME = 'TMineSubcatchmentValidator.UpdateProportionAntecedentFlow';
var
  lValue                : string;
  lMessage              : string;
  LMineSubcatchment     : IMineSubCatchment;
  LMineSubcatchmentList : IMineSubCatchmentList;
begin
  try
    LMineSubcatchmentList := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.MineSubCatchmentList;
    if (LMineSubcatchmentList <> nil) then
    begin
      with MineSubcatchmentDialog do
      begin
        LMineSubcatchment := LMineSubcatchmentList.MineSubCatchmentByIndex[FSelectedCatchmentIndex];
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'ProportionAntecedentFlow', AValue, lMessage, 1, ARowIndex)) then
        begin
          lValue := AValue;
          LMineSubcatchment.ProportionAntecedentFlows := StrToFloat(lValue);
          RePopulateDataViewer;
//          DoContextValidation(dvtDivFeatureType3Proportions);}
        end
        else
          MinimunGroundwaterFlowVolumeGrid.ValidationError[1, ARowIndex, gveCellField] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSubcatchmentValidator.UpdateGroundwaterFlowVolume(ARowIndex: integer; AValue: string);
const OPNAME = 'TMineSubcatchmentValidator.UpdateGroundwaterFlowVolume';
var
  lValue                : string;
  lMessage              : string;
  LMineSubcatchment     : IMineSubCatchment;
  LMineSubcatchmentList : IMineSubCatchmentList;
begin
  try
    LMineSubcatchmentList := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.MineSubCatchmentList;
    if (LMineSubcatchmentList <> nil) then
    begin
      with MineSubcatchmentDialog do
      begin
        LMineSubcatchment := LMineSubcatchmentList.MineSubCatchmentByIndex[FSelectedCatchmentIndex];
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'GroundwaterFlowVolume', AValue, lMessage, 2, ARowIndex)) then
        begin
          lValue := AValue;
          LMineSubcatchment.GroundwaterFlowVolume := StrToFloat(lValue);
          RePopulateDataViewer;
//          DoContextValidation(dvtDivFeatureType3Proportions);}
        end
        else
          MinimunGroundwaterFlowVolumeGrid.ValidationError[2, ARowIndex, gveCellField] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSubcatchmentValidator.UpdateAntecedentRunoffDecayFactor(ARowIndex: integer; AValue: string);
const OPNAME = 'TMineSubcatchmentValidator.UpdateAntecedentRunoffDecayFactor';
var
  lValue                : string;
  lMessage              : string;
  LMineSubcatchment     : IMineSubCatchment;
  LMineSubcatchmentList : IMineSubCatchmentList;
begin
  try
    LMineSubcatchmentList := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.MineSubCatchmentList;
    if (LMineSubcatchmentList <> nil) then
    begin
      with MineSubcatchmentDialog do
      begin
        LMineSubcatchment := LMineSubcatchmentList.MineSubCatchmentByIndex[FSelectedCatchmentIndex];
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'AntecedentRunoffDecayFactor', AValue, lMessage, 3, ARowIndex)) then
        begin
          lValue := AValue;
          LMineSubcatchment.AntecedentRunoffDecayFactor := StrToFloat(lValue);
          RePopulateDataViewer;
//          DoContextValidation(dvtDivFeatureType3Proportions);}
        end
        else
          MinimunGroundwaterFlowVolumeGrid.ValidationError[3, ARowIndex, gveCellField] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSubcatchmentValidator.ValidateNodeRefNumber(AMineSubCatchment: IMineSubCatchment);
const OPNAME = 'TMineSubcatchmentValidator.ValidateNodeRefNumber';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSubcatchmentValidator.ValidateGroundwaterFlowVolume(AMineSubCatchment: IMineSubCatchment);
const OPNAME = 'TMineSubcatchmentValidator.ValidateGroundwaterFlowVolume';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSubcatchmentValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TMineSubcatchmentValidator.DoContextValidation';
begin
  try
    inherited;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSubcatchmentValidator.ValidateProportionAntecedentFlow(AMineSubCatchment: IMineSubCatchment);
const OPNAME = 'TMineSubcatchmentValidator.ValidateProportionAntecedentFlow';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TMineSubcatchmentValidator.RePopulateReservoirListBox;
const OPNAME = 'TMineSubcatchmentValidator.RePopulateReservoirListBox';
var
  lIndex             : integer;
  LReservoirData     : IReservoirData;
  LReservoirDataList : IReservoirDataList;
begin
  try
    with MineSubcatchmentDialog do
    begin
      ReservoirListBox.Items.Clear;
      if (FSelectedCatchmentRef > 0) then
      begin
        LReservoirDataList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData
                        .ReservoirList;
        if (LReservoirDataList <> nil ) then
        begin
          for lIndex := 0 to LReservoirDataList.ReservoirCount - 1  do
          begin
            LReservoirData := LReservoirDataList.ReservoirByIndex[lIndex];
            if ((LReservoirData <> nil) AND
              (LReservoirData.ReservoirConfigurationData.CatchmentRef = FSelectedCatchmentRef)) then
            ReservoirListBox.Items.Add('(' + IntToStr(LReservoirData.ReservoirConfigurationData.ReservoirIdentifier) + ') ' +
                                      LReservoirData.ReservoirConfigurationData.ReservoirName);
          end
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

