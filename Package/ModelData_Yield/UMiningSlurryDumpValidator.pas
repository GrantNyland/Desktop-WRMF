{******************************************************************************}
//  UNIT      : Contains the class TMiningValidator.
//  AUTHOR    : Presley Mudau
//  DATE      : 2007/03/09
//  COPYRIGHT : Copyright © 2007 DWAF
{******************************************************************************}

unit UMiningSlurryDumpValidator;

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
  UMiningSlurryDumpDialog,
  UAbstractYieldDataDialogValidator,
  UYieldContextValidationType;
type
  TMiningSlurryDumpValidator = class(TAbstractYieldDataDialogValidator)
  protected
    FIdentifier     : integer;
    FSelectSlurryID : integer;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer); override;
    procedure OnGridSelectCell(ASender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure OnMonthlyRechargeFactorsClick(Sender: TObject; ACol, ARow: Longint);
    procedure OnInsertSlurryDump(Sender: TObject);
    procedure OnDeleteSlurryDump(Sender: TObject);
    procedure UpdateDumpName(ARowIndex : integer;
                             AValue    : string);
    procedure UpdateDumpSurfaceArea(ARowIndex : integer;
                                    AValue    : string);
    procedure UpdateRunoffFactorToPCD(ARowIndex : integer;
                                    AValue    : string);
    procedure UpdateSeepageSplitFactor(ARowIndex : integer;
                                    AValue    : string);
    procedure UpdateDumpPCDStorageCapacity(ARowIndex : integer;
                                    AValue    : string);
    procedure UpdateDumpPCDSurfaceArea(ARowIndex : integer;
                                    AValue    : string);
    procedure UpdateDumpPCDAnalysisStartVolume(ARowIndex : integer;
                                    AValue    : string);
    procedure PopulateSlurryDumpGrid;
    procedure RePopulateDataViewer;

  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;

    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;

    function MiningSlurryDumpDialog: TMiningSlurryDumpDialog;
//    procedure DoContextValidation (AValidationType : TDialogValidationType); override;
    property Identifier : integer read FIdentifier write FIdentifier;
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
  UErrorHandlingOperations, Math, UNetworkFeaturesData, UMiningData;

{ TMiningValidator }

procedure TMiningSlurryDumpValidator.CreateMemberObjects;
const OPNAME = 'TMiningSlurryDumpValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FIdentifier := -1;
    FPanel := TMiningSlurryDumpDialog.Create(FPanelOwner,FAppModules);
    with MiningSlurryDumpDialog do
    begin
      NumberOfSlurryDumpEdt.FieldProperty  := FAppModules.FieldProperties.FieldProperty('NrOfSlurryDump');
      NumberOfSlurryDumpEdt.OnEnter        := OnEditControlEnter;
      NumberOfSlurryDumpEdt.OnExit         := OnEditControltExit;
      NumberOfSlurryDumpEdt.IsEnabled      := False;

      SlurryDumpGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('NrOfSlurryDump'));
      SlurryDumpGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('DumpName'));
      SlurryDumpGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('DumpSurfaceArea'));
      SlurryDumpGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('RunoffFactorToPCD'));
      SlurryDumpGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('SeepageSplitFactor'));
      SlurryDumpGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('DumpPCDStorageCapacity'));
      SlurryDumpGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('DumpPCDSurfaceArea'));
      SlurryDumpGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('DumpPCDAnalysisStartVolume'));
      SlurryDumpGrid.OnBeforeCellChange   := OnStringGridCellDataHasChanged;
      SlurryDumpGrid.OnSelectCell         := OnGridSelectCell;
      SlurryDumpGrid.OnColEnter           := OnStringGridColEnter;
      SlurryDumpGrid.OnExit               := OnEditControltExit;
      SlurryDumpGrid.OnEnter              := OnEditControlEnter;

      SlurryDumpGrid.ButtonColumn[8] := True;
      SlurryDumpGrid.ButtonColumnCaption[8] := FAppModules.Language.GetString('LabelText.ThreeDots');
      SlurryDumpGrid.ButtonColumnOnClick[8] := OnMonthlyRechargeFactorsClick;

      InsertSlurryBtn.OnClick              := OnInsertSlurryDump;
      DeleteSlurryBtn.OnClick              := OnDeleteSlurryDump;

    end;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningSlurryDumpValidator.DestroyMemberObjects;
const OPNAME = 'TMiningSlurryDumpValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMiningSlurryDumpValidator.Initialise: boolean;
const OPNAME = 'TMiningSlurryDumpValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMiningSlurryDumpValidator.LanguageHasChanged: boolean;
const OPNAME = 'TMiningSlurryDumpValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := 'Discard/Slurry Dump';
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningSlurryDumpValidator.ClearDataViewer;
const OPNAME = 'TMiningSlurryDumpValidator.ClearDataViewer';
begin
  inherited ClearDataViewer;
  try
    MiningSlurryDumpDialog.NumberOfSlurryDumpEdt.SetFieldValue('');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningSlurryDumpValidator.RePopulateDataViewer;
const OPNAME = 'TMiningSlurryDumpValidator.RePopulateDataViewer';
var
  LMine  : IMine;
begin
  try
    if(FIdentifier >= 0) then
    begin
      LMine := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
                       .CastMineList.MineByNodeNumber[FIdentifier];
      if (LMine <> nil) then
      begin
        with MiningSlurryDumpDialog do
        begin
          NumberOfSlurryDumpEdt.SetFieldValue(LMine.SlurryDumpCount);
          PopulateSlurryDumpGrid;
        end;
        MiningSlurryDumpDialog.ResetButtonState(LMine.SlurryDumpCount);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMiningSlurryDumpValidator.SaveState: boolean;
const OPNAME = 'TMiningSlurryDumpValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningSlurryDumpValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TMiningSlurryDumpValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningSlurryDumpValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TMiningSlurryDumpValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningSlurryDumpValidator.OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer);
const OPNAME = 'TMiningSlurryDumpValidator.OnStringGridCellDataHasChanged';
var
  LMine : IMine;
begin
  inherited OnStringGridCellDataHasChanged(ASender, ACol, ARow);
  try
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
             .CastMineList.MineByNodeNumber[FIdentifier];
    if (LMine <> nil) then
    begin
      if (LMine.SlurryDumpCount > 0)  then
      begin
        with MiningSlurryDumpDialog do
        begin
          if ((SlurryDumpGrid = ASender) AND (ACol = 1)) then
            UpdateDumpName(ARow, Trim(SlurryDumpGrid.Cells[1, ARow]))
          else
          if ((SlurryDumpGrid = ASender) AND (ACol = 2)) then
            UpdateDumpSurfaceArea(ARow, Trim(SlurryDumpGrid.Cells[2, ARow]))
          else
          if ((SlurryDumpGrid = ASender) AND (ACol = 3)) then
            UpdateRunoffFactorToPCD(ARow, Trim(SlurryDumpGrid.Cells[3, ARow]))
          else
          if ((SlurryDumpGrid = ASender) AND (ACol = 4)) then
            UpdateSeepageSplitFactor(ARow, Trim(SlurryDumpGrid.Cells[4, ARow]))
          else
          if ((SlurryDumpGrid = ASender) AND (ACol = 5)) then
            UpdateDumpPCDStorageCapacity(ARow, Trim(SlurryDumpGrid.Cells[5, ARow]))
          else
          if ((SlurryDumpGrid = ASender) AND (ACol = 6)) then
            UpdateDumpPCDSurfaceArea(ARow, Trim(SlurryDumpGrid.Cells[6, ARow]))
          else
          if ((SlurryDumpGrid = ASender) AND (ACol = 7)) then
            UpdateDumpPCDAnalysisStartVolume(ARow, Trim(SlurryDumpGrid.Cells[7, ARow]))
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TMiningSlurryDumpValidator.MiningSlurryDumpDialog :TMiningSlurryDumpDialog;
const OPNAME = 'TMiningSlurryDumpValidator.MiningSlurryDumpDialog';
begin
  Result := Nil;
  try
    if Assigned(FPanel) then
      Result := TMiningSlurryDumpDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMiningSlurryDumpValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TMiningSlurryDumpValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if (AFieldName = 'Slurry') then
      RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMiningSlurryDumpValidator.StudyHasChanged: boolean;
const OPNAME = 'TMiningSlurryDumpValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningSlurryDumpValidator.PopulateDataViewer;
const OPNAME = 'TMiningSlurryDumpValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
//   DoContextValidation(dvtResPropAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningSlurryDumpValidator.PopulateSlurryDumpGrid;
const OPNAME = 'TMiningSlurryDumpValidator.PopulateSlurryDumpGrid';
var
  LIndex            : integer;
  LSlurryDump       : ISlurryDump;
  LMine             : IMine;
begin
  try
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList
             .MineByNodeNumber[FIdentifier];
    with MiningSlurryDumpDialog do
    begin

      SlurryDumpGrid.ColCount := 9;
      SlurryDumpGrid.RowCount := Max((SlurryDumpGrid.FixedRows + 1),(1 + LMine.SlurryDumpCount));;

      if (LMine.SlurryDumpCount > 0) then
      begin
        for LIndex := 1 to LMine.SlurryDumpCount do
        begin
          LSlurryDump := LMine.SlurryDumpByIndex[LIndex-1];
          SlurryDumpGrid.SetFieldValue(0, LIndex,LSlurryDump.Identifier);
          SlurryDumpGrid.SetFieldValue(1, LIndex,LSlurryDump.DumpName);
          SlurryDumpGrid.SetFieldValue(2, LIndex,LSlurryDump.DumpSurfaceArea);
          SlurryDumpGrid.SetFieldValue(3, LIndex,LSlurryDump.RunoffFactorToPCD);
          SlurryDumpGrid.SetFieldValue(4, LIndex,LSlurryDump.SeepageSplitFactor);
          SlurryDumpGrid.SetFieldValue(5, LIndex,LSlurryDump.PCDStorageCapacity);
          SlurryDumpGrid.SetFieldValue(6, LIndex,LSlurryDump.PCDSurfaceArea);
          SlurryDumpGrid.SetFieldValue(7, LIndex,LSlurryDump.PCDAnalysisStartVolume);
          SlurryDumpGrid.Options := SlurryDumpGrid.Options + [goEditing];
        end;
      end
      else
      begin
        SlurryDumpGrid.Cells[0, 1] := '';
        SlurryDumpGrid.Cells[1, 1] := '';
        SlurryDumpGrid.Cells[2, 1] := '';
        SlurryDumpGrid.Cells[3, 1] := '';
        SlurryDumpGrid.Cells[4, 1] := '';
        SlurryDumpGrid.Cells[5, 1] := '';
        SlurryDumpGrid.Cells[6, 1] := '';
        SlurryDumpGrid.Cells[7, 1] := '';

        SlurryDumpGrid.Options := SlurryDumpGrid.Options - [goEditing];
      end;
     end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningSlurryDumpValidator.OnMonthlyRechargeFactorsClick(Sender: TObject; ACol, ARow: Longint);
const OPNAME = 'TMiningSlurryDumpValidator.OnMonthlyRechargeFactorsClick';
var
  LForm            : TYieldModelDataGUIForm;
  LDialogValidator : TMineMonthlyDataValidator;
  LSlurryID        : integer;
  LMine            : IMine;
begin
  try
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
             .CastMineList.MineByNodeNumber[FIdentifier];
    if (LMine <> nil) then
    begin
      if (LMine.SlurryDumpCount > 0) then
      begin
        LForm   := TYieldModelDataGUIForm.CreateWithoutDFM(nil,FAppModules);
        try
          LForm.Initialise;
          LForm.BtnCancel.Visible := False;
          LForm.LanguageHasChanged;
          LDialogValidator := TMineMonthlyDataValidator.Create(LForm,FAppModules);
          try
            LDialogValidator.Identifier := FIdentifier;
            LSlurryID := StrToInt(MiningSlurryDumpDialog.SlurryDumpGrid.Cells[0,ARow]);
            LDialogValidator.ParentIdentifier := LSlurryID;
            LDialogValidator.RechargeFactorType := rftSlurryDump_RechargeFactor;
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

procedure TMiningSlurryDumpValidator.UpdateDumpName(ARowIndex : integer;
                                                    AValue    : string);
const OPNAME = 'TMiningSlurryDumpValidator.UpdateDumpName';
var
  LMine : IMine;
  lValue      : string;
  lMessage    : string;
  LDumpID      : integer;
  LSlurryDump : ISlurryDump;
begin
  try
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).
                     CastNetworkFeaturesData.CastMineList.MineByNodeNumber[FIdentifier];
    if (LMine <> nil) then
    begin
      with MiningSlurryDumpDialog do
      begin
         LDumpID := StrToInt(SlurryDumpGrid.Cells[0,ARowIndex]);
         LSlurryDump := LMine.SlurryDumpByIdentifier[LDumpID];
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'DumpName', AValue, lMessage, 0, ARowIndex)) then
        begin
          lValue := AValue;
          LSlurryDump.DumpName := lValue;
          RePopulateDataViewer;
//          DoContextValidation(dvtDivFeatureType3Proportions);}
        end
        else
          SlurryDumpGrid.ValidationError[0, ARowIndex, gveCellField] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningSlurryDumpValidator.UpdateDumpSurfaceArea(ARowIndex: integer; AValue: string);
const OPNAME = 'TMiningSlurryDumpValidator.UpdateDumpSurfaceArea';
var
  LMine : IMine;
  lValue      : string;
  lMessage    : string;
  LDumpID      : integer;
  LSlurryDump : ISlurryDump;
begin
  try
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).
                     CastNetworkFeaturesData.CastMineList.MineByNodeNumber[FIdentifier];
    if (LMine <> nil) then
    begin
      with MiningSlurryDumpDialog do
      begin
         LDumpID := StrToInt(SlurryDumpGrid.Cells[0,ARowIndex]);
         LSlurryDump := LMine.SlurryDumpByIdentifier[LDumpID];
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'DumpSurfaceArea', AValue, lMessage, 1, ARowIndex)) then
        begin
          lValue := AValue;
          LSlurryDump.DumpSurfaceArea := StrToFloat(lValue);
          RePopulateDataViewer;
//          DoContextValidation(dvtDivFeatureType3Proportions);}
        end
        else
          SlurryDumpGrid.ValidationError[1, ARowIndex, gveCellField] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningSlurryDumpValidator.UpdateDumpPCDAnalysisStartVolume(ARowIndex: integer; AValue: string);
const OPNAME = 'TMiningSlurryDumpValidator.UpdateDumpPCDAnalysisStartVolume';
var
  LMine : IMine;
  lValue      : string;
  lMessage    : string;
  LDumpID      : integer;
  LSlurryDump : ISlurryDump;
begin
  try
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).
                     CastNetworkFeaturesData.CastMineList.MineByNodeNumber[FIdentifier];
    if (LMine <> nil) then
    begin
      with MiningSlurryDumpDialog do
      begin
         LDumpID := StrToInt(SlurryDumpGrid.Cells[0,ARowIndex]);
         LSlurryDump := LMine.SlurryDumpByIdentifier[LDumpID];
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'DumpPCDAnalysisStartVolume', AValue, lMessage, 2, ARowIndex)) then
        begin
          lValue := AValue;
          LSlurryDump.PCDAnalysisStartVolume := StrToFloat(lValue);
          RePopulateDataViewer;
//          DoContextValidation(dvtDivFeatureType3Proportions);}
        end
        else
          SlurryDumpGrid.ValidationError[1, ARowIndex, gveCellField] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningSlurryDumpValidator.UpdateDumpPCDStorageCapacity(ARowIndex: integer; AValue: string);
const OPNAME = 'TMiningSlurryDumpValidator.UpdateDumpPCDStorageCapacity';
var
  LMine : IMine;
  lValue      : string;
  lMessage    : string;
  LDumpID      : integer;
  LSlurryDump : ISlurryDump;
begin
  try
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).
                     CastNetworkFeaturesData.CastMineList.MineByNodeNumber[FIdentifier];
    if (LMine <> nil) then
    begin
      with MiningSlurryDumpDialog do
      begin
         LDumpID := StrToInt(SlurryDumpGrid.Cells[0,ARowIndex]);
         LSlurryDump := LMine.SlurryDumpByIdentifier[LDumpID];
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'DumpPCDStorageCapacity', AValue, lMessage, 3, ARowIndex)) then
        begin
          lValue := AValue;
          LSlurryDump.PCDStorageCapacity := StrToFloat(lValue);
          RePopulateDataViewer;
//          DoContextValidation(dvtDivFeatureType3Proportions);}
        end
        else
          SlurryDumpGrid.ValidationError[1, ARowIndex, gveCellField] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningSlurryDumpValidator.UpdateDumpPCDSurfaceArea(ARowIndex: integer; AValue: string);
const OPNAME = 'TMiningSlurryDumpValidator.UpdateDumpPCDSurfaceArea';
var
  LMine : IMine;
  lValue      : string;
  lMessage    : string;
  LDumpID      : integer;
  LSlurryDump : ISlurryDump;
begin
  try
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).
                     CastNetworkFeaturesData.CastMineList.MineByNodeNumber[FIdentifier];
    if (LMine <> nil) then
    begin
      with MiningSlurryDumpDialog do
      begin
         LDumpID := StrToInt(SlurryDumpGrid.Cells[0,ARowIndex]);
         LSlurryDump := LMine.SlurryDumpByIdentifier[LDumpID];
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'DumpPCDSurfaceArea', AValue, lMessage, 4, ARowIndex)) then
        begin
          lValue := AValue;
          LSlurryDump.PCDSurfaceArea := StrToFloat(lValue);
          RePopulateDataViewer;
//          DoContextValidation(dvtDivFeatureType3Proportions);}
        end
        else
          SlurryDumpGrid.ValidationError[1, ARowIndex, gveCellField] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningSlurryDumpValidator.UpdateRunoffFactorToPCD(ARowIndex: integer; AValue: string);
const OPNAME = 'TMiningSlurryDumpValidator.UpdateRunoffFactorToPCD';
var
  LMine : IMine;
  lValue      : string;
  lMessage    : string;
  LDumpID      : integer;
  LSlurryDump : ISlurryDump;
begin
  try
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).
                     CastNetworkFeaturesData.CastMineList.MineByNodeNumber[FIdentifier];
    if (LMine <> nil) then
    begin
      with MiningSlurryDumpDialog do
      begin
         LDumpID := StrToInt(SlurryDumpGrid.Cells[0,ARowIndex]);
         LSlurryDump := LMine.SlurryDumpByIdentifier[LDumpID];
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'RunoffFactorToPCD', AValue, lMessage, 5, ARowIndex)) then
        begin
          lValue := AValue;
          LSlurryDump.RunoffFactorToPCD := StrToFloat(lValue);
          RePopulateDataViewer;
//          DoContextValidation(dvtDivFeatureType3Proportions);}
        end
        else
          SlurryDumpGrid.ValidationError[1, ARowIndex, gveCellField] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningSlurryDumpValidator.UpdateSeepageSplitFactor(ARowIndex: integer; AValue: string);
const OPNAME = 'TMiningSlurryDumpValidator.UpdateSeepageSplitFactor';
var
  LMine : IMine;
  lValue      : string;
  lMessage    : string;
  LDumpID      : integer;
  LSlurryDump : ISlurryDump;
begin
  try
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).
                     CastNetworkFeaturesData.CastMineList.MineByNodeNumber[FIdentifier];
    if (LMine <> nil) then
    begin
      with MiningSlurryDumpDialog do
      begin
         LDumpID := StrToInt(SlurryDumpGrid.Cells[0,ARowIndex]);
         LSlurryDump := LMine.SlurryDumpByIdentifier[LDumpID];
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'SeepageSplitFactor', AValue, lMessage, 6, ARowIndex)) then
        begin
          lValue := AValue;
          LSlurryDump.SeepageSplitFactor := StrToFloat(lValue);
          RePopulateDataViewer;
//          DoContextValidation(dvtDivFeatureType3Proportions);}
        end
        else
          SlurryDumpGrid.ValidationError[1, ARowIndex, gveCellField] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningSlurryDumpValidator.OnDeleteSlurryDump(Sender: TObject);
const OPNAME = 'TMiningSlurryDumpValidator.OnDeleteSlurryDump';
begin
  try
    if(FAppModules.Model as IYieldModel).DoDeleteSlurryDump(FIdentifier,FSelectSlurryID) then
    begin
      RePopulateDataViewer;
      DoContextValidation(dvtMineAll);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningSlurryDumpValidator.OnInsertSlurryDump(Sender: TObject);
const OPNAME = 'TMiningSlurryDumpValidator.OnInsertSlurryDump';
begin
  try
    if((FAppModules.Model as IYieldModel).DoCreateSlurryDump(FIdentifier) <> nil)then
    begin
      RePopulateDataViewer;
      DoContextValidation(dvtYMDCReturnFlowFeatureAll);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningSlurryDumpValidator.OnGridSelectCell(ASender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
const OPNAME = 'TMiningSlurryDumpValidator.OnGridSelectCell';
var
  LMine : IMine;
begin
  try
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
             .CastMineList.MineByNodeNumber[FIdentifier];
    if (LMine <> nil) then
    begin
      FSelectSlurryID := -1;
      if ASender = MiningSlurryDumpDialog.SlurryDumpGrid then
      begin
        if (LMine.SlurryDumpCount > 0) then
        begin
          FSelectSlurryID := StrToInt(MiningSlurryDumpDialog.SlurryDumpGrid.Cells[0,ARow]);
        end;
      end;
    end;
    MiningSlurryDumpDialog.ResetButtonState(LMine.SlurryDumpCount);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.


