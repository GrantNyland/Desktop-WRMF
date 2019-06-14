{******************************************************************************}
//  UNIT      : Contains the class TMiningOpenCastPitValidator.
//  AUTHOR    : Presley Mudau
//  DATE      : 2007/03/12
//  COPYRIGHT : Copyright © 2007 DWAF
{******************************************************************************}

unit UMiningOpenCastPitValidator;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  VCL.Dialogs,
  VCL.Grids,

  // arivia.kom
  VoaimsCom_TLB,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent,
  UMiningOpenCastPitDialog,
  UAbstractYieldDataDialogValidator,
  UYieldContextValidationType;
type
  TMiningOpenCastPitValidator = class(TAbstractYieldDataDialogValidator)
  protected
    FIdentifier       : integer;
    FSelectOpenCastID : integer;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnStringGridCellDataHasChanged (ASender: TObject; ACol, ARow: integer); override;
    procedure OnGridSelectCell(ASender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure OnMonthlyRechargeFactorsClick(Sender: TObject; ACol, ARow: Longint);
    procedure OnInsertOpenCast(Sender: TObject);
    procedure OnDeleteOpenCast(Sender: TObject);
    procedure PopulateOpenCastPitsGrid;

    procedure UpdatePitName(ARowIndex : integer;AValue : string);
    procedure UpdateCoalReserveArea(ARowIndex : integer;AValue : string);
    procedure UpdateWorkingsArea(ARowIndex : integer;AValue : string);
    procedure UpdateDisturbedWorkingsArea(ARowIndex : integer;AValue : string);
    procedure UpdateDisturbedArea(ARowIndex : integer;AValue : string);
    procedure UpdateWaterSurfaceEvapArea(ARowIndex : integer;AValue : string);
    procedure UpdateDisturbedAreaRunoff(ARowIndex : integer;AValue : string);
    procedure UpdateDisturbedWorkingsAreaRunoff(ARowIndex : integer;AValue : string);
    procedure UpdateDecantVolume(ARowIndex : integer;AValue : string);
    procedure UpdateSeepageVolume(ARowIndex : integer;AValue : string);
    procedure UpdateAnalysisStartVolume(ARowIndex : integer;AValue : string);
    procedure UpdateMaximumSeepageRate(ARowIndex : integer;AValue : string);
    procedure UpdateSeepageExponent(ARowIndex : integer;AValue : string);
    procedure UpdatePCDSurfaceArea(ARowIndex : integer;AValue : string);
    procedure UpdatePCDStorageCapacity(ARowIndex : integer;AValue : string);
    procedure UpdatePCDAnalysisStartVolume(ARowIndex : integer;AValue : string);

    procedure ValidatePitName(AOpenCastData : IOpenCast);
    procedure ValidateCoalReserveArea(AOpenCastData : IOpenCast);
    procedure ValidateWorkingsArea(AOpenCastData : IOpenCast);
    procedure ValidateDisturbedWorkingsArea(AOpenCastData : IOpenCast);
    procedure ValidateDisturbedArea(AOpenCastData : IOpenCast);
    procedure ValidateWaterSurfaceEvapArea(AOpenCastData : IOpenCast);
    procedure ValidateDisturbedAreaRunoff(AOpenCastData : IOpenCast);
    procedure ValidateDisturbedWorkingsAreaRunoff(AOpenCastData : IOpenCast);
    procedure ValidateDecantVolume(AOpenCastData : IOpenCast);
    procedure ValidateSeepageVolume(AOpenCastData : IOpenCast);
    procedure ValidateAnalysisStartVolume(AOpenCastData : IOpenCast);
    procedure ValidateMaximumSeepageRate(AOpenCastData : IOpenCast);
    procedure ValidateSeepageExponent(AOpenCastData : IOpenCast);
    procedure ValidatePCDSurfaceArea(AOpenCastData : IOpenCast);
    procedure ValidatePCDStorageCapacity(AOpenCastData : IOpenCast);
    procedure ValidatePCDAnalysisStartVolume(AOpenCastData : IOpenCast);
    procedure RePopulateDataViewer;

  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    function MiningOpenCastPitDialog: TMiningOpenCastPitDialog;
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
  UMiningData,
  UYieldModelDataObject,
  UErrorHandlingOperations, Math, UNetworkFeaturesData;

{ TMiningValidator }

procedure TMiningOpenCastPitValidator.CreateMemberObjects;
const OPNAME = 'TMiningOpenCastPitValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FIdentifier := -1;
    FPanel := TMiningOpenCastPitDialog.Create(FPanelOwner,FAppModules);

    with MiningOpenCastPitDialog do
    begin
      NumberOfOpenCastPitsEdt.FieldProperty    := FAppModules.FieldProperties.FieldProperty('NrOfMineCastPits');
      NumberOfOpenCastPitsEdt.OnEnter          := OnEditControlEnter;
      NumberOfOpenCastPitsEdt.OnExit           := OnEditControltExit;

      OpenCastPitsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('NrOfMineCastPits'));
      OpenCastPitsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('PitName'));
      OpenCastPitsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('CoalReserveArea'));
      OpenCastPitsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('WorkingsArea'));
      OpenCastPitsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('DisturbedWorkingsArea'));
      OpenCastPitsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('DisturbedArea'));
      OpenCastPitsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('WaterSurfaceEvapArea'));
      OpenCastPitsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('DisturbedAreaRunOff'));
      OpenCastPitsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('DisturbedWorkingsAreaRunOff'));
      OpenCastPitsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('DecantVolume'));
      OpenCastPitsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('SeepageVolume'));
      OpenCastPitsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('AnalysisStartVolume'));
      OpenCastPitsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('MaximumSeepageRate'));
      OpenCastPitsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('SeepageExponent'));
      OpenCastPitsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('OpenCastPCDSurfaceArea'));
      OpenCastPitsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('OpenCastPCDStorageCapacity'));
      OpenCastPitsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('OpenCastPCDAnalysisStartVolume'));
      OpenCastPitsGrid.OnBeforeCellChange   := OnStringGridCellDataHasChanged;
      OpenCastPitsGrid.OnSelectCell         := OnGridSelectCell;
      OpenCastPitsGrid.OnColEnter           := OnStringGridColEnter;
      OpenCastPitsGrid.OnEnter              := OnEditControlEnter;

      InsertOpenCastBtn.OnClick                := OnInsertOpenCast;
      DeleteOpenCastBtn.OnClick                := OnDeleteOpenCast;
  
      OpenCastPitsGrid.ButtonColumn[17] := True;
      OpenCastPitsGrid.ButtonColumnCaption[17] := FAppModules.Language.GetString('LabelText.ThreeDots');
      OpenCastPitsGrid.ButtonColumnOnClick[17] := OnMonthlyRechargeFactorsClick;

      OpenCastPitsGrid.ButtonColumn[18] := True;
      OpenCastPitsGrid.ButtonColumnCaption[18] := FAppModules.Language.GetString('LabelText.ThreeDots');
      OpenCastPitsGrid.ButtonColumnOnClick[18] := OnMonthlyRechargeFactorsClick;
    end;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningOpenCastPitValidator.DestroyMemberObjects;
const OPNAME = 'TMiningOpenCastPitValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMiningOpenCastPitValidator.Initialise: boolean;
const OPNAME = 'TMiningOpenCastPitValidator.Initialise';
begin
  Result := inherited Initialise;
  try
    with MiningOpenCastPitDialog do
    begin
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMiningOpenCastPitValidator.LanguageHasChanged: boolean;
const OPNAME = 'TMiningOpenCastPitValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := 'Open Cast Pits';
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningOpenCastPitValidator.ClearDataViewer;
const OPNAME = 'TMiningOpenCastPitValidator.ClearDataViewer';
begin
  inherited ClearDataViewer;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningOpenCastPitValidator.RePopulateDataViewer;
const OPNAME = 'TMiningOpenCastPitValidator.RePopulateDataViewer';
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
        with MiningOpenCastPitDialog do
        begin
          NumberOfOpenCastPitsEdt.SetFieldValue(LMine.OpenCastCount);
          PopulateOpenCastPitsGrid;
        end;
        MiningOpenCastPitDialog.ResetButtonState(LMine.OpenCastCount);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMiningOpenCastPitValidator.SaveState: boolean;
const OPNAME = 'TMiningOpenCastPitValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningOpenCastPitValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TMiningOpenCastPitValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningOpenCastPitValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TMiningOpenCastPitValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TMiningOpenCastPitValidator.OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer);
const OPNAME = 'TMiningOpenCastPitValidator.OnStringGridCellDataHasChanged';
var
  LMine : IMine;
begin
  inherited OnStringGridCellDataHasChanged(ASender, ACol, ARow);
  try
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
             .CastMineList.MineByNodeNumber[FIdentifier];
    if (LMine <> nil) then
    begin
      if (LMine.OpenCastCount > 0)  then
      begin
        with MiningOpenCastPitDialog do
        begin
          if (OpenCastPitsGrid = ASender) AND (ACol = 1) then
            UpdatePitName(ARow, Trim(OpenCastPitsGrid.Cells[1, ARow]))
          else
          if (OpenCastPitsGrid = ASender) AND (ACol = 2) then
            UpdateCoalReserveArea(ARow, Trim(OpenCastPitsGrid.Cells[2, ARow]))
          else
          if (OpenCastPitsGrid = ASender) AND (ACol = 3) then
            UpdateWorkingsArea(ARow, Trim(OpenCastPitsGrid.Cells[3, ARow]))
          else
          if (OpenCastPitsGrid = ASender) AND (ACol = 4) then
            UpdateDisturbedWorkingsArea(ARow, Trim(OpenCastPitsGrid.Cells[4, ARow]))
          else
          if (OpenCastPitsGrid = ASender) AND (ACol = 5) then
            UpdateDisturbedArea(ARow, Trim(OpenCastPitsGrid.Cells[5, ARow]))
          else
          if (OpenCastPitsGrid = ASender) AND (ACol = 6) then
            UpdateWaterSurfaceEvapArea(ARow, Trim(OpenCastPitsGrid.Cells[6, ARow]))
          else
          if (OpenCastPitsGrid = ASender) AND (ACol = 7) then
            UpdateDisturbedAreaRunoff(ARow, Trim(OpenCastPitsGrid.Cells[7, ARow]))
          else
          if (OpenCastPitsGrid = ASender) AND (ACol = 8) then
            UpdateDisturbedWorkingsAreaRunoff(ARow, Trim(OpenCastPitsGrid.Cells[8, ARow]))
          else
          if (OpenCastPitsGrid = ASender) AND (ACol = 9) then
            UpdateDecantVolume(ARow, Trim(OpenCastPitsGrid.Cells[9, ARow]))
          else
          if (OpenCastPitsGrid = ASender) AND (ACol = 10) then
            UpdateSeepageVolume(ARow, Trim(OpenCastPitsGrid.Cells[10, ARow]))
          else
          if (OpenCastPitsGrid = ASender) AND (ACol = 11) then
            UpdateAnalysisStartVolume(ARow, Trim(OpenCastPitsGrid.Cells[11, ARow]))
          else
          if (OpenCastPitsGrid = ASender) AND (ACol = 12) then
            UpdateMaximumSeepageRate(ARow, Trim(OpenCastPitsGrid.Cells[12, ARow]))
          else
          if (OpenCastPitsGrid = ASender) AND (ACol = 13) then
            UpdateSeepageExponent(ARow, Trim(OpenCastPitsGrid.Cells[13, ARow]))
          else
          if (OpenCastPitsGrid = ASender) AND (ACol = 14) then
            UpdatePCDSurfaceArea(ARow, Trim(OpenCastPitsGrid.Cells[14, ARow]))
          else
          if (OpenCastPitsGrid = ASender) AND (ACol = 15) then
            UpdatePCDStorageCapacity(ARow, Trim(OpenCastPitsGrid.Cells[15, ARow]))
          else
          if (OpenCastPitsGrid = ASender) AND (ACol = 16) then
            UpdatePCDAnalysisStartVolume(ARow, Trim(OpenCastPitsGrid.Cells[16, ARow]))
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMiningOpenCastPitValidator.MiningOpenCastPitDialog :TMiningOpenCastPitDialog;
const OPNAME = 'TMiningOpenCastPitValidator.MiningOpenCastPitDialog';
begin
  Result := Nil;
  try
    if Assigned(FPanel) then
      Result := TMiningOpenCastPitDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMiningOpenCastPitValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TMiningOpenCastPitValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if (AFieldName = 'OpenCast') then
       RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMiningOpenCastPitValidator.StudyHasChanged: boolean;
const OPNAME = 'TMiningOpenCastPitValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TMiningOpenCastPitValidator.PopulateDataViewer;
const OPNAME = 'TMiningOpenCastPitValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtResPropAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{procedure TMiningOpenCastPitValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TMiningOpenCastPitValidator.DoContextValidation';
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

procedure TMiningOpenCastPitValidator.PopulateOpenCastPitsGrid;
const OPNAME = 'TMiningOpenCastPitValidator.PopulateOpenCastPitsGrid';
var
  LRow      : integer;
  LMine     : IMine;
  LOpenCast : IOpenCast;
begin
  try
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList
             .MineByNodeNumber[FIdentifier];
    with MiningOpenCastPitDialog do
    begin

      OpenCastPitsGrid.ColCount := 19;
      OpenCastPitsGrid.RowCount := Max((OpenCastPitsGrid.FixedRows + 1),(1 + LMine.OpenCastCount));;
      if (LMine.OpenCastCount > 0 ) then
      begin
        for lRow := 1 to LMine.OpenCastCount do
        begin
          LOpenCast := LMine.OpenCastByIndex[lRow-1];
          OpenCastPitsGrid.SetFieldValue(0, lRow,LOpenCast.Identifier);
          OpenCastPitsGrid.SetFieldValue(1, lRow,LOpenCast.PitName);
          OpenCastPitsGrid.SetFieldValue(2, lRow,LOpenCast.CoalReserveArea);
          OpenCastPitsGrid.SetFieldValue(3, lRow,LOpenCast.WorkingsArea);
          OpenCastPitsGrid.SetFieldValue(4, lRow,LOpenCast.DisturbedWorkingsArea);
          OpenCastPitsGrid.SetFieldValue(5, lRow,LOpenCast.DisturbedArea);
          OpenCastPitsGrid.SetFieldValue(6, lRow,LOpenCast.WaterSurfaceEvapArea);
          OpenCastPitsGrid.SetFieldValue(7, lRow,LOpenCast.DisturbedAreaRunoff);
          OpenCastPitsGrid.SetFieldValue(8, lRow,LOpenCast.DisturbedWorkingsAreaRunoff);
          OpenCastPitsGrid.SetFieldValue(9, lRow,LOpenCast.DecantVolume);
          OpenCastPitsGrid.SetFieldValue(10,lRow,LOpenCast.SeepageVolume);
          OpenCastPitsGrid.SetFieldValue(11,lRow,LOpenCast.AnalysisStartVolume);
          OpenCastPitsGrid.SetFieldValue(12,lRow,LOpenCast.MaximumSeepageRate);
          OpenCastPitsGrid.SetFieldValue(13,lRow,LOpenCast.SeepageExponent);
          OpenCastPitsGrid.SetFieldValue(14,lRow,LOpenCast.PCDSurfaceArea);
          OpenCastPitsGrid.SetFieldValue(15,lRow,LOpenCast.PCDStorageCapacity);
          OpenCastPitsGrid.SetFieldValue(16,lRow,LOpenCast.PCDAnalysisStartVolume);
          OpenCastPitsGrid.Options := OpenCastPitsGrid.Options + [goEditing];
        end;
      end
      else
      begin
        OpenCastPitsGrid.Cells[0, 1] := '';
        OpenCastPitsGrid.Cells[1, 1] := '';
        OpenCastPitsGrid.Cells[2, 1] := '';
        OpenCastPitsGrid.Cells[3, 1] := '';
        OpenCastPitsGrid.Cells[4, 1] := '';
        OpenCastPitsGrid.Cells[5, 1] := '';
        OpenCastPitsGrid.Cells[6, 1] := '';
        OpenCastPitsGrid.Cells[7, 1] := '';
        OpenCastPitsGrid.Cells[8, 1] := '';
        OpenCastPitsGrid.Cells[9, 1] := '';
        OpenCastPitsGrid.Cells[10,1] := '';
        OpenCastPitsGrid.Cells[11,1] := '';
        OpenCastPitsGrid.Cells[12,1] := '';
        OpenCastPitsGrid.Cells[13,1] := '';
        OpenCastPitsGrid.Cells[14,1] := '';
        OpenCastPitsGrid.Cells[15,1] := '';
        OpenCastPitsGrid.Cells[16,1] := '';
       end ;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningOpenCastPitValidator.ValidatePitName(AOpenCastData : IOpenCast);
const OPNAME = 'TMiningOpenCastPitValidator.ValidatePitName';
var
  LRow  : integer;
begin
  try
    with MiningOpenCastPitDialog do
    begin
      LRow := OpenCastPitsGrid.Row;
      FErrorMessage := '';
      if(AOpenCastData.Validate(FErrorMessage,'PitName')) then
      begin
        OpenCastPitsGrid.ValidationError[1,LRow,gveCellContext] := '';
      end;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TMiningOpenCastPitValidator.ValidateCoalReserveArea(AOpenCastData: IOpenCast);
const OPNAME = 'TMiningOpenCastPitValidator.ValidateCoalReserveArea';
var
  LRow  : integer;
begin
  try
    with MiningOpenCastPitDialog do
    begin
      LRow := OpenCastPitsGrid.Row;
      FErrorMessage := '';
      if(AOpenCastData.Validate(FErrorMessage,'CoalReserveArea')) then
      begin
        OpenCastPitsGrid.ValidationError[2,LRow,gveCellContext] := '';
      end;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TMiningOpenCastPitValidator.ValidateWorkingsArea(AOpenCastData: IOpenCast);
const OPNAME = 'TMiningOpenCastPitValidator.ValidateWorkingsArea';
var
  LRow  : integer;
begin
  try
    with MiningOpenCastPitDialog do
    begin
      LRow := OpenCastPitsGrid.Row;
      FErrorMessage := '';
      if(AOpenCastData.Validate(FErrorMessage,'WorkingsArea')) then
      begin
        OpenCastPitsGrid.ValidationError[3,LRow,gveCellContext] := '';
      end;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TMiningOpenCastPitValidator.ValidateDisturbedWorkingsArea(AOpenCastData: IOpenCast);
const OPNAME = 'TMiningOpenCastPitValidator.ValidateDisturbedWorkingsArea';
var
  LRow  : integer;
begin
  try
    with MiningOpenCastPitDialog do
    begin
      LRow := OpenCastPitsGrid.Row;
      FErrorMessage := '';
      if(AOpenCastData.Validate(FErrorMessage,'DisturbedWorkingsArea')) then
      begin
        OpenCastPitsGrid.ValidationError[4,LRow,gveCellContext] := '';
      end;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TMiningOpenCastPitValidator.ValidateDisturbedArea(AOpenCastData: IOpenCast);
const OPNAME = 'TMiningOpenCastPitValidator.ValidateDisturbedArea';
var
  LRow  : integer;
begin
  try
    with MiningOpenCastPitDialog do
    begin
      LRow := OpenCastPitsGrid.Row;
      FErrorMessage := '';
      if(AOpenCastData.Validate(FErrorMessage,'DisturbedArea')) then
      begin
        OpenCastPitsGrid.ValidationError[5,LRow,gveCellContext] := '';
      end;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TMiningOpenCastPitValidator.ValidateWaterSurfaceEvapArea(AOpenCastData: IOpenCast);
const OPNAME = 'TMiningOpenCastPitValidator.ValidateWaterSurfaceEvapArea';
var
  LRow  : integer;
begin
  try
    with MiningOpenCastPitDialog do
    begin
      LRow := OpenCastPitsGrid.Row;
      FErrorMessage := '';
      if(AOpenCastData.Validate(FErrorMessage,'WaterSurfaceEvapArea')) then
      begin
        OpenCastPitsGrid.ValidationError[6,LRow,gveCellContext] := '';
      end;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TMiningOpenCastPitValidator.ValidateDisturbedAreaRunoff(AOpenCastData: IOpenCast);
const OPNAME = 'TMiningOpenCastPitValidator.ValidateDisturbedAreaRunoff';
var
  LRow  : integer;
begin
  try
    with MiningOpenCastPitDialog do
    begin
      LRow := OpenCastPitsGrid.Row;
      FErrorMessage := '';
      if(AOpenCastData.Validate(FErrorMessage,'DisturbedAreaRunOff')) then
      begin
        OpenCastPitsGrid.ValidationError[7,LRow,gveCellContext] := '';
      end;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TMiningOpenCastPitValidator.ValidateDisturbedWorkingsAreaRunoff(AOpenCastData: IOpenCast);
const OPNAME = 'TMiningOpenCastPitValidator.ValidateDisturbedWorkingsAreaRunoff';
var
  LRow  : integer;
begin
  try
    with MiningOpenCastPitDialog do
    begin
      LRow := OpenCastPitsGrid.Row;
      FErrorMessage := '';
      if(AOpenCastData.Validate(FErrorMessage,'DisturbedWorkingsAreaRunOff')) then
      begin
        OpenCastPitsGrid.ValidationError[8,LRow,gveCellContext] := '';
      end;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TMiningOpenCastPitValidator.ValidateDecantVolume(AOpenCastData: IOpenCast);
const OPNAME = 'TMiningOpenCastPitValidator.ValidateDecantVolume';
var
  LRow  : integer;
begin
  try
    with MiningOpenCastPitDialog do
    begin
      LRow := OpenCastPitsGrid.Row;
      FErrorMessage := '';
      if(AOpenCastData.Validate(FErrorMessage,'DecantVolume')) then
      begin
        OpenCastPitsGrid.ValidationError[9,LRow,gveCellContext] := '';
      end;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TMiningOpenCastPitValidator.ValidateSeepageVolume(AOpenCastData: IOpenCast);
const OPNAME = 'TMiningOpenCastPitValidator.ValidateSeepageVolume';
var
  LRow  : integer;
begin
  try
    with MiningOpenCastPitDialog do
    begin
      LRow := OpenCastPitsGrid.Row;
      FErrorMessage := '';
      if(AOpenCastData.Validate(FErrorMessage,'SeepageVolume')) then
      begin
        OpenCastPitsGrid.ValidationError[10,LRow,gveCellContext] := '';
      end;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TMiningOpenCastPitValidator.ValidateAnalysisStartVolume(AOpenCastData: IOpenCast);
const OPNAME = 'TMiningOpenCastPitValidator.ValidateAnalysisStartVolume';
var
  LRow  : integer;
begin
  try
    with MiningOpenCastPitDialog do
    begin
      LRow := OpenCastPitsGrid.Row;
      FErrorMessage := '';
      if(AOpenCastData.Validate(FErrorMessage,'AnalysisStartVolume')) then
      begin
        OpenCastPitsGrid.ValidationError[11,LRow,gveCellContext] := '';
      end;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TMiningOpenCastPitValidator.ValidateMaximumSeepageRate(AOpenCastData: IOpenCast);
const OPNAME = 'TMiningOpenCastPitValidator.ValidateMaximumSeepageRate';
var
  LRow  : integer;
begin
  try
    with MiningOpenCastPitDialog do
    begin
      LRow := OpenCastPitsGrid.Row;
      FErrorMessage := '';
      if(AOpenCastData.Validate(FErrorMessage,'MaximumSeepageRate')) then
      begin
        OpenCastPitsGrid.ValidationError[12,LRow,gveCellContext] := '';
      end;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TMiningOpenCastPitValidator.ValidateSeepageExponent(AOpenCastData: IOpenCast);
const OPNAME = 'TMiningOpenCastPitValidator.ValidateSeepageExponent';
var
  LRow  : integer;
begin
  try
    with MiningOpenCastPitDialog do
    begin
      LRow := OpenCastPitsGrid.Row;
      FErrorMessage := '';
      if(AOpenCastData.Validate(FErrorMessage,'SeepageExponent')) then
      begin
        OpenCastPitsGrid.ValidationError[13,LRow,gveCellContext] := '';
      end;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TMiningOpenCastPitValidator.ValidatePCDSurfaceArea(AOpenCastData: IOpenCast);
const OPNAME = 'TMiningOpenCastPitValidator.ValidatePCDSurfaceArea';
var
  LRow  : integer;
begin
  try
    with MiningOpenCastPitDialog do
    begin
      LRow := OpenCastPitsGrid.Row;
      FErrorMessage := '';
      if(AOpenCastData.Validate(FErrorMessage,'OpenCastPCDSurfaceArea')) then
      begin
        OpenCastPitsGrid.ValidationError[14,LRow,gveCellContext] := '';
      end;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TMiningOpenCastPitValidator.ValidatePCDStorageCapacity(AOpenCastData: IOpenCast);
const OPNAME = 'TMiningOpenCastPitValidator.ValidatePCDStorageCapacity';
var
  LRow  : integer;
begin
  try
    with MiningOpenCastPitDialog do
    begin
      LRow := OpenCastPitsGrid.Row;
      FErrorMessage := '';
      if(AOpenCastData.Validate(FErrorMessage,'OpenCastPCDStorageCapacity')) then
      begin
        OpenCastPitsGrid.ValidationError[15,LRow,gveCellContext] := '';
      end;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TMiningOpenCastPitValidator.ValidatePCDAnalysisStartVolume(AOpenCastData: IOpenCast);
const OPNAME = 'TMiningOpenCastPitValidator.ValidatePCDAnalysisStartVolume';
var
  LRow  : integer;
begin
  try
    with MiningOpenCastPitDialog do
    begin
      LRow := OpenCastPitsGrid.Row;
      FErrorMessage := '';
      if(AOpenCastData.Validate(FErrorMessage,'OpenCastPCDAnalysisStartVolume')) then
      begin
        OpenCastPitsGrid.ValidationError[16,LRow,gveCellContext] := '';
      end;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TMiningOpenCastPitValidator.OnMonthlyRechargeFactorsClick(Sender: TObject; ACol, ARow: Longint);
const OPNAME = 'TMiningOpenCastPitValidator.OnMonthlyRechargeFactorsClick';
var
  LOpenCastID      : integer;
  LForm            : TYieldModelDataGUIForm;
  LDialogValidator : TMineMonthlyDataValidator;
  LMine            : IMine;
begin
  try
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
             .CastMineList.MineByNodeNumber[FIdentifier];
    if (LMine <> nil) then
    begin
      if (LMine.OpenCastCount > 0) then
      begin
        LForm   := TYieldModelDataGUIForm.CreateWithoutDFM(nil,FAppModules);
        try
          LForm.Initialise;
          LForm.BtnCancel.Visible := False;
          LForm.LanguageHasChanged;
          LDialogValidator := TMineMonthlyDataValidator.Create(LForm,FAppModules);
          try
            if (ACol = 17) then
              LDialogValidator.RechargeFactorType := rftOpenCast_DisturbedRechargeFactor
            else
            if (ACol = 18) then
            LDialogValidator.RechargeFactorType := rftOpenCast_WorkingAreaRechargeFactor;
            LOpenCastID := StrToInt(MiningOpenCastPitDialog.OpenCastPitsGrid.Cells[0,ARow]);
            LDialogValidator.Identifier := FIdentifier;
            LDialogValidator.ParentIdentifier := LOpenCastID;
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

procedure TMiningOpenCastPitValidator.UpdatePitName(ARowIndex : integer;AValue    : string);
const OPNAME = 'TMiningOpenCastPitValidator.UpdatePitName';
var
  LMine       : IMine;
  LValue      : string;
  LMessage    : string;
  LPitID      : integer;
  LOpenCast   : IOpenCast;
begin
  try
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).
                     CastNetworkFeaturesData.CastMineList.MineByNodeNumber[FIdentifier];
    if (LMine <> nil) then
    begin
      with MiningOpenCastPitDialog do
      begin
        LPitID := StrToInt(OpenCastPitsGrid.Cells[0,ARowIndex]);
        LOpenCast := LMine.OpenCastByIdentifier[LPitID];
        if LOpenCast <> nil then
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              'PitName', AValue, LMessage, 1, ARowIndex)) then
          begin
            LValue := AValue;
            LOpenCast.PitName := LValue;
            RePopulateDataViewer;
          end
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningOpenCastPitValidator.UpdateCoalReserveArea(ARowIndex: integer; AValue: string);
const OPNAME = 'TMiningOpenCastPitValidator.UpdateCoalReserveArea';
var
  LMine       : IMine;
  LValue      : string;
  LMessage    : string;
  LPitID      : integer;
  LOpenCast   : IOpenCast;
begin
  try
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).
                     CastNetworkFeaturesData.CastMineList.MineByNodeNumber[FIdentifier];
    if (LMine <> nil) then
    begin
      with MiningOpenCastPitDialog do
      begin
        LPitID := StrToInt(OpenCastPitsGrid.Cells[0,ARowIndex]);
        LOpenCast := LMine.OpenCastByIdentifier[LPitID];
        if LOpenCast <> nil then
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              'CoalReserveArea', AValue, LMessage, 1, ARowIndex)) then
          begin
            LValue := AValue;
            LOpenCast.CoalReserveArea := StrToFloat(LValue);
            RePopulateDataViewer;
          end
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningOpenCastPitValidator.UpdateWorkingsArea(ARowIndex: integer; AValue: string);
const OPNAME = 'TMiningOpenCastPitValidator.UpdateWorkingsArea';
var
  LMine       : IMine;
  LValue      : string;
  LMessage    : string;
  LPitID      : integer;
  LOpenCast   : IOpenCast;
begin
  try
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).
                     CastNetworkFeaturesData.CastMineList.MineByNodeNumber[FIdentifier];
    if (LMine <> nil) then
    begin
      with MiningOpenCastPitDialog do
      begin
        LPitID := StrToInt(OpenCastPitsGrid.Cells[0,ARowIndex]);
        LOpenCast := LMine.OpenCastByIdentifier[LPitID];
        if LOpenCast <> nil then
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              'WorkingsArea', AValue, LMessage, 2, ARowIndex)) then
          begin
            LValue := AValue;
            LOpenCast.WorkingsArea := StrToFloat(LValue);
            RePopulateDataViewer;
          end
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningOpenCastPitValidator.UpdateDisturbedWorkingsArea(ARowIndex: integer; AValue: string);
const OPNAME = 'TMiningOpenCastPitValidator.UpdateDisturbedWorkingsArea';
var
  LMine       : IMine;
  LValue      : string;
  LMessage    : string;
  LPitID      : integer;
  LOpenCast   : IOpenCast;
begin
  try
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).
                     CastNetworkFeaturesData.CastMineList.MineByNodeNumber[FIdentifier];
    if (LMine <> nil) then
    begin
      with MiningOpenCastPitDialog do
      begin
        LPitID := StrToInt(OpenCastPitsGrid.Cells[0,ARowIndex]);
        LOpenCast := LMine.OpenCastByIdentifier[LPitID];
        if LOpenCast <> nil then
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              'DisturbedWorkingsArea', AValue, LMessage, 3, ARowIndex)) then
          begin
            LValue := AValue;
            LOpenCast.DisturbedWorkingsArea := StrToFloat(LValue);
            RePopulateDataViewer;
          end
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningOpenCastPitValidator.UpdateDisturbedArea(ARowIndex: integer; AValue: string);
const OPNAME = 'TMiningOpenCastPitValidator.UpdateDisturbedArea';
var
  LMine       : IMine;
  LValue      : string;
  LMessage    : string;
  LPitID      : integer;
  LOpenCast   : IOpenCast;
begin
  try
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).
                     CastNetworkFeaturesData.CastMineList.MineByNodeNumber[FIdentifier];
    if (LMine <> nil) then
    begin
      with MiningOpenCastPitDialog do
      begin
        LPitID := StrToInt(OpenCastPitsGrid.Cells[0,ARowIndex]);
        LOpenCast := LMine.OpenCastByIdentifier[LPitID];
        if LOpenCast <> nil then
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              'DisturbedArea', AValue, LMessage, 4, ARowIndex)) then
          begin
            LValue := AValue;
            LOpenCast.DisturbedArea := StrToFloat(LValue);
            RePopulateDataViewer;
          end
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningOpenCastPitValidator.UpdateWaterSurfaceEvapArea(ARowIndex: integer; AValue: string);
const OPNAME = 'TMiningOpenCastPitValidator.UpdateWaterSurfaceEvapArea';
var
  LMine       : IMine;
  LValue      : string;
  LMessage    : string;
  LPitID      : integer;
  LOpenCast   : IOpenCast;
begin
  try
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).
                     CastNetworkFeaturesData.CastMineList.MineByNodeNumber[FIdentifier];
    if (LMine <> nil) then
    begin
      with MiningOpenCastPitDialog do
      begin
        LPitID := StrToInt(OpenCastPitsGrid.Cells[0,ARowIndex]);
        LOpenCast := LMine.OpenCastByIdentifier[LPitID];
        if LOpenCast <> nil then
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              'WaterSurfaceEvapArea', AValue, LMessage, 5, ARowIndex)) then
          begin
            LValue := AValue;
            LOpenCast.WaterSurfaceEvapArea := StrToFloat(LValue);
            RePopulateDataViewer;
          end
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningOpenCastPitValidator.UpdateDisturbedAreaRunoff(ARowIndex: integer; AValue: string);
const OPNAME = 'TMiningOpenCastPitValidator.UpdateDisturbedAreaRunoff';
var
  LMine       : IMine;
  LValue      : string;
  LMessage    : string;
  LPitID      : integer;
  LOpenCast   : IOpenCast;
begin
  try
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).
                     CastNetworkFeaturesData.CastMineList.MineByNodeNumber[FIdentifier];
    if (LMine <> nil) then
    begin
      with MiningOpenCastPitDialog do
      begin
        LPitID := StrToInt(OpenCastPitsGrid.Cells[0,ARowIndex]);
        LOpenCast := LMine.OpenCastByIdentifier[LPitID];
        if LOpenCast <> nil then
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              'DisturbedAreaRunOff', AValue, LMessage, 6, ARowIndex)) then
          begin
            LValue := AValue;
            LOpenCast.DisturbedAreaRunoff := StrToFloat(LValue);
            RePopulateDataViewer;
          end
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningOpenCastPitValidator.UpdateDisturbedWorkingsAreaRunoff(ARowIndex: integer; AValue: string);
const OPNAME = 'TMiningOpenCastPitValidator.UpdateDisturbedWorkingsAreaRunoff';
var
  LMine       : IMine;
  LValue      : string;
  LMessage    : string;
  LPitID      : integer;
  LOpenCast   : IOpenCast;
begin
  try
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).
                     CastNetworkFeaturesData.CastMineList.MineByNodeNumber[FIdentifier];
    if (LMine <> nil) then
    begin
      with MiningOpenCastPitDialog do
      begin
        LPitID := StrToInt(OpenCastPitsGrid.Cells[0,ARowIndex]);
        LOpenCast := LMine.OpenCastByIdentifier[LPitID];
        if LOpenCast <> nil then
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              'DisturbedWorkingsAreaRunOff', AValue, LMessage, 7, ARowIndex)) then
          begin
            LValue := AValue;
            LOpenCast.DisturbedWorkingsAreaRunoff := StrToFloat(LValue);
            RePopulateDataViewer;
          end
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningOpenCastPitValidator.UpdateDecantVolume(ARowIndex: integer; AValue: string);
const OPNAME = 'TMiningOpenCastPitValidator.UpdateDecantVolume';
var
  LMine       : IMine;
  LValue      : string;
  LMessage    : string;
  LPitID      : integer;
  LOpenCast   : IOpenCast;
begin
  try
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).
                     CastNetworkFeaturesData.CastMineList.MineByNodeNumber[FIdentifier];
    if (LMine <> nil) then
    begin
      with MiningOpenCastPitDialog do
      begin
        LPitID := StrToInt(OpenCastPitsGrid.Cells[0,ARowIndex]);
        LOpenCast := LMine.OpenCastByIdentifier[LPitID];
        if LOpenCast <> nil then
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              'DecantVolume', AValue, LMessage, 8, ARowIndex)) then
          begin
            LValue := AValue;
            LOpenCast.DecantVolume := StrToFloat(LValue);
            RePopulateDataViewer;
          end
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningOpenCastPitValidator.UpdateSeepageVolume(ARowIndex: integer; AValue: string);
const OPNAME = 'TMiningOpenCastPitValidator.UpdateSeepageVolume';
var
  LMine       : IMine;
  LValue      : string;
  LMessage    : string;
  LPitID      : integer;
  LOpenCast   : IOpenCast;
begin
  try
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).
                     CastNetworkFeaturesData.CastMineList.MineByNodeNumber[FIdentifier];
    if (LMine <> nil) then
    begin
      with MiningOpenCastPitDialog do
      begin
        LPitID := StrToInt(OpenCastPitsGrid.Cells[0,ARowIndex]);
        LOpenCast := LMine.OpenCastByIdentifier[LPitID];
        if LOpenCast <> nil then
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              'SeepageVolume', AValue, LMessage, 9, ARowIndex)) then
          begin
            LValue := AValue;
            LOpenCast.SeepageVolume := StrToFloat(LValue);
            RePopulateDataViewer;
          end
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningOpenCastPitValidator.UpdateAnalysisStartVolume(ARowIndex: integer; AValue: string);
const OPNAME = 'TMiningOpenCastPitValidator.UpdateAnalysisStartVolume';
var
  LMine       : IMine;
  LValue      : string;
  LMessage    : string;
  LPitID      : integer;
  LOpenCast   : IOpenCast;
begin
  try
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).
                     CastNetworkFeaturesData.CastMineList.MineByNodeNumber[FIdentifier];
    if (LMine <> nil) then
    begin
      with MiningOpenCastPitDialog do
      begin
        LPitID := StrToInt(OpenCastPitsGrid.Cells[0,ARowIndex]);
        LOpenCast := LMine.OpenCastByIdentifier[LPitID];
        if LOpenCast <> nil then
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              'AnalysisStartVolume', AValue, LMessage, 10, ARowIndex)) then
          begin
            LValue := AValue;
            LOpenCast.AnalysisStartVolume := StrToFloat(LValue);
            RePopulateDataViewer;
          end
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningOpenCastPitValidator.UpdateMaximumSeepageRate(ARowIndex: integer; AValue: string);
const OPNAME = 'TMiningOpenCastPitValidator.UpdateMaximumSeepageRate';
var
  LMine       : IMine;
  LValue      : string;
  LMessage    : string;
  LPitID      : integer;
  LOpenCast   : IOpenCast;
begin
  try
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).
                     CastNetworkFeaturesData.CastMineList.MineByNodeNumber[FIdentifier];
    if (LMine <> nil) then
    begin
      with MiningOpenCastPitDialog do
      begin
        LPitID := StrToInt(OpenCastPitsGrid.Cells[0,ARowIndex]);
        LOpenCast := LMine.OpenCastByIdentifier[LPitID];
        if LOpenCast <> nil then
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              'MaximumSeepageRate', AValue, LMessage, 11, ARowIndex)) then
          begin
            LValue := AValue;
            LOpenCast.MaximumSeepageRate := StrToFloat(LValue);
            RePopulateDataViewer;
          end
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningOpenCastPitValidator.UpdateSeepageExponent(ARowIndex: integer; AValue: string);
const OPNAME = 'TMiningOpenCastPitValidator.UpdateSeepageExponent';
var
  LMine       : IMine;
  LValue      : string;
  LMessage    : string;
  LPitID      : integer;
  LOpenCast   : IOpenCast;
begin
  try
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).
                     CastNetworkFeaturesData.CastMineList.MineByNodeNumber[FIdentifier];
    if (LMine <> nil) then
    begin
      with MiningOpenCastPitDialog do
      begin
        LPitID := StrToInt(OpenCastPitsGrid.Cells[0,ARowIndex]);
        LOpenCast := LMine.OpenCastByIdentifier[LPitID];
        if LOpenCast <> nil then
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              'SeepageExponent', AValue, LMessage, 12, ARowIndex)) then
          begin
            LValue := AValue;
            LOpenCast.SeepageExponent := StrToFloat(LValue);
            RePopulateDataViewer;
          end
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningOpenCastPitValidator.UpdatePCDSurfaceArea(ARowIndex: integer; AValue: string);
const OPNAME = 'TMiningOpenCastPitValidator.UpdatePCDSurfaceArea';
var
  LMine       : IMine;
  LValue      : string;
  LMessage    : string;
  LPitID      : integer;
  LOpenCast   : IOpenCast;
begin
  try
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).
                     CastNetworkFeaturesData.CastMineList.MineByNodeNumber[FIdentifier];
    if (LMine <> nil) then
    begin
      with MiningOpenCastPitDialog do
      begin
        LPitID := StrToInt(OpenCastPitsGrid.Cells[0,ARowIndex]);
        LOpenCast := LMine.OpenCastByIdentifier[LPitID];
        if LOpenCast <> nil then
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              'OpenCastPCDSurfaceArea', AValue, LMessage, 13, ARowIndex)) then
          begin
            LValue := AValue;
            LOpenCast.PCDSurfaceArea := StrToFloat(LValue);
            RePopulateDataViewer;
          end
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningOpenCastPitValidator.UpdatePCDStorageCapacity(ARowIndex: integer; AValue: string);
const OPNAME = 'TMiningOpenCastPitValidator.UpdatePCDStorageCapacity';
var
  LMine       : IMine;
  LValue      : string;
  LMessage    : string;
  LPitID      : integer;
  LOpenCast   : IOpenCast;
begin
  try
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).
                     CastNetworkFeaturesData.CastMineList.MineByNodeNumber[FIdentifier];
    if (LMine <> nil) then
    begin
      with MiningOpenCastPitDialog do
      begin
        LPitID := StrToInt(OpenCastPitsGrid.Cells[0,ARowIndex]);
        LOpenCast := LMine.OpenCastByIdentifier[LPitID];
        if LOpenCast <> nil then
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              'OpenCastPCDStorageCapacity', AValue, LMessage, 14, ARowIndex)) then
          begin
            LValue := AValue;
            LOpenCast.PCDStorageCapacity := StrToFloat(LValue);
            RePopulateDataViewer;
          end
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningOpenCastPitValidator.UpdatePCDAnalysisStartVolume(ARowIndex: integer; AValue: string);
const OPNAME = 'TMiningOpenCastPitValidator.UpdatePCDAnalysisStartVolume';
var
  LMine       : IMine;
  LValue      : string;
  LMessage    : string;
  LPitID      : integer;
  LOpenCast   : IOpenCast;
begin
  try
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).
                     CastNetworkFeaturesData.CastMineList.MineByNodeNumber[FIdentifier];
    if (LMine <> nil) then
    begin
      with MiningOpenCastPitDialog do
      begin
        LPitID := StrToInt(OpenCastPitsGrid.Cells[0,ARowIndex]);
        LOpenCast := LMine.OpenCastByIdentifier[LPitID];
        if LOpenCast <> nil then
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              'OpenCastPCDAnalysisStartVolume', AValue, LMessage, 15, ARowIndex)) then
          begin
            LValue := AValue;
            LOpenCast.PCDAnalysisStartVolume := StrToFloat(LValue);
            RePopulateDataViewer;
          end
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningOpenCastPitValidator.OnInsertOpenCast(Sender: TObject);
const OPNAME = 'TMiningOpenCastPitValidator.OnInsertOpenCast';
begin
  try
    if((FAppModules.Model as IYieldModel).DoCreateOpenCast(FIdentifier) <> nil)then
    begin
      RePopulateDataViewer;
      DoContextValidation(dvtYMDCReturnFlowFeatureAll);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningOpenCastPitValidator.OnDeleteOpenCast(Sender: TObject);
const OPNAME = 'TMiningOpenCastPitValidator.OnDeleteOpenCast';
begin
  try
    if(FAppModules.Model as IYieldModel).DoDeleteOpenCast(FIdentifier,FSelectOpenCastID) then
    begin
      RePopulateDataViewer;
      DoContextValidation(dvtMineAll);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningOpenCastPitValidator.OnGridSelectCell(ASender: TObject;ACol, ARow: Integer; var CanSelect: Boolean);
const OPNAME = 'TMiningOpenCastPitValidator.OnGridSelectCell';
var
  LMine : IMine;
begin
  try
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
             .CastMineList.MineByNodeNumber[FIdentifier];
    if (LMine <> nil) then
    begin
    //  FSelectOpenCastID := -1;
      if ASender = MiningOpenCastPitDialog.OpenCastPitsGrid then
      begin
        if (LMine.OpenCastCount > 0) then
        begin
          FSelectOpenCastID := StrToInt(MiningOpenCastPitDialog.OpenCastPitsGrid.Cells[0,ARow]);
        end;
      end;
    end;
    MiningOpenCastPitDialog.ResetButtonState(LMine.OpenCastCount);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.


