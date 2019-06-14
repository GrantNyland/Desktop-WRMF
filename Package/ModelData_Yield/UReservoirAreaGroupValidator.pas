{******************************************************************************}
{*  UNIT      : Contains the class TReservoirAreaGroupValidator.                   *}
{*  AUTHOR    : Presley Mudau                                                 *}
{*  DATE      : 2007/11/05                                                    *}
{*  COPYRIGHT : Copyright © 2007 DWAF                                         *}
{******************************************************************************}

unit UReservoirAreaGroupValidator;

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
  UReservoirAreaGroup,
  UReservoirAreaGroupDialog,
  UAbstractYieldDataDialogValidator,
  UNetworkElementData,
  UYieldContextValidationType;

type

  TReservoirAreaGroupValidator = class(TAbstractYieldDataDialogValidator)
  private
  protected
    FSelectedReservoirAreaIndex : integer;
    FSelectedReservoirID        : integer;
    FValidateReservoirAreaIndex : integer;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer); override;
    procedure OnReservoirAreaGroupGridSelectCell(ASender: TObject;ACol, ARow: Longint;
                                          var CanSelect: Boolean);
    procedure UpdateNrOfReservoirAreaGroup;
    procedure UpdateReservoirAreaName(AIndex: integer; AValue: string);
    procedure RePopulateDataViewer;
    procedure RePopulateReservoirAreaGroupGrid;
    procedure RePopulateReservoirListBox;

    procedure ValidateReservoirAreaCount(AReservoirAreaGroupList : IReservoirAreaGroupList);
    procedure ValidateReservoirAreaName(AReservoirAreaGroup : IReservoirAreaGroup);

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
    function ReservoirAreaGroupDialog: TReservoirAreaGroupDialog;
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
  Math;

{******************************************************************************}
{* TReservoirAreaGroupValidator                                           *}
{******************************************************************************}

procedure TReservoirAreaGroupValidator.CreateMemberObjects;
const OPNAME = 'TReservoirAreaGroupValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FSelectedReservoirAreaIndex := 0;
    FPanel := TReservoirAreaGroupDialog.Create(FPanelOwner,FAppModules);

    ReservoirAreaGroupDialog.ReservoirAreaGroupCountEdt.FieldProperty := FAppModules.FieldProperties.FieldProperty('ReservoirAreaGroupCount');
    ReservoirAreaGroupDialog.ReservoirAreaGroupCountEdt.OnEnter       := OnEditControlEnter;
    ReservoirAreaGroupDialog.ReservoirAreaGroupCountEdt.OnExit        :=  OnEditControltExit;

    ReservoirAreaGroupDialog.ReservoirAreaGroupGrid.OnBeforeCellChange   := OnStringGridCellDataHasChanged;
    ReservoirAreaGroupDialog.ReservoirAreaGroupGrid.OnSelectCell         := OnReservoirAreaGroupGridSelectCell;
    ReservoirAreaGroupDialog.ReservoirAreaGroupGrid.OnColEnter           := OnStringGridColEnter;
    ReservoirAreaGroupDialog.ReservoirAreaGroupGrid.OnExit               := OnEditControltExit;
    ReservoirAreaGroupDialog.ReservoirAreaGroupGrid.OnEnter              := OnEditControlEnter;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirAreaGroupValidator.DestroyMemberObjects;
const OPNAME = 'TReservoirAreaGroupValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirAreaGroupValidator.Initialise: boolean;
const OPNAME = 'TReservoirAreaGroupValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirAreaGroupValidator.LanguageHasChanged: boolean;
const OPNAME = 'TReservoirAreaGroupValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('ViewData.ReservoirArea');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirAreaGroupValidator.ClearDataViewer;
const OPNAME = 'TReservoirAreaGroupValidator.ClearDataViewer';
var
  lpPanel : TReservoirAreaGroupDialog;
  lRow    : integer;
  lCol    : integer;
begin
  inherited ClearDataViewer;
  try
    lpPanel := ReservoirAreaGroupDialog;
    with lpPanel do
    begin
      ReservoirAreaGroupCountEdt.Text     := '-1';
      ReservoirAreaGroupCountEdt.Text     := '-1';
      for lRow := 1 to ReservoirAreaGroupGrid.RowCount-1 do
      begin
        ReservoirAreaGroupGrid.Cells[1, lRow] := '';
        for lCol := 2 to ReservoirAreaGroupGrid.ColCount - 1 do
          ReservoirAreaGroupGrid.Cells[lCol, lRow] := '-1';
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirAreaGroupValidator.PopulateDataViewer;
const OPNAME = 'TReservoirAreaGroupValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtReservoirAreas);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirAreaGroupValidator.ReservoirAreaGroupDialog : TReservoirAreaGroupDialog;
const OPNAME = 'TReservoirAreaGroupValidator.ReservoirAreaGroupDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TReservoirAreaGroupDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirAreaGroupValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TReservoirAreaGroupValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirAreaGroupValidator.StudyHasChanged: boolean;
const OPNAME = 'TReservoirAreaGroupValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirAreaGroupValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TReservoirAreaGroupValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirAreaGroupValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TReservoirAreaGroupValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with ReservoirAreaGroupDialog do
    begin
      if ((Sender = ReservoirAreaGroupCountEdt) AND (ReservoirAreaGroupCountEdt.HasValueChanged ))then
        UpdateNrOfReservoirAreaGroup;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirAreaGroupValidator.DetermineWizardStatus (ASequence : integer = 0) : integer;
const OPNAME = 'TReservoirAreaGroupValidator.DetermineWizardStatus';
begin
  Result := inherited DetermineWizardStatus(ASequence);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirAreaGroupValidator.UpdateNrOfReservoirAreaGroup;
const OPNAME = 'TReservoirAreaGroupValidator.UpdateNrOfReservoirAreaGroup';
var
  LReservoirAreaGroupList : IReservoirAreaGroupList;
  LMessage                : string;
  LOldCount               : integer;
  LNewCount               : integer;
  LIndex                  : integer;
begin
  try
    with ReservoirAreaGroupDialog do
    begin
      LReservoirAreaGroupList := TYieldModelDataObject(FAppModules.Model.ModelData).
                             NetworkElementData.ReservoirAreaGroupList;
      if (LReservoirAreaGroupList <> nil) then
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty('ReservoirAreaGroupCount',
            ReservoirAreaGroupCountEdt.Text, lMessage)) then
        begin
          ReservoirAreaGroupCountEdt.FieldValidationError := '';
          lOldCount := LReservoirAreaGroupList.GroupAreaCount;
          lNewCount := StrToInt(Trim(ReservoirAreaGroupCountEdt.Text));
          if (lNewCount > lOldCount) then
          begin
            for lIndex := lOldCount + 1 to lNewCount do
              LReservoirAreaGroupList.CreateReservoirAreaGroup;
          end
          else
          begin
            for lIndex := lNewCount + 1 to lOldCount do
              LReservoirAreaGroupList.RemoveReservoirAreaGroup(lIndex);
          end;
          ReservoirAreaGroupCountEdt.SetFieldValue(IntToStr(LReservoirAreaGroupList.GroupAreaCount));
          RePopulateReservoirAreaGroupGrid;
          RePopulateDataViewer;
          DoContextValidation(dvtReservoirAreasCount);
        end
        else
          ReservoirAreaGroupCountEdt.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirAreaGroupValidator.RePopulateDataViewer;
const OPNAME = 'TReservoirAreaGroupValidator.RePopulateDataViewer';
var
  LReservoirAreaGroupList : IReservoirAreaGroupList;
begin
  try
    LReservoirAreaGroupList := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.
                     CastReservoirAreaGroupList;
    if (LReservoirAreaGroupList <> nil) then
    begin
      with ReservoirAreaGroupDialog do
      begin
        ReservoirAreaGroupCountEdt.SetFieldValue(IntToStr(LReservoirAreaGroupList.GroupAreaCount));
        RePopulateReservoirAreaGroupGrid;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirAreaGroupValidator.RePopulateReservoirAreaGroupGrid;
const OPNAME = 'TReservoirAreaGroupValidator.RePopulateReservoirAreaGroupGrid';
var
  LReservoirAreaGroupList : IReservoirAreaGroupList;
  LNrOfReservoirArea      : integer;
  LRow                    : integer;
  LReservoirAreaGroup     : IReservoirAreaGroup;
  LFieldProperty          : TAbstractFieldProperty;
begin
  try
    LReservoirAreaGroupList := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.
                     CastReservoirAreaGroupList;
    if (LReservoirAreaGroupList <> nil) then
    begin
      with ReservoirAreaGroupDialog do
      begin
        LNrOfReservoirArea   := LReservoirAreaGroupList.GroupAreaCount;
        ReservoirAreaGroupGrid.ColCount := 2;
        ReservoirAreaGroupGrid.RowCount := Max((ReservoirAreaGroupGrid.FixedRows + 1),(1 + LNrOfReservoirArea));
        if (LNrOfReservoirArea > 0) then
          ReservoirAreaGroupGrid.Options := ReservoirAreaGroupGrid.Options + [goEditing	]
        else
          ReservoirAreaGroupGrid.Options := ReservoirAreaGroupGrid.Options - [goEditing	];

        ReservoirAreaGroupGrid.ClearFieldProperties;
        LFieldProperty := FAppModules.FieldProperties.FieldProperty('ReservoirAreaGroupName');
        ReservoirAreaGroupGrid.AddFieldProperty(LFieldProperty);
        ReservoirAreaGroupGrid.AddFieldProperty(LFieldProperty);
        for LRow := 1 to lNrOfReservoirArea do
        begin
          LReservoirAreaGroup := LReservoirAreaGroupList.ReservoirAreaGroupByIndex(lRow-1);
          ReservoirAreaGroupGrid.Cells[0, LRow] := IntToStr(LRow);
          ReservoirAreaGroupGrid.Cells[1, LRow] := LReservoirAreaGroup.GroupName;
        end;
        if ((FSelectedReservoirAreaIndex = 0) AND (lNrOfReservoirArea > 0)) then
        begin
          FSelectedReservoirAreaIndex := 0;
          FSelectedReservoirID    := LReservoirAreaGroupList.ReservoirAreaGroupByIndex(0).GroupID;
        end;
        RepopulateReservoirListBox;

      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirAreaGroupValidator.OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer);
const OPNAME = 'TReservoirAreaGroupValidator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender,ACol,ARow);
    try
    with ReservoirAreaGroupDialog do
    begin
      if ((ASender = ReservoirAreaGroupGrid) AND (ACol = 1)) then
        UpdateReservoirAreaName(ARow-1, Trim(ReservoirAreaGroupGrid.Cells[ACol, ARow]));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirAreaGroupValidator.UpdateReservoirAreaName(AIndex: integer; AValue: string);
const OPNAME = 'TReservoirAreaGroupValidator.UpdateReservoirAreaName';
var
  lMessage                : string;
  LReservoirAreaGroupList : IReservoirAreaGroupList;
  LReservoirAreaGroup     : IReservoirAreaGroup;
begin
  try
    LReservoirAreaGroupList := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData
                        .CastReservoirAreaGroupList;
    if (LReservoirAreaGroupList <> nil) then
    begin
      LReservoirAreaGroup := LReservoirAreaGroupList.ReservoirAreaGroupByIndex(AIndex);
      if (LReservoirAreaGroup <> nil) then
      begin
        with ReservoirAreaGroupDialog do
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty('ReservoirAreaGroupName',AValue, lMessage)) then
          begin
            ReservoirAreaGroupGrid.ValidationError[1, AIndex+1, gveCellField] := '';
            LReservoirAreaGroup.GroupName := AValue;
            RePopulateReservoirAreaGroupGrid;
            FValidateReservoirAreaIndex := AIndex;
            DoContextValidation(dvtReservoirAreaName);
            FValidateReservoirAreaIndex := 0;
          end
          else
            ReservoirAreaGroupGrid.ValidationError[1, AIndex+1, gveCellField] := lMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirAreaGroupValidator.OnReservoirAreaGroupGridSelectCell(ASender: TObject; ACol, ARow: Integer;
                                                            var CanSelect: Boolean);
const OPNAME = 'TReservoirAreaGroupValidator.OnReservoirAreaGroupGridSelectCell';
var
  LReservoirAreaGroupList : IReservoirAreaGroupList;
begin
  try
    CanSelect := True;
    LReservoirAreaGroupList := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData
                    .CastReservoirAreaGroupList;
    with ReservoirAreaGroupDialog do
    begin
      if (ARow <> FSelectedReservoirAreaIndex+1) then
      begin
        FSelectedReservoirAreaIndex := ARow - 1;
        FSelectedReservoirID        := LReservoirAreaGroupList.ReservoirAreaGroupByIndex(FSelectedReservoirAreaIndex).GroupID;
        RepopulateReservoirListbox;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirAreaGroupValidator.RePopulateReservoirListBox;
const OPNAME = 'TReservoirAreaGroupValidator.RePopulateReservoirListBox';
var
  lIndex             : integer;
  LReservoirData     : IReservoirData;
  LReservoirDataList : IReservoirDataList;
begin
  try
    with ReservoirAreaGroupDialog do
    begin
      ReservoirAreaGroupListBox.Items.Clear;
      if (FSelectedReservoirID > 0) then
      begin
        LReservoirDataList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData
                        .ReservoirList;
        if (LReservoirDataList <> nil ) then
        begin
          for lIndex := 0 to LReservoirDataList.ReservoirCount - 1  do
          begin
            LReservoirData := LReservoirDataList.ReservoirByIndex[lIndex];
            if ((LReservoirData <> nil) AND
              (LReservoirData.ReservoirConfigurationData.GroupID = FSelectedReservoirID)) then
            ReservoirAreaGroupListBox.Items.Add('(' + IntToStr(LReservoirData.ReservoirConfigurationData.ReservoirIdentifier) + ') ' +
                                      LReservoirData.ReservoirConfigurationData.ReservoirName);
          end
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirAreaGroupValidator.SaveState: boolean;
const OPNAME = 'TReservoirAreaGroupValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirAreaGroupValidator.ValidateReservoirAreaCount(AReservoirAreaGroupList: IReservoirAreaGroupList);
const OPNAME = 'TReservoirAreaGroupValidator.ValidateReservoirAreaCount';
begin
  try
    with ReservoirAreaGroupDialog do
    begin
      FErrorMessage := '';
      if (NOT AReservoirAreaGroupList.Validate(FErrorMessage, 'ReservoirAreaGroupCount')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      ReservoirAreaGroupCountEdt.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirAreaGroupValidator.ValidateReservoirAreaName(AReservoirAreaGroup: IReservoirAreaGroup);
const OPNAME = 'TReservoirAreaGroupValidator.ValidateReservoirAreaName';
begin
  try
    if (AReservoirAreaGroup <> nil) then
    begin
      with ReservoirAreaGroupDialog do
      begin
        FErrorMessage := '';
        if (AReservoirAreaGroup.Validate(FErrorMessage, 'ReservoirAreaName')) then
          ReservoirAreaGroupGrid.ValidationError[1, FValidateReservoirAreaIndex+1, gveCellContext] := ''
        else
        begin
          ReservoirAreaGroupGrid.ValidationError[1, FValidateReservoirAreaIndex+1, gveCellContext] := FErrorMessage;
          FAllErrorMessages.Add(Trim(FErrorMessage));
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirAreaGroupValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TReservoirAreaGroupValidator.DoContextValidation';
var
  LReservoirAreaGroup     : IReservoirAreaGroup;
  LReservoirAreaGroupList : IReservoirAreaGroupList;
begin
  try
    FAllErrorMessages.Clear;
    LReservoirAreaGroupList := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.
                     CastReservoirAreaGroupList;
    if (LReservoirAreaGroupList <> nil) then
    begin
      if (FValidateReservoirAreaIndex >= 0) then
      begin
        LReservoirAreaGroup := LReservoirAreaGroupList.ReservoirAreaGroupByIndex(FValidateReservoirAreaIndex);
        if LReservoirAreaGroup <> nil then
        begin
          case AValidationType of
            dvtReservoirAreas :
            begin
              ValidateReservoirAreaName(LReservoirAreaGroup);
              ValidateReservoirAreaCount(LReservoirAreaGroupList);
            end;
           dvtReservoirAreasCount : ValidateReservoirAreaCount(LReservoirAreaGroupList);
           dvtReservoirAreaName   : ValidateReservoirAreaName(LReservoirAreaGroup);
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.

