{******************************************************************************}
{*  UNIT      : Contains the class TChannelAreaValidator.                     *}
{*  AUTHOR    : Presley Mudau                                                 *}
{*  DATE      : 2003/07/18                                                    *}
{*  COPYRIGHT : Copyright © 2005 DWAF                                         *}
{******************************************************************************}

unit UChannelAreaValidator;

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
  UChannelAreaDialog,
  UAbstractYieldDataDialogValidator,
  UYieldContextValidationType;

type

  TChannelAreaValidator = class(TAbstractYieldDataDialogValidator)
  private
  protected
    FSelectedChannelAreaIndex : integer;
    FSelectedChannelID        : integer;
    FValidateChannelAreaIndex : integer;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer); override;
    procedure OnChannelAreaGridSelectCell(ASender: TObject;ACol, ARow: Longint;
                                          var CanSelect: Boolean);
    procedure UpdateNrOfChanneaAreas;
    procedure UpdateChannelAreaName(AIndex: integer; AValue: string);
    procedure RePopulateDataViewer;
    procedure RePopulateChannelAreaGrid;
    procedure RePopulateChannelListBox;

    procedure ValidateChannelAreaCount(AChannelArea : IChannelAreaList);
    procedure ValidateChannelAreaName (AChannelArea : IChannelArea);

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
    function ChannelAreaDialog: TChannelAreaDialog;
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
  USelectChannelDialog, UNetworkElementData, Math;

{******************************************************************************}
{* TChannelAreaValidator                                           *}
{******************************************************************************}

procedure TChannelAreaValidator.CreateMemberObjects;
const OPNAME = 'TChannelAreaValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FSelectedChannelAreaIndex := 0;
    FPanel := TChannelAreaDialog.Create(FPanelOwner,FAppModules);

    ChannelAreaDialog.ChannelAreaCountEdt.FieldProperty := FAppModules.FieldProperties.FieldProperty('ChannelAreaCount');
    ChannelAreaDialog.ChannelAreaCountEdt.OnEnter := OnEditControlEnter;
    ChannelAreaDialog.ChannelAreaCountEdt.OnExit  :=  OnEditControltExit;

    ChannelAreaDialog.ChannelAreaGrid.OnBeforeCellChange   := OnStringGridCellDataHasChanged;
    ChannelAreaDialog.ChannelAreaGrid.OnSelectCell         := OnChannelAreaGridSelectCell;
    ChannelAreaDialog.ChannelAreaGrid.OnColEnter           := OnStringGridColEnter;
    ChannelAreaDialog.ChannelAreaGrid.OnExit               := OnEditControltExit;
    ChannelAreaDialog.ChannelAreaGrid.OnEnter              := OnEditControlEnter;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelAreaValidator.DestroyMemberObjects;
const OPNAME = 'TChannelAreaValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelAreaValidator.Initialise: boolean;
const OPNAME = 'TChannelAreaValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelAreaValidator.LanguageHasChanged: boolean;
const OPNAME = 'TChannelAreaValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('ViewData.ChannelArea');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelAreaValidator.ClearDataViewer;
const OPNAME = 'TChannelAreaValidator.ClearDataViewer';
var
  lpPanel : TChannelAreaDialog;
  lRow    : integer;
  lCol    : integer;
begin
  inherited ClearDataViewer;
  try
    lpPanel := ChannelAreaDialog;
    with lpPanel do
    begin
      ChannelAreaCountEdt.Text     := '-1';
      ChannelAreaCountEdt.Text     := '-1';
      for lRow := 1 to ChannelAreaGrid.RowCount-1 do
      begin
        ChannelAreaGrid.Cells[1, lRow] := '';
        for lCol := 2 to ChannelAreaGrid.ColCount - 1 do
          ChannelAreaGrid.Cells[lCol, lRow] := '-1';
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelAreaValidator.PopulateDataViewer;
const OPNAME = 'TChannelAreaValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtChannelAreas);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelAreaValidator.ChannelAreaDialog : TChannelAreaDialog;
const OPNAME = 'TChannelAreaValidator.ChannelAreaDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TChannelAreaDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelAreaValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TChannelAreaValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelAreaValidator.StudyHasChanged: boolean;
const OPNAME = 'TChannelAreaValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelAreaValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TChannelAreaValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelAreaValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TChannelAreaValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with ChannelAreaDialog do
    begin
      if ((Sender = ChannelAreaCountEdt) AND (ChannelAreaCountEdt.HasValueChanged ))then
        UpdateNrOfChanneaAreas;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelAreaValidator.DetermineWizardStatus (ASequence : integer = 0) : integer;
const OPNAME = 'TChannelAreaValidator.DetermineWizardStatus';
begin
  Result := inherited DetermineWizardStatus(ASequence);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelAreaValidator.UpdateNrOfChanneaAreas;
const OPNAME = 'TChannelAreaValidator.UpdateNrOfChanneaAreas';
var
  lChannelAreaList : IChannelAreaList;
  lChannelArea     : IChannelArea;
  lMessage         : string;
  lOldCount        : integer;
  lNewCount        : integer;
  lIndex           : integer;
begin
  try
    with ChannelAreaDialog do
    begin
      lChannelAreaList := TYieldModelDataObject(FAppModules.Model.ModelData).
                       NetworkFeaturesData.ChannelAreaList;
      if (lChannelAreaList <> nil) then
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty('ChannelAreaCount',
            ChannelAreaCountEdt.Text, lMessage)) then
        begin
          ChannelAreaCountEdt.FieldValidationError := '';
          lOldCount := lChannelAreaList.AreaCount;
          lNewCount := StrToInt(Trim(ChannelAreaCountEdt.Text));
          if (lNewCount > lOldCount) then
          begin
            for lIndex := lOldCount + 1 to lNewCount do
              lChannelArea := lChannelAreaList.CreateChannelArea;
          end
          else
          begin
            for lIndex := lNewCount + 1 to lOldCount do
              lChannelAreaList.RemoveChannelArea(lIndex);
          end;
          ChannelAreaCountEdt.SetFieldValue(IntToStr(lChannelAreaList.AreaCount));
          RePopulateChannelAreaGrid;
          RePopulateDataViewer;
          DoContextValidation(dvtChannelAreaCount);
        end
        else
          ChannelAreaCountEdt.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelAreaValidator.RePopulateDataViewer;
const OPNAME = 'TChannelAreaValidator.RePopulateDataViewer';
var
  lChannelArea    : IChannelAreaList;
begin
  try
    lChannelArea := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                     ChannelAreaList;
    if (lChannelArea <> nil) then
    begin
      with ChannelAreaDialog do
      begin
        ChannelAreaCountEdt.SetFieldValue(IntToStr(lChannelArea.AreaCount));
        RePopulateChannelAreaGrid;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelAreaValidator.RePopulateChannelAreaGrid;
const OPNAME = 'TChannelAreaValidator.RePopulateChannelAreaGrid';
var
  lChannelAreaList : IChannelAreaList;
  lNrOfChannelArea : integer;
  lRow             : integer;
  lChannelArea     : IChannelArea;
  lFieldProperty   : TAbstractFieldProperty;
begin
  try
    lChannelAreaList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                     ChannelAreaList;
    if (lChannelAreaList <> nil) then
    begin
      with ChannelAreaDialog do
      begin
        lNrOfChannelArea   := lChannelAreaList.AreaCount;
        ChannelAreaGrid.ColCount := 2;
        ChannelAreaGrid.RowCount := Max((ChannelAreaGrid.FixedRows + 1),(1 + lNrOfChannelArea));
        if (lNrOfChannelArea > 0) then
          ChannelAreaGrid.Options := ChannelAreaGrid.Options + [goEditing	]
        else
          ChannelAreaGrid.Options := ChannelAreaGrid.Options - [goEditing	];

        ChannelAreaGrid.ClearFieldProperties;
        lFieldProperty := FAppModules.FieldProperties.FieldProperty('ChannelAreaName');
        ChannelAreaGrid.AddFieldProperty(lFieldProperty);
        ChannelAreaGrid.AddFieldProperty(lFieldProperty);
        for lRow := 1 to lNrOfChannelArea do
        begin
          lChannelArea := lChannelAreaList.ChannelAreaByIndex(lRow-1);
          ChannelAreaGrid.Cells[0, lRow] := IntToStr(lRow);
          ChannelAreaGrid.Cells[1, lRow] := lChannelArea.AreaName;
        end;
        if ((FSelectedChannelAreaIndex = 0) AND (lNrOfChannelArea > 0)) then
        begin
          FSelectedChannelAreaIndex := 0;
          FSelectedChannelID    := lChannelAreaList.ChannelAreaByIndex(0).AreaID;
        end;
        RepopulateChannelListBox;

      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelAreaValidator.OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer);
const OPNAME = 'TChannelAreaValidator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender,ACol,ARow);
    try
    with ChannelAreaDialog do
    begin
      if ((ASender = ChannelAreaGrid) AND (ACol = 1)) then
        UpdateChannelAreaName(ARow-1, Trim(ChannelAreaGrid.Cells[ACol, ARow]));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelAreaValidator.UpdateChannelAreaName(AIndex: integer; AValue: string);
const OPNAME = 'TChannelAreaValidator.UpdateChannelAreaName';
var
  lMessage         : string;
  lChannelAreaList : IChannelAreaList;
  lChannelAreaData : IChannelArea;
begin
  try
    lChannelAreaList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData
                        .ChannelAreaList;
    if (lChannelAreaList <> nil) then
    begin
      lChannelAreaData := lChannelAreaList.ChannelAreaByIndex(AIndex);
      if (lChannelAreaData <> nil) then
      begin
        with ChannelAreaDialog do
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty('ChannelAreaName',AValue, lMessage)) then
          begin
            ChannelAreaGrid.ValidationError[1, AIndex+1, gveCellField] := '';
            lChannelAreaData.AreaName := AValue;
            RePopulateChannelAreaGrid;
            FValidateChannelAreaIndex := AIndex;
            DoContextValidation(dvtChannelAreaName);
            FValidateChannelAreaIndex := 0;
          end
          else
            ChannelAreaGrid.ValidationError[1, AIndex+1, gveCellField] := lMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelAreaValidator.OnChannelAreaGridSelectCell(ASender: TObject; ACol, ARow: Integer;
                                                            var CanSelect: Boolean);
const OPNAME = 'TChannelAreaValidator.OnChannelAreaGridSelectCell';
var
  lChannelArea : IChannelAreaList;
begin
  try
    CanSelect := True;
    lChannelArea := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData
                    .ChannelAreaList;
    with ChannelAreaDialog do
    begin
      if (ARow <> FSelectedChannelAreaIndex+1) then
      begin
        FSelectedChannelAreaIndex := ARow - 1;
        FSelectedChannelID := lChannelArea.ChannelAreaByIndex(FSelectedChannelAreaIndex).AreaID;
        RepopulateChannelListbox;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelAreaValidator.RePopulateChannelListBox;
const OPNAME = 'TChannelAreaValidator.RePopulateChannelListBox';
var
  lIndex       : integer;
  lChannelList : IChannelList;
  lChannel     : IGeneralFlowChannel;
begin
  try
    with ChannelAreaDialog do
    begin
      ChannelAreaListBox.Items.Clear;
      if (FSelectedChannelID > 0) then
      begin
        lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData
                        .ChannelList;
        if (lChannelList <> nil ) then
        begin
          for lIndex := 0 to lChannelList.ChannelCount - 1  do
          begin
            lChannel := lChannelList.ChannelByIndex[lIndex];
            if ((lChannel <> nil) AND
              (lChannel.ChannelArea = FSelectedChannelID)) then
            ChannelAreaListBox.Items.Add('(' + IntToStr(lChannel.ChannelNumber) + ') ' +
                                      lChannel.ChannelName);
          end
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelAreaValidator.SaveState: boolean;
const OPNAME = 'TChannelAreaValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelAreaValidator.ValidateChannelAreaCount(AChannelArea: IChannelAreaList);
const OPNAME = 'TChannelAreaValidator.ValidateChannelAreaCount';
begin
  try
    with ChannelAreaDialog do
    begin
      FErrorMessage := '';
      if (NOT AChannelArea.Validate(FErrorMessage, 'ChannelAreaCount')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      ChannelAreaCountEdt.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelAreaValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TChannelAreaValidator.DoContextValidation';
var
  lChannelArea     : IChannelArea;
  lChannelAreaList : IChannelAreaList;
begin
  try
    FAllErrorMessages.Clear;
    lChannelAreaList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                     ChannelAreaList;
    if (lChannelAreaList <> nil) then
    begin
      if (FValidateChannelAreaIndex >= 0) then
        lChannelArea := lChannelAreaList.ChannelAreaByIndex(FValidateChannelAreaIndex)
      else
        lChannelArea := nil;
      if (AValidationType in [dvtChannelAreas,
                              dvtChannelAreaCount]) then
        ValidateChannelAreaCount(lChannelAreaList);
      if (AValidationType in [dvtChannelAreas,
                              dvtChannelAreaName]) then
        ValidateChannelAreaName(lChannelArea);

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TChannelAreaValidator.ValidateChannelAreaName(AChannelArea: IChannelArea);
const OPNAME = 'TChannelAreaValidator.ValidateChannelAreaName';
begin
  try
    if (AChannelArea <> nil) then
    begin
      with ChannelAreaDialog do
      begin
        FErrorMessage := '';
        if (AChannelArea.Validate(FErrorMessage, 'ChannelAreaName')) then
          ChannelAreaGrid.ValidationError[1, FValidateChannelAreaIndex+1, gveCellContext] := ''
        else
        begin
          ChannelAreaGrid.ValidationError[1, FValidateChannelAreaIndex+1, gveCellContext] := FErrorMessage;
          FAllErrorMessages.Add(Trim(FErrorMessage));
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

