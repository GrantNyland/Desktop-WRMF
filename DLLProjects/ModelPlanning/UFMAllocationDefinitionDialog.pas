{******************************************************************************}
{*  UNIT      : Contains the class TFMAllocationDefinitionDialog.             *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2006/01/11                                                    *}
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UFMAllocationDefinitionDialog;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.ExtCtrls,
  VCL.StdCtrls,
  VCL.Buttons,
  VCL.Graphics,
  UAbstractObject,
  UAbstractComponent,
  UDataEditComponent,
  UDataComponent;

type

  TFMAllocationDefinitionDialog = class(TAbstractScrollablePanel)
  private
  protected
    FLblAllocDefName     : TLabel;
    FLblStartDate        : TLabel;
    FLblEndDate          : TLabel;
    FLblDisplayEndDate   : TLabel;
    FEdtAllocDefName     : TFieldEdit;
    FCbxStartYear        : TFieldComboBox;
    FCbxStartMonth       : TFieldComboBox;
    FLblFileName         : TLabel;
    FEdtAllocDefFileName : TFieldEdit;
    FBtnFileSelector     : TFieldBitBtn;
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure RestoreColourState; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    property EdtAllocDefName     : TFieldEdit     read FEdtAllocDefName;
    property CbxStartYear        : TFieldComboBox read FCbxStartYear;
    property CbxStartMonth       : TFieldComboBox read FCbxStartMonth;
    property LblEndDate          : TLabel         read FLblEndDate;
    property LblDisplayEndDate   : TLabel         read FLblDisplayEndDate;
    property EdtAllocDefFileName : TFieldEdit     read FEdtAllocDefFileName;
    property BtnFileSelector     : TFieldBitBtn   read FBtnFileSelector;
  end;

implementation

uses
  SysUtils,
  UConstants,
  UHelpContexts,
  UErrorHandlingOperations,
  UControlCreationUtilities;

{******************************************************************************}
{* TFMAllocationDefinitionDialog                                              *}
{******************************************************************************}

procedure TFMAllocationDefinitionDialog.CreateMemberObjects;
const OPNAME = 'TFMAllocationDefinitionDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
  lParent : TWinControl;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;
    //                                                           Left  Top Width Height
    FLblAllocDefName      := CreateFieldLabel   (lOwner, lParent,  10,  10, 130, 21);
    FLblStartDate         := CreateFieldLabel   (lOwner, lParent,  10,  40, 130, 21);
    FLblEndDate           := CreateFieldLabel   (lOwner, lParent,  10,  70, 130, 21);
    FLblDisplayEndDate    := CreateFieldLabel   (lOwner, lParent,  140, 70, 130, 21);
    with FLblDisplayEndDate do
    begin
      Font.Style := [fsBold];
      Font.Size := 10;
    end;
    FLblEndDate.Visible := False;
    FLblDisplayEndDate.Visible := False;

    FEdtAllocDefName      := CreateFieldEdit    (FAppModules, lOwner, lParent, 140,  10, 220, 21, 0, FALSE);
    FCbxStartYear         := CreateFieldComboBox(FAppModules, lOwner, lParent, 140,  40,  80, 21, 3, TRUE, csDropDownList);
    FCbxStartMonth        := CreateFieldComboBox(FAppModules, lOwner, lParent, 230,  40,  80, 21, 3, TRUE, csDropDownList);
    FLblFileName          := CreateFieldLabel(lOwner, lParent, 10 , 70, 130, 21);
    FEdtAllocDefFileName  := CreateFieldEdit(FAppModules, lOwner, lParent, 140, 70, 300, 21, 0, FALSE);
    FBtnFileSelector      := TFieldBitBtn.Create(lOwner, FAppModules);
    with FBtnFileSelector do
    begin
      Parent := lParent;
      Left   := 450;
      Top    := 70;
      Width  := 30;
      Height := 21;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMAllocationDefinitionDialog.Resize;
const OPNAME = 'TFMAllocationDefinitionDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFMAllocationDefinitionDialog.Initialise: boolean;
const OPNAME = 'TFMAllocationDefinitionDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    FBtnFileSelector.Enabled := (FAppModules.User.UserRights in CUR_EditData) AND
                                (not FAppModules.StudyArea.ScenarioLocked);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFMAllocationDefinitionDialog.LanguageHasChanged: boolean;
const OPNAME = 'TFMAllocationDefinitionDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FLblAllocDefName.Caption       := FAppModules.Language.GetString('PlanningGUI.AllocDefName');
    FLblStartDate.Caption          := FAppModules.Language.GetString('PlanningGUI.AllocDefStartDate');
    FLblEndDate.Caption            := FAppModules.Language.GetString('PlanningGUI.AllocDefEndDate');
    FLblFileName.Caption           := FAppModules.Language.GetString('PlanningGUI.AllocDefFileName');
    FBtnFileSelector.Caption       := '...';
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMAllocationDefinitionDialog.RestoreColourState;
const OPNAME = 'TFMAllocationDefinitionDialog.RestoreColourState';
var
  LIndex : integer;
begin
  inherited RestoreColourState;
  try
    for LIndex := 0 to ControlsOwner.ComponentCount - 1 do
      if ControlsOwner.Components[LIndex].ClassName = TFieldEdit.ClassName then
        if TFieldEdit(ControlsOwner.Components[LIndex]).Color = clRed then
          TFieldEdit(ControlsOwner.Components[LIndex]).Color := clWindow;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMAllocationDefinitionDialog.AssignHelpContext;
const OPNAME = 'TFMAllocationDefinitionDialog.AssignHelpContext';
begin
  try
{
    SetControlHelpContext(Self,                HC_CreatingChannels);
    SetControlHelpContext(FEdtAllocDefName,    HC_CreatingChannels);
    SetControlHelpContext(FCbxStartYear,       HC_CreatingChannels);
    SetControlHelpContext(FCbxStartMonth,      HC_ChannelPenaltyStructures);
    SetControlHelpContext(FCbxEndYear,         HC_CreatingChannels);
    SetControlHelpContext(FCbxEndMonth,        HC_CreatingChannels);
}
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
