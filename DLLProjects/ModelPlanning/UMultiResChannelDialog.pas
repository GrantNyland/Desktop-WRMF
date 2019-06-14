{******************************************************************************}
{*  UNIT      : Contains the class TReservoirAndChannelsOutputDialog          *}
{*  AUTHOR    : Presley Mudau                                                 *}
{*  DATE      : 2004/12/23                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit UMultiResChannelDialog;

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
  //UReservoirData;

type

  TMultiResChannelDialog = class(TAbstractScrollablePanel)
  private
    FDecisionMonthLabel         : TLabel;
    FStartMonthLabel            : TLabel;

    FReservoirNoLabel           : TLabel;
    FReservoirNoCombo          :  TFieldComboBox;

    FDecisionMonthField         : TFieldComboBox;
    FStartMonthField            : TFieldComboBox;
    FCurtailRestrictionGrid     : TFieldStringGrid;

  protected
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public

    procedure Resize; override;
    procedure RestoreColourState; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;

   // property DecisionMonthLabel         : TLabel read FDecisionMonthLabel write FDecisionMonthLabel;
   // property StartMonthLabel            : TLabel read FStartMonthLabel write FStartMonthLabel;
   // property ReservoirNoLabel           : TLabel read FReservoirNoLabel write FReservoirNoLabel;

    property ReservoirNoCombo           : TFieldComboBox read FReservoirNoCombo write FReservoirNoCombo;
    property CurtailRestrictionGrid     : TFieldStringGrid read FCurtailRestrictionGrid write FCurtailRestrictionGrid;
    property DecisionMonthField : TFieldComboBox read FDecisionMonthField write FDecisionMonthField;
    property StartMonthField: TFieldComboBox read FStartMonthField write FStartMonthField;

  end;

  implementation

uses
  SysUtils,
  UHelpContexts,
  UErrorHandlingOperations,
  UControlCreationUtilities,
  UBasicObjects,
  VCL.ImgList;

{******************************************************************************}
{* TReservoirAndChannelOutputDialog                                                 *}
{******************************************************************************}
procedure TMultiResChannelDialog.CreateMemberObjects;
const OPNAME = 'TMultiResChannelDialog.CreateMemberObjects';
  //lOwner  : TComponent;
  //lParent : TWinControl;
  const defaultSpacing = 10;
begin
  inherited CreateMemberObjects;
  try
    //lOwner  := ControlsOwner;
    //lParent := ControlsParent;
    //                                                                 Left  Top  Width Height

   FReservoirNoLabel           :=    TLabel.Create(ControlsOwner);
   FReservoirNoLabel.Parent    :=    ControlsParent;


   FReservoirNoCombo           :=    TFieldComboBox.Create(ControlsOwner,FAppModules);
   FReservoirNoCombo.Parent    :=    ControlsParent;
   FReservoirNoCombo.Style     := TComboBoxStyle.csDropDownList;

   FDecisionMonthLabel         :=    TLabel.Create(ControlsOwner);
   FDecisionMonthLabel.Parent  :=    ControlsParent;

   FStartMonthLabel            :=    TLabel.Create(ControlsOwner);
   FStartMonthLabel.Parent     :=    ControlsParent;

   FDecisionMonthField         :=    TFieldComboBox.Create(ControlsOwner,FAppModules);
   FDecisionMonthField.Parent  :=    ControlsParent;
   FDecisionMonthField.Style     := TComboBoxStyle.csDropDownList;
   FStartMonthField            :=    TFieldComboBox.Create(ControlsOwner,FAppModules);
   FStartMonthField.Parent     :=    ControlsParent;
   FStartMonthField.Style     := TComboBoxStyle.csDropDownList;
   FCurtailRestrictionGrid     :=    TFieldStringGrid.Create(ControlsOwner,FAppModules);
   FCurtailRestrictionGrid.Parent      := ControlsParent;

   Resize;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMultiResChannelDialog.Resize;
const OPNAME = 'TMultiResChannelDialog.Resize';
const defaultSpacing = 10;
const defaultHeight = 20;
begin
  inherited Resize;
  try
    FReservoirNoLabel.Top := defaultSpacing;
    FReservoirNoLabel.Left :=  defaultSpacing;
    FReservoirNoLabel.Height := defaultHeight;
    FReservoirNoLabel.AutoSize := TRUE;

    FReservoirNoCombo.Top := FReservoirNoLabel.Top ;
    FReservoirNoCombo.Left := 120;
    FReservoirNoCombo.Height := FReservoirNoLabel.Height;
    FReservoirNoCombo.Width  := 200;


    FDecisionMonthLabel.Top := FReservoirNoLabel.Top + FReservoirNoLabel.Height + defaultSpacing;
    FDecisionMonthLabel.left := FReservoirNoLabel.Left;
    FDecisionMonthLabel.Height := FReservoirNoLabel.Height;

    FDecisionMonthField.Top := FDecisionMonthLabel.Top;
    FDecisionMonthField.Left := FReservoirNoCombo.Left;
    FDecisionMonthField.Height := FDecisionMonthLabel.Height;
    FDecisionMonthField.Width  := 50;

    FStartMonthLabel.Top := FDecisionMonthLabel.Top + FDecisionMonthLabel.Height + defaultSpacing;
    FStartMonthLabel.Left := defaultSpacing;
    FStartMonthLabel.Height := FDecisionMonthLabel.Height;

    FStartMonthField.Top := FStartMonthLabel.Top;
    FStartMonthField.Left := FReservoirNoCombo.Left;
    FStartMonthField.Height := FDecisionMonthField.Height;
    FStartMonthField.Width := FDecisionMonthField.Width;
    FStartMonthField.Width  := 50;

    FCurtailRestrictionGrid.Top := FStartMonthLabel.Top + FStartMonthLabel.Height + defaultSpacing;
    FCurtailRestrictionGrid.Left := FReservoirNoCombo.Left;
   // FCurtailRestrictionGrid.Width := FCurtailRestrictionGrid.DefaultColWidth * FCurtailRestrictionGrid.ColCount + 20;
   // FCurtailRestrictionGrid.Height := FCurtailRestrictionGrid.DefaultRowHeight * 11;

    FCurtailRestrictionGrid.Width := 3 + (1 + FCurtailRestrictionGrid.DefaultColWidth) * FCurtailRestrictionGrid.ColCount;
    FCurtailRestrictionGrid.Height := 3 + (1 + FCurtailRestrictionGrid.DefaultRowHeight) * FCurtailRestrictionGrid.RowCount;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMultiResChannelDialog.Initialise: boolean;
const OPNAME = 'TMultiResChannelDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMultiResChannelDialog.LanguageHasChanged: boolean;
const OPNAME = 'TMultiResChannelDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FReservoirNoLabel.Caption   :=    FAppModules.Language.GetString('TMultiResChannelDialog.ReservoirNoLabel');
    FDecisionMonthLabel.Caption :=    FAppModules.Language.GetString('TMultiResChannelDialog.DecisionMonthLabel');
    FStartMonthLabel.Caption    :=    FAppModules.Language.GetString('TMultiResChannelDialog.StartMonthLabel');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMultiResChannelDialog.RestoreColourState;
const OPNAME = 'TMultiResChannelDialog.RestoreColourState';
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

procedure TMultiResChannelDialog.AssignHelpContext;
const OPNAME = 'TReservoirAndChannelOutputDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,                      HC_MultiResChannelCurtail);
    SetControlHelpContext(ReservoirNoCombo,                      HC_MultiReservoir);
    SetControlHelpContext(DecisionMonthField,     HC_MultiCurDecisionMonth);
    SetControlHelpContext(StartMonthField, HC_MultiCurStartMonth);
    SetControlHelpContext(CurtailRestrictionGrid,    HC_MultiCurElevation);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.

