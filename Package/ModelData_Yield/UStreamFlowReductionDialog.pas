//
//
//  UNIT      : Contains TStreamFlowReduction Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 22/11/2006
//  COPYRIGHT : Copyright © 2006 DWAF
//
//

unit UStreamFlowReductionDialog;

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

  TStreamFlowReductionDialog = class(TAbstractScrollablePanel)
  protected
    FSFRIdentifierEdit             : TFieldEdit;
    FSFRNameLabel                  : TLabel;
    FSFRNameEdit                   : TFieldEdit;
    FInflowNodeNumberLabel         : TLabel;
    FInflowNodeNumberCbx           : TFieldComboBox;
    FSFRDescrLabel                 : TLabel;
    FSFRDescrEdit                  : TFieldRichEdit;
    FCoveredAreaLabel              : TLabel;
    FCoveredAreaEdit               : TFieldEdit;
    FUnitRunoffFileNameLabel       : TLabel;
    FUnitRunoffFileNameCbx         : TFieldComboBox;
    FUnitRunoffFileNameSelectBtn   : TFieldButton;
    FUnitRunoffFileNameGridBtn     : TFieldBitBtn;
    FUnitRunoffFileNameGraphBtn    : TFieldBitBtn;
    FUnitRunoffFileImportedLabel   : TLabel;

    FSoilMoistureFileNameLabel     : TLabel;
    FSoilMoistureFileNameCbx       : TFieldComboBox;
    FSoilMoistureFileNameSelectBtn : TFieldButton;
    FSoilMoistureFileNameGridBtn   : TFieldBitBtn;
    FSoilMoistureFileNameGraphBtn  : TFieldBitBtn;
    FSoilMoistureFileImportedLabel : TLabel;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;

    property SFRIdentifierEdit             : TFieldEdit       read FSFRIdentifierEdit;
    property SFRNameEdit                   : TFieldEdit       read FSFRNameEdit;
    property SFRDescrEdit                  : TFieldRichEdit   read FSFRDescrEdit;
    property InflowNodeNumberCbx           : TFieldComboBox   read FInflowNodeNumberCbx;
    property CoveredAreaEdit               : TFieldEdit       read FCoveredAreaEdit;
    property UnitRunoffFileNameCbx         : TFieldComboBox   read FUnitRunoffFileNameCbx;
    property UnitRunoffFileNameSelectBtn   : TFieldButton     read FUnitRunoffFileNameSelectBtn;
    property UnitRunoffFileNameGridBtn     : TFieldBitBtn     read FUnitRunoffFileNameGridBtn;
    property UnitRunoffFileNameGraphBtn    : TFieldBitBtn     read FUnitRunoffFileNameGraphBtn;
    property UnitRunoffFileImportedLabel   : TLabel           read FUnitRunoffFileImportedLabel;

    property SoilMoistureFileNameCbx       : TFieldComboBox   read FSoilMoistureFileNameCbx;
    property SoilMoistureFileNameSelectBtn : TFieldButton     read FSoilMoistureFileNameSelectBtn;
    property SoilMoistureFileNameGridBtn   : TFieldBitBtn     read FSoilMoistureFileNameGridBtn;
    property SoilMoistureFileNameGraphBtn  : TFieldBitBtn     read FSoilMoistureFileNameGraphBtn;
    property SoilMoistureFileImportedLabel : TLabel           read FSoilMoistureFileImportedLabel;
  end;

  implementation

uses
  SysUtils,
  UHelpContexts,
  UControlCreationUtilities,
  UErrorHandlingOperations;

const
  C_ControlBorder  = 5;
  C_LabelOffset    = 3;
  C_GroupBoxOffset = 5;


{ TStreamFlowReductionDialog }

procedure TStreamFlowReductionDialog.DestroyMemberObjects;
const OPNAME = 'TStreamFlowReductionDialog.DestroyMemberObjects';
begin
  try
    inherited;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStreamFlowReductionDialog.Initialise: boolean;
const OPNAME = 'TStreamFlowReductionDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    FUnitRunoffFileImportedLabel.AutoSize := True;
    FUnitRunoffFileImportedLabel.Font.Color := clRed;
    FSoilMoistureFileImportedLabel.AutoSize := True;
    FSoilMoistureFileImportedLabel.Font.Color := clRed;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStreamFlowReductionDialog.Resize;
const OPNAME = 'TStreamFlowReductionDialog.Resize';
begin
  try
    inherited;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStreamFlowReductionDialog.CreateMemberObjects;
const OPNAME = 'TStreamFlowReductionDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
  lParent : TWinControl;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;
    FSFRNameLabel                  := CreateFieldLabel  (lOwner, lParent,  10, 10, 140, 21);
    FInflowNodeNumberLabel         := CreateFieldLabel  (lOwner, lParent,  10, 35, 140, 21);
    FCoveredAreaLabel              := CreateFieldLabel  (lOwner, lParent,  10, 60, 140, 21);
    FUnitRunoffFileNameLabel       := CreateFieldLabel  (lOwner, lParent,  10, 85, 140, 21);
    FUnitRunoffFileImportedLabel   := CreateFieldLabel  (lOwner, lParent, 545, 85, 140, 21);
    FSoilMoistureFileNameLabel     := CreateFieldLabel  (lOwner, lParent,  10,110, 140, 21);
    FSoilMoistureFileImportedLabel := CreateFieldLabel  (lOwner, lParent, 545,110, 140, 21);
    FSFRDescrLabel                 := CreateFieldLabel  (lOwner, lParent,  10,135, 140, 21);

    FSFRIdentifierEdit             := CreateFieldEdit    (FAppModules, lOwner, lParent, 160, 10, 35, 21, 0, FALSE);
    FSFRNameEdit                   := CreateFieldEdit    (FAppModules, lOwner, lParent, 200, 10, 240, 21, 1, TRUE);
    FInflowNodeNumberCbx           := CreateFieldComboBox(FAppModules, lOwner, lParent, 160, 35, 280, 21, 3,   TRUE, csDropDownList);
    FCoveredAreaEdit               := CreateFieldEdit    (FAppModules, lOwner, lParent, 160, 60, 210, 21, 2, TRUE);

    FUnitRunoffFileNameCbx         := CreateFieldComboBox (FAppModules, lOwner, lParent, 160, 85, 280, 21,3, TRUE, csDropDownList);
    FUnitRunoffFileNameSelectBtn   := CreateFieldButton   (FAppModules, lOwner, lParent, 445, 85,  25, 21, 4, TRUE, '...');
    FUnitRunoffFileNameGridBtn     := CreateFieldBitButton(FAppModules, lOwner, lParent, 485, 85,  25, 21, 5, TRUE, 'VIEWDATAGRID');
    FUnitRunoffFileNameGraphBtn    := CreateFieldBitButton(FAppModules, lOwner, lParent, 515, 85,  25, 21, 6, TRUE, 'VIEWDATAGRAPH');

    FSoilMoistureFileNameCbx       := CreateFieldComboBox (FAppModules, lOwner, lParent, 160, 110, 280, 21, 7, TRUE, csDropDownList);
    FSoilMoistureFileNameSelectBtn := CreateFieldButton   (FAppModules, lOwner, lParent, 445, 110,  25, 21, 8, TRUE, '...');
    FSoilMoistureFileNameGridBtn   := CreateFieldBitButton(FAppModules, lOwner, lParent, 485, 110,  25, 21, 9, TRUE, 'VIEWDATAGRID');
    FSoilMoistureFileNameGraphBtn  := CreateFieldBitButton(FAppModules, lOwner, lParent, 515, 110,  25, 21, 10, TRUE, 'VIEWDATAGRAPH');
    FSFRDescrEdit                  := CreateFieldRichEdit (FAppModules, lOwner, lParent, 160, 135, 280, 80, 11, TRUE);


  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStreamFlowReductionDialog.LanguageHasChanged: boolean;
const OPNAME = 'TStreamFlowReductionDialog.LanguageHasChanged';
begin
  Result := True;
  try
    FSFRNameLabel.Caption            := FAppModules.Language.GetString('TField.SFRName') + ' :';
    FSFRDescrLabel.Caption           := FAppModules.Language.GetString('TField.SFRDescr') + ' :';
    FInflowNodeNumberLabel.Caption   := FAppModules.Language.GetString('TField.InflowNodeNumber')   + ' :';
    FCoveredAreaLabel.Caption        := FAppModules.Language.GetString('TField.CoveredArea') + ' :';
    FUnitRunoffFileNameLabel.Caption := FAppModules.Language.GetString('TField.UnitRunoffFileName') + ' :';
    FSoilMoistureFileNameLabel.Caption := FAppModules.Language.GetString('TField.SoilMoistureFileName') + ' :';

    FUnitRunoffFileNameSelectBtn.Hint    := FAppModules.Language.GetString('ButtonHint.SelectUnitRunoffFile');
    UnitRunoffFileNameGridBtn.Hint       := FAppModules.Language.GetString('ButtonHint.ViewDataGrid');
    UnitRunoffFileNameGraphBtn.Hint      := FAppModules.Language.GetString('ButtonHint.ViewDataGraph');

    FSoilMoistureFileNameSelectBtn.Hint    := FAppModules.Language.GetString('ButtonHint.SelectSoilMoistureFile');
    SoilMoistureFileNameGridBtn.Hint       := FAppModules.Language.GetString('ButtonHint.ViewDataGrid');
    SoilMoistureFileNameGraphBtn.Hint      := FAppModules.Language.GetString('ButtonHint.ViewDataGraph');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStreamFlowReductionDialog.AssignHelpContext;
const OPNAME = 'TStreamFlowReductionDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,                     HC_StreamflowReductions);
    SetControlHelpContext(FSFRIdentifierEdit,       HC_StreamflowReductions);
    SetControlHelpContext(FSFRNameEdit,             HC_StreamflowReductions);
    SetControlHelpContext(FInflowNodeNumberCbx,     HC_StreamflowReductions);
    SetControlHelpContext(FSFRDescrEdit,            HC_StreamflowReductions);
    SetControlHelpContext(FCoveredAreaEdit,         HC_StreamflowReductions);
    SetControlHelpContext(FUnitRunoffFileNameCbx,   HC_StreamflowReductions);
    SetControlHelpContext(FSoilMoistureFileNameCbx, HC_StreamflowReductions);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
