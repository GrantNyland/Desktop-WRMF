
{******************************************************************************}
{*  UNIT      : Contains the class TConfigurationFilesDialog.                 *}
{*  AUTHOR    : Presley Mudau                                                 *}
{*  DATE      : 2006/09/04                                                    *}
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UConfigurationFilesDialog;


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

  TConfigurationFilesDialog = class(TAbstractScrollablePanel)
  private

    FConfigurationGroupBox     :  TGroupBox;

    FDataPathPrefixLabel       : TLabel;
    FDataPathPrefixEdit        : TFieldEdit;
//    FDataPathPrefixButton      : TFieldButton;

    FInputPathLabel            : TLabel;
    FInputPathEdit             : TFieldEdit;
    FInputPathButton           : TFieldButton;

    FOutputPathLabel           : TLabel;
    FOutputPathEdit            : TFieldEdit;
    FOutputPathButton          : TFieldButton;

    FHydrologyPathLabel        : TLabel;
    FHydrologyPathEdit         : TFieldEdit;
    FHydrologyPathButton       : TFieldButton;

    FSpecifiedDemandPathLabel  : TLabel;
    FSpecifiedDemandPathEdit   : TFieldEdit;
    FSpecifiedDemandPathButton : TFieldButton;

    FParamGroupBox             : TGroupBox;
    FSelectParamFileButton     : TFieldButton;
    FParamFileNameEdit         : TFieldEdit;


  protected
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure RestoreColourState; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;


    property DataPathPrefixEdit        : TFieldEdit   read FDataPathPrefixEdit;
    property InputPathEdit             : TFieldEdit   read FInputPathEdit;
    property OutputPathEdit            : TFieldEdit   read FOutputPathEdit;
    property HydrologyPathEdit         : TFieldEdit   read FHydrologyPathEdit;
    property SpecifiedDemandPathEdit   : TFieldEdit   read FSpecifiedDemandPathEdit;

//    property DataPathPrefixButton      : TFieldButton read FDataPathPrefixButton;
    property InputPathButton           : TFieldButton read FInputPathButton;
    property OutputPathButton          : TFieldButton read FOutputPathButton;
    property HydrologyPathButton       : TFieldButton read FHydrologyPathButton;
    property SpecifiedDemandPathButton : TFieldButton read FSpecifiedDemandPathButton;
    property ParamFileNameEdit         : TFieldEdit   read FParamFileNameEdit;
    property SelectParamFileButton     : TFieldButton read FSelectParamFileButton;

  end;

  implementation

uses
  SysUtils,
  UHelpContexts,
  UErrorHandlingOperations,
  UControlCreationUtilities;

{******************************************************************************}
{* TConfigurationFilesDialog                                                       *}
{******************************************************************************}

procedure TConfigurationFilesDialog.CreateMemberObjects;
const OPNAME = 'TConfigurationFilesDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
  lParent : TWinControl;
  lTop    : Integer;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;
    lTop    := 10;
    FConfigurationGroupBox    := CreateFieldGroupBox            (lOwner,lParent,    5, 5, 600,150,0,FALSE);

    FDataPathPrefixLabel      := CreateFieldLabel               (lOwner, FConfigurationGroupBox,  10,  lTop{10}, 140,  21);
    FDataPathPrefixEdit       := CreateFieldEdit    (FAppModules, lOwner, FConfigurationGroupBox, 130, lTop{10}, 370,  21, 0, TRUE);
//    FDataPathPrefixButton     := CreateFieldButton  (FAppModules,lOwner, FConfigurationGroupBox,  510, 10, 30, 21,0,True,'');

    lTop := lTop + FDataPathPrefixEdit.Height + 4;

    if (FAppModules.StudyArea.ModelVersion <> '7') then
    begin
      FInputPathLabel           := CreateFieldLabel               ( lOwner, FConfigurationGroupBox,  10, lTop{35}, 140,  21);
      FInputPathEdit            := CreateFieldEdit    (FAppModules, lOwner, FConfigurationGroupBox, 130, lTop{35}, 370,  21, 0, TRUE);
      FInputPathButton          := CreateFieldButton  (FAppModules,lOwner, FConfigurationGroupBox,  510, lTop{35}, 30, 21,0,True,'');
      lTop := lTop + FInputPathEdit.Height + 4 ;
    end;

    FOutputPathLabel          := CreateFieldLabel                (lOwner, FConfigurationGroupBox,  10, lTop{60}, 140,  21);
    FOutputPathEdit           := CreateFieldEdit    (FAppModules, lOwner, FConfigurationGroupBox, 130, lTop{60}, 370,  21,  0, TRUE);
    FOutputPathButton         := CreateFieldButton  (FAppModules,lOwner, FConfigurationGroupBox,  510, lTop{60}, 30, 21,0,True,'');

    lTop := lTop + FOutputPathEdit.Height + 4 ;

    FHydrologyPathLabel       := CreateFieldLabel                (lOwner, FConfigurationGroupBox,  10, lTop{85}, 140,  21);
    FHydrologyPathEdit        := CreateFieldEdit    (FAppModules, lOwner, FConfigurationGroupBox, 130, lTop{85}, 370,  21,  1, TRUE);
    FHydrologyPathButton      := CreateFieldButton  (FAppModules,lOwner, FConfigurationGroupBox,  510, lTop{85}, 30, 21,0,True,'');

    lTop := lTop + FHydrologyPathEdit.Height + 4 ;

    FSpecifiedDemandPathLabel  := CreateFieldLabel                (lOwner, FConfigurationGroupBox,  10, lTop{110}, 140,  21);
    FSpecifiedDemandPathEdit   := CreateFieldEdit    (FAppModules, lOwner, FConfigurationGroupBox, 130,  lTop{110}, 370,  21,  1, TRUE);
    FSpecifiedDemandPathButton := CreateFieldButton  (FAppModules,lOwner, FConfigurationGroupBox,  510,  lTop{110} , 30, 21,0,True,'');

    FParamGroupBox          := CreateFieldGroupBox(lOwner, lParent,                                    5, 160, 600,  80, 0, FALSE);
    FParamFileNameEdit      := CreateFieldEdit    (FAppModules, FParamGroupBox, FParamGroupBox,       10, 20,  370,  21, 0, TRUE);
    FSelectParamFileButton  := CreateFieldButton  (FAppModules, FParamGroupBox, FParamGroupBox,      395, 20,   30,   21, 0, TRUE,'');


  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TConfigurationFilesDialog.Resize;
const OPNAME = 'TConfigurationFilesDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TConfigurationFilesDialog.Initialise: boolean;
const OPNAME = 'TConfigurationFilesDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TConfigurationFilesDialog.LanguageHasChanged: boolean;
const OPNAME = 'TConfigurationFilesDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FDataPathPrefixLabel.Caption      := ' ' + FAppModules.Language.GetString('TField.DataFilesPrefix');

    if (FAppModules.StudyArea.ModelVersion <> '7') then
      FInputPathLabel.Caption           := ' ' + FAppModules.Language.GetString('TField.InputPath');


    FOutputPathLabel.Caption          := ' ' + FAppModules.Language.GetString('TField.OutputPath');
    FHydrologyPathLabel.Caption       := ' ' + FAppModules.Language.GetString('TField.HydrologyPath');
    FSpecifiedDemandPathLabel.Caption := ' ' + FAppModules.Language.GetString('TField.SpecifiedDemandPath');

    FParamGroupBox.Caption            := ' ' + FAppModules.Language.GetString('RunParameters.ParamLocation');

//    FDataPathPrefixButton.Caption       := FAppModules.Language.GetString('LabelText.ThreeDots');

    if (FAppModules.StudyArea.ModelVersion <> '7') then
      FInputPathButton.Caption            := FAppModules.Language.GetString('LabelText.ThreeDots');


    FOutputPathButton.Caption           := FAppModules.Language.GetString('LabelText.ThreeDots');
    FHydrologyPathButton.Caption        := FAppModules.Language.GetString('LabelText.ThreeDots');
    FSpecifiedDemandPathButton.Caption  := FAppModules.Language.GetString('LabelText.ThreeDots');
    FSelectParamFileButton.Caption      := FAppModules.Language.GetString('LabelText.ThreeDots');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TConfigurationFilesDialog.RestoreColourState;
const OPNAME = 'TConfigurationFilesDialog.RestoreColourState';
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

procedure TConfigurationFilesDialog.AssignHelpContext;
const OPNAME = 'TConfigurationFilesDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,                     HC_DataFileLocation);

    if (FAppModules.StudyArea.ModelVersion = '7') then
      SetControlHelpContext(FInputPathEdit,         HC_DataFileLocation);

    SetControlHelpContext(FDataPathPrefixEdit,      HC_RunDescription);
    SetControlHelpContext(FOutputPathEdit,          HC_DataFileLocation);
    SetControlHelpContext(FHydrologyPathEdit,       HC_DataFileLocation);
    SetControlHelpContext(FSpecifiedDemandPathEdit, HC_DataFileLocation);
    SetControlHelpContext(FParamGroupBox,           HC_DataFileLocation);
    SetControlHelpContext(FParamFileNameEdit,       HC_DataFileLocation);
  except on E: Exception do HandleError(E, OPNAME); end;
end;
end.

