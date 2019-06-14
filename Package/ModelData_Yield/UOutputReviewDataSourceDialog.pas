//
//
//  UNIT      : Contains the class TOutputReviewDataSourceDialog.
//  AUTHOR    : Dziedzi Ramulondi
//  DATE      : 2007/09/20
//  COPYRIGHT : Copyright © 2007 DWAF
//
//

unit UOutputReviewDataSourceDialog;

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

  TOutputReviewDataSourceDialog = class(TAbstractScrollablePanel)
  protected
    FCurrentDataSourceLabel        : TLabel;
    FCurrentDataSourceEdit         : TFieldEdit;
    FAvailableDataSourceLabel      : TLabel;
    FAvailableDataSourcesCombo     : TFieldComboBox;
    FLoadFromSourceButton          : TFieldButton;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;

    property CurrentDataSourceEdit     : TFieldEdit     read FCurrentDataSourceEdit;
    property AvailableDataSourcesCombo : TFieldComboBox read FAvailableDataSourcesCombo;
    property LoadFromSourceButton      : TFieldButton   read FLoadFromSourceButton;
  end;

  implementation

uses
  SysUtils,
  UHelpContexts,
  UControlCreationUtilities,
  UErrorHandlingOperations;


{ TOutputReviewDataSourceDialog }

procedure TOutputReviewDataSourceDialog.CreateMemberObjects;
const OPNAME = 'TOutputReviewDataSourceDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
  lParent : TWinControl;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;
    FAvailableDataSourceLabel     := CreateFieldLabel (lOwner, lParent,  10, 10, 140, 21);
    FCurrentDataSourceLabel       := CreateFieldLabel (lOwner, lParent,  10, 35, 140, 21);
    FCurrentDataSourceEdit        := CreateFieldEdit(FAppModules, lOwner, lParent, 160, 10, 250, 21, 0, True);
    FAvailableDataSourcesCombo    := CreateFieldComboBox(FAppModules, lOwner, lParent, 160, 35, 250, 21, 0, TRUE,csDropDownList);
    FLoadFromSourceButton         := CreateFieldButton(FAppModules, lOwner, lParent, 413, 35, 70, 21, 0, TRUE,'');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputReviewDataSourceDialog.DestroyMemberObjects;
const OPNAME = 'TOutputReviewDataSourceDialog.DestroyMemberObjects';
begin
  try
    inherited;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputReviewDataSourceDialog.Initialise: boolean;
const OPNAME = 'TOutputReviewDataSourceDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputReviewDataSourceDialog.Resize;
const OPNAME = 'TOutputReviewDataSourceDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputReviewDataSourceDialog.LanguageHasChanged: boolean;
const OPNAME = 'TOutputReviewDataSourceDialog.LanguageHasChanged';
begin
  Result := True;
  try
    FAvailableDataSourceLabel.Caption  := FAppModules.Language.GetString('OutputReview.CurrentDataSource') + ' :';
    FCurrentDataSourceLabel.Caption    := FAppModules.Language.GetString('OutputReview.AvailableDataSources') + ' :';
    FLoadFromSourceButton.Caption      := FAppModules.Language.GetString('OutputReview.LoadFromSource');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputReviewDataSourceDialog.AssignHelpContext;
const OPNAME = 'TOutputReviewDataSourceDialog.AssignHelpContext';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
