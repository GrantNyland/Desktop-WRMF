//
//
//  UNIT      : Contains TYieldModelDataGUIForm Class
//  AUTHOR    : Valentino Naicker (ARIVIA)
//  DATE      : 08/04/2003
//  COPYRIGHT : Copyright © 2003 DWAF
//
//

unit UYieldModelDataGUIForm;

interface

uses

  // Delphi
  Vcl.Forms,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,


  VCLTee.Chart,
  VCLTee.TeeProcs,
  VCLTee.Series,
  VCLTee.TEEngine,
  //VCLTee.TeeEdit,

  Classes,
  UWRMFThemes,
  UAbstractObject,
  UDataComponent,
  UAbstractComponent;

type


  TYieldModelDataGUIForm = class(TAbstractForm)
  protected
    FPanButton : TPanel;
    FBtnCancel : TButton;
    FBtnOk     : TButton;
    FDataDialogValidator: TAbstractDataDialogValidator;
    FInputPopupPageControl : TAbstractDataPageControl;
    // Member management issues
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;

  public
    procedure Resize; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function Initialise: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    function AddModelDataPanel(ADataPanel:TAbstractScrollablePanel): boolean;
    function AddModelDataDialogValidator(ADataDialogValidator:TAbstractDataDialogValidator): boolean;

    property BtnCancel : TButton read FBtnCancel;
    property BtnOk     : TButton read FBtnOk;
    property InputPopupPageControl : TAbstractDataPageControl read FInputPopupPageControl;
  end;

implementation

uses

  // Delphi
  Vcl.Controls,
  Sysutils,

  // DWAF
  UErrorHandlingOperations;

{ TYieldModelDataGUIForm }

procedure TYieldModelDataGUIForm.CreateMemberObjects;
const OPNAME = 'TYieldModelDataGUIForm.CreateMemberObjects';
begin
  try
    FPanButton := TPanel.Create(Self);
    FBtnOk := TButton.Create(Self);
    FBtnCancel := TButton.Create(Self);
    FBtnOk.ModalResult := mrOk;
    FBtnCancel.ModalResult := mrCancel;

    //Self.Position := poScreenCenter;
    Self.FormStyle := fsNormal;

    Self.ClientHeight := 448;
    Self.ClientWidth := 448;

    BorderStyle := bsSizeToolWin;

    Caption := Application.Title;

    FDataDialogValidator := nil;

    FInputPopupPageControl := TAbstractDataPageControl.Create(Self,FAppModules);


    inherited CreateMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelDataGUIForm.AssignHelpContext;
const OPNAME = 'TYieldModelDataGUIForm.AssignHelpContext';
begin
  inherited AssignHelpContext;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataGUIForm.LanguageHasChanged: boolean;
const OPNAME = 'TYieldModelDataGUIForm.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FBtnOk.Caption := FAppModules.Language.GetString('ButtonCaption.OK');
    FBtnCancel.Caption := FAppModules.Language.GetString('ButtonCaption.Cancel');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TYieldModelDataGUIForm.StudyHasChanged: boolean;
const OPNAME = 'TYieldModelDataGUIForm.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataGUIForm.Initialise: boolean;
const OPNAME = 'TYieldModelDataGUIForm.Initialise';
begin
  Result := inherited Initialise;
  try
    FPanButton.Parent := Self;
    FBtnOk.Parent := FPanButton;
    FBtnCancel.Parent := FPanButton;
    FBtnOk.ModalResult := mrOk;
    FBtnCancel.ModalResult := mrCancel;

    FPanButton.Height := FBtnOk.Height + 10;
    FPanButton.Align  := alTop;

    FBtnOk.Top := 5;
    FBtnCancel.Top := 5;
    FInputPopupPageControl.Parent := Self;
    FInputPopupPageControl.Align := alClient;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelDataGUIForm.Resize;
const OPNAME = 'TYieldModelDataGUIForm.Resize';
begin
  inherited;
  try
    FBtnOk.Left     := 10;
    FBtnCancel.Left := FBtnOk.Left + FBtnOk.Width + 10;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TYieldModelDataGUIForm.AddModelDataPanel(ADataPanel: TAbstractScrollablePanel): boolean;
const OPNAME = 'TYieldModelDataGUIForm.AddModelDataPanel';
begin
  Result := False;
  try
    if Assigned(ADataPanel) then
    begin
      ADataPanel.Parent := Self;
      ADataPanel.Align := alClient;
      ADataPanel.Visible := True;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataGUIForm.AddModelDataDialogValidator(ADataDialogValidator: TAbstractDataDialogValidator): boolean;
const OPNAME = 'TYieldModelDataGUIForm.AddModelDataDialogValidator';
begin
  Result := False;
  try
    if(ADataDialogValidator = nil) then Exit;
    if AddModelDataPanel(ADataDialogValidator.Panel) then
    begin
      FDataDialogValidator := ADataDialogValidator;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataGUIForm.StudyDataHasChanged(AContext: TChangeContext; AFieldName, AOldValue,ANewValue: string): boolean;
const OPNAME = 'TYieldModelDataGUIForm.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext, AFieldName, AOldValue,ANewValue);
  try
    if(FDataDialogValidator <> nil) then
      Result :=  FDataDialogValidator.StudyDataHasChanged(AContext, AFieldName, AOldValue,ANewValue);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
