{******************************************************************************}
{*  UNIT      : Contains the class TWizardForm.                               *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2004/07/13                                                    *}
{*  COPYRIGHT : Copyright © 2004 DWAF                                         *}
{******************************************************************************}

unit UWizardForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, VCL.ExtCtrls, VCL.ComCtrls,
  UWizardStep,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  VCL.ImgList;

type
  TWizardForm = class(TAbstractForm)
  private
    FImageList          : TImageList;
    FWizardTreeView     : TTreeView;
    FSplitter1          : TSplitter;
    FRightPanel         : TPanel;
    FTopPanel           : TPanel;
    FHeadingLabel       : TLabel;
    FDescriptionMemo    : TMemo;
    FBottomPanel        : TPanel;
    FFinishButton       : TButton;
    FNextButton         : TButton;
    FPreviousButton     : TButton;
    FErrorMemo          : TMemo;
    FClientScrollBox    : TScrollBox;
    FWizard             : TWizard;
    FCurrentDialog      : TAbstractScrollablePanel;
    FSystemChange       : Boolean;
    FCurrentStep        : TWizardStep;
  protected
    procedure OnFinishButtonClick (Sender : TObject);
    procedure OnResizeForm (Sender : TObject);
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure NextButtonClick(Sender: TObject);
    procedure PreviousButtonClick(Sender: TObject);
    procedure FormClose (Sender     : TObject;
                         var Action : TCloseAction);
    procedure WizardTreeViewChanging (Sender          : TObject;
                                      Node            : TTreeNode;
                                      var AllowChange : Boolean);
    procedure WizardTreeViewCustomDrawItem (ASender : TCustomTreeView;
                                            ANode   : TTreeNode;
                                            AState  : TCustomDrawState;
                                            var ADefaultDraw : Boolean);
    procedure AddStepsToTree (AStepList   : TWizard;
                              AParentNode : TTreeNode);
    procedure ShowDialog;
    procedure SelectStepInTree;
    procedure DoDataHasChanged (Sender : TObject);
  public
    function StudyDataHasChanged(AContext: TChangeContext;
                                 AFieldName,AOldValue,ANewValue: string): boolean; override;
    function LanguageHasChanged: boolean; override;
    procedure SetFormCaption(AValue : String);
    procedure ClearWizard;
    procedure PopulateWizard;
    property Wizard : TWizard read FWizard;
    property ValidatorParent : TScrollBox read FClientScrollBox;

  end;

implementation

uses
  System.UITypes,
  UErrorHandlingOperations, Math;

{******************************************************************************}
{* TWizardForm                                                                *}
{******************************************************************************}

procedure TWizardForm.CreateMemberObjects;
const OPNAME = 'TWizardForm.CreateMemberObjects';
begin
  try
    FSystemChange := FALSE;
    FWizard       := TWizard.Create(FAppModules);
    Position := poScreenCenter;
    Width    := 758;
    Height   := 527;
    Caption  := 'WizardMainForm';
    OnClose  := FormClose;

    FImageList := TImageList.Create(Self);
    FImageList.Height := 18;
    FImageList.Width  := 18;
    FImageList.GetInstRes(HImagesInstance, rtBitmap, 'UNCHECKWHITE', 16, [], clWhite);
    FImageList.GetInstRes(HImagesInstance, rtBitmap, 'UNCHECKGRAY',  16, [], clWhite);
    FImageList.GetInstRes(HImagesInstance, rtBitmap, 'CHECKGRAY',    16, [], clWhite);

    FWizardTreeView := TTreeView.Create(Self);
    with FWizardTreeView do
    begin
      Parent   := Self;
      Left     := 0;
      Top      := 0;
      Width    := 190;
      Height   := 500;
      Align    := alLeft;
      Indent   := 19;
      TabOrder := 0;
      ReadOnly := TRUE;
      ShowRoot := FALSE;
      HideSelection := FALSE;
      Images        := FImageList;
      OnChanging    := WizardTreeViewChanging;
      OnCustomDrawItem := WizardTreeViewCustomDrawItem;
    end;
    FSplitter1 := TSplitter.Create(Self);
    with FSplitter1 do
    begin
      Parent := Self;
      Left   := 190;
      Top    := 0;
      Width  := 3;
      Height := 500;
      Cursor := crHSplit;
      OnMoved := OnResizeForm;
    end;
    FRightPanel := TPanel.Create(Self);
    with FRightPanel do
    begin
      Parent     := Self;
      Left       := 193;
      Top        := 0;
      Width      := 557;
      Height     := 500;
      Align      := alClient;
      BevelOuter := bvNone;
      TabOrder   := 1;
    end;
    FTopPanel := TPanel.Create(Self);
    with FTopPanel do
    begin
      Parent     := FRightPanel;
      Left       := 0;
      Top        := 0;
      Width      := 557;
      Height     := 80;
      Align      := alTop;
      BevelInner := bvLowered;
      BevelOuter := bvNone;
      TabStop    := FALSE;
      TabOrder   := 0;
    end;
    FHeadingLabel := TLabel.Create(Self);
    with FHeadingLabel do
    begin
      Parent       := FTopPanel;
      Left         := 1;
      Top          := 1;
      Width        := 555;
      Height       := 30;
      Align        := alTop;
      Alignment    := taCenter;
      AutoSize     := False;
      Caption      := 'HeadingLabel';
      Font.Charset := DEFAULT_CHARSET;
      Font.Color   := clWindowText;
      Font.Height  := -16;
      Font.Name    := 'MS Sans Serif';
      Font.Style   := [fsBold];
      ParentFont   := False;
      Layout       := tlCenter;
    end;
    FDescriptionMemo := TMemo.Create(Self);
    with FDescriptionMemo do
    begin
      Parent       := FTopPanel;
      Left         := 1;
      Top          := 31;
      Width        := 555;
      Height       := 48;
      Align        := alClient;
      Alignment    := taCenter;
      Color        := clBtnFace;
      Ctl3D        := False;
      ParentCtl3D  := False;
      TabStop      := FALSE;
      TabOrder     := 0;
      ScrollBars   := ssVertical;
    end;
    FBottomPanel := TPanel.Create(Self);
    with FBottomPanel do
    begin
      Parent     := FRightPanel;
      Left       := 0;
      Top        := 416;
      Width      := 557;
      Height     := 84;
      Align      := alBottom;
      BevelInner := bvLowered;
      BevelOuter := bvNone;
      TabOrder   := 2;
    end;
    FErrorMemo := TMemo.Create(Self);
    with FErrorMemo do
    begin
      Parent      := FBottomPanel;
      Left        := 1;
      Top         := 1;
      Width       := 555;
      Height      := 45;
      Align       := alTop;
      Color       := clBtnFace;
      Ctl3D       := False;
      ParentCtl3D := False;
      TabStop     := FALSE;
      TabOrder    := 0;
      Font.Color  := clRed;
      ScrollBars  := ssVertical;
    end;
    FPreviousButton := TButton.Create(Self);
    with FPreviousButton do
    begin
      Parent   := FBottomPanel;
      Left     := 285;
      Top      := 52;
      Width    := 75;
      Height   := 25;
      TabOrder := 1;
      OnClick  := PreviousButtonClick;
    end;
    FNextButton := TButton.Create(Self);
    with FNextButton do
    begin
      Parent   := FBottomPanel;
      Left     := 370;
      Top      := 52;
      Width    := 75;
      Height   := 25;
      TabOrder := 2;
      OnClick  := NextButtonClick;
    end;
    FFinishButton := TButton.Create(Self);
    with FFinishButton do
    begin
      Parent   := FBottomPanel;
      Left     := 455;
      Top      := 52;
      Width    := 75;
      Height   := 25;
      TabOrder := 3;
      OnClick  := OnFinishButtonClick;
    end;
    FClientScrollBox := TScrollBox.Create(Self);
    with FClientScrollBox do
    begin
      Parent     := FRightPanel;
      Left       := 0;
      Top        := 80;
      Width      := 557;
      Height     := 336;
      Align      := alClient;
      BevelInner := bvNone;
      TabOrder   := 1;
    end;
    FCurrentDialog := nil;
    Self.OnResize := OnResizeForm;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWizardForm.OnResizeForm(Sender: TObject);
const OPNAME = 'TWizardForm.OnResizeForm';
begin
  try
    FFinishButton.Left := FBottomPanel.Width - 5 - FFinishButton.Width;
    FNextButton.Left  := FFinishButton.Left - 5 - FNextButton.Width;
    FPreviousButton.Left := FNextButton.Left - 5 - FPreviousButton.Width;
    if (Assigned(FCurrentDialog)) then
    begin
      FCurrentDialog.Height := FClientScrollBox.Height - 4;
      FCurrentDialog.Width := FClientScrollBox.Width - 4;
    end;  
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWizardForm.DestroyMemberObjects;
const OPNAME = 'TWizardForm.DestroyMemberObjects';
begin
  try
    FreeAndNil(FWizard);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWizardForm.SetFormCaption(AValue: String);
const OPNAME = 'TWizardForm.SetFormCaption';
begin
  try
    Caption := AValue;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TWizardForm.ClearWizard;
const OPNAME = 'TWizardForm.ClearWizard';
begin
  try
    FWizardTreeView.Items.Clear;
    FCurrentStep   := nil;
    FCurrentDialog := nil;
    if (Assigned(FWizard)) then
      FWizard.ClearSteps;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWizardForm.PopulateWizard;
const OPNAME = 'TWizardForm.PopulateWizard';
begin
  try
    if (Assigned(FWizard)) then
    begin
      AddStepsToTree(FWizard, nil);
      FWizardTreeView.FullExpand;
      FCurrentStep := FWizard.FirstLeafStep;
      FFinishButton.Caption := FWizard.FinishButtonCaption;
      DoDataHasChanged(Self);
    end;
    ShowDialog;
    SelectStepInTree;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWizardForm.AddStepsToTree (AStepList   : TWizard;
                                      AParentNode : TTreeNode);
const OPNAME = 'TWizardForm.AddStepsToTree';
var
  lIndex   : integer;
  lStep    : TWizardStep;
  lNode    : TTreeNode;
begin
  try
    for lIndex := 0 to AStepList.StepList.Count - 1 do
    begin
      lStep := TWizardStep(AStepList.StepList.Items[lIndex]);
      lStep.Validator.OnDataChange := DoDataHasChanged;
      lNode := FWizardTreeView.Items.AddChildObject(AParentNode, lStep.Heading, lStep);
      lStep.TreeNode := lNode;
      if (lStep.SubSteps.StepList.Count > 0) then
        AddStepsToTree(lStep.SubSteps, lNode);
      if (NOT Assigned(lStep.ParentStep)) then
      begin
        lStep.UpdateStatus;
        lStep.UpdateTreeNode;
      end;
    end;
    FWizardTreeView.Repaint;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWizardForm.ShowDialog;
const OPNAME = 'TWizardForm.ShowDialog';
var
  lValidator : TAbstractDataDialogValidator;
begin
  try
    if (Assigned(FCurrentDialog)) then
      FCurrentDialog.Hide;
    if (Assigned(FCurrentStep)) then
    begin
      lValidator := FCurrentStep.Validator;
      if (Assigned(lValidator)) then
      begin
        FCurrentDialog := lValidator.Panel;
        if (FCurrentDialog.Height < FClientScrollBox.Height) then
          FCurrentDialog.Height := FClientScrollBox.Height - 4;
        if (FCurrentDialog.Width < FClientScrollBox.Width) then
          FCurrentDialog.Width := FClientScrollBox.Width - 4;
        FCurrentStep.UpdateStatus;
        lValidator.ShowWizardStep(FCurrentStep.Sequence);
        FHeadingLabel.Caption   := FCurrentStep.Heading;
        FDescriptionMemo.Text   := FCurrentStep.Description;
        FErrorMemo.Text         := FCurrentStep.Validator.AllErrorMessages.Text;
        FNextButton.Enabled     := (FCurrentStep.NextLeafStep <> nil);
        FPreviousButton.Enabled := (FCurrentStep.PreviousLeafStep <> nil);
      end;
    end
    else
    begin
      FHeadingLabel.Caption   := '';
      FDescriptionMemo.Text   := '';
      FErrorMemo.Text         := '';
      FNextButton.Enabled     := FALSE;
      FPreviousButton.Enabled := FALSE;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWizardForm.SelectStepInTree;
const OPNAME = 'TWizardForm.SelectStepInTree';
var
  lNode  : TTreeNode;
begin
  try
    FWizardTreeView.Selected := nil;
    if (Assigned(FCurrentStep)) then
    begin
      lNode := FCurrentStep.TreeNode;
      if (Assigned(lNode)) then
      begin
        FSystemChange := TRUE;
        FWizardTreeView.Selected := lNode;
        FSystemChange := FALSE;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWizardForm.WizardTreeViewCustomDrawItem (ASender : TCustomTreeView;
                                                        ANode   : TTreeNode;
                                                        AState  : TCustomDrawState;
                                                        var ADefaultDraw : Boolean);
const OPNAME = 'TWizardForm.WizardTreeViewCustomDrawItem';
begin
  try
  if ADefaultDraw then
  begin
    if (ANode.Selected) then
    begin
      if (not (fsBold in ASender.Canvas.Font.Style)) then
        ASender.Canvas.Font.Style := [fsBold];
    end else begin
      ASender.Canvas.Font.Style := [];
    end;
  end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWizardForm.NextButtonClick(Sender: TObject);
const OPNAME = 'TWizardForm.NextButtonClick';
begin
  try
    FCurrentStep := FCurrentStep.NextLeafStep;
    ShowDialog;
    SelectStepInTree;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWizardForm.PreviousButtonClick(Sender: TObject);
const OPNAME = 'TWizardForm.PreviousButtonClick';
begin
  try
    FCurrentStep := FCurrentStep.PreviousLeafStep;
    ShowDialog;
    SelectStepInTree;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWizardForm.WizardTreeViewChanging (Sender          : TObject;
                                                  Node            : TTreeNode;
                                                  var AllowChange : Boolean);
const OPNAME = 'TWizardForm.WizardTreeViewChanging';
begin
  try
    AllowChange := FSystemChange;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWizardForm.DoDataHasChanged(Sender: TObject);
const OPNAME = 'TWizardForm.DoDataHasChanged';
begin
  try
    FCurrentStep.UpdateStatus;
    FErrorMemo.Text := FCurrentStep.Validator.AllErrorMessages.Text;
    if (Assigned(FWizard.SetFinishButtonState)) then
      FWizard.SetFinishButtonState(FFinishButton);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWizardForm.LanguageHasChanged: boolean;
const OPNAME = 'TWizardForm.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FNextButton.Caption     := FAppModules.Language.GetString('ButtonCaption.Next');
    FPreviousButton.Caption := FAppModules.Language.GetString('ButtonCaption.Previous');
    if (Assigned(FWizard)) then
      FFinishButton.Caption := FWizard.FinishButtonCaption
    else
      FFinishButton.Caption   := FAppModules.Language.GetString('ButtonCaption.Finish');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWizardForm.StudyDataHasChanged (AContext: TChangeContext;
                                          AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TWizardForm.StudyDataHasChanged';
var
  lValidator : TAbstractDataDialogValidator;
begin
  Result := False;
  try
    if (Assigned(FCurrentStep)) then
    begin
      lValidator := FCurrentStep.Validator;
      if (Assigned(lValidator)) then
        Result := FCurrentStep.Validator.StudyDataHasChanged(AContext, AFieldName, AOldValue, ANewValue);
    end;    
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWizardForm.OnFinishButtonClick(Sender: TObject);
const OPNAME = 'TWizardForm.OnFinishButtonClick';
begin
  try
    if (Assigned(FWizard) AND Assigned(FWizard.OnFinishButtonClick)) then
      FWizard.OnFinishButtonClick(Self);
    Close;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TWizardForm.FormClose (Sender     : TObject;
                                 var Action : TCloseAction);
const OPNAME = 'TWizardForm.FormClose';
begin
  try
    ModalResult := mrOk;
    Action      := caFree;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
