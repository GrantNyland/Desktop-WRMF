{******************************************************************************}
{*  UNIT      : Contains the class TChangeListDialog.                         *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2005/02/11                                                    *}
{*  COPYRIGHT : Copyright © 2005 DWAF                                         *}
{******************************************************************************}


unit UChangeListDialog;

interface

uses
  Classes,
  Vcl.Controls,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.Buttons,
  Vcl.Graphics,
  UAbstractObject,
  UAbstractComponent,
  UDataEditComponent,
  Vcl.Grids,
  UDataComponent;

type

  TChangeListDialog = class(TAbstractScrollablePanel)
  protected
    FImageList           : TImageList;
    FPnlLeft             : TAbstractPanel;
    FPnlRight            : TAbstractPanel;
    FVerticalSplitter    : TSplitter;

    FPnlGroup            : TAbstractPanel;
    FLblGroupHeading     : TLabel;
    FTrvChangeGroup      : TAbstractTreeView;
    FLeftSplitter        : TSplitter;
    FTrvChangeList       : TAbstractTreeView;
    FPnlList             : TAbstractPanel;
    FLblListHeading      : TLabel;

    FPnlTop              : TAbstractPanel;
    FPnlBottom           : TAbstractPanel;
    FLblName             : TLabel;
    FLblPerson           : TLabel;
    FLblDescription      : TLabel;
    FLblDateCreated      : TLabel;
    FLblDateCreatedValue : TLabel;
    FEdtName             : TFieldEdit;
    FEdtPerson           : TFieldEdit;
    FMmoDescription      : TFieldRichEdit;
    FRightSplitter       : TSplitter;
    FGrdParamChanges     : TFieldStringGrid;
    FLblParamChanges     : TLabel;
    FLblGroupName        : TLabel;
    FEdtGroupName        : TFieldEdit;
    FOnHintChange        : TDoHintChangeFunction;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure AssignHelpContext; override;
    procedure SplitterMoved (Sender: TObject);
  public
    procedure Resize; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    procedure ShowControls (AState : integer);
    property TrvChangeGroup      : TAbstractTreeView read FTrvChangeGroup;
    property TrvChangeList       : TAbstractTreeView read FTrvChangeList;
    property GrdParamChanges     : TFieldStringGrid  read FGrdParamChanges;
    property LblDateCreatedValue : TLabel            read FLblDateCreatedValue;
    property EdtName             : TFieldEdit        read FEdtName;
    property EdtGroupName        : TFieldEdit        read FEdtGroupName;
    property EdtPerson           : TFieldEdit        read FEdtPerson;
    property MmoDescription      : TFieldRichEdit    read FMmoDescription;

  end;

  implementation

uses
  SysUtils,
  Math,
  Vcl.ImgList,
  UHelpContexts,
  UControlCreationUtilities,
  UErrorHandlingOperations;

{******************************************************************************}
{* TChangeListDialog                                                          *}
{******************************************************************************}

procedure TChangeListDialog.CreateMemberObjects;
const OPNAME = 'TChangeListDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
  lParent : TWinControl;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;

    FImageList := TImageList.Create(lOwner);
    FImageList.Height := 16;
    FImageList.Width  := 16;
    { DO NOT change the order in which the images below are loaded. }
    { The function TChangeListValidator.TreeNodeType relies on this oreder. }
    FImageList.GetInstRes(HImagesInstance, rtBitmap, 'CLCHANGELIST',   16, [], clWhite);
    FImageList.GetInstRes(HImagesInstance, rtBitmap, 'CLDEACTIVATE',   16, [], clWhite);
    FImageList.GetInstRes(HImagesInstance, rtBitmap, 'CLACTIVATE',     16, [], clWhite);
    FImageList.GetInstRes(HImagesInstance, rtBitmap, 'CGDEACTIVATE',   16, [], clWhite);
    FImageList.GetInstRes(HImagesInstance, rtBitmap, 'CGACTIVATE',     16, [], clWhite);
    FImageList.GetInstRes(HImagesInstance, rtBitmap, 'CLALIEN',        16, [], clWhite);

    FPnlLeft := TAbstractPanel.Create(lOwner, FAppModules);
    with FPnlLeft do
    begin
      Parent     := lParent;
      Left       := 0;
      Top        := 0;
      Width      := 200;
      Align      := alLeft;
      BevelOuter := bvNone;
      TabOrder   := 1;
    end;
    FVerticalSplitter := TSplitter.Create(lOwner);
    with FVerticalSplitter do
    begin
      Parent  := lParent;
      Beveled := TRUE;
      Left    := 201;
      Top     := 0;
    end;
    FPnlRight := TAbstractPanel.Create(lOwner, FAppModules);
    with FPnlRight do
    begin
      Parent     := lParent;
      Left       := 204;
      Top        := 0;
      Width      := 512;
      Height     := 423;
      BevelOuter := bvNone;
      Align      := alClient;
      TabOrder   := 1;
    end;

    FPnlGroup := TAbstractPanel.Create(lOwner, FAppModules);
    with FPnlGroup do
    begin
      Parent     := FPnlLeft;
      Height     := 300;
      BevelOuter := bvNone;
      Align      := alTop;
    end;
    FLblGroupHeading := TLabel.Create(lOwner);
    with FLblGroupHeading do
    begin
      Parent     := FPnlGroup;
      AutoSize   := FALSE;
      Height     := 21;
      Align      := alTop;
      Alignment  := taCenter;
      Layout     := tlCenter;
      Font.Style := [fsBold];
      Caption    := 'Change Groups';
    end;
    FTrvChangeGroup := TAbstractTreeView.Create(lOwner, FAppModules);
    with FTrvChangeGroup do
    begin
      Parent        := FPnlGroup;
      Align         := alClient;
      HideSelection := FALSE;
      Images        := FImageList;
      ReadOnly      := TRUE;
      ShowRoot      := TRUE;
      TabOrder      := 0;
      DragMode      := dmAutomatic;
    end;
    FLeftSplitter := TSplitter.Create(lOwner);
    with FLeftSplitter do
    begin
      Parent  := FPnlLeft;
      Beveled := TRUE;
    end;
    FPnlList := TAbstractPanel.Create(lOwner, FAppModules);
    with FPnlList do
    begin
      Parent     := FPnlLeft;
      BevelOuter := bvNone;
      Align      := alClient;
    end;
    FLblListHeading := TLabel.Create(lOwner);
    with FLblListHeading do
    begin
      Parent     := FPnlList;
      AutoSize   := FALSE;
      Height     := 21;
      Align      := alTop;
      Alignment  := taCenter;
      Layout     := tlCenter;
      Font.Style := [fsBold];
      Caption    := 'Change Lists';
    end;
    FTrvChangeList := TAbstractTreeView.Create(lOwner, FAppModules);
    with FTrvChangeList do
    begin
      Parent        := FPnlList;
      Align         := alClient;
      HideSelection := FALSE;
      Images        := FImageList;
      ReadOnly      := TRUE;
      ShowLines     := FALSE;
      TabOrder      := 1;
      DragMode      := dmAutomatic;
    end;

    FPnlTop := TAbstractPanel.Create(lOwner, FAppModules);
    with FPnlTop do
    begin
      Parent     := FPnlRight;
      BevelOuter := bvNone;
      Height     := 140;
      TabOrder   := 1;
    end;
    FRightSplitter := TSplitter.Create(lOwner);
    with FRightSplitter do
    begin
      Parent  := FPnlRight;
      Beveled := TRUE;
      OnMoved := SplitterMoved;
    end;
    FPnlBottom := TAbstractPanel.Create(lOwner, FAppModules);
    with FPnlBottom do
    begin
      Parent     := FPnlRight;
      BevelOuter := bvNone;
      TabOrder   := 2;
    end;
    FLblName := TLabel.Create(lOwner);
    with FLblName do
    begin
      Parent   := FPnlTop;
      Left     := 10;
      Top      := 14;
      Width    := 34;
    end;
    FLblPerson := TLabel.Create(lOwner);
    with FLblPerson do
    begin
      Parent   := FPnlTop;
      Left     := 10;
      Top      := 44;
      Width    := 57;
    end;
    FlblDescription := TLabel.Create(lOwner);
    with FlblDescription do
    begin
      Parent   := FPnlTop;
      Left     := 10;
      Top      := 74;
      Width    := 59;
    end;
    FlblDateCreated := TLabel.Create(lOwner);
    with FlblDateCreated do
    begin
      Parent   := FPnlTop;
      Left     := 266;
      Top      := 14;
      Width    := 68;
    end;
    FlblDateCreatedValue := TLabel.Create(lOwner);
    with FlblDateCreatedValue do
    begin
      Parent   := FPnlTop;
      Left     := 344;
      Top      := 14;
      Width    := 58;
    end;
    FEdtName := TFieldEdit.Create(lOwner, FAppModules);
    with FEdtName do
    begin
      Parent   := FPnlTop;
      Left     := 80;
      Top      := 10;
      Width    := 140;
      TabOrder := 0;
    end;
    FEdtPerson := TFieldEdit.Create(lOwner, FAppModules);
    with FEdtPerson do
    begin
      Parent   := FPnlTop;
      Left     := 80;
      Top      := 40;
      Width    := 140;
      TabOrder := 1;
    end;
    FMmoDescription := TFieldRichEdit.Create(lOwner, FAppModules);
    with FMmoDescription do
    begin
      Parent     := FPnlTop;
      Left       := 80;
      Top        := 70;
      ScrollBars := ssVertical;
      TabOrder   := 2;
    end;

    FLblGroupName := TLabel.Create(lOwner);
    with FLblGroupName do
    begin
      Parent   := FPnlTop;
      Left     := 10;
      Top      := 14;
      Width    := 34;
    end;
    FEdtGroupName := TFieldEdit.Create(lOwner, FAppModules);
    with FEdtGroupName do
    begin
      Parent   := FPnlTop;
      Left     := 80;
      Top      := 10;
      Width    := 140;
      TabOrder := 0;
    end;

    FLblParamChanges := TLabel.Create(lOwner);
    with FLblParamChanges do
    begin
      Parent   := FPnlBottom;
      AutoSize := FALSE;
      Height   := 20;
      Align    := alTop;
      Layout   := tlCenter;
    end;
    FGrdParamChanges := TFieldStringGrid.Create(lOwner, FAppModules);
    with FGrdParamChanges do
    begin
      Parent           := FPnlBottom;
      Left             := 1;
      ColCount         := 6;
      RowCount         := 1;
      DefaultColWidth  := 60;
      DefaultRowHeight := 20;
      TabOrder         := 3;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeListDialog.DestroyMemberObjects;
const OPNAME = 'TChangeListDialog.DestroyMemberObjects';
begin
  try
    inherited;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChangeListDialog.Initialise: boolean;
const OPNAME = 'TChangeListDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := True;
    FGrdParamChanges.ColWidths[0] := 30;
    FGrdParamChanges.ColWidths[1] := 200;
    FGrdParamChanges.ColWidths[2] := 350;
    FGrdParamChanges.ColWidths[3] := 60;
    FGrdParamChanges.ColWidths[4] := 60;
    FGrdParamChanges.ColWidths[5] := 100;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeListDialog.SplitterMoved (Sender: TObject);
const OPNAME = 'TChangeListDialog.SplitterMoved';
begin
  try
    Resize;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeListDialog.Resize;
const OPNAME = 'TChangeListDialog.Resize';
begin
  inherited Resize;
  try
    FPnlGroup.Top       := 0;
    FPnlGroup.Align     := alTop;
    FLeftSplitter.Top   := FPnlGroup.Height + 1;
    FLeftSplitter.Align := alTop;
    FPnlList.Top        := FLeftSplitter.Top + FLeftSplitter.Height + 1;
    FPnlList.Align      := alClient;

    FPnlTop.Top            := 0;
    FPnlTop.Align          := alTop;
    FMmoDescription.Width  := Min(420, FPnlTop.Width - 80 - 10);
    FMmoDescription.Height := Min( 60, FPnlTop.Height - 70 - 10);

    FRightSplitter.Top     := FPnlTop.Height + 1;
    FRightSplitter.Align   := alTop;
    FPnlBottom.Top         := FRightSplitter.Top + FRightSplitter.Height + 1;
    FPnlBottom.Align       := alClient;
    FGrdParamChanges.Top   := FLblParamChanges.Height + 1;
    FGrdParamChanges.Align := alClient;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChangeListDialog.LanguageHasChanged: boolean;
const OPNAME = 'TChangeListDialog.LanguageHasChanged';
begin
  Result := True;
  try
    FLblName.Caption         := FAppModules.Language.GetString('ChangeLists.ChangeListName') + ' :';
    FLblGroupName.Caption    := FAppModules.Language.GetString('ChangeLists.ChangeGroupName') + ' :';
    FLblPerson.Caption       := FAppModules.Language.GetString('ChangeLists.CreatedBy') + ' :';
    FlblDescription.Caption  := FAppModules.Language.GetString('ChangeLists.Description') + ' :';
    FlblDateCreated.Caption  := FAppModules.Language.GetString('ChangeLists.DateCreated') + ' :';
    FLblParamChanges.Caption := '    ' +
                                FAppModules.Language.GetString('ChangeLists.ParameterChanges') + ' :';

    FGrdParamChanges.Cells[0, 0] :=  FAppModules.Language.GetString('ChangeLists.Nr');
    FGrdParamChanges.Cells[1, 0] :=  FAppModules.Language.GetString('ChangeLists.ParameterName');
    FGrdParamChanges.Cells[2, 0] :=  FAppModules.Language.GetString('ChangeLists.EntityDescr');
    FGrdParamChanges.Cells[3, 0] :=  FAppModules.Language.GetString('ChangeLists.AbsOrPerc');
    FGrdParamChanges.Cells[4, 0] :=  FAppModules.Language.GetString('ChangeLists.Change');
    FGrdParamChanges.Cells[5, 0] :=  FAppModules.Language.GetString('ChangeLists.Description');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeListDialog.AssignHelpContext;
const OPNAME = 'TChangeListDialog.AssignHelpContext';
begin
  try
//    SetControlHelpContext(FNodeNameEdit,          HC_NodesNameForNodesWithInflow);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChangeListDialog.ShowControls (AState : integer);
const OPNAME = 'TChangeListDialog.ShowControls';
begin
  try
    FLblName.Visible             := AState = 0;
    FLblPerson.Visible           := AState = 0;
    FLblDescription.Visible      := AState = 0;
    FLblDateCreated.Visible      := AState = 0;
    FLblDateCreatedValue.Visible := AState = 0;
    FEdtName.Visible             := AState = 0;
    FEdtPerson.Visible           := AState = 0;
    FMmoDescription.Visible      := AState = 0;
    FGrdParamChanges.Visible     := AState = 0;
    FLblParamChanges.Visible     := AState = 0;
    FLblGroupName.Visible        := AState in [3, 4];
    FEdtGroupName.Visible        := AState in [3, 4];

  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
