//
//  UNIT      : Contains UCautionarySplashScreenDialog Class
//  AUTHOR    : Oagilwe Segola
//  DATE      : 2008/10/28
//  COPYRIGHT : Copyright © 2004 DWAF
//

unit UCautionarySplashScreenDialog;

interface
uses
  Vcl.ExtCtrls,
  Classes,
  Vcl.Grids,
  Vcl.StdCtrls,

  UAbstractObject,
  UAbstractComponent,
  UDataEditComponent,
  UDataComponent;

type
  TCautionarySplashScreenDialog = class(TAbstractScrollablePanel)
  protected
    FMainPanel,
    FTopPanel,
    FStringGridPanel,
    FCheckBoxPanel,
    FCautionaryPanel,
    FUserSupportPanel,
    FMetaDataPanel     : TPanel;

    FIssueStringGrid   : TStringGrid;
    FCautionaryLabel,
    FUserSupportLabel,
    FMetaDataLabel     : TLabel;
    FShowScreenChkBox  : TCheckBox;

    procedure CreateMemberObjects; override;
  public
    procedure Resize; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;

    property IssueStringGrid  : TStringGrid read FIssueStringGrid;
    property ShowScreenChkBox : TCheckBox   read FShowScreenChkBox;
    property CheckBoxPanel    : TPanel      read FCheckBoxPanel;
  end;

implementation
uses
  Vcl.Controls,
  Vcl.Graphics,
  SysUtils,

  UErrorHandlingOperations;

{ TCautionarySplashScreenDialog }

procedure TCautionarySplashScreenDialog.CreateMemberObjects;
const OPNAME = 'TCautionarySplashScreenDialog.CreateMemberObjects';
begin
  inherited;
  try
    FMainPanel := TPanel.Create(ControlsOwner);
    with FMainPanel do
    begin
      Parent := ControlsParent;
      Align  := alClient;
    end;

    FTopPanel := TPanel.Create(FMainPanel);
    with FTopPanel do
    begin
      Parent := FMainPanel;
      Align  := alTop;
      Height := 150;
    end;

    FCautionaryPanel := TPanel.Create(FTopPanel);
    with FCautionaryPanel do
    begin
      Parent     := FTopPanel;
      Align      := alTop;
      BevelOuter := bvNone;
      Height     := 40;
    end;
    
    FCautionaryLabel := TLabel.Create(FCautionaryPanel);
    with FCautionaryLabel do
    begin
      Parent     := FCautionaryPanel;
      Align      := alTop;
      Width      := ControlsParent.Width - 20;
      Alignment  := taCenter;
      AutoSize   := False;
      WordWrap   := True;
      AutoSize   := True;
      Font.Style := [fsBold];
      Font.Size  := 18;
      Font.Color := clBlue;
    end;

    FUserSupportPanel := TPanel.Create(FTopPanel);
    with FUserSupportPanel do
    begin
      Parent     := FTopPanel;
      Align      := alClient;
      BevelOuter := bvNone;
      Height     := 55;
    end;

    FUserSupportLabel := TLabel.Create(FUserSupportPanel);
    with FUserSupportLabel do
    begin
      Parent     := FUserSupportPanel;
      Align      := alTop;
      Left       := 20;
      Width      := ControlsParent.Width - 20;
      Alignment  := taCenter;
      AutoSize   := False;
      WordWrap   := True;
      AutoSize   := True;
      Font.Size  := 12;
      Font.Color := clBlue;
    end;

    FMetaDataPanel := TPanel.Create(FTopPanel);
    with FMetaDataPanel do
    begin
      Parent     := FTopPanel;
      Align      := alBottom;
      BevelOuter := bvNone;
      Height     := 55;
    end;

    FMetaDataLabel := TLabel.Create(FMetaDataPanel);
    with FMetaDataLabel do
    begin
      Parent     := FMetaDataPanel;
      Align      := alTop;
      Left       := 20;
      Width      := ControlsParent.Width - 20;
      Alignment  := taCenter;
      AutoSize   := False;
      WordWrap   := True;
      AutoSize   := True;
      Font.Size  := 10;
      Font.Color := clBlue;
    end;

    FStringGridPanel := TPanel.Create(FMainPanel);
    with FStringGridPanel do
    begin
      Parent := FMainPanel;
      Align  := alClient;
    end;

    FIssueStringGrid := TStringGrid.Create(FStringGridPanel);
    with FIssueStringGrid do
    begin
      Parent       := FStringGridPanel;
      Align        := alClient;
      Options      := Options + [goColSizing,goRowSizing];
      ScrollBars   := ssBoth;
      ColCount     := 5;
      FixedCols    := 0;
      RowCount     := 1;
      ColWidths[0] := 150;
      ColWidths[1] := 150;
      ColWidths[2] := 250;
      ColWidths[3] := 270;
      ColWidths[4] := 250;
    end;

    FCheckBoxPanel := TPanel.Create(FMainPanel);
    with FCheckBoxPanel do
    begin
      Parent := FMainPanel;
      Align  := alBottom;
    end;

    FShowScreenChkBox := TCheckBox.Create(FCheckBoxPanel);
    with FShowScreenChkBox do
    begin
      Parent  := FCheckBoxPanel;
      Top     := 10;
      Width   := 170;
      Left    := (FCheckBoxPanel.Width * 5) - FShowScreenChkBox.Width - 20;
    end;
    
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TCautionarySplashScreenDialog.Initialise: boolean;
const OPNAME = 'TCautionarySplashScreenDialog.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TCautionarySplashScreenDialog.LanguageHasChanged: boolean;
const OPNAME = 'TCautionarySplashScreenDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FIssueStringGrid.Cells[0,0] := FAppModules.Language.GetString('LabelCaption.StudyName');
    FIssueStringGrid.Cells[1,0] := FAppModules.Language.GetString('LabelCaption.ImportedBy');
    FIssueStringGrid.Cells[2,0] := FAppModules.Language.GetString('LabelCaption.ErrorType');
    FIssueStringGrid.Cells[3,0] := FAppModules.Language.GetString('LabelCaption.ErrorDescription');
    FIssueStringGrid.Cells[4,0] := FAppModules.Language.GetString('LabelCaption.CorrectiveAction');
    FShowScreenChkBox.Caption   := FAppModules.Language.GetString('ChkBoxCaption.ShowScreenAgain');
    FMetaDataLabel.Caption      := FAppModules.Language.GetString('LabelCaption.MetadataInformation');
    FCautionaryLabel.Caption    := FAppModules.Language.GetString('LabelCaption.CautionaryNote');
    FUserSupportLabel.Caption   := FAppModules.Language.GetString('LabelCaption.UserResponsibility');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCautionarySplashScreenDialog.Resize;
const OPNAME = 'TCautionarySplashScreenDialog.Resize';
begin
  inherited Resize;
  try
    FShowScreenChkBox.Left := Self.Width - FShowScreenChkBox.Width - 10;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
