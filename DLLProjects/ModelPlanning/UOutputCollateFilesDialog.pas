//
//
//  UNIT      : Contains the class TOutpuCollateFilesDialog.
//  AUTHOR    : Dziedzi Ramulondi (ARIVIA)
//  DATE      : 2005/05/03
//  COPYRIGHT : Copyright © 2005 DWAF
//
//

unit UOutputCollateFilesDialog;

interface

uses
  Classes,
  Windows,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.ExtCtrls,
  VCL.StdCtrls,
  VCL.Buttons,
  VCL.CheckLst,
  VCL.Forms,
  UAbstractObject,
  UAbstractComponent,
  UDataEditComponent,
  UDataComponent;

type

  TOutpuCollateFilesDialog = class(TAbstractScrollablePanel)
  protected
    FSelectorPanel     : TPanel;
    FBtnAddFiles       : TFieldBitBtn;
    FBtnDeleteFiles    : TFieldBitBtn;
    FFileListCheckList : TFieldCheckListBox;

    FCollatingLabel    : TLabel;
    FDataGrid          : TFieldStringGrid;

    FCollateFilesPanel : TPanel;
    FCollateProgressBar: TProgressBar;
    FBtnCollateFiles   : TFieldBitBtn;

    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure DoExport(AFileName: string = ''); override;
    procedure DoPrint; override;
    function CanExport : boolean; override;
    function CanPrint  : boolean; override;
    function StudyHasChanged: boolean; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;

    property grdDataGrid        : TFieldStringGrid   read FDataGrid;
    property BtnAddFiles        : TFieldBitBtn       read FBtnAddFiles;
    property BtnDeleteFiles     : TFieldBitBtn       read FBtnDeleteFiles;
    property FileListCheckList  : TFieldCheckListBox read FFileListCheckList;
    property CollateProgressBar : TProgressBar       read FCollateProgressBar;
    property BtnCollateFiles    : TFieldBitBtn       read FBtnCollateFiles;
    property CollatingLabel     : TLabel             read FCollatingLabel;
  end;

  implementation

uses
  SysUtils,
  VCL.Graphics,
  VCL.Grids,
  UHelpContexts,
  UControlCreationUtilities,
  UErrorHandlingOperations;

  { TOutpuCollateFilesDialog }

procedure TOutpuCollateFilesDialog.CreateMemberObjects;
const OPNAME = 'TOutpuCollateFilesDialog.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FSelectorPanel              := TPanel.Create(ControlsOwner);
    FSelectorPanel.Parent       := ControlsParent;
    FSelectorPanel.Align        := alTop;

    FBtnAddFiles := TFieldBitBtn.Create(ControlsOwner,FAppModules);
    with FBtnAddFiles do
    begin
      Parent    := FSelectorPanel;
      Left      := 10;
      Top       := 5;
      Width     := 100;
      Height    := 35;
      TabStop   := TRUE;
      TabOrder  := 0;
      Glyph.LoadFromResourceName(HImagesInstance,'CREATEIFRSITE');
      NumGlyphs := FBtnAddFiles.Glyph.Width div FBtnAddFiles.Glyph.Height;
      Spacing   := 6;
    end;

    FBtnDeleteFiles := TFieldBitBtn.Create(ControlsOwner,FAppModules);
    with FBtnDeleteFiles do
    begin
      Parent    := FSelectorPanel;
      Left      := 120;
      Top       := 5;
      Width     := 100;
      Height    := 35;
      TabStop   := TRUE;
      TabOrder  := 1;
      Glyph.LoadFromResourceName(HImagesInstance,'DELETEIFRSITE');
      NumGlyphs := FBtnDeleteFiles.Glyph.Width div FBtnDeleteFiles.Glyph.Height;
      Spacing   := 6;
    end;

    FFileListCheckList := TFieldCheckListBox.Create(ControlsOwner,FAppModules);
    with FFileListCheckList do
    begin
      Parent  := FSelectorPanel;
      Left    := FBtnDeleteFiles.Left + FBtnDeleteFiles.Width + 50;
      Top     := FBtnDeleteFiles.Top;
      Height  := 35;
      Width   := 350;
      Columns := 4;
    end;

    FDataGrid                     := TFieldStringGrid.Create(ControlsOwner, FAppModules);
    FDataGrid.Parent              := ControlsParent;
    FDataGrid.ColCount            := 2;
    FDataGrid.RowCount            := 6;
    FDataGrid.FixedCols           := 1;
    FDataGrid.FixedRows           := 1;
    FDataGrid.Alignment           := taLeftJustify;
    FDataGrid.Options             := [goColSizing, goRowSizing, goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine];
    FDataGrid.DblClickColAutoSize := True;
    FDataGrid.AutoSizeFixedCols   := True;

    FCollateFilesPanel := TPanel.Create(ControlsOwner);
    with FCollateFilesPanel do
    begin
      Parent := ControlsParent;
      Align  := alBottom;
      Height := 50;
    end;

    FBtnCollateFiles := TFieldBitBtn.Create(ControlsOwner,FAppModules);
    with FBtnCollateFiles do
    begin
      Parent    := FCollateFilesPanel;
      Left      := FCollateFilesPanel.Width - 90;
      Top       := 20;
      Width     := 80;
      Height    := 25;
      TabStop   := TRUE;
      TabOrder  := 2;
      Glyph.LoadFromResourceName(HImagesInstance,'BATCHWRITE');
      NumGlyphs := FBtnCollateFiles.Glyph.Width div FBtnCollateFiles.Glyph.Height;
      Spacing   := 6;
    end;
    
    FCollateProgressBar := TProgressBar.Create(ControlsOwner);
    with FCollateProgressBar do
    begin
      Parent := FCollateFilesPanel;
      Left   := 10;
      Width  := FCollateFilesPanel.Width - FBtnCollateFiles.Width - 30;
      Top    := 20;
      Height := 25;
    end;

    FCollatingLabel := TLabel.Create(ControlsOwner);
    with FCollatingLabel do
    begin
      Parent := FCollateFilesPanel;
      Left   := 350;
      Top    := 5;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutpuCollateFilesDialog.Initialise: boolean;
const OPNAME = 'TOutpuCollateFilesDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    FDataGrid.Initialise;
    FSelectorPanel.BorderStyle := bsSingle;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutpuCollateFilesDialog.StudyHasChanged: boolean;
const OPNAME = 'TOutpuCollateFilesDialog.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    FDataGrid.StudyHasChanged;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutpuCollateFilesDialog.LanguageHasChanged: boolean;
const OPNAME = 'TOutpuCollateFilesDialog.LanguageHasChanged';
var
  LIndex : integer;
begin
  Result := inherited LanguageHasChanged;
  try
    FDataGrid.LanguageHasChanged;
    FBtnAddFiles.Caption     := FAppModules.Language.GetString('ButtonCaption.Add');
    FBtnDeleteFiles.Caption  := FAppModules.Language.GetString('ButtonCaption.Delete');
    FBtnCollateFiles.Caption := FAppModules.Language.GetString('ButtonCaption.Run');

    FileListCheckList.Items.Clear;
    FileListCheckList.Items.Add(FAppModules.Language.GetString('TWRPMOutputFilesCollator.PltOut'));
    //FileListCheckList.Items.Add(FAppModules.Language.GetString('TWRPMOutputFilesCollator.SumOut'));
    FileListCheckList.Items.Add(FAppModules.Language.GetString('TWRPMOutputFilesCollator.SysOut'));
    FileListCheckList.Items.Add(FAppModules.Language.GetString('TWRPMOutputFilesCollator.ResOut'));
    FileListCheckList.Items.Add(FAppModules.Language.GetString('TWRPMOutputFilesCollator.PmpOut'));

    for LIndex := 0 to FileListCheckList.Items.Count - 1 do
      FileListCheckList.Checked[LIndex] := True;

    grdDataGrid.Cells[0,0] := FAppModules.Language.GetString('TOutpuCollateFilesValidator.FileTypes');
    grdDataGrid.Cells[0,1] := FAppModules.Language.GetString('TWRPMOutputFilesCollator.PltOut');
    grdDataGrid.Cells[0,2] := FAppModules.Language.GetString('TWRPMOutputFilesCollator.SysOut');
    grdDataGrid.Cells[0,3] := FAppModules.Language.GetString('TWRPMOutputFilesCollator.ResOut');
    grdDataGrid.Cells[0,4] := FAppModules.Language.GetString('TWRPMOutputFilesCollator.PmpOut');
    grdDataGrid.Cells[0,5] := FAppModules.Language.GetString('TField.Sequence');

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutpuCollateFilesDialog.AssignHelpContext;
const OPNAME = 'TOutpuCollateFilesDialog.AssignHelpContext';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOutpuCollateFilesDialog.Resize;
const OPNAME = 'TOutpuCollateFilesDialog.Resize';
begin
  try
    LockWindowUpdate(Self.Handle);
    try
      FSelectorPanel.Align        := alTop;
      FSelectorPanel.ClientHeight := 50;

      FDataGrid.Align             := alClient;

      FCollateProgressBar.Left   := 10;
      FCollateProgressBar.Width  := FCollateFilesPanel.Width - FBtnCollateFiles.Width - 50;

      FBtnCollateFiles.Left      := FCollateFilesPanel.Width - 90;
    finally
      LockWindowUpdate(0);
    end;
    inherited;
  except on E: Exception do HandleError(E, OPNAME); end;

end;

procedure TOutpuCollateFilesDialog.DoExport(AFileName: string = '');
const OPNAME = 'TOutpuCollateFilesDialog.DoExport';
begin
  try
    if FDataGrid.Visible then FDataGrid.DoExport(AFileName);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOutpuCollateFilesDialog.DoPrint;
const OPNAME = 'TOutpuCollateFilesDialog.DoPrint';
begin
  try
    if FDataGrid.Visible then FDataGrid.DoPrint('');
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOutpuCollateFilesDialog.CanExport: boolean;
const OPNAME = 'TOutpuCollateFilesDialog.CanExport';
begin
  Result := False;
  try
    Result := (Assigned(FDataGrid)            and (FDataGrid.Visible));
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOutpuCollateFilesDialog.CanPrint: boolean;
const OPNAME = 'TOutpuCollateFilesDialog.CanPrint';
begin
  Result := False;
  try
    Result := (Assigned(FDataGrid)            and (FDataGrid.Visible));
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.

