unit UFileCollect;

interface

uses
  Windows, Messages, SysUtils, Classes, VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs,
{$WARN UNIT_PLATFORM OFF}
  VCL.FileCtrl,
{$WARN UNIT_PLATFORM ON}
  VCL.ExtCtrls, VCL.StdCtrls, VCL.Buttons,VCL.Grids;

type
  TFileEvent = procedure(Sender: TObject; FileName,FileDir : string) of Object;
  TFileSelectEvent = procedure(Sender: TObject; AIndex: integer) of Object;
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  TFileCollect = class(TCustomPanel)
  protected
    FTopPanel      : TPanel;
    FComboPanel    : TPanel;
    FButtonPanel   : TPanel;
    FDirectories   : TDirectoryListBox;
    FFiles         : TFileListBox;
    FSelectedDirs  : TListBox;
    FSelectedSplit : TSplitter;
    FSelectedFiles : TListBox;
    FDriveCombo    : TDriveComboBox;
    FFilterCombo   : TFilterComboBox;
    FAddBtn        : TSpeedButton;
    FSelectAllBtn  : TSpeedButton;
    FRemoveBtn     : TSpeedButton;
    FFileArea      : TStringGrid;
    FScenarioPath  : string;
    FDirectory     : string;

    FOnFileAdd            : TFileEvent;
    FOnFileAddComplete    : TNotifyEvent;
    FOnFileRemove         : TFileEvent;
    FOnFileRemoveComplete : TNotifyEvent;
    FOnSelectFile         : TFileSelectEvent;

    procedure ResizeFileAreaGrid;
    procedure DirectoryChange(Sender : TObject);
    procedure DriveChange(Sender : TObject);
    procedure FilterChange(Sender : TObject);
    procedure SelectAllBtnClick(Sender : TObject);
    procedure AddBtnClick(Sender : TObject);
    procedure FilesDblClick(Sender : TObject);
    procedure RemoveBtnClick(Sender : TObject);
    procedure RemoveDblClick(Sender : TObject);
    procedure SelectFileClick(Sender : TObject);
    procedure Set_ScenarioPath(AValue:string);
    procedure SelectedFile(var AFileDir,AFileName : string);
    function Get_SelectedFileNameByIndex(AIndex: integer): string;
  public
    constructor Create(AOwner : TComponent); override;

    procedure AddItem(AFileName, AFileDir : string);
    procedure ClearAll;
    property FileAreaGrid : TStringGrid read FFileArea;
    property SelectedFileNameByIndex[AIndex: integer] : string read Get_SelectedFileNameByIndex;
    property ScenarioPath : string read FScenarioPath write Set_ScenarioPath;
  published
    property Align;
    property Anchors;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;

    property Directory: String read FDirectory write FDirectory;

    property OnFileAdd: TFileEvent read FOnFileAdd write FOnFileAdd;
    property OnFileAddComplete: TNotifyEvent read FOnFileAddComplete write FOnFileAddComplete;
    property OnFileRemove: TFileEvent read FOnFileRemove write FOnFileRemove;
    property OnFileRemoveComplete: TNotifyEvent read FOnFileRemoveComplete write FOnFileRemoveComplete;
    property FOnFileSelected: TFileSelectEvent read FOnSelectFile write FOnSelectFile;
  end;

procedure Register;

implementation

uses
  System.UITypes;

constructor TFileCollect.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  Self.BevelOuter := bvNone;
  //Bottom Panel
  FTopPanel := TPanel.Create(Self);
  FTopPanel.Parent := Self;
  FTopPanel.Brush.Color := clBlue;
  FTopPanel.Align := alTop;
  FTopPanel.Height := 47;

  //Directory List Box
  FDirectories := TDirectoryListBox.Create(Self);
  FDirectories.Parent := Self;
  FDirectories.Align := alLeft;
  FDirectories.Width := 161;
  FDirectories.Ctl3D := false;
  FDirectory := FDirectories.Directory;
  FDirectories.OnChange := DirectoryChange;

  //File List Box
  FFiles := TFileListBox.Create(Self);
  FFiles.Parent := Self;
  FFiles.MultiSelect := true;
  FFiles.Align := alLeft;
  FFiles.Width := 180;
  FFiles.Ctl3D := False;
  FFiles.Mask := '*.INC';
  FFiles.OnDblClick := FilesDblClick;

  //Selected Directories List Box
  FSelectedDirs := TListBox.Create(Self);
  FSelectedDirs.Parent := Self;
  FSelectedDirs.Align := alLeft;
  FSelectedDirs.Width := 84;
  FSelectedDirs.OnClick := SelectFileClick;

  //Spiltter
  FSelectedSplit := TSplitter.Create(Self);
  FSelectedSplit.Parent := Self;
  FSelectedSplit.Align := alLeft;
  FSelectedSplit.Left := 400;
  FSelectedSplit.Width := 2;
  
  //Selected Files List Box
  FSelectedFiles := TListBox.Create(Self);
  FSelectedFiles.Parent := Self;
  FSelectedFiles.Align := alClient;
  FSelectedFiles.OnDblClick := RemoveDblClick;
  FSelectedFiles.OnClick     := SelectFileClick;

  //Combo Panel
  FFileArea             := TStringGrid.Create(Self);
  FFileArea.Parent      := Self;
  FFileArea.Align       := alRight;
  FFileArea.ColCount    := 2;
  FFileArea.RowCount    := 2;
  FFileArea.FixedRows   := 1;
  FFileArea.FixedCols   := 1;
  FFileArea.Options     := FFileArea.Options  + [goDrawFocusSelected,goEditing,goAlwaysShowEditor];
  FFileArea.OnClick     := SelectFileClick;
  FFileArea.DefaultRowHeight  := 15;
  FFileArea.DefaultColWidth   := 100;
  FFileArea.Cells[0,0] := 'MAR';
  FFileArea.Cells[1,0] := 'Catchment Area';

  //Combo Panel
  FComboPanel := TPanel.Create(Self);
  FComboPanel.Parent := FTopPanel;
  FComboPanel.Brush.Color := clBlue;
  FComboPanel.BevelOuter := bvNone;
  FComboPanel.Align := alLeft;
  FComboPanel.Width := 161;

  //Drive Combo Box
  FDriveCombo := TDriveComboBox.Create(Self);
  FDriveCombo.Parent := FComboPanel;
  FDriveCombo.Left := 2;
  FDriveCombo.Top := 2;
  FDriveCombo.Width := 161;
  FDriveCombo.OnChange :=  DriveChange;

  //Filter Combo Box;
  FFilterCombo := TFilterComboBox.Create(Self);
  FFilterCombo.Parent := FComboPanel;
  FFilterCombo.Left := 2;
  FFilterCombo.Top := 24;
  FFilterCombo.Width := 161;
  FFilterCombo.Filter := 'INC Files (*.INC)|*.INC';
  FFilterCombo.FileList := FFiles;
  FFilterCombo.OnChange := FilterChange;

  //Button Panel
  FButtonPanel := TPanel.Create(Self);
  FButtonPanel.Parent := FTopPanel;
  FButtonPanel.Brush.Color := clBlue;
  FButtonPanel.BevelOuter := bvNone;
  FButtonPanel.Align := alClient;
  FButtonPanel.Width := 500;

  //Select All Button
  FSelectAllBtn := TSpeedButton.Create(Self);
  FSelectAllBtn.Parent := FButtonPanel;
  FSelectAllBtn.Flat := true;
  FSelectAllBtn.Align := alLeft;
  FSelectAllBtn.Width := 60;
  FSelectAllBtn.Caption := 'Select All';
  FSelectAllBtn.OnClick := SelectAllBtnClick;

  //Add Button
  FAddBtn := TSpeedButton.Create(Self);
  FAddBtn.Parent := FButtonPanel;
  FAddBtn.Flat := true;
  FAddBtn.Align := alLeft;
  FAddBtn.Width := 120;
  FAddBtn.Caption := 'Add >>>>>>>>';
  FAddBtn.OnClick := AddBtnClick;

  //Remove Button
  FRemoveBtn := TSpeedButton.Create(Self);
  FRemoveBtn.Parent := FButtonPanel;
  FRemoveBtn.Flat := true;
  FRemoveBtn.Align := alClient;
  FRemoveBtn.Caption := '<<<<<< Remove';
  FRemoveBtn.OnClick := RemoveBtnClick;
end;

//---- ---- INTERNAL EVENT HANDLERS ---- ----

procedure TFileCollect.DirectoryChange(Sender : TObject);
begin
  inherited;
  FFiles.Directory := FDirectories.Directory;
  FDirectory := FDirectories.Directory;
end;

procedure TFileCollect.DriveChange(Sender : TObject);
begin
  inherited;
  FDirectories.Drive := FDriveCombo.Drive;
end;

procedure TFileCollect.FilterChange(Sender : TObject);
begin
  inherited;
  FFiles.Mask := FFilterCombo.Mask;
end;

//---- ---- EXTERNAL EVENT HANDLERS ---- ----

procedure TFileCollect.SelectAllBtnClick(Sender : TObject);
var
  Loop : integer;
begin
  inherited Click;
  for loop := 0 to (FFiles.Items.Count - 1) do
    FFiles.Selected[Loop] := true;
end;

procedure TFileCollect.AddBtnClick(Sender : TObject);
var
  loop : integer;
begin
  inherited Click;
  for loop := 0 to (FFiles.Items.Count - 1) do
  begin
    if FFiles.Selected[loop] and ((FSelectedFiles.Items.IndexOf(FFiles.Items[loop]) = -1) or
       (FSelectedDirs.Items.IndexOf(FDirectories.Directory) = -1))  then
    begin
      AddItem(FFiles.Items[loop],FDirectories.Directory);
    end;
  end;
  ResizeFileAreaGrid;
  if Assigned(FOnFileAddComplete) then FOnFileAddComplete(Sender);
end;

procedure TFileCollect.FilesDblClick(Sender : TObject);
begin
  inherited DblClick;
  if (FSelectedFiles.Items.IndexOf(FFiles.Items[FFiles.ItemIndex]) = -1) or
     (FSelectedDirs.Items.IndexOf(FDirectories.Directory) = -1) then
  begin
      AddItem(FFiles.Items[FFiles.ItemIndex],FDirectories.Directory);
  end;
  ResizeFileAreaGrid;
  if Assigned(FOnFileAddComplete) then FOnFileAddComplete(Sender);
end;

procedure TFileCollect.RemoveBtnClick(Sender : TObject);
var
  loop : integer;
begin
  inherited Click;
  loop := FSelectedFiles.Items.Count - 1;
  while loop <> -1 do
  begin
    if FSelectedFiles.Selected[loop] then
    begin
      if Assigned(FOnFileRemove) then
        FOnFileRemove(Sender,FSelectedFiles.Items[Loop],FSelectedDirs.Items[Loop]+'\');
      FSelectedFiles.Items.Delete(Loop);
      FSelectedDirs.Items.Delete(Loop);
    end;
    Dec(loop);
  end;
  ResizeFileAreaGrid;
  if Assigned(FOnFileRemoveComplete) then FOnFileRemoveComplete(Sender);
end;

procedure TFileCollect.RemoveDblClick(Sender : TObject);
begin
  inherited DblClick;
  if Assigned(FOnFileRemove) then
    FOnFileRemove(Sender,FSelectedFiles.Items[FSelectedFiles.ItemIndex],FSelectedDirs.Items[FSelectedFiles.ItemIndex]+'\');
    FSelectedDirs.Items.Delete(FSelectedFiles.ItemIndex);
    FSelectedFiles.Items.Delete(FSelectedFiles.ItemIndex);
    ResizeFileAreaGrid;
  if Assigned(FOnFileRemoveComplete) then FOnFileRemoveComplete(Sender);
end;

//---- ---- METHODS ---- ----

procedure TFileCollect.AddItem(AFileName, AFileDir : string);
var
  LNewFileName,
  LNewFileNameStr: string;
begin
  if(FSelectedFiles.Items.IndexOf(AFileName) >= 0) then Exit;

  if(Trim(FScenarioPath) = '') then
  begin
     FScenarioPath := AFileDir;
  end;

  if(FScenarioPath <> AFileDir) then
  begin
    LNewFileNameStr   :=  IncludeTrailingPathDelimiter(AFileDir ) + AFileName;
    if(MessageDlg('The selected file (' + LNewFileNameStr + ') will be copied to the scenario folder before being added. Continue?' ,mtConfirmation,mbOKCancel,0) <> mrOk) then
      Exit;
    LNewFileName   :=  IncludeTrailingPathDelimiter(FScenarioPath ) + AFileName;
    if FileExists(LNewFileName) then
    begin
      if(MessageDlg('File already exist in the destination folder. Do you want to override it?' ,mtConfirmation,mbOKCancel,0) <> mrOk) then
        Exit;
    end;
    if not SysUtils.DirectoryExists(FScenarioPath) then
      SysUtils.ForceDirectories(FScenarioPath);
    CopyFile(PChar(LNewFileNameStr),PChar(LNewFileName),False);
  end;

  FSelectedFiles.Items.Add(AFileName);
  FSelectedDirs.Items.Add(FScenarioPath);
  ResizeFileAreaGrid;
  if Assigned(FOnFileAdd) then
    FOnFileAdd(Self,AFileName,FScenarioPath);
end;

procedure TFileCollect.ClearAll;
begin
  FSelectedDirs.Clear;
  FSelectedFiles.Clear;
  ResizeFileAreaGrid;
end;

procedure TFileCollect.ResizeFileAreaGrid;
var
  LIndex : integer;
begin
  if(FSelectedFiles.Items.Count = 0) then
    FFileArea.RowCount := 2
  else
    FFileArea.RowCount := FSelectedFiles.Items.Count+1;
  for LIndex := 1 to FFileArea.RowCount-1 do
     FFileArea.Rows[Lindex].Clear;
end;

procedure TFileCollect.SelectFileClick(Sender: TObject);
var
  LIndex : integer;
begin
  if Assigned(FOnSelectFile) then
  begin
    LIndex := -1;
    if(Sender = FSelectedFiles) then
      LIndex := FSelectedFiles.ItemIndex;
    if(Sender = FSelectedDirs) then
      LIndex := FSelectedDirs.ItemIndex;
    if(Sender = FFileArea) then
      LIndex := FFileArea.Row-1;
    if(LIndex >= 0) then
      FOnSelectFile(Self,LIndex);
  end;
end;

procedure TFileCollect.SelectedFile(var AFileDir,AFileName : string);
var
  LIndex : integer;
begin
  AFileName := '';
  AFileDir  := '';
  LIndex := -1;
  if(FSelectedFiles.ItemIndex >= 0) then
    LIndex := FSelectedFiles.ItemIndex;
  if(LIndex = -1) and (FSelectedDirs.ItemIndex >= 0) then
    LIndex := FSelectedDirs.ItemIndex;
  if(LIndex = -1) and (FFileArea.Row > 0)  then
    LIndex := FFileArea.Row-1;

  if(LIndex >= 0) then
  begin
    if(FSelectedFiles.Items.Count > 0) and (LIndex < FSelectedFiles.Items.Count) then
      AFileName := FSelectedFiles.Items[LIndex];
    if(FSelectedDirs.Items.Count > 0) and (LIndex < FSelectedDirs.Items.Count) then
      AFileDir  := FSelectedDirs.Items[LIndex];
  end;
end;

function TFileCollect.Get_SelectedFileNameByIndex(AIndex: integer): string;
var
  LPath,
  LFileName : string;
begin
  Result := '';
  if(FSelectedFiles.Items.Count > 0) and (AIndex < FSelectedFiles.Items.Count) then
  begin
    if(FSelectedDirs.Items.Count > 0) and (AIndex < FSelectedDirs.Items.Count) then
    begin
      LFileName := FSelectedFiles.Items[AIndex];
      LPath  := FSelectedDirs.Items[AIndex];
      Result := IncludeTrailingPathDelimiter(LPath) + ExtractFileName(LFileName);
    end;
  end;
end;

procedure TFileCollect.Set_ScenarioPath(AValue: string);
begin
  if SysUtils.DirectoryExists(AValue) then
  begin
    FDirectories.Directory  := AValue;
    FDriveCombo.Drive       := FDirectories.Drive;
    FDirectories.Directory  := AValue;
    FScenarioPath           := AValue;
  end;
end;

procedure Register;
begin
  RegisterComponents('Stomsa', [TFileCollect]);
end;

end.
