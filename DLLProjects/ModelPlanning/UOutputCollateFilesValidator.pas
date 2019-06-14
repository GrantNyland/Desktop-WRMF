unit UOutputCollateFilesValidator;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  VCL.Dialogs,
  Types,
  VCL.Grids,
  VCL.Graphics,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  VoaimsCom_TLB,
  UDataEditComponent,
  UYieldContextValidationType,
  UOutputCollateFilesDialog;

type
  TOutpuCollateFilesValidator = class(TAbstractOutputDialogValidator)
  protected
    FPltFilesList : TStringList;
    FSysFilesList : TStringList;
    FResFilesList : TStringList;
    FPmpFilesList : TStringList;
    FSequences    : TStringList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    procedure OnBtnAddFilesClick(Sender: TObject);
    procedure OnBtnDeleteFilesClick(Sender: TObject);
    procedure OnBtnCollateFilesClick(Sender: TObject);
    procedure OnGridColEnter(Sender: TObject;ACol, ARow: integer);

    procedure RePopulateDataViewer;
    procedure SetButtonState;

    function CheckIfOutputFilesOverite: boolean;
    procedure ShowProgress(AProgress: string; AProgressType: TProgressType; var AStop: boolean;  AUpdateSteps: boolean=False);
    function OutpuCollateFilesDialog : TOutpuCollateFilesDialog;
  public
    function Initialise: boolean; override;
    function LanguageHasChanged: boolean; override;
    function CanExport : boolean; override;
    function CanPrint  : boolean; override;
    procedure DoExport(AFileName: string = ''); override;
    procedure DoPrint; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
  end;

implementation

uses
  System.UITypes,
  Contnrs,
  Windows,
  SysUtils,
  {$WARN UNIT_PLATFORM OFF}
  VCL.FileCtrl,
  {$WARN UNIT_PLATFORM ON}
  UYieldModelDataObject,
  UPlanningModelDataObject,
  UErrorHandlingOperations,
  Math,
  UNetworkElementData,
  UChannelData,
  UReservoirData,
  VCL.CheckLst,
  VCL.Forms,
  UWRPMPltFileManager,
  UWRPMOutputFileMerger,
  URunConfigurationData;

{ TOutpuCollateFilesValidator }

procedure TOutpuCollateFilesValidator.CreateMemberObjects;
const OPNAME = 'TOutpuCollateFilesValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;

    FPltFilesList := TStringList.Create;
    FSysFilesList := TStringList.Create;
    FResFilesList := TStringList.Create;
    FPmpFilesList := TStringList.Create;
    FSequences    := TStringList.Create;

    FPanel := TOutpuCollateFilesDialog.Create(FPanelOwner,FAppModules);
    with OutpuCollateFilesDialog do
    begin
      BtnAddFiles.OnClick       := OnBtnAddFilesClick;
      BtnDeleteFiles.OnClick    := OnBtnDeleteFilesClick;
      BtnCollateFiles.OnClick   := OnBtnCollateFilesClick;
      grdDataGrid.OnColEnter    := OnGridColEnter;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutpuCollateFilesValidator.DestroyMemberObjects;
const OPNAME = 'TOutpuCollateFilesValidator.DestroyMemberObjects';
begin
  try
    FreeAndNil(FPltFilesList);
    FreeAndNil(FSysFilesList);
    FreeAndNil(FResFilesList);
    FreeAndNil(FPmpFilesList);
    FreeAndNil(FSequences);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutpuCollateFilesValidator.Initialise: boolean;
const OPNAME = 'TOutpuCollateFilesValidator.Initialise';
begin
  Result := inherited Initialise;
  try
    SetButtonState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutpuCollateFilesValidator.LanguageHasChanged: boolean;
const OPNAME = 'TOutpuCollateFilesValidator.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    TabShetCaption := FAppModules.Language.GetString('TOutpuCollateFilesValidator.CollateFiles');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutpuCollateFilesValidator.OutpuCollateFilesDialog : TOutpuCollateFilesDialog;
const OPNAME = 'TOutpuCollateFilesValidator.OutpuCollateFilesDialog';
begin
  Result := nil;
  try
    if Assigned(FPanel) then
      Result := TOutpuCollateFilesDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutpuCollateFilesValidator.ClearDataViewer;
const OPNAME = 'TOutpuCollateFilesValidator.ClearDataViewer';
begin
  inherited ClearDataViewer;
  try
    OutpuCollateFilesDialog.grdDataGrid.ColCount := 2;
    OutpuCollateFilesDialog.grdDataGrid.Cols[1].Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutpuCollateFilesValidator.PopulateDataViewer;
const OPNAME = 'TOutpuCollateFilesValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    SetButtonState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutpuCollateFilesValidator.SetButtonState;
const OPNAME = 'TOutpuCollateFilesValidator.RePopulateDataViewer';
var
  LRunType          : string;
begin
  try
    LRunType := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData.RunSequenceType;
    with OutpuCollateFilesDialog do
    begin
      BtnAddFiles.Enabled       := (LRunType <> 'H');
      BtnDeleteFiles.Enabled    := (LRunType <> 'H') and ((FPltFilesList.Count > 0));
      FileListCheckList.Enabled := (LRunType <> 'H');
      BtnCollateFiles.Enabled   := (LRunType <> 'H') and (FPltFilesList.Count > 1);
      grdDataGrid.Enabled       := (LRunType <> 'H');
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutpuCollateFilesValidator.RePopulateDataViewer;
const OPNAME = 'TOutpuCollateFilesValidator.RePopulateDataViewer';
var
  LColCount,
  LCol              : integer;
begin
  try
    ClearDataViewer;
    LColCount := MaxIntValue([FPltFilesList.Count,FSysFilesList.Count,FResFilesList.Count,FPmpFilesList.Count]);
    if(LColCount > 0) then
    begin
      with OutpuCollateFilesDialog do
      begin
        grdDataGrid.ColCount := LColCount + 1;
        for LCol := 1 to LColCount do
        begin
          if(FileListCheckList.Checked[0] and (FPltFilesList.Count >= LCol)) then
             grdDataGrid.Cells[LCol,0] := ExtractFilePath(FPltFilesList[LCol-1])
          else if(FileListCheckList.Checked[1] and (FSysFilesList.Count >= LCol)) then
             grdDataGrid.Cells[LCol,0] := ExtractFilePath(FSysFilesList[LCol-1])
          else if(FileListCheckList.Checked[2] and (FResFilesList.Count >= LCol)) then
             grdDataGrid.Cells[LCol,0] := ExtractFilePath(FResFilesList[LCol-1])
          else if(FileListCheckList.Checked[3] and (FPmpFilesList.Count>= LCol)) then
             grdDataGrid.Cells[LCol,0] := ExtractFilePath(FPmpFilesList[LCol-1]);

          if(FileListCheckList.Checked[0] and (FPltFilesList.Count >= LCol)) then
            grdDataGrid.Cells[LCol,1] := ExtractFileName(FPltFilesList[LCol-1]);
          if(FileListCheckList.Checked[1] and (FSysFilesList.Count >= LCol)) then
            grdDataGrid.Cells[LCol,2] := ExtractFileName(FSysFilesList[LCol-1]);
          if(FileListCheckList.Checked[2] and (FResFilesList.Count >= LCol)) then
            grdDataGrid.Cells[LCol,3] := ExtractFileName(FResFilesList[LCol-1]);
          if(FileListCheckList.Checked[3] and (FPmpFilesList.Count >= LCol)) then
            grdDataGrid.Cells[LCol,4] := ExtractFileName(FPmpFilesList[LCol-1]);
          if(FSequences.Count >= LCol) then
            grdDataGrid.Cells[LCol,5] := FSequences[LCol-1];

          grdDataGrid.CurCol := LCol;
          grdDataGrid.CurRow := 0;
          grdDataGrid.DoubleClickCurrentColumn;
        end;
      end;
    end;
    SetButtonState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutpuCollateFilesValidator.OnBtnAddFilesClick(Sender: TObject);
const OPNAME = 'TOutpuCollateFilesValidator.OnBtnAddFilesClick';
var
  LFilePath,
  LFileNamePrefix,
  LPltFileName,
  LSysFileName,
  LResFileName,
  LPmpFileName : string;
  LPltFileManager : TWRPMPltFileManager;
begin
  try
    LFilePath := TYieldModelDataObject(FAppModules.Model.ModelData).FileNamesObject.OutputFilesPath;
    if SelectDirectory(LFilePath,[],0) then
    begin
      LFilePath       := IncludeTrailingPathDelimiter(LFilePath);
      LFileNamePrefix :=  TYieldModelDataObject(FAppModules.Model.ModelData).CastDataFilePaths.DataFilePrefix;

      LPltFileName := LFilePath + LFileNamePrefix + FAppModules.Language.GetString('TWRPMOutputFilesCollator.PltOut');
      LSysFileName := LFilePath + LFileNamePrefix + FAppModules.Language.GetString('TWRPMOutputFilesCollator.SysOut');
      LResFileName := LFilePath + LFileNamePrefix + FAppModules.Language.GetString('TWRPMOutputFilesCollator.ResOut');
      LPmpFileName := LFilePath + LFileNamePrefix + FAppModules.Language.GetString('TWRPMOutputFilesCollator.PmpOut');

      if(not FileExists(LPltFileName)) and (not FileExists(LPltFileName)) and (not FileExists(LPltFileName)) and (not FileExists(LPltFileName)) then
      begin
        ShowMessage('The selected directory does not contain output files.');
        Exit;
      end;

      if not FileExists(LPltFileName) then
      begin
        ShowMessage('The selected directory does not contain the .PLT output files.');
        Exit;
      end;

      //if OutpuCollateFilesDialog.FileListCheckList.Checked[0] then
      //begin
        if FileExists(LPltFileName) then
        begin
          FPltFilesList.Add(LPltFileName);

          LPltFileManager := TWRPMPltFileManager.Create(FAppModules);
          try
            LPltFileManager.Initialise;
            if LPltFileManager.ReadHeaderData(LPltFileName) then
            begin
              if(LPltFileManager.HeaderData.SequencesList.Count > 0) then
                FSequences.Add(LPltFileManager.HeaderData.SequencesList[0]+'-'+LPltFileManager.HeaderData.SequencesList[LPltFileManager.HeaderData.SequencesList.Count-1])
              else
                FSequences.Add('');
            end;
          finally
            LPltFileManager.Free;
          end;
        end
        else
        begin
          FPltFilesList.Add('');
          ShowMessage('File named('+LPltFileName+') does not exist.');
        end;
      //end;

      if OutpuCollateFilesDialog.FileListCheckList.Checked[1] then
      begin
        if FileExists(LSysFileName) then
          FSysFilesList.Add(LSysFileName)
        else
        begin
          FSysFilesList.Add('');
          ShowMessage('File named('+LPltFileName+') does not exist.');
        end;
      end;

      if OutpuCollateFilesDialog.FileListCheckList.Checked[2] then
      begin
        if FileExists(LResFileName) then
          FResFilesList.Add(LResFileName)
        else
        begin
          FResFilesList.Add('');
          ShowMessage('File named('+LPltFileName+') does not exist.');
        end;
      end;

      if OutpuCollateFilesDialog.FileListCheckList.Checked[3] then
      begin
        if FileExists(LPmpFileName) then
          FPmpFilesList.Add(LPmpFileName)
        else
        begin
          FPmpFilesList.Add('');
          ShowMessage('File named('+LPltFileName+') does not exist.');
        end;
      end;
      RePopulateDataViewer;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutpuCollateFilesValidator.OnBtnDeleteFilesClick(Sender: TObject);
const OPNAME = 'TOutpuCollateFilesValidator.OnBtnDeleteFilesClick';
var
  LCol   : integer;
begin
  try
    LCol := OutpuCollateFilesDialog.grdDataGrid.Col-1;
    if(LCol <= FPltFilesList.Count-1) then
       FPltFilesList.Delete(LCol);
    if(LCol <= FSysFilesList.Count-1) then
       FSysFilesList.Delete(LCol);
    if(LCol <= FResFilesList.Count-1) then
       FResFilesList.Delete(LCol);
    if(LCol <= FPmpFilesList.Count-1) then
       FPmpFilesList.Delete(LCol);
    if(LCol <= FSequences.Count-1) then
       FSequences.Delete(LCol);

    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutpuCollateFilesValidator.CheckIfOutputFilesOverite: boolean;
const OPNAME = 'TOutpuCollateFilesValidator.OnBtnCollateFilesClick';
var
  LFilePath,
  LFileNamePrefix,
  LPltFileName,
  LSysFileName,
  LResFileName,
  LPmpFileName,
  LMessage          : string;

begin
  Result := False;
  try
    LFilePath       := TYieldModelDataObject(FAppModules.Model.ModelData).FileNamesObject.OutputFilesPath;
    LFilePath       := IncludeTrailingPathDelimiter(LFilePath);
    LFileNamePrefix :=  TYieldModelDataObject(FAppModules.Model.ModelData).CastDataFilePaths.DataFilePrefix;

    LPltFileName := LFilePath + LFileNamePrefix + FAppModules.Language.GetString('TWRPMOutputFilesCollator.PltOut');
    LSysFileName := LFilePath + LFileNamePrefix + FAppModules.Language.GetString('TWRPMOutputFilesCollator.SysOut');
    LResFileName := LFilePath + LFileNamePrefix + FAppModules.Language.GetString('TWRPMOutputFilesCollator.ResOut');
    LPmpFileName := LFilePath + LFileNamePrefix + FAppModules.Language.GetString('TWRPMOutputFilesCollator.PmpOut');

    if FileExists(LPltFileName) or FileExists(LSysFileName) or  FileExists(LResFileName) or FileExists(LPmpFileName)then
    begin
      LMessage := FAppModules.Language.GetString('TOutpuCollateFilesValidator.OverwriteExistingFiles');
      if(MessageDlg(LMessage,mtWarning,[mbYes,mbNo],0) = mrNo) then
        Exit;
    end;

    if FileExists(LPltFileName) then
      DeleteFile(LPltFileName);
    if FileExists(LSysFileName) then
      DeleteFile(LSysFileName);
    if FileExists(LResFileName) then
      DeleteFile(LResFileName);
    if FileExists(LPmpFileName) then
      DeleteFile(LPmpFileName);

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutpuCollateFilesValidator.OnBtnCollateFilesClick(Sender: TObject);
const OPNAME = 'TOutpuCollateFilesValidator.OnBtnCollateFilesClick';
var
  LOldCursor                : TCursor;
  LFileMerger               : TWRPMOutputFileMerger;
begin
  try
    if not CheckIfOutputFilesOverite then Exit;
    if(FPltFilesList.Count = 0) then
    begin
      ShowMessage('You cannot merge output files without selecting .plt files.');
      Exit;
    end;

    OutpuCollateFilesDialog.CollateProgressBar.Position := 0;
    OutpuCollateFilesDialog.CollateProgressBar.Min      := 0;
    OutpuCollateFilesDialog.CollateProgressBar.Max      := FPltFilesList.Count + FSysFilesList.Count + FResFilesList.Count + FPmpFilesList.Count;
    OutpuCollateFilesDialog.CollatingLabel.Caption      := '';
    LOldCursor    := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    LFileMerger   := TWRPMOutputFileMerger.Create(FAppModules);
    try
      OutpuCollateFilesDialog.CollatingLabel.Caption := 'Collating started.';
      Application.ProcessMessages;

      LFileMerger.Initialise;
      if LFileMerger.MergeAllOutputFiles(FPltFilesList,FSysFilesList,FResFilesList,FPmpFilesList,ShowProgress) then
      begin
        OutpuCollateFilesDialog.CollatingLabel.Caption := FAppModules.Language.GetString('TOutpuCollateFilesValidator.CollatingCompleted');
        Application.ProcessMessages;

        TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData.NumberOfSequencesInAnalysis := LFileMerger.SequencesCount;
        TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData.StartSequenceNumber         := LFileMerger.FirstSequenceNumber - 1;
      end;
    finally
      Screen.Cursor := LOldCursor;
      LFileMerger.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TOutpuCollateFilesValidator.ShowProgress(AProgress: String; AProgressType: TProgressType; var AStop: boolean;
                                                   AUpdateSteps: boolean=False);
const OPNAME = 'TOutpuCollateFilesValidator.ShowProgress';
begin
  try
    Application.ProcessMessages;
    if(Trim(AProgress) <> '') then
      OutpuCollateFilesDialog.CollatingLabel.Caption := AProgress;
    if AUpdateSteps then
      OutpuCollateFilesDialog.CollateProgressBar.StepBy(1);
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TOutpuCollateFilesValidator.OnGridColEnter(Sender: TObject; ACol, ARow: integer);
const OPNAME = 'TOutpuCollateFilesValidator.OngrdDataGridClick';
begin
  try
    SetButtonState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutpuCollateFilesValidator.CanExport: boolean;
const OPNAME = 'TOutpuCollateFilesValidator.CanExport';
begin
  Result := False;
  try
    if (OutpuCollateFilesDialog <> nil) then
      Result := OutpuCollateFilesDialog.CanExport;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutpuCollateFilesValidator.CanPrint: boolean;
const OPNAME = 'TOutpuCollateFilesValidator.CanPrint';
begin
  Result := False;
  try
    if (OutpuCollateFilesDialog <> nil) then
      Result := OutpuCollateFilesDialog.CanPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutpuCollateFilesValidator.DoExport(AFileName: string = '');
const OPNAME = 'TOutpuCollateFilesValidator.DoExport';
begin
  try
    OutpuCollateFilesDialog.DoExport(AFileName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutpuCollateFilesValidator.DoPrint;
const OPNAME = 'TOutpuCollateFilesValidator.DoPrint';
begin
  try
    OutpuCollateFilesDialog.DoPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

