unit USelectIncForm;

interface

uses
  Windows, Messages, SysUtils, Classes, VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs,
  VCL.StdCtrls, VCL.ComCtrls,VCL.ExtCtrls,
  UFileCollect, UStomsaGlobalData, UStomsaData, UAbstractObject, VCLTee.TeeProcs,
  VCLTee.TeEngine, VCLTee.Chart, VCLTee.Series, VclTee.TeeGDIPlus;

type
  TfmSelectInc = class(TFrame)
    fclIncFiles     : TFileCollect;
    stbIncStatus    : TStatusBar;
    chtIncFile: TChart;
    pnlChartOptions : TPanel;
    rgChartType     : TRadioGroup;
    chkbxMAR        : TCheckBox;
    Series1: TLineSeries;
    Series2: TLineSeries;
    Series3: TLineSeries;
    procedure fclIncFilesFileAdd(Sender: TObject; FileName, FileDir: String);
    procedure fclIncFilesFileRemove(Sender: TObject; FileName, FileDir: String);
    procedure chkbxMARClick(Sender: TObject);
    procedure rgChartTypeClick(Sender: TObject);
    procedure fclIncFilesFOnFileSelected(Sender: TObject; AIndex: Integer);
  protected
    FAppModules: TAppModules;
    FBusy : boolean;
    //FMesg22Files : boolean;
    procedure PopulateChart;
    procedure PopulateFileAreaGrid;
    procedure PopulateFileAreaData;
    function StripSlash(Directory: string): string;
    procedure OnCellSelected(Sender: TObject; ACol, ARow: Longint; var CanSelect: Boolean);
    procedure OnFileAddCompleted(Sender : TObject);
    procedure OnFileRemoveCompleted(Sender : TObject);
  public
    procedure AfterConstruction; override;
    constructor Create(AOwner: TComponent; AAppModules: TAppModules); reintroduce; virtual;
  end;

var
  fmSelectInc: TfmSelectInc;

implementation

{$R *.DFM}

uses
  UDataModule,
  UMainMenuEventType,
  UErrorHandlingOperations;

constructor TfmSelectInc.Create(AOwner: TComponent;AAppModules: TAppModules);
const OPNAME = 'TfmSelectInc.Create';
begin
  inherited Create(AOwner);
  try
    FAppModules := AAppModules;
    fclIncFiles.FileAreaGrid.OnSelectCell := OnCellSelected;
    fclIncFiles.OnFileAddComplete         := OnFileAddCompleted;
    fclIncFiles.OnFileRemoveComplete      := OnFileRemoveCompleted;
    fclIncFiles.ScenarioPath              :=  FAppModules.StudyArea.DataFilesPath;

    Series2.LinePen.Style := psDash;
    chkbxMAR.Checked := True;
    rgChartType.ItemIndex := 0;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfmSelectInc.AfterConstruction;
const OPNAME = 'TfmSelectInc.AfterConstruction';
var
  LIndex  : integer;
begin
  inherited;
  try
    FBusy := False;
    //FMesg22Files := False;
    //check to see if some stuff already exists in the datastorage object
    //this would occur as a result of a file open operation
    if fmData.DataStorage.IncFilesHaveChanged then
    begin
      fclIncFiles.ClearAll;
      for LIndex := 0 to fmData.DataStorage.IncFileCount-1 do
      begin
        if  fmData.DataStorage.GotoIndex(LIndex) then
        begin
          fclIncFiles.AddItem(fmData.DataStorage.CurrentRecord.FileName,
                              StripSlash(fmData.DataStorage.CurrentRecord.Directory));
        end;
      end;
      fmData.DataStorage.IncFilesHaveChanged := false;
      OnFileAddCompleted(nil);
      PopulateFileAreaGrid;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmSelectInc.PopulateFileAreaGrid;
const OPNAME = 'TfmSelectInc.PopulateFileAreaGrid';
var
  LIndex : integer;
begin
  try
    FBusy := True;
    try
      for LIndex := 1 to fclIncFiles.FileAreaGrid.RowCount-1 do
      begin
        fclIncFiles.FileAreaGrid.Cells[0,LIndex] := '';
        fclIncFiles.FileAreaGrid.Cells[1,LIndex] := '';
      end;
      LIndex := 1;
      if not fmData.DataStorage.First then  Exit;
      repeat
        fclIncFiles.FileAreaGrid.Cells[0,LIndex] := FormatFloat('##0.00',fmData.DataStorage.CurrentRecord.MAR);
        fclIncFiles.FileAreaGrid.Cells[1,LIndex] := FormatFloat('##0.00',fmData.DataStorage.CurrentRecord.Area);
        LIndex := LIndex + 1;
      until (not fmData.DataStorage.Next);
      fclIncFiles.FileAreaGrid.Refresh;
    finally
      FBusy := False;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmSelectInc.PopulateFileAreaData;
const OPNAME = 'TfmSelectInc.PopulateFileAreaData';
var
  LIndex    : integer;
  LRowValue : double;
  LIncData  : TIncData;
  LPath     : string;
  LFileName : string;
  LName     : string;

begin
  try
    if FBusy then Exit;
    for LIndex := 1 to fclIncFiles.FileAreaGrid.RowCount-1 do
    begin
      LName     := fclIncFiles.SelectedFileNameByIndex[LIndex-1];
      LPath     := ExtractFilePath(LName);
      LFileName := ExtractFileName(LName);
      LIncData := fmData.DataStorage.FindIncFileByName(LPath,LFileName);
      if(LIncData <> nil) then
      begin
        LRowValue := StrToFloatDef(fclIncFiles.FileAreaGrid.Cells[1,LIndex],-1);
        if(LRowValue < 0.0) then
          Beep
        else
        begin
          LIncData.Area := LRowValue;
        end;
      end;
    end;
    PopulateFileAreaGrid;
    FAppModules.Model.ProcessEvent(CmeStomsaSaveData,nil)
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmSelectInc.OnCellSelected(Sender: TObject; ACol, ARow: Longint; var CanSelect: Boolean);
const OPNAME = 'TfmSelectInc.OnCellSelected';
begin
  try
    PopulateFileAreaData;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TfmSelectInc.StripSlash(Directory : string) : string;
const OPNAME = 'TfmSelectInc.StripSlash';
begin
  Result := '';
  try
    if Directory[Length(Directory)] = '\' then
      Result := copy(Directory,1,Length(Directory)-1);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmSelectInc.fclIncFilesFileAdd(Sender: TObject; FileName, FileDir: String);
const OPNAME = 'TfmSelectInc.fclIncFilesFileAdd';
begin
  try
    //need to add the file names
    if fmData.DataStorage.IncFileCount <= 199 then
    begin
      stbIncStatus.Panels[1].Text := 'Adding File ' + FileDir + '\' + FileName;
      fmData.DataStorage.AddNode(FileDir,FileName);
      stbIncStatus.Panels[0].Text := 'Count:' + IntToStr(fmData.DataStorage.IncFileCount);
      stbIncStatus.Refresh;
      PopulateFileAreaGrid;
    end
    else
    begin
      ShowMessage('Maximum of 200 files permitted');
    end;

    {if (not FMesg22Files) and (fmData.DataStorage.IncFileCount > 22) then
    begin
      FMesg22Files := True;
      ShowMessage('There are more than 22 gauges selected, please ensure that no more than 22 key gauges are used.');
    end;}
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmSelectInc.fclIncFilesFileRemove(Sender: TObject; FileName, FileDir: String);
const OPNAME = 'TfmSelectInc.fclIncFilesFileRemove';
begin
  try
    //remove any files selected
    //FMesg22Files := False;
    stbIncStatus.Panels[1].Text := 'Removing File ' + FileDir + FileName;
    fmData.DataStorage.RemoveNode(FileDir,FileName);
    PopulateFileAreaGrid;
    stbIncStatus.Panels[0].Text := 'Count:' + IntToStr(fmData.DataStorage.IncFileCount);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmSelectInc.PopulateChart;
const OPNAME = 'TfmSelectInc.PopulateChart';
var
  LCurrentRecord : TIncData;
  LDate  : TDateTime;
  LRow   : integer;
  LCol   : integer;
begin
  try
    LCurrentRecord := fmData.DataStorage.CurrentRecord;
    if(LCurrentRecord <> nil) then
    begin
      chtIncFile.Title.Text.Text := 'Timeseries of '+ LCurrentRecord.FileName;
      LDate := EncodeDate(LCurrentRecord.StartYear,FAppModules.StudyArea.CalendarStartMonth,1);
      Series1.Clear;
      Series3.Clear;
      Series2.Clear;
      Series2.AddXY(LDate,LCurrentRecord.MAR);
      for LRow := 1 to LCurrentRecord.RecordLength do
      begin
        Series2.AddXY(LDate,LCurrentRecord.MAR);
        Series3.AddXY(LDate,LCurrentRecord.AnnualRawIncData[LRow]);
        for LCol := 1 to 12 do
        begin
          Series1.AddXY(LDate,LCurrentRecord.MonthlyRawIncData[LRow,LCol]);
          LDate := IncMonth(LDate,1);
        end;
      end;
    end;

    rgChartTypeClick(Nil);
    chkbxMARClick(Nil);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmSelectInc.chkbxMARClick(Sender: TObject);
const OPNAME = 'TfmSelectInc.chkbxMARClick';
begin
  try
    Series2.Visible := chkbxMAR.Checked;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmSelectInc.rgChartTypeClick(Sender: TObject);
const OPNAME = 'TfmSelectInc.rgChartTypeClick';
begin
  try
    Series1.Visible := (rgChartType.ItemIndex = 0);
    Series3.Visible := (rgChartType.ItemIndex = 1);
    if(rgChartType.ItemIndex = 0) then
      chtIncFile.LeftAxis.Title.Caption := 'Mm3/month'
    else
      chtIncFile.LeftAxis.Title.Caption := 'Mm3/annum'

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmSelectInc.fclIncFilesFOnFileSelected(Sender: TObject; AIndex: Integer);
const OPNAME = 'TfmSelectInc.fclIncFilesFOnFileSelected';
begin
  try
    fmData.DataStorage.GotoIndex(AIndex+1);
    PopulateChart;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmSelectInc.OnFileAddCompleted(Sender: TObject);
const OPNAME = 'TfmSelectInc.OnFileAddCompleted';
begin
  try
    FAppModules.Model.ProcessEvent(CmeStomsaFilesAdded,nil);
    PopulateFileAreaGrid;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmSelectInc.OnFileRemoveCompleted(Sender: TObject);
const OPNAME = 'TfmSelectInc.OnFileRemoveCompleted';
begin
  try
    FAppModules.Model.ProcessEvent(CmeStomsaFilesRemoved,nil);
    PopulateFileAreaGrid;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
