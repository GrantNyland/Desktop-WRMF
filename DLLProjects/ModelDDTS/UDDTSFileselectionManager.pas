//
//
//  UNIT      : Contains TYieldFileSelectionManager Class
//  AUTHOR    : Dziedzi Ramulondi
//  DATE      : 2002/12/12
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UDDTSFileselectionManager;

interface

uses
  Classes,
  VCL.ComCtrls,
  VCL.Dialogs,
  UFileNames,
  VoaimsCom_TLB,
  UAbstractObject,
  UDataFileObjects,
  UFileselectionManager,
  UAbstractFileNamesObject,
  UDDTSFileNamesFileAgent,
  UDDTSFileNamesDatabaseAgent;

type
  TDDTSFileSelectionManager = class(TFileselectionManager)
  protected
    FFileNamesFileAgent     :TDDTSFileNamesFileAgent;
    FFileNamesDatabaseAgent :TDDTSFileNamesDatabaseAgent;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function CreateInputFileNames(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames): boolean;
    function CreateOutputFileNames(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames): boolean; virtual;

    //function PopulateFileNamesFromFiles(AFileNamesObject :TModelFileNames): boolean;
    //function PopulateFileNamesFromDB(AFileNamesObject :TModelFileNames): boolean;
  public
    function PopulateFileNames(ADataFileObjects: TDataFileObjects; AFileNamesObject :TModelFileNames): boolean; override;
    function PopulateTreeView(ATreeView: TTreeView;AFileNamesObject :TModelFileNames): boolean; override;
  end;

implementation

uses
  UUtilities,
  SysUtils,
  UYieldModelDataObject,
  UProgressDialog,
  UErrorHandlingOperations;


{ TDDTSFileSelectionManager }

procedure TDDTSFileSelectionManager.CreateMemberObjects;
const OPNAME = 'TDDTSFileSelectionManager.CreateMemberObjects';
begin
  try
    FFileNamesFileAgent     := TDDTSFileNamesFileAgent.Create(FAppModules);
    FFileNamesDatabaseAgent := TDDTSFileNamesDatabaseAgent.Create(FAppModules);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDDTSFileSelectionManager.DestroyMemberObjects;
const OPNAME = 'TDDTSFileSelectionManager.DestroyMemberObjects';
begin
  try
    FreeAndNil(FFileNamesFileAgent);
    FreeAndNil(FFileNamesDatabaseAgent);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSFileSelectionManager.PopulateFileNames(ADataFileObjects: TDataFileObjects;
         AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'TDDTSFileSelectionManager.PopulateFileNames';
var
  //LStopOnFirstError: boolean;
  LProgressDialog: TProgressDialog;
  LResult : boolean;
begin
  Result := False;
  try
    if not Assigned(AFileNamesObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    AFileNamesObject.Reset;
    LResult := FFileNamesDatabaseAgent.ReadDirectoryFileContents(ADataFileObjects,AFileNamesObject);
    if not LResult then
      LResult := FFileNamesFileAgent.ReadDirectoryFileContents(ADataFileObjects,AFileNamesObject,LProgressDialog.ShowProgress);

    if LResult  then
    begin
     CreateInputFileNames(ADataFileObjects,AFileNamesObject);
     CreateOutputFileNames(ADataFileObjects,AFileNamesObject);
    end;

    LProgressDialog := TProgressDialog.Create(nil,FAppModules);
    try
      LResult := FFileNamesDatabaseAgent.ReadDirectoryFileContents(ADataFileObjects,AFileNamesObject);
      if not LResult then
        FFileNamesFileAgent.ReadDirectoryFileContents(ADataFileObjects,AFileNamesObject,LProgressDialog.ShowProgress);

      LResult :=  FFileNamesDatabaseAgent.ReadDirectoryFileName(ADataFileObjects,AFileNamesObject);
      if not LResult then
        FFileNamesFileAgent.ReadDirectoryFileName(ADataFileObjects,AFileNamesObject,LProgressDialog.ShowProgress);

      LResult :=  FFileNamesDatabaseAgent.ReadInputFileNames(ADataFileObjects,AFileNamesObject);
      if not LResult then
        FFileNamesFileAgent.ReadInputFileNames(ADataFileObjects,AFileNamesObject,LProgressDialog.ShowProgress);

      LResult :=  FFileNamesDatabaseAgent.ReadOutputFileNames(ADataFileObjects,AFileNamesObject);
      if not LResult then
        FFileNamesFileAgent.ReadOutputFileNames(ADataFileObjects,AFileNamesObject,LProgressDialog.ShowProgress);

      if (LProgressDialog.ErrorCount > 0)  and Assigned(FAppModules.MainForm()) and
         (not AFileNamesObject.ParamFileNames.FileNameObject[0].SavedInDB) then
      begin
        LProgressDialog.FStartButton.Visible := False;
        LProgressDialog.FStopButton.Visible := False;
        LProgressDialog.ActionProgressBar.Visible := False;
        LProgressDialog.ShowModal;
      end;
      AFileNamesObject.CastOutputFileNames.SetFileHintsSet(True);
    finally
      FreeAndNil(LProgressDialog);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSFileSelectionManager.CreateInputFileNames(ADataFileObjects: TDataFileObjects;
         AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'TDDTSFileSelectionManager.CreateInputFileNames';
var
  LFileName,
  LFilePrefix: string;
begin
  Result := False;
  try

    if Assigned(ADataFileObjects) and Assigned(AFileNamesObject) and
       (AFileNamesObject.InputFilesPath <> '') then
    begin
      LFilePrefix := Trim(AFileNamesObject.InputFilesPath);
      LFilePrefix := IncludeTrailingPathDelimiter(LFilePrefix);
      LFilePrefix := LFilePrefix + Trim(AFileNamesObject.FileNamePrefix);

      LFileName := LFilePrefix + 'Runoff.csv';
      AFileNamesObject.UpdateConfigFileName(0,LFileName,False,0.0,0.0);
      LFileName := LFilePrefix + 'OtherInflow.csv';
      AFileNamesObject.UpdateConfigFileName(1,LFileName,False,0.0,0.0);
      LFileName := LFilePrefix + 'IncreamentalRunoff.csv';
      AFileNamesObject.UpdateConfigFileName(2,LFileName,False,0.0,0.0);
      LFileName := LFilePrefix + 'EWR.csv';
      AFileNamesObject.UpdateConfigFileName(3,LFileName,False,0.0,0.0);
      LFileName := LFilePrefix + 'Rainfall.csv';
      AFileNamesObject.UpdateConfigFileName(4,LFileName,False,0.0,0.0);
      LFileName := LFilePrefix + 'Evaporation.csv';
      AFileNamesObject.UpdateConfigFileName(5,LFileName,False,0.0,0.0);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSFileSelectionManager.CreateOutputFileNames(ADataFileObjects: TDataFileObjects;
         AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'TDDTSFileSelectionManager.CreateOutputFileNames';
var
  LCurrentFile,
  LFilePrefix: string;
  //LCount: Integer;
begin
  Result := False;
  try
    if Assigned(ADataFileObjects) and Assigned(AFileNamesObject) and
       (AFileNamesObject.OutputFilesPath <> '') then
    begin
      LFilePrefix := Trim(AFileNamesObject.OutputFilesPath);
      LFilePrefix := IncludeTrailingPathDelimiter(LFilePrefix);
      LFilePrefix := LFilePrefix + Trim(AFileNamesObject.FileNamePrefix);

      LCurrentFile := LFilePrefix + 'Output.csv';
      if FileExists(LCurrentFile) then
        AFileNamesObject.UpdateOutputFileName(oftHydroPower,LCurrentFile,False,0.0,
        FileLastWriteDate(LCurrentFile))
      else
        AFileNamesObject.UpdateOutputFileName(oftHydroPower,LCurrentFile,False,0.0,0.0);

      LCurrentFile := LFilePrefix + 'Output.xls';
      if FileExists(LCurrentFile) then
        AFileNamesObject.UpdateOutputFileName(oftPlot,LCurrentFile,False,0.0,
        FileLastWriteDate(LCurrentFile))
      else
        AFileNamesObject.UpdateOutputFileName(oftPlot,LCurrentFile,False,0.0,0.0);

      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSFileSelectionManager.PopulateTreeView(ATreeView: TTreeView; AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'TDDTSFileSelectionManager.PopulateTreeView';
var
  LMainNode: TTreeNode;
  LIndex: integer;
  LFileName: TAbstractModelFileName;
  LOnTreeViewNodeChangedEvent  :TTVChangedEvent;
  LOnTreeViewNodeChangingEvent :TTVChangingEvent;
begin
  inherited PopulateTreeView(ATreeView,AFileNamesObject);
  Result := False;
  try
    if not Assigned(AFileNamesObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if not Assigned(ATreeView) then
      raise Exception.Create('Tree View object parameter is not yet assigned.');

    LOnTreeViewNodeChangedEvent  := ATreeView.OnChange;
    LOnTreeViewNodeChangingEvent := ATreeView.OnChanging;
    try
      ATreeView.OnChange   := nil;
      ATreeView.OnChanging := nil;
      ATreeView.Selected   := nil;

      //DirectoryFileName
      ATreeView.Items.Clear;

      //DirectoryFileName
      //if (FAppModules.StudyArea.ModelVersion <> '7') then
      //begin
        LMainNode := ATreeView.Items.AddObjectFirst(nil,AFileNamesObject.DirectoryFileNames.CaptionStr,AFileNamesObject.DirectoryFileNames);
        for LIndex := 0 to AFileNamesObject.DirectoryFileNames.Count - 1 do
        begin
          LFileName := AFileNamesObject.DirectoryFileNames.FileNameObject[LIndex];
          ATreeView.Items.AddChildObject(LMainNode,LFileName.ShortName,LFileName);
        end;
      //end;

      //ConfigFileNames
      if (AFileNamesObject.ConfigFileNames.Count > 0) then
      begin
        LMainNode := ATreeView.Items.AddObject(nil,AFileNamesObject.ConfigFileNames.CaptionStr,AFileNamesObject.ConfigFileNames);
        for LIndex := 0 to AFileNamesObject.ConfigFileNames.Count - 1 do
        begin
          if(LIndex > 5) then Break;
          LFileName := AFileNamesObject.ConfigFileNames.FileNameObject[LIndex];
          ATreeView.Items.AddChildObject(LMainNode,LFileName.ShortName,LFileName);
        end;
      end;

      //OutputFileNames
      if (AFileNamesObject.OutputFileNames.Count > 0) then
      begin
        LMainNode := ATreeView.Items.AddObject(nil,AFileNamesObject.OutputFileNames.CaptionStr,AFileNamesObject.OutputFileNames);
        for LIndex := 0 to AFileNamesObject.OutputFileNames.Count - 1 do
        begin
           if(LIndex > 1) then Break;
           LFileName := AFileNamesObject.CastOutputFileNames[LIndex];
           ATreeView.Items.AddChildObject(LMainNode,LFileName.ShortName,LFileName);
        end;
      end;
    finally
      ATreeView.OnChange   := LOnTreeViewNodeChangedEvent;
      ATreeView.OnChanging := LOnTreeViewNodeChangingEvent;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
