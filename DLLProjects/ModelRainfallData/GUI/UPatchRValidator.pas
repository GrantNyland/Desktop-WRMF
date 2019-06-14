{******************************************************************************}
{*  UNIT      : Contains TPatchRValidator Class                               *}
{*  AUTHOR    : RH Steyn(Cornastone)                                          *}
{*  DATE      : 18/05/2005                                                    *}
{*  COPYRIGHT : Copyright © 2005 DWAF                                         *}
{******************************************************************************}

unit UPatchRValidator;

interface

uses
  Classes,
  SysUtils,
  VCL.Dialogs,
  VCL.StdCtrls,
  VCL.Forms,
  VCL.Controls,
  Windows,

  UConstants,
  UAbstractObject,
  UDataComponent,
  VoaimsCom_TLB,
  UPatchRDialog;

type
  TPatchRValidator = class(TAbstractDataDialogValidator)
  protected

    FStationID           : integer;
    FSplitIndex          : integer;
    FPatchID             : integer;
    FPatchRInputFileOK   : boolean;
    FClassRMinStartYear  : string;
    FClassRMaxEndYear    : string;
    FPatchRMinStartYear  : string;
    FPatchRMaxEndYear    : string;
    FDefaultStartYear    : string;
    FDefaultEndYear      : string;
    FClassRGaugesMax     : string;
    FPatchRGaugesMax     : string;
    FClassRStartYear     : integer;
    FClassREndYear       : integer;
    FPatchRStartYear     : integer;
    FPatchREndYear       : integer;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function PatchRDialog : TPatchRDialog;
    procedure ChangeClassRPeriod (Sender : TObject);
    procedure DoRunClassR (Sender : TObject);
    procedure DoHeaderChange (Sender : TObject);
    function CreateClassRInputFiles : boolean;
    function CheckNumberOfChar (ANum : integer; AValue : string) : boolean;
    function CheckPathDosCompatibilty (AValue : string) : boolean;
    function FileReadyToBeAccessed (AFile : string) : boolean;
    function FilesReadyToBeAccessed (ADirectory : string;
                                     AFileList  : TStringList) : boolean;
    procedure DoClassRInputFileChange (Sender : TObject);
    procedure DoClassROutputFileChange (Sender : TObject);
    procedure ViewClassRInput (Sender : TObject);
    procedure ViewClassROutput (Sender : TObject);
    procedure CreatePatchRInputFile (Sender : TObject);
    procedure GetFileNames (AFileList  : TStringList;
                            AExtension : string);
    function RAWFilesExist : boolean;
    procedure DoNoOfSeasonsChanged (Sender : TObject);
    procedure SetSeasonsGrid (Sender : TObject);
    procedure SeasonsGridClick (Sender : TObject);
    procedure ChangePatchRPeriod (Sender : TObject);
    procedure ViewPatchROutput (Sender : TObject);
    procedure ViewPatchRPrinted (Sender : TObject);
    procedure ViewPatchRPlotted (Sender : TObject);
    procedure ViewPatchRInput (Sender : TObject);
    procedure PatchRPrintFilenameChanged (Sender : TObject);
    procedure PatchRPlotFilenameChanged (Sender: TObject);
    procedure PatchMultipleRadioGroupChanged (Sender: TObject);
    procedure PatchROutputFileComboBoxChanged (Sender : TObject);
    procedure DoRunPatchR (Sender : TObject);
    procedure SavePatchROutputToDB (APATFiles      : TStringList;
                                    APatchMultiple : boolean);
    procedure SavePatchRPrintToDB;
    procedure SavePatchRPlotToDB;
    procedure ShowRunProcess( aFileName : string );
    procedure ViewTextData (AFileName : string;
                            AFileDate : TDateTime;
                            AText     : TStringList);

    procedure DoSeasonsStr (APatchSeasonsStr : string;
                            ACol             : integer);
    procedure PopulatePatchInfo;
    procedure PopulateClassRInfo;
    procedure PopulatePatchRInput;
    procedure PopulateRunPatchR;
    procedure PopulatePatchROutputFiles;
    procedure ResetPatchROutputFilesCbx;
    procedure CreateClassRParamFile (ADefaultDir : string;
                                     ARunOption  : integer);
    procedure CreatePatchRParamFile (ADefaultDir    : string;
                                     ARunOption     : integer;
                                     var AStartYear : integer;
                                     var AEndYear   : integer);
    function GetSeasons (ASeasons     : TStringList;
                         var ALengths : string) : boolean;
    function ValidateSeasons : boolean;
    function ChopSeconds (ADate : TDateTime) : TDateTime;
    function StartEndYearInputWithinLimits (AUtility     : string;
                                            AStartYear   : string;
                                            AEndYear     : string;
                                            AStartLimit  : string;
                                            AEndLimit    : string) : boolean;
    function NumberOfStationsWithinLimit (AUtility : string) : boolean;
    procedure RePopulateDataViewer;
  public

    function Initialise : boolean; override;
    procedure PopulateDataViewer; override;
    procedure ClearDataViewer; override;
    function StudyHasChanged : boolean; override;
    function StudyDataHasChanged (AContext   : TChangeContext;
                                  AFieldName : string;
                                  AOldValue  : string;
                                  ANewValue  : string): Boolean; override;
    function SaveState: Boolean; override;
end;

implementation

uses
  System.UITypes,
  UErrorHandlingOperations,
  UShellExecuteObject,
  ShellAPI,
  UUtilities,
  DateUtils,
  RainfallCom_TLB,
  VCL.Graphics;

{ TPatchRValidator }

procedure TPatchRValidator.CreateMemberObjects;
const OPNAME = 'TPatchRValidator.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FStationID  := 0;
    FSplitIndex := -1;
    FPatchID    := 0;

    FPatchRInputFileOK := FALSE;

    FPanel := TPatchRDialog.Create(nil, FAppModules);
    with PatchRDialog do
    begin
      ClassRInputFileEdit.OnChange          := DoClassRInputFileChange;
      ClassRViewInputFileButton.OnClick     := ViewClassRInput;
      ClassROutputFileEdit.OnChange         := DoClassROutputFileChange;
      ClassRViewOutputFileButton.OnClick    := ViewClassROutput;
      ClassRHeaderEdit.OnChange             := DoHeaderChange;
      ClassRChangeDatesRadioGroup.OnClick   := ChangeClassRPeriod;
      ClassROption0Button.OnClick           := DoRunClassR;
      ClassROption1Button.OnClick           := DoRunClassR;
//      ClassROption2Button.OnClick           := DoRunClassR;
      ClassROption3Button.OnClick           := DoRunClassR;
      ClassROption4Button.OnClick           := DoRunClassR;
      ClassROption5Button.OnClick           := DoRunClassR;
      NoOfSeasonsEdit.OnExit                := SetSeasonsGrid;
      NoOfSeasonsEdit.OnChange              := DoNoOfSeasonsChanged;
      SeasonsGrid.OnClick                   := SeasonsGridClick;
      PatchRViewInputFileButton.OnClick     := ViewPatchRInput;
      PatchRCreateInputFileButton.OnClick   := CreatePatchRInputFile;
      PatchMultipleRadioGroup.OnClick       := PatchMultipleRadioGroupChanged;
      PatchRPeriodRadioGroup.OnClick        := ChangePatchRPeriod;
      PatchROutputFileNameComboBox.OnChange := PatchROutputFileComboBoxChanged;
      PatchRViewOutputFileButton.OnClick    := ViewPatchROutput;
      PatchRPrintFileNameEdit.OnChange      := PatchRPrintFilenameChanged;
      PatchRViewPrintFileButton.OnClick     := ViewPatchRPrinted;
      PatchRPlotFileNameEdit.OnChange       := PatchRPlotFilenameChanged;
      PatchRViewPlotFileButton.OnClick      := ViewPatchRPlotted;
      PatchRRunOption0Button.OnClick        := DoRunPatchR;
      PatchRRunOption1Button.OnClick        := DoRunPatchR;
      PatchRRunOption2Button.OnClick        := DoRunPatchR;
      PatchRRunOption3Button.OnClick        := DoRunPatchR;
      PatchRRunOption4Button.OnClick        := DoRunPatchR;
    end;
  except on E: Exception do HandleError (E, OPNAME) end;
end;

procedure TPatchRValidator.DestroyMemberObjects;
const OPNAME = 'TPatchRValidator.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
  except on E: Exception do HandleError (E, OPNAME) end;
end;

function TPatchRValidator.PatchRDialog : TPatchRDialog;
const OPNAME = 'TPatchRValidator.PatchRDialog';
begin
  Result := nil;
  try
    if (FPanel <> nil) then
      Result := TPatchRDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPatchRValidator.PopulateDataViewer;
const OPNAME = 'TPatchRValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
  except on E: Exception do HandleError (E, OPNAME) end;
end;

procedure TPatchRValidator.ClearDataViewer;
const OPNAME = 'TPatchRValidator.ClearDataViewer';
var
  lColIndex : integer;
  lRowIndex : integer;
begin
  inherited ClearDataViewer;
  try
    with PatchRDialog do
    begin
      ClassRGroupBox.Enabled := FALSE;
      PatchRGroupBox.Enabled := FALSE;
      RunPatchRGroupBox.Enabled := FALSE;
      PatchInfoGroupBox.Caption := FAppModules.Language.GetString('Rainfall.PatchInformation');

      PatchSourcesListBox.Items.Clear;
      TargetStationValue.Caption   := '';
      ClassRInputFileEdit.Text     := '';
      ClassROutputFileEdit.Text    := '';
      ClassRHeaderEdit.Text        := '';
      PatchRInputFileEdit.Text     := '';
      PatchRHeaderEdit.Text        := '';
      PatchRPrintFileNameEdit.Text := '';
      PatchRPlotFileNameEdit.Text  := '';
      PatchROutputFileNameComboBox.Clear;
      ShiftDataRadioGroup.ItemIndex          := 0;
      ClassRChangeDatesRadioGroup.ItemIndex  := -1;
      ClassRFirstYearComboBox.ItemIndex      := -1;
      ClassRLastYearComboBox.ItemIndex       := -1;
      PatchMultipleRadioGroup.ItemIndex      := -1;
      PatchRPeriodRadioGroup.ItemIndex       := 0;
      PatchRFirstYearComboBox.ItemIndex      := -1;
      PatchRLastYearComboBox.ItemIndex       := -1;
      PatchROutputFileNameComboBox.ItemIndex := -1;

      for lColIndex := 1 to SeasonsGrid.ColCount - 1 do
        for lRowIndex := 1 to SeasonsGrid.RowCount - 1 do
          SeasonsGrid.Cells[lColIndex, lRowIndex] := '';
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPatchRValidator.RePopulateDataViewer;
const OPNAME = 'TPatchRValidator.RePopulateDataViewer';
begin
  inherited;
  try
    FPatchID    := (FAppModules.Model.ModelData as IRainfallModelData).PatchRPatchID;
    FStationID  := (FAppModules.Model.ModelData as IRainfallModelData).PatchRStationID;
    FSplitIndex := (FAppModules.Model.ModelData as IRainfallModelData).PatchRSplitIndex;
    if ((FStationID <> 0) AND (FSplitIndex <> -1) AND (FPatchID <> 0)) then
    begin
      PopulatePatchInfo;
      PopulateClassRInfo;
      PopulatePatchRInput;
      PopulateRunPatchR;
    end
    else
    begin
      ShowMessage(FAppModules.Language.GetString('Rainfall.PatchNotSelected'));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPatchRValidator.CreateClassRInputFiles : boolean;
const OPNAME = 'TPatchRValidator.CreateClassRInputFiles';
var
  lFileName : string;
  lPatch    : IPatchData;
  lStation  : IStationData;
begin
  Result := False;
  try
    if ((FStationID <> 0) AND (FSplitIndex <> -1) AND (FPatchID <> 0)) then
    begin
      with PatchRDialog do
      begin
        if (ClassRHeaderEdit.Text = '') then
          ClassRHeaderEdit.Text := DateTimeToStr(Now);

        if (ClassRInputFileEdit.Text = '') then
          lFileName := FAppModules.Language.GetString('Rainfall.CInFile')
        else
          lFileName := Trim(ClassRInputFileEdit.Text);
        lStation := (FAppModules.Model.ModelData as IRainfallModelData).GetStationDataByID(FStationID);
        if (lStation <> nil) then
        begin
          lPatch := lStation.GetPatchWithID(FPatchID);
          if (lPatch <> nil) then
          begin
            lPatch.ClassRInputFileName := lFileName;
            ClassRInputFileEdit.Text  := lFileName;

            if (ClassROutputFileEdit.Text = '') then
              lFileName := FAppModules.Language.GetString('Rainfall.COutFile')
            else
              lFileName := Trim(ClassROutputFileEdit.Text);
            lPatch.ClassROutputFileName := lFileName;
            ClassROutputFileEdit.Text  := lFileName;

            Result := (FAppModules.Model.ModelData as IRainfallModelData).SaveRAWDataFiles;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError (E, OPNAME) end;
end;

function TPatchRValidator.Initialise: boolean;
const OPNAME = 'TPatchRValidator.Initialise';
var
  lYear  : Word;
  lMonth : Word;
  lDay   : word;
begin
  Result := inherited initialise;
  try
    FClassRMinStartYear := FAppModules.ViewIni.ReadString ( Self.ClassName, 'ClassRStartYear',  'DefaultStart' );
    FClassRMaxEndYear   := FAppModules.ViewIni.ReadString ( Self.ClassName, 'ClassREndYear',    'DefaultEndYear' );
    FPatchRMinStartYear := FAppModules.ViewIni.ReadString ( Self.ClassName, 'PatchRStartYear',  'DefaultStart' );
    FPatchRMaxEndYear   := FAppModules.ViewIni.ReadString ( Self.ClassName, 'PatchREndYear',    'DefaultEndYear' );
    FDefaultStartYear   := FAppModules.ViewIni.ReadString ( Self.ClassName, 'DefaultStartYear', 'DefaultStart' );
    FDefaultEndYear     := FAppModules.ViewIni.ReadString ( Self.ClassName, 'DefaultEndYear',   'DefaultEndYear' );
    FClassRGaugesMax    := FAppModules.ViewIni.ReadString ( Self.ClassName, 'ClassRGaugesMax',  'DefaultMaxGauges' );
    FPatchRGaugesMax    := FAppModules.ViewIni.ReadString ( Self.ClassName, 'PatchRGaugesMax',  'DefaultMaxGauges' );

    DecodeDate(Now, lYear, lMonth, lDay);
    FDefaultEndYear := IntToStr(lYear);

    if (StrToInt(FClassRMinStartYear) > StrToInt(FDefaultStartYear)) then
      FDefaultStartYear := FClassRMinStartYear;
    if (StrToInt(FClassRMaxEndYear) < StrToInt(FDefaultEndYear)) then
      FDefaultEndYear := FClassRMaxEndYear;

    if (StrToInt(FPatchRMinStartYear) > StrToInt(FDefaultStartYear)) then
      FDefaultStartYear := FPatchRMinStartYear;
    if (StrToInt(FPatchRMaxEndYear) < StrToInt(FDefaultEndYear)) then
      FDefaultEndYear := FPatchRMaxEndYear;

    PatchRDialog.ShiftDataRadioGroup.ItemIndex := 0;
    Result := TRUE;
  except on E: Exception do HandleError (E, OPNAME) end;
end;

procedure TPatchRValidator.DoRunClassR (Sender: TObject);
const OPNAME = 'TPatchRValidator.DoRunClassR';
var
  lParamList        : TStringList;
  lBatFileStream    : TFileStream;
  lClassROutputFile : string;
  lDirectory        : string;
  lRunOption        : integer;
  lClassRDone       : boolean;
  lMessage          : string;
  lPatch            : IPatchData;
  lStation          : IStationData;
  lFileDate         : TDateTime;
  lStartupInfo     : TStartupInfo;
  lProcessInformation : TProcessInformation;
  lInProgress         : boolean;
begin
  try
    if (NOT NumberOfStationsWithinLimit('ClassR')) then
      Exit;
    if ((PatchRDialog.ClassRChangeDatesRadioGroup.ItemIndex = 1) AND
        (NOT StartEndYearInputWithinLimits
             ('ClassR', PatchRDialog.ClassRFirstYearComboBox.Text, PatchRDialog.ClassRLastYearComboBox.Text,
              FClassRMinStartYear, FClassRMaxEndYear))) then
      Exit;
    lClassRDone := False;
    lStation := (FAppModules.Model.ModelData as IRainfallModelData).GetStationDataByID(FStationID);
    if (lStation <> nil) then
    begin
      lPatch := lStation.GetPatchWithID(FPatchID);
      if (lPatch <> nil) then
      begin
        lRunOption := StrToInt(Trim((Sender as TButton).Caption));
        lDirectory := (FAppModules.Model.ModelData as IRainfallModelData).DefaultDir;
        if (NOT CreateClassRInputFiles) then
        begin
          lMessage := FAppModules.Language.GetString('Rainfall.StationOrPatchHasNoData');
          ShowMessage(lMessage);
        end
        else
        begin
          CreateClassRParamFile(lDirectory, lRunOption);
          lParamList := TStringList.Create;
          lBatFileStream := TFileStream.Create(lDirectory + 'Input.bat', fmCreate);
          try
            //lParamList.Add('@Classr.exe < InputFile.txt');
            lParamList.Add('@CLASSR_2013d.exe < InputFile.txt');
            lParamList.Add(' ');
            lParamList.SaveToStream(lBatFileStream);
          finally
            FreeAndNil(lParamList);
            FreeAndNil(lBatFileStream);
          end;

          lClassROutputFile := lDirectory + PatchRDialog.ClassROutputFileEdit.Text;
          if (FileExists(lClassROutputFile)) then
            DeleteFile(PChar(lClassROutputFile));

          lParamList := TStringList.Create;
          lBatFileStream := TFileStream.Create(lDirectory + 'Output.bat', fmCreate);
          try
            lParamList.Add('Input.bat > RunClassR');
            lParamList.SaveToStream ( lBatFileStream );
          finally
            FreeAndNil(lParamList );
            FreeAndNil(lBatFileStream );
          end;
          //TShellExecuteObject.ExecuteShellAction('Open', 'Output.bat', '', pChar(lDirectory));

            FillChar(lStartupInfo, SizeOf(lStartupInfo), 0);
            lStartupInfo.cb := SizeOf(lStartupInfo);
            lInProgress := CreateProcess(nil, PChar(lDirectory + 'Output.bat' ), nil,
                                         nil, True, CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS,
                                         nil, PChar(lDirectory), lStartupInfo, lProcessInformation);

           if (lInProgress) then
           begin
             Screen.Cursor := crHourGlass;
             WaitForSingleObject(lProcessInformation.hProcess, INFINITE);
             CloseHandle(lProcessInformation.hThread);
             CloseHandle(lProcessInformation.hProcess);
             lClassRDone := True;
           end;
          FileReadyToBeAccessed(lClassROutputFile);

          if (FileExists(lClassROutputFile)) then
          begin
            //lFileDate := FileDateToDateTime(FileAge(lClassROutputFile));

            FileAge(lClassROutputFile,lFileDate);
            lPatch.ClassRDate := lFileDate;
            lParamList := TStringList.Create;
            try
              if (lRunOption < 2) AND (FileExists(lDirectory + 'RunClassR')) then
              begin
                if (FileExists(lClassROutputFile)) then
                  DeleteFile(PChar(lClassROutputFile));
                lParamList.LoadFromFile(lDirectory + 'RunClassR');
                lPatch.ClassROutputData := lParamList.Text;
                lParamList.SaveToFile(lClassROutputFile);
                FileSetDate(lClassROutputFile, DateTimeToFileDate(lFileDate));
              end
              else
              begin
                lParamList.LoadFromFile(lClassROutputFile);
              end;
            finally
              FreeAndNil(lParamList);
            end;
          end;
          Screen.Cursor := crDefault;

          if (lClassRDone) then
            lMessage := ''
          else
            lMessage := FAppModules.Language.GetString('Rainfall.ErrorRunningClassR') + #13#10;

          lMessage := lMessage + FAppModules.Language.GetString('Rainfall.ViewClassROutputInNotePad');
          if (MessageDlg(lMessage, mtConfirmation, [ mbYes, mbNo ], 0 ) = mrYes) then
            ShowRunProcess( lDirectory + 'RunClassR' );

          PopulateClassRInfo;
          PopulatePatchRInput;
          PopulateRunPatchR;
        end;
      end;
    end;
  except on E: Exception do HandleError (E, OPNAME) end;
end;

procedure TPatchRValidator.ChangeClassRPeriod (Sender : TObject);
const OPNAME = 'TPatchRValidator.ChangeClassRPeriod';
begin
  try
    with PatchRDialog do
    begin
      if (ClassRChangeDatesRadioGroup.ItemIndex = 1) then
      begin
        ClassRFirstYearLabel.Enabled    := TRUE;
        ClassRFirstYearComboBox.Enabled := TRUE;
        ClassRLastYearLabel.Enabled     := TRUE;
        ClassRLastYearComboBox.Enabled  := TRUE;
        if (ClassRFirstYearComboBox.Text = '') then
        begin
          if (StrToInt(FDefaultStartYear) < FClassRStartYear) then
            ClassRFirstYearComboBox.ItemIndex := ClassRFirstYearComboBox.Items.IndexOf(IntToStr(FClassRStartYear))
          else
            ClassRFirstYearComboBox.ItemIndex := ClassRFirstYearComboBox.Items.IndexOf(FDefaultStartYear);
        end;
        if (ClassRLastYearComboBox.Text = '') then
        begin
          if (StrToInt(FDefaultEndYear) > FClassREndYear) then
            ClassRLastYearComboBox.ItemIndex := ClassRLastYearComboBox.Items.IndexOf(IntToStr(FClassREndYear))
          else
            ClassRLastYearComboBox.ItemIndex := ClassRLastYearComboBox.Items.IndexOf(FDefaultEndYear);
        end;
      end
      else
      begin
        ClassRFirstYearLabel.Enabled    := FALSE;
        ClassRFirstYearComboBox.Enabled := FALSE;
        ClassRLastYearLabel.Enabled     := FALSE;
        ClassRLastYearComboBox.Enabled  := FALSE;
        ClassRFirstYearComboBox.ItemIndex := -1;
        ClassRFirstYearComboBox.Text      := '';
        ClassRLastYearComboBox.ItemIndex  := -1;
        ClassRLastYearComboBox.Text       := '';
      end;
    end;
  except on E: Exception do HandleError (E, OPNAME) end;
end;

function TPatchRValidator.StudyHasChanged: boolean;
const OPNAME = 'TPatchRValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    PopulateDataViewer;
    Result := TRUE;
  except on E: Exception do HandleError (E, OPNAME) end;
end;

function TPatchRValidator.StudyDataHasChanged (AContext   : TChangeContext;
                                               AFieldName : string;
                                               AOldValue  : string;
                                               ANewValue  : string): Boolean;
const OPNAME = 'TPatchRValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext, AFieldName, AOldValue, ANewValue);
  try
    PopulateDataViewer;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPatchRValidator.PopulatePatchInfo;
const OPNAME = 'TPatchRValidator.PopulatePatchInfo';
var
  lIndex           : integer;
  lSourceStationID : integer;
  lSourcePatchID   : integer;
  lStation         : IStationData;
  lPatch           : IPatchData;
  lSourceStation   : IStationData;
  lSourcePatch     : IPatchData;
  lTargetStation   : WideString;
  lStartYear       : integer;
  lEndYear         : integer;
  lTempStr         : string;
  lMessage         : string;
begin
  try
    with PatchRDialog do
    begin
      PatchSourcesListBox.Items.Clear;
      TargetStationValue.Caption := '';

      if ((FStationID <> 0) AND (FSplitIndex <> -1) AND (FPatchID <> 0)) then
      begin
        lStation := (FAppModules.Model.ModelData as IRainfallModelData).GetStationDataByID(FStationID);
        if (lStation <> nil) then
        begin
          lPatch := lStation.GetPatchWithID(FPatchID);

          ClassRChangeDatesRadioGroup.ItemIndex := 0;
          FClassRStartYear := 9999;
          FClassREndYear   := 0;
          FPatchRStartYear := 9999;
          FPatchREndYear   := 0;
          PatchInfoGroupBox.Caption := FAppModules.Language.GetString('Rainfall.PatchInformation') +
                                       ' - ' + lPatch.PatchName + ' ';
          for lIndex := 0 to lPatch.SourcesCount - 1 do
          begin
            lPatch.GetSourceInfoByIndex(lIndex, lSourceStationID, lSourcePatchID,
                                        lTargetStation, lStartYear, lEndYear);
            lSourceStation := (FAppModules.Model.ModelData as IRainfallModelData).GetStationDataByID(lSourceStationID);
            if (lSourceStation <> nil) then
            begin
              if (lStartYear = 0) AND (lEndYear = 0) then
              begin
                if (lSourceStation.RainfallData.HydroStartYear < FClassRStartYear) then
                  FClassRStartYear := lSourceStation.RainfallData.HydroStartYear;
                if (lSourceStation.RainfallData.HydroEndYear > FClassREndYear) then
                  FClassREndYear := lSourceStation.RainfallData.HydroEndYear;
                if (lSourceStation.RainfallData.HydroStartYear < FPatchRStartYear) then
                  FPatchRStartYear := lSourceStation.RainfallData.HydroStartYear;
                if (lSourceStation.RainfallData.HydroEndYear > FPatchREndYear) then
                  FPatchREndYear := lSourceStation.RainfallData.HydroEndYear;
                lTempStr := lSourceStation.RainfallData.StationNumber + ' (' +
                            IntToStr(lSourceStation.RainfallData.HydroStartYear) + '-' +
                            IntToStr(lSourceStation.RainfallData.HydroEndYear) + ')';
              end
              else
              begin
                if (lStartYear < FClassRStartYear) then
                  FClassRStartYear := lStartYear;
                if (lEndYear > FClassREndYear) then
                  FClassREndYear := lEndYear;
                if (lStartYear < FPatchRStartYear) then
                  FPatchRStartYear := lStartYear;
                if (lEndYear > FPatchREndYear) then
                  FPatchREndYear := lEndYear;
                lTempStr := lSourceStation.RainfallData.StationNumber + ' (' +
                            IntToStr(lStartYear) + '-' + IntToStr(lEndYear) + ')';
              end;
              if (lTargetStation = 'Y') then
                TargetStationValue.Caption := lTempStr
              else if (lSourcePatchID = 0) then
                PatchSourcesListBox.Items.Add(lTempStr)
              else
              begin
                lSourcePatch := lSourceStation.GetPatchWithID(lSourcePatchID);
                if (lSourcePatch <> nil) then
                  PatchSourcesListBox.Items.Add(lSourceStation.RainfallData.StationNumber + ' - ' + lSourcePatch.PatchName);
              end;
            end;
          end;

          if (FClassRStartYear < StrToInt(FClassRMinStartYear)) then
            FClassRStartYear := STrToInt(FClassRMinStartYear);
          if (FClassREndYear > StrToInt(FClassRMaxEndYear)) then
            FClassREndYear := STrToInt(FClassRMaxEndYear);

          lMessage := FAppModules.Language.GetString('Rainfall.ClassRDefaultStartEnd');
          ClassRCurrentDatesLabel.Caption  := Format(lMessage, [FClassRStartYear, FClassREndYear]);
          ClassRFirstYearComboBox.Items.Clear;
          ClassRLastYearComboBox.Items.Clear;
          for lIndex := FClassRStartYear to FClassREndYear do
          begin
            ClassRFirstYearComboBox.Items.Add(IntToStr(lIndex));
            ClassRLastYearComboBox.Items.Add(IntToStr(lIndex));
          end;

          if (FPatchRStartYear < StrToInt(FPatchRMinStartYear)) then
            FPatchRStartYear := STrToInt(FPatchRMinStartYear);
          if (FPatchREndYear > StrToInt(FPatchRMaxEndYear)) then
            FPatchREndYear := STrToInt(FPatchRMaxEndYear);

          PatchRFirstYearComboBox.Items.Clear;
          PatchRLastYearComboBox.Items.Clear;
          for lIndex := FPatchRStartYear to FPatchREndYear do
          begin
            PatchRFirstYearComboBox.Items.Add(IntToStr(lIndex));
            PatchRLastYearComboBox.Items.Add(IntToStr(lIndex));
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError (E, OPNAME) end;
end;

procedure TPatchRValidator.PopulateClassRInfo;
const OPNAME = 'TPatchRValidator.PopulateClassRInfo';
var
  lDirectory : string;
  lFileName  : string;
  lFileDate  : TDateTime;
  lData      : TStringList;
  lDataList  : TStringList;
  lStation   : IStationData;
  lPatch     : IPatchData;
begin
  try
    with PatchRDialog do
    begin
      ClassROption0Button.Enabled := FALSE;
      ClassROption1Button.Enabled := FALSE;
      ClassROption2ChkBox.Enabled := FALSE;
      ClassROption3Button.Enabled := FALSE;
      ClassROption4Button.Enabled := FALSE;
      ClassROption5Button.Enabled := FALSE;
      if ((FStationID <> 0) AND (FSplitIndex <> -1) AND (FPatchID <> 0)) then
      begin
        lStation := (FAppModules.Model.ModelData as IRainfallModelData).GetStationDataByID(FStationID);
        if (lStation <> nil) then
        begin
          lPatch := lStation.GetPatchWithID(FPatchID);
          if (lPatch <> nil) then
          begin
            ClassRGroupBox.Enabled    := lPatch.SourcesCount > 0;
            ClassRInputFileEdit.Text  := lPatch.ClassRInputFileName;
            ClassROutputFileEdit.Text := lPatch.ClassROutputFileName;
            ClassRHeaderEdit.Text     := DateTimeToStr(Now);

            ClassRInputFileEdit.Color  := clWhite;
            ClassROutputFileEdit.Color := clWhite;
            if ((lPatch.ClassRDate > 0) AND (lPatch.ChangeDate > 0)) then
            begin
              if (lPatch.ChangeDate > lPatch.ClassRDate) then
              begin
                ClassRInputFileEdit.Color  := clRed;
                ClassROutputFileEdit.Color := clRed;
              end
              else
              begin
                lDirectory := (FAppModules.Model.ModelData as IRainfallModelData).DefaultDir;
                lFileName  := lPatch.ClassRInputFileName;
                if (Trim(lFileName) <> '') then
                begin
                  if (FileExists(lDirectory + lFileName)) then
                  begin

                    //lFileDate  := FileDateToDateTime(FileAge(lDirectory + lFileName));
                    FileAge(lDirectory + lFileName, lFileDate);
                    if (ChopSeconds(lFileDate) > ChopSeconds(lPatch.ClassRDate)) then
                      ClassRInputFileEdit.Color := clYellow;
                  end
                  else
                    ClassRInputFileEdit.Color := clAqua;
                end;
                lFileName  := lPatch.ClassROutputFileName;
                if (Trim(lFileName) <> '') Then
                begin
                  if (FileExists(lDirectory + lFileName)) then
                  begin

                    //lFileDate  := FileDateToDateTime(FileAge(lDirectory + lFileName));

                    FileAge(lDirectory + lFileName, lFileDate);
                    if (ChopSeconds(lFileDate) > ChopSeconds(lPatch.ClassRDate)) then
                      ClassROutputFileEdit.Color := clYellow;
                  end
                  else
                    ClassROutputFileEdit.Color := clAqua;
                end;
              end;
            end;

            lData     := TStringList.Create;
            lDataList := TStringList.Create;
            try
              lData.CommaText := lPatch.ClassRInputData;
              ClassRViewInputFileButton.Enabled  := (lData <> nil) AND (lData.Count > 0);
              if FileExists(lDirectory + lPatch.ClassROutputFileName) then
                lDataList.LoadFromFile(lDirectory + lPatch.ClassROutputFileName);
              ClassRViewOutputFileButton.Enabled := (lDataList <> nil) AND (lDataList.Count > 0);
            finally
              FreeAndNil(lDataList);
              FreeAndNil(lData);
            end;
            if (ClassRGroupBox.Enabled) then
            begin
              ClassROption0Button.Enabled := TRUE;
              ClassROption1Button.Enabled := TRUE;
              ClassROption2ChkBox.Enabled := TRUE;
              ClassROption3Button.Enabled := TRUE;
              ClassROption4Button.Enabled := TRUE;
              ClassROption5Button.Enabled := TRUE;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError (E, OPNAME) end;
end;

procedure TPatchRValidator.PopulatePatchRInput;
const OPNAME = 'TPatchRValidator.PopulatePatchRInput';
var
  lDataList     : TStringList;
  lDirectory    : string;
  lFileName     : string;
  lFileDate     : TDateTime;
  lColIndex     : integer;
  lRowIndex     : integer;
  lNrOfSeasons  : integer;
  lStation      : IStationData;
  lPatch        : IPatchData;
begin
  try
    FPatchRInputFileOK := FALSE;
    if ((FStationID <> 0) AND (FSplitIndex <> -1) AND (FPatchID <> 0)) then
    begin
      lStation := (FAppModules.Model.ModelData as IRainfallModelData).GetStationDataByID(FStationID);
      if (lStation <> nil) then
      begin
        lPatch := lStation.GetPatchWithID(FPatchID);
        if (lPatch <> nil) then
        begin
          with PatchRDialog do
          begin
            PatchRGroupBox.Enabled := (lPatch.SourcesCount > 0) AND (lPatch.ClassRDate > 0);

            PatchRInputFileEdit.Text := lPatch.PatchRInputFileName;
            lDirectory := (FAppModules.Model.ModelData as IRainfallModelData).DefaultDir;

            PatchRInputFileEdit.Color  := clWhite;
            if ((lPatch.PatchRInputDate > 0) AND (lPatch.ChangeDate > 0)) then
            begin
              if (lPatch.ChangeDate > lPatch.PatchRInputDate) then
                PatchRInputFileEdit.Color := clRed
              else
              begin
                lFileName  := lPatch.PatchRInputFileName;
                if (Trim(lFileName) <> '') then
                begin
                  if (FileExists(lDirectory + lFileName)) then
                  begin
                    //lFileDate  := FileDateToDateTime(FileAge(lDirectory + lFileName));
                    FileAge(lDirectory + lFileName, lFileDate);
                    if (ChopSeconds(lFileDate) > ChopSeconds(lPatch.PatchRInputDate)) then
                      PatchRInputFileEdit.Color  := clYellow;
                  end
                  else
                    PatchRInputFileEdit.Color  := clAqua;
                end;
              end;
              FPatchRInputFileOK := (PatchRInputFileEdit.Color  = clWhite);
            end;

            lDataList := TStringList.Create;
            try
              lDataList.Text := lPatch.PatchRInputData;
              PatchRViewInputFileButton.Enabled  := (lDataList.Count > 0);
              if (lDataList.Count > 0) then
              begin
                lNrOfSeasons := StrToInt(Trim(Copy(lDataList[0], 3, 2)));
                NoOfSeasonsEdit.Value := lNrOfSeasons;
                SetSeasonsGrid(Self);
                if (lDataList.Count > lNrOfSeasons) then
                begin
                  lDataList.Delete(0);
                  lDataList.Delete(0);
                end;
                for lColIndex := 1 to SeasonsGrid.ColCount - 1 do
                  for lRowIndex := 1 to SeasonsGrid.RowCount - 1 do
                    SeasonsGrid.Cells[lColIndex, lRowIndex] := '';
                for lColIndex := 1 to lNrOfSeasons do
                  DoSeasonsStr(lDataList[lColIndex - 1], lColIndex);
              end
              else
              begin
                NoOfSeasonsEdit.Value := 2;
                SetSeasonsGrid(Self);
              end;
            finally
              FreeAndNil(lDataList);
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TPatchRValidator.PatchMultipleRadioGroupChanged (Sender: TObject);
const OPNAME = 'TPatchRValidator.PatchMultipleRadioGroupChanged';
begin
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TPatchRValidator.PatchROutputFileComboBoxChanged (Sender : TObject);
const OPNAME = 'TPatchRValidator.PatchROutputFileComboBoxChanged';
begin
  try
    ResetPatchROutputFilesCbx;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TPatchRValidator.ResetPatchROutputFilesCbx;
const OPNAME = 'TPatchRValidator.ResetPatchROutputFilesCbx';
var
  lDirectory : string;
  lFileName  : string;
  lFileDate  : TDateTime;
  lDataList  : TStringList;
  lStation   : IStationData;
  lPatch     : IPatchData;
begin
  try
    if ((FStationID <> 0) AND (FSplitIndex <> -1) AND (FPatchID <> 0)) then
    begin
      lStation := (FAppModules.Model.ModelData as IRainfallModelData).GetStationDataByID(FStationID);
      if (lStation <> nil) then
      begin
        lPatch := lStation.GetPatchWithID(FPatchID);
        if (lPatch <> nil) then
        begin
          PatchRDialog.PatchROutputFileNameComboBox.Color  := clWhite;
          if ((lPatch.PatchROutputDate > 0) AND (lPatch.ChangeDate > 0)) then
          begin
            lDirectory := (FAppModules.Model.ModelData as IRainfallModelData).DefaultDir;
            if (lPatch.ChangeDate > lPatch.PatchROutputDate) then
              PatchRDialog.PatchROutputFileNameComboBox.Color := clRed
            else
            begin
              lFileName := PatchRDialog.PatchROutputFileNameComboBox.Text;
              if (Trim(lFileName) <> '') then
              begin
                if (FileExists(lDirectory + lFileName)) then
                begin
                  //lFileDate := FileDateToDateTime(FileAge(lDirectory + lFileName));
                  FileAge(lDirectory + lFileName,lFileDate);
                  if (ChopSeconds(lFileDate) > ChopSeconds(lPatch.PatchROutputDate)) then
                    PatchRDialog.PatchROutputFileNameComboBox.Color  := clYellow;
                end
                else
                  PatchRDialog.PatchROutputFileNameComboBox.Color  := clAqua;
              end;
            end;
          end;
          lDataList := TStringList.Create;
          try
            lDataList.Text := lPatch.PatchROutputData;
            PatchRDialog.PatchRViewOutputFileButton.Enabled  := (lDataList.Count > 0);
          finally
            FreeAndNil(lDataList);
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TPatchRValidator.PopulatePatchROutputFiles;
const OPNAME = 'TPatchRValidator.PopulatePatchROutputFiles';
var
  lDirectory       : string;
  lFileName        : string;
  lIndex           : integer;
  lSourceStationID : integer;
  lSourcePatchID   : integer;
  lSourceStation   : IStationData;
  lTargetStation   : WideString;
  lStation         : IStationData;
  lPatch           : IPatchData;
  lStartYear       : integer;
  lEndYear         : integer;
begin
  try
    with PatchRDialog do
    begin
      PatchROutputFileNameComboBox.Items.Clear;
      if ((FStationID <> 0) AND (FSplitIndex <> -1) AND (FPatchID <> 0)) then
      begin
        lDirectory := (FAppModules.Model.ModelData as IRainfallModelData).DefaultDir;
        lStation   := (FAppModules.Model.ModelData as IRainfallModelData).GetStationDataByID(FStationID);
        if (lStation <> nil) then
        begin
          lPatch := lStation.GetPatchWithID(FPatchID);
          if (lPatch <> nil) then
          begin

            for lIndex := 0 to lPatch.SourcesCount - 1 do
            begin
              lPatch.GetSourceInfoByIndex(lIndex, lSourceStationID, lSourcePatchID,
                                          lTargetStation, lStartYear, lEndYear);
              if (lPatch.PatchMultiple OR (lTargetStation = 'Y')) then
              begin
                lSourceStation := (FAppModules.Model.ModelData as IRainfallModelData).GetStationDataByID(lSourceStationID);
                if (lSourceStation <> nil) then
                begin
                  lFileName := lSourceStation.RainfallData.StationNumber;
                  while (Pos(' ', lFileName ) > 0) do
                    Delete(lFileName, Pos(' ', lFileName), 1);
                  lFileName := Copy(lFileName, 1, 8) + '.PAT';
                  PatchROutputFileNameComboBox.Items.AddObject(lFileName, TObject(lSourceStation.RainfallData.StationID));
                end;
              end;
            end;

            PatchROutputFileNameComboBox.ItemIndex := 0;
            ResetPatchROutputFilesCbx;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError (E, OPNAME) end;
end;

procedure TPatchRValidator.PopulateRunPatchR;
const OPNAME = 'TPatchRValidator.PopulateRunPatchR';
var
  lDirectory : string;
  lFileName  : string;
  lFileDate  : TDateTime;
  lDataList  : TStringList;
  lStation   : IStationData;
  lPatch     : IPatchData;
begin
  try
    with PatchRDialog do
    begin
      PatchRRunOption0Button.Enabled := FALSE;
      PatchRRunOption1Button.Enabled := FALSE;
      PatchRRunOption2Button.Enabled := FALSE;
      PatchRRunOption3Button.Enabled := FALSE;
      PatchRRunOption4Button.Enabled := FALSE;
      if ((FStationID <> 0) AND (FSplitIndex <> -1) AND (FPatchID <> 0)) then
      begin
        lStation := (FAppModules.Model.ModelData as IRainfallModelData).GetStationDataByID(FStationID);
        if (lStation <> nil) then
        begin
          lPatch := lStation.GetPatchWithID(FPatchID);
          if (lPatch <> nil) then
          begin
            RunPatchRGroupBox.Enabled := FALSE;
            RAWFilesDoesNotExistLabel.Visible        := FALSE;
            PatchRInputFileDoesNotExistLabel.Visible := FALSE;
            PatchNotRunYetLabel.Visible              := FALSE;
            ClassRNotRunYetLabel.Visible             := FALSE;
            NoSourcesLabel.Visible                   := FALSE;
            if (lPatch.SourcesCount = 0) then
              NoSourcesLabel.Visible := TRUE
            else if (lPatch.ClassRDate = 0) then
              ClassRNotRunYetLabel.Visible := TRUE
            else if (NOT RAWFilesExist) then
              RAWFilesDoesNotExistLabel.Visible := TRUE
            else if (NOT FPatchRInputFileOK) then
              PatchRInputFileDoesNotExistLabel.Visible := TRUE
            else
            begin
              RunPatchRGroupBox.Enabled := TRUE;
              PatchRRunOption0Button.Enabled := TRUE;
              PatchRRunOption1Button.Enabled := TRUE;
              PatchRRunOption2Button.Enabled := TRUE;
              PatchRRunOption3Button.Enabled := TRUE;
              PatchRRunOption4Button.Enabled := TRUE;
              if (lPatch.PatchROutputDate = 0) then
                PatchNotRunYetLabel.Visible := TRUE;
            end;
            if ((lPatch.PatchStartYear <> 0) AND (lPatch.PatchEndYear <> 0)) then
            begin
              PatchRPeriodRadioGroup.ItemIndex := 1;
              PatchRFirstYearComboBox.ItemIndex := PatchRFirstYearComboBox.Items.IndexOf(IntToStr(lPatch.PatchStartYear));
              PatchRLastYearComboBox.ItemIndex  := PatchRLastYearComboBox.Items.IndexOf(IntToStr(lPatch.PatchEndYear));
            end;

            PatchRHeaderEdit.Text := DateTimeToStr(Now);
            lDirectory := (FAppModules.Model.ModelData as IRainfallModelData).DefaultDir;

            if (lPatch.PatchMultiple) then
              PatchMultipleRadioGroup.ItemIndex := 1
            else
              PatchMultipleRadioGroup.ItemIndex := 0;
            PopulatePatchROutputFiles;

            lDataList := TStringList.Create;
            try
              if FileExists(lDirectory + lPatch.PatchRPrintFileName) then
                lDataList.LoadFromFile(lDirectory + lPatch.PatchRPrintFileName );
              PatchRViewPrintFileButton.Enabled  := (lDataList.Count > 0);
              PatchRPrintFileNameEdit.Text  := lPatch.PatchRPrintFileName;
              PatchRPrintFileNameEdit.Color := clWhite;
              if ((lPatch.PatchRPrintDate > 0) AND (lPatch.ChangeDate > 0)) then
              begin
                if (lPatch.ChangeDate > lPatch.PatchRPrintDate) then
                  PatchRPrintFileNameEdit.Color := clRed
                else
                begin
                  lFileName := lPatch.PatchRPrintFileName;
                  if (Trim(lFileName) <> '') then
                  begin
                    if (FileExists(lDirectory + lFileName)) then
                    begin
                      //lFileDate := FileDateToDateTime(FileAge(lDirectory + lFileName));
                      FileAge(lDirectory + lFileName,lFileDate);
                      if (ChopSeconds(lFileDate) > ChopSeconds(lPatch.PatchRPrintDate)) then
                        PatchRPrintFileNameEdit.Color  := clYellow;
                    end
                    else
                      PatchRPrintFileNameEdit.Color  := clAqua;
                  end;
                end;
              end;
              if FileExists(lDirectory + lPatch.PatchRPlotFileName) then
                lDataList.LoadFromFile(lDirectory + lPatch.PatchRPlotFileName );
              PatchRViewPlotFileButton.Enabled  := (lDataList.Count > 0);
              PatchRPlotFileNameEdit.Text  := lPatch.PatchRPlotFileName;
              PatchRPlotFileNameEdit.Color := clWhite;
              if ((lPatch.PatchRPlotDate > 0) AND (lPatch.ChangeDate > 0)) then
              begin
                if (lPatch.ChangeDate > lPatch.PatchRPlotDate) then
                  PatchRPlotFileNameEdit.Color := clRed
                else
                begin
                  lFileName := lPatch.PatchRPlotFileName;
                  if (Trim(lFileName) <> '') then
                  begin
                    if (FileExists(lDirectory + lFileName)) then
                    begin
                      //lFileDate := FileDateToDateTime(FileAge(lDirectory + lFileName));
                      FileAge(lDirectory + lFileName,lFileDate);
                      if (ChopSeconds(lFileDate) > ChopSeconds(lPatch.PatchRPlotDate)) then
                        PatchRPlotFileNameEdit.Color  := clYellow;
                    end
                    else
                      PatchRPlotFileNameEdit.Color  := clAqua;
                  end;
                end;
              end;
            finally
              FreeAndNil(lDataList);
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError (E, OPNAME) end;
end;

procedure TPatchRValidator.ViewTextData (AFileName : string;
                                        AFileDate : TDateTime;
                                        AText     : TStringList);
const OPNAME = 'TPatchRValidator.ViewTextData';
var
  lFileStream : TFileStream;
  lDirectory  : string;
begin
  try
    lDirectory := (FAppModules.Model.ModelData as IRainfallModelData).DefaultDir;
    if (FileExists(lDirectory + AFileName)) then
      DeleteFile(PChar(lDirectory + AFileName));
    lFileStream := TFileStream.Create(lDirectory + AFileName, fmCreate);
    try
      AText.SaveToStream(lFileStream);
    finally
      FreeAndNil(lFileStream);
    end;
    FileSetDate(lDirectory + AFileName, DateTimeToFileDate(AFileDate));
    ShellExecute(Application.Handle, 'open', 'notepad.exe' , PChar(AFileName),
                 PChar(lDirectory), SW_SHOW);
  except on E: Exception do HandleError (E, OPNAME) end;
end;

procedure TPatchRValidator.ViewClassRInput (Sender : TObject);
const OPNAME = 'TPatchRValidator.ViewClassRInput';
var
  lText    : TStringList;
  lStation : IStationData;
  lPatch   : IPatchData;
begin
  try
    if ((FStationID <> 0) AND (FSplitIndex <> -1) AND (FPatchID <> 0)) then
    begin
      lStation := (FAppModules.Model.ModelData as IRainfallModelData).GetStationDataByID(FStationID);
      if (lStation <> nil) then
      begin
        lPatch := lStation.GetPatchWithID(FPatchID);
        if (lPatch <> nil) then
        begin
          lText := TStringList.Create;
          try
            lText.CommaText := lPatch.ClassRInputData;
            ViewTextData(lPatch.ClassRInputFileName, lPatch.ClassRDate, lText);
          finally
            FreeAndNil(lText);
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError (E, OPNAME) end;
end;

procedure TPatchRValidator.ViewClassROutput (Sender : TObject);
const OPNAME = 'TPatchRValidator.ViewClassROutput';
var
  lText    : TStringList;
  lStation : IStationData;
  lPatch   : IPatchData;
  lDirectory: string;
begin
  try
    if ((FStationID <> 0) AND (FSplitIndex <> -1) AND (FPatchID <> 0)) then
    begin
      lStation := (FAppModules.Model.ModelData as IRainfallModelData).GetStationDataByID(FStationID);
      if (lStation <> nil) then
      begin
        lPatch := lStation.GetPatchWithID(FPatchID);
        lDirectory := (FAppModules.Model.ModelData as IRainfallModelData).DefaultDir;
        if (lPatch <> nil) then
        begin
          lText := TStringList.Create;
          try
            if FileExists(lDirectory + lPatch.ClassROutputFileName) then
              lText.LoadFromFile(lDirectory + lPatch.ClassROutputFileName);
            ViewTextData(lPatch.ClassROutputFileName, lPatch.ClassRDate, lText);
          finally
            FreeAndNil(lText);
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError (E, OPNAME) end;
end;

procedure TPatchRValidator.DoHeaderChange (Sender : TObject);
const OPNAME = 'TPatchRValidator.DoHeaderChange';
begin
  try
    CheckNumberOfChar (40, PatchRDialog.ClassRHeaderEdit.Text)
  except on E: Exception do HandleError (E, OPNAME) end;
end;

function TPatchRValidator.CheckNumberOfChar (aNum: integer; aValue : string): boolean;
const OPNAME = 'TPatchRValidator.CheckNumberOfChar';
begin
  Result := False;
  try
    if Length (aValue) > aNum then
      raise Exception.Create ('Entered value exceed the limit');
    Result := True;
  except on E: Exception do HandleError (E, OPNAME) end;
end;

function TPatchRValidator.CheckPathDosCompatibilty (aValue : string) : boolean;
const OPNAME = 'TfrmStudyEditForm.CheckPathDosCompatibilty';
begin
  Result := False;
  try
    Result := FilePathIsDosCompatible (FAppModules, aValue);
    if not Result then
      ShowMessage (FAppModules.Language.GetString ('FilePath.PathNotDosCompatible'));
  except on E: Exception do HandleError (E, OPNAME) end;
end;

procedure TPatchRValidator.DoClassRInputFileChange (Sender : TObject);
const OPNAME = 'TPatchRValidator.DoClassRInputFileChange';
begin
  try
    CheckPathDosCompatibilty(PatchRDialog.ClassRInputFileEdit.Text);
  except on E: Exception do HandleError (E, OPNAME) end;
end;

procedure TPatchRValidator.DoClassROutputFileChange (Sender : TObject);
const OPNAME = 'TPatchRValidator.DoClassROutputFileChange';
begin
  try
    CheckPathDosCompatibilty (PatchRDialog.ClassROutputFileEdit.Text);
  except on E: Exception do HandleError (E, OPNAME) end;
end;

procedure TPatchRValidator.CreateClassRParamFile (ADefaultDir : string;
                                                  ARunOption  : integer);
const OPNAME = 'TPatchRValidator.CreateClassRParamFile';
var
  lFileStream  : TFileStream;
  lParamList   : TStringList;
begin
  try
    with PatchRDialog do
    begin
      lFileStream := TFileStream.Create(aDefaultDir + FAppModules.Language.GetString('Rainfall.InputFile'), fmCreate);
      lParamList := TStringList.Create;
      try
        lParamList.Add(IntToStr(ShiftDataRadioGroup.ItemIndex));
        lParamList.Add(ClassRInputFileEdit.Text);
        lParamList.Add(ClassRHeaderEdit.Text);
        lParamList.Add(ClassROutputFileEdit.Text);

        if (aRunOption = 0) then
          lParamList.Add(IntToStr(aRunOption))
        else
        if (aRunOption = 1) then
        begin
          lParamList.Add(IntToStr(aRunOption));
          lParamList.Add(IntToStr(VK_RETURN));
          lParamList.Add(IntToStr(VK_RETURN));
        end
        else
        if (ARunOption > 1) then {3,4,5}
        begin
          if (ClassROption2ChkBox.Checked) then
          begin
            lParamList.Add('2');
            lParamList.Add(IntToStr(ClassRChangeDatesRadioGroup.ItemIndex));
            if (ClassRChangeDatesRadioGroup.ItemIndex = 1) then
            begin
              lParamList.Add(ClassRFirstYearComboBox.Text);
              lParamList.Add(ClassRLastYearComboBox.Text);
            end;
          end;
          lParamList.Add(IntToStr(aRunOption));
          lParamList.Add(IntToStr(ClassRChangeDatesRadioGroup.ItemIndex));
          if (ClassRChangeDatesRadioGroup.ItemIndex = 1) then
          begin
            lParamList.Add(ClassRFirstYearComboBox.Text);
            lParamList.Add(ClassRLastYearComboBox.Text);
          end;
        end;

        lParamList.Add('6');

        lParamList.SaveToStream (lFileStream);
        lParamList.Clear;
      finally
        FreeAndNil (lFileStream);
        FreeAndNil (lParamList);
      end;
    end;
  except on E: Exception do HandleError (E, OPNAME) end;
end;

function TPatchRValidator.FileReadyToBeAccessed (aFile : string): boolean;
const OPNAME = 'TPatchRValidator.FileReadyToBeAccessed';
var
  lFileHandle : integer;
  lMessage    : string;
begin
  Result := False;
  try
    while (NOT Result) do
    begin
      lFileHandle := FileOpen(aFile, fmOpenWrite or fmShareDenyNone);
      if (lFileHandle >= 0) then
        Result := True
      else
      begin
        lMessage := FAppModules.Language.GetString('Rainfall.BusyWritingResultFile') + #13#10 +
                    FAppModules.Language.GetString('Rainfall.WrtingOutputClickOK') + #13#10 +
                    FAppModules.Language.GetString('Rainfall.WritingOutputClickAbort');
        if (MessageDlg(lMessage, mtConfirmation, [mbOK, mbAbort], 0) = mrAbort) then
          Result := True
        else
          Sleep(5000);
        Application.ProcessMessages;
      end;
      FileClose(lFileHandle);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TPatchRValidator.FilesReadyToBeAccessed (ADirectory : string;
                                                 AFileList  : TStringList): boolean;
const OPNAME = 'TPatchRValidator.FilesReadyToBeAccessed';
var
  lFileHandle : integer;
  lMessage    : string;
  //lAbort      : boolean;
  lFile       : string;
  lIndex      : integer;
  lFileList   : TStringList;
  //lCount      : integer;
begin
  Result := FALSE;
  try
    lFileList := TStringList.Create;
    try
      for lIndex := 0 to AFileList.Count - 1 do
        lFileList.Add(AFileList.Strings[lIndex]);
      if (lFileList.Count > 0) then
      begin
        //lCount := lCount + 1;
        for lIndex := 0 to lFileList.Count - 1 do
        begin
          lFile := ADirectory + lFileList.Strings[0];
          lFileHandle := FileOpen(lFile, fmOpenWrite or fmShareDenyNone );
          if (lFileHandle >= 0) then
            lFileList.Delete(0);
          FileClose(lFileHandle);
        end;

        if (lFileList.Count > 0) then
        begin
          lMessage := 'There is an error creating the patch, select suitable gauges and try again';
          MessageDlg(lMessage, mtConfirmation, [mbOK],0);
          Application.ProcessMessages;
        end;
      end;
      Result := TRUE;
    finally
      FreeAndNil(lFileList);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{* Patch R ********************************************************************}

procedure TPatchRValidator.DoNoOfSeasonsChanged (Sender : TObject);
const OPNAME = 'TPatchRValidator.DoNoOfSeasonsChanged';
begin
  try
    if (Trim(PatchRDialog.NoOfSeasonsEdit.Text) = '') then
      PatchRDialog.NoOfSeasonsEdit.Value := 1;
    SetSeasonsGrid(Self);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TPatchRValidator.SetSeasonsGrid (Sender : TObject);
const OPNAME = 'TPatchRValidator.SetSeasonsGrid';
var
  lColIndex : integer;
  lOldCount : integer;
begin
  try
    with PatchRDialog do
    begin
      lOldCount := SeasonsGrid.ColCount;
      SeasonsGrid.ColCount := NoOfSeasonsEdit.Value + 1;
      if (lOldCount <> SeasonsGrid.ColCount) then
      begin
        for lColIndex := 1 to SeasonsGrid.ColCount - 1 do
          SeasonsGrid.Cells[lColIndex, 0] := IntToStr(lColIndex);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TPatchRValidator.DoSeasonsStr (APatchSeasonsStr : string;
                                        ACol             : integer);
const OPNAME = 'TPatchRValidator.DoSeasonsStr';
var
  lStrLenth : integer;
  lValue    : string;
  lMonth    : integer;
begin
  try
    lValue    := APatchSeasonsStr;
    lStrLenth := Length(lValue);
    while (lStrLenth > 0) do
    begin
      if (Pos(' ', lValue) > 0) then
      begin
        lMonth := StrToInt(Trim(Copy(lValue, 1, Pos(' ',lValue))));
        PatchRDialog.SeasonsGrid.Cells[ACol, lMonth] := 'X';
        Delete(lValue, 1, Pos(' ', lValue));
      end
      else
      begin
        lMonth := StrToInt(Trim(Copy(lValue, 1, Length(lValue))));
        PatchRDialog.SeasonsGrid.Cells[ACol, lMonth] := 'X';;
        Delete(lValue, 1, Length(lValue));
      end;
      lStrLenth := Length(lValue);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TPatchRValidator.SeasonsGridClick (Sender : TObject);
const OPNAME = 'TPatchRValidator.SeasonsGridClick';
var
  lIndex   : integer;
begin
  try
    with PatchRDialog do
    begin
      for lIndex := 1 to 4 do
        SeasonsGrid.Cells[lIndex, SeasonsGrid.Row] := '';
      SeasonsGrid.Cells[SeasonsGrid.Col, SeasonsGrid.Row] := 'X';
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TPatchRValidator.ValidateSeasons : boolean;
const OPNAME = 'TPatchRValidator.ValidateSeasons';
var
  lCounter  : integer;
  lRowIndex : integer;
  lColIndex : integer;
begin
  Result := TRUE;
  try
    lRowIndex := 1;
    with PatchRDialog do
    begin
      while (Result AND (lRowIndex < SeasonsGrid.RowCount)) do
      begin
        lCounter := 0;
        for lColIndex := 1 to SeasonsGrid.ColCount - 1 do
        begin
          if (SeasonsGrid.Cells[lColIndex, lRowIndex] <> '') then
            lCounter := lCounter + 1;
        end;
        if (lCounter <> 1) then
        begin
          Result := FALSE;
          ShowMessage('Invalid number of ticks for ' + SeasonsGrid.Cells[0, lRowIndex]);
        end
        else
          lRowIndex := lRowIndex + 1;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TPatchRValidator.GetSeasons (ASeasons     : TStringList;
                                      var ALengths : string) : boolean;
const OPNAME = 'TPatchRValidator.GetSeasons';
var
  lCounter  : integer;
  lRowIndex : integer;
  lColIndex : integer;
  lTempStr  : string;
  lMonth    : integer;
begin
  Result := FALSE;
  try
    ASeasons.Clear;
    ALengths := '';
    if (ValidateSeasons) then
    begin
      with PatchRDialog do
      begin
        for lColIndex := 1 to SeasonsGrid.ColCount - 1 do
        begin
          lCounter := 0;
          lTempStr := '';
          for lRowIndex := 1 to SeasonsGrid.RowCount - 1 do
          begin
            if (SeasonsGrid.Cells[lColIndex, lRowIndex] <> '') then
            begin
               lMonth := lRowIndex;
              if (lCounter = 0) then
                lTempStr := IntToStr(lMonth)
              else
                lTempStr := lTempStr + ' ' + IntToStr(lMonth);
              inc(lCounter);
            end;
          end;
          if (lCounter > 0) then
          begin
            ASeasons.Add(lTempStr);
            if (ALengths <> '') then
              ALengths := ALengths + ' ' + IntToStr(lCounter)
            else
              ALengths := IntToStr(lCounter);
          end;
        end;
        Result := TRUE;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TPatchRValidator.RAWFilesExist : boolean;
const OPNAME = 'TPatchRValidator.RAWFilesExist';
var
  lIndex           : integer;
  lDirectory       : string;
  lFileList        : TStringList;
begin
  Result := FALSE;
  try
    lDirectory := (FAppModules.Model.ModelData as IRainfallModelData).DefaultDir;
    lFileList  := TStringList.Create;
    try
      Result := TRUE;
      GetFileNames(lFileList, '.RAW');
      lIndex := 0;
      while (Result AND (lIndex < lFileList.Count)) do
      begin
        if (NOT FileExists(lDirectory + lFileList.Strings[lIndex])) then
          Result := FALSE
        else
          lIndex := lIndex + 1;
      end;
    finally
      FreeAndNil(lFileList);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TPatchRValidator.GetFileNames (AFileList  : TStringList;
                                        AExtension : string);
const OPNAME = 'TPatchRValidator.GetFileNames';
var
  lIndex           : integer;
  lSourceStationID : integer;
  lSourcePatchID   : integer;
  lSourceStation   : IStationData;
  lFileName        : string;
  lRainfallObj     : IRainfallModelData;
  lTargetStation   : WideString;
  lStation         : IStationData;
  lPatch           : IPatchData;
  lStartYear       : integer;
  lEndYear         : integer;
begin
  try
    AFileList.Clear;
    if ((FStationID <> 0) AND (FSplitIndex <> -1) AND (FPatchID <> 0)) then
    begin
      lRainfallObj := (FAppModules.Model.ModelData as IRainfallModelData);
      lStation     := lRainfallObj.GetStationDataByID(FStationID);
      if (lStation <> nil) then
      begin
        lPatch := lStation.GetPatchWithID(FPatchID);
        if (lPatch <> nil) then
        begin
          for lIndex := 0 to lPatch.SourcesCount - 1 do
          begin
            lPatch.GetSourceInfoByIndex(lIndex, lSourceStationID, lSourcePatchID,
                                        lTargetStation, lStartYear, lEndYear);
            lSourceStation := lRainfallObj.GetStationDataByID(lSourceStationID);
            if (lSourceStation <> nil) then
            begin
              lFileName := lSourceStation.RainfallData.StationNumber;
              while (Pos(' ', lFileName) > 0) do
                Delete(lFileName, Pos(' ', lFileName), 1);
              lFileName := Copy(lFileName, 1, 8);
              if (lTargetStation = 'Y') then
                AFileList.Insert(0, lFileName + AExtension)
              else
                AFileList.Add(lFileName + AExtension);
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TPatchRValidator.CreatePatchRInputFile (Sender : TObject);
const OPNAME = 'TPatchRValidator.CreatePatchRInputFile';
var
  lSeasons             : TStringList;
  lParamList           : TStringList;
  lStationFile         : TFileStream;
  lIndex               : integer;
  lLengthOfSeason      : string;
  lDirectory           : string;
  lNrOfStations        : integer;
  lNrOfSeasons         : integer;
  lPatchRInputFileName : string;
  lPos                 : integer;
  lRAWFileList         : TStringList;
  lStation             : IStationData;
  lPatch               : IPatchData;
  LPatchRInputDate     : TDatetime;
begin
  try
    if ((FStationID <> 0) AND (FSplitIndex <> -1) AND (FPatchID <> 0)) then
    begin
      lStation := (FAppModules.Model.ModelData as IRainfallModelData).GetStationDataByID(FStationID);
      if (lStation <> nil) then
      begin
        lPatch := lStation.GetPatchWithID(FPatchID);
        if (lPatch <> nil) then
        begin
          lSeasons   := TStringList.Create;
          lParamList := TStringList.Create;
          try
            if (GetSeasons(lSeasons, lLengthOfSeason)) then
            begin
              lDirectory := (FAppModules.Model.ModelData as IRainfallModelData).DefaultDir;

              if (PatchRDialog.PatchRInputFileEdit.Text = '') then
                lPatchRInputFileName := FAppModules.Language.GetString('Rainfall.InputR')
              else
                lPatchRInputFileName := Trim(PatchRDialog.PatchRInputFileEdit.Text);

              lNrOfStations := lPatch.SourcesCount;
              lNrOfSeasons  := lSeasons.Count;
              lParamList.Add(IntToStr(lNrOfStations) + ' ' + IntToStr(lNrOfSeasons));
              lParamList.Add(lLengthOfSeason);
              for lIndex := 0 to lSeasons.Count - 1 do
                if (lSeasons[lIndex] <> '') then
                  lParamList.Add(lSeasons[lIndex]);

              lRAWFileList    := TStringList.Create;
              try
                GetFileNames(lRAWFileList, '.RAW');
                for lIndex := 0 to lRAWFileList.Count - 1 do
                  if (lRAWFileList[lIndex] <> '') then
                    lParamList.Add(lRAWFileList[lIndex]);
              finally
                FreeAndNil(lRAWFileList);
              end;

              lPos := Pos('.', lPatchRInputFileName);
              if (lPos > 0) then
                lPatchRInputFileName := Copy(lPatchRInputFileName, 1, lPos-1);
              lPatchRInputFileName := lPatchRInputFileName + '.txt';
              PatchRDialog.PatchRInputFileEdit.Text := lPatchRInputFileName;
              lStationFile := TFileStream.Create(lDirectory + lPatchRInputFileName, fmCreate );
              try
                lParamList.SaveToStream(lStationFile);
                lPatch.PatchRInputFileName := lPatchRInputFileName;
                lPatch.PatchRInputData     := lParamList.Text;
                //lPatch.PatchRInputDate := FileDateToDateTime(FileAge(lDirectory + lPatchRInputFileName));
                FileAge(lDirectory + lPatchRInputFileName,LPatchRInputDate);
                lPatch.PatchRInputDate := LPatchRInputDate;

                PopulatePatchRInput;
                PopulateRunPatchR;
              finally
                FreeAndNil(lStationFile);
              end;
            end;
          finally
            FreeAndNil(lSeasons);
            FreeAndNil(lParamList);
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TPatchRValidator.ChangePatchRPeriod (Sender : TObject);
const OPNAME = 'TPatchRValidator.ChangePatchRPeriod';
begin
  try
    with PatchRDialog do
    begin
      if (PatchRPeriodRadioGroup.ItemIndex = 1) then
      begin
        PatchRFistYearLabel.Enabled     := TRUE;
        PatchRFirstYearComboBox.Enabled := TRUE;
        PatchRLastYearLabel.Enabled     := TRUE;
        PatchRLastYearComboBox.Enabled  := TRUE;
        if (PatchRFirstYearComboBox.Text = '') then
        begin
          if (StrToInt(FDefaultStartYear) < FPatchRStartYear) then
            PatchRFirstYearComboBox.ItemIndex := PatchRFirstYearComboBox.Items.IndexOf(IntToStr(FPatchRStartYear))
          else
            PatchRFirstYearComboBox.ItemIndex := PatchRFirstYearComboBox.Items.IndexOf(FDefaultStartYear);
        end;
        if (PatchRLastYearComboBox.Text = '') then
        begin
          if (StrToInt(FDefaultEndYear) > FPatchREndYear) then
            PatchRLastYearComboBox.ItemIndex := PatchRLastYearComboBox.Items.IndexOf(IntToStr(FPatchREndYear))
          else
            PatchRLastYearComboBox.ItemIndex := PatchRLastYearComboBox.Items.IndexOf(FDefaultEndYear);
        end;
      end
      else
      begin
        PatchRFistYearLabel.Enabled     := FALSE;
        PatchRFirstYearComboBox.Enabled := FALSE;
        PatchRLastYearLabel.Enabled     := FALSE;
        PatchRLastYearComboBox.Enabled  := FALSE;
        PatchRFirstYearComboBox.ItemIndex := -1;
        PatchRFirstYearComboBox.Text      := '';
        PatchRLastYearComboBox.ItemIndex  := -1;
        PatchRLastYearComboBox.Text       := '';
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TPatchRValidator.ViewPatchRInput (Sender : TObject);
const OPNAME = 'TPatchRValidator.ViewPatchRInput';
var
  lText    : TStringList;
  lStation : IStationData;
  lPatch   : IPatchData;
begin
  try
    if ((FStationID <> 0) AND (FSplitIndex <> -1) AND (FPatchID <> 0)) then
    begin
      lStation := (FAppModules.Model.ModelData as IRainfallModelData).GetStationDataByID(FStationID);
      if (lStation <> nil) then
      begin
        lPatch := lStation.GetPatchWithID(FPatchID);
        if (lPatch <> nil) then
        begin
          lText := TStringList.Create;
          try
            lText.Text := lPatch.PatchRInputData;
            ViewTextData(lPatch.PatchRInputFileName, lPatch.PatchRInputDate, lText);
          finally
            FreeAndNil(lText);
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError (E, OPNAME) end;
end;

procedure TPatchRValidator.ViewPatchROutput (Sender : TObject);
const OPNAME = 'TPatchRValidator.ViewPatchROutput';
var
  lRainfallObj : IRainfallModelData;
  lStationID   : integer;
  lFileName    : string;
  lText        : TStringList;
  lCurrStation : IStationData;
  lCurrPatch   : IPatchData;
  lStation     : IStationData;
  lPatch       : IPatchData;
  lIndex       : integer;
begin
  try
    if ((FStationID <> 0) AND (FSplitIndex <> -1) AND (FPatchID <> 0)) then
    begin
      lRainfallObj := (FAppModules.Model.ModelData as IRainfallModelData);
      lCurrStation := lRainfallObj.GetStationDataByID(FStationID);
      if (lCurrStation <> nil) then
      begin
        lCurrPatch := lCurrStation.GetPatchWithID(FPatchID);
        if (lCurrPatch <> nil) then
        begin
          lIndex := PatchRDialog.PatchROutputFileNameComboBox.ItemIndex;
          if (lIndex >= 0) then
          begin
            lFileName  := PatchRDialog.PatchROutputFileNameComboBox.Items.Strings[lIndex];
            lStationID := Integer(PatchRDialog.PatchROutputFileNameComboBox.Items.Objects[lIndex]);
            lStation   := lRainfallObj.GetStationDataByID(lStationID);
            if (lStation <> nil) then
            begin
              lPatch := lStation.GetPatchWithID(lCurrPatch.PatchID);
              if (lPatch <> nil) then
              begin
                lText := TStringList.Create;
                try
                  lText.CommaText := lPatch.PatchROutputData;
                  ViewTextData(lFileName, lPatch.PatchROutputDate, lText);
                finally
                  FreeAndNil(lText);
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError (E, OPNAME) end;
end;

procedure TPatchRValidator.ViewPatchRPrinted (Sender : TObject);
const OPNAME = 'TPatchRValidator.ViewPatchRPrinted';
var
  lText    : TStringList;
  lStation : IStationData;
  lPatch   : IPatchData;
  lDirectory: string;
begin
  try
    if ((FStationID <> 0) AND (FSplitIndex <> -1) AND (FPatchID <> 0)) then
    begin
      lStation := (FAppModules.Model.ModelData as IRainfallModelData).GetStationDataByID(FStationID);
      if (lStation <> nil) then
      begin
        lPatch := lStation.GetPatchWithID(FPatchID);
        if (lPatch <> nil) then
        begin
          lText := TStringList.Create;
          try
            lDirectory := (FAppModules.Model.ModelData as IRainfallModelData).DefaultDir;
            if FileExists(lDirectory + lPatch.PatchRPrintFileName) then
              lText.LoadFromFile(lDirectory + lPatch.PatchRPrintFileName);
            ViewTextData(lPatch.PatchRPrintFileName, lPatch.PatchRPrintDate, lText);
          finally
            FreeAndNil(lText);
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError (E, OPNAME) end;
end;

procedure TPatchRValidator.ViewPatchRPlotted (Sender : TObject);
const OPNAME = 'TPatchRValidator.ViewPatchRPlotted';
var
  lText    : TStringList;
  lStation : IStationData;
  lPatch   : IPatchData;
  lDirectory: string;
begin
  try
    if ((FStationID <> 0) AND (FSplitIndex <> -1) AND (FPatchID <> 0)) then
    begin
      lStation := (FAppModules.Model.ModelData as IRainfallModelData).GetStationDataByID(FStationID);
      if (lStation <> nil) then
      begin
        lPatch := lStation.GetPatchWithID(FPatchID);
        if (lPatch <> nil) then
        begin
          lText := TStringList.Create;
          try
            lDirectory := (FAppModules.Model.ModelData as IRainfallModelData).DefaultDir;
            if FileExists(lDirectory + lPatch.PatchRPlotFileName) then
              lText.LoadFromFile(lDirectory + lPatch.PatchRPlotFileName);
            ViewTextData(lPatch.PatchRPlotFileName, lPatch.PatchRPlotDate, lText);
          finally
            FreeAndNil(lText);
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError (E, OPNAME) end;
end;

procedure TPatchRValidator.PatchRPrintFilenameChanged (Sender : TObject);
const OPNAME = 'TPatchRValidator.PatchRPrintFilenameChanged';
begin
  try
    CheckPathDosCompatibilty(PatchRDialog.PatchRPrintFileNameEdit.Text );
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TPatchRValidator.PatchRPlotFilenameChanged (Sender: TObject);
const OPNAME = 'TPatchRValidator.PatchRPlotFilenameChanged';
begin
  try
    CheckPathDosCompatibilty(PatchRDialog.PatchRPlotFileNameEdit.Text);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


procedure TPatchRValidator.CreatePatchRParamFile (ADefaultDir    : string;
                                                 ARunOption     : integer;
                                                 var AStartYear : integer;
                                                 var AEndYear   : integer);
const OPNAME = 'TPatchRValidator.CreatePatchRParamFile';
var
  lParamList       : TStringList;
  lFileStream      : TFileStream;
  lPatchPeriod     : string;
  lPrintFileName   : string;
  lPlotFileName    : string;
begin
  try
    with PatchRDialog do
    begin
      if (PatchRPeriodRadioGroup.ItemIndex = 0) then
        lPatchPeriod := 'N'
      else
      begin
        lPatchPeriod := 'Y';
        if (PatchRFirstYearComboBox.Text <> '') then
          AStartYear := StrToInt(Trim(PatchRFirstYearComboBox.Text));
        if (PatchRLastYearComboBox.Text <> '') then
          AEndYear := StrToInt(Trim(PatchRLastYearComboBox.Text));
      end;

      lPrintFileName := '';
      lPlotFileName  := '';
      lParamList     := TStringList.Create;
      lFileStream    := TFileStream.Create(ADefaultDir + FAppModules.Language.GetString('Rainfall.InputFile'), fmCreate);
      try
        lParamList.Add(PatchRInputFileEdit.Text);
        lParamList.Add(PatchRHeaderEdit.Text);
        lParamList.Add(IntToStr(ARunOption));
        if (ARunOption <> 0) then
        begin
          lPrintFileName := Trim(PatchRPrintFileNameEdit.Text);
          if (lPrintFileName = '') then
          begin
            lPrintFileName := FAppModules.Language.GetString('Rainfall.PtintFileName');
            PatchRPrintFileNameEdit.Text := lPrintFileName;
          end;
          if (ARunOption <> 1) then
          begin
            lPlotFileName := Trim(PatchRPlotFileNameEdit.Text);
            if (lPlotFileName = '') then
            begin
              lPlotFileName := FAppModules.Language.GetString('Rainfall.PlotFileName');
              PatchRPlotFileNameEdit.Text := lPlotFileName;
            end;
          end;
        end;
        case ARunOption of
          0 :
          begin
            lParamList.Add(lPatchPeriod);
            if (lPatchPeriod <> 'N') then
            begin
              lParamList.Add(IntToStr(AStartYear));
              lParamList.Add(IntToStr(AEndYear));
            end;
          end;
          1 :
          begin
            lParamList.Add(lPrintFileName);
            lParamList.Add(lPatchPeriod);
            if (lPatchPeriod <> 'N' ) then
            begin
              lParamList.Add(IntToStr(AStartYear));
              lParamList.Add(IntToStr(AEndYear));
            end;
          end;
          2..4 :
          begin
            lParamList.Add(lPrintFileName);
            lParamList.Add(lPlotFileName);
            lParamList.Add(lPatchPeriod);
            if (lPatchPeriod <> 'N') then
            begin
              lParamList.Add(IntToStr(AStartYear));
              lParamList.Add(IntToStr(AEndYear));
            end;
          end;
        end;
        lParamList.SaveToStream(lFileStream);
      finally
        FreeAndNil(lParamList);
        FreeAndNil(lFileStream);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TPatchRValidator.DoRunPatchR (Sender : TObject);
const OPNAME = 'TPatchRValidator.DoRunPatchR';
var
  lParamList       : TStringList;
  lBatFileStream   : TFileStream;
  lRunOption       : integer;
  lDirectory       : string;
  lStartYear       : integer;
  lEndYear         : integer;
  lPrintFileName   : string;
  lPlotFileName    : string;
  lTempList        : TStringList;
  lOutputFileName  : string;
  lPatchRDone      : boolean;
  lIndex           : integer;
  lPatchMultiple   : boolean;
  lSourceStationID : integer;
  lSourceStation   : IStationData;
  lSourceStr       : string;
  lRainfallObj     : IRainfallModelData;
  lTargetStation   : IStationData;
  lMessage         : string;
  lStation         : IStationData;
  lPatch           : IPatchData;
  LPatchRInputDate,
  lDate            : TDateTime;
  lStartupInfo     : TStartupInfo;
  lProcessInformation : TProcessInformation;
  lInProgress         : boolean;
begin
  try
    if (NOT NumberOfStationsWithinLimit('PatchR')) then
      Exit;
    with PatchRDialog do
    begin
      if ((PatchRPeriodRadioGroup.ItemIndex = 1) AND
          (NOT StartEndYearInputWithinLimits
                ('PatchR', PatchRFirstYearComboBox.Text, PatchRLastYearComboBox.Text,
                  FPatchRMinStartYear, FPatchRMaxEndYear))) then
        Exit;
       lPatchRDone := False;
      if ((FStationID <> 0) AND (FSplitIndex <> -1) AND (FPatchID <> 0)) then
      begin
        lRainfallObj := (FAppModules.Model.ModelData as IRainfallModelData);
        lDirectory   := lRainfallObj.DefaultDir;
        lStation     := lRainfallObj.GetStationDataByID(FStationID);
        if (lStation <> nil) then
        begin
          lPatch := lStation.GetPatchWithID(FPatchID);
          if (lPatch <> nil) then
          begin
            lRunOption   := StrToInt ((Sender as TButton).Caption);
            lStartYear   := FPatchRStartYear;
            lEndYear     := FPatchREndYear;

            CreatePatchRParamFile(lDirectory, lRunOption, lStartYear, lEndYear);
            lParamList     := TStringList.Create;
            lBatFileStream := TFileStream.Create(lDirectory + 'Input.bat', fmCreate);
            try
              lParamList.Clear;
              //lParamList.Add ( '@Patchr.exe < InputFile.txt' );
              lParamList.Add ( '@PATCHR_2013d.exe < InputFile.txt' );
              lParamList.SaveToStream(lBatFileStream);
            finally
              FreeAndNil(lParamList);
              FreeAndNil(lBatFileStream);
            end;

            lTempList := TStringList.Create;
            try
              GetFileNames(lTempList, '.PAT');
              if (lRunOption in [0,1,2]) then
              begin
                for lIndex := 0 to lTempList.Count - 1 do
                begin
                  lOutputFileName := lTempList.Strings[lIndex];
                  if (FileExists(lDirectory + lOutputFileName)) then
                    DeleteFile(PChar(lDirectory + lOutputFileName));
                end;
              end;
              lPrintFileName := Trim(PatchRPrintFileNameEdit.Text);
              lPlotFileName  := Trim(PatchRPlotFileNameEdit.Text);
              if ((lRunOption in [1,2]) AND FileExists(lDirectory + lPrintFileName)) then
                DeleteFile(PChar(lDirectory + lPrintFileName));
              if ((lRunOption in [2]) AND FileExists(lDirectory + lPlotFileName)) then
                DeleteFile(PChar(lDirectory + lPlotFileName));

              lParamList := TStringList.Create;
              lBatFileStream := TFileStream.Create(lDirectory + 'Output.bat', fmCreate);
              try
                lParamList.Add('Input.bat > RunPatchR');
                lParamList.SaveToStream(lBatFileStream);
              finally
                FreeAndNil(lParamList);
                FreeAndNil(lBatFileStream);
              end;

              //TShellExecuteObject.ExecuteShellAction('Open', 'Output.bat', '', pChar(lDirectory));

              FillChar(lStartupInfo, SizeOf(lStartupInfo), 0);
              lStartupInfo.cb := SizeOf(lStartupInfo);
              lInProgress := CreateProcess(nil, PChar(lDirectory + 'Output.bat' ), nil,
                                           nil, True, CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS,
                                           nil, PChar(lDirectory), lStartupInfo, lProcessInformation);
             if (lInProgress) then
             begin
               Screen.Cursor := crHourGlass;
               WaitForSingleObject(lProcessInformation.hProcess, INFINITE);
               CloseHandle(lProcessInformation.hThread);
               CloseHandle(lProcessInformation.hProcess);
               lPatchRDone := True;
             end;
              if (lRunOption in [0,1,2]) then
                FilesReadyToBeAccessed(lDirectory, lTempList);

              if (lPatchRDone AND (lRunOption in [0,1,2])) then
              begin
                lPatch.PatchStartYear   := lStartYear;
                lPatch.PatchEndYear     := lEndYear;
                if (lTempList.Count > 0) then
                begin
                  lOutputFileName := lTempList.Strings[0];
                  //lPatch.PatchROutputDate := FileDateToDateTime(FileAge(lDirectory + lOutputFileName));
                  FileAge(lDirectory + lOutputFileName,LPatchRInputDate);
                  lPatch.PatchROutputDate := LPatchRInputDate;
                end;
                lPatchMultiple := PatchMultipleRadioGroup.ItemIndex = 1;
                SavePatchROutputToDB(lTempList, lPatchMultiple);
                lPatch.PatchMultiple := lPatchMultiple;

                if (lRunOption in [1,2]) then
                begin
                  if (FileExists(lDirectory + lPrintFileName)) then
                  begin
                    SavePatchRPrintToDB;
                    //lDate := FileDateToDateTime(FileAge(lDirectory + lPrintFileName));
                    FileAge(lDirectory + lPrintFileName,lDate);
                    lPatch.PatchRPrintDate := lDate;
                  end;
                  if (lRunOption = 2) then
                  begin
                    if (FileExists(lDirectory + lPlotFileName)) then
                    begin
                      SavePatchRPlotToDB;
                      //lDate := FileDateToDateTime(FileAge(lDirectory + lPlotFileName));
                      FileAge(lDirectory + lPlotFileName,lDate);
                      lPatch.PatchRPlotDate := lDate;
                    end;
                  end;
                end;

                lTempList.Clear;
                lTempList.CommaText := lPatch.SourceInfo;
                lPatch := nil;
                lTargetStation := nil;
                for lIndex := 0 to lTempList.Count - 1 do
                begin
                  lSourceStr       := lTempList.Strings[lIndex];
                  lSourceStationID := StrToInt(Copy(lSourceStr, 1, Pos(',', lSourceStr) - 1));
                  lSourceStation   := lRainfallObj.GetStationDataByID(lSourceStationID);
                  if (lSourceStation <> nil) then
                    lSourceStation.LoadMonthlyData;
                  if (Pos('Y', lSourceStr) > 0) then
                    lTargetStation := lSourceStation;
                end;
                if (lStation.RainfallData.StationID <> lTargetStation.RainfallData.StationID) then
                  lRainfallObj.CurrentStationID := lTargetStation.RainfallData.StationID;
              end;
            finally
              FreeAndNil(lTempList);
            end;
            Screen.Cursor := crDefault;

            if (lPatchRDone) then
              lMessage := ''
            else
              lMessage := FAppModules.Language.GetString('Rainfall.ErrorRunningPatchR') + #13#10;

            lMessage := FAppModules.Language.GetString('Rainfall.ViewPatchRInputInNotePad');
            if (MessageDlg(lMessage, mtConfirmation, [ mbYes, mbNo ], 0 ) = mrYes) then
              ShowRunProcess(lDirectory + 'RunPatchR' );

            PopulateRunPatchR;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TPatchRValidator.SavePatchROutputToDB (APATFiles      : TStringList;
                                                APatchMultiple : boolean);
const OPNAME = 'TPatchRValidator.SavePatchROutputToDB';
var
  lData            : TStringList;
  lFileName        : string;
  lPatSearchRec    : TSearchRec;
  lDirectory       : string;
  lIndex           : integer;
  lRainfallObj     : IRainfallModelData;
  lSourceStationID : integer;
  lSourcePatchID   : integer;
  lTargetStation   : WideString;
  lSourceStation   : IStationData;
  lStation         : IStationData;
  lPatch           : IPatchData;
  lStartYear       : integer;
  lEndYear         : integer;
begin
  try
    if ((FStationID <> 0) AND (FSplitIndex <> -1) AND (FPatchID <> 0)) then
    begin
      lRainfallObj := (FAppModules.Model.ModelData as IRainfallModelData);
      lDirectory   := lRainfallObj.DefaultDir;
      lStation     := lRainfallObj.GetStationDataByID(FStationID);
      if (lStation <> nil) then
      begin
        lPatch := lStation.GetPatchWithID(FPatchID);
        if (lPatch <> nil) then
        begin
          lPatch.DeleteMonthlyPatchData;
          lData := TStringList.Create;
          try
            for lIndex := 0 to lPatch.SourcesCount - 1 do
            begin
              lPatch.GetSourceInfoByIndex(lIndex, lSourceStationID, lSourcePatchID,
                                          lTargetStation, lStartYear, lEndYear);
              lSourceStation := lRainfallObj.GetStationDataByID(lSourceStationID);
              if (lSourceStation <> nil) then
              begin
                if ((lTargetStation = 'Y') OR APatchMultiple) then
                begin
                  lFileName := lSourceStation.RainfallData.StationNumber;
                  while (Pos(' ', lFileName) > 0) do
                    Delete(lFileName, Pos(' ', lFileName), 1);
                  lFileName := Copy(lFileName, 1, 8);
                  lFileName := lFileName + '.PAT';
                  if (FindFirst(lDirectory + lFileName, faAnyFile, lPatSearchRec) = 0) then
                  begin
                    lData.LoadFromFile(lDirectory + lFileName);
                    lPatch.SavePatchROutputData(lSourceStationID, lData.Text);
                    lData.Clear;
                  end;
                end;
              end;
            end;
          finally
            FreeAndNil(lData);
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TPatchRValidator.SavePatchRPrintToDB;
const OPNAME = 'TPatchRValidator.SavePatchRPrintToDB';
var
  lData      : TStringList;
  lDirectory : string;
  lStation   : IStationData;
  lPatch     : IPatchData;
  lMessage   : string;
begin
  try
    if ((FStationID <> 0) AND (FSplitIndex <> -1) AND (FPatchID <> 0)) then
    begin
      with PatchRDialog do
      begin
        lDirectory := (FAppModules.Model.ModelData as IRainfallModelData).DefaultDir;
        lStation   := (FAppModules.Model.ModelData as IRainfallModelData).GetStationDataByID(FStationID);
        if (lStation <> nil) then
        begin
          lPatch := lStation.GetPatchWithID(FPatchID);
          if (lPatch <> nil) then
          begin

            if (NOT (FileExists(lDirectory + PatchRPrintFileNameEdit.Text))) then
            begin
              lMessage := FAppModules.Language.GetString('Rainfall.NotFound');
              lMessage := Format(lMessage, [lDirectory]);
              ShowMessage(lMessage);
            end
            else
            begin
              lData := TStringList.Create;
              try
                lData.LoadFromFile(lDirectory + PatchRPrintFileNameEdit.Text);
                lPatch.PatchRPrintData := lData.Text;
                lPatch.PatchRPrintFileName := Trim(PatchRPrintFileNameEdit.Text);
              finally
                FreeAndNil(lData);
              end;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TPatchRValidator.SavePatchRPlotToDB;
const OPNAME = 'TPatchRValidator.SavePatchRPlotToDB';
var
  lData      : TStringList;
  lDirectory : string;
  lStation   : IStationData;
  lPatch     : IPatchData;
  lMessage   : string;
begin
  try
    if ((FStationID <> 0) AND (FSplitIndex <> -1) AND (FPatchID <> 0)) then
    begin
      with PatchRDialog do
      begin
        lDirectory := (FAppModules.Model.ModelData as IRainfallModelData).DefaultDir;
        lStation   := (FAppModules.Model.ModelData as IRainfallModelData).GetStationDataByID(FStationID);
        if (lStation <> nil) then
        begin
          lPatch := lStation.GetPatchWithID(FPatchID);
          if (lPatch <> nil) then
          begin
            if (NOT (FileExists(lDirectory + PatchRPlotFileNameEdit.Text))) then
            begin
              lMessage := FAppModules.Language.GetString('Rainfall.NotFound');
              lMessage := Format(lMessage, [PatchRPlotFileNameEdit.Text]);
              ShowMessage(lMessage);
            end
            else
            begin
              lData := TStringList.Create;
              try
                lData.LoadFromFile(lDirectory + PatchRPlotFileNameEdit.Text);
                lPatch.PatchRPlotFileName := Trim(PatchRPlotFileNameEdit.Text);
                lPatch.PatchRPlotData := lData.Text;
              finally
                FreeAndNil(lData);
              end;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TPatchRValidator.ChopSeconds (ADate : TDateTime) : TDateTime;
const OPNAME = 'TPatchRValidator.ChopSeconds';
var
  lYear, lMonth, lDay : Word;
  lHour, lMin, lSeconds, lMilli : Word;
begin
  Result := 0;
  try
    DecodeDate(ADate, lYear, lMonth, lDay);
    DecodeTime(ADate, lHour, lMin, lSeconds, lMilli);
    Result := EncodeDateTime(lYear, lMonth, lDay, lHour, lMin, 0, 0);
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TPatchRValidator.ShowRunProcess ( aFileName : string );
const OPNAME = 'TPatchRValidator.ShowRunProcess';
begin
  try
    ShellExecute ( Application.Handle, 'open', 'notepad.exe' , PChar ( aFileName ),
                   PChar ( ExtractFileDir ( aFileName ) ), SW_SHOW );
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TPatchRValidator.StartEndYearInputWithinLimits (AUtility     : string;
                                                        AStartYear   : string;
                                                        AEndYear     : string;
                                                        AStartLimit  : string;
                                                        AEndLimit    : string) : boolean;
const OPNAME = 'TPatchRValidator.StartEndYearInputWithinLimits';
var
  lStartYear : integer;
  lEndYear   : integer;
  lStartMsg  : string;
  lEndMsg    : string;
begin
  Result := FALSE;
  try
    lStartMsg := FAppModules.Language.GetString('Rainfall.StartYearInvalid');
    lEndMsg   := FAppModules.Language.GetString('Rainfall.EndYearInvalid');
    if (Trim(AStartYear) = '') then
      MessageDlg(lStartMsg, mtInformation, [mbOK], 0)
    else
    if (Trim(AEndYear) = '') then
      MessageDlg(lEndMsg, mtInformation, [mbOK], 0)
    else
    begin
      try
        lStartYear := StrToInt(Trim(AStartYear));
        try
          lEndYear := StrToInt(Trim(AEndYear));
          Result := ((lStartYear >= StrToInt(AStartLimit)) AND
                     (lEndYear   <= StrToInt(AEndLimit)));
          if (NOT Result) then
            MessageDlg(Format(FAppModules.Language.GetString('Rainfall.YearsLimit'),
                             [AStartYear, AEndYear, AUtility]), mtInformation, [mbOK], 0 );
        except
          MessageDlg(lEndMsg, mtInformation, [mbOK], 0 );
        end;
      except
        MessageDlg(lStartMsg, mtInformation, [mbOK], 0 );
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TPatchRValidator.NumberOfStationsWithinLimit (AUtility : string ): boolean;
const OPNAME = 'TPatchRValidator.NumberOfStationsWithinLimit';
var
  lMax     : string;
  lStation : IStationData;
  lPatch   : IPatchData;
begin
  Result := False;
  try
    if ((FStationID <> 0) AND (FSplitIndex <> -1) AND (FPatchID <> 0)) then
    begin
      lStation := (FAppModules.Model.ModelData as IRainfallModelData).GetStationDataByID(FStationID);
      if (lStation <> nil) then
      begin
        lPatch := lStation.GetPatchWithID(FPatchID);
        if (lPatch <> nil) then
        begin
          if (AUtility = 'ClassR') then
          begin
            Result := (lPatch.SourcesCount <= StrToInt(FClassRGaugesMax));
            lMax   :=  FClassRGaugesMax;
          end;
          if (AUtility = 'PatchR') then
          begin
            Result := (lPatch.SourcesCount <= StrToInt(FPatchRGaugesMax));
            lMax   := FPatchRGaugesMax;
          end;
          if (NOT Result) then
            MessageDlg(Format(FAppModules.Language.GetString('Rainfall.StationsLimit'),
                              [lMax, AUtility]), mtInformation, [mbOK], 0);
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TPatchRValidator.SaveState : Boolean;
const OPNAME = 'TPatchRValidator.SaveState';
begin
  Result := False;
  try
    FAppModules.ViewIni.WriteInteger('TPatchRValidator', 'CurrentpatchID', FPatchID);
    FAppModules.ViewIni.WriteInteger('TPatchRValidator', 'CurrentStationID', FStationID);
    FAppModules.ViewIni.WriteInteger('TPatchRValidator', 'CurrentSplitIndex', FSplitIndex);
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
