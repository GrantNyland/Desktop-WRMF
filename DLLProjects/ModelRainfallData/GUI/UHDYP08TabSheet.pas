//
//
//  UNIT      : Contains THDYP08TabSheet Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 27/05/2004
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UHDYP08TabSheet;

interface
uses
  UAbstractObject,
  UAbstractComponent,
  UShellExecuteObject,
  VCL.Graphics,
  Classes,
  VCL.stdctrls,
  VCL.ComCtrls,
  VCL.extctrls,
  VCL.CheckLst,
  Math,
  VCL.Dialogs,
  VCL.Forms,
  Windows,
  VCL.Controls,
  VCL.Buttons;

type
  THDYP08TabSheet = class(TAbstractTabSheet)
  protected

    FZoneInfoGroupBox       : TGroupBox;
    FlblNrOfStationsHeading : TLabel;
    FlblNrOfStationsValue   : TLabel;
    FlblRainfallStations    : TLabel;
    FmmoRainfallStations    : TMemo;

    FViewInfoLabel            : TLabel;
    FPatchChangedShape        : TShape;
    FPatchChangedLabel        : TLabel;
    FFileNotOnDiskShape       : TShape;
    FFileNotOnDiskLabel       : TLabel;
    FFileInvalidShape         : TShape;
    FFileInvalidLabel         : TLabel;
    FNoStationsLabel          : TLabel;
    FNotRunYetLabel           : TLabel;

    FInputGroupBox          : TGroupBox;
    FOutputGroupBox         : TGroupBox;
    FlblCatchment           : TLabel;
    FedtCatchment           : TEdit;
    FViewCatchmentButton    : TButton;
    FlblOutpuFileName       : TLabel;
    FedtOutpuFileName       : TEdit;
    FViewOutputFileButton   : TButton;

    FlblFirstYear           : TLabel;
    FlblLastYear            : TLabel;
    FCbxFirstYear           : TComboBox;
    FCbxLastYear            : TComboBox;

    FlblPlotingQuestion     : TLabel;
    FcmbPlotingQuestion     : TCombobox;
    FbtnRunHDYP08           : TButton;

    FStationList            : TStringList;
    FHDYP08StartYear        : integer;
    FHDYP08EndYear          : integer;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetToolBar: TAbstractToolBar; override;
    procedure RunHDYP08 ( Sender : TObject );
    function ValidateInput : boolean;
    function StartEndYearInputWithinLimits (AStartYear   : string;
                                            AEndYear     : string;
                                            AStartLimit  : integer;
                                            AEndLimit    : integer) : boolean;
    procedure SaveFilesToDB (ADirectory      : string;
                             ACatchFilename  : string;
                             AOutputFilename : string);
    procedure AddInputStations ( Sender : TObject );
    procedure ViewCatchment (Sender : TObject);
    procedure ViewOutputFile (Sender : TObject);
    procedure ViewTextData (AFileName : string;
                            AFileDate : TDateTime;
                            AText     : TStringList);
    procedure PopulateZoneInfo;
    procedure PopulateHDYP08Input;
    function ChopSeconds (ADate : TDateTime) : TDateTime;
  public
    function Initialise: boolean; override;
    procedure TabHasChanged; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    procedure ProcessCustomEvent ( AData: TObject ); override;
    function SaveState: boolean; override;
end;

implementation

uses
  System.UITypes,
  SysUtils,
  DateUtils,
  UErrorHandlingOperations,
  RainfallCom_TLB, 
  ShellApi;

{ THDYP08TabSheet }

procedure THDYP08TabSheet.CreateMemberObjects;
const OPNAME = 'THDYP08TabSheet.CreateMemberObjects';
var
  LViewButtonCaption : String;
begin
  inherited CreateMemberObjects;
  try
    LViewButtonCaption := FAppModules.Language.GetString('ButtonCaption.View');
    FTabCaptionKey := 'HDYP08Editing';
    FStationList := TStringList.Create;

    FZoneInfoGroupBox       := TGroupBox.Create(Self);
    FZoneInfoGroupBox.Parent  := Self;
    FZoneInfoGroupBox.Left    := 10;
    FZoneInfoGroupBox.Top     := 5;
    FZoneInfoGroupBox.Width   := 880;
    FZoneInfoGroupBox.Height  := 155;
    FZoneInfoGroupBox.Caption := FAppModules.Language.GetString('GroupBoxCaption.ZoneInformation');

    FlblNrOfStationsHeading         := TLabel.Create ( Self );
    FlblNrOfStationsHeading.Parent  := FZoneInfoGroupBox;
    FlblNrOfStationsHeading.Left    := 10;
    FlblNrOfStationsHeading.Top     := 20;
    FlblNrOfStationsHeading.Caption := FAppModules.Language.GetString('LabelText.NumberOfStations');

    FlblNrOfStationsValue           := TLabel.Create ( Self );
    FlblNrOfStationsValue.Parent  := FZoneInfoGroupBox;
    FlblNrOfStationsValue.Left    := 110;
    FlblNrOfStationsValue.Top     := 20;

    FlblRainfallStations         := TLabel.Create ( Self );
    FlblRainfallStations.Parent  := FZoneInfoGroupBox;
    FlblRainfallStations.Top     := 40;
    FlblRainfallStations.Left    := 10;
    FlblRainfallStations.Caption := FAppModules.Language.GetString('LabelText.Rainfall');

    FmmoRainfallStations          := TMemo.Create ( Self );// TListBox.Create ( Self );
    FmmoRainfallStations.Parent   := FZoneInfoGroupBox;
    FmmoRainfallStations.Top      := 40;
    FmmoRainfallStations.Width    := 190;
    FmmoRainfallStations.Height   := 100;
    FmmoRainfallStations.Left     := 110;
    FmmoRainfallStations.ReadOnly := True;
    FmmoRainfallStations.Color    := clBtnFace;
    FmmoRainfallStations.TabStop  := FALSE;

    FViewInfoLabel  := TLabel.Create(Self);
    FViewInfoLabel.Parent   := FZoneInfoGroupBox;
    FViewInfoLabel.Left     := 330;
    FViewInfoLabel.Top      := 20;
    FViewInfoLabel.AutoSize := FALSE;
    FViewInfoLabel.Width    := 500;
    FViewInfoLabel.Height   := 26;
    FViewInfoLabel.Layout   := tlCenter;
    FViewInfoLabel.WordWrap := TRUE;
    FViewInfoLabel.Caption  := FAppModules.Language.GetString('LabelText.ViewButton');

    FPatchChangedShape := TShape.Create(Self);
    FPatchChangedShape.Parent := FZoneInfoGroupBox;
    FPatchChangedShape.Left   := 330;
    FPatchChangedShape.Top    := 45;
    FPatchChangedShape.Width  := 20;
    FPatchChangedShape.Height := 20;
    FPatchChangedShape.Brush.Color := clRed;

    FPatchChangedLabel  := TLabel.Create(Self);
    FPatchChangedLabel.Parent   := FZoneInfoGroupBox;
    FPatchChangedLabel.Left     := 360;
    FPatchChangedLabel.Top      := 43;
    FPatchChangedLabel.AutoSize := FALSE;
    FPatchChangedLabel.Width    := 500;
    FPatchChangedLabel.Height   := 26;
    FPatchChangedLabel.Layout   := tlCenter;
    FPatchChangedLabel.WordWrap := TRUE;
    FPatchChangedLabel.Caption := FAppModules.Language.GetString('LabelText.ResultsStoredMayBeInvalid');

    FFileNotOnDiskShape := TShape.Create(Self);
    FFileNotOnDiskShape.Parent := FZoneInfoGroupBox;
    FFileNotOnDiskShape.Left   := 330;
    FFileNotOnDiskShape.Top    := 70;
    FFileNotOnDiskShape.Width  := 20;
    FFileNotOnDiskShape.Height := 20;
    FFileNotOnDiskShape.Brush.Color := clAqua;

    FFileNotOnDiskLabel  := TLabel.Create(Self);
    FFileNotOnDiskLabel.Parent   := FZoneInfoGroupBox;
    FFileNotOnDiskLabel.Left     := 360;
    FFileNotOnDiskLabel.Top      := 68;
    FFileNotOnDiskLabel.AutoSize := FALSE;
    FFileNotOnDiskLabel.Width    := 500;
    FFileNotOnDiskLabel.Height   := 26;
    FFileNotOnDiskLabel.Layout   := tlCenter;
    FFileNotOnDiskLabel.WordWrap := TRUE;
    FFileNotOnDiskLabel.Caption := FAppModules.Language.GetString('LabelText.Indication');

    FFileInvalidShape := TShape.Create(Self);
    FFileInvalidShape.Parent := FZoneInfoGroupBox;
    FFileInvalidShape.Left   := 330;
    FFileInvalidShape.Top    := 95;
    FFileInvalidShape.Width  := 20;
    FFileInvalidShape.Height := 20;
    FFileInvalidShape.Brush.Color := clYellow;

    FFileInvalidLabel  := TLabel.Create(Self);
    FFileInvalidLabel.Parent   := FZoneInfoGroupBox;
    FFileInvalidLabel.Left     := 360;
    FFileInvalidLabel.Top      := 93;
    FFileInvalidLabel.AutoSize := FALSE;
    FFileInvalidLabel.Width    := 500;
    FFileInvalidLabel.Height   := 26;
    FFileInvalidLabel.Layout   := tlCenter;
    FFileInvalidLabel.WordWrap := TRUE;
    FFileInvalidLabel.Caption  := FAppModules.Language.GetString('LabelText.FileStoredOnDiskMayBeInvalid');

    FNoStationsLabel            := TLabel.Create(Self);
    FNoStationsLabel.Parent     := FZoneInfoGroupBox;
    FNoStationsLabel.Left       := 330;
    FNoStationsLabel.Top        := 120;
    FNoStationsLabel.Layout     := tlCenter;
    FNoStationsLabel.Caption    := FAppModules.Language.GetString('LabelText.NoStations');
    FNoStationsLabel.Visible    := FALSE;
    FNoStationsLabel.Font.Style := [fsBold];
    FNoStationsLabel.Font.Color := clRed;

    FNotRunYetLabel            := TLabel.Create(Self);
    FNotRunYetLabel.Parent     := FZoneInfoGroupBox;
    FNotRunYetLabel.Left       := 330;
    FNotRunYetLabel.Top        := 120;
    FNotRunYetLabel.Layout     := tlCenter;
    FNotRunYetLabel.Caption    := FAppModules.Language.GetString('LabelText.HDYP08NotRun');
    FNotRunYetLabel.Visible    := FALSE;
    FNotRunYetLabel.Font.Style := [fsBold];
    FNotRunYetLabel.Font.Color := clRed;

    FInputGroupBox       := TGroupBox.Create(Self);
    FInputGroupBox.Parent  := Self;
    FInputGroupBox.Left    := 10;
    FInputGroupBox.Top     := 170;
    FInputGroupBox.Width   := 380;
    FInputGroupBox.Height  := 400;
    FInputGroupBox.Caption := FAppModules.Language.GetString('GroupBoxCaption.HDYP08');

    FlblCatchment          := TLabel.Create ( Self );
    FlblCatchment.Parent   := FInputGroupBox;
    FlblCatchment.Caption  := FAppModules.Language.GetString('LabelText.CatchmentCode');
    FlblCatchment.Left     := 10;
    FlblCatchment.Top      := 20;
    FlblCatchment.AutoSize := FALSE;
    FlblCatchment.Height   := 26;
    FlblCatchment.Width    := 130;
    FlblCatchment.WordWrap := TRUE;

    FedtCatchment           := TEdit.Create ( Self );
    FedtCatchment.Parent    := FInputGroupBox;
    FedtCatchment.Width     := 100;
    FedtCatchment.Left      := 150;
    FedtCatchment.Top       := 20;
    FedtCatchment.MaxLength := 6;
    FedtCatchment.MaxLength := 6;

    FViewCatchmentButton := TButton.Create(Self);
    FViewCatchmentButton.Parent  := FInputGroupBox;
    FViewCatchmentButton.Left    := 260;
    FViewCatchmentButton.Top     := 20;
    FViewCatchmentButton.Width   := 40;
    FViewCatchmentButton.Height  := 21;
    FViewCatchmentButton.Caption := LViewButtonCaption;
    FViewCatchmentButton.Enabled := FALSE;
    FViewCatchmentButton.OnClick := ViewCatchment;

    FlblOutpuFileName         := TLabel.Create ( Self );
    FlblOutpuFileName.Parent  := FInputGroupBox;
    FlblOutpuFileName.Caption := FAppModules.Language.GetString('LabelText.OutputFile');
    FlblOutpuFileName.Top     := 60;
    FlblOutpuFileName.Left    := 10;

    FedtOutpuFileName        := TEdit.Create ( Self );
    FedtOutpuFileName.Parent := FInputGroupBox;
    FedtOutpuFileName.Top    := 60;
    FedtOutpuFileName.Left   := 150;
    FedtOutpuFileName.Width  := 100;

    FViewOutputFileButton := TButton.Create(Self);
    FViewOutputFileButton.Parent  := FInputGroupBox;
    FViewOutputFileButton.Left    := 260;
    FViewOutputFileButton.Top     := 60;
    FViewOutputFileButton.Width   := 40;
    FViewOutputFileButton.Height  := 21;
    FViewOutputFileButton.Caption := LViewButtonCaption;
    FViewOutputFileButton.Enabled := FALSE;
    FViewOutputFileButton.OnClick := ViewOutputFile;

    FlblFirstYear         := TLabel.Create ( Self );
    FlblFirstYear.Parent  := FInputGroupBox;
    FlblFirstYear.Caption := FAppModules.Language.GetString('LabelText.FirstYear');
    FlblFirstYear.Left    := 10;
    FlblFirstYear.Top     := 100;

    FCbxFirstYear        := TComboBox.Create ( Self );
    FCbxFirstYear.Parent := FInputGroupBox;
    FCbxFirstYear.Top    := 100;
    FCbxFirstYear.Width  := 50;
    FCbxFirstYear.Left   := 150;
    FCbxFirstYear.Style  := csDropDown;
    FCbxFirstYear.AutoComplete := FALSE;

    FlblLastYear         := TLabel.Create ( Self );
    FlblLastYear.Parent  := FInputGroupBox;
    FlblLastYear.Caption := FAppModules.Language.GetString('LabelText.LastYear');
    FlblLastYear.Left    := 10;
    FlblLastYear.Top     := 140;

    FCbxLastYear        := TComboBox.Create ( Self );
    FCbxLastYear.Parent := FInputGroupBox;
    FCbxLastYear.Top    := 140;
    FCbxLastYear.Width  := 50;
    FCbxLastYear.Left   := 150;
    FCbxLastYear.Style  := csDropDown;
    FCbxLastYear.AutoComplete := FALSE;

    FlblPlotingQuestion         := TLabel.Create ( Self );
    FlblPlotingQuestion.Parent  := FInputGroupBox;
    FlblPlotingQuestion.Left    := 10;
    FlblPlotingQuestion.Top     := 180;
    FlblPlotingQuestion.Caption := FAppModules.Language.GetString('LabelText.Graphics');

    FcmbPlotingQuestion         := TCombobox.Create ( Self );
    FcmbPlotingQuestion.Parent  := FInputGroupBox;
    FcmbPlotingQuestion.Top     := 180;
    FcmbPlotingQuestion.Left    := 150;
    FcmbPlotingQuestion.Width   := 200;

    FbtnRunHDYP08               := TButton.Create ( Self );
    FbtnRunHDYP08.Parent        := FInputGroupBox;
    FbtnRunHDYP08.Caption       := FAppModules.Language.GetString('ButtonCaption.Run');
    FbtnRunHDYP08.Width         := 100;
    FbtnRunHDYP08.Left          := 150;
    FbtnRunHDYP08.Top           := 220;
    FbtnRunHDYP08.OnClick       := RunHDYP08;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure THDYP08TabSheet.DestroyMemberObjects;
const OPNAME = 'THDYP08TabSheet.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FreeAndNil(FStationList);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function THDYP08TabSheet.GetToolBar: TAbstractToolBar;
const OPNAME = 'THDYP08TabSheet.GetToolBar';
begin
  Result := nil;
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function THDYP08TabSheet.Initialise: boolean;
const OPNAME = 'THDYP08TabSheet.Initialise';
var
  lIndex    : integer;
  lStartTxt : string;
  lEndTxt   : string;
begin
  Result := inherited Initialise;
  try
    lStartTxt := FAppModules.ViewIni.ReadString(Self.ClassName, 'Hdyp08StartYear', 'DefaultStart');
    lEndTxt   := FAppModules.ViewIni.ReadString(Self.ClassName, 'Hdyp08EndYear', 'DefaultEndYear');
    FHDYP08StartYear := StrToInt(lStartTxt);
    FHDYP08EndYear   := StrToInt(lEndTxt);
    if (Parent <> nil) then
    begin
      FCbxLastYear.Items.Clear;
      for lIndex := FHDYP08StartYear to FHDYP08EndYear do
        FCbxLastYear.Items.Add( IntToStr ( lIndex ) );
      FCbxFirstYear.Items.Clear;
      for lIndex := FHDYP08StartYear to FHDYP08EndYear do
        FCbxFirstYear.Items.Add( IntToStr ( lIndex ) );
      FcmbPlotingQuestion.Items.Clear;
      FcmbPlotingQuestion.Items.Add ( 'No graphics card / No plot required' );
      FcmbPlotingQuestion.Items.Add ( 'Hercules card' );
      FcmbPlotingQuestion.Items.Add ( 'EGA or VGA card' );
      FcmbPlotingQuestion.Items.Add ( 'CGA card' );
      FcmbPlotingQuestion.ItemIndex := 2;
    end;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure THDYP08TabSheet.TabHasChanged;
const OPNAME = 'THDYP08TabSheet.TabHasChanged';
var
  lDirectory : string;
begin
  inherited;
  try
    lDirectory := (FAppModules.Model.ModelData as IRainfallModelData).DefaultDir;
    PopulateZoneInfo;
    PopulateHDYP08Input;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure THDYP08TabSheet.PopulateZoneInfo;
const OPNAME = 'THDYP08TabSheet.PopulateZoneInfo';
var
  lIndex       : integer;
  lZoneList    : TStringList;
  lStationID   : integer;
  lPatchID     : integer;
  lStation     : IStationData;
  lPatch       : IPatchData;
  lStartYear   : integer;
  lEndYear     : integer;
  lTempStr     : string;
  lRainfallObj : IRainfallModelData;
begin
  try
    FStationList.Clear;
    FmmoRainfallStations.Clear;
    lRainfallObj := (FAppModules.Model.ModelData as IRainfallModelData);
    lZoneList    := TStringList.Create;
    try
      lZoneList.CommaText := lRainfallObj.GetZoneStations;
      for lIndex := 0 to lZoneList.Count - 1 do
      begin
        lStationID := StrToInt(Trim(Copy(lZoneList[LIndex],  1, 6)));
        lPatchID   := StrToInt(Trim(Copy(lZoneList[LIndex],  7, 6)));
        lStartYear := StrToInt(Trim(Copy(lZoneList[LIndex], 13, 4)));
        lEndYear   := StrToInt(Trim(Copy(lZoneList[LIndex], 17, 4)));
        lStation   := lRainfallObj.GetStationDataByID(lStationID);
        if (lStation <> nil) then
        begin
          FStationList.Add(lStation.RainfallData.StationNumber);
          if (lPatchID = 0) then
          begin
            if (lStartYear = 0) AND (lEndYear = 0) then
              lTempStr := lStation.RainfallData.StationNumber +
                          '(' + IntToStr(lStation.RainfallData.HydroStartYear) + ' - ' +
                          IntToStr(lStation.RainfallData.HydroEndYear) + ')'
            else
              lTempStr := lStation.RainfallData.StationNumber +
                          '(' + IntToStr(lStartYear) + ' - ' + IntToStr(lEndYear) + ')';
            FmmoRainfallStations.Lines.Add(lTempStr)
          end
          else
          begin
            lPatch := lStation.GetPatchWithID(lPatchID);
            if (lPatch <> nil) then
              FmmoRainfallStations.Lines.Add(lStation.RainfallData.StationNumber + ' - ' + lPatch.PatchName);
          end;
        end;
      end;
    finally
      if (LZoneList <> nil) then
        FreeAndNil(LZoneList);
    end;
    FlblNrOfStationsValue.Caption := IntToStr(FStationList.Count);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function THDYP08TabSheet.ChopSeconds (ADate : TDateTime) : TDateTime;
const OPNAME = 'THDYP08TabSheet.ChopSeconds';
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

procedure THDYP08TabSheet.PopulateHDYP08Input;
const OPNAME = 'THDYP08TabSheet.PopulateHDYP08Input';
var
  lData        : TStringList;
  lRainfallObj : IRainfallModelData;
  lChangeDate  : TDateTime;
  lRunDate     : TDateTime;
  lDirectory   : string;
  lFileName    : string;
  lFileDate    : TDateTime;
begin
  try
    FViewCatchmentButton.Enabled      := FALSE;
    FViewOutputFileButton.Enabled     := FALSE;
    FNoStationsLabel.Visible          := FALSE;
    FNotRunYetLabel.Visible           := FALSE;

    lRainfallObj := (FAppModules.Model.ModelData as IRainfallModelData);
    lChangeDate  := lRainfallObj.ZoneChangeDate;
    lRunDate     := lRainfallObj.ZoneRunDate;

    if (lRainfallObj.GetZoneCount = 0) then
      FNoStationsLabel.Visible := TRUE
    else if (lRunDate = 0) then
      FNotRunYetLabel.Visible := TRUE;

    FedtCatchment.Text     := lRainfallObj.CatchmentFileName;
    FedtOutpuFileName.Text := lRainfallObj.OutputFileName;

    FedtCatchment.Color  := clWhite;
    FedtOutpuFileName.Color := clWhite;
    if ((lChangeDate > 0) AND (lRunDate > 0)) then
    begin
      if (lChangeDate > lRunDate) then
      begin
        FedtCatchment.Color  := clRed;
        FedtOutpuFileName.Color := clRed;
      end
      else
      begin
        lDirectory := lRainfallObj.DefaultDir;
        lFileName  := lRainfallObj.CatchmentFileName;
        if (Trim(lFileName) <> '') then
        begin
          if (FileExists(lDirectory + lFileName)) then
          begin
            //lFileDate  := FileDateToDateTime(FileAge(lDirectory + lFileName));
            FileAge(lDirectory + lFileName,lFileDate);
            if (ChopSeconds(lFileDate) > ChopSeconds(lRunDate)) then
              FedtCatchment.Color := clYellow;
          end
          else
            FedtCatchment.Color := clAqua;
        end;
        lFileName  := lRainfallObj.OutputFileName;
        if (Trim(lFileName) <> '') then
        begin
          if (FileExists(lDirectory + lFileName)) then
          begin
            //lFileDate  := FileDateToDateTime(FileAge(lDirectory + lFileName));
            FileAge(lDirectory + lFileName,lFileDate);
            if (ChopSeconds(lFileDate) > ChopSeconds(lRunDate)) then
              FedtOutpuFileName.Color := clYellow;
          end
          else
            FedtOutpuFileName.Color := clAqua;
        end;
      end;
    end;

    lData := TStringList.Create;
    try
      lData.CommaText := lRainfallObj.CatchmentFileData;
      if ((lData <> nil) AND (lData.Count > 0)) then
        FViewCatchmentButton.Enabled := TRUE;

      lData.Clear;
      lData.CommaText := lRainfallObj.OutputFileData;
      if ((lData <> nil) AND (lData.Count > 0)) then
        FViewOutputFileButton.Enabled := TRUE;
    finally
      FreeAndNil(lData);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function THDYP08TabSheet.LanguageHasChanged: boolean;
const OPNAME = 'THDYP08TabSheet.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure THDYP08TabSheet.ProcessCustomEvent ( AData : TObject );
const OPNAME = 'THDYP08TabSheet.ProcessCustomEvent';
begin
  inherited;
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure THDYP08TabSheet.AddInputStations ( Sender : TObject );
const OPNAME = 'THDYP08TabSheet.AddInputStations';
begin
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure THDYP08TabSheet.RunHDYP08 ( Sender : TObject );
const OPNAME = 'THDYP08TabSheet.RunHDYP08';
var
  lParamList          : TStringList;
  lFileStream         : TFileStream;
  lBatFileStream      : TFileStream;
  lInputfileName      : string;
  lDirectory          : string;
  lIndex              : integer;
  lCatchFilename      : string;
  lOutputFilename     : string;
  lMPFileName         : string;
  lDot                : integer;
  lStartupInfo        : TStartupInfo;
  lProcessInformation : TProcessInformation;
  lInProgress         : boolean;
begin
  try
    if ( ValidateInput ) then
    begin
      (FAppModules.Model.ModelData as IRainfallModelData).SaveMPDataFiles;

      lCatchFilename := Trim(FedtCatchment.Text);
      lDot := Pos('.', lCatchFilename);
      if (lDot > 0) then
        lCatchFilename := Copy(lCatchFilename, 1, lDot-1);
      lCatchFilename := lCatchFilename + '.RAN';

      lOutputFilename := Trim(FedtOutpuFileName.Text);
      lDot := Pos('.', lOutputFilename);
      if (lDot > 0) then
        lOutputFilename := Copy(lOutputFilename, 1, lDot-1);
      lOutputFilename := lOutputFilename + '.txt';

      lDirectory := (FAppModules.Model.ModelData as IRainfallModelData).DefaultDir;
      if (FileExists(lDirectory + lCatchFilename)) then
        DeleteFile(lDirectory + lCatchFilename);
      if (FileExists(lDirectory + lOutputFilename)) then
        DeleteFile(lDirectory + lOutputFilename);
      lInputfileName := FAppModules.Language.GetString('Rainfall.InputFile');

      lFileStream    := TFileStream.Create ( lDirectory + lInputfileName, fmCreate );
      lBatFileStream := TFileStream.Create ( lDirectory + 'Input.bat', fmCreate );
      lParamList     := TStringList.Create;
      try
        lParamList.Add ( lCatchFilename );
        lParamList.Add ( FCbxFirstYear.Text + ' ' + FCbxLastYear.Text );
        lParamList.Add ( FlblNrOfStationsValue.Caption );
        lParamList.Add ( lOutputFilename );
        lParamList.Add ( lCatchFilename );
        for lIndex := 0 to FStationList.Count -1 do
        begin
          lMPFileName := FStationList.Strings[lIndex];
          while (Pos(' ', lMPFileName) > 0) do
            Delete(lMPFileName, Pos(' ', lMPFileName), 1);
          lMPFileName := Copy(lMPFileName, 1, 8);
          lMPFileName := lMPFileName + '.MP';
          lParamList.Add(lMPFileName);
        end;
        lParamList.Add ( InttoStr ( FcmbPlotingQuestion.ItemIndex ) );
        lParamList.Add ( 'Key_Enter' );
        lParamList.Add ( 'Y' );
        lParamList.Add ( 'Y' );
        lParamList.SaveToStream ( lFileStream );
        lParamList.Clear;
        lParamList.Add ( '@HDYP08.exe < InputFile.txt' );
        lParamList.Add ( ' ' );
        lParamList.SaveToStream ( lBatFileStream );
      finally
        FreeAndNil ( lParamList );
        FreeAndNil ( lFileStream );
        FreeAndNil ( lBatFileStream );
      end;
      FillChar ( lStartupInfo, SizeOf ( lStartupInfo ), 0 );
      lStartupInfo.cb := SizeOf ( lStartupInfo );
      lInProgress := CreateProcess ( nil, PChar ( lDirectory + 'Input.bat' ), nil,
                                     nil, True, CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS,
                                     nil, PChar( lDirectory ), lStartupInfo, lProcessInformation );
      if ( lInProgress ) then
      begin
        WaitForSingleObject ( lProcessInformation.hProcess, INFINITE );
        CloseHandle ( lProcessInformation.hThread );
        CloseHandle ( lProcessInformation.hProcess );
      end;
      Screen.Cursor := crHourGlass;
      SaveFilesToDB(lDirectory, lCatchFilename, lOutputFilename);
      Screen.Cursor := crDefault;
      PopulateHDYP08Input;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function THDYP08TabSheet.SaveState : boolean;
const OPNAME = 'THDYP08TabSheet.SaveState';
begin
  Result := inherited SaveState;
  try
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function THDYP08TabSheet.StudyHasChanged : boolean;
const OPNAME = 'THDYP08TabSheet.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function THDYP08TabSheet.StartEndYearInputWithinLimits (AStartYear   : string;
                                                        AEndYear     : string;
                                                        AStartLimit  : integer;
                                                        AEndLimit    : integer) : boolean;
const OPNAME = 'THDYP08TabSheet.StartEndYearInputWithinLimits';
var
  lStartYear : integer;
  lEndYear   : integer;
begin
  Result := FALSE;
  try
    if (Trim(AStartYear) = '') then
      MessageDlg(FAppModules.Language.GetString('ModelRainfall.StartYear'), mtInformation, [mbOK], 0 )
    else if (Trim(AEndYear) = '') then
      MessageDlg(FAppModules.Language.GetString('ModelRainfall.EndYear'), mtInformation, [mbOK], 0 )
    else
    begin
      try
        lStartYear := StrToInt(Trim(AStartYear));
        try
          lEndYear := StrToInt(Trim(AEndYear));
          Result := ((lStartYear >= AStartLimit) AND
                     (lEndYear   <= AEndLimit));
          if (NOT Result) then
            MessageDlg(Format(FAppModules.Language.GetString('Rainfall.YearsLimit'),
                             [AStartYear, AEndYear, 'HDYP08']), mtInformation, [mbOK], 0 );
        except
          MessageDlg(FAppModules.Language.GetString('ModelRainfall.EndYear'), mtInformation, [mbOK], 0 );
        end;
      except
        MessageDlg(FAppModules.Language.GetString('ModelRainfall.StartYear'), mtInformation, [mbOK], 0 );
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function THDYP08TabSheet.ValidateInput : boolean;
const OPNAME = 'THDYP08TabSheet.ValidateInput';
begin
  Result := FALSE;
  try
    Result := StartEndYearInputWithinLimits
                (FCbxFirstYear.Text, FCbxLastYear.Text, FHDYP08StartYear, FHDYP08EndYear);
    if (Result) then
    begin
      if (Trim(FedtCatchment.Text) = '') then
        FedtCatchment.Text := FAppModules.Language.GetString('Rainfall.Zone');
      if (Trim(FedtOutpuFileName.Text) = '') then
        FedtOutpuFileName.Text := FAppModules.Language.GetString('Rainfall.ZoneFile');
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure THDYP08TabSheet.SaveFilesToDB (ADirectory      : string;
                                         ACatchFilename  : string;
                                         AOutputFilename : string);
const OPNAME = 'THDYP08TabSheet.SaveFilesToDB';
var
  lData        : TStringList;
  LCatchFileAge,
  LOutputFileAge,
  lFileDate    : TDateTime;
  lRainfallObj : IRainfallModelData;
begin
  try
    if (FileExists(ADirectory + ACatchFilename) AND FileExists(ADirectory + AOutputFilename)) then
    begin
      lRainfallObj := (FAppModules.Model.ModelData as IRainfallModelData);
      lData := TStringList.Create;
      try
        lData.LoadFromFile(ADirectory + ACatchFilename);
        lRainfallObj.CatchmentFileName := ACatchFilename;
        lRainfallObj.CatchmentFileData := lData.CommaText;

        lData.Clear;
        lData.LoadFromFile(ADirectory + AOutputFilename);
        lRainfallObj.OutputFileName := AOutputFilename;
        lRainfallObj.OutputFileData := lData.CommaText;

        FileAge(ADirectory + ACatchFilename,LCatchFileAge);
        FileAge(ADirectory + AOutputFilename,LOutputFileAge);

{         if (LCatchFileAge) > FileDateToDateTime(FileAge(ADirectory + AOutputFilename))) then
          lFileDate  := FileDateToDateTime(FileAge(ADirectory + ACatchFilename))
        else
          lFileDate  := FileDateToDateTime(FileAge(ADirectory + AOutputFilename));
        lRainfallObj.ZoneRunDate := lFileDate;}

       if (LCatchFileAge > LOutputFileAge) then
         lFileDate  := LCatchFileAge
       else
         lFileDate  := LOutputFileAge;

       lRainfallObj.ZoneRunDate := lFileDate;
      finally
        FreeAndNil(lData);
      end;
    end
    else
      ShowMessage(FAppModules.Language.GetString('Message.CatchmentZone'));
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure THDYP08TabSheet.ViewTextData (AFileName : string;
                                        AFileDate : TDateTime;
                                        AText     : TStringList);
const OPNAME = 'THDYP08TabSheet.ViewTextData';
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

procedure THDYP08TabSheet.ViewCatchment (Sender : TObject);
const OPNAME = 'THDYP08TabSheet.ViewCatchment';
var
  lText : TStringList;
  lFile : string;
  lDate : TDateTime;
begin
  try
    lText := TStringList.Create;
    try
      lText.CommaText := (FAppModules.Model.ModelData as IRainfallModelData).CatchmentFileData;
      lFile := (FAppModules.Model.ModelData as IRainfallModelData).CatchmentFileName;
      lDate := (FAppModules.Model.ModelData as IRainfallModelData).ZoneRunDate;
      ViewTextData(lFile, lDate, lText);
    finally
      FreeAndNil(lText);
    end;
  except on E: Exception do HandleError (E, OPNAME) end;
end;

procedure THDYP08TabSheet.ViewOutputFile (Sender : TObject);
const OPNAME = 'THDYP08TabSheet.ViewOutputFile';
var
  lText : TStringList;
  lFile : string;
  lDate : TDateTime;
begin
  try
    lText := TStringList.Create;
    try
      lText.CommaText := (FAppModules.Model.ModelData as IRainfallModelData).OutputFileData;
      lFile := (FAppModules.Model.ModelData as IRainfallModelData).OutputFileName;
      lDate := (FAppModules.Model.ModelData as IRainfallModelData).ZoneRunDate;
      ViewTextData(lFile, lDate, lText);
    finally
      FreeAndNil(lText);
    end;
  except on E: Exception do HandleError (E, OPNAME) end;
end;

end.

