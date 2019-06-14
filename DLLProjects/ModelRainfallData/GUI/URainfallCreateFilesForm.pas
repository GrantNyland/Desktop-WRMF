{******************************************************************************}
{*  UNIT      : Contains the class TRainfallCreateFilesForm.                  *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2005/07/26                                                    *}
{*  COPYRIGHT : Copyright © 2005 DWAF                                         *}
{******************************************************************************}

unit URainfallCreateFilesForm;

interface
{$WARN UNIT_PLATFORM OFF}

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, VCL.ExtCtrls, VCL.ComCtrls, VCL.Grids, VCL.Buttons, VCL.FileCtrl,
  UUtilities,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent,
  UChildToolbar,
  RainfallCom_TLB,
  VCL.ImgList;

type
  TRainfallCreateFilesForm = class(TAbstractForm)
  private
    FStartYear          : integer;
    FEndYear            : integer;
    FDirectory          : string;
    FStationID          : integer;
    FSplitIndex         : integer;
    FPatchID            : integer;

    FScrollBox          : TScrollBox;
    FPnlButtons         : TAbstractPanel;
    FBtnOK              : TSpeedButton;
    FBtnCancel          : TSpeedButton;
    FPnlClient          : TAbstractPanel;
    FLblStartDate       : TLabel;
    FCbxStartYear       : TAbstractComboBox;
    FLblEndDate         : TLabel;
    FCbxEndYear         : TAbstractComboBox;
    FLblDirectory       : TLabel;
    FLblDirectoryValue  : TLabel;
    FCbxDrive           : TDriveComboBox;
    FLbxDirectory       : TDirectoryListBox;
    FRAWChkBox          : TCheckBox;
    FMPChkBox           : TCheckBox;
    FPATChkBox          : TCheckBox;
    FbtnCreateDir       : TSpeedButton;
    procedure PopulateControls;
    function InputValid : Boolean;
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure FormClose (Sender     : TObject;
                         var Action : TCloseAction);
    procedure FormShow (Sender: TObject);
    procedure OnResizeForm (Sender : TObject);
    procedure OnCancelClick(Sender: TObject);
    procedure OnOKClick(Sender: TObject);
    procedure OnDirectoryChange (Sender : TObject);
    procedure OnbtnCreateDirClick(Sender : TObject);
  public
    function StudyDataHasChanged(AContext: TChangeContext;
                                 AFieldName,AOldValue,ANewValue: string): boolean; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    procedure SetData (AStationID  : integer;
                       ASplitIndex : integer;
                       APatchID    : integer);
    property StartYear  : integer read FStartYear;
    property EndYear    : integer read FEndYear;
    property Directory  : string  read FDirectory;
    property StationID  : integer read FStationID;
    property PatchID    : integer read FPatchID;

  end;

implementation

uses
  UDataSetType,
  UConstants,
  UDBConstants,
  UErrorHandlingOperations;

{******************************************************************************}
{* TRainfallCreateFilesForm                                                   *}
{******************************************************************************}

procedure TRainfallCreateFilesForm.CreateMemberObjects;
const OPNAME = 'TRainfallCreateFilesForm.CreateMemberObjects';
begin
  try
    FStartYear     := 0;
    FEndYear       := 0;

    Position := poScreenCenter;
    OnClose  := FormClose;
    OnShow   := FormShow;
    ClientHeight := 350;
    ClientWidth  := 590;

    FScrollBox := TScrollBox.Create(Self);
    with FScrollBox do
    begin
      Parent     := Self;
      Left       := 0;
      Top        := 0;
      Align      := alClient;
      BevelInner := bvNone;
      TabOrder   := 0;
    end;
    FPnlButtons := TAbstractPanel.Create(Self, FAppModules);
    with FPnlButtons do
    begin
      Parent     := FScrollBox;
      Left       := 0;
      Top        := 0;
      Height     := 30;
      Align      := alTop;
      BevelOuter := bvNone;
      TabOrder   := 0;
    end;
    FBtnOK := TSpeedButton.Create(FPnlButtons);
    FBtnOK.Glyph.LoadFromResourceName(HImagesInstance, UpperCase('CPOK'));
    with FBtnOK do
    begin
      Parent   := FPnlButtons;
      Left     := 0;
      Top      := 0;
      Width    := 30;
      Height   := 30;
      TabOrder := 0;
      ShowHint := TRUE;
      OnClick  := OnOKClick;
    end;
    FBtnCancel := TSpeedButton.Create(FPnlButtons);
    FBtnCancel.Glyph.LoadFromResourceName(HImagesInstance, UpperCase('CPCancel'));
    with FBtnCancel do
    begin
      Parent   := FPnlButtons;
      Left     := 30;
      Top      := 0;
      Width    := 30;
      Height   := 30;
      TabOrder := 1;
      ShowHint := TRUE;
      OnClick  := OnCancelClick;
    end;
    FPnlClient := TAbstractPanel.Create(Self, FAppModules);
    with FPnlClient do
    begin
      Parent     := FScrollBox;
      Left       := 0;
      Top        := 30;
      Width      := 400;
      Height     := 230;
      BevelOuter := bvNone;
      TabOrder   := 1;
    end;

    FRAWChkBox := TCheckBox.Create(Self);
    with FRAWChkBox do
    begin
      Parent   := FPnlClient;
      AutoSize := FALSE;
      Left     := 220;
      Top      := 10;
      Width    := 80;
      Caption  := FAppModules.Language.GetString('Rainfall.RAWFile');
    end;
    FMPChkBox  := TCheckBox.Create(Self);
    with FMPChkBox do
    begin
      Parent   := FPnlClient;
      AutoSize := FALSE;
      Left     := 220;
      Top      := 28;
      Width    := 80;
      Caption  := FAppModules.Language.GetString('Rainfall.MPFile');
    end;
    FPATChkBox := TCheckBox.Create(Self);
    with FPATChkBox do
    begin
      Parent   := FPnlClient;
      AutoSize := FALSE;
      Left     := 220;
      Top      := 46;
      Width    := 80;
      Caption  := FAppModules.Language.GetString('Rainfall.PATFile');
    end;
    FLblStartDate := TLabel.Create(Self);
    with FLblStartDate do
    begin
      Parent   := FPnlClient;
      AutoSize := FALSE;
      Left     := 10;
      Top      := 10;
      Width    := 110;
      Height   := 13;
    end;
    FCbxStartYear := TAbstractComboBox.Create(Self, FAppModules);
    with FCbxStartYear do
    begin
      Parent   := FPnlClient;
      Left     := 130;
      Top      := 10;
      Width    := 60;
      Height   := 21;
      Style    := csDropDownList;
      TabOrder := 0;
    end;
    FLblEndDate := TLabel.Create(Self);
    with FLblEndDate do
    begin
      Parent   := FPnlClient;
      AutoSize := FALSE;
      Left     := 10;
      Top      := 40;
      Width    := 110;
      Height   := 13;
    end;
    FCbxEndYear := TAbstractComboBox.Create(Self, FAppModules);
    with FCbxEndYear do
    begin
      Parent   := FPnlClient;
      Left     := 130;
      Top      := 40;
      Width    := 60;
      Height   := 21;
      Style    := csDropDownList;
      TabOrder := 1;
    end;
    FLblDirectory := TLabel.Create(Self);
    with FLblDirectory do
    begin
      Parent   := FPnlClient;
      AutoSize := FALSE;
      Left     := 10;
      Top      := 70;
      Width    := 110;
      Height   := 13;
    end;
    FLblDirectoryValue := TLabel.Create(Self);
    with FLblDirectoryValue do
    begin
      Parent   := FPnlClient;
      AutoSize := TRUE;
      Left     := 130;
      Top      := 70;
      Height   := 13;
    end;
    FLbxDirectory := TDirectoryListBox.Create(Self);
    with FLbxDirectory do
    begin
      Parent   := FPnlClient;
      Left     := 130;
      Top      := 115;
      Width    := 145;
      Height   := 100;
      OnChange := OnDirectoryChange;
      TabOrder := 3;
    end;
    FCbxDrive := TDriveComboBox.Create(Self);
    with FCbxDrive do
    begin
      Parent   := FPnlClient;
      Left     := 130;
      Top      := 90;
      Width    := 145;
      Height   := 19;
      DirList  := FLbxDirectory;
      TabOrder := 2;
    end;
    FbtnCreateDir := TSpeedButton.Create(FPnlButtons);
    FbtnCreateDir.Glyph.LoadFromResourceName(HImagesInstance, UpperCase('CreateDirectory'));
    with FbtnCreateDir do
    begin
      Parent   := FPnlClient;
      Left     := 275;
      Top      := 90;
      Width    := 20;
      Height   := 20;
      TabOrder := 3;
      ShowHint := True;
      OnClick  := OnbtnCreateDirClick;
    end;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallCreateFilesForm.Initialise: boolean;
const OPNAME = 'TRainfallCreateFilesForm.Initialise';
begin
  Result := FALSE;
  try
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallCreateFilesForm.FormShow(Sender: TObject);
const OPNAME = 'TRainfallCreateFilesForm.FormShow';
begin
  try
    PopulateControls;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallCreateFilesForm.SetData (AStationID : integer;
                                            ASplitIndex : integer;
                                            APatchID   : integer);
const OPNAME = 'TRainfallCreateFilesForm.SetData';
begin
  try
    FStationID  := AStationID;
    FSplitIndex := ASplitIndex;
    FPatchID    := APatchID;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallCreateFilesForm.OnResizeForm(Sender: TObject);
const OPNAME = 'TRainfallCreateFilesForm.OnResizeForm';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallCreateFilesForm.DestroyMemberObjects;
const OPNAME = 'TRainfallCreateFilesForm.DestroyMemberObjects';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallCreateFilesForm.LanguageHasChanged: boolean;
const OPNAME = 'TRainfallCreateFilesForm.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    Caption          := FAppModules.Language.GetString('Rainfall.CreatePATFiles');
    FBtnOK.Hint      := FAppModules.Language.GetString('ButtonHint.CPOK');
    FBtnCancel.Hint  := FAppModules.Language.GetString('ButtonHint.CPCancel');

    FLblStartDate.Caption   := FAppModules.Language.GetString('Rainfall.HydroStartYear') + ' :';
    FLblEndDate.Caption     := FAppModules.Language.GetString('Rainfall.HydroEndYear') + ' :';
    FLblDirectory.Caption   := FAppModules.Language.GetString('TField.OutputPath') + ' :';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallCreateFilesForm.StudyDataHasChanged (AContext: TChangeContext;
                                          AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TRainfallCreateFilesForm.StudyDataHasChanged';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallCreateFilesForm.FormClose (Sender     : TObject;
                                              var Action : TCloseAction);
const OPNAME = 'TRainfallCreateFilesForm.FormClose';
begin
  try
    Action := caFree;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallCreateFilesForm.PopulateControls;
const OPNAME = 'TRainfallCreateFilesForm.PopulateControls';
var
  lIndex        : integer;
  LUpdateUser   : boolean;
  lRainfallObj  : IRainfallModelData;
  lStation      : IStationData;
  lSplit        : IRainfallDataSplit;
  lPatch        : IPatchData;
begin
  try
    LUpdateUser := (FAppModules.User <> nil) and (FAppModules.User.UserRights in CUR_EditData);
    LUpdateUser := LUpdateUser and (FAppModules.StudyArea <> nil) and (not(FAppModules.StudyArea.ScenarioLocked));

    FBtnOK.Enabled     := LUpdateUser;
    FBtnCancel.Enabled := LUpdateUser;

    FPATChkBox.Enabled := FPatchID <> 0;
    lRainfallObj := (FAppModules.Model.ModelData as IRainfallModelData);
    FDirectory   := lRainfallObj.DefaultDir;
    if (Trim(FDirectory) = '') then
      FDirectory := GetAppDataLocalDir+'\WRCDATA\'; //ExtractFilePath(ApplicationExeName) + 'wrcdata\';
    FLbxDirectory.Directory := FDirectory;
    if (FStationID <> 0) then
    begin
      lStation := lRainfallObj.GetStationDataByID(FStationID);
      if (lStation <> nil) then
      begin
        if (FSplitIndex <> -1) then
          lSplit := lStation.GetSplitWithIndex(FSplitIndex);
        if (FPatchID <> 0) then
        begin
          lPatch := lStation.GetPatchWithID(FPatchID);
          if (lPatch <> nil) then
          begin
            FStartYear := lPatch.RainfallData.HydroStartYear;
            FEndYear   := lPatch.RainfallData.HydroEndYear;
          end;
        end
        else
        begin
          if (lSplit <> nil) then
          begin
            FStartYear := lSplit.HydroStartYear;
            FEndYear   := lSplit.HydroEndYear;
          end
          else
          begin
            FStartYear := lStation.RainfallData.HydroStartYear;
            FEndYear   := lStation.RainfallData.HydroEndYear;
          end;
        end;
        FCbxStartYear.Clear;
        FCbxEndYear.Clear;
        for lIndex := FStartYear to FEndYear do
        begin
          FCbxStartYear.Items.Add(IntToStr(lIndex));
          FCbxEndYear.Items.Add(IntToStr(lIndex));
        end;
        FCbxStartYear.ItemIndex := 0;
        FCbxEndYear.ItemIndex   := FCbxEndYear.Items.Count - 1;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallCreateFilesForm.InputValid : Boolean;
const OPNAME = 'TRainfallCreateFilesForm.InputValid';
begin
  Result := FALSE;
  try
    Result := TRUE;
    if (FCbxStartYear.ItemIndex >= 0) then
      FStartYear := StrToInt(FCbxStartYear.Text)
    else
    begin
      Result := FALSE;
      ShowMessage(FAppModules.Language.GetString('ChangeLists.PleaseSelectStartYear'));
    end;
    if (Result) then
    begin
      if (FCbxEndYear.ItemIndex >= 0) then
        FEndYear := StrToInt(FCbxEndYear.Text)
      else
      begin
        Result := FALSE;
        ShowMessage(FAppModules.Language.GetString('ChangeLists.PleaseSelectEndYear'));
      end;
    end;
    if (Result) then
    begin
      if (FEndYear < FStartYear) then
      begin
        Result := FALSE;
        ShowMessage(FAppModules.Language.GetString('Rainfall.EndYearBeforeStartYear'));
      end;
    end;
    if (Result) then
    begin
      if (Trim(FDirectory) = '') OR (NOT SysUtils.DirectoryExists(FDirectory)) then
      begin
        Result := FALSE;
        ShowMessage(FAppModules.Language.GetString('Rainfall.InvalidOutputDirectory'));
      end
      else
      begin
        if (Copy(FDirectory, Length(FDirectory), 1) <> '\') then
          FDirectory := FDirectory + '\';
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallCreateFilesForm.OnOKClick(Sender: TObject);
const OPNAME = 'TRainfallCreateFilesForm.OnOKClick';
var
  lRainfallObj  : IRainfallModelData;
  lStation      : IStationData;
  lPatch        : IPatchData;
begin
  try
    if (InputValid) then
    begin
      lRainfallObj := (FAppModules.Model.ModelData as IRainfallModelData);
      if (FStationID <> 0) then
      begin
        lStation := lRainfallObj.GetStationDataByID(FStationID);
        if (lStation <> nil) then
        begin
          if (FPatchID <> 0) then
          begin
            lPatch := lStation.GetPatchWithID(FPatchID);
            if (lPatch <> nil) then
            begin
              if (FRAWChkBox.Checked) then
                lPatch.SaveRAWFile(FStartYear, FEndYear, FDirectory);
              if (FMPChkBox.Checked) then
                lPatch.SaveMPFile(FStartYear, FEndYear, FDirectory);
              if (FPATChkBox.Checked) then
                lPatch.SavePATFile(FStartYear, FEndYear, FDirectory);
            end;
          end
          else
          begin
            if (FRAWChkBox.Checked) then
              lStation.SaveRAWFile(FStartYear, FEndYear, FDirectory);
            if (FMPChkBox.Checked) then
              lStation.SaveMPFile(FStartYear, FEndYear, FDirectory);
          end;
        end;
      end;
      ModalResult := mrOk;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallCreateFilesForm.OnbtnCreateDirClick(Sender : TObject);
const OPNAME = 'TRainfallCreateFilesForm.OnbtnCreateDirClick';
var
  LFolder : string;
begin
  try
    LFolder := InputBox('Enter New Directory','New Directory','');
    if (LFolder <> '') then
      if not SysUtils.DirectoryExists(FDirectory+'\'+LFolder) then
      begin
        SysUtils.ForceDirectories(FDirectory+'\'+LFolder);
        FDirectory := FDirectory+'\'+LFolder;
        FLbxDirectory.Directory := FDirectory;;
      end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;
procedure TRainfallCreateFilesForm.OnCancelClick(Sender: TObject);
const OPNAME = 'TRainfallCreateFilesForm.OnCancelClick';
begin
  try
    ModalResult := mrCancel;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallCreateFilesForm.OnDirectoryChange (Sender: TObject);
const OPNAME = 'TRainfallCreateFilesForm.OnDirectoryChange';
begin
  try
    FDirectory                 := FLbxDirectory.Directory;
    FLblDirectoryValue.Caption := FDirectory;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
