//
//  UNIT      : Contains TAppModulesConstructionGeneric Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/02/07
//  COPYRIGHT : Copyright © 2002 DWAF
//
unit UAppModulesConstructionGeneric;

interface

uses
  VCL.Forms,
  VCL.Dialogs,
  Classes,
  UAbstractObject,
  UAbstractComponent,
  UAbstractModelObjects,
  USystemModelLinkClasses,
  UAppModulesWithDatabase,
  UHydrologyFileTypeManager,
  UDbTablePropertyManager,
  UDbTableFieldsList;

type
  TAppModulesConstructionGeneric = class(TAppModulesWithDatabase)
  protected
    FLanguageManager          : TAbstractLanguageManager;
    FHydrologyFileTypeManager : THydrologyFileTypeManager;
    FDBTablePropertyManager   : TDBTablePropertyManager;
    FDbTableFieldsList        : TTableFieldsDefList;
    FFieldProperties          : TAbstractFieldsProperties;
    FUser                     : TAbstractUser;
    FAccessControlManager     : TAbstractAccessControlManager;
    FStudyDocumentManager     : TAbstractStudyDocumentManager;
    FStudyAreaManager         : TAbstractStudyAreaManager;
    FStudyArea                : TAbstractStudyArea;
    FModel                    : TAbstractModelManager;
    FMainForm                 : TAbstractMainFormManager;
    FFileSaveDialog           : TSaveDialog;
    FPrintManager             : TAbstractPrintManager;
    FChangeManager            : TAbstractChangeManager;
    FMetaDataManager          : TAbstractMetaDataManager;
    FWeatherEventsManager     : TAbstractWeatherEventsManager;
    FScenarioLockManager      : TAbstractScenarioLockManager;
    //FLicenceManager           : TAbstractLicenceManager;

    // Initialisation methods.
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure CreateLanguageManager; virtual;
    procedure CreateFieldProperties; virtual;
    procedure CreateAccessControlManager; virtual;
    procedure CreateStudyDocumentManager; virtual;
    procedure CreateStudyAreaManager; virtual;
    procedure CreatePrintManager; virtual;
    procedure CreateChangeManager; virtual;
    procedure CreateMetaDataManager; virtual;
    procedure CreateWeatherEventsManager; virtual;
    procedure LoadNewModel(ANewStudyArea: TAbstractStudyArea);virtual;
    procedure UnLoadCurrentModel; virtual;
    function SaveUser: boolean;
    function SaveStudy: boolean;
  private
    procedure SetToolBarVisibility;
    procedure SetStudyPanelVisibility;
    procedure CloseApplication;
    procedure DoLaunchExampleFiles;
    procedure DoLaunchReleseNote;
    procedure DoLaunchReadMe;
    procedure DoOpenAnyFile;
   public
    function ResetState: boolean; virtual;
    function Initialise: boolean; override;
    function LanguageHasChanged: boolean; override;
    function Language: TAbstractLanguage; override;
    function HydrologyFileType: TAbstractHydrologyFileType; override;
    function FieldProperties : TAbstractFieldsProperties; override;
    function User: TAbstractUser; override;
    function AccessControlManager: TAbstractAccessControlManager; override;
    function PrintManager : TAbstractPrintManager; override;
    function Changes : TAbstractChangeManager; override;
    function MetaData : TAbstractMetaDataManager; override;
    function WeatherEvents : TAbstractWeatherEventsManager; override;
    function ScenarioLockManager:TAbstractScenarioLockManager; override;
    //function LicenceManager:TAbstractLicenceManager; override;
    function StudyAreaManager:TAbstractStudyAreaManager; override;
    function DBTablePropertyManager:TAbstractDBTablePropertyManager; override;
    function DBTableFieldsDefList:TAbstractTableFieldsDefList; override;

    function ProcessAutoLogon: boolean;
    procedure DoLogOff; override;
    procedure DoLogOn; override;
    procedure DoUserAdministration; override;
    function StudyDocumentManager: TAbstractStudyDocumentManager; override;
    function StudyArea : TAbstractStudyArea; override;
    procedure SetStudyArea(ANewStudyArea: TAbstractStudyArea); override;
    function InitialiseStudy: boolean;
    procedure SelectStudyArea; override;
    procedure SelectStudyDetails(AData: TObject); override;
    function Model: TAbstractModelManager; override;
    function AddMenuItem(AMainMenuKeys: array of string; ASortWeight: integer; AEvent: integer; AData: TObject): TObject; override;
    function SetMenuItem(AMainMenuKeys: array of string; AnAction: TMenuSetAction; AStatusReason: string = ''): boolean; override;
    function SetMenuItemCaption(AMainMenuKeys: array of string; ACaption : string): boolean; override;
    function GetMenuItemProperties(AMenuKeys: array of string) : TSetOfMenuAction; override;

    function MainForm: TAbstractMainFormManager; override;
    function GetExportFilename(const ADefaultExt, AFilter: string; var AFileName: string): boolean; override;
    function ProcessEvent(AEventType: integer; AData: TObject): boolean; override;
  end;

  var
  GOCXMode: boolean;

implementation

uses
  Windows,
  Messages,
  VCL.Controls,
  UITypes,
  SysUtils,
  ShellApi,
  UStudyArea,
  UDataSetType,
  UHelpContexts,
  //UFunctionTimer,
  USQLDatabaseLayer,
  //USystemModelManager,
  UScenarioLockManager,
  //ULicenceManager,
  UMainMenuEventType,
  USystemModelDatasetConstructor,
  UErrorHandlingOperations;

procedure TAppModulesConstructionGeneric.CreateMemberObjects;
const OPNAME = 'TAppModulesConstructionGeneric.CreateMemberObjects';
begin
  //ProcessFunctionCall(OPNAME);
  try

    // Call the ancestor.
    inherited CreateMemberObjects;

    if FCanProceed then
    begin
      // Create the system objects.
      FHydrologyFileTypeManager := THydrologyFileTypeManager.Create(Self);
      FDBTablePropertyManager   := TDBTablePropertyManager.Create(Self);
      FDbTableFieldsList        := TTableFieldsDefList.Create(Self);

      FUser := nil;
      FStudyArea := TStudyArea.Create(Self);
      FScenarioLockManager := TScenarioLockManager.Create(Self);
      //FLicenceManager      := TLicenceManager.Create(Self);
      TSQLDatabaseLayer(FDatabase).AddDataSetConstructor(TSystemModelDatasetConstructor.Create(self));
      FFileSaveDialog := TSaveDialog.Create(nil);

      // Create DLL based member objects.
      CreateLanguageManager;
      CreateFieldProperties;
      CreateStudyDocumentManager;
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
  //ProcessFunctionReturn(OPNAME);
end;

function TAppModulesConstructionGeneric.DBTableFieldsDefList: TAbstractTableFieldsDefList;
const OPNAME = 'TAppModulesConstructionGeneric.DBTableFieldsDefList';
begin
  Result := nil;
  try
    Result := FDbTableFieldsList;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAppModulesConstructionGeneric.DBTablePropertyManager: TAbstractDBTablePropertyManager;
const OPNAME = 'TAppModulesConstructionGeneric.DBTablePropertyManager';
begin
  Result := nil;
  try
    Result := FDBTablePropertyManager;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAppModulesConstructionGeneric.DestroyMemberObjects;
const OPNAME = 'TAppModulesConstructionGeneric.DestroyMemberObjects';
begin
  try
    SaveUser;
    SaveStudy;
    FreeAndNil(FStudyArea);
    FreeAndNil(FUser);
    FreeAndNil(FHydrologyFileTypeManager);
    FreeAndNil(FDBTablePropertyManager);
    FreeAndNil(FDBTablePropertyManager);
    FreeAndNil(FFileSaveDialog);
    FreeAndNil(FScenarioLockManager);
    //FreeAndNil(FLicenceManager);

    DestroyDLLObject(TAbstractAppObject(FLanguageManager), OPNAME);
    DestroyDLLObject(TAbstractAppObject(FFieldProperties), OPNAME);
    DestroyDLLObject(TAbstractAppObject(FAccessControlManager), OPNAME);
    DestroyDLLObject(TAbstractAppObject(FStudyAreaManager), OPNAME);
    DestroyDLLObject(TAbstractAppObject(FModel), OPNAME);
    DestroyDLLObject(TAbstractAppObject(FChangeManager), OPNAME);
    DestroyDLLObject(TAbstractAppObject(FMetaDataManager), OPNAME);
    DestroyDLLObject(TAbstractAppObject(FWeatherEventsManager), OPNAME);

    // Call the ancestor.
    inherited DestroyMemberObjects;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAppModulesConstructionGeneric.CreateLanguageManager;
const OPNAME = 'TAppModulesConstructionGeneric.CreateLanguageManager';
begin
  try
    CreateDLLObject(ExtractFilePath(ApplicationExeName) + 'bin\LanguageManager.dll',
      TAbstractAppObject(FLanguageManager), self, True, OPNAME);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TAppModulesConstructionGeneric.CreateFieldProperties;
const OPNAME = 'TAppModulesConstructionGeneric.CreateFieldProperties';
begin
  try
    CreateDLLObject(ExtractFilePath(ApplicationExeName) + 'bin\FieldProperties.dll',
      TAbstractAppObject(FFieldProperties), self, False, OPNAME);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TAppModulesConstructionGeneric.CreateAccessControlManager;
const OPNAME = 'TAppModulesConstructionGeneric.CreateAccessControlManager';
begin
  try
    CreateDLLObject(ExtractFilePath(ApplicationExeName) + 'bin\AccessControl.dll',
      TAbstractAppObject(FAccessControlManager), self, True, OPNAME);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TAppModulesConstructionGeneric.CreateStudyDocumentManager;
const OPNAME = 'TAppModulesConstructionGeneric.CreateStudyDocumentManager';
begin
  try
    CreateDLLObject(ExtractFilePath(ApplicationExeName) + 'bin\StudyDocuments.dll',
      TAbstractAppObject(FStudyDocumentManager), self, False, OPNAME);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TAppModulesConstructionGeneric.CreateStudyAreaManager;
const OPNAME = 'TAppModulesConstructionGeneric.CreateStudyAreaManager';
begin
  try
    CreateDLLObject(ExtractFilePath(ApplicationExeName) + 'bin\StudySelection.dll',
      TAbstractAppObject(FStudyAreaManager), self, True, OPNAME);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TAppModulesConstructionGeneric.CreatePrintManager;
const OPNAME = 'TAppModulesConstructionGeneric.CreatePrintManager';
begin
  try
    CreateDLLObject(ExtractFilePath(ApplicationExeName) + 'bin\PrintManager.dll',
      TAbstractAppObject(FPrintManager), self, False, OPNAME);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TAppModulesConstructionGeneric.CreateChangeManager;
const OPNAME = 'TAppModulesConstructionGeneric.CreateChangeManager';
begin
  try
    CreateDLLObject(ExtractFilePath(ApplicationExeName) + 'bin\Changes.dll',
      TAbstractAppObject(FChangeManager), self, False, OPNAME);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TAppModulesConstructionGeneric.CreateMetaDataManager;
const OPNAME = 'TAppModulesConstructionGeneric.CreateMetaDataManager';
begin
  try
    CreateDLLObject(ExtractFilePath(ApplicationExeName) + 'bin\MetaData.dll',
      TAbstractAppObject(FMetaDataManager), self, False, OPNAME);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TAppModulesConstructionGeneric.CreateWeatherEventsManager;
const OPNAME = 'TAppModulesConstructionGeneric.CreateWeatherEventsManager';
begin
  try
    CreateDLLObject(ExtractFilePath(ApplicationExeName) + 'bin\WeatherEvents.dll',
      TAbstractAppObject(FWeatherEventsManager), self, False, OPNAME);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TAppModulesConstructionGeneric.Initialise: boolean;
const OPNAME = 'TAppModulesConstructionGeneric.Initialise';
begin
  Result := False;
  try

    // Connect to the database.
    if inherited Initialise then
    begin

      // This code must be removed. Whereever Global databasename is used should be replaced
      // with a request to a clean query that will have the databasename set.
      if Assigned(GlobalData()) then
        GlobalData.SetDatabaseName(FDatabase.Connection);

      // Initialise the member objects.
      FDBTablePropertyManager.Initialise;
      FDbTableFieldsList.Initialise;
      if (not Assigned(FLanguageManager)) or FLanguageManager.Initialise then
      begin
        if FHydrologyFileTypeManager.Initialise then
        begin
          if (not Assigned(FFieldProperties)) or FFieldProperties.Initialise then
          begin
            if (not Assigned(FPrintManager)) or FPrintManager.Initialise then
            begin
              if (NOT Assigned(FChangeManager)) OR FChangeManager.Initialise then
              begin
                if (NOT Assigned(FMetaDataManager)) OR FMetaDataManager.Initialise then
                begin
                  if (NOT Assigned(FWeatherEventsManager)) OR FWeatherEventsManager.Initialise then
                  begin
                    if ProcessAutoLogon then
                    begin
                      if InitialiseStudy then
                      begin
                        Result := True;
                      end;
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAppModulesConstructionGeneric.ProcessAutoLogon: boolean;
const OPNAME = 'TAppModulesConstructionGeneric.ProcessAutoLogon';
var LAutoLogon: string;
begin
  Result := False;
  try
    LAutoLogon := IniFile.ReadString('USER', 'AutoLogon', 'Y');
    if Assigned(AccessControlManager()) and (UpperCase(Trim(LAutoLogon)) = 'Y') then
    begin
      AccessControlManager.DoAutoLogOn(FUser);
      if Assigned(FUser) then
      begin
        if Assigned(Self.MainForm()) then
          FStudyAreaManager.SetSelectStudyAreaEnabled(FUser.LoggedOn);
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAppModulesConstructionGeneric.DoLogOn;
const OPNAME = 'TAppModulesConstructionGeneric.DoLogOn';
begin
  try
    if Assigned(AccessControlManager()) then
      AccessControlManager.DoLogOn(FUser);
    FStudyAreaManager.SetSelectStudyAreaEnabled(FUser.LoggedOn);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAppModulesConstructionGeneric.DoLogOff;
const OPNAME = 'TAppModulesConstructionGeneric.DoLogOff';
begin
  try
    if Assigned(AccessControlManager()) then
    begin
      if(FModel <> nil) then
        LoadModel(mmModelSystem);

      AccessControlManager.DoLogOff(FUser);
      FStudyAreaManager.SetSelectStudyAreaEnabled(FUser.LoggedOn);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAppModulesConstructionGeneric.LanguageHasChanged: boolean;
const
  OPNAME = 'TAppModulesConstructionGeneric.LanguageHasChanged';
begin
  Result := True;
  try
    if Assigned(FModel) then
      Result := FModel.LanguageHasChanged;
    if Assigned(FChangeManager) then
      Result := FChangeManager.LanguageHasChanged;
    if Assigned(FMetaDataManager) then
      Result := FMetaDataManager.LanguageHasChanged;
    if Assigned(FWeatherEventsManager) then
      Result := FWeatherEventsManager.LanguageHasChanged;
    if not GOCXMode then
    begin
      if Result and Assigned(FMainForm) then
        Result := FMainForm.LanguageHasChanged;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TAppModulesConstructionGeneric.AddMenuItem(
  AMainMenuKeys: array of string; ASortWeight: integer; AEvent: integer; AData: TObject): TObject;
const OPNAME = 'TAppModulesConstructionGeneric.AddMenuItem';
begin
  Result := nil;
  try
    if Assigned(FMainForm) and Assigned(FMainForm.Menu) then
      Result := TAbstractMainMenu(FMainForm.Menu).AddMenuItem(AMainMenuKeys, ASortWeight, AEvent, AData);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAppModulesConstructionGeneric.SetMenuItem(
  AMainMenuKeys: array of string; AnAction: TMenuSetAction; AStatusReason: string = ''): boolean;
const OPNAME = 'TAppModulesConstructionGeneric.SetMenuItem';
begin
  Result := False;
  try
    if Assigned(FMainForm) and Assigned(FMainForm.Menu) then
      Result := TAbstractMainMenu(FMainForm.Menu).SetMenuItem(AMainMenuKeys, AnAction, AStatusReason);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAppModulesConstructionGeneric.Language: TAbstractLanguage;
const OPNAME = 'TAppModulesConstructionGeneric.Language';
begin
  Result := nil;
  try
    if Assigned(FLanguageManager) and Assigned(FLanguageManager.Language()) then
      Result := FLanguageManager.Language;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAppModulesConstructionGeneric.HydrologyFileType: TAbstractHydrologyFileType;
const OPNAME = 'TAppModulesConstructionGeneric.HydrologyFileType';
begin
  Result := nil;
  try
    if Assigned(FHydrologyFileTypeManager) and Assigned(FHydrologyFileTypeManager.HydrologyFileType) then
      Result := FHydrologyFileTypeManager.HydrologyFileType;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAppModulesConstructionGeneric.User: TAbstractUser;
const OPNAME = 'TAppModulesConstructionGeneric.User';
begin
  Result := nil;
  try
    Result := FUser;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAppModulesConstructionGeneric.StudyArea: TAbstractStudyArea;
const OPNAME = 'TAppModulesConstructionGeneric.StudyArea';
begin
  Result := nil;
  try
    Result := FStudyArea;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAppModulesConstructionGeneric.AccessControlManager: TAbstractAccessControlManager;
const OPNAME = 'TAppModulesConstructionGeneric.AccessControlManager';
begin
  Result := nil;
  try
    Result := FAccessControlManager;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAppModulesConstructionGeneric.StudyDocumentManager: TAbstractStudyDocumentManager;
const OPNAME = 'TAppModulesConstructionGeneric.StudyDocumentManager';
begin
  Result := nil;
  try
    Result := FStudyDocumentManager;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAppModulesConstructionGeneric.SetStudyArea(ANewStudyArea: TAbstractStudyArea);
const OPNAME = 'TAppModulesConstructionGeneric.SetStudyArea';
begin
  try
    FreeAndNil(FStudyArea);
    FStudyArea := ANewStudyArea;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAppModulesConstructionGeneric.Model: TAbstractModelManager;
const OPNAME = 'TAppModulesConstructionGeneric.Model';
begin
  Result := nil;
  try
    Result := FModel;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAppModulesConstructionGeneric.MainForm: TAbstractMainFormManager;
const OPNAME = 'TAppModulesConstructionGeneric.MainForm';
begin
  Result := nil;
  try
    Result := FMainForm;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAppModulesConstructionGeneric.FieldProperties: TAbstractFieldsProperties;
const OPNAME = 'TAppModulesConstructionGeneric.FieldProperties';
begin
  Result := nil;
  try
    Result := FFieldProperties;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAppModulesConstructionGeneric.GetExportFilename(const ADefaultExt, AFilter: string; var AFileName: string): boolean;
const OPNAME = 'TAppModulesConstructionGeneric.GetExportFilename';
begin
  Result := False;
  try
    FFileSaveDialog.DefaultExt := ADefaultExt;
    FFileSaveDialog.Filter     := AFilter;
    FFileSaveDialog.FileName   := AFileName;
    FFileSaveDialog.Options := [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing];
    if FFileSaveDialog.Execute then
    begin
      AFileName := FFileSaveDialog.FileName;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAppModulesConstructionGeneric.SetMenuItemCaption(
  AMainMenuKeys: array of string; ACaption: string): boolean;
const OPNAME = 'TAppModulesConstructionGeneric.SetMenuItemCaption';
begin
  Result := False;
  try
    if Assigned(FMainForm) and Assigned(FMainForm.Menu) then
      Result := TAbstractMainMenu(FMainForm.Menu).SetMenuItemCaption(AMainMenuKeys, ACaption);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAppModulesConstructionGeneric.InitialiseStudy: boolean;
const OPNAME = 'TAppModulesConstructionGeneric.InitialiseStudy';
      ErrMessage = 'Study selection cannot be done when no user is currently logged.'+ #10#13+
                   ' It appears as if your INI file or the database is corrupted.';
var
  LAutoStudy: string;
  LStudyArea: TAbstractStudyArea;
begin
  Result := False;
  try

    LAutoStudy := Self.IniFile.ReadString('STUDY','AutoStudy','');
    if (UpperCase(Trim(LAutoStudy)) = 'Y') then
    begin
      if (not Assigned(FUser)) or (not FUser.LoggedOn) then
      begin
        MessageDlg(ErrMessage,mtError,[mbOK],0);
      end
      else
      begin
        LStudyArea := nil;
        try

          if Assigned(FStudyAreaManager) then
            if FStudyAreaManager.AutoSelectStudy(self, LStudyArea) then
            begin
              LoadNewModel(LStudyArea);
              if Assigned(FMainForm) then
                FMainForm.RefreshState;
            end;
        finally
          LStudyArea.Free;
        end;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAppModulesConstructionGeneric.SelectStudyArea;
const OPNAME = 'TAppModulesConstructionGeneric.SelectStudyArea';
var LStudyArea: TAbstractStudyArea;
    //LCursor:TCursor;

begin
  try
    if (not Assigned(FUser)) or (not FUser.LoggedOn) then
      raise Exception.Create('Study selection cannot be done when no user is currently logged.'+
            ' It appears as if your INI file or the database is corrupted.');

    //LCursor := Screen.Cursor;
    //Screen.Cursor := crHourGlass;
    //try
      LStudyArea := nil;
      try
        if Assigned(FStudyAreaManager) then
          if FStudyAreaManager.SelectStudy(self, LStudyArea) then
          begin
             LoadNewModel(LStudyArea);
             if Assigned(FMainForm) then
               FMainForm.RefreshState;
          end;
      finally
        LStudyArea.Free;
      end;
    //finally
    //  Screen.Cursor := LCursor;
    //end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAppModulesConstructionGeneric.SelectStudyDetails(AData: TObject);
const OPNAME = 'TAppModulesConstructionGeneric.SelectStudyDetails';
var
  LStudyArea: TAbstractStudyArea;
begin
  try
    if not Assigned(AData) then
      raise Exception.Create('Study labels message object is not assigned');
    if not (AData.ClassName = TStudyLabels.ClassName) then
      raise Exception.Create('Study labels message is of incorrect type');
    if (not Assigned(FUser)) or (not FUser.LoggedOn) then
      raise Exception.Create('Study selection cannot be done when no user is currently logged.'+
            ' It appears as if your INI file or the database is corrupted.');

    LStudyArea := nil;
    try
      if Assigned(FStudyAreaManager) then
        if FStudyAreaManager.SelectStudyDetails(self, LStudyArea, TStudyLabels(AData)) then
        begin
          LoadNewModel(LStudyArea);
          if Assigned(FMainForm) then
            FMainForm.RefreshState;
        end;
    finally
      LStudyArea.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAppModulesConstructionGeneric.LoadNewModel(ANewStudyArea: TAbstractStudyArea);
const OPNAME = 'TAppModulesConstructionGeneric.LoadNewModel';
begin
  try
    if Assigned(FStudyArea) then
      FScenarioLockManager.ReleaseLock;
    if ANewStudyArea.Selected then
    begin
      if (not Assigned(FStudyArea)) or
         (ANewStudyArea.ModelSubCode <> FStudyArea.ModelSubCode)then
      begin
        if (not Assigned(FStudyArea)) then
          SetStudyArea(TStudyArea.Create(Self));
        FStudyArea.AssignFrom(ANewStudyArea);
        if      (ANewStudyArea.ModelCode = CYield) then LoadModel(mmModelYield)
        else if (ANewStudyArea.ModelCode = CRainfall) then LoadModel(mmModelRainfallData)
        else if (ANewStudyArea.ModelCode = CPlanning) then LoadModel(mmModelPlanning)
        else if (ANewStudyArea.ModelCode = CPreProcessor) then LoadModel(mmBroadScalePreprocessor)
        else if (ANewStudyArea.ModelCode = CHydrology) then LoadModel(mmModelHydrology)
        else if (ANewStudyArea.ModelCode = CYRC) then LoadModel(mmModelYRC)
        else if (ANewStudyArea.ModelCode = CDailyDiversion) then LoadModel(mmDailyDiversionPreprocessor)
        else if (ANewStudyArea.ModelCode = CIFRPreProcessor) then LoadModel(mmIFRPreprocessor)
        else if (ANewStudyArea.ModelCode = CDamSedimentation) then LoadModel(mmDamSedimentation)
        else if (ANewStudyArea.ModelCode = CStomsa) then LoadModel(mmStomsa)
        else if (ANewStudyArea.ModelCode = CRWH) then LoadModel(mmRWH)
        else if (ANewStudyArea.ModelCode = CDDTS) then LoadModel(mmDDTS);
      end
      else
      begin
        FStudyArea.AssignFrom(ANewStudyArea);
        LoadModel(mmModelUnchanged)
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAppModulesConstructionGeneric.SaveStudy: boolean;
const OPNAME = 'TAppModulesConstructionGeneric.SaveStudy';
begin
  Result := False;
  try
    if Assigned(FIniFile) and Assigned(StudyArea()) and (Trim(StudyArea.StudyLabel) <> '') then
    begin
      FIniFile.WriteString('STUDY', 'Study',    StudyArea.StudyLabel);
      FIniFile.WriteString('STUDY', 'Model',    StudyArea.ModelLabel);
      FIniFile.WriteString('STUDY', 'SubArea',  StudyArea.SubAreaLabel);
      FIniFile.WriteString('STUDY', 'Scenario', StudyArea.ScenarioLabel);
      if User.AutoSelectStudy then
        FIniFile.WriteString('STUDY', 'AutoStudy', 'Y')
      else
        FIniFile.WriteString('STUDY', 'AutoStudy', 'N');
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAppModulesConstructionGeneric.SaveUser: boolean;
const OPNAME = 'TAppModulesConstructionGeneric.SaveUser';
begin
  Result := False;
  try
    if Assigned(FIniFile) and Assigned(User()) and (Trim(User.UserId) <> '') then
    begin
      FIniFile.WriteString('USER', 'UserID',   User.UserId);
      FIniFile.WriteString('USER', 'Password', User.Password);
      FIniFile.WriteString('USER', 'Language', User.PreferedLanguage);
      if User.AutoLogOn then
        FIniFile.WriteString('USER', 'AutoLogon', 'Y')
      else
        FIniFile.WriteString('USER', 'AutoLogon', 'N');
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAppModulesConstructionGeneric.GetMenuItemProperties(
  AMenuKeys: array of string): TSetOfMenuAction;
const OPNAME = 'TAppModulesConstructionGeneric.GetMenuItemProperties';
begin
  Result := [];
  try
    if Assigned(FMainForm) and Assigned(FMainForm.Menu) then
      Result := TAbstractMainMenu(FMainForm.Menu).GetMenuItemProperties(AMenuKeys);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAppModulesConstructionGeneric.ProcessEvent(AEventType: integer; AData: TObject): boolean;
const OPNAME = 'TAppModulesConstructionGeneric.ProcessEvent';
begin
  Result := False;
  try
    case AEventType of
      CmeViewToolBar         : SetToolBarVisibility;
      CmeViewStudyPanel      : SetStudyPanelVisibility;
      CmeViewReset           : ResetState;
      CmeSelectStudy         : SelectStudyArea;
      CmeSelectStudyDetails  : SelectStudyDetails(AData);
      CmeLogOn               :
                               begin
                                 DoLogOn;
                                 if Assigned(FUser) and FUser.LoggedOn then
                                   SelectStudyArea;
                               end;
      CmeLogOff              : DoLogOff;
      CmeUserAdministration  : DoUserAdministration;
      CmePrintSettings       : if Assigned(FPrintManager) then FPrintManager.DoPrintSettings;
      CmePrintPreview        : if Assigned(FPrintManager) then FPrintManager.DoPrintPreview(AData);
      CmePrint               : if Assigned(FPrintManager) then FPrintManager.DoPrint(AData);
      CmeLaunchStudyReport   : if Assigned(FStudyDocumentManager) then FStudyDocumentManager.LaunchReport(AData);

      CmeChangeListCreate        : if Assigned(FChangeManager) then FChangeManager.DoCreateNewChangeList;
      CmeChangeListDelete        : if Assigned(FChangeManager) then FChangeManager.DoDeleteChangeList(-1);
      CmeChangeListCopy          : if Assigned(FChangeManager) then FChangeManager.DoCopyChangeList(-1);
      CmeChangeElementMoveUp     : if Assigned(FChangeManager) then FChangeManager.DoMoveUpChangeElement(0,0,TRUE);
      CmeChangeElementMoveDown   : if Assigned(FChangeManager) then FChangeManager.DoMoveDownChangeElement(0,0,TRUE);
      CmeChangeElementActivate   : if Assigned(FChangeManager) then FChangeManager.DoActivateChangeElement(0,0,TRUE);
      CmeChangeElementDeactivate : if Assigned(FChangeManager) then FChangeManager.DoDeactivateChangeElement(0,0,TRUE);
      CmeChangeListApply         : if Assigned(FChangeManager) then FChangeManager.DoApplyChangeList(-1);
      CmeChangeListImport        : if Assigned(FChangeManager) then FChangeManager.DoImportChangeList(-1);
      CmeChangeListExport        : if Assigned(FChangeManager) then FChangeManager.DoExportChangeList(-1);
      CmeChangeGroupCreate       : if Assigned(FChangeManager) then FChangeManager.DoCreateNewChangeList;
      CmeChangeGroupDelete       : if Assigned(FChangeManager) then FChangeManager.DoDeleteChangeList(-1);

      CmeExit                 : CloseApplication;
      {$IFDEF BuildNetworkworkVisualiser}
      CmeWRMFHelpContents     : Application.HelpContext(10010);
      {$ELSE}
      CmeWRMFHelpContents     : Application.HelpContext(HC_Introduction);
      {$ENDIF}
      CmeWRMFHelpReleaseNote  : DoLaunchReleseNote;
      CmeWRMFHelpReadMe       : DoLaunchReadMe;
      CmeExampleFile          : DoLaunchExampleFiles;

      CmeAddReport            : if Assigned(FStudyDocumentManager) then FStudyDocumentManager.AddReport;
      CmeDeleteReport         : if Assigned(FStudyDocumentManager) then FStudyDocumentManager.DeleteReport;
      CmeEditReport           : if Assigned(FStudyDocumentManager) then FStudyDocumentManager.EditReport;

      CmeHelpAbout            : DoAbout;
      CmeOpenFile             : DoOpenAnyFile;
      CmeHelpWhatIsThis       : if Assigned(FMainForm) then FMainForm.ProcessEvent(AEventType,AData);
      CmeSetHelpWhatIsThisOff : if Assigned(FMainForm) then FMainForm.ProcessEvent(AEventType,AData);
      {CmeLicenceModels        :
        begin
          if Assigned(FLicenceManager) then
          begin
            FLicenceManager.LicenceModels;
            if Assigned(FUser) then FUser.ReloadData;
          end;
        end;}

    else
      if Assigned(FModel) then
        Result := FModel.ProcessEvent(AEventType,AData)
      else
      // If this point is reached the event was not handled.
      raise Exception.CreateFmt('Unknown event type [%d]. ', [integer(AEventType)]);
    end;//case

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAppModulesConstructionGeneric.SetToolBarVisibility;
const OPNAME = 'TAppModulesConstructionGeneric.SetToolBarVisibility';
begin
  try
    FMainForm.ToolBarVisible :=
      not FMainForm.ToolBarVisible;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TAppModulesConstructionGeneric.SetStudyPanelVisibility;
const OPNAME = 'TAppModulesConstructionGeneric.SetStudyPanelVisibility';
begin
  try
    if Assigned(FStudyArea) then
    begin
      FMainForm.StudyPanelVisible :=
        not FMainForm.StudyPanelVisible;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TAppModulesConstructionGeneric.ResetState: boolean;
const OPNAME = 'TAppModulesConstructionGeneric.ResetState';
begin
  Result := False;
  try
    if Result then
      ViewIni.ResetToDefaults;
    if Assigned(FMainForm) then
      FMainForm.ResetState;
    if Assigned(FModel) then
      FModel.ResetState;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TAppModulesConstructionGeneric.CloseApplication;
const OPNAME = 'TAppModulesConstructionGeneric.CloseApplication';
begin
  try
    if Assigned(FMainForm) and Assigned(FMainForm.MainForm) then
    begin
      FMainForm.SaveState;
      PostMessage(FMainForm.MainForm.Handle,WM_CLOSE,0,0);
    end
    else
      Application.Terminate;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TAppModulesConstructionGeneric.PrintManager: TAbstractPrintManager;
const OPNAME = 'TAppModulesConstructionGeneric.PrintManager';
begin
  Result := nil;
  try
    Result := FPrintManager;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAppModulesConstructionGeneric.Changes : TAbstractChangeManager;
const OPNAME = 'TAppModulesConstructionGeneric.Changes';
begin
  Result := nil;
  try
    Result := FChangeManager;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAppModulesConstructionGeneric.MetaData : TAbstractMetaDataManager;
const OPNAME = 'TAppModulesConstructionGeneric.MetaData';
begin
  Result := nil;
  try
    Result := FMetaDataManager;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAppModulesConstructionGeneric.WeatherEvents :TAbstractWeatherEventsManager;
const OPNAME = 'TAppModulesConstructionGeneric.WeatherEvents';
begin
  Result := nil;
  try
    Result := FWeatherEventsManager;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAppModulesConstructionGeneric.DoLaunchReleseNote;
const OPNAME = 'TAppModulesConstructionGeneric.DoLaunchReleseNote';
var
  LFileName: string;
begin
  try
    LFileName := ExtractFilePath(ApplicationExeName) + 'help\WRMF Release Note.pdf';
    FStudyDocumentManager.ViewFile(LFileName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAppModulesConstructionGeneric.ScenarioLockManager: TAbstractScenarioLockManager;
const OPNAME = 'TAppModulesConstructionGeneric.ScenarioLockManager';
begin
  Result := nil;
  try
    Result := FScenarioLockManager;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{function TAppModulesConstructionGeneric.LicenceManager: TAbstractLicenceManager;
const OPNAME = 'TAppModulesConstructionGeneric.LicenceManager';
begin
  Result := nil;
  try
    Result := FLicenceManager;
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

function TAppModulesConstructionGeneric.StudyAreaManager: TAbstractStudyAreaManager;
const OPNAME = 'TAppModulesConstructionGeneric.StudyAreaManager';
begin
  Result := nil;
  try
    Result := FStudyAreaManager;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAppModulesConstructionGeneric.UnLoadCurrentModel;
const OPNAME = 'TAppModulesConstructionGeneric.UnLoadCurrentModel';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;

end;

procedure TAppModulesConstructionGeneric.DoUserAdministration;
const OPNAME = 'TAppModulesConstructionGeneric.DoUserAdministration';
begin
  try
    if Assigned(AccessControlManager()) then
      AccessControlManager.DoUserAdministration(FUser);
    if Assigned(FMainForm) then
      FMainForm.RefreshState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAppModulesConstructionGeneric.DoLaunchExampleFiles;
const OPNAME = 'TAppModulesConstructionGeneric.DoLaunchExampleFiles';
var
  LFileName   : string;
  LDirectory  : string;
  LOpenDialog : TOpenDialog;
begin
  try
    LDirectory := ExtractFilePath(ApplicationExeName) + 'Example Files\';
    LOpenDialog := TOpenDialog.Create(nil);
    try
      if (not DirectoryExists(LDirectory)) then
        ForceDirectories(LDirectory);
      LOpenDialog.Title := 'View Example Files';
      LOpenDialog.InitialDir := LDirectory;
      LOpenDialog.Options := [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing];
      if LOpenDialog.Execute then
      begin
        LFileName := LOpenDialog.FileName;
        FStudyDocumentManager.ViewFile(LFileName);
      end;
    finally
      LOpenDialog.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAppModulesConstructionGeneric.DoOpenAnyFile;
const OPNAME = 'TAppModulesConstructionGeneric.DoOpenAnyFile';
var
  LDialog : TOpenDialog;
begin
  try
    LDialog := TOpenDialog.Create(nil);
    try

      if LDialog.Execute then
      begin
        ShellExecute(Application.Handle, 'open', PChar(LDialog.FileName),nil,nil,SW_SHOWNORMAL) ;
      end;
    finally
      LDialog.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAppModulesConstructionGeneric.DoLaunchReadMe;
const OPNAME = 'TAppModulesConstructionGeneric.DoLaunchReadMe';
var
  LFileName: string;
begin
  try
    LFileName := ExtractFilePath(ApplicationExeName) + 'help\WRMF ReadMe.pdf';
    FStudyDocumentManager.ViewFile(LFileName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
