//
//
//  UNIT      : Contains TAbstractObject Class
//  AUTHOR    : Titi Ngubane(PDNA)
//  DATE      : 25/01/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//

unit UAppModulesConstructionMain;

interface

uses
  Classes,
//  Datasnap.Win.MConnect,
//  Midas,
  VCL.Forms,
  UGlobalData,
  UAbstractObject,
  //VoaimsCom_TLB,
  USystemModelLinkClasses,
  UStringListOfStringLists,
  UAppModulesConstructionGeneric;

type
  TConstructDLLObjectMainForm = function (var pao_Object: TObject; AAppModules: TAppModules; AImagesInstance: integer): boolean; stdcall;
  TAppModulesConstructorMain = class(TAppModulesConstructionGeneric)
  protected
    FGlobalData: TGlobalData;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function LoadGraphicsModule: boolean;
    procedure CreateMainForm;
    function LaunchSplashScreen(var ADLLFileName: string; var ADLLHandle: longword ): boolean;
    procedure WaitForSplashScreen(var ADLLFileName: string; var ADLLHandle:longword);
    procedure UnLoadCurrentModel; override;
    procedure RefreshCurrentState;
    function LoadSystemModel: boolean;
    function LoadStomsaModelDLL : boolean;
    function LoadYieldModelDLL : boolean;
    function LoadRainfallDataModelDLL : boolean;
    function LoadYRCModelDLL : boolean;
    function LoadHydrologyModelDLL : boolean;
    function LoadPlanningModelDLL : boolean;
    function LoadBroadScalePreprocessor : Boolean;
    function LoadDailyDiversionPreprocessor : boolean;
    function LoadIFRPreprocessor : boolean;
    function LoadDamSedimentationPreprocessor: boolean;
    function LoadRWHModel : boolean;
    function LoadDDTSModel : boolean;


  public
    constructor Create(var ACanProceed: boolean);
    function Initialise: boolean; override;
    function LanguageHasChanged: boolean; override;
    function GlobalData: TAbstractGlobalData; override;
    function LoadModel(AModel: TModelManagerType): boolean; override;
    function GetModelViewItems(AModelName: string;AItems: TStringListOfStringLists): boolean; override;
    procedure DoAbout; override;
    function Com_Logon(AUserId: String; APassword: String): boolean;
    function Com_Logoff: boolean;
    function Com_SelectStudy(AModel,AStudy,ASubArea,AScenario: String): boolean;
  end;


var
  GAppModules: TAppModulesConstructorMain;
  GStartTickCount: longword;
  GMainForm: TForm;

implementation

uses
  SysUtils,
  VCL.Dialogs,
  Windows,
  winapi.shellapi,
  UUtilities,
  //ULicencing,
  UStudyArea,
  UDLLOperations,
  UAbstractComponent,
  //UFunctionTimer,
  USystemModelManager,
  //UYieldModelManager,
  UErrorHandlingOperations,
  UAppModulesWithDatabase,
  UHTMLHelp,
{$IFDEF MERGE_DLLS}
  UMainFormManager,
{$ENDIF}
  D6OnHelpFix;

{ TAppModulesConstructor }

constructor TAppModulesConstructorMain.Create(var ACanProceed: boolean);
const OPNAME = 'TAppModulesConstructorMain.Create';
begin
  // Dependecies can be database specific so later in the creation sequence we still
  // need to confirm dependencies so we use a the following flag internally
  ACanProceed := False;

  FCanProceed := True;
  try
    GStartTickCount := GetTickCount;
    if CheckForRequiredModules([
      'WRMFGraphics.dll',
      'LanguageManager.dll',
      'StudySelection.dll',
      'AccessControl.dll',
      'FieldProperties.dll',
      'Changes.dll',
      'MainForm.dll']) then
    begin
      inherited Create;
      ACanProceed := FCanproceed;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAppModulesConstructorMain.CreateMemberObjects;
const OPNAME = 'TAppModulesConstructorMain.CreateMemberObjects';
var
  LDoAboutDLLFileName: string;
  LDLLHandle: longword ;
begin
  //StartFunctionTimer;
  //ProcessFunctionCall(OPNAME);
  try
    if GOCXMode then
      ShowMessage('Establishing connection to COM server in order to access model data...Press OK to continue.');
    // Load the images resource DLL.
    if FCanProceed then
    begin
      if LoadGraphicsModule then
      begin

        // Launch the splash screen.
        if not GOCXMode then
        begin
          if not LaunchSplashScreen(LDoAboutDLLFileName, LDLLHandle) then
          begin
            FCanProceed := False;
            Exit;
          end;
        end;

        // Create the application objects.
        GAppModules := self;
        inherited CreateMemberObjects;
        if FCanProceed then
        begin
          FGlobalData := TGlobalData.Create;
          FGlobalData.SetCOMServerMode(GOCXMode);

          // Create the main form.
          if not GOCXMode then
            CreateMainForm;

          CreateAccessControlManager;
          CreateStudyAreaManager;

          // Create Print manager after main form so that the menu will be seen.
          if not GOCXMode then
            CreatePrintManager;

          CreateMetaDataManager;
          CreateChangeManager;
          CreateWeatherEventsManager;

          // Do not auto logon and auto select study.
          if Assigned(FIniFile) then
          begin
            if GOCXMode then
            begin
              IniFile.WriteString('STUDY','AutoStudy','N');
              IniFile.WriteString('USER','AutoLogon','N');
            end;
          end;
          
          // Launch the initialisation process.
          if Initialise then
            // Refresh the current state.
            if not GOCXMode then
              RefreshCurrentState;
        end;
        // Wait until the splash has finished.
        if not GOCXMode then
          WaitForSplashScreen(LDoAboutDLLFileName, LDLLHandle);

      end;
    end;

    // Show the main form.
    if(not GOCXMode) and FCanProceed and (Assigned(FMainForm)) then
      ViewIni.LoadFormView(FMainForm.MainForm);

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
  //ProcessFunctionReturn(OPNAME);
  //FunctionTimerCallTraceTable;
  //FunctionTimerFunctionCountTable;
end;

procedure TAppModulesConstructorMain.DestroyMemberObjects;
const OPNAME = 'TAppModulesConstructorMain.DestroyMemberObjects';
begin
  try
    if Assigned(FModel) then
      FScenarioLockManager.ReleaseLock;
      
    if Assigned(FIniFile) then
    begin
      if GOCXMode then
      begin
        IniFile.WriteString('STUDY','AutoStudy','N');
        IniFile.WriteString('USER','AutoLogon','N');
      end;
      if Assigned(FGlobaldata) then
        FGlobalData.SaveState(FIniFile);
    end;
    //if (FModel.ModelName = CYield) then
    //  FYieldModelInterface := nil
    //else
    //FreeAndNil(FModel);
    DestroyDLLObject(TAbstractAppObject(FModel), OPNAME);
    DestroyDLLObject(TAbstractAppObject(FStudyDocumentManager), OPNAME);
    if not GOCXMode then
    begin
      DestroyDLLObject(TAbstractAppObject(FPrintManager), OPNAME);
      FreeAndNil(FMainForm);
    end;
    FreeLibrary(HImagesInstance);
    inherited DestroyMemberObjects;
    FreeAndNil(FGlobalData);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAppModulesConstructorMain.LoadGraphicsModule: boolean;
const OPNAME = 'TAppModulesConstructorMain.LoadGraphicsModule';
begin
  Result := False;
  try
    if FCanProceed then
    begin
{$IFDEF MERGE_DLLS}
      HImagesInstance := HINSTANCE;
      FCanProceed := True;
{$ELSE}
      HImagesInstance := 0;
      FCanProceed := LoadDLL(ExtractFilePath(ApplicationExeName) + 'Bin\WRMFGraphics.dll', HImagesInstance, True, OPNAME);
{$ENDIF}
    end;
    Result := FCanProceed;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAppModulesConstructorMain.CreateMainForm;
const OPNAME = 'TAppModulesConstructorMain.CreateMainForm';
//  external  name ;

{$IFNDEF MERGE_DLLS}
var
  LCreateMainFormFunction: TConstructDLLObjectMainForm;
  LDllName: string;
  LDLLHandle: longword;
{$ENDIF}
begin
  try
{$IFDEF MERGE_DLLS}
    FMainForm := TMainFormManager.Create(self);
    Application.CreateForm(FMainForm.MainFormClassReference, GMainForm);
    FMainForm.MainForm := GMainForm;
    FMainForm.MenuItemManager.AddMenuItems;
{$ELSE}

    LDLLHandle := 0;
    LCreateMainFormFunction := nil;
    LDllName := ExtractFilePath(ApplicationExeName) + 'bin\MainForm.dll';
    if LoadDLL(LDllName,LDLLHandle,True,ExtractFilePath(ApplicationExeName)) then
    begin
      if GetDLLFunction(LDLLHandle,LDllName,'ConstructDLLObject',@LCreateMainFormFunction,ExtractFilePath(ApplicationExeName)) then
      begin
        if LCreateMainFormFunction(TObject(FMainForm), self, HImagesInstance) then
        begin
          Application.CreateForm(FMainForm.MainFormClassReference, GMainForm);
          FMainForm.MainForm := GMainForm;
        end;
        FMainForm.MenuItemManager.AddMenuItems;
      end;
    end;
{$ENDIF}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAppModulesConstructorMain.Initialise: boolean;
const OPNAME = 'TAppModulesConstructorMain.Initialise';
begin
  //ProcessFunctionCall(OPNAME);
  Result := False;
  try
    //Application.HelpFile := IncludeTrailingPathDelimiter(ExtractFilePath(ApplicationExeName))+ 'Help\WRMF.hlp';
    Application.HelpFile := IncludeTrailingPathDelimiter(ExtractFilePath(ApplicationExeName))+ 'Html Help\wrmf.chm';
    FGlobalData.Initialise(FIniFile);
    Result := inherited Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
  //ProcessFunctionReturn(OPNAME);
end;

procedure TAppModulesConstructorMain.RefreshCurrentState;
const OPNAME = 'TAppModulesConstructorMain.RefreshCurrentState';
begin
  try
    if GOCXMode then  Exit;
    LanguageHasChanged;
    if Assigned(FMainForm) then
      FMainForm.RefreshState;
    //if Assigned(FModel) then
    //  TSystemModelManager(FModel).OnTabHasChanged(nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

//
// WARNING : Do not call this method from an instance of a model manager
//           because this method frees the currently loaded model manager.
//
function TAppModulesConstructorMain.LoadModel(AModel: TModelManagerType): boolean;
const OPNAME = 'TAppModulesConstructorMain.LoadModel';
begin
  //ProcessFunctionCall(OPNAME);
  Result := False;
  try

    // Make sure that the old model is unloaded.

    if (AModel <> mmModelUnchanged) then
      UnLoadCurrentModel;

    // Load the new model.
    case AModel of
      mmModelUnchanged      : ;
      mmModelYield          :
        begin
          if LoadYieldModelDLL then
            if Assigned(FChangeManager) then
              FChangeManager.AddTabsheetToPageControl;
        end;

      mmModelPlanning          :
        begin
          if LoadPlanningModelDLL then
            if Assigned(FChangeManager) then
              FChangeManager.AddTabsheetToPageControl;
        end;

      mmModelRainfallData   :
        begin
          if LoadRainfallDataModelDLL then
          begin
            if Assigned(FChangeManager) then
              FChangeManager.AddTabsheetToPageControl;
            if Assigned(FWeatherEventsManager) then
              FWeatherEventsManager.AddTabsheetToPageControl;
          end;
        end;
      mmModelHydrology             : LoadHydrologyModelDLL;
      mmModelYRC                   : LoadYRCModelDLL;
      mmModelSystem                : LoadSystemModel;
      mmBroadScalePreprocessor     : LoadBroadScalePreprocessor;
      mmDailyDiversionPreprocessor : LoadDailyDiversionPreprocessor;
      mmIFRPreprocessor            : LoadIFRPreprocessor;
      mmDamSedimentation           : LoadDamSedimentationPreprocessor;
      mmStomsa                     : LoadStomsaModelDLL;
      mmRWH                        : LoadRWHModel;
      mmDDTS                       : LoadDDTSModel;

    else
      raise Exception.CreateFmt('Unknown model type [%d].', [integer(AModel)]);
    end;

    // Attempt to initialise the model manager.
    if Assigned(FModel) then
    begin
      FStudyArea.ScenarioLocked := not FScenarioLockManager.RequestLock;
      if not  FStudyArea.ScenarioLocked then
      begin
        if FStudyArea.DataImported then
        begin
          FStudyArea.ScenarioLocked := True;
        end;
      end;

      if FModel.Initialise then
        Result := FModel.StudyHasChanged;


      if Assigned(FMainForm) then
        FMainForm.StudyHasChanged;

       // Done in System Model Manager
      //if Assigned(FChangeManager) then
      //  FChangeManager.StudyHasChanged;

      if Assigned(FMetaDataManager) then
        FMetaDataManager.StudyHasChanged;

      if Assigned(FWeatherEventsManager) then
        FWeatherEventsManager.StudyHasChanged;

      // Refresh the current state.
      RefreshCurrentState;
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
  //ProcessFunctionReturn(OPNAME);
end;

procedure TAppModulesConstructorMain.UnLoadCurrentModel;
const OPNAME = 'TAppModulesConstructorMain.UnLoadCurrentModel';
begin
  inherited;
  try
    if Assigned(FModel)  then
    begin
      if (FModel.DLLHandle <> 0) then
        DestroyDLLObject(TAbstractAppObject(FModel), OPNAME)
      else
        FreeAndNil(FModel);

      FScenarioLockManager.ReleaseLock;
    end;

    FModel := nil;

    if (FChangeManager <> nil) then
      FChangeManager.RemoveTabsheetFromPageControl;
    if (FWeatherEventsManager <> nil) then
      FWeatherEventsManager.RemoveTabsheetFromPageControl;
    {if Assigned(FMainForm) then
      if Assigned(FMainForm.PageControl) then
      begin
        while (FMainForm.PageControl.PageCount > 0) do
        begin
          FMainForm.PageControl.Pages[0].PageControl := nil;
        end;
      end;}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAppModulesConstructorMain.LanguageHasChanged: boolean;
const OPNAME = 'TAppModulesConstructorMain.LanguageHasChanged';
begin
  Result := False;
  try
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAppModulesConstructorMain.GlobalData: TAbstractGlobalData;
const OPNAME = 'TAppModulesConstructorMain.GlobalData';
begin
  Result := nil;
  try
    Result := FGlobalData;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAppModulesConstructorMain.LaunchSplashScreen(var ADLLFileName: string; var ADLLHandle: longword): boolean;
const OPNAME = 'TAppModulesConstructorMain.LaunchSplashScreen';
type TLaunchSplashScreenFunction = function:boolean; stdcall;
var
  LDLLFunctionName: string;
  LLaunchSplashScreen: TLaunchSplashScreenFunction;
begin
  Result := False;
  try
    if GOCXMode then
    begin
      Result := True;
      Exit;
    end;

    // Set the DLL filename depending on project.
    ADLLFileName := '';
    ADLLHandle := 0;
    ADLLFileName := ExtractFilePath(ApplicationExeName) + 'bin\WRMFSplashScreen.dll';

    // Load the DLL.
    if LoadDLL(ADLLFileName, ADLLHandle, False, OPNAME) then
    begin

      // Get the create function addresses.
      LDLLFunctionName := 'LaunchSplashScreen';
      if GetDLLFunction(ADLLHandle, ADLLFileName, LDLLFunctionName, @LLaunchSplashScreen, OPNAME) then
      begin

        // Call the about function.
        Result := LLaunchSplashScreen;
      end;
    end
    else
      Result := True;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAppModulesConstructorMain.DoAbout;
const OPNAME = 'TAppModulesConstructorMain.DoAbout';
type TDoAboutFunction = procedure (AAppModules: TObject); stdcall;
var
  LDLLFileName, LDLLFunctionName: string;
  LDoAbout: TDoAboutFunction;
  LDLLHandle: longword ;
begin
  try
    if GOCXMode then Exit;
    LDLLFileName := ExtractFilePath(ApplicationExeName) + 'bin\WRMFSplashScreen.dll';

    // Load the DLL.
    if LoadDLL(LDLLFileName, LDLLHandle, True, OPNAME) then
    begin
      try
        LDLLFunctionName := 'DoAboutDialog';
        if GetDLLFunction(LDLLHandle, LDLLFileName, LDLLFunctionName, @LDoAbout, OPNAME) then
        begin

          // Call the about function.
          LDoAbout(self);
        end;
      finally
        FreeLibrary(LDLLHandle);
      end;
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAppModulesConstructorMain.WaitForSplashScreen(var ADLLFileName: string; var ADLLHandle: longword);
const OPNAME = 'TAppModulesConstructorMain.WaitForSplashScreen';
type TWaitForSplashScreenFunction = procedure; stdcall;
var
  LDLLFunctionName: string;
  LWaitForSplashScreen: TWaitForSplashScreenFunction;
begin
  try
      if GOCXMode then Exit;

    // Check that the DLL has been loaded.
    if (ADLLHandle <> 0) then
    begin
      LDLLFunctionName := 'WaitForSplashScreen';
      if GetDLLFunction(ADLLHandle, ADLLFileName, LDLLFunctionName, @LWaitForSplashScreen, OPNAME) then
      begin

        // Call the wait function.
        LWaitForSplashScreen;
      end;

      // Unload the DLL.
      FreeLibrary(ADLLHandle);
      ADLLHandle := 0;
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAppModulesConstructorMain.LoadHydrologyModelDLL: boolean;
const OPNAME = 'TAppModulesConstructorMain.LoadHydrologyModelDLL';
begin
  Result := False;
  try
    //Result := CreateDLLObject(ExtractFilePath(ApplicationExeName) + 'bin\ModelHydrology.dll',
    Result := CreateDLLObject(ExtractFilePath(ApplicationExeName) + 'bin\HydrologyModel.dll', //RianaHydro
      TAbstractAppObject(FModel), self, False, OPNAME);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAppModulesConstructorMain.LoadBroadScalePreprocessor : Boolean;
const OPNAME = 'TAppModulesConstructorMain.LoadBroadScalePreprocessor';
begin
  Result := False;
  try
    Result := CreateDLLObject(ExtractFilePath(ApplicationExeName) + 'bin\ModelPesIMS.dll', TAbstractAppObject(FModel), Self, True, OPNAME);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAppModulesConstructorMain.LoadDailyDiversionPreprocessor : boolean;
const OPNAME = 'TAppModulesConstructorMain.LoadDailyDiversionPreprocessor';
begin
  Result := False;
  try
    Result := CreateDLLObject(ExtractFilePath(ApplicationExeName) + 'bin\DailyDiversionPreprocessor.dll', TAbstractAppObject(FModel), Self, False, OPNAME);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAppModulesConstructorMain.LoadIFRPreprocessor: boolean;
const OPNAME = 'TAppModulesConstructorMain.LoadIFRPreprocessor';
begin
  Result := False;
  try
    Result := CreateDLLObject(ExtractFilePath(ApplicationExeName) + 'bin\IFRPreProcessor.dll', TAbstractAppObject(FModel), Self, False, OPNAME);
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TAppModulesConstructorMain.LoadDamSedimentationPreprocessor: boolean;
const OPNAME = 'TAppModulesConstructorMain.LoadDamSedimentationPreprocessor';
begin
  Result := False;
  try
    Result := CreateDLLObject(ExtractFilePath(ApplicationExeName) + 'bin\SedimentationPreProcessor.dll', TAbstractAppObject(FModel), Self, False, OPNAME);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAppModulesConstructorMain.LoadRainfallDataModelDLL: boolean;
const OPNAME = 'TAppModulesConstructorMain.LoadRainfallDataModelDLL';
begin
  Result := False;
  try
    Result := CreateDLLObject(ExtractFilePath(ApplicationExeName) + 'bin\ModelRainfallData.dll',
      TAbstractAppObject(FModel), self, False, OPNAME);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAppModulesConstructorMain.LoadRWHModel: boolean;
const OPNAME = 'TAppModulesConstructorMain.LoadRWHModel';
begin
  Result := False;
  try
    Result := CreateDLLObject(ExtractFilePath(ApplicationExeName) + 'bin\ModelRWH.dll', TAbstractAppObject(FModel), Self, False, OPNAME);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAppModulesConstructorMain.LoadDDTSModel : boolean;
const OPNAME = 'TAppModulesConstructorMain.LoadDDTSModel';
begin
  Result := False;
  try
    Result := CreateDLLObject(ExtractFilePath(ApplicationExeName) + 'bin\ModelDDTS.dll', TAbstractAppObject(FModel), Self, False, OPNAME);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAppModulesConstructorMain.LoadSystemModel: boolean;
const OPNAME = 'TAppModulesConstructorMain.LoadSystemModel';
begin
  Result := False;
  try
    TStudyArea(FStudyArea).Reset;
    FModel := TSystemModelManager.Create(Self);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAppModulesConstructorMain.LoadStomsaModelDLL: boolean;
const OPNAME = 'TAppModulesConstructorMain.LoadStomsaModelDLL';
begin
  Result := False;
  try
    ChDir(ExtractFilePath(ApplicationExeName) + 'Stomsa\');
    Result :=  CreateDLLObject(ExtractFilePath(ApplicationExeName) + 'Stomsa\ModelStomsa.dll',
      TAbstractAppObject(FModel), self, True, OPNAME);
      
  except on E: Exception do HandleError(E, OPNAME) end;
end;



function TAppModulesConstructorMain.LoadYieldModelDLL: boolean;
const OPNAME = 'TAppModulesConstructorMain.LoadYieldModelDLL';
begin
  Result := False;
  try
    //FModel := nil;
    Result :=  CreateDLLObject(ExtractFilePath(ApplicationExeName) + 'bin\ModelYield.dll',
      TAbstractAppObject(FModel), self, False, OPNAME);
    //if Assigned(FModel) then
    //   FYieldModelInterface := (FModel as IYieldModel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAppModulesConstructorMain.LoadPlanningModelDLL: boolean;
const OPNAME = 'TAppModulesConstructorMain.LoadPlanningModelDLL';
begin
  Result := False;
  try
    //FModel := nil;
    Result :=  CreateDLLObject(ExtractFilePath(ApplicationExeName) + 'bin\ModelPlanning.dll',
      TAbstractAppObject(FModel), self, False, OPNAME);
    //if Assigned(FModel) then
    //   FYieldModelInterface := (FModel as IYieldModel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAppModulesConstructorMain.LoadYRCModelDLL: boolean;
const OPNAME = 'TAppModulesConstructorMain.LoadYRCModelDLL';
begin
  Result := False;
  try
    Result := CreateDLLObject(ExtractFilePath(ApplicationExeName) + 'bin\ModelYRC.dll',
      TAbstractAppObject(FModel), self, False, OPNAME);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAppModulesConstructorMain.Com_Logon(AUserId: String; APassword: String): boolean;
const OPNAME = 'TAppModulesConstructorMain.Com_Logon';
begin
  Result := False;
  try
    IniFile.WriteString('USER','UserId',AUserId);
    IniFile.WriteString('USER','Password',APassword);
    IniFile.WriteString('USER','AutoLogon','Y');
    if ProcessAutoLogon then
      Result := User.LoggedOn;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAppModulesConstructorMain.Com_Logoff: boolean;
const OPNAME = 'TAppModulesConstructorMain.Com_Logoff';
begin
  Result := False;
  try
    if User.LoggedOn then
    begin
      DoLogOff;
      Result := not User.LoggedOn;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAppModulesConstructorMain.Com_SelectStudy(AModel,AStudy,ASubArea,AScenario: String): boolean;
const OPNAME = 'TAppModulesConstructorMain.Com_SelectStudy';
begin
  Result := false;
  try
    IniFile.WriteString('STUDY','Study',AStudy);
    IniFile.WriteString('STUDY','Model',AModel);
    IniFile.WriteString('STUDY','SubArea',ASubArea);
    IniFile.WriteString('STUDY','Scenario',AScenario);
    IniFile.WriteString('STUDY','AutoStudy','Y');
    Result := InitialiseStudy;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAppModulesConstructorMain.GetModelViewItems(AModelName: string; AItems: TStringListOfStringLists): boolean;
const OPNAME = 'TAppModulesConstructorMain.GetModelViewItems';
begin
  Result := False;
  try
    if(FModel <> nil) then
      if (FModel.ModelName = AModelName) then
        FModel.GetModelViewItems(AItems);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
