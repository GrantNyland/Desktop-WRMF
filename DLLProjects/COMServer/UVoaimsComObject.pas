unit UVoaimsComObject;

{$WARN SYMBOL_PLATFORM OFF}
interface

uses
  Windows, ActiveX, Classes, ComObj, VoaimsCom_TLB, RainfallCom_TLB, StdVcl, psapi,UUtilities,
  UAppModulesConstructionMain;

type
  TVoaimsComObject = class(TTypedComObject, IVoaimsComObject)
  private
    //FYieldModelIntf: IYieldModel;
    function FrameworkAvailable: boolean;
  protected
    function Logon(const AUserID: WideString; const APassword: WideString): WordBool; safecall;
    function SelectStudy(const AModel: WideString; const AStudy: WideString;
                         const ASubArea: WideString; const AScenario: WideString): WordBool; safecall;
    function Get_YieldModel: IYieldModel; safecall;
    function Initialise: WordBool; safecall;
    function Get_INIFileName: WideString; safecall;
    function Get_RainfallModel: IRainfallModel; safecall;
    function HandleVNVEvent(const AVisioApp, AVisioDoc: IUnknown;
      AVisioEventCode_: Integer; const ASourceObj: IUnknown; AEventID,
      AEventSeqNum: Integer; const ASubjectObj: IUnknown;
      AMoreInfo: OleVariant): WordBool; safecall;
    function ProcessVNVSpecial(const AParameter: WideString): WordBool; safecall;
    function IsServerInitialised: WordBool; safecall;
    function IsStudySelected: WordBool; safecall;
    function IsUserLoggedOn: WordBool; safecall;
    function LoggedOnUserName: WideString; safecall;
    function SelectedStudyKeys: WideString; safecall;
    function CloseScenario: WordBool; safecall;
    function UnlockScenario(const AStudyAreaCode, AModelCode, ASubAreaCode,AScenarioCode: WideString): WordBool; safecall;
    function Logoff: WordBool; safecall;
    //function Get_HydrologyModel: IHydrologyModel; safecall;
  
    function IVoaimsComObject_Get_HydrologyModel: IUnknown; safecall;
    function IVoaimsComObject_Get_RainfallModel: IUnknown; safecall;
    function Get_PlanningModel: IPlanningModel; safecall;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;


implementation

uses
  ComServ,
  SysUtils,
  UAbstractObject,
  UAppModulesConstructionGeneric,
  UErrorHandlingOperations;

procedure TVoaimsComObject.AfterConstruction;
const OPNAME = 'TVoaimsComObject.AfterConstruction';
begin
  inherited;
  try
    GAppModules := nil;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVoaimsComObject.BeforeDestruction;
const OPNAME = 'TVoaimsComObject.BeforeDestruction';
var
  LEventHandle: THandle;
begin
  inherited;
  try
    if(GAppModules <> nil) then
    begin
      LEventHandle := OpenEvent(EVENT_ALL_ACCESS,True,'EditingFinished');
      if (LEventHandle <> 0) then
        SetEvent(LEventHandle);
      GAppModules := nil;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVoaimsComObject.Initialise: WordBool;
const OPNAME = 'TVoaimsComObject.Initialise';
var
  LCanProceed: boolean;
begin
  Result := False;
  //AResult := False;
  try
    if Assigned(GAppModules) then
      Result := True
    else
    begin
      GOCXMode := True;
      LCanProceed := True;
      GAppModules := TAppModulesConstructorMain.Create(LCanProceed);
      if not LCanProceed then
      begin
         FreeAndNil(GAppModules);
      end
      else
      begin
      if Assigned(GAppModules) then
      begin
        GAppModules.IniFile.WriteString('USER','AutoLogon','N');
        GAppModules.IniFile.WriteString('STUDY','AutoStudy','N');
        Result := True
      end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVoaimsComObject.FrameworkAvailable: boolean;
const OPNAME = 'TVoaimsComObject.FrameworkAvailable';
begin
  Result := False;
  try
    Result := Assigned(GAppModules);
    if not Result then
      raise Exception.Create('Application framework has not yet been initialised. Please call initialise before any further actions.');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVoaimsComObject.Logon(const AUserID: WideString;
         const APassword: WideString): WordBool;
const OPNAME = 'TVoaimsComObject.Logon';
begin
  //Result := 0;
  Result := False;
  try
    if FrameworkAvailable then
    begin
      Result := GAppModules.Com_Logon(AUserID,APassword);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVoaimsComObject.SelectStudy(const AModel: WideString; const AStudy: WideString;
                         const ASubArea: WideString; const AScenario: WideString): WordBool;
const OPNAME = 'TVoaimsComObject.SelectStudy';
begin
  //Result := 0;
  Result := False;
  try
    if FrameworkAvailable then
    begin
      Result := GAppModules.Com_SelectStudy(AModel,AStudy,ASubArea,AScenario);
      //if GAppModules.Model.ModelName = CYield then
      //  FYieldModelIntf := GAppModules.Model as IYieldModel
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVoaimsComObject.Get_YieldModel: IYieldModel;
const OPNAME = 'TVoaimsComObject.Get_YieldModel';
begin
  //Result := 0;
  Result := nil;
  try
    if FrameworkAvailable then
    begin
      if GAppModules.Model.ModelName = CYield then
        Result := GAppModules.Model as IYieldModel
      else
      raise Exception.CreateFmt('The yield model is not currently loaded. The currently loaded model is (%s)',
            [GAppModules.Model.ModelName]);

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVoaimsComObject.Get_RainfallModel: IRainfallModel;
const OPNAME = 'TVoaimsComObject.Get_RainfallModel';
begin
  //Result := 0;
  Result := nil;
  try
    if FrameworkAvailable then
    begin
      if GAppModules.Model.ModelName = CRainfall then
        Result := GAppModules.Model as IRainfallModel
      else
      raise Exception.CreateFmt('The rainfall model is not currently loaded. The currently loaded model is (%s)',
            [GAppModules.Model.ModelName]);

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVoaimsComObject.HandleVNVEvent(const AVisioApp,
  AVisioDoc: IUnknown; AVisioEventCode_: Integer;
  const ASourceObj: IUnknown; AEventID, AEventSeqNum: Integer;
  const ASubjectObj: IUnknown; AMoreInfo: OleVariant): WordBool;
const OPNAME = 'TVoaimsComObject.HandleVNVEvent';
var
  LYieldModel: IYieldModel;
  //LHydrologyModel : IHydrologyModel;
begin
  Result := false;       
  try
    if(GAppModules.Model.ModelName = CYield) then
    begin
      LYieldModel := Get_YieldModel;
      if (LYieldModel <> nil) then
        Result := LYieldModel.HandleVNVEvent(AVisioApp,AVisioDoc,AVisioEventCode_,
                  ASourceObj, AEventID, AEventSeqNum,ASubjectObj,AMoreInfo);
    end;
    {else if(GAppModules.Model.ModelName = CHydrology) then //RianaHydro
    begin
      LHydrologyModel := Get_HydrologyModel;
      if (LHydrologyModel <> nil) then
        Result := LHydrologyModel.HandleVNVEvent(AVisioApp,AVisioDoc,AVisioEventCode_,
                  ASourceObj, AEventID, AEventSeqNum,ASubjectObj,AMoreInfo);
    end}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVoaimsComObject.ProcessVNVSpecial (const AParameter: WideString): WordBool;
const OPNAME = 'TVoaimsComObject.ProcessVNVSpecial';
var
  LYieldModel: IYieldModel;
  //LHydrologyModel : IHydrologyModel;
begin
  Result := false;
  try
    if(GAppModules.Model.ModelName = CYield) then
    begin
      LYieldModel := Get_YieldModel;
      if (LYieldModel <> nil) then
        Result := LYieldModel.ProcessVNVSpecial(AParameter);
    end;
    {else if(GAppModules.Model.ModelName = CHydrology) then //RianaHydro
    begin
      LHydrologyModel := Get_HydrologyModel;
      if (LHydrologyModel <> nil) then
        Result := LHydrologyModel.ProcessVNVSpecial(AParameter);
    end;}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function GetLongPathNameA(strTemp: PChar; strTemp1: PChar; size:DWORD): DWORD; stdcall; external 'kernel32.dll';
const OPNAME = 'GetLongPathNameA';

function TVoaimsComObject.Get_INIFileName: WideString;
const OPNAME = 'TVoaimsComObject.Get_INIFileName';
var strTemp: string;
   LFileName : string;
begin
  try
    SetLength(strTemp,MAX_PATH);
    GetModuleFileName(HInstance,PChar(strTemp),MAX_PATH);
    strTemp := PChar(strTemp);
    SetLength(strTemp,MAX_PATH);
    GetLongPathNameA(PChar(strTemp),PChar(strTemp),MAX_PATH);
    strTemp := PChar(strTemp);
    LFileName := ExtractFileName(strTemp);
    strTemp := GetAppDataLocalDir;
    strTemp := IncludeTrailingPathDelimiter(strTemp);
    Result := ChangeFileExt(strTemp+LFileName,'.INI');

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVoaimsComObject.IsServerInitialised: WordBool;
const OPNAME = 'TVoaimsComObject.IsServerInitialised';
begin
  Result := False;
  try
    Result := FrameworkAvailable;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVoaimsComObject.IsStudySelected: WordBool;
const OPNAME = 'TVoaimsComObject.IsStudySelected';
begin
  Result := False;
  try
    if FrameworkAvailable then
    begin
      Result := (GAppModules.Model <> nil) and (GAppModules.Model.ModelName <> CSystem);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVoaimsComObject.IsUserLoggedOn: WordBool;
const OPNAME = 'TVoaimsComObject.IsUserLoggedOn';
begin
  Result := False;
  try
    if FrameworkAvailable then
    begin
      Result := (GAppModules.User <> nil) and (GAppModules.User.LoggedOn);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVoaimsComObject.LoggedOnUserName: WideString;
const OPNAME = 'TVoaimsComObject.LoggedOnUserName';
begin
  Result := '';
  try
    if IsUserLoggedOn then
    begin
        Result :=  GAppModules.User.UserId;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVoaimsComObject.SelectedStudyKeys: WideString;
const OPNAME = 'TVoaimsComObject.SelectedStudyKeys';
var
  LData: TStringList;
begin
  Result := '';
  try
    if IsStudySelected then
    begin
      LData := TStringList.Create;
      try
        LData.Add(GAppModules.StudyArea.StudyAreaCode);
        LData.Add(GAppModules.StudyArea.ModelCode);
        LData.Add(GAppModules.StudyArea.SubAreaCode);
        LData.Add(GAppModules.StudyArea.ScenarioCode);
        Result := LData.CommaText;
      finally
        LData.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVoaimsComObject.CloseScenario: WordBool;
const OPNAME = 'TVoaimsComObject.CloseScenario';
begin
  Result := False;
  try
    if IsStudySelected then
    begin
      Result := GAppModules.LoadModel(mmModelSystem);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVoaimsComObject.UnlockScenario(const AStudyAreaCode, AModelCode, ASubAreaCode, AScenarioCode: WideString): WordBool;
const OPNAME = 'TVoaimsComObject.UnlockScenario';
begin
  Result := False;
  try
    if FrameworkAvailable then
    begin
      Result :=  GAppModules.ScenarioLockManager.RequestScenarioUnlock(AStudyAreaCode, AModelCode, ASubAreaCode, AScenarioCode);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVoaimsComObject.Logoff: WordBool;
const OPNAME = 'TVoaimsComObject.Logoff';
begin
  //Result := 0;
  Result := False;
  try
    if FrameworkAvailable then
    begin
      Result := GAppModules.Com_Logoff;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{function TVoaimsComObject.Get_HydrologyModel: IHydrologyModel;
const OPNAME = 'TVoaimsComObject.Get_HydrologyModel';
begin
  Result := nil;
  try
    if FrameworkAvailable then
    begin
      if GAppModules.Model.ModelName = CHydrology then
        Result := GAppModules.Model as IHydrologyModel
      else
      raise Exception.CreateFmt('The hydrology model is not currently loaded. The currently loaded model is (%s)',
            [GAppModules.Model.ModelName]);

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

function TVoaimsComObject.IVoaimsComObject_Get_HydrologyModel: IUnknown;
begin

end;

function TVoaimsComObject.IVoaimsComObject_Get_RainfallModel: IUnknown;
begin

end;

function TVoaimsComObject.Get_PlanningModel: IPlanningModel;
begin

end;

initialization
  TTypedComObjectFactory.Create(ComServer, TVoaimsComObject, Class_VoaimsComObject,ciMultiInstance, tmApartment);
end.
