//
//
//  UNIT      : Contains TRWHGUIManager Class
//  AUTHOR    : Dziedzi Ramulondi (arivia.kom)
//  DATE      : 09/01/2007
//  COPYRIGHT : Copyright © 2007 DWAF
//
//
unit URWHGUIManager;

interface
uses

  // Delphi
  classes,
  contnrs,
  VCL.controls,

  // DWAF
  UAbstractObject,
  UAbstractComponent,
  UDataComponent;

type
  TRWHGUIManager = class(TAbstractAppObject)
  protected
    FInputPageControl   : TAbstractDataPageControl;
    FOutputPageControl  : TAbstractDataPageControl;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnPageControlTabHasChanged(Sender: TObject);
    function  ViewInputData(AParent : TWincontrol; ACommaTextContextData : String;  AOwner : TWincontrol = nil): boolean;
    function  ViewOutputData(AParent : TWincontrol; ACommaTextContextData : String;  AOwner : TWincontrol = nil): boolean;
    function OutputViewType(AViewName: string): boolean;
  public

    function Initialise: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function SaveState: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    function ProcessParameterChangeEvent : boolean;
    function ProcessMetaDataEvent : boolean;
    function ViewInputDialog(AParent : TWincontrol; ACommaTextContextData : String; AOwner : TWincontrol = nil): boolean;
    function ViewInputPopupFormData(AParent : TWincontrol; ACommaTextContextData : String;  AOwner : TWincontrol = nil): boolean;
    function CanCopyToCLipboard: boolean; virtual;
    function CanExport: boolean; virtual;
    function CanPrint: boolean; virtual;
    procedure DoCopyToCLipboard; virtual;
    procedure DoExport; virtual;
    procedure DoPrint; virtual;
    procedure ExitCurrentEditControl;
    function CurrentTabSheet: TModelTabSheetName;
  end;

implementation
uses
// Delphi
  VCL.Forms,
  Sysutils,
  Windows,

  // DWAF
  UConstants,
  URWHDataObject,
  //URWHSiteValidator,
  UErrorHandlingOperations;
const
  CCopyToClipboard     : array[0..1] of WideString = ('Edit','CopyToClipboard');
  CExportToFile        : array[0..1] of WideString = ('Edit','ExportToFile');
  CPrint               : array[0..1] of WideString = ('File','Print');

{ TRWHGUIManager }

procedure TRWHGUIManager.CreateMemberObjects;
const OPNAME = 'TRWHGUIManager.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FInputPageControl   := TAbstractDataPageControl.Create(nil,FAppModules);
    FOutputPageControl  := TAbstractDataPageControl.Create(nil,FAppModules);
    FInputPageControl.Align := alClient;
    FOutputPageControl.Align := alClient;
    FInputPageControl.OnChange  := OnPageControlTabHasChanged;
    FOutputPageControl.OnChange := OnPageControlTabHasChanged;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRWHGUIManager.DestroyMemberObjects;
const OPNAME = 'TRWHGUIManager.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRWHGUIManager.CurrentTabSheet: TModelTabSheetName;
const OPNAME = 'TRWHGUIManager.CurrentTabSheet';
begin
  Result := mtsnNone;
  try
    if(FAppModules.MainForm <> nil) then
    begin
       Result := TAbstractTabSheet(FAppModules.MainForm.PageControl.ActivePage).ModelTabSheetName;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRWHGUIManager.Initialise: boolean;
const OPNAME = 'TRWHGUIManager.Initialise';
begin
  Result := inherited Initialise;;
  try
    FInputPageControl.Initialise;
    FOutputPageControl.Initialise;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRWHGUIManager.LanguageHasChanged: boolean;
const OPNAME = 'TRWHGUIManager.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FInputPageControl.Initialise;
    FOutputPageControl.Initialise;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRWHGUIManager.StudyHasChanged: boolean;
const OPNAME = 'TRWHGUIManager.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    FInputPageControl.DeleteAllTabSheets;
    FOutputPageControl.DeleteAllTabSheets;
    FInputPageControl.StudyHasChanged;
    FOutputPageControl.StudyHasChanged;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRWHGUIManager.SaveState: boolean;
const OPNAME = 'TRWHGUIManager.SaveState';
begin
  Result := inherited SaveState;
  try
    FInputPageControl.SaveState;
    FOutputPageControl.SaveState;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRWHGUIManager.ProcessParameterChangeEvent : boolean;
const OPNAME = 'TRWHGUIManager.ProcessParameterChangeEvent';
begin
  Result := FALSE;
  try
    Result := FInputPageControl.ProcessParameterChangeEvent;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRWHGUIManager.ProcessMetaDataEvent : boolean;
const OPNAME = 'TRWHGUIManager.ProcessMetaDataEvent';
begin
  Result := FALSE;
  try
    Result := FInputPageControl.ProcessMetaDataEvent;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRWHGUIManager.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TRWHGUIManager.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    FInputPageControl.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);;
    FOutputPageControl.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRWHGUIManager.OnPageControlTabHasChanged(Sender: TObject);
const OPNAME = 'TRWHGUIManager.OnPageControlTabHasChanged';
begin
  try
    if CanCopyToCLipboard then
      FAppModules.SetMenuItem(CCopyToClipboard, msEnable)
    else
      FAppModules.SetMenuItem(CCopyToClipboard, msDisable);
    if CanExport then
      FAppModules.SetMenuItem(CExportToFile, msEnable)
    else
      FAppModules.SetMenuItem(CExportToFile, msDisable);
    if CanPrint then
      FAppModules.SetMenuItem(CPrint, msEnable)
    else
      FAppModules.SetMenuItem(CPrint, msDisable);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRWHGUIManager.CanCopyToCLipboard: boolean;
const OPNAME = 'TRWHGUIManager.CanCopyToCLipboard';
begin
  Result := False;
  try
    case CurrentTabSheet of
      mtsnInput : Result := FInputPageControl.CanCopyToCLipboard;
      mtsnOutput: Result := FOutputPageControl.CanCopyToCLipboard;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRWHGUIManager.CanExport: boolean;
const OPNAME = 'TRWHGUIManager.CanExport';
begin
  Result := False;
  try
    case CurrentTabSheet of
      mtsnInput : Result := FInputPageControl.CanExport;
      mtsnOutput: Result := FOutputPageControl.CanExport;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRWHGUIManager.CanPrint: boolean;
const OPNAME = 'TRWHGUIManager.CanPrint';
begin
  Result := False;
  try
    case CurrentTabSheet of
      mtsnInput : Result := FInputPageControl.CanPrint;
      mtsnOutput: Result := FOutputPageControl.CanPrint;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRWHGUIManager.DoCopyToCLipboard;
const OPNAME = 'TRWHGUIManager.DoCopyToCLipboard';
begin
  try
    case CurrentTabSheet of
      mtsnInput : FInputPageControl.DoCopyToCLipboard;
      mtsnOutput: FOutputPageControl.DoCopyToCLipboard;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRWHGUIManager.DoExport;
const OPNAME = 'TRWHGUIManager.DoExport';
begin
  try
    case CurrentTabSheet of
      mtsnInput : FInputPageControl.DoExport;
      mtsnOutput: FOutputPageControl.DoExport;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRWHGUIManager.DoPrint;
const OPNAME = 'TRWHGUIManager.DoPrint';
begin
  try
    case CurrentTabSheet of
      mtsnInput : FInputPageControl.DoPrint;
      mtsnOutput: FOutputPageControl.DoPrint;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRWHGUIManager.ExitCurrentEditControl;
const OPNAME = 'TRWHGUIManager.ExitCurrentEditControl';
begin
  try
    case CurrentTabSheet of
      mtsnInput : FInputPageControl.ExitCurrentEditControl;
      mtsnOutput: FOutputPageControl.ExitCurrentEditControl;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRWHGUIManager.OutputViewType(AViewName: string): boolean;
const OPNAME = 'TRWHGUIManager.OutputViewType';
begin
  Result := False;
  try
    AViewName := UpperCase(AViewName);
    Result :=(AViewName = 'OUTPUT');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRWHGUIManager.ViewInputDialog(AParent : TWincontrol; ACommaTextContextData : String;  AOwner : TWincontrol = nil): boolean;
const OPNAME = 'TRWHGUIManager.ViewInputDialog';
var
  LContextDataList : TStringList;
  LViewType        : string;
begin
  Result := False;
  try
    if (Trim(ACommaTextContextData) = '') then Exit;
    if(AParent = nil)  then
    begin
      Result := ViewInputPopupFormData(AParent,ACommaTextContextData,AOwner);
    end
    else
    begin
      LContextDataList := TStringList.Create;
      try
        LContextDataList.CommaText := ACommaTextContextData;
        LViewType := UpperCase(Trim(LContextDataList.Values['VIEWNAME']));
      finally
        FreeAndNil(LContextDataList);
      end;
      if(LViewType = '') then Exit;

      LViewType := UpperCase(LViewType);
      if OutputViewType(LViewType) then
       Result := ViewOutputData(AParent,ACommaTextContextData,AOwner)
      else
       Result := ViewInputData(AParent,ACommaTextContextData,AOwner);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRWHGUIManager.ViewInputPopupFormData(AParent: TWincontrol; ACommaTextContextData: String;
  AOwner: TWincontrol): boolean;
const OPNAME = 'TRWHGUIManager.ViewInputPopupFormData';
begin
  Result := False;
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRWHGUIManager.ViewInputData(AParent: TWincontrol; ACommaTextContextData: String;
                                                          AOwner: TWincontrol): boolean;
const OPNAME = 'TRWHGUIManager.ViewInputData';
var
  LViewType              : string;
  LContextDataList       : TStringList;
  //LDialogValidator       : TAbstractDataDialogValidator;
  LIdentifier            : integer;
  //LRWHSiteData           : TRWHSiteDataObject;
  LOldCursor             : TCursor;
begin
  Result := False;
  try
    FInputPageControl.DeleteAllTabSheets;
    FInputPageControl.Parent := AParent;
    FInputPageControl.Visible := False;

    if (Trim(ACommaTextContextData) = '') then
      Exit;

    LOldCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    LockWindowUpdate(AParent.Handle);
    LContextDataList := TStringList.Create;
    try
      LContextDataList.CommaText := ACommaTextContextData;
      LViewType := UpperCase(Trim(LContextDataList.Values['VIEWNAME']));
      if (LViewType = '') then Exit;

      LViewType     := UpperCase(LViewType);
      LIdentifier   := StrToInt(LContextDataList.Values['MODELELEMENTID']);
      if (LIdentifier = NullInteger) then
        Exit;

      FInputPageControl.Visible := True;
      {if (LViewType = 'IFRSITEDATA') then
      begin
        //IFR Data
        LRWHSiteData := TRWHModelData(FAppModules.Model.ModelData).RWHSiteDataList.RWHSiteDataByIdentifier[LIdentifier];
        if (LRWHSiteData <> nil)  then
        begin
          LDialogValidator := TRWHSiteValidator.Create(nil,FAppModules);
          FInputPageControl.AddValidator(LDialogValidator);
          LDialogValidator.Identifier := LIdentifier;
          LDialogValidator.Initialise;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;
        end;
      end;
      FInputPageControl.Visible := (FInputPageControl.ValidatorCount > 0);
      Result := FInputPageControl.Visible;
      FInputPageControl.LanguageHasChanged;
      FInputPageControl.SelectLastActiveTabsheet;
      }
   finally
     LockWindowUpdate(0);
     Screen.Cursor := LOldCursor;
     FreeAndNil(LContextDataList);
   end;

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRWHGUIManager.ViewOutputData(AParent: TWincontrol;ACommaTextContextData: String; AOwner: TWincontrol): boolean;
const OPNAME = 'TRWHGUIManager.ViewOutputData';
begin
  Result := False;
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
