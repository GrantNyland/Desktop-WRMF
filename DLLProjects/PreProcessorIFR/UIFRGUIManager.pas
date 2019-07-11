//
//
//  UNIT      : Contains TIFRGUIManager Class
//  AUTHOR    : Dziedzi Ramulondi (arivia.kom)
//  DATE      : 09/01/2007
//  COPYRIGHT : Copyright © 2007 DWAF
//
//
unit UIFRGUIManager;

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
  TIFRGUIManager = class(TAbstractAppObject)
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
  UIFRDataObject,
  UIFRSiteValidator,
  UErrorHandlingOperations;
const
  CCopyToClipboard     : array[0..1] of WideString = ('Edit','CopyToClipboard');
  CExportToFile        : array[0..1] of WideString = ('Edit','ExportToFile');
  CPrint               : array[0..1] of WideString = ('File','Print');

{ TIFRGUIManager }

procedure TIFRGUIManager.CreateMemberObjects;
const OPNAME = 'TIFRGUIManager.CreateMemberObjects';
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

procedure TIFRGUIManager.DestroyMemberObjects;
const OPNAME = 'TIFRGUIManager.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIFRGUIManager.CurrentTabSheet: TModelTabSheetName;
const OPNAME = 'TIFRGUIManager.CurrentTabSheet';
begin
  Result := mtsnNone;
  try
    if(FAppModules.MainForm <> nil) then
    begin
       Result := TAbstractTabSheet(FAppModules.MainForm.PageControl.ActivePage).ModelTabSheetName;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIFRGUIManager.Initialise: boolean;
const OPNAME = 'TIFRGUIManager.Initialise';
begin
  Result := inherited Initialise;;
  try
    FInputPageControl.Initialise;
    FOutputPageControl.Initialise;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIFRGUIManager.LanguageHasChanged: boolean;
const OPNAME = 'TIFRGUIManager.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FInputPageControl.Initialise;
    FOutputPageControl.Initialise;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIFRGUIManager.StudyHasChanged: boolean;
const OPNAME = 'TIFRGUIManager.StudyHasChanged';
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

function TIFRGUIManager.SaveState: boolean;
const OPNAME = 'TIFRGUIManager.SaveState';
begin
  Result := inherited SaveState;
  try
    FInputPageControl.SaveState;
    FOutputPageControl.SaveState;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIFRGUIManager.ProcessParameterChangeEvent : boolean;
const OPNAME = 'TIFRGUIManager.ProcessParameterChangeEvent';
begin
  Result := FALSE;
  try
    Result := FInputPageControl.ProcessParameterChangeEvent;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIFRGUIManager.ProcessMetaDataEvent : boolean;
const OPNAME = 'TIFRGUIManager.ProcessMetaDataEvent';
begin
  Result := FALSE;
  try
    Result := FInputPageControl.ProcessMetaDataEvent;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIFRGUIManager.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TIFRGUIManager.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    FInputPageControl.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);;
    FOutputPageControl.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRGUIManager.OnPageControlTabHasChanged(Sender: TObject);
const OPNAME = 'TIFRGUIManager.OnPageControlTabHasChanged';
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

function TIFRGUIManager.CanCopyToCLipboard: boolean;
const OPNAME = 'TIFRGUIManager.CanCopyToCLipboard';
begin
  Result := False;
  try
    case CurrentTabSheet of
      mtsnInput : Result := FInputPageControl.CanCopyToCLipboard;
      mtsnOutput: Result := FOutputPageControl.CanCopyToCLipboard;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIFRGUIManager.CanExport: boolean;
const OPNAME = 'TIFRGUIManager.CanExport';
begin
  Result := False;
  try
    case CurrentTabSheet of
      mtsnInput : Result := FInputPageControl.CanExport;
      mtsnOutput: Result := FOutputPageControl.CanExport;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIFRGUIManager.CanPrint: boolean;
const OPNAME = 'TIFRGUIManager.CanPrint';
begin
  Result := False;
  try
    case CurrentTabSheet of
      mtsnInput : Result := FInputPageControl.CanPrint;
      mtsnOutput: Result := FOutputPageControl.CanPrint;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRGUIManager.DoCopyToCLipboard;
const OPNAME = 'TIFRGUIManager.DoCopyToCLipboard';
begin
  try
    case CurrentTabSheet of
      mtsnInput : FInputPageControl.DoCopyToCLipboard;
      mtsnOutput: FOutputPageControl.DoCopyToCLipboard;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRGUIManager.DoExport;
const OPNAME = 'TIFRGUIManager.DoExport';
begin
  try
    case CurrentTabSheet of
      mtsnInput : FInputPageControl.DoExport;
      mtsnOutput: FOutputPageControl.DoExport;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRGUIManager.DoPrint;
const OPNAME = 'TIFRGUIManager.DoPrint';
begin
  try
    case CurrentTabSheet of
      mtsnInput : FInputPageControl.DoPrint;
      mtsnOutput: FOutputPageControl.DoPrint;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRGUIManager.ExitCurrentEditControl;
const OPNAME = 'TIFRGUIManager.ExitCurrentEditControl';
begin
  try
    case CurrentTabSheet of
      mtsnInput : FInputPageControl.ExitCurrentEditControl;
      mtsnOutput: FOutputPageControl.ExitCurrentEditControl;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIFRGUIManager.OutputViewType(AViewName: string): boolean;
const OPNAME = 'TIFRGUIManager.OutputViewType';
begin
  Result := False;
  try
    AViewName := UpperCase(AViewName);
    Result :=(AViewName = 'OUTPUT');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIFRGUIManager.ViewInputDialog(AParent : TWincontrol; ACommaTextContextData : String;  AOwner : TWincontrol = nil): boolean;
const OPNAME = 'TIFRGUIManager.ViewInputDialog';
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

function TIFRGUIManager.ViewInputPopupFormData(AParent: TWincontrol; ACommaTextContextData: String;
  AOwner: TWincontrol): boolean;
const OPNAME = 'TIFRGUIManager.ViewInputPopupFormData';
begin
  Result := False;
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TIFRGUIManager.ViewInputData(AParent: TWincontrol; ACommaTextContextData: String;
                                                          AOwner: TWincontrol): boolean;
const OPNAME = 'TIFRGUIManager.ViewInputData';
var
  LViewType              : string;
  LContextDataList       : TStringList;
  LDialogValidator       : TAbstractDataDialogValidator;
  LIdentifier            : integer;
  LIFRSiteData        : TIFRSiteDataObject;
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
      if (LViewType = 'IFRSITEDATA') then
      begin
        //IFR Data
        LIFRSiteData := TIFRModelData(FAppModules.Model.ModelData).IFRSiteDataList.IFRSiteDataByIdentifier[LIdentifier];
        if (LIFRSiteData <> nil)  then
        begin
          LDialogValidator := TIFRSiteValidator.Create(nil,FAppModules);
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

   finally
     LockWindowUpdate(0);
     Screen.Cursor := LOldCursor;
     FreeAndNil(LContextDataList);
   end;

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TIFRGUIManager.ViewOutputData(AParent: TWincontrol;ACommaTextContextData: String; AOwner: TWincontrol): boolean;
const OPNAME = 'TIFRGUIManager.ViewOutputData';
begin
  Result := False;
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
