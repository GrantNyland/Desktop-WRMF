//
//
//  UNIT      : Contains   UDailyDiversionGUIManager  Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 22/01/2004
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UDailyDiversionGUIManager;

interface
uses

  // Delphi
  classes,
  contnrs,
  VCL.controls,

  // DWAF
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UDailyDiversionGaugeData,
  UDailyDiversionDataObject,
  VoaimsCom_TLB;

type

  TDailyDiversionGUIManager = class(TAbstractAppObject)
  protected
    FInputPageControl   : TAbstractDataPageControl;
    FOutputPageControl  : TAbstractDataPageControl;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnPageControlTabHasChanged(Sender: TObject);
    function  ViewInputData(AParent : TWincontrol; ACommaTextContextData : String;  AOwner : TWincontrol = nil): boolean;
    function  ViewOutputData(AParent : TWincontrol; ACommaTextContextData : String;  AOwner : TWincontrol = nil): boolean;
    function OutputViewType(AViewName: string): boolean;
    procedure DoOnDataChange(Sender : TObject);
    procedure SetupSystemMenuState;
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
    procedure DoExport(AFileName: string = ''); virtual;
    procedure DoPrint; virtual;
    procedure ExitCurrentEditControl;
    function CurrentTabSheet: TModelTabSheetName;
    property InputPageControl : TAbstractDataPageControl read FInputPageControl;
  end;

implementation
uses
// Delphi
  VCL.Forms,
  Sysutils,
  Windows,

  // DWAF
  UConstants,
  UMonthlyReferenceFlowValidator,
  UMonthlyDiversionFlowValidator,
  UFlowDiversionRelationshipValidator,
  UDailyIFRDataValidator,
  UDailyIFRData,
  UErrorHandlingOperations, UMonthlyDiversionFlowDialog, VCL.ComCtrls;

const
  CCopyToClipboard     : array[0..1] of string = ('Edit','CopyToClipboard');
  CExportToFile        : array[0..1] of string = ('Edit','ExportToFile');
  CPrint               : array[0..1] of string = ('File','Print');

{ TDailyDiversionGUIManager }

procedure TDailyDiversionGUIManager.CreateMemberObjects;
const OPNAME = 'TDailyDiversionGUIManager.CreateMemberObjects';
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

procedure TDailyDiversionGUIManager.DestroyMemberObjects;
const OPNAME = 'TDailyDiversionGUIManager.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDailyDiversionGUIManager.CurrentTabSheet: TModelTabSheetName;
const OPNAME = 'TDailyDiversionGUIManager.CurrentTabSheet';
begin
  Result := mtsnNone;
  try
    if(FAppModules.MainForm <> nil) then
    begin
       Result := TAbstractTabSheet(FAppModules.MainForm.PageControl.ActivePage).ModelTabSheetName;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDailyDiversionGUIManager.Initialise: boolean;
const OPNAME = 'TDailyDiversionGUIManager.Initialise';
begin
  Result := inherited Initialise;;
  try
    FInputPageControl.Initialise;
    FOutputPageControl.Initialise;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDailyDiversionGUIManager.LanguageHasChanged: boolean;
const OPNAME = 'TDailyDiversionGUIManager.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FInputPageControl.Initialise;
    FOutputPageControl.Initialise;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDailyDiversionGUIManager.StudyHasChanged: boolean;
const OPNAME = 'TDailyDiversionGUIManager.StudyHasChanged';
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

function TDailyDiversionGUIManager.SaveState: boolean;
const OPNAME = 'TDailyDiversionGUIManager.SaveState';
begin
  Result := inherited SaveState;
  try
    FInputPageControl.SaveState;
    FOutputPageControl.SaveState;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDailyDiversionGUIManager.ProcessParameterChangeEvent : boolean;
const OPNAME = 'TDailyDiversionGUIManager.ProcessParameterChangeEvent';
begin
  Result := FALSE;
  try
    Result := FInputPageControl.ProcessParameterChangeEvent;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDailyDiversionGUIManager.ProcessMetaDataEvent : boolean;
const OPNAME = 'TDailyDiversionGUIManager.ProcessMetaDataEvent';
begin
  Result := FALSE;
  try
    Result := FInputPageControl.ProcessMetaDataEvent;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDailyDiversionGUIManager.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TDailyDiversionGUIManager.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    FInputPageControl.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);;
    FOutputPageControl.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDailyDiversionGUIManager.OnPageControlTabHasChanged(Sender: TObject);
const OPNAME = 'TDailyDiversionGUIManager.OnPageControlTabHasChanged';
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
    SetupSystemMenuState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDailyDiversionGUIManager.CanCopyToCLipboard: boolean;
const OPNAME = 'TDailyDiversionGUIManager.CanCopyToCLipboard';
begin
  Result := False;
  try
    case CurrentTabSheet of
      mtsnInput : Result := FInputPageControl.CanCopyToCLipboard;
      mtsnOutput: Result := FOutputPageControl.CanCopyToCLipboard;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDailyDiversionGUIManager.CanExport: boolean;
const OPNAME = 'TDailyDiversionGUIManager.CanExport';
var
  LDialogValidator : TAbstractDataDialogValidator;
begin
  Result := False;
  try
    LDialogValidator := FInputPageControl.GetValidatorByIndex(FInputPageControl.ActivePageIndex);

    if(LDialogValidator = nil) then Exit;

    if LDialogValidator.ClassNameIs('TMonthlyReferenceFlowValidator') then
      Result := TMonthlyReferenceFlowValidator(LDialogValidator).MonthlyReferenceFlowDialog.pgcAvgFlowFactor.ActivePageIndex in [1,3]
    else if LDialogValidator.ClassNameIs('TMonthlyDiversionFlowValidator') then
      Result := TMonthlyDiversionFlowValidator(LDialogValidator).MonthlyDiversionFlowDialog.pgcInstreamFlow.ActivePageIndex in [1,3]
    else if LDialogValidator.ClassNameIs('TFlowDiversionRelationshipValidator') then
      Result := TFlowDiversionRelationshipValidator(LDialogValidator).FlowDiversionRelationshipDialog.pgcFlowDiversionRelationship.ActivePageIndex > 0;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDailyDiversionGUIManager.CanPrint: boolean;
const OPNAME = 'TDailyDiversionGUIManager.CanPrint';
var
  LDialogValidator : TAbstractDataDialogValidator;
begin
  Result := False;
  try
    LDialogValidator := FInputPageControl.GetValidatorByIndex(FInputPageControl.ActivePageIndex);
    if(LDialogValidator = nil) then Exit;
    Result := LDialogValidator.ClassNameIs('TMonthlyReferenceFlowValidator') or
              LDialogValidator.ClassNameIs('TMonthlyDiversionFlowValidator') or
              LDialogValidator.ClassNameIs('TFlowDiversionRelationshipValidator');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDailyDiversionGUIManager.DoCopyToCLipboard;
const OPNAME = 'TDailyDiversionGUIManager.DoCopyToCLipboard';
begin
  try
    case CurrentTabSheet of
      mtsnInput : FInputPageControl.DoCopyToCLipboard;
      mtsnOutput: FOutputPageControl.DoCopyToCLipboard;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDailyDiversionGUIManager.DoExport(AFileName: string = '');
const OPNAME = 'TDailyDiversionGUIManager.DoExport';
var
  LDialogValidator : TAbstractDataDialogValidator;
  LResult          : boolean;
begin
  try
    LResult := False;
    LDialogValidator := FInputPageControl.GetValidatorByIndex(FInputPageControl.ActivePageIndex);

    if(LDialogValidator = nil) then Exit;

    if LDialogValidator.ClassNameIs('TMonthlyReferenceFlowValidator') then
      LResult := TMonthlyReferenceFlowValidator(LDialogValidator).MonthlyReferenceFlowDialog.pgcAvgFlowFactor.ActivePageIndex in [1,3]
    else if LDialogValidator.ClassNameIs('TMonthlyDiversionFlowValidator') then
      LResult := TMonthlyDiversionFlowValidator(LDialogValidator).MonthlyDiversionFlowDialog.pgcInstreamFlow.ActivePageIndex in [1,3]
    else if LDialogValidator.ClassNameIs('TFlowDiversionRelationshipValidator') then
      LResult := TFlowDiversionRelationshipValidator(LDialogValidator).FlowDiversionRelationshipDialog.pgcFlowDiversionRelationship.ActivePageIndex > 0;

    if LResult then
      FInputPageControl.DoExport(AFileName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDailyDiversionGUIManager.DoPrint;
const OPNAME = 'TDailyDiversionGUIManager.DoPrint';
var
  LDialogValidator : TAbstractDataDialogValidator;
  LResult          : boolean;
begin
  try
    LDialogValidator := FInputPageControl.GetValidatorByIndex(FInputPageControl.ActivePageIndex);
    if(LDialogValidator = nil) then Exit;
    LResult := LDialogValidator.ClassNameIs('TMonthlyReferenceFlowValidator') or
               LDialogValidator.ClassNameIs('TMonthlyDiversionFlowValidator') or
               LDialogValidator.ClassNameIs('TFlowDiversionRelationshipValidator');
    if LResult then
      FInputPageControl.DoPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDailyDiversionGUIManager.ExitCurrentEditControl;
const OPNAME = 'TDailyDiversionGUIManager.ExitCurrentEditControl';
begin
  try
    case CurrentTabSheet of
      mtsnInput : FInputPageControl.ExitCurrentEditControl;
      mtsnOutput: FOutputPageControl.ExitCurrentEditControl;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDailyDiversionGUIManager.OutputViewType(AViewName: string): boolean;
const OPNAME = 'TDailyDiversionGUIManager.OutputViewType';
begin
  Result := False;
  try
    AViewName := UpperCase(AViewName);
    Result :=(AViewName = 'OUTPUT');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDailyDiversionGUIManager.ViewInputDialog(AParent : TWincontrol; ACommaTextContextData : String;  AOwner : TWincontrol = nil): boolean;
const OPNAME = 'TDailyDiversionGUIManager.ViewInputDialog';
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

function TDailyDiversionGUIManager.ViewInputPopupFormData(
  AParent: TWincontrol; ACommaTextContextData: String;
  AOwner: TWincontrol): boolean;
const OPNAME = 'TDailyDiversionGUIManager.ViewInputPopupFormData';
begin
  Result := False;
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionGUIManager.ViewInputData(AParent: TWincontrol; ACommaTextContextData: String;
                                                          AOwner: TWincontrol): boolean;
const OPNAME = 'TDailyDiversionGUIManager.ViewInputData';
var
  LViewType              : string;
  LContextDataList       : TStringList;
  LDialogValidator       : TAbstractDataDialogValidator;
  LIdentifier            : integer;
  LDiversionGauge        : TDiversionGauge;
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
      if (LViewType = 'DAILYDIVERSIONDATA') then
      begin
        //DAILY DIVERSION STATION DATA
        LDiversionGauge := TDailyDiversionDataObject(FAppModules.Model.ModelData).
                           DailyDiversionGaugeDataList.DiversionGaugeByStationID[LIdentifier];
        if (LDiversionGauge <> nil) and (LDiversionGauge.DailyDataCount > 0) then
        begin
          LDialogValidator := TMonthlyReferenceFlowValidator.Create(nil,FAppModules);
          TMonthlyReferenceFlowValidator(LDialogValidator).MonthlyReferenceFlowDialog.pgcAvgFlowFactor.OnChange := OnPageControlTabHasChanged;
          FInputPageControl.AddValidator(LDialogValidator);
          LDialogValidator.Identifier := LIdentifier;
          LDialogValidator.Initialise;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;

          LDialogValidator := TMonthlyDiversionFlowValidator.Create(nil,FAppModules);
          TMonthlyDiversionFlowValidator(LDialogValidator).MonthlyDiversionFlowDialog.pgcInstreamFlow.OnChange := OnPageControlTabHasChanged;
          FInputPageControl.AddValidator(LDialogValidator);
          LDialogValidator.Identifier := LIdentifier;
          LDialogValidator.OnDataChange := DoOnDataChange;
          LDialogValidator.Initialise;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;

          if (LDiversionGauge.ImportIFR) then
          begin
            LDialogValidator := TDailyIFRDataValidator.Create(nil,FAppModules);
            FInputPageControl.AddValidator(LDialogValidator);
            LDialogValidator.Identifier := LIdentifier;
            LDialogValidator.Initialise;
            LDialogValidator.PopulateDataViewer;
            LDialogValidator.LanguageHasChanged;
          end;

          if LDiversionGauge.RankedFlowDiversionRelationshipCount > 0 then
          begin
            LDialogValidator := TFlowDiversionRelationshipValidator.Create(nil,FAppModules);
            TFlowDiversionRelationshipValidator(LDialogValidator).FlowDiversionRelationshipDialog.pgcFlowDiversionRelationship.OnChange := OnPageControlTabHasChanged;
            FInputPageControl.AddValidator(LDialogValidator);
            LDialogValidator.Identifier := LIdentifier;
            LDialogValidator.Initialise;
            LDialogValidator.PopulateDataViewer;
            LDialogValidator.LanguageHasChanged;
          end;
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

function TDailyDiversionGUIManager.ViewOutputData(AParent: TWincontrol;
  ACommaTextContextData: String; AOwner: TWincontrol): boolean;
const OPNAME = 'TDailyDiversionGUIManager.ViewOutputData';
begin
  Result := False;
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyDiversionGUIManager.DoOnDataChange(Sender : TObject);
const OPNAME = 'TDailyDiversionGUIManager.DoOnDataChange';
var
  LDialogValidator : TAbstractDataDialogValidator;
begin
  try
    if (Sender.ClassName = 'TMonthlyDiversionFlowValidator') then
    begin
      LDialogValidator := FInputPageControl.GetValidatorByClassName('TDailyIFRDataValidator');
      if (LDialogValidator = nil) and
        ((Sender as TMonthlyDiversionFlowValidator).MonthlyDiversionFlowDialog.rgrpImportIFR.ItemIndex = 1)  then
      begin
        LDialogValidator := TDailyIFRDataValidator.Create(nil,FAppModules);
        FInputPageControl.AddValidator(LDialogValidator);
        LDialogValidator.Identifier := (Sender as TMonthlyDiversionFlowValidator).Identifier;
        LDialogValidator.Initialise;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;
        FInputPageControl.LanguageHasChanged;
      end
      else
      if (LDialogValidator <> nil) and
        ((Sender as TMonthlyDiversionFlowValidator).MonthlyDiversionFlowDialog.rgrpImportIFR.ItemIndex = 0)  then
        FInputPageControl.DeleteValidatorByClassName('TDailyIFRDataValidator');

    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyDiversionGUIManager.SetupSystemMenuState;
const OPNAME = 'TDailyDiversionGUIManager.SetupSystemMenuState';
begin
  try
    FAppModules.MainForm.MenuItemManager.SetClipboardEnabled(CanCopyToClipboard);
    FAppModules.MainForm.MenuItemManager.SetExportEnabled(CanExport);
    FAppModules.PrintManager.SetPrintEnabled(CanPrint);
  except on E: Exception do
    HandleError(E, OPNAME)
  end;
end;

end.
