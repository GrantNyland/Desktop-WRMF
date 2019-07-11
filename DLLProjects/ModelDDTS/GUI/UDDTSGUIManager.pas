//
//
//  UNIT      : Contains   UDailyDiversionGUIManager  Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 07/07/2014
//  COPYRIGHT : Copyright © 2014 DWA
//
//

unit UDDTSGUIManager;

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
  UReservoirPhysicalCharacteristicsValidator,
  UDDTSData,
  UDDTSDataObject,
  VoaimsCom_TLB;

type

  TDDTSGUIManager = class(TAbstractAppObject)
  protected
    FInputPageControl   : TAbstractDataPageControl;
    FOutputPageControl  : TAbstractDataPageControl;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnPageControlTabHasChanged(Sender: TObject);
    function  ViewInputData(AParent : TWincontrol; ACommaTextContextData : String;  AOwner : TWincontrol = nil): boolean;
    function  ViewOutputData(AParent : TWincontrol; ACommaTextContextData : String;  AOwner : TWincontrol = nil): boolean;
    function OutputViewType(AViewName: string): boolean;
    //procedure DoOnDataChange(Sender : TObject);
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
    function ViewOutputDialog(AParent : TWincontrol; ACommaTextContextData : String; AOwner : TWincontrol = nil): boolean;
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
    property OutputPageControl : TAbstractDataPageControl read   FOutputPageControl;
  end;

implementation
uses
// Delphi
  VCL.Forms,
  Sysutils,
  Windows,

  // DWAF
  UConstants,
  UDDTSDamConfigValidator,
  UDDTSDamPropertiesValidator,
  UDDTSInputDataValidator,
  UDDTSInputMinMaxValidator,
  UOutputGridValidator,
  UOutputGraphValidator,
  UErrorHandlingOperations, VCL.ComCtrls;

const
  CCopyToClipboard     : array[0..1] of WideString = ('Edit','CopyToClipboard');
  CExportToFile        : array[0..1] of WideString = ('Edit','ExportToFile');
  CPrint               : array[0..1] of WideString = ('File','Print');

{ TDDTSGUIManager }

procedure TDDTSGUIManager.CreateMemberObjects;
const OPNAME = 'TDDTSGUIManager.CreateMemberObjects';
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

procedure TDDTSGUIManager.DestroyMemberObjects;
const OPNAME = 'TDDTSGUIManager.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
    FreeAndNil(FInputPageControl);
    FreeAndNil(FOutputPageControl);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSGUIManager.CurrentTabSheet: TModelTabSheetName;
const OPNAME = 'TDDTSGUIManager.CurrentTabSheet';
begin
  Result := mtsnNone;
  try
    if(FAppModules.MainForm <> nil) then
    begin
       Result := TAbstractTabSheet(FAppModules.MainForm.PageControl.ActivePage).ModelTabSheetName;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSGUIManager.Initialise: boolean;
const OPNAME = 'TDDTSGUIManager.Initialise';
begin
  Result := inherited Initialise;;
  try
    FInputPageControl.Initialise;
    FOutputPageControl.Initialise;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSGUIManager.LanguageHasChanged: boolean;
const OPNAME = 'TDDTSGUIManager.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FInputPageControl.Initialise;
    FOutputPageControl.Initialise;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSGUIManager.StudyHasChanged: boolean;
const OPNAME = 'TDDTSGUIManager.StudyHasChanged';
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

function TDDTSGUIManager.SaveState: boolean;
const OPNAME = 'TDDTSGUIManager.SaveState';
begin
  Result := inherited SaveState;
  try
    FInputPageControl.SaveState;
    FOutputPageControl.SaveState;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSGUIManager.ProcessParameterChangeEvent : boolean;
const OPNAME = 'TDDTSGUIManager.ProcessParameterChangeEvent';
begin
  Result := FALSE;
  try
    Result := FInputPageControl.ProcessParameterChangeEvent;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSGUIManager.ProcessMetaDataEvent : boolean;
const OPNAME = 'TDDTSGUIManager.ProcessMetaDataEvent';
begin
  Result := FALSE;
  try
    Result := FInputPageControl.ProcessMetaDataEvent;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSGUIManager.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TDDTSGUIManager.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    FInputPageControl.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);;
    FOutputPageControl.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSGUIManager.OnPageControlTabHasChanged(Sender: TObject);
const OPNAME = 'TDDTSGUIManager.OnPageControlTabHasChanged';
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

function TDDTSGUIManager.CanCopyToCLipboard: boolean;
const OPNAME = 'TDDTSGUIManager.CanCopyToCLipboard';
begin
  Result := False;
  try
    case CurrentTabSheet of
      mtsnInput : Result := FInputPageControl.CanCopyToCLipboard;
      mtsnOutput: Result := FOutputPageControl.CanCopyToCLipboard;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSGUIManager.CanExport: boolean;
const OPNAME = 'TDDTSGUIManager.CanExport';
var
  LDialogValidator : TAbstractDataDialogValidator;
begin
  Result := False;
  try
    LDialogValidator := FInputPageControl.GetValidatorByIndex(FInputPageControl.ActivePageIndex);

    if(LDialogValidator = nil) then Exit;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSGUIManager.CanPrint: boolean;
const OPNAME = 'TDDTSGUIManager.CanPrint';
var
  LDialogValidator : TAbstractDataDialogValidator;
begin
  Result := False;
  try
    LDialogValidator := FInputPageControl.GetValidatorByIndex(FInputPageControl.ActivePageIndex);
    if(LDialogValidator = nil) then Exit;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSGUIManager.DoCopyToCLipboard;
const OPNAME = 'TDDTSGUIManager.DoCopyToCLipboard';
begin
  try
    case CurrentTabSheet of
      mtsnInput : FInputPageControl.DoCopyToCLipboard;
      mtsnOutput: FOutputPageControl.DoCopyToCLipboard;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSGUIManager.DoExport(AFileName: string = '');
const OPNAME = 'TDDTSGUIManager.DoExport';
var
  LDialogValidator : TAbstractDataDialogValidator;
  LResult          : boolean;
begin
  try

    LDialogValidator := FInputPageControl.GetValidatorByIndex(FInputPageControl.ActivePageIndex);

    if(LDialogValidator = nil) then Exit;
    LResult := True;
    if LResult then
      FInputPageControl.DoExport(AFileName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSGUIManager.DoPrint;
const OPNAME = 'TDDTSGUIManager.DoPrint';
var
  LDialogValidator : TAbstractDataDialogValidator;
  LResult          : boolean;
begin
  try
    LDialogValidator := FInputPageControl.GetValidatorByIndex(FInputPageControl.ActivePageIndex);
    if(LDialogValidator = nil) then Exit;
     LResult := True;
    if LResult then
      FInputPageControl.DoPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSGUIManager.ExitCurrentEditControl;
const OPNAME = 'TDDTSGUIManager.ExitCurrentEditControl';
begin
  try
    case CurrentTabSheet of
      mtsnInput : FInputPageControl.ExitCurrentEditControl;
      mtsnOutput: FOutputPageControl.ExitCurrentEditControl;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSGUIManager.OutputViewType(AViewName: string): boolean;
const OPNAME = 'TDDTSGUIManager.OutputViewType';
begin
  Result := False;
  try
    AViewName := UpperCase(AViewName);
    Result :=(AViewName = 'RESERVOIR');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSGUIManager.ViewInputDialog(AParent : TWincontrol; ACommaTextContextData : String;  AOwner : TWincontrol = nil): boolean;
const OPNAME = 'TDDTSGUIManager.ViewInputDialog';
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

function TDDTSGUIManager.ViewInputPopupFormData(
  AParent: TWincontrol; ACommaTextContextData: String;
  AOwner: TWincontrol): boolean;
const OPNAME = 'TDDTSGUIManager.ViewInputPopupFormData';
begin
  Result := False;
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDDTSGUIManager.ViewInputData(AParent: TWincontrol; ACommaTextContextData: String;
                                                          AOwner: TWincontrol): boolean;
const OPNAME = 'TDDTSGUIManager.ViewInputData';
var
  LViewType              : string;
  LContextDataList       : TStringList;
  LDialogValidator       : TAbstractDataDialogValidator;
  LIdentifier            : integer;
  LOldCursor             : TCursor;
  LReservoir : IReservoirData;
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

      if (LViewType = 'RUNCONFIGURATION') then
      begin
          LDialogValidator := TDDTSDamConfigValidator.Create(nil,FAppModules);
          FInputPageControl.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TDDTSDamConfigValidator(LDialogValidator).Identifier := 1;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;

          LDialogValidator := TDDTSInputMinMaxValidator.Create(nil,FAppModules);
          FInputPageControl.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TDDTSInputMinMaxValidator(LDialogValidator).Identifier := 1;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;

          LDialogValidator := TDDTSInputDataValidator.Create(nil,FAppModules);
          FInputPageControl.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TDDTSInputDataValidator(LDialogValidator).Identifier := 1;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;

      end;

      if (LViewType = 'DDTSDAMDATA') then
      begin



        LReservoir := TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.CastReservoirList.ReservoirByIdentifier[LIdentifier];
        if LReservoir <> nil then
        begin


          LDialogValidator := TDDTSDamPropertiesValidator.Create(nil,FAppModules);
          FInputPageControl.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TDDTSDamPropertiesValidator(LDialogValidator).Identifier := LReservoir.ReservoirConfigurationData.RecordIdentifier;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;


          LDialogValidator := TReservoirPhysicalCharacteristicsValidator.Create(nil,FAppModules);
          FInputPageControl.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TReservoirPhysicalCharacteristicsValidator(LDialogValidator).Identifier := LReservoir.ReservoirConfigurationData.RecordIdentifier;
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


function TDDTSGUIManager.ViewOutputDialog(AParent : TWincontrol; ACommaTextContextData : String; AOwner : TWincontrol = nil): boolean;
const OPNAME = 'TDDTSGUIManager.ViewOutputDialog';
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

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSGUIManager.ViewOutputData(AParent: TWincontrol;
  ACommaTextContextData: String; AOwner: TWincontrol): boolean;
const OPNAME = 'TDDTSGUIManager.ViewOutputData';
var
  LViewName           : string;
  LElementName        : string;
  LContextDataList    : TStringList;
  LDialogValidator    : TAbstractDataDialogValidator;
  LIdentifier         : integer;
  LNetworkElementType : TNetworkElementType;
  LOldCursor          : TCursor;
 // LNodeIndex          : integer;
begin
  Result := False;
  try

    FOutputPageControl.DeleteAllTabSheets;
    FOutputPageControl.Parent := AParent;
    FOutputPageControl.Visible := False;

    if (Trim(ACommaTextContextData) = '') then
      Exit;

    LOldCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    LockWindowUpdate(AParent.Handle);

    if (Trim(ACommaTextContextData) = '') then Exit;
    try
      LContextDataList := TStringList.Create;
      try
        LContextDataList.CommaText := ACommaTextContextData;
        LViewName := UpperCase(Trim(LContextDataList.Values['VIEWNAME']));
        if(LViewName = '') then Exit;

        LIdentifier   := StrToIntDef(LContextDataList.Values['MODELELEMENTID'],NullInteger);
        if(LIdentifier = NullInteger) then Exit;
  //      LNodeIndex    := StrToIntDef(LContextDataList.Values['NODEINDEX'],-1);

        LElementName := LContextDataList.Values['MODELELEMENTNAME'];
        if(LElementName <> '') then
          LElementName := AnsiDequotedStr(LElementName,'''');
      finally
        FreeAndNil(LContextDataList);
      end;


      if(LViewName = mdvnReservoir)then
      begin
        LNetworkElementType := votReviewDamStorage;

          // Create the grid
        LDialogValidator := TOutputGridValidator.Create(nil,FAppModules);
        FOutputPageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TOutputGridValidator(LDialogValidator).NetworkElementType   :=  LNetworkElementType;
        TOutputGridValidator(LDialogValidator).Identifier :=  LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;


        // Create the graph
        LDialogValidator := TOutputGraphValidator.Create(nil,FAppModules);
        FOutputPageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TOutputGraphValidator(LDialogValidator).NetworkElementType   :=  LNetworkElementType;
        TOutputGraphValidator(LDialogValidator).Identifier :=  LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;
      end;
      FOutputPageControl.Visible := (FOutputPageControl.ValidatorCount > 0);
      Result := FOutputPageControl.Visible;
      FOutputPageControl.LanguageHasChanged;
      FOutputPageControl.SelectLastActiveTabsheet;

    finally
      LockWindowUpdate(0);
      Screen.Cursor := LOldCursor;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


(*
procedure TDDTSGUIManager.DoOnDataChange(Sender : TObject);
const OPNAME = 'TDDTSGUIManager.DoOnDataChange';
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
   *)
procedure TDDTSGUIManager.SetupSystemMenuState;
const OPNAME = 'TDDTSGUIManager.SetupSystemMenuState';
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
