//
//
//  UNIT      : Contains TRainTabSheetManager Class
//  AUTHOR    : Sam Dhlamini(arivia.kom)
//  DATE      : 06/02/2004
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit URainfallGaugeSelectionTabSheet;

interface
{$WARN UNIT_PLATFORM OFF}
uses
  Classes,
  VCL.stdctrls,
  VCL.ComCtrls,
  VCL.extctrls,
  VCL.CheckLst,
  Math,
  VCL.Dialogs,
  VCL.Forms,
  Windows,
  UUtilities,
  UProgressDialog,
  UGenericModelLinkClasses,
  UHelpContexts,
  UAbstractObject,
  UAbstractComponent,
  URainfallGaugeSelectionMenuItemManager,
  URainfallGaugeSelectionValidator,
  RainfallCom_TLB,
  URaingaugeGISPanel,
  URGProgressDialog,
  USearchDialog,
  UShellExecuteObject;

type
  { Rain Tabsheet class }
  TRainfallGaugeSelectionTabSheet = class( TAbstractTabSheet  )
  private
    FMenuItemManager                 : TRainfallGaugeSelectionMenuItemManager;
    FRainfallGaugeSelectionValidator : TRainfallGaugeSelectionValidator;

    FReArangeData                    : boolean;
    FAppendData                      : boolean;
    FOverwrite                       : boolean;
    FHydroStartMonth                 : boolean; 
    FProgressDialog                   : TProgressDialog;
    function GetMenuItemManager: TRainfallGaugeSelectionMenuItemManager;
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnItemCheckUncheck(Sender: TObject);
    function GetToolBar: TAbstractToolBar; override;
    function ImportStationData(AFileName : string) : boolean;

  public
    procedure DoCreateReport(Sender: TObject);
    procedure DoImportUserData(ASender: TObject);
    procedure DoClearUserData(ASender: TObject);
    procedure DoImportSawsDwafData(ASender: TObject);

    procedure DoExport(AFileName: string = ''); override;
    function CanExport: Boolean; override;
    function CanCopyToCLipboard: Boolean; override;
    procedure DoCopyToCLipboard; override;
    function CanPrint: Boolean; override;
    procedure DoPrint; override;

    procedure SetMenuVisible ( AVisible : Boolean ); override;
    function Initialise: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged (AContext   : TChangeContext;
                                  AFieldName : string;
                                  AOldValue  : string;
                                  ANewValue  : string): Boolean; override;
    function CanTabChange: Boolean; override;
    procedure TabHasChanged; override;
    function DoCustomTabSheetEvent (ACustomModelEvent : TModelMenuData): Boolean; override;
    property MenuItemManager: TRainfallGaugeSelectionMenuItemManager read GetMenuItemManager;

end;

implementation

uses
  SysUtils,
  VCL.Controls,
  VCL.FileCtrl,
  VCL.Graphics,
  VCL.Clipbrd,
  VCL.Printers,
  UDataObjectRainfall,
  URainfallImportDialog,
  UErrorHandlingOperations;

{ TRainfallGaugeSelectionTabSheet }

procedure TRainfallGaugeSelectionTabSheet.CreateMemberObjects;
const OPNAME = 'TRainfallGaugeSelectionTabSheet.CreateMemberObjects';
const LPracticalHeight : integer = 300;
begin
  inherited CreateMemberObjects;
  try
    FRainfallGaugeSelectionValidator := TRainfallGaugeSelectionValidator.Create(Self, FAppModules);
    FRainfallGaugeSelectionValidator.Panel.Parent := Self;
    FRainfallGaugeSelectionValidator.Panel.Align  := alClient;

    FMenuItemManager       := TRainfallGaugeSelectionMenuItemManager.Create(FAppModules);
    FRainfallGaugeSelectionValidator.MenuItemManager  := FMenuItemManager;
    MenuItemManager.ToolBar.CreateReportBtn.OnClick   := DoCreateReport;
    MenuItemManager.ToolBar.ToggleTreeBtn.OnClick     := FRainfallGaugeSelectionValidator.DoToggleTree;
    MenuItemManager.ToolBar.ImportUserDataBtn.OnClick := DoImportUserData;
    MenuItemManager.ToolBar.ClearUserDataBtn.OnClick  := DoClearUserData;

  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TRainfallGaugeSelectionTabSheet.DestroyMemberObjects;
const OPNAME = 'TRainfallGaugeSelectionTabSheet.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FreeAndNil(FMenuItemManager);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallGaugeSelectionTabSheet.GetToolBar: TAbstractToolBar;
const OPNAME = 'TRainfallGaugeSelectionTabSheet.GetToolBar';
begin
  Result := nil;
  try
    Result := TRainfallGaugeSelectionMenuItemManager(FMenuItemManager).ToolBar;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallGaugeSelectionTabSheet.ImportStationData(AFileName : string): boolean;
const OPNAME = 'TRainfallGaugeSelectionTabSheet.ImportStationData';
var
  LFileData : TStringList;
  LRainfallObj : TDataObjectRainfall;

begin
  Result := False;
  try
    if Trim(AFileName) <> '' then
    begin
      LFileData := TStringList.Create;
      try
        LFileData.LoadFromFile(AFileName);
        if LFileData.Count > 0 then
        begin
          FProgressDialog := TProgressDialog.Create(nil,FAppModules);
          FProgressDialog.clbOptions.OnClickCheck := OnItemCheckUncheck;

          LRainfallObj := TDataObjectRainfall(FAppModules.Model.ModelData);
          try
            if LRainfallObj <> nil then
            begin
              if LRainfallObj.SetImportRawStationData(LFileData,FReArangeData,FAppendData,FOverwrite) then
              begin
                FProgressDialog.AddExecutionFunctions(LRainfallObj.ImportRawStationData);
                FProgressDialog.Caption := 'Import Station Data';
                FProgressDialog.Succsessful := False;
                FProgressDialog.clbOptions.Items.Add('Re-arange Data to Monthly Format');
                FProgressDialog.clbOptions.Items.Add('Append Data');
                FProgressDialog.clbOptions.Items.Add('Over-write Data');
                FProgressDialog.clbOptions.Items.Add('Set Hydro Start Month');
                FProgressDialog.ShowModal;

              end;
            end;
          finally
            FreeAndNil(FProgressDialog);
          end;
        end;
      finally
        FreeAndNil(LFileData);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGaugeSelectionTabSheet.OnItemCheckUncheck(Sender: TObject);
const OPNAME = 'TRainfallGaugeSelectionTabSheet.OnItemCheckUncheck';
begin
  try
    FReArangeData := FProgressDialog.clbOptions.Checked[1];
    FAppendData := FProgressDialog.clbOptions.Checked[2];
    FOverwrite := FProgressDialog.clbOptions.Checked[3];
    FHydroStartMonth := FProgressDialog.clbOptions.Checked[4];
    TDataObjectRainfall(FAppModules.Model.ModelData).SetImportProperties(FReArangeData,FAppendData,FOverwrite,FHydroStartMonth);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallGaugeSelectionTabSheet.Initialise: boolean;
const OPNAME = 'TRainfallGaugeSelectionTabSheet.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := FRainfallGaugeSelectionValidator.Initialise;
    FMenuItemManager.SetMenuState(mcFileIsLoaded);
    SetMenuVisible(FALSE);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallGaugeSelectionTabSheet.LanguageHasChanged: boolean;
const OPNAME = 'TRainfallGaugeSelectionTabSheet.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;;
  try
    FTabCaptionKey := 'Rainfall';
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallGaugeSelectionTabSheet.StudyHasChanged: boolean;
const OPNAME = 'TRainfallGaugeSelectionTabSheet.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    FRainfallGaugeSelectionValidator.PopulateDataViewer;
    Result := true;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallGaugeSelectionTabSheet.StudyDataHasChanged (AContext   : TChangeContext;
                                                              AFieldName : string;
                                                              AOldValue  : string;
                                                              ANewValue  : string): Boolean;
const OPNAME = 'TRainfallGaugeSelectionTabSheet.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext, AFieldName, AOldValue, ANewValue);
  try
    Result := FRainfallGaugeSelectionValidator.StudyDataHasChanged(AContext, AFieldName, AOldValue, ANewValue);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGaugeSelectionTabSheet.DoCreateReport(Sender: TObject);
const OPNAME = 'TRainfallGaugeSelectionTabSheet.DoCreateReport';
var
  lDirectory    : string;
  lRainfallData : IRainfallModelData;
  lDataList     : TStringList;
  lFile         : TFileStream;
begin
  try
    lDirectory := GetAppDataLocalDir+'\WRCDATA\'; //ExtractFilePath(ApplicationExeName) + 'wrcdata\';
    if (SelectDirectory(lDirectory, [sdPerformCreate], 0)) then
    begin
      lDirectory := lDirectory + '\';
      lRainfallData := (FAppModules.Model.ModelData as IRainfallModelData);

      lDataList := TStringList.Create;
      try
        lDataList.CommaText := lRainfallData.CreateReport;
        if (lDataList.Count > 0) then
        begin
          lFile  := TFileStream.Create(lDirectory + FAppModules.StudyArea.SubAreaCode + '.txt', fmCreate);
          try
            lDataList.SaveToStream(lFile);
          finally
            FreeAndNil(lFile);
          end;
        end
      finally
        FreeAndNil(lDataList);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGaugeSelectionTabSheet.DoImportUserData(ASender: TObject);
const OPNAME = 'TRainfallGaugeSelectionTabSheet.DoImportUserData';
var
  lForm : TRainfallImportDialog;
begin
  try
    lForm := TRainfallImportDialog.CreateWithoutDFM(nil, FAppModules);
    try
      lForm.Initialise;
      lForm.LanguageHasChanged;
      lForm.ShowModal;
    finally
      FreeAndNil(lForm);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGaugeSelectionTabSheet.DoImportSawsDwafData(ASender: TObject);
const OPNAME = 'TRainfallGaugeSelectionTabSheet.DoImportSawsDwafData';
var
  LOpenDialog : TOpenDialog;
begin
  try
    LOpenDialog := TOpenDialog.Create(nil);
    try
      LOpenDialog.FileEditStyle := fsEdit;
      LOpenDialog.Title := 'Select a space separated file to import...';
      LOpenDialog.FileName := '';
      LOpenDialog.Filter := 'TXT files (*.txt)|*.txt';
      if LOpenDialog.Execute then
        ImportStationData(LOpenDialog.FileName);
    finally
      FreeAndNil(LOpenDialog);
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TRainfallGaugeSelectionTabSheet.CanTabChange: Boolean;
const OPNAME = 'TRainfallGaugeSelectionTabSheet.CanTabChange';
begin
  Result := True;
  try
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TRainfallGaugeSelectionTabSheet.TabHasChanged;
const OPNAME = 'TRainfallGaugeSelectionTabSheet.TabHasChanged';
begin
  inherited TabHasChanged;
  try
    FRainfallGaugeSelectionValidator.PopulateDataViewer;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TRainfallGaugeSelectionTabSheet.GetMenuItemManager : TRainfallGaugeSelectionMenuItemManager;
const OPNAME = 'TRainfallGaugeSelectionTabSheet.GetMenuItemManager';
begin
  Result := nil;
  try
    Result := TRainfallGaugeSelectionMenuItemManager ( FMenuItemManager );
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGaugeSelectionTabSheet.SetMenuVisible ( AVisible: Boolean );
const OPNAME = 'TRainfallGaugeSelectionTabSheet.SetMenuVisible';
begin
  inherited SetMenuVisible ( AVisible );
  try
    if Assigned ( MenuItemManager ) then
    begin
      if AVisible then
        MenuItemManager.Show
      else
        MenuItemManager.Hide;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallGaugeSelectionTabSheet.CanPrint : Boolean;
const OPNAME = 'TRainfallGaugeSelectionTabSheet.CanPrint';
begin
  Result := FALSE;
  try
    Result := FRainfallGaugeSelectionValidator.CanPrint;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGaugeSelectionTabSheet.DoPrint;
const OPNAME = 'TRainfallGaugeSelectionTabSheet.DoPrint';
begin
  try
    FRainfallGaugeSelectionValidator.DoPrint;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallGaugeSelectionTabSheet.CanCopyToCLipboard : Boolean;
const OPNAME = 'TRainfallGaugeSelectionTabSheet.CanCopyToCLipboard';
begin
  Result := FALSE;
  try
    Result := FRainfallGaugeSelectionValidator.CanCopyToClipboard;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGaugeSelectionTabSheet.DoCopyToCLipboard;
const OPNAME = 'TRainfallGaugeSelectionTabSheet.DoCopyToCLipboard';
begin
  try
    FRainfallGaugeSelectionValidator.DoCopyToCLipboard;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallGaugeSelectionTabSheet.CanExport : Boolean;
const OPNAME = 'TRainfallGaugeSelectionTabSheet.CanExport';
begin
  Result := FALSE;
  try
    Result := FRainfallGaugeSelectionValidator.CanExport;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGaugeSelectionTabSheet.DoExport(AFileName: string = '');
const OPNAME = 'TRainfallGaugeSelectionTabSheet.DoExport';
begin
  try
    FRainfallGaugeSelectionValidator.DoExport(AFileName);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallGaugeSelectionTabSheet.DoCustomTabSheetEvent (ACustomModelEvent : TModelMenuData): Boolean;
const OPNAME = 'TRainfallGaugeSelectionTabSheet.DoCustomTabSheetEvent';
begin
  Result := False;
  try
    if Assigned( ACustomModelEvent ) then
    begin
      case ACustomModelEvent.Action of
        MeFileReport             :
          begin
            DoCreateReport(nil);
            Result := TRUE;
          end;
        meImportUserData :
          begin
            DoImportUserData(nil);
            Result := TRUE;
          end;
        meClearUserData :
          begin
            DoClearUserData(nil);
            Result := TRUE;
          end;
        meImportSawsDwafData :
          begin
            DoImportSawsDwafData(nil);
            Result := TRUE;
          end;
        MeOptionUpdateGISLive    :
          begin
            ShowMessage(FAppModules.Language.GetString('Message.OptionNotImplemented') + #10 + #13 +
                        FAppModules.Language.GetString('Message.ImplementWhenGISViewerImproves'));
            Result := TRUE;
          end;
        MeViewStatusBar,
        MeUnSelectAll,
        MeUnSelect,
        MeSelectByStationNumber,
        MeSelectByStationName,
        MeSelectRect,
        MeSelectByDistance,
        MeOptionAddToSelection,
        meToggleRainGaugeTree    :
          Result := FRainfallGaugeSelectionValidator.DoCustomEvent(ACustomModelEvent);
      else
        ShowMessage(FAppModules.Language.GetString('Message.MenuClickNotHandled'));
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGaugeSelectionTabSheet.DoClearUserData(ASender: TObject);
const OPNAME = 'TRainfallGaugeSelectionTabSheet.DoClearUserData';
begin
  try
    FRainfallGaugeSelectionValidator.DoClearUserData(nil);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.


