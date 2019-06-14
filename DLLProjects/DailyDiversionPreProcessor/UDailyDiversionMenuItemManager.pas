
//
//
//  UNIT      : Contains TDailyDiversionMenuItemManager Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 22/01/2004
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UDailyDiversionMenuItemManager;

interface
uses

  UMenuItemManager,
  UGenericModelLinkClasses,
  UHelpContexts,
  UAbstractComponent,
  UAbstractObject,
  UDailyDiversionMenuItemToolBar,
  VCL.Dialogs,
  VCL.Menus,
  Classes,
  Windows;

type
  TDailyDiversionMenuItemManager = class(TMenuItemManager)
  protected
    FToolBar : TDailyDiversionMenuItemToolBar;
    procedure DisableAllMenus;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure AddMenuItems; override;
    function  Initialise: boolean; override;
    function  StudyHasChanged: boolean; override;
    function  LanguageHasChanged: boolean; override;
    procedure Show; override;
    procedure TabHasChanged (AGridTabSelected: boolean);

    procedure SetCreateDailyDiversion(AEnabled: boolean);
    procedure SetRenameDailyDiversion(AEnabled: boolean);

    procedure SetDeleteDailyDiversion(AEnabled: boolean);

    procedure SetImportDailyFlowDataFromCSVFile(AEnabled: boolean);
    procedure SetCImportFile14(AEnabled: boolean);
    procedure SetClearFile14(AEnabled: boolean);

    procedure SetClearDailyFlowDataFromCSVFile(AEnabled: boolean);

    procedure SetImportDailyInstreamFlowFile(AEnabled: boolean);
    procedure SetClearDailyInstreamFlowFile(AEnabled: boolean);

    procedure SetGenerateFlowDiversionRelation(AEnabled: boolean);
    procedure SetClearFlowDiversionRelation(AEnabled: boolean);

    procedure SetExportDailyIFR(AEnabled: boolean);
    procedure SetExportMonthlyIFR(AEnabled: boolean);
    procedure SetExportFlowDiversionRelationship(AEnabled: boolean);

    //procedure SetGenerateWRYMData(AEnabled: boolean);
//    procedure SetClearWRYMData(AEnabled: boolean);

    procedure TreeNodeHasChanged (ADataType : string;AElementID : integer);

    property ToolBar: TDailyDiversionMenuItemToolBar read FToolBar;



  end;

implementation

uses
  SysUtils,
  UMainMenuEventType,
  UDailyDiversionGaugeData,
  UDailyIFRData,
  UDailyDiversionDataObject,
  UErrorHandlingOperations;

const
  CDailyDiversionData                : array[0..0] of string = ('Data');
  CCreateDailyDiversion              : array[0..1] of string = ('Data','CreateDailyDiversion');
  CRenameDailyDiversion              : array[0..1] of string = ('Data','RenameDailyDiversion');
  CDeleteDailyDiversion              : array[0..1] of string = ('Data','DeleteDailyDiversion');
  CImportDailyFlowDataFromCSVFile    : array[0..1] of string = ('Data','ImportDailyFlowDataFromCSVFile');
  CClearDailyFlowDataFromCSVFile     : array[0..1] of string = ('Data','ClearDailyFlowDataFromCSVFile');
  CImportDailyInstreamFlowFile       : array[0..1] of string = ('Data','ImportDailyInstreamFlowFile');
  CClearDailyInstreamFlowFile        : array[0..1] of string = ('Data','ClearDailyInstreamFlowFile');
  CImportFile14                      : array[0..1] of string = ('Data','ImportFile14');
  CClearFile14                       : array[0..1] of string = ('Data','ClearFile14');
  CGenerateFlowDiversionRelation     : array[0..1] of string = ('Data','GenerateFlowDiversionRelation');
  CClearFlowDiversionRelation        : array[0..1] of string = ('Data','ClearFlowDiversionRelation');

  CExportDailyIFR                    : array[0..1] of string = ('Data','ExportDailyIFR');
  CExportMonthlyIFR                  : array[0..1] of string = ('Data','ExportMonthlyIFR');
  CExportFlowDiversionRelationship   : array[0..1] of string = ('Data','ExportFlowDiversionRelationship');

  //CGenerateWRYMData                  : array[0..1] of string = ('Data','GenerateWRYMData');
//  CClearWRYMData                     : array[0..1] of string = ('Data','ClearWRYMData');
  CHelpDailyDiversionUserGuide       : array[0..1] of string = ('Help','HelpDailyDiversionUserGuide');

procedure TDailyDiversionMenuItemManager.AddMenuItems;
const OPNAME = 'TDailyDiversionMenuItemManager.AddMenuItems';
begin
  inherited;
  try
    AddMenuItemEntry(CDailyDiversionData,  800);

    AddMenuItemEntry(CCreateDailyDiversion,     820, CmeCreateDailyDiversion);
    AddMenuItemEntry(CRenameDailyDiversion,     825, CmeRenameDailyDiversion);
    AddMenuItemEntry(CDeleteDailyDiversion,     830, CmeDeleteDailyDiversion);

    AddMenuItemEntry(CImportDailyFlowDataFromCSVFile,     840, CmeImportDailyFlowDataFromCSVFile);
    AddMenuItemEntry(CClearDailyFlowDataFromCSVFile,     850, CmeClearDailyFlowDataFromCSVFile);

    AddMenuItemEntry(CImportFile14,     860, cmeImportFile14);
    AddMenuItemEntry(CClearFile14,     860, CmeClearFile14);
    AddMenuItemEntry(CImportDailyInstreamFlowFile,     870, CmeImportDailyInstreamFlowFile);
    AddMenuItemEntry(CClearDailyInstreamFlowFile,     880, CmeClearDailyInstreamFlowFile);

    AddMenuItemEntry(CGenerateFlowDiversionRelation, 890, CmeGenerateFlowDiversionRelation);
    AddMenuItemEntry(CClearFlowDiversionRelation, 900, CmeClearFlowDiversionRelation);
    AddMenuItemEntry(CExportDailyIFR,910,CmeExportDailyIFR);
    AddMenuItemEntry(CExportMonthlyIFR,920,CmeExportMonthlyIFR);
    AddMenuItemEntry(CExportFlowDiversionRelationship,930,CmeExportFlowDiversionRelationship);

    //AddMenuItemEntry(CGenerateWRYMData, 910, CmeGenerateWRYMData);
//    AddMenuItemEntry(CClearWRYMData, 910, CmeClearWRYMData);

    AddMenuItemEntry(CHelpDailyDiversionUserGuide, 230, CmeDailyDiversionUserGuide);

  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TDailyDiversionMenuItemManager.CreateMemberObjects;
const OPNAME = 'TDailyDiversionMenuItemManager.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
      FToolBar := TDailyDiversionMenuItemToolBar.Create(nil, FAppModules);
  except  on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyDiversionMenuItemManager.DestroyMemberObjects;
const OPNAME = 'TDailyDiversionMenuItemManager.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FAppModules.MainForm.RemoveSystemChildToolBar(FToolBar);
    FToolBar.Parent := nil;
    FreeAndNil(FToolBar);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDailyDiversionMenuItemManager.DisableAllMenus;
const OPNAME = 'TDailyDiversionMenuItemManager.DisableAllMenus';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDailyDiversionMenuItemManager.Initialise: boolean;
const OPNAME = 'TDailyDiversionMenuItemManager.Initialise';
begin
  Result := false;
  try
    SetCreateDailyDiversion(True);
    SetDeleteDailyDiversion(False);
    SetRenameDailyDiversion(False);
    SetImportDailyFlowDataFromCSVFile(False);
    SetCImportFile14(False);
    SetCLearFile14(False);
    SetClearDailyFlowDataFromCSVFile(False);
    SetImportDailyInstreamFlowFile(False);
    SetClearDailyInstreamFlowFile(False);
    SetGenerateFlowDiversionRelation(False);
    SetClearFlowDiversionRelation(False);
    //SetGenerateWRYMData(False);

//    SetClearWRYMData(False);

    FAppModules.MainForm.AddSystemChildToolBar(FToolBar);
    Result := FToolBar.Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDailyDiversionMenuItemManager.LanguageHasChanged: boolean;
const OPNAME = 'TDailyDiversionMenuItemManager.LanguageHasChanged';
begin
  Result := false;
  try
    Result := FToolBar.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDailyDiversionMenuItemManager.Show;
const OPNAME = 'TDailyDiversionMenuItemManager.Show';
begin
  try
    inherited Show;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDailyDiversionMenuItemManager.StudyHasChanged: boolean;
const OPNAME = 'TDailyDiversionMenuItemManager.StudyHasChanged';
begin
  Result := false;
  try

    Result := true;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDailyDiversionMenuItemManager.TabHasChanged(AGridTabSelected: boolean);
const OPNAME = 'TDailyDiversionMenuItemManager.TabHasChanged';
var
  LAction: TMenuSetAction;
begin
  try
    FToolBar.TabHasChanged(AGridTabSelected);

    if AGridTabSelected then
      LAction := msShow
     else
      LAction := msHide;
    FAppModules.SetMenuItem(CCreateDailyDiversion,LAction);
    FAppModules.SetMenuItem(CRenameDailyDiversion,LAction);
    FAppModules.SetMenuItem(CDeleteDailyDiversion,LAction);
    FAppModules.SetMenuItem(CImportDailyFlowDataFromCSVFile,LAction);
    FAppModules.SetMenuItem(CImportFile14,LAction);
    FAppModules.SetMenuItem(CClearFile14,LAction);
    FAppModules.SetMenuItem(CClearDailyFlowDataFromCSVFile,LAction);
    FAppModules.SetMenuItem(CImportDailyInstreamFlowFile,LAction);
    FAppModules.SetMenuItem(CClearDailyInstreamFlowFile,LAction);
    FAppModules.SetMenuItem(CGenerateFlowDiversionRelation,LAction);
    FAppModules.SetMenuItem(CClearFlowDiversionRelation,LAction);

    FAppModules.SetMenuItem(CExportDailyIFR,LAction);
    FAppModules.SetMenuItem(CExportMonthlyIFR,LAction);
    FAppModules.SetMenuItem(CExportFlowDiversionRelationship,LAction);

    //FAppModules.SetMenuItem(CGenerateWRYMData,LAction);
//    FAppModules.SetMenuItem(CClearWRYMData,LAction);

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyDiversionMenuItemManager.SetCreateDailyDiversion(AEnabled: boolean);
const OPNAME = 'TDailyDiversionMenuItemManager.SetCreateDailyDiversion';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CCreateDailyDiversion, msEnable)
    else
      FAppModules.SetMenuItem(CCreateDailyDiversion, msDisable, '');
    FToolBar.SetCreateDailyDiversion(AEnabled);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDailyDiversionMenuItemManager.SetRenameDailyDiversion(AEnabled: boolean);
const OPNAME = 'TDailyDiversionMenuItemManager.SetRenameDailyDiversion';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CRenameDailyDiversion, msEnable)
    else
      FAppModules.SetMenuItem(CRenameDailyDiversion, msDisable, '');
    FToolBar.SetRenameDailyDiversion(AEnabled);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDailyDiversionMenuItemManager.SetDeleteDailyDiversion(AEnabled: boolean);
const OPNAME = 'TDailyDiversionMenuItemManager.SetDeleteDailyDiversion';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CDeleteDailyDiversion, msEnable)
    else
      FAppModules.SetMenuItem(CDeleteDailyDiversion, msDisable, '');
    FToolBar.SetDeleteDailyDiversion(AEnabled);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDailyDiversionMenuItemManager.SetImportDailyFlowDataFromCSVFile(AEnabled: boolean);
const OPNAME = 'TDailyDiversionMenuItemManager.SetImportDailyFlowDataFromCSVFile';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CImportDailyFlowDataFromCSVFile, msEnable)
    else
      FAppModules.SetMenuItem(CImportDailyFlowDataFromCSVFile, msDisable, '');
    FToolBar.SetImportDailyFlowDataFromCSVFileBtn(AEnabled);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDailyDiversionMenuItemManager.SetCImportFile14(AEnabled: boolean);
const OPNAME = 'TDailyDiversionMenuItemManager.SetCImportFile14';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CImportFile14, msEnable)
    else
      FAppModules.SetMenuItem(CImportFile14, msDisable, '');
    FToolBar.SetCImportFile14Btn(AEnabled);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDailyDiversionMenuItemManager.SetClearFile14(AEnabled: boolean);
const OPNAME = 'TDailyDiversionMenuItemManager.SetClearFile14';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CClearFile14, msEnable)
    else
      FAppModules.SetMenuItem(CClearFile14, msDisable, '');
    FToolBar.SetClearFile14Btn(AEnabled);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDailyDiversionMenuItemManager.SetClearDailyFlowDataFromCSVFile(AEnabled: boolean);
const OPNAME = 'TDailyDiversionMenuItemManager.SetClearDailyFlowDataFromCSVFile';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CClearDailyFlowDataFromCSVFile, msEnable)
    else
      FAppModules.SetMenuItem(CClearDailyFlowDataFromCSVFile, msDisable, '');
    FToolBar.SetClearDailyFlowDataFromCSVFileBtn(AEnabled);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDailyDiversionMenuItemManager.SetImportDailyInstreamFlowFile(AEnabled: boolean);
const OPNAME = 'TDailyDiversionMenuItemManager.SetImportDailyInstreamFlowFile';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CImportDailyInstreamFlowFile, msEnable)
    else
      FAppModules.SetMenuItem(CImportDailyInstreamFlowFile, msDisable, '');
    FToolBar.SetImportDailyInstreamFlowFileBtn(AEnabled);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDailyDiversionMenuItemManager.SetClearDailyInstreamFlowFile(AEnabled: boolean);
const OPNAME = 'TDailyDiversionMenuItemManager.SetClearDailyInstreamFlowFile';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CClearDailyInstreamFlowFile, msEnable)
    else
      FAppModules.SetMenuItem(CClearDailyInstreamFlowFile, msDisable, '');
    FToolBar.SetClearDailyInstreamFlowFileBtn(AEnabled);
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TDailyDiversionMenuItemManager.SetGenerateFlowDiversionRelation(AEnabled: boolean);
const OPNAME = 'TDailyDiversionMenuItemManager.SetGenerateFlowDiversionRelation';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CGenerateFlowDiversionRelation, msEnable)
    else
      FAppModules.SetMenuItem(CGenerateFlowDiversionRelation, msDisable, '');
    FToolBar.SetGenerateFlowDiversionRelationBtn(AEnabled);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDailyDiversionMenuItemManager.SetClearFlowDiversionRelation(AEnabled: boolean);
const OPNAME = 'TDailyDiversionMenuItemManager.SetClearFlowDiversionRelation';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CClearFlowDiversionRelation, msEnable)
    else
      FAppModules.SetMenuItem(CClearFlowDiversionRelation, msDisable, '');
    FToolBar.SetClearFlowDiversionRelationBtn(AEnabled);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDailyDiversionMenuItemManager.SetExportDailyIFR(AEnabled: boolean);
const OPNAME = 'TDailyDiversionMenuItemManager.SetExportDailyIFR';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CExportDailyIFR, msEnable)
    else
      FAppModules.SetMenuItem(CExportDailyIFR, msDisable, '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDailyDiversionMenuItemManager.SetExportMonthlyIFR(AEnabled: boolean);
const OPNAME = 'TDailyDiversionMenuItemManager.SetExportMonthlyIFR';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CExportMonthlyIFR, msEnable)
    else
      FAppModules.SetMenuItem(CExportMonthlyIFR, msDisable, '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDailyDiversionMenuItemManager.SetExportFlowDiversionRelationship(AEnabled: boolean);
const OPNAME = 'TDailyDiversionMenuItemManager.SetExportFlowDiversionRelationship';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CExportFlowDiversionRelationship, msEnable)
    else
      FAppModules.SetMenuItem(CExportFlowDiversionRelationship, msDisable, '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{procedure TDailyDiversionMenuItemManager.SetGenerateWRYMData(AEnabled: boolean);
const OPNAME = 'TDailyDiversionMenuItemManager.SetGenerateWRYMData';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CGenerateWRYMData, msEnable)
    else
      FAppModules.SetMenuItem(CGenerateWRYMData, msDisable, '');
    FToolBar.SetGenerateWRYMDataBtn(AEnabled);
  except on E: Exception do HandleError(E, OPNAME) end;
end;
 }
procedure TDailyDiversionMenuItemManager.TreeNodeHasChanged(ADataType: string; AElementID: integer);
const OPNAME = 'TDailyDiversionMenuItemManager.TreeNodeHasChanged';
var
  LDiversionGauge : TDiversionGauge;
  LDailyIFRData : TDailyIFRData;
  LDailyIFRDataList : TDailyIFRDataList;
begin
  try
    Initialise;
    if (AElementID = 10000) then
    begin
      SetCreateDailyDiversion(True);
      SetDeleteDailyDiversion(False);
      SetRenameDailyDiversion(False);
      SetImportDailyFlowDataFromCSVFile(False);
      SetImportDailyInstreamFlowFile(False);
      SetGenerateFlowDiversionRelation(False);
      SetClearDailyFlowDataFromCSVFile(False);
      SetClearDailyInstreamFlowFile(False);
      SetGenerateFlowDiversionRelation(False);
      SetClearFlowDiversionRelation(False);
      //SetGenerateWRYMData(False);
      SetCImportFile14(False);
      SetCLearFile14(False);
      SetExportMonthlyIFR(False);
      SetExportDailyIFR(False);
      SetExportFlowDiversionRelationship(False);
    end
    else
    begin
      SetCreateDailyDiversion(False);
      SetDeleteDailyDiversion(True);
      SetRenameDailyDiversion(True);
      SetGenerateFlowDiversionRelation(False);
      SetExportFlowDiversionRelationship(False);
      SetExportMonthlyIFR(False);
      SetExportDailyIFR(False);
      LDiversionGauge := TDailyDiversionDataObject(FAppModules.Model.ModelData).
                         DailyDiversionGaugeDataList.DiversionGaugeByStationID[AElementID];
      if LDiversionGauge <> nil then
      begin
        LDailyIFRDataList := TDailyDiversionDataObject(FAppModules.Model.ModelData).DailyIFRDataList;
        LDailyIFRData := LDailyIFRDataList.GetDailyIFRDataByStationNo(LDiversionGauge.StationNo);
        if LDailyIFRData <> nil then
        begin
          if (LDailyIFRData.ChannelList.ChannelCount > 0) and
              (LDailyIFRData.IFRFeatureList.MonthlyIFRFeatureCount > 0) then
          begin
            SetCImportFile14(False);
            SetCLearFile14(True);
          end
          else
          if (LDiversionGauge.ImportIFR) and (LDailyIFRData.IFRFeatureList.MonthlyIFRFeatureCount = 0) then
          begin
            SetCImportFile14(True);
            SetCLearFile14(False);
          end;
        end;
        if LDiversionGauge.DailyDataCount > 0 then
        begin
          SetImportDailyFlowDataFromCSVFile(False);
          SetClearDailyFlowDataFromCSVFile(True);
        end
        else
        if LDiversionGauge.DailyDataCount <= 0 then
        begin
          SetImportDailyFlowDataFromCSVFile(True);
          SetClearDailyFlowDataFromCSVFile(False);
        end;
        if LDiversionGauge.DailyInstreamDataCount > 0 then
        begin
          SetImportDailyInstreamFlowFile(False);
          SetClearDailyInstreamFlowFile(True);
          SetExportMonthlyIFR(True);
          SetExportDailyIFR(True);
        end
        else
        if not (LDiversionGauge.ImportIFR) and (LDiversionGauge.DailyInstreamDataCount <= 0) then
        begin
          SetImportDailyInstreamFlowFile(True);
          SetClearDailyInstreamFlowFile(False);
        end;
        if (LDiversionGauge.MonthlyDailyFlowCount > 0) and (LDiversionGauge.MonthlyInstreamFlowCount > 0)
          and (LDiversionGauge.RankedFlowDiversionRelationshipCount <= 0 )then
        begin
          SetGenerateFlowDiversionRelation(True);
          SetClearFlowDiversionRelation(False);
          SetExportFlowDiversionRelationship(False);
        end
        else
        if (LDiversionGauge.RankedFlowDiversionRelationshipCount > 0 )then
        begin
          SetGenerateFlowDiversionRelation(False);
          SetClearFlowDiversionRelation(True);
          SetExportFlowDiversionRelationship(True);
        end;
        {if (LDiversionGauge.WRYMDataCount <= 0) and (LDiversionGauge.RankedFlowDiversionRelationshipCount > 0) then
          SetGenerateWRYMData(True)
        else
        if (LDiversionGauge.WRYMDataCount > 0) then
          SetGenerateWRYMData(True)
          }
      end
      else
        SetImportDailyFlowDataFromCSVFile(True);

    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.

