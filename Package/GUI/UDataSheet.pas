//
//
//  UNIT      : Contains TDataSheet Class
//  AUTHOR    : Dziedzi Ramulondi (Aravia)
//  DATE      : 2003/12/10
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UDataSheet;

interface

uses
  VCLTee.Chart,
  VCL.ComCtrls,
  UViewDataItem,
  UViewDataList,
  UHelpContexts,
  UAbstractObject,
  UMenuItemManager,
  UAbstractComponent;

type
  TDataSheet = class(TAbstractTabSheet)
  protected
    FPageControl: TPageControl;
    FCurrentSheet: TAbstractTabSheet;

    // Overriden from TAbstractTabSheet
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure AssignHelpContext; override;
    function GetToolBar: TAbstractToolBar; override;
    procedure PageControlPageChange(Sender: TObject);
    procedure PageControlOnShow(Sender: TObject);
    procedure OnTabChangeRequest(Sender: TObject; var AllowChange: Boolean);
    function GetModelTabSheetName:TModelTabSheetName;override;
  public
    function Initialise: Boolean; override;
    function CanCopyToCLipboard: boolean; override;
    function CanExport: boolean; override;
    function CanPrint: boolean; override;
    procedure DoCopyToCLipboard; override;
    procedure DoExport(AFileName: string = ''); override;
    procedure DoPrint; override;

    procedure SetMenuVisible(AVisible: boolean); override;
    property CurrentSheet:TAbstractTabSheet read FCurrentSheet;

    property PageControl: TPageControl read FPageControl;
    property ToolBar : TAbstractToolBar Read GetToolBar;
  end;

implementation

uses
  SysUtils,
  VCL.Controls,
  VCL.Graphics,
  Windows,
  VCL.Clipbrd,
  VCL.Printers,
  UStudyArea,
  UTreeViewTabSheet,
  UMainMenuEventType,
  UErrorHandlingOperations;

procedure TDataSheet.CreateMemberObjects;
const OPNAME = 'TDataSheet.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FTabCaptionKey := 'Data';
    FPageControl := TPageControl.Create(Self);
    FPageControl.Parent := Self;
    FPageControl.TabPosition := tpBottom;
    FPageControl.OnChange := PageControlPageChange;
    FPageControl.OnChanging := OnTabChangeRequest;
    Self.OnShow := PageControlOnShow;
    FCurrentSheet := nil;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TDataSheet.AssignHelpContext;
const OPNAME = 'TDataSheet.AssignHelpContext';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDataSheet.DestroyMemberObjects;
const OPNAME = 'TDataSheet.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TDataSheet.Initialise: Boolean;
const OPNAME = 'TDataSheet.Initialise';
begin
  Result := inherited Initialise;
  try
    if (FPageControl.PageCount > 0) then
      FCurrentSheet:= TAbstractTabSheet(FPageControl.ActivePage);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataSheet.GetToolBar: TAbstractToolBar;
const OPNAME = 'TDataSheet.GetToolBar';
begin
  Result := nil;
  try
    if Assigned(FCurrentSheet) then
      Result := FCurrentSheet.ToolBar;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataSheet.CanCopyToClipboard: boolean;
const OPNAME = 'TDataSheet.CanCopyToClipboard';
begin
  Result := False;
  try
    Result := Assigned(FCurrentSheet) and FCurrentSheet.CanCopyToClipboard;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataSheet.CanExport: boolean;
const OPNAME = 'TDataSheet.CanExport';
begin
  Result := False;
  try
    Result := Assigned(FCurrentSheet) and FCurrentSheet.CanExport;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataSheet.CanPrint: boolean;
const OPNAME = 'TDataSheet.CanPrint';
begin
  Result := False;
  try
    Result := Assigned(FCurrentSheet) and FCurrentSheet.CanPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDataSheet.DoCopyToClipboard;
const OPNAME = 'TDataSheet.DoCopyToClipboard';
begin
  try
    if Assigned(FCurrentSheet) then
       FCurrentSheet.DoCopyToClipboard;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDataSheet.DoExport(AFileName: string = '');
const OPNAME = 'TDataSheet.DoExport';
begin
  try
    if Assigned(FCurrentSheet) then
       FCurrentSheet.DoExport(AFileName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDataSheet.DoPrint;
const OPNAME = 'TDataSheet.DoPrint';
begin
  try
    if Assigned(FCurrentSheet) then
       FCurrentSheet.DoPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDataSheet.PageControlPageChange(Sender: TObject);
const OPNAME = 'TDataSheet.PageControlPageChange';
begin
  try
    FCurrentSheet := TAbstractTabSheet(FPageControl.Pages[FPageControl.ActivePageIndex]);
    FAppModules.Model.ProcessEvent(CmeResultPageControlChanged,nil)
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDataSheet.OnTabChangeRequest(Sender: TObject; var AllowChange: Boolean);
const OPNAME = 'TDataSheet.OnTabChangeRequest';
begin
  try
    if Assigned(CurrentSheet) then
    FAppModules.Model.ProcessEvent(CmeResultPageControlChanging,nil)
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDataSheet.PageControlOnShow(Sender: TObject);
const OPNAME = 'TDataSheet.PageControlOnShow';
begin
  try
    FPageControl.Align := alClient;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDataSheet.SetMenuVisible(AVisible: boolean);
const OPNAME = 'TDataSheet.SetMenuVisible';
begin
  try
    if Assigned(CurrentSheet) then
      CurrentSheet.SetMenuVisible(AVisible);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataSheet.GetModelTabSheetName: TModelTabSheetName;
const OPNAME = 'TDataSheet.GetModelTabSheetName';
begin
  Result := mtsnNone;
  try
    if Assigned(CurrentSheet) then
      Result := CurrentSheet.ModelTabSheetName;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

