

unit UModelHydrologyTabSheet;

interface

uses
//  DB,
  VCL.Menus,
  Classes,
  VCL.stdctrls,
  VCL.ComCtrls,
  contnrs,
  VCL.extctrls,
  VCL.CheckLst,
  VCL.Dialogs,
  UHelpContexts,
  UAbstractObject,
  UAbstractComponent,
  UGenericModelLinkClasses,
  UModelHydrologyMenuItem,
  UTreeViewTabSheet,
  UMainMenuEventType;
type

  TModelHydrologyTabSheet = class ( TAbstractTabSheet ) // TTreeViewTabSheet
    protected
    FPageControl: TPageControl;
    FCurrentSheet: TAbstractTabSheet;

    procedure CreateMemberObjects; override;
    function GetToolBar: TAbstractToolBar; override;
    procedure DestroyMemberObjects; override;
    procedure PageControlPageChange(Sender: TObject);
    procedure PageControlOnShow(Sender: TObject);
    procedure OnTabChangeRequest(Sender: TObject; var AllowChange: Boolean);
  public
    function Initialise: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function CanCopyToCLipboard: boolean; override;
    function CanExport: boolean; override;
    function CanPrint: boolean; override;
    procedure DoCopyToCLipboard; override;
    procedure DoExport(AFileName: string = ''); override;
    procedure DoPrint; override;
    function DoCustomTabSheetEvent ( ACustomModelEvent: TModelMenuData ): boolean; override;

    property CurrentSheet:TAbstractTabSheet read FCurrentSheet;
    property PageControl: TPageControl read FPageControl;
    property ToolBar : TAbstractToolBar Read GetToolBar;
    function CanApplicationClose: Boolean; override;
    
end;

implementation

uses
  SysUtils,
  VCL.Controls,
  UErrorHandlingOperations;


{ TModelHydrologyTabSheet }

function TModelHydrologyTabSheet.CanCopyToClipboard: boolean;
const OPNAME = 'TModelHydrologyTabSheet.CanCopyToClipboard';
begin
  Result := False;
  try
    Result := Assigned(FCurrentSheet) and FCurrentSheet.CanCopyToClipboard;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TModelHydrologyTabSheet.CanExport: boolean;
const OPNAME = 'TModelHydrologyTabSheet.CanExport';
begin
  Result := False;
  try
    Result := Assigned(FCurrentSheet) and FCurrentSheet.CanExport;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TModelHydrologyTabSheet.CanPrint: boolean;
const OPNAME = 'TModelHydrologyTabSheet.CanPrint';
begin
  Result := False;
  try
    Result := Assigned(FCurrentSheet) and FCurrentSheet.CanPrint;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TModelHydrologyTabSheet.CreateMemberObjects;
const OPNAME = 'TModelHydrologyTabSheet.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FTabCaptionKey := 'Hydrology';
//    FMenuItemManager := TModelHydrologyMenuItem.Create ( FAppModules );
//    Caption := FAppModules.Language.GetString ( 'TabCaption.' + FTabCaptionKey );
    FPageControl := TPageControl.Create( self );
    FPageControl.Parent := Self;
    FPageControl.TabPosition := tpBottom;
    FPageControl.OnChange := PageControlPageChange;
    FPageControl.OnChanging := OnTabChangeRequest;
    Self.OnShow := PageControlOnShow;
    FCurrentSheet := nil;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TModelHydrologyTabSheet.DestroyMemberObjects;
const OPNAME = 'TModelHydrologyTabSheet.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
//  FreeAndNil ( FMenuItemManager );
//    FreeAndNil ( FPageControl );
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TModelHydrologyTabSheet.DoCopyToClipboard;
const OPNAME = 'TModelHydrologyTabSheet.DoCopyToClipboard';
begin
  try
    if Assigned(FCurrentSheet) then
       FCurrentSheet.DoCopyToClipboard;
  except on E: Exception do HandleError(E, OPNAME) end;       
end;

function TModelHydrologyTabSheet.DoCustomTabSheetEvent(
  ACustomModelEvent: TModelMenuData): boolean;
const OPNAME = 'TModelHydrologyTabSheet.DoCustomTabSheetEvent';
begin
  result := false;
  try
    Result := true;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TModelHydrologyTabSheet.DoExport(AFileName: string = '');
const OPNAME = 'TModelHydrologyTabSheet.DoExport';
begin
  inherited;
  { do nothing }
end;

procedure TModelHydrologyTabSheet.DoPrint;
const OPNAME = 'TModelHydrologyTabSheet.DoPrint';
begin
  inherited;
  { do nothing }
end;

function TModelHydrologyTabSheet.GetToolBar: TAbstractToolBar;
const OPNAME = 'TModelHydrologyTabSheet.GetToolBar';
begin
  Result := nil;
  try
    if Assigned(FCurrentSheet) then
      Result := FCurrentSheet.ToolBar;
//    Result := TModelHydrologyMenuItem ( FMenuItemManager ).ToolBar;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TModelHydrologyTabSheet.Initialise: boolean;
const OPNAME = 'TModelHydrologyTabSheet.Initialise';
begin
  Result := inherited Initialise;
  try
    if (FPageControl.PageCount > 0) then
      FCurrentSheet:= TAbstractTabSheet(FPageControl.ActivePage);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TModelHydrologyTabSheet.LanguageHasChanged: boolean;
const OPNAME = 'TModelHydrologyTabSheet.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;;
  try
    result := true;

  except on E: Exception do HandleError(E, OPNAME) end;

end;

procedure TModelHydrologyTabSheet.PageControlPageChange(Sender: TObject);
const OPNAME = 'TModelHydrologyTabSheet.PageControlPageChange';
begin
  try
    FCurrentSheet := TAbstractTabSheet ( FPageControl.Pages [ FPageControl.ActivePageIndex ] );
    FAppModules.Model.ProcessEvent ( CmeResultPageControlChanging ,nil )
  except on E: Exception do HandleError(E, OPNAME) end;

end;

procedure TModelHydrologyTabSheet.PageControlOnShow(Sender: TObject);
const OPNAME = 'TModelHydrologyTabSheet.PageControlOnShow';
begin
  try
    FPageControl.Align := alClient;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelHydrologyTabSheet.StudyHasChanged: boolean;
const OPNAME = 'TModelHydrologyTabSheet.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    Result := true;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
procedure TModelHydrologyTabSheet.OnTabChangeRequest ( Sender: TObject;
                                                       var AllowChange: Boolean );
const OPNAME = 'TModelHydrologyTabSheet.OnTabChangeRequest';
begin
  try
    if ( Assigned ( CurrentSheet ) ) then
      FAppModules.Model.ProcessEvent ( CmeResultPageControlChanging  ,nil )
    { end if..the }
  except on E: Exception do HandleError(E, OPNAME ) end;
end;

function TModelHydrologyTabSheet.CanApplicationClose: Boolean;
const OPNAME = 'TModelHydrologyTabSheet.CanApplicationClose';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.

