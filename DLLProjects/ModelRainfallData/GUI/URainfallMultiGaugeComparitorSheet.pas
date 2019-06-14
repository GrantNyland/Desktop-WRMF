unit URainfallMultiGaugeComparitorSheet;

interface

uses
  VCL.Controls,
  UAbstractObject,
  UAbstractComponent,
  UMenuItemManager,
//  URainfallGaugeStatsMenuItemManager,
  URainfallMultiGaugeComparitorValidator,
  UGenericModelLinkClasses;

type

  TRainfallMultiGaugeComparitorSheet = class (TAbstractTabSheet)
  protected
    FMenuItemManager             : TMenuItemManager;
    FRainfallMultiGaugeComparitorValidator : TRainfallMultiGaugeComparitorValidator;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
//    function GetMenuItemManager : TRainfallGaugeStatsMenuItemManager;
    function GetToolBar : TAbstractToolBar; override;
  public
    function Initialise: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged (AContext   : TChangeContext;
                                  AFieldName : string;
                                  AOldValue  : string;
                                  ANewValue  : string): boolean; override;
    procedure TabHasChanged; override;
    procedure SetMenuVisible (AVisible : Boolean);override;

    function CanCopyToCLipboard: boolean; override;
    function CanExport: boolean; override;
    function CanPrint: boolean; override;
    procedure DoCopyToCLipboard; override;
    procedure DoExport(AFileName: string = ''); override;
    procedure DoPrint; override;
    function DoCustomTabSheetEvent (ACustomModelEvent : TModelMenuData): Boolean; override;
//    property MenuItemManager: TRainfallGaugeStatsMenuItemManager read GetMenuItemManager;
  end;

implementation

uses
  SysUtils,
  UErrorHandlingOperations;

{ TRainfallMultiGaugeComparitorSheet }

procedure TRainfallMultiGaugeComparitorSheet.CreateMemberObjects;
const OPNAME = 'TRainfallMultiGaugeComparitorSheet.CreateMemberObjects';
begin
  inherited;
  try
    FTabCaptionKey := 'GaugeComparitor';

    FRainfallMultiGaugeComparitorValidator := TRainfallMultiGaugeComparitorValidator.Create(Self, FAppModules);
    FRainfallMultiGaugeComparitorValidator.Panel.Parent := Self;
    FRainfallMultiGaugeComparitorValidator.Panel.Align  := alClient;

  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TRainfallMultiGaugeComparitorSheet.DestroyMemberObjects;
const OPNAME = 'TRainfallMultiGaugeComparitorSheet.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FreeAndNil(FMenuItemManager);
    FreeAndNil(FRainfallMultiGaugeComparitorValidator);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallMultiGaugeComparitorSheet.Initialise: boolean;
const OPNAME = 'TRainfallMultiGaugeComparitorSheet.Initialise';
begin
  Result := inherited Initialise;
  try
    FRainfallMultiGaugeComparitorValidator.Initialise;
    SetMenuVisible(False);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallMultiGaugeComparitorSheet.LanguageHasChanged: boolean;
const OPNAME = 'TRainfallMultiGaugeComparitorSheet.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallMultiGaugeComparitorSheet.StudyHasChanged: boolean;
const OPNAME = 'TRainfallMultiGaugeComparitorSheet.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    Result := FRainfallMultiGaugeComparitorValidator.StudyHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallMultiGaugeComparitorSheet.StudyDataHasChanged (AContext   : TChangeContext;
                                                       AFieldName : string;
                                                       AOldValue  : string;
                                                       ANewValue  : string): boolean;
const OPNAME = 'TRainfallMultiGaugeComparitorSheet.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext, AFieldName, AOldValue, ANewValue);
  try
    Result := FRainfallMultiGaugeComparitorValidator.StudyDataHasChanged(AContext, AFieldName, AOldValue, ANewValue);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallMultiGaugeComparitorSheet.TabHasChanged;
const OPNAME = 'TRainfallMultiGaugeComparitorSheet.TabHasChanged';
begin
  inherited;
  try
    FRainfallMultiGaugeComparitorValidator.PopulateDataViewer;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRainfallMultiGaugeComparitorSheet.CanPrint: Boolean;
const OPNAME = 'TRainfallMultiGaugeComparitorSheet.CanPrint';
begin
  Result := TRUE;
  try
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRainfallMultiGaugeComparitorSheet.CanCopyToClipboard: Boolean;
const OPNAME = 'TRainfallMultiGaugeComparitorSheet.CanCopyToClipboard';
begin
  Result := TRUE;
  try
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRainfallMultiGaugeComparitorSheet.CanExport: Boolean;
const OPNAME = 'TRainfallMultiGaugeComparitorSheet.CanExport';
begin
  Result := TRUE;
  try
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRainfallMultiGaugeComparitorSheet.DoPrint;
const OPNAME = 'TRainfallMultiGaugeComparitorSheet.DoPrint';
begin
  try
    FRainfallMultiGaugeComparitorValidator.DoPrint;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRainfallMultiGaugeComparitorSheet.DoCopyToClipboard;
const OPNAME = 'TRainfallMultiGaugeComparitorSheet.DoCopyToClipboard';
begin
  try
    FRainfallMultiGaugeComparitorValidator.DoCopyToClipboard;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRainfallMultiGaugeComparitorSheet.DoExport(AFileName: string = '');
const OPNAME = 'TRainfallMultiGaugeComparitorSheet.DoExport';
begin
  try
    FRainfallMultiGaugeComparitorValidator.DoExport(AFileName);
  except on E:Exception do HandleError(E,OPNAME) end;
end;

{function TRainfallMultiGaugeComparitorSheet.GetMenuItemManager: TRainfallGaugeStatsMenuItemManager;
const OPNAME = 'TRainfallMultiGaugeComparitorSheet.GetMenuItemManager';
begin
  Result := nil;
  try
    Result := TRainfallGaugeStatsMenuItemManager(FMenuItemManager);
  except on E: Exception do HandleError(E, OPNAME) end;
end;
 }
function TRainfallMultiGaugeComparitorSheet.GetToolBar: TAbstractToolBar;
const OPNAME = 'TRainfallMultiGaugeComparitorSheet.GetToolBar';
begin
  Result := nil;
  try
//    Result := MenuItemManager.ToolBar;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallMultiGaugeComparitorSheet.SetMenuVisible (AVisible : Boolean);
const OPNAME = 'TRainfallMultiGaugeComparitorSheet.SetMenuVisible';
begin
  inherited SetMenuVisible(AVisible);
  try
{    if Assigned(MenuItemManager) then
    begin
      if AVisible then
        MenuItemManager.Show
      else
        MenuItemManager.Hide;
    end;}
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallMultiGaugeComparitorSheet.DoCustomTabSheetEvent(ACustomModelEvent: TModelMenuData): boolean;
const OPNAME = 'TRainfallMultiGaugeComparitorSheet.DoCustomTabSheetEvent';
begin
  Result := False;
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
