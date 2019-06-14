unit URainfallCatchmentSummaryTabSheetManager;

interface
uses
  UTabsheetManager;

type
  TRainfallCatchmentSummaryTabSheetManager = class(TTabSheetManager)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function Initialise: Boolean; override;
    function LanguageHasChanged: Boolean; override;
    function ProcessParameterChangeEvent : boolean; override;
    function ProcessMetaDataEvent : boolean; override;
  end;

implementation
uses
  SysUtils,
  URainfallCatchmentSummaryTabSheet,
  UErrorHandlingOperations;
{ TRainfallCatchmentSummaryTabSheetManager }

function TRainfallCatchmentSummaryTabSheetManager.Initialise: Boolean;
const OPNAME = 'TRainfallCatchmentSummaryTabSheetManager.Initialise';
begin
  Result := True;
  try
    if Assigned(FTabSheet) then
      FTabSheet.Initialise;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallCatchmentSummaryTabSheetManager.CreateMemberObjects;
const OPNAME = 'TRainfallCatchmentSummaryTabSheetManager.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FTabSheet := TRainfallCatchmentSummaryTabSheet.Create(nil, FAppModules);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallCatchmentSummaryTabSheetManager.DestroyMemberObjects;
const OPNAME = 'TRainfallCatchmentSummaryTabSheetManager.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallCatchmentSummaryTabSheetManager.LanguageHasChanged: Boolean;
const OPNAME = 'TRainfallCatchmentSummaryTabSheetManager.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    if Assigned(FTabSheet) then
      Result := Result and FTabSheet.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallCatchmentSummaryTabSheetManager.ProcessMetaDataEvent: boolean;
const OPNAME = 'TRainfallCatchmentSummaryTabSheetManager.ProcessMetaDataEvent';
begin
  Result := False;
  try
    Result := TRainfallCatchmentSummaryTabSheet(FTabSheet).ProcessParameterChangeEvent;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallCatchmentSummaryTabSheetManager.ProcessParameterChangeEvent: boolean;
const OPNAME = 'TRainfallCatchmentSummaryTabSheetManager.ProcessParameterChangeEvent';
begin
  Result := False;
  try
    Result := TRainfallCatchmentSummaryTabSheet(FTabSheet).ProcessMetaDataEvent;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
