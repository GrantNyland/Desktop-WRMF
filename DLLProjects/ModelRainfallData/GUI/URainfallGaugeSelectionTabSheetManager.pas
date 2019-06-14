unit URainfallGaugeSelectionTabSheetManager;

interface

uses
  UTabsheetManager,
  UAbstractObject;

type
  TRainfallGaugeSelectionTabSheetManager = class(TTabSheetManager)
  protected
    procedure CreateMemberObjects; override;
  public
    function Initialise: Boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext;
      AFieldName: String; AOldValue: String; ANewValue: String): Boolean; override;
    function StudyHasChanged: Boolean; override;
    function SaveState: Boolean; override;
    function LanguageHasChanged: Boolean; override;
  end;

implementation

uses
  VCL.Dialogs,
  SysUtils,
  URainfallGaugeSelectionTabSheet,
  UErrorHandlingOperations;

{ TRainfallGaugeSelectionTabSheetManager }

procedure TRainfallGaugeSelectionTabSheetManager.CreateMemberObjects;
const OPNAME = 'TRainfallGaugeSelectionTabSheetManager.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FTabSheet := TRainfallGaugeSelectionTabSheet.Create(nil, FAppModules);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallGaugeSelectionTabSheetManager.Initialise: Boolean;
const OPNAME = 'TRainfallGaugeSelectionTabSheetManager.Initialise';
begin
  Result := True;

  try
    if Assigned(FTabSheet) then
      FTabSheet.Initialise;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallGaugeSelectionTabSheetManager.LanguageHasChanged : Boolean;
const OPNAME = 'TRainfallGaugeSelectionTabSheetManager.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    if Assigned(FTabSheet) then
      Result := Result and FTabSheet.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallGaugeSelectionTabSheetManager.SaveState: Boolean;
const OPNAME = 'TRainfallGaugeSelectionTabSheetManager.SaveState';
begin
  Result := inherited SaveState;
  try
    if Assigned ( FTabSheet ) then
      Result := Result and FTabSheet.SaveState;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallGaugeSelectionTabSheetManager.StudyDataHasChanged(
  AContext: TChangeContext; AFieldName, AOldValue,
  ANewValue: String): Boolean;
const OPNAME = 'TRainfallGaugeSelectionTabSheetManager.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
//    if Assigned ( FTabSheet ) then
//      Result := Result and FTabSheet.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallGaugeSelectionTabSheetManager.StudyHasChanged: Boolean;
const OPNAME = 'TRainfallGaugeSelectionTabSheetManager.StudyHasChanged';
begin
  Result := FALSE;
  try
    if Assigned(FTabSheet) then
      Result := FTabSheet.StudyHasChanged;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.


