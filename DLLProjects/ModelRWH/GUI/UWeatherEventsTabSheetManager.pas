//
//
//  UNIT      : Contains TWeatherEventsTabSheetManager Class
//  AUTHOR    : Dziedzi Ramulondi (arivia.kom)
//  DATE      : 09/01/2007
//  COPYRIGHT : Copyright © 2007 DWAF
//
//
unit UWeatherEventsTabSheetManager;

interface

uses
  Classes,
  UTabSheetManager;

type
  TWeatherEventsTabSheetManager = class(TTabSheetManager)
  protected
    procedure CreateMemberObjects; override;
  public
    function Initialise: Boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
  end;

implementation

uses
  SysUtils,
  UWeatherEventsTabSheet,
  UErrorHandlingOperations;

procedure TWeatherEventsTabSheetManager.CreateMemberObjects;
const OPNAME = 'TViewContoursTabSheetManager.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FTabSheet := TWeatherEventsTabSheet.Create(nil, AppModules);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWeatherEventsTabSheetManager.Initialise: Boolean;
const OPNAME = 'TWeatherEventsTabSheetManager.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := FTabSheet.Initialise;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TWeatherEventsTabSheetManager.LanguageHasChanged: boolean;
const OPNAME = 'TWeatherEventsTabSheetManager.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    Result := FTabSheet.LanguageHasChanged;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TWeatherEventsTabSheetManager.StudyHasChanged: boolean;
const OPNAME = 'TWeatherEventsTabSheetManager.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    Result := FTabSheet.StudyHasChanged;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
