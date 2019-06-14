//
//
//  UNIT      : Contains TWeatherEventsTabSheet Class
//  AUTHOR    : Dziedzi Ramulondi (arivia.kom)
//  DATE      : 25/02/2014
//  COPYRIGHT : Copyright © 2014 DWAF
//
//
unit UWeatherEventsTabSheet;

interface
uses
  Classes,
  VCL.Controls,
  UAbstractComponent,
  UDataViewerSheet,
  UWeatherEventsValidator;
type

  TWeatherEventsTabSheet = class(TAbstractTabSheet)
  protected
    FWeatherEventsValidator : TWeatherEventsValidator;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetToolBar: TAbstractToolBar; override;
  public
    function Initialise: boolean; override;
    function StudyHasChanged: boolean; override;
    function LanguageHasChanged: boolean; override;
  end;


implementation
uses
  SysUtils,
  UUtilities,
  UErrorHandlingOperations;

{ TRWHTabSheet }

procedure TWeatherEventsTabSheet.CreateMemberObjects;
const OPNAME = 'TWeatherEventsTabSheet.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;


    FWeatherEventsValidator := TWeatherEventsValidator.Create(Self,FAppModules);
    FWeatherEventsValidator.Panel.Parent := Self;
    FWeatherEventsValidator.Panel.Align  := alClient;

    FTabCaptionKey := 'WeatherEvents';
    //FViewTypeConstant := 'RWHViewOutput';
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TWeatherEventsTabSheet.DestroyMemberObjects;
const OPNAME = 'TWeatherEventsTabSheet.CreateMemberObjects';
begin
  try
    inherited DestroyMemberObjects;

    //FViewTypeConstant := 'RWHViewOutput';
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TWeatherEventsTabSheet.GetToolBar: TAbstractToolBar;
const OPNAME = 'TWeatherEventsTabSheet.GetToolBar';
begin
  Result := nil;
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TWeatherEventsTabSheet.Initialise: boolean;
const OPNAME = 'TWeatherEventsTabSheet.Initialise';
begin
  Result := inherited Initialise;
  try
    FWeatherEventsValidator.Initialise;
    FWeatherEventsValidator.PopulateDataViewer;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TWeatherEventsTabSheet.LanguageHasChanged: boolean;
const OPNAME = 'TWeatherEventsTabSheet.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    Result := FWeatherEventsValidator.LanguageHasChanged;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TWeatherEventsTabSheet.StudyHasChanged: boolean;
const OPNAME = 'TWeatherEventsTabSheet.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    Result := FWeatherEventsValidator.StudyHasChanged;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
