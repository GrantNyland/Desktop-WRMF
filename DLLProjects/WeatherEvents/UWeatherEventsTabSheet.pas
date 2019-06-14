{******************************************************************************}
{*  UNIT      : Contains the class TWeatherEventsTabSheet.                    *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2005/02/11                                                    *}
{*  COPYRIGHT : Copyright © 2005 DWAF                                         *}
{******************************************************************************}

unit UWeatherEventsTabSheet;

interface

uses
  VCL.Menus,
  Classes,
  VCL.StdCtrls,
  VCL.ComCtrls,
  Contnrs,
  VCL.Controls,
  UHelpContexts,
  UAbstractObject,
  UAbstractComponent;
type

  TWeatherEventsTabSheet = class(TAbstractTabSheet)
  private
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    function Initialise: boolean; override;
    procedure TabHasChanged; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function GetToolBar: TAbstractToolBar; override;
  end;

implementation

{$WARN UNIT_PLATFORM OFF}

uses
  UConstants,
  Windows,
  VCL.Forms,
  VCL.Graphics,
  VCL.ImgList,
  SysUtils,
  UMainMenuEventType,
  UErrorHandlingOperations;

{ TWeatherEventsTabSheet }

procedure TWeatherEventsTabSheet.CreateMemberObjects;
const OPNAME = 'TWeatherEventsTabSheet.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FTabCaptionKey := 'WeatherEvents';
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TWeatherEventsTabSheet.DestroyMemberObjects;
const OPNAME = 'TWeatherEventsTabSheet.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWeatherEventsTabSheet.Initialise: boolean;
const OPNAME = 'TWeatherEventsTabSheet.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWeatherEventsTabSheet.TabHasChanged;
const OPNAME = 'TWeatherEventsTabSheet.TabHasChanged';
begin
  inherited;
  try
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TWeatherEventsTabSheet.AssignHelpContext;
const OPNAME = 'TWeatherEventsTabSheet.AssignHelpContext';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWeatherEventsTabSheet.StudyHasChanged: boolean;
const OPNAME = 'TWeatherEventsTabSheet.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWeatherEventsTabSheet.LanguageHasChanged: boolean;
const OPNAME = 'TWeatherEventsTabSheet.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWeatherEventsTabSheet.GetToolbar : TAbstractToolBar;
const OPNAME = 'TWeatherEventsTabSheet.GetToolbar';
begin
  Result := nil;
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.

