{******************************************************************************}
{*  UNIT      : Contains the class TRainfallGraphTabSheetManager.             *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2005/04/28                                                    *}
{*  COPYRIGHT : Copyright © 2005 DWAF                                         *}
{******************************************************************************}

unit URainfallGraphTabSheetManager;

interface

uses
  UTabsheetManager;

type
  TRainfallGraphTabSheetManager = class(TTabSheetManager)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function ProcessParameterChangeEvent : boolean; override;
    function ProcessMetaDataEvent : boolean; override;
  end;

implementation

uses
  SysUtils,
  URainfallGraphTabsheet,
  UErrorHandlingOperations;

{ TRainfallGraphTabSheetManager }

procedure TRainfallGraphTabSheetManager.CreateMemberObjects;
const OPNAME = 'TRainfallGraphTabSheetManager.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FTabSheet := TRainfallGraphTabSheet.Create(nil, FAppModules);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphTabSheetManager.DestroyMemberObjects;
const OPNAME = 'TRainfallGraphTabSheetManager.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallGraphTabSheetManager.ProcessParameterChangeEvent: Boolean;
const OPNAME = 'TRainfallGraphTabSheetManager.ProcessParameterChangeEvent';
begin
  Result := FALSE;
  try
    Result := TRainfallGraphTabSheet(FTabSheet).ProcessParameterChangeEvent;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallGraphTabSheetManager.ProcessMetaDataEvent: Boolean;
const OPNAME = 'TRainfallGraphTabSheetManager.ProcessMetaDataEvent';
begin
  Result := FALSE;
  try
    Result := TRainfallGraphTabSheet(FTabSheet).ProcessMetaDataEvent;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.

