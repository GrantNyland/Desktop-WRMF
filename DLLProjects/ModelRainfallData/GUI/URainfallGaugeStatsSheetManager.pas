{******************************************************************************}
{*  UNIT      : Contains TRainfallGaugeStatsSheetManager Class                *}
{*  AUTHOR    : RH Steyn(Cornastone)                                          *}
{*  DATE      : 01/11/2004                                                    *}
{*  COPYRIGHT : Copyright © 2004 DWAF                                         *}
{******************************************************************************}

unit URainfallGaugeStatsSheetManager;

interface
uses
  UTabsheetManager;

type
  TRainfallGaugeStatsSheetManager = class( TTabSheetManager )
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
  end;

implementation

uses

   SysUtils,
   UErrorHandlingOperations,
   URainfallGaugeStatsSheet;

{ TRainFallDataTabSheetManager }

procedure TRainfallGaugeStatsSheetManager.DestroyMemberObjects;
const OPNAME = 'TRainfallGaugeStatsSheetManager.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallGaugeStatsSheetManager.CreateMemberObjects;
const OPNAME = 'TRainfallGaugeStatsSheetManager.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FTabSheet := TRainfallGaugeStatsSheet.Create(nil, fAppModules);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.

