unit URainfallMultiGaugeComparitorManager;

interface
uses
  UTabsheetManager;

type
  TRainfallMultiGaugeComparitorManager = class(TTabSheetManager)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
  end;

implementation

uses

   SysUtils,
   UErrorHandlingOperations,
   URainfallMultiGaugeComparitorSheet;

{ TRainfallMultiGaugeComparitorManager }

procedure TRainfallMultiGaugeComparitorManager.DestroyMemberObjects;
const OPNAME = 'TRainfallMultiGaugeComparitorManager.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallMultiGaugeComparitorManager.CreateMemberObjects;
const OPNAME = 'TRainfallMultiGaugeComparitorManager.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FTabSheet := TRainfallMultiGaugeComparitorSheet.Create(nil, fAppModules);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
