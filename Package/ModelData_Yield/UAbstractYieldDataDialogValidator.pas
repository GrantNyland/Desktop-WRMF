//
//
//  UNIT      : Contains the class TAbstractYieldDataDialogValidator.
//  AUTHOR    : Dziedzi Ramulondi(Aravia)
//  DATE      : 2004/12/22
//  COPYRIGHT : Copyright © 2004 DWAF
//
//
unit UAbstractYieldDataDialogValidator;

interface

uses
  Classes,
  UDataComponent,
  UYieldContextValidationType;

type

  TAbstractYieldDataDialogValidator = class(TAbstractDataDialogValidator)
  protected
  public
    procedure DoContextValidation (AValidationType : TDialogValidationType); virtual;
  end;

implementation

uses
  SysUtils,
  UErrorHandlingOperations;

{ TAbstractYieldDataDialogValidator }

procedure TAbstractYieldDataDialogValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TAbstractYieldDataDialogValidator.DoContextValidation';
begin
  try
    //FAppModules.GlobalData.SetStopOnFirstErr(True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
