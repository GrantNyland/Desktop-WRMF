unit RainfallComObject;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, ActiveX, Classes, ComObj, RainfallCom_TLB, StdVcl;

type
  TRainfallComObject = class(TTypedComObject, IRainfallComObject)
  protected
    {Declare IRainfallComObject methods here}
  end;

implementation

uses ComServ;

initialization
  TTypedComObjectFactory.Create(ComServer, TRainfallComObject, Class_RainfallComObject,
    ciMultiInstance, tmApartment);
end.
