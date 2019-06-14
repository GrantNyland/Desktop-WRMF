(******************************************************************************)
(*  Contains : Class THydrologyComObject.
(******************************************************************************)
unit UHydrologyComObject;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, ActiveX, Classes, ComObj,HydrologyCom_TLB,StdVcl;

type
  THydrologyComObject = class(TTypedComObject, IHydrologyComObject)
  protected
    {Declare IHydrologyComObject methods here}
  public
  end;

implementation

uses
  ComServ;

initialization
  TTypedComObjectFactory.Create(ComServer, THydrologyComObject, Class_HydrologyComObject,
    ciMultiInstance, tmApartment);


end.
