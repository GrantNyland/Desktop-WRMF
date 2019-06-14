//
//  Contains : Classes used to transfer search criteria
//

unit UGaugeSearch;

interface

uses windows;

type
  TRGResultType = (rtReplaceSelection,rtAddtoSelection);
  TBaseSearchOptions = (SoWildCardSearch, SoExactMatch, SoPartialMatch);
  TSearchOptions = set of TBaseSearchOptions;
  TGaugeSearch = class(TObject)
    private
      FResultType : TRGResultType;
    public
      constructor Create;
      property ResultType : TRGResultType read FResultType write FResultType;
  end;

  TGaugeSearchByString = class(TGaugeSearch)
    FSearchString : string;
    FSearchType   : TSearchOptions;
  end;

  TGaugeSearchByRect = class (TGaugeSearch)
    FSearchCord : TRect;
  end;

  TGaugeSearchByCircle = class (TGaugeSearch)
    FX1, FY1, FRadius : integer;
  end;

implementation

uses
  // Delphi
  Sysutils,
  // DWAF
  UErrorHandlingOperations;

{ TGaugeSearch }

constructor TGaugeSearch.Create;
const OPNAME = 'TGaugeSearch.Create';
begin
  try
    inherited create;
    FResultType := rtReplaceSelection;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

end.
