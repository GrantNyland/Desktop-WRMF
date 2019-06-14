//
//  Contains : Classes used to transfer search criteria
//

unit UAbstractPatchObject;

interface

type

  TAbstractPatchObject = class(TObject)
    function GetPatchID : integer; virtual; abstract;
    function GetStationID : integer; virtual; abstract;
    function GetDataVersion : integer; virtual; abstract;
  end;

implementation  

end.
