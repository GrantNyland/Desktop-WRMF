//
//
//  UNIT      : Contains TTabSheetManager Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/03/02
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UVisioTabSheetManager;

interface

uses
  UTabSheetManager;

type
  TVisioTabSheetManager = class(TTabSheetManager)
  public
    function HandleVNVEvent (AVisioApp       : IUnknown;
                             AVisioDoc       : IUnknown;
                             AVisioEventCode : integer;
                             ASourceObj      : IUnknown;
                             AEventID        : Integer;
                             AEventSeqNum    : Integer;
                             ASubjectObj     : IUnknown;
                             AMoreInfo       : OleVariant) : boolean; virtual; abstract;
    function ProcessVNVSpecial(const AParameter: WideString): boolean; virtual; abstract;
  end;

implementation

end.
