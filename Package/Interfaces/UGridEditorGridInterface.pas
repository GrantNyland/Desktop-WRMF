//
//
//  UNIT      : Contains IGridEditorGrid Interface
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2003/02/28
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UGridEditorGridInterface;

interface

uses
  Classes,
  UAbstractObject;

type
  IGridEditorGrid = interface(ILanguageEventsInterface)
    ['{99B658E8-510E-4472-84E2-838260DC164A}']
    function IsVisible: boolean;
    function SaveState: boolean;
    procedure SetBrowseMode;
    procedure SetReadOnlyMode(AIsReadOnly: boolean);
    procedure ClearDataViewer;
    procedure PopulateDataViewer;
    procedure UpdateHintText;
    function CurrentFieldHintText: string;
    procedure CopyGridDataInto(var AStringList: TStringList);
  end;

implementation

end.
