{******************************************************************************}
{*  UNIT      : Contains the class TGaugeList.                                *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2005/04/01                                                    *}
{*  COPYRIGHT : Copyright © 2005 DWAF                                         *}
{******************************************************************************}

unit UAbstractGaugeList;

interface

uses
  Contnrs,
  Classes,
  Windows,
  UAbstractObject,
  UAbstractGauge;

type

  TIterateFunction = function (AGauge: TAbstractRainGauge; AIndex: integer; ASender: TObject): boolean of object;
  TGaugeListSortOption = (glsoFile, glsoGroup, glsoLatitude, glsoLongitude);
  TAbstractGaugeList = class(TAbstractAppObject)
  protected
    FListIsChanging    : boolean;
    FListHasChanged    : boolean;
    FFileLoaded        : boolean;
    FFileSaved         : boolean;
    FLoadFileName      : string;
    FSaveFileName      : string;
    function GetTotalCount : integer; virtual; abstract;
    function GetSelectedCount : integer; virtual; abstract;
    function GetUnselectedCount : integer; virtual; abstract;
    procedure Iterate (AFunction : TIterateFunction; ASender: TObject); virtual; abstract;
  public
    function LoadFromDatabase(AProgressFunction : TNotifyEvent = nil) : boolean; virtual; abstract;
    function SaveToFile : boolean; virtual; abstract;
    function SaveToDB : boolean; virtual; abstract;
    function ShowSelGauges : boolean; virtual; abstract;
    procedure SelectAll; virtual; abstract;
    procedure DeSelectAll; virtual; abstract;
    procedure InvertSelection; virtual; abstract;

    procedure SelectByStationID(ANumber : integer; AReplaceSelection : boolean = true); virtual; abstract;
    procedure SelectByStationNumber(AStringIn : string; AReplaceSelection : boolean = true); virtual; abstract;
    procedure SelectByStationName(AStringIn : string; AReplaceSelection : boolean = true); virtual; abstract;

    procedure SelectByClosestPoint(ASearchPoint : TPoint ; ALength,AWidth : Integer; AReplaceSelection : boolean = true); overload; virtual; abstract;
    procedure SelectByClosestPoint(ASearchPoint : TPoint ; ARadius : integer; AReplaceSelection : boolean = true); overload; virtual; abstract;
    procedure SelectByDistance (ALatitude         : double;
                                ALongitude        : double;
                                ADistance         : double;
                                AReplaceSelection : boolean = TRUE); virtual; abstract;

    procedure SelectByCoOrdinateWindow(ACordRange : TRect; AReplaceSelection : boolean = true); virtual; abstract;
    procedure SetSortOption(ASortOption: TGaugeListSortOption); virtual; abstract;

    function GetItemByStationID(AIndex : integer) : TAbstractRainGauge; virtual; abstract;
    function GetItem(AIndex : integer) : TAbstractRainGauge; overload; virtual; abstract;
    function GetItem(AGaugeNr : string) : TAbstractRainGauge; overload; virtual; abstract;

    procedure GetSelectedList(AStringList : TStringList);  virtual; abstract;
    procedure GetUnSelectedList(AStringList : TStringList);  virtual; abstract;
    procedure GetFullList(AStringList : TStringList); virtual; abstract;

    property LoadFileName    : string   read FLoadFileName       write FLoadFileName;
    property SaveFileName    : string   read FSaveFileName       write FSaveFileName;
    property TotalCount      : integer  read GetTotalCount;
    property SelectedCount   : integer  read GetSelectedCount;
    property UnselectedCount : integer  read GetUnselectedCount;
    property FileLoaded      : boolean  read FFileLoaded;
    property FileSaved       : boolean  read FFileSaved;
    property ListIsChanging  : boolean  read FListIsChanging     write FListIsChanging;
    property ListHasChanged  : boolean  read FListHasChanged     write FListHasChanged;
  end;

implementation

{ TAbstractGaugeList }

end.
