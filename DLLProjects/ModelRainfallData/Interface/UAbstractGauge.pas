{******************************************************************************}
{*  UNIT      : Contains the class TAbstractRainGauge.                        *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2005/04/01                                                    *}
{*  COPYRIGHT : Copyright © 2005 DWAF                                         *}
{******************************************************************************}

unit UAbstractGauge;

interface

uses
  contnrs,
  classes,
  windows,
  UAbstractObject;

type
  TAbstractRainGauge = class(TAbstractAppObject)
  protected
  public
    function ListIndex   : integer; virtual; abstract;
    function Group       : string;  virtual; abstract;
    function GaugeID     : integer; virtual; abstract;
    function GaugeNumber : string;  virtual; abstract;
    function GaugeName   : string;  virtual; abstract;
    function Latitude    : integer; virtual; abstract;
    function Longitude   : integer; virtual; abstract;
    function IsSelected  : boolean; virtual; abstract;
    function Select      : boolean; virtual; abstract;
    function UnSelect    : boolean; virtual; abstract;
    function IsInWR90    : boolean; virtual; abstract;

    function SetIsSelected (AValue : boolean): boolean; virtual; abstract;

  end;

implementation

end.
