//
//
//  UNIT      : Contains TAbstractYieldModelDataObject Class
//  AUTHOR    : Dziedzi Ramulondi(Arivia)
//  DATE      : 2003/09/18
//  COPYRIGHT : Copyright © 2003 DWAF
//
//

unit UAbstractRainfallStationData;

interface

uses

  // Delphi RTL, VCL, etc
  Classes,
  SysUtils,
  Contnrs,

  // DWAF - arivia.kom
  UAbstractObject;

type

  TRainfallRec = record
                   FIsNull    : boolean;
                   FPatchChar : char;
                   FValue     : smallint;
                 end;
  TDailyRainfallRec = array[1..31] of TRainfallRec;
  TMonthlyRainfallRec = array[1..12] of TDailyRainfallRec;
  TYearlyRainfallData = array of TMonthlyRainfallRec;

  TAbstractRainfallStationData = class(TAbstractObject)
  protected
    function IsPatch : boolean; virtual; abstract;
    function GetName : string; virtual; abstract;
    function GetGaugeID : integer; virtual; abstract;
    function GetRecordVersion : byte; virtual; abstract;
    function GetLongitude : string; virtual; abstract;
    function GetLatitude : string; virtual; abstract;
  public

    function GetDailyValue(AYear : smallint; AMonth : byte; ADay : byte) : TRainfallRec; virtual; abstract;
    function GetMonthValue(AYear : smallint; AMonth : byte) : TRainfallRec; virtual; abstract;
    function GetYearValue(AYear : smallint) : TYearlyRainfallData; overload; virtual; abstract;
    function GetYearValue : TYearlyRainfallData; overload; virtual; abstract;
    function GetFirstYear : smallint; virtual; abstract;
    function GetLastYear : smallint; virtual; abstract;
    function GetNoOfYears : smallint; virtual; abstract;
    function SetYearValue (AValue : TYearlyRainfallData): boolean;virtual; abstract;

    property Name : string          read GetName;
    property GaugeID   : integer    read GetGaugeID;
    property RecordVersion : byte   read GetRecordVersion;
    property Longitude : string     read GetLongitude;
    property Latitude : string      read GetLatitude;
  end;

implementation

end.
