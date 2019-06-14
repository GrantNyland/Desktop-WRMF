//
//
//  UNIT      : Contains     Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 28/07/2004
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UAbstractDataObjectRainfall;

interface

uses

  Classes,
  SysUtils,
  Contnrs,
  Controls,

  UAbstractObject,
  UAbstractGaugeList,
  UYearlyStationData,
  UAbstractModelData;

type

  TAbstractDataObjectRainfall = class(TAbstractModelData)
  protected

  public
    function LoadData : boolean; virtual; abstract;

    function CreateAPatch (APatchTypeID : integer;
                           AStationID   : integer;
                           ADescription : string) : integer; virtual; abstract;
    function DeleteAPatch (APatchID : integer) : boolean; virtual; abstract;
    function ModifyPatchDescription (AStationID      : integer;
                                     APatchID        : integer;
                                     ANewDescription : string) : boolean; virtual; abstract;

    function AddToPatch(AStationID       : integer;
                        APatchID         : integer;
                        ASourceStationID : integer;
                        ASourcePatchID   : Integer = 0): Boolean; virtual; abstract;
    function RemoveFromPatch (AStationID       : integer;
                              APatchID         : integer;
                              ASourceStationID : integer;
                              ASourcePatchID   : Integer = 0) : boolean; virtual; abstract;

    function GaugeList : TAbstractGaugeList; virtual; abstract;

    function GetZoneStations : TStringList; virtual; abstract;

    function CheckAndCreateZone : boolean; virtual; abstract;

    function AddToRainFallZone (AStationId : integer; ASourcePatchID : integer) : boolean; virtual; abstract;
    function RemoveFromRainfallZone (AStationId : integer; ASourcePatchID : integer) : boolean; virtual; abstract;

    function GetZoneChangeDate : TDateTime; virtual; abstract;
    procedure SetZoneChangeDate (ADate : TDateTime); virtual; abstract;
    function GetZoneRunDate : TDateTime; virtual; abstract;
    procedure SetZoneRunDate (ADate : TDateTime); virtual; abstract;

    function GetCatchmentFileName : string; virtual; abstract;
    function SetCatchmentFileName (AValue : string) : boolean; virtual; abstract;
    function GetOutputFileName : string; virtual; abstract;
    function SetOutputFileName (AValue : string) : boolean; virtual; abstract;

    function GetCatchmentFileData : TStringList; virtual; abstract;
    function SetCatchmentFileData (AValue : TStringList) : boolean; virtual; abstract;
    function GetOutputFileData : TStringList; virtual; abstract;
    function SetOutputFileData (AValue : TStringList) : boolean; virtual; abstract;

    function GetDefaultDir: string; virtual; abstract;
    procedure SetDefaultDir ( const aValue : string ); virtual; abstract;

    function SetRainfallDataPatchBlob(const APatchID : integer; AValue : TStringList; AFieldName : string) : boolean; virtual; abstract;
    function GetRainfallDataPatchBlob(const APatchID : integer; AFieldName : string) : TStringList; virtual; abstract;

    procedure GetDailyDataByMonthAndYear (AStationID : integer;
                                          APatchID   : integer;
                                          AMonth     : integer;
                                          AYear      : integer;
                                          AData      : TStringList); virtual; abstract;

    function SaveRAWDataFiles : boolean; virtual; abstract;
    function SaveMPDataFiles (AStationList : TStringList) : boolean; virtual; abstract;
    function CreateReport (ADataList : TStringList) : boolean; virtual; abstract;
    function GetStationList : TObjectList; virtual; abstract;
    function GetStationDataByNumber (AStationNumber : string) : TStationData; virtual; abstract;
    function GetStationDataByID (AStationID : integer) : TStationData; virtual; abstract;
    function GetStationDataByIndex (AIndex : integer) : TStationData; virtual; abstract;
    function GetCurrentPatchID : Integer; virtual; abstract;
    function SetCurrentPatchID (APatchID: Integer): Boolean; virtual; abstract;
    function GetCurrentStationID : Integer; virtual; abstract;
    function SetCurrentStationID (AStationID: Integer): Boolean; virtual; abstract;
    function GetHydroStartMonth : integer; virtual; abstract;
    procedure SetHydroStartMonth (AValue : integer); virtual; abstract;
    procedure GetPatchSourceStationNumbers (AStationID   : integer;
                                            APatchID     : integer;
                                            ANumbersList : TStringList); virtual; abstract;

    function LoadMonthlyData : boolean; virtual; abstract;
    procedure LatLong (AStationNumber : string;
                       var ALat       : double;
                       var ALong      : double); virtual; abstract;
    property StationList : TObjectList read GetStationList;
    property HydroStartMonth : integer read GetHydroStartMonth write SetHydroStartMonth;
  end;

implementation

end.
