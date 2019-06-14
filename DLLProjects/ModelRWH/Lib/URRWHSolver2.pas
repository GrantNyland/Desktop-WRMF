{*******************************************************}
{                                                       }
{  UNIT      : Contains TSolver                         }
{  AUTHOR    : Dziedzi Ramulondi                        }
{  DATE      : 27/08/2010                               }
{  COPYRIGHT : Copyright © 2010 DRR Data Solutions	    }
{                                                       }
{*******************************************************}

unit URRWHSolver2;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ComCtrls,
  Vcl.Grids,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,

  UColourButtons,
  UAbstractObject,
  URWHDataObject,
  UOutputFileHeader,
  URainFallFileAgent,
  UOutputDailyFileAgent;

type

  TIntegerOneDimensionArray     = array of integer;
  TIntegerTwoDimensionArray     = array of array of integer;
  TIntegerThreeDimensionArray   = array of array of array of integer;
  TIntegerTwoArray              = array [1..2] of integer;

  TDoubleOneDimensionArray      = array of double;
  TDoubleTwoDimensionArray      = array of array of double;
  TDoubleThreeDimensionArray    = array of array of array of double;
  TDoubleAnnualArray            = array [1..366] of double;

  TSolver = class(TAbstractAppObject)
  protected
    FDecide         : integer;
    FNoTanks        : integer;
    FNYears         : integer;
    FNDays          : integer;
    FNoHseHolds     : integer;
    FHseHoldSize    : integer;

    FRoofArea       : double;
    FRunCoeff       : double;
    FPerCapitaDem   : double;

    FInitStorage    : double;
    FUpperL         : double;
    FLowerL         : double;

    //********************************************************************
    FYear          : TIntegerOneDimensionArray;
    FMonth         : TIntegerOneDimensionArray;
    FDay           : TIntegerOneDimensionArray;
    FRiverS        : TIntegerThreeDimensionArray;
    FRainS         : TIntegerThreeDimensionArray;
    FOtherS        : TIntegerThreeDimensionArray;
    FRanked        : TIntegerTwoDimensionArray;
    FRiverAnRank   : TIntegerTwoDimensionArray;
    FRainAnRank    : TIntegerTwoDimensionArray;
    FOtherAnRank   : TIntegerTwoDimensionArray;
    FRivRainS      : TIntegerThreeDimensionArray;
    FRivRainAnRank : TIntegerTwoDimensionArray;
    FMiss          : TIntegerOneDimensionArray;
    FLeap          : TIntegerOneDimensionArray;

    //********************************************************************
    FmagFlow        : TDoubleOneDimensionArray;
    FmagSortFlow    : TDoubleTwoDimensionArray;
    FmagRanked      : TDoubleTwoDimensionArray;
    FEnvFlow        : TDoubleOneDimensionArray;
    FmagRain        : TDoubleOneDimensionArray;
    FmagSortRain    : TDoubleTwoDimensionArray;
    FmagAvail       : TDoubleTwoDimensionArray;
    FStorage        : TDoubleThreeDimensionArray;
    FSpill          : TDoubleThreeDimensionArray;
    FDeficit        : TDoubleThreeDimensionArray;
    FTankVol        : TDoubleOneDimensionArray;

    //**********************************************************************
    FSumDailyRain      : TDoubleAnnualArray;
    FIsumAvailData     : TDoubleAnnualArray;
    FAverageDailyRain  : TDoubleAnnualArray;
    FAntecedentRain    : TDoubleAnnualArray;

    Fl1                : TIntegerTwoArray;
    Fu1                : TIntegerTwoArray;

    FOutputDailyFileName   : string;
    FOutputMonthlyFileName : string;
    FOutputAnnualFileName  : string;
    FOutputBestTankFileName  : string;

    FRunConfig     : TRWHRunConfig;
    FStation       : TRainfallStation;

    FOutputDailyFileData    : TStringList;
    FOutputMonthlyFileData  : TStringList;
    FOutputAnnualFileData   : TStringList;
    FOutputBestTankFileData : TStringList;

    FOutputDailyRecord      : TOutputDailyRecord;
    FOutputFileHeader       : TOutputFileHeader;
    FRainFallFileAgent      : TRainFallFileAgent;

   procedure PopulateInputData;
   procedure PopulateFileHeader;
   procedure PopulateInputArrays;
   procedure InitialseArraysSize;
   procedure Ranking(var ADummyA : TIntegerThreeDimensionArray;var ARanked : TIntegerTwoDimensionArray; ANYears,ANoTanks,Aim: integer);
   procedure WriteOutputDailyLine(ATankIndex,ACounter,ADay : integer; ARainfall,AStorage : Double	; ADecide : integer; ASpill,ADeficit : Double	);
   procedure WriteOutputMonthlyLine(AYear:integer;ADataArray:TMonthyIntegerArray);
   procedure WriteOutputAnnualLine(ACount: integer;ADataArray: TMonthyIntegerArray);
   procedure WriteTwoDimensionDoubleArray(AContainer:TStrings;ADataArray: TTwoDimensionDoubleArray);
   procedure WriteOneDimensionDoubleArray(AContainer:TStrings;ADataArray: TOneDimensionDoubleArray);
   function RunModel: Boolean;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function Initialise : boolean; override;
    function AfterInitialise : boolean;
    function Finalise   : boolean;
    function Run(ARunConfig : TRWHRunConfig; AStation : TRainfallStation;AProgressUpdateFuntion: TProgressUpdateFuntion) : boolean;
  end;

implementation
uses
  SysConst,
  DateUtils,
  UUtilities,
  UConstants,
  UErrorHandlingOperations;

const
  rwhDaysPerYear = 367;

  { TSolver }

procedure TSolver.AfterConstruction;
const OPNAME = 'TSolver.AfterConstruction';
begin
  inherited;
  try
    FRunConfig               := nil;
    FStation                 := nil;
    FOutputDailyFileData     := TStringList.Create;
    FOutputMonthlyFileData   := TStringList.Create;
    FOutputAnnualFileData    := TStringList.Create;
    FOutputBestTankFileData  := TStringList.Create;
    FRainFallFileAgent       := TRainFallFileAgent.Create;
    FOutputFileHeader        := TOutputFileHeader.Create;
    FOutputDailyRecord       := TOutputDailyRecord.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSolver.BeforeDestruction;
const OPNAME = 'TSolver.BeforeDestruction';
begin
  inherited;
  try
    FreeAndNil(FOutputDailyRecord);
    FreeAndNil(FOutputDailyFileData);
    FreeAndNil(FOutputMonthlyFileData);
    FreeAndNil(FOutputAnnualFileData);
    FreeAndNil(FOutputBestTankFileData);
    FreeAndNil(FRainFallFileAgent);
    FreeAndNil(FOutputFileHeader);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSolver.Initialise : boolean;
const OPNAME = 'TSolver.Initialise';
begin
  Result := False;
  try
    FOutputDailyFileName   := '';
    FOutputMonthlyFileName   := '';
    FOutputAnnualFileName   := '';
    FOutputBestTankFileName := '';
    FOutputDailyFileData.Clear;
    FOutputMonthlyFileData.Clear;
    FOutputAnnualFileData.Clear;
    FOutputBestTankFileData.Clear;
    FOutputFileHeader.Reset;
    Finalise;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSolver.AfterInitialise: boolean;
const OPNAME = 'TSolver.AfterInitialise';
begin
  Result := False;
  try
    FOutputDailyFileName     := RWHModelData.GetRainfallStationOutputDailyFileName(FStation.StationNumber);
    FOutputMonthlyFileName   := RWHModelData.GetRainfallStationOutputMonthlyFileName(FStation.StationNumber);
    FOutputAnnualFileName    := RWHModelData.GetRainfallStationOutputAnnualFileName(FStation.StationNumber);
    FOutputBestTankFileName  := RWHModelData.GetRainfallStationOutputBestTankFileName(FStation.StationNumber);

    FRainFallFileAgent.Initialise;
    FRainFallFileAgent.LoadStationDailyData(FStation.StationNumber);
    PopulateInputData;
    PopulateFileHeader;
    InitialseArraysSize;
    PopulateInputArrays;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSolver.PopulateInputData;
const OPNAME = 'TSolver.PopulateInputData';
var
  LDate : TDate;
begin
  try
    LDate := FRainFallFileAgent.StartNoNullDate;
    if(FRunConfig.PeriodStartDate = NullDateTime) or (FRunConfig.PeriodStartDate < LDate) then
       FRunConfig.PeriodStartDate := LDate;
    LDate := FRainFallFileAgent.EndNoNullDate;
    if(FRunConfig.PeriodEndDate = NullDateTime) or (FRunConfig.PeriodEndDate > LDate) then
       FRunConfig.PeriodEndDate := LDate;

    FNYears       := FRunConfig.YearsInAnalysis;                                                                    // DSR fix when you know how gaps in data are done
    FNDays        := FRunConfig.DaysInAnalysis;                                                                     // DSR fix when you know how gaps in data are done
    //FNYears       := FRunConfig.YearsInAnalysis;
    //FNDays        := FRainFallFileAgent.DaysInAnalysis;

    FRoofArea     := FRunConfig.RoofArea;
    FRunCoeff     := FRunConfig.RoofRunoffCoef;

    FHseHoldSize  := FRunConfig.HouseHoldMembers;
    FPerCapitaDem := FRunConfig.HouseHoldDemandPP;
    FNoHseHolds   := FRunConfig.HouseHoldNumber;

    FInitStorage  := FRunConfig.RunStartVolume;
    FUpperL       := FRunConfig.RunStartLevel;
    FLowerL       := FRunConfig.RunStopLevel;

    FNoTanks     := FRunConfig.TankCount;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSolver.PopulateFileHeader;
const OPNAME = 'TSolver.PopulateFileHeader';
var
  LMonthIndex : integer;
  LIndex : integer;
begin
  try
    FOutputFileHeader.FRunDate             := Now;
    FOutputFileHeader.FStationNumber       := FStation.StationNumber;
    FOutputFileHeader.FStationName         := FStation.StationName;
    FOutputFileHeader.FPeriod_StartDate    := FRunConfig.PeriodStartDate;
    FOutputFileHeader.FPeriod_EndDate      := FRunConfig.PeriodEndDate;
    FOutputFileHeader.FRun_TypeID          := FRunConfig.RunTypeID;;
    FOutputFileHeader.FRun_StartVolume     := FRunConfig.RunStartVolume;
    FOutputFileHeader.FRun_StartLevel      := FRunConfig.RunStartLevel;
    FOutputFileHeader.FRun_StopLevel       := FRunConfig.RunStopLevel;
    FOutputFileHeader.FRoof_Area           := FRunConfig.RoofArea;
    FOutputFileHeader.FRoof_RunoffCoef     := FRunConfig.RoofRunoffCoef;
    FOutputFileHeader.FHouseHold_Number    := FRunConfig.HouseHoldNumber;
    FOutputFileHeader.FHouseHold_Members   := FRunConfig.HouseHoldMembers;
    FOutputFileHeader.FHouseHold_DemandPP  := FRunConfig.HouseHoldDemandPP;

    //LMonthIndex := MonthOf(FRunConfig.PeriodStartDate);
    LMonthIndex := 1;
    for LIndex := 1 to 12 do
    begin
      FOutputFileHeader.FMonthNumbers[LIndex] := LMonthIndex;
      case LMonthIndex of
        01: FOutputFileHeader.FMonthNames[LIndex] := SLongMonthNameJan;
        02: FOutputFileHeader.FMonthNames[LIndex] := SLongMonthNameFeb;
        03: FOutputFileHeader.FMonthNames[LIndex] := SLongMonthNameMar;
        04: FOutputFileHeader.FMonthNames[LIndex] := SLongMonthNameApr;
        05: FOutputFileHeader.FMonthNames[LIndex] := SLongMonthNameMay;
        06: FOutputFileHeader.FMonthNames[LIndex] := SLongMonthNameJun;
        07: FOutputFileHeader.FMonthNames[LIndex] := SLongMonthNameJul;
        08: FOutputFileHeader.FMonthNames[LIndex] := SLongMonthNameAug;
        09: FOutputFileHeader.FMonthNames[LIndex] := SLongMonthNameSep;
        10: FOutputFileHeader.FMonthNames[LIndex] := SLongMonthNameOct;
        11: FOutputFileHeader.FMonthNames[LIndex] := SLongMonthNameNov;
        12: FOutputFileHeader.FMonthNames[LIndex] := SLongMonthNameDec;
      end;
      LMonthIndex := LMonthIndex + 1;
      if(LMonthIndex > 12) then
        LMonthIndex := 1;
    end;
    if(FNoTanks >= 1) then
       FOutputFileHeader.FTankSizes[01] := FRunConfig.TankSize01;
    if(FNoTanks >= 2) then
       FOutputFileHeader.FTankSizes[02] := FRunConfig.TankSize02;
    if(FNoTanks >= 3) then
       FOutputFileHeader.FTankSizes[03] := FRunConfig.TankSize03;
    if(FNoTanks >= 4) then
       FOutputFileHeader.FTankSizes[04] := FRunConfig.TankSize04;
    if(FNoTanks >= 5) then
       FOutputFileHeader.FTankSizes[05] := FRunConfig.TankSize05;
    if(FNoTanks >= 6) then
       FOutputFileHeader.FTankSizes[06] := FRunConfig.TankSize06;
    if(FNoTanks >= 7) then
       FOutputFileHeader.FTankSizes[07] := FRunConfig.TankSize07;
    if(FNoTanks >= 8) then
       FOutputFileHeader.FTankSizes[08] := FRunConfig.TankSize08;
    if(FNoTanks >= 9) then
       FOutputFileHeader.FTankSizes[09] := FRunConfig.TankSize09;
    if(FNoTanks >= 10) then
       FOutputFileHeader.FTankSizes[10] := FRunConfig.TankSize10;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSolver.PopulateInputArrays;
const OPNAME = 'TSolver.PopulateInputArrays';
var
  LIndex : integer;
  LRainfallRecord : TRainfallRecord;
  LPrevDate        : TDate;
begin
  try
    if(FNoTanks >= 1) then
       FTankVol[01] := FRunConfig.TankSize01;
    if(FNoTanks >= 2) then
       FTankVol[02] := FRunConfig.TankSize02;
    if(FNoTanks >= 3) then
       FTankVol[03] := FRunConfig.TankSize03;
    if(FNoTanks >= 4) then
       FTankVol[04] := FRunConfig.TankSize04;
    if(FNoTanks >= 5) then
       FTankVol[05] := FRunConfig.TankSize05;
    if(FNoTanks >= 6) then
       FTankVol[06] := FRunConfig.TankSize06;
    if(FNoTanks >= 7) then
       FTankVol[07] := FRunConfig.TankSize07;
    if(FNoTanks >= 8) then
       FTankVol[08] := FRunConfig.TankSize08;
    if(FNoTanks >= 9) then
       FTankVol[09] := FRunConfig.TankSize09;
    if(FNoTanks >= 10) then
       FTankVol[10] := FRunConfig.TankSize10;

    //LStartDate      := FRunConfig.PeriodStartDate;                                                                // DSR fix when you know how gaps in data are done
    //LEndDate        := FRunConfig.PeriodEndDate;                                                                  // DSR fix when you know how gaps in data are done

    LRainfallRecord := FRainFallFileAgent.First;
    LPrevDate       := LRainfallRecord.Date;
    for LIndex := 1 to FNDays do
    begin
      FYear[LIndex]    := YearOf(LPrevDate);
      FMonth[LIndex]   := MonthOf(LPrevDate);
      FDay[LIndex]     := DayOf(LPrevDate);
      FmagFlow[LIndex] := 0.0;
      FmagRain[LIndex] := 0.0;
      if(LRainfallRecord <> nil) then
      begin
        if(LRainfallRecord.Date = LPrevDate) then
        begin
          FYear[LIndex]    := YearOf(LRainfallRecord.Date);
          FMonth[LIndex]   := MonthOf(LRainfallRecord.Date);
          FDay[LIndex]     := DayOf(LRainfallRecord.Date);
          if(LRainfallRecord.Rainfall >= 0.0) then
            FmagRain[LIndex] := LRainfallRecord.Rainfall;
          LRainfallRecord := FRainFallFileAgent.Next;
        end;
      end;
      LPrevDate := LPrevDate + 1.0;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSolver.InitialseArraysSize;
const OPNAME = 'TSolver.InitialseArraysSize';
begin
  try
    //********************************************************************
    SetLength(FYear,FNDays+1);
    SetLength(FMonth,FNDays+1);
    SetLength(FDay,FNDays+1);
    SetLength(FmagFlow,FNDays+1);

    SetLength(FmagSortFlow,FNYears+2,rwhDaysPerYear);
    SetLength(FmagRanked,FNYears+2,rwhDaysPerYear);

    SetLength(FEnvFlow,rwhDaysPerYear);
    SetLength(FmagRain,FNDays+1);

    SetLength(FDeficit,FNYears+2,rwhDaysPerYear,FNoTanks+1);

    SetLength(FmagSortRain,FNYears+2,rwhDaysPerYear);
    SetLength(FmagAvail,FNYears+2,rwhDaysPerYear);

    SetLength(FStorage,FNYears+2,rwhDaysPerYear,FNoTanks+1);
    SetLength(FSpill,FNYears+2,rwhDaysPerYear,FNoTanks+1);

    SetLength(FRiverS,FNYears+2,13,FNoTanks+1);
    SetLength(FRainS,FNYears+2,13,FNoTanks+1);;
    SetLength(FOtherS,FNYears+2,13,FNoTanks+1);
    SetLength(FTankVol,FNoTanks+1);

    SetLength(FRanked,FNYears+2,FNoTanks+1);
    SetLength(FRiverAnRank,FNYears+2,FNoTanks+1);
    SetLength(FRainAnRank,FNYears+2,FNoTanks+1);
    SetLength(FOtherAnRank,FNYears+2,FNoTanks+1);

    SetLength(FRivRainS,FNYears+2,13,FNoTanks+1);
    SetLength(FRivRainAnRank,FNYears+2,FNoTanks+1);
    SetLength(FMiss,FNYears+1);
    SetLength(FLeap,FNYears+1);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSolver.Finalise: boolean;
const OPNAME = 'TSolver.Finalise';
begin
  Result := False;
  try
    //********************************************************************
    Finalize(FYear);
    Finalize(FMonth);
    Finalize(FDay);
    Finalize(FRiverS);
    Finalize(FRainS);

    Finalize(FOtherS);
    Finalize(FRanked);
    Finalize(FRiverAnRank);
    Finalize(FRainAnRank);

    Finalize(FOtherAnRank);
    Finalize(FRivRainS);
    Finalize(FRivRainAnRank);
    Finalize(FMiss);
    Finalize(FLeap);

    //********************************************************************
    Finalize(FmagFlow);
    Finalize(FmagSortFlow);
    Finalize(FmagRanked);
    Finalize(FEnvFlow);
    Finalize(FmagRain);
    Finalize(FmagSortRain);
    Finalize(FmagAvail);
    Finalize(FStorage);
    Finalize(FSpill);
    Finalize(FDeficit);
    Finalize(FTankVol);

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSolver.Run(ARunConfig: TRWHRunConfig; AStation: TRainfallStation;AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TSolver.Run';
var
  LStop : boolean;
begin
  Result := False;
  try
    if(ARunConfig = nil)  then
      raise Exception.Create('Configuration parameter not yet assigned');
    if(AStation = nil)  then
      raise Exception.Create('Rainfall station parameter not yet assigned');
    AProgressUpdateFuntion('Started running Solver for Rainfall station : '+ AStation.StationNumber,ptNone,LStop);

    Initialise;
    FRunConfig := ARunConfig;
    FStation   := AStation;
    FOutputFileHeader.FProvinceName := AStation.StationName;
    AfterInitialise;

    if(FRunConfig.PeriodStartDate = FRunConfig.PeriodEndDate) then
    begin
      AProgressUpdateFuntion('Start and end date is the same : '+ AStation.StationNumber,ptError,LStop);
      Exit;
    end;
    if RunModel then
    begin
      Result := True;
      AProgressUpdateFuntion('Completed running Solver for Rainfall station : '+ AStation.StationNumber,ptNone,LStop);
    end
    else
      AProgressUpdateFuntion('Could not complete running Solver for Rainfall station : '+ AStation.StationNumber,ptError,LStop);
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TSolver.RunModel: Boolean;
const OPNAME = 'TSolver.RunModel';
var
  LYear     : integer;
  //LIndex    : integer;
  i         : integer;
  i5        : integer;
  im        : integer;
  im2       : integer;
  it        : integer;
  id        : integer;
  id2       : integer;
  i2        : integer;
  j         : integer;
  j2        : integer;
  k         : integer;
  k1        : integer;
  k2        : integer;
  k3        : integer;
  l         : integer;
  ix        : integer;
  iymax     : integer;
  ny        : integer;
  NLeapY    : integer;
  Rank      : integer;
  LiDaysNo  : integer;
  LICountLeap : integer;
  Dummy     : Double;
  LConvert1  : Double;
  LConvert2  : Double;
  LDailyDem  : Double;
  F4Array   : TMonthyIntegerArray;
  LTempStr  : string;
  LAntecedentLow : double;
  Label     Label_20,Label_220,Label_400,Label_500,Label_700,Label_1000,Label_2030,Label_2120,Label_2122,Label_2110,Label_2130,Label_2140,Label_2150,Label_2160,Label_2170;
begin
  Result := False;
  //LContainer := TStringList.Create;
  try
    //sort data into years and days
    im                 := 1;
    i                  := 1;
    j                  := 1;
    id                 := FDay[1];
    id2                := FDay[FNDays];

    Label_20:
    k := FDay[j];
    FmagSortFlow[im,k] := FmagFlow[j];
    FmagSortRain[im,k] := FmagRain[j];
    if(i >= FNDays) then
    begin
    end
    else if(FYear[i] = FYear[i+1]) then
    begin
      i:=i+1;
      j:=j+1;
      goto Label_20;
    end
    else if(j < FNDays) then
    begin
      im:=im+1;
      i:=i+1;
      j:=j+1;
      goto Label_20;
    end;//


    iymax := FNYears;
    //**********************************************************************
    //Obtain average daily rainfall
    //Initialize summation of daily rainfalls and antecedent rainfall
    for i := 1 to 366  do
    begin
      FSumDailyRain[i]   := 0.0;
      FIsumAvailData[i]  := 0.0;
      FAntecedentRain[i] := 0.0;
    end;

    //add daily rainfalls for all years
    for k := id to 365  do
    begin
      for im := 1 to iymax-1  do
      begin
        if(FmagSortRain[im,k] >= 0.0) then
        begin
          FSumDailyRain[k] := FSumDailyRain[k]+FmagSortRain[im,k];
          FIsumAvailData[k]:= FIsumAvailData[k] + 1;
        end;//
      end;
    end;


    for k := 1 to id-1  do
    begin
      for im := 2 to iymax-1  do
      begin
        if(FmagSortRain[im,k] >= 0.0) then
        begin
          FSumDailyRain[k]:=FSumDailyRain[k]+FmagSortRain[im,k];
          FIsumAvailData[k]:=FIsumAvailData[k] + 1;
        end;
      end;
    end;

    //add daily rainfalls for FDay 366 of FLeap years  k=366
    for im := 1 to iymax-1  do
    begin
      if(((FYear[1]+im) div 4) = ((FYear[1]+im)/4.0)) then                                                          //DSR is this statement neccesary.
      begin
        if(FmagSortRain[im,366] >= 0.0) then
        begin
          FSumDailyRain[366]  := FSumDailyRain[366] + FmagSortRain[im,366];
          FIsumAvailData[366] := FIsumAvailData[366] + 1;
        end;
      end;
    end;

    //add daily rainfalls for last FYear
    for k := 1 to id2  do
    begin
      if(FmagSortRain[iymax,k] >=0) then
      begin
        FSumDailyRain[k]   := FSumDailyRain[k] + FmagSortRain[iymax,k];
        FIsumAvailData[k]  := FIsumAvailData[k] + 1;
      end;
    end;

    //compute average daily rainfalls
    for k := 1 to 366  do
    begin
      if (FIsumAvailData[k] = 0.0) then
        FAverageDailyRain[k] := 0.0
      else
        FAverageDailyRain[k] := FSumDailyRain[k]/FIsumAvailData[k];
    end;

    //obtain 90 FDay antecedent rainfalls for every FDay
    for k := 1 to 366  do
    begin
      if(k < 90)then
      begin
        k1 := 366-90+k;
        for k2 := k1 to 366  do
        begin
          FAntecedentRain[k] := FAntecedentRain[k] + FAverageDailyRain[k2];
        end;
        for k2 := 1 to k  do
        begin
          FAntecedentRain[k] := FAntecedentRain[k] + FAverageDailyRain[k2];
        end;
      end
      else
      begin
        for k2 := k-90+1 to k  do
        begin
          FAntecedentRain[k] := FAntecedentRain[k] + FAverageDailyRain[k2];
        end;
      end;
    end;

    //obtain location (day) with lowest Antecedent 90 day rainfall
    //this FDay will be considered as the start of the new FYear
    LAntecedentLow := 10e20;
    k3             := 0;                                                                                            //DSR is this the correct initialization
    for k := 1 to 366  do
    begin
      if(FAntecedentRain[k] <= LAntecedentLow) then
      begin
        k3:=k;
        LAntecedentLow := FAntecedentRain[k];
      end
    end;

    //initialise array indicating if there are missing rainfalls
    for im := 1 to FNYears  do
    begin
      FMiss[im] := 0;
    end;

    //determine years with missing rainfall
    im:=1;
    if(k3 < id) then
    begin
      FMiss[im] := 1;
      im        := im+1;
    end;

    Label_2130:

    LiDaysNo := 365;
    if(((FYear[1]+im) div 4) = ((FYear[1]+im)/4.0)) then                                                             //DSR is this statement neccesary.
    begin
      LiDaysNo := 366;
    end;

    k:=k3;
    Label_2120:

    if(k <= LiDaysNo) then
    begin
      if(FmagSortRain[im,k] < 0.0) then
      begin
        FMiss[im] := 1;
        im        := im + 1;
        goto Label_2120;
      end
      else
      if(k < LiDaysNo) then
      begin
        k := k+1;
        goto Label_2120;
      end;
    end;

    im := im+1;
    k  := 1;

    Label_2122:

    if(k < k3) then
    begin
      if(FmagSortRain[im,k] < 0.0) then
      begin
        FMiss[im-1]:=1;
        goto Label_2110;
      end
      else if(k < k3-1) then
      begin
        k := k+1;
        goto Label_2122;
      end;
    end;

    Label_2110:

    if(im  <= iymax-2) then
    begin
      goto Label_2130;
    end;

    //      write[*,*] im,iymax,id2,k3

    //      now find if the last FYear is long enough and if it has missing values
    if(id2 < k3) then
    begin
      FMiss[iymax-1] := 1;
    end
    else
    begin
      if(((FYear[1]+iymax-1) div 4) = ((FYear[1]+iymax-1)/4.0)) then                                                //DSR is this statement neccesary.
      begin
        LiDaysNo := 366;
      end
      else
      begin
        LiDaysNo := 365;
      end;

      k := k3;

      Label_2150:

      if(k <= LiDaysNo) then
      begin
        if(FmagSortRain[iymax-1,k] < 0.0) then
        begin
          FMiss[iymax-1] := 1;
          goto Label_2140;
        end
        else if(k<LiDaysNo) then
        begin
          k := k + 1;
          goto Label_2150;
        end
      end;

      k:=1;
      Label_2160:

      if(k < k3) then
      begin
        if(FmagSortRain[iymax,k] < 0.0) then
        begin
          FMiss[iymax-1]:=1;
          goto Label_2140;
        end
        else if(k < k3-1) then
        begin
          k := k + 1;
          goto Label_2160;
        end;
      end;
    end;

    Label_2140:
    FMiss[iymax] := 1;
    //      do 2142 im=1,FNYears
    //      write[*,*]im,FMiss[im]
    //2142  continue
    //      recompose rainfall data with continuous record

    LICountLeap :=0;
    im          :=0;
    im2         :=1;

    Label_2170:

    if((FMiss[im2] = 1) and (im2 <= iymax-2)) then
    begin
      im2 := im2 + 1;
      goto Label_2170;
    end
    else if(im2 <= iymax-2) then
    begin
      im:=im+1;
      if(((FYear[1]+im2) div 4) = ((FYear[1]+im2)/4.0)) then                                                          //DSR is this statement neccesary.
      begin
        LiDaysNo     := 366;
        FLeap[im]    := 1;
        LICountLeap  := LICountLeap + 1;
      end
      else
      begin
        LiDaysNo := 365;
        FLeap[im]:=0;
      end;

      for k := k3 to LiDaysNo  do
      begin
        FmagSortRain[im,k] := FmagSortRain[im2,k];
      end;
      for k := 1 to k3-1  do
      begin
       FmagSortRain[im+1,k]:=FmagSortRain[im2+1,k];
      end;
    end;
    if(im2 < iymax-2) then
    begin
      im2:=im2+1;
      goto Label_2170;
    end;//

    //Recompose for the last FYear
    if(FMiss[iymax-1] <> 1.0) then
    begin
      im := im+1        ;
      if(((FYear[1]+ iymax-1) div 4) = ((FYear[1]+iymax-1)/4.0)) then                                               //DSR is this statement neccesary.
      begin
        LiDaysNo    := 366;
        FLeap[im]   :=1;
        LICountLeap :=LICountLeap + 1;
      end
      else
      begin
        LiDaysNo  := 365;
        FLeap[im] :=0;
      end;

      for k := k3 to LiDaysNo  do
      begin
        FmagSortRain[im,k] := FmagSortRain[iymax-1,k];
      end;
      for  k := 1 to k3-1  do
      begin
        FmagSortRain[im+1,k] := FmagSortRain[iymax,k];
      end;

      ny := im;
      FNDays := 365*ny+LICountLeap;
      id:=k3;
    end
    else
    begin
      ny     := im;
      FNDays := 365*ny+LICountLeap;
      id     := k3;
    end;

    //      write[*,*]im,ny,FNDays
    //      write recomposed record to file
    //AssignFile[Funit:=10,'RecompRain.txt' ]; Reset[Funit:=10];                                                    //DSR uncomment this
    ix := 1;
    LiDaysNo:=365;
    if(FLeap[ix] = 1) then
    begin
      LiDaysNo:=366;
    end;

    for k := id to LiDaysNo  do
    begin
      //writeln[F10,ix,k,FmagSortRain[ix,k]];                                                                       //DSR uncomment this
    end;

    for ix := 2 to ny do
    begin
      LiDaysNo:=365;
      if(FLeap[ix] = 1.0) then
      begin
        LiDaysNo:=366;
      end;
      for k := 1 to LiDaysNo  do
      begin
        //writeln[F10,ix,k,FmagSortRain[ix,k]];                                                                     //DSR uncomment this
      end;
    end;
    for k := 1 to id-1  do
    begin
      //writeln[F10,ny+1,k,FmagSortRain[ny+1,k]];                                                                   //DSR uncomment this
    end;
    //closeFile[F10];                                                                                               //DSR uncomment this

    //!!********************************************************************
    // rank sorted flow data for determination of low fow
    // environmental requirement [flow at 90% exceedance]

    Fl1[1]  := id;
    Fu1[1]  := 366;
    Fl1[2]  := 1;
    Fu1[2]  := id-1;

    for i2 := 1 to 2  do
    begin
      for i := Fl1[i2] to Fu1[i2]  do
      begin
        for j2 := 1 to im  do
        begin
          dummy := -1;
          k     := -1;
          for j := 1 to im  do
          begin
            if(FmagSortFlow[j,i] > dummy) then
            begin
              dummy:=FmagSortFlow[j,i];
              k:=j;
            end;
          end;
          if(k >= 0) then
          begin
             FmagRanked[j2,i] := FmagSortFlow[k,i];
             //write[*,*]j2,i,FmagRanked[j2,i]
             FmagSortFlow[k,i] := -2;
          end;
        end;
      end;
    end;

    //obtain magnitudes  and take care of
    //the FLeap FYear extra FDay - FDay [number 60 - February 29]
    //count number of FLeap years
    NLeapY:=0;
    for i := 0 to im  do
    begin
      if(((FYear[1]+i) div 4) = ((FYear[1]+i)/4.0)) then                                                          //DSR is this statement neccesary.
       NLeapY := NLeapY + 1;
    end;

    for i := 1 to 366  do
    begin
      im2:=im;
      if(i = 366) then
      begin
        im2:=NLeapY;
      end;

      rank := (90*(im2+1)) div 100;
      //      write[*,*]i, rank
      FEnvFlow[i] := FmagRanked[rank,i];
      //      write[*,*]i,FEnvFlow[i]
    end;


    //      reassign flows back to magSortedFlow Matrix
    //     sort data into years and days and obtain magnitude of flow available
    //      for domestic supply by subtracting environmental flow
    im      := 1;
    i       := 1;
    j       := 1;
    //id      := FDay[1];

    Label_220:


    k := FDay[j];
    FmagSortFlow[im,k] := FmagFlow[j];
    FmagAvail[im,k] := 0;
    if(FmagSortFlow[im,k] > FEnvFlow[k]) then
    begin
      FmagAvail[im,k] := FmagSortFlow[im,k] - FEnvFlow[k] ;
    end;
    //      write[*,*] im,k,j,FmagSortFlow[im,k]
    if(FYear[i] = FYear[i+1]) then
    begin
      i:=i+1;
      j:=j+1;
      goto Label_220;
    end
    else if(j < FNDays) then
    begin
      im:=im+1;
      i:=i+1;
      j:=j+1;
      goto Label_220;
    end;

    //      behaviour analysis of rainwater storage tank
    //      supplies water if run-of-river supply is inadequate or unavailable
    //      open[unit=3,file='ReitvleiOutput.txt',status='unknown']
    //      open[unit=3,file='Output.txt',status='unknown']
    //AssignFile[Funit:=3,'678144Output.txt' ]; Reset[Funit:=3];                                                    //DSR uncomment this
    LConvert1 := (3600*24) / FNoHseHolds;
    LConvert2 := (FRoofArea*FRunCoeff) / 1000.0;
    LDailyDem := (FHseHoldSize*FPerCapitaDem);

    //      initialize - found I  cannot assume they are zero unlike in the
    //      good old days - [does global warming have something to do with this?!]
    for i := 1 to im  do
    begin
      for j := 1 to 12  do
      begin
        for it := 1 to FNoTanks  do
        begin
          FRiverS[i,j,it] := 0;
          FRainS[i,j,it]  := 0;
          FOtherS[i,j,it] := 0;
        end;
      end;
    end;

    //========================================================================
    //DSR start write file headings---------------------------------------------------------------------------------
    FOutputFileHeader.WriteToStringList(FOutputDailyFileData);
    for it := 1 to FNoTanks  do
    begin
      FOutputDailyFileData.Add('');
      FOutputDailyFileData.Add(strHeadingFile1+ FormatFloat('0.000',FTankvol[it]));
      //DSR end write file headings---------------------------------------------------------------------------------
      im := 1;
      j  := 1;
      FStorage[im,FDay[1],it] := FInitStorage*FTankVol[it];
      if((FStorage[im,FDay[1],it] >= (FUpperL*FTankVol[it])) and (FTankVol[it] > 0)) then
      begin
        FDecide := 1;
      end
      else
      begin
        FDecide := 2;
      end;

      Label_700:

      if((FDecide = 1) and (j < FNDays)) then
      begin
        goto Label_400;
      end
      else if((FDecide = 2) and (j < FNDays)) then
      begin
        goto Label_500;
      end else
      begin
        goto Label_1000;
      end;

      Label_400:
      k := FDay[j];
      k2:=FMonth[j];
      if((FYear[j] = FYear[j+1]) and (j<FNDays)) then
      begin
        if((FStorage[im,k,it] + FmagSortRain[im,k] * LConvert2)>= LDailyDem) then
        begin
          FStorage[im,k+1,it]:= FStorage[im,k,it] + FmagSortRain[im,k]*LConvert2-LDailyDem;
          FRainS[im,k2,it]:= FRainS[im,k2,it]+1;
          if(FStorage[im,k+1,it] > FTankVol[it]) then
          begin
            FSpill[im,k,it] := FStorage[im,k+1,it] - FTankVol[it];
            FStorage[im,k+1,it] := FTankVol[it];
          end;
        end
        else
        begin
          FStorage[im,k+1,it] := 0;
          if((FmagAvail[im,k]*LConvert1) >= LDailyDem) then
          begin
            FRiverS[im,k2,it] := FRiverS[im,k2,it] + 1;
          end
          else
          begin
            FOtherS[im,k2,it] := FOtherS[im,k2,it] + 1;
            FDeficit[im,k,it] := LDailyDem;
          end;
        end;

        //WriteOutputDailyLine(it,j,k,FmagAvail[im,k]*LConvert1,FStorage[im,k,it],FDecide,FSpill[im,k,it],FDeficit[im,k,it]);
        WriteOutputDailyLine(it,j,k,FmagRain[j],FStorage[im,k,it],FDecide,FSpill[im,k,it],FDeficit[im,k,it]);
        //Writeln[F3,j,k,FmagAvail[im,k]*LConvert1,FStorage[im,k,it],]FDecide,FSpill[im,k,it],FDeficit[im,k,it] ; //DSR uncomment this
        j := j + 1;
        if((FStorage[im,k+1,it] <= (FLowerL*FTankVol[it])) or (FTankVol[it] = 0.0)) then
        begin
          FDecide := 2;
        end;
        goto Label_700;

      end
      else if(j < FNDays) then
      begin
        if(FStorage[im,k,it]+(FmagSortRain[im,k]*LConvert2) >= LDailyDem) then
        begin
          FStorage[im+1,1,it] := FStorage[im,k,it]+ FmagSortRain[im,k]*LConvert2-LDailyDem;
          FRainS[im,k2,it]    := FRainS[im,k2,it] + 1;
          if(FStorage[im+1,1,it] > FTankVol[it]) then
          begin
            FStorage[im+1,1,it] := FTankVol[it];
          end;
        end
        else
        begin
          FStorage[im+1,1,it] := 0;
          if((FmagAvail[im,k]*LConvert1) >= LDailyDem) then
          begin
            FRiverS[im,k2,it] := FRiverS[im,k2,it] + 1;
          end
          else
          begin
            FOtherS[im,k2,it] := FOtherS[im,k2,it] + 1;
            FDeficit[im,k,it] := LDailyDem                                           ;
          end;//
        end;//

        WriteOutputDailyLine(it,j,k,FmagRain[j],FStorage[im,k,it],FDecide,FSpill[im,k,it],FDeficit[im,k,it]);
        //WriteOutputDailyLine(it,j,k,FmagAvail[im,k]*LConvert1,FStorage[im,k,it],FDecide,FSpill[im,k,it],FDeficit[im,k,it]);
        //Writeln[F3,j,k,FmagAvail[im,k]*LConvert1,FStorage[im,k,it],FDecide,FSpill[im,k,it],FDeficit[im,k,it] ;    //DSR uncomment this
        im := im + 1;
        j := j + 1;
        if((FStorage[im,1,it] <= (FLowerL*FTankVol[it])) or (FTankVol[it] = 0)) then
        begin
          FDecide := 2;
        end;//
        goto Label_700;
      end;//

      Label_500:
      k    := FDay[j];
      k2   :=FMonth[j];
      if((FYear[j] = FYear[j+1]) and (j < FNDays)) then
      begin
      if((FmagAvail[im,k]*LConvert1) >= LDailyDem) then
      begin
        FRiverS[im,k2,it]   := FRiverS[im,k2,it] + 1;
        FStorage[im,k+1,it] := FStorage[im,k,it] + FmagSortRain[im,k]*LConvert2;
        if(FStorage[im,k+1,it] > FTankVol[it]) then
        begin
          FSpill[im,k,it]     := FStorage[im,k+1,it]-FTankVol[it];
          FStorage[im,k+1,it] := FTankVol[it];
        end;
      end
      else
      begin
        FStorage[im,k+1,it] := FStorage[im,k,it]+ (FmagSortRain[im,k]*LConvert2) - (LDailyDem+FmagAvail[im,k]*LConvert1);
        if(FStorage[im,k+1,it] > FTankVol[it]) then
        begin
          FSpill[im,k,it] := FStorage[im,k+1,it]-FTankVol[it];
          FStorage[im,k+1,it] := FTankVol[it];
          if(FTankVol[it] >= (FmagSortRain[im,k]*LConvert2)) then
          begin
            FRainS[im,k2,it] := FRainS[im,k2,it] + 1;
          end
          else
          begin
            FOtherS[im,k2,it] := FOtherS[im,k2,it] + 1;
            FDeficit[im,k,it] := LDailyDem;
          end;//
        end
        else if(FStorage[im,k+1,it] < 0.0) then
        begin
          FOtherS[im,k2,it] := FOtherS[im,k2,it] + 1;
          FDeficit[im,k,it] := LDailyDem;
          FStorage[im,k+1,it] := 0;
        end
        else if(FTankVol[it] >= (FmagSortRain[im,k]*LConvert2)) then
        begin
          FRainS[im,k2,it] := FRainS[im,k2,it] + 1;
        end
        else
        begin
          FOtherS[im,k2,it] := FOtherS[im,k2,it] + 1;
          FDeficit[im,k,it] := LDailyDem;
        end;//
      end;//

      WriteOutputDailyLine(it,j,k,FmagRain[j],FStorage[im,k,it],FDecide,FSpill[im,k,it],FDeficit[im,k,it]);
      //WriteOutputDailyLine(it,j,k,FmagAvail[im,k]*LConvert1,FStorage[im,k,it],FDecide,FSpill[im,k,it],FDeficit[im,k,it]);
      //Writeln[F3,j,k,FmagAvail[im,k]*LConvert1,FStorage[im,k,it],] FDecide,FSpill[im,k,it],FDeficit[im,k,it] ;//DSR uncomment this

      j := j + 1;
      if((FStorage[im,k+1,it] >= (FUpperL*FTankVol[it])) and  (FTankVol[it] > 0)) then
      begin
        FDecide := 1;
        end;//
        goto Label_700;
      end
      else if(j < FNDays) then
      begin
        if((FmagAvail[im,k]*LConvert1) >= LDailyDem) then
        begin
          FRiverS[im,k2,it] := FRiverS[im,k2,it] + 1;
          FStorage[im+1,1,it] := FStorage[im,k,it]+ (FmagSortRain[im,k]*LConvert2);
          if(FStorage[im+1,1,it] > FTankVol[it]) then
          begin
            FSpill[im,k,it] := FStorage[im+1,1,it]-FTankVol[it];
            FStorage[im+1,1,it] := FTankVol[it];
          end;
          end
          else
          begin
            FStorage[im+1,1,it] := FStorage[im,k,it]+ FmagSortRain[im,k]*LConvert2 - LDailyDem+FmagAvail[im,k]*LConvert1;
            if(FStorage[im+1,1,it] > FTankVol[it]) then
            begin
              FSpill[im+1,1,it] := FStorage[im+1,1,it]-FTankVol[it];
              FStorage[im+1,1,it] := FTankVol[it];
              if(FTankVol[it] >= (FmagSortRain[im,k]*LConvert2)) then
              begin
                FRainS[im,k2,it] := FRainS[im,k2,it] + 1;
              end
              else
              begin
                FOtherS[im,k2,it] := FOtherS[im,k2,it] + 1;
                FDeficit[im,k,it] := LDailyDem   ;
              end;
            end
            else if(FStorage[im+1,1,it] < 0.0) then
            begin
              FOtherS[im,k2,it] := FOtherS[im,k2,it] + 1;
              FDeficit[im,k,it] := LDailyDem;
              FStorage[im+1,1,it] := 0;
            end
            else if(FTankVol[it] >= (FmagSortRain[im,k]*LConvert2)) then
            begin
              FRainS[im,k2,it] := FRainS[im,k2,it] + 1;
            end
            else
            begin
              FOtherS[im,k2,it] := FOtherS[im,k2,it] + 1;
              FDeficit[im,k,it] := LDailyDem   ;
            end;//
          end;//

          WriteOutputDailyLine(it,j,k,FmagRain[j],FStorage[im,k,it],FDecide,FSpill[im,k,it],FDeficit[im,k,it]);
          //WriteOutputDailyLine(it,j,k,FmagAvail[im,k]*LConvert1,FStorage[im,k,it],FDecide,FSpill[im,k,it],FDeficit[im,k,it]);
          //writeln[F3,j,k,FmagAvail[im,k]*LConvert1,FStorage[im,k,it],FDecide,FSpill[im,k,it],FDeficit[im,k,it];   //DSR uncomment this

          im := im + 1;
          j := j + 1;
          if((FStorage[im,1,it] >=(FUpperL*FTankVol[it])) and (FTankVol[it] > 0)) then
          begin
            FDecide := 1;
          end;//
          goto Label_700;
        end;//

        Label_1000:
      end;
                                                                                                               //Check fortran code for two end statements.
      // 420   format[2[i6,1x],2[f10.3,1x],i3,1x,2[f10.3,1x))

      //DSR start write file headings---------------------------------------------------------------------------------
      FOutputFileHeader.WriteDaysSupplied(FOutputDailyFileData);
      FOutputDailyFileData.SaveToFile(FOutputDailyFileName);

      //DSR start write file headings---------------------------------------------------------------------------------
      FOutputFileHeader.WriteToStringList(FOutputMonthlyFileData);
      LTempStr := 'Year '+FOutputFileHeader.MonthNamesText;

      //	===================================================================
      for  it := 1 to FNoTanks  do
      begin
        FOutputMonthlyFileData.Add('');
        FOutputMonthlyFileData.Add(strHeadingFile2);
        FOutputMonthlyFileData.Add(LTempStr+' Tank Size = '+ FormatFloat('0.000',FTankvol[it]));
        //DSR end write file headings-----------------------------------------------------------------------------------

        LYear := YearOf(FRunConfig.PeriodStartDate);                                                                 //DSR write only the days supplied as we are not dealing with river supply.
        for  i := 1 to im  do
        begin
          F4Array[0] := i;
          for l := 1 to 12 do
            F4Array[l] := FRainS[i,l,it];
          WriteOutputMonthlyLine(LYear,F4Array);
          LYear := LYear + 1;
        end;

        {LYear := YearOf(FRunConfig.PeriodStartDate);
        for  i := 1 to im  do
        begin
          F4Array[0] := i;
          for l := 1 to 12 do
            F4Array[l] := FOtherS[i,l,it];
          WriteOutputMonthlyLine(LYear,F4Array);
          LYear := LYear + 1;
        end;}

        {//===================================================================                                       //DSR uncomment this
        //AssignFile[Funit:=4,'Output2.txt' ]; Reset[Funit:=4];
        for i := 1 to im  do
        begin
          //writeln[F4, i,[FRiverS[i,j,it],j:=1,12));                                                               //DSR uncomment this
        end;
        //writeln[F4,];                                                                                             //DSR uncomment this

        for i := 1 to im  do
        begin
          //writeln[F4, i,[FRainS[i,j,it],j:=1,12));                                                                //DSR uncomment this
        end;
        //writeln[F4,];                                                                                             //DSR uncomment this

        for i := 1 to im  do
        begin
          //writeln[F4, i,[FOtherS[i,j,it],j:=1,12));                                                               //DSR uncomment this
        end;
        //writeln[F4,];                                                                                             //DSR uncomment this

        for i := 1 to im  do
        begin
          for j := 1 to 12  do
          begin
            FRivRainS[i,j,it] := FRiverS[i,j,it]+FRainS[i,j,it];
          end;
          //writeln[F4, i,[FRivRainS[i,j,it],j:=1,12));                                                             //DSR uncomment this
        end;
        //writeln[F4,];}                                                                                             //DSR uncomment this
      end;

    // 120  format[13[i7,1x))
    //      ===================================================================
    //      rank no of days of supply for reliability analysis
    //      set no of years for reliability analysis as the number of complete years
    FOutputFileHeader.WriteDaysSupplied(FOutputMonthlyFileData);
    FOutputMonthlyFileData.SaveToFile(FOutputMonthlyFileName);
    // 120	format(13(i7,1x))

    //DSR start write file headings---------------------------------------------------------------------------------
    FOutputFileHeader.WriteToStringList(FOutputAnnualFileData);
    FOutputAnnualFileData.Add('');
    FOutputAnnualFileData.Add('Reliability'+FOutputFileHeader.TankSizesText);

    im := ny;
    {Ranking(FRiverS,FRanked,FNYears,FNoTanks,im);
    for it := 1 to FNoTanks  do
    begin
      for i := 1 to im  do
      begin
        FRiverAnRank[i,it] := FRanked[i,it];
        //write[*,*]FRiverAnRank[i,it]
      end;
    end;}

    Ranking(FRainS,FRanked,FNYears,FNoTanks,im);

    for  it := 1 to FNoTanks  do
    begin
      for  i := 1 to im  do
      begin
        FRainAnRank[i,it] := FRanked[i,it];
        //write[*,*]i,FRainAnRank[i,it]
      end;
    end;


    {Ranking(FOtherS,FRanked,FNYears,FNoTanks,im);

    for it := 1 to FNoTanks  do
    begin
      for i := 1 to im  do
      begin
         FOtherAnRank[i,it] := FRanked[i,it];
         //write[*,*]FOtherAnRank[i,it]
      end;
    end;

    Ranking(FRivRainS,FRanked,FNYears,FNoTanks,im);

    for it := 1 to FNoTanks  do
    begin
      for i := 1 to im  do
      begin
        FRivRainAnRank[i,it] := FRanked[i,it];
        //write[*,*]FOtherAnRank[i,it]
      end;
    end;}

    //AssignFile[Funit:=5,'Output3.txt' ]; Reset[Funit:=5];                                                         //DSR uncomment this
     i5 := 1;
    //i5 := Trunc(0.8*(im+1)+1);

    for  i := i5 to im  do
    begin
      F4Array[0] := Trunc(100*i/(im+1));
      for l := 1 to FNoTanks do
          F4Array[l] := FRainAnRank[i,l];
        WriteOutputAnnualLine(FNoTanks,F4Array);
    end;
      //writeln[F5,100*i/[im+1],[FRiverAnRank[i,it],it:=1,FNoTanks],];                                               //DSR uncomment this
      //&                        100*i/[im+1],[FRainAnRank[i,it],it:=1,FNoTanks],;                                   //DSR uncomment this
      //&                        100*i/[im+1],[FOtherAnRank[i,it],it:=1,FNoTanks],;                                  //DSR uncomment this
      //&                        100*i/[im+1],[FRivRainAnRank[i,it],it:=1,FNoTanks];                                 //DSR uncomment this

    FOutputAnnualFileData.Add('');

    FOutputFileHeader.WriteDaysSupplied(FOutputAnnualFileData);
    FOutputAnnualFileData.SaveToFile(FOutputAnnualFileName);
    FOutputFileHeader.WriteBestTankSize(FOutputBestTankFileData);
    FOutputBestTankFileData.SaveToFile(FOutputBestTankFileName);

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;//
//      ---------------------------------------------------------------------

procedure TSolver.Ranking(var ADummyA: TIntegerThreeDimensionArray; var ARanked: TIntegerTwoDimensionArray; ANYears, ANoTanks, Aim: integer);
const OPNAME = 'TSolver.Ranking';
var
  i,j,k,i2,LMax : integer;
  Dummy2: TTwoDimensionIntegerArray;
  Dummy3: TTwoDimensionIntegerArray;
  Irank: TTwoDimensionIntegerArray;
begin
  try
    SetLength(Dummy2,ANYears+2,ANoTanks+1);
    SetLength(Dummy3,ANYears+2,ANoTanks+1);
    SetLength(Irank,ANYears+2,ANoTanks+1);

    try
      for k := 1 to ANoTanks  do
      begin
        for i := 1 to Aim  do
        begin
          Dummy2[i,k] := 0;
        end;
      end;

      for k := 1 to ANoTanks  do
      begin
        for i := 1 to Aim  do
        begin
          for j := 1 to 12  do
          begin
            Dummy2[i,k] := Dummy2[i,k] + ADummyA[i,j,k];
          end;
        end;
      end;

      for  i := 1 to Aim  do
      begin
        for  k := 1 to ANoTanks  do
        begin
          Dummy3[i,k] := Dummy2[i,k];
        end;
      end;

      i2 := 0;                                                                                                      // DSR is this the correct initialization
      for k := 1 to ANoTanks  do
      begin
        for i := 1 to Aim  do
        begin
          LMax:=-100;
          for j := 1 to Aim  do
          begin
            if(Dummy2[j,k] > LMax) then
            begin
              i2  := j;
              LMax := Dummy2[j,k];
           end;
          end;

          Irank[i,k]   := i2;
          Dummy2[i2,k] := - 200;
        end;
      end;

      for k := 1 to ANoTanks  do
      begin
        for i := 1 to Aim  do
        begin
          ARanked[i,k] := Dummy3[Irank[i,k],k];
        end;
      end;
    finally
      Finalize(Dummy2);
      Finalize(Dummy3);
      Finalize(Irank);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;//

procedure TSolver.WriteOutputDailyLine(ATankIndex,ACounter, ADay: integer; ARainfall,AStorage: Double	; ADecide: integer; ASpill, ADeficit: Double);
const OPNAME = 'TSolver.WriteOutputDailyLine';
var
  LLine : string;
begin
  try
    if(ADeficit = 0.0) then
      FOutputFileHeader.FDaysSupplied[ATankIndex] := FOutputFileHeader.FDaysSupplied[ATankIndex]+1;
    FOutputDailyRecord.Reset;
    FOutputDailyRecord.Date      := FRunConfig.PeriodStartDate+(ACounter-1);
    FOutputDailyRecord.Rainfall  := ARainfall;
    FOutputDailyRecord.Storage   := AStorage;
    FOutputDailyRecord.Spill     := ASpill;
    FOutputDailyRecord.Deficit   := ADeficit;
    //FOutputDailyRecord.Decide    := ADecide;
    LLine                        := FOutputDailyRecord.WriteToString;
    FOutputDailyFileData.Add(LLine);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSolver.WriteOutputMonthlyLine(AYear:integer;ADataArray: TMonthyIntegerArray);
const OPNAME = 'TSolver.WriteOutputMonthlyLine';
var
  LLine  : string;
  LIndex : integer;
begin
  try
    LLine :=IntToStr(AYear) + ' ';

    for LIndex := 1 to 12 do
      LLine := LLine + PadLeftString(IntToStr(ADataArray[LIndex]),10);
    FOutputMonthlyFileData.Add(LLine);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSolver.WriteOutputAnnualLine(ACount: integer;ADataArray: TMonthyIntegerArray);
const OPNAME = 'TSolver.WriteOutputAnnualLine';
var
  LLine  : string;
  LIndex : integer;
begin
  try
    LLine := PadLeftString(IntToStr(ADataArray[0]),11);
    for LIndex := 1 to ACount do
      LLine := LLine + PadLeftString(IntToStr(ADataArray[LIndex]),10);
    FOutputAnnualFileData.Add(LLine);
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TSolver.WriteTwoDimensionDoubleArray(AContainer: TStrings; ADataArray: TTwoDimensionDoubleArray);
const OPNAME = 'TSolver.WriteTwoDimensionDoubleArray';
var
 LIndex,
 LIndex2 : integer;
 LLine : string;
begin
  try
    for LIndex := Low(ADataArray)+1 to High(ADataArray) do
    begin
      LLine := PadLeftString(IntToStr(LIndex),4) + ' ';
      for LIndex2 := Low(ADataArray[LIndex])+1 to High(ADataArray[LIndex]) do
      begin
        LLine := LLine + PadLeftString(FormatFloat('#.000',ADataArray[LIndex,LIndex2]),10) + ' ';
      end;
      AContainer.Add(LLine);
    end;
    AContainer.Add('');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSolver.WriteOneDimensionDoubleArray(AContainer: TStrings;ADataArray: TOneDimensionDoubleArray);
const OPNAME = 'TSolver.WriteOneDimensionDoubleArray';
var
 LIndex : integer;
 LLine : string;
begin
  try
    LLine := PadLeftString(IntToStr(1),4) + ' ';
    for LIndex := Low(ADataArray)+1 to High(ADataArray) do
    begin
      LLine := LLine + PadLeftString(FormatFloat('#.000',ADataArray[LIndex]),10) + ' ';
    end;
    AContainer.Add(LLine);
    AContainer.Add('');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
