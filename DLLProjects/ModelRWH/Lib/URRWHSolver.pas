{*******************************************************}
{                                                       }
{  UNIT      : Contains TSolver				                  }
{  AUTHOR    : Dziedzi Ramulondi                        }
{  DATE      : 27/08/2010                               }
{  COPYRIGHT : Copyright © 2010 DRR Data Solutions	    }
{                                                       }
{*******************************************************}

unit URRWHSolver;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
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
  TSolver = class(TAbstractAppObject)
  protected
    NoTanks        : integer;
    NYears         : integer;
    NDays          : integer;
    Decide         : integer;
    HseHoldSize    : integer;
    NoHseHolds     : integer;

    l1             : array[0..2] of integer;
    u1             : array[0..2] of integer;

    Year           : TOneDimensionIntegerArray;
    Month          : TOneDimensionIntegerArray;
    Day            : TOneDimensionIntegerArray;

    Ranked         : TTwoDimensionIntegerArray;
    RiverAnRank    : TTwoDimensionIntegerArray;
    RainAnRank     : TTwoDimensionIntegerArray;
    OtherAnRank    : TTwoDimensionIntegerArray;
    RivRainAnRank  : TTwoDimensionIntegerArray;

    RiverS         : TThreeDimensionIntegerArray;
    RainS          : TThreeDimensionIntegerArray;
    OtherS         : TThreeDimensionIntegerArray;
    RivRainS       : TThreeDimensionIntegerArray;

    RoofArea       : Double;
    RunCoeff       : Double;
    InitStorage    : Double;
    UpperL         : Double;
    LowerL         : Double;
    PerCapitaDem   : Double;

    TankVol        : TOneDimensionDoubleArray;
    EnvFlow        : TOneDimensionDoubleArray;
    magFlow        : TOneDimensionDoubleArray;
    magRain        : TOneDimensionDoubleArray;

    magSortFlow    : TTwoDimensionDoubleArray;
    magRanked      : TTwoDimensionDoubleArray;
    magSortRain    : TTwoDimensionDoubleArray;
    magAvail       : TTwoDimensionDoubleArray;

    Storage        : TThreeDimensionDoubleArray;
    Spill          : TThreeDimensionDoubleArray;
    Deficit        : TThreeDimensionDoubleArray;

    FOutputDailyFileName   : string;
    FOutputMonthlyFileName : string;
    FOutputAnnualFileName  : string;

    FRunConfig     : TRWHRunConfig;
    FStation       : TRainfallStation;

    FOutputDailyFileData    : TStringList;
    FOutputMonthlyFileData  : TStringList;
    FOutputAnnualFileData   : TStringList;

    FOutputDailyRecord      : TOutputDailyRecord;
    FOutputFileHeader       : TOutputFileHeader;
    FRainFallFileAgent      : TRainFallFileAgent;

   procedure PopulateInputData;
   procedure PopulateFileHeader;
   procedure PopulateInputArrays;
   procedure InitialseArraysSize;
   procedure Ranking(var ADummyA : TThreeDimensionIntegerArray;var ARanked : TTwoDimensionIntegerArray; ANYears,ANoTanks,Aim: integer);
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
    FOutputDailyFileData.Clear;
    FOutputMonthlyFileData.Clear;
    FOutputAnnualFileData.Clear;
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

    FRainFallFileAgent.Initialise;
    FRainFallFileAgent.LoadStationDailyData(FStation.StationNumber);
    PopulateInputData;
    PopulateFileHeader;
    InitialseArraysSize;
    PopulateInputArrays;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSolver.Finalise: boolean;
const OPNAME = 'TSolver.Finalise';
begin
  Result := False;
  try
    Finalize(Year);
    Finalize(Month);
    Finalize(Day);
    Finalize(magFlow);

    Finalize(magSortFlow);
    Finalize(magRanked);

    Finalize(EnvFlow);
    Finalize(magRain);
    Finalize(Deficit);
    Finalize(magSortRain);
    Finalize(magAvail);
    Finalize(Storage);
    Finalize(Spill);
    Finalize(RiverS);
    Finalize(RainS);
    Finalize(OtherS);
    Finalize(TankVol);
    Finalize(Ranked);
    Finalize(RiverAnRank);
    Finalize(RainAnRank);
    Finalize(OtherAnRank);
    Finalize(RivRainS);
    Finalize(RivRainAnRank);
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
    if RunModel then
    begin
      Result := True;
      AProgressUpdateFuntion('Completed running Solver for Rainfall station : '+ AStation.StationNumber,ptNone,LStop);
    end
    else
      AProgressUpdateFuntion('Could not complete running Solver for Rainfall station : '+ AStation.StationNumber,ptError,LStop);
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

	  NYears       := FRunConfig.YearsInAnalysis+1;
    NDays        := FRunConfig.DaysInAnalysis;

	  RoofArea     := FRunConfig.RoofArea;
    RunCoeff     := FRunConfig.RoofRunoffCoef;

    HseHoldSize  := FRunConfig.HouseHoldMembers;
    PerCapitaDem := FRunConfig.HouseHoldDemandPP;
    NoHseHolds   := FRunConfig.HouseHoldNumber;

    InitStorage  := FRunConfig.RunStartVolume;
    UpperL       := FRunConfig.RunStartLevel;
    LowerL       := FRunConfig.RunStopLevel;

	  NoTanks     := FRunConfig.TankCount;
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
    if(NoTanks >= 1) then
       FOutputFileHeader.FTankSizes[01] := FRunConfig.TankSize01;
    if(NoTanks >= 2) then
       FOutputFileHeader.FTankSizes[02] := FRunConfig.TankSize02;
    if(NoTanks >= 3) then
       FOutputFileHeader.FTankSizes[03] := FRunConfig.TankSize03;
    if(NoTanks >= 4) then
       FOutputFileHeader.FTankSizes[04] := FRunConfig.TankSize04;
    if(NoTanks >= 5) then
       FOutputFileHeader.FTankSizes[05] := FRunConfig.TankSize05;
    if(NoTanks >= 6) then
       FOutputFileHeader.FTankSizes[06] := FRunConfig.TankSize06;
    if(NoTanks >= 7) then
       FOutputFileHeader.FTankSizes[07] := FRunConfig.TankSize07;
    if(NoTanks >= 8) then
       FOutputFileHeader.FTankSizes[08] := FRunConfig.TankSize08;
    if(NoTanks >= 9) then
       FOutputFileHeader.FTankSizes[09] := FRunConfig.TankSize09;
    if(NoTanks >= 10) then
       FOutputFileHeader.FTankSizes[10] := FRunConfig.TankSize10;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSolver.PopulateInputArrays;
const OPNAME = 'TSolver.PopulateInputArrays';
var
  LIndex : integer;
  LRainfallRecord : TRainfallRecord;
  LStartDate      : TDate;
  LEndDate        : TDate;
begin
  try
    if(NoTanks >= 1) then
       TankVol[01] := FRunConfig.TankSize01;
    if(NoTanks >= 2) then
       TankVol[02] := FRunConfig.TankSize02;
    if(NoTanks >= 3) then
       TankVol[03] := FRunConfig.TankSize03;
    if(NoTanks >= 4) then
       TankVol[04] := FRunConfig.TankSize04;
    if(NoTanks >= 5) then
       TankVol[05] := FRunConfig.TankSize05;
    if(NoTanks >= 6) then
       TankVol[06] := FRunConfig.TankSize06;
    if(NoTanks >= 7) then
       TankVol[07] := FRunConfig.TankSize07;
    if(NoTanks >= 8) then
       TankVol[08] := FRunConfig.TankSize08;
    if(NoTanks >= 9) then
       TankVol[09] := FRunConfig.TankSize09;
    if(NoTanks >= 10) then
       TankVol[10] := FRunConfig.TankSize10;

    LStartDate      := FRunConfig.PeriodStartDate;
    LEndDate        := FRunConfig.PeriodEndDate;
    LIndex          := 0;
    LRainfallRecord := FRainFallFileAgent.First;
    while(LRainfallRecord <> nil)  do
    begin
      if(LRainfallRecord.Date >= LStartDate) and (LRainfallRecord.Date <= LEndDate) then
      begin
        Year[LIndex]    := YearOf(LRainfallRecord.Date);
        Month[LIndex]   := MonthOf(LRainfallRecord.Date);
        Day[LIndex]     := DayOf(LRainfallRecord.Date);
        magFlow[LIndex] := 0.0;
        magRain[LIndex] := 0.0;
        if(LRainfallRecord.Rainfall >= 0.0) then
          magRain[LIndex] := LRainfallRecord.Rainfall;
        LIndex           := LIndex + 1;
      end;
      LRainfallRecord := FRainFallFileAgent.Next;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSolver.InitialseArraysSize;
const OPNAME = 'TSolver.InitialseArraysSize';
begin
  try
    SetLength(Year,NDays+1);
    SetLength(Month,NDays+1);
    SetLength(Day,NDays+1);
    SetLength(magFlow,NDays+1);

    SetLength(magSortFlow,NYears+2,367);
    SetLength(magRanked,NYears+2,367);

    SetLength(EnvFlow,367);
    SetLength(magRain,NDays+1);
    SetLength(Deficit,NYears+2,367,NoTanks+1);
    SetLength(magSortRain,NYears+2,367);
    SetLength(magAvail,NYears+2,367);
    SetLength(Storage,NYears+2,367,NoTanks+1);
    SetLength(Spill,NYears+2,367,NoTanks+1);
    SetLength(RiverS,NYears+2,13,NoTanks+1);
    SetLength(RainS,NYears+2,13,NoTanks+1);
    SetLength(OtherS,NYears+2,13,NoTanks+1);
    SetLength(TankVol,NoTanks+1);
    SetLength(Ranked,NYears+2,NoTanks+1);
    SetLength(RiverAnRank,NYears+2,NoTanks+1);
    SetLength(RainAnRank,NYears+2,NoTanks+1);
    SetLength(OtherAnRank,NYears+2,NoTanks+1);
    SetLength(RivRainS,NYears+2,13,NoTanks+1);
    SetLength(RivRainAnRank,NYears+2,NoTanks+1);

    {ZeroMemory(Year,Length(Year));
    ZeroMemory(Month,Length(Month));
    ZeroMemory(Day,Length(Day));
    ZeroMemory(magFlow,Length(magFlow));

    ZeroMemory(magSortFlow,Length(magSortFlow));
    ZeroMemory(magRanked,Length(magRanked));

    ZeroMemory(EnvFlow,Length(EnvFlow));
    ZeroMemory(magRain,Length(magRain));
    ZeroMemory(Deficit,Length(Deficit));
    ZeroMemory(magSortRain,Length(magSortRain));
    ZeroMemory(magAvail,Length(magAvail));
    ZeroMemory(Storage,Length(Storage));
    ZeroMemory(Spill,Length(Spill));
    ZeroMemory(RiverS,Length(RiverS));
    ZeroMemory(RainS,Length(RainS));
    ZeroMemory(OtherS,Length(OtherS));
    ZeroMemory(TankVol,Length(TankVol));
    ZeroMemory(Ranked,Length(Ranked));
    ZeroMemory(RiverAnRank,Length(RiverAnRank));
    ZeroMemory(RainAnRank,Length(RainAnRank));
    ZeroMemory(OtherAnRank,Length(OtherAnRank));
    ZeroMemory(RivRainS,Length(RivRainS));
    ZeroMemory(RivRainAnRank,Length(RivRainAnRank));}

    {
    SetLength(Year,NDays);
    SetLength(Month,NDays);
    SetLength(Day,NDays);
    SetLength(magFlow,NDays);

    SetLength(magSortFlow,NYears+1,366);
    SetLength(magRanked,NYears+1,366);

    SetLength(EnvFlow,366);
    SetLength(magRain,NDays);
    SetLength(Deficit,NYears+1,366,NoTanks);
    SetLength(magSortRain,NYears+1,366);
    SetLength(magAvail,NYears+1,366);
    SetLength(Storage,NYears+1,366,NoTanks);
    SetLength(Spill,NYears+1,366,NoTanks);
    SetLength(RiverS,NYears+1,12,NoTanks);
    SetLength(RainS,NYears+1,12,NoTanks);
    SetLength(OtherS,NYears+1,12,NoTanks);
    SetLength(TankVol,NoTanks);
    SetLength(Ranked,NYears+1,NoTanks);
    SetLength(RiverAnRank,NYears+1,NoTanks);
    SetLength(RainAnRank,NYears+1,NoTanks);
    SetLength(OtherAnRank,NYears+1,NoTanks);
    SetLength(RivRainS,NYears+1,12,NoTanks);
    SetLength(RivRainAnRank,NYears+1,NoTanks);
    }

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
  i2        : integer;
  j         : integer;
  j2        : integer;
  k         : integer;
  k2        : integer;
  l         : integer;
  NLeapY    : integer;
  Rank      : integer;
  Dummy     : Double	;
  Convert1  : integer	;
  Convert2  : Double	;
  DailyDem  : Double	;
  F4Array   : TMonthyIntegerArray;
  LTempStr  : string;
  Label     Label_20,Label_220,Label_400,Label_500,Label_700,Label_1000;
begin
  Result := False;
  //LContainer := TStringList.Create;
  try
    //     sort data into years and days
    im  := 1;
    i   := 1;
    j   := 1;
    id  := Day[1];
  Label_20:
    k := Day[j];
    magSortFlow[im,k] := magFlow[j];
    magsortRain[im,k] := magRain[j];
    if(i >= NDays) then
    begin
    end
    else  if(Year[i] = Year[i+1]) then
    begin
      i:=i+1;
      j:=j+1;
      goto Label_20;
    end
    else if(j < NDays) then
    begin
      im:=im+1;
      i:=i+1;
      j:=j+1;
      goto Label_20;
    end;

    {//DSR
    WriteTwoDimensionDoubleArray(LContainer,magSortFlow);
    WriteTwoDimensionDoubleArray(LContainer,magsortRain);
    }
    //	rank sorted flow data for determination of low fow
    //	environmental requirement [flow at 90% exceedance]
    l1[1]  := id;
    u1[1]  := 366;
    l1[2]  := 1;
    u1[2]  := id-1;

    for  i2 := 1 to 2  do
    begin
      for  i := l1[i2] to u1[i2]  do
      begin
        for  j2 := 1 to im  do
        begin
          Dummy  := -1;
          for  j := 1 to im  do
          begin
            if(magSortFlow[j,i] > Dummy) then
            begin
              Dummy:=magSortFlow[j,i];
              k:=j;
            end;//
          end;
          magRanked[j2,i] := magSortFlow[k,i];
          magSortFlow[k,i]:= -2;
        end;
      end;
    end;

    {//DSR
    WriteTwoDimensionDoubleArray(LContainer,magRanked);
    }
    //	obtain magnitudes  and take care of
    //	the leap Year extra Day - Day (number 60 - February 29)
    //	count number of leap years
    NLeapY := 0;
    for  i := 0 to im  do
    begin
      if(Trunc(int((Year[1]+i)/4)) = (Year[1]+i)/4.0) then
        NLeapY:=NLeapY + 1;
    end;
    for  i := 1 to 366  do
    begin
      if(i = 366) then
        im2 := NLeapY
      else
        im2 := im;

      //DSR horrors of horrows. when all operands are integers the temporary product is an integer;
      // thus 90*(im2+1)/100 is stored as an integer;
      //rank = int(90*(im2+1)/100 + 0.5)
      Rank  := Trunc(int(int(90*(im2+1)/100)+0.5));
      {//DSR
      //LContainer.Add(FormatFloat('00.000000',Rank));}
      EnvFlow[i] := magRanked[Rank,i];
    end;

    {//DSR
    WriteOneDimensionDoubleArray(LContainer,EnvFlow);
    }
    
    //	reassign flows back to magSortedFlow Matrix
    //     sort data into years and days and obtain magnitude of flow available
    //	for domestic supply by subtracting environmental flow
    im  := 1;
    i   := 1;
    j   := 1;

  Label_220:
    k := Day[j];
    magSortFlow[im,k] := magFlow[j];
    if(magSortFlow[im,k] > EnvFlow[k]) then
      magAvail[im,k] := magSortFlow[im,k] - EnvFlow[k]
    else
      magAvail[im,k] := 0;

    if(i >= NDays) then
    begin
    end
    else if(Year[i] = Year[i+1]) then
    begin
      i := i+1;
      j := j+1;
      goto Label_220;
    end
    else if(j < NDays) then
    begin
      im := im+1;
      i  := i+1;
      j  := j+1;
      goto Label_220;
    end;

    //	behaviour analysis of rainwater storage tank
    //	supplies water if run-of-river supply is inadequate or unavailable
    Convert1 := Trunc(Int((3600 * 24)/NoHseHolds));
    Convert2 := (RoofArea * RunCoeff)/1000;
    DailyDem := HseHoldSize * PerCapitaDem;
    //	initialize - found I  cannot assume they are zero unlike in the
    //	good old days - [does global warming have something to do with this?!]
    for  i := 1 to im  do
    begin
      for  j := 1 to 12  do
      begin
        for  it := 1 to NoTanks  do
        begin
          RiverS[i,j,it] := 0;
          RainS[i,j,it]  := 0;
          OtherS[i,j,it] := 0;
        end;
      end;
    end;

    //	========================================================================
    //DSR start write file headings---------------------------------------------------------------------------------
    FOutputFileHeader.WriteToStringList(FOutputDailyFileData);
    for  it := 1 to NoTanks  do
    begin
      FOutputDailyFileData.Add('');
      FOutputDailyFileData.Add(strHeadingFile1+ FormatFloat('0.000',Tankvol[it]));
      //DSR end write file headings---------------------------------------------------------------------------------
      im := 1;
      j  := 1;
      Storage[im,Day[1],it] := InitStorage*TankVol[it];
      if((Storage[im,Day[1],it] >= UpperL*TankVol[it]) and (Tankvol[it] > 0)) then
        Decide := 1
      else
        Decide := 2;

    Label_700:
      if((Decide = 1) and (j < NDays)) then
        goto Label_400
      else if((Decide = 2) and (j < NDays)) then
        goto Label_500
      else
        Continue;
      //goto Label_1000;

    Label_400:
      k  := Day[j];
      k2 := Month[j];
      if((Year[j] = Year[j+1]) and (j < NDays)) then
      begin
        if((Storage[im,k,it] + magSortRain[im,k] * Convert2) >= dailyDem) then
        begin
          Storage[im,k+1,it] := Storage[im,k,it] + magSortRain[im,k]* Convert2 - dailyDem;
          RainS[im,k2,it]    := RainS[im,k2,it] + 1;
          if(Storage[im,k+1,it] > TankVol[it]) then
          begin
            Spill[im,k,it]     := Storage[im,k+1,it] - TankVol[it];
            Storage[im,k+1,it] := TankVol[it];
          end;//
        end
        else
        begin
          Storage[im,k+1,it] := 0;
          if((magAvail[im,k] * Convert1) >= dailyDem) then
            RiverS[im,k2,it] := RiverS[im,k2,it] + 1
          else
          begin
            OtherS[im,k2,it] := OtherS[im,k2,it] + 1;
            Deficit[im,k,it] := dailyDem;
          end;//
        end;//

        WriteOutputDailyLine(it,j,k,magRain[j],Storage[im,k,it],Decide,Spill[im,k,it],deficit[im,k,it]);
        //WriteOutputDailyLine(j,k,magAvail[im,k]*Convert1,Storage[im,k,it],Decide,Spill[im,k,it],deficit[im,k,it]);

        j := j + 1;
        if((Storage[im,k+1,it] <= (LowerL*TankVol[it])) or (TankVol[it] = 0)) then
          Decide := 2;

        goto Label_700;

      end
      else if(j < NDays) then
      begin
        if((Storage[im,k,it] + magSortRain[im,k] * Convert2) >= dailyDem) then
        begin
          Storage[im+1,1,it] := Storage[im,k,it] + magSortRain[im,k] * Convert2 - dailyDem;
          RainS[im,k2,it]    := RainS[im,k2,it] + 1;
          if (Storage[im+1,1,it] > TankVol[it]) then
            Storage[im+1,1,it] := TankVol[it];
        end
        else
        begin
          Storage[im+1,1,it] := 0;
          if((magAvail[im,k]* Convert1) >= dailyDem) then
            RiverS[im,k2,it] := RiverS[im,k2,it] + 1
          else
          begin
            OtherS[im,k2,it] := OtherS[im,k2,it] + 1;
            Deficit[im,k,it] := dailyDem;
          end;//
        end;//

        WriteOutputDailyLine(it,j,k,magRain[j],Storage[im,k,it],Decide,Spill[im,k,it],deficit[im,k,it]);
        //WriteOutputDailyLine(j,k,magAvail[im,k]*Convert1,Storage[im,k,it],Decide,Spill[im,k,it],deficit[im,k,it]);

        im := im + 1;
        j  := j + 1;

        if((Storage[im,1,it] <= (LowerL*TankVol[it])) or (TankVol[it] = 0)) then
          Decide := 2;
        goto Label_700;
      end;//

    Label_500:
      k    := Day[j];
      k2   :=Month[j];
      if((Year[j] = Year[j+1]) and (j < NDays)) then
      begin
        if((magAvail[im,k] * Convert1) >= dailyDem) then
        begin
          RiverS[im,k2,it]   := RiverS[im,k2,it] + 1;
          Storage[im,k+1,it] := Storage[im,k,it] + magSortRain[im,k] * Convert2;
          if(Storage[im,k+1,it] > TankVol[it]) then
          begin
            Spill[im,k,it]     := Storage[im,k+1,it]-TankVol[it];
            Storage[im,k+1,it] := TankVol[it];
          end;//
        end
        else
        begin
          Storage[im,k+1,it] := Storage[im,k,it] + magSortRain[im,k] * Convert2 - dailyDem+magAvail[im,k] * Convert1;
          if(Storage[im,k+1,it] > TankVol[it]) then
          begin
            Spill[im,k,it] := Storage[im,k+1,it]-TankVol[it];
            Storage[im,k+1,it] := TankVol[it];
            if(TankVol[it] >= magSortRain[im,k] * Convert2) then
              RainS[im,k2,it] := RainS[im,k2,it] + 1
            else
            begin
              OtherS[im,k2,it] := OtherS[im,k2,it] + 1;
              Deficit[im,k,it] := dailyDem;
            end;//
          end
          else
          if(Storage[im,k+1,it] < 0) then
          begin
            OtherS[im,k2,it] := OtherS[im,k2,it] + 1;
            Deficit[im,k,it] := dailyDem;
            Storage[im,k+1,it] := 0;
          end
          else if(TankVol[it] >= magSortRain[im,k] * Convert2) then
          begin
            RainS[im,k2,it] := RainS[im,k2,it] + 1;
          end
          else
          begin
            OtherS[im,k2,it] := OtherS[im,k2,it] + 1;
            Deficit[im,k,it] := dailyDem;
          end;//
        end;//
        //DSR 1
        WriteOutputDailyLine(it,j,k,magRain[j],Storage[im,k,it],Decide,Spill[im,k,it],deficit[im,k,it]);
        //WriteOutputDailyLine(j,k,magAvail[im,k]*Convert1,Storage[im,k,it],Decide,Spill[im,k,it],deficit[im,k,it]);

        j := j + 1;

        if((Storage[im,k+1,it] >= (UpperL * TankVol[it])) and (TankVol[it] > 0)) then
          Decide := 1;
        goto Label_700;
      end
      else if( j < NDays) then
      begin
        if((magAvail[im,k]* Convert1) >= dailyDem) then
        begin
          RiverS[im,k2,it]    := RiverS[im,k2,it] + 1 	;
          Storage[im+1,1,it]  := Storage[im,k,it]+ magSortRain[im,k] * Convert2;
          if(Storage[im+1,1,it] > TankVol[it]) then
          begin
            Spill[im,k,it]     := Storage[im+1,1,it] - TankVol[it];
            Storage[im+1,1,it] := TankVol[it];
          end;//
        end
        else
        begin
          Storage[im+1,1,it]   := Storage[im,k,it] + magSortRain[im,k] * Convert2 - dailyDem+magAvail[im,k] * Convert1;
          if(Storage[im+1,1,it] > TankVol[it]) then
          begin
            Spill[im+1,1,it]   := Storage[im+1,1,it] - TankVol[it];
            Storage[im+1,1,it] := TankVol[it];
            if(TankVol[it] >= (magSortRain[im,k] * Convert2)) then
              RainS[im,k2,it] := RainS[im,k2,it] + 1
            else
            begin
              OtherS[im,k2,it] := OtherS[im,k2,it] + 1;
              Deficit[im,k,it] := dailyDem	;
            end;//
          end
          else if(Storage[im+1,1,it]<0) then
          begin
            OtherS[im,k2,it] := OtherS[im,k2,it] + 1;
            Deficit[im,k,it] := dailyDem;
            Storage[im+1,1,it] := 0;
          end
          else if(TankVol[it] >= (magSortRain[im,k] * Convert2)) then
            RainS[im,k2,it] := RainS[im,k2,it] + 1
          else
          begin
            OtherS[im,k2,it] := OtherS[im,k2,it] + 1;
            Deficit[im,k,it] := dailyDem	;
          end;//
        end;//
        //DSR2
        WriteOutputDailyLine(it,j,k,magRain[j],Storage[im,k,it],Decide,Spill[im,k,it],deficit[im,k,it]);
        //WriteOutputDailyLine(j,k,magAvail[im,k]*Convert1,Storage[im,k,it],Decide,Spill[im,k,it],deficit[im,k,it]);

        im := im + 1;
        j := j + 1;

        if((Storage[im,1,it] >= (UpperL*TankVol[it])) and (TankVol[it] > 0)) then
        begin
          Decide := 1;
        end;
        goto Label_700;
      end;//
    end;

    Label_1000:
    //DSR start write file headings---------------------------------------------------------------------------------
    FOutputFileHeader.WriteDaysSupplied(FOutputDailyFileData);
    FOutputDailyFileData.SaveToFile(FOutputDailyFileName);

    //DSR start write file headings---------------------------------------------------------------------------------
    FOutputFileHeader.WriteToStringList(FOutputMonthlyFileData);
    LTempStr := 'Year '+FOutputFileHeader.MonthNamesText;

    //	===================================================================
    for  it := 1 to NoTanks  do
    begin
      FOutputMonthlyFileData.Add('');
      FOutputMonthlyFileData.Add(strHeadingFile2);
      FOutputMonthlyFileData.Add(LTempStr+' Tank Size = '+ FormatFloat('0.000',Tankvol[it]));
      //DSR end write file headings-----------------------------------------------------------------------------------
      {for  i := 1 to im  do
      begin
        F4Array[0] := i;
        for l := 1 to 12 do
          F4Array[l] := RiverS[i,l,it];
        WriteOutputMonthlyLine(F4Array);
      end;
      FOutputMonthlyFileData.Add('');}

      LYear := YearOf(FRunConfig.PeriodStartDate);
      for  i := 1 to im  do
      begin
        F4Array[0] := i;
        for l := 1 to 12 do
          F4Array[l] := RainS[i,l,it];
        WriteOutputMonthlyLine(LYear,F4Array);
        LYear := LYear + 1;
      end;
      //FOutputMonthlyFileData.Add('');

      {for  i := 1 to im  do
      begin
        F4Array[0] := i;
        for l := 1 to 12 do
          F4Array[l] := OtherS[i,l,it];
        WriteOutputMonthlyLine(F4Array);
      end;
      FOutputMonthlyFileData.Add('');

      for  i := 1 to im  do
      begin
        for  l := 1 to 12  do
        begin
          RivRainS[i,l,it]:=RiverS[i,l,it]+RainS[i,l,it];
        end;
        F4Array[0] := i;
        for l := 1 to 12 do
          F4Array[l] := RivRainS[i,l,it];
        WriteOutputMonthlyLine(F4Array);
      end;
      FOutputMonthlyFileData.Add('');}
    end;
    FOutputFileHeader.WriteDaysSupplied(FOutputMonthlyFileData);
    FOutputMonthlyFileData.SaveToFile(FOutputMonthlyFileName);
    // 120	format(13(i7,1x))

    //DSR start write file headings---------------------------------------------------------------------------------
    FOutputFileHeader.WriteToStringList(FOutputAnnualFileData);
    FOutputAnnualFileData.Add('');
    FOutputAnnualFileData.Add('Reliability'+FOutputFileHeader.TankSizesText);

    //	rank no of days of supply for reliability analysis
    {Ranking(RiverS,Ranked,NYears,NoTanks,im);
    for  it := 1 to NoTanks  do
    begin
      for  i := 1 to im  do
      begin
        RiverAnRank[i,it] := Ranked[i,it];
      end;
    end;}

    Ranking(RainS,Ranked,NYears,NoTanks,im);
    for  it := 1 to NoTanks  do
    begin
      for  i := 1 to im  do
      begin
        RainAnRank[i,it] := Ranked[i,it];
      end;
    end;

    {Ranking(OtherS,Ranked,NYears,NoTanks,im);
    for  it := 1 to NoTanks  do
    begin
      for  i := 1 to im  do
      begin
        OtherAnRank[i,it] := Ranked[i,it];
      end;
    end;

    Ranking(RivRainS,Ranked,NYears,NoTanks,im);
    for  it := 1 to NoTanks  do
    begin
      for  i := 1 to im  do
      begin
        RivRainAnRank[i,it] := Ranked[i,it];
      end;
    end;}

    i5 := 1;
    //i5:=Trunc(0.8*(im+1)+1);

    {for  i := i5 to im  do
    begin
      F4Array[0] := Trunc(100*i/(im+1));
      for l := 1 to NoTanks do
          F4Array[l] := RiverAnRank[i,l];
        WriteOutputAnnualLine(NoTanks,F4Array);
    end;
    FOutputAnnualFileData.Add('');}

    for  i := i5 to im  do
    begin
      F4Array[0] := Trunc(100*i/(im+1));
      for l := 1 to NoTanks do
          F4Array[l] := RainAnRank[i,l];
        WriteOutputAnnualLine(NoTanks,F4Array);
    end;
    FOutputAnnualFileData.Add('');

    {for  i := i5 to im  do
    begin
      F4Array[0] := Trunc(100*i/(im+1));
      for l := 1 to NoTanks do
          F4Array[l] := OtherAnRank[i,l];
        WriteOutputAnnualLine(NoTanks,F4Array);
    end;
    FOutputAnnualFileData.Add('');

    for  i := i5 to im  do
    begin
      F4Array[0] := Trunc(100*i/(im+1));
      for l := 1 to NoTanks do
          F4Array[l] := RivRainAnRank[i,l];
        WriteOutputAnnualLine(NoTanks,F4Array);
    end;}

    FOutputFileHeader.WriteDaysSupplied(FOutputAnnualFileData);
    FOutputAnnualFileData.SaveToFile(FOutputAnnualFileName);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
  {LContainer.SaveToFile('R:\RRWH\Deployment\Data\DelphiDebug.txt');
  LContainer.Free;
  }
end;

procedure TSolver.Ranking(var ADummyA: TThreeDimensionIntegerArray; var ARanked: TTwoDimensionIntegerArray; ANYears, ANoTanks, Aim: integer);
const OPNAME = 'TSolver.Ranking';
var
  LMax  : integer;
  i,i2,j,k : integer;
  Dummy2: TTwoDimensionIntegerArray;
  Dummy3: TTwoDimensionIntegerArray;
  Irank : TTwoDimensionIntegerArray;
begin
  try
    SetLength(Dummy2,ANYears+2,ANoTanks+1);
    SetLength(Dummy3,ANYears+2,ANoTanks+1);
    SetLength(Irank,ANYears+2,ANoTanks+1);
    {ZeroMemory(Dummy2, Length(Dummy2));
    ZeroMemory(Dummy3, Length(Dummy3));
    ZeroMemory(Irank, Length(Irank));}
    try
      for  k := 1 to ANoTanks  do
      begin
        for  i := 1 to AIm  do
        begin
          Dummy2[i,k] := 0;
        end;
      end;

      for  k := 1 to ANoTanks  do
      begin
        for  i := 1 to AIm  do
        begin
          for  j := 1 to 12  do
          begin
            Dummy2[i,k]:=Dummy2[i,k] + ADummyA[i,j,k];
          end;
        end;
      end;

      for  i := 1 to AIm  do
      begin
        for  k := 1 to ANoTanks  do
        begin
          Dummy3[i,k]:=Dummy2[i,k];
        end;
      end;

      for  k := 1 to ANoTanks  do
      begin
        for  i := 1 to AIm  do
        begin
          LMax := -100;
          i2   := 1;//DSR What is the default value for i2. I was getting a warning that it may not be initialised.
          for  j := 1 to AIm  do
          begin
            if(Dummy2[j,k] > LMax) then
            begin
              i2:=j;
              LMax := Dummy2[j,k];
            end;
          end;
          Irank[i,k]:=i2;
          Dummy2[i2,k]:= -200;
        end;
      end;

      for  k := 1 to ANoTanks  do
      begin
        for  i := 1 to AIm  do
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
end;

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
