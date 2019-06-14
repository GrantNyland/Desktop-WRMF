//
//
//  UNIT      : Contains TWRPMOutputSettings Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/03/02
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UWRPMOutputSettings;

interface

uses
  Classes,
  UAbstractObject,
  VoaimsCom_TLB;
const
  Exceedence: array[0..14] of double = (0.00,0.005,0.01,0.02,0.05,0.10,0.25,0.50,0.75,0.90,0.95,0.98,0.99,0.995,1.00);
  LRExceedence: array[1..13] of double = (0.00,0.005,0.01,0.10,0.05,0.25,0.50,0.75,0.95,0.90,0.99,0.995,1.00);
  LChannelExceedence: array[1..13] of double = (0.00,0.005,0.01,0.02,0.05,0.25,0.50,0.75,0.95,0.98,0.99,0.995,1.00);
  Pecentile13: array[1..13] of double = (0.0,0.5,1.0,2.0,5.0,25.0,50.0,75.0,95.0,98.0,99.0,99.5,100.0);
  //Pecentile13: array[1..13] of double = (0.0,0.5,1.0,2.0,5.0,25.0,50.0,75.0,95.0,98.0,99.0,99.5,100.0); //for testing against old charts

  DaysPerMonth: array[1..12] of double = (31.0,28.25,31.0,30.0,31.0,30.0,31.0,31.0,30.0,31.0,30.0,31.0);



type
  TWRPMPltFileHeaderData = class(TObject)
  protected
    FMonthsCount             : integer;
    FSequencesCount          : integer;
    FReservoirsCount         : integer;
    FSubSystemsCount         : integer;
    FDemandChannelsCount     : integer;
    FSequencesList           : TStringList;
    FSubSystemsList          : TStringList;
    FReservoirsList          : TStringList;
    FDemandChannelsList      : TStringList;
    FFixedBlock              : TStringList;
    FFileName                : string;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function Initialise: boolean; 
    property FileName                : string      read FFileName            write FFileName       ;
    property MonthsCount             : integer     read FMonthsCount         write FMonthsCount    ;
    property SequencesCount          : integer     read FSequencesCount      write FSequencesCount ;
    property ReservoirsCount         : integer     read FReservoirsCount     write FReservoirsCount;
    property SubSystemsCount         : integer     read FSubSystemsCount     write FSubSystemsCount;
    property DemandChannelsCount     : integer     read FDemandChannelsCount write FDemandChannelsCount;
    property SequencesList           : TStringList read FSequencesList      ;
    property SubSystemsList          : TStringList read FSubSystemsList     ;
    property ReservoirsList          : TStringList read FReservoirsList     ;
    property DemandChannelsList      : TStringList read FDemandChannelsList ;
    property FixedBlock              : TStringList read FFixedBlock ;
  end;

  TWRPMPmpFileHeaderData = class(TObject)
  protected
    FMonthsCount             : integer;
    FSequencesCount          : integer;
    FPumpingChannelsCount    : integer;
    FSequencesList           : TStringList;
    FPumpingChannelsList      : TStringList;
    FFileName                : string;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function Initialise: boolean; 
    property FileName                : string      read FFileName            write FFileName       ;
    property MonthsCount             : integer     read FMonthsCount         write FMonthsCount    ;
    property SequencesCount          : integer     read FSequencesCount      write FSequencesCount ;
    property PumpingChannelsCount    : integer     read FPumpingChannelsCount write FPumpingChannelsCount;
    property SequencesList           : TStringList read FSequencesList      ;
    property PumpingChannelsList      : TStringList read FPumpingChannelsList ;
  end;

  TWRPMOutputSettings = class(TAbstractAppObject)
  protected
    FStudyName      : string;
    FModelName      : string;
    FSubAreaName    : string;
    FScenarioName   : string;

    FStartMonth     : integer;
    FStartYear      : integer;
    FEndMonth     : integer;
    FEndYear      : integer;
    FSequencesCount : integer;
    FYearsCount     : integer;

    FPlotFileName   : string;
    FPumpFileName   : string;
    FElementName    : string;
    FElementID      : integer;
    FElementType    : TNetworkElementType;
    FDataType       : TOutputDataType;
    FOutputTimeStep : TOutputTimeStep;

    procedure BeforeDestroyMemberObjects; override;
    procedure LoadDataFromIniFile;
    procedure SaveDataToIniFile;
  public
    function Initialise: boolean; override;
    property StudyName      : string  read    FStudyName      write     FStudyName;
    property ModelName      : string  read    FModelName      write     FModelName;
    property SubAreaName    : string  read    FSubAreaName    write     FSubAreaName;
    property ScenarioName   : string  read    FScenarioName   write     FScenarioName;

    property StartMonth     : integer read    FStartMonth     write     FStartMonth;
    property StartYear      : integer read    FStartYear      write     FStartYear;
    property EndMonth       : integer read    FEndMonth       write     FEndMonth;
    property EndYear        : integer read    FEndYear        write     FEndYear;
    property SequencesCount : integer read    FSequencesCount write     FSequencesCount;
    property YearsCount     : integer read    FYearsCount     write     FYearsCount;

    property PlotFileName   : string  read    FPlotFileName   write     FPlotFileName;
    property PumpFileName   : string  read    FPumpFileName   write     FPumpFileName;
    property ElementName    : string  read    FElementName    write     FElementName;
    property ElementID      : integer read    FElementID      write     FElementID;

    property ElementType    : TNetworkElementType read    FElementType     write     FElementType;
    property DataType       : TOutputDataType     read    FDataType        write     FDataType;
    property OutputTimeStep : TOutputTimeStep     read    FOutputTimeStep  write     FOutputTimeStep;
  end;

implementation

uses
  UConstants,
  SysUtils,
  UErrorHandlingOperations;

procedure TWRPMOutputSettings.BeforeDestroyMemberObjects;
const OPNAME = 'TWRPMOutputSettings.BeforeDestroyMemberObjects';
begin
  try
    SaveDataToIniFile;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRPMOutputSettings.Initialise: boolean;
const OPNAME = 'TWRPMOutputSettings.Initialise';
begin
  Result := False;
  try
    LoadDataFromIniFile;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRPMOutputSettings.LoadDataFromIniFile;
const OPNAME = 'TWRPMOutputSettings.LoadDataFromIniFile';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRPMOutputSettings.SaveDataToIniFile;
const OPNAME = 'TWRPMOutputSettings.SaveDataToIniFile';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;


{ TWRPMPltFileHeaderData }

procedure TWRPMPltFileHeaderData.AfterConstruction;
const OPNAME = 'TWRPMPltFileHeaderData.AfterConstruction';
begin
  inherited;
  try
    FSequencesList           := TStringList.Create;
    FSubSystemsList          := TStringList.Create;
    FReservoirsList          := TStringList.Create;
    FDemandChannelsList      := TStringList.Create;
    FFixedBlock              := TStringList.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRPMPltFileHeaderData.BeforeDestruction;
const OPNAME = 'TWRPMPltFileHeaderData.BeforeDestruction';
begin
  inherited;
  try
    FreeAndNil(FSequencesList);
    FreeAndNil(FSubSystemsList);
    FreeAndNil(FReservoirsList);
    FreeAndNil(FDemandChannelsList);
    FreeAndNil(FFixedBlock);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRPMPltFileHeaderData.Initialise: boolean;
const OPNAME = 'TWRPMPltFileHeaderData.Initialise';
begin
  Result  :=False;
  try
    FMonthsCount             := NullInteger;
    FSequencesCount          := NullInteger;
    FReservoirsCount         := NullInteger;
    FSubSystemsCount         := NullInteger;
    FDemandChannelsCount     := NullInteger;
    FFileName                := '';
    FSequencesList.Clear;
    FSubSystemsList.Clear;
    FReservoirsList.Clear;
    FDemandChannelsList.Clear;
    FFixedBlock.Clear;
    Result  := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TWRPMPmpFileHeaderData }

procedure TWRPMPmpFileHeaderData.AfterConstruction;
const OPNAME = 'TWRPMPmpFileHeaderData.AfterConstruction';
begin
  inherited;
  try
    FSequencesList           := TStringList.Create;
    FPumpingChannelsList     := TStringList.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRPMPmpFileHeaderData.BeforeDestruction;
const OPNAME = 'TWRPMPltFileHeaderData.BeforeDestruction';
begin
  inherited;
  try
    FreeAndNil(FSequencesList);
    FreeAndNil(FPumpingChannelsList);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRPMPmpFileHeaderData.Initialise: boolean;
const OPNAME = 'TWRPMPltFileHeaderData.Initialise';
begin
  Result  :=False;
  try
    FMonthsCount             := NullInteger;
    FSequencesCount          := NullInteger;
    FPumpingChannelsCount     := NullInteger;
    FFileName                := '';
    FSequencesList.Clear;
    FPumpingChannelsList.Clear;
    Result  := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
