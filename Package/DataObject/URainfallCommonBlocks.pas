//
//  Contains : Class TRainfallCommonBlocks.
//
unit URainfallCommonBlocks;


interface


//
// Dimension declarations.
// IMPLICIT REAL*8(A-H,O-Z)
//
const
  NYEAR      =   200; // Maximum number of years.
  NMNTH      =    12; // Number of months.
  NSTAT      =    20; // Maximum number of gauges.

type
  Real8 = double;
  PReal8 = ^Real8;


//
// Common block and local block numbers.
//
type
  TGlobalCommonBlock = (
    //
    // Common blocks from Common.fcb.
    //
    bnRCOM01,     //  0
    bnRCOM02,     //  1
    bnRCOM03,     //  2
    bnRCOM04,     //  3
    bnRCOM05      //  4
  );


//
// Common block record definitions.
//
type
  TFCB_RCOM01 = record
    ISHFT     : Integer;     // Shift data about mean 0 = No, 1 = Yes.
    IREFY     : Integer;     // Reference year = 1899
    JBEGY     : Integer;     // ClassR Begin year (initialise = 2100);
    JENDY     : Integer;     // ClassR End year (initialise = 1900);
    NGAG      : Integer;     // Number of gauges in patch.
    NOPT      : Integer;     // 0 = Determine percentage intact data. 1 = Run PatchR
    NROP      : Integer;     // 1 = Do Rough Patch (only applicable if NOPT = 1)
    NSVM      : Integer;     // 1 = Do Stations vs Months(only applicable if NOPT = 1)
    NSVY      : Integer;     // 1 = Do Stations vs Years(only applicable if NOPT = 1)
    NCZN      : Integer;     // Number of seasons.
    IBEGY     : Integer;     // PatchR Begin year (initialise = 2100);
    IENDY     : Integer;     // PatchR End year (initialise = 1900);
    ISIT      : Integer;     // PatchR Run Option
  end;
  PTFCB_RCOM01 = ^TFCB_RCOM01;

  TFCB_RCOM02 = record
    NGAUGE    : array[1..NSTAT,1..40] of Char; // Contains the names of the *.RAW files, one for each gauge.
    LABELN   : array[1..NSTAT,1..8] of Char;  // Contains the names of the gauges.
  end;
  PTFCB_RCOM02 = ^TFCB_RCOM02;

  TFCB_RCOM03 = record
    IDATA     : array[1..NMNTH,1..NSTAT,1..NYEAR] of Integer; // Contains the monthly rainfall data for the gauges.
  end;
  PTFCB_RCOM03 = ^TFCB_RCOM03;

  TFCB_RCOM04 = record
    JCODE     : array[1..NMNTH,1..NSTAT,1..NYEAR] of Char; // Contains the monthly rainfall flags for the gauges.
  end;
  PTFCB_RCOM04 = ^TFCB_RCOM04;

  TFCB_RCOM05 = record
    MCZN      : array[1..NMNTH] of Integer;          // Length of each season.
    LCZN      : array[1..NMNTH,1..NMNTH] of Integer; // Month numbers of each season.
  end;
  PTFCB_RCOM05 = ^TFCB_RCOM05;

//
// Collection of all common blocks.
//
  TRainfallCommonBlocks = class(TObject)
  public
    // Global common blocks.
    RCOM01     : PTFCB_RCOM01;
    RCOM02     : PTFCB_RCOM02;
    RCOM03     : PTFCB_RCOM03;
    RCOM04     : PTFCB_RCOM04;
    RCOM05     : PTFCB_RCOM05;

    // Construction.
    constructor Create;
    procedure Reset;
    procedure Clear;

    // Introduced in this class.
    procedure SetBlockAddress(ABlockNumber: integer; AAddress: pointer);
  end;


//
// This is the global variable that makes the common block available to
// all agents in the application.
//
var
  GFCB: TRainfallCommonBlocks;


implementation


//
// Implementation dependencies
//
uses

  // Delphi
  SysUtils,
  Classes,

  // Alborak
  UErrorHandlingOperations;


constructor TRainfallCommonBlocks.Create;
const OPNAME = 'TRainfallCommonBlocks.Create';
begin
  try
    inherited Create;
    Reset;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallCommonBlocks.Reset;
const OPNAME = 'TRainfallCommonBlocks.Reset';
begin
  try
    // Global common blocks.
    RCOM01     := nil;
    RCOM02     := nil;
    RCOM03     := nil;
    RCOM04     := nil;
    RCOM05     := nil;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallCommonBlocks.Clear;
const OPNAME = 'TRainfallCommonBlocks.Clear';
var
  LIndexS  : Integer;
  LIndexM  : Integer;
  LIndexY  : Integer;
begin
  try
    RCOM01.ISHFT := 0;
    RCOM01.IREFY := 0;
    RCOM01.JBEGY := 0;
    RCOM01.JENDY := 0;
    RCOM01.NOPT  := 0;
    RCOM01.NROP  := 0;
    RCOM01.NSVM  := 0;
    RCOM01.NSVY  := 0;
    RCOM01.NCZN  := 0;
    RCOM01.IBEGY := 0;
    RCOM01.IENDY := 0;
    RCOM01.ISIT  := 0;

    for LIndexS := 1 to NSTAT do
    begin
      RCOM02.NGAUGE[LIndexS] := '                                        ';
      RCOM02.LABELN[LIndexS] := '        ';
      for LIndexY := 1 to NYEAR do
      begin
        for LIndexM := 1 to NMNTH do
        begin
          RCOM03.IDATA[LIndexM, LIndexS, LIndexY] := 0;
          RCOM04.JCODE[LIndexM, LIndexS, LIndexY] := '@';
        end;
      end;
    end;

    for LIndexS := 1 to NMNTH do
    begin
      RCOM05.MCZN[LIndexS] := 0;
      for LIndexM := 1 to NMNTH do
        RCOM05.LCZN[LIndexS, LIndexM] := 0;
    end;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

//
// Sets the required block number.
//
procedure TRainfallCommonBlocks.SetBlockAddress(ABlockNumber: integer; AAddress: pointer);
const OPNAME = 'TRainfallCommonBlocks.SetBlockAddress';
begin
  try
    // Branch on common block number.
    case TGlobalCommonBlock(ABlockNumber) of
      bnRCOM01      : RCOM01      := PTFCB_RCOM01(AAddress);
      bnRCOM02      : RCOM02      := PTFCB_RCOM02(AAddress);
      bnRCOM03      : RCOM03      := PTFCB_RCOM03(AAddress);
      bnRCOM04      : RCOM04      := PTFCB_RCOM04(AAddress);
      bnRCOM05      : RCOM05      := PTFCB_RCOM05(AAddress);

    else
      raise Exception.CreateFmt('Unknown FORTRAN global common block number [%d]. ', [ABlockNumber]);
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.
