//
//
//  UNIT      : Contains TReservoirAndFilesImplementationObject Class
//  AUTHOR    : Dziedzi Ramulondi(Aravia)
//  DATE      : 08/03/2006
//  COPYRIGHT : Copyright © 2006 DWAF
//
//
unit UReservoirImplementationFileDataObjects;

interface

uses
  Classes, sysutils,contnrs,

  //  DWAF VCL
  UBasicObjects,
  UAbstractDataObject,
  UFileNames,
  UDataFileObjects,
  UAbstractObject;

type

  TReservoirImplementationObject = class(TAbstractDataObject)
  protected
    // Line 2
    FReservoirNumber         : TInteger;
    FReplacedReservoirNumber : TInteger;
    FYearDamActive           : TInteger;
    FMonthDamActive          : TInteger;
    FYearDamAbsolete         : TInteger;
    FMonthDamAbsolete        : TInteger;
    FEconomicLifeOfDam       : TInteger;
    FCapitalCost             : TDouble;
    FMaintenanceCost         : TDouble;

    // Line 3
    FYearsInConstruction     : TInteger;
    FCostSchedule            : TString;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure Reset;override;
    function Initialise: boolean;override;
    property ReservoirNumber         : TInteger  read FReservoirNumber;
    property ReplacedReservoirNumber : TInteger  read FReplacedReservoirNumber;
    property YearDamActive           : TInteger  read FYearDamActive;
    property MonthDamActive          : TInteger  read FMonthDamActive ;
    property YearDamAbsolete         : TInteger  read FYearDamAbsolete;
    property MonthDamAbsolete        : TInteger  read FMonthDamAbsolete;
    property EconomicLifeOfDam       : TInteger  read FEconomicLifeOfDam;
    property CapitalCost             : TDouble   read FCapitalCost;
    property MaintenanceCost         : TDouble   read FMaintenanceCost;

    // Line 3
    property YearsInConstruction     : TInteger  read FYearsInConstruction;
    property CostSchedule            : TString   read FCostSchedule;
  end;

  TShortTermFamilyGroupObject = class(TAbstractDataObject)
  protected
    // Line 5a
    FYearFileActive           : TInteger;
    FMonthFileActive          : TInteger;
    // Line 5b
    FShortTermFamilyFileObjectList   : TObjectList;
    procedure InitialiseFileName(AFileName : TString);
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetShortTermFamilyFileObjectByIndex(AIndex: integer):TString;
  public
    procedure Reset;override;
    function Initialise: boolean;override;
    function CreateShortTermFamilyFileObject : TString;
    function ShortTermFamilyFileObjectCount  : integer;
    property YearFileActive   : TInteger  read FYearFileActive;
    property MonthFileActive  : TInteger  read FMonthFileActive;
    property ShortTermFamilyFileObjectByIndex[AIndex: integer] : TString  read GetShortTermFamilyFileObjectByIndex;
  end;

  // Line 7
  TChannelSwitchFileObject = class(TAbstractDataObject)
  protected
    FYearFileActive           : TInteger;
    FMonthFileActive          : TInteger;
    FFileName                 : TString;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure Reset;override;
    function Initialise: boolean;override;
    property YearFileActive   : TInteger  read FYearFileActive;
    property MonthFileActive  : TInteger  read FMonthFileActive;
    property FileName         : TString   read FFileName;
  end;

  // Line 9
  THydroPowerAllocFileObject = class(TAbstractDataObject)
  protected
    FYearFileActive           : TInteger;
    FFileName                 : TString;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure Reset;override;
    function Initialise: boolean;override;
    property YearFileActive : TInteger  read FYearFileActive;
    property FileName       : TString   read FFileName;
  end;

  TReservoirAndFilesImplementationObject = class(TAbstractDataObject)
  protected
    FAllocationControlChannelFileName    : TString;
    FReservoirImplementationObjectList   : TObjectList;
    FShortTermFamilyGroupObjectList      : TObjectList;
    FChannelSwitchFileObjectList         : TObjectList;
    FHydroPowerAllocFileObjectList       : TObjectList;
    //Line 10... : Extra useless lines
    FFMExtraLines                        : TStringList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetReservoirImplementationObjectByIndex(AIndex: integer):TReservoirImplementationObject;
    function GetShortTermFamilyGroupObjectByIndex(AIndex: integer):TShortTermFamilyGroupObject;
    function GetChannelSwitchFileObject(AIndex: integer):TChannelSwitchFileObject;
    function GetHydroPowerAllocFileObjectByIndex(AIndex: integer):THydroPowerAllocFileObject;
  public
    function Initialise: boolean;override;
    procedure Reset;override;
    function AddReservoirImplementationObject:TReservoirImplementationObject;
    function AddShortTermFamilyGroupObject:TShortTermFamilyGroupObject;
    function AddChannelSwitchFileObject:TChannelSwitchFileObject;
    function AddHydroPowerAllocFileObject:THydroPowerAllocFileObject;
    // Line 1
    function ReservoirImplementationObjectCount: integer;
    // Line 4
    function ShortTermFamilyGroupObjectCount: integer;
    // Line 6
    function ChannelSwitchFileObjectCount: integer;
    // Line 8
    function HydroPowerAllocFileObjectCount: integer;

    function GetChannelSwitchByFileName(AFileName: string): TChannelSwitchFileObject;

    property AllocationControlChannelFileName                      : TString                        read FAllocationControlChannelFileName;
    property ReservoirImplementationObjectByIndex[AIndex: integer] : TReservoirImplementationObject read GetReservoirImplementationObjectByIndex;
    property ShortTermFamilyGroupObjectByIndex[AIndex: integer]    : TShortTermFamilyGroupObject    read GetShortTermFamilyGroupObjectByIndex;
    property ChannelSwitchFileObjectByIndex[AIndex: integer]       : TChannelSwitchFileObject       read GetChannelSwitchFileObject;
    property HydroPowerAllocFileObjectByIndex[AIndex: integer]     : THydroPowerAllocFileObject     read GetHydroPowerAllocFileObjectByIndex;
    property FMExtraLines                                          : TStringList                    read FFMExtraLines ;
  end;
implementation

uses
  UErrorHandlingOperations;


{ TReservoirImplementationObject }

procedure TReservoirImplementationObject.CreateMemberObjects;
const OPNAME = 'TReservoirImplementationObject.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    // Line 2
    FReservoirNumber         := TInteger.Create;
    FReplacedReservoirNumber := TInteger.Create;
    FYearDamActive           := TInteger.Create;
    FMonthDamActive          := TInteger.Create;
    FYearDamAbsolete         := TInteger.Create;
    FMonthDamAbsolete        := TInteger.Create;
    FEconomicLifeOfDam       := TInteger.Create;
    FCapitalCost             := TDouble.Create;
    FMaintenanceCost         := TDouble.Create;

    // Line 3
    FYearsInConstruction     := TInteger.Create;
    FCostSchedule            := TString.Create;

    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirImplementationObject.DestroyMemberObjects;
const OPNAME = 'TReservoirImplementationObject.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    // Line 2
    FReservoirNumber.Free;
    FReplacedReservoirNumber.Free;
    FYearDamActive.Free;
    FMonthDamActive.Free;
    FYearDamAbsolete.Free;
    FMonthDamAbsolete.Free;
    FEconomicLifeOfDam.Free;
    FCapitalCost.Free;
    FMaintenanceCost.Free;

    // Line 3
    FYearsInConstruction.Free;
    FCostSchedule.Free;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirImplementationObject.Initialise: boolean;
const OPNAME = 'TReservoirImplementationObject.Initialise';
begin
  Result := False;
  try
    // Line 2
    FReservoirNumber.FData := 0;
    FReservoirNumber.FInitalised := False;
    FReservoirNumber.FLength := 5;
    FReservoirNumber.FDecimal := 0;
    FReservoirNumber.FDefaultPadding := True;

    FReplacedReservoirNumber.FData := 0;
    FReplacedReservoirNumber.FInitalised := False;
    FReplacedReservoirNumber.FLength := 4;
    FReplacedReservoirNumber.FDecimal := 0;
    FReplacedReservoirNumber.FDefaultPadding := True;

    FYearDamActive.FData := 0;
    FYearDamActive.FInitalised := False;
    FYearDamActive.FLength := 6;
    FYearDamActive.FDecimal := 0;
    FYearDamActive.FDefaultPadding := True;

    FMonthDamActive.FData := 0;
    FMonthDamActive.FInitalised := False;
    FMonthDamActive.FLength := 4;
    FMonthDamActive.FDecimal := 0;
    FMonthDamActive.FDefaultPadding := True;

    FYearDamAbsolete.FData := 0;
    FYearDamAbsolete.FInitalised := False;
    FYearDamAbsolete.FLength := 6;
    FYearDamAbsolete.FDecimal := 0;
    FYearDamAbsolete.FDefaultPadding := True;

    FMonthDamAbsolete.FData := 0;
    FMonthDamAbsolete.FInitalised := False;
    FMonthDamAbsolete.FLength := 5;
    FMonthDamAbsolete.FDecimal := 0;
    FMonthDamAbsolete.FDefaultPadding := True;

    FEconomicLifeOfDam.FData := 0;
    FEconomicLifeOfDam.FInitalised := False;
    FEconomicLifeOfDam.FLength := 7;
    FEconomicLifeOfDam.FDecimal := 0;
    FEconomicLifeOfDam.FDefaultPadding := True;

    FCapitalCost.FData := 0;
    FCapitalCost.FInitalised := False;
    FCapitalCost.FLength := 7;
    FCapitalCost.FDecimal := 2;
    FCapitalCost.FDefaultPadding := True;
    FCapitalCost.ShowDecimalPoint := True;

    FMaintenanceCost.FData := 0;
    FMaintenanceCost.FInitalised := False;
    FMaintenanceCost.FLength := 8;
    FMaintenanceCost.FDecimal := 2;
    FMaintenanceCost.FDefaultPadding := True;
    FMaintenanceCost.ShowDecimalPoint := True;

    // Line 3
    FYearsInConstruction.FData := 0;
    FYearsInConstruction.FInitalised := False;
    FYearsInConstruction.FLength := 5;
    FYearsInConstruction.FDecimal := 0;
    FYearsInConstruction.FDefaultPadding := True;

    FCostSchedule.FData := '';
    FCostSchedule.FInitalised := False;
    FCostSchedule.FLength := 0;
    FCostSchedule.FDecimal := 0;
    FCostSchedule.FDefaultPadding := True;

    Result :=  True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirImplementationObject.Reset;
const OPNAME = 'TReservoirImplementationObject.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TShortTermFamilyGroupObject }

procedure TShortTermFamilyGroupObject.CreateMemberObjects;
const OPNAME = 'TShortTermFamilyGroupObject.CreateMemberObjects';
begin
  inherited;
  try
    FYearFileActive  := TInteger.Create;
    FMonthFileActive := TInteger.Create;
    FShortTermFamilyFileObjectList   := TObjectList.CReate(True);
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TShortTermFamilyGroupObject.DestroyMemberObjects;
const OPNAME = 'TShortTermFamilyGroupObject.CreateMemberObjects';
begin
  inherited;
  try
    FreeAndNil(FYearFileActive);
    FreeAndNil(FMonthFileActive);
    FreeAndNil(FShortTermFamilyFileObjectList);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TShortTermFamilyGroupObject.CreateShortTermFamilyFileObject: TString;
const OPNAME = 'TShortTermFamilyGroupObject.CreateShortTermFamilyFileObject';
begin
  Result := nil;
  try
    Result := TString.Create;
    InitialiseFileName(Result);
    FShortTermFamilyFileObjectList.Add(Result);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TShortTermFamilyGroupObject.InitialiseFileName(AFileName: TString);
const OPNAME = 'TShortTermFamilyGroupObject.InitialiseFileName';
begin
  try
    AFileName.FData := '';
    AFileName.FInitalised := False;
    AFileName.FLength := 0;
    AFileName.FDecimal := 0;
    AFileName.FDefaultPadding := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TShortTermFamilyGroupObject.Initialise: boolean;
const OPNAME = 'TShortTermFamilyGroupObject.Initialise';
begin
  Result := False;
  try
    FYearFileActive.FData := 0;
    FYearFileActive.FInitalised := False;
    FYearFileActive.FLength := 6;
    FYearFileActive.FDecimal := 0;
    FYearFileActive.FDefaultPadding := True;

    FMonthFileActive.FData := 0;
    FMonthFileActive.FInitalised := False;
    FMonthFileActive.FLength := 3;
    FMonthFileActive.FDecimal := 0;
    FMonthFileActive.FDefaultPadding := True;
    FShortTermFamilyFileObjectList.Clear;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TShortTermFamilyGroupObject.Reset;
const OPNAME = 'TShortTermFamilyGroupObject.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TShortTermFamilyGroupObject.ShortTermFamilyFileObjectCount: integer;
const OPNAME = 'TShortTermFamilyGroupObject.Initialise';
begin
  Result := 0;
  try
    Result :=FShortTermFamilyFileObjectList.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TShortTermFamilyGroupObject.GetShortTermFamilyFileObjectByIndex(AIndex: integer): TString;
const OPNAME = 'TReservoirAndFilesImplementationObject.GetShortTermFamilyFileObjectByIndex';
begin
  Result := nil;
  try
   if(AIndex >= 0) and (AIndex < FShortTermFamilyFileObjectList.Count) then
     Result := TString(FShortTermFamilyFileObjectList[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TChannelSwitchFileObject }

procedure TChannelSwitchFileObject.CreateMemberObjects;
const OPNAME = 'TChannelSwitchFileObject.CreateMemberObjects';
begin
  inherited;
  try
    FYearFileActive  := TInteger.Create;
    FMonthFileActive := TInteger.Create;
    FFileName        := TString.Create;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelSwitchFileObject.DestroyMemberObjects;
const OPNAME = 'TChannelSwitchFileObject.DestroyMemberObjects';
begin
  inherited;
  try
    FYearFileActive.Free;
    FMonthFileActive.Free;
    FFileName.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelSwitchFileObject.Initialise: boolean;
const OPNAME = 'TChannelSwitchFileObject.Initialise';
begin
  Result := False;
  try
    FYearFileActive.FData := 0;
    FYearFileActive.FInitalised := False;
    FYearFileActive.FLength := 6;
    FYearFileActive.FDecimal := 0;
    FYearFileActive.FDefaultPadding := True;

    FMonthFileActive.FData := 0;
    FMonthFileActive.FInitalised := False;
    FMonthFileActive.FLength := 3;
    FMonthFileActive.FDecimal := 0;
    FMonthFileActive.FDefaultPadding := True;

    FFileName.FData := '';
    FFileName.FInitalised := False;
    FFileName.FLength := 0;
    FFileName.FDecimal := 0;
    FFileName.FDefaultPadding := True;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelSwitchFileObject.Reset;
const OPNAME = 'TChannelSwitchFileObject.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ THydroPowerAllocFileObject }

procedure THydroPowerAllocFileObject.CreateMemberObjects;
const OPNAME = 'THydroPowerAllocFileObject.CreateMemberObjects';
begin
  inherited;
  try
    FYearFileActive  := TInteger.Create;
    FFileName        := TString.Create;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroPowerAllocFileObject.DestroyMemberObjects;
const OPNAME = 'THydroPowerAllocFileObject.DestroyMemberObjects';
begin
  inherited;
  try
    FYearFileActive.Free;
    FFileName.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroPowerAllocFileObject.Initialise: boolean;
const OPNAME = 'THydroPowerAllocFileObject.Initialise';
begin
  Result := False;
  try
    FYearFileActive.FData := 0;
    FYearFileActive.FInitalised := False;
    FYearFileActive.FLength := 6;
    FYearFileActive.FDecimal := 0;
    FYearFileActive.FDefaultPadding := True;

    FFileName.FData := '';
    FFileName.FInitalised := False;
    FFileName.FLength := 0;
    FFileName.FDecimal := 0;
    FFileName.FDefaultPadding := True;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroPowerAllocFileObject.Reset;
const OPNAME = 'THydroPowerAllocFileObject.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TReservoirAndFilesImplementationObject }
procedure TReservoirAndFilesImplementationObject.CreateMemberObjects;
const OPNAME = 'TReservoirAndFilesImplementationObject.CreateMemberObjects';
begin
  inherited;
  try
    FAllocationControlChannelFileName    := TString.Create;
    FReservoirImplementationObjectList   := TObjectList.Create(True);
    FShortTermFamilyGroupObjectList      := TObjectList.Create(True);
    FChannelSwitchFileObjectList         := TObjectList.Create(True);
    FHydroPowerAllocFileObjectList       := TObjectList.Create(True);
    FFMExtraLines                        := TStringList.Create;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirAndFilesImplementationObject.DestroyMemberObjects;
const OPNAME = 'TReservoirAndFilesImplementationObject.CreateMemberObjects';
begin
  inherited;
  try
    FreeAndNil(FAllocationControlChannelFileName);
    FreeAndNil(FReservoirImplementationObjectList);
    FreeAndNil(FShortTermFamilyGroupObjectList);
    FreeAndNil(FChannelSwitchFileObjectList);
    FreeAndNil(FHydroPowerAllocFileObjectList);
    FreeAndNil(FFMExtraLines);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirAndFilesImplementationObject.Initialise: boolean;
const OPNAME = 'TReservoirAndFilesImplementationObject.Initialise';
begin
  Result := False;
  try
    FReservoirImplementationObjectList.Clear;
    FShortTermFamilyGroupObjectList.Clear;
    FChannelSwitchFileObjectList.Clear;
    FHydroPowerAllocFileObjectList.Clear;
    FFMExtraLines.Clear;


    FAllocationControlChannelFileName.FData := '';
    FAllocationControlChannelFileName.FInitalised := False;
    FAllocationControlChannelFileName.FLength := 0;
    FAllocationControlChannelFileName.FDecimal := 0;
    FAllocationControlChannelFileName.FDefaultPadding := True;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirAndFilesImplementationObject.Reset;
const OPNAME = 'TReservoirAndFilesImplementationObject.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirAndFilesImplementationObject.AddChannelSwitchFileObject: TChannelSwitchFileObject;
const OPNAME = 'TReservoirAndFilesImplementationObject.AddChannelSwitchFileObject';
begin
  Result := nil;
  try
    Result := TChannelSwitchFileObject.Create;
    FChannelSwitchFileObjectList.Add(Result);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirAndFilesImplementationObject.AddHydroPowerAllocFileObject: THydroPowerAllocFileObject;
const OPNAME = 'TReservoirAndFilesImplementationObject.AddHydroPowerAllocFileObject';
begin
  Result := nil;
  try
    Result := THydroPowerAllocFileObject.Create;
    FHydroPowerAllocFileObjectList.Add(Result);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirAndFilesImplementationObject.AddReservoirImplementationObject: TReservoirImplementationObject;
const OPNAME = 'TReservoirAndFilesImplementationObject.AddReservoirImplementationObject';
begin
  Result := nil;
  try
    Result := TReservoirImplementationObject.Create;
    FReservoirImplementationObjectList.Add(Result);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirAndFilesImplementationObject.AddShortTermFamilyGroupObject: TShortTermFamilyGroupObject;
const OPNAME = 'TReservoirAndFilesImplementationObject.AddShortTermFamilyGroupObject';
begin
  Result := nil;
  try
    Result := TShortTermFamilyGroupObject.Create;
    FShortTermFamilyGroupObjectList.Add(Result);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirAndFilesImplementationObject.ChannelSwitchFileObjectCount: integer;
const OPNAME = 'TReservoirAndFilesImplementationObject.ChannelSwitchFileObjectCount';
begin
  Result := 0;
  try
    Result := FChannelSwitchFileObjectList.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirAndFilesImplementationObject.HydroPowerAllocFileObjectCount: integer;
const OPNAME = 'TReservoirAndFilesImplementationObject.HydroPowerAllocFileObjectCount';
begin
  Result := 0;
  try
    Result := FHydroPowerAllocFileObjectList.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirAndFilesImplementationObject.ReservoirImplementationObjectCount: integer;
const OPNAME = 'TReservoirAndFilesImplementationObject.ReservoirImplementationObjectCount';
begin
  Result := 0;
  try
    Result := FReservoirImplementationObjectList.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirAndFilesImplementationObject.ShortTermFamilyGroupObjectCount: integer;
const OPNAME = 'TReservoirAndFilesImplementationObject.ShortTermFamilyGroupObjectCount';
begin
  Result := 0;
  try
    Result := FShortTermFamilyGroupObjectList.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TReservoirAndFilesImplementationObject.GetChannelSwitchByFileName(AFileName: string): TChannelSwitchFileObject;
const OPNAME = 'TReservoirAndFilesImplementationObject.GetChannelSwitchByFileName';
var
  LIndex : integer;
  LChannelSwitchFile        : TChannelSwitchFileObject;
begin
  Result := nil;
  if(AFileName = '') then
    Exit;
  try
    AFileName := UpperCase(Trim(AFileName));
    for LIndex := 0 to ChannelSwitchFileObjectCount -1 do
    begin
      //line 7 +++++++++++++++++++++++++
      LChannelSwitchFile := ChannelSwitchFileObjectByIndex[LIndex];
      if(UpperCase(Trim(LChannelSwitchFile.FileName.FData)) = AFileName) then
      begin
         Result := LChannelSwitchFile;
         Exit;
      end;;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirAndFilesImplementationObject.GetChannelSwitchFileObject(AIndex: integer): TChannelSwitchFileObject;
const OPNAME = 'TReservoirAndFilesImplementationObject.GetChannelSwitchFileObject';
begin
  Result := nil;
  try
   if(AIndex >= 0) and (AIndex < FChannelSwitchFileObjectList.Count) then
     Result := TChannelSwitchFileObject(FChannelSwitchFileObjectList[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirAndFilesImplementationObject.GetHydroPowerAllocFileObjectByIndex(AIndex: integer): THydroPowerAllocFileObject;
const OPNAME = 'TReservoirAndFilesImplementationObject.GetHydroPowerAllocFileObjectByIndex';
begin
  Result := nil;
  try
   if(AIndex >= 0) and (AIndex < FHydroPowerAllocFileObjectList.Count) then
     Result := THydroPowerAllocFileObject(FHydroPowerAllocFileObjectList[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirAndFilesImplementationObject.GetReservoirImplementationObjectByIndex(AIndex: integer): TReservoirImplementationObject;
const OPNAME = 'TReservoirAndFilesImplementationObject.GetReservoirImplementationObjectByIndex';
begin
  Result := nil;
  try
   if(AIndex >= 0) and (AIndex < FReservoirImplementationObjectList.Count) then
     Result := TReservoirImplementationObject(FReservoirImplementationObjectList[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirAndFilesImplementationObject.GetShortTermFamilyGroupObjectByIndex(AIndex: integer): TShortTermFamilyGroupObject;
const OPNAME = 'TReservoirAndFilesImplementationObject.GetShortTermFamilyGroupObjectByIndex';
begin
  Result := nil;
  try
   if(AIndex >= 0) and (AIndex < FShortTermFamilyGroupObjectList.Count) then
     Result := TShortTermFamilyGroupObject(FShortTermFamilyGroupObjectList[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.


