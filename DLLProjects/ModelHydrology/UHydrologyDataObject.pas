
//
//
//  UNIT      : Contains THydrologyFilesObject Class
//  AUTHOR    : Titi Ngubane(PDNA)
//  DATE      : 30/04/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
unit UHydrologyDataObject;

interface
uses
  Classes, sysutils,contnrs,

  //  DWAF VCL
  UAbstractComponent,
  UAbstractObject,
  UAbstractDataObject,
  UBasicObjects,
  UConstants,
  UFileNames,
  UAbstractHydrologyModelData,
  UViewModelDataObject,
  UParameterData;

type

  THydrologyFileLine = class ( TAbstractDataObject )
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    //Number of lines in the file
     FFileName         : TString;
     FYearValue        : TInteger;
     FYearValuePatch   : TChar;
     FMonthValues      : Array[MinMonths..MaxMonths] Of TDouble;
     FMonthValuesPatch : Array[MinMonths..MaxMonths] Of TChar;
     FTotalValue       : TDouble;
    procedure Reset; override;
    function Initialise: boolean; override;
  end;

  THydrologyFileObject = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FProjectsDetails : TObjectList;
    FType            : TInteger;
    FFileNumber            : TInteger;
    procedure Reset;override;
    function Initialise: boolean;override;
    function AddDetailsLines: boolean;
  end;

  THydrologyFilesObject = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FFiles : TObjectList;
    function AddFile(AFileNumber: integer): boolean;
    procedure Reset;override;
    function Initialise: boolean;override;
  end;

implementation

{ THydrologyFileObject }
uses UErrorHandlingOperations;

procedure THydrologyFileObject.CreateMemberObjects;
const OPNAME = 'THydrologyFileObject.CreateMemberObjects';
begin
  try
    FProjectsDetails  := TObjectList.Create(True);
    FType             := TInteger.Create;
    FFileNumber             := TInteger.Create;
    Initialise;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydrologyFileObject.DestroyMemberObjects;
const OPNAME = 'THydrologyFileObject.DestroyMemberObjects';
begin
  try
    FProjectsDetails.Free;
    FType.Free;
    FFileNumber.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyFileObject.Initialise: boolean;
const OPNAME = 'THydrologyFileObject.Initialise';
Begin
  Result := False;
  try
    FProjectsDetails.Clear;

    FType.FData := 0;
    FType.FInitalised := False;
    FType.FLength  := 5;
    FType.FDecimal := 0;
    FType.FDefaultPadding := True;

    FFileNumber.FData := 0;
    FFileNumber.FInitalised := False;
    FFileNumber.FLength  := 5;
    FFileNumber.FDecimal := 0;
    FFileNumber.FDefaultPadding := True;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyFileObject.AddDetailsLines: boolean;
const OPNAME = 'THydrologyFileObject.AddDetailsLines';
var
  LProjectsDetails :THydrologyFileLine;
Begin
  Result := False;
  try
    LProjectsDetails := THydrologyFileLine.Create;
    FProjectsDetails.Add(LProjectsDetails);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydrologyFileLine.CreateMemberObjects;
const OPNAME = 'THydrologyFileLine.CreateMemberObjects';
var
  LCount: integer;
  LDouble : TDouble;
  LChar: TChar;
begin
  try

    FFileName               := TString.Create;
    FYearValue              := TInteger.Create;
    FYearValuePatch         := TChar.Create;
    FTotalValue             := TDouble.Create;
    for LCount := MinMonths to MaxMonths do
    begin
      LDouble              := TDouble.Create;
      FMonthValues[LCount] := LDouble;
      LChar := TChar.Create;
      FMonthValuesPatch[LCount] := LChar;
    end;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydrologyFileLine.DestroyMemberObjects;
const OPNAME = 'THydrologyFileLine.DestroyMemberObjects';
var
  LCount: integer;
begin
  try
    FFileName.Free;
    FYearValue.Free;
    FYearValuePatch.Free;
    FTotalValue.Free;
    for LCount := MinMonths to MaxMonths do
    begin
      FMonthValues[LCount].Free;
      FMonthValuesPatch[LCount].Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyFileLine.Initialise: boolean;
const OPNAME = 'THydrologyFileLine.Initialise';
var
  LCount : Integer;
Begin
  Result := False;
  try

    FFileName.FData := '';
    FFileName.FInitalised := False;
    FFileName.FLength := 0;
    FFileName.FDecimal := 0;
    FFileName.FDefaultPadding := True;

    FYearValue.FData := 0;
    FYearValue.FInitalised := False;
    FYearValue.FLength := 8;
    FYearValue.FDecimal := 0;
    FYearValue.FDefaultPadding := True;

    FYearValuePatch.FData := ' ';
    FYearValuePatch.FInitalised := False;
    FYearValuePatch.FLength := 1;
    FYearValuePatch.FDecimal := 0;
    FYearValuePatch.FDefaultPadding := True;

    for LCount := MinMonths to MaxMonths do
    begin
      FMonthValues[LCount].FData := 0;
      FMonthValues[LCount].FInitalised := False;
      FMonthValues[LCount].FLength  := 7;
      FMonthValues[LCount].FDecimal := 2;
      FMonthValues[LCount].FDefaultPadding := True;
    end;

    for LCount := MinMonths to MaxMonths do
    begin
      FMonthValuesPatch[LCount].FData := ' ';
      FMonthValuesPatch[LCount].FInitalised := False;
      FMonthValuesPatch[LCount].FLength  := 1;
      FMonthValuesPatch[LCount].FDecimal := 0;
      FMonthValuesPatch[LCount].FDefaultPadding := True;
    end;
    FTotalValue.FData := 0;
    FTotalValue.FInitalised := False;
    FTotalValue.FLength := 9;
    FTotalValue.FDecimal := 2;
    FTotalValue.FDefaultPadding := True;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure THydrologyFileObject.Reset;
const OPNAME = 'THydrologyFileObject.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ THydrologyFilesObject }

function THydrologyFilesObject.AddFile(AFileNumber: integer): boolean;
const OPNAME = 'THydrologyFilesObject.AddFile';
var
  LFile: THydrologyFileObject;
  LCount: integer;
begin
  Result := False;
  try
    for LCount := FFiles.Count to AFileNumber do
    begin
      LFile := THydrologyFileObject.Create;
      FFiles.Add(LFile);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydrologyFilesObject.CreateMemberObjects;
const OPNAME = 'THydrologyFilesObject.CreateMemberObjects';
begin
  try
    FFiles := TObjectList.Create(True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydrologyFilesObject.DestroyMemberObjects;
const OPNAME = 'THydrologyFilesObject.DestroyMemberObjects';
begin
  try
    FreeAndNil(FFiles);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydrologyFileLine.Reset;
const OPNAME = 'THydrologyFileLine.Reset';
begin
  inherited;
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyFilesObject.Initialise: boolean;
const OPNAME = 'THydrologyFilesObject.Initialise';
begin
  Result := False;
  try
   FFiles.Clear;
   Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydrologyFilesObject.Reset;
const OPNAME = 'THydrologyFilesObject.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
