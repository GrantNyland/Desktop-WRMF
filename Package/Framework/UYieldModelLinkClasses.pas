//
//
//  UNIT      : Contains link classes used by the yield model.
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/02/27
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UYieldModelLinkClasses;

interface
uses  contnrs;

const
   MaxHydrologyFiles = 4;
   MaxOutputFiles = 3;
   MaxDataFiles = 16;

type
  TYieldModelMenuAction = (meValidateFiles, meLoadModelDataFromFiles, meSaveModelDataToDB,
                           meLoadModelDataFromDB, meSaveModelDataToFiles, meCompareFiles,
                           fsApplySelection,meDisableModelMenus);

  TYieldModelMenuData = class(TObject)
  public
    Action: TYieldModelMenuAction;
    constructor Create(AAction: TYieldModelMenuAction);
  end;

  TAbstractYieldModelLinkClass = class(TObject)
  public
    constructor Create;
    procedure Reset;virtual;abstract;
  end;

  {TFileName = class(TAbstractYieldModelLinkClass)
  protected
    FDefaultName: string;
    FCurrentPath: string;
    FCurrentName: string;
    FCurrentFullName: string;
    FPreviousName: string;
    FLangDescription: string;
    FLoaded: boolean;
    procedure SetFileName(AFileName: string);
  public
    constructor Create;
    procedure Reset; override;
    property DefaultName: string       read FDefaultName      write FDefaultName;
    property CurrentName: string       read FCurrentName      write FCurrentName;
    property CurrentPath: string       read FCurrentPath      write FCurrentPath;
    property FileName   : string       read FCurrentFullName  write SetFileName;
    property PreviousName: string      read FPreviousName     write FPreviousName;
    property LangDescription: string   read FLangDescription  write FLangDescription;
    property Loaded: boolean           read FLoaded           write FLoaded;
    function CopyFromFile(AFileName: TFileName): boolean;
  end;
  }{
  //TFileNameArray = array of TFileName;
  TDataFileNames = array[1..MaxDataFiles] of TFileName;
  //TOutputFileNames = array[1..MaxOutputFiles] of TFileName;
  //THydroFileNames = array [1..MaxHydrologyFiles] of TFileNameArray;
  //THydroFileIndex = (hfInc,hfRan,hfAff,hfIrr);
  }
  {TFileNames = class(TObject)
  protected
    FDataFileNames : TDataFileNames;
    //FOutputFileNames : TOutputFileNames;
    //FHydroFileNames : THydroFileNames;
    //function GetHydroFileNames(AIndex:integer): TFileNameArray;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Reset;
    function AssignFrom(AFileNames: TFileNames): boolean;
    //function AddHydroFile(AFileName: string; AFilesArray:TFileNameArray): boolean;
    property DataFileNames: TDataFileNames read FDataFileNames;
    //property OutputFileNames: TOutputFileNames read FOutputFileNames ;
    //property HydroFileNames[AIndex:integer]:TFileNameArray read GetHydroFileNames;
  end;}

implementation

uses SysUtils,UErrorHandlingOperations;

{ TAbstractYieldModelLinkClass }

constructor TAbstractYieldModelLinkClass.Create;
const OPNAME = 'TAbstractYieldModelLinkClass.Create';
begin
  inherited Create;
  try
    Reset;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


{ TFileName }

{function TFileName.CopyFromFile(AFileName: TFileName): boolean;
const OPNAME = 'TFileName.Create';
begin
  Result := False;
  try
   FDefaultName     := AFileName.DefaultName;
   FCurrentFullName := AFileName.FileName;
   FCurrentName     := AFileName.CurrentName;
   FCurrentPath     := AFileName.CurrentPath;
   FPreviousName    := AFileName.PreviousName;
   FLangDescription := AFileName.LangDescription;
   FLoaded          := AFileName.Loaded;
   Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

constructor TFileName.Create;
const OPNAME = 'TFileName.Create';
begin
  try
    inherited Create;
    FDefaultName := '';
    FCurrentPath := '';
    FCurrentName := '';
    FCurrentFullName := '';
    FPreviousName := '';
    FLangDescription := '';
    FLoaded := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileName.Reset;
const OPNAME = 'TFileName.Reset';
begin
  try
    FCurrentPath := '';
    FCurrentName := '';
    FCurrentFullName := '';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileName.SetFileName(AFileName: string);
const OPNAME = 'TFileName.SetFileName';
begin
  try
    if(FCurrentFullName <> '') then
      FPreviousName := FCurrentFullName;

    if(AFileName = '') then
    begin
      FCurrentPath := '';
      FCurrentName := '';
      FCurrentFullName := '';
    end
    else
    begin
      FCurrentFullName  := AFileName;
      FCurrentPath := ExtractFilePath(AFileName);
      FCurrentName := ExtractFileName(AFileName);
    end;

    if(FPreviousName = '') then
      FPreviousName := FCurrentFullName;
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

{ TFileNames }
{function TFileNames.AddHydroFile(AFileName: string;
  AFilesArray: TFileNameArray): boolean;
const OPNAME = 'TFileNames.AddHydroFile';
var
  LFileName: TFileName;
begin
  Result := False;
  try
    SetLength(AFilesArray,Length(AFilesArray)+1);
    LFileName := TFileName.Create;
    LFileName.FileName := AFileName;
    FDataFileNames[High(AFilesArray)] := LFileName;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileNames.AssignFrom(AFileNames: TFileNames): boolean;
const OPNAME = 'TFileNames.AssignFrom';
var
  LCount: integer;
  //LIndex: integer;
begin
  Result := False;
  try
    for LCount := Low(FDataFileNames) to High(FDataFileNames) do
      FDataFileNames[LCount].CopyFromFile(AFileNames.FDataFileNames[LCount]);

    {for LCount := Low(FOutputFileNames) to High(FOutputFileNames) do
      FOutputFileNames[LCount].CopyFromFile(AFileNames.FOutputFileNames[LCount]);

    for LCount := Low(FHydroFileNames) to High(FHydroFileNames) do
    begin
      for LIndex := Low(FHydroFileNames[LCount]) to High(FHydroFileNames[LCount]) do
        FHydroFileNames[LCount,LIndex].Free;
      SetLength(FHydroFileNames[LCount],0);
    end;

    for LCount := 1 to MaxHydrologyFiles do
    begin
      for LIndex := Low(AFileNames.HydroFileNames[LCount]) to High(AFileNames.HydroFileNames[LCount]) do
      begin
        AddHydroFile('',HydroFileNames[LCount]);
        FHydroFileNames[LCount,LIndex].CopyFromFile(AFileNames.FHydroFileNames[LCount,LIndex]);
      end;
    end;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

constructor TFileNames.Create;
const OPNAME = 'TFileNames.Create';
var
  LCount: integer;
  LFileName: TFileName;
  //LObjectList: TObjectList;
begin
  inherited Create;
  try
    for LCount := Low(FDataFileNames) to High(FDataFileNames) do
    begin
      LFileName := TFileName.Create;
      FDataFileNames[LCount] := LFileName;
    end;
    {for LCount := Low(FOutputFileNames) to High(FOutputFileNames) do
    begin
      LFileName := TFileName.Create;
      FOutputFileNames[LCount] := LFileName;
    end;

    for LCount := Low(FHydroFileNames) to High(FHydroFileNames) do
      SetLength(FHydroFileNames[LCount],0);


  except on E: Exception do HandleError(E, OPNAME) end;
end;

destructor TFileNames.Destroy;
const OPNAME = 'TFileNames.Destroy';
var
  LCount: integer;
begin
  try
    try
      for LCount := Low(FDataFileNames) to High(FDataFileNames) do
        FreeAndNil(FDataFileNames[LCount]);
      {for LCount := Low(FOutputFileNames) to High(FOutputFileNames) do
        FreeAndNil(FOutputFileNames[LCount]);
      for LCount := Low(FHydroFileNames) to High(FHydroFileNames) do
        FreeAndNil(FHydroFileNames[LCount]);

    finally
      inherited Destroy;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{function TFileNames.GetHydroFileNames(AIndex:integer): TFileNameArray;
const OPNAME = 'TFileNames.Reset';
begin
  Result := nil;
  try
    if (AIndex >= Low(FHydroFileNames)) and (AIndex <= High(FHydroFileNames)) then
    Result := FHydroFileNames[AIndex];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileNames.Reset;
const OPNAME = 'TFileNames.Reset';
var
  LCount: integer;
begin
  try
    for LCount := Low(FDataFileNames) to High(FDataFileNames) do
     FDataFileNames[LCount].Reset;
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

{ TYieldModelMenuData }

constructor TYieldModelMenuData.Create(AAction: TYieldModelMenuAction);
const OPNAME = 'TYieldModelMenuData.Create';
begin
  try
    inherited Create;
    Action := AAction;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
