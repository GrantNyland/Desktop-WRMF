unit UFilesLineTypeObject;

interface

uses
  Classes, sysutils,contnrs,

  //  DWAF VCL
  UConstants,
  UFileNames,
  UAbstractObject,
  UAbstractFileNamesObject;
  //UAbstractYieldModelDataObject;

type
  TFileLineTypesObject = class(TAbstractFileLineTypesObject)
  protected
    FFileNameObject: TAbstractModelFileName;
    FFileLinesTypes: TStringList;
    function GetFileNameObject: TAbstractModelFileName; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddLineType(ALineNumber: integer;ALineType: string);
    procedure ClearLineTypes;
    function GetFileLineType(ALineNumber: Integer): string;override;
    function LinesCount: integer; override;
    property FileNameObject: TAbstractModelFileName  read FFileNameObject write FFileNameObject;
  end;

  TFilesLineTypes = class(TAbstractFilesLineTypes)
  protected
    FFilesLineTypeList: TObjectList;
    function GetFileLineTypesObject(AFileObject: TAbstractModelFileName): TAbstractFileLineTypesObject; override;
  public
    constructor Create;
    destructor Destroy; override;
    function FilesCount: integer; override;
    function AddFile(AFileNameObject: TAbstractModelFileName): TAbstractFileLineTypesObject; override;
    procedure Clear;
    function GetFileLineType(AFileObject: TAbstractModelFileName; ALineNumber: Integer): string; override;
  end;


implementation

uses UUtilities,
     UFileNameConstants,
     UErrorHandlingOperations;

{ TFileLineTypesObject }

constructor TFileLineTypesObject.Create;
const OPNAME = 'TFileLineTypesObject.Create';
begin
  inherited;
  try
    FFileNameObject := nil;
    FFileLinesTypes := TStringList.Create;
  except on E: Exception do HandleError(E, OPNAME) end;

end;

destructor TFileLineTypesObject.Destroy;
const OPNAME = 'TFileLineTypesObject.Destroy';
begin
  try
    FFileNameObject := nil;
    FFileLinesTypes.Free;
    inherited;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileLineTypesObject.AddLineType(ALineNumber: integer;ALineType: string);
const OPNAME = 'TFileLineTypesObject.AddLineType';
var
  LCount: integer;
begin
  try
    if (ALineNumber = FFileLinesTypes.Count) then
      FFileLinesTypes.Add(ALineType)
    else
      if (ALineNumber < FFileLinesTypes.Count) then
        FFileLinesTypes.Strings[ALineNumber] := ALineType
    else
    begin
      for LCount:= ALineNumber downto FFileLinesTypes.Count do
        FFileLinesTypes.Add(ALineType)
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileLineTypesObject.ClearLineTypes;
const OPNAME = 'TFileLineTypesObject.ClearLineTypes';
begin
  try
    FFileLinesTypes.Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileLineTypesObject.GetFileLineType(ALineNumber: Integer): string;
const OPNAME = 'TFileLineTypesObject.GetFileLineType';
begin
  Result := '';
  try
    if (ALineNumber > 0) and (ALineNumber < FFileLinesTypes.Count ) then
      Result := FFileLinesTypes.Strings[ALineNumber];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileLineTypesObject.LinesCount: integer;
const OPNAME = 'TFileLineTypesObject.LinesCount';
begin
  Result := 0;
  try
    Result := FFileLinesTypes.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileLineTypesObject.GetFileNameObject: TAbstractModelFileName;
const OPNAME = 'TFileLineTypesObject.GetFileNameObject';
begin
  Result := nil;
  try
    Result := FFileNameObject;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TFilesLineTypes }

constructor TFilesLineTypes.Create;
const OPNAME = 'TFilesLineTypes.Create';
begin
  inherited;
  try
    FFilesLineTypeList := TObjectList.Create(True);
    Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

destructor TFilesLineTypes.Destroy;
const OPNAME = 'TFilesLineTypes.Destroy';
begin
  try
    FFilesLineTypeList.Free;
    inherited;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFilesLineTypes.Clear;
const OPNAME = 'TFilesLineTypes.Clear';
var
  LObject: TFileLineTypesObject;
  LCount: integer;
begin
  try
    FFilesLineTypeList.Clear;
    //Add dummy to fix indexes (File 1 .. 20);
    LObject := TFileLineTypesObject.Create;
    FFilesLineTypeList.Add(LObject);

    //Add File 1 .. 20;
    for LCount := 1 to 20 do
    begin
      LObject := TFileLineTypesObject.Create;
      FFilesLineTypeList.Add(LObject);
    end;

    //Add parameter File;
    LObject := TFileLineTypesObject.Create;
    FFilesLineTypeList.Add(LObject);

    //Add directory File;
    LObject := TFileLineTypesObject.Create;
    FFilesLineTypeList.Add(LObject);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesLineTypes.FilesCount: integer;
const OPNAME = 'TFilesLineTypes.FilesCount';
begin
  Result := 0;
  try
    Result := FFilesLineTypeList.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesLineTypes.AddFile(AFileNameObject: TAbstractModelFileName): TAbstractFileLineTypesObject;
const OPNAME = 'TFilesLineTypes.AddFile';
var LFileLineTypesObject: TFileLineTypesObject;
begin
  Result := nil;
  try
    if Assigned(AFileNameObject) then
    begin
      LFileLineTypesObject := TFileLineTypesObject.Create;
      LFileLineTypesObject.FileNameObject :=  AFileNameObject;
      FFilesLineTypeList.Add(LFileLineTypesObject);
      Result := LFileLineTypesObject;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesLineTypes.GetFileLineTypesObject(AFileObject: TAbstractModelFileName): TAbstractFileLineTypesObject;
const OPNAME = 'TFilesLineTypes.GetFileLineTypesObject';
var
  LFileLineTypesObject: TFileLineTypesObject;
begin
  Result := nil;
  try
    if Assigned(AFileObject) then
    begin
      LFileLineTypesObject := nil;
      case AFileObject.FileGroup of
        fgConfiguration:
          begin
            LFileLineTypesObject := TFileLineTypesObject(FFilesLineTypeList.Items[AFileObject.FileNumber]);
          end;
        fgParameter:
          begin
            LFileLineTypesObject := TFileLineTypesObject(FFilesLineTypeList.Items[FFilesLineTypeList.Count-2]);
          end;
        fgDirectories:
          begin
            LFileLineTypesObject := TFileLineTypesObject(FFilesLineTypeList.Items[FFilesLineTypeList.Count-1]);
          end;
        fgAllocationDefinition,
        fgGrowthFactors,
        fgDisbenefitDefinition,
        fgMonthlyWaterRequirement,
        fgReservoirImplementation,
        fgHydropowerAllocation,
        fgPumpingChannelControl,
        fgGeneralChannelControl,
        fgReclamationPlantControl,
        fgReturnFlowChannel,
        fgChannelSwitchControl,
        fgTariffCalculation,
        fgAllocationChannel,
        fgReleaseStructure,
        fgMIMM,
        fgCurtail:
          LFileLineTypesObject := TFileLineTypesObject(FFilesLineTypeList.Items[AFileObject.FileNumber]);

      end;//case

      if Assigned(LFileLineTypesObject) then
      begin
         LFileLineTypesObject.FileNameObject := AFileObject;
         Result := LFileLineTypesObject;
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesLineTypes.GetFileLineType(AFileObject: TAbstractModelFileName; ALineNumber: Integer): string;
const OPNAME = 'TFilesLineTypes.GetFileLineType';
var
  LCount: integer;
  LFileLineTypesObject: TAbstractFileLineTypesObject;
begin
  Result := '';
  try
    for LCount := 0 to FFilesLineTypeList.Count - 1 do
    begin
      LFileLineTypesObject := FileLineTypesObject[AFileObject];
      if Assigned(LFileLineTypesObject) then
      begin
        if AFileObject.CheckForEqualityTrue(LFileLineTypesObject.FileNameObject) then
          Result := LFileLineTypesObject.GetFileLineType(ALineNumber);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
