unit UDailyDiversionModelFileDataObjects;

interface
uses
  Classes,
  sysutils,
  contnrs,
  //  DWAF VCL
  UBasicObjects,
  UAbstractDataObject,
  UFileNames,
//  UDataFileObjects,
  UDailyDiversionFileDataObject,
  UAbstractObject;

type
  TDailyDiversionModelFileDataObjects = class(TAbstractObject)    
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    //Daily Flow Data commatext
    FDailyDiversionFileDataObject : TDailyDiversionFileDataObject;
    function Initialise: Boolean; override;
    function ValidateFileData(AAppModules:TAppModules;AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;virtual;

end;

implementation
uses
  UErrorHandlingOperations;


{ TDailyDiversionModelFileDataObjects }

procedure TDailyDiversionModelFileDataObjects.CreateMemberObjects;
const OPNAME = 'TDailyDiversionModelFileDataObjects.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyDiversionModelFileDataObjects.DestroyMemberObjects;
const OPNAME = 'TDailyDiversionModelFileDataObjects.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionModelFileDataObjects.Initialise: Boolean;
const OPNAME = 'TDailyDiversionModelFileDataObjects.Initialise';
begin
  Result := False;
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionModelFileDataObjects.ValidateFileData(AAppModules: TAppModules;
  AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TDailyDiversionModelFileDataObjects.ValidateFileData';
begin
  Result := False;
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
 