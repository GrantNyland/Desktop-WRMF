//
//
//  UNIT      : Contains THydrologyFileTypeManager Class
//  AUTHOR    : Dziedzi Ramulondi (PDNA)
//  DATE      : 2002/05/10
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UHydrologyFileTypeManager;

interface

uses
  UHydrologyFileType,
  UAbstractObject;

type
  THydrologyFileTypeManager = class(TAbstractAppObject)
  protected
    FHydrologyFileType: THydrologyFileType;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function Initialise: boolean; override;
    property HydrologyFileType: THydrologyFileType read FHydrologyFileType;
  end;

implementation

uses
  SysUtils,
  UDataSetType,
  USQLDatabaseLayer,
  UHydrologyFileTypeDataSetConstructor,
  UErrorHandlingOperations;

procedure THydrologyFileTypeManager.CreateMemberObjects;
const OPNAME = 'THydrologyFileTypeManager.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    TSQLDatabaseLayer(FAppModules.Database).AddDataSetConstructor(THydrologyFileTypeDataSetConstructor.Create(FAppModules));
    FHydrologyFileType := THydrologyFileType.Create;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydrologyFileTypeManager.DestroyMemberObjects;
const OPNAME = 'THydrologyFileTypeManager.DestroyMemberObjects';
begin
  try
    FreeAndNil(FHydrologyFileType);
    TSQLDatabaseLayer(FAppModules.Database).DeleteDataSetConstructorsOfType(THydrologyFileTypeDataSetConstructor);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyFileTypeManager.Initialise: boolean;
const OPNAME = 'THydrologyFileTypeManager.Initialise';
var LHydrologyFileTypeDataSet: TAbstractModelDataset;
begin
  Result := False;
  try
    LHydrologyFileTypeDataSet := nil;
    try
      if Assigned(FAppModules.Database()) and (FAppModules.Database.Connected) then
      begin
        if FAppModules.Database.CreateDataset(integer(dtHydrologyFileType), LHydrologyFileTypeDataSet) then
        begin
          if FHydrologyFileType.Initialise(LHydrologyFileTypeDataSet) then
          begin
            Result := True;
          end;
        end;
      end;
    finally
      LHydrologyFileTypeDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
