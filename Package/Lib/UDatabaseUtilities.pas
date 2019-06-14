unit UDatabaseUtilities;

interface
uses
  //  Delphi VCL
  Classes,db,SysUtils,Vcl.Dialogs,Vcl.Controls,System.Variants,

  //  DWAF VCL
  UDWADBComponents,
  UConstants,
  UAbstractObject;

function ClearDataset(ADataset: TDWADataSet): boolean;
function ClearQueryParams(AQuery: TDWAQuery; AParamType:TParamType=prString): boolean;

implementation

uses UErrorHandlingOperations;

function ClearQueryParams(AQuery: TDWAQuery; AParamType:TParamType=prString): boolean;
const OPNAME = 'ClearQueryParams';
Var
 LCount: integer;
begin
  Result := False;
  try
    for LCount := 0 to AQuery.Parameters.Count -1 do
    begin
      if(UpperCase(AQuery.Params[LCount].Name) = UpperCase('Model')) then
        AQuery.Params[LCount].DataType  := ftString
      else
      if(UpperCase(AQuery.Params[LCount].Name) = UpperCase('StudyAreaName')) then
        AQuery.Params[LCount].DataType  := ftString
      else
      if(UpperCase(AQuery.Params[LCount].Name) = UpperCase('SubArea')) then
        AQuery.Params[LCount].DataType  := ftString
      else
      if(UpperCase(AQuery.Params[LCount].Name) = UpperCase('Scenario')) then
        AQuery.Params[LCount].DataType  := ftString
      else
      if(UpperCase(AQuery.Params[LCount].Name) = UpperCase('Identifier')) then
        AQuery.Params[LCount].DataType  := ftInteger
      else
      if(UpperCase(AQuery.Params[LCount].Name) = UpperCase('Comment')) then
        AQuery.Params[LCount].DataType  := ftString
      else
      if(UpperCase(AQuery.Params[LCount].Name) = UpperCase('Fullname')) then
        AQuery.Params[LCount].DataType  := ftString
      else
      if(UpperCase(AQuery.Params[LCount].Name) = UpperCase('MasterChannelType')) then
        AQuery.Params[LCount].DataType  := ftString
      else
      if(UpperCase(AQuery.Params[LCount].Name) = UpperCase('Stochastic')) then
        AQuery.Params[LCount].DataType  := ftString
      else
      case AParamType of
        prInt  : AQuery.Params[LCount].DataType  := ftInteger;
        prFloat: AQuery.Params[LCount].DataType  := ftFloat;
      else
       AQuery.Params[LCount].DataType  := ftString;
      end;
      AQuery.Params[LCount].Value := Unassigned;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function ClearDataset(ADataset: TDWADataSet): boolean;
const OPNAME = 'ClearDataset';
begin
  Result := False;
  try
    if not Assigned(ADataset) then
     raise Exception.Create('The parameter dataset is not assigned.');

    if not ADataset.Active then
     raise Exception.Create('The parameter dataset is not active. '+
                           'You can only clear an active dataset.');

    //ADataset.DisableControls;
    //try
    ADataset.First;
    while not ADataset.IsEmpty do
     ADataset.Delete;
    ADataset.Close;
    ADataset.Open;
    //finally
    //  ADataset.EnableControls;
    //end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.
