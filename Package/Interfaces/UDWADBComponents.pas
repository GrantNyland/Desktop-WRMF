
//
//
//  UNIT      : Contains DWADBComponents Class
//  AUTHOR    : Dziedzi Ramulondi(Cornastone)
//  DATE      : 05/07/2013
//  COPYRIGHT : Copyright © 2013 DWAF
//
//
unit UDWADBComponents;

interface

uses
  Classes, sysutils, Data.DB, Data.Win.ADODB;

type

  TDWAConnection = class(TADOConnection)
  protected
  public
  end;

  {TDWATField = class(TField)
  protected
    function GetAsTrimmedString: string; virtual;
    procedure SetAsTrimmedString(const AValue: string); virtual;
  public
    property AsTrimmedString: string  read GetAsTrimmedString;// write SetAsTrimmedString;
  end;}

  TDWAQuery = class(TADOQuery)
  protected
    function Get_Parameters : TParameters;
    function Get_ParametersCount : integer;
    function Get_DatabaseName : TADOConnection;
    Procedure Set_DatabaseName(ADatabaseName : TADOConnection);
  public
    //function FieldByName(const AFieldName: string): TDWATField;
    function ParamByName(AParameterName : string): TParameter;
    property Params: TParameters  read Get_Parameters;
    property ParamCount: integer  read Get_ParametersCount;
    property DatabaseName: TADOConnection  read Get_DatabaseName write Set_DatabaseName;
  end;

  TDWATable = class(TADOTable)
  protected
    function Get_Parameters : TParameters;
    function Get_ParametersCount : integer;
    function Get_DatabaseName : TADOConnection;
    Procedure Set_DatabaseName(ADatabaseName : TADOConnection);
  public
    property Params: TParameters  read Get_Parameters;
    property ParamsCount: integer  read Get_ParametersCount;
    property DatabaseName: TADOConnection  read Get_DatabaseName write Set_DatabaseName;
  end;

  TDWABlobStream = class(TADOBlobStream)
  protected
  public
  end;


  TDWAParameter = class(TParameter)
  protected
  public
  end;

  //TDWADataSet = class(TCustomADODataSet)
  TDWADataSet = class(TDWAQuery)
  protected
    //function Get_Parameters : TParameters;
    //function Get_ParametersCount : integer;
  public
    //property Params: TParameters  read Get_Parameters;
    //property ParamsCount: integer  read Get_ParametersCount;
  end;

implementation


uses UErrorHandlingOperations;

{ TDWAQuery }

function TDWAQuery.ParamByName(AParameterName: string): TParameter;
const OPNAME = 'TDWAQuery.ParamByName';
begin
  Result := nil;
  try
    Result := Parameters.ParamByName(AParameterName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TDWAQuery.Get_Parameters: TParameters;
const OPNAME = 'TDWAQuery.Get_Parameters';
begin
  Result := nil;
  try
    Result := Parameters;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDWAQuery.Get_ParametersCount: integer;
const OPNAME = 'TDWAQuery.Get_ParametersCount';
begin
  Result := 0;
  try
    Result := Parameters.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{function TDWAQuery.FieldByName(const AFieldName: string): TDWATField;
const OPNAME = 'TDWAQuery.FieldByName';
begin
  Result := nil;
  try
    Result := (inherited FieldByName(AFieldName)) as TDWATField;
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

function TDWAQuery.Get_DatabaseName: TADOConnection;
const OPNAME = 'TDWAQuery.Get_DatabaseName';
begin
  Result := nil;
  try
    Result := Connection;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDWAQuery.Set_DatabaseName(ADatabaseName: TADOConnection);
const OPNAME = 'TDWAQuery.Set_DatabaseName';
begin
  try
    Connection := ADatabaseName;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TDWATField }

{function TDWATField.GetAsTrimmedString: string;
const OPNAME = 'TDWATField.GetAsTrimmedString';
begin
  Result := '';
  try
    Result := inherited GetAsString;
    Result := Trim(Result);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDWATField.SetAsTrimmedString(const AValue: string);
const OPNAME = 'TDWATField.SetAsTrimmedString';
begin
  try
    inherited SetAsString(Value);
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

{ TDWATable }

function TDWATable.Get_Parameters: TParameters;
const OPNAME = 'TDWATable.Get_Parameters';
begin
  Result := nil;
  try
    Result := Parameters;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDWATable.Get_ParametersCount: integer;
const OPNAME = 'TDWATable.Get_ParametersCount';
begin
  Result := 0;
  try
    Result := Parameters.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDWATable.Get_DatabaseName: TADOConnection;
const OPNAME = 'TDWATable.Get_DatabaseName';
begin
  Result := nil;
  try
    Result := Connection;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDWATable.Set_DatabaseName(ADatabaseName: TADOConnection);
const OPNAME = 'TDWATable.Set_DatabaseName';
begin
  try
    Connection := ADatabaseName;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TDWADataSet }

{function TDWADataSet.Get_Parameters: TParameters;
const OPNAME = 'TDWADataSet.Get_Parameters';
begin
  Result := nil;
  try
    Result := Parameters;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDWADataSet.Get_ParametersCount: integer;
const OPNAME = 'TDWADataSet.Get_ParametersCount';
begin
  Result := 0;
  try
    Result := Parameters.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end; }

end.
