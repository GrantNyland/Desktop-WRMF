//
//
//  UNIT      : Contains TModelDataset Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/02/06
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UModelDataset;

interface

uses
  Classes,
  Data.DB,
  System.Variants,
  UDWADBComponents,
  UAbstractObject;

type
  TCreatedDataset  = (crdstNone,crdstDataset,crdstQuery,crdstTable);
  TModelDataset = class(TAbstractModelDataset)
  protected
    FDataSet: TDWADataSet;
    FCreatedDataset : TCreatedDataset;
    FReadOnly: Boolean;
    FUniDirectional : boolean;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function BuildSQL: string;
    function GetTableName: string; virtual;
    //function GetFieldNamesCommaText: string; virtual;
    //function GetParamNamesCommaText: string; virtual;
    function GetSelectClause: string; virtual;
    function GetFromClause: string; virtual;
    function GetWhereClause: string; virtual;
    function GetGroupOrOrderClause: string; virtual;
    function ClearQueryTypeParams(AParamType :TParamType): boolean;
  public
    function DataSet: TDWADataSet; override;
    procedure ClearSQL; override;
    procedure ResetDefaultSQL; override;
    procedure SetSQL(ASQL: string); override;
    procedure AppendSQL(ASQL: string); override;
    procedure SetParams(AParamNames, AParamValues: array of string); override;
    procedure SetParamValue(AParamName: string; AValue: WideString; ADataType: TFieldType; ABlobField: boolean=False; ABlobLength: integer=0); override;
    procedure ReplaceSQLParam(AParamName, AParamValue: String); override;
    procedure ReplaceSQLParamArray(AParamName, AParamValue: array of String); override;
    procedure ExecSQL(AIgnoreErrors : boolean = false); override;
    procedure SetReadOnly(AReadOnly: boolean); override;
    function IsReadOnly: boolean; override;
    //procedure SetUniDirectional(AUniDirectional: boolean); override;
    //function IsUniDirectional: boolean; override;
    function AreAllParamsBound(AReportError: boolean = False): boolean; override;
    function ClearDataset : boolean; override;
    function ClearQueryParams(AParamType :TParamType=prAll): boolean; override;
    function GetParamByName( AParamName: string): TDWAParameter; override;
  end;

implementation

uses
  Math,
  SysUtils,
  USQLDatabaseLayer,
  UErrorHandlingOperations;

procedure TModelDataset.CreateMemberObjects;
const OPNAME = 'TModelDataset.CreateMemberObjects';
begin
  try
    FReadOnly         := True;
    FUniDirectional   := True;
    FDataSet          := TSQLDatabaseLayer(FAppModules.Database).CreateDatasetDataset;
    FCreatedDataset   := crdstQuery;
    ResetDefaultSQL;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelDataset.DestroyMemberObjects;
const OPNAME = 'TModelDataset.DestroyMemberObjects';
begin
  try
    FDataSet.Close;
    FreeAndNil(FDataSet);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelDataset.DataSet: TDWADataSet;
const OPNAME = 'TModelDataset.DataSet';
begin
  Result := nil;
  try
    Result := FDataSet;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelDataset.ClearSQL;
const OPNAME = 'TModelDataset.ClearSQL';
begin
  try
    FDataSet.SQL.Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelDataset.ResetDefaultSQL;
const OPNAME = 'TModelDataset.ResetDefaultSQL';
begin
  try
    SetSQL(BuildSQL);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelDataset.SetSQL(ASQL: string);
const OPNAME = 'TModelDataset.SetSQL';
begin
  try
    FDataSet.SQL.Text := Trim(ASQL);
    FDataSet.Prepared := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelDataset.AppendSQL(ASQL: string);
const OPNAME = 'TModelDataset.AppendSQL';
begin
  try
    SetSQL(FDataSet.SQL.Text + ASQL);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelDataset.SetParams(AParamNames, AParamValues: array of string);
const OPNAME = 'TModelDataset.SetParams';
var LPassedParamIndex, LDatasetParamIndex: integer;
begin
  try
    for LPassedParamIndex := Low(AParamNames) to High(AParamNames) do
    begin
      try
        for LDatasetParamIndex := 0 to FDataSet.ParamCount -1 do
        begin
          if UpperCase(FDataSet.Params[LDatasetParamIndex].Name) = UpperCase(AParamNames[LPassedParamIndex]) then
          begin
            FDataSet.Params[LDatasetParamIndex].Value := AParamValues[LPassedParamIndex];
            Break;
          end;
        end;
      except
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelDataset.AreAllParamsBound(AReportError: boolean = False): boolean;
const OPNAME = 'TModelDataset.AreAllParamsBound';
var LIndex: integer;
begin
  Result := True;
  try
    for LIndex := 0 to FDataSet.ParamCount - 1 do
    begin
      //if(FDataSet.Params[LIndex].Value = Unassigned) then
      if(FDataSet.Params[LIndex].Value = Null) then
      begin
        Result := False;
          if AReportError then
            ReportError(Format('Parameter [%s] has not been set.', [FDataSet.Params[LIndex].Name]), OPNAME);
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelDataset.ExecSQL(AIgnoreErrors : boolean = false);
begin
  // Do not use the general exception handling here. Exception will be caught by
  // the calling function
  try
    FDataSet.ExecSQL;
  except on E: Exception do
    if not(AIgnoreErrors) then
      raise Exception.CreateFmt('Error attempting to execute SQL [%s]. ' + E.Message, [FDataSet.SQL.Text]);
  end
end;

function TModelDataset.IsReadOnly: boolean;
const OPNAME = 'TModelDataset.IsReadOnly';
begin
  Result := True;
  try
    Result := FReadOnly;
    if FDataSet.Active then
      FReadOnly := not FDataSet.CanModify;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelDataset.SetReadOnly(AReadOnly: boolean);
const OPNAME = 'TModelDataset.SetReadOnly';
begin
  try
    FReadOnly := AReadOnly;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelDataset.BuildSQL: string;
const OPNAME = 'TModelDataset.BuildSQL';
begin
  Result := '';
  try
    Result := GetSelectClause + ' '+
              GetFromClause   + ' '+
              GetWhereClause  + ' '+
              GetGroupOrOrderClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelDataset.GetSelectClause: string;
const OPNAME = 'TModelDataset.GetSelectClause';
begin
  Result := '';
  try
    Result := '';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelDataset.GetFromClause: string;
const OPNAME = 'TModelDataset.GetFromClause';
begin
  Result := '';
  try
    Result := '';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelDataset.GetWhereClause: string;
const OPNAME = 'TModelDataset.GetWhereClause';
begin
  Result := '';
  try
    Result := '';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelDataset.GetGroupOrOrderClause: string;
const OPNAME = 'TModelDataset.GetGroupOrOrderClause';
begin
  Result := '';
  try
    Result := '';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelDataset.GetTableName: string;
const OPNAME = 'TModelDataset.GetTableName';
var
  LPos : integer;
  LSQL : string;
begin
  Result := '';
  try
    LSQL := Trim(FDataSet.SQL.Text);
    if(Pos('SELECT',LSQL) = 1) then
    begin
      LPos := Pos(' FROM ',LSQL);
      if(LPos > 0) then
      begin
        LSQL := Trim(Copy(LSQL,LPos+6,Length(LSQL)));
        LPos := Pos(' ',LSQL);
        if(LPos > 0) then
        begin
          Result := Trim(Copy(LSQL,1,LPos));
        end;
      end;
    end
    else if(Pos('UPDATE',LSQL) = 1) then
    begin
      LSQL := Trim(Copy(LSQL,7,Length(LSQL)));
      LPos := Pos(' ',LSQL);
      if(LPos > 0) then
      begin
        Result := Trim(Copy(LSQL,1,LPos));
      end;
    end
    else if(Pos('INSERT',LSQL) = 1) then
    begin
      LPos := Pos(' INTO ',LSQL);
      if(LPos > 0) then
      begin
        LSQL := Trim(Copy(LSQL,LPos+6,Length(LSQL)));
        LPos := Pos(' ',LSQL);
        if(LPos > 0) then
        begin
          Result := Trim(Copy(LSQL,1,LPos));
        end;
      end;
    end
    else
      Result := '';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelDataset.ClearQueryParams (AParamType :TParamType=prAll): boolean;
const OPNAME = 'TModelDataset.ClearQueryParams';
var
  LIndex      : integer;
  LTableName  : string;
  LParaName  : string;
  LTableFieldsDef : TAbstractTableFieldsDef;
  LTableFieldDef : TAbstractTableFieldDef;
begin
  Result := False;
  try
    LTableName := GetTableName;
    if(LTableName <> '') then
    begin
      LTableFieldsDef := AppModules.DBTableFieldsDefList.TableFieldsDefByName[LTableName];
      if(LTableFieldsDef <> nil) then
      begin
        for LIndex := 0 to FDataSet.ParamCount -1 do
        begin
          LParaName      := FDataSet.Params[LIndex].Name;
          LTableFieldDef := LTableFieldsDef.FieldByName[LParaName];
          if(LTableFieldDef <> nil) then
          begin
            if UpperCase(LTableFieldDef.FieldType)  = 'INTEGER' then
               FDataSet.Params[LIndex].DataType  := ftInteger
            else if UpperCase(LTableFieldDef.FieldType)  = 'FLOAT' then
               FDataSet.Params[LIndex].DataType  := ftFloat
            else if UpperCase(LTableFieldDef.FieldType)  = 'STRING' then
            begin
              if(LTableFieldDef.FieldLength = 1) then
                FDataSet.Params[LIndex].DataType  := ftFixedChar
              else
               FDataSet.Params[LIndex].DataType  := ftString;
            end;
               FDataSet.Params[LIndex].DataType  := ftString;
            FDataSet.Params[LIndex].Value := Unassigned;
          end
          else
          begin
            raise Exception.Create('Parameter named('+LParaName+') has not yet been defined in the parameter list.');
          end;
        end;
        Result := True;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelDataset.ClearQueryTypeParams(AParamType: TParamType): boolean;
const OPNAME = 'TModelDataset.ClearQueryParams';
Var
 LCount: integer;
begin
  Result := False;
  try
    if(AParamType = prAll) then Exit;
    for LCount := 0 to FDataSet.ParamCount -1 do
    begin
      if(UpperCase(FDataSet.Params[LCount].Name) = UpperCase('Model')) then
        FDataSet.Params[LCount].DataType  := ftString
      else
      if(UpperCase(FDataSet.Params[LCount].Name) = UpperCase('StudyAreaName')) then
        FDataSet.Params[LCount].DataType  := ftString
      else
      if(UpperCase(FDataSet.Params[LCount].Name) = UpperCase('SubArea')) then
        FDataSet.Params[LCount].DataType  := ftString
      else
      if(UpperCase(FDataSet.Params[LCount].Name) = UpperCase('Scenario')) then
        FDataSet.Params[LCount].DataType  := ftString
      else
      if(UpperCase(FDataSet.Params[LCount].Name) = UpperCase('Identifier')) then
        FDataSet.Params[LCount].DataType  := ftInteger
      else
      if(UpperCase(FDataSet.Params[LCount].Name) = UpperCase('Comment')) then
        FDataSet.Params[LCount].DataType  := ftString
      else
      if(UpperCase(FDataSet.Params[LCount].Name) = UpperCase('Fullname')) then
        FDataSet.Params[LCount].DataType  := ftString
      else
      if(UpperCase(FDataSet.Params[LCount].Name) = UpperCase('MasterChannelType')) then
        FDataSet.Params[LCount].DataType  := ftString
      else
      if(UpperCase(FDataSet.Params[LCount].Name) = UpperCase('Stochastic')) then
        FDataSet.Params[LCount].DataType  := ftString
      else
      case AParamType of
        prInt  : FDataSet.Params[LCount].DataType  := ftInteger;
        prFloat: FDataSet.Params[LCount].DataType  := ftFloat;
      else
       FDataSet.Params[LCount].DataType  := ftString;
      end;
      FDataSet.Params[LCount].Value := Unassigned;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelDataSet.ClearDataset: boolean;
const OPNAME = 'TModelDataSet.ClearDataset';
begin
  Result := False;
  try

    if not FDataset.Active then
     raise Exception.Create('The parameter dataset is not active. '+
                           'You can only clear an active dataset.');

    //ADataset.DisableControls;
    //try
    FDataSet.First;
    while not FDataSet.IsEmpty do
     FDataSet.Delete;
    FDataSet.Close;
    FDataSet.Open;
    //finally
    //  ADataset.EnableControls;
    //end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelDataset.SetParamValue(AParamName: string; AValue: WideString; ADataType: TFieldType; ABlobField: boolean=False; ABlobLength: integer=0);
const OPNAME = 'TModelDataset.SetParamValue';
begin
  try
    if (FDataSet.Params.FindParam(AParamName) = nil) then
     raise Exception.Create('The parameter '+AParamName + ' does not exist in the dataset.');
    if (AValue = 'NULL') then
    begin
      FDataSet.ParamByName(AParamName).DataType := ADataType;
      FDataSet.ParamByName(AParamName).Value := Unassigned;
    end else begin
      if ABlobField then
      begin
        //FDataSet.ParamByName(AParamName).SetBlobData(@AValue, ABlobLength)
        FDataSet.ParamByName(AParamName).Value := AValue
      end else begin
        FDataSet.ParamByName(AParamName).DataType := ADataType;
        FDataSet.ParamByName(AParamName).Value := AValue;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelDataset.GetParamByName(AParamName: string): TDWAParameter;
const OPNAME = 'TModelDataset.GetParamByName';
begin
  Result := nil;
  try
    Result := TDWAParameter(FDataSet.Parameters.ParamByName(AParamName));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelDataset.ReplaceSQLParam(AParamName, AParamValue: String);
const OPNAME = 'TModelDataset.ReplaceSQLParam';
var
  LPos: Integer;
  LOriginalSQL, LNewSQL: String;
begin
  try
    LNewSQL := FDataSet.SQL.Text;
    LOriginalSQL := FDataSet.SQL.Text;
    LPos := Pos(AParamName, LOriginalSQL);
    if (LPos > 0) then
    begin
      LNewSQL := StringReplace(LOriginalSQL, ':' + AParamName, '''' + AParamValue + '''', [rfReplaceAll, rfIgnoreCase]);
      FDataSet.SQL.Text := LNewSQL;
    end;
  except on E: Exception do
    raise Exception.CreateFmt('Error attempting to replace SQL parameter [%s] with value [%s] in SQL [%s]. ' + E.Message,
                              [AParamName, AParamValue, LOriginalSQL]);
  end;
end;

procedure TModelDataset.ReplaceSQLParamArray(AParamName, AParamValue: array of String);
const OPNAME = 'TModelDataset.ReplaceSQLParamArray';
var
  I: Integer;
  LOriginalSQL: String;
begin
  try
    LOriginalSQL := FDataSet.SQL.Text;
    for I := 0 to Length(AParamName) - 1 do
      ReplaceSQLParam(AParamName[I], AParamValue[I]);
  except on E: Exception do
    raise Exception.CreateFmt('Error attempting to replace SQL parameters in [%s]. ' + E.Message, [LOriginalSQL]);
  end;
end;

end.
