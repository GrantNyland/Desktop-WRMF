unit UMultiRestrictionSQLAgent;

interface
  uses
    Classes,
    VoaimsCom_TLB,
    UAbstractObject;
type
  TMultiRestrictionSQLAgent = class(TAbstractSQLAgent)
  protected

    function GetScenarioWhereClause: string;
  public
    procedure LoadContextData(AContextData: TStringList; AIdentifier, AFieldNameIdentifier : string); overload;
    procedure LoadContextData(AContextData: TStringList; AIdentifier : string); overload;
    function GetMultiResChannelDataSQL: String;
    function GetMultiRestrictionElevationDataSQL(AIdentifier, AChannelNo: Integer; AReseviorNo: Integer): string;
    function GetMultiRestrictionFactorDataSQL(AIdentifier, AChannelNo: Integer; AReseviorNo: Integer): string;
    function GetDeleteMultiRestrictionSQL(AIdentifier, AChannelNo, AReseviorNo: integer): string;
    function GetDeleteMultiRestrictionElevationSQL(AIdentifier, AChannelNo: Integer; AReseviorNo: Integer): string;
    function GetDeleteMultiRestrictionFactorSQL(AIdentifier, AChannelNo: Integer; AReseviorNo: Integer): string;


  end;
implementation
uses
  Math,
  SysUtils,
  UConstants,
  UDataSetType,
  UErrorHandlingOperations;


procedure TMultiRestrictionSQLAgent.LoadContextData(AContextData: TStringList; AIdentifier, AFieldNameIdentifier : string);
const OPNAME = 'TMultiRestrictionSQLAgent.LoadContextData';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model=' +FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' +FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea=' +FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario=' +FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('Identifier=' + AIdentifier);
    AContextData.Add('FieldNameIdentifier=' + AFieldNameIdentifier);
  except on E : Exception do HandleError(E,OPNAME); end;
end;


function TMultiRestrictionSQLAgent.GetDeleteMultiRestrictionSQL(AIdentifier, AChannelNo: Integer; AReseviorNo: Integer): String;
const OPNAME ='TMultiRestrictionSQLAgent.GetDeleteMultiRestrictionSQL';
begin
    Result := '';
    try
      Result :=  'DELETE FROM MultiResChannelCurtail' +
      ' WHERE Model       =  ' + QuotedStr(FAppModules.StudyArea.ModelCode)+
      ' AND StudyAreaName =  ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
      ' AND SubArea       =  ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)+
      ' AND Scenario      =  ' + QuotedStr(FAppModules.StudyArea.ScenarioCode) +
      ' AND ChannelNo     =  ' + IntToStr(AChannelNo) +
      ' AND ReservoirNo   =  ' + IntToStr(AReseviorNo) +
      ' AND Identifier    =  ' + IntToStr(AIdentifier);
    except on E: Exception do HandleError(E, OPNAME)
  end;
end;

function TMultiRestrictionSQLAgent.GetDeleteMultiRestrictionElevationSQL(AIdentifier, AChannelNo: Integer; AReseviorNo: Integer): string;
const OPNAME ='TMultiRestrictionSQLAgent.GetDeleteMultiRestrictionElevationSQL';
begin
  Result := '';
  try
    Result := ' DELETE FROM MultiResChannelElevation' +
      ' WHERE Model       =  ' + QuotedStr(FAppModules.StudyArea.ModelCode)+
      ' AND StudyAreaName =  ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
      ' AND SubArea       =  ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)+
      ' AND Scenario      =  ' + QuotedStr(FAppModules.StudyArea.ScenarioCode) +
      ' AND ChannelNo     =  ' + IntToStr(AChannelNo) +
      ' AND ReservoirNo   =  ' + IntToStr(AReseviorNo) +
      ' AND Identifier     =  ' + IntToStr(AIdentifier);
    except on E: Exception do HandleError(E, OPNAME)
  end;
end;

function TMultiRestrictionSQLAgent.GetDeleteMultiRestrictionFactorSQL(AIdentifier, AChannelNo: Integer; AReseviorNo: Integer): string;
const OPNAME ='TMultiRestrictionSQLAgent.GetDeleteMultiRestrictionFactorSQL';
begin
  Result := '';
  try
      Result := ' DELETE FROM MultiResChannelFactor' +
      ' WHERE Model       =  ' + QuotedStr(FAppModules.StudyArea.ModelCode)+
      ' AND StudyAreaName =  ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
      ' AND SubArea       =  ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)+
      ' AND Scenario      =  ' + QuotedStr(FAppModules.StudyArea.ScenarioCode) +
      ' AND ChannelNo    =  ' + IntToStr(AChannelNo) +
      ' AND ReservoirNo = ' + IntToStr(AReseviorNo) +
      ' AND Identifier     =  ' + IntToStr(AIdentifier);
    except on E: Exception do HandleError(E, OPNAME)
  end;
end;

function TMultiRestrictionSQLAgent.GetScenarioWhereClause: string;
const OPNAME =
      'TMultiResChannelCurtailmentDefinitionsSQLAgent.getScenarioWhereClause';
begin
    Result := '';
    try
    Result := '(Model  =' + QuotedStr(FAppModules.StudyArea.ModelCode)   + ') AND ' +
      ' (StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
      ' (SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
      ' (Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ' )';
    except on E: Exception do HandleError(E, OPNAME)
  end;
end;

procedure TMultiRestrictionSQLAgent.LoadContextData(AContextData: TStringList;AIdentifier: string);
const OPNAME = 'TMultiRestrictionSQLAgent.LoadContextData';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model=' +FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' +FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea=' +FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario=' +FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('Identifier=' + AIdentifier);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TMultiRestrictionSQLAgent.GetMultiResChannelDataSQL: String;
const OPNAME = 'TMultiResChannelCurtailmentDefinitionsSQLAgent.GetMultiResChannelDataSQL';
begin
  Result := '';
  try

    Result := 'SELECT MultiResChannelCurtail.Identifier, '+
      '  MultiResChannelCurtail.ChannelNo,MultiResChannelCurtail.ReservoirNo'+
      ' ,DecisionMonth,StartMonth'+
      ' ,Elevation01,Elevation02,Elevation03,Elevation04,Elevation05,Elevation06,Elevation07,Elevation08,Elevation09,Elevation10'+
      ' ,Factor01,Factor02,Factor03,Factor04,Factor05,Factor06,Factor07,Factor08,Factor09,Factor10' +
      '  FROM '+
      '  MultiResChannelCurtail'+
      ' ,MultiResChannelElevation'+
      ' ,MultiResChannelFactor'+
      ' WHERE MultiResChannelCurtail.Model            =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
      ' AND MultiResChannelCurtail.StudyAreaName      =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
      ' AND MultiResChannelCurtail.SubArea            =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
      ' AND MultiResChannelCurtail.Scenario           =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
      ' AND MultiResChannelCurtail.ChannelNo          = MultiResChannelElevation.ChannelNo'+
      ' AND MultiResChannelCurtail.ReservoirNo        = MultiResChannelElevation.ReservoirNo'+
      ' AND MultiResChannelElevation.Model         = MultiResChannelCurtail.Model'+
      ' AND MultiResChannelElevation.StudyAreaName = MultiResChannelCurtail.StudyAreaName'+
      ' AND MultiResChannelElevation.SubArea       = MultiResChannelCurtail.SubArea'+
      ' AND MultiResChannelElevation.Scenario      = MultiResChannelCurtail.Scenario'+
      ' AND MultiResChannelElevation.ChannelNo          = MultiResChannelFactor.ChannelNo'+
      ' AND MultiResChannelElevation.ReservoirNo        = MultiResChannelFactor.ReservoirNo'+
      ' AND MultiResChannelFactor.Model         = MultiResChannelCurtail.Model'+
      ' AND MultiResChannelFactor.StudyAreaName = MultiResChannelCurtail.StudyAreaName'+
      ' AND MultiResChannelFactor.SubArea       = MultiResChannelCurtail.SubArea '+
      ' AND MultiResChannelFactor.Scenario      = MultiResChannelCurtail.Scenario';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMultiRestrictionSQLAgent.GetMultiRestrictionElevationDataSQL(AIdentifier, AChannelNo: Integer; AReseviorNo: Integer): string;
const OPNAME = 'TMultiRestrictionSQLAgent.GetMultiRestrictionElevationDataSQL';
begin
  Result := '';
  try
    Result := 'SELECT * FROM MultiResChannelElevation WHERE ' +
              getScenarioWhereClause +
              ' ( AND ChannelNo    =  ' + IntToStr(AChannelNo) +')'+
              ' ( AND ReservoirNo = ' + IntToStr(AReseviorNo) +')'+
              ' ( AND Identifier     =  ' + IntToStr(AIdentifier) +')';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMultiRestrictionSQLAgent.GetMultiRestrictionFactorDataSQL(AIdentifier, AChannelNo: Integer; AReseviorNo: Integer): string;
const OPNAME = 'TMultiRestrictionSQLAgent.GetMultiRestrictionFactorDataSQL';
begin
  Result := '';
  try
    Result := 'SELECT * FROM MultiResChannelFactor WHERE '
    + getScenarioWhereClause+
              ' ( AND ChannelNo    =  ' + IntToStr(AChannelNo) +')'+
              ' ( AND ReservoirNo = ' + IntToStr(AReseviorNo) +')'+
              ' ( AND Identifier     =  ' + IntToStr(AIdentifier) +')';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
