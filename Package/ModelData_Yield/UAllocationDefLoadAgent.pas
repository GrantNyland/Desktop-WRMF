{******************************************************************************}
{*  UNIT      : Contains the class TAllocationDefLoadAgent.                   *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2005/06/05                                                    *}
{*  COPYRIGHT : Copyright © 2005 DWAF                                         *}
{******************************************************************************}

unit UAllocationDefLoadAgent;

interface

uses
  Classes,
  UAllocationdefinitionData,
  UAbstractObject;

type
  TAllocationDefLoadAgent = class(TAbstractAppObject)
  protected
    function GetScenarioWhereClause: string;
    function GetLastAllocationDefinitionID : integer;
    function GetLastSubSystemID : integer;
    function GetLastDemandDefID : integer;
    function GetLastSupportSubSystemID : integer;
    function GetStartYearAndMonthFromDamDotDat(var ASwitchDefID : integer; var AStartYear : integer; var AStartMonth : integer;
                                               const AAllocDefFileName : string) : boolean;
    function PopulateAllocationDefinition (ADataSet  : TAbstractModelDataset;
                                           AAllocDef : TAllocationDefinition): boolean;
    function GetLastUserCategoryID : integer;
    function GetLastAllocationLevelID : integer;
    function GetLastFixedPositionID (AAllocDefID : integer) : integer;
    function GetLastSpecificOrderID : integer;
    function GetLastSupportChannelID : integer;
    function LoadUserCategories (AAllocDef : TAllocationDefinition) : boolean;
    function PopulateUserCategory (ADataSet      : TAbstractModelDataset;
                                   ANrOfRIs      : integer;
                                   AUserCategory : TUserCategory): boolean;
    function LoadAllocationLevels (AAllocDef : TAllocationDefinition) : boolean;
    function PopulateAllocationLevel (ADataSet    : TAbstractModelDataset;
                                      ANrOfRIs    : integer;
                                      AAllocLevel : TAllocationLevel): boolean;
    function LoadSubSystems (AAllocDef : TAllocationDefinition) : boolean;
    function PopulateSubSystem (ADataSet   : TAbstractModelDataset;
                                ASubSystem : TSubSystem): boolean;
    function LoadCoefficients (AAllocDef  : TAllocationDefinition;
                               ASubSystem : TSubSystem) : boolean;
    function LoadSupportStrategy (AAllocDef : TAllocationDefinition) : boolean;

    function LoadSupportChannels (AAllocDef : TAllocationDefinition) : boolean;
    function PopulateSupportChannel (ADataSet        : TAbstractModelDataset;
                                     ASupportChannel : TSupportChannel): boolean;
    function LoadDemandDefinitions (AAllocDef : TAllocationDefinition) : boolean;
    function PopulateDemandDefinition (ADataSet          : TAbstractModelDataset;
                                       ADemandDefinition : TDemandDefinition): boolean;
    function LoadSupportSubSystems (AAllocDef         : TAllocationDefinition;
                                    ADemandDefinition : TDemandDefinition) : boolean;
    function PopulateSupportSubSystem (ADataSet          : TAbstractModelDataset;
                                       ASupportSubSystem : TSupportSubSystem): boolean;
  public
    function LoadAllocationDefinitions (AAllocDefList : TAllocationDefinitionsList): boolean;
    function InsertAllocationDefinition (var AAllocDefID : integer) : Boolean;
    function DeleteAllocationDefinition (AAllocDefID : integer): boolean;

    function InsertUserCategory ( AAllocDefID : integer; var AUserCategoryID : integer): boolean;
    function DeleteCategoryByID ( AUserCategoryID,AAllocDefID : integer ) : boolean;

    function InsertAllocationLevel ( AAllocDefID : integer; var AAllocationLevelID : integer): boolean;
    function DeleteAllocationLevelByID ( AAllocationLevelID,AAllocDefID : integer ) : boolean;

    function InsertSubSystem  (AAllocDefID      : integer;
                               var ASubSystemID : integer): boolean;
    function DeleteSubSystemByID ( ASubSystemID, AAllocDefID : integer ) : boolean;

    function InsertSupportChannel (AAllocDefID           : integer;
                                   var ASupportChannelID : integer ) : boolean;
    function DeleteSupportChannelByID (ASupportChannelID, AAllocDefID : integer ) : boolean;

    function InsertDemandDefinition ( AAllocDefID : integer; var ADemandDefID : integer ) : boolean;
    function DeleteDemandDefinitionByID ( ADemandDefID, AAllocDefID : integer ) : boolean;

    function InsertSupportSubSystem ( AAllocDefID , ADemandDefID : integer; var ASupportSubSystemID : integer ) : boolean;
    function DeleteSupportSubSystemByID ( ASupportSubSystemID, AAllocDefID, ADemandDefID : integer ) : boolean;

    function InsertCoefficient ( AAllocDefID ,ASubSystemID, AStartPercNr,
                                 ACurveSetNr, ALoadCaseNr : integer ) : boolean;
    function DeleteCoeficient ( AAllocDefID ,ASubSystemID, AStartPercNr,
                                ACurveSetNr, ALoadCaseNr : integer ) : boolean;
    function InsertFixedPosition (AAllocDefID          : integer;
                                  var AFixedPositionID : integer ) : boolean;
    function DeleteFixedPosition ( AAllocDefID : integer; AFixedPositionID : integer ) : boolean;

    function InsertSpecificOrder (AAllocDefID          : integer;
                                  var ASpecificOrderID : integer) : boolean;
    function DeleteSpecificOrder ( AAllocDefID, ASpecificOrderID : integer ) : boolean;
    //Allocation Definition Load context data
    procedure LoadContextData_AllocDefID (AContextData : TStringList;
                                          AAllocDefID  : string);

    procedure LoadContextData_FamilyFileInDamfile(AContextData : TStringList;
                                                  ASwitchDefID  : string);

    procedure LoadContextData_AllocDefIDFieldNameID (AContextData : TStringList;
                                                     AAllocDefID  : string;
                                                     AFieldNameID : string);
    //User Category Load context data
    procedure LoadContextData_UserCategory (AContextData    : TStringList;
                                            AAllocDefID     : string;
                                            AUserCategoryID : string );
    procedure LoadContextData_UserCategoryFieldNameID (AContextData    : TStringList;
                                                       AAllocDefID     : string;
                                                       AUserCategoryID : string;
                                                       AFieldNameID    : string);
    // Allocation Level Load Context data
    procedure LoadContextData_AllocationLevel (AContextData       : TStringList;
                                               AAllocDefID        : string;
                                               AAllocationLevelID : string);
    procedure LoadContextData_AllocationLevelFieldNameID (AContextData       : TStringList;
                                                          AAllocDefID        : string;
                                                          AAllocationLevelID : string;
                                                          AFieldNameID    : string);
    //SubSystem Load context data
    procedure LoadContextData_SubSystem ( AContextData : TStringList;
                                          AAllocDefID  : string;
                                          ASubSystemID : string);
    procedure LoadContextData_SubSystemFieldNameID (AContextData : TStringList;
                                                    AAllocDefID  : string;
                                                    ASubSystemID : string;
                                                    AFieldNameID : string);
    // Support Channel Load Context Data....
    procedure LoadContextData_SupportChannel ( AContextData      : TStringList;
                                               AAllocDefID       : string;
                                               ASupportChannelID : string);
    procedure LoadContextData_SupportChannelFieldNameID ( AContextData      : TStringList;
                                                          AAllocDefID       : string;
                                                          ASupportChannelID : string;
                                                          AFieldNameID      : string);
    // Support Sub-System Load Context Data...
    procedure LoadContextData_SupportSubSystem ( AContextData : TStringList; AAllocDefID : string;
                                                 ADemandDefID : string; ASupportSubSystemID : string );
    procedure LoadContextData_SupportSubSystemFieldNameID( AContextData : TStringList; AAllocDefID : string;
                                                           ADemandDefID : string; ASupportSubSystemID : string;
                                                           AFieldNameID : string );
    procedure LoadContextData_DemandDefinition ( AContextData : TStringList; AAllocDefID : string;
                                                 ADemandDefID : string );
    procedure LoadContextData_Coefficient ( AContextData : TStringList;AAllocDefID,
                                           ASubSystemID,AStartPercs,ACurveSets,ALoadCases : string) ;
    procedure LoadContextData_FixedPosition ( AContextData     : TStringList; AAllocDefID,
                                              AFixedPositionID : string );
    procedure LoadContextData_SpecificOrder( AContextData     : TStringList; AAllocDefID,
                                             ASpecificOrderID : string );

  end;

implementation

uses
  Math,
  SysUtils,
  UConstants,
  UDataSetType,
  UErrorHandlingOperations;

procedure TAllocationDefLoadAgent.LoadContextData_AllocDefID (AContextData : TStringList;
                                                              AAllocDefID  : string);
const OPNAME = 'TAllocationDefLoadAgent.LoadContextData_AllocDefID';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='         + FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' + FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='       + FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='      + FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('AllocDefID='    + AAllocDefID);
  except on E : Exception do HandleError(E,OPNAME); end;
end;
                                 // SwitchDefID

procedure TAllocationDefLoadAgent.LoadContextData_FamilyFileInDamfile(AContextData : TStringList;
                                                                      ASwitchDefID  : string);
const OPNAME = 'TAllocationDefLoadAgent.LoadContextData_FamilyFileInDamfile';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='         + FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' + FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='       + FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='      + FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('SwitchDefID='   + ASwitchDefID);
    AContextData.Add('FileGroupID=1');
  except on E : Exception do HandleError(E,OPNAME); end;
end;


procedure TAllocationDefLoadAgent.LoadContextData_AllocDefIDFieldNameID (AContextData : TStringList;
                                                                         AAllocDefID  : string;
                                                                         AFieldNameID : string);
const OPNAME = 'TAllocationDefLoadAgent.LoadContextData_AllocDefIDFieldNameID';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='         + FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' + FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='       + FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='      + FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('AllocDefID='    + AAllocDefID);
    AContextData.Add('FieldNameIdentifier=' + AFieldNameID);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TAllocationDefLoadAgent.LoadContextData_UserCategory (AContextData    : TStringList;
                                                                AAllocDefID     : string;
                                                                AUserCategoryID : string);
const OPNAME = 'TAllocationDefLoadAgent.LoadContextData_UserCategory';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='              + FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName='      + FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='            + FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='           + FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('AllocDefID='         + AAllocDefID);
    AContextData.Add('UserCategoryID='     + AUserCategoryID);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TAllocationDefLoadAgent.LoadContextData_AllocationLevel (AContextData       : TStringList;
                                                                   AAllocDefID        : string;
                                                                   AAllocationLevelID : string);
const OPNAME = 'TAllocationDefLoadAgent.LoadContextData_AllocationLevel';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='              + FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName='      + FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='            + FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='           + FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('AllocDefID='         + AAllocDefID);
    AContextData.Add('AllocLevelID='       + AAllocationLevelID);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TAllocationDefLoadAgent.LoadContextData_AllocationLevelFieldNameID (AContextData       : TStringList;
                                                                              AAllocDefID        : string;
                                                                              AAllocationLevelID : string;
                                                                              AFieldNameID       : string);
const OPNAME = 'TAllocationDefLoadAgent.LoadContextData_AllocationLevelFieldNameID';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='              + FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName='      + FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='            + FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='           + FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('AllocDefID='         + AAllocDefID);
    AContextData.Add('AllocLevelID='       + AAllocationLevelID);
    AContextData.Add('FieldNameIdentifier=' + AFieldNameID);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TAllocationDefLoadAgent.LoadContextData_UserCategoryFieldNameID (AContextData    : TStringList;
                                                                           AAllocDefID     : string;
                                                                           AUserCategoryID : string;
                                                                           AFieldNameID    : string);
const OPNAME = 'TAllocationDefLoadAgent.LoadContextData_UserCategoryFieldNameID';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='              + FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName='      + FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='            + FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='           + FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('AllocDefID='         + AAllocDefID);
    AContextData.Add('UserCategoryID='     + AUserCategoryID);
    AContextData.Add('FieldNameIdentifier=' + AFieldNameID);
  except on E : Exception do HandleError(E,OPNAME); end;
end;


procedure TAllocationDefLoadAgent.LoadContextData_SubSystem (AContextData : TStringList;
                                                             AAllocDefID  : string;
                                                             ASubSystemID : string);
const OPNAME = 'TAllocationDefLoadAgent.LoadContextData_SubSystem';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='              + FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName='      + FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='            + FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='           + FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('AllocDefID='         + AAllocDefID);
    AContextData.Add('SubSystemID='       + ASubSystemID);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TAllocationDefLoadAgent.LoadContextData_SubSystemFieldNameID (AContextData : TStringList;
                                                                        AAllocDefID  : string;
                                                                        ASubSystemID : string;
                                                                        AFieldNameID : string);
const OPNAME = 'TAllocationDefLoadAgent.LoadContextData_SubSystemFieldNameID';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='              + FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName='      + FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='            + FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='           + FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('AllocDefID='         + AAllocDefID);
    AContextData.Add('SubSystemID='        + ASubSystemID);
    AContextData.Add('FieldNameIdentifier=' + AFieldNameID);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TAllocationDefLoadAgent.LoadContextData_SupportChannel ( AContextData      : TStringList;
                                                                   AAllocDefID       : string;
                                                                   ASupportChannelID : string);
const OPNAME = 'TAllocationDefLoadAgent.LoadContextData_SupportChannel';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='              + FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName='      + FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='            + FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='           + FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('AllocDefID='         + AAllocDefID);
    AContextData.Add('SupportChannelID='   + ASupportChannelID);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TAllocationDefLoadAgent.LoadContextData_SupportChannelFieldNameID ( AContextData      : TStringList;
                                                                              AAllocDefID       : string;
                                                                              ASupportChannelID : string;
                                                                              AFieldNameID      : string);
const OPNAME = 'TAllocationDefLoadAgent.LoadContextData_SupportChannelFieldNameID';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='              + FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName='      + FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='            + FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='           + FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('AllocDefID='         + AAllocDefID);
    AContextData.Add('SupportChannelID='   + ASupportChannelID);
    AContextData.Add('FieldNameIdentifier='+ AFieldNameID);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TAllocationDefLoadAgent.LoadContextData_SupportSubSystem ( AContextData : TStringList; AAllocDefID : string;
                                                                     ADemandDefID : string; ASupportSubSystemID : string );
const OPNAME = 'TAllocationDefLoadAgent.LoadContextData_SupportSubSystem';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='              + FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName='      + FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='            + FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='           + FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('AllocDefID='         + AAllocDefID);
    AContextData.Add('DemandDefID='        + ADemandDefID);
    AContextData.Add('SupportSubSystemID=' + ASupportSubSystemID);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TAllocationDefLoadAgent.LoadContextData_SupportSubSystemFieldNameID( AContextData : TStringList; AAllocDefID : string;
                                                                               ADemandDefID : string; ASupportSubSystemID : string;
                                                                               AFieldNameID : string );
const OPNAME = 'TAllocationDefLoadAgent.LoadContextData_SupportSubSystemFieldNameID';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='              + FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName='      + FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='            + FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='           + FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('AllocDefID='         + AAllocDefID);
    AContextData.Add('DemandDefID='        + ADemandDefID);
    AContextData.Add('SupportSubSystemID=' + ASupportSubSystemID);
    AContextData.Add('FieldNameIdentifier='+ AFieldNameID);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TAllocationDefLoadAgent.LoadContextData_DemandDefinition ( AContextData : TStringList; AAllocDefID : string;
                                                                     ADemandDefID : string );
const OPNAME = 'TAllocationDefLoadAgent.LoadContextData_DemandDefinition';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='              + FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName='      + FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='            + FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='           + FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('AllocDefID='         + AAllocDefID);
    AContextData.Add('DemandDefID='        + ADemandDefID);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TAllocationDefLoadAgent.LoadContextData_Coefficient ( AContextData : TStringList;AAllocDefID,
                                                               ASubSystemID,AStartPercs,ACurveSets,ALoadCases : string);
const OPNAME = 'TAllocationDefLoadAgent.LoadContextData_Coefficient';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='              + FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName='      + FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='            + FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='           + FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('AllocDefID='         + AAllocDefID);
    AContextData.Add('SubSystemID='        + ASubSystemID);
    AContextData.Add('StartStorageNr='     + AStartPercs);
    AContextData.Add('CurveSetNr='         + ACurveSets);
    AContextData.Add('LoadCaseNr='         + ALoadCases);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TAllocationDefLoadAgent.LoadContextData_FixedPosition ( AContextData : TStringList; AAllocDefID,
                                              AFixedPositionID : string );
const OPNAME = 'TAllocationDefLoadAgent.LoadContextData_FixedPosition';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='              + FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName='      + FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='            + FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='           + FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('AllocDefID='         + AAllocDefID);
    AContextData.Add('FixedPositionID='    + AFixedPositionID);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TAllocationDefLoadAgent.LoadContextData_SpecificOrder( AContextData : TStringList; AAllocDefID,
                                                                 ASpecificOrderID : string );
const OPNAME = 'TAllocationDefLoadAgent.LoadContextData_SpecificOrder';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='              + FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName='      + FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='            + FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='           + FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('AllocDefID='         + AAllocDefID);
    AContextData.Add('SpecificOrderID='  + ASpecificOrderID);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TAllocationDefLoadAgent.GetScenarioWhereClause : string;
const OPNAME = 'TAllocationDefLoadAgent.GetScenarioWhereClause';
begin
  Result := '';
  try
    Result :=
      ' (Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND ' +
      ' (StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
      ' (SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
      ' (Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAllocationDefLoadAgent.LoadAllocationDefinitions (AAllocDefList : TAllocationDefinitionsList): boolean;
const OPNAME = 'TAllocationDefLoadAgent.LoadAllocationDefinitions';
var
  lDataSet     : TAbstractModelDataset;
  lAllocDef    : TAllocationDefinition;
  lSQL         : string;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(lDataSet) and ( AAllocDefList <> nil ) then
      begin
        lSQL := 'SELECT * FROM FMAllocationDefinition WHERE ' +
                GetScenarioWhereClause ;
        lDataSet.SetSQL(lSQL);
        lDataset.DataSet.Open;
        Result := True;
        AAllocDefList.Initialise;
        while (NOT lDataset.DataSet.EOF) do
        begin
          lAllocDef := AAllocDefList.CreateAllocationDefinition;
          if (NOT PopulateAllocationDefinition(lDataset, lAllocDef)) then
            AAllocDefList.DeleteAllocationDefinitionWithID(lAllocDef.AllocationDefinitionID)
          else
          begin
            Result := Result AND LoadUserCategories(lAllocDef);
            Result := Result AND LoadAllocationLevels(lAllocDef);
            Result := Result AND LoadSubSystems(lAllocDef);
            Result := Result AND LoadSupportStrategy(lAllocDef);
            Result := Result AND LoadSupportChannels(lAllocDef);
            Result := Result AND LoadDemandDefinitions(lAllocDef);
          end;
          lDataset.DataSet.Next;
        end;
        lDataset.DataSet.Close;
      end;
    finally
      lDataset.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TAllocationDefLoadAgent.GetStartYearAndMonthFromDamDotDat(var ASwitchDefID : integer; var AStartYear : integer; var AStartMonth : integer;
                                               const AAllocDefFileName : string) : boolean;
const OPNAME = 'TAllocationDefLoadAgent.GetStartYearAndMonthFromDamDotDat';
var
  LSQL : string;
  LDataSet : TAbstractModelDataset;
begin
  Result := False;
  try
    try
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      LSQL := ' SELECT distinct sf.FileName, sf.SwitchDefID,sd.StartYear,sd.StartMonth '+
              ' FROM ReservoirSwitchFileName sf, ReservoirSwitchDefinition sd '+
              ' WHERE sf.FileName = '+ QuotedStr(AAllocDefFileName) +
              ' AND sf.Model       =    ' + QuotedStr(FAppModules.StudyArea.ModelCode) +
              ' AND sf.StudyAreaName =  '+ QuotedStr(FAppModules.StudyArea.StudyAreaCode) +
              ' AND sf.SubArea       =  '+ QuotedStr(FAppModules.StudyArea.SubAreaCode) +
              ' AND sf.Scenario      =  '+ QuotedStr(FAppModules.StudyArea.ScenarioCode) +
              ' AND sf.Model       =  sd.Model '+
              ' AND sf.StudyAreaName =  sd.StudyAreaName '+
              ' AND sf.SubArea       =  sd.SubArea '+
              ' AND sf.Scenario      =  sd.Scenario '+
              ' AND  sf.SwitchDefID =  sd.SwitchDefID '+
              ' AND  sf.FileGroupID = sd.FileGroupID '+
              ' AND  sf.FileGroupID = 1 ';
      LDataSet.SetSQL(LSQL);
      LDataset.DataSet.Open;
      ASwitchDefID := LDataset.DataSet.FieldByName('SwitchDefID').AsInteger;
      AStartYear := LDataset.DataSet.FieldByName('StartYear').AsInteger;
      AStartMonth := LDataset.DataSet.FieldByName('StartMonth').AsInteger;
    finally
      LDataset.Free;
    end;
    Result := True;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TAllocationDefLoadAgent.PopulateAllocationDefinition
                                            (ADataSet  : TAbstractModelDataset;
                                             AAllocDef : TAllocationDefinition): boolean;
const OPNAME = 'TAllocationDefLoadAgent.PopulateAllocationDefinition';
var
  lAllocDefID               : integer;
  lName                     : string;
  LSwitchDefID              : integer;
  LAllocDefFileName         : string;
  lStartYear                : integer;
  lStartMonth               : integer;
  lEndYear                  : integer;
  lEndMonth                 : integer;
  lNrOfReliabilityClasses   : integer;
  lNrOfLoadCases            : integer;
  lNrOfStartingPercentages  : integer;
  lNrOfCurveSets            : integer;
  lPeriodLength             : integer;
  lSupportStrategyType      : integer;
  lBalancingOption          : integer;
  lRecurrenceIntervals      : array of integer;
  lRecurrenceIntervalLabels : array of string;
  lDecisionCurveSet         : array of integer;
  lStartingPercentages      : array of double;
  lIndex                    : integer;
  lFieldName                : string;
begin
  Result := FALSE;
  try
    if Assigned(ADataSet) and Assigned(AAllocDef) then
    begin
      lAllocDefID              := ADataSet.DataSet.FieldByName('AllocDefID').AsInteger;
      lName                    := Trim(ADataSet.DataSet.FieldByName('AllocDefName').AsString);
      lAllocDefFileName        := Trim(ADataSet.DataSet.FieldByName('FamilyFile').AsString);
      lStartYear               := ADataSet.DataSet.FieldByName('AllocDefStartYear').AsInteger;
      lStartMonth              := ADataSet.DataSet.FieldByName('AllocDefStartMonth').AsInteger;
      lEndYear                 := ADataSet.DataSet.FieldByName('AllocDefEndYear').AsInteger;
      lEndMonth                := ADataSet.DataSet.FieldByName('AllocDefEndMonth').AsInteger;
      lNrOfReliabilityClasses  := ADataSet.DataSet.FieldByName('NrOfReliabilityClasses').AsInteger;
      lNrOfLoadCases           := ADataSet.DataSet.FieldByName('NrOfLoadCases').AsInteger;
      lNrOfStartingPercentages := ADataSet.DataSet.FieldByName('NrOfStartStoragePercs').AsInteger;
      lNrOfCurveSets           := ADataSet.DataSet.FieldByName('NrOfCurveSets').AsInteger;
      lPeriodLength            := ADataSet.DataSet.FieldByName('PeriodLength').AsInteger;
      lSupportStrategyType     := ADataSet.DataSet.FieldByName('SupportStrategy').AsInteger;
      lBalancingOption         := ADataSet.DataSet.FieldByName('BalancingOption').AsInteger;

      SetLength(lRecurrenceIntervals,      lNrOfReliabilityClasses  + 1);
      SetLength(lRecurrenceIntervalLabels, lNrOfReliabilityClasses  + 1);
      SetLength(lStartingPercentages,      lNrOfStartingPercentages + 1);
      SetLength(lDecisionCurveSet,         12 + 1);



      for lIndex := 1 to lNrOfReliabilityClasses do
      begin
        lFieldName := Format('RIValue%2.2d',[lIndex]);
        lRecurrenceIntervals[lIndex] := ADataSet.DataSet.FieldByName(lFieldName).AsInteger;
        lFieldName := Format('RILabel%2.2d',[lIndex]);
        lRecurrenceIntervalLabels[lIndex] := Trim(ADataSet.DataSet.FieldByName(lFieldName).AsString);
      end;
      for lIndex := 1 to 12 do
      begin
        lFieldName := Format('MonthCurveSet%2.2d',[lIndex]);
        lDecisionCurveSet[lIndex] := ADataSet.DataSet.FieldByName(lFieldName).AsInteger;
      end;
      for lIndex := 1 to lNrOfStartingPercentages do
      begin
        lFieldName := Format('StartStoragePerc%2.2d',[lIndex]);
        lStartingPercentages[lIndex] := ADataSet.DataSet.FieldByName(lFieldName).AsFloat;
      end;

      if not ADataSet.DataSet.FieldByName('FamilyFile').IsNull then
        GetStartYearAndMonthFromDamDotDat(LSwitchDefID,lStartYear,lStartMonth,lAllocDefFileName);

      Result :=  AAllocDef.Populate
                   (lAllocDefID, lName,LSwitchDefID, LAllocDefFileName, lStartYear, lStartMonth, lEndYear, lEndMonth,
                    lNrOfReliabilityClasses, lNrOfLoadCases, lNrOfStartingPercentages,
                    lNrOfCurveSets, lPeriodLength, lSupportStrategyType, lBalancingOption,
                    lRecurrenceIntervals, lRecurrenceIntervalLabels, lDecisionCurveSet,
                    lStartingPercentages);
      Finalize(lRecurrenceIntervals);
      Finalize(lRecurrenceIntervalLabels);
      Finalize(lStartingPercentages);
      Finalize(lDecisionCurveSet);
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TAllocationDefLoadAgent.LoadUserCategories (AAllocDef : TAllocationDefinition) : boolean;
const OPNAME = 'TAllocationDefLoadAgent.LoadUserCategories';
var
  lDataSet     : TAbstractModelDataset;
  lCategory    : TUserCategory;
  lSQL         : string;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(lDataSet) then
      begin
        lSQL := 'SELECT * FROM FMUserCategory WHERE ' +
                GetScenarioWhereClause +
                ' AND AllocDefID = ' + IntToStr(AAllocDef.AllocationDefinitionID);
        lDataSet.SetSQL(lSQL);
        lDataset.DataSet.Open;

        while (NOT lDataset.DataSet.EOF) do
        begin
          lCategory := AAllocDef.CreateCategory;
          if (NOT PopulateUserCategory(lDataset, AAllocDef.NrOfReliabilityClasses, lCategory)) then
            AAllocDef.DeleteCategoryByIndex(AAllocDef.NrOfCategories-1);
          lDataset.DataSet.Next;
        end;
        lDataset.DataSet.Close;
      end;
    finally
      lDataset.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TAllocationDefLoadAgent.PopulateUserCategory (ADataSet      : TAbstractModelDataset;
                                                       ANrOfRIs      : integer;
                                                       AUserCategory : TUserCategory): boolean;
const OPNAME = 'TAllocationDefLoadAgent.PopulateUserCategory';
var
  lAllocDefID   : integer;
  lCategoryID   : integer;
  lDescription  : string;
  lDistribution : array of double;
  lIndex        : integer;
  lFieldName    : string;
begin
  Result := FALSE;
  try
    lAllocDefID   := ADataSet.DataSet.FieldByName('AllocDefID').AsInteger;
    lCategoryID   := ADataSet.DataSet.FieldByName('UserCategoryID').AsInteger;
    lDescription  := Trim(ADataSet.DataSet.FieldByName('UserCategoryName').AsString);
    SetLength(lDistribution, ANrOfRIs + 1);
    for lIndex := 1 to ANrOfRIs do
    begin
      lFieldName := Format('Distribution%2.2d',[lIndex]);
      lDistribution[lIndex] := ADataSet.DataSet.FieldByName(lFieldName).AsFloat;
    end;
    Result := AUserCategory.Populate(lAllocDefID, lCategoryID, lDescription, ANrOfRIs, lDistribution);
    Finalize(lDistribution);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAllocationDefLoadAgent.LoadAllocationLevels (AAllocDef : TAllocationDefinition) : boolean;
const OPNAME = 'TAllocationDefLoadAgent.LoadAllocationLevels';
var
  lDataSet     : TAbstractModelDataset;
  lAllocLevel  : TAllocationLevel;
  lSQL         : string;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(lDataSet) then
      begin
        lSQL := 'SELECT * FROM FMAllocationLevel WHERE ' +
                GetScenarioWhereClause +
                ' AND AllocDefID = ' + IntToStr(AAllocDef.AllocationDefinitionID);
        lDataSet.SetSQL(lSQL);
        lDataset.DataSet.Open;

        while (NOT lDataset.DataSet.EOF) do
        begin
          lAllocLevel := AAllocDef.CreateAllocationLevel;
          if (NOT PopulateAllocationLevel(lDataset, AAllocDef.NrOfReliabilityClasses, lAllocLevel)) then
            AAllocDef.DeleteAllocationLevelByIndex(AAllocDef.NrOfAllocationLevels-1);
          lDataset.DataSet.Next;
        end;
        lDataset.DataSet.Close;
      end;
    finally
      lDataset.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TAllocationDefLoadAgent.PopulateAllocationLevel (ADataSet    : TAbstractModelDataset;
                                                          ANrOfRIs    : integer;
                                                          AAllocLevel : TAllocationLevel): boolean;
const OPNAME = 'TAllocationDefLoadAgent.PopulateAllocationLevel';
var
  lAllocDefID   : integer;
  lAllocLevelID : integer;
  lDescription  : string;
  lCurtailment  : array of double;
  lIndex        : integer;
  lFieldName    : string;
begin
  Result := FALSE;
  try
    lAllocDefID   := ADataSet.DataSet.FieldByName('AllocDefID').AsInteger;
    lAllocLevelID := ADataSet.DataSet.FieldByName('AllocLevelID').AsInteger;
    lDescription  := Trim(ADataSet.DataSet.FieldByName('AllocLevelName').AsString);
    SetLength(lCurtailment, ANrOfRIs + 1);
    for lIndex := 1 to ANrOfRIs do
    begin
      lFieldName := Format('Curtailment%2.2d',[lIndex]);
      lCurtailment[lIndex] := ADataSet.DataSet.FieldByName(lFieldName).AsFloat;
    end;
    Result := AAllocLevel.Populate(lAllocDefID, lAllocLevelID, lDescription, ANrOfRIs, lCurtailment);
    Finalize(lCurtailment);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAllocationDefLoadAgent.LoadSubSystems (AAllocDef : TAllocationDefinition) : boolean;
const OPNAME = 'TAllocationDefLoadAgent.LoadSubSystems';
var
  lDataSet    : TAbstractModelDataset;
  lSubSystem  : TSubSystem;
  lSQL        : string;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(lDataSet) then
      begin
        lSQL := 'SELECT * FROM FMSubSystem WHERE ' +
                GetScenarioWhereClause +
                ' AND AllocDefID = ' + IntToStr(AAllocDef.AllocationDefinitionID);
        lDataSet.SetSQL(lSQL);
        lDataset.DataSet.Open;

        while (NOT lDataset.DataSet.EOF) do
        begin
          lSubSystem := AAllocDef.CreateSubSystem;
          if (PopulateSubSystem(lDataset, lSubSystem)) then
          begin
            lSubSystem.SetCoefficientDimensions(AAllocDef.NrOfStartingPercentages,
                                                AAllocDef.NrOfCurveSets,
                                                AAllocDef.NrOfLoadCases);
            LoadCoefficients(AAllocDef, lSubSystem);
          end
          else
            AAllocDef.DeleteSubSystemByIndex(AAllocDef.NrOfSubSystems-1);
          lDataset.DataSet.Next;
        end;
        lDataset.DataSet.Close;
      end;
    finally
      lDataset.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TAllocationDefLoadAgent.PopulateSubSystem (ADataSet   : TAbstractModelDataset;
                                                    ASubSystem : TSubSystem): boolean;
const OPNAME = 'TAllocationDefLoadAgent.PopulateSubSystem';
var
  lAllocDefID       : integer;
  lSubSystemID      : integer;
  lName             : string;
  lOrder            : integer;
  lStartMonth       : integer;
  lStartYear        : integer;
  lEndMonth         : integer;
  lEndYear          : integer;
  lSubtractID       : integer;
  lSupportID        : integer;
  lSupportChannelNr : integer;
  lShortTermYield   : double;
  lLongTermYield    : double;
  lLowestStreamFlow : double;
  lFirmYield        : Boolean;
  lReservoirNrs     : string;
  lChannelNrs       : array of integer;
  lSupportCalcType  : integer;
  lIndex            : integer;
  lFieldName        : string;
begin
  Result := FALSE;
  try
    lAllocDefID       := ADataSet.DataSet.FieldByName('AllocDefID').AsInteger;
    lSubSystemID      := ADataSet.DataSet.FieldByName('SubSystemID').AsInteger;
    lName             := Trim(ADataSet.DataSet.FieldByName('SubSystemName').AsString);
    lOrder            := ADataSet.DataSet.FieldByName('SubSystemOrder').AsInteger;
    lStartYear        := ADataSet.DataSet.FieldByName('SubSystemStartYear').AsInteger;
    lStartMonth       := ADataSet.DataSet.FieldByName('SubSystemStartMonth').AsInteger;
    lEndYear          := ADataSet.DataSet.FieldByName('SubSystemEndYear').AsInteger;
    lEndMonth         := ADataSet.DataSet.FieldByName('SubSystemEndMonth').AsInteger;
    lSubtractID       := ADataSet.DataSet.FieldByName('SubtractedSubSystemID').AsInteger;
    lSupportID        := ADataSet.DataSet.FieldByName('SupportingSubSystemID').AsInteger;
    lSupportChannelNr := ADataSet.DataSet.FieldByName('SupportingChannelNr').AsInteger;
    lShortTermYield   := ADataSet.DataSet.FieldByName('ShortTermYield').AsFloat;
    lLongTermYield    := ADataSet.DataSet.FieldByName('LongTermYield').AsFloat;
    lLowestStreamFlow := ADataSet.DataSet.FieldByName('LowestStreamFlow').AsFloat;
    lFirmYield        := (UpperCase(Trim(ADataSet.DataSet.FieldByName('FirmYield').AsString)) = 'Y');
    lSupportCalcType  := ADataSet.DataSet.FieldByName('SupportCalcType').AsInteger;
    lReservoirNrs     := Trim(ADataSet.DataSet.FieldByName('SubSystemReservoirNrs').AsString);
    SetLength(lChannelNrs, 5 + 1);
    for lIndex := 1 to 5 do
    begin
      lFieldName := Format('RoutingChannelNr%2.2d',[lIndex]);
      lChannelNrs[lIndex] := ADataSet.DataSet.FieldByName(lFieldName).AsInteger;
    end;

    Result := ASubSystem.Populate(lAllocDefID, lSubSystemID, lName, lOrder, lStartYear, lStartMonth,
                                  lEndYear, lEndMonth, lSubtractID, lSupportID,
                                  lSupportChannelNr, lShortTermYield, lLongTermYield,
                                  lLowestStreamFlow, lFirmYield, lReservoirNrs, lChannelNrs,
                                  lSupportCalcType);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAllocationDefLoadAgent.LoadCoefficients (AAllocDef  : TAllocationDefinition;
                                                   ASubSystem : TSubSystem) : boolean;
const OPNAME = 'TAllocationDefLoadAgent.LoadCoefficients';
var
  lDataSet      : TAbstractModelDataset;
  lCoefficient  : TCoefficient;
  lSQL          : string;
  lStartPercNr  : integer;
  lCurveSetNr   : integer;
  lLoadCaseNr   : integer;

begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(lDataSet) then
      begin
        lSQL := 'SELECT * FROM FMCoefficients WHERE ' +
                GetScenarioWhereClause +
                ' AND AllocDefID = ' + IntToStr(AAllocDef.AllocationDefinitionID) +
                ' AND SubSystemID = ' + IntToStr(ASubSystem.SubSystemID) +
                ' ORDER BY StartStorageNr, CurveSetNr, LoadCaseNr';
        lDataSet.SetSQL(lSQL);
        lDataset.DataSet.Open;

        while (NOT lDataset.DataSet.EOF) do
        begin
          lStartPercNr := lDataSet.DataSet.FieldByName('StartStorageNr').AsInteger;
          lCurveSetNr  := lDataSet.DataSet.FieldByName('CurveSetNr').AsInteger;
          lLoadCaseNr  := lDataSet.DataSet.FieldByName('LoadCaseNr').AsInteger;
          lCoefficient := ASubSystem.CreateCoefficient(lStartPercNr, lCurveSetNr, lLoadCaseNr);
          if (lCoefficient <> nil) then
          begin
            lCoefficient.Populate(AAllocDef.AllocationDefinitionID,
                                  ASubSystem.SubSystemID,
                                  lStartPercNr,
                                  lCurveSetNr,
                                  lLoadCaseNr,
                                  lDataSet.DataSet.FieldByName('LoadCase').AsFloat,
                                  lDataSet.DataSet.FieldByName('CoefficientA').AsFloat,
                                  lDataSet.DataSet.FieldByName('CoefficientB').AsFloat,
                                  lDataSet.DataSet.FieldByName('CoefficientC').AsFloat,
                                  lDataSet.DataSet.FieldByName('CoefficientD').AsFloat,
                                  lDataSet.DataSet.FieldByName('RiskProportion').AsFloat);
          end;
          lDataset.DataSet.Next;
        end;
        lDataset.DataSet.Close;
      end;
    finally
      lDataset.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TAllocationDefLoadAgent.LoadSupportStrategy (AAllocDef : TAllocationDefinition) : boolean;
const OPNAME = 'TAllocationDefLoadAgent.LoadSupportStrategy';
var
  lDataSet         : TAbstractModelDataset;
  LFixedPosition   : TFixedPosition;
  LSpecificOrder   : TSpecificOrder;
  lPosition        : integer;
  lSubSystemID     : integer;
  lSQL             : string;
  LFixedPositionID : integer;
  LSpecificOrderID : integer;
  lBeforeID        : integer;
  lAfterID         : integer;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(lDataSet) then
      begin
        lSQL := 'SELECT * FROM FMSolveFixedPosition WHERE ' +
                GetScenarioWhereClause +
                ' AND AllocDefID = ' + IntToStr(AAllocDef.AllocationDefinitionID);
        lDataSet.SetSQL(lSQL);
        lDataset.DataSet.Open;
        while (NOT lDataset.DataSet.EOF) do
        begin
          LFixedPositionID := lDataSet.DataSet.FieldByName('FixedPositionID').AsInteger;
          lPosition        := lDataSet.DataSet.FieldByName('FixedPositionNr').AsInteger;
          lSubSystemID     := lDataSet.DataSet.FieldByName('FixedPosSubSystemID').AsInteger;
          LFixedPosition   := AAllocDef.CreateFixedPosition;
          LFixedPosition.Populate(AAllocDef.AllocationDefinitionID, LFixedPositionID, lPosition,lSubSystemID );
          lDataset.DataSet.Next;
        end;
        lDataset.DataSet.Close;

        lSQL := 'SELECT * FROM FMSolveSpecificOrder WHERE ' +
                GetScenarioWhereClause +
                ' AND AllocDefID = ' + IntToStr(AAllocDef.AllocationDefinitionID);
        lDataSet.SetSQL(lSQL);
        lDataset.DataSet.Open;
        while (NOT lDataset.DataSet.EOF) do
        begin
          LSpecificOrderID := lDataSet.DataSet.FieldByName('SpecificOrderID').AsInteger;
          lBeforeID := lDataSet.DataSet.FieldByName('BeforeSubSystemID').AsInteger;
          lAfterID  := lDataSet.DataSet.FieldByName('AfterSubSystemID').AsInteger;
          LSpecificOrder := AAllocDef.CreateSpecificOrder;
          LSpecificOrder.populate( AAllocDef.AllocationDefinitionID, LSpecificOrderID,
                                   lBeforeID, lAfterID );
          lDataset.DataSet.Next;
        end;
        lDataset.DataSet.Close;
      end;
    finally
      lDataset.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TAllocationDefLoadAgent.LoadSupportChannels (AAllocDef : TAllocationDefinition) : boolean;
const OPNAME = 'TAllocationDefLoadAgent.LoadSupportChannels';
var
  lDataSet        : TAbstractModelDataset;
  lSupportChannel : TSupportChannel;
  lSQL            : string;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(lDataSet) then
      begin
        lSQL := 'SELECT * FROM FMSupportChannel WHERE ' +
                GetScenarioWhereClause +
                ' AND AllocDefID = ' + IntToStr(AAllocDef.AllocationDefinitionID);
        lDataSet.SetSQL(lSQL);
        lDataset.DataSet.Open;

        while (NOT lDataset.DataSet.EOF) do
        begin
          lSupportChannel := AAllocDef.CreateSupportChannel;
          if (NOT PopulateSupportChannel(lDataset, lSupportChannel)) then
            AAllocDef.DeleteSupportChannelByIndex(AAllocDef.NrOfSupportChannels-1);
          lDataset.DataSet.Next;
        end;
        lDataset.DataSet.Close;
      end;
    finally
      lDataset.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TAllocationDefLoadAgent.PopulateSupportChannel (ADataSet        : TAbstractModelDataset;
                                                         ASupportChannel : TSupportChannel): boolean;
const OPNAME = 'TAllocationDefLoadAgent.PopulateSupportChannel';
var
  lAllocDefID       : integer;
  lSupportChannelID : integer;
  lChannelNr        : integer;
  lNrOfSubSystems   : integer;
  lSubSystemIDs     : array [0..10] of integer;
  lFactors          : array [0..10] of double;
  lIndex            : integer;
  lFieldName        : string;
begin
  Result := FALSE;
  try
    lAllocDefID       := ADataSet.DataSet.FieldByName('AllocDefID').AsInteger;
    lSupportChannelID := ADataSet.DataSet.FieldByName('SupportChannelID').AsInteger;
    lChannelNr        := ADataSet.DataSet.FieldByName('ChannelNumber').AsInteger;
    lNrOfSubSystems   := ADataSet.DataSet.FieldByName('NrOfCntrlSubSystems').AsInteger;
    for lIndex := 1 to lNrOfSubSystems do
    begin
      lFieldName            := Format('CntrlSubSystemID%2.2d',[lIndex]);
      lSubSystemIDs[lIndex] := ADataSet.DataSet.FieldByName(lFieldName).AsInteger;
      lFieldName            := Format('CntrlFactor%2.2d',[lIndex]);
      lFactors[lIndex]      := ADataSet.DataSet.FieldByName(lFieldName).AsFloat;
    end;
    Result := ASupportChannel.Populate(lAllocDefID, lSupportChannelID, lChannelNr, lNrOfSubSystems, lSubSystemIDs, lFactors);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAllocationDefLoadAgent.LoadDemandDefinitions (AAllocDef : TAllocationDefinition) : boolean;
const OPNAME = 'TAllocationDefLoadAgent.LoadDemandDefinitions';
var
  lDataSet          : TAbstractModelDataset;
  lDemandDefinition : TDemandDefinition;
  lSQL              : string;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(lDataSet) then
      begin
        lSQL := 'SELECT * FROM FMDemandDefinition WHERE ' +
                GetScenarioWhereClause +
                ' AND AllocDefID = ' + IntToStr(AAllocDef.AllocationDefinitionID);
        lDataSet.SetSQL(lSQL);
        lDataset.DataSet.Open;

        while (NOT lDataset.DataSet.EOF) do
        begin
          lDemandDefinition := AAllocDef.CreateDemandDefinition;
          if (NOT PopulateDemandDefinition(lDataset, lDemandDefinition)) then
            AAllocDef.DeleteDemandDefinitionByIndex(AAllocDef.NrOfDemandDefinitions-1)
          else
            LoadSupportSubSystems(AAllocDef, lDemandDefinition);
          lDataset.DataSet.Next;
        end;
        lDataset.DataSet.Close;
      end;
    finally
      lDataset.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TAllocationDefLoadAgent.PopulateDemandDefinition (ADataSet          : TAbstractModelDataset;
                                                           ADemandDefinition : TDemandDefinition): boolean;
const OPNAME = 'TAllocationDefLoadAgent.PopulateDemandDefinition';
var
  lAllocDefID        : integer;
  lDemandDefID       : integer;
  lName              : string;
  lParentSubSystemID : integer;
  lOrder             : integer;
  lGrowthType        : integer;
  lTargetDemand      : double;
  lDemandCentreID    : integer;
  lUserCategoryID    : integer;
  lSupportArc1       : integer;
  lSupportArc2       : integer;
begin
  Result := FALSE;
  try
    lAllocDefID        := ADataSet.DataSet.FieldByName('AllocDefID').AsInteger;
    lDemandDefID       := ADataSet.DataSet.FieldByName('DemandDefID').AsInteger;
    lName              := Trim(ADataSet.DataSet.FieldByName('DemandDefName').AsString);
    lParentSubSystemID := ADataSet.DataSet.FieldByName('ParentSubSystemID').AsInteger;
    lOrder             := ADataSet.DataSet.FieldByName('DemandDefOrder').AsInteger;
    lGrowthType        := ADataSet.DataSet.FieldByName('GrowthType').AsInteger;
    lTargetDemand      := ADataSet.DataSet.FieldByName('TargetDemand').AsFloat;
    lDemandCentreID    := ADataSet.DataSet.FieldByName('DemandCentreID').AsInteger;
    lUserCategoryID    := ADataSet.DataSet.FieldByName('DCUserCategoryID').AsInteger;
    lSupportArc1       := ADataSet.DataSet.FieldByName('SupportArc1').AsInteger;
    lSupportArc2       := ADataSet.DataSet.FieldByName('SupportArc2').AsInteger;
    Result := ADemandDefinition.Populate(lAllocDefID, lDemandDefID, lName, lParentSubSystemID, lOrder,
                                     lGrowthType, lTargetDemand, lDemandCentreID, lUserCategoryID, lSupportArc1, lSupportArc2 );
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAllocationDefLoadAgent.LoadSupportSubSystems (AAllocDef         : TAllocationDefinition;
                                                        ADemandDefinition : TDemandDefinition) : boolean;
const OPNAME = 'TAllocationDefLoadAgent.LoadSupportSubSystems';
var
  lDataSet          : TAbstractModelDataset;
  lSQL              : string;
  lSupportSubSystem : TSupportSubSystem;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(lDataSet) then
      begin
        lSQL := 'SELECT * FROM FMSupportSubSystem WHERE ' +
                GetScenarioWhereClause +
                ' AND AllocDefID = ' + IntToStr(AAllocDef.AllocationDefinitionID) +
                ' AND DemandDefID = ' + IntToStr(ADemandDefinition.DemandDefID);
        lDataSet.SetSQL(lSQL);
        lDataset.DataSet.Open;

        while (NOT lDataset.DataSet.EOF) do
        begin
          lSupportSubSystem := ADemandDefinition.CreateSupportSubSystem;
          if (NOT PopulateSupportSubSystem(lDataset, lSupportSubSystem)) then
            ADemandDefinition.DeleteSupportSubSystem(ADemandDefinition.NrOfSupportSubSystems-1);
          lDataset.DataSet.Next;
        end;
        lDataset.DataSet.Close;
      end;
    finally
      lDataset.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAllocationDefLoadAgent.PopulateSupportSubSystem (ADataSet          : TAbstractModelDataset;
                                                           ASupportSubSystem : TSupportSubSystem): boolean;
const OPNAME = 'TAllocationDefLoadAgent.PopulateSupportSubSystem';
var
  lIndex          : integer;
  lAllocDefID     : integer;
  lDemandDefID    : integer;
  lSupportID      : integer;
  lSubSystemID    : integer;
  lFieldName      : string;
  lChannelNrs     : array of integer;
begin
  try
    lAllocDefID     := ADataSet.DataSet.FieldByName('AllocDefID').AsInteger;
    lDemandDefID    := ADataSet.DataSet.FieldByName('DemandDefID').AsInteger;
    lSupportID      := ADataSet.DataSet.FieldByName('SupportSubSystemID').AsInteger;
    lSubSystemID    := ADataSet.DataSet.FieldByName('SupSubSystemID').AsInteger;
    SetLength(lChannelNrs, 6);
    for lIndex := 1 to 5 do
    begin
      lFieldName          := Format('SupSubSysChannelNr%2.2d',[lIndex]);
      lChannelNrs[lIndex] := ADataSet.DataSet.FieldByName(lFieldName).AsInteger;
    end;
    Result := ASupportSubSystem.Populate(lAllocDefID, lDemandDefID, lSupportID, lSubSystemID, lChannelNrs);
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TAllocationDefLoadAgent.InsertUserCategory ( AAllocDefID : integer; var AUserCategoryID : integer): boolean;
const OPNAME = 'TAllocationDefLoadAgent.InsertUserCategory';
var
  lDataSet        : TAbstractModelDataset;
  lUserCategoryID : integer;
  lImportDate     : TDateTime;
  lSQL            : string;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(lDataSet) then
      begin
        lUserCategoryID := GetLastUserCategoryID + 1;
        lSQL:= 'INSERT  INTO FMUserCategory ' +
              '(Model, StudyAreaName, SubArea, Scenario, AllocDefID, UserCategoryID) ' +
              'VALUES (' +
              QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
              QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
              QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
              QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
              IntToStr(AAllocDefID) + ','+
              QuotedStr(IntToStr(lUserCategoryID)) +
              ')';

        lDataSet.SetSQL(lSQL);
        lDataset.ExecSQL;
        lDataset.DataSet.Close;

        AUserCategoryID := lUserCategoryID;
        FAppModules.StudyArea.LastUpdateDate := Now();

        lImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if (lImportDate = NullDateTime) then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

        Result := TRUE;
      end;
    finally
      lDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TAllocationDefLoadAgent.InsertAllocationLevel ( AAllocDefID : integer; var AAllocationLevelID : integer): boolean;
const OPNAME = 'TAllocationDefLoadAgent.InsertAllocationLevel';
var
  lDataSet        : TAbstractModelDataset;
  lAllocationLevelID : integer;
  lImportDate     : TDateTime;
  lSQL            : string;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(lDataSet) then
      begin
        lAllocationLevelID := GetLastAllocationLevelID + 1;
        lSQL:= 'INSERT  INTO FMAllocationLevel ' +
              '(Model, StudyAreaName, SubArea, Scenario, AllocDefID, AllocLevelID) ' +
              'VALUES (' +
              QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
              QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
              QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
              QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
              IntToStr(AAllocDefID) + ','+
              QuotedStr(IntToStr(lAllocationLevelID)) +
              ')';

        lDataSet.SetSQL(lSQL);
        lDataset.ExecSQL;
        lDataset.DataSet.Close;

        AAllocationLevelID := lAllocationLevelID;
        FAppModules.StudyArea.LastUpdateDate := Now();

        lImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if (lImportDate = NullDateTime) then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

        Result := TRUE;
      end;
    finally
      lDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TAllocationDefLoadAgent.InsertSubSystem  (AAllocDefID      : integer;
                                                   var ASubSystemID : integer): boolean;
const OPNAME = 'TAllocationDefLoadAgent.InsertSubSystem';
var
  lDataSet     : TAbstractModelDataset;
  lSubSystemID : integer;
  lImportDate  : TDateTime;
  lSQL         : string;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(lDataSet) then
      begin
        lSubSystemID := GetLastSubSystemID + 1;
        lSQL:= 'INSERT INTO FMSubSystem ' +
              '(Model, StudyAreaName, SubArea, Scenario, AllocDefID, SubSystemID) ' +
              'VALUES (' +
              QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
              QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
              QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
              QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
              IntToStr(AAllocDefID) + ','+
              QuotedStr(IntToStr(lSubSystemID)) + 
              ')';
        lDataSet.SetSQL(lSQL);
        lDataset.ExecSQL;
        lDataset.DataSet.Close;
        ASubSystemID := lSubSystemID;
        FAppModules.StudyArea.LastUpdateDate := Now();
        lImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if (lImportDate = NullDateTime) then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);
        Result := TRUE;
      end;
    finally
      lDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TAllocationDefLoadAgent.InsertAllocationDefinition (var AAllocDefID : integer): boolean;
const OPNAME = 'TAllocationDefLoadAgent.InsertAllocationDefinition';
var
  lDataSet       : TAbstractModelDataset;
  lAllocDefID    : integer;
  lImportDate    : TDateTime;
  lSQL           : string;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(lDataSet) then
      begin
        lAllocDefID := GetLastAllocationDefinitionID + 1;
        lSQL:= 'INSERT  INTO FMAllocationDefinition ' +
              '(Model, StudyAreaName, SubArea, Scenario, AllocDefID, AllocDefName) ' +
              //'(Model, StudyAreaName, SubArea, Scenario, AllocDefID, AllocDefName, FamilyFile) ' +
              'VALUES (' +
              QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
              QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
              QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
              QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
              IntToStr(lAllocDefID) + ','+
              QuotedStr('Allocation definition ' + IntToStr(lAllocDefID)) + //',' +
//              QuotedStr('Allocation definition  filename ' + IntToStr(lAllocDefID)) +
              ')';

        lDataSet.SetSQL(lSQL);
        lDataset.ExecSQL;
        lDataset.DataSet.Close;

        AAllocDefID := lAllocDefID;
        FAppModules.StudyArea.LastUpdateDate := Now();

        lImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if (lImportDate = NullDateTime) then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

        Result := TRUE;
      end;
    finally
      lDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TAllocationDefLoadAgent.GetLastSupportChannelID : integer;
const OPNAME = 'TAllocationDefLoadAgent.GetLastSupportChannelID';
var
  lDataSet : TAbstractModelDataset;
  lSQL     : string;
begin
  Result := 0;
  try
    lSQL := 'SELECT MAX(SupportChannelID) AS LastID FROM FMSupportChannel WHERE ' +
            GetScenarioWhereClause;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(lSQL);
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('LastID').AsInteger;
        LDataset.DataSet.Close;
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAllocationDefLoadAgent.InsertSupportChannel (AAllocDefID           : integer;
                                                       var ASupportChannelID : integer ) : boolean;
const OPNAME = 'TAllocationDefLoadAgent.InsertSupportChannel';
var
  lDataSet        : TAbstractModelDataset;
  lImportDate     : TDateTime;
  lSQL            : string;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(lDataSet) then
      begin
        ASupportChannelID := GetLastSupportChannelID + 1;
        lSQL:= 'INSERT  INTO FMSupportChannel ' +
              '(Model, StudyAreaName, SubArea, Scenario, AllocDefID, SupportChannelID) ' +
              'VALUES (' +
              QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
              QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
              QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
              QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
              IntToStr(AAllocDefID) + ','+
              IntToStr(ASupportChannelID) +
              ')';
        lDataSet.SetSQL(lSQL);
        lDataset.ExecSQL;
        lDataset.DataSet.Close;
        FAppModules.StudyArea.LastUpdateDate := Now();
        lImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if (lImportDate = NullDateTime) then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);
        Result := TRUE;
      end;
    finally
      lDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TAllocationDefLoadAgent.InsertDemandDefinition ( AAllocDefID : integer; var ADemandDefID : integer ) : boolean;
const OPNAME = 'TAllocationDefLoadAgent.InsertDemandDefinition';
var
  LDataSet        : TAbstractModelDataset;
  LDemandDefID    : integer;
  LImportDate     : TDateTime;
  LSQL            : string;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned ( LDataSet ) then
      begin
        LDemandDefID := GetLastDemandDefID + 1;
        lSQL:= 'INSERT  INTO FMDemandDefinition ' +
              '(Model, StudyAreaName, SubArea, Scenario, AllocDefID, DemandDefID, DemandDefName) ' +
              'VALUES (' +
              QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
              QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
              QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
              QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
              IntToStr(AAllocDefID) + ','+
              QuotedStr(IntToStr(LDemandDefID)) + ',' +
              QuotedStr('Undefined Demand Centre ' + IntToStr(LDemandDefID)) +
              ')';
        lDataSet.SetSQL(lSQL);
        lDataset.ExecSQL;
        lDataset.DataSet.Close;
        ADemandDefID := LDemandDefID;
        FAppModules.StudyArea.LastUpdateDate := Now();
        lImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if (lImportDate = NullDateTime) then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);
        Result := TRUE;
      end;
    finally
      lDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TAllocationDefLoadAgent.InsertSupportSubSystem ( AAllocDefID , ADemandDefID : integer; var ASupportSubSystemID : integer ) : boolean;
const OPNAME = 'TAllocationDefLoadAgent.InsertSupportSubSystem';
var
  LDataSet : TAbstractModelDataset;
  LSupportSubSystemID : integer;
  LImportDate : TDateTime;
  LSQL : string;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned ( LDataSet ) then
      begin
        LSupportSubSystemID := GetLastSupportSubSystemID + 1;
        lSQL:= 'INSERT  INTO FMSupportSubSystem ' +
              '(Model, StudyAreaName, SubArea, Scenario, AllocDefID, DemandDefID, SupportSubSystemID) ' +
              'VALUES (' +
              QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
              QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
              QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
              QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
              IntToStr(AAllocDefID) + ','+
              QuotedStr(IntToStr(ADemandDefID)) +','+
              QuotedStr(IntToStr(LSupportSubSystemID))+
              ')';
        lDataSet.SetSQL(lSQL);
        lDataset.ExecSQL;
        lDataset.DataSet.Close;
        ASupportSubSystemID := LSupportSubSystemID;
        FAppModules.StudyArea.LastUpdateDate := Now();
        lImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if (lImportDate = NullDateTime) then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);
        Result := TRUE;
      end;
    finally
      lDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TAllocationDefLoadAgent.InsertCoefficient ( AAllocDefID ,ASubSystemID, AStartPercNr,
                                                     ACurveSetNr, ALoadCaseNr  : integer ) : boolean;
const OPNAME = 'TAllocationDefLoadAgent.InsertCoefficient';
var
  LDataSet : TAbstractModelDataset;
  LImportDate : TDateTime;
  LSQL : string;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned ( LDataSet ) then
      begin
        lSQL:= 'INSERT  INTO FMCoefficients ' +
              '(Model, StudyAreaName, SubArea, Scenario, AllocDefID, SubSystemID, ' +
              ' StartStorageNr, CurveSetNr, LoadCaseNr ) ' +
              'VALUES (' +
              QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
              QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
              QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
              QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
              IntToStr(AAllocDefID)  + ',' +
              IntToStr(ASubSystemID) + ',' +
              IntToStr(AStartPercNr) + ',' +
              IntToStr(ACurveSetNr)  + ',' +
              IntToStr(ALoadCaseNr)  +
              ')';
        lDataSet.SetSQL(lSQL);
        lDataset.ExecSQL;
        lDataset.DataSet.Close;
        FAppModules.StudyArea.LastUpdateDate := Now();
        lImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if (lImportDate = NullDateTime) then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);
        Result := TRUE;
      end;
    finally
      lDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TAllocationDefLoadAgent.GetLastFixedPositionID (AAllocDefID : integer) : integer;
const OPNAME = 'TAllocationDefLoadAgent.GetLastFixedPositionID';
var
  lDataSet : TAbstractModelDataset;
  lSQL     : string;
begin
  Result := 0;
  try
    lSQL := 'SELECT MAX(FixedPositionID) AS LastID FROM FMSolveFixedPosition WHERE ' +
            GetScenarioWhereClause;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(lSQL);
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('LastID').AsInteger;
        LDataset.DataSet.Close;
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAllocationDefLoadAgent.InsertFixedPosition (AAllocDefID          : integer;
                                                      var AFixedPositionID : integer ) : boolean;
const OPNAME = 'TAllocationDefLoadAgent.InsertFixedPosition';
var
  LDataSet    : TAbstractModelDataset;
  LImportDate : TDateTime;
  LSQL        : string;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned ( LDataSet ) then
      begin
        AFixedPositionID := GetLastFixedPositionID(AAllocDefID) + 1;
        lSQL:= 'INSERT  INTO FMSolveFixedPosition ' +
              '(Model, StudyAreaName, SubArea, Scenario, AllocDefID, FixedPositionID ) ' +
              'VALUES (' +
              QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
              QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
              QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
              QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
              IntToStr(AAllocDefID)  + ',' +
              IntToStr(AFixedPositionID) +
              ')';
        lDataSet.SetSQL(lSQL);
        lDataset.ExecSQL;
        lDataset.DataSet.Close;
        FAppModules.StudyArea.LastUpdateDate := Now();
        lImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if (lImportDate = NullDateTime) then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);
        Result := TRUE;
      end;
    finally
      lDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TAllocationDefLoadAgent.GetLastSpecificOrderID : integer;
const OPNAME = 'TAllocationDefLoadAgent.GetLastSpecificOrderID';
var
  lDataSet : TAbstractModelDataset;
  lSQL     : string;
begin
  Result := 0;
  try
    lSQL := 'SELECT MAX(SpecificOrderID) AS LastID FROM FMSolveSpecificOrder WHERE ' +
            GetScenarioWhereClause;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(lSQL);
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('LastID').AsInteger;
        LDataset.DataSet.Close;
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAllocationDefLoadAgent.InsertSpecificOrder (AAllocDefID          : integer;
                                                      var ASpecificOrderID : integer) : boolean;
const OPNAME = 'TAllocationDefLoadAgent.InsertSpecificOrder';
var
  LDataSet : TAbstractModelDataset;
  LImportDate : TDateTime;
  LSQL : string;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned ( LDataSet ) then
      begin
        ASpecificOrderID := GetLastSpecificOrderID + 1;
        lSQL:= 'INSERT  INTO FMSolveSpecificOrder ' +
              '(Model, StudyAreaName, SubArea, Scenario, AllocDefID, SpecificOrderID) ' +
              'VALUES (' +
              QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
              QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
              QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
              QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
              IntToStr(AAllocDefID)  + ',' +
              IntToStr(ASpecificOrderID) +
              ')';
        lDataSet.SetSQL(lSQL);
        lDataset.ExecSQL;
        lDataset.DataSet.Close;
        FAppModules.StudyArea.LastUpdateDate := Now();
        lImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if (lImportDate = NullDateTime) then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);
        Result := TRUE;
      end;
    finally
      lDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TAllocationDefLoadAgent.DeleteAllocationDefinition (AAllocDefID : integer): boolean;
const OPNAME = 'TAllocationDefLoadAgent.DeleteAllocationDefinition';
var
  lDataSet    : TAbstractModelDataset;
  lImportDate : TDateTime;
  lSQL        : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(lDataSet) then
      begin
        FAppModules.Database.StartTransaction;
        try
          lSQL := 'DELETE * FROM FMSupportSubSystem WHERE ' +
                  GetScenarioWhereClause + ' AND AllocDefID = ' + IntToStr(AAllocDefID);
          LDataSet.SetSQL(lSQL);
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          lSQL := 'DELETE * FROM FMDemandDefinition WHERE ' +
                  GetScenarioWhereClause + ' AND AllocDefID = ' + IntToStr(AAllocDefID);
          LDataSet.SetSQL(lSQL);
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          lSQL := 'DELETE * FROM FMAllocationLevel WHERE ' +
                  GetScenarioWhereClause + ' AND AllocDefID = ' + IntToStr(AAllocDefID);
          LDataSet.SetSQL(lSQL);
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          lSQL := 'DELETE * FROM FMUserCategory WHERE ' +
                  GetScenarioWhereClause + ' AND AllocDefID = ' + IntToStr(AAllocDefID);
          LDataSet.SetSQL(lSQL);
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          lSQL := 'DELETE * FROM FMSolveSpecificOrder WHERE ' +
                  GetScenarioWhereClause + ' AND AllocDefID = ' + IntToStr(AAllocDefID);
          LDataSet.SetSQL(lSQL);
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          lSQL := 'DELETE * FROM FMSolveFixedPosition WHERE ' +
                  GetScenarioWhereClause + ' AND AllocDefID = ' + IntToStr(AAllocDefID);
          LDataSet.SetSQL(lSQL);
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          lSQL := 'DELETE * FROM FMSupportChannel WHERE ' +
                  GetScenarioWhereClause +
                  ' AND AllocDefID = ' + IntToStr(AAllocDefID);
          LDataSet.SetSQL(lSQL);
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          lSQL := 'DELETE * FROM FMCoefficients WHERE ' +
                  GetScenarioWhereClause +
                  ' AND AllocDefID = ' + IntToStr(AAllocDefID);
          LDataSet.SetSQL(lSQL);
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          lSQL := 'DELETE * FROM FMSubSystem WHERE ' +
                  GetScenarioWhereClause + ' AND AllocDefID = ' + IntToStr(AAllocDefID);
          LDataSet.SetSQL(lSQL);
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          lSQL := 'DELETE * FROM FMAllocationDefinition WHERE ' +
                  GetScenarioWhereClause + ' AND AllocDefID = ' + IntToStr(AAllocDefID);
          LDataSet.SetSQL(lSQL);
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          FAppModules.StudyArea.LastUpdateDate := Now();
          lImportDate := FAppModules.StudyArea.GetStudyImportDate;
          if (lImportDate = NullDateTime) then
            FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

          FAppModules.Database.Commit;
          Result := TRUE;
        except
          FAppModules.Database.Rollback;
          raise;
        end;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TAllocationDefLoadAgent.DeleteCategoryByID ( AUserCategoryID,AAllocDefID : integer ) : boolean;
const OPNAME = 'TAllocationDefLoadAgent.DeleteCategoryByID';
var
  lDataSet    : TAbstractModelDataset;
  lImportDate : TDateTime;
  lSQL        : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(lDataSet) then
      begin
        lSQL := 'DELETE * FROM FMUserCategory WHERE ' +
                GetScenarioWhereClause +
                ' AND AllocDefID = ' + IntToStr(AAllocDefID)+
                ' AND UserCategoryID = ' + IntToStr(AUserCategoryID);

        LDataSet.SetSQL(lSQL);
        LDataset.ExecSQL;
        LDataset.DataSet.Close;

        FAppModules.StudyArea.LastUpdateDate := Now();

        lImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if (lImportDate = NullDateTime) then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

        Result := True;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TAllocationDefLoadAgent.DeleteAllocationLevelByID ( AAllocationLevelID,AAllocDefID : integer ) : boolean;
const OPNAME = 'TAllocationDefLoadAgent.DeleteAllocationLevelByID';
var
  lDataSet    : TAbstractModelDataset;
  lImportDate : TDateTime;
  lSQL        : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(lDataSet) then
      begin
        lSQL := 'DELETE * FROM FMAllocationLevel WHERE ' +
                GetScenarioWhereClause +
                ' AND AllocDefID = ' + IntToStr(AAllocDefID)+
                ' AND AllocLevelID = ' + IntToStr(AAllocationLevelID);

        LDataSet.SetSQL(lSQL);
        LDataset.ExecSQL;
        LDataset.DataSet.Close;

        FAppModules.StudyArea.LastUpdateDate := Now();

        lImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if (lImportDate = NullDateTime) then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

        Result := True;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TAllocationDefLoadAgent.DeleteSubSystemByID ( ASubSystemID, AAllocDefID : integer ) : boolean;
const OPNAME = 'TAllocationDefLoadAgent.DeleteSubSystemByID';
var
  lDataSet    : TAbstractModelDataset;
  lImportDate : TDateTime;
  lSQL        : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(lDataSet) then
      begin
        FAppModules.Database.StartTransaction;
        try
          lSQL := 'DELETE * FROM FMCoefficients WHERE ' +
                  GetScenarioWhereClause +
                  ' AND AllocDefID = ' + IntToStr(AAllocDefID)+
                  ' AND SubSystemID = ' + IntToStr(ASubSystemID);
          LDataSet.SetSQL(lSQL);
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          lSQL := 'DELETE * FROM FMSubSystem WHERE ' +
                  GetScenarioWhereClause +
                  ' AND AllocDefID = ' + IntToStr(AAllocDefID)+
                  ' AND SubSystemID = ' + IntToStr(ASubSystemID);
          LDataSet.SetSQL(lSQL);
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          FAppModules.StudyArea.LastUpdateDate := Now();
          lImportDate := FAppModules.StudyArea.GetStudyImportDate;
          if (lImportDate = NullDateTime) then
            FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

          FAppModules.Database.Commit;
          Result := TRUE;
        except
          FAppModules.Database.Rollback;
          raise;
        end;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TAllocationDefLoadAgent.DeleteSupportChannelByID (ASupportChannelID, AAllocDefID : integer ) : boolean;
const OPNAME = 'TAllocationDefLoadAgent.DeleteSupportChannelByID';
var
  lDataSet    : TAbstractModelDataset;
  lImportDate : TDateTime;
  lSQL        : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(lDataSet) then
      begin
        lSQL := 'DELETE * FROM FMSupportChannel WHERE ' +
                GetScenarioWhereClause +
                ' AND AllocDefID = ' + IntToStr(AAllocDefID)+
                ' AND SupportChannelID = ' + IntToStr(ASupportChannelID);

        LDataSet.SetSQL(lSQL);
        LDataset.ExecSQL;
        LDataset.DataSet.Close;

        FAppModules.StudyArea.LastUpdateDate := Now();

        lImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if (lImportDate = NullDateTime) then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

        Result := True;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TAllocationDefLoadAgent.DeleteDemandDefinitionByID ( ADemandDefID, AAllocDefID : integer ) : boolean;
const OPNAME = 'TAllocationDefLoadAgent.DeleteDemandDefinitionByID';
var
  lDataSet    : TAbstractModelDataset;
  lImportDate : TDateTime;
  lSQL        : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(lDataSet) then
      begin
        FAppModules.Database.StartTransaction;
        try
          lSQL := 'DELETE * FROM FMSupportSubSystem WHERE ' +
                  GetScenarioWhereClause +
                  ' AND AllocDefID = ' + IntToStr(AAllocDefID)+
                  ' AND DemandDefID = ' + IntToStr(ADemandDefID);
          LDataSet.SetSQL(lSQL);
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          lSQL := 'DELETE * FROM FMDemandDefinition WHERE ' +
                  GetScenarioWhereClause +
                  ' AND AllocDefID = ' + IntToStr(AAllocDefID)+
                  ' AND DemandDefID = ' + IntToStr(ADemandDefID);
          LDataSet.SetSQL(lSQL);
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          FAppModules.StudyArea.LastUpdateDate := Now();
          lImportDate := FAppModules.StudyArea.GetStudyImportDate;
          if (lImportDate = NullDateTime) then
            FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

          FAppModules.Database.Commit;
          Result := TRUE;
        except
          FAppModules.Database.Rollback;
          raise;
        end;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TAllocationDefLoadAgent.DeleteSupportSubSystemByID ( ASupportSubSystemID,
                                                              AAllocDefID, ADemandDefID : integer ) : boolean;
const OPNAME = 'TAllocationDefLoadAgent.DeleteSupportSubSystemByID';
var
  lDataSet    : TAbstractModelDataset;
  lImportDate : TDateTime;
  lSQL        : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(lDataSet) then
      begin
        lSQL := 'DELETE * FROM FMSupportSubSystem WHERE ' +
                GetScenarioWhereClause +
                ' AND AllocDefID = ' + IntToStr(AAllocDefID)+
                ' AND DemandDefID = ' + IntToStr(ADemandDefID) +
                ' AND SupportSubSystemID = ' + IntToStr ( ASupportSubSystemID );
        LDataSet.SetSQL(lSQL);
        LDataset.ExecSQL;
        LDataset.DataSet.Close;
        FAppModules.StudyArea.LastUpdateDate := Now();
        lImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if (lImportDate = NullDateTime) then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);
        Result := True;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TAllocationDefLoadAgent.DeleteCoeficient ( AAllocDefID ,ASubSystemID, AStartPercNr,
                                                    ACurveSetNr, ALoadCaseNr  : integer ) : boolean;
const OPNAME = 'TAllocationDefLoadAgent.DeleteCoeficient';
var
  lDataSet    : TAbstractModelDataset;
  lImportDate : TDateTime;
  lSQL        : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(lDataSet) then
      begin
        lSQL := 'DELETE * FROM FMCoefficients WHERE ' +
                GetScenarioWhereClause +
                ' AND AllocDefID = ' + IntToStr(AAllocDefID)+
                ' AND SubSystemID = ' + IntToStr(ASubSystemID) +
                ' AND StartStorageNr = ' + IntToStr(AStartPercNr) +
                ' AND CurveSetNr = ' + IntToStr(ACurveSetNr) +
                ' AND LoadCaseNr = ' + IntToStr ( ALoadCaseNr );
        LDataSet.SetSQL(lSQL);
        LDataset.ExecSQL;
        LDataset.DataSet.Close;
        FAppModules.StudyArea.LastUpdateDate := Now();
        lImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if (lImportDate = NullDateTime) then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);
        Result := True;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TAllocationDefLoadAgent.DeleteFixedPosition ( AAllocDefID : integer; AFixedPositionID : integer ) : boolean;
const OPNAME = 'TAllocationDefLoadAgent.DeleteFixedPosition';
var
  lDataSet    : TAbstractModelDataset;
  lImportDate : TDateTime;
  lSQL        : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(lDataSet) then
      begin
        lSQL := 'DELETE * FROM FMSolveFixedPosition WHERE ' +
                GetScenarioWhereClause +
                ' AND AllocDefID = ' + IntToStr(AAllocDefID)+
                ' AND FixedPositionID = ' + IntToStr(AFixedPositionID);

        LDataSet.SetSQL(lSQL);
        LDataset.ExecSQL;
        LDataset.DataSet.Close;
        FAppModules.StudyArea.LastUpdateDate := Now();
        lImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if (lImportDate = NullDateTime) then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);
        Result := True;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TAllocationDefLoadAgent.DeleteSpecificOrder ( AAllocDefID, ASpecificOrderID : integer ) : boolean;
const OPNAME = 'TAllocationDefLoadAgent.DeleteSpecificOrder';
var
  lDataSet    : TAbstractModelDataset;
  lImportDate : TDateTime;
  lSQL        : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(lDataSet) then
      begin
        lSQL := 'DELETE * FROM FMSolveSpecificOrder WHERE ' +
                GetScenarioWhereClause +
                ' AND AllocDefID = ' + IntToStr(AAllocDefID)+
                ' AND SpecificOrderID = ' + IntToStr ( ASpecificOrderID );

        LDataSet.SetSQL(lSQL);
        LDataset.ExecSQL;
        LDataset.DataSet.Close;
        FAppModules.StudyArea.LastUpdateDate := Now();
        lImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if (lImportDate = NullDateTime) then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);
        Result := True;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TAllocationDefLoadAgent.GetLastAllocationDefinitionID : integer;
const OPNAME = 'TAllocationDefLoadAgent.GetLastAllocationDefinitionID';
var
  lDataSet : TAbstractModelDataset;
  lSQL     : string;
begin
  Result := 0;
  try
    lSQL := 'SELECT MAX(AllocDefID) AS LastID FROM FMAllocationDefinition WHERE ' +
            GetScenarioWhereClause;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(lSQL);
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('LastID').AsInteger;
        LDataset.DataSet.Close;
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAllocationDefLoadAgent.GetLastUserCategoryID : integer;
const OPNAME = 'TAllocationDefLoadAgent.GetLastUserCategoryID';
var
  lDataSet : TAbstractModelDataset;
  lSQL     : string;
begin
  Result := 0;
  try
    lSQL := 'SELECT MAX(UserCategoryID) AS LastID FROM FMUserCategory WHERE ' +
            GetScenarioWhereClause;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(lSQL);
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('LastID').AsInteger;
        LDataset.DataSet.Close;
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAllocationDefLoadAgent.GetLastAllocationLevelID : integer;
const OPNAME = 'TAllocationDefLoadAgent.GetLastAllocationLevelID';
var
  lDataSet : TAbstractModelDataset;
  lSQL     : string;
begin
  Result := 0;
  try
    lSQL := 'SELECT MAX(AllocLevelID) AS LastID FROM FMAllocationLevel WHERE ' +
            GetScenarioWhereClause;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(lSQL);
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('LastID').AsInteger;
        LDataset.DataSet.Close;
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAllocationDefLoadAgent.GetLastSubSystemID : integer;
const OPNAME = 'TAllocationDefLoadAgent.GetLastSubSystemID';
var
  lDataSet : TAbstractModelDataset;
  lSQL     : string;
begin
  Result := 0;
  try
    lSQL := 'SELECT MAX(SubSystemID) AS LastID FROM FMSubSystem WHERE ' +
            GetScenarioWhereClause;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(lSQL);
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('LastID').AsInteger;
        LDataset.DataSet.Close;
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAllocationDefLoadAgent.GetLastDemandDefID : integer;
const OPNAME = 'TAllocationDefLoadAgent.GetLastDemandDefID';
var
  lDataSet : TAbstractModelDataset;
  lSQL     : string;
begin
  Result := 0;
  try
    lSQL := 'SELECT MAX(DemandDefID) AS LastID FROM FMDemandDefinition WHERE ' +
            GetScenarioWhereClause;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(lSQL);
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('LastID').AsInteger;
        LDataset.DataSet.Close;
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAllocationDefLoadAgent.GetLastSupportSubSystemID : integer;
const OPNAME = 'TAllocationDefLoadAgent.GetLastSupportSubSystemID';
var
  lDataSet : TAbstractModelDataset;
  lSQL     : string;
begin
  Result := 0;
  try
    lSQL := 'SELECT MAX(SupportSubSystemID) AS LastID FROM FMSupportSubSystem WHERE ' +
            GetScenarioWhereClause;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(lSQL);
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('LastID').AsInteger;
        LDataset.DataSet.Close;
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.
