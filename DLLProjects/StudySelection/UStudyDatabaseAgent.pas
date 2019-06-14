//
//
//  UNIT      : Contains TStudyDatabaseAgent Class
//  AUTHOR    : Dziedzi Ramulondi (PDNA)
//  DATE      : 06/06/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//

unit UStudyDatabaseAgent;

interface

uses
  Classes,
  Contnrs,
  Sysutils,
  Db,
  UConstants,
  UAbstractObject,
  UDatabaseUtilities;

const
  C_SubAreaShapeFile = 'Covers\Study Sub-Areas\';
  C_StudyShapeFile = 'Covers\Study Area\';
  C_ClassName = 'TGISStudyAreaSelectorPanel';

type
  TStudyTables = class(TObject)
  public
    FModel: string;
    FTableName: string;
    FSequence: integer;
    FContext: string;
    FIndexCount: integer;
  end;

  TStudyDatabaseAgent = class(TAbstractAppObject)
  protected
    FLoaded: boolean;
    FModelID: string;
    FStudyTableNames: TObjectList;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    function PopulateTablesSQL(AModelID: string): string;
    function PopulateStudyTableNames(AModelID: string): boolean;

    function DeleteStudySQL(ADeleteTableName: string): string;
    function DeleteModelSQL(ADeleteTableName: string): string;
    function DeleteSubAreaSQL(ADeleteTableName: string): string;
    function DeleteScenarioSQL(ADeleteTableName: string): string;

    function SubAreaCount(AModelID,AStudyID: string): integer;
    function ScenarioCount(AModelID,AStudyID,ASubareaID: string): integer;

    function UpdateStudySQL: string;
    function UpdateSubAreaSQL: string;
    function UpdateScenarionSQL: string;
    function UpdateSubAreaCoordsSQL: string;

    function NewStudySQL: string;
    function NewSubAreaSQL: string;
    function NewScenarionSQL: string;
    function CreateDrawingSQL: string;
    function GetDrawingGroupSQL: string;
    function GetDrawingSQL: string;
    function GetMaxDrawingGroupIdSQL: string;
    function GetMaxDrawingIdSQL: string;
    function CreateDrawingGroupSQL: string;
    function GetVSDFileName (AStudyAreaName,ASubArea,AScenarioLabel,AGroupName   : string; ADrawingName : string) : string;

  public
    function DeleteStudy(AProgressFunction:TDBProgressUpdate;AStudyID: string): boolean;
    function DeleteModel(AProgressFunction:TDBProgressUpdate;AModelID,AStudyID: string): boolean;
    function DeleteSubArea(AProgressFunction:TDBProgressUpdate;AModelID,AStudyID,ASubareaID: string): boolean;
    function DeleteScenario(AProgressFunction:TDBProgressUpdate;AModelID,AStudyID,ASubareaID,AScenarioID: string): boolean;

    function CreateStudy(AModel,AStudyAreaName,AConsultant,AClient,AStudyLabel,AStudyAreaDescr: string;
                         AStudyDate: TDateTime; AStudyNumber, AShapeFile : string): boolean;
    function CreateSubArea(AModel, AStudyAreaName, ASubArea, ASubAreaLabel, ASubAreaDescr, AShapeFile : string): boolean;
    function CreateScenario(AModel, AStudyAreaName, ASubArea, AScenario, AScenarioLabel, AScenarioDescr,
                            ADataFilesPrefix, ADataFilesPath: string; AFilesLoaded: boolean; AEditable : boolean;
                            ACalenderStartMonth: integer; AVersion: string):boolean;
    function CreateScenarioVisioDiagram(AModel,AStudyAreaName,ASubArea,AScenarioLabel: string): boolean;

    function UpdateStudy(AModel,AStudyAreaName,AConsultant,AClient,AStudyLabel,AStudyAreaDescr: string;
                         AStudyDate: TDateTime; AStudyNumber, AShapeFile : string): boolean;
    function UpdateSubArea(AModel, AStudyAreaName, ASubArea, ASubAreaLabel, ASubAreaDescr, AShapeFile : string): boolean;
    function UpdateSubAreaCoords(AModel, AStudyAreaName, ASubArea: string;ATopLeft, ATopRight, ABottomLeft, ABottomRight : double ): boolean;

    function UpdateScenario(AModel, AStudyAreaName, ASubArea, AScenario, AScenarioLabel, AScenarioDescr,
                            ADataFilesPrefix, ADataFilesPath: string; AFilesLoaded: boolean; AEditable : boolean;
                            ACalenderStartMonth: integer;
                            AVersion: string):boolean;
    function CheckAndCreateRainFallGaugesProject (AModel, AStudyAreaName, ASubArea, AScenario, AScenarioLabel, AScenarioDescr,
                            ADataFilesPrefix, ADataFilesPath: string;
                            AFilesLoaded: boolean; AEditable : boolean;
                            ACalenderStartMonth: integer;
                            AVersion: string):boolean;

  end;


implementation

uses
  WinApi.Windows,

  UUtilities,
  UDataSetType,
  UErrorHandlingOperations;

{ TStudyDatabaseAgent }

procedure TStudyDatabaseAgent.CreateMemberObjects;
const OPNAME = 'TStudyDatabaseAgent.CreateMemberObjects';
begin
  inherited;
  try
    FLoaded      := False;
    FModelID     := '';
    FStudyTableNames := TObjectList.Create(True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyDatabaseAgent.DestroyMemberObjects;
const OPNAME = 'TStudyDatabaseAgent.DestroyMemberObjects';
begin
  inherited;
  try
    FStudyTableNames.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyDatabaseAgent.PopulateTablesSQL(AModelID: string): string;
const OPNAME = 'TStudyDatabaseAgent.PopulateTablesSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model,StudyTableName,Sequence,Context,IndexLevel'+
              ' FROM StudyTableNames'+
              ' WHERE Model = ' + QuotedStr(AModelID)+
              ' ORDER BY Sequence';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyDatabaseAgent.PopulateStudyTableNames(AModelID: string): boolean;
const OPNAME = 'TStudyDatabaseAgent.PopulateStudyTableNames';
var
  LDataSet    : TAbstractModelDataset;
  LStudyTable : TStudyTables;
begin
  Result := False;
  try
    if (not FLoaded) or (FModelID <> AModelID) then
    begin
      FStudyTableNames.Clear;
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      try
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(PopulateTablesSQL(AModelID));
        LDataSet.DataSet.Open;
        while not(LDataSet.DataSet.EOF) do
        begin
          LStudyTable := TStudyTables.Create;
          LStudyTable.FModel := Trim(LDataSet.DataSet.FieldByName('Model').AsString);
          LStudyTable.FTableName := Trim(LDataSet.DataSet.FieldByName('StudyTableName').AsString);
          LStudyTable.FSequence := LDataSet.DataSet.FieldByName('Sequence').AsInteger;
          LStudyTable.FContext := Trim(LDataSet.DataSet.FieldByName('Context').AsString);
          LStudyTable.FIndexCount := LDataSet.DataSet.FieldByName('IndexLevel').AsInteger;
          FStudyTableNames.Add(LStudyTable);
          LDataSet.DataSet.Next;
        end;
        FLoaded := true;
        FModelID := AModelID;
      finally
        LDataSet.Free;
      end;
    end;
    Result := True;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyDatabaseAgent.NewStudySQL: string;
const OPNAME = 'TStudyDatabaseAgent.NewStudySQL';
begin
  Result := '';
  try
    Result :=
    'INSERT INTO StudyArea'+
    '(Model,StudyAreaName,StudyDate,Consultant,Client,StudyNumber,StudyLabel,StudyAreaDescr,ShapeFileName)'+
    ' VALUES '+
    '(:AModel,:AStudyAreaName,:AStudyDate,:AConsultant,:AClient,:AStudyNumber,:AStudyLabel,:AStudyAreaDescr,:AShapeFileName)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyDatabaseAgent.NewSubAreaSQL: string;
const OPNAME = 'TStudyDatabaseAgent.NewSubAreaSQL';
begin
  Result := '';
  try
    Result :=
    'INSERT INTO StudySubArea'+
    '(Model, StudyAreaName, SubArea, SubAreaLabel, SubAreaDescr, ShapeFileName)'+
    ' VALUES '+
    '(:AModel, :AStudyAreaName, :ASubArea, :ASubAreaLabel, :ASubAreaDescr, :AShapeFileName)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyDatabaseAgent.NewScenarionSQL: string;
const OPNAME = 'TStudyDatabaseAgent.NewScenarionSQL';
begin
  Result := '';
  try
    Result :=
    'INSERT INTO StudyScenario'+
    '(Model, SubArea, Scenario, StudyAreaName, ScenarioLabel, ScenarioDescr, DataFilesPrefix, DataFilesPath,FilesLoaded,CalenderStartMonth, Version, DataImported)'+
    ' VALUES'+
    ' (:AModel, :ASubArea, :AScenario, :AStudyAreaName, :AScenarioLabel, :AScenarioDescr, :ADataFilesPrefix, :ADataFilesPath,:AFilesLoaded,:ACalenderStartMonth, :AVersion,:DataImported)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyDatabaseAgent.UpdateStudySQL: string;
const OPNAME = 'TStudyDatabaseAgent.UpdateStudySQL';
begin
  Result := '';
  try
    Result := 'UPDATE StudyArea SET '+
              ' StudyDate       = :AStudyDate'+
              ' ,Consultant     = :AConsultant'+
              ' ,Client         = :AClient'+
              ' ,StudyNumber    = :AStudyNumber'+
              ' ,StudyLabel     = :AStudyLabel'+
              ' ,StudyAreaDescr = :AStudyAreaDescr'+
              ' ,ShapeFileName  = :AShapeFileName'+
              ' WHERE StudyAreaName = :AStudyAreaName';
//              ' WHERE Model     = :AModel'+
//              ' AND StudyAreaName   = :AStudyAreaName';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyDatabaseAgent.UpdateSubAreaSQL: string;
const OPNAME = 'TStudyDatabaseAgent.UpdateSubAreaSQL';
begin
  Result := '';
  try
    Result := 'UPDATE StudySubArea SET '+
              ' SubAreaLabel     = :ASubAreaLabel'+
              ' ,SubAreaDescr    = :ASubAreaDescr'+
              ' ,ShapeFileName   = :AShapeFileName'+
              ' WHERE Model      = :AModel'+
              ' AND StudyAreaName    = :AStudyAreaName'+
              ' AND SubArea          = :ASubArea';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyDatabaseAgent.UpdateSubAreaCoordsSQL: string;
const OPNAME = 'TStudyDatabaseAgent.UpdateSubAreaCoordsSQL';
begin
  Result := '';
  try
    Result := 'UPDATE StudySubArea SET '+
              ' TopLeftCoord      = :ATopLeftCoord'+
              ' ,TopRightCoord    = :ATopRightCoord'+
              ' ,BottomLeftCoord  = :ABottomLeftCoord'+
              ' ,BottomRightCoord = :ABottomRightCoord'+
              ' WHERE Model       = :AModel'+
              ' AND StudyAreaName = :AStudyAreaName'+
              ' AND SubArea       = :ASubArea';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyDatabaseAgent.UpdateScenarionSQL: string;
const OPNAME = 'TStudyDatabaseAgent.UpdateScenarionSQL';
begin
  Result := '';
  try
    Result := 'UPDATE StudyScenario SET '+
              '  ScenarioLabel      = :AScenarioLabel'+
              ' ,ScenarioDescr      = :AScenarioDescr'+
              ' ,DataFilesPrefix    = :ADataFilesPrefix'+
              ' ,DataFilesPath      = :ADataFilesPath'+
              ' ,FilesLoaded        = :AFilesLoaded'+
              ' ,CalenderStartMonth = :ACalenderStartMonth'+
              ' ,Version            = :AVersion'+
              ' ,DataImported       = :DataImported'+
              ' WHERE Model         = :AModel'+
              ' AND StudyAreaName   = :AStudyAreaName'+
              ' AND SubArea         = :ASubArea'+
              ' AND Scenario        = :AScenario';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyDatabaseAgent.DeleteStudySQL(ADeleteTableName: string): string;
const OPNAME = 'TStudyDatabaseAgent.DeleteStudySQL';
begin
  Result := '';
  try
    Result := ' DELETE FROM ' + ADeleteTableName +
      ' WHERE StudyAreaName = :AStudyAreaName' ;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyDatabaseAgent.DeleteModelSQL(ADeleteTableName: string): string;
const OPNAME = 'TStudyDatabaseAgent.DeleteModelSQL';
begin
  Result := '';
  try
    Result := ' DELETE FROM ' + ADeleteTableName +
      ' WHERE MODEL       = :AModel' +
      ' AND StudyAreaName = :AStudyAreaName' ;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyDatabaseAgent.DeleteSubAreaSQL(ADeleteTableName: string): string;
const OPNAME = 'TStudyDatabaseAgent.DeleteSubareaSQL';
begin
  Result := '';
  try
    Result := ' DELETE FROM ' + ADeleteTableName +
      ' WHERE MODEL       = :AModel'+
      ' AND StudyAreaName = :AStudyAreaName'+
      ' AND SubArea       = :ASubArea';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyDatabaseAgent.DeleteScenarioSQL(ADeleteTableName: string): string;
const OPNAME = 'TStudyDatabaseAgent.DeleteScenarioSQL';
begin
  Result := '';
  try
    Result := ' DELETE FROM ' + ADeleteTableName +
      ' WHERE MODEL       = :AModel' +
      ' AND StudyAreaName = :AStudyAreaName'+
      ' AND SubArea       = :ASubArea'+
      ' AND Scenario      = :AScenario';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyDatabaseAgent.DeleteStudy(AProgressFunction: TDBProgressUpdate; AStudyID: string): boolean;
const OPNAME = 'TStudyDatabaseAgent.DeleteStudy';
var
  LIndex : integer;
  LDataSet : TAbstractModelDataset;
  LStudyTable:TStudyTables;
begin
  Result := false;
  try
    if (not PopulateStudyTableNames(AStudyID)) then
      Exit;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      for LIndex := 0 to FStudyTableNames.Count - 1 do
      begin
        if Assigned(AProgressFunction) then
          AProgressFunction('',LIndex, FStudyTableNames.Count);

        LStudyTable := TStudyTables(FStudyTableNames.Items[LIndex]);
        if (LStudyTable.FIndexCount < 2) then
          Continue;

        LDataSet.DataSet.Close;
        LDataSet.SetSQL(DeleteModelSQL(LStudyTable.FTableName));
        LDataSet.SetParams(['AStudyAreaName'], [AStudyID]);
        LDataSet.ExecSQL;
      end;
      Result := true;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyDatabaseAgent.DeleteModel(AProgressFunction: TDBProgressUpdate; AModelID,AStudyID: string): boolean;
const OPNAME = 'TStudyDatabaseAgent.DeleteModel';
var
  LIndex : integer;
  LDataSet : TAbstractModelDataset;
  LStudyTable:TStudyTables;
begin
  Result := false;
  try
    if (not PopulateStudyTableNames(AModelID)) then
      Exit;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      for LIndex := 0 to FStudyTableNames.Count - 1 do
      begin
        if Assigned(AProgressFunction) then
          AProgressFunction('',LIndex, FStudyTableNames.Count);

        LStudyTable := TStudyTables(FStudyTableNames.Items[LIndex]);
        if (LStudyTable.FIndexCount < 2) then
          Continue;

        LDataSet.DataSet.Close;
        LDataSet.SetSQL(DeleteModelSQL(LStudyTable.FTableName));
        LDataSet.SetParams(['AModel', 'AStudyAreaName'], [AModelID, AStudyID]);
        LDataSet.ExecSQL;
      end;
      Result := true;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyDatabaseAgent.DeleteSubArea(AProgressFunction: TDBProgressUpdate; AModelID,
         AStudyID, ASubareaID: string): boolean;
const OPNAME = 'TStudyDatabaseAgent.DeleteSubArea';
var
  LIndex : integer;
  LDataSet : TAbstractModelDataset;
  LStudyTable:TStudyTables;
begin
  Result := false;
  try
    if (SubAreaCount(AModelID,AStudyID) <= 1) then
    begin
      Result := DeleteModel(AProgressFunction,AModelID,AStudyID);
      Exit;
    end;

    if (not PopulateStudyTableNames(AModelID)) then
      Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      for LIndex := 0 to FStudyTableNames.Count - 1 do
      begin
        if Assigned(AProgressFunction) then
          AProgressFunction('',LIndex, FStudyTableNames.Count);

        LStudyTable := TStudyTables(FStudyTableNames.Items[LIndex]);
        if (LStudyTable.FIndexCount < 3) then
          Continue;

        LDataSet.DataSet.Close;
        LDataSet.SetSQL(DeleteSubAreaSQL(LStudyTable.FTableName));
        LDataSet.SetParams(['AModel','AStudyAreaName','ASubArea'], [AModelID,AStudyID,ASubareaID]);
        LDataSet.ExecSQL;
      end;
      Result := true;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyDatabaseAgent.DeleteScenario(AProgressFunction: TDBProgressUpdate; AModelID,
         AStudyID, ASubareaID, AScenarioID: string): boolean;
const OPNAME = 'TStudyDatabaseAgent.DeleteScenario';
var
  LIndex : integer;
  LDataSet : TAbstractModelDataset;
  LStudyTable:TStudyTables;
begin
  Result := false;
  try
    if (ScenarioCount(AModelID,AStudyID,ASubareaID) <= 1) then
    begin
      Result := DeleteSubArea(AProgressFunction,AModelID,AStudyID, ASubareaID);
      Exit;
    end;

    if (not PopulateStudyTableNames(AModelID)) then
      Exit;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      for LIndex := 0 to FStudyTableNames.Count - 1 do
      begin
        if Assigned(AProgressFunction) then
          AProgressFunction('',LIndex, FStudyTableNames.Count);

        LStudyTable := TStudyTables(FStudyTableNames.Items[LIndex]);
        if (LStudyTable.FIndexCount < 4) then
          Continue;

        LDataSet.DataSet.Close;
        LDataSet.SetSQL(DeleteScenarioSQL(LStudyTable.FTableName));
        LDataSet.SetParams(['AModel','AStudyAreaName','ASubArea','AScenario'], [AModelID,AStudyID,ASubareaID,AScenarioID]);
        LDataSet.ExecSQL;
      end;
      Result := true;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyDatabaseAgent.CreateStudy(AModel, AStudyAreaName,
  AConsultant, AClient, AStudyLabel, AStudyAreaDescr: string;
  AStudyDate: TDateTime; AStudyNumber, AShapeFile : string): boolean;
const OPNAME = 'TStudyDatabaseAgent.CreateStudy';
var
  LDataSet : TAbstractModelDataset;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset ( integer ( dtExecSQL ), LDataset );
    try
      if ( LDataset <> nil ) then
      begin
        LDataSet.SetSQL(NewStudySQL);
        LDataSet.ReplaceSQLParam('AModel',          Copy(Trim(AModel),          1,  50));
        LDataSet.ReplaceSQLParam('AStudyAreaName',  Copy(Trim(AStudyAreaName),  1,  50));
        LDataSet.ReplaceSQLParam('AStudyDate',      DateTimeToStr(AStudyDate));
        LDataSet.ReplaceSQLParam('AConsultant',     Copy(Trim(AConsultant),     1, 255));
        LDataSet.ReplaceSQLParam('AClient',         Copy(Trim(AClient),         1, 255));
        LDataSet.ReplaceSQLParam('AStudyNumber',    Copy(Trim(AStudyNumber),    1,  50));
        LDataSet.ReplaceSQLParam('AStudyLabel',     Copy(Trim(AStudyLabel),     1, 255));
        LDataSet.ReplaceSQLParam('AStudyAreaDescr', Copy(Trim(AStudyAreaDescr), 1,  50));
        LDataSet.ReplaceSQLParam('AShapeFileName',  Copy(Trim(AShapeFile),      1, 255));
        LDataSet.ExecSQL;
      end;
      Result := True;
    finally
      FreeAndNil ( LDataset );
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyDatabaseAgent.CreateSubArea(AModel, AStudyAreaName,
  ASubArea, ASubAreaLabel, ASubAreaDescr, AShapeFile: string): boolean;
const OPNAME = 'TStudyDatabaseAgent.CreateSubArea';
var
  LDataSet : TAbstractModelDataset;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.SetSQL(NewSubAreaSQL);
      LDataSet.ReplaceSQLParam('AModel',          Copy(Trim(AModel),          1,  50));
      LDataSet.ReplaceSQLParam('AStudyAreaName',  Copy(Trim(AStudyAreaName),  1,  50));
      LDataSet.ReplaceSQLParam('ASubAreaLabel',   Copy(Trim(ASubAreaLabel),   1, 255));
      LDataSet.ReplaceSQLParam('ASubAreaDescr',   Copy(Trim(ASubAreaDescr),   1, 255));
      LDataSet.ReplaceSQLParam('ASubArea',        Copy(Trim(ASubArea),        1,  50));
      LDataSet.ReplaceSQLParam('AShapeFileName',  Copy(Trim(AShapeFile),      1, 255));
      LDataSet.ExecSQL;
      Result := True;
    finally
      FreeAndNil ( LDataset );
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyDatabaseAgent.CreateScenario (AModel, AStudyAreaName, ASubArea, AScenario,
                                             AScenarioLabel, AScenarioDescr, ADataFilesPrefix,
                                             ADataFilesPath      : string;
                                             AFilesLoaded        : boolean;
                                             AEditable           : boolean;
                                             ACalenderStartMonth : integer;
                                             AVersion            : string): boolean;
const OPNAME = 'TStudyDatabaseAgent.CreateScenario';
var
  LDataSet : TAbstractModelDataset;
  lSQL     : string;
begin
  Result := False;
  try
    if ((UpperCase(AModel) = 'RAINFALL') AND (Trim(AScenario) = '')) then
      Result := TRUE
    else
    begin
      if (AScenarioLabel = '') then
        AScenarioLabel := AScenario;
      if (AScenarioDescr = '') then
        AScenarioDescr := AScenario;
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      try
        LDataSet.SetSQL(NewScenarionSQL);
        LDataSet.ReplaceSQLParam('AModel',           Copy(Trim(AModel),           1,  50));
        LDataSet.ReplaceSQLParam('AStudyAreaName',   Copy(Trim(AStudyAreaName),   1,  50));
        LDataSet.ReplaceSQLParam('ASubArea',         Copy(Trim(ASubArea),         1,  50));
        LDataSet.ReplaceSQLParam('AScenarioLabel',   Copy(Trim(AScenarioLabel),   1, 255));
        LDataSet.ReplaceSQLParam('AScenarioDescr',   Copy(Trim(AScenarioDescr),   1, 255));
        LDataSet.ReplaceSQLParam('AScenario',        Copy(Trim(AScenario),        1,  50));
        LDataSet.ReplaceSQLParam('ADataFilesPrefix', Copy(Trim(ADataFilesPrefix), 1,  10));
        LDataSet.ReplaceSQLParam('ADataFilesPath',   Copy(Trim(ADataFilesPath),   1, 255));
        LDataSet.ReplaceSQLParam('AVersion',         Copy(Trim(AVersion),         1,  15));
        LDataSet.ReplaceSQLParam('ACalenderStartMonth', IntToStr(ACalenderStartMonth));
        if AFilesLoaded then
          LDataSet.ReplaceSQLParam('AFilesLoaded', 'Y')
        else
          LDataSet.ReplaceSQLParam('AFilesLoaded', 'N');
        if AEditable then
          LDataSet.ReplaceSQLParam('DataImported', 'N')
        else
          LDataSet.ReplaceSQLParam('DataImported', 'Y');
        LDataSet.ExecSQL;
        if AModel = CYield then
          CreateScenarioVisioDiagram(AModel,AStudyAreaName,ASubArea,AScenarioLabel);

        if ((UpperCase(AModel) = 'RAINFALL') AND
            (UpperCase(AScenario) <> 'PROJECT GAUGES')) then
        begin
          lDataset.DataSet.Close;
          lSQL := 'INSERT INTO RainfallCatchment ' +
                  ' (Model, StudyAreaName, SubArea, Scenario) VALUES (' +
                  QuotedStr(AModel) + ',' +
                  QuotedStr(AStudyAreaName) + ',' +
                  QuotedStr(ASubArea) + ',' +
                  QuotedStr(AScenario) + ')';
          lDataset.SetSQL(lSQL);
          lDataset.ExecSQL;
        end;

        Result := true;
      finally
        lDataset.DataSet.Close;
        LDataSet.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TStudyDatabaseAgent.GetDrawingGroupSQL: string;
const OPNAME = 'TStudyDatabaseAgent.GetDrawingGroupSQL';
begin
  Result := '';
  try
    Result := 'SELECT DrawingGroupID,DrawingGroupName FROM VNVDrawingGroup  ' ;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyDatabaseAgent.GetDrawingSQL: string;
const OPNAME = 'TStudyDatabaseAgent.GetDrawingSQL';
begin
  Result := '';
  try
    Result := 'SELECT DrawingGroupID,DrawingID,DrawingName,GISMode FROM VNVDrawing  ' ;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyDatabaseAgent.GetMaxDrawingGroupIdSQL: string;
const OPNAME = 'TStudyDatabaseAgent.GetMaxDrawingGroupIdSQL';
begin
  Result := '';
  try
    Result := 'SELECT MAX(DrawingGroupID) AS MAXDrawingGroupID FROM VNVDrawingGroup  ' ;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyDatabaseAgent.GetMaxDrawingIdSQL: string;
const OPNAME = 'TStudyDatabaseAgent.GetMaxDrawingIdSQL';
begin
  Result := '';
  try
    Result := 'SELECT MAX(DrawingID) AS MAXDrawingID FROM VNVDrawing  ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyDatabaseAgent.CreateDrawingGroupSQL: string;
const OPNAME = 'TStudyDatabaseAgent.CreateDrawingGroupSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO VNVDrawingGroup'+
              ' (Model,StudyAreaName,SubArea,Scenario,DrawingGroupID,DrawingGroupName)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:DrawingGroupID,:DrawingGroupName)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TStudyDatabaseAgent.CreateDrawingSQL: string;
const OPNAME = 'TStudyDatabaseAgent.CreateDrawingSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO VNVDrawing'+
              ' (Model,StudyAreaName,SubArea,Scenario,DrawingGroupID,DrawingID,DrawingName,GISMode)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:DrawingGroupID,:DrawingID,:DrawingName,:GISMode)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TStudyDatabaseAgent.CreateScenarioVisioDiagram(AModel,AStudyAreaName,ASubArea,AScenarioLabel: string): boolean;
const OPNAME = 'TStudyDatabaseAgent.CreateScenarioVisioDiagram';
var
  LDataSet : TAbstractModelDataset;
  LGDataSet : TAbstractModelDataset;
  LDDataSet : TAbstractModelDataset;
  LFileName,
  LPath, LSourceFile,
  LSQL : string;
//  LDrawingID,
  LDrawingGroupID: integer;
  LGroupDrawingName,
  LDrawingName: string;
  LGISMode: char;
  LMaxGId,
  LMaxId   : integer;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LGDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDDataSet);
    try
      LSQL :=
        ' SELECT DrawingGroupID,DrawingGroupName FROM VNVDrawingGroup  ' +
        ' WHERE         ' +
        ' (Model         = ' + QuotedStr(AModel)     + ') AND ' +
        ' (StudyAreaName = ' + QuotedStr(AStudyAreaName) + ') AND ' +
        ' (SubArea       = ' + QuotedStr(ASubArea)   + ') ';

      LDataSet.SetSQL(LSQL);
      LDataSet.DataSet.Open;
      LGroupDrawingName    := Trim(LDataSet.DataSet.FieldByName('DrawingGroupName').AsString);
      if not(LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
      begin

        LDrawingGroupID := LDataSet.DataSet.FieldByName('DrawingGroupID').AsInteger;
        LDataSet.DataSet.Close;

        LSQL := ' SELECT DrawingGroupID,DrawingID,DrawingName,GISMode FROM VNVDrawing  ' +
                ' WHERE         ' +
                ' (Model         = ' + QuotedStr(AModel)     + ') AND ' +
                ' (StudyAreaName = ' + QuotedStr(AStudyAreaName) + ') AND ' +
                ' (SubArea       = ' + QuotedStr(ASubArea)   + ') ';

        LSQL := LSQL + ' AND DrawingGroupID = '+IntToStr(LDrawingGroupID);
        LDataSet.SetSQL(LSQL);
        LDataSet.DataSet.Open;

        if not(LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
        begin
//          LDrawingGroupID := LDataSet.DataSet.FieldByName('DrawingGroupID').AsInteger;
//          LDrawingID      := LDataSet.DataSet.FieldByName('DrawingID').AsInteger;
          LDrawingName    := Trim(LDataSet.DataSet.FieldByName('DrawingName').AsString);
//          LGISMode        := (Trim(LDataSet.DataSet.FieldByName('GISMode').AsString)+'N')[1];


          LMaxGId := 0;
          LGDataSet.SetSQL(GetMaxDrawingGroupIdSQL  +
                           ' WHERE         ' +
                           ' (Model         = ' + QuotedStr(AModel)     + ') AND ' +
                           ' (StudyAreaName = ' + QuotedStr(AStudyAreaName) + ') AND ' +
                           ' (SubArea       = ' + QuotedStr(ASubArea)   + ') '   );

          LGDataSet.DataSet.Open;
          if not(LGDataSet.DataSet.Eof and LGDataSet.DataSet.Bof) then
          begin
            LMaxGId := LGDataSet.DataSet.FieldByName('MAXDrawingGroupID').AsInteger;
          end;
          LMaxGId := LMaxGId + 1;
          LDDataSet.SetSQL(CreateDrawingGroupSQL);

          LDDataSet.SetParams(['Model'], [AModel]);
          LDDataSet.SetParams(['StudyAreaName'], [AStudyAreaName]);
          LDDataSet.SetParams(['SubArea'], [ASubArea]);
          LDDataSet.SetParams(['Scenario'], [AScenarioLabel]);
          LDDataSet.SetParams(['DrawingGroupID'], [IntToStr(LMaxGId)]);
          LDDataSet.SetParams(['DrawingGroupName'], [AScenarioLabel]);

          LDDataSet.ExecSQL;



          LMaxId := 0;
          LDataSet.SetSQL(GetMaxDrawingIdSQL +
                           ' WHERE         ' +
                           ' (Model         = ' + QuotedStr(AModel)     + ') AND ' +
                           ' (StudyAreaName = ' + QuotedStr(AStudyAreaName) + ') AND ' +
                           ' (SubArea       = ' + QuotedStr(ASubArea)   + ') AND ' +
                           ' (Scenario      = ' + QuotedStr(AScenarioLabel)  + ') ' );
          LDataSet.DataSet.Open;
          if not(LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
          begin
            LMaxId := LDataSet.DataSet.FieldByName('MAXDrawingID').AsInteger;
          end;
          LMaxId := LMaxId + 1;

          LGISMode := 'N';

          LDataSet.DataSet.Close;
          LDataSet.SetSQL(CreateDrawingSQL);
          LDataSet.SetParams(['Model'], [AModel]);
          LDataSet.SetParams(['StudyAreaName'], [AStudyAreaName]);
          LDataSet.SetParams(['SubArea'], [ASubArea]);
          LDataSet.SetParams(['Scenario'], [AScenarioLabel]);
          LDataSet.SetParams(['DrawingGroupID'], [IntToStr(LMaxGId)]);
          LDataSet.SetParams(['DrawingID'], [IntToStr(LMaxId)]);
          LDataSet.SetParams(['DrawingName'], ['Fig '+IntToStr(LMaxId)]);
          LDataSet.SetParams(['GISMode'], [LGISMode]);

          LDataSet.ExecSQL;


          LSourceFile := NetworkDiagramsPath +ChopCharacters(AStudyAreaName) + '\'+ ChopCharacters(ASubArea)+'\'+ ChopCharacters(LGroupDrawingName)+'\'+ LGroupDrawingName+'-'+ LDrawingName+'.VSD'; //'DefaultDrawingYield.VSD';
          if (FileExists(LSourceFile)) then
          begin

            LFileName := GetVSDFileName(AStudyAreaName, ASubArea, AScenarioLabel,AScenarioLabel , 'Fig '+IntToStr(LMaxId));
            LPath     := ExtractFilePath(LFileName);
            if (not DirectoryExists(LPath)) then
              ForceDirectories(LPath);
            Result := CopyFile(PChar(LSourceFile), PChar(LFileName), True);
          end;
        end;
      end;
      Result := true;
    finally
        FreeAndNil(LDataSet);
        FreeAndNil(LDDataSet);
        FreeAndNil(LGDataSet);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{$WARN SYMBOL_PLATFORM OFF}

function TStudyDatabaseAgent.GetVSDFileName (AStudyAreaName,ASubArea,AScenarioLabel,AGroupName   : string; ADrawingName : string) : string;
const OPNAME = 'TStudyDatabaseAgent.GetVSDFileName';
var
  lFileName : string;
  lPath     : string;
begin
  Result := '';
  try
    LPath := NetworkDiagramsPath +
             ChopCharacters(AStudyAreaName) + '\' +
             ChopCharacters(ASubArea)   + '\' +
             ChopCharacters(AScenarioLabel)  + '\';
    if not DirectoryExists(LPath) then
       ForceDirectories(LPath);
    LFileName := LPath +  AGroupName + '-' + ADrawingName + '.VSD';
    Result := LFileName;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TStudyDatabaseAgent.UpdateStudy(AModel, AStudyAreaName,
  AConsultant, AClient, AStudyLabel, AStudyAreaDescr: string;
  AStudyDate: TDateTime; AStudyNumber, AShapeFile: string): boolean;
const OPNAME = 'TStudyDatabaseAgent.UpdateStudy';
var
  LDataSet : TAbstractModelDataset;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.SetSQL(UpdateStudySQL);
      LDataSet.SetParams(
        ['AStudyAreaName','AConsultant','AClient','AStudyLabel',
         'AStudyAreaDescr','AStudyDate','AStudyNumber', 'AShapeFileName'],
        [AStudyAreaName,AConsultant,AClient,AStudyLabel,
         Copy(AStudyAreaDescr,1,255),DateTimeToStr(AStudyDate),AStudyNumber, AShapeFile]);
        LDataSet.ExecSQL;
    finally
      LDataSet.Free;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyDatabaseAgent.UpdateSubArea(AModel, AStudyAreaName,
  ASubArea, ASubAreaLabel, ASubAreaDescr, AShapeFile: string): boolean;
const OPNAME = 'TStudyDatabaseAgent.UpdateSubArea';
var
  LDataSet : TAbstractModelDataset;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.SetSQL(UpdateSubAreaSQL);
      LDataSet.SetParams(
        ['AModel','AStudyAreaName','ASubArea','ASubAreaLabel','ASubAreaDescr', 'AShapeFileName'],
        [AModel, AStudyAreaName, ASubArea, ASubAreaLabel, Copy(ASubAreaDescr,1,255), AShapeFile]);

      LDataSet.ExecSQL;
      Result := true;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyDatabaseAgent.UpdateSubAreaCoords(AModel, AStudyAreaName, ASubArea: string;
         ATopLeft, ATopRight, ABottomLeft, ABottomRight: double): boolean;
const OPNAME = 'TStudyDatabaseAgent.UpdateSubAreaCoords';
var
  LDataSet : TAbstractModelDataset;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.SetSQL(UpdateSubAreaCoordsSQL);
      //LDataSet.ClearQueryParams();
      LDataSet.SetParams(
        ['AModel','AStudyAreaName','ASubArea','ATopLeftCoord','ATopRightCoord', 'ABottomLeftCoord', 'ABottomRightCoord'],
        [AModel, AStudyAreaName, ASubArea, FloatToStr(ATopLeft),FloatToStr(ATopRight),FloatToStr(ABottomLeft),FloatToStr(ABottomRight)]);
      LDataSet.ExecSQL;
      Result := true;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyDatabaseAgent.UpdateScenario(AModel, AStudyAreaName,ASubArea, AScenario, AScenarioLabel, AScenarioDescr,
         ADataFilesPrefix, ADataFilesPath: string; AFilesLoaded: boolean;AEditable : boolean;ACalenderStartMonth: integer;AVersion: string): boolean;
const OPNAME = 'TStudyDatabaseAgent.UpdateScenario';
var
  LDataSet : TAbstractModelDataset;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.SetSQL(UpdateScenarionSQL);
      LDataSet.SetParams(
        ['AModel','AStudyAreaName','ASubArea','AScenario','AScenarioLabel',
         'AScenarioDescr','ADataFilesPrefix','ADataFilesPath','AVersion'],
        [AModel, AStudyAreaName, ASubArea, AScenario, AScenarioLabel,
         Copy(AScenarioDescr,1,255), ADataFilesPrefix, ADataFilesPath,AVersion]);
      if AFilesLoaded then
        LDataSet.SetParams(['AFilesLoaded'], ['Y'])
      else
        LDataSet.SetParams(['AFilesLoaded'], ['N']);
      if AEditable then
        LDataSet.SetParams(['DataImported'], ['N'])
      else
        LDataSet.SetParams(['DataImported'], ['Y']);
      LDataSet.SetParams(['ACalenderStartMonth'], [IntToStr(ACalenderStartMonth)]);

      LDataSet.ExecSQL;
      Result := true;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyDatabaseAgent.ScenarioCount(AModelID, AStudyID, ASubareaID: string): integer;
const OPNAME = 'TStudyDatabaseAgent.ScenarioCount';
var
  LDataSet : TAbstractModelDataset;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.SetSQL('SELECT Count(*) AS ScenarioCount from StudyScenario '+
                      ' WHERE Model       = '+ QuotedStr(AModelID)+
                      ' AND StudyAreaName = '+ QuotedStr(AStudyID)+
                      ' AND SubArea       = '+ QuotedStr(ASubareaID));
      LDataSet.DataSet.Open;
      Result := LDataSet.DataSet.FieldByName('ScenarioCount').AsInteger;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyDatabaseAgent.SubAreaCount(AModelID, AStudyID: string): integer;
const OPNAME = 'TStudyDatabaseAgent.SubAreaCount';
var
  LDataSet : TAbstractModelDataset;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.SetSQL('SELECT Count(*) AS SubAreaCount from StudySubArea '+
                      ' WHERE Model = '+ QuotedStr(AModelID)+
                      ' AND StudyAreaName = '+ QuotedStr(AStudyID));
      LDataSet.DataSet.Open;
      Result := LDataSet.DataSet.FieldByName('SubAreaCount').AsInteger;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TStudyDatabaseAgent.CheckAndCreateRainFallGaugesProject(AModel,
  AStudyAreaName, ASubArea, AScenario, AScenarioLabel, AScenarioDescr,
  ADataFilesPrefix, ADataFilesPath: string; AFilesLoaded: boolean; AEditable : boolean;
  ACalenderStartMonth: integer; AVersion: string): boolean;
const
  OPNAME = 'TStudyDatabaseAgent.CheckAndCreateRainFallGaugesProject';
var
  LDataSet : TAbstractModelDataset;

begin
  Result := False;
  try

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.SetSQL(
        ' SELECT * FROM StudyScenario         ' +
        ' WHERE MODEL       = :AModel         ' +
        ' AND StudyAreaName = :AStudyAreaName ' +
        ' AND SubArea       = :ASubArea       ' +
        ' AND Scenario      = ' + QuotedStr(UAbstractObject.CProjectGauges));

      LDataSet.SetParams(
        ['AModel','AStudyAreaName','ASubArea'],
        [AModel, AStudyAreaName, ASubArea]);

      LDataSet.Dataset.Open;

      if LDataSet.DataSet.RecordCount = 0 then
      begin
        CreateScenario(AModel, AStudyAreaName, ASubArea, UAbstractObject.CProjectGauges, UAbstractObject.CProjectGauges,
           UAbstractObject.CProjectGauges, ADataFilesPrefix, ADataFilesPath, AFilesLoaded, AEditable, ACalenderStartMonth, AVersion);
      end;

      Result := true;
    finally
      LDataSet.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
