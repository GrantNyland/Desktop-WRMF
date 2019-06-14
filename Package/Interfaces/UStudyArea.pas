//
//  UNIT      : Contains the class TStudyArea.
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/02/18
//  COPYRIGHT : Copyright © 2002 DWAF
//
unit UStudyArea;

interface

uses
  Classes,
  Messages,
  UStudyObjects,
  UDataSetType,
  UAbstractObject,
  UStringListOfStringLists;

type
  TStudyArea = class(TAbstractStudyArea)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure Reset;
    procedure AssignFrom(AStudyArea: TAbstractStudyArea); override;
    procedure CopyFromScenarioDataObject(AScenarioDataObject: TScenarioDataObject);
    function GetDefaultStudyNameList(
      AAfterSep : string = '';
      ATableName : string = '';
      ALevel : integer = 4): string; override;
    function GetDefaultStudyValueList(
      AAfterSep : string = '';
      ALevel : integer = 4): string; override;
    function GetDefaultStudyNameValueList(
      AAfterSep : string = '';
      ATableName : string = '';
      ALevel : integer = 4): string; override;
    function GetSQLStudyCriteria(
      AAfterSep : string = '';
      ATableName : string = '';
      ALevel : integer = 4) : string; override;
    procedure SetDefaultParams(var ADataset : TAbstractModelDataset; ALevel : integer = 4); override;

    procedure SetNewStudy(AModelCode, AStudyAreaCode: string);
    procedure SetStudyImportDate(AValue: TDateTime); override;
    procedure SetLastUpdateDate(AValue: TDateTime); override;
    function GetStudyImportDate: TDateTime; override;
    function GetLastUpdateDate: TDateTime; override;
  end;

implementation

uses
  SysUtils,
  UErrorHandlingOperations, DB;

procedure TStudyArea.CreateMemberObjects;
const OPNAME = 'TStudyArea.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FSubAreaDocumentDetail := TStudyDocumentList.Create;
    FScenarioDocumentDetail := TStudyDocumentList.Create;

    FSubAreaScenario      := TStringListOfStringLists.Create;
    FSubAreaDescriptions  := TStringListOfStringLists.Create;
    FSubAreaLabels        := TStringListOfStringLists.Create;
    FScenarioDescriptions := TStringListOfStringLists.Create;
    FScenarioLabels       := TStringListOfStringLists.Create;
    Reset;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyArea.DestroyMemberObjects;
const OPNAME = 'TStudyArea.DestroyMemberObjects';
begin
  try
    FreeAndNil(FSubAreaDocumentDetail);
    FreeAndNil(FScenarioDocumentDetail);
    FreeAndNil(FSubAreaScenario);
    FreeAndNil(FSubAreaDescriptions);
    FreeAndNil(FSubAreaLabels);
    FreeAndNil(FScenarioLabels);
    FreeAndNil(FScenarioDescriptions);

    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyArea.Reset;
const OPNAME = 'TStudyArea.Reset';
begin
  try
    FStudyAreaCode        := '';
    FStudyLabel           := '';
    FStudyAreaDescription := '';
    FStudyDate            := 0.0;
    FConsultant           := '';
    FClient               := '';
    FStudyNumber          := '';

    FModelCode            := '';
    FModelLabel           := '';
    FModelSubCode         := '';
    FModelVersion         := '';

    FSubArea              := '';
    FSubAreaLabel         := '';
    FSubAreaDescription   := '';
    FTopLeftCoord         := 0.0;
    FTopRightCoord        := 0.0;
    FBottomLeftCoord      := 0.0;
    FBottomRightCoord     := 0.0;

    FScenario             := '';
    FScenarioLabel        := '';
    FScenarioDescription  := '';

    FScenarioLocked       := True;
    FFilesLoaded          := False;
    FCalendarStartMonth   := 0;
    FDataFilesPrefix      := '';
    FDataFilesPath        := '';
    FDataImported         := False;
    
    FEditable             := False;
    FSelected             := False;
    FSubAreaIndex         := -1;
    FScenarioIndex        := -1;

    FSubAreaDocumentDetail.Clear;
    FScenarioDocumentDetail.Clear;
    FSubAreaScenario.Clear;
    FSubAreaDescriptions.Clear;
    FSubAreaLabels.Clear;
    FScenarioLabels.Clear;
    FScenarioDescriptions.Clear;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyArea.AssignFrom(AStudyArea: TAbstractStudyArea);
const OPNAME = 'TStudyArea.AssignFrom';
begin
  try
    FStudyAreaCode        := AStudyArea.StudyAreaCode;
    FStudyLabel           := AStudyArea.StudyLabel;
    FStudyAreaDescription := AStudyArea.StudyAreaDescription;
    FStudyDate            := AStudyArea.StudyDate;
    FConsultant           := AStudyArea.Consultant;
    FClient               := AStudyArea.Client;
    FStudyNumber          := AStudyArea.StudyNumber;

    FModelCode            := AStudyArea.ModelCode;
    FModelLabel           := AStudyArea.ModelLabel;
    FModelSubCode         := AStudyArea.ModelSubCode;
    FModelVersion         := AStudyArea.ModelVersion;

    FSubArea              := AStudyArea.SubAreaCode;
    FSubAreaLabel         := AStudyArea.SubAreaLabel;
    FSubAreaDescription   := AStudyArea.SubAreaDescription;
    FTopLeftCoord         := AStudyArea.TopLeftCoord;
    FTopRightCoord        := AStudyArea.TopRightCoord;
    FBottomLeftCoord      := AStudyArea.BottomLeftCoord;
    FBottomRightCoord     := AStudyArea.BottomRightCoord;

    FScenario             := AStudyArea.ScenarioCode;
    FScenarioLabel        := AStudyArea.ScenarioLabel;
    FScenarioDescription  := AStudyArea.ScenarioDescription;

    FDataFilesPrefix      := AStudyArea.DataFilesPrefix;
    FDataFilesPath        := AStudyArea.DataFilesPath;
    FFilesLoaded          := AStudyArea.FilesLoaded;
    FCalendarStartMonth   := AStudyArea.CalendarStartMonth;

    FStudyImportDate      := AStudyArea.StudyImportDate;
    FLastUpdateDate       := AStudyArea.LastUpdateDate;
    FDataImported         := AStudyArea.DataImported;

    FEditable             := AStudyArea.Editable;
    FSelected             := AStudyArea.Selected;
    FSubAreaIndex         := TStudyArea(AStudyArea).SubAreaIndex;
    FScenarioIndex        := TStudyArea(AStudyArea).ScenarioIndex;

    TStudyDocumentList(FSubAreaDocumentDetail).AssignFrom(TStudyDocumentList(TStudyArea(AStudyArea).SubAreaDocumentDetail));
    TStudyDocumentList(FScenarioDocumentDetail).AssignFrom(TStudyDocumentList(TStudyArea(AStudyArea).ScenarioDocumentDetail));
    FSubAreaScenario.AssignFrom(AStudyArea.SubAreaScenario);
    FSubAreaLabels.AssignFrom(TStudyArea(AStudyArea).SubAreaLabels);
    FSubAreaDescriptions.AssignFrom(TStudyArea(AStudyArea).SubAreaDescriptions);
    FScenarioLabels.AssignFrom(TStudyArea(AStudyArea).ScenarioLabels);
    FScenarioDescriptions.AssignFrom(TStudyArea(AStudyArea).ScenarioDescriptions);

  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TStudyArea.SetNewStudy(AModelCode, AStudyAreaCode: string);
const OPNAME = 'TStudyArea.SetNewStudy';
begin
  try
    FSelected := True;
    FModelCode := AModelCode;
    FModelSubCode := AModelCode;
    FStudyAreaCode := AStudyAreaCode;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyArea.CopyFromScenarioDataObject(AScenarioDataObject: TScenarioDataObject);
const OPNAME = 'TStudyArea.CopyFromScenarioDataObject';
var
  LSubAreaIndex, LScenarioIndex: integer;
  LSubArea,
  LScenario: string;
begin
  try
    if Assigned(AScenarioDataObject) then
    begin

      // Reset the object.
      Reset;

      FSelected             := True;
      FEditable             := AScenarioDataObject.Editable;
      // Set the study data.
      FStudyAreaCode        := AScenarioDataObject.SubArea.Model.Study.Study;
      FStudyLabel           := AScenarioDataObject.SubArea.Model.Study.StudyLabel;
      FStudyAreaDescription := AScenarioDataObject.SubArea.Model.Study.StudyDescr;
      FStudyDate            := AScenarioDataObject.SubArea.Model.Study.StudyDate;
      FConsultant           := AScenarioDataObject.SubArea.Model.Study.Consultant;
      FClient               := AScenarioDataObject.SubArea.Model.Study.Client;
      FStudyNumber          := AScenarioDataObject.SubArea.Model.Study.StudyNumber;

      // Set the model data.
      FModelCode            := AScenarioDataObject.SubArea.Model.Model;
      FModelSubCode         := AScenarioDataObject.SubArea.Model.SubModel;
      FModelLabel           := AScenarioDataObject.SubArea.Model.ModelLabel;
      FModelVersion         := AScenarioDataObject.Version;

      // Set the sub area data.
      FSubArea              := AScenarioDataObject.SubArea.SubArea;
      FSubAreaLabel         := AScenarioDataObject.SubArea.SubAreaLabel;
      FSubAreaDescription   := AScenarioDataObject.SubArea.SubAreaDescr;
      FTopLeftCoord         := AScenarioDataObject.SubArea.TopLeftCoord;
      FTopRightCoord        := AScenarioDataObject.SubArea.TopRightCoord;
      FBottomLeftCoord      := AScenarioDataObject.SubArea.BottomLeftCoord;
      FBottomRightCoord     := AScenarioDataObject.SubArea.BottomRightCoord;


      // Set the scenario data.
      FScenario             := AScenarioDataObject.Scenario;
      FScenarioLabel        := AScenarioDataObject.ScenarioLabel;
      FScenarioDescription  := AScenarioDataObject.ScenarioDescr;

      FDataFilesPrefix      := AScenarioDataObject.DataFilesPrefix;
      FDataFilesPath        := AScenarioDataObject.DataFilesPath;
      FFilesLoaded          := AScenarioDataObject.FilesLoaded;
      FCalendarStartMonth   := AScenarioDataObject.CalenderStartMonth;
      FDataImported         := AScenarioDataObject.DataImported;

      // Set the item indexes.
      FSubAreaIndex         := AScenarioDataObject.SubArea.ItemIndex;
      FScenarioIndex        := AScenarioDataObject.ItemIndex;

      // Set the document details.
      TStudyDocumentList(FSubAreaDocumentDetail).AssignFrom(TStudyDocumentList(AScenarioDataObject.SubArea.DocumentDetail));
      TStudyDocumentList(FScenarioDocumentDetail).AssignFrom(TStudyDocumentList(AScenarioDataObject.DocumentDetail));

      // Load the other sub areas and scenarios
      for LSubAreaIndex := 0 to AScenarioDataObject.SubArea.Model.SubAreaCount - 1 do
      begin
        for LScenarioIndex := 0 to AScenarioDataObject.SubArea.Model.SubArea[LSubAreaIndex].ScenarioCount - 1 do
        begin
          LSubArea := AScenarioDataObject.SubArea.Model.SubArea[LSubAreaIndex].SubArea;
          LScenario:= AScenarioDataObject.SubArea.Model.SubArea[LSubAreaIndex].Scenario[LScenarioIndex].Scenario;

          FSubAreaScenario.Add(LSubArea,LScenario);

          FSubAreaDescriptions.Add(LSubArea,
            AScenarioDataObject.SubArea.Model.SubArea[LSubAreaIndex].SubAreaDescr);
          FSubAreaLabels.Add(LSubArea,
            AScenarioDataObject.SubArea.Model.SubArea[LSubAreaIndex].SubAreaLabel);

          FScenarioDescriptions.Add(LSubArea+LScenario,
            AScenarioDataObject.SubArea.Model.SubArea[LSubAreaIndex].Scenario[LScenarioIndex].ScenarioDescr);
          FScenarioLabels.Add(LSubArea+LScenario,
            AScenarioDataObject.SubArea.Model.SubArea[LSubAreaIndex].Scenario[LScenarioIndex].ScenarioLabel);
        end;
      end;
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyArea.GetDefaultStudyNameList(
  AAfterSep : string = '';
  ATableName : string = '';
  ALevel : integer = 4): string;
const OPNAME = 'TStudyArea.GetDefaultStudyNameList';
begin
  Result := '';
  try
    if ATableName <> '' then
    begin
      if (ALevel > 0) then
        Result := Result  + Format('%s.Model',[ATableName]);
      if (ALevel > 1) then
        Result := Result  + Format(',%s.StudyAreaName',[ATableName]);
      if (ALevel > 2) then
        Result := Result  + Format(',%s.SubArea',[ATableName]);
      if (ALevel > 3) then
        Result := Result  + Format(',%s.Scenario',[ATableName]);
      Result := Result + AAfterSep;
    end
    else
    begin
      if (ALevel > 0) then
        Result := Result  + 'Model';
      if (ALevel > 1) then
        Result := Result  + ',StudyAreaName';
      if (ALevel > 2) then
        Result := Result  + ',SubArea';
      if (ALevel > 3) then
        Result := Result  + ',Scenario';
      Result := Result + AAfterSep;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TStudyArea.GetDefaultStudyValueList(
  AAfterSep : string = '';
  ALevel : integer = 4): string;
const OPNAME = 'TStudyArea.GetDefaultStudyValueList';
begin
  Result := '';
  try
    if (ALevel > 0) then
      Result := Result + Format(      #39 + '%s' + #39, [ModelCode]); // #39 = ' (Single Quote)
    if (ALevel > 1) then
      Result := Result + Format(',' + #39 + '%s' + #39, [StudyAreaCode]);
    if (ALevel > 2) then
      Result := Result + Format(',' + #39 + '%s' + #39, [SubAreaCode]);
    if (ALevel > 3) then
      Result := Result + Format(',' + #39 + '%s' + #39, [ScenarioCode]);
    Result := Result + AAfterSep;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TStudyArea.GetDefaultStudyNameValueList(
  AAfterSep : string = '';
  ATableName : string = '';
  ALevel : integer = 4): string;
const OPNAME = 'TStudyArea.GetDefaultStudyNameValueList';
begin
  Result := '';
  try
    if ATableName <> '' then
    begin
      if (ALevel > 0) then
        Result := Result + Format('%s.Model='          + #39 + '%s' + #39,[ATableName,ModelCode]);
      if (ALevel > 1) then
        Result := Result + Format(',%s.StudyAreaName=' + #39 + '%s' + #39,[ATableName,StudyAreaCode]);  // #39 = ' (SingleQuote)
      if (ALevel > 2) then
        Result := Result + Format(',%s.SubArea='       + #39 + '%s' + #39,[ATableName,SubAreaCode]);
      if (ALevel > 3) then
        Result := Result + Format(',%s.Scenario='      + #39 + '%s' + #39,[ATableName,ScenarioCode]);
      Result := Result + AAfterSep;
    end
    else
    begin
      if (ALevel > 0) then
        Result := Result + Format('Model='          + #39 + '%s' + #39,[ModelCode]);
      if (ALevel > 1) then
        Result := Result + Format(',StudyAreaName=' + #39 + '%s' + #39,[StudyAreaCode]);  // #39 = ' (SingleQuote)
      if (ALevel > 2) then
        Result := Result + Format(',SubArea='       + #39 + '%s' + #39,[SubAreaCode]);
      if (ALevel > 3) then
        Result := Result + Format(',Scenario='      + #39 + '%s' + #39,[ScenarioCode]);
      Result := Result + AAfterSep;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TStudyArea.GetSQLStudyCriteria(
  AAfterSep : string = '';
  ATableName : string = '';
  ALevel : integer = 4) : string;
const OPNAME = 'TStudyArea.GetSQLStudyCriteria';
begin
  Result := '';
  try
    if ATableName <> '' then
    begin
      if (ALevel > 0) then
        Result := Result + Format(' ((%s.Model) = '             + #39'%s'#39 + ')',[ATableName,ModelCode]);
      if (ALevel > 1) then
        Result := Result + Format(' AND ((%s.StudyAreaName) = ' + #39'%s'#39 + ')',[ATableName,StudyAreaCode]);
      if (ALevel > 2) then
        Result := Result + Format(' AND ((%s.SubArea) = '       + #39'%s'#39 + ')',[ATableName,SubAreaCode]);
      if (ALevel > 3) then
        Result := Result + Format(' AND ((%s.Scenario) = '      + #39'%s'#39 + ')',[ATableName,ScenarioCode]);
      Result := Result + AAfterSep;
    end
    else
    begin
      if (ALevel > 0) then
        Result := Result + Format(' (Model = '             + #39'%s'#39 + ')',[ModelCode]);
      if (ALevel > 1) then
        Result := Result + Format(' AND (StudyAreaName = ' + #39'%s'#39 + ')',[StudyAreaCode]);
      if (ALevel > 2) then
        Result := Result + Format(' AND (SubArea = '       + #39'%s'#39 + ')',[SubAreaCode]);
      if (ALevel > 3) then
        Result := Result + Format(' AND (Scenario = '      + #39'%s'#39 + ')',[ScenarioCode]);
      Result := Result + AAfterSep;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TStudyArea.SetDefaultParams(
  var ADataset: TAbstractModelDataset;
  ALevel : integer = 4);
const OPNAME = 'TStudyArea.SetDefaultParams';
begin
  try
    // if required implement level here -- VGN
    ADataset.SetParams(
      ['AModelCode',
       'AStudyAreaCode',
       'ASubAreaCode',
       'AScenarioCode'],
      [ModelCode,
       StudyAreaCode,
       SubAreaCode,
       ScenarioCode]);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TStudyArea.SetStudyImportDate(AValue: TDateTime);
const OPNAME = 'TStudyArea.SetStudyImportDate';
var
  LDataSet : TAbstractModelDataset;
begin
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.SetSQL('UPDATE StudyScenario SET StudyImportDate = '+ QuotedStr(DateTimeToStr(AValue))+
                      ' WHERE Model       =  '+QuotedStr(FModelCode)+
                      ' AND StudyAreaName =  '+QuotedStr(FStudyAreaCode)+
                      ' AND SubArea       =  '+QuotedStr(FSubArea)+
                      ' AND Scenario      =  '+QuotedStr(FScenario));
      LDataSet.ExecSQL;
      FStudyImportDate := AValue;
    finally
      LDataSet.Free;
    end;
 except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TStudyArea.SetLastUpdateDate(AValue: TDateTime);
const OPNAME = 'TStudyArea.SetLastUpdateDate';
var
  LDataSet : TAbstractModelDataset;
begin
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.SetSQL('UPDATE StudyScenario SET LastUpDateDate = '+ QuotedStr(DateTimeToStr(AValue))+
                      ' WHERE Model       =  '+QuotedStr(FModelCode)+
                      ' AND StudyAreaName =  '+QuotedStr(FStudyAreaCode)+
                      ' AND SubArea       =  '+QuotedStr(FSubArea)+
                      ' AND Scenario      =  '+QuotedStr(FScenario));
      LDataSet.ExecSQL;
      FLastUpdateDate := AValue;
    finally
      LDataSet.Free;
    end;
 except on E : Exception do HandleError(E,OPNAME); end;
end;

function TStudyArea.GetLastUpdateDate: TDateTime;
const OPNAME = 'TStudyArea.GetLastUpdateDate';
var
  LDataSet : TAbstractModelDataset;
  LSQL : string;
begin
  Result := 0.0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LSQL :=
          'SELECT * FROM StudyScenario WHERE '+
          ' (Model         = ' + QuotedStr(FModelCode)     + ') AND ' +
          ' (StudyAreaName = ' + QuotedStr(FStudyAreaCode) + ') AND ' +
          ' (SubArea       = ' + QuotedStr(FSubArea)       + ') AND ' +
          ' (Scenario      = ' + QuotedStr(FScenario)      + ')';
        LDataSet.SetReadOnly(False);
        LDataSet.SetSQL(LSQL);
        LDataset.DataSet.Open;
        Result := LDataSet.DataSet.FieldByName('LastUpdateDate').AsDateTime;
      end;
    finally
      LDataSet.Free;
    end;
 except on E : Exception do HandleError(E,OPNAME); end;
end;

function TStudyArea.GetStudyImportDate: TDateTime;
const OPNAME = 'TStudyArea.GetStudyImportDate';
var
  LDataSet : TAbstractModelDataset;
  LSQL : string;
begin
  Result := 0.0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LSQL :=
          'SELECT * FROM StudyScenario WHERE '+
          ' (Model         = ' + QuotedStr(FModelCode)     + ') AND ' +
          ' (StudyAreaName = ' + QuotedStr(FStudyAreaCode) + ') AND ' +
          ' (SubArea       = ' + QuotedStr(FSubArea)       + ') AND ' +
          ' (Scenario      = ' + QuotedStr(FScenario)      + ')';
        LDataSet.SetSQL(LSQL);
        LDataset.DataSet.Open;
        Result := LDataSet.DataSet.FieldByName('StudyImportDate').AsDateTime;
      end;
    finally
      LDataSet.Free;
    end;
 except on E : Exception do HandleError(E,OPNAME); end;
end;

end.

