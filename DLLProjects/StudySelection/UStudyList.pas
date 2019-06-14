//
//
//  UNIT      : Contains the class TStudyList.
//  AUTHOR    : Grant Nyland
//  DATE      : 2002/04/16
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
                                               
unit UStudyList;

interface

uses
  DB,
  Contnrs,
  Classes,
  UStudyArea,
  UStudyObjects,
  UAbstractObject;

type
  TStudyList = class(TAbstractAppObject)
  protected
    FAllOwnedObjects: TObjectList;
    FStudyList: TList;
    FModelList: TList;
    FSubAreaList: TList;
    FScenarioList: TList;
    FStudyArea: TStudyArea;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure AddSubStudies;
    procedure AddPreProcessors;
    procedure AddIFRPreProcessor;
    procedure AddDailyDiversionPreProcessor;
    procedure AddDamSedimentation;
    procedure AddStudies(AStudyDataset, AStudyDocuments: TAbstractModelDataset);
    procedure AddStudyModels(AStudy: TStudyDataObject; AStudyDataset, AStudyDocuments: TAbstractModelDataset);
    procedure AddModelSubAreas(AModel: TModelDataObject; AStudyDataset, AStudyDocuments: TAbstractModelDataset);
    procedure AddSubAreaScenarios(ASubArea: TSubAreaDataObject; AStudyDataset, AStudyDocuments: TAbstractModelDataset);
    procedure PopulateDocuments(ADocumentDetail: TStudyDocumentList; AStudyDocuments: TAbstractModelDataset);
    function GetStudy(AStudyIndex: integer): TStudyDataObject;
  public
    procedure Clear;
    procedure Populate(AStudyDataset, AStudyDocuments: TAbstractModelDataset);
    procedure LoadAvailableModelsInto(AList: TStrings);
    function FindFirstSubAreaAfter(AModelName, ASubAreaName: string; ADateLaterThan: TDateTime): TSubAreaDataObject;
    function FindFirstObjectByLabel(AStudyLabel, AModelLabel, ASubAreaLabel, AScenarioLabel: string): TStudySelectionNodeData;
    function StudyCount: integer;
    property Study[AStudyIndex: integer]: TStudyDataObject read GetStudy; default;
    property StudyArea: TStudyArea read FStudyArea;
  end;

implementation

uses
  SysUtils,
  UErrorHandlingOperations;

procedure TStudyList.CreateMemberObjects;
const
  OPNAME = 'TStudyList.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FAllOwnedObjects := TObjectList.Create;
    FStudyList       := TList.Create;
    FModelList       := TList.Create;
    FSubAreaList     := TList.Create;
    FScenarioList    := TList.Create;
    FStudyArea       := TStudyArea.Create(FAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyList.DestroyMemberObjects;
const
  OPNAME = 'TStudyList.DestroyMemberObjects';
begin
  try
    Clear;
    FreeAndNil(FStudyArea);
    FreeAndNil(FScenarioList);
    FreeAndNil(FSubAreaList);
    FreeAndNil(FModelList);
    FreeAndNil(FStudyList);
    FreeAndNil(FAllOwnedObjects);
    inherited DestroyMemberObjects;

  // Handle Exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyList.Clear;
const OPNAME = 'TStudyList.Clear';
begin
  try
    FScenarioList.Clear;
    FSubAreaList.Clear;
    FModelList.Clear;
    FStudyList.Clear;
    FAllOwnedObjects.Clear;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStudyList.StudyCount: integer;
const OPNAME = 'TStudyList.StudyCount';
begin
  Result := 0;
  try
    Result := FStudyList.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStudyList.GetStudy(AStudyIndex: integer): TStudyDataObject;
const OPNAME = 'TStudyList.GetStudy';
begin
  Result := nil;
  try
    Result := TStudyDataObject(FStudyList[AStudyIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TStudyList.LoadAvailableModelsInto(AList: TStrings);
const OPNAME = 'TStudyList.LoadAvailableModelsInto';
var LIndex: integer;
    LNamesList: TStringList;
begin
  try
    AList.Clear;
    LNamesList := TStringList.Create;
    try
      LNamesList.Sorted := True;
      LNamesList.Duplicates := dupIgnore;
      for LIndex := 0 to FModelList.Count - 1 do
        LNamesList.Add(TModelDataObject(FModelList[LIndex]).ModelLabel);
      AList.Assign(LNamesList);
    finally
      LNamesList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStudyList.FindFirstSubAreaAfter(AModelName, ASubAreaName: string; ADateLaterThan: TDateTime): TSubAreaDataObject;
const OPNAME = 'TStudyList.FindFirstSubAreaAfter';
var LIndex: integer;
begin
  Result := nil;
  try
    for LIndex := 0 to FSubAreaList.Count - 1 do
    begin
      if (UpperCase(TSubAreaDataObject(FSubAreaList[LIndex]).Model.ModelLabel) = UpperCase(AModelName)) then
      begin
        if (TSubAreaDataObject(FSubAreaList[LIndex]).Model.Study.StudyDate >= ADateLaterThan) then
        begin
          if (UpperCase(TSubAreaDataObject(FSubAreaList[LIndex]).SubArea) = UpperCase(ASubAreaName)) or
             (ASubAreaName = '') then
          begin
            Result := TSubAreaDataObject(FSubAreaList[LIndex]);
            break;
          end;
        end;
      end;
    end;

  // Handle all exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStudyList.FindFirstObjectByLabel(AStudyLabel, AModelLabel, ASubAreaLabel, AScenarioLabel: string): TStudySelectionNodeData;
const OPNAME = 'TStudyList.FindFirstObjectByLabel';
var
  LSubAreaIndex, LScenarioIndex: integer;
  LSubArea: TSubAreaDataObject;
begin
  Result := nil;
  try
    for LSubAreaIndex := 0 to FSubAreaList.Count - 1 do
    begin
      LSubArea := TSubAreaDataObject(FSubAreaList[LSubAreaIndex]);
      if (AStudyLabel   = LSubArea.Model.Study.StudyLabel) and
         (AModelLabel   = LSubArea.Model.ModelLabel) and
         (ASubAreaLabel = LSubArea.SubAreaLabel) then
      begin
        if (AScenarioLabel = '') then
        begin
          Result := LSubArea.Scenario[0];
        end else begin
          for LScenarioIndex := 0 to LSubArea.ScenarioCount - 1 do
          begin
            if (AScenarioLabel = LSubArea.Scenario[LScenarioIndex].ScenarioLabel) then
            begin
              Result := LSubArea.Scenario[LScenarioIndex];
              break;
            end;
          end;
        end;
        break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TStudyList.Populate(AStudyDataset, AStudyDocuments: TAbstractModelDataset);
const OPNAME = 'TStudyList.Populate';
begin
  try
    AddStudies(AStudyDataset, AStudyDocuments);
    //AddPreProcessors;
    //AddSubStudies;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TStudyList.AddStudies(AStudyDataset, AStudyDocuments: TAbstractModelDataset);
const OPNAME = 'TStudyList.AddStudies';
var
  LStudy: TStudyDataObject;
  LStudyIndex: integer;
  //LUserStudies : TStringList;
  //LModel: string;
  //LIndex: integer;
begin
  try
    // Clear all the existing data.
    Clear;
    {LUserStudies := TStringList.Create;
    try
      for lIndex := 0 to FAppModules.User.UserModelCount -1 do
      begin
        LModel := UpperCase(FAppModules.User.UserModelByIndex(lIndex));
        LUserStudies.Add(LModel);
      end;}

      // Loop while there is data.
      LStudyIndex := 0;
      AStudyDataset.DataSet.Open;
      while (not AStudyDataset.DataSet.EOF) do
      begin
        {LModel := UpperCase(Trim(AStudyDataset.DataSet.FieldByName('Model').AsString)));
        if(LUserStudies.IndexOf(LModel) < 0) then
        begin
          AStudyDataset.DataSet.Next;
          Continue;
        end;}

        // Create the scenario object.
        LStudy := TStudyDataObject.Create;
        LStudy.ItemIndex := LStudyIndex;
        FAllOwnedObjects.Add(LStudy);
        FStudyList.Add(LStudy);

        // Assign the record data.
        LStudy.Study       := Trim(AStudyDataset.DataSet.FieldByName('StudyAreaName').AsString);
        LStudy.StudyLabel  := Trim(AStudyDataset.DataSet.FieldByName('StudyLabel').AsString);
        LStudy.StudyDescr  := Trim(AStudyDataset.DataSet.FieldByName('StudyAreaDescr').AsString);
        LStudy.StudyDate   := AStudyDataset.DataSet.FieldByName('StudyDate').AsDateTime;
        LStudy.StudyNumber := Trim(AStudyDataset.DataSet.FieldByName('StudyNumber').AsString);
        LStudy.Consultant  := Trim(AStudyDataset.DataSet.FieldByName('Consultant').AsString);
        LStudy.Client      := Trim(AStudyDataset.DataSet.FieldByName('Client').AsString);
        LStudy.StudyShapeFileName := Trim(AStudyDataset.DataSet.FieldByName('ShapeFileName').AsString);

        // Add the document references.
        AStudyDocuments.DataSet.Close;
        AStudyDocuments.ResetDefaultSQL;
        AStudyDocuments.AppendSQL(
          ' AND A.StudyAreaName = '#39 + LStudy.Study + #39 +
          ' AND A.Model    Is Null ' +
          ' AND A.SubArea  Is Null ' +
          ' AND A.Scenario Is Null ');
        PopulateDocuments(LStudy.DocumentDetail, AStudyDocuments);

        // Add the models for this study.
        AddStudyModels(LStudy, AStudyDataset, AStudyDocuments);
        Inc(LStudyIndex);
      end;
   {finally
     LUserStudies.Free;
   end;}
  // Handle all exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TStudyList.AddStudyModels(AStudy: TStudyDataObject; AStudyDataset, AStudyDocuments: TAbstractModelDataset);
const OPNAME = 'TStudyList.AddStudyModels';
var LModel: TModelDataObject;
  LModelIndex: integer;
begin
  try

    // Loop while there is data and the parent nodes still apply
    LModelIndex := 0;
    while (not AStudyDataset.DataSet.EOF) and
          (AStudy.Study = Trim(AStudyDataset.DataSet.FieldByName('StudyAreaName').AsString)) do
    begin

      // Create the model object.
      LModel := TModelDataObject.Create;
      LModel.Study := AStudy;
      LModel.ItemIndex := LModelIndex;
      AStudy.AddModel(LModel);
      FAllOwnedObjects.Add(LModel);
      FModelList.Add(LModel);

      // Assign the record data.
      LModel.Model      := Trim(AStudyDataset.DataSet.FieldByName('Model').AsString);
      LModel.SubModel   := Trim(AStudyDataset.DataSet.FieldByName('Model').AsString);
      LModel.ModelLabel := Trim(AStudyDataset.DataSet.FieldByName('ModelLabel').AsString);
      LModel.ModelDescr := Trim(AStudyDataset.DataSet.FieldByName('ModelDescr').AsString);

      // Add the document references.
      AStudyDocuments.DataSet.Close;
      AStudyDocuments.ResetDefaultSQL;
      AStudyDocuments.AppendSQL(
        ' AND A.StudyAreaName = '#39 + AStudy.Study + #39 +
        ' AND A.Model         = '#39 + LModel.Model + #39 +
        ' AND A.SubArea  Is Null ' +
        ' AND A.Scenario Is Null ');
      PopulateDocuments(LModel.DocumentDetail, AStudyDocuments);

      // Add the sub areas for this model.
      AddModelSubAreas(LModel, AStudyDataset, AStudyDocuments);
      Inc(LModelIndex);
    end;

  // Handle all exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TStudyList.AddModelSubAreas(AModel: TModelDataObject; AStudyDataset, AStudyDocuments: TAbstractModelDataset);
const OPNAME = 'TStudyList.AddModelSubAreas';
var
  LSubArea: TSubAreaDataObject;
  LSubAreaIndex: integer;
begin
  try

    // Loop while there is data and the parent nodes still apply
    LSubAreaIndex := 0;
    while (not AStudyDataset.DataSet.EOF) and
          (AModel.Study.Study = Trim(AStudyDataset.DataSet.FieldByName('StudyAreaName').AsString)) and
          (AModel.Model = Trim(AStudyDataset.DataSet.FieldByName('Model').AsString)) do
    begin

      // Create the sub area object.
      LSubArea := TSubAreaDataObject.Create;
      LSubArea.Model := AModel;
      // if ( UpperCase ( LSubArea.Model.Model ) = 'RAINFALL' ) then
      //   LSubArea.Editable := False;
      {end if..then }
      LSubArea.ItemIndex := LSubAreaIndex;
      AModel.AddSubArea(LSubArea);
      FAllOwnedObjects.Add(LSubArea);
      FSubAreaList.Add(LSubArea);

      // Assign the record data.
      LSubArea.SubArea      := Trim(AStudyDataset.DataSet.FieldByName('SubArea').AsString);
      LSubArea.SubAreaLabel := Trim(AStudyDataset.DataSet.FieldByName('SubAreaLabel').AsString);
      LSubArea.SubAreaDescr := Trim(AStudyDataset.DataSet.FieldByName('SubAreaDescr').AsString);
      LSubArea.SubAreaShapeFileName := Trim(AStudyDataset.DataSet.FieldByName ( 'SubAreaShapeFileName' ).AsString);
      LSubArea.TopLeftCoord := AStudyDataset.DataSet.FieldByName ( 'TopLeftCoord' ).AsFloat;
      LSubArea.TopRightCoord := AStudyDataset.DataSet.FieldByName ( 'TopRightCoord' ).AsFloat;
      LSubArea.BottomLeftCoord := AStudyDataset.DataSet.FieldByName ( 'BottomLeftCoord' ).AsFloat;
      LSubArea.BottomRightCoord := AStudyDataset.DataSet.FieldByName ( 'BottomRightCoord' ).AsFloat;

      // Add the document references.
      AStudyDocuments.DataSet.Close;
      AStudyDocuments.ResetDefaultSQL;
      AStudyDocuments.AppendSQL(
        ' AND A.StudyAreaName = '#39 + AModel.Study.Study + #39 +
        ' AND A.Model         = '#39 + AModel.Model       + #39 +
        ' AND A.SubArea       = '#39 + LSubArea.SubArea   + #39 +
        ' AND A.Scenario Is Null ');
      PopulateDocuments(LSubArea.DocumentDetail, AStudyDocuments);

      // Add the scenarios for this sub areas.
      AddSubAreaScenarios(LSubArea, AStudyDataset, AStudyDocuments);
      Inc(LSubAreaIndex);
    end;

  // Handle all exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TStudyList.AddSubAreaScenarios(ASubArea: TSubAreaDataObject; AStudyDataset, AStudyDocuments: TAbstractModelDataset);
const OPNAME = 'TStudyList.AddSubAreaScenarios';
var
  LScenario: TScenarioDataObject;
  LScenarioIndex: integer;
begin
  try

    // Loop while there is data and the parent nodes still apply
    LScenarioIndex := 0;
    while (not AStudyDataset.DataSet.EOF) and
          (ASubArea.Model.Study.Study = Trim(AStudyDataset.DataSet.FieldByName('StudyAreaName').AsString)) and
          (ASubArea.Model.Model = Trim(AStudyDataset.DataSet.FieldByName('Model').AsString)) and
          (ASubArea.SubArea = Trim(AStudyDataset.DataSet.FieldByName('SubArea').AsString)) do
    begin
      if (ASubArea.Model.Model = CRainfall) and
        (UpperCase(Trim(AStudyDataset.DataSet.FieldByName('Scenario').AsString)) <> (UpperCase(Trim(UAbstractObject.CProjectGauges)))) then
      begin
        AStudyDataset.DataSet.Next;
        Continue;
      end;
      // Create the scenario object.
      LScenario := TScenarioDataObject.Create;
      LScenario.SubArea := ASubArea;
      LScenario.ItemIndex := LScenarioIndex;
      ASubArea.AddScenario(LScenario);
      FAllOwnedObjects.Add(LScenario);
      FScenarioList.Add(LScenario);

      // Assign the record data.
      LScenario.Scenario        := Trim(AStudyDataset.DataSet.FieldByName('Scenario').AsString);
      // LScenario.Editable := LScenario.Scenario <> UAbstractObject.CProjectGauges;
      LScenario.ScenarioLabel   := Trim(AStudyDataset.DataSet.FieldByName('ScenarioLabel').AsString);
      LScenario.ScenarioDescr   := Trim(AStudyDataset.DataSet.FieldByName('ScenarioDescr').AsString);
      LScenario.DataFilesPrefix := Trim(AStudyDataset.DataSet.FieldByName('DataFilesPrefix').AsString);
      LScenario.DataFilesPath   := Trim(AStudyDataset.DataSet.FieldByName('DataFilesPath').AsString);
      LScenario.FilesLoaded     := UpperCase(Trim(AStudyDataset.DataSet.FieldByName('FilesLoaded').AsString)) ='Y';
      LScenario.CalenderStartMonth     := AStudyDataset.DataSet.FieldByName('CalenderStartMonth').AsInteger;
      LScenario.Version                := Trim(AStudyDataset.DataSet.FieldByName('Version').AsString);
      LScenario.DataImported           := UpperCase(Trim(AStudyDataset.DataSet.FieldByName('DataImported').AsString)) = 'Y';


      // Add the document references.
      AStudyDocuments.DataSet.Close;
      AStudyDocuments.ResetDefaultSQL;
      AStudyDocuments.AppendSQL(
        ' AND A.StudyAreaName = '#39 + ASubArea.Model.Study.Study + #39 +
        ' AND A.Model         = '#39 + ASubArea.Model.Model       + #39 +
        ' AND A.SubArea       = '#39 + ASubArea.SubArea           + #39 +
        ' AND A.Scenario      = '#39 + LScenario.Scenario         + #39);
      PopulateDocuments(LScenario.DocumentDetail, AStudyDocuments);

      // Goto the next record.
      AStudyDataset.DataSet.Next;
      Inc(LScenarioIndex);
    end;

  // Handle all exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TStudyList.PopulateDocuments(ADocumentDetail: TStudyDocumentList; AStudyDocuments: TAbstractModelDataset);
const OPNAME = 'TStudyList.PopulateDocuments';
begin
  try
    if Assigned(FAppModules.StudyDocumentManager()) then
    begin
      AStudyDocuments.DataSet.Open;
      while (not AStudyDocuments.DataSet.EOF) do
      begin
        ADocumentDetail.AddDocument(
          Trim(AStudyDocuments.DataSet.FieldByName('Category').AsString),
          Trim(AStudyDocuments.DataSet.FieldByName('Identifier').AsString),
          Trim(AStudyDocuments.DataSet.FieldByName('Filename').AsString),
          Trim(AStudyDocuments.DataSet.FieldByName('Bookmark').AsString),
          AStudyDocuments.DataSet.FieldByName('PageNumber').AsInteger);
        AStudyDocuments.DataSet.Next;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TStudyList.AddSubStudies;
const OPNAME = 'TStudyList.AddSubStudies';
var
  LModelPos,
  LStudyIndex,
  LModelIndex,
  LSubAreaIndex,
  LScenarioIndex : integer;
  LStudy: TStudyDataObject;
  LYRCModel: TModelDataObject;
  LPlanningModel: TModelDataObject;
  LModel: TModelDataObject;
  LSubArea: TSubAreaDataObject;
  LScenario: TScenarioDataObject;
  //LCreatePlanningModel: boolean;
begin
  try
    //LCreatePlanningModel := FAppModules.LicenceManager.ModelIsLicenced('PLANN');
    //Add YRC Sub studies
    for LStudyIndex := 0 to FStudyList.Count - 1 do
    begin
      LStudy  := TStudyDataObject(FStudyList[LStudyIndex]);
      for LModelIndex := 0 to LStudy.ModelList.Count - 1 do
      begin
        LModel := LStudy.Model[LModelIndex];
        if(UpperCase(LModel.Model) = 'YIELD') then
        begin
          //Copy YRC the model from old
          LYRCModel := TModelDataObject.Create;
          LYRCModel.AssignFrom(LModel);
          LYRCModel.SubModel   := CYRC;
          LYRCModel.ModelLabel := FAppModules.Language.GetString('StudySelection.YRCModel');
          LYRCModel.ModelDescr := FAppModules.Language.GetString('StudySelection.YRCPost');
          LYRCModel.Editable   := False;
          LStudy.AddModel(LYRCModel);
          LModelPos := FModelList.Add(LYRCModel);
          LYRCModel.ItemIndex := LModelPos;
          FAllOwnedObjects.Add(LYRCModel);
          FModelList.Add(LYRCModel);

          for LSubAreaIndex  := 0 to LYRCModel.SubAreaList.Count -1 do
          begin
            LSubArea := TSubAreaDataObject(LYRCModel.SubAreaList[LSubAreaIndex]);
            LSubArea.ItemIndex := LSubAreaIndex;
            LSubArea.Editable := False;
            FAllOwnedObjects.Add(LSubArea);
            FSubAreaList.Add(LSubArea);
            for LScenarioIndex := 0 to LSubArea.ScenarioList.Count -1 do
            begin
              LScenario := TScenarioDataObject(LSubArea.ScenarioList[LScenarioIndex]);
              LScenario.Editable := False;
              LScenario.ItemIndex := LScenarioIndex;
              FAllOwnedObjects.Add(LScenario);
              FScenarioList.Add(LScenario);
            end;
          end;

          //Copy Planning model from old
          //if LCreatePlanningModel then
          //begin
            LPlanningModel := TModelDataObject.Create;
            LPlanningModel.AssignFrom(LModel);
            LPlanningModel.SubModel   := CPlanning;
            LPlanningModel.ModelLabel := FAppModules.Language.GetString('StudySelection.PlanningModel');
            LPlanningModel.ModelDescr := FAppModules.Language.GetString('StudySelection.PlanningModel');
            LPlanningModel.Editable := False;
            LStudy.AddModel(LPlanningModel);
            LModelPos := FModelList.Add(LPlanningModel);
            LPlanningModel.ItemIndex := LModelPos;
            FAllOwnedObjects.Add(LPlanningModel);
            FModelList.Add(LPlanningModel);

            for LSubAreaIndex  := 0 to LPlanningModel.SubAreaList.Count -1 do
            begin
              LSubArea := TSubAreaDataObject(LPlanningModel.SubAreaList[LSubAreaIndex]);
              LSubArea.ItemIndex := LSubAreaIndex;
              LSubArea.Editable := False;
              FAllOwnedObjects.Add(LSubArea);
              FSubAreaList.Add(LSubArea);
              for LScenarioIndex := 0 to LSubArea.ScenarioList.Count -1 do
              begin
                LScenario := TScenarioDataObject(LSubArea.ScenarioList[LScenarioIndex]);
                LScenario.Editable := False;
                LScenario.ItemIndex := LScenarioIndex;
                FAllOwnedObjects.Add(LScenario);
                FScenarioList.Add(LScenario);
              end;
            end;
          //end;
          Break;
        end;
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TStudyList.AddPreProcessors;
const OPNAME = 'TStudyList.AddPreProcessors';
begin
  try
    AddDailyDiversionPreProcessor;
    AddIFRPreProcessor;
    AddDamSedimentation;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


procedure TStudyList.AddDamSedimentation;
const OPNAME = 'TStudyList.AddDamSedimentation';
var
  LModelPos,
  LStudyIndex,
  LModelIndex,
  LSubAreaIndex,
  LScenarioIndex : integer;
  LStudy: TStudyDataObject;
  LDamSedimentationModel: TModelDataObject;
  LModel: TModelDataObject;
  LSubArea: TSubAreaDataObject;
  LScenario: TScenarioDataObject;
begin
  try
    for LStudyIndex := 0 to FStudyList.Count - 1 do
    begin
      LStudy  := TStudyDataObject(FStudyList[LStudyIndex]);
      for LModelIndex := 0 to LStudy.ModelList.Count - 1 do
      begin
        LModel := LStudy.Model[LModelIndex];
        if(UpperCase(LModel.Model) = 'YIELD') then
        begin
          LDamSedimentationModel := TModelDataObject.Create;
          LDamSedimentationModel.AssignFrom(LModel);
          LDamSedimentationModel.SubModel   := CDamSedimentation;
          LDamSedimentationModel.ModelLabel := FAppModules.Language.GetString('StudySelection.DamSedimentationPreProcessor');
          LDamSedimentationModel.ModelDescr := FAppModules.Language.GetString('StudySelection.DamSedimentationPreProcessor');
          LDamSedimentationModel.Model := CDamSedimentation;
          LDamSedimentationModel.Editable := False;
          LStudy.AddModel(LDamSedimentationModel);
          LModelPos := FModelList.Add(LDamSedimentationModel);
          LDamSedimentationModel.ItemIndex := LModelPos;
          FAllOwnedObjects.Add(LDamSedimentationModel);
          FModelList.Add(LDamSedimentationModel);

          for LSubAreaIndex  := 0 to LDamSedimentationModel.SubAreaList.Count -1 do
          begin
            LSubArea := TSubAreaDataObject(LDamSedimentationModel.SubAreaList[LSubAreaIndex]);
            LSubArea.ItemIndex := LSubAreaIndex;
            LSubArea.Editable := False;
            FAllOwnedObjects.Add(LSubArea);
            FSubAreaList.Add(LSubArea);
            for LScenarioIndex := 0 to LSubArea.ScenarioList.Count -1 do
            begin
              LScenario := TScenarioDataObject(LSubArea.ScenarioList[LScenarioIndex]);
              LScenario.Editable := False;
              LScenario.ItemIndex := LScenarioIndex;
              FAllOwnedObjects.Add(LScenario);
              FScenarioList.Add(LScenario);
            end;
          end;
          Break;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TStudyList.AddDailyDiversionPreProcessor;
const OPNAME = 'TStudyList.AddDailyDiversionPreProcessor';
var
  LModelPos,
  LStudyIndex,
  LModelIndex,
  LSubAreaIndex,
  LScenarioIndex : integer;
  LStudy: TStudyDataObject;
  LDailyDiversionModel: TModelDataObject;
  LModel: TModelDataObject;
  LSubArea: TSubAreaDataObject;
  LScenario: TScenarioDataObject;
begin
  try
    for LStudyIndex := 0 to FStudyList.Count - 1 do
    begin
      LStudy  := TStudyDataObject(FStudyList[LStudyIndex]);
      for LModelIndex := 0 to LStudy.ModelList.Count - 1 do
      begin
        LModel := LStudy.Model[LModelIndex];
        if(UpperCase(LModel.Model) = 'YIELD') then
        begin
          LDailyDiversionModel := TModelDataObject.Create;
          LDailyDiversionModel.AssignFrom(LModel);
          LDailyDiversionModel.SubModel   := CDailyDiversion;
          LDailyDiversionModel.ModelLabel := FAppModules.Language.GetString('StudySelection.DailyDiversionPreProcessor');
          LDailyDiversionModel.ModelDescr := FAppModules.Language.GetString('StudySelection.DailyDiversionPreProcessor');
          LDailyDiversionModel.Model := CDailyDiversion;
          LDailyDiversionModel.Editable := False;
          LStudy.AddModel(LDailyDiversionModel);
          LModelPos := FModelList.Add(LDailyDiversionModel);
          LDailyDiversionModel.ItemIndex := LModelPos;
          FAllOwnedObjects.Add(LDailyDiversionModel);
          FModelList.Add(LDailyDiversionModel);

          for LSubAreaIndex  := 0 to LDailyDiversionModel.SubAreaList.Count -1 do
          begin
            LSubArea := TSubAreaDataObject(LDailyDiversionModel.SubAreaList[LSubAreaIndex]);
            LSubArea.ItemIndex := LSubAreaIndex;
            LSubArea.Editable := False;
            FAllOwnedObjects.Add(LSubArea);
            FSubAreaList.Add(LSubArea);
            for LScenarioIndex := 0 to LSubArea.ScenarioList.Count -1 do
            begin
              LScenario := TScenarioDataObject(LSubArea.ScenarioList[LScenarioIndex]);
              LScenario.Editable := False;
              LScenario.ItemIndex := LScenarioIndex;
              FAllOwnedObjects.Add(LScenario);
              FScenarioList.Add(LScenario);
            end;
          end;
          Break;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TStudyList.AddIFRPreProcessor;
const OPNAME = 'TStudyList.AddIFRPreProcessor';
var
  LModelPos,
  LStudyIndex,
  LModelIndex,
  LSubAreaIndex,
  LScenarioIndex : integer;
  LStudy: TStudyDataObject;
  LIFRModel: TModelDataObject;
  LModel: TModelDataObject;
  LSubArea: TSubAreaDataObject;
  LScenario: TScenarioDataObject;
begin
  try
    for LStudyIndex := 0 to FStudyList.Count - 1 do
    begin
      LStudy  := TStudyDataObject(FStudyList[LStudyIndex]);
      for LModelIndex := 0 to LStudy.ModelList.Count - 1 do
      begin
        LModel := LStudy.Model[LModelIndex];
        if(UpperCase(LModel.Model) = 'YIELD') then
        begin
          LIFRModel := TModelDataObject.Create;
          LIFRModel.AssignFrom(LModel);
          LIFRModel.SubModel   := CIFRPreProcessor;
          LIFRModel.ModelLabel := FAppModules.Language.GetString('StudySelection.IFRPreProcessor');
          LIFRModel.ModelDescr := FAppModules.Language.GetString('StudySelection.IFRPreProcessor');
          LIFRModel.Model := CIFRPreProcessor;
          LIFRModel.Editable := False;
          LStudy.AddModel(LIFRModel);
          LModelPos := FModelList.Add(LIFRModel);
          LIFRModel.ItemIndex := LModelPos;
          FAllOwnedObjects.Add(LIFRModel);
          FModelList.Add(LIFRModel);

          for LSubAreaIndex  := 0 to LIFRModel.SubAreaList.Count -1 do
          begin
            LSubArea := TSubAreaDataObject(LIFRModel.SubAreaList[LSubAreaIndex]);
            LSubArea.ItemIndex := LSubAreaIndex;
            LSubArea.Editable := False;
            FAllOwnedObjects.Add(LSubArea);
            FSubAreaList.Add(LSubArea);
            for LScenarioIndex := 0 to LSubArea.ScenarioList.Count -1 do
            begin
              LScenario := TScenarioDataObject(LSubArea.ScenarioList[LScenarioIndex]);
              LScenario.Editable := False;
              LScenario.ItemIndex := LScenarioIndex;
              FAllOwnedObjects.Add(LScenario);
              FScenarioList.Add(LScenario);
            end;
          end;
          Break;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
