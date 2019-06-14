//
//
//  UNIT      : Contains TFile05DatabaseAgent Class
//  AUTHOR    : Titi Ngubane(PDNA)
//  DATE      : 18/02/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UFile05DatabaseAgent;

interface

uses
  Classes, sysutils,Db,

  //  DWAF VCL
  UFileNames,
  UConstants,
  UDatabaseUtilities,
  UAbstractObject,
  UDataFileObjects,
  UAbstractFileNamesObject,
  UZoneDefinitionsObject,
  UAbstractDatabaseAgent,
  UYieldModelDataObject;

type

  TFile05DatabaseAgent = class(TAbstractDatabaseAgent)
  protected
    function ReadF05UnkownDataSQL: string;
    function ReadStorageZonesDataSQL: string;
    function ReadStorageZoneDetailsDataSQL: string;
//    function ReadReservoirZonePenaltySQL: string;
    function ReadNodesDetailsDataSQL: string;
    function ReadReservoirLevelsDataSQL: string;

    function WriteStorageZonesDataSQL: string;
    function WriteStorageZoneDetailsDataSQL: string;
//    function WriteReservoirZonePenaltySQL: string;
    function WriteNodesDetailsDataSQL: string;
    function WriteReservoirLevelsDataSQL: string;
    function WriteUnknownDataSQL: string;

  public
    { Public declarations }
    function ReadModelDataFromDatabase(AFileName:TFileNameObject; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
    function WriteModelDataToDatabase(AFileName:TFileNameObject;ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
    function ClearModelDataInDatabase(AFileName:TFileNameObject;AProgressFunction: TProgressUpdateFuntion;
             AQuetly: boolean = False): boolean; override;
  end;


implementation

uses
  System.Contnrs,
  UUtilities,
  UDataSetType,
  UErrorHandlingOperations, UReservoirObject, UBasicObjects;

function TFile05DatabaseAgent.ReadF05UnkownDataSQL: string;
const OPNAME = 'TFile05DatabaseAgent.ReadF05UnkownDataSQL';
begin

  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,LineNumber,LineData '+
              ' FROM WRYMFileLines'+
              ' WHERE Model       =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' AND FileGroup     = :FileGroup' +
              ' AND FileType      = :FileType'+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,LineNumber,LineSection';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile05DatabaseAgent.ReadStorageZonesDataSQL: string;
const OPNAME = 'TFile05DatabaseAgent.ReadStorageZonesDataSQL';
begin
  Result := '';
  try
    Result := 'SELECT'+
      '  St.Model,St.StudyAreaName,St.SubArea,St.Scenario,St.StorageZoneCount,St.ZoneLowerBoundary,St.PenaltyStructureCount'+
      ' ,St.NodesCount,St.ReservoirLevelsCount,St.StorageZonesComment as Comment1,St.ZeroComment'+
      ' FROM StorageZones St'+
      ' WHERE ((St.Model              ='+QuotedStr(FAppModules.StudyArea.ModelCode)+') and'+
      '       (St.StudyAreaName      ='+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+') and'+
      '       (St.SubArea            ='+QuotedStr(FAppModules.StudyArea.SubAreaCode)+') and'+
      '       (St.Scenario           ='+QuotedStr(FAppModules.StudyArea.ScenarioCode)+'))';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile05DatabaseAgent.ReadStorageZoneDetailsDataSQL: string;
const OPNAME = 'TFile05DatabaseAgent.ReadStorageZoneDetailsDataSQL';
begin
  Result := '';
  try
    Result := 'SELECT'+
      '  Sd.Model,Sd.StudyAreaName,Sd.SubArea,Sd.Scenario'+
      ' ,Sd.Identifier,Sd.ReservoirZoneName,Sd.StrategyIndicator,Sd.BalancingVariable,Sd.BalancingPolicy'+
      ' ,Sd.BalRef01,Sd.BalRef02,Sd.BalRef03,Sd.BalRef04,Sd.BalRef05'+
      ' ,Sd.BalRef06,Sd.BalRef07,Sd.BalRef08,Sd.BalRef09,Sd.BalRef10'+
      ' ,Sd.BalRef11,Sd.BalRef12,Sd.BalRef13,Sd.BalRef14,Sd.BalRef15'+
      ' ,Sd.BalRef16,Sd.BalRef17,Sd.BalRef18,Sd.BalRef19,Sd.BalRef20'+
      ' ,Sd.StorageZoneDetailsComment as Comment2'+
      ' FROM StorageZoneDetails Sd'+
      ' WHERE ((Sd.Model              ='+QuotedStr(FAppModules.StudyArea.ModelCode)+') and'+
      '       (Sd.StudyAreaName      ='+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+') and'+
      '       (Sd.SubArea            ='+QuotedStr(FAppModules.StudyArea.SubAreaCode)+') and'+
      '       (Sd.Scenario           ='+QuotedStr(FAppModules.StudyArea.ScenarioCode)+'))'+
      '       ORDER BY Sd.Model,Sd.StudyAreaName,Sd.SubArea,Sd.Scenario,Sd.Identifier';

  except on E: Exception do HandleError(E, OPNAME) end;
end;
(*
function TFile05DatabaseAgent.ReadReservoirZonePenaltySQL: string;
const OPNAME = 'TFile05DatabaseAgent.ReadReservoirZonePenaltySQL';
begin

  Result := '';
  try
    Result :=
      'SELECT Model, StudyAreaName, SubArea, Scenario, Identifier, ' +
      'ZonePenalty01, ZonePenalty02, ZonePenalty03, ZonePenalty04, ZonePenalty05, ' +
      'ZonePenalty06, ZonePenalty07, ZonePenalty08, ZonePenalty09, ZonePenalty10 ' +
      'FROM ReservoirZonePenalty ' +
      'WHERE Model         =' + QuotedStr(FAppModules.StudyArea.ModelCode) +
      '  AND StudyAreaName =' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +
      '  AND SubArea       =' + QuotedStr(FAppModules.StudyArea.SubAreaCode) +
      '  AND Scenario      =' + QuotedStr(FAppModules.StudyArea.ScenarioCode) +
      ' ORDER BY Identifier';

  except on E: Exception do HandleError(E, OPNAME) end;
end;
*)
function TFile05DatabaseAgent.ReadNodesDetailsDataSQL: string;
const OPNAME = 'TFile05DatabaseAgent.ReadNodesDetailsDataSQL';
begin

  Result := '';
  try
    Result := 'SELECT'+
      ' Nd.Model,Nd.StudyAreaName,Nd.SubArea,Nd.Scenario,Nd.Identifier'+
      ' ,Nd.NodeNumberStorage,Nd.StatusIndicator,Nd.PlottingOption,Nd.ReservoirPriority,Nd.FullSupplyLevel'+
      ' ,Nd.DeadStorageLevel ,Nd.BottomOfReservoir,Nd.FullSupplyAllocation,Nd.NodesDetailsComment as Comment3'+
      ' FROM  NodesDetails Nd'+
      '          WHERE ((Nd.Model ='+QuotedStr(FAppModules.StudyArea.ModelCode)+')'+
      '            AND (Nd.StudyAreaName ='+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+')'+
      '            AND (Nd.SubArea ='+QuotedStr(FAppModules.StudyArea.SubAreaCode)+')'+
      '            AND (Nd.Scenario ='+QuotedStr(FAppModules.StudyArea.ScenarioCode)+'))'+
      '          ORDER BY Nd.Model,Nd.StudyAreaName,Nd.SubArea,Nd.Scenario,Nd.Identifier';

  except on E: Exception do HandleError(E, OPNAME) end;
end;
function TFile05DatabaseAgent.ReadReservoirLevelsDataSQL: string;
const OPNAME = 'TFile05DatabaseAgent.ReadReservoirLevelsDataSQL';
begin

  Result := '';
  try
    Result := 'SELECT'+
      '  Rs.Model,Rs.StudyAreaName,Rs.SubArea,Rs.Scenario,Rs.RecordIdentifier,Rs.ReservoirIdentifier,Rs.LevelIdentifier'+
      ' ,Rs.ReservoirLev01,Rs.ReservoirLev02,Rs.ReservoirLev03,Rs.ReservoirLev04,Rs.ReservoirLev05,Rs.ReservoirLev06,Rs.ReservoirLev07,Rs.ReservoirLev08'+
      ' ,Rs.ReservoirLev09,Rs.ReservoirLev10,Rs.ReservoirLev11,Rs.ReservoirLev12,Rs.ResLevelsComment as Comment4'+
      ' FROM ReservoirLevels Rs'+
      '          WHERE ((Rs.Model ='+QuotedStr(FAppModules.StudyArea.ModelCode)+') AND'+
      '               (Rs.StudyAreaName ='+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+') AND'+
      '               (Rs.SubArea ='+QuotedStr(FAppModules.StudyArea.SubAreaCode)+') AND'+
      '               (Rs.Scenario ='+QuotedStr(FAppModules.StudyArea.ScenarioCode)+'))'+
      '          ORDER BY Rs.Model,Rs.StudyAreaName,Rs.SubArea,Rs.Scenario,Rs.RecordIdentifier';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile05DatabaseAgent.WriteStorageZonesDataSQL: string;
const OPNAME = 'TFile05DatabaseAgent.WriteStorageZonesDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO StorageZones'+
              ' (Model,StudyAreaName,SubArea,Scenario,StorageZoneCount,ZoneLowerBoundary,PenaltyStructureCount'+
              ',NodesCount,ReservoirLevelsCount,StorageZonesComment,ZeroComment)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:StorageZoneCount,:ZoneLowerBoundary'+
              ',:PenaltyStructureCount,:NodesCount,:ReservoirLevelsCount,:StorageZonesComment,:ZeroComment)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile05DatabaseAgent.WriteStorageZoneDetailsDataSQL: string;
const OPNAME = 'TFile05DatabaseAgent.WriteStorageZoneDetailsDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO StorageZoneDetails'+
              ' (Model,StudyAreaName,SubArea,Scenario,Identifier,ReservoirZoneName' +
              ' ,StrategyIndicator,BalancingVariable,BalancingPolicy' +
              ' ,BalRef01,BalRef02,BalRef03,BalRef04,BalRef05' +
              ' ,BalRef06,BalRef07,BalRef08,BalRef09,BalRef10' +
              ' ,BalRef11,BalRef12,BalRef13,BalRef14,BalRef15' +
              ' ,BalRef16,BalRef17,BalRef18,BalRef19,BalRef20' +
              ' ,StorageZoneDetailsComment)'+
              ' Values'+
              ' (:Model,:StudyAreaName,:SubArea,:Scenario,:Identifier,:ReservoirZoneName' +
              ' ,:StrategyIndicator,:BalancingVariable,:BalancingPolicy' +
              ' ,:BalRef01,:BalRef02,:BalRef03,:BalRef04,:BalRef05' +
              ' ,:BalRef06,:BalRef07,:BalRef08,:BalRef09,:BalRef10' +
              ' ,:BalRef11,:BalRef12,:BalRef13,:BalRef14,:BalRef15' +
              ' ,:BalRef16,:BalRef17,:BalRef18,:BalRef19,:BalRef20' +
              ' ,:StorageZoneDetailsComment)';

  except on E: Exception do HandleError(E, OPNAME) end;
end;
(*
function TFile05DatabaseAgent.WriteReservoirZonePenaltySQL: string;
const OPNAME = 'TFile05DatabaseAgent.WriteReservoirZonePenaltySQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO ReservoirZonePenalty'+
              ' (Model,StudyAreaName,SubArea,Scenario,Identifier,PenaltyName,' +
              'ZonePenalty01,ZonePenalty02,ZonePenalty03,ZonePenalty04,ZonePenalty05,' +
              'ZonePenalty06,ZonePenalty07,ZonePenalty08,ZonePenalty09,ZonePenalty10)'+
              ' Values'+
              ' (:Model,:StudyAreaName,:SubArea,:Scenario,:Identifier,:PenaltyName,' +
              ':ZonePenalty01,:ZonePenalty02,:ZonePenalty03,:ZonePenalty04,:ZonePenalty05,' +
              ':ZonePenalty06,:ZonePenalty07,:ZonePenalty08,:ZonePenalty09,:ZonePenalty10)';

  except on E: Exception do HandleError(E, OPNAME) end;
end;
*)
function TFile05DatabaseAgent.WriteNodesDetailsDataSQL: string;
const OPNAME = 'TFile05DatabaseAgent.WriteNodesDetailsDataSQL';
begin
  Result := '';
  try                                            
    Result := 'INSERT INTO NodesDetails'+
              ' (Model,StudyAreaName,SubArea,Scenario,Identifier,NodeNumberStorage,StatusIndicator,PlottingOption,'+
              '  ReservoirPriority,FullSupplyLevel,DeadStorageLevel,BottomOfReservoir,FullSupplyAllocation,NodesDetailsComment)'+
              ' Values'+
              ' (:Model,:StudyAreaName,:SubArea,:Scenario,:Identifier,:NodeNumberStorage,:StatusIndicator,:PlottingOption,'+
              '  :ReservoirPriority,:FullSupplyLevel,:DeadStorageLevel,:BottomOfReservoir,:FullSupplyAllocation,:NodesDetailsComment)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile05DatabaseAgent.WriteReservoirLevelsDataSQL: string;
const OPNAME = 'TFile05DatabaseAgent.WriteReservoirLevelsDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO ReservoirLevels'+
              ' (Model,StudyAreaName,SubArea,Scenario,RecordIdentifier,LevelIdentifier,ReservoirIdentifier,ReservoirLev01,ReservoirLev02,ReservoirLev03,ReservoirLev04,ReservoirLev05'+
              ',ReservoirLev06,ReservoirLev07,ReservoirLev08,ReservoirLev09,ReservoirLev10,ReservoirLev11,ReservoirLev12,ResLevelsComment)'+
              ' Values'+
              '(:Model,:StudyAreaName,:SubArea,:Scenario,:RecordIdentifier,:LevelIdentifier,:ReservoirIdentifier,:ReservoirLev01,:ReservoirLev02,:ReservoirLev03,:ReservoirLev04'+
              ' ,:ReservoirLev05,:ReservoirLev06,:ReservoirLev07,:ReservoirLev08,:ReservoirLev09,:ReservoirLev10,:ReservoirLev11,:ReservoirLev12,:ResLevelsComment)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TFile05DatabaseAgent.WriteUnknownDataSQL: string;
const OPNAME = 'TFile05DatabaseAgent.WriteUnknownDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO WRYMFileLines'+
              ' (Model,StudyAreaName,SubArea,Scenario,FileType,FileGroup,LineNumber,LineSection,LineData)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:FileType,:FileGroup,:LineNumber,:LineSection,:LineData)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile05DatabaseAgent.ReadModelDataFromDatabase(AFileName:TFileNameObject; ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile05DatabaseAgent.ReadModelDataFromDatabase';
var
  LMessage             : string;
  LDataSet             : TAbstractModelDataset;
  LCount               : Integer;
  LNoData              : Boolean;
  LStop                : boolean;
  lFieldName           : string;
  lIndex               : integer;
  LZoneDefinitions     : TZoneDefinitionsObject;
  LStorageZones        : TStorageZones;
  LNode                : TNodes;
  LReservoirLevel      : TReservoirLevels;
  LReservoirName       : String;
  LReservoirNodeNumber : Integer;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile05DatabaseAgent.strReadStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    LZoneDefinitions := ADataObject.FZoneDefinitionsObject;

    if not LZoneDefinitions.Initialise then
    Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try

      LDataSet.SetSQL(ReadStorageZonesDataSQL);
      LDataSet.DataSet.Open;

      //Check if there is any data.
      LNoData := False;
      if (LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
      begin
        LNoData := True;
        LMessage := FAppModules.Language.GetString('TFile05DatabaseAgent.strNoDataReturned');
        AProgressFunction(LMessage,ptWarning,LStop);
        LZoneDefinitions.FStorageZoneCount.FData := 0;
        LZoneDefinitions.FStorageZoneCount.FInitalised := True;
        LZoneDefinitions.FZoneLowerBoundary.FData := 0;
        LZoneDefinitions.FZoneLowerBoundary.FInitalised := True;
        LZoneDefinitions.FPenaltyStructureCount.FData := 0;
        LZoneDefinitions.FPenaltyStructureCount.FInitalised := True;
      //  Exit;
      end;

      //Read the F05 file
      // Line 1
      if not LDataSet.DataSet.FieldByName('StorageZoneCount').IsNull then
      begin
        LZoneDefinitions.FStorageZoneCount.FData := LDataSet.DataSet.FieldByName('StorageZoneCount').AsInteger;
        LZoneDefinitions.FStorageZoneCount.FInitalised := True;
      end;

      if not LDataSet.DataSet.FieldByName('ZoneLowerBoundary').IsNull then
      begin
        LZoneDefinitions.FZoneLowerBoundary.FData := LDataSet.DataSet.FieldByName('ZoneLowerBoundary').AsInteger;
        LZoneDefinitions.FZoneLowerBoundary.FInitalised := True;
      end;

      if not LDataSet.DataSet.FieldByName('PenaltyStructureCount').IsNull then
      begin
        LZoneDefinitions.FPenaltyStructureCount.FData := LDataSet.DataSet.FieldByName('PenaltyStructureCount').AsInteger;
        LZoneDefinitions.FPenaltyStructureCount.FInitalised := True;
      end;

      if not LDataSet.DataSet.FieldByName('NodesCount').IsNull then
      begin
        LZoneDefinitions.FNodesCount.FData := LDataSet.DataSet.FieldByName('NodesCount').AsInteger;
        LZoneDefinitions.FNodesCount.FInitalised := True;
      end;
      if not LDataSet.DataSet.FieldByName('ReservoirLevelsCount').IsNull then
      begin
        LZoneDefinitions.FReservoirLevelsCount.FData := LDataSet.DataSet.FieldByName('ReservoirLevelsCount').AsInteger;
        LZoneDefinitions.FReservoirLevelsCount.FInitalised := True;
      end;

      if not LDataSet.DataSet.FieldByName('Comment1').IsNull then
      begin
        LZoneDefinitions.FComment.FData := TrimRight(LDataSet.DataSet.FieldByName('Comment1').AsString);
        LZoneDefinitions.FComment.FLength := Length(LZoneDefinitions.FComment.FData);
        LZoneDefinitions.FComment.FInitalised := True;
      end;

      if not LDataSet.DataSet.FieldByName('ZeroComment').IsNull then
      begin
        LZoneDefinitions.FZeroComment.FData   := TrimRight(LDataSet.DataSet.FieldByName('ZeroComment').AsString);
        LZoneDefinitions.FZeroComment.FLength := Length(LZoneDefinitions.FZeroComment.FData);
        LZoneDefinitions.FZeroComment.FInitalised:= True;
      end;

      if not LZoneDefinitions.AddStorageZones then
       Exit;

      LDataSet.DataSet.Close;
      LDataSet.SetSQL(ReadStorageZoneDetailsDataSQL);
      LDataSet.DataSet.Open;

      // Line 1a
      for LCount := 1 to LZoneDefinitions.FStorageZoneCount.FData do
      begin
        if(LCount >= LZoneDefinitions.FStorageZones.Count) then Break;

        LStorageZones := TStorageZones(LZoneDefinitions.FStorageZones[LCount]);

        if not LDataSet.DataSet.FieldByName('ReservoirZoneName').IsNull then
        begin
          LStorageZones.FResevoirZoneName.FData := Trim(LDataSet.DataSet.FieldByName('ReservoirZoneName').AsString);
          LStorageZones.FResevoirZoneName.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('StrategyIndicator').IsNull then
        begin
          LStorageZones.FStrategyIndicator.FData := LDataSet.DataSet.FieldByName('StrategyIndicator').AsInteger;
          LStorageZones.FStrategyIndicator.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('BalancingVariable').IsNull then
        begin
          LStorageZones.FBalancingVariable.FData := LDataSet.DataSet.FieldByName('BalancingVariable').AsInteger;
          LStorageZones.FBalancingVariable.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('BalancingPolicy').IsNull then
        begin
          LStorageZones.FBalancingPolicy.FData := LDataSet.DataSet.FieldByName('BalancingPolicy').AsInteger;
          LStorageZones.FBalancingPolicy.FInitalised := True;
        end;

        for lIndex := 1 to 20 do
        begin
          lFieldName := Format('%s%2.2d',['BalRef',lIndex]);
          if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
          begin
            LStorageZones.FBalanceReference[lIndex].FData := LDataSet.DataSet.FieldByName(LFieldName).AsFloat;
            LStorageZones.FBalanceReference[lIndex].FInitalised := True;
          end;
        end;


        if not LDataSet.DataSet.FieldByName('Comment2').IsNull then
        begin
          LStorageZones.FComment.FData   := TrimRight(LDataSet.DataSet.FieldByName('Comment2').AsString);
          LStorageZones.FComment.FLength := Length(LStorageZones.FComment.FData);
          LStorageZones.FComment.FInitalised:= True;
        end;
        LDataSet.DataSet.next;
      end;
(*
      LDataSet.SetSQL(ReadReservoirZonePenaltySQL);
      LDataSet.DataSet.Open;

      //Check if there is any data.
      if (LDataSet.DataSet.RecordCount = 0) then
      begin
        LNoData := True;
        LMessage := FAppModules.Language.GetString('TFile05DatabaseAgent.strNoDataReturned');
        AProgressFunction(LMessage,ptWarning,LStop);
      //  Exit;
      end;
      // Line 1a
      for LCount := MinPenaltyStructure to MaxPenaltyStructure do
      begin
        for LLinesCount := 1 to LZoneDefinitions.FStorageZoneCount.FData do
        begin
          LFieldName := Format('%s%2.2d',['ZonePenalty',LLinesCount]);
          LDataSet.SetParams([LFieldName], [FloatToStr(TStorageZones(LZoneDefinitions.FStorageZones[LLinesCount]).FBalanceReference[LCount].FData)]);
          if (NOT LDataSet.DataSet.FieldByName(LFieldName).IsNull) then
          begin
            TStorageZones(LZoneDefinitions.FStorageZones[lLinesCount]).FBalanceReference[lCount].FData :=
              LDataSet.DataSet.FieldByName(LFieldName).AsFloat;
            TStorageZones(LZoneDefinitions.FStorageZones[lLinesCount]).FBalanceReference[lCount].FInitalised := True;
          end;
        end;
        LDataSet.DataSet.Next;
      end;
*)
      LDataSet.DataSet.Close;
      LDataSet.SetSQL(ReadNodesDetailsDataSQL);
      LDataSet.DataSet.Open;

      //Line 2
      for LCount := 1 to LZoneDefinitions.FNodesCount.FData do
        if not LZoneDefinitions.AddNode then Exit;

      LDataSet.DataSet.First;
      for LCount := 1 to LZoneDefinitions.FNodesCount.FData do
      begin

        LNode := TNodes(LZoneDefinitions.FNodes[LCount]);

        if not LDataSet.DataSet.FieldByName('NodeNumberStorage').IsNull then
        begin
          LNode.FNodeNumberStorage.FData := LDataSet.DataSet.FieldByName('NodeNumberStorage').AsInteger;
          LNode.FNodeNumberStorage.FInitalised := True;
          if  LNode.FNodeNumberStorage.FData = 0 then
          Break;
        end;

        if not LDataSet.DataSet.FieldByName('StatusIndicator').IsNull then
        begin
          LNode.FStatusIndicator.FData := LDataSet.DataSet.FieldByName('StatusIndicator').AsInteger;
          LNode.FStatusIndicator.FInitalised := True;

          if((LNode.FStatusIndicator.FData = 0) and (ADataObject.FReservoirObject.ReservoirByReservoirNumber(LNode.FNodeNumberStorage.FData) <> nil)) then
          begin
            LReservoirNodeNumber := LDataSet.DataSet.FieldByName('NodeNumberStorage').AsInteger;
            LReservoirName       := ADataObject.FReservoirObject.ReservoirByReservoirNumber(LNode.FNodeNumberStorage.FData).FName.FData;
            LMessage             := FAppModules.Language.GetString('TFile05Agent.strSelectionStatusErr');
            LMessage             := Format(LMessage,[LReservoirNodeNumber,Trim(LReservoirName)]);
            AProgressFunction(LMessage,ptWarning,LStop);
          end;
        end;

        if not LDataSet.DataSet.FieldByName('PlottingOption').IsNull then
        begin
          LNode.FPlottingOption.FData := LDataSet.DataSet.FieldByName('PlottingOption').AsInteger;
          LNode.FPlottingOption.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('ReservoirPriority').IsNull then
        begin
          LNode.FReservoirPriority.FData := LDataSet.DataSet.FieldByName('ReservoirPriority').AsFloat;
          LNode.FReservoirPriority.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('FullSupplyLevel').IsNull then
        begin
          LNode.FFullSupplyLevel.FData := LDataSet.DataSet.FieldByName('FullSupplyLevel').AsFloat;
          LNode.FFullSupplyLevel.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('DeadStorageLevel').IsNull then
        begin
          LNode.FDeadStorageLevel.FData :=LDataSet.DataSet.FieldByName('DeadStorageLevel').AsFloat;
          LNode.FDeadStorageLevel.FInitalised:= True;
        end;

        if not LDataSet.DataSet.FieldByName('BottomOfReservoir').IsNull then
        begin
          LNode.FBottomOfReservoir.FData :=LDataSet.DataSet.FieldByName('BottomOfReservoir').AsFloat;
          LNode.FBottomOfReservoir.FInitalised:= True;
        end;

        if not LDataSet.DataSet.FieldByName('FullSupplyAllocation').IsNull then
        begin
          LNode.FFullSupplyAllocation.FData :=LDataSet.DataSet.FieldByName('FullSupplyAllocation').AsFloat;
          LNode.FFullSupplyAllocation.FInitalised:= True;
        end;

        if not LDataSet.DataSet.FieldByName('Comment3').IsNull then
        begin
          LNode.FComment.FData   := TrimRight(LDataSet.DataSet.FieldByName('Comment3').AsString);
          LNode.FComment.FLength := Length(LNode.FComment.FData);
          LNode.FComment.FInitalised:= True;
        end;
        LDataSet.DataSet.Next;
      end;

      LDataSet.DataSet.Close;
      LDataSet.SetSQL(ReadReservoirLevelsDataSQL);
      LDataSet.DataSet.Open;

      LCount := 0;
      while not LDataSet.DataSet.Eof do
      begin
        LCount := LCount + 1;
        LDataSet.DataSet.next;
      end;
      LZoneDefinitions.FReservoirLevelsCount.FData := LCount;
      LZoneDefinitions.FReservoirLevelsCount.FInitalised := True;


      //Line 3
      if not LZoneDefinitions.AddReservoirLevels then
       Exit;

      LDataSet.DataSet.First;
      for LCount := 1 to  LZoneDefinitions.FReservoirLevelsCount.FData do
      begin
        LReservoirLevel := TReservoirLevels(LZoneDefinitions.FReservoirLevels[LCount]);
        if not LDataSet.DataSet.FieldByName('ReservoirIdentifier').IsNull then
        begin
          LReservoirLevel.FReservoirIdentifier.FData :=LDataSet.DataSet.FieldByName('ReservoirIdentifier').AsInteger;
          LReservoirLevel.FReservoirIdentifier.FInitalised:= True;
        end;
        if not LDataSet.DataSet.FieldByName('ReservoirLev01').IsNull then
        begin
          LReservoirLevel.FResLevel[1].FData :=LDataSet.DataSet.FieldByName('ReservoirLev01').AsFloat;
          LReservoirLevel.FResLevel[1].FInitalised:= True;
        end;
        if not LDataSet.DataSet.FieldByName('ReservoirLev02').IsNull then
        begin
          LReservoirLevel.FResLevel[2].FData :=LDataSet.DataSet.FieldByName('ReservoirLev02').AsFloat;
          LReservoirLevel.FResLevel[2].FInitalised:= True;
        end;
        if not LDataSet.DataSet.FieldByName('ReservoirLev03').IsNull then
        begin
          LReservoirLevel.FResLevel[3].FData :=LDataSet.DataSet.FieldByName('ReservoirLev03').AsFloat;
          LReservoirLevel.FResLevel[3].FInitalised:= True;
        end;
        if not LDataSet.DataSet.FieldByName('ReservoirLev04').IsNull then
        begin
          LReservoirLevel.FResLevel[4].FData :=LDataSet.DataSet.FieldByName('ReservoirLev04').AsFloat;
          LReservoirLevel.FResLevel[4].FInitalised:= True;
        end;
        if not LDataSet.DataSet.FieldByName('ReservoirLev05').IsNull then
        begin
          LReservoirLevel.FResLevel[5].FData :=LDataSet.DataSet.FieldByName('ReservoirLev05').AsFloat;
          LReservoirLevel.FResLevel[5].FInitalised:= True;
        end;
        if not LDataSet.DataSet.FieldByName('ReservoirLev06').IsNull then
        begin
          LReservoirLevel.FResLevel[6].FData :=LDataSet.DataSet.FieldByName('ReservoirLev06').AsFloat;
          LReservoirLevel.FResLevel[6].FInitalised:= True;
        end;
        if not LDataSet.DataSet.FieldByName('ReservoirLev07').IsNull then
        begin
          LReservoirLevel.FResLevel[7].FData :=LDataSet.DataSet.FieldByName('ReservoirLev07').AsFloat;
          LReservoirLevel.FResLevel[7].FInitalised:= True;
        end;
        if not LDataSet.DataSet.FieldByName('ReservoirLev08').IsNull then
        begin
          LReservoirLevel.FResLevel[8].FData :=LDataSet.DataSet.FieldByName('ReservoirLev08').AsFloat;
          LReservoirLevel.FResLevel[8].FInitalised:= True;
        end;
        if not LDataSet.DataSet.FieldByName('ReservoirLev09').IsNull then
        begin
          LReservoirLevel.FResLevel[9].FData :=LDataSet.DataSet.FieldByName('ReservoirLev09').AsFloat;
          LReservoirLevel.FResLevel[9].FInitalised:= True;
        end;
        if not LDataSet.DataSet.FieldByName('ReservoirLev10').IsNull then
        begin
          LReservoirLevel.FResLevel[10].FData :=LDataSet.DataSet.FieldByName('ReservoirLev10').AsFloat;
          LReservoirLevel.FResLevel[10].FInitalised:= True;
        end;
        if not LDataSet.DataSet.FieldByName('ReservoirLev11').IsNull then
        begin
          LReservoirLevel.FResLevel[11].FData :=LDataSet.DataSet.FieldByName('ReservoirLev11').AsFloat;
          LReservoirLevel.FResLevel[11].FInitalised:= True;
        end;
        if not LDataSet.DataSet.FieldByName('ReservoirLev12').IsNull then
        begin
          LReservoirLevel.FResLevel[12].FData :=LDataSet.DataSet.FieldByName('ReservoirLev12').AsFloat;
          LReservoirLevel.FResLevel[12].FInitalised:= True;
        end;
        if not LDataSet.DataSet.FieldByName('Comment4').IsNull then
        begin
          LReservoirLevel.FComment.FData   := TrimRight(LDataSet.DataSet.FieldByName('Comment4').AsString);
          LReservoirLevel.FComment.FLength := Length(LReservoirLevel.FComment.FData);
          LReservoirLevel.FComment.FInitalised:= True;
        end;
        LDataSet.DataSet.next;
      end;

      //Line 4 on wards+++++++++++++++++++++++++++
      LDataSet.DataSet.Close;
      LDataSet.SetSQL(ReadF05UnkownDataSQL);
      LDataSet.SetParams(['FileType'], [IntToStr(AFileName.FileNumber)]);
      LDataSet.SetParams(['FileGroup'], [IntToStr(AFileName.FileGroup)]);
      LDataSet.DataSet.Open;

      //check if there is unknown data
      LNoData := LNoData and (LDataSet.DataSet.RecordCount = 0);

      while not LDataSet.DataSet.Eof do
      begin
        LZoneDefinitions.FF05ExtraLines.Add(TrimRight(LDataSet.DataSet.FieldByName('LineData').AsString));
        LDataSet.DataSet.Next;
      end;
      LDataSet.DataSet.Close;

      LMessage := FAppModules.Language.GetString('TFile05DatabaseAgent.strReadEnded');
      AProgressFunction(LMessage,ptNone,LStop);
      Result :=  not LNoData;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile05DatabaseAgent.WriteModelDataToDatabase(AFileName:TFileNameObject;ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile05DatabaseAgent.WriteModelDataToDatabase';
var
  LFieldName,
  LMessage:string;
  LCount,
  LLinesCount,
  LCounter : integer;
  LDataSet : TAbstractModelDataset;
  LLevelIndex,
  LReservoirNumber : Integer;
  LStop            : boolean;
  LZoneDefinitions : TZoneDefinitionsObject;
  LStorageZones    : TStorageZones;
  LNode            : TNodes;
  LReservoirLevel  : TReservoirLevels;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile05DatabaseAgent.strWriteStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if not ClearModelDataInDatabase(AFileName,AProgressFunction,True) then
      Exit;

    LZoneDefinitions := ADataObject.FZoneDefinitionsObject;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try

      //Line 1
      LDataSet.DataSet.Close;
      LDataSet.SetSQL(WriteStorageZonesDataSQL);
      LDataSet.ClearQueryParams();
      LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
      LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
      LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
      LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
      if LZoneDefinitions.FStorageZoneCount.FInitalised then
        LDataSet.SetParams(['StorageZoneCount'], [IntToStr(LZoneDefinitions.FStorageZoneCount.FData)]);

      if LZoneDefinitions.FZoneLowerBoundary.FInitalised then
        LDataSet.SetParams(['ZoneLowerBoundary'], [IntToStr(LZoneDefinitions.FZoneLowerBoundary.FData)]);

      if LZoneDefinitions.FPenaltyStructureCount.FInitalised then
        LDataSet.SetParams(['PenaltyStructureCount'], [IntToStr(LZoneDefinitions.FPenaltyStructureCount.FData)]);

      if LZoneDefinitions.FNodesCount.FInitalised then
        LDataSet.SetParams(['NodesCount'], [IntToStr(LZoneDefinitions.FNodesCount.FData)]);

      if LZoneDefinitions.FReservoirLevelsCount.FInitalised then
        LDataSet.SetParams(['ReservoirLevelsCount'], [IntToStr(LZoneDefinitions.FReservoirLevelsCount.FData)]);

      if LZoneDefinitions.FComment.FInitalised then
        LDataSet.SetParams(['StorageZonesComment'], [LZoneDefinitions.FComment.FData]);

      if LZoneDefinitions.FZeroComment.FInitalised then
          LDataSet.SetParams(['ZeroComment'], [LZoneDefinitions.FZeroComment.FData]);

      LDataSet.ExecSQL;

      //Line 1a+++++++++++++++++++++++++++
      for LCount := 1 to LZoneDefinitions.FStorageZoneCount.FData do
      begin
        LStorageZones := TStorageZones(LZoneDefinitions.FStorageZones[LCount]);

        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteStorageZoneDetailsDataSQL);
        LDataSet.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['Identifier'], [IntToStr(LCount)]);
        if LStorageZones.FResevoirZoneName.FInitalised  then
          LDataSet.SetParams(['ReservoirZoneName'], [LStorageZones.FResevoirZoneName.FData]);

        if LStorageZones.FStrategyIndicator.FInitalised then
         LDataSet.SetParams(['StrategyIndicator'], [IntToStr(LStorageZones.FStrategyIndicator.FData)]);

        if LStorageZones.FBalancingVariable.FInitalised then
         LDataSet.SetParams(['BalancingVariable'], [IntToStr(LStorageZones.FBalancingVariable.FData)]);

        if LStorageZones.FBalancingPolicy.FInitalised then
         LDataSet.SetParams(['BalancingPolicy'], [IntToStr(LStorageZones.FBalancingPolicy.FData)]);

        for LLinesCount := MinPenaltyStructure to MaxPenaltyStructure do
        begin
           LFieldName := Format('%s%2.2d',['BalRef',LLinesCount]);
           if LStorageZones.FBalanceReference[LLinesCount].FInitalised then
            LDataSet.SetParams([LFieldName], [FloatToStr(LStorageZones.FBalanceReference[LLinesCount].FData)]);
        end;
        if LStorageZones.FComment.FInitalised then
         LDataSet.SetParams(['StorageZoneDetailsComment'], [LStorageZones.FComment.FData]);
        LDataSet.ExecSQL;
      end;
(*
      for LCount := MinPenaltyStructure to MaxPenaltyStructure do
      begin
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteReservoirZonePenaltySQL);
        LDataSet.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['Identifier'], [IntToStr(LCount)]);
        LDataSet.SetParams(['PenaltyName'], ['Penalty ' + IntToStr(LCount)]);
        for LLinesCount := 1 to LZoneDefinitions.FStorageZoneCount.FData do
        begin
          LFieldName := Format('%s%2.2d',['ZonePenalty',LLinesCount]);
          if TStorageZones(LZoneDefinitions.FStorageZones[LLinesCount]).FBalanceReference[LCount].FInitalised then
            LDataSet.SetParams([LFieldName], [FloatToStr(TStorageZones(LZoneDefinitions.FStorageZones[LLinesCount]).FBalanceReference[LCount].FData)]);
        end;
        LDataSet.ExecSQL;
      end;
*)
      //Line 2
       for LCount := 1 to LZoneDefinitions.FNodesCount.FData do
       begin

         LNode := TNodes(LZoneDefinitions.FNodes[LCount]);

         LDataSet.DataSet.Close;
         LDataSet.SetSQL(WriteNodesDetailsDataSQL);
         LDataSet.ClearQueryParams();
         LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
         LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
         LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
         LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
         LDataSet.SetParams(['Identifier'], [IntToStr(LCount)]);
         if LNode.FNodeNumberStorage.FInitalised then
         LDataSet.SetParams(['NodeNumberStorage'], [IntToStr(LNode.FNodeNumberStorage.FData)]);

         if LNode.FNodeNumberStorage.FData = 0 then
         Break;

         if LNode.FStatusIndicator.FInitalised then
           LDataSet.SetParams(['StatusIndicator'], [IntToStr(LNode.FStatusIndicator.FData)]);

         if LNode.FPlottingOption.FInitalised then
           LDataSet.SetParams(['PlottingOption'], [IntToStr(LNode.FPlottingOption.FData)]);

         if LNode.FReservoirPriority.FInitalised then
          LDataSet.SetParams(['ReservoirPriority'], [FloatToStr(LNode.FReservoirPriority.FData)]);

         if LNode.FFullSupplyLevel.FInitalised then
          LDataSet.SetParams(['FullSupplyLevel'], [FloatToStr(LNode.FFullSupplyLevel.FData)]);

         if LNode.FDeadStorageLevel.FInitalised then
          LDataSet.SetParams(['DeadStorageLevel'], [FloatToStr(LNode.FDeadStorageLevel.FData)]);

         if LNode.FBottomOfReservoir.FInitalised then
          LDataSet.SetParams(['BottomOfReservoir'], [FloatToStr(LNode.FBottomOfReservoir.FData)]);

         if LNode.FFullSupplyAllocation.FInitalised then
          LDataSet.SetParams(['FullSupplyAllocation'], [FloatToStr(LNode.FFullSupplyAllocation.FData)]);

         if LNode.FComment.FInitalised then
          LDataSet.SetParams(['NodesDetailsComment'], [LNode.FComment.FData]);

         LDataSet.ExecSQL;
      end;

      //Line 3
      LLevelIndex := 1;
      LReservoirNumber := -1;

      for LCount := 1 to LZoneDefinitions.FReservoirLevelsCount.FData do
      begin
        LReservoirLevel := TReservoirLevels(LZoneDefinitions.FReservoirLevels[LCount]);

        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteReservoirLevelsDataSQL);
        LDataSet.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['RecordIdentifier'], [IntToStr(LCount)]);


        if(LReservoirNumber = LReservoirLevel.FReservoirIdentifier.FData) then
          LLevelIndex := LLevelIndex + 1
        else
        begin
          LLevelIndex := 1;
          LReservoirNumber := LReservoirLevel.FReservoirIdentifier.FData
        end;
        LDataSet.SetParams(['LevelIdentifier'], [IntToStr(LLevelIndex)]);

        if LReservoirLevel.FReservoirIdentifier.FInitalised then
          LDataSet.SetParams(['ReservoirIdentifier'], [IntToStr(LReservoirLevel.FReservoirIdentifier.FData)]);

        for LLinesCount := MinReservoirMonths to MaxReservoirMonths do
        begin
          LFieldName := Format('%s%2.2d',['ReservoirLev',LLinesCount]);
          if LReservoirLevel.FResLevel[LLinesCount].FInitalised then
           LDataSet.SetParams([LFieldName], [FloatToStr(LReservoirLevel.FResLevel[LLinesCount].FData)]);
        end;
         if LReservoirLevel.FComment.FInitalised then
          LDataSet.SetParams(['ResLevelsComment'], [LReservoirLevel.FComment.FData]);
        LDataSet.ExecSQL;
      end;

      //line 4 onwards++++++++++++++++++++++++++++
      for LCounter := 0 to LZoneDefinitions.FF05ExtraLines.Count - 1 do
      begin
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteUnknownDataSQL);
        LDataSet.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['FileType'], [IntToStr(AFileName.FileNumber)]);
        LDataSet.SetParams(['FileGroup'], [IntToStr(AFileName.FileGroup)]);
        LDataSet.SetParams(['LineNumber'], [IntToStr(5 + LCounter)]);
        LDataSet.SetParams(['LineSection'], [IntToStr(0)]);
        LDataSet.SetParams(['LineData'], [LZoneDefinitions.FF05ExtraLines[LCounter]]);

        LDataSet.ExecSQL;
      end;
      LDataSet.DataSet.Close;

      Result := InsertFileName(AFileName);
      if Result then
      begin
        LMessage := FAppModules.Language.GetString('TFile05DatabaseAgent.strWriteEnded');
        AProgressFunction(LMessage,ptNone,LStop);
      end;
    finally
      LDataSet.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile05DatabaseAgent.ClearModelDataInDatabase(AFileName: TFileNameObject; AProgressFunction: TProgressUpdateFuntion;
         AQuetly: boolean = False): boolean;
const OPNAME = 'TFile05DatabaseAgent.ClearModelDataInDatabase';
var
  LTableNames,
  LMessage: string;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    if not AQuetly then
    begin
      LMessage := FAppModules.Language.GetString('TAbstractDatabaseAgent.strClearModelDataInDatabaseStarted');
      AProgressFunction(LMessage,ptNone,LStop);
    end;

    if not Assigned(AFileName) then
      raise Exception.Create('File name object parameter is not yet assigned.');

//    LTableNames := 'StorageZones,StorageZoneDetails,ReservoirZonePenalty,NodesDetails,ReservoirLevels';
    LTableNames := 'StorageZones,StorageZoneDetails,NodesDetails,ReservoirLevels';
    Result := DeleteModelData(LTableNames,'',AProgressFunction,AQuetly);
    Result := Result and DeleteUnknownModelData(AFileName,AProgressFunction,AQuetly);
    Result := Result and DeleteFileName(AFileName);

    if Result and (not AQuetly)then
    begin
      LMessage := FAppModules.Language.GetString('TAbstractDatabaseAgent.strClearModelDataInDatabaseEnded');
      AProgressFunction(LMessage,ptNone,LStop);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
