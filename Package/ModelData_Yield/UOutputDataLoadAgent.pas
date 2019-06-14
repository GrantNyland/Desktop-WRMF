//
//
//  UNIT      : Contains TOutputDataLoadAgent Class
//  AUTHOR    : Dziedzi Ramulondi (Aravia)
//  DATE      : 2005/05/19
//  COPYRIGHT : Copyright © 2005 DWAF
//
//
unit UOutputDataLoadAgent;

interface

uses
  Classes,
  UOutputData,
  UAbstractObject,
  UYieldModelDataObject,
  UOutputDataSQLAgent;

type
  TOutputDataLoadAgent = class(TAbstractAppObject)
  protected
    FSQLAgent: TOutputDataSQLAgent;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetElementID(ABlockNumber, ALoadCaseNumber,ASequenceNumber: Integer; var LElementID: integer): boolean;
    function LoadSummaryOutputDataFromSumOutFile(AOutputData:TOutputData;AYieldModelData: TYieldModelDataObject): boolean;
    function LoadLongtermSupplyDataFromSumOutFile(AOutputData:TOutputData;AYieldModelData: TYieldModelDataObject): boolean;
    //function LoadSummaryOutputDataFromDatabase(AOutputData:TOutputData;AYieldModelData: TYieldModelDataObject): boolean;
    //function LoadSummaryOutputDataFromBlobFile(AOutputData:TOutputData;AYieldModelData: TYieldModelDataObject): boolean;
    //function LoadSummaryOutputDataFromBlobDatabase(AOutputData:TOutputData;AYieldModelData: TYieldModelDataObject): boolean;
    function LoadSummaryOutputDataSources(AOutputData:TOutputData;AYieldModelData: TYieldModelDataObject): boolean;
    function LoadSummaryOutputDataFromPlotOutFile(AOutputData:TOutputData;AYieldModelData: TYieldModelDataObject): boolean;
    function Get_MasterControlChannelNumber(AYieldModelData: TYieldModelDataObject;ABlockTitle:string) : integer;
  public
    function ConstructPlotOutputData(AOutputData:TOutputData;AYieldModelData: TYieldModelDataObject): boolean;
    function ConstructSummaryOutputData(AOutputData:TOutputData;AYieldModelData: TYieldModelDataObject;
             ADataSource:TSummaryOutputDataSourceName): boolean;
    function ConstructData(AOutputData:TOutputData;AYieldModelData: TYieldModelDataObject): boolean;
  end;

implementation

uses
  SysUtils,
  UFileNames,
  Uconstants,
  UPlotFileManager,
  UDataSetType,
  UUtilities,
  VoaimsCom_TLB,
  USumOutFileManager,
  USumOutDataObjects,
  UAbstractFileNamesObject,
  UErrorHandlingOperations;

procedure TOutputDataLoadAgent.CreateMemberObjects;
const OPNAME = 'TOutputDataLoadAgent.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FSQLAgent := TOutputDataSQLAgent.Create(FAppModules);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TOutputDataLoadAgent.DestroyMemberObjects;
const OPNAME = 'TOutputDataLoadAgent.DestroyMemberObjects';
begin
  try
    FreeAndNil(FSQLAgent);
    inherited DestroyMemberObjects;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TOutputDataLoadAgent.ConstructData(AOutputData:TOutputData;AYieldModelData: TYieldModelDataObject): boolean;
const OPNAME = 'TOutputDataLoadAgent.ConstructData';
begin
  Result := False;
  try
    AOutputData.Initialise;
    Result := ConstructPlotOutputData(AOutputData,AYieldModelData);
    Result := Result and ConstructSummaryOutputData(AOutputData,AYieldModelData,AOutputData.CastSummaryOutputData.DataSources.CurrentSource);
  // Handle exceptions.
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TOutputDataLoadAgent.GetElementID(ABlockNumber, ALoadCaseNumber,ASequenceNumber: integer; var LElementID: integer): boolean;
const OPNAME = 'TOutputDataLoadAgent.GetElementID';
var
  LDataSet: TAbstractModelDataset;
  LLine: string;
  //LPos: integer;
begin
  Result := False;
  LElementID    := NullInteger;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(FSQLAgent.GetBlockHeaderSQL(ABlockNumber,ALoadCaseNumber,ASequenceNumber));
        LDataSet.DataSet.Open;
        if not LDataSet.DataSet.Eof then
        begin
          {LLine := UpperCase(Trim(LDataSet.DataSet.FieldByName('LineData').AsString));
          LPos  := Pos('LOAD CASE',LLine);
          if(LPos = 1) then
          begin
            LLine := Trim(Copy(LLine,14,2));
            ALoadCase2 := StrToIntDef(LLine,NullInteger);
          end;}
          LDataSet.DataSet.Next;
          if not LDataSet.DataSet.Eof then
          begin
            LLine := Trim(LDataSet.DataSet.FieldByName('LineData').AsString);
            LLine := Trim(Copy(LLine,92,4));
            LElementID := StrToIntDef(LLine,NullInteger);
          end;
          Result := True;
        end;
        LDataSet.DataSet.Close;
      end;
    finally
      LDataSet.Free;
    end;
  // Handle exceptions.
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TOutputDataLoadAgent.ConstructPlotOutputData(AOutputData: TOutputData;AYieldModelData: TYieldModelDataObject): boolean;
const OPNAME = 'TOutputDataLoadAgent.ConstructPlotOutputData';
var
  LDataSet: TAbstractModelDataset;
  LElementID         : integer;
  LPltElementType    : integer;
  LElementType       : TNetworkElementType;
  LElementDataType   : TOutputDataType;
  LElementName       : string;
  LMasterControlFeature : IMasterControlFeature;
begin
  Result := False;
  try
    AOutputData.CastPlottingOutputData.Initialise;
    if not (LoadSummaryOutputDataFromPlotOutFile(AOutputData,AYieldModelData)) then
    begin
      // Loop for all the records in the table.
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      try

        if Assigned(LDataSet) then
        begin
          LDataSet.SetSQL(FSQLAgent.GetPlotElemetSQL);
          LDataSet.DataSet.Open;

          while not LDataSet.DataSet.Eof do
          begin
            LElementID         := LDataSet.DataSet.FieldByName('ElementNumber').AsInteger;
            LPltElementType    := LDataSet.DataSet.FieldByName('ElementType').AsInteger;
            LElementName       := Trim(LDataSet.DataSet.FieldByName('ElementName').AsString);
            LElementDataType   := btNone;
            LElementType       := votNone;
            case LPltElementType of
              1:begin
                  LElementDataType := btMonthEndReservoirVolume;
                  LElementType     := votReservoir;
                end;
              2:begin
                  LElementDataType := btMonthlyAverageChannelFlow;
                  LElementType     := votChannel;
                end;
              3:begin
                  LElementDataType := btMonthlyAverageChannelFlow;
                  LElementType     := votMasterControl;
                end;
              4:begin
                  LElementDataType := btMonthEndReservoirVolume;
                end;
            end;

            if(LElementType = votMasterControl) then
            begin
              LMasterControlFeature := AYieldModelData.NetworkFeaturesData.MasterControlFeatureList.MasterControlFeatureByIndex[0];
              if(LMasterControlFeature <> nil) and (LMasterControlFeature.Channel <> nil)then
                LElementID := LMasterControlFeature.Channel.ChannelNumber;
            end;
            AOutputData.CastPlottingOutputData.Populate(LElementType,LElementDataType,LElementID,LElementName);
            LDataSet.DataSet.Next;
          end;
          LDataSet.DataSet.Close;
        end;


      finally
        LDataSet.Free;
      end;
    end;
    Result := True;


  // Handle exceptions.
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TOutputDataLoadAgent.ConstructSummaryOutputData(AOutputData: TOutputData;
         AYieldModelData: TYieldModelDataObject;ADataSource:TSummaryOutputDataSourceName): boolean;
const OPNAME = 'TOutputDataLoadAgent.ConstructSummaryOutputData';
begin
  Result := False;
  try
    AOutputData.CastSummaryOutputData.Initialise;
    if LoadSummaryOutputDataSources(AOutputData,AYieldModelData) then
    begin
      if not AOutputData.CastSummaryOutputData.DataSources.DataSourceAvailable[ADataSource] then
      begin
        if(AOutputData.CastSummaryOutputData.DataSources.DataSourcesCount > 1) then
          AOutputData.CastSummaryOutputData.DataSources.CurrentSource :=
          AOutputData.CastSummaryOutputData.DataSources.DataSourceByIndex[1];
          ADataSource := AOutputData.CastSummaryOutputData.DataSources.CurrentSource;
      end;

      case ADataSource of
        sodsSumFile:      LoadSummaryOutputDataFromSumOutFile(AOutputData,AYieldModelData);
        //sodsDatabase:     LoadSummaryOutputDataFromDatabase(AOutputData,AYieldModelData);
        //sodsBlobFile:     LoadSummaryOutputDataFromBlobFile(AOutputData,AYieldModelData);
        //sodsBlobDatabase: LoadSummaryOutputDataFromBlobDatabase(AOutputData,AYieldModelData);
        sodsPltFile:      LoadSummaryOutputDataFromPlotOutFile(AOutputData,AYieldModelData);
        else
        begin
          {if AOutputData.CastSummaryOutputData.DataSources.DataSourceAvailable[sodsBlobFile] then
             LoadSummaryOutputDataFromBlobFile(AOutputData,AYieldModelData)
          else
          if AOutputData.CastSummaryOutputData.DataSources.DataSourceAvailable[sodsBlobDatabase] then
             LoadSummaryOutputDataFromBlobDatabase(AOutputData,AYieldModelData)
          else}
          if AOutputData.CastSummaryOutputData.DataSources.DataSourceAvailable[sodsSumFile] then
             LoadSummaryOutputDataFromSumOutFile(AOutputData,AYieldModelData)
          else
          {if AOutputData.CastSummaryOutputData.DataSources.DataSourceAvailable[sodsDatabase] then
             LoadSummaryOutputDataFromDatabase(AOutputData,AYieldModelData)
          else}
          if AOutputData.CastSummaryOutputData.DataSources.DataSourceAvailable[sodsPltFile] then
             LoadSummaryOutputDataFromPlotOutFile(AOutputData,AYieldModelData);
        end;
      end;
    end;
    Result := True;
    // Handle exceptions.
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TOutputDataLoadAgent.LoadSummaryOutputDataSources(AOutputData: TOutputData;
         AYieldModelData: TYieldModelDataObject): boolean;
const OPNAME = 'TOutputDataLoadAgent.LoadSummaryOutputDataSources';
var
  LFileNameObject    : TAbstractModelFileName;
  LModelFileName     : TModelFileNames;
  LFileName          : string;
begin
  Result := False;
  try
    AOutputData.CastSummaryOutputData.DataSources.Initialise;
    if(FAppModules.Model.ModelName = CPlanning) then
    begin
      AOutputData.CastSummaryOutputData.DataSources.AddSource(sodsSumFile);
    end
    else
    begin
      LModelFileName     := AYieldModelData.CastFileNamesObject;
      LFileNameObject    := LModelFileName.GetSumOutFile;
      if(LFileNameObject <> nil) then
      begin
       LFileName := LFileNameObject.FileName;
       if FileExists(LFileName) then
         AOutputData.CastSummaryOutputData.DataSources.AddSource(sodsSumFile);

        {LFileName := ChangeFileExt(LFileName, '.blb');
        if FileExists(LFileName) then
         AOutputData.CastSummaryOutputData.DataSources.AddSource(sodsBlobFile);}
      end;

      LFileNameObject    := LModelFileName.GetPlotOutFile;
      if(LFileNameObject <> nil) then
      begin
       LFileName := LFileNameObject.FileName;
       if FileExists(LFileName) then
         AOutputData.CastSummaryOutputData.DataSources.AddSource(sodsPltFile);
      end;

      {FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      try
        if Assigned(LDataSet) then
        begin
          LDataSet.SetSQL(FSQLAgent.GetSumOutBlobSQL);
          LDataSet.DataSet.Open;
          if not (LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
           AOutputData.CastSummaryOutputData.DataSources.AddSource(sodsBlobDatabase);
          LDataSet.DataSet.Close;

          LDataSet.SetSQL(FSQLAgent.GetGenericBlockDescriptionSQL);
          LDataSet.DataSet.Open;
          if not (LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
           AOutputData.CastSummaryOutputData.DataSources.AddSource(sodsDatabase);
          LDataSet.DataSet.Close;
        end;
      finally
        LDataSet.Free;
      end;}
    end;
    Result := True;
    // Handle exceptions.
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

{function TOutputDataLoadAgent.LoadSummaryOutputDataFromBlobDatabase( AOutputData: TOutputData;
         AYieldModelData: TYieldModelDataObject): boolean;
const OPNAME = 'TOutputDataLoadAgent.LoadSummaryOutputDataFromBlobDatabase';
var
  LDataSet           : TAbstractModelDataset;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(FSQLAgent.GetSumOutBlobSQL);
        LDataSet.DataSet.Open;
        if not (LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
        begin
          LDataSet.DataSet.Close;
          if AOutputData.CastSummaryOutputData.PopulateFromDataBaseBlob then
            AOutputData.CastSummaryOutputData.DataSources.CurrentSource := sodsBlobDatabase;
        end;
      end;
      Result := True;
    finally
      LDataSet.Free;
    end;
    // Handle exceptions.
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;}


{function TOutputDataLoadAgent.LoadSummaryOutputDataFromDatabase(AOutputData: TOutputData;
         AYieldModelData: TYieldModelDataObject): boolean;
const OPNAME = 'TOutputDataLoadAgent.LoadSummaryOutputDataFromDatabase';
var
  LDataSet           : TAbstractModelDataset;
  LElementID         : integer;
  LBlockType         : integer;
  LBlockNumber       : integer;
  LLoadCaseNumber    : integer;
  LSequenceNumber    : integer;
  LAnnualWaterDemand : double;
  LAnnualPowerDemand : double;
  LBlockHeading      : string;
  LBlockTitle        : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(FSQLAgent.GetGenericBlockDescriptionSQL);
        LDataSet.DataSet.Open;
        while not LDataSet.DataSet.Eof do
        begin
          LBlockType         := LDataSet.DataSet.FieldByName('BlockType').AsInteger;
          LBlockNumber       := LDataSet.DataSet.FieldByName('BlockNumber').AsInteger;
          LLoadCaseNumber    := LDataSet.DataSet.FieldByName('LoadCaseNumber').AsInteger;
          LSequenceNumber    := LDataSet.DataSet.FieldByName('SequenceNumber').AsInteger;
          LElementID         := NullInteger;
          LAnnualWaterDemand := LDataSet.DataSet.FieldByName('AnnualWaterDemand').AsFloat;
          LAnnualPowerDemand := LDataSet.DataSet.FieldByName('AnnualPowerDemand').AsFloat;
          LBlockHeading      := Trim(LDataSet.DataSet.FieldByName('BlockHeading').AsString);
          LBlockTitle        := Trim(LDataSet.DataSet.FieldByName('BlockTitle').AsString);
          if not LDataSet.DataSet.FieldByName('ElementID').IsNull then
            LElementID    := LDataSet.DataSet.FieldByName('ElementID').AsInteger;

          if(LElementID = NullInteger) then
          begin
            if GetElementID(LBlockNumber,LLoadCaseNumber,LSequenceNumber,LElementID) then
            begin
              if(LElementID = NullInteger) then
              begin
                LElementID := Get_MasterControlChannelNumber(AYieldModelData,LBlockTitle);
              end;
            end;
          end;

          if(LElementID <> NullInteger) then
          begin
            AOutputData.CastSummaryOutputData.PopulateElement(TOutputDataType(LBlockType),LElementID,LBlockNumber,
            LLoadCaseNumber,LSequenceNumber,LAnnualWaterDemand,LAnnualPowerDemand,LBlockHeading,LBlockTitle,nil);
          end;

          LDataSet.DataSet.Next;
        end;
        if not (LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
          AOutputData.CastSummaryOutputData.DataSources.CurrentSource := sodsDatabase;
        LDataSet.DataSet.Close;
      end;
      Result := True;
    finally
      LDataSet.Free;
    end;

    // Handle exceptions.
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;}

function TOutputDataLoadAgent.Get_MasterControlChannelNumber(AYieldModelData: TYieldModelDataObject;ABlockTitle: string): integer;
const OPNAME = 'TOutputDataLoadAgent.Get_MasterControlChannelNumber';
var
  lFeature : IMasterControlFeature;
  LIndex   : integer;
begin
  Result := NullInteger;
  try
    ABlockTitle := Trim(UpperCase(ABlockTitle));
    if(ABlockTitle = '') then Exit;

    for LIndex := 0 to AYieldModelData.NetworkFeaturesData.MasterControlFeatureList.MasterControlFeatureCount-1 do
    begin
      lFeature := AYieldModelData.NetworkFeaturesData.MasterControlFeatureList.MasterControlFeatureByIndex[LIndex];
      if(lFeature.Channel <> nil) then
      begin
        if(lFeature.MasterControlType = 'W') and (Pos('WATER YIELD SUMMARY',ABlockTitle) = 1) then
        begin
          Result := lFeature.Channel.ChannelNumber;
          Break;
        end;
        if(lFeature.MasterControlType = 'P') and (Pos('TOTAL INTEGRATED POWER SYSTEM',ABlockTitle) = 1) then
        begin
          Result := lFeature.Channel.ChannelNumber;
          Break;
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


{function TOutputDataLoadAgent.LoadSummaryOutputDataFromBlobFile(AOutputData: TOutputData;
         AYieldModelData: TYieldModelDataObject): boolean;
const OPNAME = 'TOutputDataLoadAgent.LoadSummaryOutputDataFromBlobFile';
var
  LFileNameObject    : TAbstractModelFileName;
  LModelFileName     : TModelFileNames;
  LFileName          : string;
begin
  Result := False;
  try
    LModelFileName     := AYieldModelData.CastFileNamesObject;
    LFileNameObject    := LModelFileName.GetSumOutFile;
    if(LFileNameObject <> nil) then
    begin
     LFileName := LFileNameObject.FileName;
      LFileName := ChangeFileExt(LFileName, '.blb');
      if FileExists(LFileName) then
      begin
        if AOutputData.CastSummaryOutputData.PopulateFromFileBlob then
          AOutputData.CastSummaryOutputData.DataSources.CurrentSource := sodsBlobFile;
      end;
    end;
    Result := True;
    // Handle exceptions.
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;}

function TOutputDataLoadAgent.LoadSummaryOutputDataFromSumOutFile(AOutputData: TOutputData;
         AYieldModelData: TYieldModelDataObject): boolean;
const OPNAME = 'TOutputDataLoadAgent.LoadSummaryOutputDataFromSumOutFile';
var
  LFileNameObject    : TAbstractModelFileName;
  LSumOutFileManager : TSumOutFileManager;
  LModelFileName     : TModelFileNames;
  LFileName          : string;
begin
  Result := False;
  try
    LModelFileName     := AYieldModelData.CastFileNamesObject;
    LFileNameObject    := LModelFileName.GetSumOutFile;
    if(LFileNameObject <> nil) then
    begin
     LFileName := LFileNameObject.FileName;
      if FileExists(LFileName) then
      begin
        LSumOutFileManager := TSumOutFileManager.Create(FAppModules);
        try
          if LSumOutFileManager.PopulateSumOutData(LFileNameObject,AOutputData,AYieldModelData) then
          begin
            AOutputData.CastSummaryOutputData.DataSources.CurrentSource := sodsSumFile;
            LoadLongtermSupplyDataFromSumOutFile(AOutputData,AYieldModelData);
          end;
        finally
          LSumOutFileManager.Free;
        end;
      end;
    end;
    Result := True;
    // Handle exceptions.
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TOutputDataLoadAgent.LoadLongtermSupplyDataFromSumOutFile(AOutputData:TOutputData;AYieldModelData: TYieldModelDataObject): boolean;
const OPNAME = 'TOutputDataLoadAgent.LoadSummaryOutputDataFromPlotOutFile';
var
  LModel,
  LStudyAreaName,
  LSubArea,
  LScenario,
  LSelectedData : string;
begin
  Result := False;
  try
    LSelectedData := FAppModules.ViewIni.ReadString(ClassName,'SelectedValues','');
    LModel := FAppModules.ViewIni.ReadString(ClassName,'Model','');
    LStudyAreaName := FAppModules.ViewIni.ReadString(ClassName,'StudyAreaName','');
    LSubArea := FAppModules.ViewIni.ReadString(ClassName,'SubArea','');
    LScenario := FAppModules.ViewIni.ReadString(ClassName,'Scenario','');
    if (FAppModules.StudyArea.ModelCode = LModel) and (FAppModules.StudyArea.StudyAreaCode = LStudyAreaName) and
       (FAppModules.StudyArea.SubAreaCode= LSubArea) and
       (FAppModules.StudyArea.ScenarioCode = LScenario) then
      AOutputData.CastLongtermSupplyData.SelectedChannels := LSelectedData;
    Result := AOutputData.CastLongtermSupplyData.Initialise;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;


function TOutputDataLoadAgent.LoadSummaryOutputDataFromPlotOutFile(AOutputData: TOutputData;AYieldModelData: TYieldModelDataObject): boolean;
const OPNAME = 'TOutputDataLoadAgent.LoadSummaryOutputDataFromPlotOutFile';
var
  LFileNameObject    : TAbstractModelFileName;
  LModelFileName     : TModelFileNames;
  LFileName          : string;
  LPlotFileManager   : TPlotFileManager;
begin
  Result := False;
  try
    LModelFileName     := AYieldModelData.CastFileNamesObject;
    LFileNameObject    := LModelFileName.GetPlotOutFile;
    if(LFileNameObject <> nil) then
    begin
      LFileName := LFileNameObject.FileName;
      if FileExists(LFileName) then
      begin
        LPlotFileManager := TPlotFileManager.Create(FAppModules);
        try
          if LPlotFileManager.PopulatePlotOutData(LFileNameObject,AOutputData,AYieldModelData,nil) then
            AOutputData.CastSummaryOutputData.DataSources.CurrentSource := sodsPltFile;

        finally
          LPlotFileManager.Free;
        end;
      end;
    end;
    Result := True;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

end.
