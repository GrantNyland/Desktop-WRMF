//
//
//  UNIT      : Contains TFile02DatabaseAgent Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 21/01/2002
//  COPYRIGHT : Copyright © 2001 DWAF
//
//
unit UFile02DatabaseAgent;

interface

uses
  Classes, sysutils,Db,

  //  DWAF VCL
  VoaimsCom_TLB,
  UFileNames,
  UConstants,
  UDatabaseUtilities,
  UAbstractObject,
  UDataFileObjects,
  UAbstractFileNamesObject,
  UReservoirObject,
  UAbstractDatabaseAgent,
  UYieldModelDataObject;

type

  TFile02DatabaseAgent = class(TAbstractDatabaseAgent)
  protected
    function ReadF02KnownDataSQL: string;
    function ReadF02UnkownDataSQL: string;

    function WriteReservoirDataSQL: string;
    function WriteReservoirAreaDataSQL: string;
    function WriteReservoirChannelsDataSQL: string;
    function WriteReservoirDetailsDataSQL: string;
    function WriteReservoirElevationDataSQL: string;
    function WriteReservoirEvapDataSQL: string;
    function WriteReservoirVolumeDataSQL: string;
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
  UErrorHandlingOperations;

function TFile02DatabaseAgent.ReadF02UnkownDataSQL: string;
const OPNAME = 'TFile02DatabaseAgent.ReadF02UnkownDataSQL';
begin

  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,LineNumber,LineData,FileType'+
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

function TFile02DatabaseAgent.ReadF02KnownDataSQL: string;
const OPNAME = 'TFile02DatabaseAgent.ReadF02KnownDataSQL';
begin

  Result := '';
  try
    Result := 'SELECT Res.Model,Res.StudyAreaName,Res.SubArea,Res.Scenario'+
      ' ,Res.ReservoirCount,Res.HydroUnitsCode,Res.ReservoirComment'+
      ' ,Det.Identifier,Det.ReservoirName,Det.IncludeSummary,Det.NodeCount,Det.PenaltyStruct'+
      ' ,Det.PointsCount,Det.DrainageScale,Det.AfforestationScale,Det.IrrigationScale'+
      ' ,Det.AreaFull,Det.RainCoef,Det.CatchmentRef,Det.ChannelsCount,Det.ReservoirDetailsComment'+
      ' ,Det.Comment02,Det.Comment03,Det.Comment04,Det.NodeType,Det.UrbanRunoff,Det.NaturalInflowChannel'+
      ' ,Area.Area01,Area.Area02,Area.Area03,Area.Area04,Area.Area05,Area.Area06'+
      ' ,Area.Area07,Area.Area08,Area.Area09,Area.Area10,Area.Area11,Area.Area12'+
      ' ,Area.Area13,Area.Area14,Area.Area15'+
      ' ,Cha.Channel01,Cha.Channel02,Cha.Channel03,Cha.Channel04,Cha.Channel05,Cha.Channel06'+
      ' ,Cha.Channel07,Cha.Channel08,Cha.Channel09,Cha.Channel10,Cha.Channel11,Cha.Channel12'+
      ' ,Cha.Channel13,Cha.Channel14,Cha.Channel15,Cha.Channel16,Cha.Channel17,Cha.Channel18'+
      ' ,Cha.Channel19,Cha.Channel20'+
      ' ,Elev.ReservoirElev01,Elev.ReservoirElev02,Elev.ReservoirElev03,Elev.ReservoirElev04,Elev.ReservoirElev05,Elev.ReservoirElev06,Elev.ReservoirElev07'+
      ' ,Elev.ReservoirElev08,Elev.ReservoirElev09,Elev.ReservoirElev10,Elev.ReservoirElev11,Elev.ReservoirElev12,Elev.ReservoirElev13,Elev.ReservoirElev14'+
      ' ,Elev.ReservoirElev15'+
      ' ,Evap.Evapo01,Evap.Evapo02,Evap.Evapo03,Evap.Evapo04,Evap.Evapo05,Evap.Evapo06'+
      ' ,Evap.Evapo07,Evap.Evapo08,Evap.Evapo09,Evap.Evapo10,Evap.Evapo11,Evap.Evapo12'+
      ' ,Vol.Volume01,Vol.Volume02,Vol.Volume03,Vol.Volume04,Vol.Volume05,Vol.Volume06'+
      ' ,Vol.Volume07,Vol.Volume08,Vol.Volume09,Vol.Volume10,Vol.Volume11,Vol.Volume12'+
      ' ,Vol.Volume13,Vol.Volume14,Vol.Volume15'+
      ' FROM '+
      '  Reservoir Res,'+
      '  ReservoirDetails Det, '+
      '  ReservoirArea Area,    '+
      '  ReservoirChannels Cha,'+
      '  ReservoirElevation Elev,'+
      '  ReservoirEvap Evap,'+
      '  ReservoirVolume Vol'+
      ' WHERE Res.Model       =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
      ' AND Res.StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
      ' AND Res.SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
      ' AND Res.Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+

      ' AND Det.Model         = Res.Model'+
      ' AND Det.StudyAreaName = Res.StudyAreaName'+
      ' AND Det.SubArea       = Res.SubArea'+
      ' AND Det.Scenario      = Res.Scenario'+

      ' AND Area.Model         = Res.Model'+
      ' AND Area.StudyAreaName = Res.StudyAreaName'+
      ' AND Area.SubArea       = Res.SubArea'+
      ' AND Area.Scenario      = Res.Scenario'+
      ' AND Area.Identifier    = Det.Identifier'+

      ' AND Cha.Model         = Res.Model'+
      ' AND Cha.StudyAreaName = Res.StudyAreaName'+
      ' AND Cha.SubArea       = Res.SubArea'+
      ' AND Cha.Scenario      = Res.Scenario'+
      ' AND Cha.Identifier    = Det.Identifier'+


      ' AND Elev.Model         = Res.Model'+
      ' AND Elev.StudyAreaName = Res.StudyAreaName'+
      ' AND Elev.SubArea       = Res.SubArea'+
      ' AND Elev.Scenario      = Res.Scenario'+
      ' AND Elev.Identifier    = Det.Identifier'+

      ' AND Evap.Model         = Res.Model'+
      ' AND Evap.StudyAreaName = Res.StudyAreaName'+
      ' AND Evap.SubArea       = Res.SubArea'+
      ' AND Evap.Scenario      = Res.Scenario'+
      ' AND Evap.Identifier    = Det.Identifier'+

      ' AND Vol.Model         = Res.Model'+
      ' AND Vol.StudyAreaName = Res.StudyAreaName'+
      ' AND Vol.SubArea       = Res.SubArea'+
      ' AND Vol.Scenario      = Res.Scenario'+
      ' AND Vol.Identifier    = Det.Identifier'+

      ' ORDER BY Res.Model,Res.StudyAreaName,Res.SubArea,Res.Scenario,Det.NodeType,Det.Identifier';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile02DatabaseAgent.WriteReservoirDataSQL: string;
const OPNAME = 'TFile02DatabaseAgent.WriteReservoirDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO Reservoir'+
              ' (Model,StudyAreaName,SubArea,Scenario,ReservoirCount,HydroUnitsCode,ReservoirComment)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:ReservoirCount,:HydroUnitsCode,:ReservoirComment)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile02DatabaseAgent.WriteReservoirAreaDataSQL: string;
const OPNAME = 'TFile02DatabaseAgent.WriteReservoirAreaDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO ReservoirArea'+
              ' (Model,StudyAreaName,SubArea,Scenario,Identifier,Area01,Area02,Area03,Area04,Area05'+
              ' ,Area06,Area07,Area08,Area09,Area10,Area11,Area12,Area13,Area14,Area15)'+
              ' Values '+
              ' (:Model,:StudyAreaName,:SubArea,:Scenario,:Identifier,:Area01,:Area02,:Area03,:Area04,:Area05'+
              ' ,:Area06,:Area07,:Area08,:Area09,:Area10,:Area11,:Area12,:Area13,:Area14,:Area15)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile02DatabaseAgent.WriteReservoirChannelsDataSQL: string;
const OPNAME = 'TFile02DatabaseAgent.WriteReservoirChannelsDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO ReservoirChannels'+
              ' (Model,StudyAreaName,SubArea,Scenario,Identifier,Channel01,Channel02,Channel03,Channel04'+
              ' ,Channel05,Channel06,Channel07,Channel08,Channel09,Channel10,Channel11,Channel12'+
              ' ,Channel13,Channel14,Channel15,Channel16,Channel17,Channel18,Channel19,Channel20)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:Identifier,:Channel01,:Channel02,:Channel03,:Channel04'+
              ' ,:Channel05,:Channel06,:Channel07,:Channel08,:Channel09,:Channel10,:Channel11,:Channel12'+
              ' ,:Channel13,:Channel14,:Channel15,:Channel16,:Channel17,:Channel18,:Channel19,:Channel20)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile02DatabaseAgent.WriteReservoirDetailsDataSQL: string;
const OPNAME = 'TFile02DatabaseAgent.WriteReservoirDetailsDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO ReservoirDetails'+
              ' (Model,StudyAreaName,SubArea,Scenario,Identifier,ReservoirName,IncludeSummary,NodeCount,PenaltyStruct,'+
              ' PointsCount,DrainageScale,AfforestationScale,IrrigationScale,AreaFull,RainCoef,CatchmentRef,'+
              ' ChannelsCount,ReservoirDetailsComment,Comment02,Comment03,Comment04,NodeType,UrbanRunoff,NaturalInflowChannel)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:Identifier,:ReservoirName,:IncludeSummary, :NodeCount,:PenaltyStruct'+
              ' ,:PointsCount,:DrainageScale,:AfforestationScale,:IrrigationScale,:AreaFull,:RainCoef'+
              ' ,:CatchmentRef,:ChannelsCount,:ReservoirDetailsComment,:Comment02,:Comment03,:Comment04,:NodeType,:UrbanRunoff,:NaturalInflowChannel)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile02DatabaseAgent.WriteReservoirElevationDataSQL: string;
const OPNAME = 'TFile02DatabaseAgent.WriteReservoirElevationDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO ReservoirElevation'+
              ' (Model,StudyAreaName,SubArea,Scenario,Identifier,ReservoirElev01,ReservoirElev02,ReservoirElev03,ReservoirElev04,ReservoirElev05,ReservoirElev06'+
              ' ,ReservoirElev07,ReservoirElev08,ReservoirElev09,ReservoirElev10,ReservoirElev11,ReservoirElev12,ReservoirElev13,ReservoirElev14,ReservoirElev15)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:Identifier,:ReservoirElev01,:ReservoirElev02,:ReservoirElev03,:ReservoirElev04'+
              ' ,:ReservoirElev05,:ReservoirElev06,:ReservoirElev07,:ReservoirElev08,:ReservoirElev09,:ReservoirElev10,:ReservoirElev11,:ReservoirElev12,:ReservoirElev13,:ReservoirElev14,:ReservoirElev15)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile02DatabaseAgent.WriteReservoirEvapDataSQL: string;
const OPNAME = 'TFile02DatabaseAgent.WriteReservoirEvapDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO ReservoirEvap'+
              ' (Model,StudyAreaName,SubArea,Scenario,Identifier,Evapo01,Evapo02,Evapo03,Evapo04,Evapo05,Evapo06'+
              ' ,Evapo07,Evapo08,Evapo09,Evapo10,Evapo11,Evapo12)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:Identifier,:Evapo01,:Evapo02,:Evapo03,:Evapo04,:Evapo05'+
              ' ,:Evapo06,:Evapo07,:Evapo08,:Evapo09,:Evapo10,:Evapo11,:Evapo12)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile02DatabaseAgent.WriteReservoirVolumeDataSQL: string;
const OPNAME = 'TFile02DatabaseAgent.WriteReservoirVolumeDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO ReservoirVolume'+
              ' (Model,StudyAreaName,SubArea,Scenario,Identifier,Volume01,Volume02,Volume03,Volume04,Volume05'+
              ' ,Volume06,Volume07,Volume08,Volume09,Volume10,Volume11,Volume12,Volume13,Volume14,Volume15)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:Identifier,:Volume01,:Volume02,:Volume03,:Volume04,:Volume05'+
              ' ,:Volume06,:Volume07,:Volume08,:Volume09,:Volume10,:Volume11,:Volume12,:Volume13'+
              ' ,:Volume14,:Volume15)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile02DatabaseAgent.WriteUnknownDataSQL: string;
const OPNAME = 'TFile02DatabaseAgent.WriteUnknownDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO WRYMFileLines'+
              ' (Model,StudyAreaName,SubArea,Scenario,FileType,FileGroup,LineNumber,LineSection,LineData)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:FileType,:FileGroup,:LineNumber,:LineSection,:LineData)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile02DatabaseAgent.ReadModelDataFromDatabase(AFileName:TFileNameObject;ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile02DatabaseAgent.ReadModelDataFromDatabase';
var
  LMessage : string;
  LDataSet : TAbstractModelDataset;
  LNoData  : Boolean;
  LReservoirList: TReservoirObject;
  LReservoir : TReservoir;
  LNodeTypeCount: integer;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile02DatabaseAgent.strReadStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    LReservoirList := ADataObject.FReservoirObject;

    if not LReservoirList.Initialise then
    Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try

      LDataSet.SetSQL(ReadF02KnownDataSQL);
      LDataSet.DataSet.Open;

      //Check if there is any data.
      LNoData := False;
      if (LDataSet.DataSet.RecordCount = 0) then
      begin
        LNoData := True;
        LMessage := FAppModules.Language.GetString('TFile02DatabaseAgent.strNoDataReturned');
        AProgressFunction(LMessage,ptWarning,LStop);
        //Exit;
      end;

      // go to the last record if there is more than one record.
      //LQuery.Last;

      if (LDataSet.DataSet.RecordCount > 0) then
      begin
        //Read the F02 file
        //Title Line 1
        if not LDataSet.DataSet.FieldByName('ReservoirCount').IsNull then
        begin
          LReservoirList.FReserviorNum.FData := LDataSet.DataSet.FieldByName('ReservoirCount').AsInteger;
          LReservoirList.FReserviorNum.FInitalised := True;
          LReservoirList.FReserviorAndNodesNum.FData := LDataSet.DataSet.FieldByName('ReservoirCount').AsInteger;
          LReservoirList.FReserviorAndNodesNum.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('HydroUnitsCode').IsNull then
        begin
          LReservoirList.FHydroUnits.FData := Trim(LDataSet.DataSet.FieldByName('HydroUnitsCode').AsString);
          LReservoirList.FHydroUnits.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('ReservoirComment').IsNull then
        begin
          LReservoirList.FComment.FData := TrimRight(LDataSet.DataSet.FieldByName('ReservoirComment').AsString);
          LReservoirList.FComment.FLength := Length(LReservoirList.FComment.FData);
          LReservoirList.FComment.FInitalised := True;
        end;

        for LNodeTypeCount := 1 to 2 do
        begin
          LDataSet.DataSet.First;
          while not LDataSet.DataSet.Eof do
          begin
            if(LNodeTypeCount = 1) then
            begin
              if not(LDataSet.DataSet.FieldByName('NodeType').AsInteger in ReservoirsSet) then
              begin
                LDataSet.DataSet.Next;
                Continue;
              end;
            end
            else
            begin
              if (LDataSet.DataSet.FieldByName('NodeType').AsInteger in ReservoirsSet) then
              begin
                LDataSet.DataSet.Next;
                Continue;
              end;
            end;

            LReservoir := LReservoirList.AddReservoir;
            if(LReservoir = nil) then Break;

             //Line 2 : Resevior name
            if not LDataSet.DataSet.FieldByName('ReservoirName').IsNull then
            begin
              LReservoir.FName.FData := Trim(LDataSet.DataSet.FieldByName('ReservoirName').AsString);
              LReservoir.FName.FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('IncludeSummary').IsNull then
            begin
              LReservoir.FSummaryInclude.FData :=
                (Trim(LDataSet.DataSet.FieldByName('IncludeSummary').AsString)+' ')[1];
              LReservoir.FSummaryInclude.FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('NodeCount').IsNull then
            begin
              LReservoir.FNodeNo.FData :=
                LDataSet.DataSet.FieldByName('NodeCount').AsInteger;
              LReservoir.FNodeNo.FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('PenaltyStruct').IsNull then
            begin
              LReservoir.FPenaltyStructure.FData :=
                LDataSet.DataSet.FieldByName('PenaltyStruct').AsInteger;
              LReservoir.FPenaltyStructure.FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('PointsCount').IsNull then
            begin
              LReservoir.FMaxPoints.FData :=
                LDataSet.DataSet.FieldByName('PointsCount').AsInteger;
              LReservoir.FMaxPoints.FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('NodeType').IsNull then
            begin
              LReservoir.FNodeType.FData :=
                LDataSet.DataSet.FieldByName('NodeType').AsInteger;
              LReservoir.FNodeType.FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('DrainageScale').IsNull then
            begin
              LReservoir.FDrainScale.FData :=
                LDataSet.DataSet.FieldByName('DrainageScale').AsFloat;
              LReservoir.FDrainScale.FInitalised := True;
            end;

            if(FAppModules.Model.ModelName = CPlanning) then
            begin
              if not LDataSet.DataSet.FieldByName('UrbanRunoff').IsNull then
              begin
                LReservoir.FUrbanRunoff.FData :=
                  LDataSet.DataSet.FieldByName('UrbanRunoff').AsFloat;
                LReservoir.FUrbanRunoff.FInitalised := True;
              end
              else
              if (LDataSet.DataSet.FieldByName('UrbanRunoff').IsNull) and
                (not LDataSet.DataSet.FieldByName('DrainageScale').IsNull) then
              begin
                LReservoir.FUrbanRunoff.FData :=
                  LDataSet.DataSet.FieldByName('DrainageScale').AsFloat;
                LReservoir.FUrbanRunoff.FInitalised := True;
                LMessage := FAppModules.Language.GetString('TFile02DatabaseAgent.strUrbanRunoffDefaultedToDrainageScale');
                AProgressFunction(LMessage,ptWarning,LStop);
              end;

            end;

            if not LDataSet.DataSet.FieldByName('AfforestationScale').IsNull then
            begin
              LReservoir.FForestScale.FData :=
                LDataSet.DataSet.FieldByName('AfforestationScale').AsFloat;
              LReservoir.FForestScale.FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('IrrigationScale').IsNull then
            begin
              LReservoir.FIrrigateScale.FData :=
                LDataSet.DataSet.FieldByName('IrrigationScale').AsFloat;
              LReservoir.FIrrigateScale.FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('ReservoirComment').IsNull then
            begin
              LReservoir.FComment01.FData := Trim(LDataSet.DataSet.FieldByName('ReservoirComment').AsString);
              LReservoir.FComment01.FLength := Length(LReservoir.FComment01.FData);
              LReservoir.FComment01.FInitalised := True;
            end;

            //Line 3 : Full reservoir surface area
            if not LDataSet.DataSet.FieldByName('AreaFull').IsNull then
            begin
              LReservoir.FFullSurf.FData :=
                LDataSet.DataSet.FieldByName('AreaFull').AsFloat;
              LReservoir.FFullSurf.FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('RainCoef').IsNull then
            begin
              LReservoir.FRainCoef.FData :=
                LDataSet.DataSet.FieldByName('RainCoef').AsFloat;
              LReservoir.FRainCoef.FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('CatchmentRef').IsNull then
            begin
              LReservoir.FCatchRef.FData :=
                LDataSet.DataSet.FieldByName('CatchmentRef').AsInteger;
              LReservoir.FCatchRef.FInitalised := True;
            end;

            if(FAppModules.Model.ModelName = CPlanning) then
            begin
              if not LDataSet.DataSet.FieldByName('NaturalInflowChannel').IsNull then
              begin
                LReservoir.FNaturalInflowChannel.FData :=
                  LDataSet.DataSet.FieldByName('NaturalInflowChannel').AsInteger;
                LReservoir.FNaturalInflowChannel.FInitalised := True;
              end;
            end;

            if not LDataSet.DataSet.FieldByName('Comment04').IsNull then
            begin
              LReservoir.FComment04.FData := TrimRight(LDataSet.DataSet.FieldByName('Comment04').AsString);
              LReservoir.FComment04.FLength := Length(LReservoir.FComment04.FData);
              LReservoir.FComment04.FInitalised := True;
            end;

            //Line 4 : Number of power channels downstream of reservoir
            if not LDataSet.DataSet.FieldByName('ChannelsCount').IsNull then
            begin
              LReservoir.FPowerNum.FData :=
                LDataSet.DataSet.FieldByName('ChannelsCount').AsInteger;
              LReservoir.FPowerNum.FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Comment03').IsNull then
            begin
              LReservoir.FComment03.FData := TrimRight(LDataSet.DataSet.FieldByName('Comment03').AsString);
              LReservoir.FComment03.FLength := Length(LReservoir.FComment03.FData);
              LReservoir.FComment03.FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Channel01').IsNull then
            begin
              LReservoir.FPowerChannels[1].FData :=
                LDataSet.DataSet.FieldByName('Channel01').AsInteger;
              LReservoir.FPowerChannels[1].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Channel02').IsNull then
            begin
              LReservoir.FPowerChannels[2].FData :=
                LDataSet.DataSet.FieldByName('Channel02').AsInteger;
              LReservoir.FPowerChannels[2].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Channel03').IsNull then
            begin
              LReservoir.FPowerChannels[3].FData :=
                LDataSet.DataSet.FieldByName('Channel03').AsInteger;
              LReservoir.FPowerChannels[3].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Channel04').IsNull then
            begin
              LReservoir.FPowerChannels[4].FData :=
                LDataSet.DataSet.FieldByName('Channel04').AsInteger;
              LReservoir.FPowerChannels[4].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Channel05').IsNull then
            begin
              LReservoir.FPowerChannels[5].FData :=
                LDataSet.DataSet.FieldByName('Channel05').AsInteger;
              LReservoir.FPowerChannels[5].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Channel06').IsNull then
            begin
              LReservoir.FPowerChannels[6].FData :=
                LDataSet.DataSet.FieldByName('Channel06').AsInteger;
              LReservoir.FPowerChannels[6].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Channel07').IsNull then
            begin
              LReservoir.FPowerChannels[7].FData :=
                LDataSet.DataSet.FieldByName('Channel07').AsInteger;
              LReservoir.FPowerChannels[7].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Channel08').IsNull then
            begin
              LReservoir.FPowerChannels[8].FData :=
                LDataSet.DataSet.FieldByName('Channel08').AsInteger;
              LReservoir.FPowerChannels[8].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Channel09').IsNull then
            begin
              LReservoir.FPowerChannels[9].FData :=
                LDataSet.DataSet.FieldByName('Channel09').AsInteger;
              LReservoir.FPowerChannels[9].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Channel10').IsNull then
            begin
              LReservoir.FPowerChannels[10].FData :=
                LDataSet.DataSet.FieldByName('Channel10').AsInteger;
              LReservoir.FPowerChannels[10].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Channel11').IsNull then
            begin
              LReservoir.FPowerChannels[11].FData :=
                LDataSet.DataSet.FieldByName('Channel11').AsInteger;
              LReservoir.FPowerChannels[11].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Channel12').IsNull then
            begin
              LReservoir.FPowerChannels[12].FData :=
                LDataSet.DataSet.FieldByName('Channel12').AsInteger;
              LReservoir.FPowerChannels[12].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Channel13').IsNull then
            begin
              LReservoir.FPowerChannels[13].FData :=
                LDataSet.DataSet.FieldByName('Channel13').AsInteger;
              LReservoir.FPowerChannels[13].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Channel14').IsNull then
            begin
              LReservoir.FPowerChannels[14].FData :=
                LDataSet.DataSet.FieldByName('Channel14').AsInteger;
              LReservoir.FPowerChannels[14].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Channel15').IsNull then
            begin
              LReservoir.FPowerChannels[15].FData :=
                LDataSet.DataSet.FieldByName('Channel15').AsInteger;
              LReservoir.FPowerChannels[15].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Channel16').IsNull then
            begin
              LReservoir.FPowerChannels[16].FData :=
                LDataSet.DataSet.FieldByName('Channel16').AsInteger;
              LReservoir.FPowerChannels[16].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Channel17').IsNull then
            begin
              LReservoir.FPowerChannels[17].FData :=
                LDataSet.DataSet.FieldByName('Channel17').AsInteger;
              LReservoir.FPowerChannels[17].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Channel18').IsNull then
            begin
              LReservoir.FPowerChannels[18].FData :=
                LDataSet.DataSet.FieldByName('Channel18').AsInteger;
              LReservoir.FPowerChannels[18].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Channel19').IsNull then
            begin
              LReservoir.FPowerChannels[19].FData :=
                LDataSet.DataSet.FieldByName('Channel19').AsInteger;
              LReservoir.FPowerChannels[19].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Channel20').IsNull then
            begin
              LReservoir.FPowerChannels[20].FData :=
                LDataSet.DataSet.FieldByName('Channel20').AsInteger;
              LReservoir.FPowerChannels[20].FInitalised := True;
            end;

            //Line 5 : Surface elevation (m) for each point on the area
            if not LDataSet.DataSet.FieldByName('ReservoirElev01').IsNull then
            begin
              LReservoir.FSurfElev[1].FData :=
                LDataSet.DataSet.FieldByName('ReservoirElev01').AsFloat;
              LReservoir.FSurfElev[1].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('ReservoirElev02').IsNull then
            begin
              LReservoir.FSurfElev[2].FData :=
                LDataSet.DataSet.FieldByName('ReservoirElev02').AsFloat;
              LReservoir.FSurfElev[2].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('ReservoirElev03').IsNull then
            begin
              LReservoir.FSurfElev[3].FData :=
                LDataSet.DataSet.FieldByName('ReservoirElev03').AsFloat;
              LReservoir.FSurfElev[3].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('ReservoirElev04').IsNull then
            begin
              LReservoir.FSurfElev[4].FData :=
                LDataSet.DataSet.FieldByName('ReservoirElev04').AsFloat;
              LReservoir.FSurfElev[4].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('ReservoirElev05').IsNull then
            begin
              LReservoir.FSurfElev[5].FData :=
                LDataSet.DataSet.FieldByName('ReservoirElev05').AsFloat;
              LReservoir.FSurfElev[5].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('ReservoirElev06').IsNull then
            begin
              LReservoir.FSurfElev[6].FData :=
                LDataSet.DataSet.FieldByName('ReservoirElev06').AsFloat;
              LReservoir.FSurfElev[6].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('ReservoirElev07').IsNull then
            begin
              LReservoir.FSurfElev[7].FData :=
                LDataSet.DataSet.FieldByName('ReservoirElev07').AsFloat;
              LReservoir.FSurfElev[7].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('ReservoirElev08').IsNull then
            begin
              LReservoir.FSurfElev[8].FData :=
                LDataSet.DataSet.FieldByName('ReservoirElev08').AsFloat;
              LReservoir.FSurfElev[8].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('ReservoirElev09').IsNull then
            begin
              LReservoir.FSurfElev[9].FData :=
                LDataSet.DataSet.FieldByName('ReservoirElev09').AsFloat;
              LReservoir.FSurfElev[9].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('ReservoirElev10').IsNull then
            begin
              LReservoir.FSurfElev[10].FData :=
                LDataSet.DataSet.FieldByName('ReservoirElev10').AsFloat;
              LReservoir.FSurfElev[10].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('ReservoirElev11').IsNull then
            begin
              LReservoir.FSurfElev[11].FData :=
                LDataSet.DataSet.FieldByName('ReservoirElev11').AsFloat;
              LReservoir.FSurfElev[11].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('ReservoirElev12').IsNull then
            begin
              LReservoir.FSurfElev[12].FData :=
                LDataSet.DataSet.FieldByName('ReservoirElev12').AsFloat;
              LReservoir.FSurfElev[12].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('ReservoirElev13').IsNull then
            begin
              LReservoir.FSurfElev[13].FData :=
                LDataSet.DataSet.FieldByName('ReservoirElev13').AsFloat;
              LReservoir.FSurfElev[13].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('ReservoirElev14').IsNull then
            begin
              LReservoir.FSurfElev[14].FData :=
                LDataSet.DataSet.FieldByName('ReservoirElev14').AsFloat;
              LReservoir.FSurfElev[14].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('ReservoirElev15').IsNull then
            begin
              LReservoir.FSurfElev[15].FData :=
                LDataSet.DataSet.FieldByName('ReservoirElev15').AsFloat;
              LReservoir.FSurfElev[15].FInitalised := True;
            end;

            {if not LDataSet.DataSet.FieldByName('Comment02').IsNull then
            begin
              LReservoir.FComment02.FData := TrimRight(LDataSet.DataSet.FieldByName('Comment02').AsString);
              LReservoir.FComment02.FLength := Length(LReservoir.FComment02.FData);
              LReservoir.FComment02.FInitalised := True;
            end;}

            //Line 6 : Volume of reservoir (million m3) corresponding to each point
            if not LDataSet.DataSet.FieldByName('Volume01').IsNull then
            begin
              LReservoir.FResVol[1].FData :=
                LDataSet.DataSet.FieldByName('Volume01').AsFloat;
              LReservoir.FResVol[1].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Volume02').IsNull then
            begin
              LReservoir.FResVol[2].FData :=
                LDataSet.DataSet.FieldByName('Volume02').AsFloat;
              LReservoir.FResVol[2].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Volume03').IsNull then
            begin
              LReservoir.FResVol[3].FData :=
                LDataSet.DataSet.FieldByName('Volume03').AsFloat;
              LReservoir.FResVol[3].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Volume04').IsNull then
            begin
              LReservoir.FResVol[4].FData :=
                LDataSet.DataSet.FieldByName('Volume04').AsFloat;
              LReservoir.FResVol[4].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Volume05').IsNull then
            begin
              LReservoir.FResVol[5].FData :=
                LDataSet.DataSet.FieldByName('Volume05').AsFloat;
              LReservoir.FResVol[5].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Volume06').IsNull then
            begin
              LReservoir.FResVol[6].FData :=
                LDataSet.DataSet.FieldByName('Volume06').AsFloat;
              LReservoir.FResVol[6].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Volume07').IsNull then
            begin
              LReservoir.FResVol[7].FData :=
                LDataSet.DataSet.FieldByName('Volume07').AsFloat;
              LReservoir.FResVol[7].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Volume08').IsNull then
            begin
              LReservoir.FResVol[8].FData :=
                LDataSet.DataSet.FieldByName('Volume08').AsFloat;
              LReservoir.FResVol[8].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Volume09').IsNull then
            begin
              LReservoir.FResVol[9].FData :=
                LDataSet.DataSet.FieldByName('Volume09').AsFloat;
              LReservoir.FResVol[9].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Volume10').IsNull then
            begin
              LReservoir.FResVol[10].FData :=
                LDataSet.DataSet.FieldByName('Volume10').AsFloat;
              LReservoir.FResVol[10].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Volume11').IsNull then
            begin
              LReservoir.FResVol[11].FData :=
                LDataSet.DataSet.FieldByName('Volume11').AsFloat;
              LReservoir.FResVol[11].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Volume12').IsNull then
            begin
              LReservoir.FResVol[12].FData :=
                LDataSet.DataSet.FieldByName('Volume12').AsFloat;
              LReservoir.FResVol[12].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Volume13').IsNull then
            begin
              LReservoir.FResVol[13].FData :=
                LDataSet.DataSet.FieldByName('Volume13').AsFloat;
              LReservoir.FResVol[13].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Volume14').IsNull then
            begin
              LReservoir.FResVol[14].FData :=
                LDataSet.DataSet.FieldByName('Volume14').AsFloat;
              LReservoir.FResVol[14].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Volume15').IsNull then
            begin
              LReservoir.FResVol[15].FData :=
                LDataSet.DataSet.FieldByName('Volume15').AsFloat;
              LReservoir.FResVol[15].FInitalised := True;
            end;

            //Line 7 : Surface area (km2) of reservoir corresponding to each point
            if not LDataSet.DataSet.FieldByName('Area01').IsNull then
            begin
              LReservoir.FSurfArea[1].FData :=
                LDataSet.DataSet.FieldByName('Area01').AsFloat;
              LReservoir.FSurfArea[1].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Area02').IsNull then
            begin
              LReservoir.FSurfArea[2].FData :=
                LDataSet.DataSet.FieldByName('Area02').AsFloat;
              LReservoir.FSurfArea[2].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Area03').IsNull then
            begin
              LReservoir.FSurfArea[3].FData :=
                LDataSet.DataSet.FieldByName('Area03').AsFloat;
              LReservoir.FSurfArea[3].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Area04').IsNull then
            begin
              LReservoir.FSurfArea[4].FData :=
                LDataSet.DataSet.FieldByName('Area04').AsFloat;
              LReservoir.FSurfArea[4].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Area05').IsNull then
            begin
              LReservoir.FSurfArea[5].FData :=
                LDataSet.DataSet.FieldByName('Area05').AsFloat;
              LReservoir.FSurfArea[5].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Area06').IsNull then
            begin
              LReservoir.FSurfArea[6].FData :=
                LDataSet.DataSet.FieldByName('Area06').AsFloat;
              LReservoir.FSurfArea[6].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Area07').IsNull then
            begin
              LReservoir.FSurfArea[7].FData :=
                LDataSet.DataSet.FieldByName('Area07').AsFloat;
              LReservoir.FSurfArea[7].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Area08').IsNull then
            begin
              LReservoir.FSurfArea[8].FData :=
                LDataSet.DataSet.FieldByName('Area08').AsFloat;
              LReservoir.FSurfArea[8].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Area09').IsNull then
            begin
              LReservoir.FSurfArea[9].FData :=
                LDataSet.DataSet.FieldByName('Area09').AsFloat;
              LReservoir.FSurfArea[9].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Area10').IsNull then
            begin
              LReservoir.FSurfArea[10].FData :=
                LDataSet.DataSet.FieldByName('Area10').AsFloat;
              LReservoir.FSurfArea[10].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Area11').IsNull then
            begin
              LReservoir.FSurfArea[11].FData :=
                LDataSet.DataSet.FieldByName('Area11').AsFloat;
              LReservoir.FSurfArea[11].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Area12').IsNull then
            begin
              LReservoir.FSurfArea[12].FData :=
                LDataSet.DataSet.FieldByName('Area12').AsFloat;
              LReservoir.FSurfArea[12].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Area13').IsNull then
            begin
              LReservoir.FSurfArea[13].FData :=
                LDataSet.DataSet.FieldByName('Area13').AsFloat;
              LReservoir.FSurfArea[13].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Area14').IsNull then
            begin
              LReservoir.FSurfArea[14].FData :=
                LDataSet.DataSet.FieldByName('Area14').AsFloat;
              LReservoir.FSurfArea[14].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Area15').IsNull then
            begin
              LReservoir.FSurfArea[15].FData :=
                LDataSet.DataSet.FieldByName('Area15').AsFloat;
              LReservoir.FSurfArea[15].FInitalised := True;
            end;

            //Line 8 : Monthly lake evaporation
            if not LDataSet.DataSet.FieldByName('Evapo01').IsNull then
            begin
              LReservoir.FMonthEvap[1].FData :=
                LDataSet.DataSet.FieldByName('Evapo01').AsFloat;
              LReservoir.FMonthEvap[1].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Evapo02').IsNull then
            begin
              LReservoir.FMonthEvap[2].FData :=
                LDataSet.DataSet.FieldByName('Evapo02').AsFloat;
              LReservoir.FMonthEvap[2].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Evapo03').IsNull then
            begin
              LReservoir.FMonthEvap[3].FData :=
                LDataSet.DataSet.FieldByName('Evapo03').AsFloat;
              LReservoir.FMonthEvap[3].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Evapo04').IsNull then
            begin
              LReservoir.FMonthEvap[4].FData :=
                LDataSet.DataSet.FieldByName('Evapo04').AsFloat;
              LReservoir.FMonthEvap[4].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Evapo05').IsNull then
            begin
              LReservoir.FMonthEvap[5].FData :=
                LDataSet.DataSet.FieldByName('Evapo05').AsFloat;
              LReservoir.FMonthEvap[5].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Evapo06').IsNull then
            begin
              LReservoir.FMonthEvap[6].FData :=
                LDataSet.DataSet.FieldByName('Evapo06').AsFloat;
              LReservoir.FMonthEvap[6].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Evapo07').IsNull then
            begin
              LReservoir.FMonthEvap[7].FData :=
                LDataSet.DataSet.FieldByName('Evapo07').AsFloat;
              LReservoir.FMonthEvap[7].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Evapo08').IsNull then
            begin
              LReservoir.FMonthEvap[8].FData :=
                LDataSet.DataSet.FieldByName('Evapo08').AsFloat;
              LReservoir.FMonthEvap[8].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Evapo09').IsNull then
            begin
              LReservoir.FMonthEvap[9].FData :=
                LDataSet.DataSet.FieldByName('Evapo09').AsFloat;
              LReservoir.FMonthEvap[9].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Evapo10').IsNull then
            begin
              LReservoir.FMonthEvap[10].FData :=
                LDataSet.DataSet.FieldByName('Evapo10').AsFloat;
              LReservoir.FMonthEvap[10].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Evapo11').IsNull then
            begin
              LReservoir.FMonthEvap[11].FData :=
                LDataSet.DataSet.FieldByName('Evapo11').AsFloat;
              LReservoir.FMonthEvap[11].FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Evapo12').IsNull then
            begin
              LReservoir.FMonthEvap[12].FData :=
                LDataSet.DataSet.FieldByName('Evapo12').AsFloat;
              LReservoir.FMonthEvap[12].FInitalised := True;
            end;
            LDataSet.DataSet.Next;
          end;
        end;
      end;

      if (LReservoirList.FReservoirs.Count > 0) then
      begin
        LReservoirList.FReserviorNum.FData := LReservoirList.FReservoirs.Count-1;
        LReservoirList.FReserviorNum.FInitalised := True;
        LReservoirList.FReserviorAndNodesNum.FData := LReservoirList.FReservoirs.Count-1;
        LReservoirList.FReserviorAndNodesNum.FInitalised := True;
      end;

      //line9 on wards+++++++++++++++++++++++++++
      LDataSet.DataSet.Close;
      LDataSet.SetSQL(ReadF02UnkownDataSQL);
      LDataSet.SetParams(['FileType'], [IntToStr(AFileName.FileNumber)]);
      LDataSet.SetParams(['FileGroup'], [IntToStr(AFileName.FileGroup)]);
      LDataSet.DataSet.Open;

      //Check if there is any unknown data.
      LNoData := LNoData and (LDataSet.DataSet.RecordCount = 0);

      while not LDataSet.DataSet.Eof do
      begin
        LReservoirList.FF02ExtraLines.Add(TrimRight(LDataSet.DataSet.FieldByName('LineData').AsString));
        LDataSet.DataSet.Next;
      end;
      LDataSet.DataSet.Active := LNoData;

      LMessage := FAppModules.Language.GetString('TFile02DatabaseAgent.strReadEnded');
      AProgressFunction(LMessage,ptNone,LStop);
      Result :=  not LNoData;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile02DatabaseAgent.WriteModelDataToDatabase(AFileName:TFileNameObject;ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile02DatabaseAgent.WriteModelDataToDatabase';
var
  LMessage,
  LStudyArea,
  LFieldName  : string;
  LCount,
  LArrayCount : integer;
  LDataSet    : TAbstractModelDataset;
  LReservoirList: TReservoirObject;
  LReservoir : TReservoir;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile02DatabaseAgent.strWriteStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if not ClearModelDataInDatabase(AFileName,AProgressFunction,True) then
      Exit;

    LReservoirList := ADataObject.FReservoirObject;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try

      LStudyArea := FAppModules.StudyArea.StudyAreaCode;

      //line1++++++++++++++++++++++++++++
      LDataSet.DataSet.Close;
      LDataSet.SetSQL(WriteReservoirDataSQL);
      LDataSet.ClearQueryParams();
      LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
      LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
      LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
      LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);

      if LReservoirList.FReserviorNum.FInitalised then
        LDataSet.SetParams(['ReservoirCount'], [IntToStr(LReservoirList.FReserviorNum.FData)]);

      if LReservoirList.FHydroUnits.FInitalised then
        LDataSet.SetParams(['HydroUnitsCode'], [LReservoirList.FHydroUnits.FData]);

      if LReservoirList.FComment.FInitalised then
        LDataSet.SetParams(['ReservoirComment'], [LReservoirList.FComment.FData]);


      LDataSet.ExecSQL;

      for LCount := 1 to LReservoirList.FReserviorAndNodesNum.FData do
      begin
        //line2 +++++++++++++++++++++++++++++
        LReservoir := TReservoir(LReservoirList.FReservoirs[LCount]);

        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteReservoirDetailsDataSQL);
        LDataSet.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['Identifier'], [IntToStr(LCount)]);

        LReservoir.FIdentifier.FData := LCount;
        LReservoir.FIdentifier.FInitalised := True;

        if LReservoir.FName.FInitalised then
          LDataSet.SetParams(['ReservoirName'],
          [LReservoir.FName.FData]);
        if LReservoir.FSummaryInclude.FInitalised then
        begin
          if(Trim(LReservoir.FSummaryInclude.FData) = '') then
            LReservoir.FSummaryInclude.FData := 'Y';
          LDataSet.SetParams(['IncludeSummary'],
          [LReservoir.FSummaryInclude.FData]);
        end;
        if LReservoir.FNodeNo.FInitalised then
          LDataSet.SetParams(['NodeCount'],
          [IntToStr(LReservoir.FNodeNo.FData)]);
        if LReservoir.FPenaltyStructure.FInitalised then
          LDataSet.SetParams(['PenaltyStruct'],
          [IntToStr(LReservoir.FPenaltyStructure.FData)]);
        if LReservoir.FMaxPoints.FInitalised then
          LDataSet.SetParams(['PointsCount'],
          [IntToStr(LReservoir.FMaxPoints.FData)]);
        if LReservoir.FNodeType.FInitalised then
          LDataSet.SetParams(['NodeType'],
          [IntToStr(LReservoir.FNodeType.FData)]);
        if LReservoir.FDrainScale.FInitalised then
          LDataSet.SetParams(['DrainageScale'],
          [FloatToStr(LReservoir.FDrainScale.FData)]);
        if LReservoir.FForestScale.FInitalised then
          LDataSet.SetParams(['AfforestationScale'],
          [FloatToStr(LReservoir.FForestScale.FData)]);
        if LReservoir.FIrrigateScale.FInitalised then
          LDataSet.SetParams(['IrrigationScale'],
          [FloatToStr(LReservoir.FIrrigateScale.FData)]);
        if LReservoir.FComment01.FInitalised then
          LDataSet.SetParams(['ReservoirDetailsComment'],
          [LReservoir.FComment01.FData]);
        if LReservoir.FComment03.FInitalised then
          LDataSet.SetParams(['Comment03'],
          [LReservoir.FComment03.FData]);
        if LReservoir.FUrbanRunoff.FInitalised then
          LDataSet.SetParams(['UrbanRunoff'],
          [FloatToStr(LReservoir.FUrbanRunoff.FData)]);


        //Line 3 : Full reservoir surface area
        if LReservoir.FFullSurf.FInitalised then
          LDataSet.SetParams(['AreaFull'],
          [FloatToStr(LReservoir.FFullSurf.FData)]);
        if LReservoir.FRainCoef.FInitalised then
          LDataSet.SetParams(['RainCoef'],
          [FloatToStr(LReservoir.FRainCoef.FData)]);
        if LReservoir.FCatchRef.FInitalised then
          LDataSet.SetParams(['CatchmentRef'],
          [IntToStr(LReservoir.FCatchRef.FData)]);
        if LReservoir.FNaturalInflowChannel.FInitalised then
          LDataSet.SetParams(['NaturalInflowChannel'],
          [IntToStr(LReservoir.FNaturalInflowChannel.FData)]);
        if LReservoir.FComment04.FInitalised then
          LDataSet.SetParams(['Comment04'],
          [LReservoir.FComment04.FData]);

        //Line 4 : Number of power channels downstream of reservoir
        if LReservoir.FPowerNum.FInitalised then
          LDataSet.SetParams(['ChannelsCount'],
          [IntToStr(LReservoir.FPowerNum.FData)]);

        LDataSet.ExecSQL;

        if not (LReservoir.FNodeType.FData in ReservoirsAndNodeWithInflowSet) then
          Continue;

        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteReservoirChannelsDataSQL);
        LDataSet.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['Identifier'], [IntToStr(LCount)]);
        for LArrayCount :=  MinPowerChannels to MaxPowerChannels do
        begin
          LFieldName := Format('%s%2.2d',['Channel',LArrayCount]);
          if LReservoir.FPowerChannels[LArrayCount].FInitalised then
            LDataSet.SetParams([LFieldName],
            [IntToStr(LReservoir.FPowerChannels[LArrayCount].FData)]);
        end;

        LDataSet.ExecSQL;

        //Line 5 : Surface elevation (m) for each point on the area
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteReservoirElevationDataSQL);
        LDataSet.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['Identifier'], [IntToStr(LCount)]);
        {if LReservoir.FComment02.FInitalised then
          LDataSet.SetParams(['Comment02'], [LReservoir.FComment02.FData;}
        for LArrayCount :=  MinPoints to MaxPoints do
        begin
          LFieldName := Format('%s%2.2d',['ReservoirElev',LArrayCount]);
          if LReservoir.FSurfElev[LArrayCount].FInitalised then
            LDataSet.SetParams([LFieldName],
            [FloatToStr(LReservoir.FSurfElev[LArrayCount].FData)]);
        end;

        LDataSet.ExecSQL;

        //Line 6 : Volume of reservoir (million m3) corresponding to each point
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteReservoirVolumeDataSQL);
        LDataSet.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['Identifier'], [IntToStr(LCount)]);
        for LArrayCount :=  MinPoints to MaxPoints do
        begin
          LFieldName := Format('%s%2.2d',['Volume',LArrayCount]);
          if LReservoir.FResVol[LArrayCount].FInitalised then
            LDataSet.SetParams([LFieldName], [FloatToStr(
            LReservoir.FResVol[LArrayCount].FData)]);
        end;

        LDataSet.ExecSQL;

        //Line 7 : Surface area (km2) of reservoir corresponding to each point
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteReservoirAreaDataSQL);
        LDataSet.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['Identifier'], [IntToStr(LCount)]);
        for LArrayCount :=  MinPoints to MaxPoints do
        begin
          LFieldName := Format('%s%2.2d',['Area',LArrayCount]);
          if LReservoir.FSurfArea[LArrayCount].FInitalised then
            LDataSet.SetParams([LFieldName], [FloatToStr(
            LReservoir.FSurfArea[LArrayCount].FData)]);
        end;

        LDataSet.ExecSQL;

        //Line 8 : Monthly lake evaporation
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteReservoirEvapDataSQL);
        LDataSet.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['Identifier'], [IntToStr(LCount)]);
        for LArrayCount :=  MinMonths to MaxMonths do
        begin
          LFieldName := Format('%s%2.2d',['Evapo',LArrayCount]);
          if LReservoir.FMonthEvap[LArrayCount].FInitalised then
            LDataSet.SetParams([LFieldName], [FloatToStr(
            LReservoir.FMonthEvap[LArrayCount].FData)]);
        end;

        LDataSet.ExecSQL;

      end;

       //line9 onwards++++++++++++++++++++++++++++
      for LCount := 0 to LReservoirList.FF02ExtraLines.Count - 1 do
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
        LDataSet.SetParams(['LineNumber'], [IntToStr(9+LCount)]);
        LDataSet.SetParams(['LineSection'], [IntToStr(0)]);
        LDataSet.SetParams(['LineData'], [LReservoirList.FF02ExtraLines[LCount]]);

        LDataSet.ExecSQL;
      end;
      LDataSet.DataSet.Close;

      Result := InsertFileName(AFileName);
      if Result then
      begin
        LMessage := FAppModules.Language.GetString('TFile02DatabaseAgent.strWriteEnded');
        AProgressFunction(LMessage,ptNone,LStop);
      end;
    finally
      LDataSet.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile02DatabaseAgent.ClearModelDataInDatabase(AFileName: TFileNameObject; AProgressFunction: TProgressUpdateFuntion;
         AQuetly: boolean = False): boolean;
const OPNAME = 'TFile02DatabaseAgent.ClearModelDataInDatabase';
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

    LTableNames := 'Reservoir,ReservoirDetails,ReservoirArea,ReservoirChannels,'+
                   'ReservoirElevation,ReservoirEvap,ReservoirVolume,ReservoirGroup';
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
