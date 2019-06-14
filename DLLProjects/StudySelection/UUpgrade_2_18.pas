{******************************************************************************}
{*  UNIT      : Contains function to upgrade database to WRMF 2.18.0         *}
{*  AUTHOR    : Presley Mudau                                                 *}
{*  DATE      : 2006/05/02                                                    *}
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UUpgrade_2_18;

interface

uses
  Classes,
  SysUtils,
  UAbstractObject;

function UpgradeToVersion_2_18_0 (AAppModules: TAppModules;
                                  ADirectory             : string;
                                  AProgressUpdateFuntion : TProgressUpdateFuntion;
                                  AFileNames             : TStringList;
                                  var AVersion           : string) : boolean;
function Upgrade_WRYMDataPaths_2_18_0 (AAppModules: TAppModules;ADirectory : string) : boolean;

function Upgrade_PhysicalFlowConstraints_2_18_0 (AAppModules: TAppModules;ADirectory : string; var AFileName  : string) : boolean;

implementation

uses
  Winapi.Windows,
  DBClient,
  DB,
  xmldom,
  XMLDoc,
  XMLIntf,
  Provider,
  UDataSetType,
  Vcl.Dialogs,
  UUtilities,
  VoaimsCom_TLB,
  UErrorHandlingOperations;

function UpgradeToVersion_2_18_0 (AAppModules: TAppModules;
                                  ADirectory             : string;
                                  AProgressUpdateFuntion : TProgressUpdateFuntion;
                                  AFileNames             : TStringList;
                                  var AVersion           : string) : boolean;
const OPNAME = 'UpgradeToVersion_2_18_0';
var
  lFileName : string;
  lStop     : boolean;
begin
  Result := FALSE;
  try
    AProgressUpdateFuntion('Upgrading data to version 2.18.0.', ptNone, lStop);
    if(AFileNames.IndexOf('LD_RainfallProjectGauges.xml') < 0) then
      Upgrade_WRYMDataPaths_2_18_0(AAppModules,ADirectory);

    lFileName := '';
    Upgrade_PhysicalFlowConstraints_2_18_0 (AAppModules,ADirectory,lFileName);
    if FileExists(ADirectory + lFileName) and (AFileNames.IndexOf(lFileName) < 0) then
    begin

      AFileNames.Add(lFileName);
      if(AFileNames.IndexOf('LD_FlowConstraintsDeltaH.xml') >= 0) then
         AFileNames.Delete(AFileNames.IndexOf('LD_FlowConstraintsDeltaH.xml'));
      if(AFileNames.IndexOf('LD_FlowConstraintsDischarge.xml') >= 0) then
         AFileNames.Delete(AFileNames.IndexOf('LD_FlowConstraintsDischarge.xml'));
      if(AFileNames.IndexOf('LD_FlowConstraintsDiversion.xml') >= 0) then
         AFileNames.Delete(AFileNames.IndexOf('LD_FlowConstraintsDiversion.xml'));
      if(AFileNames.IndexOf('LD_FlowConstraintsElevation.xml') >= 0) then
         AFileNames.Delete(AFileNames.IndexOf('LD_FlowConstraintsElevation.xml'));
      if(AFileNames.IndexOf('LD_FlowConstraintsType11Depth.xml') >= 0) then
         AFileNames.Delete(AFileNames.IndexOf('LD_FlowConstraintsType11Depth.xml'));
      if(AFileNames.IndexOf('LD_FlowConstraintsType11Flow.xml') >= 0) then
         AFileNames.Delete(AFileNames.IndexOf('LD_FlowConstraintsType11Flow.xml'));
    end;

    AFileNames[0] := 'TableVersion=2.18.0';
    AVersion := '2.18.0';
    Result   := TRUE;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function Upgrade_WRYMDataPaths_2_18_0 (AAppModules: TAppModules;ADirectory : string) : boolean;
const OPNAME = 'Upgrade_WRYMDataPaths_2_18_0';
var
  LIndex         : integer;
  LDataSet       : TAbstractModelDataset;
  lNewDataSet    : TClientDataSet;
  LFileDataSet   : TClientDataSet;
  LStrData       : string;
  LFileName      : string;
  LHydrologyPath : string;
  LDataSetProvider     : TDataSetProvider;
  LSpecifiedDemandPath : string;
begin
  Result := FALSE;
  try
    AAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.SetSQL('SELECT * FROM WRYMDat WHERE Model IS NULL');
      LDataset.DataSet.Open;
      LDataSetProvider := TDataSetProvider.Create(nil);
      lNewDataSet := TClientDataSet.Create(nil);
      try
        LDataSetProvider.DataSet := LDataset.DataSet;
        lNewDataSet.ReadOnly := True;
        lNewDataSet.SetProvider(LDataSetProvider);
        lNewDataSet.StoreDefs := True;
        lNewDataSet.Open;
        lNewDataSet.SaveToFile(ADirectory +  'LD_WRYMDat_18.xml', dfXML);
      finally
        LDataset.DataSet.Close;
        lNewDataSet.Close;
        lNewDataSet.Free;
        LDataSetProvider.Free;
      end;
    finally
      LDataset.Free;
    end;


    lNewDataSet := TClientDataSet.Create(nil);
    LFileDataSet := TClientDataSet.Create(nil);
    try
      lNewDataSet.ReadOnly  := False;
      lNewDataSet.StoreDefs := True;
      lNewDataSet.FileName  := ADirectory +  'LD_WRYMDat_18.xml';
      lNewDataSet.Open;

      LFileDataSet.ReadOnly  := True;
      LFileDataSet.StoreDefs := True;
      LFileDataSet.FileName  := ADirectory +  'LD_WRYMDat.xml';
      LFileDataSet.Open;

      while not LFileDataSet.Eof do
      begin
        lNewDataSet.Insert;
        for LIndex := 0 to LFileDataSet.FieldList.Count-1 do
        begin
          LFileName := LFileDataSet.FieldList.Fields[LIndex].FieldName;
          if(lNewDataSet.FieldList.Find(LFileName) <> nil) then
            lNewDataSet.FieldByName(LFileName).Value := LFileDataSet.FieldByName(LFileName).Value;
        end;
        lNewDataSet.Post;
        LFileDataSet.Next;
      end;

      lNewDataSet.SaveToFile(ADirectory +  'LD_WRYMDat_18.xml', dfXML);
      LFileDataSet.Close;
      lNewDataSet.Close;
      if SysUtils.DeleteFile(ADirectory +  'LD_WRYMDat.xml') then
      begin
        RenameFile(ADirectory +  'LD_WRYMDat_18.xml',ADirectory +  'LD_WRYMDat.xml');
      end;
    finally
      LFileDataSet.Free;
      lNewDataSet.Free;
    end;


    LHydrologyPath       := '';
    LSpecifiedDemandPath := '';
    LFileDataSet := TClientDataSet.Create(nil);
    try
      LFileName := ADirectory + 'LD_FileNames.xml';
      if FileExists(LFileName) then
      begin
        LFileDataSet.Close;
        LFileDataSet.FileName  := LFileName;
        LFileDataSet.ReadOnly := False;
        LFileDataSet.StoreDefs := True;
        LFileDataSet.Open;
        LFileDataSet.LogChanges := False;
        while not LFileDataSet.Eof do
        begin
          LStrData := Trim(LFileDataSet.FieldByName('FileName').AsString);
          if(LFileDataSet.FieldByName('FileGroup').AsInteger = 2) and (LHydrologyPath = '') then
          begin
            LHydrologyPath := ExtractFilePath(LStrData);
          end;
          if(LFileDataSet.FieldByName('FileGroup').AsInteger = 4) and (LSpecifiedDemandPath = '') then
          begin
            LSpecifiedDemandPath := ExtractFilePath(LStrData);
          end;
          LStrData := ExtractFileName(LStrData);
          LFileDataSet.Edit;
          LFileDataSet.FieldByName('FileName').AsString := LStrData;
          LFileDataSet.Post;
          LFileDataSet.Next;
        end;
        LFileDataSet.MergeChangeLog;
        LFileDataSet.SaveToFile(LFileName, dfXML);
        lFileDataSet.Close;
      end;

      LFileName := ADirectory + 'LD_HydrologyFileData.xml';
      if FileExists(LFileName) then
      begin
        LFileDataSet.Close;
        LFileDataSet.FileName  := LFileName;
        LFileDataSet.ReadOnly := False;
        LFileDataSet.StoreDefs := True;
        LFileDataSet.Open;
        LFileDataSet.LogChanges := False;
        while not LFileDataSet.Eof do
        begin
          LStrData := Trim(LFileDataSet.FieldByName('FileName').AsString);
          LStrData := ExtractFileName(LStrData);
          LFileDataSet.Edit;
          LFileDataSet.FieldByName('FileName').AsString := LStrData;
          LFileDataSet.Post;
          LFileDataSet.Next;
        end;
        LFileDataSet.SaveToFile(LFileName, dfXML);
        lFileDataSet.Close;
      end;

      LFileName := ADirectory + 'LD_ParamStochastics.xml';
      if FileExists(LFileName) then
      begin
        LFileDataSet.Close;
        LFileDataSet.FileName  := LFileName;
        LFileDataSet.ReadOnly := False;
        LFileDataSet.StoreDefs := True;
        LFileDataSet.Open;
        LFileDataSet.LogChanges := False;
        while not LFileDataSet.Eof do
        begin
          LStrData := Trim(LFileDataSet.FieldByName('GaugePathName').AsString);
          LStrData := ExtractFileName(LStrData);
          LFileDataSet.Edit;
          LFileDataSet.FieldByName('GaugePathName').AsString := LStrData;
          LFileDataSet.Post;
          LFileDataSet.Next;
        end;
        LFileDataSet.SaveToFile(LFileName, dfXML);
        lFileDataSet.Close;
      end;

      LFileName := ADirectory + 'LD_RunParameters.xml';
      if FileExists(LFileName) then
      begin
        LFileDataSet.Close;
        LFileDataSet.FileName  := LFileName;
        LFileDataSet.ReadOnly := False;
        LFileDataSet.StoreDefs := True;
        LFileDataSet.Open;
        LFileDataSet.LogChanges := False;
        while not LFileDataSet.Eof do
        begin
          LStrData := Trim(LFileDataSet.FieldByName('ParamFile').AsString);
          LStrData := ExtractFileName(LStrData);
          LFileDataSet.Edit;
          LFileDataSet.FieldByName('ParamFile').AsString := LStrData;
          LFileDataSet.Post;
          LFileDataSet.Next;
        end;
        LFileDataSet.SaveToFile(LFileName, dfXML);
        lFileDataSet.Close;
      end;

      LFileName := ADirectory + 'LD_SpecifiedDemandFeature.xml';
      if FileExists(LFileName) then
      begin
        LFileDataSet.Close;
        LFileDataSet.FileName  := LFileName;
        LFileDataSet.ReadOnly := False;
        LFileDataSet.StoreDefs := True;
        LFileDataSet.Open;
        LFileDataSet.LogChanges := False;
        while not LFileDataSet.Eof do
        begin
          LStrData := Trim(LFileDataSet.FieldByName('Fullname').AsString);
          LStrData := ExtractFileName(LStrData);
          LFileDataSet.Edit;
          LFileDataSet.FieldByName('Fullname').AsString := LStrData;
          LFileDataSet.Post;
          LFileDataSet.Next;
        end;
        LFileDataSet.SaveToFile(LFileName, dfXML);
        lFileDataSet.Close;
      end;

    finally
      lFileDataSet.Free;
    end;

    lNewDataSet := TClientDataSet.Create(nil);
    try
      lNewDataSet.ReadOnly  := False;
      lNewDataSet.StoreDefs := True;
      lNewDataSet.FileName  := ADirectory +  'LD_WRYMDat.xml';
      lNewDataSet.Open;
      lNewDataSet.LogChanges := False;
      while not lNewDataSet.Eof do
      begin
        lNewDataSet.Edit;
        if(LHydrologyPath = '') then
          lNewDataSet.FieldByName('HydrologyPath').Clear
        else
          lNewDataSet.FieldByName('HydrologyPath').AsString       := LHydrologyPath;

        if(LSpecifiedDemandPath = '') then
          lNewDataSet.FieldByName('SpecifiedDemandPath').Clear
        else
          lNewDataSet.FieldByName('SpecifiedDemandPath').AsString := LSpecifiedDemandPath;
        lNewDataSet.Post;
        lNewDataSet.Next;
      end;
      lNewDataSet.SaveToFile(ADirectory + 'LD_WRYMDat.xml', dfXML);
      lNewDataSet.Close;
      Result := TRUE;
    finally
      lNewDataSet.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function Upgrade_PhysicalFlowConstraints_2_18_0 (AAppModules: TAppModules;ADirectory : string; var AFileName  : string) : boolean;
const OPNAME = 'Upgrade_PhysicalFlowConstraints_2_18_0';
var
  LConstraintDataset    : TClientDataSet;
  LOldValuesDataset,
  LNewValuesDataset     : TClientDataSet;
  LDataSet              : TAbstractModelDataset;
  LFromFieldName,
  LToFieldName,
  LSQL                  : string;
  LDataSetProvider      : TDataSetProvider;
  LMasterSource         : TDataSource;
  LIndex                : integer;
  LMonthlyAverageInflows      : array[1..10] of Double;
  LGroupNumber          : integer;
  LSubGroupNumber       : integer;
  LStructureType        : integer;
  LLineNumber           : integer;
begin
  Result := FALSE;
  try
    AFileName := 'LD_FlowConstraintsValue.xml';
    if FileExists(ADirectory + 'LD_FlowConstraints.xml') then
    begin
      LConstraintDataset := TClientDataSet.Create(nil);
      try
        LConstraintDataset.FileName := ADirectory + 'LD_FlowConstraints.xml';
        LConstraintDataset.Open;
        if(LConstraintDataset.RecordCount > 0) then
        begin
          AAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
          try
            LSQL := 'SELECT * FROM FlowConstraintsValue WHERE Model IS NULL';
            LDataSet.SetSQL(LSQL);
            LDataset.DataSet.Open;
            LDataSetProvider := TDataSetProvider.Create(nil);
            LNewValuesDataset := TClientDataSet.Create(nil);
            try
              LDataSetProvider.DataSet := LDataset.DataSet;
              LNewValuesDataset.ReadOnly := True;
              LNewValuesDataset.SetProvider(LDataSetProvider);
              LNewValuesDataset.StoreDefs := True;
              LNewValuesDataset.Open;
              LNewValuesDataset.SaveToFile(ADirectory + AFileName,dfXML);
            finally
              LDataset.DataSet.Close;
              LNewValuesDataset.Close;
              LNewValuesDataset.Free;
              LDataSetProvider.Free;
            end;
          finally
            LDataset.Free;
          end;

          if FileExists(ADirectory + AFileName) then
          begin
            LMasterSource := TDataSource.Create(nil);
            LNewValuesDataset := TClientDataSet.Create(nil);
            try
              LNewValuesDataset.StoreDefs := True;
              LMasterSource.DataSet := LConstraintDataset;
              LNewValuesDataset.LoadFromFile(ADirectory + AFileName);
              LNewValuesDataset.ReadOnly := False;

              //_________________________LD_FlowConstraintsDeltaH.xml_______________________________________________
              if FileExists(ADirectory + 'LD_FlowConstraintsDeltaH.xml') then
              begin
                LOldValuesDataset := TClientDataSet.Create(nil);
                try
                  LOldValuesDataset.FileName        := ADirectory + 'LD_FlowConstraintsDeltaH.xml';
                  LOldValuesDataset.MasterSource    := LMasterSource;
                  LOldValuesDataset.IndexFieldNames := 'Model;StudyAreaName;SubArea;Scenario;Identifier';
                  LOldValuesDataset.MasterFields    := 'Model;StudyAreaName;SubArea;Scenario;Identifier';
                  LOldValuesDataset.Open;

                  LConstraintDataset.First;
                  while not LConstraintDataset.Eof do
                  begin
                    if not (LOldValuesDataset.Eof and LOldValuesDataset.Bof) then
                    begin
                      LOldValuesDataset.First;
                      while not LOldValuesDataset.Eof do
                      begin
                        LNewValuesDataset.Insert;
                        LNewValuesDataset.FieldByName('Model').AsString           := Trim(LConstraintDataset.FieldByName('Model').AsString);
                        LNewValuesDataset.FieldByName('StudyAreaName').AsString   := Trim(LConstraintDataset.FieldByName('StudyAreaName').AsString);
                        LNewValuesDataset.FieldByName('SubArea').AsString         := Trim(LConstraintDataset.FieldByName('SubArea').AsString);
                        LNewValuesDataset.FieldByName('Scenario').AsString        := Trim(LConstraintDataset.FieldByName('Scenario').AsString);
                        LNewValuesDataset.FieldByName('Identifier').AsInteger     := LConstraintDataset.FieldByName('Identifier').AsInteger;
                        LNewValuesDataset.FieldByName('GroupNumber').AsInteger    := pfcgSubmergedOutlet;
                        LNewValuesDataset.FieldByName('SubGroupNumber').AsInteger := pfcstElevationDifference;
                        LNewValuesDataset.FieldByName('LineNumber').AsInteger     := 0;

                        for LIndex := 1 to 10 do
                        begin
                          LFromFieldName := Format('DeltaH%2.2d',[LIndex]);
                          LToFieldName   := Format('Value%2.2d',[LIndex]);
                          LNewValuesDataset.FieldByName(LToFieldName).AsFloat := LOldValuesDataset.FieldByName(LFromFieldName).AsFloat;
                        end;
                        LNewValuesDataset.Post;
                        LOldValuesDataset.Next;
                      end;
                    end;
                    LConstraintDataset.Next;
                  end;
                finally
                  LOldValuesDataset.Close;
                  LOldValuesDataset.Free;
                end;
              end;

              //_________________________LD_FlowConstraintsType11Flow.xml_______________________________________________
              if FileExists(ADirectory + 'LD_FlowConstraintsType11Flow.xml') then
              begin
                LOldValuesDataset := TClientDataSet.Create(nil);
                try
                  LOldValuesDataset.FileName        := ADirectory + 'LD_FlowConstraintsType11Flow.xml';
                  LOldValuesDataset.MasterSource    := LMasterSource;
                  LOldValuesDataset.IndexFieldNames := 'Model;StudyAreaName;SubArea;Scenario;Identifier';
                  LOldValuesDataset.MasterFields    := 'Model;StudyAreaName;SubArea;Scenario;Identifier';
                  LOldValuesDataset.Open;

                  LConstraintDataset.First;
                  while not LConstraintDataset.Eof do
                  begin
                    if not (LOldValuesDataset.Eof and LOldValuesDataset.Bof) then
                    begin
                      LOldValuesDataset.First;
                      while not LOldValuesDataset.Eof do
                      begin
                        LNewValuesDataset.Insert;
                        LNewValuesDataset.FieldByName('Model').AsString            := Trim(LConstraintDataset.FieldByName('Model').AsString);
                        LNewValuesDataset.FieldByName('StudyAreaName').AsString    := Trim(LConstraintDataset.FieldByName('StudyAreaName').AsString);
                        LNewValuesDataset.FieldByName('SubArea').AsString          := Trim(LConstraintDataset.FieldByName('SubArea').AsString);
                        LNewValuesDataset.FieldByName('Scenario').AsString         := Trim(LConstraintDataset.FieldByName('Scenario').AsString);
                        LNewValuesDataset.FieldByName('Identifier').AsInteger      := LConstraintDataset.FieldByName('Identifier').AsInteger;
                        LNewValuesDataset.FieldByName('GroupNumber').AsInteger     := pfcgSandAquifer;
                        LNewValuesDataset.FieldByName('SubGroupNumber').AsInteger  := pfcstDownStreamNodeInflow;
                        LNewValuesDataset.FieldByName('LineNumber').AsInteger      := 0;

                        for LIndex := 1 to 10 do
                        begin
                          LFromFieldName := Format('FlowValue%2.2d',[LIndex]);
                          LToFieldName   := Format('Value%2.2d',[LIndex]);
                          LNewValuesDataset.FieldByName(LToFieldName).AsFloat := LOldValuesDataset.FieldByName(LFromFieldName).AsFloat;
                        end;
                        LNewValuesDataset.Post;
                        LOldValuesDataset.Next;
                      end;
                    end;
                    LConstraintDataset.Next;
                  end;
                finally
                  LOldValuesDataset.Close;
                  LOldValuesDataset.Free;
                end;
              end;

              //_________________________LD_FlowConstraintsType11Depth.xml_______________________________________________
              if FileExists(ADirectory + 'LD_FlowConstraintsType11Depth.xml') then
              begin
                LOldValuesDataset := TClientDataSet.Create(nil);
                try
                  LOldValuesDataset.FileName        := ADirectory + 'LD_FlowConstraintsType11Depth.xml';
                  LOldValuesDataset.MasterSource    := LMasterSource;
                  LOldValuesDataset.IndexFieldNames := 'Model;StudyAreaName;SubArea;Scenario;Identifier';
                  LOldValuesDataset.MasterFields    := 'Model;StudyAreaName;SubArea;Scenario;Identifier';
                  LOldValuesDataset.Open;

                  LConstraintDataset.First;
                  while not LConstraintDataset.Eof do
                  begin
                    if not (LOldValuesDataset.Eof and LOldValuesDataset.Bof) then
                    begin
                      LOldValuesDataset.First;
                      while not LOldValuesDataset.Eof do
                      begin
                        LNewValuesDataset.Insert;
                        LNewValuesDataset.FieldByName('Model').AsString           := Trim(LConstraintDataset.FieldByName('Model').AsString);
                        LNewValuesDataset.FieldByName('StudyAreaName').AsString   := Trim(LConstraintDataset.FieldByName('StudyAreaName').AsString);
                        LNewValuesDataset.FieldByName('SubArea').AsString         := Trim(LConstraintDataset.FieldByName('SubArea').AsString);
                        LNewValuesDataset.FieldByName('Scenario').AsString        := Trim(LConstraintDataset.FieldByName('Scenario').AsString);
                        LNewValuesDataset.FieldByName('Identifier').AsInteger     := LConstraintDataset.FieldByName('Identifier').AsInteger;
                        LNewValuesDataset.FieldByName('GroupNumber').AsInteger    := pfcgSandAquifer;
                        LNewValuesDataset.FieldByName('SubGroupNumber').AsInteger := pfcstRiverDepth;
                        LNewValuesDataset.FieldByName('LineNumber').AsInteger     := 0;

                        for LIndex := 1 to 10 do
                        begin
                          LFromFieldName := Format('DepthValue%2.2d',[LIndex]);
                          LToFieldName   := Format('Value%2.2d',[LIndex]);
                          LNewValuesDataset.FieldByName(LToFieldName).AsFloat := LOldValuesDataset.FieldByName(LFromFieldName).AsFloat;
                        end;
                        LNewValuesDataset.Post;
                        LOldValuesDataset.Next;
                      end;
                    end;
                    LConstraintDataset.Next;
                  end;
                finally
                  LOldValuesDataset.Close;
                  LOldValuesDataset.Free;
                end;
              end;

              //_________________________LD_FlowConstraintsDiversion.xml_______________________________________________
              if FileExists(ADirectory + 'LD_FlowConstraintsDiversion.xml') then
              begin
                LOldValuesDataset := TClientDataSet.Create(nil);
                try
                  LOldValuesDataset.FileName        := ADirectory + 'LD_FlowConstraintsDiversion.xml';
                  LOldValuesDataset.MasterSource    := LMasterSource;
                  LOldValuesDataset.IndexFieldNames := 'Model;StudyAreaName;SubArea;Scenario;Identifier';
                  LOldValuesDataset.MasterFields    := 'Model;StudyAreaName;SubArea;Scenario;Identifier';
                  LOldValuesDataset.Open;

                  LConstraintDataset.First;
                  while not LConstraintDataset.Eof do
                  begin
                    if not (LOldValuesDataset.Eof and LOldValuesDataset.Bof) then
                    begin
                      LOldValuesDataset.First;
                      for LIndex := 1 to 10 do
                        LMonthlyAverageInflows[LIndex] := 0.0;

                      LLineNumber := 0;
                      while not LOldValuesDataset.Eof do
                      begin
                        LNewValuesDataset.Insert;
                        LLineNumber := LLineNumber + 1;
                        LNewValuesDataset.FieldByName('Model').AsString           := Trim(LConstraintDataset.FieldByName('Model').AsString);
                        LNewValuesDataset.FieldByName('StudyAreaName').AsString   := Trim(LConstraintDataset.FieldByName('StudyAreaName').AsString);
                        LNewValuesDataset.FieldByName('SubArea').AsString         := Trim(LConstraintDataset.FieldByName('SubArea').AsString);
                        LNewValuesDataset.FieldByName('Scenario').AsString        := Trim(LConstraintDataset.FieldByName('Scenario').AsString);
                        LNewValuesDataset.FieldByName('Identifier').AsInteger     := LConstraintDataset.FieldByName('Identifier').AsInteger;
                        LNewValuesDataset.FieldByName('GroupNumber').AsInteger    := pfcgSubmergedOutlet;
                        LNewValuesDataset.FieldByName('SubGroupNumber').AsInteger := pfcstMonthlyAverageDivertedFlow;
                        LNewValuesDataset.FieldByName('LineNumber').AsInteger     := LLineNumber;

                        LMonthlyAverageInflows[LLineNumber] := LOldValuesDataset.FieldByName('DFlow01').AsFloat;

                        for LIndex := 2 to 11 do
                        begin
                          LFromFieldName := Format('DFlow%2.2d',[LIndex]);
                          LToFieldName   := Format('Value%2.2d',[LIndex-1]);
                          LNewValuesDataset.FieldByName(LToFieldName).AsFloat := LOldValuesDataset.FieldByName(LFromFieldName).AsFloat;
                        end;
                        LNewValuesDataset.Post;
                        LOldValuesDataset.Next;
                      end;
                      LNewValuesDataset.Insert;
                      LNewValuesDataset.FieldByName('Model').AsString           := Trim(LConstraintDataset.FieldByName('Model').AsString);
                      LNewValuesDataset.FieldByName('StudyAreaName').AsString   := Trim(LConstraintDataset.FieldByName('StudyAreaName').AsString);
                      LNewValuesDataset.FieldByName('SubArea').AsString         := Trim(LConstraintDataset.FieldByName('SubArea').AsString);
                      LNewValuesDataset.FieldByName('Scenario').AsString        := Trim(LConstraintDataset.FieldByName('Scenario').AsString);
                      LNewValuesDataset.FieldByName('Identifier').AsInteger     := LConstraintDataset.FieldByName('Identifier').AsInteger;
                      LNewValuesDataset.FieldByName('GroupNumber').AsInteger    := pfcgSubmergedOutlet;
                      LNewValuesDataset.FieldByName('SubGroupNumber').AsInteger := pfcstMonthlyAverageInflow;
                      LNewValuesDataset.FieldByName('LineNumber').AsInteger     := 0;

                      for LIndex := 1 to 10 do
                      begin
                        LToFieldName   := Format('Value%2.2d',[LIndex]);
                        LNewValuesDataset.FieldByName(LToFieldName).AsFloat := LMonthlyAverageInflows[LIndex];
                      end;
                      LNewValuesDataset.Post;
                    end;
                    LConstraintDataset.Next;
                  end;
                finally
                  LOldValuesDataset.Close;
                  LOldValuesDataset.Free;
                end;
              end;

              //_________________________LD_FlowConstraintsElevation.xml_______________________________________________
              if FileExists(ADirectory + 'LD_FlowConstraintsElevation.xml') then
              begin
                LOldValuesDataset := TClientDataSet.Create(nil);
                try
                  LOldValuesDataset.FileName        := ADirectory + 'LD_FlowConstraintsElevation.xml';
                  LOldValuesDataset.MasterSource    := LMasterSource;
                  LOldValuesDataset.IndexFieldNames := 'Model;StudyAreaName;SubArea;Scenario;Identifier';
                  LOldValuesDataset.MasterFields    := 'Model;StudyAreaName;SubArea;Scenario;Identifier';
                  LOldValuesDataset.Open;

                  LConstraintDataset.First;
                  while not LConstraintDataset.Eof do
                  begin
                    if not (LOldValuesDataset.Eof and LOldValuesDataset.Bof) then
                    begin
                      LOldValuesDataset.First;
                      while not LOldValuesDataset.Eof do
                      begin
                        LNewValuesDataset.Insert;
                        LNewValuesDataset.FieldByName('Model').AsString         := Trim(LConstraintDataset.FieldByName('Model').AsString);
                        LNewValuesDataset.FieldByName('StudyAreaName').AsString := Trim(LConstraintDataset.FieldByName('StudyAreaName').AsString);
                        LNewValuesDataset.FieldByName('SubArea').AsString       := Trim(LConstraintDataset.FieldByName('SubArea').AsString);
                        LNewValuesDataset.FieldByName('Scenario').AsString      := Trim(LConstraintDataset.FieldByName('Scenario').AsString);
                        LNewValuesDataset.FieldByName('Identifier').AsInteger   := LConstraintDataset.FieldByName('Identifier').AsInteger;
                        LGroupNumber          := pfcgNone;
                        LSubGroupNumber       := pfcstNone;
                        LStructureType        := LConstraintDataset.FieldByName('StructureType').AsInteger;
                        case LStructureType of
                          4,5,7,8,9:
                          begin
                            LGroupNumber    := pfcgDischargeCurve;
                            LSubGroupNumber := pfcstElevation;
                          end;
                          10:
                          begin
                            LGroupNumber    := pfcgKFactors;
                            LSubGroupNumber := pfcstChannelNumber;
                          end;
                          11:
                          begin
                            LGroupNumber    := pfcgSandAquifer;
                            LSubGroupNumber := pfcstHeadDifference;
                          end;
                          12:
                          begin
                            LGroupNumber    := pfcgSubmergedOutlet;
                            LSubGroupNumber := pfcstElevationDifference;
                          end;
                          13:
                          begin
                            LGroupNumber    := pfcgPumpStation;
                            LSubGroupNumber := pfcstPumpingHead;
                          end;
                        end;//case

                        LNewValuesDataset.FieldByName('GroupNumber').AsInteger    := LGroupNumber;
                        LNewValuesDataset.FieldByName('SubGroupNumber').AsInteger := LSubGroupNumber;
                        LNewValuesDataset.FieldByName('LineNumber').AsInteger     := 0;


                        for LIndex := 1 to 10 do
                        begin
                          LFromFieldName := Format('ConstraintsElev%2.2d',[LIndex]);
                          LToFieldName   := Format('Value%2.2d',[LIndex]);
                          LNewValuesDataset.FieldByName(LToFieldName).AsFloat := LOldValuesDataset.FieldByName(LFromFieldName).AsFloat;
                        end;
                        LNewValuesDataset.Post;
                        LOldValuesDataset.Next;
                      end;
                    end;
                    LConstraintDataset.Next;
                  end;
                finally
                  LOldValuesDataset.Close;
                  LOldValuesDataset.Free;
                end;
              end;

              //_________________________LD_FlowConstraintsDischarge.xml_______________________________________________
              if FileExists(ADirectory + 'LD_FlowConstraintsDischarge.xml') then
              begin
                LOldValuesDataset := TClientDataSet.Create(nil);
                try
                  LOldValuesDataset.FileName        := ADirectory + 'LD_FlowConstraintsDischarge.xml';
                  LOldValuesDataset.MasterSource    := LMasterSource;
                  LOldValuesDataset.IndexFieldNames := 'Model;StudyAreaName;SubArea;Scenario;Identifier';
                  LOldValuesDataset.MasterFields    := 'Model;StudyAreaName;SubArea;Scenario;Identifier';
                  LOldValuesDataset.Open;

                  LConstraintDataset.First;
                  while not LConstraintDataset.Eof do
                  begin
                    if not (LOldValuesDataset.Eof and LOldValuesDataset.Bof) then
                    begin
                      LOldValuesDataset.First;
                      while not LOldValuesDataset.Eof do
                      begin
                        LNewValuesDataset.Insert;
                        LNewValuesDataset.FieldByName('Model').AsString          := Trim(LConstraintDataset.FieldByName('Model').AsString);
                        LNewValuesDataset.FieldByName('StudyAreaName').AsString  := Trim(LConstraintDataset.FieldByName('StudyAreaName').AsString);
                        LNewValuesDataset.FieldByName('SubArea').AsString        := Trim(LConstraintDataset.FieldByName('SubArea').AsString);
                        LNewValuesDataset.FieldByName('Scenario').AsString       := Trim(LConstraintDataset.FieldByName('Scenario').AsString);
                        LNewValuesDataset.FieldByName('Identifier').AsInteger    := LConstraintDataset.FieldByName('Identifier').AsInteger;
                        LGroupNumber          := pfcgNone;
                        LSubGroupNumber       := pfcstNone;
                        LStructureType        := LConstraintDataset.FieldByName('StructureType').AsInteger;
                        case LStructureType of
                          4,5,7,8,9:
                          begin
                            LGroupNumber    := pfcgDischargeCurve;
                            LSubGroupNumber := pfcstDischarge;
                          end;
                          10:
                          begin
                            LGroupNumber    := pfcgKFactors;
                            LSubGroupNumber := pfcstKFactor;
                          end;
                          11:
                          begin
                            LGroupNumber    := pfcgSandAquifer;
                            LSubGroupNumber := pfcstAquiferFlow;
                          end;
                          12:
                          begin
                            LGroupNumber    := pfcgSubmergedOutlet;
                            LSubGroupNumber := pfcstMonthlyAverageInflow;
                          end;
                          13:
                          begin
                            LGroupNumber    := pfcgPumpStation;
                            LSubGroupNumber := pfcstPumpingDischarge;
                          end;
                        end;//case

                        LNewValuesDataset.FieldByName('GroupNumber').AsInteger    := LGroupNumber;
                        LNewValuesDataset.FieldByName('SubGroupNumber').AsInteger := LSubGroupNumber;
                        LNewValuesDataset.FieldByName('LineNumber').AsInteger     := 0;


                        for LIndex := 1 to 10 do
                        begin
                          LFromFieldName := Format('Disch%2.2d',[LIndex]);
                          LToFieldName   := Format('Value%2.2d',[LIndex]);
                          LNewValuesDataset.FieldByName(LToFieldName).AsFloat := LOldValuesDataset.FieldByName(LFromFieldName).AsFloat;
                        end;
                        LNewValuesDataset.Post;
                        LOldValuesDataset.Next;
                      end;
                    end;
                    LConstraintDataset.Next;
                  end;
                finally
                  LOldValuesDataset.Close;
                  LOldValuesDataset.Free;
                end;
              end;

              LNewValuesDataset.SaveToFile(ADirectory + AFileName,dfXML);
            finally
              LNewValuesDataset.Close;
              LNewValuesDataset.Free;
              LMasterSource.Free;
            end;
          end;
          Result := TRUE;
        end;
      finally
        LConstraintDataset.Close;
        LConstraintDataset.Free;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
