{******************************************************************************}
{*  UNIT      : Contains function to upgrade database to WRMF 2.19.0         *}
{*  AUTHOR    : Presley Mudau                                                 *}
{*  DATE      : 2006/05/02                                                    *}
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UUpgrade_2_19;

interface

uses
  Classes,
  SysUtils,
  UAbstractObject;



function UpgradeToVersion_2_19_0 (AAppModules: TAppModules;
                                  ADirectory             : string;
                                  AProgressUpdateFuntion : TProgressUpdateFuntion;
                                  AFileNames             : TStringList;
                                  var AVersion           : string) : boolean;

function Upgrade_ChangeLists_2_19_0 (AAppModules: TAppModules;
                                     ADirectory             : string;
                                     AProgressUpdateFuntion : TProgressUpdateFuntion;
                                     AFileNames             : TStringList) : boolean;
function UpgradeTableTo_2_19_0(ATableName: string;
                               AAppModules: TAppModules;
                               ADirectory             : string;
                               AProgressUpdateFuntion : TProgressUpdateFuntion;
                               AFileNames             : TStringList) : boolean;

function UpgradeToVersion_2_23_0 (AAppModules: TAppModules;
                                  ADirectory             : string;
                                  AProgressUpdateFuntion : TProgressUpdateFuntion;
                                  AFileNames             : TStringList;
                                  var AVersion           : string) : boolean;
function  Upgrade_HydrologyFileData_2_23_0 (AAppModules: TAppModules;
                                     ADirectory             : string;
                                     AProgressUpdateFuntion : TProgressUpdateFuntion;
                                     AFileNames             : TStringList) : boolean;
function UpgradeTableTo_2_23_0(ATableName: string;
                               AAppModules: TAppModules;
                               ADirectory             : string;
                               AProgressUpdateFuntion : TProgressUpdateFuntion;
                               AFileNames             : TStringList) : boolean;

function UpgradeToVersion_3_4_1(AAppModules: TAppModules;
                                  ADirectory             : string;
                                  AProgressUpdateFuntion : TProgressUpdateFuntion;
                                  AFileNames             : TStringList;
                                  var AVersion           : string) : boolean;
function Upgrade_DailyDiversionRelationship_3_4_0(AAppModules: TAppModules;
                                     ADirectory             : string;
                                     AProgressUpdateFuntion : TProgressUpdateFuntion;
                                     AFileNames             : TStringList) : boolean;
function UpgradeTableTo_3_4_1(ATableName: string;
                               AAppModules: TAppModules;
                               ADirectory             : string;
                               AProgressUpdateFuntion : TProgressUpdateFuntion;
                               AFileNames             : TStringList) : boolean;
function UpgradeToVersion_3_7_1(AAppModules: TAppModules;
                                  ADirectory             : string;
                                  AProgressUpdateFuntion : TProgressUpdateFuntion;
                                  AFileNames             : TStringList;
                                  var AVersion           : string) : boolean;
function Upgrade_RainfallCatchment_3_7_1(AAppModules: TAppModules;
                                     ADirectory             : string;
                                     AProgressUpdateFuntion : TProgressUpdateFuntion;
                                     AFileNames             : TStringList) : boolean;
function UpgradeTableTo_3_7_1(ATableName: string;
                               AAppModules: TAppModules;
                               ADirectory             : string;
                               AProgressUpdateFuntion : TProgressUpdateFuntion;
                               AFileNames             : TStringList) : boolean;


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

function UpgradeToVersion_2_19_0 (AAppModules: TAppModules;
                                  ADirectory             : string;
                                  AProgressUpdateFuntion : TProgressUpdateFuntion;
                                  AFileNames             : TStringList;
                                  var AVersion           : string) : boolean;
const OPNAME = 'UpgradeToVersion_2_19_0';
var
  LStop: boolean;
begin
  Result := FALSE;
  try
    AProgressUpdateFuntion('Upgrading data to version 2.19.0.', ptNone, lStop);
    Upgrade_ChangeLists_2_19_0(AAppModules,ADirectory,AProgressUpdateFuntion,AFileNames);

    AFileNames[0] := 'TableVersion=2.19.0';
    AVersion := '2.19.0';
    Result   := TRUE;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function UpgradeToVersion_2_23_0 (AAppModules: TAppModules;
                                  ADirectory             : string;
                                  AProgressUpdateFuntion : TProgressUpdateFuntion;
                                  AFileNames             : TStringList;
                                  var AVersion           : string) : boolean;
const OPNAME = 'UpgradeToVersion_2_23_0';
var
  LStop: boolean;
begin
  Result := FALSE;
  try
    AProgressUpdateFuntion('Upgrading data to version 2.23.0.', ptNone, lStop);
    Upgrade_HydrologyFileData_2_23_0(AAppModules,ADirectory,AProgressUpdateFuntion,AFileNames);
    AFileNames[0] := 'TableVersion=2.23.0';
    AVersion := '2.23.0';
    Result   := TRUE;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function UpgradeTableTo_2_19_0(ATableName: string;
                               AAppModules: TAppModules;
                               ADirectory             : string;
                               AProgressUpdateFuntion : TProgressUpdateFuntion;
                               AFileNames             : TStringList) : boolean;
const OPNAME = 'UpgradeTableTo_2_19_0';
var
  LIndex         : integer;
  LDataSet       : TAbstractModelDataset;
  lNewDataSet    : TClientDataSet;
  LFileDataSet   : TClientDataSet;
  LFilename,
  LFieldName     : string;
  LDataSetProvider     : TDataSetProvider;
  lStop      : Boolean;
  LStudy     : string;
  LModel     : string;
  LSubArea   : string;
  LScenario  : string;
begin
  Result := FALSE;
  try
    AProgressUpdateFuntion('Upgrading ('+ ATableName +') data to version 2.19.0.', ptNone, lStop);
    LFilename := ADirectory +  'LD_StudyScenario.xml';
    if(AFileNames.IndexOf('LD_StudyScenario.xml') < 0) or (not FileExists(LFilename)) then
    begin
      AProgressUpdateFuntion('File ('+ LFilename +') not found. Upgrading of Change List tables not done', ptNone, lStop);
    end
    else
    begin
      LStudy     := '';
      LModel     := '';
      LSubArea   := '';
      LScenario  := '';
      LFileDataSet := TClientDataSet.Create(nil);
      try
        LFileDataSet.ReadOnly  := True;
        LFileDataSet.StoreDefs := True;
        LFileDataSet.FileName  := LFilename;
        LFileDataSet.Open;

        LStudy     := Trim(LFileDataSet.FieldByName('Model').AsString);
        LModel     := Trim(LFileDataSet.FieldByName('StudyAreaName').AsString);
        LSubArea   := Trim(LFileDataSet.FieldByName('SubArea').AsString);
        LScenario  := Trim(LFileDataSet.FieldByName('Scenario').AsString);
      finally
        LFileDataSet.Close;
        LFileDataSet.Free;
      end;

      LFilename := ADirectory +  'LD_'+ATableName+'.xml';
      if(AFileNames.IndexOf('LD_'+ATableName+'.xml') >= 0) and
        (FileExists(LFilename)) then
      begin
        AAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
        try
          LDataSet.SetSQL('SELECT * FROM '+ATableName+' WHERE Model IS NULL');
          LDataset.DataSet.Open;
          LDataSetProvider := TDataSetProvider.Create(nil);
          lNewDataSet := TClientDataSet.Create(nil);
          try
            LDataSetProvider.DataSet := LDataset.DataSet;
            lNewDataSet.ReadOnly := True;
            lNewDataSet.SetProvider(LDataSetProvider);
            lNewDataSet.StoreDefs := True;
            lNewDataSet.Open;
            lNewDataSet.SaveToFile(ADirectory +  'LD_'+ATableName+'_19.xml', dfXML);
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
          lNewDataSet.FileName  := ADirectory +  'LD_'+ATableName+'_19.xml';
          lNewDataSet.Open;

          LFileDataSet.ReadOnly  := True;
          LFileDataSet.StoreDefs := True;
          LFileDataSet.FileName  := LFilename;
          LFileDataSet.Open;

          while not LFileDataSet.Eof do
          begin
            lNewDataSet.Insert;
            for LIndex := 0 to LFileDataSet.FieldList.Count-1 do
            begin
              LFieldName := LFileDataSet.FieldList.Fields[LIndex].FieldName;
              if(lNewDataSet.FieldList.Find(LFieldName) <> nil) then
                lNewDataSet.FieldByName(LFieldName).Value := LFileDataSet.FieldByName(LFieldName).Value;
            end;
            lNewDataSet.FieldByName('Model').Value         := LStudy;
            lNewDataSet.FieldByName('StudyAreaName').Value := LModel;
            lNewDataSet.FieldByName('SubArea').Value       := LSubArea;
            lNewDataSet.FieldByName('Scenario').Value      := LScenario;

            lNewDataSet.Post;
            LFileDataSet.Next;
          end;
          lNewDataSet.SaveToFile(ADirectory +  'LD_'+ATableName+'_19.xml', dfXML);
          LFileDataSet.Close;
          lNewDataSet.Close;
          if SysUtils.DeleteFile(ADirectory +  'LD_'+ATableName+'.xml') then
          begin
            RenameFile(ADirectory +  'LD_'+ATableName+'_19.xml',ADirectory +  'LD_'+ATableName+'.xml');
          end;
        finally
          LFileDataSet.Free;
          lNewDataSet.Free;
        end;
      end;
      Result := True;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function Upgrade_ChangeLists_2_19_0 (AAppModules: TAppModules;
                                     ADirectory             : string;
                                     AProgressUpdateFuntion : TProgressUpdateFuntion;
                                     AFileNames             : TStringList) : boolean;
const OPNAME = 'Upgrade_ChangeLists_2_19_0';
begin
  Result := FALSE;
  try
    Result := UpgradeTableTo_2_19_0('ChangeGroup',AAppModules,ADirectory,AProgressUpdateFuntion,AFileNames);
    Result :=  Result and UpgradeTableTo_2_19_0('ChangeGroupElement',AAppModules,ADirectory,AProgressUpdateFuntion,AFileNames);
    Result :=  Result and UpgradeTableTo_2_19_0('ChangeList',AAppModules,ADirectory,AProgressUpdateFuntion,AFileNames);
    Result :=  Result and UpgradeTableTo_2_19_0('ChangeParameter',AAppModules,ADirectory,AProgressUpdateFuntion,AFileNames);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function UpgradeToVersion_3_4_1(AAppModules: TAppModules;
                                  ADirectory             : string;
                                  AProgressUpdateFuntion : TProgressUpdateFuntion;
                                  AFileNames             : TStringList;
                                  var AVersion           : string) : boolean;
const OPNAME = 'UpgradeToVersion_3_4_1';
var
  LStop: boolean;
begin
  Result := FALSE;
  try
    AProgressUpdateFuntion('Upgrading data to version 3.4.1', ptNone, lStop);
    Upgrade_DailyDiversionRelationship_3_4_0(AAppModules,ADirectory,AProgressUpdateFuntion,AFileNames);
    AFileNames[0] := 'TableVersion=3.4.1';
    AVersion := '3.4.1';
    Result   := TRUE;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function Upgrade_DailyDiversionRelationship_3_4_0(AAppModules: TAppModules;
                                     ADirectory             : string;
                                     AProgressUpdateFuntion : TProgressUpdateFuntion;
                                     AFileNames             : TStringList) : boolean;
const OPNAME = 'Upgrade_DailyDiversionRelationship_3_4_0';
begin
  Result := FALSE;
  try
    Result := UpgradeTableTo_3_4_1('DailyDiversionFlowRelationship',AAppModules,ADirectory,AProgressUpdateFuntion,AFileNames);
    Result := Result and UpgradeTableTo_3_4_1('DailyDiversionWRYMData',AAppModules,ADirectory,AProgressUpdateFuntion,AFileNames);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function UpgradeTableTo_3_4_1(ATableName: string;
                              AAppModules: TAppModules;
                              ADirectory             : string;
                              AProgressUpdateFuntion : TProgressUpdateFuntion;
                              AFileNames             : TStringList) : boolean;
const OPNAME = 'UpgradeTableTo_3_4_1';
var
  LIndex         : integer;
  LDataSet       : TAbstractModelDataset;
  LNewDataSet    : TClientDataSet;
  LFileDataSet   : TClientDataSet;
  LFilename,
  LFieldName     : string;
  LDataSetProvider     : TDataSetProvider;
  LStop      : Boolean;
begin
  Result := FALSE;
  try
    if(AFileNames.IndexOf('LD_'+ATableName+'.xml')<0) then
      Exit;
    LFilename := ADirectory +  'LD_'+ATableName+'.xml';
    AProgressUpdateFuntion('Upgrading ('+ ATableName +') data to version 3.4.1.', ptNone, lStop);
    if (not FileExists(LFilename)) then
      AProgressUpdateFuntion('File ('+ LFilename +') not found. Upgrading of Daily Diversion table not done', ptNone, lStop)
    else
    begin
      LFileDataSet := TClientDataSet.Create(nil);
      try
        LFileDataSet.ReadOnly  := True;
        LFileDataSet.StoreDefs := True;
        LFileDataSet.FileName  := LFilename;
        LFileDataSet.Open;
      finally
        LFileDataSet.Close;
        LFileDataSet.Free;
      end;
      if(AFileNames.IndexOf('LD_'+ATableName+'.xml') >= 0) and
        (FileExists(LFilename)) then
      begin
        AAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
        try
          LDataSet.SetSQL('SELECT * FROM '+ATableName+' WHERE Model IS NULL');
          LDataset.DataSet.Open;
          LDataSetProvider := TDataSetProvider.Create(nil);
          LNewDataSet := TClientDataSet.Create(nil);
          try
            LDataSetProvider.DataSet := LDataset.DataSet;
            LNewDataSet.ReadOnly := True;
            LNewDataSet.SetProvider(LDataSetProvider);
            LNewDataSet.StoreDefs := True;
            LNewDataSet.Open;
            LNewDataSet.SaveToFile(ADirectory +  'LD_'+ATableName+'_4.xml', dfXML);
          finally
            LDataset.DataSet.Close;
            LNewDataSet.Close;
            LNewDataSet.Free;
            LDataSetProvider.Free;
          end;
        finally
          LDataset.Free;
        end;
        LNewDataSet := TClientDataSet.Create(nil);
        LFileDataSet := TClientDataSet.Create(nil);
        try
          LNewDataSet.ReadOnly  := False;
          LNewDataSet.StoreDefs := True;
          LNewDataSet.FileName  := ADirectory +  'LD_'+ATableName+'_4.xml';
          LNewDataSet.Open;

          LFileDataSet.ReadOnly  := True;
          LFileDataSet.StoreDefs := True;
          LFileDataSet.FileName  := LFilename;
          LFileDataSet.Open;
          while not LFileDataSet.Eof do
          begin
            lNewDataSet.Insert;
            for LIndex := 0 to LFileDataSet.FieldList.Count-1 do
            begin
              LFieldName := LFileDataSet.FieldList.Fields[LIndex].FieldName;
              if(lNewDataSet.FieldList.Find(LFieldName) <> nil) then
                lNewDataSet.FieldByName(LFieldName).Value := LFileDataSet.FieldByName(LFieldName).Value;
            end;
            LNewDataSet.Post;
            LFileDataSet.Next;
          end;
          LNewDataSet.SaveToFile(ADirectory +  'LD_'+ATableName+'_4.xml', dfXML);
          LFileDataSet.Close;
          LNewDataSet.Close;
          if SysUtils.DeleteFile(ADirectory +  'LD_'+ATableName+'.xml') then
          begin
            RenameFile(ADirectory +  'LD_'+ATableName+'_4.xml',ADirectory +  'LD_'+ATableName+'.xml');
          end;
        finally
          LFileDataSet.Free;
          LNewDataSet.Free;
        end;
      end;
    Result := True;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function Upgrade_HydrologyFileData_2_23_0 (AAppModules: TAppModules;
                                     ADirectory             : string;
                                     AProgressUpdateFuntion : TProgressUpdateFuntion;
                                     AFileNames             : TStringList) : boolean;
const OPNAME = 'Upgrade_HydrologyFileData_2_23_0';
begin
  Result := FALSE;
  try
    Result := UpgradeTableTo_2_23_0('HydrologyFileData',AAppModules,ADirectory,AProgressUpdateFuntion,AFileNames);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function UpgradeTableTo_2_23_0(ATableName: string;
                               AAppModules: TAppModules;
                               ADirectory             : string;
                               AProgressUpdateFuntion : TProgressUpdateFuntion;
                               AFileNames             : TStringList) : boolean;
const OPNAME = 'UpgradeTableTo_2_23_0';
var
  LIndex         : integer;
  LDataSet       : TAbstractModelDataset;
  LNewDataSet    : TClientDataSet;
  LFileDataSet   : TClientDataSet;
  LFilename,
  LFieldName     : string;
  LDataSetProvider     : TDataSetProvider;
  LStop      : Boolean;
  LStudy     : string;
begin
  Result := FALSE;
  try
    AProgressUpdateFuntion('Upgrading ('+ ATableName +') data to version 2.23.0.', ptNone, lStop);
    LFilename := ADirectory +  'LD_StudyScenario.xml';
    if(AFileNames.IndexOf('LD_StudyScenario.xml') < 0) or (not FileExists(LFilename)) then
    begin
      AProgressUpdateFuntion('File ('+ LFilename +') not found. Upgrading of Hydrology File Data table not done', ptNone, lStop);
    end
    else
    begin
      LStudy     := '';
      LFileDataSet := TClientDataSet.Create(nil);
      try
        LFileDataSet.ReadOnly  := True;
        LFileDataSet.StoreDefs := True;
        LFileDataSet.FileName  := LFilename;
        LFileDataSet.Open;
        LStudy := Trim(LFileDataSet.FieldByName('StudyAreaName').AsString);
      finally
        LFileDataSet.Close;
        LFileDataSet.Free;
      end;
      LFilename := ADirectory +  'LD_'+ATableName+'.xml';
      if(AFileNames.IndexOf('LD_'+ATableName+'.xml') >= 0) and
        (FileExists(LFilename)) then
      begin
        AAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
        try
          LDataSet.SetSQL('SELECT * FROM '+ATableName+' WHERE StudyAreaName IS NULL');
          LDataset.DataSet.Open;
          LDataSetProvider := TDataSetProvider.Create(nil);
          LNewDataSet := TClientDataSet.Create(nil);
          try
            LDataSetProvider.DataSet := LDataset.DataSet;
            LNewDataSet.ReadOnly := True;
            LNewDataSet.SetProvider(LDataSetProvider);
            LNewDataSet.StoreDefs := True;
            LNewDataSet.Open;
            LNewDataSet.SaveToFile(ADirectory +  'LD_'+ATableName+'_23.xml', dfXML);
          finally
            LDataset.DataSet.Close;
            LNewDataSet.Close;
            LNewDataSet.Free;
            LDataSetProvider.Free;
          end;
        finally
          LDataset.Free;
        end;
        LNewDataSet := TClientDataSet.Create(nil);
        LFileDataSet := TClientDataSet.Create(nil);
        try
          LNewDataSet.ReadOnly  := False;
          LNewDataSet.StoreDefs := True;
          LNewDataSet.FileName  := ADirectory +  'LD_'+ATableName+'_23.xml';
          LNewDataSet.Open;

          LFileDataSet.ReadOnly  := True;
          LFileDataSet.StoreDefs := True;
          LFileDataSet.FileName  := LFilename;
          LFileDataSet.Open;
          while not LFileDataSet.Eof do
          begin
            lNewDataSet.Insert;
            for LIndex := 0 to LFileDataSet.FieldList.Count-1 do
            begin
              LFieldName := LFileDataSet.FieldList.Fields[LIndex].FieldName;
              if(lNewDataSet.FieldList.Find(LFieldName) <> nil) then
                lNewDataSet.FieldByName(LFieldName).Value := LFileDataSet.FieldByName(LFieldName).Value;
            end;
            LNewDataSet.FieldByName('StudyAreaName').Value := LStudy;
            LNewDataSet.Post;
            LFileDataSet.Next;
          end;
          LNewDataSet.SaveToFile(ADirectory +  'LD_'+ATableName+'_23.xml', dfXML);
          LFileDataSet.Close;
          LNewDataSet.Close;
          if SysUtils.DeleteFile(ADirectory +  'LD_'+ATableName+'.xml') then
          begin
            RenameFile(ADirectory +  'LD_'+ATableName+'_23.xml',ADirectory +  'LD_'+ATableName+'.xml');
          end;
        finally
          LFileDataSet.Free;
          LNewDataSet.Free;
        end;
      end;
      Result := True;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function UpgradeToVersion_3_7_1(AAppModules: TAppModules;
                                  ADirectory             : string;
                                  AProgressUpdateFuntion : TProgressUpdateFuntion;
                                  AFileNames             : TStringList;
                                  var AVersion           : string) : boolean;
const OPNAME = 'UpgradeToVersion_3_7_1';
var
  LStop: boolean;
begin
  Result := FALSE;
  try
    AProgressUpdateFuntion('Upgrading data to version 3.7.1', ptNone, lStop);
    Upgrade_RainfallCatchment_3_7_1(AAppModules,ADirectory,AProgressUpdateFuntion,AFileNames);
    AFileNames[0] := 'TableVersion=3.7.1';
    AVersion := '3.7.1';
    Result   := TRUE;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;



function Upgrade_RainfallCatchment_3_7_1(AAppModules: TAppModules;
                                     ADirectory             : string;
                                     AProgressUpdateFuntion : TProgressUpdateFuntion;
                                     AFileNames             : TStringList) : boolean;
const OPNAME = 'Upgrade_RainfallCatchment_3_7_1';
begin
  Result := FALSE;
  try
    Result := UpgradeTableTo_3_7_1('RainfallCatchmentFileDetail',AAppModules,ADirectory,AProgressUpdateFuntion,AFileNames);
    Result := Result and UpgradeTableTo_3_7_1('RainfallCatchmentSource',AAppModules,ADirectory,AProgressUpdateFuntion,AFileNames);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function UpgradeTableTo_3_7_1(ATableName: string;
                              AAppModules: TAppModules;
                              ADirectory             : string;
                              AProgressUpdateFuntion : TProgressUpdateFuntion;
                              AFileNames             : TStringList) : boolean;
const OPNAME = 'UpgradeTableTo_3_7_1';
var
  LIndex         : integer;
  LDataSet       : TAbstractModelDataset;
  LNewDataSet    : TClientDataSet;
  LFileDataSet   : TClientDataSet;
  LFilename,
  LFieldName     : string;
  LDataSetProvider     : TDataSetProvider;
  LStop      : Boolean;
  LCount : integer;
begin
  Result := FALSE;
  try
    if(AFileNames.IndexOf('LD_'+ATableName+'.xml')<0) then
      Exit;
    LFilename := ADirectory +  'LD_'+ATableName+'.xml';
    AProgressUpdateFuntion('Upgrading ('+ ATableName +') data to version 3.7.1.', ptNone, lStop);
    if (not FileExists(LFilename)) then
      AProgressUpdateFuntion('File ('+ LFilename +') not found. Upgrading of Rainfall Catchment table not done', ptNone, lStop)
    else
    begin
      LFileDataSet := TClientDataSet.Create(nil);
      try
        LFileDataSet.ReadOnly  := True;
        LFileDataSet.StoreDefs := True;
        LFileDataSet.FileName  := LFilename;
        LFileDataSet.Open;
      finally
        LFileDataSet.Close;
        LFileDataSet.Free;
      end;
      if(AFileNames.IndexOf('LD_'+ATableName+'.xml') >= 0) and
        (FileExists(LFilename)) then
      begin
        AAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
        try
          LDataSet.SetSQL('SELECT * FROM '+ATableName+' WHERE Model IS NULL');
          LDataset.DataSet.Open;
          LDataSetProvider := TDataSetProvider.Create(nil);
          LNewDataSet := TClientDataSet.Create(nil);
          try
            LDataSetProvider.DataSet := LDataset.DataSet;
            LNewDataSet.ReadOnly := True;
            LNewDataSet.SetProvider(LDataSetProvider);
            LNewDataSet.StoreDefs := True;
            LNewDataSet.Open;
            LNewDataSet.SaveToFile(ADirectory +  'LD_'+ATableName+'_7.xml', dfXML);
          finally
            LDataset.DataSet.Close;
            LNewDataSet.Close;
            LNewDataSet.Free;
            LDataSetProvider.Free;
          end;
        finally
          LDataset.Free;
        end;
        LNewDataSet := TClientDataSet.Create(nil);
        LFileDataSet := TClientDataSet.Create(nil);
        try
          LNewDataSet.ReadOnly  := False;
          LNewDataSet.StoreDefs := True;
          LNewDataSet.FileName  := ADirectory +  'LD_'+ATableName+'_7.xml';
          LNewDataSet.Open;

          LFileDataSet.ReadOnly  := True;
          LFileDataSet.StoreDefs := True;
          LFileDataSet.FileName  := LFilename;
          LFileDataSet.Open;
          LCount := 0;
          while not LFileDataSet.Eof do
          begin
            lNewDataSet.Insert;
            for LIndex := 0 to LFileDataSet.FieldList.Count-1 do
            begin
              LFieldName := LFileDataSet.FieldList.Fields[LIndex].FieldName;
              if(lNewDataSet.FieldList.Find(LFieldName) <> nil) then
                lNewDataSet.FieldByName(LFieldName).Value := LFileDataSet.FieldByName(LFieldName).Value;
            end;
            if(lNewDataSet.FieldList.Find('SPLITID') <> nil) then
            begin
              lNewDataSet.FieldByName('SPLITID').Value := LCount;
              LCount := LCount+1;
            end;
            if(lNewDataSet.FieldList.Find('SOURCEID') <> nil) then
            begin
              lNewDataSet.FieldByName('SOURCEID').Value := LCount;
              LCount := LCount+1;
            end;

            LNewDataSet.Post;
            LFileDataSet.Next;
          end;
          LNewDataSet.SaveToFile(ADirectory +  'LD_'+ATableName+'_7.xml', dfXML);
          LFileDataSet.Close;
          LNewDataSet.Close;
          if SysUtils.DeleteFile(ADirectory +  'LD_'+ATableName+'.xml') then
          begin
            RenameFile(ADirectory +  'LD_'+ATableName+'_7.xml',ADirectory +  'LD_'+ATableName+'.xml');
          end;
        finally
          LFileDataSet.Free;
          LNewDataSet.Free;
        end;
      end;
    Result := True;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


end.
