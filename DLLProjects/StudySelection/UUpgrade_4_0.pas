{******************************************************************************}
{*  UNIT      : Contains function to upgrade database to WRMF 2.19.0         *}
{*  AUTHOR    : Presley Mudau                                                 *}
{*  DATE      : 2006/05/02                                                    *}
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UUpgrade_4_0;

interface

uses
  Classes,
  SysUtils,
  UAbstractObject;



function UpgradeToVersion_4_0_0 (AAppModules: TAppModules;
                                  ADirectory             : string;
                                  AProgressUpdateFuntion : TProgressUpdateFuntion;
                                  AFileNames             : TStringList;
                                  var AVersion           : string) : boolean;

function Upgrade_ReservoirLevels_4_0_0 (AAppModules: TAppModules;
                                        ADirectory             : string;
                                        AProgressUpdateFuntion : TProgressUpdateFuntion;
                                        AFileNames             : TStringList) : boolean;


implementation

uses
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

function UpgradeToVersion_4_0_0 (AAppModules: TAppModules;
                                  ADirectory             : string;
                                  AProgressUpdateFuntion : TProgressUpdateFuntion;
                                  AFileNames             : TStringList;
                                  var AVersion           : string) : boolean;
const OPNAME = 'UpgradeToVersion_4_0_0';
var
  LStop: boolean;
begin
  Result := FALSE;
  try
    AProgressUpdateFuntion('Upgrading data to version 4.0.0.', ptNone, lStop);
    Upgrade_ReservoirLevels_4_0_0(AAppModules,ADirectory,AProgressUpdateFuntion,AFileNames);

    AFileNames[0] := 'TableVersion=4.0.0';
    AVersion := '4.0.0';
    Result   := TRUE;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function Upgrade_ReservoirLevels_4_0_0 (AAppModules            : TAppModules;
                                        ADirectory             : string;
                                        AProgressUpdateFuntion : TProgressUpdateFuntion;
                                        AFileNames             : TStringList) : boolean;
const OPNAME = 'Upgrade_ReservoirLevels_4_0_0';

var
  LFileName    : string;
  lOldDataSet : TClientDataSet;
  lNewDataSet : TClientDataSet;
  lFieldDef   : TFieldDef;
  LStudy     : string;
  LModel     : string;
  LSubArea   : string;
  LScenario  : string;
  LRecordID  : integer;
  LRecordIdentifierCreated : boolean;
  LResLevelsCommentCreated : boolean;
begin
  Result := FALSE;
  try
    LFileName := 'LD_ReservoirLevels.xml';
    if(AFileNames.IndexOf(LFileName) >= 0) then
    begin
      lOldDataSet := TClientDataSet.Create(nil);
      lNewDataSet := TClientDataSet.Create(nil);
      try
        lOldDataSet.FileName := ADirectory + LFileName;
        lOldDataSet.Active   := TRUE;

        lNewDataSet.FieldDefs.Assign(lOldDataSet.FieldDefs);
        LRecordIdentifierCreated := False;
        LResLevelsCommentCreated := False;

        if(lNewDataSet.FieldDefs.IndexOf('RecordIdentifier') < 0) then
        begin
          lFieldDef := lNewDataSet.FieldDefs.AddFieldDef;
          with lFieldDef do
          begin
            Name     := 'RecordIdentifier';
            DataType := ftInteger;
            Required := TRUE;
          end;
          LRecordIdentifierCreated := True;
        end;

        if(lNewDataSet.FieldDefs.IndexOf('ResLevelsComment') < 0) then
        begin
          lFieldDef := lNewDataSet.FieldDefs.AddFieldDef;
          with lFieldDef do
          begin
            Name     := 'ResLevelsComment';
            DataType := ftString;
            Size     := 50;
            Required := False;
          end;
          LResLevelsCommentCreated := True;
        end;

        if(LRecordIdentifierCreated or LResLevelsCommentCreated) then
        begin
          lNewDataSet.CreateDataSet;
          lNewDataSet.Active := TRUE;

          lOldDataSet.First;
          LStudy     := Trim(lOldDataSet.FieldByName('StudyAreaName').AsString);
          LModel     := Trim(lOldDataSet.FieldByName('Model').AsString);
          LSubArea   := Trim(lOldDataSet.FieldByName('SubArea').AsString);
          LScenario  := Trim(lOldDataSet.FieldByName('Scenario').AsString);
          LRecordID  := 0;

          while (NOT lOldDataSet.Eof) do
          begin
            if(LStudy     <> Trim(lOldDataSet.FieldByName('StudyAreaName').AsString)) or
              (LModel     <> Trim(lOldDataSet.FieldByName('Model').AsString)) or
              (LSubArea   <> Trim(lOldDataSet.FieldByName('SubArea').AsString)) or
              (LScenario  <> Trim(lOldDataSet.FieldByName('Scenario').AsString)) then
            begin
              LStudy     := Trim(lOldDataSet.FieldByName('StudyAreaName').AsString);
              LModel     := Trim(lOldDataSet.FieldByName('Model').AsString);
              LSubArea   := Trim(lOldDataSet.FieldByName('SubArea').AsString);
              LScenario  := Trim(lOldDataSet.FieldByName('Scenario').AsString);
              LRecordID  := 0;
            end;
            LRecordID  := LRecordID + 1;


            lNewDataSet.Insert;
            lNewDataSet.FieldByName('Model').AsString                 := Trim(lOldDataSet.FieldByName('Model').AsString);
            lNewDataSet.FieldByName('StudyAreaName').AsString         := Trim(lOldDataSet.FieldByName('StudyAreaName').AsString);
            lNewDataSet.FieldByName('SubArea').AsString               := Trim(lOldDataSet.FieldByName('SubArea').AsString);
            lNewDataSet.FieldByName('Scenario').AsString              := Trim(lOldDataSet.FieldByName('Scenario').AsString);
            if LRecordIdentifierCreated  then
              lNewDataSet.FieldByName('RecordIdentifier').AsInteger     := LRecordID
            else
              lNewDataSet.FieldByName('RecordIdentifier').AsInteger  := lOldDataSet.FieldByName('RecordIdentifier').AsInteger;
            lNewDataSet.FieldByName('ReservoirIdentifier').AsInteger  := lOldDataSet.FieldByName('ReservoirIdentifier').AsInteger;
            lNewDataSet.FieldByName('LevelIdentifier').AsInteger      := lOldDataSet.FieldByName('LevelIdentifier').AsInteger;
            lNewDataSet.FieldByName('ReservoirLev01').AsInteger       := lOldDataSet.FieldByName('ReservoirLev01').AsInteger;
            lNewDataSet.FieldByName('ReservoirLev02').AsInteger       := lOldDataSet.FieldByName('ReservoirLev02').AsInteger;
            lNewDataSet.FieldByName('ReservoirLev03').AsInteger       := lOldDataSet.FieldByName('ReservoirLev03').AsInteger;
            lNewDataSet.FieldByName('ReservoirLev04').AsInteger       := lOldDataSet.FieldByName('ReservoirLev04').AsInteger;
            lNewDataSet.FieldByName('ReservoirLev05').AsInteger       := lOldDataSet.FieldByName('ReservoirLev05').AsInteger;
            lNewDataSet.FieldByName('ReservoirLev06').AsInteger       := lOldDataSet.FieldByName('ReservoirLev06').AsInteger;
            lNewDataSet.FieldByName('ReservoirLev07').AsInteger       := lOldDataSet.FieldByName('ReservoirLev07').AsInteger;
            lNewDataSet.FieldByName('ReservoirLev08').AsInteger       := lOldDataSet.FieldByName('ReservoirLev08').AsInteger;
            lNewDataSet.FieldByName('ReservoirLev09').AsInteger       := lOldDataSet.FieldByName('ReservoirLev09').AsInteger;
            lNewDataSet.FieldByName('ReservoirLev10').AsInteger       := lOldDataSet.FieldByName('ReservoirLev10').AsInteger;
            lNewDataSet.FieldByName('ReservoirLev11').AsInteger       := lOldDataSet.FieldByName('ReservoirLev11').AsInteger;
            lNewDataSet.FieldByName('ReservoirLev12').AsInteger       := lOldDataSet.FieldByName('ReservoirLev12').AsInteger;
            if LResLevelsCommentCreated  then
              lNewDataSet.FieldByName('').AsString      := ''
            else
              lNewDataSet.FieldByName('ResLevelsComment').AsString  := lOldDataSet.FieldByName('ResLevelsComment').AsString;

            lNewDataSet.Post;
            lOldDataSet.Next;
          end;
          lNewDataSet.SaveToFile(ADirectory + LFileName, dfXML);
          lNewDataSet.Close;
        end;
      finally
        lOldDataSet.Free;
        lNewDataSet.Free;
      end;
    end;
    Result := TRUE;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


end.
