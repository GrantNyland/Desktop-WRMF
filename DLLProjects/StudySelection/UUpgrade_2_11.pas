{******************************************************************************}
{*  UNIT      : Contains function to upgrade database to WRMF 2.11.0         *}
{*  AUTHOR    : RH Steyn                                                      *}
{*  DATE      : 2005/07/29                                                    *}
{*  COPYRIGHT : Copyright © 2005 DWAF                                         *}
{******************************************************************************}

unit UUpgrade_2_11;

interface                               

uses
  Classes,
  SysUtils,
  UAbstractObject;

function UpgradeToVersion_2_10_1 (ADirectory             : string;
                                  AProgressUpdateFuntion : TProgressUpdateFuntion;
                                  AFileNames             : TStringList;
                                  var AVersion           : string) : boolean;
function UpgradeToVersion_2_11_0 (ADirectory             : string;
                                  AProgressUpdateFuntion : TProgressUpdateFuntion;
                                  AFileNames             : TStringList;
                                  var AVersion           : string) : boolean;
function Upgrade_RainfallCatchmentSource_2_11_0 (ADirectory : string;
                                                 AFileName  : string) : boolean;
function Upgrade_RainfallPatchSource_2_11_0 (ADirectory : string;
                                             AFileName  : string) : boolean;
function Upgrade_IFRFeatures_2_11_0 (ADirectory : string;
                                     AFileName  : string) : boolean;
function Upgrade_ParamHeader_2_11_0 (ADirectory : string;
                                     AFileName  : string) : boolean;
function Upgrade_StudyScenario_2_11_0 (ADirectory : string;
                                       AFileName  : string) : boolean;
function Upgrade_StudyDocuments_2_11_0 (ADirectory : string;
                                        AFileName  : string) : boolean;
function Upgrade_VNVDrawing_2_11_0 (ADirectory : string;
                                    AFileName  : string) : boolean;
function Upgrade_SummaryOutputGeneral_2_11_0 (ADirectory : string;
                                              AFileName  : string) : boolean;


implementation

uses
  DBClient,
  DB,
  Vcl.Dialogs,
  UUtilities,
  UErrorHandlingOperations;

function UpgradeToVersion_2_10_1 (ADirectory             : string;
                                  AProgressUpdateFuntion : TProgressUpdateFuntion;
                                  AFileNames             : TStringList;
                                  var AVersion           : string) : boolean;
const OPNAME = 'UpgradeToVersion_2_10_1';
var
  lStop : boolean;
begin
  Result := FALSE;
  try
    AProgressUpdateFuntion('Upgrading data to version 2.10.1.', ptNone, lStop);

    AFileNames[0] := 'TableVersion=2.10.1';
    AVersion := '2.10.1';
    Result   := TRUE;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function UpgradeToVersion_2_11_0 (ADirectory             : string;
                                  AProgressUpdateFuntion : TProgressUpdateFuntion;
                                  AFileNames             : TStringList;
                                  var AVersion           : string) : boolean;
const OPNAME = 'UpgradeToVersion_2_11_0';
var
  lFileName : string;
  lStop     : boolean;
  lIndex    : integer;
begin
  Result := FALSE;
  try
    AProgressUpdateFuntion('Upgrading data to version 2.11.0.', ptNone, lStop);
    for lIndex := 1 to AFileNames.Count - 1 do
    begin
      lFileName := AFileNames[lIndex];
      if (lFileName = 'LD_RainfallPatchSource.xml') then
        Upgrade_RainfallPatchSource_2_11_0(ADirectory, lFileName);
      if (lFileName = 'LD_RainfallCatchmentSource.xml') then
        Upgrade_RainfallCatchmentSource_2_11_0(ADirectory, lFileName);
      if (lFileName = 'LD_IFRFeatures.xml') then
        Upgrade_IFRFeatures_2_11_0(ADirectory, lFileName);
      if (lFileName = 'LD_ParamHeader.xml') then
        Upgrade_ParamHeader_2_11_0(ADirectory, lFileName);
      if (lFileName = 'LD_StudyScenario.xml') then
        Upgrade_StudyScenario_2_11_0(ADirectory, lFileName);
      if (lFileName = 'LD_StudyDocuments.xml') then
        Upgrade_StudyDocuments_2_11_0(ADirectory, lFileName);
      if (lFileName = 'LD_VNVDrawing.xml') then
        Upgrade_VNVDrawing_2_11_0(ADirectory, lFileName);
      if (lFileName = 'LD_suBlockAnualAverageInflowValues.xml') OR
         (lFileName = 'LD_suBlockAnualSummaryValues.xml') OR
         (lFileName = 'LD_suBlockAvarageValues.xml') OR
         (lFileName = 'LD_suBlockCriticalPeriodsValues.xml') OR
         (lFileName = 'LD_suBlockGenericValues.xml') OR
         (lFileName = 'LD_suBlockHeader.xml') OR
         (lFileName = 'LD_suBlockOutputSummaryValues.xml') OR
         (lFileName = 'LD_suBlockSequencesWithFailuresValues.xml') OR
         (lFileName = 'LD_suBlockDescription.xml') then
        Upgrade_SummaryOutputGeneral_2_11_0(ADirectory, lFileName);

    end;
    AFileNames[0] := 'TableVersion=2.11.0';
    AVersion := '2.11.0';
    Result   := TRUE;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function Upgrade_RainfallCatchmentSource_2_11_0 (ADirectory : string;
                                                 AFileName  : string) : boolean;
const OPNAME = 'Upgrade_RainfallCatchmentSource_2_11_0';
var
  lOldDataSet : TClientDataSet;
  lNewDataSet : TClientDataSet;
  lFieldDef   : TFieldDef;
begin
  Result := FALSE;
  try
    lOldDataSet := TClientDataSet.Create(nil);
    lNewDataSet := TClientDataSet.Create(nil);
    try
      lOldDataSet.FileName := ADirectory + AFileName;
      lOldDataSet.Active   := TRUE;

      lNewDataSet.FieldDefs.Assign(lOldDataSet.FieldDefs);
      lFieldDef := lNewDataSet.FieldDefs.AddFieldDef;
      with lFieldDef do
      begin
        Name     := 'HydroStartYear';
        DataType := ftInteger;
        Required := TRUE;
      end;
      lFieldDef := lNewDataSet.FieldDefs.AddFieldDef;
      with lFieldDef do
      begin
        Name     := 'HydroEndYear';
        DataType := ftInteger;
        Required := TRUE;
      end;
      lNewDataSet.CreateDataSet;
      lNewDataSet.Active := TRUE;

      lOldDataSet.First;
      while (NOT lOldDataSet.Eof) do
      begin
        lNewDataSet.Insert;
        lNewDataSet.FieldByName('Model').AsString           := Trim(lOldDataSet.FieldByName('Model').AsString);
        lNewDataSet.FieldByName('StudyAreaName').AsString   := Trim(lOldDataSet.FieldByName('StudyAreaName').AsString);
        lNewDataSet.FieldByName('SubArea').AsString         := Trim(lOldDataSet.FieldByName('SubArea').AsString);
        lNewDataSet.FieldByName('Scenario').AsString        := Trim(lOldDataSet.FieldByName('Scenario').AsString);
        lNewDataSet.FieldByName('CatchmentID').AsInteger    := lOldDataSet.FieldByName('CatchmentID').AsInteger;
        lNewDataSet.FieldByName('SourcePatchID').AsInteger  := lOldDataSet.FieldByName('SourcePatchID').AsInteger;
        lNewDataSet.FieldByName('StationID').AsInteger      := lOldDataSet.FieldByName('StationID').AsInteger;
        lNewDataSet.FieldByName('HydroStartYear').AsInteger := 0;
        lNewDataSet.FieldByName('HydroEndYear').AsInteger   := 0;
        lNewDataSet.Post;
        lOldDataSet.Next;
      end;
      lNewDataSet.SaveToFile(ADirectory + AFileName, dfXML);
      lNewDataSet.Close;
      Result := TRUE;
    finally
      lOldDataSet.Free;
      lNewDataSet.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function Upgrade_RainfallPatchSource_2_11_0 (ADirectory : string;
                                             AFileName  : string) : boolean;
const OPNAME = 'Upgrade_RainfallPatchSource_2_11_0';
var
  lOldDataSet : TClientDataSet;
  lNewDataSet : TClientDataSet;
  lFieldDef   : TFieldDef;
begin
  Result := FALSE;
  try
    lOldDataSet := TClientDataSet.Create(nil);
    lNewDataSet := TClientDataSet.Create(nil);
    try
      lOldDataSet.FileName := ADirectory + AFileName;
      lOldDataSet.Active   := TRUE;

      lNewDataSet.FieldDefs.Assign(lOldDataSet.FieldDefs);
      lFieldDef := lNewDataSet.FieldDefs.AddFieldDef;
      with lFieldDef do
      begin
        Name     := 'HydroStartYear';
        DataType := ftInteger;
        Required := TRUE;
      end;
      lFieldDef := lNewDataSet.FieldDefs.AddFieldDef;
      with lFieldDef do
      begin
        Name     := 'HydroEndYear';
        DataType := ftInteger;
        Required := TRUE;
      end;
      lNewDataSet.CreateDataSet;
      lNewDataSet.Active := TRUE;

      lOldDataSet.First;
      while (NOT lOldDataSet.Eof) do
      begin
        lNewDataSet.Insert;
        lNewDataSet.FieldByName('PatchID').AsInteger         := lOldDataSet.FieldByName('PatchID').AsInteger;
        lNewDataSet.FieldByName('SourceStationID').AsInteger := lOldDataSet.FieldByName('SourceStationID').AsInteger;
        if (lOldDataSet.FieldByName('SourcePatchID').AsInteger = -1) then
          lNewDataSet.FieldByName('SourcePatchID').AsInteger := 0
        else
          lNewDataSet.FieldByName('SourcePatchID').AsInteger := lOldDataSet.FieldByName('SourcePatchID').AsInteger;
        lNewDataSet.FieldByName('TargetStation').AsString    := Trim(lOldDataSet.FieldByName('TargetStation').AsString);
        lNewDataSet.FieldByName('HydroStartYear').AsInteger  := 0;
        lNewDataSet.FieldByName('HydroEndYear').AsInteger    := 0;
        lNewDataSet.Post;
        lOldDataSet.Next;
      end;
      lNewDataSet.SaveToFile(ADirectory + AFileName, dfXML);
      lNewDataSet.Close;
      Result := TRUE;
    finally
      lOldDataSet.Free;
      lNewDataSet.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function Upgrade_IFRFeatures_2_11_0 (ADirectory : string;
                                     AFileName  : string) : boolean;
const OPNAME = 'Upgrade_IFRFeatures_2_11_0';
var
  lOldDataSet  : TClientDataSet;
  lNewDataSet  : TClientDataSet;
  lOldFieldDef : TFieldDef;
  lFieldDef    : TFieldDef;
  lIndex       : integer;
  lNodeNumbers : string;
  lFieldName   : string;
  lRefNr       : string;
begin
  Result := FALSE;
  try
    lOldDataSet := TClientDataSet.Create(nil);
    lNewDataSet := TClientDataSet.Create(nil);
    try
      lOldDataSet.FileName := ADirectory + AFileName;
      lOldDataSet.Active   := TRUE;

      lNewDataSet.FieldDefs.Clear;
      for lIndex := 0 to lOldDataSet.FieldDefs.Count - 1 do
      begin
        lOldFieldDef := lOldDataSet.FieldDefs[lIndex];
        if (Pos('RefNodeNumber', lOldFieldDef.Name) = 0) then
        begin
          lFieldDef := lNewDataSet.FieldDefs.AddFieldDef;
          lFieldDef.Name     := lOldFieldDef.Name;
          lFieldDef.DataType := lOldFieldDef.DataType;
          lFieldDef.Required := lOldFieldDef.Required;
        end;
      end;
      lFieldDef := lNewDataSet.FieldDefs.AddFieldDef;
      with lFieldDef do
      begin
        Name     := 'RefNodeNumbers';
        DataType := ftString;
        Size     := 255;
        Required := FALSE;
      end;
      lNewDataSet.CreateDataSet;
      lNewDataSet.Active := TRUE;

      lOldDataSet.First;
      while (NOT lOldDataSet.Eof) do
      begin
        lNodeNumbers := '';
        for lIndex := 1 to 10 do
        begin
          lFieldName := Format('RefNodeNumber%2.2d', [lIndex]);
          lRefNr     := Trim(lOldDataSet.FieldByName(lFieldName).AsString);
          if (lRefNr <> '') then
          begin
            if (lNodeNumbers = '') then
              lNodeNumbers := lNodeNumbers + lRefNr
            else
              lNodeNumbers := lNodeNumbers + ',' + lRefNr;
          end;
        end;
        lNewDataSet.Insert;
        lNewDataSet.FieldByName('Model').AsString               := Trim(lOldDataSet.FieldByName('Model').AsString);
        lNewDataSet.FieldByName('StudyAreaName').AsString       := Trim(lOldDataSet.FieldByName('StudyAreaName').AsString);
        lNewDataSet.FieldByName('SubArea').AsString             := Trim(lOldDataSet.FieldByName('SubArea').AsString);
        lNewDataSet.FieldByName('Scenario').AsString            := Trim(lOldDataSet.FieldByName('Scenario').AsString);
        lNewDataSet.FieldByName('Identifier').AsInteger         := lOldDataSet.FieldByName('Identifier').AsInteger;
        lNewDataSet.FieldByName('IFRChannelNumber').AsInteger   := lOldDataSet.FieldByName('IFRChannelNumber').AsInteger;
        lNewDataSet.FieldByName('FeatureName').AsString         := Trim(lOldDataSet.FieldByName('FeatureName').AsString);
        lNewDataSet.FieldByName('ReferenceNodeCount').AsInteger := lOldDataSet.FieldByName('ReferenceNodeCount').AsInteger;
        lNewDataSet.FieldByName('LagInMonthsCount').AsInteger   := lOldDataSet.FieldByName('LagInMonthsCount').AsInteger;
        lNewDataSet.FieldByName('PointsCount').AsInteger        := lOldDataSet.FieldByName('PointsCount').AsInteger;
        lNewDataSet.FieldByName('RefNodeNumbers').AsString      := lNodeNumbers;
        lNewDataSet.Post;
        lOldDataSet.Next;
      end;
      lNewDataSet.SaveToFile(ADirectory + AFileName, dfXML);
      lNewDataSet.Close;
      Result := TRUE;
    finally
      lOldDataSet.Free;
      lNewDataSet.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function Upgrade_ParamHeader_2_11_0 (ADirectory : string;
                                     AFileName  : string) : boolean;
const OPNAME = 'Upgrade_ParamHeader_2_11_0';
var
  lOldDataSet  : TClientDataSet;
  lNewDataSet  : TClientDataSet;
  lOldFieldDef : TFieldDef;
  lFieldDef    : TFieldDef;
  lIndex       : integer;
  lKeyGauges   : string;
  lFieldName   : string;
  lRefNr       : string;
begin
  Result := FALSE;
  try
    lOldDataSet := TClientDataSet.Create(nil);
    lNewDataSet := TClientDataSet.Create(nil);
    try
      lOldDataSet.FileName := ADirectory + AFileName;
      lOldDataSet.Active   := TRUE;

      lNewDataSet.FieldDefs.Clear;
      for lIndex := 0 to lOldDataSet.FieldDefs.Count - 1 do
      begin
        lOldFieldDef := lOldDataSet.FieldDefs[lIndex];
        if (lOldFieldDef.Name = 'KeyGaugeCount') OR (Pos('KeyGauge', lOldFieldDef.Name) = 0) then
        begin
          lFieldDef := lNewDataSet.FieldDefs.AddFieldDef;
          lFieldDef.Name     := lOldFieldDef.Name;
          lFieldDef.DataType := lOldFieldDef.DataType;
          lFieldDef.Required := lOldFieldDef.Required;
        end;
      end;
      lFieldDef := lNewDataSet.FieldDefs.AddFieldDef;
      with lFieldDef do
      begin
        Name     := 'KeyGauges';
        DataType := ftString;
        Size     := 255;
        Required := FALSE;
      end;
      lNewDataSet.CreateDataSet;
      lNewDataSet.Active := TRUE;

      lOldDataSet.First;
      while (NOT lOldDataSet.Eof) do
      begin
        lKeyGauges := '';
        for lIndex := 1 to 20 do
        begin
          lFieldName := Format('KeyGauge%2.2d', [lIndex]);
          lRefNr     := Trim(lOldDataSet.FieldByName(lFieldName).AsString);
          if (lRefNr <> '') then
          begin
            if (lKeyGauges = '') then
              lKeyGauges := lKeyGauges + lRefNr
            else
              lKeyGauges := lKeyGauges + ',' + lRefNr;
          end;
        end;
        lNewDataSet.Insert;
        lNewDataSet.FieldByName('Model').AsString               := Trim(lOldDataSet.FieldByName('Model').AsString);
        lNewDataSet.FieldByName('StudyAreaName').AsString       := Trim(lOldDataSet.FieldByName('StudyAreaName').AsString);
        lNewDataSet.FieldByName('SubArea').AsString             := Trim(lOldDataSet.FieldByName('SubArea').AsString);
        lNewDataSet.FieldByName('Scenario').AsString            := Trim(lOldDataSet.FieldByName('Scenario').AsString);
        lNewDataSet.FieldByName('GaugeCount').AsInteger         := lOldDataSet.FieldByName('GaugeCount').AsInteger;
        lNewDataSet.FieldByName('GaugeComment').AsString        := Trim(lOldDataSet.FieldByName('GaugeComment').AsString);
        lNewDataSet.FieldByName('KeyGaugeCount').AsInteger      := lOldDataSet.FieldByName('KeyGaugeCount').AsInteger;
        lNewDataSet.FieldByName('KeyGauges').AsString           := lKeyGauges;
        lNewDataSet.Post;
        lOldDataSet.Next;
      end;
      lNewDataSet.SaveToFile(ADirectory + AFileName, dfXML);
      lNewDataSet.Close;
      Result := TRUE;
    finally
      lOldDataSet.Free;
      lNewDataSet.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function Upgrade_StudyScenario_2_11_0 (ADirectory : string;
                                       AFileName  : string) : boolean;
const OPNAME = 'Upgrade_StudyScenario_2_11_0';
var
  lOldDataSet  : TClientDataSet;
  lNewDataSet  : TClientDataSet;
  lFieldDef    : TFieldDef;
  lIndex       : integer;
  lFieldName   : string;
begin
  Result := FALSE;
  try
    lOldDataSet := TClientDataSet.Create(nil);
    lNewDataSet := TClientDataSet.Create(nil);
    try
      lOldDataSet.FileName := ADirectory + AFileName;
      lOldDataSet.Active   := TRUE;

      lNewDataSet.FieldDefs.Assign(lOldDataSet.FieldDefs);
      lFieldDef := lNewDataSet.FieldDefs.AddFieldDef;
      with lFieldDef do
      begin
        Name     := 'StudyImportDate';
        DataType := ftDateTime;
        Required := FALSE;
      end;
      lFieldDef := lNewDataSet.FieldDefs.AddFieldDef;
      with lFieldDef do
      begin
        Name     := 'LastUpdateDate';
        DataType := ftDateTime;
        Required := FALSE;
      end;

      lNewDataSet.CreateDataSet;
      lNewDataSet.Active := TRUE;

      lOldDataSet.First;
      while (NOT lOldDataSet.Eof) do
      begin
        lNewDataSet.Insert;
        for lIndex := 0 to lOldDataSet.Fields.Count - 1 do
        begin
          lFieldName := lOldDataSet.Fields[lIndex].FieldName;
          lNewDataSet.FieldByName(lFieldName).AsString := Trim(lOldDataSet.FieldByName(lFieldName).AsString);
        end;
        lNewDataSet.FieldByName('StudyImportDate').AsDateTime := Now;
        lNewDataSet.FieldByName('LastUpdateDate').AsDateTime := Now;
        lNewDataSet.Post;
        lOldDataSet.Next;
      end;
      lNewDataSet.SaveToFile(ADirectory + AFileName, dfXML);
      lNewDataSet.Close;
      Result := TRUE;
    finally
      lOldDataSet.Free;
      lNewDataSet.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function Upgrade_StudyDocuments_2_11_0 (ADirectory : string;
                                        AFileName  : string) : boolean;
const OPNAME = 'Upgrade_StudyDocuments_2_11_0';
var
  lOldDataSet  : TClientDataSet;
  lNewDataSet  : TClientDataSet;
  lOldFieldDef : TFieldDef;
  lFieldDef    : TFieldDef;
  lIndex       : integer;
  lBlobSize    : integer;
  lStudyArea   : string;
  lCategory    : string;
  lFileName    : string;
  lDocsPath    : string;
  lFilePath    : string;
  lMsg         : string;
  lFileCreated : Boolean;
begin
  Result := FALSE;
  try
    lOldDataSet := TClientDataSet.Create(nil);
    lNewDataSet := TClientDataSet.Create(nil);
    try
      lOldDataSet.FileName := ADirectory + AFileName;
      lOldDataSet.Active   := TRUE;

      lNewDataSet.FieldDefs.Clear;
      for lIndex := 0 to lOldDataSet.FieldDefs.Count - 1 do
      begin
        lOldFieldDef := lOldDataSet.FieldDefs[lIndex];
        if (lOldFieldDef.Name <> 'Document') then
        begin
          lFieldDef := lNewDataSet.FieldDefs.AddFieldDef;
          lFieldDef.Name     := lOldFieldDef.Name;
          lFieldDef.DataType := lOldFieldDef.DataType;
          lFieldDef.Required := lOldFieldDef.Required;
        end;
      end;

      lNewDataSet.CreateDataSet;
      lNewDataSet.Active := TRUE;

      lDocsPath := StudyDocumentsPath;
      lOldDataSet.First;
      while (NOT lOldDataSet.Eof) do
      begin
        lStudyArea := Trim(lOldDataSet.FieldByName('StudyAreaName').AsString);
        lCategory  := Trim(lOldDataSet.FieldByName('Category').AsString);
        lFileName  := Trim(lOldDataSet.FieldByName('FileName').AsString);

        lFileCreated := FALSE;
        lBlobSize := (lOldDataSet.FieldByName('Document') as TBlobField).BlobSize;
        if (lBlobSize < 32000) then
        begin
          lFilePath := lDocsPath + lStudyArea + '\' + lCategory;
          if (NOT (DirectoryExists(lFilePath))) then
            ForceDirectories(lFilePath);
          try
            TBlobField(lOldDataSet.FieldByName('Document')).SaveToFile(lFilePath + '\' + lFileName);
            lFileCreated := TRUE;
          except
            lMsg := 'Could not recreate file ' + lFileName +
                    '. Please add it to the study manually.';
            ShowMessage(lMsg);
          end;
        end
        else
        begin
          lMsg := 'Could not recreate file ' + lFileName +
                  '. Please add it to the study manually.';
          ShowMessage(lMsg);
        end;
        if (lFileCreated) then
        begin
          lNewDataSet.Insert;
          lNewDataSet.FieldByName('StudyAreaName').AsString := lStudyArea;
          lNewDataSet.FieldByName('Category').AsString      := lCategory;
          lNewDataSet.FieldByName('FileName').AsString      := lFileName;
          lNewDataSet.FieldByName('Identifier').AsString    := Trim(lOldDataSet.FieldByName('Identifier').AsString);
          lNewDataSet.FieldByName('MenuCaption').AsString   := Trim(lOldDataSet.FieldByName('MenuCaption').AsString);
          lNewDataSet.Post;
        end;
        lOldDataSet.Next;
      end;
      lNewDataSet.SaveToFile(ADirectory + AFileName, dfXML);
      lNewDataSet.Close;
      Result := TRUE;
    finally
      lOldDataSet.Free;
      lNewDataSet.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function Upgrade_VNVDrawing_2_11_0 (ADirectory : string;
                                    AFileName  : string) : boolean;
const OPNAME = 'Upgrade_VNVDrawing_2_11_0';
var
  lOldDataSet  : TClientDataSet;
  lNewDataSet  : TClientDataSet;
  lOldFieldDef : TFieldDef;
  lFieldDef    : TFieldDef;
  lIndex       : integer;
  lMsg         : string;
begin
  Result := FALSE;
  try
    lMsg := 'Network diagrams from version 2.10.1 and earlier ' +
            'can not be upgraded to version 2.11.0. Please redraw network diagrams.';
    ShowMessage(lMsg);
    lOldDataSet := TClientDataSet.Create(nil);
    lNewDataSet := TClientDataSet.Create(nil);
    try
      lOldDataSet.FileName := ADirectory + AFileName;
      lOldDataSet.Active   := TRUE;

      lNewDataSet.FieldDefs.Clear;
      for lIndex := 0 to lOldDataSet.FieldDefs.Count - 1 do
      begin
        lOldFieldDef := lOldDataSet.FieldDefs[lIndex];
        if (lOldFieldDef.Name <> 'DrawingObject') then
        begin
          lFieldDef := lNewDataSet.FieldDefs.AddFieldDef;
          lFieldDef.Name     := lOldFieldDef.Name;
          lFieldDef.DataType := lOldFieldDef.DataType;
          lFieldDef.Required := lOldFieldDef.Required;
        end;
      end;

      lNewDataSet.CreateDataSet;
      lNewDataSet.Active := TRUE;
      lNewDataSet.SaveToFile(ADirectory + AFileName, dfXML);
      lNewDataSet.Close;
      Result := TRUE;
    finally
      lOldDataSet.Free;
      lNewDataSet.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function Upgrade_SummaryOutputGeneral_2_11_0 (ADirectory : string;
                                              AFileName  : string) : boolean;
const OPNAME = 'Upgrade_SummaryOutputGeneral_2_11_0';
var
  lOldDataSet  : TClientDataSet;
  lNewDataSet  : TClientDataSet;
  lFieldDef    : TFieldDef;
  lOldFieldDef : TFieldDef;
  lIndex       : integer;
  lFieldName   : string;
begin
  Result := FALSE;
  try
    lOldDataSet := TClientDataSet.Create(nil);
    lNewDataSet := TClientDataSet.Create(nil);
    try
      lOldDataSet.FileName := ADirectory + AFileName;
      lOldDataSet.Active   := TRUE;

      lNewDataSet.FieldDefs.Clear;
      for lIndex := 0 to lOldDataSet.FieldDefs.Count - 1 do
      begin
        lOldFieldDef := lOldDataSet.FieldDefs[lIndex];
        if (AFileName = 'LD_suBlockDescription.xml') AND (lOldFieldDef.Name = 'BlockTitleID')then
        begin
          lFieldDef := lNewDataSet.FieldDefs.AddFieldDef;
          with lFieldDef do
          begin
            Name     := 'ElementID';
            DataType := ftInteger;
            Required := FALSE;
          end;
        end
        else
        if (lOldFieldDef.Name = 'RunNumber') then
        begin
          lFieldDef := lNewDataSet.FieldDefs.AddFieldDef;
          with lFieldDef do
          begin
            Name     := 'LoadCaseNumber';
            DataType := ftInteger;
            Required := FALSE;
          end;
        end
        else
        begin
          lFieldDef := lNewDataSet.FieldDefs.AddFieldDef;
          lFieldDef.Name     := lOldFieldDef.Name;
          lFieldDef.DataType := lOldFieldDef.DataType;
          lFieldDef.Required := lOldFieldDef.Required;
        end;
      end;
      lFieldDef := lNewDataSet.FieldDefs.AddFieldDef;
      with lFieldDef do
      begin
        Name     := 'SequenceNumber';
        DataType := ftInteger;
        Required := FALSE;
      end;
      lNewDataSet.CreateDataSet;
      lNewDataSet.Active := TRUE;

      lOldDataSet.First;
      while (NOT lOldDataSet.Eof) do
      begin
        lNewDataSet.Insert;
        for lIndex := 0 to lOldDataSet.Fields.Count - 1 do
        begin
          lFieldName := lOldDataSet.Fields[lIndex].FieldName;
          if (AFileName = 'LD_suBlockDescription.xml') AND (lFieldName = 'BlockTitleID')then
            lNewDataSet.FieldByName('ElementID').AsInteger := lOldDataSet.FieldByName('BlockTitleID').AsInteger
          else
          if (lFieldName = 'RunNumber') then
            lNewDataSet.FieldByName('LoadCaseNumber').AsInteger := lOldDataSet.FieldByName('RunNumber').AsInteger
          else
            lNewDataSet.FieldByName(lFieldName).AsString := Trim(lOldDataSet.FieldByName(lFieldName).AsString);
        end;
        lNewDataSet.FieldByName('SequenceNumber').AsInteger := 1;
        lNewDataSet.Post;
        lOldDataSet.Next;
      end;
      lNewDataSet.SaveToFile(ADirectory + AFileName, dfXML);
      lNewDataSet.Close;
      Result := TRUE;
    finally
      lOldDataSet.Free;
      lNewDataSet.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
