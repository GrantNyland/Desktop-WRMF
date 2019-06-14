//
//
//  UNIT      : Contains TIFRDataLoadAgent Class
//  AUTHOR    : Dziedzi Ramulondi (arivia.kom)
//  DATE      : 09/01/2007
//  COPYRIGHT : Copyright © 2007 DWAF
//
//

unit UIFRDataLoadAgent;

interface

uses
  Classes,
  Contnrs,
  UIFRDataObject,
  UAbstractObject;

type
  TIFRDataLoadAgent = class(TAbstractAppObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function ValidateCSVFile(AFileName: string):boolean;
    function LoadDataFromFile(AFileName: string; AIFRSiteData : TIFRSiteDataObject): boolean;
    function LoadDataFromDB(AIFRSiteDataList : TIFRSiteDataList): boolean;
  end;
implementation
uses
  VCL.Controls,
  SysUtils,
  VCL.Dialogs,
  UUtilities,
  UDataSetType,
  UConstants,
  UIFRDataSQLAgent,
  UErrorHandlingOperations,
  DB;

{ TIFRDataLoadAgent }

procedure TIFRDataLoadAgent.CreateMemberObjects;
const OPNAME = 'TIFRDataLoadAgent.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TIFRDataLoadAgent.DestroyMemberObjects;
const OPNAME = 'TIFRDataLoadAgent.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TIFRDataLoadAgent.LoadDataFromDB(AIFRSiteDataList: TIFRSiteDataList): boolean;
const OPNAME = 'TIFRDataLoadAgent.LoadDataFromDB';
var
  LDataSet,
  LSubDataset : TAbstractModelDataset;
  LAgent   : TIFRDataSQLAgent;
  LIFRSite  : TIFRSiteDataObject;
  LSiteIdentifier: integer;
  LSQL,
  LSiteName,
  LSiteDescription,
  LCSVFileName,
  LQuaternaryCatchment,
  LRiverName,
  LAssociatedEMC,
  LLevelOfDetail,
  LLevelOfConfidence,
  LDataSource,
  LMonthNames: WideString;
  LXCoord,
  LYCoord: double;
begin
  Result := False;
  try
    AIFRSiteDataList.Initialise;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LSubDataset);
    LAgent   := TIFRDataSQLAgent.Create(FAppModules);
    try
      if Assigned(LDataSet) and Assigned(LSubDataset) then
      begin
        LDataSet.SetSQL(LAgent.GetIFRDataSQL);
        LDataset.DataSet.Open;

        while (NOT LDataset.DataSet.EOF) do
        begin
          LIFRSite             := AIFRSiteDataList.NewIFRSite;
          LSiteIdentifier      := LDataset.DataSet.FieldByName('Identifier').AsInteger;
          LSiteName            := Trim(LDataset.DataSet.FieldByName('SiteName').AsString);
          LSiteDescription     := Trim(LDataset.DataSet.FieldByName('SiteDescr').AsString);
          LCSVFileName         := Trim(LDataset.DataSet.FieldByName('CSVFileName').AsString);
          LQuaternaryCatchment := Trim(LDataset.DataSet.FieldByName('QuaternaryCatchment').AsString);
          LRiverName           := Trim(LDataset.DataSet.FieldByName('RiverName').AsString);
          LAssociatedEMC       := Trim(LDataset.DataSet.FieldByName('AssociatedEMC').AsString);
          LLevelOfDetail       := Trim(LDataset.DataSet.FieldByName('LevelDetail').AsString);
          LLevelOfConfidence   := Trim(LDataset.DataSet.FieldByName('LevelConfidence').AsString);
          LDataSource          := Trim(LDataset.DataSet.FieldByName('Source').AsString);
          LXCoord              := LDataset.DataSet.FieldByName('XCoord').AsFloat;
          LYCoord              := LDataset.DataSet.FieldByName('YCoord').AsFloat;
          LMonthNames          := Trim(LDataset.DataSet.FieldByName('MonthNames').AsString);
          LIFRSite.Populate(LSiteIdentifier,LSiteName,LSiteDescription,LCSVFileName,LQuaternaryCatchment,LRiverName,
                            LAssociatedEMC,LLevelOfDetail,LLevelOfConfidence,LDataSource,LMonthNames,LXCoord,LYCoord);

          LSQL := LAgent.GetIFRFlowDataSQL + ' AND Identifier = '+ IntToStr(LSiteIdentifier) +
                  ' ORDER BY Model, StudyAreaName, SubArea, Scenario, Identifier, FlowIdentifier';
          LSubDataset.DataSet.Close;
          LSubDataset.SetSQL(LSQL);
          LSubDataset.DataSet.Open;
          LIFRSite.PopulateArraysWithDataset(LSubDataset);

          LDataset.DataSet.Next;
        end;
        LSubDataset.DataSet.Close;
        LDataset.DataSet.Close;
        Result := True;
      end;
    finally
      LDataset.Free;
      LAgent.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TIFRDataLoadAgent.LoadDataFromFile(AFileName: string; AIFRSiteData: TIFRSiteDataObject): boolean;
const OPNAME = 'TIFRDataLoadAgent.LoadDataFromFile';
CMonthNames = 'Exceedence %,Oct,Nov,Dec,Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep';
var
  LFileData: TStringList;
  LCurrentline : integer;
  LMessage,
  LReadString,
  LTempString: string;
  LCount,
  LErrorCode : Integer;
  LReadReal : Double;
  LExceedencePerc      : TExceedencePercentagesArray;
  LRequiredFlows       : TIFRArray;
  LLineData            : TStringList;
  LMonthNames          : WideString;
  //LLastData,
  //LFirstData           : string;
begin
  Result := False;
  try
    if not FileExists(AFileName) then
    begin
      LMessage := FAppModules.Language.GetString('TIFRDataLoadAgent.strFileNoExist');
      LMessage := Format(LMessage,[ExtractFileName(AFileName)]);
      ShowMessage(LMessage);
      Exit;
    end;

    LFileData := TStringList.Create;
    LLineData := TStringList.Create;
    try
      //Read  file
      LFileData.LoadFromFile(AFileName);
      if(LFileData.Count > 1) then
      begin
        {LFirstData := '100%,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0';
        LLastData  := '0%,999.0,999.0,999.0,999.0,999.0,999.0,999.0,999.0,999.0,999.0,999.0,999.0';
        LLineData.CommaText :=  LFileData[1];
        if(LLineData.Count > 0) then
        begin
          LTempString := Trim(LLineData[0]);
          if(Pos('100',LTempString) <> 1) then
            LFileData.Insert(1,LFirstData);
        end;

        LLineData.CommaText := LFileData[LFileData.Count-1];
        if(LLineData.Count > 0) then
        begin
          LTempString := Trim(LLineData[0]);
          if(Pos('0',LTempString) <> 1) then
            LFileData.Insert(LFileData.Count,LLastData);
        end;
        }

        SetLength(LExceedencePerc, LFileData.Count-1);
        SetLength(LRequiredFlows, LFileData.Count-1,12);
        try
          LMonthNames  := CMonthNames;  //LFileData[0];
          for LCurrentline := 1 to LFileData.Count - 1 do
          begin
            LReadString  := Trim(LFileData[LCurrentline]);
            LLineData.CommaText :=  LReadString;
            if(LLineData.Count < 13) then
            begin
              LMessage := FAppModules.Language.GetString('TIFRDataLoadAgent.strFewColsErr');
              LMessage := Format(LMessage,[LCurrentLine+1]);
              ShowMessage(LMessage);
              Exit;
            end;
            LTempString  := Trim(LLineData[0]);
            LTempString  := StringReplace(LTempString,'%','',[rfReplaceAll]);

            Val(LTempString,LReadReal,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TIFRDataLoadAgent.strExceedenceErr');
              LMessage := Format(LMessage,[LCurrentLine+1,1]);
              ShowMessage(LMessage);
              Exit;
            end
            else
              LExceedencePerc[LCurrentline-1] := LReadReal;

            for LCount := MinMonths to MaxMonths do
            begin
              LTempString  := Trim(LLineData[LCount]);
              LTempString  := Trim(LTempString);
              Val(LTempString,LReadReal,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TIFRDataLoadAgent.strMonthvalueErr');
                LMessage := Format(LMessage,[LCurrentLine+1,LCount]);
                ShowMessage(LMessage);
                Exit;
              end
              else
                LRequiredFlows[LCurrentline-1,LCount-1] := LReadReal;
            end;
          end;
          AIFRSiteData.PopulateMonthNamesCommaText(LMonthNames);
          AIFRSiteData.PopulateArrays(LExceedencePerc,LRequiredFlows);
          Result := True;
        finally
          Finalize(LExceedencePerc);
          Finalize(LRequiredFlows);
        end;
      end;
    finally
      LLineData.Free;
      LFileData.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TIFRDataLoadAgent.ValidateCSVFile(AFileName: string): boolean;
const OPNAME = 'TIFRDataLoadAgent.ValidateCSVFile';
var
  LFileData: TStringList;
  LCurrentline : integer;
  LMessage,
  LReadString,
  LTempString: string;
  LCount,
  LErrorCode : Integer;
  LReadReal : Double;
  LLineData            : TStringList;
begin
  Result := False;
  try
    if not FileExists(AFileName) then
    begin
      LMessage := FAppModules.Language.GetString('TIFRDataLoadAgent.strFileNoExist');
      LMessage := Format(LMessage,[ExtractFileName(AFileName)]);
      ShowMessage(LMessage);
      Exit;
    end;

    LFileData := TStringList.Create;
    LLineData := TStringList.Create;
    try
      //Read  file
      LFileData.LoadFromFile(AFileName);
      for LCurrentline := 1 to LFileData.Count - 1 do
      begin
        LReadString  := Trim(LFileData[LCurrentline]);
        LLineData.CommaText :=  LReadString;
        if(LLineData.Count < 13) then
        begin
          LMessage := FAppModules.Language.GetString('TIFRDataLoadAgent.strFewColsErr');
          LMessage := Format(LMessage,[LCurrentLine+1]);
          ShowMessage(LMessage);
          Exit;
        end;
        LTempString  := Trim(LLineData[0]);
        LTempString  := StringReplace(LTempString,'%','',[rfReplaceAll]);

        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TIFRDataLoadAgent.strExceedenceErr');
          LMessage := Format(LMessage,[LCurrentLine+1,1]);
          ShowMessage(LMessage);
          Exit;
        end;
        if(LReadReal > 0.0) then;

        for LCount := MinMonths to MaxMonths do
        begin
          LTempString  := Trim(LLineData[LCount]);
          LTempString  := Trim(LTempString);
          Val(LTempString,LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TIFRDataLoadAgent.strMonthvalueErr');
            LMessage := Format(LMessage,[LCurrentLine+1,LCount]);
            ShowMessage(LMessage);
            Exit;
          end;
          if(LReadReal > 0.0) then;
        end;
      end;
      Result := True;
    finally
      LLineData.Free;
      LFileData.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
