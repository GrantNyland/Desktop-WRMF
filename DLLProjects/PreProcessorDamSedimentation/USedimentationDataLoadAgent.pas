
unit USedimentationDataLoadAgent;

interface

uses
  Classes,
  Contnrs,
  UDamSedimentationDataObject,
  UAbstractObject;

type
  TSedimentationDataLoadAgent = class(TAbstractAppObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function ValidateSurveyFile(AFileName: string):boolean;
    function LoadDataFromFile(AFileName: string; ADamSedimentationDataList : TDamSedimentationDataList): boolean;
    function LoadDataFromDB(ADamSedimentationDataList : TDamSedimentationDataList): boolean;
  end;
implementation
uses
  VCL.Controls,
  SysUtils,
  VCL.Dialogs,
  UUtilities,
  UDataSetType,
  UConstants,
  USedimentationDataSQLAgent,
  UErrorHandlingOperations,
  DB;

{ TSedimentationDataLoadAgent }

procedure TSedimentationDataLoadAgent.CreateMemberObjects;
const OPNAME = 'TSedimentationDataLoadAgent.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSedimentationDataLoadAgent.DestroyMemberObjects;
const OPNAME = 'TSedimentationDataLoadAgent.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSedimentationDataLoadAgent.LoadDataFromDB(ADamSedimentationDataList : TDamSedimentationDataList): boolean;
const OPNAME = 'TSedimentationDataLoadAgent.LoadDataFromDB';
var
  LDataSet,
  LSubDataset : TAbstractModelDataset;
  LAgent   : TSedimentationDataSQLAgent;
  LDamSedimentation  : TDamSedimentationDataObject;
  LDamIdentifier: integer;
  LSQL,
  LDamName,
  LDamDescription,
  LSurveyFileName,

  LDataSource: WideString;
  LXCoord,
  LYCoord: double;
begin
  Result := False;
  try
    ADamSedimentationDataList.Initialise;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LSubDataset);
    LAgent   := TSedimentationDataSQLAgent.Create(FAppModules);
    try
      if Assigned(LDataSet) and Assigned(LSubDataset) then
      begin
        LDataSet.SetSQL(LAgent.GetDamSedimentationDataSQL);
        LDataset.DataSet.Open;

        while (NOT LDataset.DataSet.EOF) do
        begin
          LDamSedimentation             := ADamSedimentationDataList.NewDamSedimentation;
          LDamIdentifier      := LDataset.DataSet.FieldByName('Identifier').AsInteger;
          LDamName            := Trim(LDataset.DataSet.FieldByName('DamName').AsString);
          LDamDescription     := Trim(LDataset.DataSet.FieldByName('DamCode').AsString);
          LSurveyFileName         := Trim(LDataset.DataSet.FieldByName('SurveyFileName').AsString);

          LDataSource          := Trim(LDataset.DataSet.FieldByName('Source').AsString);
          LXCoord              := LDataset.DataSet.FieldByName('XCoord').AsFloat;
          LYCoord              := LDataset.DataSet.FieldByName('YCoord').AsFloat;

          LDamSedimentation.Populate(LDamIdentifier,LDamName,LDamDescription,LSurveyFileName,LDataSource,LXCoord,LYCoord);

          LSQL := LAgent.GetDamSedimentationDataSQL + ' AND Identifier = '+ IntToStr(LDamIdentifier) +
                  ' ORDER BY Model, StudyAreaName, SubArea, Scenario, DamIdentifier, Identifier';
          LSubDataset.DataSet.Close;
          LSubDataset.SetSQL(LSQL);
          LSubDataset.DataSet.Open;


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

function TSedimentationDataLoadAgent.LoadDataFromFile(AFileName: string; ADamSedimentationDataList : TDamSedimentationDataList): boolean;
const OPNAME = 'TSedimentationDataLoadAgent.LoadDataFromFile';
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
      LMessage := FAppModules.Language.GetString('TSedimentationDataLoadAgent.strFileNoExist');
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
              LMessage := FAppModules.Language.GetString('TSedimentationDataLoadAgent.strFewColsErr');
              LMessage := Format(LMessage,[LCurrentLine+1]);
              ShowMessage(LMessage);
              Exit;
            end;
            LTempString  := Trim(LLineData[0]);
            LTempString  := StringReplace(LTempString,'%','',[rfReplaceAll]);

            Val(LTempString,LReadReal,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TSedimentationDataLoadAgent.strExceedenceErr');
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
                LMessage := FAppModules.Language.GetString('TSedimentationDataLoadAgent.strMonthvalueErr');
                LMessage := Format(LMessage,[LCurrentLine+1,LCount]);
                ShowMessage(LMessage);
                Exit;
              end
              else
                LRequiredFlows[LCurrentline-1,LCount-1] := LReadReal;
            end;
          end;
         // ADamSedimentationDataList.PopulateMonthNamesCommaText(LMonthNames);
         // ADamSedimentationDataList.PopulateArrays(LExceedencePerc,LRequiredFlows);
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

function TSedimentationDataLoadAgent.ValidateSurveyFile(AFileName: string): boolean;
const OPNAME = 'TSedimentationDataLoadAgent.ValidateSurveyFile';
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
      LMessage := FAppModules.Language.GetString('TSedimentationDataLoadAgent.strFileNoExist');
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
          LMessage := FAppModules.Language.GetString('TSedimentationDataLoadAgent.strFewColsErr');
          LMessage := Format(LMessage,[LCurrentLine+1]);
          ShowMessage(LMessage);
          Exit;
        end;
        LTempString  := Trim(LLineData[0]);
        LTempString  := StringReplace(LTempString,'%','',[rfReplaceAll]);

        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TSedimentationDataLoadAgent.strExceedenceErr');
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
            LMessage := FAppModules.Language.GetString('TSedimentationDataLoadAgent.strMonthvalueErr');
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
