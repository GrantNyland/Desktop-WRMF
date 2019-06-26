
//
//
//  UNIT      : Contains TMonthlyDamLevelsFileAgent Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 26/07/2005
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UMonthlyDamLevelsFileAgent;

interface

uses
  Classes, sysutils,

  //  DWAF VCL
  VoaimsCom_TLB,
  UConstants,
  UFileNames,
  UAbstractObject,
  UDataFileObjects,
  UAbstractFileNamesObject,
  UAbstractFileAgent,
  UMonthlyDamLevelsObject,
  UYieldModelDataObject;

type

  TMonthlyDamLevelsFileAgent = class(TAbstractFileAgent)
  protected
    FReadFileNamesOnly: boolean;
    procedure CreateMemberObjects; override;
  public
    function ReadModelDataFromFile(AFileName:TAbstractModelFileName;  ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
    function WriteModelDataToFile(AFileName:TAbstractModelFileName;  ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
    property ReadFileNamesOnly: boolean read FReadFileNamesOnly write FReadFileNamesOnly;
  end;


implementation

uses
  UUtilities,
  UErrorHandlingOperations;

procedure TMonthlyDamLevelsFileAgent.CreateMemberObjects;
const OPNAME = 'TMonthlyDamLevelsFileAgent.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FReadFileNamesOnly := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMonthlyDamLevelsFileAgent.ReadModelDataFromFile(AFileName: TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TMonthlyDamLevelsFileAgent.ReadModelDataFromFile';
Var
  LLineData: TStringList;
  LFileData: TStringList;
  LFileName,
  LMessage,
  LReadString,
  //LGaugeCode,
  LTempString : String;

  LIndex,
  LMonth,
  LYear,
  LLinesRead,
  LErrorCode : Integer;
  LDamLevel : Double;
  LMonthlyDamLevelsObject : TMonthlyDamLevelsObject;
  LStop: boolean;
  LReservoir : IReservoirData;
  //LDeadStorageLevel ,
  //LBottomOfReservoir : double;

begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TMonthlyDamLevelsFileAgent.strReadingStarted');
    LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if (AFilename.FileName = '') then
      raise Exception.Create('File name parameter is blank.');

    if not FilePathIsDosCompatible(FAppModules,AFilename.FileName) then
    begin
      LMessage := FAppModules.Language.GetString('TMonthlyDamLevelsFileAgent.strPathNotDosCompatible');
      LMessage := Format(LMessage,[AFilename.FileName]);
      AProgressFunction(LMessage,ptError,LStop);
    end;

    //Check if file exists.
    If not FileExists(AFilename.FileName) then
    begin
      LMessage := FAppModules.Language.GetString('TMonthlyDamLevelsFileAgent.strFileNoExist');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptWarning,LStop);
      Result := True;
      Exit;
    end;

    LReservoir := nil;

    for LIndex := 0 to TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirCount-1 do
    begin
     LFileName := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.
                  ReservoirByIndex[LIndex].ReservoirConfigurationData.DamLevelsFileName;
     if(UpperCase(Trim(ExtractFileName(LFileName))) = UpperCase(Trim(ExtractFileName(AFileName.FileName)))) then
     begin
       LReservoir := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIndex[LIndex];
       Break;
     end;
    end;

    if(LReservoir = nil) then
      raise Exception.Create('No resevoir uses this file. You can only import a dam level file for a specific reservoir.');
    {else
    begin
      LDeadStorageLevel  := LReservoir.ReservoirZoneElevationsData.DeadStorageLevel.Elevation;
      LBottomOfReservoir := LReservoir.ReservoirZoneElevationsData.BottomOfReservoir.Elevation;
    end;}


    LMonthlyDamLevelsObject := ADataObject.FMonthlyDamLevelsObject;
    TFileNameObject(AFilename).FileDate := FileLastWriteDate(AFileName.FileName);

    if not LMonthlyDamLevelsObject.Initialise then
      Exit;

    LFileData := TStringList.Create;
    LLineData := TStringList.Create;
    try
      //Read the Monthly Dam Levels file
      LFileData.LoadFromFile(AFilename.FileName);
      //Validate no data
      if(LFileData.Count < 2) then
      begin
        LMessage := FAppModules.Language.GetString('TMonthlyDamLevelsFileAgent.strNoDataReturned');
        LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
        AProgressFunction(LMessage,ptError,LStop);
      end;

      //Line 2
      LLinesRead := 1;
      while (LLinesRead < LFileData.Count) do
      begin
        if(LLinesRead >= LFileData.Count)  then
          Break;

        LReadString := LFileData[LLinesRead];
        if(Trim(LReadString) = '') then
          Break;

        LLineData.CommaText := LReadString;
        if(LLineData.Count >= 2) then
        begin
          LYear   := 0;
          LMonth  := 0;
          LReadString  := LLineData[0];
          if(Length(LReadString) = 6) then
          begin
            LTempString := Copy(LReadString,1,4);
            Val(LTempString,LYear,LErrorCode);
            if(LErrorCode <> 0)  then
            begin
              LMessage := FAppModules.Language.GetString('TMonthlyDamLevelsFileAgent.strDamLevelYear');
              LMessage := Format(LMessage,[LLinesRead+1]);
              AProgressFunction(LMessage,ptError,LStop);
              LLinesRead := LLinesRead + 1;
              Continue;
            end;

            LTempString := Copy(LReadString,5,2);
            Val(LTempString,LMonth,LErrorCode);
            if(LErrorCode <> 0) or (LMonth > 12)  then
            begin
              LMessage := FAppModules.Language.GetString('TMonthlyDamLevelsFileAgent.strDamLevelMonth');
              LMessage := Format(LMessage,[LLinesRead+1]);
              AProgressFunction(LMessage,ptError,LStop);
              LLinesRead := LLinesRead + 1;
              Continue;
            end;
          end;

          LTempString  := LLineData[1];
          Val(LTempString,LDamLevel,LErrorCode);
          if(LErrorCode <> 0)  then
          begin
            LMessage := FAppModules.Language.GetString('TMonthlyDamLevelsFileAgent.strDamLevelValue');
            LMessage := Format(LMessage,[LLinesRead+1]);
            AProgressFunction(LMessage,ptError,LStop);
            LLinesRead := LLinesRead + 1;
            Continue;
          end;
          LMonthlyDamLevelsObject.MonthlyDamLevelsContainer.AddMonthlyDamLevels(LYear,LMonth,LDamLevel);
          LLinesRead := LLinesRead + 1;
        end;
      end;
    finally
      LFileData.Free;
      LLineData.Free;
    end;

      LMessage := FAppModules.Language.GetString('TMonthlyDamLevelsFileAgent.strReadingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TMonthlyDamLevelsFileAgent.WriteModelDataToFile(AFileName: TAbstractModelFileName; ADataObject: TDataFileObjects;
                                                         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TMonthlyDamLevelsFileAgent.WriteModelDataToFile';
begin
  Result := FALSE;
  try
    //This is not in use.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.


