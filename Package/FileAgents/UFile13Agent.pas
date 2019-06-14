//
//
//  UNIT      : Contains TFile13Agent Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 05/08/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UFile13Agent;

interface

uses
  Classes, sysutils,

  //  DWAF VCL
  UFileNames,
  UConstants,
  UAbstractObject,
  UPowerDemandObject,
  UAbstractFileAgent,
  UDataFileObjects,
  UAbstractFileNamesObject,
  UYieldModelDataObject;
type

  TFile13Agent = class(TAbstractFileAgent)
  public
    { Public declarations }
    function ReadModelDataFromFile(AFileName:TAbstractModelFileName;  ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
    function WriteModelDataToFile(AFileName:TAbstractModelFileName;  ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
  end;


implementation

uses
  System.Contnrs,
  UUtilities,
  UFilesLineTypeObject,
  UErrorHandlingOperations;

function TFile13Agent.ReadModelDataFromFile(AFileName:TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile13Agent.ReadModelDataFromFile';
Var
  LFileData: TStringList;
  //LInteger :TInteger;
  LMessage,
  LReadString,
  LTempString : String;
  LStart,
  LCount,
  LLinesRead,
  LErrorCode : Integer;
  LReadReal : Double;
  LPowerDemandObject: TPowerDemandObject;
  LPowerDemand:TPowerDemand;
  LFileLineTypesObject: TAbstractFileLineTypesObject;
  LStop: boolean;
  LReadInteger          : integer;
  lFeatureName          : string;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile13Agent.strReadingStarted');
    LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if (AFilename.FileName = '') then
      raise Exception.Create('File name parameter is blank.');

    if not FilePathIsDosCompatible(FAppModules,AFilename.FileName) then
    begin
      LMessage := FAppModules.Language.GetString('TAbstractFileAgent.strPathNotDosCompatible');
      LMessage := Format(LMessage,[AFilename.FileName]);
      AProgressFunction(LMessage,ptError,LStop);
      if FAppModules.GlobalData.StopOnFirstErr then Exit ;
    end;

    //Check if file exists.
    If not FileExists(AFilename.FileName) then
    begin
      LMessage := FAppModules.Language.GetString('TFile13Agent.strFileNoExist');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptError,LStop);
      Exit;
    end;

    LPowerDemandObject := ADataObject.FPowerDemandObject;
    TFileNameObject(AFilename).FileDate := FileLastWriteDate(AFileName.FileName);

    if not LPowerDemandObject.Initialise then
    Exit;

    LFileLineTypesObject := ModelData.FilesLineTypes.FileLineTypesObject[AFileName];
    if not Assigned(LFileLineTypesObject) then
       Exit;
    TFileLineTypesObject(LFileLineTypesObject).ClearLineTypes;

    LFileData := TStringList.Create;
    try
      //Read the F13 file
      LFileData.LoadFromFile(AFilename.FileName);
      //Add this dummy for line 0
      TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'0');

      LLinesRead := 0;
      while (LLinesRead < LFileData.Count) do
      begin
        // Line 1
        LReadString := LFileData[LLinesRead];
        LLinesRead  := LLinesRead + 1;
        LPowerDemand := nil;

        if (Trim(LReadString) = '') then
          Break;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'1');
        LFeatureName   := Copy(LReadString,1,36);
        LTempString    := Copy(LReadString,37,6);
        Val(LTempString, LReadInteger, LErrorCode);
        if (LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile13Agent.strPowerControlChannelNumberErr');
          LMessage := Format(LMessage,[LLinesRead,37,42]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LPowerDemand := TPowerDemand.Create;
          LPowerDemandObject.FPowerDemandsLines.Add(LPowerDemand);
          if (LReadInteger <> 0) then
          begin
            LPowerDemand.FChannelNumber.FData := LReadInteger;
            LPowerDemand.FChannelNumber.FInitalised := True;
          end;
          if (Trim(LFeatureName) = '') then
          begin
            LMessage := FAppModules.Language.GetString('TFile13Agent.strPowerControlChannelNameErr');
            LMessage := Format(LMessage,[LLinesRead,1,36]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LPowerDemand.FFeatureName.FData := Trim(LFeatureName);
            LPowerDemand.FFeatureName.FInitalised := True;
          end;

          if not (LPowerDemand.FFeatureName.FInitalised) and
            (LPowerDemand.FChannelNumber.FInitalised) then
          begin
            LPowerDemand.FFeatureName.FData := 'Feature '+IntToStr(LReadInteger);
            LPowerDemand.FFeatureName.FInitalised := True;
          end;

          if(FAppModules.Model.ModelName = CPlanning) then
          begin
            LTempString    := Copy(LReadString,43,6);
            Val(LTempString, LReadInteger, LErrorCode);
            if (LErrorCode <> 0) then
            begin
              //LMessage := FAppModules.Language.GetString('TFile13Agent.strDistributionPatternErr');
              //LMessage := Format(LMessage,[LLinesRead,43,48]);
              //AProgressFunction(LMessage,ptWarning,LStop);
              //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LPowerDemand.FDistributionPattern.FData := LReadInteger;
              LPowerDemand.FDistributionPattern.FInitalised := True;
            end;
          end;
        end;

        if (LLinesRead < LFileData.Count) then
        begin
          LReadString := LFileData[LLinesRead];
          LLinesRead  := LLinesRead + 1;

          if (Trim(LReadString) = '') then
            Break;

          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'1a');
          if (LPowerDemand.FChannelNumber.FData <> 0) then
          begin
            LStart := 1;
            for LCount := MinPowerControl to MaxPowerControl do
            begin
              LTempString := Copy(LReadString,LStart,6);
              if(Trim(LTempString) = '') then
                Break;
              Val(LTempString,LReadReal,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFile13Agent.strPowerControlErr');
                LMessage := Format(LMessage,[LLinesRead+1,LStart,LStart+5]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit ;
              end
              else
              begin
                LPowerDemand.FValues[LCount].FData := LReadReal;
                LPowerDemand.FValues[LCount].FInitalised := True;
              end;
              LStart := LStart + 6;
            end;

            if(FAppModules.Model.ModelName = CPlanning) then
            begin
              if(LPowerDemand.FDistributionPattern.FData = 0) then
              begin
                LPowerDemand.FStorageFraction.FData := 1.0;
                LPowerDemand.FStorageFraction.FInitalised := True;
              end
              else
              begin
                LTempString    := Trim(LReadString);
                Val(LTempString, LReadReal, LErrorCode);
                if (LErrorCode <> 0) then
                begin
                  //LMessage := FAppModules.Language.GetString('TFile13Agent.strStorageFractionErr');
                  //LMessage := Format(LMessage,[LLinesRead,43,48]);
                  //AProgressFunction(LMessage,ptWarning,LStop);
                  //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
                end
                else
                begin
                  LPowerDemand.FStorageFraction.FData := LReadReal;
                  LPowerDemand.FStorageFraction.FInitalised := True;
                end;
              end;
            end;
          end;
        end;
      end;

      for LLinesRead := LLinesRead to LFileData.Count - 1 do
        LPowerDemandObject.FF13ExtraLines.Add(LFileData[LLinesRead]);

      LMessage := FAppModules.Language.GetString('TFile13Agent.strReadingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);
      Result := True;

    finally
      LFileData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile13Agent.WriteModelDataToFile(AFileName:TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile13Agent.WriteModelDataToFile';
var
  LMessage : String;
  LOutString,
  LTempString  : String;
  LLinesCount,
  LCount: Integer;
  LF13File :TStringlist;
  LPowerDemandObject: TPowerDemandObject;
  LPowerDemand:TPowerDemand;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile13Agent.strWritingStarted');
    LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if (AFilename.FileName = '') then
      raise Exception.Create('File name parameter is blank.');

    if not FilePathIsDosCompatible(FAppModules,AFilename.FileName) then
    begin
      LMessage := FAppModules.Language.GetString('TAbstractFileAgent.strPathNotDosCompatible');
      LMessage := Format(LMessage,[AFilename.FileName]);
      AProgressFunction(LMessage,ptError,LStop);
      if FAppModules.GlobalData.StopOnFirstErr then Exit ;
    end;

    LF13File:= TStringList.Create;
    LPowerDemandObject := ADataObject.FPowerDemandObject;
    if not Assigned(LPowerDemandObject) then
      Exit;

    try

      for LLinesCount := 0 to LPowerDemandObject.FPowerDemandsLines.Count - 1 do
      begin
        LPowerDemand := TPowerDemand(LPowerDemandObject.FPowerDemandsLines[LLinesCount]);
        if Assigned(LPowerDemand) then
        begin
          if(LPowerDemand.FFeatureName.FInitalised AND
             LPowerDemand.FChannelNumber.FInitalised) then
          begin
            //Line 1
            LOutString:='';

            LTempString:=PadString(LPowerDemand.FFeatureName);
            LOutString:=LOutString+LTempString;

            LTempString:=PadInt(LPowerDemand.FChannelNumber);
            LOutString:=LOutString+LTempString;

            if(FAppModules.Model.ModelName = CPlanning) then
            begin
              LTempString:=PadInt(LPowerDemand.FDistributionPattern);
              LOutString:=LOutString+LTempString;
            end;
            LF13File.Add(LOutString);

            //Line 2
            LOutString:='';
            for LCount := MinPowerControl to MaxPowerControl do
            begin
              if not LPowerDemand.FValues[LCount].FInitalised then
                Break;
              LTempString:=PadDouble(LPowerDemand.FValues[LCount]);
              LOutString:=LOutString+LTempString;
            end;
            LF13File.Add(LOutString);
          end;
        end;
      end;

      //Two empty lines
      LF13File.Add('');
      LF13File.Add('');

      for LLinesCount := 0 to LPowerDemandObject.FF13ExtraLines.Count -1 do
      begin
        LF13File.Add(LPowerDemandObject.FF13ExtraLines[LLinesCount]);
      end;

      LF13File.SaveToFile(AFilename.FileName);
      SetFileDate(AFileName);

      LMessage := FAppModules.Language.GetString('TFile13Agent.strWritingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);

      Result := True;

    finally
      LF13File.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

