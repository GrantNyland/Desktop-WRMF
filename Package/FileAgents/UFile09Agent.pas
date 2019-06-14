//
//
//  UNIT      : Contains TFile09Agent Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 05/08/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UFile09Agent;

interface

uses
  Classes, sysutils,

  //  DWAF VCL
  UFileNames,
  UConstants,
  UAbstractObject,
  UAbstractFileAgent,
  UIrrigationAreasObject,
  UDataFileObjects,
  UAbstractFileNamesObject,
  UYieldModelDataObject;
type

  TFile09Agent = class(TAbstractFileAgent)
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

function TFile09Agent.ReadModelDataFromFile(AFileName:TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile09Agent.ReadModelDataFromFile';
Var
  LFileData: TStringList;
  //LInteger :TInteger;
  LMessage,
  LReadString,
  LTempString : String;
  LStart,
  LReadInteger,
  LCount,
  LLinesCount,
  LLinesRead,
  LLinesRepeat,
  LErrorCode : Integer;
  LReadReal : Double;
  LIrrigationAreaObject: TIrrigationAreaObject;
  LIrrigationArea:TIrrigationArea;
  LFileLineTypesObject: TAbstractFileLineTypesObject;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile09Agent.strReadingStarted');
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
      LMessage := FAppModules.Language.GetString('TFile09Agent.strFileNoExist');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptError,LStop);
      Exit;
    end;

    LIrrigationAreaObject := ADataObject.FIrrigationAreaObject;
    TFileNameObject(AFilename).FileDate := FileLastWriteDate(AFileName.FileName);

    if not LIrrigationAreaObject.Initialise then
    Exit;

    LFileLineTypesObject := ModelData.FilesLineTypes.FileLineTypesObject[AFileName];
    if not Assigned(LFileLineTypesObject) then
       Exit;
    TFileLineTypesObject(LFileLineTypesObject).ClearLineTypes;

    LFileData := TStringList.Create;
    try
      //Read the F09 file
      LFileData.LoadFromFile(AFilename.FileName);

      //Add this dummy for line 0
      TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'0');

      LLinesRead := 0;
      LLinesRepeat := 1;
      LIrrigationArea := nil;
      for LLinesCount := 0 to LFileData.Count - 1 do
      begin


        LReadString := LFileData[LLinesCount];
        if(Trim(LReadString) = '') then
          Break;

        LLinesRead := LLinesRead + 1;

        case LLinesRepeat of
          1:
            begin
              LIrrigationArea := TIrrigationArea.Create;
              LIrrigationAreaObject.FIrrigationAreasLines.Add(LIrrigationArea);
              TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'1');
              LTempString := Trim(Copy(LReadString,1,36));
              if(LTempString <> '') then
              begin
                LIrrigationArea.FIrrigationAreaName.FData := LTempString;
                LIrrigationArea.FIrrigationAreaName.FInitalised := True;
              end;

              LTempString := Copy(LReadString,37,6);
              Val(LTempString,LReadInteger,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFile09Agent.strIrrigationNodeNumberErr');
                LMessage := Format(LMessage,[LLinesCount+1,37,42]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit ;
              end
              else
              if (LReadInteger < 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFile09Agent.strIrrigationNodeNumberErr');
                LMessage := Format(LMessage,[LLinesCount+1,37,42]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit ;
              end
              else
              begin
                LIrrigationArea.FIrrigationNodeNumber.FData := LReadInteger;
                LIrrigationArea.FIrrigationNodeNumber.FInitalised := True;
                if not LIrrigationArea.FIrrigationAreaName.FInitalised then
                begin
                  LIrrigationArea.FIrrigationAreaName.FData := 'Irrigation '+ IntToStr(LReadInteger);
                  LIrrigationArea.FIrrigationAreaName.FInitalised := True;
                end;
              end;
              LLinesRepeat := 2;
            end;
          2:
            begin
              TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'1a');
              LStart := 1;
              for LCount := MinDiversionFlow to MaxDiversionFlow do
              begin
                LTempString := Copy(LReadString,LStart,6);
                if(Trim(LTempString) = '') then
                  Break;
                Val(LTempString,LReadReal,LErrorCode);
                if(LErrorCode <> 0) then
                begin
                  LMessage := FAppModules.Language.GetString('TFile09Agent.strDiversionFlowErr');
                  LMessage := Format(LMessage,[LLinesCount+1,LStart,LStart+5]);
                  AProgressFunction(LMessage,ptError,LStop);
                  if FAppModules.GlobalData.StopOnFirstErr then Exit ;
                end
                else
                begin
                  LIrrigationArea.FDiversionFlowValues[LCount].FData := LReadReal;
                  LIrrigationArea.FDiversionFlowValues[LCount].FInitalised := True;
                end;
                LStart := LStart + 6;
              end;
              LLinesRepeat := 3;
            end;
          3:
            begin
              TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'1b');
              LStart := 1;
              for LCount := MinReturnFlow to MaxReturnFlow do
              begin
                LTempString := Copy(LReadString,LStart,6);
                if(Trim(LTempString) = '') then
                  Break;
                Val(LTempString,LReadReal,LErrorCode);
                if(LErrorCode <> 0) then
                begin
                  LMessage := FAppModules.Language.GetString('TFile09Agent.strReturnFlowErr');
                  LMessage := Format(LMessage,[LLinesCount+1,LStart,LStart+5]);
                  AProgressFunction(LMessage,ptError,LStop);
                  if FAppModules.GlobalData.StopOnFirstErr then Exit ;
                end
                else
                begin
                  LIrrigationArea.FReturnFlowValues[LCount].FData := LReadReal;
                  LIrrigationArea.FReturnFlowValues[LCount].FInitalised := True;
                end;
                LStart := LStart + 6;
              end;
              LLinesRepeat := 1;
            end;
        end;
      end;

      for LLinesCount := (LLinesRead) to LFileData.Count - 1 do
        LIrrigationAreaObject.FF09ExtraLines.Add(LFileData[LLinesCount]);

      LMessage := FAppModules.Language.GetString('TFile09Agent.strReadingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);
      Result := True;

    finally
      LFileData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile09Agent.WriteModelDataToFile(AFileName:TAbstractModelFileName;  ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile09Agent.WriteModelDataToFile';
var
  LMessage : String;
  LOutString,
  LTempString  : String;
  LLinesCount,
  LCount: Integer;
  LF09File :TStringlist;
  LIrrigationAreaObject: TIrrigationAreaObject;
  LIrrigationArea:TIrrigationArea;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile09Agent.strWritingStarted');
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

    LF09File:= TStringList.Create;
    LIrrigationAreaObject := ADataObject.FIrrigationAreaObject;
    if not Assigned(LIrrigationAreaObject) then
      Exit;

    try
      for LLinesCount := 0 to LIrrigationAreaObject.FIrrigationAreasLines.Count -1 do
      begin
        LIrrigationArea := TIrrigationArea(LIrrigationAreaObject.FIrrigationAreasLines[LLinesCount]);
        // Line 1
        LOutString:='';
        LTempString:=PadString(LIrrigationArea.FIrrigationAreaName);
        LOutString:=LOutString+LTempString;

        LTempString:=PadInt(LIrrigationArea.FIrrigationNodeNumber);
        LOutString:=LOutString+LTempString;
        LF09File.Add(LOutString);

        // Line 1a
        LOutString:='';
        for LCount := MinDiversionFlow to MaxDiversionFlow do
        begin
          if not LIrrigationArea.FDiversionFlowValues[LCount].FInitalised then
            Break;
          LTempString:=PadDouble(LIrrigationArea.FDiversionFlowValues[LCount]);
          LOutString:=LOutString+LTempString;
        end;
        LF09File.Add(LOutString);

        // Line 1b
        LOutString:='';
        for LCount := MinReturnFlow to MaxReturnFlow do
        begin
          if not LIrrigationArea.FReturnFlowValues[LCount].FInitalised then
            Break;
          LTempString:=PadDouble(LIrrigationArea.FReturnFlowValues[LCount]);
          LOutString:=LOutString+LTempString;
        end;
        LF09File.Add(LOutString);
      end;

      for LLinesCount := 0 to LIrrigationAreaObject.FF09ExtraLines.Count -1 do
      begin
        LF09File.Add(LIrrigationAreaObject.FF09ExtraLines[LLinesCount]);
      end;

      LF09File.SaveToFile(AFilename.FileName);
      SetFileDate(AFileName);

      LMessage := FAppModules.Language.GetString('TFile09Agent.strWritingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);

      Result := True;

    finally
      LF09File.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

