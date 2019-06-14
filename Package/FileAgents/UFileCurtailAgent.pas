//
//  Unit      : TFileCurtailAgent
//  AUTHOR    : Phatedi Lethabo
//  DATE      :
//  COPYRIGHT : Copyright © 2016 DWS
//
unit UFileCurtailAgent;

interface

uses
  Classes, sysutils,

  //  DWAF VCL
  UFileNames,
  UConstants,
  UAbstractObject,
  UDataFileObjects,
  UAbstractFileNamesObject,
  UAbstractFileAgent,
  UCurtailObject,
  UYieldModelDataObject,
  UPlanningFileDataObjects;

type

  TFileCurtailAgent = class(TAbstractFileAgent)
  public
    function ReadModelDataFromFile(AFileName:TAbstractModelFileName;  ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
    function WriteModelDataToFile(AFileName:TAbstractModelFileName;  ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
  end;


implementation

uses
  UUtilities,
  UFilesLineTypeObject,
  VoaimsCom_TLB,
  UErrorHandlingOperations, UPathsObject;

function TFileCurtailAgent.ReadModelDataFromFile(AFileName: TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFileCurtailAgent.ReadModelDataFromFile';
Var
  LFileData: TStringList;
  LMessage,
  LReadString,
  LTempString : String;
  LStart,
  LReadInteger,
  LCount,
  LStartPos,
  LErrorCode : Integer;
  LReadReal : Double;
  LCurtail : TCurtailObject; // TPlanningFileDataObjects;
  LCurtailDataObject : TCurtailDataObject;
  LFileLineTypesObject: TAbstractFileLineTypesObject;
  LStop: boolean;
  LLocalCount,
  LIndex: Integer;
  begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileCurtailAgent.strReadingStarted');
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
      LMessage := FAppModules.Language.GetString('TFileCurtailAgent.strFileNoExist');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptError,LStop);
      Exit;
    end;

    LCurtailDataObject    := TPlanningFileDataObjects(ADataObject).CurtailDataObject;
    LCurtail := LCurtailDataObject.AddCurtailData;

    if not LCurtailDataObject.Initialise then
    Exit;

    LFileLineTypesObject := ModelData.FilesLineTypes.FileLineTypesObject[AFileName];
    if not Assigned(LFileLineTypesObject) then
       Exit;
    TFileLineTypesObject(LFileLineTypesObject).ClearLineTypes;

    LFileData := TStringList.Create;
    try
      //Read the Curtail file
      LFileData.LoadFromFile(AFilename.FileName);

      //Check Curtail.Dat file contains at leat 11 lines
      {*if ((FAppModules.StudyArea.ModelVersion = '7') AND (LFileData.Count < 11)) then
      begin
        LMessage := FAppModules.Language.GetString('TFileCurtailAgent.strCurtailStructureErr');
        LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
        AProgressFunction(LMessage,ptError,LStop);
      end;
      if ((FAppModules.StudyArea.ModelVersion <> '7') AND (LFileData.Count < 12)) then
      begin
        LMessage := FAppModules.Language.GetString('TFileCurtailAgent.strCurtailStructureErrB');
        LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
        AProgressFunction(LMessage,ptError,LStop);
      end;*}

     //Line 1 Number of channels
       LStart := 0;

       LReadString := Trim(LFileData[LStart]);

       TFileLineTypesObject(LFileLineTypesObject).AddLineType(1,'1');
       LTempString:=GetSubstring(LReadString,1,1);
       LTempString := Trim(LTempString);
       Val(LTempString,LReadInteger,LErrorCode);
       if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileCurtailAgent.strNumberOfChannelErr');
          LMessage := Format(LMessage,[1,1,6]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LCurtailDataObject.FNumberOfChannel.FData :=LReadInteger;
          LCurtailDataObject.FNumberOfChannel.FInitalised := True;
        end;

       //Go through all channel number Count
       for LIndex := LStart to LCurtailDataObject.FNumberOfChannel.FData -1 do
        begin

          LCurtail := LCurtailDataObject.AddCurtailData;

          LStart := LStart + 1;

          LReadString := LFileData[LStart];

          LLocalCount := LFileLineTypesObject.LinesCount;
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'2');
          LTempString:=GetSubstring(LReadString,1,4);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFileCurtailAgent.strChannelNumberErr');
            LMessage := Format(LMessage,[2,1,4]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LCurtail.FChannelNumber.FData :=LReadInteger;
            LCurtail.FChannelNumber.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,5,4);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFileCurtailAgent.strFReservoirNumberErr');
            LMessage := Format(LMessage,[2,5,8]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LCurtail.FReservoirNumber.FData :=LReadInteger;
            LCurtail.FReservoirNumber.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,9,2);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFileCurtailAgent.strDecisionMonthErr');
            LMessage := Format(LMessage,[2,10,12]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LCurtail.FDecisionMonth.FData :=LReadInteger;
            LCurtail.FDecisionMonth.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,11,2);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFileCurtailAgent.strStartMonthErr');
            LMessage := Format(LMessage,[2,11,16]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LCurtail.FStartMonth.FData :=LReadInteger;
            LCurtail.FStartMonth.FInitalised := True;
          end;

          //Line 3 Reservoir Elevation Restriction
          LStartPos:=1;
          LStart := LStart + 1;
          LReadString := LFileData[LStart];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(3,'3');
          for LCount := 1 to 10   do
          begin

            LTempString:=GetSubstring(LReadString,LStartPos,6);
            LTempString := Trim(LTempString);
            Val(LTempString,LReadReal,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFileCurtailAgent.strReservoirElevationErr');
              LMessage := Format(LMessage,[3,LStartPos,LStartPos+5]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LCurtail.FReservoirElevation[LCount].FData:=LReadReal;
              LCurtail.FReservoirElevation[LCount].FInitalised:= True;
            end;
            LStartPos := LStartPos + 1;
            Inc(LStartPos ,6);
          end;

          //Line 4 Start Month
          LStartPos:=1;
          LStart := LStart + 1;
          LReadString := LFileData[LStart];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(4,'4');
          for LCount := 1 to 10  do
          begin
            LTempString:=GetSubstring(LReadString,LStartPos,4);
            LTempString := Trim(LTempString);
            Val(LTempString,LReadReal,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFileCurtailAgent.strMultiplicationRestrictionErr');
              LMessage := Format(LMessage,[4,LStartPos,LStartPos+3]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LCurtail.FMultiplicationRestriction[LCount].FData:=LReadReal;
              LCurtail.FMultiplicationRestriction[LCount].FInitalised:= True;
            end;
            LStartPos := LStartPos + 1;
            Inc(LStartPos,4)
           end;
          //Loop LIndex ends here for Number of Channels
       end;

         //Notes within the CUR.dat file (ExtraLines)

        for LCount := LStart + 1 to LFileData.Count - 1 do
         LCurtail.FFileCurtailExtraLines.Add(LFileData[LCount]);

          LMessage := FAppModules.Language.GetString('TFileCurtailAgent.strReadingCompleted');
          LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
          AProgressFunction(LMessage,ptNone,LStop);

      result := true;
    finally
      LFileData.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;

end;
function TFileCurtailAgent.WriteModelDataToFile(AFileName:TAbstractModelFileName;ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFileCurtailAgent.WriteModelDataToFile';
var
  LMessage       ,
  LOutString     ,
  LTempString    : string;
  LCurtailList   : TStringlist;
  LCurtail       : TCurtailDataObject;
  LCurtailObj    : TCurtailObject;
  LStop          : boolean;
  LCount         : integer;
  LIndex         : integer;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileCurtailAgent.strWritingStarted');
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

    LCurtail := TPlanningFileDataObjects(ADataObject).CurtailDataObject;
    LCurtailObj := LCurtail.AddCurtailData;

    LCurtailList:= TStringList.Create;
    try

      //line1++++ Number of Curtail channels to be restricted
      LCurtailList.Add(IntToStr(LCurtail.FNumberOfChannel.FData));

      //line2++++ Channel Number / Reservoir number / Decision Month / month after

      for LIndex := 0 to LCurtail.FNumberOfChannel.FData -1 do
      Begin
        LCurtailObj := LCurtail.GetCurtailObjectByIndex(LIndex);
        LOutString :='';

        LTempString:= IntToStr(LCurtailObj.FChannelNumber.FData);
        LOutString:= LTempString+' ';

        LTempString:= IntToStr(LCurtailObj.FReservoirNumber.FData);
        LOutString:= LOutString+LTempString;

        LTempString:=PadInt(LCurtailObj.FDecisionMonth);
        LOutString:= LOutString+LTempString;

        LTempString:=PadInt(LCurtailObj.FStartMonth);
        LOutString:= LOutString+LTempString;
        LCurtailList.Add(LOutString);

        //line3 ++++ Elevation in Reserviour
          LOutString:='';
          for LCount := 1 to 10 do
          begin
            LTempString:= PadDouble(LCurtailObj.FReservoirElevation[LCount]);
            LOutString:=LOutString + LTempString+' ';
          end;
          LCurtailList.Add(LOutString);

          //line4 ++++  FMultiplicationRestriction
          LOutString:='';
          for LCount := 1 to 10 do
          begin
            LTempString:=PadDouble(LCurtailObj.FMultiplicationRestriction[LCount]);
            LOutString:=LOutString+LTempString+' ';
          end;
          LCurtailList.Add(LOutString);

      End;
        //Write Curtail fields to file

       for LCount := 0 to LCurtailObj.FFileCurtailExtraLines.Count -1 do
         begin
         LCurtailList.Add(LCurtailObj.FFileCurtailExtraLines[LCount]);
       end;

        LCurtailList.SaveToFile(AFilename.FileName);
        SetFileDate(AFileName);

        LMessage := FAppModules.Language.GetString('TFileCurtailAgent.strWritingCompleted');
        LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
        AProgressFunction(LMessage,ptNone,LStop);

        Result := True;

    finally
      LCurtail.Free;
      FreeAndNil(LCurtailList);
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
