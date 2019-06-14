//
//
//  UNIT      : Contains TFileParamAgent Class
//  AUTHOR    : Dziedzi Ramulondi(Aravia)
//  DATE      : 02/04/2003
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UFileParamAgent;

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
  UParamObject,
  UYieldModelDataObject;

type

  TFileParamAgent = class(TAbstractFileAgent)
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

uses UUtilities,
     UFilesLineTypeObject,
     UErrorHandlingOperations;

procedure TFileParamAgent.CreateMemberObjects;
const OPNAME = 'TFileParamAgent.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FReadFileNamesOnly := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileParamAgent.ReadModelDataFromFile(AFileName: TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFileParamAgent.ReadModelDataFromFile';
Var
  LFileData: TStringList;
  //LInteger :TInteger;
  LMessage: String;
  LReadString,
  LTempString : String;
  LReadInteger,
  LCount,
  LLinesRead,
  LErrorCode : Integer;
  LReadReal : Double;
  LParamObject :TParamObject;
  LParamHeaderObject : TParamHeader;
  LGaugeStochastics: TGaugeStochastics;
  LMatrixLine: TMatrixLine;
  LFileLineTypesObject: TAbstractFileLineTypesObject;
  LStop: boolean;
  LPath1 : string;
  LPath2 : string;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileParamAgent.strReadingStarted');
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
      LMessage := FAppModules.Language.GetString('TFileParamAgent.strFileNoExist');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptError,LStop);
      Exit;
    end;

    LParamObject := ADataObject.FParamObject;
    TFileNameObject(AFilename).FileDate := FileLastWriteDate(AFileName.FileName);

    if not LParamObject.Initialise then
    Exit;

    LFileLineTypesObject := ModelData.FilesLineTypes.FileLineTypesObject[AFileName];
    if not Assigned(LFileLineTypesObject) then
       Exit;
    TFileLineTypesObject(LFileLineTypesObject).ClearLineTypes;

    LFileData := TStringList.Create;
    try
      //Read the Param.dat file
      LFileData.LoadFromFile(AFilename.FileName);

      //Add this dummy for line 0
      TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'0');

      LLinesRead := 0;
      while (LLinesRead < LFileData.Count) do
      begin
        if(LFileData.Count = 0) or
          (Trim(LFileData[LLinesRead]) = '') then
          Break;

        //Line 1
        LReadString := LFileData[LLinesRead];
        LParamHeaderObject := LParamObject.ParamHeaderObject;

        LTempString:=GetSubstring(LReadString,1,12);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileParamAgent.strGaugeCountErr');
          LMessage := Format(LMessage,[LLinesRead+1,1,12]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LParamHeaderObject.GaugeCount.FData := LReadInteger;
          LParamHeaderObject.GaugeCount.FInitalised := True;
        end;

        LTempString:=GetSubstring(LReadString,13,Length(LReadString));
        if(Trim(LTempString) <> '') then
        begin
          LParamHeaderObject.GaugeComment.FData := Trim(LTempString);
          LParamHeaderObject.GaugeComment.FInitalised := True;
          LParamHeaderObject.GaugeComment.FLength  := Length(LParamHeaderObject.GaugeComment.FData);
        end;

        //Line 2 (empty)
        LLinesRead := LLinesRead + 1;
        if(LLinesRead >= LFileData.Count) then
          Break;

        //Line 3
        LLinesRead := LLinesRead + 1;
        if(LLinesRead >= LFileData.Count) then
          Break;

        LReadString := LFileData[LLinesRead];

        if(Trim(LReadString) <> '') then
        begin
          LTempString := ExtractFirstSubstring(LReadString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFileParamAgent.strKeyGaugeCountErr');
            LMessage := Format(LMessage,[2,1,1]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LParamHeaderObject.KeyGaugeCount.FData :=LReadInteger;
            LParamHeaderObject.KeyGaugeCount.FInitalised := True;
          end;

          LCount := 0;
          while(Trim(LReadString) <> '') do
          begin
            LCount := LCount + 1;

            LTempString := ExtractFirstSubstring(LReadString);
            if(Trim(LTempString) = '') then
              Break;
            Val(LTempString,LReadInteger,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFileParamAgent.strKeyGaugesErr');
              LMessage := Format(LMessage,[2,3,2]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            if (LCount > MaxKeyGauge) then
            begin
              LMessage := FAppModules.Language.GetString('TFileParamAgent.strKeyGaugesTooManyErr');
              LMessage := Format(LMessage,[2,3,2]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
              Break;
            end
            else
            begin
              LParamHeaderObject.KeyGauges[LCount].FData :=LReadInteger;
              LParamHeaderObject.KeyGauges[LCount].FInitalised := True;
            end;
          end;
        end;

        LPath1 := '';
        LPath2 := '';
        for LCount := 1 to  LParamHeaderObject.GaugeCount.FData do
        begin
          //Line 4 (Empty line)
          LLinesRead := LLinesRead + 1;
          if(LLinesRead >= LFileData.Count) then
            Break;

          //Line 5
          LLinesRead := LLinesRead + 1;
          if(LLinesRead >= LFileData.Count) then
            Break;

          if((LLinesRead mod 1000) = 0) then
          begin
            AProgressFunction('',ptNone,LStop);
            if LStop then Exit;
          end;

          LGaugeStochastics := LParamObject.GaugeStochasticsContainer.AddGaugeStochastics;
          if not Assigned(LGaugeStochastics) then
            Exit;

          LReadString := LFileData[LLinesRead];
          LTempString:=Trim(LReadString);
          if not DirectoryExists(ExtractFilePath(LTempString)) then
          begin
            LMessage := FAppModules.Language.GetString('TAbstractFileAgent.strPathDoesNotExists');
            LMessage := Format(LMessage,[ExtractFilePath(LTempString)]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          if not FilePathIsDosCompatible(FAppModules,LTempString) then
          begin
            LMessage := FAppModules.Language.GetString('TAbstractFileAgent.strPathNotDosCompatible');
            LMessage := Format(LMessage,[LTempString]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          if(LTempString <> '') then
          begin
            LGaugeStochastics.GaugePathName.FData       := LTempString;
            LGaugeStochastics.GaugePathName.FInitalised := True;
            LGaugeStochastics.GaugePathName.FLength     := Length(LGaugeStochastics.GaugePathName.FData);
            if(LPath1 = '') then
            begin
              LPath1 := UpperCase(ExtractFilePath(LTempString));
              LPath2 := LPath1;
            end
            else
            begin
              if(LPath1 <> UpperCase(ExtractFilePath(LTempString))) then
              begin
                LPath2 := UpperCase(ExtractFilePath(LTempString));
              end;
            end;
          end;

          //Line 6
          LLinesRead := LLinesRead + 1;
          if(LLinesRead >= LFileData.Count) then
            Break;

          LReadString := LFileData[LLinesRead];
          LTempString := ExtractFirstSubstring(LReadString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFileParamAgent.strYearsNumberErr');
            LMessage := Format(LMessage,[LLinesRead+1,1,12]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LGaugeStochastics.YearsNumber.FData := LReadInteger;
            LGaugeStochastics.YearsNumber.FInitalised := True;
          end;

          LTempString := Trim(ExtractFirstSubstring(LReadString));
          if(Length(LTempString) = 2) then
             LTempString := '19'+LTempString;
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFileParamAgent.strYearStartErr');
            LMessage := Format(LMessage,[LLinesRead+1,13,24]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LGaugeStochastics.YearStart.FData := LReadInteger;
            LGaugeStochastics.YearStart.FInitalised := True;
          end;

          LTempString := ExtractFirstSubstring(LReadString);
          Val(LTempString,LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            if(LTempString <> '') then
            begin
              LMessage := FAppModules.Language.GetString('TFileParamAgent.strResidual1Err');
              LMessage := Format(LMessage,[LLinesRead+1,25,36]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end;
          end
          else
          begin
            LGaugeStochastics.Residual1.FData := LReadReal;
            LGaugeStochastics.Residual1.FInitalised := True;
          end;

          LTempString := ExtractFirstSubstring(LReadString);
          Val(LTempString,LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            if(LTempString <> '') then
            begin
              LMessage := FAppModules.Language.GetString('TFileParamAgent.strResidual2Err');
              LMessage := Format(LMessage,[LLinesRead+1,37,48]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end;
          end
          else
          begin
            LGaugeStochastics.Residual2.FData := LReadReal;
            LGaugeStochastics.Residual2.FInitalised := True;
          end;

          LTempString := ExtractFirstSubstring(LReadString);
          Val(LTempString,LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            if(LTempString <> '') then
            begin
              LMessage := FAppModules.Language.GetString('TFileParamAgent.strVariate1Err');
              LMessage := Format(LMessage,[LLinesRead+1,49,60]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end;
          end
          else
          begin
            LGaugeStochastics.Variate1.FData := LReadReal;
            LGaugeStochastics.Variate1.FInitalised := True;
          end;

          LTempString := ExtractFirstSubstring(LReadString);
          Val(LTempString,LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            if(LTempString <> '') then
            begin
              LMessage := FAppModules.Language.GetString('TFileParamAgent.strVariate2Err');
              LMessage := Format(LMessage,[LLinesRead+1,61,72]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end;
          end
          else
          begin
            LGaugeStochastics.Variate2.FData := LReadReal;
            LGaugeStochastics.Variate2.FInitalised := True;
          end;

          //Line 7
          LLinesRead := LLinesRead + 1;
          if(LLinesRead >= LFileData.Count) then
            Break;

          LReadString := LFileData[LLinesRead];

          LTempString := ExtractFirstSubstring(LReadString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFileParamAgent.strTransformTypeErr');
            LMessage := Format(LMessage,[LLinesRead+1,1,3]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LGaugeStochastics.TransformType.FData := LReadInteger;
            LGaugeStochastics.TransformType.FInitalised := True;
          end;

          LTempString := ExtractFirstSubstring(LReadString);
          Val(LTempString,LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            if(LTempString <> '') then
            begin
              LMessage := FAppModules.Language.GetString('TFileParamAgent.strTransformGammaErr');
              LMessage := Format(LMessage,[LLinesRead+1,4,23]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end;
          end
          else
          begin
            LGaugeStochastics.TransformGamma.FData := LReadReal;
            LGaugeStochastics.TransformGamma.FInitalised := True;
          end;

          LTempString := ExtractFirstSubstring(LReadString);
          Val(LTempString,LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            if(LTempString <> '') then
            begin
              LMessage := FAppModules.Language.GetString('TFileParamAgent.strTransformDeltaErr');
              LMessage := Format(LMessage,[LLinesRead+1,24,43]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end;
          end
          else
          begin
            LGaugeStochastics.TransformDelta.FData := LReadReal;
            LGaugeStochastics.TransformDelta.FInitalised := True;
          end;

          LTempString := ExtractFirstSubstring(LReadString);
          Val(LTempString,LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            if(LTempString <> '') then
            begin
              LMessage := FAppModules.Language.GetString('TFileParamAgent.strTransformXlamErr');
              LMessage := Format(LMessage,[LLinesRead+1,44,63]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end;
          end
          else
          begin
            LGaugeStochastics.TransformXlam.FData := LReadReal;
            LGaugeStochastics.TransformXlam.FInitalised := True;
          end;

          LTempString := ExtractFirstSubstring(LReadString);
          Val(LTempString,LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            if(LTempString <> '') then
            begin
              LMessage := FAppModules.Language.GetString('TFileParamAgent.strTransformXiErr');
              LMessage := Format(LMessage,[LLinesRead+1,64,83]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end;
          end
          else
          begin
            LGaugeStochastics.TransformXi.FData := LReadReal;
            LGaugeStochastics.TransformXi.FInitalised := True;
          end;

          //Line 8
          LLinesRead := LLinesRead + 1;
          if(LLinesRead >= LFileData.Count) then
            Break;

          LReadString := LFileData[LLinesRead];

          LTempString := ExtractFirstSubstring(LReadString);
          Val(LTempString,LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            if(LTempString <> '') then
            begin
              LMessage := FAppModules.Language.GetString('TFileParamAgent.strResidualMeanErr');
              LMessage := Format(LMessage,[LLinesRead+1,1,12]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end;
          end
          else
          begin
            LGaugeStochastics.ResidualMean.FData := LReadReal;
            LGaugeStochastics.ResidualMean.FInitalised := True;
          end;

          LTempString := ExtractFirstSubstring(LReadString);
          Val(LTempString,LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            if(LTempString <> '') then
            begin
              LMessage := FAppModules.Language.GetString('TFileParamAgent.strResidualStdDevErr');
              LMessage := Format(LMessage,[LLinesRead+1,13,24]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end;
          end
          else
          begin
            LGaugeStochastics.ResidualStdDev.FData := LReadReal;
            LGaugeStochastics.ResidualStdDev.FInitalised := True;
          end;

          LTempString := ExtractFirstSubstring(LReadString);
          Val(LTempString,LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            if(LTempString <> '') then
            begin
              LMessage := FAppModules.Language.GetString('TFileParamAgent.strArmaPhi1Err');
              LMessage := Format(LMessage,[LLinesRead+1,25,36]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end;
          end
          else
          begin
            LGaugeStochastics.ArmaPhi1.FData := LReadReal;
            LGaugeStochastics.ArmaPhi1.FInitalised := True;
          end;

          LTempString := ExtractFirstSubstring(LReadString);
          Val(LTempString,LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            if(LTempString <> '') then
            begin
              LMessage := FAppModules.Language.GetString('TFileParamAgent.strArmaPhi2Err');
              LMessage := Format(LMessage,[LLinesRead+1,37,48]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end;
          end
          else
          begin
            LGaugeStochastics.ArmaPhi2.FData := LReadReal;
            LGaugeStochastics.ArmaPhi2.FInitalised := True;
          end;

          LTempString := ExtractFirstSubstring(LReadString);
          Val(LTempString,LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            if(LTempString <> '') then
            begin
              LMessage := FAppModules.Language.GetString('TFileParamAgent.strArmaTheta1Err');
              LMessage := Format(LMessage,[LLinesRead+1,49,60]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end;
          end
          else
          begin
            LGaugeStochastics.ArmaTheta1.FData := LReadReal;
            LGaugeStochastics.ArmaTheta1.FInitalised := True;
          end;

          LTempString := ExtractFirstSubstring(LReadString);
          Val(LTempString,LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            if(LTempString <> '') then
            begin
              LMessage := FAppModules.Language.GetString('TFileParamAgent.strArmaTheta2Err');
              LMessage := Format(LMessage,[LLinesRead+1,61,72]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end;
          end
          else
          begin
            LGaugeStochastics.ArmaTheta2.FData := LReadReal;
            LGaugeStochastics.ArmaTheta2.FInitalised := True;
          end;

          LTempString := ExtractFirstSubstring(LReadString);
          Val(LTempString,LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            if(LTempString <> '') then
            begin
              LMessage := FAppModules.Language.GetString('TFileParamAgent.strPhiZeroErr');
              LMessage := Format(LMessage,[LLinesRead+1,73,84]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end;
          end
          else
          begin
            LGaugeStochastics.PhiZero.FData := LReadReal;
            LGaugeStochastics.PhiZero.FInitalised := True;
          end;

          LTempString := ExtractFirstSubstring(LReadString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            if(LTempString <> '') then
            begin
              LMessage := FAppModules.Language.GetString('TFileParamAgent.strZTVariatesErr');
              LMessage := Format(LMessage,[LLinesRead+1,85,96]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end;
          end
          else
          begin
            LGaugeStochastics.ZTVariates.FData := LReadInteger;
            LGaugeStochastics.ZTVariates.FInitalised := True;
          end;

          //Line 9
          LLinesRead := LLinesRead + 1;
          if(LLinesRead >= LFileData.Count) then
            Break;

          LReadString := LFileData[LLinesRead];

          LTempString := ExtractFirstSubstring(LReadString);
          Val(LTempString,LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            if(LTempString <> '') then
            begin
              LMessage := FAppModules.Language.GetString('TFileParamAgent.strParamXAErr');
              LMessage := Format(LMessage,[LLinesRead+1,1,12]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end;
          end
          else
          begin
            LGaugeStochastics.ParamXA.FData := LReadReal;
            LGaugeStochastics.ParamXA.FInitalised := True;
          end;

          LTempString := ExtractFirstSubstring(LReadString);
          Val(LTempString,LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            if(LTempString <> '') then
            begin
              LMessage := FAppModules.Language.GetString('TFileParamAgent.strParamXSDErr');
              LMessage := Format(LMessage,[LLinesRead+1,13,24]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end;
          end
          else
          begin
            LGaugeStochastics.ParamXSD.FData := LReadReal;
            LGaugeStochastics.ParamXSD.FInitalised := True;
          end;

          LTempString := ExtractFirstSubstring(LReadString);
          Val(LTempString,LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            if(LTempString <> '') then
            begin
              LMessage := FAppModules.Language.GetString('TFileParamAgent.strParamAICErr');
              LMessage := Format(LMessage,[LLinesRead+1,25,36]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end;
          end
          else
          begin
            LGaugeStochastics.ParamAIC.FData := LReadReal;
            LGaugeStochastics.ParamAIC.FInitalised := True;
          end;

          LTempString := ExtractFirstSubstring(LReadString);
          Val(LTempString,LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            if(LTempString <> '') then
            begin
              LMessage := FAppModules.Language.GetString('TFileParamAgent.strParamANCErr');
              LMessage := Format(LMessage,[LLinesRead+1,37,48]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end;
          end
          else
          begin
            LGaugeStochastics.ParamANC.FData := LReadReal;
            LGaugeStochastics.ParamANC.FInitalised := True;
          end;

          if(FAppModules.StudyArea.ModelVersion = '7' ) then
          begin
            LTempString := ExtractFirstSubstring(LReadString);
            Val(LTempString,LReadReal,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              //if(LTempString <> '') then
              //begin
                LMessage := FAppModules.Language.GetString('TFileParamAgent.strCatchmentAreaErr');
                LMessage := Format(LMessage,[LLinesRead+1,49,60]);
                AProgressFunction(LMessage,ptWarning,LStop);
                //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
              //end;
              LGaugeStochastics.CatchmentArea.FData := 1.0;
              LGaugeStochastics.CatchmentArea.FInitalised := True;
            end
            else
            begin
              LGaugeStochastics.CatchmentArea.FData := LReadReal;
              LGaugeStochastics.CatchmentArea.FInitalised := True;
            end;
          end;
        end;

        if(LPath1 <> LPath2) then
        begin
          LMessage := FAppModules.Language.GetString('TFileParamAgent.PathsErr');
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit;
        end;

        if not FReadFileNamesOnly  then
        begin
          //Line MatrixB

          //Line 2 (empty)
          LLinesRead := LLinesRead + 1;
          if(LLinesRead >= LFileData.Count) then
            Break;

          //Comment Line
          LLinesRead := LLinesRead + 1;
          if(LLinesRead >= LFileData.Count) then
            Break;

          LReadString := LFileData[LLinesRead];
          ADataObject.FParamObject.MatrixComments.MatrixBComment.FData := Trim(LReadString);
          ADataObject.FParamObject.MatrixComments.MatrixBComment.FInitalised := True;
          ADataObject.FParamObject.MatrixComments.MatrixBComment.FLength := Length(Trim(LReadString));
          while True do
          begin
            LLinesRead := LLinesRead + 1;
            if(LLinesRead >= LFileData.Count) then
              Break;

            if((LLinesRead mod 1000) = 0) then
            begin
              AProgressFunction('',ptNone,LStop);
              if LStop then Exit;
            end;

            LReadString := LFileData[LLinesRead];
            if(Trim(LReadString) = '') then
              Break;

            LMatrixLine := ADataObject.FParamObject.MatrixB.AddMatrixLine;
            if not Assigned(LMatrixLine) then
              Break;

            for LCount := MinMatrix to MaxMatrix do
            begin
              //LTempString := ExtractFirstSubstring(LReadString);
              //if(Trim(LTempString) = '') then
              //  Break;

              LTempString := Copy(LReadString,(12*(LCount-1))+1,12);
              LTempString := Trim(LTempString);

              if(LTempString = '') then
                Break;

              if(Pos(' ',LTempString) > 0) then
              begin
                LTempString := StringReplace(LTempString,' ','0',[rfReplaceAll]);

                LMessage := FAppModules.Language.GetString('TFileParamAgent.strMatrixBLineValueErr');
                LMessage := Format(LMessage,[LLinesRead+1,(12*(LCount-1))+1,12*LCount]);
                AProgressFunction(LMessage,ptError,LStop);
              end;

              Val(LTempString,LReadReal,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                if(LTempString <> '') then
                begin
                  LMessage := FAppModules.Language.GetString('TFileParamAgent.strMatrixBLineValueErr');
                  LMessage := Format(LMessage,[LLinesRead+1,(12*(LCount-1))+1,12*LCount]);
                  AProgressFunction(LMessage,ptError,LStop);
                  if FAppModules.GlobalData.StopOnFirstErr then Exit ;
                end;
              end
              else
              begin
                LMatrixLine.MatrixLine[LCount].FData := LReadReal;
                LMatrixLine.MatrixLine[LCount].FInitalised := True;
              end;
            end;
          end;

          AProgressFunction('',ptNone,LStop);
          if LStop then Exit;

          //Line MatrixB0

          //Comment Line
          LLinesRead := LLinesRead + 1;
          if(LLinesRead >= LFileData.Count) then
            Break;

          LReadString := LFileData[LLinesRead];
          ADataObject.FParamObject.MatrixComments.MatrixB0Comment.FData := Trim(LReadString);
          ADataObject.FParamObject.MatrixComments.MatrixB0Comment.FInitalised := True;
          ADataObject.FParamObject.MatrixComments.MatrixB0Comment.FLength := Length(Trim(LReadString));
          while True do
          begin
            LLinesRead := LLinesRead + 1;
            if(LLinesRead >= LFileData.Count) then
              Break;

            if((LLinesRead mod 1000) = 0) then
            begin
              AProgressFunction('',ptNone,LStop);
              if LStop then Exit;
            end;

            LReadString := LFileData[LLinesRead];
            if(Trim(LReadString) = '') then
              Break;

            LMatrixLine := ADataObject.FParamObject.MatrixB0.AddMatrixLine;
            if not Assigned(LMatrixLine) then
              Break;

            for LCount := MinMatrix to MaxMatrix do
            begin
              //LTempString := ExtractFirstSubstring(LReadString);
              //if(Trim(LTempString) = '') then
              //  Break;

              LTempString := Copy(LReadString,(12*(LCount-1))+1,12);
              LTempString := Trim(LTempString);

              if(LTempString = '') then
                Break;

              if(Pos(' ',LTempString) > 0) then
              begin
                LTempString := StringReplace(LTempString,' ','0',[rfReplaceAll]);

                LMessage := FAppModules.Language.GetString('TFileParamAgent.strMatrixB0LineValueErr');
                LMessage := Format(LMessage,[LLinesRead+1,(12*(LCount-1))+1,12*LCount]);
                AProgressFunction(LMessage,ptError,LStop);
              end;

              Val(LTempString,LReadReal,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                if(LTempString <> '') then
                begin
                  LMessage := FAppModules.Language.GetString('TFileParamAgent.strMatrixB0LineValueErr');
                  LMessage := Format(LMessage,[LLinesRead+1,(12*(LCount-1))+1,12*LCount]);
                  AProgressFunction(LMessage,ptError,LStop);
                  if FAppModules.GlobalData.StopOnFirstErr then Exit ;
                end;
              end
              else
              begin
                LMatrixLine.MatrixLine[LCount].FData := LReadReal;
                LMatrixLine.MatrixLine[LCount].FInitalised := True;
              end;
            end;
          end;

          //Line MatrixB1

          AProgressFunction('',ptNone,LStop);
          if LStop then Exit;

          //Comment Line
          LLinesRead := LLinesRead + 1;
          if(LLinesRead >= LFileData.Count) then
            Break;

          LReadString := LFileData[LLinesRead];
          ADataObject.FParamObject.MatrixComments.MatrixB1Comment.FData := Trim(LReadString);
          ADataObject.FParamObject.MatrixComments.MatrixB1Comment.FInitalised := True;
          ADataObject.FParamObject.MatrixComments.MatrixB1Comment.FLength := Length(Trim(LReadString));
          while True do
          begin
            LLinesRead := LLinesRead + 1;
            if(LLinesRead >= LFileData.Count) then
              Break;

            if((LLinesRead mod 1000) = 0) then
            begin
              AProgressFunction('',ptNone,LStop);
              if LStop then Exit;
            end;

            LReadString := LFileData[LLinesRead];
            if(Trim(LReadString) = '') then
              Break;

            LMatrixLine := ADataObject.FParamObject.MatrixB1.AddMatrixLine;
            if not Assigned(LMatrixLine) then
              Break;

            for LCount := MinMatrix to MaxMatrix do
            begin
              //LTempString := ExtractFirstSubstring(LReadString);
              //if(Trim(LTempString) = '') then
              //  Break;

              LTempString := Copy(LReadString,(12*(LCount-1))+1,12);
              LTempString := Trim(LTempString);

              if(LTempString = '') then
                Break;

              if(Pos(' ',LTempString) > 0) then
              begin
                LTempString := StringReplace(LTempString,' ','0',[rfReplaceAll]);

                LMessage := FAppModules.Language.GetString('TFileParamAgent.strMatrixB1LineValueErr');
                LMessage := Format(LMessage,[LLinesRead+1,(12*(LCount-1))+1,12*LCount]);
                AProgressFunction(LMessage,ptError,LStop);
              end;

              Val(LTempString,LReadReal,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                if(LTempString <> '') then
                begin
                  LMessage := FAppModules.Language.GetString('TFileParamAgent.strMatrixB1LineValueErr');
                  LMessage := Format(LMessage,[LLinesRead+1,(12*(LCount-1))+1,12*LCount]);
                  AProgressFunction(LMessage,ptError,LStop);
                  if FAppModules.GlobalData.StopOnFirstErr then Exit ;
                end;
              end
              else
              begin
                LMatrixLine.MatrixLine[LCount].FData := LReadReal;
                LMatrixLine.MatrixLine[LCount].FInitalised := True;
              end;
            end;
          end;

          //Line MatrixA

          AProgressFunction('',ptNone,LStop);
          if LStop then Exit;

          //Comment Line
          LLinesRead := LLinesRead + 1;
          if(LLinesRead >= LFileData.Count) then
            Break;

          LReadString := LFileData[LLinesRead];
          ADataObject.FParamObject.MatrixComments.MatrixAComment.FData := Trim(LReadString);
          ADataObject.FParamObject.MatrixComments.MatrixAComment.FInitalised := True;
          ADataObject.FParamObject.MatrixComments.MatrixAComment.FLength := Length(Trim(LReadString));
          while True do
          begin
            LLinesRead := LLinesRead + 1;
            if(LLinesRead >= LFileData.Count) then
              Break;

            if((LLinesRead mod 1000) = 0) then
            begin
              AProgressFunction('',ptNone,LStop);
              if LStop then Exit;
            end;

            LReadString := LFileData[LLinesRead];
            if(Trim(LReadString) = '') then
              Break;

            LMatrixLine := ADataObject.FParamObject.MatrixA.AddMatrixLine;
            if not Assigned(LMatrixLine) then
              Break;

            for LCount := MinMatrix to MaxMatrix do
            begin
              //LTempString := ExtractFirstSubstring(LReadString);
              //if(Trim(LTempString) = '') then
              //  Break;

              LTempString := Copy(LReadString,(12*(LCount-1))+1,12);
              LTempString := Trim(LTempString);

              if(LTempString = '') then
                Break;

              if(Pos(' ',LTempString) > 0) then
              begin
                LTempString := StringReplace(LTempString,' ','0',[rfReplaceAll]);

                LMessage := FAppModules.Language.GetString('TFileParamAgent.strMatrixALineValueErr');
                LMessage := Format(LMessage,[LLinesRead+1,(12*(LCount-1))+1,12*LCount]);
                AProgressFunction(LMessage,ptError,LStop);
              end;

              Val(LTempString,LReadReal,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                if(LTempString <> '') then
                begin
                  LMessage := FAppModules.Language.GetString('TFileParamAgent.strMatrixALineValueErr');
                  LMessage := Format(LMessage,[LLinesRead+1,(12*(LCount-1))+1,12*LCount]);
                  AProgressFunction(LMessage,ptError,LStop);
                  if FAppModules.GlobalData.StopOnFirstErr then Exit ;
                end;
              end
              else
              begin
                LMatrixLine.MatrixLine[LCount].FData := LReadReal;
                LMatrixLine.MatrixLine[LCount].FInitalised := True;
              end;
            end;
          end;

          //Line MatrixC

          AProgressFunction('',ptNone,LStop);
          if LStop then Exit;

          //Comment Line
          LLinesRead := LLinesRead + 1;
          if(LLinesRead >= LFileData.Count) then
            Break;

          LReadString := LFileData[LLinesRead];
          ADataObject.FParamObject.MatrixComments.MatrixCComment.FData := Trim(LReadString);
          ADataObject.FParamObject.MatrixComments.MatrixCComment.FInitalised := True;
          ADataObject.FParamObject.MatrixComments.MatrixCComment.FLength := Length(Trim(LReadString));
          while True do
          begin
            LLinesRead := LLinesRead + 1;
            if(LLinesRead >= LFileData.Count) then
              Break;

            if((LLinesRead mod 1000) = 0) then
            begin
              AProgressFunction('',ptNone,LStop);
              if LStop then Exit;
            end;

            LReadString := LFileData[LLinesRead];
            if(Trim(LReadString) = '') then
              Break;

            LMatrixLine := ADataObject.FParamObject.MatrixC.AddMatrixLine;
            if not Assigned(LMatrixLine) then
              Break;

            for LCount := MinMatrix to MaxMatrix do
            begin
              //LTempString := ExtractFirstSubstring(LReadString);
              //if(Trim(LTempString) = '') then
              //  Break;

              LTempString := Copy(LReadString,(12*(LCount-1))+1,12);
              LTempString := Trim(LTempString);

              if(LTempString = '') then
                Break;

              if(Pos(' ',LTempString) > 0) then
              begin
                LTempString := StringReplace(LTempString,' ','0',[rfReplaceAll]);

                LMessage := FAppModules.Language.GetString('TFileParamAgent.strMatrixCLineValueErr');
                LMessage := Format(LMessage,[LLinesRead+1,(12*(LCount-1))+1,12*LCount]);
                AProgressFunction(LMessage,ptError,LStop);
              end;

              Val(LTempString,LReadReal,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                if(LTempString <> '') then
                begin
                  LMessage := FAppModules.Language.GetString('TFileParamAgent.strMatrixCLineValueErr');
                  LMessage := Format(LMessage,[LLinesRead+1,(12*(LCount-1))+1,12*LCount]);
                  AProgressFunction(LMessage,ptError,LStop);
                  if FAppModules.GlobalData.StopOnFirstErr then Exit ;
                end;
              end
              else
              begin
                LMatrixLine.MatrixLine[LCount].FData := LReadReal;
                LMatrixLine.MatrixLine[LCount].FInitalised := True;
              end;
            end;
          end;
        end;
        Break;
      end;
      LMessage := FAppModules.Language.GetString('TFileParamAgent.strReadingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);
      Result := True;
    finally
      LFileData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileParamAgent.WriteModelDataToFile(AFileName:TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFileParamAgent.WriteModelDataToFile';
var
  LMessage : String;
  LOutString,
  LTempString  : String;
  LIndex,
  LCount: Integer;
  LParamFile :TStringlist;
  LParamObject :TParamObject;
  LParamHeaderObject : TParamHeader;
  LGaugeStochastics: TGaugeStochastics;
  LMatrixLine: TMatrixLine;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileParamAgent.strWritingStarted');
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

    LParamFile:= TStringList.Create;
    LParamObject := ADataObject.FParamObject;
    if not Assigned(LParamObject) then
      Exit;

    try
      LParamHeaderObject := LParamObject.ParamHeaderObject;
      if not Assigned(LParamObject) then
        Exit;

      //Line 1
      LOutString:='';

      LTempString:=PadInt(LParamHeaderObject.GaugeCount);
      LOutString:=LOutString+LTempString + '  ';

      LTempString:=PadString(LParamHeaderObject.GaugeComment);
      LOutString:=LOutString+LTempString;
      LParamFile.Add(LOutString);

      //Line 2 (Blank)
      LParamFile.Add('');

      //Line 3
      LOutString:='';
      LTempString:=PadInt(LParamHeaderObject.KeyGaugeCount);
      LOutString:=LOutString+Trim(LTempString) + ' ';

      for LCount := MinKeyGauge to MaxKeyGauge do
      begin
        if not LParamHeaderObject.KeyGauges[LCount].FInitalised then
          Break;
        LTempString:=PadInt(LParamHeaderObject.KeyGauges[LCount]);
        LOutString:=LOutString+' '+ Trim(LTempString);
      end;
      LParamFile.Add(LOutString);

      for LCount := 0 to LParamObject.GaugeStochasticsContainer.ItemsCount - 1 do
      begin

        if((LCount mod 100) = 0) then
        begin
          AProgressFunction('',ptNone,LStop);
          if LStop then Exit;
        end;

        LGaugeStochastics := LParamObject.GaugeStochasticsContainer.GaugeStochasticsByIndex[LCount];
        if not Assigned(LParamObject) then
          Exit;

        //Line 4 (Blank)
        LParamFile.Add('');

        //Line 5
        LOutString:=PadString(LGaugeStochastics.GaugePathName);
        LParamFile.Add(LOutString);

        //Line 6
        LOutString := '';
        LTempString:=PadInt(LGaugeStochastics.YearsNumber);
        LOutString:=LOutString+LTempString;

        LTempString:=PadInt(LGaugeStochastics.YearStart);
        LOutString:=LOutString+LTempString;

        LTempString:=PadDouble(LGaugeStochastics.Residual1);
        LOutString:=LOutString+LTempString;

        LTempString:=PadDouble(LGaugeStochastics.Residual2);
        LOutString:=LOutString+LTempString;

        LTempString:=PadDouble(LGaugeStochastics.Variate1);
        LOutString:=LOutString+LTempString;

        LTempString:=PadDouble(LGaugeStochastics.Variate2);
        LOutString:=LOutString+LTempString;

        LParamFile.Add(LOutString);

        //Line 7
        LOutString := '';
        LTempString:=PadInt(LGaugeStochastics.TransformType);
        LOutString:=LOutString+LTempString+' ';

        LTempString:=PadDouble(LGaugeStochastics.TransformGamma);
        LOutString:=LOutString+LTempString;

        LTempString:=PadDouble(LGaugeStochastics.TransformDelta);
        LOutString:=LOutString+LTempString;

        LTempString:=PadDouble(LGaugeStochastics.TransformXlam);
        LOutString:=LOutString+LTempString;

        LTempString:=PadDouble(LGaugeStochastics.TransformXi);
        LOutString:=LOutString+LTempString;

        LParamFile.Add(LOutString);

        //Line 8
        LOutString := '';

        LTempString:=PadDouble(LGaugeStochastics.ResidualMean);
        LOutString:=LOutString+LTempString;

        LTempString:=PadDouble(LGaugeStochastics.ResidualStdDev);
        LOutString:=LOutString+LTempString;

        LTempString:=PadDouble(LGaugeStochastics.ArmaPhi1);
        LOutString:=LOutString+LTempString;

        LTempString:=PadDouble(LGaugeStochastics.ArmaPhi2);
        LOutString:=LOutString+LTempString;

        LTempString:=PadDouble(LGaugeStochastics.ArmaTheta1);
        LOutString:=LOutString+LTempString;

        LTempString:=PadDouble(LGaugeStochastics.ArmaTheta2);
        LOutString:=LOutString+LTempString;

        LTempString:=PadDouble(LGaugeStochastics.PhiZero);
        LOutString:=LOutString+LTempString;

        LTempString:=PadInt(LGaugeStochastics.ZTVariates);
        LOutString:=LOutString+LTempString;

        LParamFile.Add(LOutString);

        //Line 9
        LOutString := '';

        LTempString:=PadDouble(LGaugeStochastics.ParamXA);
        LOutString:=LOutString+LTempString;

        LTempString:=PadDouble(LGaugeStochastics.ParamXSD);
        LOutString:=LOutString+LTempString;

        LTempString:=PadDouble(LGaugeStochastics.ParamAIC);
        LOutString:=LOutString+LTempString;

        LTempString:=PadDouble(LGaugeStochastics.ParamANC);
        LOutString:=LOutString+LTempString;

        if(FAppModules.StudyArea.ModelVersion = '7' ) then
        begin
          if(LGaugeStochastics.CatchmentArea.FData <= 0) or (not LGaugeStochastics.CatchmentArea.FInitalised )then
          begin
            LGaugeStochastics.CatchmentArea.FData := 1.0;
            LGaugeStochastics.CatchmentArea.FInitalised := True;
          end;
          LTempString:=PadDouble(LGaugeStochastics.CatchmentArea);
          LOutString:=LOutString+LTempString;
        end;
        LParamFile.Add(LOutString);
      end;

      AProgressFunction('',ptNone,LStop);
      if LStop then Exit;

      //Line MatrixB
      LParamFile.Add('');
      LOutString:=PadString(LParamObject.MatrixComments.MatrixBComment);
      LParamFile.Add(LOutString);

      for LCount := 0 to LParamObject.MatrixB.ItemsCount - 1 do
      begin

        if((LCount mod 100) = 0) then
        begin
          AProgressFunction('',ptNone,LStop);
          if LStop then Exit;
        end;

        LMatrixLine := LParamObject.MatrixB.MatrixLineByIndex[LCount];
        if not Assigned(LMatrixLine) then
          Exit;

        LOutString := '';
        for LIndex := MinMatrix to MaxMatrix do
        begin
          if not LMatrixLine.MatrixLine[LIndex].FInitalised then
            Break;
          LTempString:=PadDouble(LMatrixLine.MatrixLine[LIndex]);
          LOutString:=LOutString+LTempString;
        end;
        LParamFile.Add(LOutString);
      end;

      AProgressFunction('',ptNone,LStop);
      if LStop then Exit;

      //Line MatrixB0
      LParamFile.Add('');
      LOutString:=PadString(LParamObject.MatrixComments.MatrixB0Comment);
      LParamFile.Add(LOutString);

      for LCount := 0 to LParamObject.MatrixB0.ItemsCount - 1 do
      begin

        if((LCount mod 100) = 0) then
        begin
          AProgressFunction('',ptNone,LStop);
          if LStop then Exit;
        end;

        LMatrixLine := LParamObject.MatrixB0.MatrixLineByIndex[LCount];
        if not Assigned(LMatrixLine) then
          Exit;

        LOutString := '';
        for LIndex := MinMatrix to MaxMatrix do
        begin
          if not LMatrixLine.MatrixLine[LIndex].FInitalised then
            Break;
          LTempString:=PadDouble(LMatrixLine.MatrixLine[LIndex]);
          LOutString:=LOutString+LTempString;
        end;
        LParamFile.Add(LOutString);
      end;

      AProgressFunction('',ptNone,LStop);
      if LStop then Exit;

      //Line MatrixB1
      LParamFile.Add('');
      LOutString:=PadString(LParamObject.MatrixComments.MatrixB1Comment);
      LParamFile.Add(LOutString);

      for LCount := 0 to LParamObject.MatrixB1.ItemsCount - 1 do
      begin

        if((LCount mod 100) = 0) then
        begin
          AProgressFunction('',ptNone,LStop);
          if LStop then Exit;
        end;

        LMatrixLine := LParamObject.MatrixB1.MatrixLineByIndex[LCount];
        if not Assigned(LMatrixLine) then
          Exit;

        LOutString := '';
        for LIndex := MinMatrix to MaxMatrix do
        begin
          if not LMatrixLine.MatrixLine[LIndex].FInitalised then
            Break;
          LTempString:=PadDouble(LMatrixLine.MatrixLine[LIndex]);
          LOutString:=LOutString+LTempString;
        end;
        LParamFile.Add(LOutString);
      end;

      AProgressFunction('',ptNone,LStop);
      if LStop then Exit;

      //Line MatrixA
      LParamFile.Add('');
      LOutString:=PadString(LParamObject.MatrixComments.MatrixAComment);
      LParamFile.Add(LOutString);

      for LCount := 0 to LParamObject.MatrixA.ItemsCount - 1 do
      begin

        if((LCount mod 100) = 0) then
        begin
          AProgressFunction('',ptNone,LStop);
          if LStop then Exit;
        end;

        LMatrixLine := LParamObject.MatrixA.MatrixLineByIndex[LCount];
        if not Assigned(LMatrixLine) then
          Exit;

        LOutString := '';
        for LIndex := MinMatrix to MaxMatrix do
        begin
          if not LMatrixLine.MatrixLine[LIndex].FInitalised then
            Break;
          LTempString:=PadDouble(LMatrixLine.MatrixLine[LIndex]);
          LOutString:=LOutString+LTempString;
        end;
        LParamFile.Add(LOutString);
      end;

      AProgressFunction('',ptNone,LStop);
      if LStop then Exit;

      //Line MatrixC
      LParamFile.Add('');
      LOutString:=PadString(LParamObject.MatrixComments.MatrixCComment);
      LParamFile.Add(LOutString);

      for LCount := 0 to LParamObject.MatrixC.ItemsCount - 1 do
      begin

        if((LCount mod 100) = 0) then
        begin
          AProgressFunction('',ptNone,LStop);
          if LStop then Exit;
        end;

        LMatrixLine := LParamObject.MatrixC.MatrixLineByIndex[LCount];
        if not Assigned(LMatrixLine) then
          Exit;

        LOutString := '';
        for LIndex := MinMatrix to MaxMatrix do
        begin
          if not LMatrixLine.MatrixLine[LIndex].FInitalised then
            Break;
          LTempString:=PadDouble(LMatrixLine.MatrixLine[LIndex]);
          LOutString:=LOutString+LTempString;
        end;
        LParamFile.Add(LOutString);
      end;

      LParamFile.Add('');
      LParamFile.SaveToFile(AFilename.FileName);
      SetFileDate(AFileName);

      LMessage := FAppModules.Language.GetString('TFileParamAgent.strWritingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);
      Result := True;
    finally
      LParamFile.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
