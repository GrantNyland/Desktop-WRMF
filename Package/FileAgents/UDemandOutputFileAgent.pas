//
//
//  UNIT      : Contains TDemandOutputFileAgent Class
//  AUTHOR    : Dziedzi Ramulondi
//  DATE      : 17/05/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UDemandOutputFileAgent;

interface

uses
  Classes, sysutils,contnrs,

  //  DWAF VCL
  UConstants,
  UFileNames,
  UAbstractObject,
  UDataFileObjects,
  UAbstractFileNamesObject,
  UAbstractFileAgent,
  UHydrologyFilesObject,
  UYieldModelDataObject;


type
  TDemandOutputFileAgent = class(TAbstractFileAgent)
  protected
    function WeAreInTheCorrectBlock(ALineData,ANetworkFeatureName : string;AChannelNumber,ACurrentSequence,ACurrentLoadCase: integer): boolean;
  public
    function ReadModelDataFromFile(AFileName:TAbstractModelFileName;  ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
    function WriteModelDataToFile(AFileName:TAbstractModelFileName;  ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
    function ReadChannelDataCommaText(AChannelNumber,ASequenceCount,ALoadCaseCount,AYearInAnalysis,ACurrentSequence,ACurrentLoadCase : integer; ANetworkFeatureName: string; ADataContainer: TStrings): boolean;
   end;

implementation



uses UUtilities,
     UErrorHandlingOperations;



function TDemandOutputFileAgent.WeAreInTheCorrectBlock(ALineData,ANetworkFeatureName: string;
         AChannelNumber, ACurrentSequence, ACurrentLoadCase: integer): boolean;
const OPNAME = 'TDemandOutputFileAgent.WeAreInTheCorrectBlock';
var
  LChannelNumber    : integer;
  LCurrentSequence  : integer;
  LCurrentLoadCase  : integer;
  LLineData         : string;
  LData             : string;
  LIndex            : integer;
begin
  Result := False;
  try
    LLineData := UpperCase(Trim(ALineData));
    
    //Check if this is line 4
    if(Pos('TARGET DRAFT =',LLineData) <> 1) then Exit;

    //Check if this is the correct network feature
    ANetworkFeatureName := UpperCase(ANetworkFeatureName);
    if(Pos(ANetworkFeatureName,LLineData) = 0) then Exit;

    //Check if Target drafts are the same
    Delete(LLineData,1,Length('TARGET DRAFT ='));
    LData := ExtractFirstSubstring(LLineData);
    LCurrentLoadCase := StrToIntDef(LData,-1);
    if(LCurrentLoadCase <> ACurrentLoadCase) then Exit;

    //Check if Sequences the same
    LLineData := Trim(LLineData);
    if(Pos('HISTORIC SEQUENCE',LLineData)=1) then
    begin
      if (ACurrentSequence <> 1) then Exit;
    end;
    if(Pos('STOCHASTIC SEQUENCE',LLineData)=1) then
    begin
      Delete(LLineData,1,Length('STOCHASTIC SEQUENCE'));
      LData := ExtractFirstSubstring(LLineData);
      LCurrentSequence := StrToIntDef(LData,-1);
      if(LCurrentSequence <> ACurrentSequence) then Exit;
    end;

    //Check if channel number is the same
    LIndex := Pos(' CHANNEL = ',LLineData);
    if(LIndex = 0) then  Exit;
    Delete(LLineData,1,(LIndex+Length(' CHANNEL =')));
    LData := ExtractFirstSubstring(LLineData);
    LChannelNumber := StrToIntDef(LData,-1); //NB: We must fix YIELD Channel
    if(LChannelNumber <> AChannelNumber) then Exit;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDemandOutputFileAgent.ReadChannelDataCommaText(
         AChannelNumber,ASequenceCount,ALoadCaseCount,AYearInAnalysis,ACurrentSequence,ACurrentLoadCase : integer;
         ANetworkFeatureName: string; ADataContainer: TStrings): boolean;
const OPNAME = 'TDemandOutputFileAgent.ReadChannelDataCommaText';
Var
  LModelFileName              : TAbstractModelFileName;
  LFileName  :string;
  LFileData  : TStringList;
  LLineData : string;
  LCurrentline : integer;
  LIndex : integer;
  LCount : integer;
begin
  Result := False;
  try
    ADataContainer.Clear;
    LModelFileName  := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.GetDemandOutFile;
    if(LModelFileName = nil) then
      Exit;
    LFileName :=  LModelFileName.FileName;
    //Check if file exists.
    If not FileExists(LFileName) then
      Exit;

    LFileData := TStringList.Create;
    try
      //Read  file
      LFileData.LoadFromFile(LFileName);
      for LCurrentline := 0 to LFileData.Count - 1 do
      begin
        LLineData  := Trim(LFileData[LCurrentline]);
        if WeAreInTheCorrectBlock(LLineData,ANetworkFeatureName,AChannelNumber,ACurrentSequence,ACurrentLoadCase) then
        begin
          LIndex := LCurrentline + 4;
          for LCount := LIndex to LFileData.Count - 1 do
          begin
            if(LCount >= LFileData.Count) then Break;
            LLineData  := Trim(UpperCase(LFileData[LCount]));
            if(Length(LLineData) = 0) then Break;
            if(Pos('--------',LLineData) > 0) then Break;
            if(Pos('========',LLineData) > 0) then Break;
            if(Pos('AVE',LLineData) > 0) then Break;
            if(Pos('TOTAL',LLineData) > 0) then Break;

            while(Pos('  ',LLineData) > 0) do
              LLineData := StringReplace(LLineData,'  ',' ',[rfReplaceAll]);
            LLineData := StringReplace(LLineData,' ',',',[rfReplaceAll]);
            ADataContainer.Add(LLineData);
          end;
          if(ADataContainer.Count > 0) then
            Break;
        end;
      end;
      Result := ADataContainer.Count > 0;
    finally
      LFileData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TDemandOutputFileAgent.ReadModelDataFromFile(AFileName: TAbstractModelFileName; ADataObject: TDataFileObjects;
  AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TDemandOutputFileAgent.ReadModelDataFromFile';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDemandOutputFileAgent.WriteModelDataToFile(AFileName: TAbstractModelFileName; ADataObject: TDataFileObjects;
  AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TDemandOutputFileAgent.WriteModelDataToFile';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.


