//
//
//  UNIT      : Contains TYRCModelDataLoadAgent Class
//  AUTHOR    : Titi Ngubane (Arivia)
//  DATE      : 2003/07/10
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UYRCModelDataLoadAgent;

interface

uses
  Classes,
  UFileNames,
  UAbstractYRCModelDataObject,
  UYRCGraphDataObject,
  UYRCModelDataObject,
  USumOutFileManager,
  UAbstractObject;

type
  TYRCModelDataLoadAgent = class(TAbstractAppObject)
  protected
    FSumOutFileManager: TSumOutFileManager;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function ReadYRCDataFromFile(AYRCGraphDataObject : TYRCGraphDataObject;AFileNames:TModelFileNames):boolean;virtual;
    function ReadYRCDataFromDB(AYRCGraphDataObject : TYRCGraphDataObject):boolean;virtual;
    function SaveYRCDataToDB(AYRCGraphDataObject : TYRCGraphDataObject):boolean;virtual;
    function DeleteYRCDataFromDB(AYRCGraphDataObject : TYRCGraphDataObject):boolean;virtual;

  end;

implementation

uses
  SysUtils,
  System.UITypes,
  UConstants,
  UDataSetType,
  UUtilities,
  Contnrs,
  VCL.Controls,
  VCL.Dialogs,
  UYRCMergeSumOutFilesDialog,
  UFileNameConstants,
  UAbstractFileNamesObject,
  UErrorHandlingOperations;

procedure TYRCModelDataLoadAgent.CreateMemberObjects;
const OPNAME = 'TYRCModelDataLoadAgent.CreateMemberObjects';
begin
  inherited;
  try
    FSumOutFileManager   := TSumOutFileManager.Create(FAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCModelDataLoadAgent.DestroyMemberObjects;
const OPNAME = 'TYRCModelDataLoadAgent.DestroyMemberObjects';
begin
  inherited;
  try
    FreeAndNil(FSumOutFileManager);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCModelDataLoadAgent.ReadYRCDataFromDB(AYRCGraphDataObject : TYRCGraphDataObject): boolean;
const OPNAME = 'TYRCModelDataLoadAgent.ReadYRCDataFromDB';
var
  LPrimaryKeys : array[0..4] of string;
begin
  Result := False;
  try
    AYRCGraphDataObject.ErrorMsg := '';
    LPrimaryKeys[0] := FAppModules.Database.DatabaseName;
    LPrimaryKeys[1] := FAppModules.StudyArea.ModelCode;
    LPrimaryKeys[2] := FAppModules.StudyArea.StudyAreaCode;
    LPrimaryKeys[3] := FAppModules.StudyArea.SubAreaCode;
    LPrimaryKeys[4] := FAppModules.StudyArea.ScenarioCode;
    Result :=  AYRCGraphDataObject.LoadDataFromDB(LPrimaryKeys);
    if (AYRCGraphDataObject.PlanesCount = 0) then
      AYRCGraphDataObject.ErrorMsg := 'CaptionNoDbData';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCModelDataLoadAgent.ReadYRCDataFromFile(AYRCGraphDataObject : TYRCGraphDataObject;
         AFileNames:TModelFileNames): boolean;
const OPNAME = 'TYRCModelDataLoadAgent.ReadYRCDataFromFile';
var
  LMesgAnswer: word;
  LFileName: string;
  LYRCBlocks: TObjectList;
  LTDToBeDeleteCommaText,
  LTDToBeAddedCommaText: String;
  LFileNameObject: TAbstractModelFileName;
  LYRCGraphDataObject:TYRCGraphDataObject;
  LDialog:TYRCMergeSumOutFilesDialog;
begin
  Result := False;
  try

    LYRCBlocks := TObjectList.Create(True);
    try
      if(AYRCGraphDataObject.PlanesCount > 0) then
      begin
        LMesgAnswer := MessageDlg(FAppModules.Language.GetString('Message.SumOutDataExist'),mtConfirmation,mbYesNoCancel,0);
        case LMesgAnswer of
          mrCancel: Exit;
          mrNo    :
            begin
              AYRCGraphDataObject.ErrorMsg := '';
              LFileNameObject := AFileNames.GetSumOutFile;
              if not Assigned(LFileNameObject) then
                AYRCGraphDataObject.ErrorMsg := 'CaptionNoOutputFile';
              if(AYRCGraphDataObject.ErrorMsg <> '') then Exit;
              if not LFileNameObject.FileFound then
                AYRCGraphDataObject.ErrorMsg := 'CaptionNoOutputFile';
              if(AYRCGraphDataObject.ErrorMsg <> '') then Exit;
              Result := FSumOutFileManager.GetDeficitsBlocksFromFile(LFileNameObject, LYRCBlocks);
              if (LYRCBlocks.Count = 0) then
                AYRCGraphDataObject.ErrorMsg := 'CaptionHistoric'
              else
                Result := Result and TYRCGraphDataObject(AYRCGraphDataObject).LoadDataFromFile(LYRCBlocks,[]);
            end;
          mrYes:
            begin
              //get sum.out file name
              if PromptForFileName(LFileName,'Output Files|*.out','out','Select SUM.OUT file','',False) then
              begin
                LFileNameObject := TFileNameObject.Create(FAppModules);
                TFileNameObject(LFileNameObject).SetFileName(ExtractFilePath(LFileName),LFileName,False,fgOutput,1,0,FileLastWriteDate(LFileName));

                //load sum.out data
                Result := FSumOutFileManager.GetDeficitsBlocksFromFile(LFileNameObject, LYRCBlocks);
                if not Result then Exit;
                if (LYRCBlocks.Count = 0) then
                begin
                  ShowMessage(FAppModules.Language.GetString('TYRCSelectorsPanel.CaptionHistoric'));
                  Exit;
                end
                else
                begin
                  //load data to temp memory
                  LYRCGraphDataObject := TYRCGraphDataObject.Create(FAppModules);
                  try
                    Result := LYRCGraphDataObject.LoadDataFromFile(LYRCBlocks,[]);
                    if not Result then Exit;
                    //validate data
                    if (LYRCGraphDataObject.PlanesCount <> AYRCGraphDataObject .PlanesCount) then
                    begin
                      ShowMessage(FAppModules.Language.GetString('Message.OldSumOut') +
                                  IntToStr(AYRCGraphDataObject.PlanesCount) +
                                  FAppModules.Language.GetString('Message.YearsWhile') +
                                  FAppModules.Language.GetString('Message.NewSumOut') +
                                  IntToStr(LYRCGraphDataObject.PlanesCount) +
                                  FAppModules.Language.GetString('Message.Years') +
                                  FAppModules.Language.GetString('Message.NumberYearsMustBeSame'));
                      Exit;
                    end;
                    if (LYRCGraphDataObject.SequencesCount <> AYRCGraphDataObject.SequencesCount) then
                    begin
                      ShowMessage(FAppModules.Language.GetString('Message.OldSumOut') +
                                  IntToStr(AYRCGraphDataObject.SequencesCount) +
                                  FAppModules.Language.GetString('Message.SequencesWhile') +
                                  FAppModules.Language.GetString('Message.NewSumOut') +
                                  IntToStr(LYRCGraphDataObject.SequencesCount) +
                                  FAppModules.Language.GetString('Message.Sequences') +
                                  FAppModules.Language.GetString('Message.NumberSequencesMustBeSame'));
                      Exit;
                    end;

                    //load data to dialog
                    LDialog := TYRCMergeSumOutFilesDialog.Create(nil);
                    try
                      LDialog.PopulateData(AYRCGraphDataObject.TargetDraftValuesCommaText,
                                           LYRCGraphDataObject.TargetDraftValuesCommaText);
                      LDialog.ShowModal;                     
                      if (LDialog.ModalResult = mrCancel) then Exit;
                      //load data from dialog
                      LDialog.GetSeletedData(LTDToBeDeleteCommaText,LTDToBeAddedCommaText);
                      //merge data
                      Result := AYRCGraphDataObject.MergeData(LYRCGraphDataObject,LTDToBeDeleteCommaText,LTDToBeAddedCommaText);
                    finally
                      LDialog.free;
                    end;
                  finally
                    LYRCGraphDataObject.Free;
                  end;
                end;
              end;
            end;//mrYes
        end;//case
      end
      else
      begin
        AYRCGraphDataObject.ErrorMsg := '';
        LFileNameObject := AFileNames.GetSumOutFile;
        if not Assigned(LFileNameObject) then
          AYRCGraphDataObject.ErrorMsg := 'CaptionNoOutputFile';
        if(AYRCGraphDataObject.ErrorMsg <> '') then Exit;
        if not LFileNameObject.FileFound then
          AYRCGraphDataObject.ErrorMsg := 'CaptionNoOutputFile';
        if(AYRCGraphDataObject.ErrorMsg <> '') then Exit;
        Result := FSumOutFileManager.GetDeficitsBlocksFromFile(LFileNameObject, LYRCBlocks);
        if (LYRCBlocks.Count = 0) then
          AYRCGraphDataObject.ErrorMsg := 'CaptionHistoric'
        else
          Result := Result and TYRCGraphDataObject(AYRCGraphDataObject).LoadDataFromFile(LYRCBlocks,[]);
      end;
    finally
      LYRCBlocks.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCModelDataLoadAgent.SaveYRCDataToDB(AYRCGraphDataObject : TYRCGraphDataObject): boolean;
const OPNAME = 'TYRCModelDataLoadAgent.SaveYRCDataToDB';
var
  LPrimaryKeys : array[0..4] of string;
begin
  Result := False;
  try
    AYRCGraphDataObject.ErrorMsg := '';
    LPrimaryKeys[0] := FAppModules.Database.DatabaseName;
    LPrimaryKeys[1] := FAppModules.StudyArea.ModelCode;
    LPrimaryKeys[2] := FAppModules.StudyArea.StudyAreaCode;
    LPrimaryKeys[3] := FAppModules.StudyArea.SubAreaCode;
    LPrimaryKeys[4] := FAppModules.StudyArea.ScenarioCode;
    Result :=  AYRCGraphDataObject.SaveDataToDB(LPrimaryKeys);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCModelDataLoadAgent.DeleteYRCDataFromDB(AYRCGraphDataObject: TYRCGraphDataObject): boolean;
const OPNAME = 'TYRCModelDataLoadAgent.DeleteYRCDataFromDB';
var
  LPrimaryKeys : array[0..4] of string;
begin
  Result := False;
  try
    AYRCGraphDataObject.ErrorMsg := '';
    LPrimaryKeys[0] := FAppModules.Database.DatabaseName;
    LPrimaryKeys[1] := FAppModules.StudyArea.ModelCode;
    LPrimaryKeys[2] := FAppModules.StudyArea.StudyAreaCode;
    LPrimaryKeys[3] := FAppModules.StudyArea.SubAreaCode;
    LPrimaryKeys[4] := FAppModules.StudyArea.ScenarioCode;
    Result :=  AYRCGraphDataObject.DeleteChartData(LPrimaryKeys);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
