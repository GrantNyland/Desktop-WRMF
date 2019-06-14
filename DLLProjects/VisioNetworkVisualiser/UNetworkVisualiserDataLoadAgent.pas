//
//
//  UNIT      : Contains TNetworkVisualiserDataLoadAgent Class
//  AUTHOR    : Dziedzi Ramulondi
//  DATE      : 2005/02/28
//  COPYRIGHT : Copyright © 2005 DWAF
//
//
unit UNetworkVisualiserDataLoadAgent;

interface

uses
  Classes,
  VCL.Controls,
  UReservoirData,
  UAbstractObject,
  UNetworkVisualiserData,
  UNetworkVisualiserDataSQLAgent;

type
  TNetworkVisualiserDataLoadAgent = class(TAbstractAppObject)
  protected
    FSQLAgent  : TNetworkVisualiserDataSQLAgent;
    FParentTab : TWinControl;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function Get_DefaultDrawingFileName(AGISMode: boolean): string;
    function LoadBackGroundPicture(AFileName : string): boolean;
  public
	  function GetVSDFileName (AGroupName   : string;
                             ADrawingName : string): string;
    function ConstructData (ADrawingData : TDrawingData): boolean;
    function CreateDrawingGroup (ADrawingGroup : TDrawingGroup): boolean;
    function DeleteDrawingGroup (AGroupId : integer): boolean;
    function CreateDrawing (ADrawingGroup : TDrawingGroup;
                            ADrawing      : TDrawing): boolean;
    function CopyDrawing (ATargetGroup   : TDrawingGroup;
                          ATargetDrawing : TDrawing;
                          ASourceGroup   : TDrawingGroup;
                          ASourceDrawing : TDrawing): boolean;
    function DeleteDrawing (ADrawingGroup : TDrawingGroup;
                            ADrawing      : TDrawing): boolean;
    function UpdateGroupName (AGroupID : integer;
                              ANewName : string): boolean;
    function UpdateDrawingName (AGroupID   : integer;
                                ADrawingID : integer;
                                ANewName   : string): boolean;
    function SetDrawingReadOnlyFlag (AGroupID   : integer;
                                     ADrawingID : integer): boolean;
    function RemoveDrawingReadOnlyFlag (AGroupID   : integer;
                                        ADrawingID : integer): boolean;
    function IsDrawingReadOnly (AGroupID   : integer;
                                ADrawingID : integer):boolean;
    //property ParentTab : TWinControl read FParentTab write FParentTab;

  end;

implementation

uses
  DB,
  Windows,
  SysUtils,                           
  UDataSetType,
  VoaimsCom_TLB,
  Visio_TLB,
  VisOcx_TLB,
  UUtilities,
  VCL.Dialogs,
  UErrorHandlingOperations;

procedure TNetworkVisualiserDataLoadAgent.CreateMemberObjects;
const OPNAME = 'TNetworkVisualiserDataLoadAgent.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FSQLAgent := TNetworkVisualiserDataSQLAgent.Create(FAppModules);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TNetworkVisualiserDataLoadAgent.DestroyMemberObjects;
const OPNAME = 'TNetworkVisualiserDataLoadAgent.DestroyMemberObjects';
begin
  try
    FreeAndNil(FSQLAgent);
    inherited DestroyMemberObjects;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TNetworkVisualiserDataLoadAgent.ConstructData (ADrawingData : TDrawingData): boolean;
const OPNAME = 'TNetworkVisualiserDataLoadAgent.ConstructData';
var
   LDataSet: TAbstractModelDataset;
   LDrawingID,
   LDrawingGroupID: integer;
   LDrawingName,
   LDrawingGroupName: string;
   LDrawing:TDrawing;
   LDrawingGroup: TDrawingGroup;
   LGISMode: boolean;
begin
  Result := False;
  try
    ADrawingData.Initialise;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(FSQLAgent.GetDrawingGroupSQL);
        LDataSet.DataSet.Open;
        while not LDataSet.DataSet.Eof do
        begin
          LDrawingGroupID   := LDataSet.DataSet.FieldByName('DrawingGroupID').AsInteger;
          LDrawingGroupName := Trim(LDataSet.DataSet.FieldByName('DrawingGroupName').AsString);
          LDrawingGroup     := ADrawingData.DrawingGroupList.CreateDrawingGroup(LDrawingGroupName);
          LDrawingGroup.Populate(LDrawingGroupID,LDrawingGroupName);
          LDataSet.DataSet.Next;
        end;
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(FSQLAgent.GetDrawingSQL);
        LDataSet.DataSet.Open;
        while not LDataSet.DataSet.Eof do
        begin
          LDrawingGroupID := LDataSet.DataSet.FieldByName('DrawingGroupID').AsInteger;
          LDrawingID      := LDataSet.DataSet.FieldByName('DrawingID').AsInteger;
          LDrawingName    := Trim(LDataSet.DataSet.FieldByName('DrawingName').AsString);
          LGISMode        := Trim(LDataSet.DataSet.FieldByName('GISMode').AsString) = 'Y';
          LDrawingGroup   := ADrawingData.DrawingGroupList.DrawingGroupByID[LDrawingGroupID];
          if (LDrawingGroup <> nil) then
          begin
            LDrawing  := LDrawingGroup.DrawingList.CreateDrawing(LDrawingName);
            LDrawing.Populate(LDrawingGroupID,LDrawingID,LDrawingName,LGISMode);
          end
          else
            Result := False;
          LDataSet.DataSet.Next;
        end;
        LDataSet.DataSet.Close;
      end;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TNetworkVisualiserDataLoadAgent.CreateDrawingGroup (ADrawingGroup : TDrawingGroup): boolean;
const OPNAME = 'TNetworkVisualiserDataLoadAgent.CreateDrawingGroup';
var
  LDataSet : TAbstractModelDataset;
  LMaxId   : integer;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LMaxId := 0;
        LDataSet.SetSQL(FSQLAgent.GetMaxDrawingGroupIdSQL);
        LDataSet.DataSet.Open;
        if not(LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
        begin
          LMaxId := LDataSet.DataSet.FieldByName('MAXDrawingGroupID').AsInteger;
        end;
        LMaxId := LMaxId + 1;
        LDataSet.SetSQL(FSQLAgent.CreateDrawingGroupSQL);
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['DrawingGroupID'], [IntToStr(LMaxId)]);
        LDataSet.SetParams(['DrawingGroupName'], [ADrawingGroup.DrawingGroupName]);
        LDataSet.ExecSQL;
        ADrawingGroup.Populate(LMaxId,ADrawingGroup.DrawingGroupName);
        Result := True;
      end;
    finally
      LDataSet.Free;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TNetworkVisualiserDataLoadAgent.DeleteDrawingGroup(AGroupId: integer): boolean;
const OPNAME = 'TNetworkVisualiserDataLoadAgent.DeleteDrawingGroup';
var
  LDataSet : TAbstractModelDataset;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(FSQLAgent.DeleteDrawingInGroupSQL);
        LDataSet.SetParams(['DrawingGroupID'], [IntToStr(AGroupId)]);
        LDataSet.ExecSQL;

        LDataSet.DataSet.Close;
        LDataSet.SetSQL(FSQLAgent.DeleteDrawingGroupSQL);
        LDataSet.SetParams(['DrawingGroupID'], [IntToStr(AGroupId)]);
        LDataSet.ExecSQL;

        Result := True;
      end;
    finally
      LDataSet.Free;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

{$WARN SYMBOL_PLATFORM OFF}

function TNetworkVisualiserDataLoadAgent.Get_DefaultDrawingFileName(AGISMode: boolean): string;
const OPNAME = 'TNetworkVisualiserDataLoadAgent.Get_DefaultDrawingFileName';
begin
  Result := '';
  try
    if(FAppModules.Model.ModelName = CYield) then
    begin
      if AGISMode then
        Result := NetworkDiagramsPath + 'DefaultDrawingYieldGIS.VSD'
      else
        Result := NetworkDiagramsPath + 'DefaultDrawingYield.VSD';
    end
    else
    if(FAppModules.Model.ModelName = CPlanning) then
    begin
      if AGISMode then
        Result := NetworkDiagramsPath + 'DefaultDrawingPlannGIS.VSD'
      else
        Result := NetworkDiagramsPath + 'DefaultDrawingPlann.VSD';
    end
    else
    if(FAppModules.Model.ModelName = CHydrology) then
    begin
      if AGISMode then
        Result := NetworkDiagramsPath + 'DefaultDrawingHydroGIS.VSD'
      else
        Result := NetworkDiagramsPath + 'DefaultDrawingHydro.VSD';
    end
    else
    begin
      if AGISMode then
        Result := NetworkDiagramsPath + 'DefaultDrawingGIS.VSD'
      else
        Result := NetworkDiagramsPath + 'DefaultDrawing.VSD';
    end;

  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TNetworkVisualiserDataLoadAgent.CreateDrawing (ADrawingGroup : TDrawingGroup;
                                                        ADrawing      : TDrawing): boolean;
const OPNAME = 'TNetworkVisualiserDataLoadAgent.CreateDrawing';
var
  LDataSet     : TAbstractModelDataset;
  LMaxId       : integer;
  LPath        : string;
  LFileName    : string;
  lDefaultFile : string;
  lAttributes  : word;
  LGISMode     : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        FAppModules.Database.StartTransaction;
        try
          LMaxId := 0;
          LDataSet.SetSQL(FSQLAgent.GetMaxDrawingIdSQL);
          LDataSet.DataSet.Open;
          if not(LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
          begin
            LMaxId := LDataSet.DataSet.FieldByName('MAXDrawingID').AsInteger;
          end;
          LMaxId := LMaxId + 1;
          if ADrawing.GISMode then
            LGISMode := 'Y'
          else
            LGISMode := 'N';

          LDataSet.DataSet.Close;
          LDataSet.SetSQL(FSQLAgent.CreateDrawingSQL);
          LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
          LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
          LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
          LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
          LDataSet.SetParams(['DrawingGroupID'], [IntToStr(ADrawingGroup.DrawingGroupID)]);
          LDataSet.SetParams(['DrawingID'], [IntToStr(LMaxId)]);
          LDataSet.SetParams(['DrawingName'], [ADrawing.DrawingName]);
          LDataSet.SetParams(['GISMode'], [LGISMode]);
          LDataSet.ExecSQL;
          ADrawing.Populate(ADrawingGroup.DrawingGroupID,LMaxId,ADrawing.DrawingName,ADrawing.GISMode);
          lDefaultFile := Get_DefaultDrawingFileName(ADrawing.GISMode);
          if (FileExists(lDefaultFile)) then
          begin
            LFileName := GetVSDFileName(ADrawingGroup.DrawingGroupName, ADrawing.DrawingName);
            LPath     := ExtractFilePath(LFileName);
            if (NOT DirectoryExists(LPath)) then
              ForceDirectories(LPath);
            Result := CopyFile(PChar(lDefaultFile), PChar(LFileName), TRUE);
            lAttributes := FileGetAttr(lDefaultFile);
            lAttributes := lAttributes AND (NOT faReadOnly);
            FileSetAttr(LFileName, lAttributes);
          end;
          if ADrawing.GISMode then
          begin
            LoadBackGroundPicture(LFileName);
          end;
          if Result then
            FAppModules.Database.Commit
          else
            FAppModules.Database.Rollback;
        except on E : Exception do
          begin
            FAppModules.Database.Rollback;
            raise;
          end;
        end;
      end;
    finally
      LDataSet.Free;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TNetworkVisualiserDataLoadAgent.LoadBackGroundPicture(AFileName: string): boolean;
const OPNAME = 'TNetworkVisualiserDataLoadAgent.LoadBackGroundPicture';
var
  LShape       : IVShape;
  LPage1        : IVPage;
  LPage2        : IVPage;
  LDrawingApp  : TDrawingControl;
  LPath,
  LPictureFile : string;
begin
  Result := False;
  try
    LPath := NetworkDiagramsPath +
             ChopCharacters(FAppModules.StudyArea.StudyAreaCode) + '\' +
             ChopCharacters(FAppModules.StudyArea.SubAreaCode)   + '\' +
             ChopCharacters(FAppModules.StudyArea.ScenarioCode)  + '\';
    if PromptForFileName(LPictureFile,'Bitmap files (*.bmp)|*.BMP','BMP','Select backgroud picture file',LPath,False) then
    begin
      LDrawingApp := TDrawingControl.Create(nil);
      try
        LDrawingApp.Visible := false;
        LDrawingApp.Parent  := nil;
        LDrawingApp.Src     := AFileName;
        if(LDrawingApp.Document.Pages.Count > 1) then
        begin
          LPage1 := LDrawingApp.Document.Pages.Item[1];
          LPage2 := LDrawingApp.Document.Pages.Item[2];
          if(LPage2 <> nil) then
          begin
            LShape := LPage2.Import(LPictureFile);
            LPage2.CenterDrawing;
            //LShape.CellsU['PinX'].Formula := '0';
            //LShape.CellsU['PinY'].Formula := '0';
            LPage1.BackPageFromName := LPage2.Name;
            LDrawingApp.Document.SaveAs(AFileName);
          end;
        end;
      finally
        LDrawingApp.Free;
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;
{$WARN SYMBOL_PLATFORM ON}

function TNetworkVisualiserDataLoadAgent.CopyDrawing (ATargetGroup   : TDrawingGroup;
                                                      ATargetDrawing : TDrawing;
                                                      ASourceGroup   : TDrawingGroup;
                                                      ASourceDrawing : TDrawing): boolean;
const OPNAME = 'TNetworkVisualiserDataLoadAgent.CopyDrawing';
var
  LDataSet        : TAbstractModelDataset;
  LMaxId          : integer;
  LSourceFileName : string;
  LTargetPath     : string;
  LTargetFileName : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        FAppModules.Database.StartTransaction;
        try
          LMaxId := 0;
          LDataSet.SetSQL(FSQLAgent.GetMaxDrawingIdSQL);
          LDataSet.DataSet.Open;
          if not(LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
          begin
            LMaxId := LDataSet.DataSet.FieldByName('MAXDrawingID').AsInteger;
          end;
          LMaxId := LMaxId + 1;

          LDataSet.DataSet.Close;
          LDataSet.SetSQL(FSQLAgent.CreateDrawingSQL);
          LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
          LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
          LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
          LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
          LDataSet.SetParams(['DrawingGroupID'], [IntToStr(ATargetGroup.DrawingGroupID)]);
          LDataSet.SetParams(['DrawingID'], [IntToStr(LMaxId)]);
          LDataSet.SetParams(['DrawingName'], [ATargetDrawing.DrawingName]);
          LDataSet.SetParams(['GISMode'], ['N']);
          LDataSet.ExecSQL;
          ATargetDrawing.Populate(ATargetGroup.DrawingGroupID,LMaxId,ATargetDrawing.DrawingName,False);

          LSourceFileName := GetVSDFileName(ASourceGroup.DrawingGroupName, ASourceDrawing.DrawingName);
          if (FileExists(LSourceFileName)) then
          begin
            LTargetFileName := GetVSDFileName(ATargetGroup.DrawingGroupName, ATargetDrawing.DrawingName);
            LTargetPath     := ExtractFilePath(LTargetFileName);
            if (NOT DirectoryExists(LTargetPath)) then
              ForceDirectories(LTargetPath);
            Result := CopyFile(PChar(LSourceFileName), PChar(LTargetFileName), TRUE);
          end
          else
            ShowMessage(FAppModules.Language.GetString('VNV.DrawingFileNotFound'));
          if Result then
            FAppModules.Database.Commit
          else
            FAppModules.Database.Rollback;
        except on E : Exception do
          begin
            FAppModules.Database.Rollback;
            raise;
          end;
        end;
      end;
    finally
      LDataSet.Free;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TNetworkVisualiserDataLoadAgent.IsDrawingReadOnly (AGroupID   : integer;
                                                            ADrawingID : integer): boolean;
const OPNAME = 'TNetworkVisualiserDataLoadAgent.IsDrawingReadOnly';
var
  LDataSet : TAbstractModelDataset;
  LTempStr : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    if Assigned(LDataSet)then
    begin
    	try
        LTempStr := 'SELECT ReadOnly FROM VNVDrawing where DrawingGroupID = ' +
                    IntToStr(AGroupID) + ' AND ' + ' DrawingID = ' + IntToStr(ADrawingID);
        LDataSet.SetSQL(LTempStr);
        LDataSet.DataSet.Open;
        if not(LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
        begin
          if LDataSet.DataSet.FieldByName('ReadOnly').AsInteger <> 0 then
            Result := True;
        end
      finally
      	LDataSet.Free;
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TNetworkVisualiserDataLoadAgent.DeleteDrawing (ADrawingGroup : TDrawingGroup;
                                                        ADrawing      : TDrawing): boolean;
const OPNAME = 'TNetworkVisualiserDataLoadAgent.DeleteDrawing';
var
  LDataSet  : TAbstractModelDataset;
  lFileName : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(FSQLAgent.DeleteDrawingSQL);
        LDataSet.SetParams(['DrawingGroupID'], [IntToStr(ADrawingGroup.DrawingGroupID)]);
        LDataSet.SetParams(['DrawingID'], [IntToStr(ADrawing.DrawingID)]);
        LDataSet.ExecSQL;

        lFileName := GetVSDFileName(ADrawingGroup.DrawingGroupName, ADrawing.DrawingName);
        if (FileExists(lFileName)) then
          DeleteFile(lFileName);

        Result := True;
      end;
    finally
      LDataSet.Free;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TNetworkVisualiserDataLoadAgent.UpdateDrawingName (AGroupID   : integer;
                                                            ADrawingID : integer;
                                                            ANewName   : string): boolean;
const OPNAME = 'TNetworkVisualiserDataLoadAgent.UpdateDrawingName';
var
  LDataSet : TAbstractModelDataset;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(FSQLAgent.UpdateDrawingNameSQL);
        LDataSet.SetParams(['NewName'], [ANewName]);
        LDataSet.SetParams(['DrawingGroupID'], [IntToStr(AGroupId)]);
        LDataSet.SetParams(['DrawingID'], [IntToStr(ADrawingID)]);
        LDataSet.ExecSQL;
        Result := True;
      end;
    finally
      LDataSet.Free;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TNetworkVisualiserDataLoadAgent.SetDrawingReadOnlyFlag (AGroupID   : integer;
                                                                 ADrawingID : integer): boolean;
const OPNAME = 'TNetworkVisualiserDataLoadAgent.SetDrawingReadOnlyFlag';
var
  LDataSet : TAbstractModelDataset;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(FSQLAgent.SetDrawingReadOnlySQL);
        LDataSet.SetParams(['DrawingGroupID'], [IntToStr(AGroupId)]);
        LDataSet.SetParams(['DrawingID'], [IntToStr(ADrawingID)]);
        LDataSet.ExecSQL;
        Result := True;
      end;
    finally
      LDataSet.Free;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TNetworkVisualiserDataLoadAgent.RemoveDrawingReadOnlyFlag(AGroupId,ADrawingID: integer): boolean;
const OPNAME = 'TNetworkVisualiserDataLoadAgent.RemoveDrawingReadOnlyFlag';
var
  LDataSet : TAbstractModelDataset;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(FSQLAgent.RemoveDrawingReadOnlySQL);
        LDataSet.SetParams(['DrawingGroupID'], [IntToStr(AGroupId)]);
        LDataSet.SetParams(['DrawingID'], [IntToStr(ADrawingID)]);
        LDataSet.ExecSQL;
        Result := True;
      end;
    finally
      LDataSet.Free;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TNetworkVisualiserDataLoadAgent.UpdateGroupName (AGroupID : integer;
                                                          ANewName : string): boolean;
const OPNAME = 'TNetworkVisualiserDataLoadAgent.UpdateGroupName';
var
  LDataSet : TAbstractModelDataset;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(FSQLAgent.UpdateGroupNameSQL);
        LDataSet.SetParams(['NewName'], [ANewName]);
        LDataSet.SetParams(['DrawingGroupID'], [IntToStr(AGroupId)]);
        LDataSet.ExecSQL;
        LDataSet.DataSet.Close;
        Result := True;
      end;
    finally
      LDataSet.Free;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TNetworkVisualiserDataLoadAgent.GetVSDFileName (AGroupName   : string;
                                                         ADrawingName : string) : string;
const OPNAME = 'TNetworkVisualiserDataLoadAgent.GetVSDFileName';
var
  lFileName : string;
  lPath     : string;
begin
  Result := '';
  try
    LPath := NetworkDiagramsPath +
             ChopCharacters(FAppModules.StudyArea.StudyAreaCode) + '\' +
             ChopCharacters(FAppModules.StudyArea.SubAreaCode)   + '\' +
             ChopCharacters(FAppModules.StudyArea.ScenarioCode)  + '\';
    if not DirectoryExists(LPath) then
       ForceDirectories(LPath);
    LFileName := LPath +  AGroupName + '-' + ADrawingName + '.VSD';
    Result := LFileName;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

end.
