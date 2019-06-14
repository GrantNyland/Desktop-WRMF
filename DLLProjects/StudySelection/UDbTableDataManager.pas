//
//  UNIT      : Contains TDbTableDataManager Class
//  AUTHOR    : Philemon Setshedi(PDNA)
//  DATE      : 2004/03/23
//  COPYRIGHT : Copyright © 2004 DWAF
//
unit UDbTableDataManager;

interface
uses
  Classes,
  Vcl.StdCtrls,
  Vcl.Graphics,
  Vcl.ComCtrls,
  Variants,
  SysUtils,
  IdGlobal,
  idglobalProtocols,
  Vcl.Forms,
  Contnrs,
  Vcl.CheckLst,
  DB,
  DBClient,
  //ZipMstr,
  UAbstractObject,
  UAbstractModelObjects,
  UDbTableProperty,
  UProgressDialog,
  UDbTablePropertyManager,
  UDbTableField,
  UDbTableFieldsList,
  UStudyObjects,
  UStudyCopyForm,
  UCautionarySplashScreenValidator;

type
  TDbTableDataManager = class(TAbstractAppObject)
  protected
    FSelectionLevel       : TModelActionLevel;
    FScriptList           : TStringList;
    FDocsList             : TStringList;
    FClassRPatchRList     : TStringList;
    FZipFileList          : TStringList;
    FStudyArea            : TAbstractStudyArea;
    FStudyFields          : TStudyFields;
    FTempDirectory        : string;
    FDirectory            : string;
    FZipFileName          : string;
    FChangeListIDs        : string;
    FZipFileNameList      : TStringList;
    FProgressDialog       : TProgressDialog;
    FStudyCopyForm        : TfrmStudyCopyForm;
    FIncludeRainfallBaseData,
    FExportAll,
    FImportAll,
    FIncludeOutputData    : boolean;
    FResult               : integer;
    FStudy                : string;
    FModel                : string;
    FSubArea              : string;
    FScenario             : string;
    FExportingSystemData  : boolean;
    FExportingChangeListData  : boolean;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    procedure ParseZipFileName;
    procedure OnItemCheckUncheck(Sender: TObject);
    function CreateZipFileList(AFileName : string) : string;
    function CreateDbTable (ATableProperty : TAbstractDbTableProperty; ATableFieldsDefList : TTableFieldsDefList;
                           AProgressUpdateFuntion : TProgressUpdateFuntion) : boolean;
    function TableDropped (ATableName : string) : boolean;
    function NewRowAlreadyExists (ANewDataSet     : TDataSet;
                                  AOldDataset     : TDataSet;
                                  AKeyFieldsNames : TStrings): boolean;
    function TableNameFromFileName(AFileName: string): string;
    function SelectFilesDirectory: boolean;
    function SelectZipFile : boolean;
    function CopySelectedFileToTempDir(AZipFileName : string) : boolean;
    function ExtractZipFile(AZipFileName : string) : string;
    function ActivateCopyStudyDataDialog: boolean;
    function UpdateProgress(AResult: boolean): boolean;
    function ValidateStudyData : boolean;

    function CreateStudyZip (AFileName   : string;
                             AScriptList : TStringList) : boolean;
    function CreateSubAreaZip : boolean;
    procedure DeleteDuplicatedFilesBeforeZipping(AList : TStrings);
    function UnZipFiles : boolean;
    function ImportDailyDiversionData(ATableProperty:TAbstractDbTableProperty;AClientDataSet : TClientDataSet):boolean;

    function ImportBlobFields(AFieldIndex    : string;
                               AAppendDataSet : TAbstractModelDataset;
                               AClientDataSet : TClientDataSet;
                               ATableProperty : TAbstractDbTableProperty) : boolean;
    procedure OnProgressEvent (Sender    : TObject;
                               Filename  : String;
                               FileSize  : Longint);
    procedure OnMessageEvent (Sender   : TObject;
                              ErrCode  : Integer;
                              AMessage : String);
    function LoadSystemData(AProgressUpdateFuntion : TProgressUpdateFuntion) : boolean;

    function GetTableWhereClause (ATableProperty           : TAbstractDbTableProperty;
                                  AWhereClauseColumnNames  : string;
                                  AWhereClauseColumnValues : string;
                                  ATableColumnNames        : string): string;
    function GetGeneralWhereClause(ATableProperty:TAbstractDbTableProperty): string;
    function GetScenarioCountPerStudy: integer;

    function ExecExportSystemData(AProgressUpdateFuntion : TProgressUpdateFuntion): boolean;
    //function ProcessExportHydrologyFileDataTable(ATableProperty:TAbstractDbTableProperty): boolean;
    function ExecBuildDataBase(AProgressUpdateFuntion : TProgressUpdateFuntion): boolean;

    function CopyFileToTempZipDirectory(AFileName : string): boolean;
    function CreateClassRAndPatchRListFile(AFileName : string) : boolean;
    function CopyDiagramsToDisk(AKeyField, AKeyValues : string; ADataSet : TDataset) : boolean;
    function CopyReportDocToDisk(AKeyField, AKeyValues : string; ADataSet : TDataset) : boolean;
    function GetDrawingGroupName(AStudyAreaCode, ASubAreaCode, AScenarioCode : string) : string;
    procedure ImportDiagramsAndDocuments;
    procedure ImportClassRPatchRFiles;
    function ProcessCopyFromGeneralTable (ATableProperty:TAbstractDbTableProperty): boolean;
    function ProcessCopySpecialTable(ATableProperty:TAbstractDbTableProperty): boolean;
    function ProcessCopyStudyDucumentTables(ATableProperty: TAbstractDbTableProperty): boolean;
    function ProcessCopySingleRecord (ADataSet       : TDataset;
                                      ATableProperty : TAbstractDbTableProperty;
                                      AKeyFields     : string;
                                      AKeyValues     : string): boolean;

    function ProcessDeleteFromGeneralTable(ATableProperty: TAbstractDbTableProperty): boolean;
    function ProcessDeleteSpecialTable (ATableProperty:TAbstractDbTableProperty): boolean;
    //function ProcessDeleteHydrologyFileDataTable(ATableProperty: TAbstractDbTableProperty): boolean;
    function ProcessDeleteStudyDucumentTables(ATableProperty: TAbstractDbTableProperty): boolean;

    function ExecImportYieldSubArea (AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;
    //function ExecImportHydrologySubArea (AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;
    function ExecImportRainfallSubArea(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;
    function ProcessImportSpecialTable (AXMLFileNames          : TStringList;
                                        ATableProperty         : TAbstractDbTableProperty;
                                        AProgressUpdateFuntion : TProgressUpdateFuntion): boolean;
    function PreProcessImportChangeListTables: boolean;
    function PreProcessImportMetaDataTables (AFileNameList : string;
                                             AFileNameItem : string) : boolean;
    function ProcessImportTableData (AXMLFilename           : string;
                                     ATableProperty         : TAbstractDbTableProperty;
                                     AProgressUpdateFuntion : TProgressUpdateFuntion): boolean;
    function ProcessImportOlderVersion(AXMLFilename           : string;
                                     ATableProperty         : TAbstractDbTableProperty;
                                     AProgressUpdateFuntion : TProgressUpdateFuntion): boolean;

    function ProcessImportSelectedChangeLists (AXMLFilename           : string;
                                     ATableProperty         : TAbstractDbTableProperty;
                                     AProgressUpdateFuntion : TProgressUpdateFuntion): boolean;


    function PreProcessImportRainfallTable (AFileNamesList         : TStrings;
                                            ATablePropertyList     : TObjectList;
                                            AProgressUpdateFuntion :TProgressUpdateFuntion): boolean;
    procedure ReplaceKeyValues (var AKeyValues : string);
    procedure ReplacePatchIDValues (var AKeyValues : string;
                                    APatchIDs      : TStringList);
    procedure ReplaceStationIDValues (var AKeyValues : string;
                                      AStationIDs    : TStringList);

    function ProcessWRYMDataTable (AXMLFileNames          : TStringList;
                                   ATableProperty         : TAbstractDbTableProperty;
                                   AProgressUpdateFuntion : TProgressUpdateFuntion): boolean;
    function ExecVersionUpgrade (AProgressUpdateFuntion : TProgressUpdateFuntion;
                                 AFileNames             : TStringList) : boolean;
{***********}
    function SubAreaFileName(ALevel : TModelActionLevel): string;
    function ExecExportConfirmHydrologyFiles(AProgressUpdateFuntion : TProgressUpdateFuntion) : boolean;
    function ExecExportStudy(AProgressUpdateFuntion : TProgressUpdateFuntion) : boolean;
    function ExecExportAll(AProgressUpdateFuntion : TProgressUpdateFuntion) : boolean;
    function ExecExportSubArea (AProgressUpdateFuntion : TProgressUpdateFuntion) : boolean;
    //function ExecExportHydrologySubArea (AProgressUpdateFuntion : TProgressUpdateFuntion) : boolean;
    function ExecExportRainfallSubArea (AProgressUpdateFuntion : TProgressUpdateFuntion) : boolean;
    function ExecExportYieldSubArea (AProgressUpdateFuntion : TProgressUpdateFuntion) : boolean;
    function ExecExportChangeLists (AProgressUpdateFuntion : TProgressUpdateFuntion) : boolean;

    function GetClassRAndPatchRFileNames ( ASubArea : string ) : string;
    function CreateScriptListFile : boolean;
    function CreateDocsListFile : boolean;
    procedure FindKeyValue (AKeyValues : string;
                            AKey       : string;
                            var AValue : string);
    function ProcessExportYieldChangeTables : boolean;
    function ProcessExportYieldMetaDataTables : boolean;
    function ProcessExportRainfallChangeTables : boolean;
    function ProcessExportRainfallMetaDataTables : boolean;
    function ProcessExportGeneralTable (ATableProperty : TAbstractDbTableProperty) : boolean;
    function ProcessExportSelectedChangeLists(ATableProperty : TAbstractDbTableProperty) : boolean;
    function ExportNetworkDiagrams : boolean;
    function Get_NetworkDiagramsPath : string;
    function ExportStudyDocuments : boolean;
    function ExportStudyAreaShapeFiles : boolean;
    function ExportStudySubAreaShapeFiles : boolean;

    function ExecImportStudy (AProgressUpdateFuntion : TProgressUpdateFuntion) : boolean;
    function ExecCopyStudy (AProgressUpdateFuntion : TProgressUpdateFuntion) : boolean;
    function ExecCopySubArea (AProgressUpdateFuntion : TProgressUpdateFuntion) : boolean;
    function ExecCopyYieldSubArea(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;
    function ExecCopyRainfallSubArea (AProgressUpdateFuntion : TProgressUpdateFuntion):boolean;
    //function ExecCopyHydrologySubArea(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;
    function ProcessCopyChangeTables (ATableProperty: TAbstractDbTableProperty): boolean;
    function ProcessCopyMetaDataTables (ATableProperty: TAbstractDbTableProperty): boolean;
    function CopyStudyTable : boolean;
    function CopySubAreaTable : boolean;
    function CopyScenarioTable : boolean;

    function ExecDeleteStudy (AProgressUpdateFuntion : TProgressUpdateFuntion) : boolean;
    function ExecDeleteSubArea (AProgressUpdateFuntion : TProgressUpdateFuntion) : boolean;
    function ExecDeleteYieldSubArea(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;
    function ExecDeleteRainfallSubArea (AProgressUpdateFuntion : TProgressUpdateFuntion):boolean;
    //function ExecDeleteHydrologySubArea (AProgressUpdateFuntion : TProgressUpdateFuntion):boolean;
    function DeleteRainfallScenario (AProgressUpdateFuntion : TProgressUpdateFuntion;
                                     AArea                  : string;
                                     ASubArea               : string;
                                     AScenario              : string) : boolean;
    function DeleteRainfallSubArea (AProgressUpdateFuntion : TProgressUpdateFuntion;
                                    AArea                  : string;
                                    ASubArea               : string) : boolean;
    function DeleteStudyTable : boolean;
    function DeleteSubAreaTable : boolean;
    function DeleteScenarioTable : boolean;
    function ProcessDeleteChangeTables(ATableProperty: TAbstractDbTableProperty): boolean;
    function ProcessDeleteMetaDataTables(ATableProperty: TAbstractDbTableProperty): boolean;
    function ExtractAllStudyData(AProgressUpdateFuntion : TProgressUpdateFuntion) : boolean;
    function ExecImportChangeLists (AProgressUpdateFuntion : TProgressUpdateFuntion) : boolean;
    function ShowIssueListSplashScreen : boolean;
  public
    function ExportSystemData: boolean;
    function BuildDataBase : boolean;
    function Initialise: Boolean; override;
{***********}
    function DeleteStudyData (ASelectionLevel       : TModelActionLevel;
                              AStudyArea            : TAbstractStudyArea):boolean;
    function ExportStudyData (ASelectionLevel       : TModelActionLevel;
                              AStudyArea            : TAbstractStudyArea) : boolean;
    function CopyStudyData (ASelectionLevel       : TModelActionLevel;
                            AStudyArea            : TAbstractStudyArea;
                            AStudyFields          : TStudyFields) : boolean;
    function ImportStudyData : boolean;
    function ExportAllStudyData : boolean;
    function ImportAllStudyData : boolean;
    function ExportChangeLists(AChangeListNumbersCommaText: string): boolean;
    function ImportChangeLists: boolean;
    //function CreateHydrologyNetwork(AStudyFields: TStudyFields): boolean;
    //function UpdateHydrologyNetwork(AStudyFields: TStudyFields): boolean;
    //function LinkHydrologyStudies(ASelectionLevel: TModelActionLevel;AStudyArea : TAbstractStudyArea): boolean;
    //function UnLinkHydrologyStudies(ASelectionLevel: TModelActionLevel;AStudyArea : TAbstractStudyArea): boolean;

  end;

implementation

uses
  Windows,
  System.UITypes,
  Vcl.Controls,
  Vcl.Dialogs,
  System.Zip,
  {$WARN UNIT_PLATFORM OFF}
    Vcl.FileCtrl,
  {$WARN UNIT_PLATFORM ON}
  Messages,
  Xml.Xmldom,
  Xml.XMLDoc,
  Xml.XMLIntf,
  Data.Win.ADODB,
  Datasnap.Provider,
  UDataSetType,
  UUtilities,
  UYieldModelDataGUIForm,
  UModuleVersionOperations,
  UErrorHandlingOperations,
  StrUtils,
  USelectZipFileDialog,
  //UBDEDatabaseLayer,
  UAbstractComponent,
  UStudyDatabaseAgent,
  UUpgrade_2_11,
  UUpgrade_2_16,
  UUpgrade_2_18,
  UUpgrade_2_19,
  UUpgrade_4_0,
  UDataEditComponent,
  //UImportHydrologyStudiesForm,
  UStringListOfStringLists,
  UDataComponent,
  UStudyMetaDataLoadAgent,
  UOleVariantEnum,
  UStudyMetaData;
  //UHydroDBManager;

const
  CMainScriptPrefix = 'MAIN_';
  CDocsScriptPrefix = 'DOCS_';
  CClassRPatchRScriptPrefix = 'CLASSRPATCHR_';

{******************************************************************************}
{* TDbTableDataManager                                                        *}
{******************************************************************************}

procedure TDbTableDataManager.CreateMemberObjects;
const OPNAME = 'TDbTableDataManager.CreateMemberObjects';
begin
  inherited;
  try
    FSelectionLevel          := malNone;
    FStudyArea               := nil;
    FStudyFields             := nil;
    FDirectory               := '';
    FTempDirectory           := '';
    FZipFileName             := '';
    FExportingSystemData     := False;
    FExportingChangeListData := False;
    FZipFileNameList         := TStringList.Create;
    FScriptList              := TStringList.Create;
    FDocsList                := TStringList.Create;
    FZipFileList             := TStringList.Create;
    FClassRPatchRList        := TStringList.Create;
    FProgressDialog          := TProgressDialog.Create(nil,FAppModules);
    FStudyCopyForm           := TfrmStudyCopyForm.Create(nil,FAppModules);
    FProgressDialog.clbOptions.OnClickCheck := OnItemCheckUncheck;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDbTableDataManager.DestroyMemberObjects;
const OPNAME = 'TDbTableDataManager.DestroyMemberObjects';
begin
  inherited;
  try
    FSelectionLevel       := malNone;
    FStudyArea            := nil;
    FStudyFields          := nil;
    FDirectory            := '';
    FTempDirectory        := '';
    FSubArea              := '';
    FProgressDialog.Free;
    FZipFileNameList.Free;
    FDocsList.Free;
    FScriptList.Free;
    FZipFileList.Free;
    FClassRPatchRList.Free;
    FStudyCopyForm.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.Initialise: Boolean;
const OPNAME = 'TDbTableDataManager.Initialise';
begin
  Result := False;
  try
    FScriptList.Clear;
    FDocsList.Clear;
    FClassRPatchRList.Clear;
    FZipFileNameList.Clear;
    FZipFileList.Clear;
    FZipFileName          := '';
    FSelectionLevel       := malNone;
    FStudyArea            := nil;
    FStudyFields          := nil;
    FDirectory            := '';
    FTempDirectory        := GetTempDir+'WRMF\';
    //DeleteDirectory(FTempDirectory);
    if SysUtils.DirectoryExists(FTempDirectory) then
      DeleteMultipleFiles(FTempDirectory+'*.*')
    else
      SysUtils.ForceDirectories(FTempDirectory);
    FProgressDialog.ProgressRichEdit.Clear;
    FIncludeRainfallBaseData := False;
    FIncludeOutputData       := False;

    FZipFileNameList.Sorted  := True;
    FResult := mrNo;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDbTableDataManager.OnItemCheckUncheck(Sender: TObject);
const OPNAME = 'TDbTableDataManager.OnItemCheckUncheck';
begin
  try
    FAppModules.GlobalData.SetStopOnFirstErr(FProgressDialog.clbOptions.Checked[0]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.TableNameFromFileName(AFileName: string): string;
const OPNAME = 'TDbTableDataManager.TableNameFromFileName';
var
  LExt: string;
begin
  Result := '';              
  try
    AFileName := Trim(AFileName);
    AFileName := ExtractFileName(AFileName);
    if(AFileName <> '') then
    begin
     LExt := ExtractFileExt(AFileName);
     if(LExt <> '') then
       Result := Copy(AFileName,4,Length(AFileName) - Length(LExt) - 3)
     else
       Result := Copy(AFileName,4,Length(AFileName));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.SelectFilesDirectory: boolean;
const OPNAME = 'TDbTableDataManager.SelectFilesDirectory';
begin
  Result := False;
  try
    FDirectory := Trim(FAppModules.ViewIni.ReadString(ClassName,'ExportDirectory',''));
    if (FDirectory <> '') then
    begin
       if SysUtils.DirectoryExists(FDirectory) then
        ChDir(FDirectory)
      else
        FDirectory := '';
    end;

    if SelectDirectory(FDirectory,[sdAllowCreate, sdPerformCreate, sdPrompt],0) then
    begin
      if(FDirectory[Length(FDirectory)] <> '\') then
         FDirectory := FDirectory + '\';
      if not SysUtils.DirectoryExists(FDirectory) then
         SysUtils.ForceDirectories(FDirectory);
      FAppModules.ViewIni.WriteString(ClassName,'ExportDirectory',FDirectory);
      Result := True;
      ChDir(FTempDirectory);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.ExecExportSystemData ( AProgressUpdateFuntion : TProgressUpdateFuntion ): boolean;
const OPNAME = 'TDbTableDataManager.ExecExportSystemData';
var
  LResult,
  LStop : boolean;
  LIndex : integer;
  LTable : TAbstractDbTableProperty;
  LModelTables: TObjectList;
begin
  Result := False;
  try
    LResult := False;
    LModelTables := TObjectList.Create(False);
    try
      if ( FAppModules.DBTablePropertyManager.GetTablesPerGroup ( tgSystem, LModelTables ) ) then
      begin
        FProgressDialog.ActionProgressBar.Min := 0;
        FProgressDialog.ActionProgressBar.Max := LModelTables.Count+2;
        for LIndex := 0 to LModelTables.Count -1 do
        begin
          LTable := TAbstractDbTableProperty(LModelTables.Items[LIndex]);
          AProgressUpdateFuntion( FAppModules.Language.GetString('Message.ProcessingTable')+ LTable.TableName,ptNone,LStop);
          if LStop then Exit;

          case LTable.TableGroup of
            tgSystem : LResult := ProcessExportGeneralTable(LTable);
          end;
          if not UpdateProgress(LResult) then Exit;
          Result := True;
        end;
         LResult := True;
        AProgressUpdateFuntion( FAppModules.Language.GetString('Message.CreatingScriptListFile'),ptNone,LStop);
        LResult := LResult and CreateScriptListFile;
        AProgressUpdateFuntion( FAppModules.Language.GetString('Message.CreatingDocumentsListFile'),ptNone,LStop);
        LResult := LResult and CreateDocsListFile;
        if not UpdateProgress(LResult) then Exit;
        AProgressUpdateFuntion( FAppModules.Language.GetString('Message.MovingFileToZipFile'),ptNone,LStop);
        LResult := LResult and CreateSubAreaZip;
        if not UpdateProgress(LResult) then Exit;
        Result := LResult;
      end;
    finally
      LModelTables.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.UpdateProgress(AResult: boolean): boolean;
const OPNAME = 'TDbTableDataManager.UpdateProgress';
begin
  Result := False;
  try
    FProgressDialog.UpdateProgressBar;
    if FProgressDialog.Stopped  or ((not AResult) and FAppModules.GlobalData.StopOnFirstErr)then
      Result := False
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDbTableDataManager.OnMessageEvent(Sender: TObject;
  ErrCode: Integer; AMessage: String);
const OPNAME = 'TDbTableDataManager.OnMessageEvent';
var
  LStop: boolean;
begin
  try
    FProgressDialog.ShowProgress(AMessage,ptNone,LStop);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDbTableDataManager.OnProgressEvent(Sender: TObject; Filename: String; FileSize: Integer);
const OPNAME = 'TDbTableDataManager.OnProgressEvent';
var
  LStop: boolean;
begin
  try
    FProgressDialog.ShowProgress(Filename,ptNone,LStop);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDbTableDataManager.DeleteDuplicatedFilesBeforeZipping(AList : TStrings);
const OPNAME = 'TDbTableDataManager.DeleteDuplicatedFilesBeforeZipping';
var
  LData : TStringList;
  LOriginalData : TStringList;
  LIndex : integer;
begin
  try
    if AList <> nil then
    begin
      if AList.Count > 1 then
      begin
        LData := TStringList.Create;
        LOriginalData := TStringList.Create;
        try
          for LIndex := 0 to AList.Count-1 do
          begin
            if LData.IndexOf(UpperCase(ExtractFileName(AList[LIndex])))<0 then
            begin
              LData.Add(UpperCase(ExtractFileName(AList[LIndex])));
              LOriginalData.Add(AList[LIndex]);
            end;
          end;
          AList.Clear;
          AList.CommaText := LOriginalData.CommaText;
        finally
          LData.Free;
          LOriginalData.Free;
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDbTableDataManager.CreateSubAreaZip: boolean;
const OPNAME = 'TDbTableDataManager.CreateSubAreaZip';
var
  LZipFile   : TZipFile;
  LFileName  : string;
  LIndex     : integer;
  LFileNames  :  TstringList;
begin
  Result := False;
  try
    LZipFile   := TZipFile.Create;
    LFileNames :=  TstringList.Create;
    try
      if (FSelectionLevel = malStudy) then
        LFileName := FTempDirectory + SubAreaFileName(malSubArea) + '.zip'
      else
        LFileName := FDirectory + SubAreaFileName(malSubArea) + '.zip';

      if FExportingChangeListData then
        LFileName := FDirectory + SubAreaFileName(malSubArea) + '_ChangeLists.zip'
      else
      if FExportingSystemData then
        LFileName := FDirectory + 'SystemData.zip'
      else if FExportAll then
            LFileName := FTempDirectory + SubAreaFileName(malSubArea) + '.zip';

      SysUtils.DeleteFile(LFileName);
      LZipFile.Open(LFileName,zmWrite);
      UUtilities.SearchFiles(FTempDirectory+'*.*',LFileNames);
      for LIndex := 0 to LFileNames.Count-1 do
      begin
        if(UpperCase(ExtractFileExt(LFileNames[LIndex])) <> '.ZIP') then
          LZipFile.Add(LFileNames[LIndex]);
      end;
      LZipFile.Close;
      for LIndex := 0 to LFileNames.Count-1 do
      begin
        if(UpperCase(ExtractFileExt(LFileNames[LIndex])) <> '.ZIP') then
          DeleteFile(PChar(LFileNames[LIndex]));
      end;
      Result := True;
    finally
      LZipFile.Free;
      LFileNames.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.CreateStudyZip ( AFileName : string; AScriptList : TStringList ) : boolean;
const OPNAME = 'TDbTableDataManager.CreateStudyZip';
var
  LZipFile   : TZipFile;
  LFileName  : string;
begin
  Result := False;
  try
    LZipFile := TZipFile.Create;
    try
      LFileName := FDirectory + ChopCharacters(AFileName)+'.zip';
      SysUtils.DeleteFile(LFileName);
      LZipFile.ZipDirectoryContents(LFileName,FTempDirectory);
      if FExportAll then
        FZipFileList.Add(LFileName);
    finally
      LZipFile.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.ProcessCopyFromGeneralTable(ATableProperty: TAbstractDbTableProperty): boolean;
const OPNAME = 'TDbTableDataManager.ProcessCopyFromGeneralTable';
var
  LDataSet: TAbstractModelDataset;
  LSQL: string;
  LKeyField,
  LKeyFieldValue: TStringList;
begin
  Result := False;
  try
    if Assigned(ATableProperty) then
    begin
      LKeyField := TStringList.Create;
      LKeyFieldValue := TStringList.Create;
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      try
        if Assigned(LDataSet) then
        begin
          LDataSet.ClearSQL;
          LSQL := 'SELECT '+
          Trim(ATableProperty.DelimitedFieldNamesCommatext) +
          //Trim(ATableProperty.FieldNames.CommaText) +
          ' FROM '+
          ATableProperty.TableName;
          LSQL := LSQL +  GetGeneralWhereClause(ATableProperty);
          LDataSet.SetSQL(LSQL);
          LDataset.DataSet.Open;

          case FSelectionLevel of
            malStudy:
            begin
              if(ATableProperty.FieldNames.IndexOf('StudyAreaName') >= 0) then
              begin
                LKeyField.Add('StudyAreaName');
                LKeyFieldValue.Add(FStudyFields.FStudyAreaName);
              end;
            end;
            malSubArea:
            begin
              if(ATableProperty.FieldNames.IndexOf('SubArea') >= 0) then
              begin
                LKeyField.Add('SubArea');
                LKeyFieldValue.Add(FStudyFields.FSubArea);
              end;
            end;
            malScenarion:
            begin
              if(ATableProperty.FieldNames.IndexOf('Scenario') >= 0) then
              begin
                LKeyField.Add('Scenario');
                LKeyFieldValue.Add(FStudyFields.FScenario);
              end;
            end;
          end;
          if (LKeyField.Count = 0) OR (NOT(Assigned (LDataSet.DataSet.FindField(LKeyField[0])))) then
          begin
            Result := True;
            Exit;
          end;
          if(LDataset.DataSet.RecordCount = 0) then
          begin
            Result := True;
            Exit;
          end
          else
          begin
            while (not LDataset.DataSet.Eof) do
            begin
              if not ProcessCopySingleRecord(LDataSet.DataSet, ATableProperty, LKeyField.CommaText, LKeyFieldValue.CommaText) then
                Exit;
              LDataSet.DataSet.Next;
            end;
            LDataSet.DataSet.Close;
          end;
          Result := True;
        end;
      finally
        LKeyField.Free;
        LKeyFieldValue.Free;
        LDataset.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.ProcessCopySingleRecord (ADataSet       : TDataset;
                                                      ATableProperty : TAbstractDbTableProperty;
                                                      AKeyFields     : string;
                                                      AKeyValues     : string) : boolean;
const OPNAME = 'TDbTableDataManager.ProcessCopySingleRecord';
var
  LBlobFields  : TStringList;
  LKeyFields   : TStringList;
  LKeyValues   : TStringList;
  LNullFields  : TStringList;
  LIndex       : integer;
  LKeyIndex    : integer;
  LWhere,
  LSQL         : string;
  LDataSet     : TAbstractModelDataset;
//  LToStream    : TStream;
//  LFromStream  : TStream;
  //LStreamList  : TObjectList;
  LField       : TField;
  LValue       : string;
  LFieldNames  : string;
  LFieldValues : string;
begin
  Result := False;
  try
    LKeyFields := TStringList.Create;
    LKeyValues := TStringList.Create;
    LBlobFields := TStringList.Create;
    LNullFields := TStringList.Create;
    try
      LKeyFields.CommaText := AKeyFields;
      LKeyValues.CommaText := AKeyValues;
      LKeyFields.CaseSensitive := False;

      if UpperCase ( ATableProperty.TableName ) = 'STUDYSCENARIOLOCK' then
      begin
        Result := True;
        Exit;
      end;

      LFieldNames  := ' (';
      LFieldValues := ' (';
      for LIndex := 0 to ATableProperty.FieldNames.Count-1 do
      begin
        if ADataSet.FieldByName(ATableProperty.FieldNames[LIndex]).IsNull then
          LNullFields.Add(ATableProperty.FieldNames[LIndex])
        else
        begin
          LFieldNames  := LFieldNames  + ATableProperty.FieldNames[LIndex] +',';
          LFieldValues := LFieldValues + ':' + ATableProperty.FieldNames[LIndex] +',';
        end;
      end;
      LFieldNames[Length(LFieldNames)] := ')';
      LFieldValues[Length(LFieldValues)] := ')';
      LSQL := 'INSERT INTO '+ ATableProperty.TableName +
              LFieldNames + ' VALUES'+ LFieldValues;

      {LSQL := 'INSERT INTO '+
      ATableProperty.TableName +
      ' ('+ ATableProperty.FieldNames.CommaText +')'+
      ' VALUES'+
      ATableProperty.FieldsParams;}


      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(LSQL);
        LDataSet.ClearQueryParams;
        for LIndex := 0 to ATableProperty.FieldNames.Count-1 do
        begin
          if(LNullFields.IndexOf(ATableProperty.FieldNames[LIndex]) >= 0) then
            Continue;
          LKeyIndex := LKeyFields.IndexOf(ATableProperty.FieldNames[LIndex]);
          if (LKeyIndex >= 0) then
            LDataSet.SetParamValue(ATableProperty.FieldNames[LIndex], LKeyValues.Strings[lKeyIndex],
            ADataSet.FieldByName(ATableProperty.FieldNames[LIndex]).DataType)
          else
          begin
            if (ADataSet.FieldByName(ATableProperty.FieldNames[LIndex]).IsNull) then
              LDataSet.SetParamValue
                (ATableProperty.FieldNames[LIndex],'NULL',
                 ADataSet.FieldByName(ATableProperty.FieldNames[LIndex]).DataType,False,0)
            else
            if ADataSet.FieldByName(ATableProperty.FieldNames[LIndex]).IsBlob then
            begin
               LBlobFields.Add(ATableProperty.FieldNames[LIndex]);
            end
            else
            begin
              LDataSet.SetParamValue
                (ATableProperty.FieldNames[LIndex],
                 ADataSet.FieldByName(ATableProperty.FieldNames[LIndex]).AsString,
                 ADataSet.FieldByName(ATableProperty.FieldNames[LIndex]).DataType);

              //Rename the drawing group to the scenario name
              if (UpperCase(ATableProperty.TableName) = 'VNVDRAWINGGROUP') and
                 (UpperCase(ATableProperty.FieldNames[LIndex]) = 'DRAWINGGROUPNAME') and
                 (FSelectionLevel = malScenarion) then
              begin
                LDataSet.SetParamValue
                (ATableProperty.FieldNames[LIndex],
                LKeyValues[LKeyValues.Count-1],
                ADataSet.FieldByName(ATableProperty.FieldNames[LIndex]).DataType);
              end;

              if(UpperCase(ATableProperty.TableName) = 'VNVDRAWING') and
                (UpperCase(ATableProperty.FieldNames[LIndex]) = 'DRAWINGNAME') and
                (not ADataSet.FieldByName(ATableProperty.FieldNames[LIndex]).IsNull) then
              begin
                CopyDiagramsToDisk(AKeyFields, AKeyValues, ADataSet);
              end;

              if (UpperCase(ATableProperty.TableName) = 'STUDYDOCUMENTS') and
                 (UpperCase(ATableProperty.FieldNames[LIndex]) = 'FILENAME') and
                 (not ADataSet.FieldByName(ATableProperty.FieldNames[LIndex]).IsNull) then
                CopyReportDocToDisk(AKeyFields, AKeyValues, ADataSet);
            end;
          end;
        end;

        if(LBlobFields.Count > 0) then
          LDataSet.ExecSQL
        else
          if LDataSet.AreAllParamsBound(True) then
          LDataSet.ExecSQL;

        if(LBlobFields.Count > 0) then
        begin
          LDataSet.DataSet.Close;
          //LStreamList := TObjectList.Create(True);
          try
            LSQL := 'SELECT * FROM '+ ATableProperty.TableName;

            for LIndex := 0 to ATableProperty.IndexFieldNames.Count-1 do
            begin
              LField := ADataSet.FieldByName(ATableProperty.IndexFieldNames[LIndex]);
              LKeyIndex := LKeyFields.IndexOf(LField.FieldName);
              if(LKeyIndex >= 0) then
                LValue := LKeyValues[LKeyIndex]
              else
                LValue := Trim(ADataSet.FieldByName(LField.FieldName).AsString);

              if(LField.DataType in [ftString, ftWideString, ftFixedWideChar]) then
                LWhere := LField.FieldName + ' = ' + QuotedStr(LValue)
              else
                LWhere := LField.FieldName + ' = ' + LValue;
                if (LIndex = 0) then
                  LSQL := LSQL + ' WHERE ' + LWhere
                else
                  LSQL := LSQL + ' AND ' + LWhere;
            end;

            LDataSet.SetSQL(LSQL);
            LDataSet.SetReadOnly(False);
            LDataSet.DataSet.Open;
            if not LDataset.DataSet.Eof then
            begin
              LDataSet.DataSet.Edit;
              for LIndex := 0 to LBlobFields.Count-1 do
              begin
                LDataset.Dataset.FieldByName(LBlobFields[LIndex]).Value := ADataSet.FieldByName(LBlobFields[LIndex]).Value;
                {LFromStream := ADataSet.CreateBlobStream(ADataSet.FieldByName(LBlobFields[LIndex]), bmRead);
                LToStream   := LDataset.Dataset.CreateBlobStream(lDataset.Dataset.FieldByName(LBlobFields[LIndex]), bmWrite);
                LToStream.CopyFrom(LFromStream,0);
                //LStreamList.Add(LToStream);
              }
              end;
              LDataSet.DataSet.Post;
            end;
          finally
            //LStreamList.Free;
          end;
        end;

        Result := True;
        LDataSet.DataSet.Close;;
        LDataSet.Free;
      end;
    finally
      FreeAndNil(LKeyFields);
      FreeAndNil(LKeyValues);
      FreeAndNil(LBlobFields);
      FreeAndNil(LNullFields);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.GetTableWhereClause (ATableProperty           : TAbstractDbTableProperty;
                                                  AWhereClauseColumnNames  : string;
                                                  AWhereClauseColumnValues : string;
                                                  ATableColumnNames        : string): string;
const OPNAME = 'TDbTableDataManager.GetTableWhereClause';
Var
  lClauseNames      : TStringList;
  lClauseValues     : TStringList;
  LTableColumnNames : TStringList;
  LIndex            : integer;
  LFirst            : boolean;
begin
  Result := '';
  try
    lClauseNames      := TStringList.Create;
    lClauseValues     := TStringList.Create;
    LTableColumnNames := TStringList.Create;
    try
      lClauseNames.CommaText      := AWhereClauseColumnNames;
      lClauseValues.CommaText     := AWhereClauseColumnValues;
      LTableColumnNames.CommaText := ATableColumnNames;

      LFirst := True;
      for LIndex := 0 to  lClauseNames.Count - 1 do
      begin
        if (ATableProperty.FieldNames.IndexOf(lClauseNames[LIndex]) >= 0 ) and
           (LTableColumnNames.IndexOf(lClauseNames[LIndex]) >= 0) then
        begin
          if LFirst then
          begin
            if (lClauseValues[LIndex] = 'IS NOT NULL') then
              Result := Result + ' WHERE ' + lClauseNames[LIndex] + ' ' + lClauseValues[LIndex]
            else
              Result := Result + ' WHERE ' + lClauseNames[LIndex] + ' = ' + lClauseValues[LIndex];
            LFirst := False;
          end
          else
          begin
            if (lClauseValues[LIndex] = 'IS NOT NULL') then
              Result := Result + ' AND ' + lClauseNames[LIndex] + ' ' + lClauseValues[LIndex]
            else
              Result := Result + ' AND ' + lClauseNames[LIndex] + ' = ' + lClauseValues[LIndex];
          end;
        end
      end;
    finally
      lClauseNames.Free;
      lClauseValues.Free;
      LTableColumnNames.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.GetGeneralWhereClause(ATableProperty:TAbstractDbTableProperty): string;
const OPNAME = 'TDbTableDataManager.GetGeneralWhereClause';
var
  lClauseValues : TStringList;
begin
  Result := '';
  try
    lClauseValues:= TStringList.Create;
    try
      if (FSelectionLevel = malScenarion) then
      begin
        lClauseValues.Add(QuotedStr(FModel));
        lClauseValues.Add(QuotedStr(FStudy));
        lClauseValues.Add(QuotedStr(FSubArea));
        lClauseValues.Add(QuotedStr(FStudyArea.ScenarioCode));
        lClauseValues.Add('0');
        Result := GetTableWhereClause(ATableProperty,
                                      'Model,StudyAreaName,SubArea,Scenario',
                                      lClauseValues.CommaText,
                                      ATableProperty.FieldNames.CommaText);
      end
      else
      begin
        lClauseValues.Add(QuotedStr(FModel));
        lClauseValues.Add(QuotedStr(FStudy));
        lClauseValues.Add(QuotedStr(FSubArea));
        lClauseValues.Add('IS NOT NULL');
        lClauseValues.Add('0');
        Result := GetTableWhereClause(ATableProperty,
                                      'Model,StudyAreaName,SubArea,Scenario',
                                       lClauseValues.CommaText,
                                       ATableProperty.FieldNames.CommaText);
      end;

      if (ATableProperty.TableFilter <> '') then
      begin
        if (Result <> '') then
          Result := Result + ' AND ' + ATableProperty.TableFilter
        else
          Result := ' WHERE ' + ATableProperty.TableFilter;
      end;

    finally
      lClauseValues.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDbTableDataManager.ImportClassRPatchRFiles;
const OPNAME = 'TDbTableDataManager.ImportClassRPatchRFiles';
var
  LFileList : TStringList;
  LFound    : boolean;
  LIndex    : integer;
  LTempFileName,
  LImportFile,
  LFileName : string;
begin
  try
    ChDir(FTempDirectory);
    LFileList := TStringList.Create;
    try
      LFound := False;
      if SearchFiles(FTempDirectory + CClassRPatchRScriptPrefix + '*', LFileList) then
      begin
        lIndex := 0;
        while ((not LFound) and (LIndex < LFileList.Count)) do
        begin
          LFileName := ExtractFileName(LFileList[LIndex]);
          if (UpperCase(Copy(LFileName, 1, 13)) = CClassRPatchRScriptPrefix) then
            LFound := True
          else
            LIndex := LIndex + 1;
        end;
        if (LFound) then
        begin
          LFileName := LFileList[LIndex];
          LFileList.LoadFromFile(LFileName);
          for LIndex := 0 to LFileList.Count - 1 do
          begin
            if Pos('WRCDATA\', LFileList[LIndex]) > 0 then
              LImportFile := GetAppDataLocalDir+LFileList[LIndex]   //ExtractFilePath(ApplicationExeName) + LFileList[LIndex]
            else
              LImportFile := LFileList[LIndex];
            LTempFileName := FTempDirectory + ExtractFileName(LFileList[LIndex]);
            if FileExists(LTempFileName) then
            begin
              if (not (SysUtils.DirectoryExists(ExtractFilePath(LImportFile)))) then
                SysUtils.ForceDirectories(ExtractFilePath(LImportFile));
              CopyFile(PChar(LTempFileName), PChar(LImportFile), FALSE);
              DeleteFile(PChar(LTempFileName));
            end;
          end;
        end;
      end;
    finally
      FreeAndNil(LFileList);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{function TDbTableDataManager.ProcessExportHydrologyFileDataTable(ATableProperty: TAbstractDbTableProperty): boolean;
const OPNAME = 'TDbTableDataManager.ProcessExportHydrologyFileDataTable';
var
  LDataSet: TAbstractModelDataset;
  LSQL,
  LSubSQL,
  LFileName,
  LWhereClause: string;
  LClientDataSet:TClientDataSet;
  LDataSetProvider:TDataSetProvider;
  LSubTableProperty: TAbstractDbTableProperty;
  lClauseValues : TStringList;
begin
  Result := false;
  try
    LSubTableProperty := FAppModules.DBTablePropertyManager.TablePropertyByName['FileNames'];
    if Assigned(ATableProperty) and Assigned(LSubTableProperty)then
    begin
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      lClauseValues := TStringList.Create;
      try
        if Assigned(LDataSet) then
        begin
          LSubSQL := 'SELECT FileName FROM '+
          LSubTableProperty.TableName;
          LWhereClause := GetGeneralWhereClause(LSubTableProperty);
          LSubSQL := LSubSQL +  LWhereClause +
          ' AND Identifier IS NOT NULL AND FileGroup = 5';

          LSQL := 'SELECT '+
          Trim(ATableProperty.FieldNames.CommaText) +
          ' FROM '+
          ATableProperty.TableName +
          ' WHERE FileName IN ( '+ LSubSQL + ' )'+
          ' AND StudyAreaName = '+QuotedStr(FStudy);
          LDataSet.SetSQL(LSQL);
          LDataset.DataSet.Open;
          if(LDataset.DataSet.Eof) and (LDataset.DataSet.Bof) then
          begin
            Result := True;
          end
          else
          begin
            LDataSetProvider := TDataSetProvider.Create(nil);
            LClientDataSet := TClientDataSet.Create(nil);
            try
              LDataSetProvider.DataSet := LDataset.DataSet;
              LClientDataSet.ReadOnly := True;
              LClientDataSet.SetProvider(LDataSetProvider);
              LClientDataSet.StoreDefs := True;
              LClientDataSet.Active := True;
              LFileName := Trim(FTempDirectory) + 'LD_' + Trim(ATableProperty.TableName) + '.xml';
              LClientDataSet.SaveToFile(LFileName,dfXML);
              FScriptList.Add(ExtractFileName(LFileName));
            finally
              LDataset.DataSet.Active := False;
              LClientDataSet.Active := False;
              LClientDataSet.Free;
              LDataSetProvider.Free;
            end;
          end;
          LDataset.DataSet.Close;
          Result := True;
        end;
      finally
        LDataset.Free;
        FreeAndNil(lClauseValues);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

function TDbTableDataManager.CopySelectedFileToTempDir(AZipFileName : string):boolean;
const OPNAME = 'TDbTableDataManager.CopySelectedFileToTempDir';
var
  lTempZipFile : string;
  lTempDir     : string;
  lZipFileName : string;
  lPath        : string;
begin
  Result := FALSE;
  try
    if (FZipFileNameList.Count > 0) then
    begin
      lTempDir := GetTempDir + 'WRMF';
      if not SysUtils.DirectoryExists(lTempDir) then
        SysUtils.ForceDirectories(lTempDir);
      FDirectory := IncludeTrailingPathDelimiter(lTempDir);
      lZipFileName := ExtractFileName(AZipFileName);

      if (Pos(CRainfall, lZipFileName) > 0 ) OR
         (Pos(CYield, lZipFileName) > 0 ) OR
         (Pos(CHydrology,lZipFileName) > 0 ) OR
         (Pos(CDailyDiversion, lZipFileName) > 0 ) OR
         (Pos(CIFRPreProcessor, lZipFileName) > 0 ) OR
         (Pos(CDamSedimentation, lZipFileName) > 0 ) OR
         (Pos(CStomsa, lZipFileName) > 0 ) OR
         (Pos(CPlanning, lZipFileName) > 0 ) OR
         (Pos(CRWH, lZipFileName) > 0 ) OR
         (Pos(CDDTS, lZipFileName) > 0 )then
      begin
        DeleteMultipleFiles(FTempDirectory + '*.*');
        lTempZipFile := FDirectory + lZipFileName;
        lPath := ExtractFilePath(AZipFileName);
        if (UpperCase(Copy(lPath, Length(lPath) - 4, 4)) = '.ZIP') then
        begin
          LZipFileName := ExtractZipFile(AZipFileName);
        end
        else
          LZipFileName := AZipFileName;
      end
      else
      begin
        LZipFileName := AZipFileName;
        lTempZipFile := FDirectory + ExtractFileName(lZipFileName);
      end;
      
      if FImportAll then
        FileCopy(AZipFileName,lTempZipFile)
      else
        CopyFileTo(lZipFileName, lTempZipFile);

      FZipFileName := lTempZipFile;

    end;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDbTableDataManager.UnZipFiles: boolean;
const OPNAME = 'TDbTableDataManager.UnZipFiles';
var
  LZipFile   : TZipFile;
begin
  Result := False;
  try
    if not FileExists(FZipFileName) then
      raise Exception.Create('Zip file does not exists ('+FZipFileName+')');

    LZipFile := TZipFile.Create;
    try
      LZipFile.ExtractZipFile(FZipFileName,FTempDirectory);
    finally
      LZipFile.Free;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.ExtractZipFile(AZipFileName : string) : string;
const OPNAME = 'TDbTableDataManager.ExtractZipFile';
var
  LZipFile     : TZipFile;
  LIndex       : integer;
  LZipFileName : string;
begin
  Result := '';
  try
    LZipFile := TZipFile.Create;
    try
      if not FImportAll then
        LZipFileName := ExtractFileDir(AZipFileName)
      else
        LZipFileName := AZipFileName;

      LZipFile.ExtractZipFile(LZipFileName,FTempDirectory);
      LZipFile.Open(LZipFileName,zmRead);
      try
        for LIndex := Low(LZipFile.FileNames) to High(LZipFile.FileNames) do
        begin
          if(UpperCase(ExtractFileName(AZipFileName)) =
             UpperCase(ExtractFileName(LZipFile.FileNames[LIndex]))) then
            Result := FTempDirectory + ExtractFileName(AZipFileName);
        end;
      finally
        LZipFile.Close;
      end;
    finally
      LZipFile.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


procedure TDbTableDataManager.ParseZipFileName;
const OPNAME = 'TDbTableDataManager.ParseZipFileName';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDbTableDataManager.ImportDiagramsAndDocuments;
const OPNAME = 'TDbTableDataManager.ImportDiagramsAndDocuments';
var
  lFileList     : TStringList;
  lFileStr      : string;
  lPath         : string;
  lFileDir      : string;
  lFound        : boolean;
  lIndex        : integer;
  lFileName     : string;
  lTempFileName : string;
  lMsgPrompt    : string;
begin
  try
    ChDir ( FTempDirectory );
    lFileList := TStringList.Create;
    try
      LFound := FALSE;
      if SearchFiles(FTempDirectory + CDocsScriptPrefix + '*', lFileList) then
      begin
        lIndex := 0;
        while ((NOT LFound) AND (lIndex < lFileList.Count)) do
        begin
          LFileName := ExtractFileName(lFileList[LIndex]);
          if (UpperCase(Copy(LFileName, 1, 5)) = CDocsScriptPrefix) then
            LFound := TRUE
          else
            LIndex := LIndex + 1;
        end;
        if (LFound) then
        begin
          lFileName := lFileList[lIndex];
          lFileList.LoadFromFile(lFileName);
          lPath := ApplicationExeName;
          lPath := ExtractFilePath(lPath);
          for lIndex := 0 to lFileList.Count - 1 do
          begin
            lFileStr := lFileList.Strings[lIndex];
            lFileDir := ExtractFilePath(lFileStr);
            lFileStr := ExtractFileName(lFileStr);
            lTempFileName := FTempDirectory + lFileStr;

            if not (FileExists(lTempFileName)) then //new version from 3.10
              lTempFileName := FTempDirectory + 'WRMF\Deployment\'+ lFileList.Strings[lIndex];


            if (FileExists(lTempFileName)) then
            begin
              lFileDir := lPath + lFileDir;
              if (NOT (SysUtils.DirectoryExists(lFileDir))) then
                SysUtils.ForceDirectories(lFileDir);
              lFileName := lFileDir + lFileStr;
              if (FileExists(lFileName)) then
              begin
                if ((FResult <> mrYesToAll) AND (FResult <> mrNoToAll)) then
                begin
                  lMsgPrompt := FAppModules.Language.GetString('Message.File') + lFileName +
                                FAppModules.Language.GetString('Message.LastChangedOn') +
                                DateTimeToStr(FileLastWriteDate(lFileName)) +
                                FAppModules.Language.GetString('Message.AlreadyExists') + #13#10 +
                                FAppModules.Language.GetString('Message.WantToReplaceWithFile') + lFileStr +
                                FAppModules.Language.GetString('Message.LastChangedOn') +
                                DateTimeToStr(FileLastWriteDate(lTempFileName)) + '?';
                  FResult := MessageDlg(lMsgPrompt, mtConfirmation, [mbYesToAll, mbYes, mbNo, mbNoToAll], 0);
                end;
                if ((FResult = mrYes) OR (FResult = mrYesToAll)) then
                begin
                  DeleteFile(PChar(lFileName));
                  CopyFile(PChar(lTempFileName), PChar(lFileName), FALSE);
                end;
              end
              else
              begin
                CopyFile(PChar(lTempFileName), PChar(lFileName), FALSE);
              end;
              DeleteFile(PChar(lTempFileName));
            end;
          end;
        end;
      end;
    finally
      FreeAndNil(lFileList);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.ProcessImportSpecialTable (AXMLFileNames          : TStringList;
                                                        ATableProperty         : TAbstractDbTableProperty;
                                                        AProgressUpdateFuntion : TProgressUpdateFuntion): boolean;
const OPNAME = 'TDbTableDataManager.ProcessImportSpecialTable';
var
  lFileName       : string;
  lFileName2      : string;
  lTableProperty2 : TAbstractDbTableProperty;
  LResult         : boolean;
  LStop           : boolean;
  lFound          : boolean;
begin
  Result := False;
  try
    LStop := FALSE;
    if (UpperCase(ATableProperty.TableName) = 'CHANGEPARAMETER') then
      Result := TRUE
    else
    if (UpperCase(ATableProperty.TableName) = 'CHANGELIST') then
    begin
      lFileName  := 'LD_ChangeList.xml';
      lFileName2 := 'LD_ChangeParameter.xml';
      lTableProperty2 := FAppModules.DBTablePropertyManager.TablePropertyByName['ChangeParameter'];
      if (AXMLFileNames.IndexOf(lFileName) < 0) then
        AProgressUpdateFuntion(lFileName + FAppModules.Language.GetString('Message.NotFound'), ptError, lStop)
      else
      if (AXMLFileNames.IndexOf(lFileName2) < 0) then
        AProgressUpdateFuntion(lFileName2 + FAppModules.Language.GetString('Message.NotFound'), ptError, lStop)
      else
      if (PreProcessImportChangeListTables) then
      begin
        AProgressUpdateFuntion( FAppModules.Language.GetString('Message.ProcessingTable') + ATableProperty.TableName, ptNone, LStop);
        if LStop then Exit;
        lResult := ProcessImportTableData(lFileName, ATableProperty, AProgressUpdateFuntion);
        if not UpdateProgress(LResult) then Exit;

        AProgressUpdateFuntion( FAppModules.Language.GetString('Message.ProcessingTable') + lTableProperty2.TableName, ptNone, LStop);
        if LStop then Exit;
        lResult := ProcessImportTableData(lFileName2, lTableProperty2, AProgressUpdateFuntion);
        if not UpdateProgress(LResult) then Exit;
        Result := TRUE;
      end
    end
    else
    if (UpperCase(ATableProperty.TableName) = 'METADATAITEM') then
      Result := TRUE
    else
    if (UpperCase(ATableProperty.TableName) = 'METADATALIST') then
    begin
      lTableProperty2 := FAppModules.DBTablePropertyManager.TablePropertyByName['MetaDataItem'];
      lFound := TRUE;
      lFileName := 'LD_MetaDataList.xml';
      if (AXMLFileNames.IndexOf('LD_MetaDataList.xml') < 0) then
      begin
        if (AXMLFileNames.IndexOf('LD_MetaDataGroup.xml') >= 0) then
          lFileName := 'LD_MetaDataGroup.xml'
        else
        begin
          AProgressUpdateFuntion(lFileName + FAppModules.Language.GetString('Message.NotFound'), ptError, lStop);
          lFound := FALSE;
        end;
      end;
      lFileName2 := 'LD_MetaDataItem.xml';
      if (AXMLFileNames.IndexOf('LD_MetaDataItem.xml') < 0) then
      begin
        if (AXMLFileNames.IndexOf('LD_MetaData.xml') >= 0) then
          lFileName2 := 'LD_MetaData.xml'
        else
        begin
          AProgressUpdateFuntion(lFileName + FAppModules.Language.GetString('Message.NotFound'), ptError, lStop);
          lFound := FALSE;
        end;
      end;
      if (lFound) AND (PreProcessImportMetaDataTables(lFileName, lFileName2)) then
      begin
        AProgressUpdateFuntion(FAppModules.Language.GetString('Message.ProcessingTable') + ATableProperty.TableName, ptNone, LStop);
        if LStop then Exit;
        lResult := ProcessImportTableData(lFileName, ATableProperty, AProgressUpdateFuntion);
        if not UpdateProgress(LResult) then Exit;

        AProgressUpdateFuntion(FAppModules.Language.GetString('Message.ProcessingTable') + lTableProperty2.TableName, ptNone, LStop);
        if LStop then Exit;
        lResult := ProcessImportTableData(lFileName2, lTableProperty2, AProgressUpdateFuntion);
        if not UpdateProgress(LResult) then Exit;
        Result := TRUE;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.PreProcessImportChangeListTables : boolean;
const OPNAME = 'TDbTableDataManager.PreProcessImportChangeListTables';
var
  lDBDataSet         : TAbstractModelDataset;
  lSQL               : string;
  lXMLDatasetMaster  : TClientDataSet;
  lXMLDatasetDetail  : TClientDataSet;
  lFileNameMaster    : string;
  lFileNameDetail    : string;
  lOldIDNumber       : integer;
  lNewIDNumber       : integer;
  lChangeListIDs     : TStringList;
  lIndex             : integer;
begin
  Result := False;
  try
    lXMLDatasetMaster  := TClientDataSet.Create(nil);
    lXMLDatasetDetail  := TClientDataSet.Create(nil);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDBDataset);
    lChangeListIDs := TStringList.Create;
    try
      if Assigned(lDBDataset) then
      begin
        lDBDataSet.DataSet.Close;
        lSQL := 'SELECT Max(ChangeListID) AS NewID FROM ChangeList';
        lDBDataSet.SetSQL(lSQL);
        lDBDataSet.Dataset.Open;
        lDBDataSet.Dataset.First;
        if (NOT lDBDataSet.DataSet.EOF) then
          lNewIDNumber := lDBDataSet.Dataset.FieldByName('NewID').AsInteger
        else
          lNewIDNumber := 0;

        lFileNameMaster            := 'LD_ChangeList.xml';
        lXMLDatasetMaster.FileName := lFileNameMaster;
        lXMLDatasetMaster.Active   := TRUE;
        lXMLDatasetMaster.First;
        while (NOT lXMLDatasetMaster.Eof) do
        begin
          lXMLDatasetMaster.Edit;
          lOldIDNumber := lXMLDatasetMaster.FieldByName('ChangeListID').AsInteger;
          lNewIDNumber := lNewIDNumber + 1;
          if (lOldIDNumber <> lNewIDNumber) then
          begin
            lChangeListIDs.AddObject(IntToStr(lOldIDNumber), TObject(lNewIDNumber));
            // Update XML datasets with new value for ChangeListID
            lXMLDatasetMaster.FieldByName('ChangeListID').AsInteger := lNewIDNumber;
          end;
          lXMLDatasetMaster.Post;
          lXMLDatasetMaster.Next;
        end;
        lXMLDatasetMaster.Active := FALSE;

        lFileNameDetail            := 'LD_ChangeParameter.xml';
        lXMLDatasetDetail.FileName := lFileNameDetail;
        lXMLDatasetDetail.Active   := TRUE;
        lXMLDatasetDetail.First;
        while (NOT lXMLDatasetDetail.EOF) do
        begin
          lOldIDNumber := lXMLDatasetDetail.FieldByName('ChangeListID').AsInteger;
          lIndex       := lChangeListIDs.IndexOf(IntToStr(lOldIDNumber));
          if (lIndex >= 0) then
          begin
            lNewIDNumber := Integer(lChangeListIDs.Objects[lIndex]);
            lXMLDatasetDetail.Edit;
            lXMLDatasetDetail.FieldByName('ChangeListID').AsInteger := lNewIDNumber;
            lXMLDatasetDetail.Post;
          end;
          lXMLDatasetDetail.Next;
        end;
        lXMLDatasetDetail.Active := FALSE;

        Result := True;
      end;
    finally
      lDBDataSet.Free;
      lXMLDatasetMaster.Free;
      lXMLDatasetDetail.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.PreProcessImportMetaDataTables (AFileNameList : string;
                                                             AFileNameItem : string) : boolean;
const OPNAME = 'TDbTableDataManager.PreProcessImportMetaDataTables';
var
  lDBDataSet         : TAbstractModelDataset;
  lSQL               : string;
  lXMLDatasetMaster  : TClientDataSet;
  lXMLDatasetDetail  : TClientDataSet;
  lListKey           : string;
  lOldIDNumber       : integer;
  lNewIDNumber       : integer;
begin
  Result := False;
  try
    lXMLDatasetMaster  := TClientDataSet.Create(nil);
    lXMLDatasetDetail  := TClientDataSet.Create(nil);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDBDataset);
    try
      if Assigned(lDBDataset) then
      begin
        lDBDataSet.DataSet.Close;
        lSQL := 'SELECT Max(MetaDataListID) AS NewID FROM MetaDataList';
        lDBDataSet.SetSQL(lSQL);
        lDBDataSet.Dataset.Open;
        lDBDataSet.Dataset.First;
        if (NOT lDBDataSet.DataSet.EOF) then
          lNewIDNumber := lDBDataSet.Dataset.FieldByName('NewID').AsInteger
        else
          lNewIDNumber := 0;

        lXMLDatasetMaster.FileName := AFileNameList;
        lXMLDatasetMaster.Active   := TRUE;

        lXMLDatasetDetail.FileName := AFileNameItem;
        lXMLDatasetDetail.Active   := TRUE;

        lXMLDatasetMaster.First;
        while (NOT lXMLDatasetMaster.Eof) do
        begin
          lOldIDNumber := lXMLDatasetMaster.FieldByName('MetaDataListID').AsInteger;
          lListKey     := Trim(lXMLDatasetMaster.FieldByName('MetaDataListKey').AsString);
          lSQL         := 'SELECT * FROM MetaDataList WHERE MetaDataListKey = ' + QuotedStr(lListKey);
          lDBDataSet.DataSet.Close;
          lDBDataset.SetSQL(lSQL);
          lDBDataset.DataSet.Open;
          if (NOT lDBDataSet.DataSet.Eof) then
            lNewIDNumber := lDBDataSet.DataSet.FieldByName('MetaDataListID').AsInteger
          else
            lNewIDNumber := lNewIDNumber + 1;
          if (lOldIDNumber <> lNewIDNumber) then
          begin
            // Update XML datasets with new value for MetaDataListID
            lXMLDatasetMaster.Edit;
            lXMLDatasetMaster.FieldByName('MetaDataListID').AsInteger := lNewIDNumber;
            lXMLDatasetMaster.Post;

            lXMLDatasetDetail.First;
            while (NOT lXMLDatasetDetail.EOF) do
            begin
              if (lXMLDatasetDetail.FieldByName('MetaDataListID').AsInteger = lOldIDNumber) then
              begin
                lXMLDatasetDetail.Edit;
                lXMLDatasetDetail.FieldByName('MetaDataListID').AsInteger := lNewIDNumber;
                lXMLDatasetDetail.Post;
              end;
              lXMLDatasetDetail.Next;
            end;
          end;
          lXMLDatasetMaster.Next;
        end;
        lXMLDatasetMaster.Active := FALSE;
        lXMLDatasetDetail.Active := FALSE;
        Result := True;
      end;
    finally
      lDBDataSet.Free;
      lXMLDatasetMaster.Free;
      lXMLDatasetDetail.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.NewRowAlreadyExists (ANewDataSet     : TDataSet;
                                                  AOldDataset     : TDataSet;
                                                  AKeyFieldsNames : TStrings): boolean;
const OPNAME = 'TDbTableDataManager.NewRowAlreadyExists';
var
  LKeys,LValues: string;
  LIndex: integer;
begin
  Result := False;
  try
    if Assigned(ANewDataSet) and Assigned(AOldDataset) and
       Assigned(AKeyFieldsNames) and (AKeyFieldsNames.Count > 0) then
    begin
      LKeys   := StringReplace(AKeyFieldsNames.CommaText,',',';',[rfReplaceAll]);
      LValues := '';
      for LIndex := 0 to AKeyFieldsNames.Count -1 do
        LValues := LValues + Trim(ANewDataSet.FieldByName(AKeyFieldsNames[LIndex]).AsString) + ';';
      LValues := Copy(LValues,1,Length(LValues)-1);
      Result  :=  AOldDataset.Locate(LKeys,LValues,[loCaseInsensitive]);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.ProcessImportTableData (AXMLFilename           : string;
                                                     ATableProperty         : TAbstractDbTableProperty;
                                                     AProgressUpdateFuntion : TProgressUpdateFuntion): boolean;
const OPNAME = 'TDbTableDataManager.ProcessImportTableData';
var
  LStop               : boolean;
  LDeleteDataSet      : TAbstractModelDataset;
  LAppendDataSet      : TAbstractModelDataset;
  LClientDataSet      : TAbstractModelDataset;//TClientDataSet;
  LLinesCount         : integer;
  LIndex              : integer;
  LParamName          : String;
  LParamValue         : String;
  LSQLDelete          : String;
  LFieldName          : String;
  LTableHasBlobFields : boolean;
  lBlobIdx            : String;
  LModel              : String;
begin
  Result := False;
  try
    if(ExtractFilePath(AXMLFilename) = '')then
      AXMLFilename := FTempDirectory + AXMLFilename;

    if not FileExists(AXMLFilename) then
    begin
     AProgressUpdateFuntion(FAppModules.Language.GetString('Message.FileXML') + AXMLFilename +
      FAppModules.Language.GetString('Message.FileDoesNotExist'), ptError, LStop);
     if LStop then Exit;
    end
    else
    begin
      if not ProcessImportOlderVersion(AXMLFilename,ATableProperty,AProgressUpdateFuntion) then
      begin
        AProgressUpdateFuntion( FAppModules.Language.GetString('Message.ProcessingFile') + ExtractFileName(AXMLFilename),ptNone,LStop);
        if LStop then Exit;

        if (ATableProperty.TableGroup = tgOutputData) and (not FIncludeOutputData) then
        begin
          Result := True;
          Exit;
        end;
        if (UpperCase(ATableProperty.TableName) = 'STUDYSCENARIOLOCK') or
           (UpperCase(ATableProperty.TableName) = 'TSCCHART') or
           (UpperCase(ATableProperty.TableName) = 'TSCCHARTSERIES') or
           (UpperCase(ATableProperty.TableName) = 'TSCSERIES') or
           (UpperCase(ATableProperty.TableName) = 'TSCVIEW') or
           (UpperCase(ATableProperty.TableName) = 'TSCVIEWCHART') or
           (UpperCase(ATableProperty.TableName) = 'TSCVIEWSCENARIO') then
        begin
          Result := True;
          Exit;
        end;
        LSQLDelete := 'DELETE FROM '+ ATableProperty.TableName + ' WHERE';
        for LIndex := 0 to ATableProperty.IndexFieldNames.Count -1 do
        begin
          LSQLDelete := LSQLDelete + ' ' + ATableProperty.IndexFieldNames[LIndex];
          LSQLDelete := LSQLDelete + ' = :A'+ATableProperty.IndexFieldNames[LIndex];
          if(LIndex <> (ATableProperty.IndexFieldNames.Count -1)) then
            LSQLDelete := LSQLDelete + ' AND'
        end;

        //LClientDataSet := TClientDataSet.Create(nil);
        FAppModules.Database.CreateDataset(integer(dtExecSQL), LClientDataSet);
        FAppModules.Database.CreateDataset(integer(dtExecSQL), LDeleteDataSet);
        FAppModules.Database.CreateDataset(integer(dtExecSQL), LAppendDataSet);
        try
          if Assigned(LDeleteDataSet) and Assigned(LAppendDataSet) then
          begin

            LClientDataSet.DataSet.LoadFromFile(AXMLFilename);
            LClientDataSet.DataSet.Active := True;
              //if (UpperCase(ATableProperty.TableName) = 'DAILYINSTREAMFILEDATA') or
            //     (UpperCase(ATableProperty.TableName) = 'DAILYDIVERSIONFILEDATA') then
             //   ImportDailyDiversionData(ATableProperty,LClientDataSet)
            //  else
            //  begin
                LAppendDataSet.SetSQL('SELECT * FROM '+ ATableProperty.TableName);
                LAppendDataSet.SetReadOnly(False);
                LAppendDataSet.DataSet.Active := True;

                LLinesCount := 0;

                while not LClientDataSet.DataSet.Eof do
                begin
                  LDeleteDataSet.DataSet.Close;
                  LDeleteDataSet.SetSQL(LSQLDelete);
                  for LIndex := 0 to ATableProperty.IndexFieldNames.Count -1 do
                  begin
                    LFieldName := ATableProperty.IndexFieldNames[LIndex];
                    LParamName  := 'A'+LFieldName;
                    if LClientDataSet.DataSet.FieldByName(LFieldName).IsNull then
                      LParamValue := 'NULL'
                    else
                      LParamValue := LClientDataSet.DataSet.FieldByName(LFieldName).AsString;
                    LDeleteDataSet.SetParamValue(LParamName,LParamValue,LClientDataSet.DataSet.FieldByName(LFieldName).DataType);
                  end;

                  if LDeleteDataSet.AreAllParamsBound(True) then
                  begin
                    try
                      LDeleteDataSet.ExecSQL;
                    except on E: Exception do
                      begin
                        AProgressUpdateFuntion( E.Message,ptError,LStop);
                        if LStop then Break;
                      end;
                    end;
                  end;

                  lBlobIdx := '';
                  LAppendDataSet.DataSet.Append;
                  LTableHasBlobFields := FALSE;
                  for LIndex := 0 to LClientDataSet.DataSet.FieldCount -1 do
                  begin
                    LFieldName := LClientDataSet.DataSet.FieldDefs.Items[LIndex].Name;
                   { if  (LClientDataSet.DataSet.FieldByName(LFieldName).IsBlob) then
                    begin
                      LTableHasBlobFields := True;
                      if (lBlobIdx = '') then
                        lBlobIdx := IntToStr(LIndex)
                      else
                        lBlobIdx := lBlobIdx + ',' + IntToStr(LIndex);
                    end
                    else
                    begin }
                      LAppendDataSet.DataSet.FieldByName(LFieldName).Value := LClientDataSet.DataSet.FieldByName(LFieldName).Value;
                    //end;
                  end;
                  try
                    // make all imported senarios read only.
                    if(ATableProperty.TableName = 'StudyScenario') then
                    begin
                      LModel := Trim(LAppendDataSet.DataSet.FieldByName('Model').AsString);
                      if ((LModel = CYield) or (LModel = CPlanning))then
                        LAppendDataSet.DataSet.FieldByName('DataImported').Value := 'Y';
                    end;
                    LAppendDataSet.DataSet.Post;
                    if (LTableHasBlobFields) then
                      //ImportBlobFields(lBlobIdx, LAppendDataSet, LClientDataSet, ATableProperty);
                  except on E: Exception do
                    begin
                        AProgressUpdateFuntion( E.Message,ptError,LStop);
                      if LStop then Break;
                    end;
                  end;

                  LLinesCount := LLinesCount + 1;
                  if((LLinesCount mod 50) = 0) then
                  begin
                     AProgressUpdateFuntion('',ptNone,LStop);
                     if LStop then Break;
                  end;

                  LClientDataSet.DataSet.Next;
                end;
                LAppendDataSet.DataSet.Close;
              //end;
              Result := True;
            end;

        finally
          //LClientDataSet.Free;
          LAppendDataSet.Free;
          LDeleteDataSet.Free;
          LClientDataSet := nil;

        end;
      end;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;




function TDbTableDataManager.ImportDailyDiversionData(ATableProperty:TAbstractDbTableProperty;AClientDataSet : TClientDataSet):boolean;
const OPNAME = 'TDbTableDataManager.ImportDailyDiversionData';
var
  LDataSet : TAbstractModelDataset;
  LSQL : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LSQL  := ' Drop table DiversionTemp';
        try
          LDataSet.ClearSQL;
          LDataSet.SetSQL(LSQL);
          LDataSet.ClearQueryParams();
          LDataset.ExecSQL;
        except on E: Exception do
        end;
        try
          if (UpperCase(ATableProperty.TableName) = 'DAILYINSTREAMFILEDATA')  then
            LSQL  := ' Create table DiversionTemp( Model char(50),StudyAreaName char(50),SubArea char(50),'+
                 ' Scenario char(50),Identifier integer,StationID integer,InstreamDate date,'+
                 ' AvgFlow float,QualityCode integer);'
          else
          if (UpperCase(ATableProperty.TableName) = 'DAILYDIVERSIONFILEDATA') then
            LSQL  := ' Create table DiversionTemp( Model char(50),StudyAreaName char(50),SubArea char(50),'+
                 ' Scenario char(50),Identifier integer,StationID integer,DiversionDate date,'+
                 ' AvgFlow float,QualityCode integer);';

          LDataSet.ClearSQL;
          LDataSet.SetSQL(LSQL);
          LDataSet.ClearQueryParams();
          LDataset.ExecSQL;
        except on E: Exception do
        end;

        while not AClientDataSet.Eof do
        begin

          LSQL := 'INSERT INTO DiversionTemp'+
          //' ('+ ATableProperty.FieldNames.CommaText +')'+
          ' ('+ ATableProperty.DelimitedFieldNamesCommatext +')'+
          ' VALUES'+
          ATableProperty.FieldsParams;
          try
            LDataSet.ClearSQL;
            LDataSet.SetSQL(LSQL);
            LDataSet.ClearQueryParams();
            LDataSet.SetParams(
            ['Model','StudyAreaName','SubArea','Scenario','Identifier','StationID'],
            [Trim(AClientDataSet.FieldByName('Model').AsString),
            Trim(AClientDataSet.FieldByName('StudyAreaName').AsString),
            Trim(AClientDataSet.FieldByName('SubArea').AsString),
            Trim(AClientDataSet.FieldByName('Scenario').AsString),
            Trim(AClientDataSet.FieldByName('Identifier').AsString),
            Trim(AClientDataSet.FieldByName('StationID').AsString)]);
            if (UpperCase(ATableProperty.TableName) = 'DAILYINSTREAMFILEDATA')then
              LDataSet.SetParams(['InstreamDate'],[AClientDataSet.FieldByName('InstreamDate').Value])
            else
              LDataSet.SetParams(['DiversionDate'],[AClientDataSet.FieldByName('DiversionDate').Value]);

            if not AClientDataSet.FieldByName('AvgFlow').IsNull then
              LDataSet.SetParams(['AvgFlow'],[AClientDataSet.FieldByName('AvgFlow').Value]);

            if not AClientDataSet.FieldByName('QualityCode').IsNull then
              LDataSet.SetParams(['QualityCode'],[AClientDataSet.FieldByName('QualityCode').Value]);

            LDataset.ExecSQL;
          except on E: Exception do
          end;

          AClientDataSet.Next;
        end;
      end;
      if (UpperCase(ATableProperty.TableName) = 'DAILYINSTREAMFILEDATA')  then
        LSQL  := ' insert into DailyInstreamFileData Select Model,StudyAreaName,SubArea,Scenario,'+
                 ' Identifier,StationID,InstreamDate,'+
                 ' AvgFlow,QualityCode'+
                 ' from DiversionTemp '
      else
      if (UpperCase(ATableProperty.TableName) = 'DAILYDIVERSIONFILEDATA') then
        LSQL  := ' insert into DailyDiversionFileData Select Model,StudyAreaName,SubArea,Scenario,'+
                 ' Identifier,StationID,DiversionDate,'+
                 ' AvgFlow,QualityCode'+
                 ' from DiversionTemp ';

      LDataSet.ClearSQL;
      LDataSet.SetSQL(LSQL);
      LDataSet.ClearQueryParams();
      LDataset.ExecSQL;

      LSQL  := ' Drop table DiversionTemp';
      try
        LDataSet.ClearSQL;
        LDataSet.SetSQL(LSQL);
        LDataSet.ClearQueryParams();
        LDataset.ExecSQL;
      except on E: Exception do
      end;

    finally
      FreeAndNil(LDataSet);
    end;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDbTableDataManager.ReplaceStationIDValues (var AKeyValues : string;
                                                      AStationIDs    : TStringList);
const OPNAME = 'TDbTableDataManager.ReplaceStationIDValues';
var
  lPos     : integer;
  lStart   : integer;
  lEnd     : integer;
  lOldID   : string;
  lNewID   : string;
  lIndex   : integer;
begin
  try
    lPos := Pos('StationID=', AKeyValues);
    if (lPos > 0) then
    begin
      lStart := PosEx('=', AKeyValues, lPos);
      lEnd   := PosEx(',', AKeyValues, lPos);
      if (lEnd > 0) then
        lOldID := Copy(AKeyValues, lStart+1, lEnd - (lStart+1))
      else
        lOldID := Copy(AKeyValues, lStart+1, Length(AKeyValues) - lStart);
      lIndex := AStationIDs.IndexOf(lOldID);
      if (lIndex >= 0) then
      begin
        lNewID := IntToStr(Integer(AStationIDs.Objects[lIndex]));
        if (lNewID = '0') then
          lNewID := lOldID;
      end
      else
        lNewID := lOldID;
      if (lEnd > 0) then
        AKeyValues := Copy(AKeyValues, 1, lStart) + lNewID +
                      Copy(AKeyValues, lEnd, Length(AKeyValues) - lEnd + 1)
      else
        AKeyValues := Copy(AKeyValues, 1, lStart) + lNewID;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.ProcessDeleteFromGeneralTable(ATableProperty: TAbstractDbTableProperty): boolean;
const OPNAME = 'TDbTableDataManager.ProcessDeleteFromGeneralTable';
var
  LSQL: string;
begin
  Result := false;
  try
    if Assigned(ATableProperty) then
    begin
      case FSelectionLevel of
        malStudy:
        begin
          if (UpperCase(ATableProperty.TableName) = 'STUDYMODEL') then
          begin
            Result := True;
            Exit;
          end
        end;
        malSubArea:
        begin
          if (UpperCase(ATableProperty.TableName) = 'STUDYMODEL') or
             (UpperCase(ATableProperty.TableName) = 'STUDYAREA') then
          begin
            Result := True;
            Exit;
          end
        end;
        malScenarion:
        begin
          if (UpperCase(ATableProperty.TableName) = 'STUDYMODEL') or
             (UpperCase(ATableProperty.TableName) = 'STUDYAREA') or
             (UpperCase(ATableProperty.TableName) = 'STUDYSUBAREA') or
             (UpperCase(ATableProperty.TableName) = 'TSCCHART') or
             (UpperCase(ATableProperty.TableName) = 'TSCCHARTSERIES') or
             (UpperCase(ATableProperty.TableName) = 'TSCVIEW') or
             (UpperCase(ATableProperty.TableName) = 'TSCVIEWCHART') then
          begin
            Result := True;
            Exit;
          end
        end;
      end;
      LSQL := 'DELETE' +
      ' FROM '+
      ATableProperty.TableName;
      LSQL := LSQL +  GetGeneralWhereClause(ATableProperty);
      FAppModules.Database.ExecSQL(LSQL);
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{function TDbTableDataManager.ProcessDeleteHydrologyFileDataTable(ATableProperty: TAbstractDbTableProperty): boolean;
const OPNAME = 'TDbTableDataManager.ProcessDeleteHydrologyFileDataTable';
var
  LSubTableProperty: TAbstractDbTableProperty;
  LDataSet: TAbstractModelDataset;
  LSQL,
  LSubSQL: string;
  LStudyAreaNameList,
  LFileContents : TStringList;
  LIndex: integer;
begin
  Result := False;
  try
    LSubTableProperty := FAppModules.DBTablePropertyManager.TablePropertyByName['FileNames'];
    if Assigned(ATableProperty) and Assigned(LSubTableProperty)then
    begin
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      LFileContents := TStringList.Create;
      LStudyAreaNameList := TStringList.Create;
      try
        if Assigned(LDataSet) then
        begin
          LSubSQL := 'SELECT FileName,StudyAreaName FROM '+
          LSubTableProperty.TableName;
          LSubSQL := LSubSQL +  GetGeneralWhereClause(LSubTableProperty)+
          ' AND Identifier IS NOT NULL AND FileGroup = 5';

          LDataset.DataSet.Close;
          LDataSet.SetSQL(LSubSQL);
          LDataset.DataSet.Open;
          while not LDataset.DataSet.Eof do
          begin
            LStudyAreaNameList.Add(Trim(LDataset.DataSet.FieldByName('StudyAreaName').AsString));
            LFileContents.Add(Trim(LDataset.DataSet.FieldByName('FileName').AsString));
            LDataset.DataSet.Next;
          end;

          LIndex := 0;
          while ((LIndex < LFileContents.Count) and (LFileContents.Count > 0)) and
                ((LIndex < LStudyAreaNameList.Count) and (LStudyAreaNameList.Count > 0))do
          begin
            LSubSQL := 'SELECT COUNT(*) AS FilesCount FROM '+
            LSubTableProperty.TableName +
             ' WHERE FileName = '+QuotedStr(LFileContents[LIndex])+
             ' AND StudyAreaName = '+QuotedStr(LStudyAreaNameList[LIndex]);
            LDataset.DataSet.Close;
            LDataSet.SetSQL(LSubSQL);
            LDataset.DataSet.Open;
            if (LDataset.DataSet.FieldByName('FilesCount').AsInteger > 1) then
            begin
              LFileContents.Delete(LIndex);
              LStudyAreaNameList.Delete(LIndex);
            end
            else
              LIndex := LIndex + 1;
          end;

          for LIndex := 0 to LFileContents.Count -1 do
          begin
            LSQL := 'DELETE FROM '+
            ATableProperty.TableName+
             ' WHERE FileName = '+QuotedStr(LFileContents[LIndex])+
             ' AND StudyAreaName = '+QuotedStr(LStudyAreaNameList[LIndex]);
            FAppModules.Database.ExecSQL(LSQL);
          end;
        end;
      finally
        LDataset.Free;
        LFileContents.Free;
        LStudyAreaNameList.Free;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

function TDbTableDataManager.ProcessDeleteSpecialTable(ATableProperty: TAbstractDbTableProperty): boolean;
const OPNAME = 'TDbTableDataManager.ProcessDeleteSpecialTable';
begin
  Result := false;
  try
    if Assigned(ATableProperty) then
    begin
      if ((UpperCase(ATableProperty.TableName)) = 'CHANGEGROUP') then
        Result := ProcessDeleteFromGeneralTable(ATableProperty);
      if ((UpperCase(ATableProperty.TableName)) = 'CHANGEGROUPELEMENT') then
        Result := ProcessDeleteFromGeneralTable(ATableProperty);
      if ((UpperCase(ATableProperty.TableName)) = 'CHANGELIST') then
        Result := ProcessDeleteFromGeneralTable(ATableProperty);
      if ((UpperCase(ATableProperty.TableName)) = 'CHANGEPARAMETER') then
        Result := ProcessDeleteFromGeneralTable(ATableProperty);
      if ((UpperCase(ATableProperty.TableName)) = 'METADATALIST') then
        Result := ProcessDeleteMetaDataTables(ATableProperty);
      if ((UpperCase(ATableProperty.TableName)) = 'METADATAITEM') then
        Result := TRUE;
      //if ((UpperCase(ATableProperty.TableName)) = 'HYDROLOGYFILEDATA') then
      //  Result := ProcessDeleteHydrologyFileDataTable(ATableProperty);
      if ((UpperCase(ATableProperty.TableName)) = 'STUDYDOCUMENTS') then
        Result := ProcessDeleteStudyDucumentTables(ATableProperty);
      if ((UpperCase(ATableProperty.TableName)) = 'STUDYSCENARIODOCUMENTS') then
        Result := ProcessDeleteStudyDucumentTables(ATableProperty);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.ProcessCopySpecialTable(ATableProperty: TAbstractDbTableProperty): boolean;
const OPNAME = 'TDbTableDataManager.ProcessCopySpecialTable';
begin
  Result := false;
  try
    if Assigned(ATableProperty) then
    begin
      if ((UpperCase(ATableProperty.TableName)) = 'CHANGELIST') then
        Result := ProcessCopyChangeTables(ATableProperty);
      if ((UpperCase(ATableProperty.TableName)) = 'CHANGEPARAMETER') then
        Result := TRUE;
      if ((UpperCase(ATableProperty.TableName)) = 'CHANGEGROUP') then
        Result := TRUE;
      if ((UpperCase(ATableProperty.TableName)) = 'CHANGEGROUPELEMENT') then
        Result := TRUE;
      if ((UpperCase(ATableProperty.TableName)) = 'METADATALIST') then
        Result := ProcessCopyMetaDataTables(ATableProperty);
      if ((UpperCase(ATableProperty.TableName)) = 'METADATAITEM') then
        Result := TRUE;
      if ((UpperCase(ATableProperty.TableName)) = 'STUDYDOCUMENTS') then
        Result := ProcessCopyStudyDucumentTables(ATableProperty);
      if ((UpperCase(ATableProperty.TableName)) = 'STUDYSCENARIODOCUMENTS') then
        Result := ProcessCopyStudyDucumentTables(ATableProperty);
      if ((UpperCase(ATableProperty.TableName)) = 'HYDROLOGYFILEDATA') then
        Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.ProcessCopyStudyDucumentTables(ATableProperty: TAbstractDbTableProperty): boolean;
const OPNAME = 'TDbTableDataManager.ProcessCopyStudyDucumentTables';
begin
  Result := False;
  try
      if((UpperCase(ATableProperty.TableName)) = 'STUDYDOCUMENTS') and
        (FSelectionLevel = malStudy)then
         Result := ProcessCopyFromGeneralTable(ATableProperty)
      else
      begin
        Result := True;
        Exit;
      end;
      if((UpperCase(ATableProperty.TableName)) = 'STUDYSCENARIODOCUMENTS') and
        ((FSelectionLevel = malStudy) or (FSelectionLevel = malSubArea))then
        Result := ProcessCopyFromGeneralTable(ATableProperty)
      else
      begin
        Result := True;
        Exit;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.ProcessDeleteStudyDucumentTables(ATableProperty: TAbstractDbTableProperty): boolean;
const OPNAME = 'TDbTableDataManager.ProcessDeleteStudyDucumentTables';
var
  LSQL: string;
begin
  Result := false;
  try
    if Assigned(ATableProperty) then
    begin
      if ((UpperCase(ATableProperty.TableName)) = 'STUDYDOCUMENTS') then
      begin
        if (FSelectionLevel <> malStudy) then
        begin
          if(GetScenarioCountPerStudy > 1) then
          begin
            Result := True;
            Exit;
          end;
        end;
        LSQL := 'DELETE FROM '+ ATableProperty.TableName;
        LSQL := LSQL +  ' WHERE StudyAreaName = '+ QuotedStr(FStudyArea.StudyAreaCode);
        FAppModules.Database.ExecSQL(LSQL);
        Result := True;
      end;
      if ((UpperCase(ATableProperty.TableName)) = 'STUDYSCENARIODOCUMENTS') then
      begin
        LSQL := 'DELETE FROM '+ ATableProperty.TableName;
        LSQL := LSQL +  ' WHERE '+
                ' StudyAreaName = ' + QuotedStr(FStudyArea.StudyAreaCode) +
                ' AND SubArea = ' + QuotedStr(FStudyArea.SubAreaCode) +
                ' AND Scenario = ' + QuotedStr(FStudyArea.ScenarioCode);

        FAppModules.Database.ExecSQL(LSQL);
        Result := True;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.GetScenarioCountPerStudy: integer;
const OPNAME = 'TDbTableDataManager.GetScenarioCountPerStudy';
var
  LDataSet: TAbstractModelDataset;
  LSQL: string;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LSQL := 'SELECT COUNT(*) AS ScenarioCount FROM StudyScenario'+
              ' WHERE StudyAreaName = '+ QuotedStr(FStudyArea.StudyAreaCode);
      LDataSet.SetSQL(LSQL);
      LDataset.DataSet.Open;
      Result := LDataset.DataSet.FieldByName('ScenarioCount').AsInteger;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.ExportSystemData: boolean;
const OPNAME = 'TDbTableDataManager.ExportSystemData';
begin
  Result := false;
  try
    Self.Initialise;
    if SelectFilesDirectory then
    begin
      FExportingSystemData := True;
      FProgressDialog.clbOptions.Items.Clear;
      FProgressDialog.clbOptions.Items.Add('Stop on first error');
      FProgressDialog.clbOptions.Checked[0] :=  False;//FAppModules.GlobalData.StopOnFirstErr;
      FProgressDialog.AddExecutionFunctions ( ExecExportSystemData );
      FProgressDialog.Caption := FAppModules.Language.GetString ( 'TDbTableDataManager.strExport' );
      FProgressDialog.Succsessful := False;
      FProgressDialog.ShowModal;
      FExportingSystemData := False;
      Result := FProgressDialog.Succsessful;
      FProgressDialog.Hide;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDbTableDataManager.BuildDataBase : boolean;
const OPNAME = 'TDbTableDataManager.BuildDataBase';
begin
  Result := false;
  try
    Self.Initialise;
    FProgressDialog.clbOptions.Items.Clear;
    FProgressDialog.clbOptions.Items.Add('Stop on first error');
    FProgressDialog.clbOptions.Checked[0] :=  False;//FAppModules.GlobalData.StopOnFirstErr;
    FProgressDialog.AddExecutionFunctions ( ExecBuildDataBase );
    FProgressDialog.Caption := FAppModules.Language.GetString ( 'TDbTableDataManager.strBuildDataBase' );
    FProgressDialog.Succsessful := False;
    FProgressDialog.ShowModal;
    Result := FProgressDialog.Succsessful;
    FProgressDialog.Hide;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDbTableDataManager.ExecBuildDataBase ( AProgressUpdateFuntion : TProgressUpdateFuntion ) : boolean;
const OPNAME = 'TDbTableDataManager.ExecBuildDataBase';
var
  LMainDatFileName,
  LTableName : string;
  LMainDatFile : TStringList;
  LIndex : integer;
  LResult,
  LStop : boolean;
  LDataBase : TAbstractDatabaseLayer;
  LTableProperty : TAbstractDbTableProperty;
  LExistingTableList : TStringList;
  LDbTableFieldsList : TTableFieldsDefList;
  LLoadMainDatFileDialog : TOpenDialog;
  function TableDoesNotExist : boolean;
  const OPNAME = 'UDbTableDataManager.TableDoesNotExist';
  begin
    Result := False;
    if (LExistingTableList.Count > 0) then
      Result := (LExistingTableList.IndexOf(LTableProperty.TableName) < 0);
  end;
begin
  Result := False;
  try
    try
      LResult := False;
      LExistingTableList := TStringList.Create;
      LDbTableFieldsList := TTableFieldsDefList.Create ( FAppModules );
      LDbTableFieldsList.Initialise;
      LDataBase := FAppModules.Database;
      LDataBase.GetTableNames ( LExistingTableList );
      LMainDatFile  := TStringList.Create;
      try
        LLoadMainDatFileDialog := TOpenDialog.Create ( nil );
        LLoadMainDatFileDialog.DefaultExt  := 'DAT';
        LLoadMainDatFileDialog.Filter      := 'Dat files (*.dat)|*.DAT';
        LLoadMainDatFileDialog.FilterIndex := 1;
        AProgressUpdateFuntion ( FAppModules.Language.GetString('Message.LoadingMainBuildScriptFile'), ptNone, LStop );
        if ( LLoadMainDatFileDialog.Execute ) then
          LMainDatFileName := LLoadMainDatFileDialog.FileName;
      finally
        FreeAndNil ( LLoadMainDatFileDialog );
      end;
      LMainDatFile.LoadFromFile ( LMainDatFileName );
      if ( LMainDatFile.Count > 0 ) then
      begin
        FProgressDialog.ActionProgressBar.Min := 0;
        FProgressDialog.ActionProgressBar.Max := LMainDatFile.Count - 45;
        for LIndex := 0 to LMainDatFile.Count -1 do
        begin
          if ( Copy ( LMainDatFile [ LIndex ], 1, 1 ) = ';' ) or ( Trim ( LMainDatFile [ LIndex ] ) = '' ) then
            Continue;
          LTableName := Trim ( LMainDatFile [ LIndex ] );
          LTableProperty := FAppModules.DBTablePropertyManager.TablePropertyByName [ LTableName ];
          if ( LTableProperty = nil ) then
          begin
            AProgressUpdateFuntion (FAppModules.Language.GetString('Message.Table') + LTableName +
                                    FAppModules.Language.GetString('Message.DoesNotHavePSroperties'),ptError,LStop );
            if ( LStop ) then
              Exit;
          end
          else
          begin
            AProgressUpdateFuntion (FAppModules.Language.GetString('Message.ProcessingTable') + LTableName , ptNone, LStop );
            if ( LStop ) then
              Exit;

            if ( TableDoesNotExist ) then
              LResult := CreateDbTable ( LTableProperty, LDbTableFieldsList, AProgressUpdateFuntion )
            else
            if ( TableDropped ( LTableName ) ) then
              LResult := CreateDbTable ( LTableProperty, LDbTableFieldsList, AProgressUpdateFuntion )
          end;
          if not UpdateProgress ( LResult ) then
            Exit;
          LTableProperty := nil;
          Result := LResult;
        end;
      end;
    AProgressUpdateFuntion (FAppModules.Language.GetString('Message.LoadingSystemData') , ptNone, LStop );
    if ( LStop ) then
     Exit;
    LoadSystemData ( AProgressUpdateFuntion );
    AProgressUpdateFuntion ( FAppModules.Language.GetString('Message.Done') , ptNone, LStop );
  finally
    FreeAndNil ( LMainDatFile );
    FreeAndNil ( LExistingTableList );
    FreeAndNil ( LDbTableFieldsList );
  end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDbTableDataManager.CreateDbTable ( ATableProperty : TAbstractDbTableProperty; ATableFieldsDefList : TTableFieldsDefList;
                                           AProgressUpdateFuntion : TProgressUpdateFuntion ) : boolean;
const OPNAME = 'TDbTableDataManager.CreateDbTable';
var
  LDataSet : TAbstractModelDataset;
  LSQL : string;
  LDbTableFields : TAbstractTableFieldsDef;
  LStop : boolean;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset ( integer ( dtExecSQL ), LDataSet );
    try
     if ( ATableProperty <> nil ) then
        LSQL := 'CREATE TABLE ' + ATableProperty.TableName +' ('
      else
      begin
       AProgressUpdateFuntion (FAppModules.Language.GetString('Message.NoTableProperties') ,ptError,LStop );
       if ( LStop ) then
         Exit;
      end;
      if ( ATableFieldsDefList <> nil ) then
      begin
        LDbTableFields := ATableFieldsDefList.TableFieldsDefByName [ ATableProperty.TableName ];
        LSQL := LSQL + LDbTableFields.CreateTableFields;
        LSQL := LSQL + ', PRIMARY KEY (';
        LSQL := LSQL + ATableProperty.IndexFieldNames.CommaText;
        LSQL := LSQL + ') );';
        LDataSet.ClearSQL;
        LDataSet.SetSQL ( LSQL );
        LDataSet.ExecSQL;
        Result := True;
      end
      else
      begin
        AProgressUpdateFuntion (FAppModules.Language.GetString('Message.NoFieldsDefinition') ,ptError,LStop );
        if ( LStop ) then
          Exit;
      end;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDbTableDataManager.TableDropped ( ATableName : string ): boolean;
const OPNAME = 'TDbTableDataManager.TableDropped';
var
  LDataSet : TAbstractModelDataset;
  LSQL : string;
begin
  Result := false;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL),LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.ClearSQL;
        LSQL := 'Drop Table ' + ATableName + ';';
        LDataSet.SetSQL(LSQL);
        FAppModules.Database.ExecSQL(LSQL);
      end;
    finally
      LDataSet.Free;
    end;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDbTableDataManager.LoadSystemData(AProgressUpdateFuntion : TProgressUpdateFuntion):boolean;
const OPNAME = 'TDbTableDataManager.LoadSystemData';
var
  LIndex: integer;
  LFound: boolean;
  LTableName,
  LFileName: string;
  LTempFilesContainer,
  LFilesContainer: TStringList;
  LSystemTables: TObjectList;
  LTableProperty: TAbstractDbTableProperty;
  LResult,
  LStop: boolean;
  LLoadSystemFileDialog : TOpenDialog;
begin
  Result := False;
  try
    LLoadSystemFileDialog := TOpenDialog.Create(nil);
    LLoadSystemFileDialog.DefaultExt  := 'ZIP';
    LLoadSystemFileDialog.Filter      := 'ZIP files (*.zip)|*.ZIP';
    LLoadSystemFileDialog.FilterIndex := 1;
    if (LLoadSystemFileDialog.Execute) then
      FZipFileName := LLoadSystemFileDialog.FileName;
  finally
    FreeAndNil(LLoadSystemFileDialog);
  end;
  try
    if UnZipFiles then
    begin
      LFilesContainer := TStringList.Create;
      LTempFilesContainer := TStringList.Create;
      try
        LFound := False;
        if SearchFiles(FTempDirectory + CMainScriptPrefix + '*', LTempFilesContainer) then
        begin
          for LIndex := 0 to LTempFilesContainer.Count -1 do
          begin
            LFileName := ExtractFileName(LTempFilesContainer[LIndex]);
            if (UpperCase(Copy(LFileName, 1, 5 )) = CMainScriptPrefix) then
            begin
              LFound := True;
              break;
            end;
          end;
          if not LFound then
            raise Exception.Create('Main Script File ' + LFileName + ' not found!')
          else
          begin
            LFileName := LTempFilesContainer[LIndex];
            LTempFilesContainer.LoadFromFile(LFileName);
            LSystemTables := TObjectList.Create(False);
            try
              for LIndex := 1 to LTempFilesContainer.Count -1 do
              begin
                LTableName := TableNameFromFileName(LTempFilesContainer[LIndex]);
                LTableProperty := FAppModules.DBTablePropertyManager.TablePropertyByName[LTableName];
                if (LTableProperty = nil) then
                begin
                  AProgressUpdateFuntion(FAppModules.Language.GetString('Message.Table') +
                  LTableName + FAppModules.Language.GetString('Message.DoesNotHavePSroperties'),ptError,LStop);
                  if LStop then Exit;
                end
                else
                begin
                  LFilesContainer.Add(LTempFilesContainer[LIndex]);
                  LSystemTables.Add(LTableProperty);
                end;
              end;
              FProgressDialog.ActionProgressBar.Max := LSystemTables.Count+1;
              AProgressUpdateFuntion (FAppModules.Language.GetString('Message.PreProcessingFileData') ,ptNone,LStop);
              if LStop then Exit;
              for LIndex := 0 to LSystemTables.Count -1 do
              begin
                LTableProperty := TAbstractDbTableProperty(LSystemTables[LIndex]);
                AProgressUpdateFuntion(FAppModules.Language.GetString('Message.ProcessingTable') + LTableProperty.TableName,ptNone,LStop);
                if LStop then
                  Exit;
                LResult := ProcessImportTableData(LFilesContainer[LIndex],LTableProperty,AProgressUpdateFuntion);
                if not UpdateProgress(LResult) then Exit;
              end;
              Result := TRUE;
            finally
              LSystemTables.Free;
            end;
          end;
        end;
      finally
        LFilesContainer.Free;
        LTempFilesContainer.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.ImportBlobFields(AFieldIndex    : string;
                                              AAppendDataSet : TAbstractModelDataset;
                                              AClientDataSet : TClientDataSet;
                                              ATableProperty : TAbstractDbTableProperty) : boolean;
const OPNAME = 'TDbTableDataManager.ImportBlobFields';
var
  LDataSet : TAbstractModelDataset;
  LIndex   : integer;
  LFieldName,
  LParamName,
  LParamValue,
  LSQLWhere      : string;
  //LBlobStream    : TBlobField;
  //LMemStream     : TMemoryStream;
  //LValueStr      : TStringList;
  LBlobList      : TStringList;
  LSQLBlobFields : string;
  lBlobIdx       : integer;
begin
  Result := False;
  try
    if Assigned(AAppendDataSet) and Assigned(ATableProperty) and Assigned(AClientDataSet) then
    begin
      FAppModules.Database.CreateDataset(integer(dtExecSQL),LDataSet);
      if Assigned(LDataSet) then
      begin
        LBlobList := TStringList.Create;
        try
          LBlobList.CommaText := AFieldIndex;
          LSQLBlobFields      := '';
          for LIndex := 0 to LBlobList.Count - 1 do
          begin
            lBlobIdx := StrToInt(LBlobList.Strings[LIndex]);
            if (LSQLBlobFields = '') then
              LSQLBlobFields := AClientDataSet.FieldDefs.Items[lBlobIdx].Name
            else
              LSQLBlobFields := LSQLBlobFields + ', ' + AClientDataSet.FieldDefs.Items[lBlobIdx].Name;
          end;
          for LIndex := 0 to ATableProperty.IndexFieldNames.Count -1 do
          begin
            LSQLWhere := LSQLWhere + ' ' + ATableProperty.IndexFieldNames[LIndex];
            LSQLWhere := LSQLWhere + ' = :A'+ATableProperty.IndexFieldNames[LIndex];
            if (LIndex <> (ATableProperty.IndexFieldNames.Count -1)) then
              LSQLWhere := LSQLWhere + ' AND '
          end;
          LDataSet.ClearSQL;
          LDataSet.SetSQL ('SELECT * FROM ' +
                           ATableProperty.TableName + ' WHERE ' + LSQLWhere);
          for LIndex := 0 to ATableProperty.IndexFieldNames.Count -1 do
          begin
            LFieldName := ATableProperty.IndexFieldNames[LIndex];
            LParamName  := 'A'+LFieldName;
            if AClientDataSet.FieldByName(LFieldName).IsNull then
              LParamValue := 'NULL'
            else
              LParamValue := AClientDataSet.FieldByName(LFieldName).AsString;
            LDataSet.SetParamValue(LParamName,LParamValue,AClientDataSet.FieldByName(LFieldName).DataType);
          end;
          LDataSet.SetReadOnly(False);
          LDataSet.DataSet.Open;
          LDataSet.DataSet.Edit;
          for LIndex := 0 to LBlobList.Count - 1 do
          begin
            lBlobIdx    := StrToInt(LBlobList.Strings[lIndex]);
            LFieldName  := AClientDataSet.FieldDefs.Items[lBlobIdx].Name;
            LDataSet.DataSet.FieldByName(LFieldName).Value := AClientDataSet.FieldByName(LFieldName).Value;

            {LBlobStream := LDataSet.DataSet.CreateBlobStream(LDataSet.DataSet.FieldByName(LFieldName),bmWrite);
            LValueStr   := TStringList.Create;
            LMemStream  := TMemoryStream.Create;
            try
              LMemStream.Position := 0;
              LValueStr.Add(Trim(AClientDataSet.FieldByName(LFieldName).AsString));
              LValueStr.SaveToStream(LMemStream);
              LMemStream.Position := 0;
              try
                LBlobStream.CopyFrom(LMemStream,LMemStream.Size);
                LDataSet.DataSet.Post;
              finally

              end;
            finally
              //FreeAndNil(LBlobStream);
              FreeAndNil(LMemStream);
              FreeAndNil(LValueStr);
            end;}
          end;
          LDataSet.DataSet.Post;
          Result := True;
        finally
          FreeAndNil(LBlobList);
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDbTableDataManager.CopyDiagramsToDisk ( AKeyField, AKeyValues : string; ADataset : TDataset ) : boolean;
const OPNAME = 'TDbTableDataManager.CopyDiagramsToDisk';
var
  LSourceFileName,
  LTargetFileName,

  LSourceStudyAreaCode,
  LSourceSubAreaCode,
  LSourceScenarioCode,

  LSourceDrawingName,
  LSourceGroupName,
  LDestGroupName,

  LNewValue,
  LPath : string;
  LKeyFields   : TStringList;
  LKeyValues   : TStringList;
begin
  Result := False;
  try
    if ( ADataset <> nil ) and ( AKeyValues <> '' ) and ( AKeyField <> '' ) then
    begin
      LKeyFields := TStringList.Create;
      LKeyValues := TStringList.Create;
      try
        LKeyFields.CommaText  := AKeyField;
        LKeyValues.CommaText  := AKeyValues;
        LNewValue             := Trim(LKeyValues[LKeyValues.Count-1]);

        LSourceStudyAreaCode  := Trim(ADataset.FieldByName ( 'StudyAreaName' ).AsString);
        LSourceSubAreaCode    := Trim(ADataset.FieldByName ( 'SubArea' ).AsString);
        LSourceScenarioCode   := Trim(ADataset.FieldByName ( 'Scenario' ).AsString);
        LSourceDrawingName    := Trim(ADataset.FieldByName ( 'DrawingName' ).AsString) +'.VSD';
        LSourceGroupName      := GetDrawingGroupName ( LSourceStudyAreaCode, LSourceSubAreaCode, LSourceScenarioCode );
        LDestGroupName        := LNewValue;

        LPath                 := NetworkDiagramsPath;
        LSourceFileName       := LPath + ChopCharacters( LSourceStudyAreaCode ) +'\'+
                                         ChopCharacters ( LSourceSubAreaCode ) +'\'+
                                         ChopCharacters ( LSourceScenarioCode ) +'\'+
                                         LSourceGroupName + '-' + LSourceDrawingName;

        LTargetFileName       := '';
        case FSelectionLevel of
          malStudy:  LTargetFileName       := LPath + ChopCharacters ( LNewValue ) +'\'+
                                                      ChopCharacters ( LSourceSubAreaCode ) +'\'+
                                                      ChopCharacters ( LSourceScenarioCode ) + '\'+
                                                      LSourceGroupName + '-' + LSourceDrawingName;
          malSubArea:  LTargetFileName     := LPath + ChopCharacters ( LSourceStudyAreaCode ) +'\'+
                                                      ChopCharacters ( LNewValue ) +'\'+
                                                      ChopCharacters ( LSourceScenarioCode ) + '\'+
                                                      LSourceGroupName + '-' + LSourceDrawingName;
          malScenarion:  LTargetFileName   := LPath + ChopCharacters ( LSourceStudyAreaCode ) +'\'+
                                                      ChopCharacters ( LSourceSubAreaCode ) +'\'+
                                                      ChopCharacters ( LNewValue ) + '\'+
                                                      LNewValue + '-' + LSourceDrawingName;
        end;

        SysUtils.ForceDirectories ( ExtractFilePath ( LTargetFileName ) );
        if not FileExists ( LSourceFileName ) then
          Exit
        else
          CopyFile ( PChar ( LSourceFileName ), PChar ( LTargetFileName ), False );

        Result := True;
      finally
        FreeAndNil(LKeyFields);
        FreeAndNil(LKeyValues);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDbTableDataManager.CopyFileToTempZipDirectory(AFileName: string): boolean;
const OPNAME = 'TDbTableDataManager.CopyFileToTempZipDirectory';
var
  LTargetFileName : string;
begin
  Result := False;
  try
    if FileExists (AFileName) then
    begin
      LTargetFileName := FTempDirectory + ExtractFileName(AFileName);
      CopyFile(PChar(AFileName), PChar(LTargetFileName), False);
    end;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDbTableDataManager.CopyReportDocToDisk ( AKeyField, AKeyValues : string; ADataSet : TDataset ) : boolean;
const OPNAME = 'TDbTableDataManager.CopyReportDocToDisk';
var
  LSourceFileName,
  LTargetFileName,
  LStudyAreaCode,
  LFileName,
  LCategory,
  LPath : string;
begin
  Result := False;
  try
    if Pos(' ', AKeyValues) > 0 then
      AKeyValues := Copy(AKeyValues, 2, Length(AKeyValues) - 2);
    if (ADataset <> nil) and (AKeyValues <> '') and (AKeyField <> '') then
    begin
      LStudyAreaCode := Trim(ADataset.FieldByName('StudyAreaName').AsString);
      LCategory := Trim(ADataset.FieldByName('Category').AsString);
      LFileName := Trim(ADataset.FieldByName('FileName').AsString);
      LPath := StudyDocumentsPath;
      LSourceFileName := LPath + LStudyAreaCode +'\'+ LCategory +'\'+ LFileName;
      case FSelectionLevel of
        malStudy:
          if (AKeyField = 'StudyAreaName') then
            LTargetFileName := LPath + AKeyValues +'\'+ LCategory +'\'+ LFileName;
      end;
      SysUtils.ForceDirectories(ExtractFilePath(LTargetFileName));
      if not FileExists (LSourceFileName) then
        Exit
      else
        CopyFile(PChar(LSourceFileName), PChar(LTargetFileName), False);
      Result := True;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDbTableDataManager.GetDrawingGroupName(AStudyAreaCode, ASubAreaCode, AScenarioCode : string) : string;
const OPNAME = 'TDbTableDataManager.GetDrawingGroupName';
var
  LDataSet : TAbstractModelDataset;
begin
  Result := '';
  try
    FAppModules.Database.CreateDataset(integer (dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.DataSet.Close;
        LDataSet.SetSQL ('Select DrawingGroupName from VNVDrawingGroup ' +
                         ' Where '+
                         ' StudyAreaName = ' + QuotedStr(AStudyAreaCode) +
                         ' and SubArea = ' + QuotedStr(ASubAreaCode) +
                         ' and Scenario = ' + QuotedStr(AScenarioCode));
        LDataSet.DataSet.Open;
        Result := Trim(LDataSet.Dataset.FieldByName('DrawingGroupName').AsString);
      end;
    finally
      FreeAndNil(LDataSet);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDbTableDataManager.CreateClassRAndPatchRListFile(AFileName : string) : boolean;
const OPNAME = 'TDbTableDataManager.CreateClassRAndPatchRListFile';
var
  LFileName : string;
  LExtractedFileName : string;
  LExtractedFilePath : string;
begin
  Result := False;
  try
    LFileName := FTempDirectory+CClassRPatchRScriptPrefix+AFileName + '.dat';
    LExtractedFileName := ExtractFileName(LFileName);
    LExtractedFilePath := ExtractFilePath(LFileName);
    DeleteFile(PChar(LFileName));
    FClassRPatchRList.SaveToFile(LFileName);
    LExtractedFileName := ExtractFileName(LFileName);
    FScriptList.Add(LExtractedFileName);
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{******************************************************************************}
{* EXPORT                                                                     *}
{******************************************************************************}

function TDbTableDataManager.SubAreaFileName (ALevel : TModelActionLevel) : string;
const OPNAME = 'TDbTableDataManager.SubAreaFileName';
begin
  Result := '';
  try
    case ALevel of
      malStudy     : Result := ChopCharacters(FStudy);
      malSubArea   : Result := ChopCharacters(FModel) + '_' +
                               ChopCharacters(FStudy) + '_' +
                               ChopCharacters(FSubArea);
      malScenarion : Result := ChopCharacters(FModel) + '_' +
                               ChopCharacters(FStudy) + '_' +
                               ChopCharacters(FSubArea) + '_' +
                               ChopCharacters(FScenario);
      else
        Result := 'System_Data';
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.ExportStudyData (ASelectionLevel       : TModelActionLevel;
                                              AStudyArea            : TAbstractStudyArea): boolean;
const OPNAME = 'TDbTableDataManager.ExportStudyData';
begin
  Result := FALSE;
  try
    Self.Initialise;
    FSelectionLevel       := ASelectionLevel;
    FStudyArea            := AStudyArea;
    if (AStudyArea <> nil) then
      FStudy        := AStudyArea.StudyAreaCode;
    FModel        := '';
    FSubArea      := '';
    FScenario     := '';
    if SelectFilesDirectory then
    begin
      FProgressDialog.clbOptions.Items.Clear;
      FProgressDialog.clbOptions.Items.Add('Stop on first error');
      FProgressDialog.clbOptions.Checked[0] := False;// FAppModules.GlobalData.StopOnFirstErr;
      FProgressDialog.clbOptions.Items.Add('Include Base Data');
      FProgressDialog.clbOptions.Items.Add('Include Output Data');
      FProgressDialog.AddExecutionFunctions(ExecExportConfirmHydrologyFiles);
      if AStudyArea = nil then
        FProgressDialog.AddExecutionFunctions(ExecExportAll);

      if (FSelectionLevel = malStudy) and (AStudyArea <> nil) then
        FProgressDialog.AddExecutionFunctions(ExecExportStudy)
      else
      begin
        if (AStudyArea <> nil) then
        begin
          FModel   := AStudyArea.ModelCode;
          FSubArea := AStudyArea.SubAreaCode;
          if (FSelectionLevel = malScenarion) then
            FScenario := AStudyArea.ScenarioCode;
          FProgressDialog.AddExecutionFunctions(ExecExportSubArea);
        end;
      end;
      FProgressDialog.Caption := FAppModules.Language.GetString('TDbTableDataManager.strExport');
      FProgressDialog.Succsessful := False;
      FProgressDialog.ShowModal;
      Result := FProgressDialog.Succsessful;
      FProgressDialog.Hide;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.ExecExportAll(AProgressUpdateFuntion : TProgressUpdateFuntion) : boolean;
const OPNAME = 'TDbTableDataManager.ExecExportAll';
var
  LSQL         : string;
  LResult      : boolean;
  LStop        : boolean;
  LIndex       : integer;
  LDataSet     : TAbstractModelDataset;
  LStudyList   : TStringList;
  LPreStudy    : string;
begin
  Result := False;
  try
    LStudyList   := TStringList.Create;
    try
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      try
        if Assigned(LDataSet) then
        begin
          LDataSet.DataSet.Close;
          LSQL := ' SELECT distinct(StudyAreaName) as Study, Model, SubArea From StudySubArea ';
          LDataSet.SetSQL(LSQL);
          LDataSet.DataSet.Open;
          LResult := True;

          while (not LDataSet.DataSet.Eof) do
          begin
            if (UpperCase(Trim(LDataSet.DataSet.FieldByName('Study').AsString)) <> UpperCase(LPreStudy)) then
            begin
              FStudy   := Trim(LDataSet.DataSet.FieldByName('Study').AsString);
              AProgressUpdateFuntion('.  ', ptNone, lStop );
              AProgressUpdateFuntion('*********************************************', ptNone, lStop );
              AProgressUpdateFuntion(FAppModules.Language.GetString('Message.Processing') + FStudy, ptNone, lStop );
              AProgressUpdateFuntion('*********************************************', ptNone, lStop );
              LResult := LResult and ExecExportStudy(AProgressUpdateFuntion);
              LStudyList.Add (ChopCharacters(FStudy)+ '.zip');
              if (not UpdateProgress(lResult)) then
                Exit;
              LPreStudy := Trim(LDataSet.DataSet.FieldByName('Study').AsString);
            end;
            LDataSet.DataSet.Next;
          end;
          AProgressUpdateFuntion(FAppModules.Language.GetString('Message.MovingStudiesZipFiles') ,ptNone,lStop);
          if (LStudyList.Count > 0) then
          begin
            for LIndex := 0 to LStudyList.Count - 1 do
              LStudyList[LIndex] := FDirectory + LStudyList[LIndex];
          end;
          LStudyList.Add(CreateZipFileList(FDirectory));
          LResult := LResult and CreateStudyZip('Study_Data', LStudyList);
          if (not UpdateProgress(lResult)) then Exit;

        end;
      finally
        FreeAndNil(LDataSet);
      end;
    finally
      FreeAndNil(LStudyList);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.CreateZipFileList(AFileName : string) : string;
const OPNAME = 'TDbTableDataManager.CreateZipFileList';
var
  LFileName : string;
  LExtractedFileName : string;
  LExtractedFilePath : string;
begin
  Result := '';
  try
    LFileName := AFileName+CMainScriptPrefix+'AllStadyData.dat';
    LExtractedFileName := ExtractFileName(LFileName);
    LExtractedFilePath := ExtractFilePath(LFileName);
    FZipFileList.Insert(0,'TableVersion=' + GetCurrentModuleVersion);
    DeleteFile(PChar(LFileName));
    FZipFileList.SaveToFile(LFileName);
    LExtractedFileName := ExtractFileName(LFileName);
    FScriptList.Add(LExtractedFileName);
    Result := LFileName;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TDbTableDataManager.ExecExportStudy (AProgressUpdateFuntion : TProgressUpdateFuntion) : boolean;
const OPNAME = 'TDbTableDataManager.ExecExportStudy';
var
  lSQL         : string;
  lResult      : boolean;
  lStop        : boolean;
  lIndex       : integer;
  lDataSet     : TAbstractModelDataset;
  lSubAreaList : TStringList;
begin
  Result := FALSE;
  try
    lSubAreaList := TStringList.Create;
    try
      FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
      try
        if Assigned(lDataSet) then
        begin
          lDataSet.DataSet.Close;
          lSQL := ' SELECT Model, SubArea From StudySubArea '+
                  ' WHERE StudyAreaName = ' + QuotedStr(FStudy);
          lDataSet.SetSQL(lSQL);
          lDataSet.DataSet.Open;

          lResult := TRUE;
          while (lResult) AND (NOT LDataSet.DataSet.Eof) do
          begin
            FModel   := Trim(lDataSet.DataSet.FieldByName('Model').AsString);
            FSubArea := Trim(lDataSet.DataSet.FieldByName('SubArea').AsString);

            AProgressUpdateFuntion('.  ', ptNone, lStop );
            AProgressUpdateFuntion('*********************************************', ptNone, lStop );
            AProgressUpdateFuntion(FAppModules.Language.GetString('Message.Processing') + FStudy + ' - ' + FModel + ' - ' + FSubArea, ptNone, lStop );
            AProgressUpdateFuntion('*********************************************', ptNone, lStop );
            lResult := lResult AND ExecExportSubArea(AProgressUpdateFuntion);

            lSubAreaList.Add (ChopCharacters(FModel) + '_' +
                              ChopCharacters(FStudy) + '_' +
                              ChopCharacters(FSubArea) + '.zip');

            if (NOT UpdateProgress(lResult)) then Exit;
            lDataSet.DataSet.Next;
          end;
          AProgressUpdateFuntion(FAppModules.Language.GetString('Message.MovingSubZipFiles') ,ptNone,lStop);

          if not FExportAll then
          begin
            if (lSubAreaList.Count > 1) then
            begin
              for lIndex := 0 to lSubAreaList.Count - 1 do
                lSubAreaList[lIndex] := FTempDirectory + lSubAreaList[lIndex];
            end;
            LResult := LResult and CreateStudyZip(SubAreaFileName(malStudy), lSubAreaList)
          end
          else
          if FExportAll then
          begin
            for lIndex := 0 to lSubAreaList.Count - 1 do
              LSubAreaList[lIndex] := FTempDirectory + lSubAreaList[lIndex];
            LResult := LResult and CreateStudyZip(FStudy, LSubAreaList);
          end;
          Result :=  LResult;
          if (NOT UpdateProgress(lResult)) then Exit;
        end;
      finally
        FreeAndNil(lDataSet);
      end;
    finally
      FreeAndNil(lSubAreaList);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.ExecExportSubArea (AProgressUpdateFuntion : TProgressUpdateFuntion): boolean;
const OPNAME = 'TDbTableDataManager.ExecExportSubArea';
begin
  Result := FALSE;
  try
    FDocsList.Clear;
    FScriptList.Clear;
    FClassRPatchRList.Clear;
    if (FModel = CRainfall) then
      Result := ExecExportRainfallSubArea(AProgressUpdateFuntion)
    //else if (FModel = CHydrology) then
    //  Result := ExecExportHydrologySubArea(AProgressUpdateFuntion)
    else if (FModel = CYield) OR (FModel = CPlanning) or
    (FModel = CDailyDiversion) or (FModel = CIFRPreProcessor)  or (FModel =  CDamSedimentation) or
    (FModel = CStomsa) or (FModel = CRWH) or (FModel = CDDTS) then
      Result := ExecExportYieldSubArea(AProgressUpdateFuntion);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.CreateScriptListFile : boolean;
const OPNAME = 'TDbTableDataManager.CreateScriptListFile';
var
  LFileName,
  LExtractedFileName,
  LExtractedFilePath: string;
begin
  Result := False;
  try
    LFileName := FTempDirectory + SubAreaFileName(malSubArea) + '.dat';
    LExtractedFileName := ExtractFileName(LFileName);
    LExtractedFilePath := ExtractFilePath(LFileName);
    LFileName := LExtractedFilePath + CMainScriptPrefix + LExtractedFileName;
    DeleteFile(PChar(LFileName));
    FScriptList.Insert(0,'TableVersion=' + GetCurrentModuleVersion);
    FScriptList.SaveToFile(LFileName);
    FScriptList.Delete(0);
    LExtractedFileName := ExtractFileName(LFileName);
    FScriptList.Add(LExtractedFileName);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.CreateDocsListFile : boolean;
const OPNAME = 'TDbTableDataManager.CreateDocsListFile';
var
  lFileName           : string;
  lExtractedFileName  : string;
  lExtractedFilePath  : string;
  lIndex              : integer;
  lTempList           : TStringList;
  lTempStr            : string;
  lPos                : integer;
begin
  Result := FALSE;
  try
    lFileName := FTempDirectory + SubAreaFileName(malSubArea) + '.dat';
    lExtractedFileName := ExtractFileName(LFileName);
    lExtractedFilePath := ExtractFilePath(LFileName);
    lFileName := lExtractedFilePath + CDocsScriptPrefix + lExtractedFileName;
    DeleteFile(PChar(lFileName));

    lTempList := TStringList.Create;
    try
      lTempList.CommaText := FDocsList.CommaText;
      for lIndex := 0 to lTempList.Count - 1 do
      begin
        lTempStr := lTempList.Strings[lIndex];
        lPos     := Pos('Network Diagrams', lTempStr);
        if (lPos = 0) then
          lPos   := Pos('Reports', lTempStr);
        if (lPos = 0) then
          lPos   := Pos('Covers', lTempStr);
        lTempList.Strings[lIndex] := Copy(lTempStr, lPos, Length(lTempStr)-lPos+1);
      end;
      lTempList.SaveToFile(LFileName);
    finally
      FreeAndNil(lTempList);
    end;
    LExtractedFileName := ExtractFileName(LFileName);
    FScriptList.Add(LExtractedFileName);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.ProcessExportGeneralTable (ATableProperty : TAbstractDbTableProperty): boolean;
const OPNAME = 'TDbTableDataManager.ProcessExportGeneralTable';
var
  LDataSet         : TAbstractModelDataset;
  LSQL             : string;
  LWhereClause     : string;
  LFileName        : string;
  //LClientDataSet   : TClientDataSet;
  //LDataSetProvider : TDataSetProvider;
  lClauseValues    : TStringList;
begin
  Result := false;
  try
    if UpperCase (ATableProperty.TableName) = 'STUDYSCENARIOLOCK' then
    begin
      Result := True;
      Exit;
    end;
    if Assigned(ATableProperty) then
    begin
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      lClauseValues := TStringList.Create;

      try
        if Assigned(LDataSet) then
        begin
          if (ATableProperty.TableFilter <> '') then
            LWhereClause := ' WHERE ' + ATableProperty.TableFilter
          else
          if UpperCase (ATableProperty.TableName) <> 'VIEWDATANODE' then
            LWhereClause := GetGeneralWhereClause(ATableProperty);

          //LSQL := 'SELECT ' + Trim(ATableProperty.FieldNames.CommaText) +
          LSQL := 'SELECT ' + Trim(ATableProperty.DelimitedFieldNamesCommatext) +
                  ' FROM ' + ATableProperty.TableName +  LWhereClause;

          LDataSet.SetSQL(LSQL);
          LDataset.DataSet.Open;
          if(LDataset.DataSet.Eof) and (LDataset.DataSet.Bof) then
          begin
            Result := True;
          end
          else
          begin
            //LDataSetProvider := TDataSetProvider.Create(nil);
           // LClientDataSet := TClientDataSet.Create(nil);
            try
            {
              LDataSetProvider.DataSet := LDataset.DataSet;
              LClientDataSet.ReadOnly := True;
              LClientDataSet.SetProvider(LDataSetProvider);
              LClientDataSet.StoreDefs := True;
              LClientDataSet.Active := True;
              }

              LFileName := Trim(FTempDirectory) + 'LD_' + Trim(ATableProperty.TableName) + '.xml';

              //LClientDataSet.SaveToFile(LFileName,dfXML);
              LDataset.DataSet.SaveToFile(LFileName,pfXML);

              FScriptList.Add(ExtractFileName(LFileName));
            finally
              LDataset.DataSet.Active := False;
             // LClientDataSet.Active := False;
             // LClientDataSet.Free;
             // LDataSetProvider.Free;
            end;
          end;
          LDataset.DataSet.Close;
          LDataset.Free;
          Result := True;
        end;
        if (ATableProperty.TableName = 'VNVDrawing') then
          ExportNetworkDiagrams;
        if (ATableProperty.TableName = 'StudyDocuments') then
          ExportStudyDocuments;
        if (ATableProperty.TableName = 'StudyArea') then
          ExportStudyAreaShapeFiles;
        if (ATableProperty.TableName = 'StudySubArea') then
          ExportStudySubAreaShapeFiles;
      finally
        FreeAndNil(lClauseValues);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.ExportNetworkDiagrams : boolean;
const OPNAME = 'TDbTableDataManager.ExportNetworkDiagrams';
var
  lDataSetA        : TAbstractModelDataset;
  lDataSetB        : TAbstractModelDataset;
  LWhereClause     : string;
  lSQL             : string;
  lFileName        : string;
  lFilePath        : string;
  lStudyArea       : string;
  lSubArea         : string;
  lScenario        : string;
  lGroupID         : integer;
  lGroupName       : string;
  lDrawingName     : string;
  lGroupProperty   : TAbstractDbTableProperty;
  lDrawingProperty : TAbstractDbTableProperty;
  lClauseValues    : TStringList;
begin
  Result := FALSE;
  try
    lGroupProperty   := FAppModules.DBTablePropertyManager.TablePropertyByName['VNVDrawingGroup'];
    lDrawingProperty := FAppModules.DBTablePropertyManager.TablePropertyByName['VNVDrawing'];
    if (Assigned(lGroupProperty) AND Assigned(lDrawingProperty)) then
    begin
      FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSetA);
      FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSetB);
      lClauseValues := TStringList.Create;
      try
        if (Assigned(lDataSetA) AND Assigned(lDataSetB)) then
        begin
          LWhereClause := GetGeneralWhereClause ( lGroupProperty );
          lSQL := 'SELECT * FROM VNVDrawingGroup ' + LWhereClause;
          lDataSetA.SetSQL(lSQL);
          lDataSetA.DataSet.Open;
          while (NOT lDataSetA.DataSet.EOF) do
          begin
            lGroupID   := lDataSetA.DataSet.FieldByName('DrawingGroupID').AsInteger;
            lGroupName := Trim(lDataSetA.DataSet.FieldByName('DrawingGroupName').AsString);
            lStudyArea := Trim(lDataSetA.DataSet.FieldByName('StudyAreaName').AsString);
            lSubArea   := Trim(lDataSetA.DataSet.FieldByName('SubArea').AsString);
            lScenario  := Trim(lDataSetA.DataSet.FieldByName('Scenario').AsString);

            lFilePath  := NetworkDiagramsPath +
                          ChopCharacters(lStudyArea) + '\' +
                          ChopCharacters(lSubArea) + '\' +
                          ChopCharacters(lScenario) + '\';
            lSQL := 'SELECT * FROM VNVDrawing ' +
                    ' WHERE StudyAreaName = ' + QuotedStr(lStudyArea) +
                    ' AND SubArea = '         + QuotedStr(lSubArea) +
                    ' AND Scenario = '        + QuotedStr(lScenario) +
                    ' AND DrawingGroupID = '  + IntToStr(lGroupID);
            lDataSetB.SetSQL(lSQL);
            lDataSetB.DataSet.Open;
            while (NOT lDataSetB.DataSet.EOF) do
            begin
              lDrawingName := Trim(lDataSetB.DataSet.FieldByName('DrawingName').AsString);
              lFileName    := lFilePath + lGroupName + '-' + lDrawingName + '.VSD';
              if (FDocsList.IndexOf(lFileName) < 0) then
              begin
                CopyFileToTempZipDirectory(lFileName);
                FDocsList.Add(lFileName);
              end;
              lDataSetB.DataSet.Next
            end;
            lDataSetB.DataSet.Close;
            lDataSetA.DataSet.Next;
          end;
          lDataSetA.DataSet.Close;
        end;
      finally
        FreeAndNil(lDataSetA);
        FreeAndNil(lDataSetB);
        FreeAndNil ( lClauseValues );
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.ExportStudyDocuments : boolean;
const OPNAME = 'TDbTableDataManager.ExportStudyDocuments';
var
  lDataSet         : TAbstractModelDataset;
  LWhereClause,
  lSQL             : string;
  lFileName        : string;
  lStudyArea       : string;
  lCategory        : string;
  lDBFileName      : string;
  lProperty        : TAbstractDbTableProperty;
  lClauseValues : TStringList;
begin
  Result := FALSE;
  try
    lProperty := FAppModules.DBTablePropertyManager.TablePropertyByName['StudyDocuments'];
    if (Assigned(lProperty)) then
    begin
      FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
      lClauseValues := TStringList.Create;
      try
        if (Assigned(lDataSet)) then
        begin
          LWhereClause := GetGeneralWhereClause ( lProperty );
          lSQL := 'SELECT * FROM StudyDocuments ' + LWhereClause;
          lDataSet.SetSQL(lSQL);
          lDataSet.DataSet.Open;
          while (NOT lDataSet.DataSet.EOF) do
          begin
            lStudyArea   := Trim(lDataSet.DataSet.FieldByName('StudyAreaName').AsString);
            lCategory    := Trim(lDataSet.DataSet.FieldByName('Category').AsString);
            lDBFileName  := Trim(lDataSet.DataSet.FieldByName('FileName').AsString);
            lFileName    := StudyDocumentsPath +
                            lStudyArea + '\' +
                            lCategory + '\' +
                            lDBFileName;
            if (FDocsList.IndexOf(lFileName) < 0) then
            begin
              CopyFileToTempZipDirectory(lFileName);
              FDocsList.Add(lFileName);
            end;
            lDataSet.DataSet.Next
          end;
          lDataSet.DataSet.Close;
        end;
      finally
        FreeAndNil(lDataSet);
        FreeAndNil ( lClauseValues );
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.ExportStudyAreaShapeFiles : boolean;
const OPNAME = 'TDbTableDataManager.ExportStudyAreaShapeFiles';
var
  lDataSet         : TAbstractModelDataset;
  lSQL             : string;
  lFileName        : string;
  lDBFileName      : string;
  lProperty        : TAbstractDbTableProperty;
  lFilePath        : string;
  lMoreFiles       : boolean;
  lSearchRec       : TSearchRec;
begin
  Result := FALSE;
  try
    lProperty := FAppModules.DBTablePropertyManager.TablePropertyByName['StudyArea'];
    if (Assigned(lProperty)) then
    begin
      FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
      try
        if (Assigned(lDataSet)) then
        begin
          lSQL := 'SELECT * FROM StudyArea ' + GetGeneralWhereClause(lProperty);
          lDataSet.SetSQL(lSQL);
          lDataSet.DataSet.Open;
          while (NOT lDataSet.DataSet.EOF) do
          begin
            lDBFileName := Trim(lDataSet.DataSet.FieldByName('ShapeFileName').AsString);
            if (lDBFileName <> '') then
            begin
              lFilePath := DeploymentPath + C_StudyShapeFile + lDBFileName + '.*';
              lMoreFiles := (FindFirst(lFilePath, faAnyFile, lSearchRec) = 0);
              while lMoreFiles do
              begin
                lFileName := DeploymentPath + C_StudyShapeFile + lSearchRec.Name;
                if (FDocsList.IndexOf(lFileName) < 0) then
                begin
                  CopyFileToTempZipDirectory(lFileName);
                  FDocsList.Add(lFileName);
                end;
                lMoreFiles := (FindNext(lSearchRec)= 0);
              end;
              SysUtils.FindClose(LSearchRec);
            end;
            lDataSet.DataSet.Next
          end;
          lDataSet.DataSet.Close;
        end;
      finally
        FreeAndNil(lDataSet);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.ExportStudySubAreaShapeFiles : boolean;
const OPNAME = 'TDbTableDataManager.ExportStudySubAreaShapeFiles';
var
  lDataSet         : TAbstractModelDataset;
  LWhereClause,
  lSQL             : string;
  lFileName        : string;
  lDBFileName      : string;
  lProperty        : TAbstractDbTableProperty;
  lFilePath        : string;
  lMoreFiles       : boolean;
  lSearchRec       : TSearchRec;
  lClauseValues : TStringList;
begin
  Result := FALSE;
  try
    lProperty := FAppModules.DBTablePropertyManager.TablePropertyByName['StudySubArea'];
    if (Assigned(lProperty)) then
    begin
      FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
      lClauseValues := TStringList.Create;
      try
        if (Assigned(lDataSet)) then
        begin
          LWhereClause := GetGeneralWhereClause ( lProperty );
          lSQL := 'SELECT * FROM StudySubArea ' + LWhereClause;
          lDataSet.SetSQL(lSQL);
          lDataSet.DataSet.Open;
          while (NOT lDataSet.DataSet.EOF) do
          begin
            lDBFileName := Trim(lDataSet.DataSet.FieldByName('ShapeFileName').AsString);
            if (lDBFileName <> '') then
            begin
              lFilePath := DeploymentPath + C_SubAreaShapeFile + lDBFileName + '.*';
              lMoreFiles := (FindFirst(lFilePath, faAnyFile, lSearchRec) = 0);
              while lMoreFiles do
              begin
                lFileName := DeploymentPath + C_SubAreaShapeFile + lSearchRec.Name;
                if (FDocsList.IndexOf(lFileName) < 0) then
                begin
                  CopyFileToTempZipDirectory(lFileName);
                  FDocsList.Add(lFileName);
                end;
                lMoreFiles := (FindNext(lSearchRec)= 0);
              end;
              SysUtils.FindClose(LSearchRec);
            end;
            lDataSet.DataSet.Next
          end;
          lDataSet.DataSet.Close;
        end;
      finally
        FreeAndNil(lDataSet);
        FreeAndNil ( lClauseValues );
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDbTableDataManager.FindKeyValue (AKeyValues : string;
                                            AKey       : string;
                                            var AValue : string);
const OPNAME = 'TDbTableDataManager.FindKeyValue';
var
  lPos   : integer;
  lStart : integer;
  lEnd   : integer;
begin
  try
    AValue := '';
    lPos := Pos(AKey, AKeyValues);
    if (lPos > 0) then
    begin
      lStart := PosEx('=', AKeyValues, lPos);
      lEnd   := PosEx(',', AKeyValues, lPos);
      if (lEnd > 0) then
        AValue := Copy(AKeyValues, lStart+1, lEnd - (lStart+1))
      else
        AValue := Copy(AKeyValues, lStart+1, Length(AKeyValues) - lStart);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{******************************************************************************}
{ COPY                                                                        *}
{******************************************************************************}

function TDbTableDataManager.CopyStudyData (ASelectionLevel       : TModelActionLevel;
                                            AStudyArea            : TAbstractStudyArea;
                                            AStudyFields          : TStudyFields) : boolean;
const OPNAME = 'TDbTableDataManager.CopyStudyData';
begin
  Result := FALSE;
  try
    Self.Initialise;
    FSelectionLevel       := ASelectionLevel;
    FStudyArea            := AStudyArea;
    FStudyFields          := AStudyFields;

    FStudy        := AStudyArea.StudyAreaCode;
    FModel        := '';
    FSubArea      := '';
    FScenario     := '';
    if ActivateCopyStudyDataDialog then
    begin
      FProgressDialog.clbOptions.Items.Clear;
      FProgressDialog.clbOptions.Items.Add('Stop on first error');
      FProgressDialog.clbOptions.Checked[0] := False;// FAppModules.GlobalData.StopOnFirstErr;
      FProgressDialog.ProgressRichEdit.Clear;

      if (FSelectionLevel = malStudy) then
      begin
        FProgressDialog.AddExecutionFunctions(ExecCopyStudy);
      end
      else
      begin
        FModel   := AStudyArea.ModelCode;
        FSubArea := AStudyArea.SubAreaCode;
        if (FSelectionLevel = malScenarion) then
          FScenario := AStudyArea.ScenarioCode;
        FProgressDialog.AddExecutionFunctions(ExecCopySubArea);
      end;

      FProgressDialog.Caption := FAppModules.Language.GetString('TDbTableDataManager.strCopy');
      FProgressDialog.Succsessful := FALSE;
      FProgressDialog.ShowModal;
      Result := FProgressDialog.Succsessful;
      FProgressDialog.Hide;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.ExecCopyStudy (AProgressUpdateFuntion : TProgressUpdateFuntion) : boolean;
const OPNAME = 'TDbTableDataManager.ExecCopyStudy';
var
  lSQL         : string;
  lResult      : boolean;
  lStop        : boolean;
  lDataSet     : TAbstractModelDataset;
begin
  Result := FALSE;
  try
    if (CopyStudyTable) then
    begin
      FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
      try
        if not ValidateStudyData then
        begin
          Result := True;
          Exit
        end;

        if Assigned(lDataSet) then
        begin
          lDataSet.DataSet.Close;
          lSQL := ' SELECT Model, SubArea FROM StudySubArea '+
                  ' WHERE StudyAreaName = ' + QuotedStr(FStudy);
          lDataSet.SetSQL(lSQL);
          lDataSet.DataSet.Open;

          while (NOT LDataSet.DataSet.Eof) do
          begin
            FModel   := Trim(lDataSet.DataSet.FieldByName('Model').AsString);
            FSubArea := Trim(lDataSet.DataSet.FieldByName('SubArea').AsString);

            FProgressDialog.ActionProgressBar.Min := 0;
            FProgressDialog.ActionProgressBar.Max := FAppModules.DBTablePropertyManager.TablesCount;

            AProgressUpdateFuntion('.  ', ptNone, lStop );
            AProgressUpdateFuntion('*********************************************', ptNone, lStop );
            AProgressUpdateFuntion(FAppModules.Language.GetString('Message.Copying') + FStudy + ' - ' + FModel + ' - ' + FSubArea, ptNone, lStop );
            AProgressUpdateFuntion('*********************************************', ptNone, lStop );
            lResult := ExecCopySubArea(AProgressUpdateFuntion);
            if (NOT UpdateProgress(lResult)) then Exit;
            lDataSet.DataSet.Next;
          end;
        end;
        Result := TRUE;
      finally
        FreeAndNil(lDataSet);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.ExecCopySubArea (AProgressUpdateFuntion : TProgressUpdateFuntion): boolean;
const OPNAME = 'TDbTableDataManager.ExecCopySubArea';
begin
  Result := FALSE;
  try
    if (FSelectionLevel = malScenarion) OR
       ((FSelectionLevel <> malScenarion) AND CopySubAreaTable) then
    begin
      if (FModel = CRainfall) then
        ExecCopyRainfallSubArea(AProgressUpdateFuntion)
      //else if (FModel = CHydrology) then
      //  ExecCopyHydrologySubArea(AProgressUpdateFuntion)
      else if (FModel = CYield) OR (FModel = CPlanning) OR
        (FModel = CDailyDiversion)  or (FModel = CIFRPreProcessor)  or (FModel =  CDamSedimentation) or
        (FModel = CStomsa)  OR (FModel = CRWH)  OR (FModel = CDDTS) then
        ExecCopyYieldSubArea(AProgressUpdateFuntion);
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.ExecCopyYieldSubArea (AProgressUpdateFuntion : TProgressUpdateFuntion) : boolean;
const OPNAME = 'TDbTableDataManager.ExecCopyYieldSubArea';
var
  lIndex       : integer;
  lTable       : TAbstractDbTableProperty;
  lResult      : boolean;
  lStop        : boolean;
  lModelTables : TObjectList;
begin
  Result := FALSE;
  try
    if (CopyScenarioTable) then
    begin
      lModelTables := TObjectList.Create(False);
      try
        if (FAppModules.DBTablePropertyManager.GetTablesPerModel(FModel, lModelTables)) then
        begin
          Result := TRUE;
          FProgressDialog.ActionProgressBar.Min := 0;
          FProgressDialog.ActionProgressBar.Max := lModelTables.Count;
          for lIndex := 0 to lModelTables.Count -1 do
          begin
            lResult := TRUE;
            lTable := TAbstractDbTableProperty(lModelTables.Items[lIndex]);
            AProgressUpdateFuntion( FAppModules.Language.GetString('Message.ProcessingTable')+ lTable.TableName,ptNone,lStop);
            if lStop then Exit;

            if (FSelectionLevel = malScenarion) then
            begin
              if (UpperCase(lTable.TableName) = 'TSCCHART') OR
                 (UpperCase(lTable.TableName) = 'TSCCHARTSERIES') OR
                 (UpperCase(lTable.TableName) = 'TSCVIEW') OR
                 (UpperCase(lTable.TableName) = 'TSCVIEWCHART') then
              begin
                if not UpdateProgress(lResult) then Exit;
                Continue;
              end;
            end;

            case lTable.TableGroup of
              tgModelData :
              begin
                if (UpperCase(lTable.TableName) <> 'STUDYMODEL') AND
                   (UpperCase(lTable.TableName) <> 'STUDYAREA') AND
                   (UpperCase(lTable.TableName) <> 'STUDYSUBAREA') AND
                   (UpperCase(lTable.TableName) <> 'STUDYSCENARIO') then
                begin
                  lResult := ProcessCopyFromGeneralTable(lTable);
                end;
              end;
              tgSpecial:
                begin
                  lResult := ProcessCopySpecialTable(lTable);
                end;
            end;
            if not UpdateProgress(lResult) then Exit;
            Result := Result AND lResult;
          end;
        end;
      finally
        lModelTables.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.ExecCopyRainfallSubArea (AProgressUpdateFuntion : TProgressUpdateFuntion) : boolean;
const OPNAME = 'TDbTableDataManager.ExecCopyRainfallSubArea';
var
  lIndex             : integer;
  lTableProperty     : TAbstractDbTableProperty;
  lResult            : boolean;
  lStop              : boolean;
  lModelTables       : TObjectList;
  lDBDataSet         : TAbstractModelDataset;
  lDBDataSet2        : TAbstractModelDataset;
  lDBDataSet3        : TAbstractModelDataset;
  LKeyData,
  lSQL               : string;
  lOldID             : integer;
  lNewID             : integer;
  lTblPrpPatch       : TAbstractDbTableProperty;
  lTblPrpPatchData   : TAbstractDbTableProperty;
  lTblPrpPatchScr    : TAbstractDbTableProperty;
  lKeyFields         : TStringList;
  lKeyValues         : TStringList;
  lPatchIDs          : TStringList;
  lPatchIDStr        : string;
begin
  Result := FALSE;
  try
    if (CopyScenarioTable) then
    begin
      lModelTables := TObjectList.Create(FALSE);
      LKeyFields := TStringList.Create;
      LKeyValues := TStringList.Create;
      try
        if (FAppModules.DBTablePropertyManager.GetTablesPerModel(FModel, lModelTables)) then
        begin
          Result := TRUE;
          lTblPrpPatch       := nil;
          lTblPrpPatchData   := nil;
          lTblPrpPatchScr    := nil;


          for lIndex := 0 to lModelTables.Count -1 do
          begin
            lResult := TRUE;
            lTableProperty := TAbstractDbTableProperty(lModelTables.Items[lIndex]);

            if ((UpperCase(lTableProperty.TableName)) = 'CHANGELIST') then
              ProcessCopyChangeTables(lTableProperty);

            if (lTableProperty.TableGroup = tgSpecial) then
            begin
              if ((UpperCase(lTableProperty.TableName)) <> 'CHANGEGROUP') AND
                 ((UpperCase(lTableProperty.TableName)) <> 'CHANGEGROUPELEMENT') AND
                 ((UpperCase(lTableProperty.TableName)) <> 'CHANGELIST') AND
                 ((UpperCase(lTableProperty.TableName)) <> 'CHANGEPARAMETER') AND
                 ((UpperCase(lTableProperty.TableName)) <> 'METADATALIST') AND
                 ((UpperCase(lTableProperty.TableName)) <> 'METADATAITEM') then
              begin
                lResult := ProcessCopySpecialTable(lTableProperty);
              end;
            end
            else
            if (lTableProperty.TableGroup = tgModelData) then
            begin
              if (UpperCase(lTableProperty.TableName) = 'RAINFALLPROJECTGAUGES') AND
                 (FSelectionLevel <> malScenarion) then
              begin
                lResult := ProcessCopyFromGeneralTable(lTableProperty);
                AProgressUpdateFuntion(FAppModules.Language.GetString('Message.ProcessingTable') + lTableProperty.TableName, ptNone, lStop);
                if not UpdateProgress(lResult) then Exit;
              end
              else if ((UpperCase(lTableProperty.TableName) = 'RAINFALLCATCHMENT') OR
                       (UpperCase(lTableProperty.TableName) = 'RAINFALLCATCHMENTFILEDATA') OR
                       (UpperCase(lTableProperty.TableName) = 'RAINFALLCATCHMENTFILEDETAIL') OR
                       (UpperCase(lTableProperty.TableName) = 'RAINFALLCATCHMENTSOURCE')) then
              begin
                lResult := ProcessCopyFromGeneralTable(lTableProperty);
                AProgressUpdateFuntion( FAppModules.Language.GetString('Message.ProcessingTable')+ lTableProperty.TableName,ptNone,lStop);
                if not UpdateProgress(lResult) then Exit;
              end
              else if (UpperCase(lTableProperty.TableName) = 'RAINFALLPATCHR') then
                lTblPrpPatch := lTableProperty
              else if (UpperCase(lTableProperty.TableName) = 'RAINFALLMONTHLYPATCHDATA') then
                lTblPrpPatchData := lTableProperty
              else if (UpperCase(lTableProperty.TableName) = 'RAINFALLPATCHSOURCE') then
                lTblPrpPatchScr := lTableProperty;
            end;
            Result := Result AND lResult;
          end;

          if (FSelectionLevel <> malScenarion) then
          begin
            FAppModules.Database.CreateDataset(integer(dtExecSQL), lDBDataset);
            FAppModules.Database.CreateDataset(integer(dtExecSQL), lDBDataset2);
            FAppModules.Database.CreateDataset(integer(dtExecSQL), lDBDataset3);
            lPatchIDs := TStringList.Create;
            try
              if (Assigned(lDBDataSet) AND Assigned(lDBDataSet2)) then
              begin
                lDBDataSet.DataSet.Close;
                lSQL := 'SELECT Max(PatchID) AS NewPatchID FROM RainfallPatchR';
                lDBDataSet.SetSQL(lSQL);
                lDBDataSet.Dataset.Open;
                lDBDataSet.Dataset.First;
                lNewID := 0;
                if (NOT lDBDataSet.DataSet.EOF) then
                  lNewID := lDBDataSet.Dataset.FieldByName('NewPatchID').AsInteger;
                if (lNewID = 0) then
                  lNewID := 20000;

                AProgressUpdateFuntion(FAppModules.Language.GetString('Message.ProcessingTable') + lTblPrpPatch.TableName,ptNone,lStop);
                AProgressUpdateFuntion(FAppModules.Language.GetString('Message.ProcessingTable') + lTblPrpPatchScr.TableName,ptNone,lStop);
                AProgressUpdateFuntion(FAppModules.Language.GetString('Message.ProcessingTable') + lTblPrpPatchData.TableName,ptNone,lStop);
                lDBDataSet.DataSet.Close;

                lSQL := 'SELECT * FROM RainfallPatchR ' +
                        'WHERE StudyAreaName = ' + QuotedStr(FStudy);
                if (FSelectionLevel <> malStudy) then
                  lSQL := lSQL + ' AND SubArea = ' + QuotedStr(FSubArea);

                lDBDataSet.SetSQL(lSQL);
                lDBDataSet.Dataset.Open;
                lDBDataSet.Dataset.First;

                while (NOT lDBDataSet.DataSet.Eof) do
                begin
                  lOldID := lDBDataSet.DataSet.FieldByName('PatchID').AsInteger;
                  lNewID := lNewID + 1;
                  lPatchIDs.AddObject(IntToStr(lOldID), TObject(lNewID));
                  LKeyFields.Clear;
                  LKeyValues.Clear;
                  // Copy RainfallPatchR
                  if (FSelectionLevel = malStudy) then
                  begin
                    lKeyFields.Add('StudyAreaName');
                    lKeyFields.Add('PatchID');
                    lKeyValues.Add(FStudyFields.FStudyAreaName);
                    lKeyValues.Add(IntToStr(lNewID));
                  end
                  else if (FSelectionLevel = malSubArea) then
                  begin
                    lKeyFields.Add('SubArea');
                    lKeyFields.Add('PatchID');
                    lKeyValues.Add(FStudyFields.FSubArea);
                    lKeyValues.Add(IntToStr(lNewID));
                  end;
                  if (NOT ProcessCopySingleRecord(lDBDataSet.DataSet, lTblPrpPatch, lKeyFields.CommaText, lKeyValues.CommaText)) then
                    Exit;

                  // Copy RainfallPatchSource
                  lDBDataSet2.DataSet.Close;
                  lSQL := 'SELECT * FROM RainfallPatchSource WHERE PatchID = ' + IntToStr(lOldID);
                  lDBDataSet2.SetSQL(lSQL);
                  lDBDataSet2.Dataset.Open;
                  lDBDataSet2.Dataset.First;
                  while (NOT lDBDataSet2.DataSet.Eof) do
                  begin
                    if (NOT ProcessCopySingleRecord(lDBDataSet2.DataSet, lTblPrpPatchScr, 'PatchID', IntToStr(lNewID))) then
                      Exit;
                    lDBDataSet2.DataSet.Next;
                  end;

                  // Copy RainfallMonthlyPatchData
                  lDBDataSet2.DataSet.Close;
                  lSQL := 'SELECT * FROM RainfallMonthlyPatchData WHERE PatchID = ' + IntToStr(lOldID);
                  lDBDataSet2.SetSQL(lSQL);
                  lDBDataSet2.Dataset.Open;
                  lDBDataSet2.Dataset.First;
                  while (NOT lDBDataSet2.DataSet.Eof) do
                  begin
                    if (NOT ProcessCopySingleRecord(lDBDataSet2.DataSet, lTblPrpPatchData, 'PatchID', IntToStr(lNewID))) then
                      Exit;
                    lDBDataSet2.DataSet.Next;
                  end;
                  lDBDataSet.DataSet.Next;
                end;

                // Copy ChangeParameter
                lDBDataSet.DataSet.Close;
                lSQL := 'SELECT * FROM ChangeParameter ' +
                        'WHERE ParamField IN (' + QuotedStr('MonthlyPatchData') +
                        ',' + QuotedStr('MonthlyPatchSign') + ')';
                lDBDataSet.DataSet.Close;
                lDBDataSet.SetSQL(lSQL);
                lDBDataSet.Dataset.Open;
                lDBDataSet.Dataset.First;
                while (NOT lDBDataSet.DataSet.Eof) do
                begin
                  LKeyData  := Trim(lDBDataSet.DataSet.FieldByName('KeyValues').AsString);
                  FindKeyValue(LKeyData, 'PatchID', lPatchIDStr);
                  if (lPatchIDs.IndexOf(lPatchIDStr) >= 0) then
                  begin
                    ReplacePatchIDValues(LKeyData, lPatchIDs);
                    lSQL := 'INSERT INTO ChangeParameter '+
                            '(Model,StudyAreaName,SubArea,Scenario,ChangeListID, ParamField, KeyValues,'+
                            ' FieldIndex, Absolut, Change, Filtered) ' +
                            'VALUES (' +
                            QuotedStr(Trim(lDBDataSet.DataSet.FieldByName('Model').AsString)) + ',' +
                            QuotedStr(Trim(lDBDataSet.DataSet.FieldByName('StudyAreaName').AsString)) + ',' +
                            QuotedStr(Trim(lDBDataSet.DataSet.FieldByName('SubArea').AsString)) + ',' +
                            QuotedStr(Trim(lDBDataSet.DataSet.FieldByName('Scenario').AsString)) + ',' +
                            IntToStr(lDBDataSet.DataSet.FieldByName('ChangeListID').AsInteger) + ',' +
                            QuotedStr(Trim(lDBDataSet.DataSet.FieldByName('ParamField').AsString)) + ',' +
                            QuotedStr(LKeyData) + ',' +
                            QuotedStr(Trim(lDBDataSet.DataSet.FieldByName('FieldIndex').AsString)) + ',' +
                            QuotedStr(Trim(lDBDataSet.DataSet.FieldByName('Absolut').AsString)) + ',' +
                            QuotedStr(Trim(lDBDataSet.DataSet.FieldByName('Change').AsString)) + ',' +
                            QuotedStr(Trim(lDBDataSet.DataSet.FieldByName('Filtered').AsString)) + ')';
                    lDBDataSet2.DataSet.Close;
                    lDBDataSet2.SetSQL(lSQL);
                    lDBDataSet2.ExecSQL;
                  end;
                  lDBDataSet.DataSet.Next;
                end;

                // Copy MetaDataItem
                lDBDataSet.DataSet.Close;
                lSQL := 'SELECT * FROM MetaDataItem ' +
                        'WHERE ParamField IN (' + QuotedStr('MonthlyPatchData') +
                        ',' + QuotedStr('MonthlyPatchSign') + ')';
                lDBDataSet.DataSet.Close;
                lDBDataSet.SetSQL(lSQL);
                lDBDataSet.Dataset.Open;
                lDBDataSet.Dataset.First;
                while (NOT lDBDataSet.DataSet.Eof) do
                begin
                  LKeyData  := Trim(lDBDataSet.DataSet.FieldByName('KeyValues').AsString);
                  FindKeyValue(LKeyData, 'PatchID', lPatchIDStr);
                  if (lPatchIDs.IndexOf(lPatchIDStr) >= 0) then
                  begin
                    ReplacePatchIDValues(LKeyData, lPatchIDs);
                    lSQL := 'INSERT INTO MetaDataItem ' +
                            '(MetaDataListID, ParamField, KeyValues, FieldIndex, ' +
                            'DateCreated, CreatedBy, Comment) VALUES (' +
                            IntToStr(lDBDataSet.DataSet.FieldByName('MetaDataListID').AsInteger) + ',' +
                            QuotedStr(Trim(lDBDataSet.DataSet.FieldByName('ParamField').AsString)) + ',' +
                            QuotedStr(LKeyData) + ',' +
                            QuotedStr(Trim(lDBDataSet.DataSet.FieldByName('FieldIndex').AsString)) + ',' +
                            QuotedStr(DateToStr(lDBDataSet.DataSet.FieldByName('DateCreated').AsDateTime)) + ',' +
                            QuotedStr(Trim(lDBDataSet.DataSet.FieldByName('CreatedBy').AsString)) + ',' +
                            QuotedStr(Trim(lDBDataSet.DataSet.FieldByName('Comment').AsString)) + ')';
                    lDBDataSet2.DataSet.Close;
                    lDBDataSet2.SetSQL(lSQL);
                    lDBDataSet2.ExecSQL;
                  end;
                  lDBDataSet.DataSet.Next;
                end;
              end;
            finally
              lDBDataSet.Free;
              lDBDataSet2.Free;
              FreeAndNil(lPatchIDs);
            end;
            Result := TRUE;
          end;
        end;
      finally
        FreeAndNil(LKeyFields);
        FreeAndNil(LKeyValues);
        FreeAndNil(lModelTables);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.CopyStudyTable : boolean;
const OPNAME = 'TDbTableDataManager.CopyStudyTable';
var
  lDataSet : TAbstractModelDataset;
  lSQL     : string;
begin
  Result  := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(lDataSet) then
      begin
        lDataSet.ClearSQL;
        lSQL := 'SELECT * FROM StudyArea WHERE StudyAreaName = ' + QuotedStr(FStudy);
        lDataSet.SetSQL(lSQL);
        lDataSet.SetReadOnly(FALSE);
        lDataset.DataSet.Open;
        lDataSet.DataSet.Edit;

        if (lDataset.DataSet.RecordCount = 0) then
        begin
          Result := TRUE;
          Exit;
        end
        else
        begin
          while (NOT lDataset.DataSet.Eof) do
          begin
            lSQL := 'INSERT INTO StudyArea ' +
                    '(Model, StudyAreaName, StudyDate, Consultant, Client, StudyNumber, ' +
                    'StudyLabel, StudyAreaDescr, ShapeFileName) VALUES (' +
                    QuotedStr(Trim(lDataset.DataSet.FieldByName('Model').AsString)) + ',' +
                    QuotedStr(FStudyFields.FStudyAreaName) + ',' +
                    QuotedStr(DateTimeToStr(FStudyFields.FStudyDate)) + ',' +
                    QuotedStr(FStudyFields.FConsultant) + ',' +
                    QuotedStr(FStudyFields.FClient) + ',' +
                    QuotedStr(FStudyFields.FStudyNumber) + ',' +
                    QuotedStr(FStudyFields.FStudyLabel) + ',' +
                    QuotedStr(FStudyFields.FStudyAreaDescr) + ',' +
                    QuotedStr(Trim(LDataset.DataSet.FieldByName('ShapeFileName').AsString)) + ')';

            FAppModules.Database.ExecSQL(lSQL);
            lDataSet.DataSet.Next;
          end;
          Result := TRUE;
        end;
        lDataSet.DataSet.Close;
      end;
    finally
      FreeAndNil(lDataset);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.CopySubAreaTable : boolean;
const OPNAME = 'TDbTableDataManager.CopySubAreaTable';
var
  lDataSet : TAbstractModelDataset;
  lSQL     : string;
begin
  Result  := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(lDataSet) then
      begin
        lDataSet.ClearSQL;
        lSQL := 'SELECT * FROM StudySubArea WHERE StudyAreaName = ' + QuotedStr(FStudy) +
                ' AND Model = ' + QuotedStr(FModel) +
                ' AND SubArea = ' + QuotedStr(FSubArea);
        lDataSet.SetSQL(lSQL);
        lDataSet.SetReadOnly(FALSE);
        lDataset.DataSet.Open;
        lDataSet.DataSet.Edit;

        if (lDataset.DataSet.RecordCount = 0) then
        begin
          Result := TRUE;
          Exit;
        end
        else
        begin
          while (NOT lDataset.DataSet.Eof) do
          begin
            lSQL := 'INSERT INTO StudySubArea ' +
                    '(Model, StudyAreaName, SubArea, SubAreaLabel, SubAreaDescr, ' +
                    'ShapeFileName) VALUES (';
            case FSelectionLevel of
              malStudy :
                lSQL := lSQL +
                QuotedStr(Trim(lDataset.DataSet.FieldByName('Model').AsString)) + ',' +
                QuotedStr(FStudyFields.FStudyAreaName) + ',' +
                QuotedStr(Trim(LDataset.DataSet.FieldByName('SubArea').AsString)) + ',' +
                QuotedStr(Trim(LDataset.DataSet.FieldByName('SubAreaLabel').AsString)) + ',' +
                QuotedStr(Trim(LDataset.DataSet.FieldByName('SubAreaDescr').AsString)) + ',' +
                QuotedStr(Trim(LDataset.DataSet.FieldByName('ShapeFileName').AsString)) + ')';
              malSubArea :
                lSQL := lSQL +
                QuotedStr(Trim(lDataset.DataSet.FieldByName('Model').AsString)) + ',' +
                QuotedStr(Trim(LDataset.DataSet.FieldByName('StudyAreaName').AsString)) + ',' +
                QuotedStr(FStudyFields.FSubArea) + ',' +
                QuotedStr(FStudyFields.FSubAreaLabel) + ',' +
                QuotedStr(FStudyFields.FSubAreaDescr) + ',' +
                QuotedStr(Trim(LDataset.DataSet.FieldByName('ShapeFileName').AsString)) + ')';
            end;
            FAppModules.Database.ExecSQL(lSQL);
            lDataSet.DataSet.Next;
          end;
          Result := TRUE;
        end;
        lDataSet.DataSet.Close;
      end;
    finally
      FreeAndNil(lDataset);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TDbTableDataManager.CopyScenarioTable : boolean;
const OPNAME = 'TDbTableDataManager.CopyScenarioTable';
var
  lDataSet : TAbstractModelDataset;
  lSQL     : string;
begin
  Result  := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(lDataSet) then
      begin
        lDataSet.ClearSQL;
        lSQL := 'SELECT * FROM StudyScenario WHERE StudyAreaName = ' + QuotedStr(FStudy) +
                ' AND Model = ' + QuotedStr(FModel) +
                ' AND SubArea = ' + QuotedStr(FSubArea);
        if (FSelectionLevel = malScenarion) then
          lSQL := lSQL + ' AND Scenario = ' + QuotedStr(FScenario);
        lDataSet.SetSQL(lSQL);
        lDataSet.SetReadOnly(FALSE);
        lDataset.DataSet.Open;
        lDataSet.DataSet.Edit;

        if (lDataset.DataSet.RecordCount = 0) then
        begin
          Result := TRUE;
          Exit;
        end
        else
        begin
          while (NOT lDataset.DataSet.Eof) do
          begin
            lSQL := 'INSERT INTO StudyScenario ' +
                    '(Model, StudyAreaName, SubArea, Scenario, ScenarioLabel, ' +
                    'ScenarioDescr, DataFilesPrefix, DataFilesPath, FilesLoaded, ' +
                    'CalenderStartMonth, Version, StudyImportDate, LastUpdateDate,DataImported) VALUES (';
            case FSelectionLevel of
              malStudy :
                lSQL := lSQL +
                QuotedStr(Trim(lDataset.DataSet.FieldByName('Model').AsString)) + ',' +
                QuotedStr(FStudyFields.FStudyAreaName) + ',' +
                QuotedStr(Trim(LDataset.DataSet.FieldByName('SubArea').AsString)) + ',' +
                QuotedStr(Trim(LDataset.DataSet.FieldByName('Scenario').AsString)) + ',' +
                QuotedStr(Trim(LDataset.DataSet.FieldByName('ScenarioLabel').AsString)) + ',' +
                QuotedStr(Trim(LDataset.DataSet.FieldByName('ScenarioDescr').AsString)) + ',' +
                QuotedStr(Trim(LDataset.DataSet.FieldByName('DataFilesPrefix').AsString)) + ',' +
                QuotedStr(Trim(LDataset.DataSet.FieldByName('DataFilesPath').AsString)) + ',' +
                QuotedStr(Trim(LDataset.DataSet.FieldByName('FilesLoaded').AsString)) + ',' +
                IntToStr(LDataset.DataSet.FieldByName('CalenderStartMonth').AsInteger) + ',' +
                QuotedStr(Trim(LDataset.DataSet.FieldByName('Version').AsString)) + ',' +
                QuotedStr(DateToStr(LDataset.DataSet.FieldByName('StudyImportDate').AsDateTime)) + ',' +
                QuotedStr(DateToStr(LDataset.DataSet.FieldByName('LastUpdateDate').AsDateTime)) + ','+
                QuotedStr('N') + ')';
              malSubArea :
                lSQL := lSQL +
                QuotedStr(Trim(lDataset.DataSet.FieldByName('Model').AsString)) + ',' +
                QuotedStr(Trim(LDataset.DataSet.FieldByName('StudyAreaName').AsString)) + ',' +
                QuotedStr(FStudyFields.FSubArea) + ',' +
                QuotedStr(Trim(LDataset.DataSet.FieldByName('Scenario').AsString)) + ',' +
                QuotedStr(Trim(LDataset.DataSet.FieldByName('ScenarioLabel').AsString)) + ',' +
                QuotedStr(Trim(LDataset.DataSet.FieldByName('ScenarioDescr').AsString)) + ',' +
                QuotedStr(Trim(LDataset.DataSet.FieldByName('DataFilesPrefix').AsString)) + ',' +
                QuotedStr(Trim(LDataset.DataSet.FieldByName('DataFilesPath').AsString)) + ',' +
                QuotedStr(Trim(LDataset.DataSet.FieldByName('FilesLoaded').AsString)) + ',' +
                IntToStr(LDataset.DataSet.FieldByName('CalenderStartMonth').AsInteger) + ',' +
                QuotedStr(Trim(LDataset.DataSet.FieldByName('Version').AsString)) + ',' +
                QuotedStr(DateToStr(LDataset.DataSet.FieldByName('StudyImportDate').AsDateTime)) + ',' +
                QuotedStr(DateToStr(LDataset.DataSet.FieldByName('LastUpdateDate').AsDateTime)) + ','+
                QuotedStr('N') + ')';
              malScenarion :
                lSQL := lSQL +
                QuotedStr(Trim(lDataset.DataSet.FieldByName('Model').AsString)) + ',' +
                QuotedStr(Trim(LDataset.DataSet.FieldByName('StudyAreaName').AsString)) + ',' +
                QuotedStr(Trim(LDataset.DataSet.FieldByName('SubArea').AsString)) + ',' +
                QuotedStr(FStudyFields.FScenario) + ',' +
                QuotedStr(FStudyFields.FScenarioLabel) + ',' +
                QuotedStr(FStudyFields.FScenarioDescr) + ',' +
                QuotedStr(Trim(LDataset.DataSet.FieldByName('DataFilesPrefix').AsString)) + ',' +
                QuotedStr(Trim(LDataset.DataSet.FieldByName('DataFilesPath').AsString)) + ',' +
                QuotedStr(Trim(LDataset.DataSet.FieldByName('FilesLoaded').AsString)) + ',' +
                IntToStr(LDataset.DataSet.FieldByName('CalenderStartMonth').AsInteger) + ',' +
                QuotedStr(FStudyFields.FVersion) + ',' +
                QuotedStr(DateToStr(LDataset.DataSet.FieldByName('StudyImportDate').AsDateTime)) + ',' +
                QuotedStr(DateToStr(LDataset.DataSet.FieldByName('LastUpdateDate').AsDateTime)) + ','+
                QuotedStr('N') + ')';
            end;
            FAppModules.Database.ExecSQL(lSQL);
            lDataSet.DataSet.Next;
          end;
          Result := TRUE;
        end;
        lDataSet.DataSet.Close;
      end;
    finally
      FreeAndNil(lDataset);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.ProcessCopyChangeTables (ATableProperty: TAbstractDbTableProperty): boolean;
const OPNAME = 'TDbTableDataManager.ProcessCopyChangeTables';
var
  lDataSetA         : TAbstractModelDataSet;
  lDataSetB         : TAbstractModelDataSet;
  lDataSetC         : TAbstractModelDataSet;
  lDataSetX         : TAbstractModelDataSet;
  lKeyValues        : string;
  LSQL              : string;
  lOldID            : integer;
  lNewID            : integer;
  lModelKey         : string;
  LModel            : string;
  LStudyAreaName    : string;
  LSubArea          : string;
  LScenario         : string;
begin
  Result := False;
  try
    if (FSelectionLevel = malScenarion) then
      lModelKey := ' Model=' + QuotedStr(FModel) +
                   ' AND StudyAreaName=' + QuotedStr(FStudy) +
                   ' AND SubArea=' + QuotedStr(FSubArea) +
                   ' AND Scenario=' + QuotedStr(FScenario)
    else
      lModelKey := 'Model=' + QuotedStr(FModel)+
                   'AND StudyAreaName=' + QuotedStr(FStudy)+
                   'AND SubArea=' + QuotedStr(FSubArea);

    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSetA);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSetX);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSetB);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSetC);
    try
      if Assigned(lDataSetA) AND Assigned(lDataSetB) AND Assigned(lDataSetC) then
      begin
        lDataSetA.DataSet.Close;
        lSQL := 'SELECT MAX(ChangeListID) AS NewID FROM ChangeList';
        lDataSetA.SetSQL(lSQL);
        lDataSetA.Dataset.Open;
        lDataSetA.Dataset.First;
        lNewID := 0;
        if (NOT lDataSetA.DataSet.EOF) then
          lNewID := lDataSetA.Dataset.FieldByName('NewID').AsInteger;

        lSQL := 'SELECT * FROM ChangeList ' +
                ' WHERE '+ lModelKey +
                ' ORDER BY ChangeListID';

        lDataSetA.ClearSQL;
        lDataSetA.SetSQL(LSQL);
        lDataSetA.DataSet.Open;

        if (lDataSetA.DataSet.RecordCount = 0) then
        begin
          Result := True;
          Exit;
        end
        else
        begin
          LModel            := FModel;
          LStudyAreaName    := FStudy;
          while (not lDataSetA.DataSet.Eof) do
          begin
            if (FSelectionLevel = malScenarion) then
            begin
              LSubArea          := Trim(lDataSetA.DataSet.FieldByName('SubArea').AsString);
              LScenario         := FStudyFields.FScenario;
            end
            else
            begin
              LSubArea          := FSubArea; //FStudyFields.FSubArea;
              LScenario         := Trim(lDataSetA.DataSet.FieldByName('Scenario').AsString);
            end;

            lOldID := lDataSetA.DataSet.FieldByName('ChangeListID').AsInteger;
            lNewID := lNewID + 1;
            lKeyValues := Trim(lDataSetA.DataSet.FieldByName('ChangeListKey').AsString);
            ReplaceKeyValues(lKeyValues);

            lSQL := 'INSERT INTO ChangeList '+
                    '(Model,StudyAreaName,SubArea,Scenario, ChangeListID, ListName,'+
                    ' DateCreated, CreatedBy, ListDescr,ChangeListKey) ' +
                    'VALUES (' +
                    QuotedStr(LModel) + ',' +
                    QuotedStr(LStudyAreaName) + ',' +
                    QuotedStr(LSubArea) + ',' +
                    QuotedStr(LScenario) + ',' +
                    IntToStr(lNewID) + ',' +
                    QuotedStr(Trim(lDataSetA.DataSet.FieldByName('ListName').AsString)) + ',' +
                    QuotedStr(DateToStr(lDataSetA.DataSet.FieldByName('DateCreated').AsDateTime)) + ',' +
                    QuotedStr(Trim(lDataSetA.DataSet.FieldByName('CreatedBy').AsString)) + ',' +
                    QuotedStr(Trim(lDataSetA.DataSet.FieldByName('ListDescr').AsString)) + ',' +
                    QuotedStr(lKeyValues) + ')';
            lDataSetX.DataSet.Close;
            lDataSetX.SetSQL(lSQL);
            lDataSetX.ExecSQL;

            // Copy ChangeParameter
            lDataSetB.DataSet.Close;
            lSQL := 'SELECT * FROM ChangeParameter WHERE ChangeListID = ' + IntToStr(lOldID);
            lDataSetB.SetSQL(lSQL);
            lDataSetB.Dataset.Open;
            lDataSetB.Dataset.First;
            while (NOT lDataSetB.DataSet.Eof) do
            begin
              lKeyValues := Trim(lDataSetB.DataSet.FieldByName('KeyValues').AsString);
              lSQL := 'INSERT INTO ChangeParameter '+
                      '(Model,StudyAreaName,SubArea,Scenario,ChangeListID, ParamField, KeyValues,'+
                      ' FieldIndex, Absolut, Change,ParamDescr,Filtered) ' +
                      'VALUES (' +
                      QuotedStr(LModel) + ',' +
                      QuotedStr(LStudyAreaName) + ',' +
                      QuotedStr(LSubArea) + ',' +
                      QuotedStr(LScenario) + ',' +
                      IntToStr(lNewID) + ',' +
                      QuotedStr(Trim(lDataSetB.DataSet.FieldByName('ParamField').AsString)) + ',' +
                      QuotedStr(lKeyValues) + ',' +
                      QuotedStr(Trim(lDataSetB.DataSet.FieldByName('FieldIndex').AsString)) + ',' +
                      QuotedStr(Trim(lDataSetB.DataSet.FieldByName('Absolut').AsString)) + ',' +
                      QuotedStr(Trim(lDataSetB.DataSet.FieldByName('Change').AsString)) + ',' +
                      QuotedStr(Trim(lDataSetB.DataSet.FieldByName('ParamDescr').AsString))+ ',' +
                      QuotedStr(Trim(lDataSetB.DataSet.FieldByName('Filtered').AsString)) + ')';
              lDataSetC.DataSet.Close;
              lDataSetC.SetSQL(lSQL);
              lDataSetC.ExecSQL;
              lDataSetB.DataSet.Next;
            end;
            lDataSetA.DataSet.Next;
          end;
          lDataSetA.DataSet.Close;
        end;
        Result := True;
      end;
    finally
      lDataSetA.Free;
      lDataSetB.Free;
      lDataSetC.Free;
      lDataSetX.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDbTableDataManager.ReplaceKeyValues (var AKeyValues : string);
const OPNAME = 'TDbTableDataManager.ReplaceKeyValues';
var
  lPos     : integer;
  lStart   : integer;
  lEnd     : integer;
begin
  try
    if (FSelectionLevel = malStudy) then
    begin
      lPos := Pos('StudyAreaName=', AKeyValues);
      if (lPos > 0) then
      begin
        lStart := PosEx('=', AKeyValues, lPos);
        lEnd   := PosEx(',', AKeyValues, lPos);
        if (lEnd > 0) then
          AKeyValues := Copy(AKeyValues, 1, lStart) +
                        QuotedStr(FStudyFields.FStudyAreaName) +
                        Copy(AKeyValues, lEnd, Length(AKeyValues) - lEnd + 1)
        else
          AKeyValues := Copy(AKeyValues, 1, lStart) +
                          QuotedStr(FStudyFields.FStudyAreaName);
      end
    end;
    if (FSelectionLevel = malSubArea) then
    begin
      lPos := Pos('SubArea=', AKeyValues);
      if (lPos > 0) then
      begin
        lStart := PosEx('=', AKeyValues, lPos);
        lEnd   := PosEx(',', AKeyValues, lPos);
        if (lEnd > 0) then
          AKeyValues := Copy(AKeyValues, 1, lStart) +
                        QuotedStr(FStudyFields.FSubArea) +
                        Copy(AKeyValues, lEnd, Length(AKeyValues) - lEnd + 1)
        else
          AKeyValues := Copy(AKeyValues, 1, lStart) +
                        QuotedStr(FStudyFields.FSubArea);
      end;
    end;
    if (FSelectionLevel = malScenarion) then
    begin
      lPos := Pos('Scenario=', AKeyValues);
      if (lPos > 0) then
      begin
        lStart := PosEx('=', AKeyValues, lPos);
        lEnd   := PosEx(',', AKeyValues, lPos);
        if (lEnd > 0) then
          AKeyValues := Copy(AKeyValues, 1, lStart) +
                        QuotedStr(FStudyFields.FScenario) +
                        Copy(AKeyValues, lEnd, Length(AKeyValues) - lEnd + 1)
        else
          AKeyValues := Copy(AKeyValues, 1, lStart) +
                        QuotedStr(FStudyFields.FScenario);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.ProcessCopyMetaDataTables(ATableProperty: TAbstractDbTableProperty): boolean;
const OPNAME = 'TDbTableDataManager.ProcessCopyMetaDataTables';
var
  LDataSetA         : TAbstractModelDataset;
  LDataSetB         : TAbstractModelDataset;
  LDataSetC         : TAbstractModelDataset;
  LDataSetX         : TAbstractModelDataset;
  LSQL              : string;
  lOldID            : integer;
  lNewID            : integer;
  lKeyValues        : string;
  lModelKey         : string;
begin
  Result := False;
  try
    if (FSelectionLevel = malScenarion) then
      lModelKey := 'Model=' + QuotedStr(FModel) +
                   ',StudyAreaName=' + QuotedStr(FStudy) +
                   ',SubArea=' + QuotedStr(FSubArea) +
                   ',Scenario=' + QuotedStr(FScenario) + '%'
    else
      lModelKey := 'Model=' + QuotedStr(FModel) +
                   ',StudyAreaName=' + QuotedStr(FStudy) +
                   ',SubArea=' + QuotedStr(FSubArea) + '%';
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSetA);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSetB);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSetC);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSetX);
    try
      if Assigned(LDataSetA) AND Assigned(LDataSetB) AND Assigned(LDataSetC) AND Assigned(LDataSetX) then
      begin
        LDataSetA.DataSet.Close;
        lSQL := 'SELECT Max(MetaDataListID) AS NewID FROM MetaDataList';
        LDataSetA.SetSQL(lSQL);
        LDataSetA.Dataset.Open;
        LDataSetA.Dataset.First;
        lNewID := 0;
        if (NOT LDataSetA.DataSet.EOF) then
          lNewID := LDataSetA.Dataset.FieldByName('NewID').AsInteger;

        lSQL := 'SELECT * FROM MetaDataList ' +
                'WHERE MetaDataListKey LIKE ' + QuotedStr(lModelKey);

        LDataSetA.ClearSQL;
        LDataSetA.SetSQL(LSQL);
        LDataSetA.DataSet.Open;

        if (LDataSetA.DataSet.RecordCount = 0) then
        begin
          Result := True;
          Exit;
        end
        else
        begin
          while (not LDataSetA.DataSet.Eof) do
          begin
            lOldID := LDataSetA.DataSet.FieldByName('MetaDataListID').AsInteger;
            lNewID := lNewID + 1;
            lKeyValues := Trim(lDataSetA.DataSet.FieldByName('MetaDataListKey').AsString);
            ReplaceKeyValues(lKeyValues);

            lSQL := 'INSERT INTO MetaDataList '+
                    '(MetaDataListKey, MetaDataListID) VALUES (' +
                    QuotedStr(lKeyValues) + ',' +
                    IntToStr(lNewID) + ')';
            lDataSetX.DataSet.Close;
            lDataSetX.SetSQL(lSQL);
            lDataSetX.ExecSQL;

            // Copy MetaDataItem
            LDataSetB.DataSet.Close;
            lSQL := 'SELECT * FROM MetaDataItem WHERE MetaDataListID = ' + IntToStr(lOldID);
            LDataSetB.SetSQL(lSQL);
            LDataSetB.Dataset.Open;
            LDataSetB.Dataset.First;
            while (NOT LDataSetB.DataSet.Eof) do
            begin
              lKeyValues := Trim(LDataSetB.DataSet.FieldByName('KeyValues').AsString);

              lSQL := 'INSERT INTO MetaDataItem '+
                      '(MetaDataListID, ParamField, KeyValues, FieldIndex, DateCreated, CreatedBy, Comment) ' +
                      'VALUES (' +
                      IntToStr(lNewID) + ',' +
                      QuotedStr(Trim(LDataSetB.DataSet.FieldByName('ParamField').AsString)) + ',' +
                      QuotedStr(lKeyValues) + ',' +
                      QuotedStr(Trim(LDataSetB.DataSet.FieldByName('FieldIndex').AsString)) + ',' +
                      QuotedStr(DateToStr(LDataSetB.DataSet.FieldByName('DateCreated').AsDateTime)) + ',' +
                      QuotedStr(Trim(LDataSetB.DataSet.FieldByName('CreatedBy').AsString)) + ',' +
                      QuotedStr(Trim(LDataSetB.DataSet.FieldByName('Comment').AsString)) + ')';
              LDataSetC.DataSet.Close;
              LDataSetC.SetSQL(lSQL);
              LDataSetC.ExecSQL;
              LDataSetB.DataSet.Next;
            end;
            LDataSetA.DataSet.Next;
          end;
          LDataSetA.DataSet.Close;
        end;
        Result := True;
      end;
    finally
      LDataSetA.Free;
      LDataSetB.Free;
      LDataSetC.Free;
      LDataSetX.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.ActivateCopyStudyDataDialog: boolean;
const OPNAME = 'TDbTableDataManager.ActivateCopyStudyDataDialog';
begin
  Result := False;
  try
    if not Assigned(FStudyCopyForm) then
    begin
      FStudyCopyForm := TfrmStudyCopyForm.Create(nil, FAppModules);
    end;
    FStudyCopyForm.StudyNameHasChanged := False;
    FStudyCopyForm.SubAreHasChanged := False;
    FStudyCopyForm.ScenarioHasChanged := False;
    FStudyCopyForm.SelectionLevel :=  FSelectionLevel;
    FStudyCopyForm.PopulateEditDialog(FStudyFields);
    FStudyCopyForm.ShowModal;

    if (FStudyCopyForm.ModalResult = mrOk) then
    begin
      if FStudyCopyForm.ValidateEditDialog(FStudyFields) then
      begin
        if not ValidateStudyData then
          raise Exception.Create('Study Data already exist.Please enter new study data');
        if not Assigned(FStudyFields) then
           raise Exception.Create('Study fields parameter is not yet assigned.');
        Result := True;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.ValidateStudyData:boolean;
const OPNAME = 'TDbTableDataManager.ValidateStudyData';
var
  lDataSet : TAbstractModelDataset;
  lSQL     : string;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(lDataSet) then
      begin
        lDataSet.ClearSQL;
        case FSelectionLevel of
          malStudy:
            lSQL := 'SELECT * FROM StudyArea ' +
                    ' WHERE Model = ' + QuotedStr(FStudyFields.FModel) +
                    ' AND StudyAreaName = ' + QuotedStr(FStudyFields.FStudyAreaName);
          malSubArea:
            lSQL := 'SELECT * FROM StudySubArea ' +
                    ' WHERE Model = ' + QuotedStr(FStudyFields.FModel) +
                    ' AND StudyAreaName = ' + QuotedStr(FStudyFields.FStudyAreaName)+
                    ' AND SubArea = ' + QuotedStr(FStudyFields.FSubArea);
          malScenarion:
            lSQL := 'SELECT * FROM StudyScenario ' +
                    ' WHERE Model = ' + QuotedStr(FStudyFields.FModel) +
                    ' AND StudyAreaName = ' + QuotedStr(FStudyFields.FStudyAreaName)+
                    ' AND SubArea = ' + QuotedStr(FStudyFields.FSubArea)+
                    ' AND Scenario = ' + QuotedStr(FStudyFields.FScenario);
        end;

        LDataSet.SetSQL(LSQL);
        LDataset.DataSet.Open;
        if (LDataSet.DataSet.IsEmpty) then
          Result := True;
        LDataset.DataSet.Close;
      end;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{******************************************************************************}
{* DELETE                                                                     *}
{******************************************************************************}

function TDbTableDataManager.DeleteStudyData (ASelectionLevel       : TModelActionLevel;
                                              AStudyArea            : TAbstractStudyArea): boolean;
const OPNAME = 'TDbTableDataManager.DeleteStudyData';
begin
  Result := FALSE;
  try
    Self.Initialise;
    FSelectionLevel       := ASelectionLevel;
    FStudyArea            := AStudyArea;


    FStudy        := AStudyArea.StudyAreaCode;
    FModel        := '';
    FSubArea      := '';
    FScenario     := '';

    FProgressDialog.ProgressRichEdit.Clear;
    FProgressDialog.ActionProgressBar.Min := 0;
    FProgressDialog.ActionProgressBar.Max := FAppModules.DBTablePropertyManager.TablesCount + 1;
    FProgressDialog.clbOptions.Items.Clear;
    FProgressDialog.clbOptions.Items.Add('Stop on first error');
    FProgressDialog.clbOptions.Checked[0] := False;// FAppModules.GlobalData.StopOnFirstErr;
    if (FSelectionLevel = malStudy) then
      FProgressDialog.AddExecutionFunctions(ExecDeleteStudy)
    else
    begin
      FModel   := AStudyArea.ModelCode;
      FSubArea := AStudyArea.SubAreaCode;
      if (FSelectionLevel = malScenarion) then
        FScenario := AStudyArea.ScenarioCode;
      FProgressDialog.AddExecutionFunctions(ExecDeleteSubArea);
    end;
    FProgressDialog.Caption := FAppModules.Language.GetString('TDbTableDataManager.strDelete');
    FProgressDialog.Succsessful := False;
    FProgressDialog.ShowModal;
    Result := FProgressDialog.Succsessful;
    FProgressDialog.Hide;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.ExecDeleteStudy (AProgressUpdateFuntion : TProgressUpdateFuntion) : boolean;
const OPNAME = 'TDbTableDataManager.ExecDeleteStudy';
var
  lSQL         : string;
  lResult      : boolean;
  lStop        : boolean;
  lDataSet     : TAbstractModelDataset;
begin
  Result := FALSE;
  try
    if (DeleteStudyTable) then
    begin
      FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
      try
        if Assigned(lDataSet) then
        begin
          lDataSet.DataSet.Close;
          lSQL := ' SELECT Model, SubArea FROM StudySubArea '+
                  ' WHERE StudyAreaName = ' + QuotedStr(FStudy);
          lDataSet.SetSQL(lSQL);
          lDataSet.DataSet.Open;

          while (NOT LDataSet.DataSet.Eof) do
          begin
            FModel   := Trim(lDataSet.DataSet.FieldByName('Model').AsString);
            FSubArea := Trim(lDataSet.DataSet.FieldByName('SubArea').AsString);

            FProgressDialog.ActionProgressBar.Min := 0;
            FProgressDialog.ActionProgressBar.Max := FAppModules.DBTablePropertyManager.TablesCount;

            AProgressUpdateFuntion('.  ', ptNone, lStop );
            AProgressUpdateFuntion('*********************************************', ptNone, lStop );
            AProgressUpdateFuntion(FAppModules.Language.GetString('Message.Deleting') + FStudy + ' - ' + FModel + ' - ' + FSubArea, ptNone, lStop );
            AProgressUpdateFuntion('*********************************************', ptNone, lStop );
            lResult := ExecDeleteSubArea(AProgressUpdateFuntion);
            if (NOT UpdateProgress(lResult)) then Exit;
            lDataSet.DataSet.Next;
          end;
        end;
        Result := TRUE;
      finally
        FreeAndNil(lDataSet);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.ExecDeleteSubArea (AProgressUpdateFuntion : TProgressUpdateFuntion): boolean;
const OPNAME = 'TDbTableDataManager.ExecDeleteSubArea';
begin
  Result := FALSE;
  try
    if (FSelectionLevel = malScenarion) OR
       ((FSelectionLevel <> malScenarion) AND DeleteSubAreaTable) then
    begin
      if (FModel = CRainfall) then
        ExecDeleteRainfallSubArea(AProgressUpdateFuntion)
      //else if (FModel = CHydrology) then
      //  ExecDeleteHydrologySubArea(AProgressUpdateFuntion)
      else if (FModel = CYield) OR (FModel = CPlanning) OR (FModel = CDailyDiversion)  or
      (FModel = CIFRPreProcessor) OR (FModel = CDamSedimentation) OR (FModel = CStomsa)  OR (FModel = CRWH)  or (FModel = CDDTS) then
        ExecDeleteYieldSubArea(AProgressUpdateFuntion);

      if(FSelectionLevel = malModel) then
        DeleteStudyTable;

      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.ExecDeleteYieldSubArea(AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TDbTableDataManager.ExecDeleteYieldSubArea';
var
  lPath        : string;
  lIndex       : integer;
  lTable       : TAbstractDbTableProperty;
  lResult      : boolean;
  lStop        : boolean;
  lModelTables : TObjectList;
begin
  Result := FALSE;
  try
    if (DeleteScenarioTable) then
    begin
      lModelTables := TObjectList.Create(FALSE);
      try
        if (FAppModules.DBTablePropertyManager.GetTablesPerModel(FModel, lModelTables)) then
        begin
          lResult := TRUE;
          FProgressDialog.ActionProgressBar.Min := 0;
          FProgressDialog.ActionProgressBar.Max := LModelTables.Count;
          for lIndex := 0 to lModelTables.Count -1 do
          begin
            lTable := TAbstractDbTableProperty(LModelTables.Items[lIndex]);
            AProgressUpdateFuntion(FAppModules.Language.GetString('Message.ProcessingTable') + lTable.TableName, ptNone, lStop);
            if lStop then Exit;

            case lTable.TableGroup of
              tgModelData :
              begin
                if (UpperCase(lTable.TableName) <> 'STUDYMODEL') AND
                   (UpperCase(lTable.TableName) <> 'STUDYAREA') AND
                   (UpperCase(lTable.TableName) <> 'STUDYSUBAREA') AND
                   (UpperCase(lTable.TableName) <> 'STUDYSCENARIO') then
                begin
                  lResult := ProcessDeleteFromGeneralTable(lTable);
                end;
              end;
              tgOutputData : lResult := ProcessDeleteFromGeneralTable(lTable);
              tgSpecial    : lResult := ProcessDeleteSpecialTable(lTable);
              tgSystem,
              tgBaseData   : lResult := True;
            end;

            if not UpdateProgress(lResult) then Exit;
            if not lResult then
              Result := FALSE;
          end;

          //delete files
          if lResult then
          begin
            lPath := Get_NetworkDiagramsPath;
            if SysUtils.DirectoryExists(lPath) then
            begin
              if (MessageDlg(FAppModules.Language.GetString('Message.NetworkDiagramsFound'),mtConfirmation,[mbYes, mbNo],0) = mrYes) then
              begin
                UUtilities.DeleteDirectory(lPath);
              end;
            end;
          end;
        end;
      finally
        lModelTables.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.ExecDeleteRainfallSubArea (AProgressUpdateFuntion : TProgressUpdateFuntion): boolean;
const OPNAME = 'TDbTableDataManager.ExecDeleteRainfallSubArea';
var
  lTable : TAbstractDbTableProperty;
 lStop: boolean;
begin
  Result  := FALSE;
  try
    case FSelectionLevel of
      malStudy,
      malSubArea :
        Result := DeleteRainfallSubArea(AProgressUpdateFuntion, FStudy, FSubArea);
      malScenarion :
        Result := DeleteRainfallScenario(AProgressUpdateFuntion, FStudy, FSubArea, FStudyArea.ScenarioCode);
    end;
    lTable := FAppModules.DBTablePropertyManager.TablePropertyByName['ChangeGroup'];
    if (lTable <> nil) then
    begin
      AProgressUpdateFuntion( FAppModules.Language.GetString('Message.ProcessingTable')+lTable.TableName, ptNone, lStop);
      if lStop then exit;
       ProcessDeleteSpecialTable(lTable);
    end;
    lTable := FAppModules.DBTablePropertyManager.TablePropertyByName['ChangeGroupElement'];
    if (lTable <> nil) then
    begin
      AProgressUpdateFuntion( FAppModules.Language.GetString('Message.ProcessingTable')+lTable.TableName, ptNone, lStop);
      if lStop then exit;
       ProcessDeleteSpecialTable(lTable);
    end;
    lTable := FAppModules.DBTablePropertyManager.TablePropertyByName['ChangeList'];
    if (lTable <> nil) then
    begin
      AProgressUpdateFuntion( FAppModules.Language.GetString('Message.ProcessingTable')+lTable.TableName, ptNone, lStop);
      if lStop then exit;
       ProcessDeleteSpecialTable(lTable);
    end;
    lTable := FAppModules.DBTablePropertyManager.TablePropertyByName['ChangeParameter'];
    if (lTable <> nil) then
    begin
      AProgressUpdateFuntion( FAppModules.Language.GetString('Message.ProcessingTable')+lTable.TableName, ptNone, lStop);
      if lStop then exit;
       ProcessDeleteSpecialTable(lTable);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.DeleteRainfallSubArea (AProgressUpdateFuntion : TProgressUpdateFuntion;
                                                    AArea                  : string;
                                                    ASubArea               : string) : boolean;
const OPNAME = 'TDbTableDataManager.DeleteRainfallSubArea';
var
  lDataSet     : TAbstractModelDataset;
  lSQL         : string;
  lWhereClause : string;
  lInClause    : string;
  lStop        : boolean;
  lResult      : boolean;
  lPatchIDs    : TStringList;
  lOldID       : integer;
begin
  Result := FALSE;
  try
    if (DeleteScenarioTable) then
    begin
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      lPatchIDs := TStringList.Create;
      try
        lResult := TRUE;
        lWhereClause := ' WHERE StudyAreaName = ' + QuotedStr(AArea) +
                        ' AND SubArea = '         + QuotedStr(ASubArea);
        lInClause := '(SELECT PatchID from RainfallPatchR ' + lWhereClause + ')';

        lSQL := 'SELECT * FROM RainfallPatchR ' + lWhereClause;
        lDataSet.SetSQL(lSQL);
        lDataSet.Dataset.Open;
        lDataSet.Dataset.First;
        while (NOT lDataSet.DataSet.Eof) do
        begin
          lOldID := lDataSet.DataSet.FieldByName('PatchID').AsInteger;
          lPatchIDs.Add(IntToStr(lOldID));
          lDataSet.DataSet.Next;
        end;
        lDataSet.Dataset.Close;

        lStop := FALSE;
        AProgressUpdateFuntion( FAppModules.Language.GetString('Message.ProcessingRainfallMonthlyPatchData'), ptNone, lStop);
        lSQL := 'DELETE * FROM RainfallMonthlyPatchData WHERE PatchID IN ' + lInClause;
        lDataSet.DataSet.Close;
        lDataSet.SetSQL(lSQL);
        lDataSet.ExecSQL;

        AProgressUpdateFuntion( FAppModules.Language.GetString('Message.ProcessingRainfallPatchSource'), ptNone, lStop);
        lSQL := 'DELETE * FROM RainfallPatchSource WHERE PatchID IN ' + lInClause;
        lDataSet.DataSet.Close;
        lDataSet.SetSQL(lSQL);
        lDataSet.ExecSQL;

        AProgressUpdateFuntion( FAppModules.Language.GetString('Message.ProcessingRainfallPatchR'), ptNone, lStop);
        lSQL := 'DELETE * FROM RainfallPatchR ' + lWhereClause;
        lDataSet.DataSet.Close;
        lDataSet.SetSQL(lSQL);
        lDataSet.ExecSQL;

        AProgressUpdateFuntion( FAppModules.Language.GetString('Message.ProcessingRainfallProjectGauges'), ptNone, lStop);
        lSQL := 'DELETE * FROM RainfallProjectGauges ' + lWhereClause;
        lDataSet.DataSet.Close;
        lDataSet.SetSQL(lSQL);
        lDataSet.ExecSQL;

        AProgressUpdateFuntion( FAppModules.Language.GetString('Message.ProcessingRainfallCatchmentSource'), ptNone, lStop);
        lSQL := 'DELETE * FROM RainfallCatchmentSource ' + lWhereClause;
        lDataSet.DataSet.Close;
        lDataSet.SetSQL(lSQL);
        lDataSet.ExecSQL;
        
        AProgressUpdateFuntion( FAppModules.Language.GetString('Message.ProcessingRainfallcatchmentFiledData'), ptNone, lStop);
        lSQL := 'DELETE * FROM RAINFALLCATCHMENTFILEDATA ' + lWhereClause;
        lDataSet.DataSet.Close;
        lDataSet.SetSQL(lSQL);
        lDataSet.ExecSQL;
        Result := lResult;

        AProgressUpdateFuntion( FAppModules.Language.GetString('Message.ProcessingRainfallcatchmentFiledDetail'), ptNone, lStop);
        lSQL := 'DELETE * FROM RAINFALLCATCHMENTFILEDETAIL ' + lWhereClause;
        lDataSet.DataSet.Close;
        lDataSet.SetSQL(lSQL);
        lDataSet.ExecSQL;
        Result := lResult;

        AProgressUpdateFuntion( FAppModules.Language.GetString('Message.ProcessingRainfallCatchment'), ptNone, lStop);
        lSQL := 'DELETE * FROM RainfallCatchment ' + lWhereClause;
        lDataSet.DataSet.Close;
        lDataSet.SetSQL(lSQL);
        lDataSet.ExecSQL;
        Result := lResult;

      finally
        lDataSet.DataSet.Close;
        FreeAndNil(lDataSet);
        FreeAndNil(lPatchIDs);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.DeleteRainfallScenario (AProgressUpdateFuntion : TProgressUpdateFuntion;
                                                     AArea                  : string;
                                                     ASubArea               : string;
                                                     AScenario              : string) : boolean;
const OPNAME = 'TDbTableDataManager.DeleteRainfallScenario';
var
  lDataSet     : TAbstractModelDataset;
  lSubDataSet  : TAbstractModelDataset;
  lSQL         : string;
  lWhereClause : string;
  lStop        : boolean;
begin
  Result := FALSE;
  try
    if (DeleteScenarioTable) then
    begin
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LSubDataSet);
      try
        lWhereClause := ' WHERE Model = '       + QuotedStr(FModel) +
                        ' AND StudyAreaName = ' + QuotedStr(AArea) +
                        ' AND SubArea = '       + QuotedStr(ASubArea) +
                        ' AND Scenario = '      + QuotedStr(AScenario);
        lStop := FALSE;

        if (UpperCase(AScenario) <> 'PROJECT GAUGES') then
        begin
          AProgressUpdateFuntion( FAppModules.Language.GetString('Message.ProcessingRainfallCatchmentSource'), ptNone, lStop);
          lSQL := 'DELETE * FROM RainfallCatchmentSource ' + lWhereClause;
          lDataSet.DataSet.Close;
          lDataSet.SetSQL(lSQL);
          lDataSet.ExecSQL;

          AProgressUpdateFuntion( FAppModules.Language.GetString('Message.ProcessingRainfallcatchmentFiledData'), ptNone, lStop);
          lSQL := 'DELETE * FROM RAINFALLCATCHMENTFILEDATA ' + lWhereClause;
          lDataSet.DataSet.Close;
          lDataSet.SetSQL(lSQL);
          lDataSet.ExecSQL;

          AProgressUpdateFuntion( FAppModules.Language.GetString('Message.ProcessingRainfallcatchmentFiledDetail'), ptNone, lStop);
          lSQL := 'DELETE * FROM RAINFALLCATCHMENTFILEDETAIL ' + lWhereClause;
          lDataSet.DataSet.Close;
          lDataSet.SetSQL(lSQL);
          lDataSet.ExecSQL;

          AProgressUpdateFuntion( FAppModules.Language.GetString('Message.ProcessingRainfallCatchment'), ptNone, lStop);
          lSQL := 'DELETE * FROM RainfallCatchment ' + lWhereClause;
          lDataSet.DataSet.Close;
          lDataSet.SetSQL(lSQL);
          lDataSet.ExecSQL;

        end;
        Result := TRUE;
      finally
        FreeAndNil(lDataSet);
        FreeAndNil(lSubDataSet);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.DeleteStudyTable : boolean;
const OPNAME = 'TDbTableDataManager.DeleteStudyTable';
var
  lDataSet : TAbstractModelDataset;
  lSQL     : string;
begin
  Result  := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(lDataSet) then
      begin
        lDataSet.ClearSQL;
        lSQL := 'DELETE FROM StudyArea WHERE StudyAreaName = ' + QuotedStr(FStudy);
        if (FSelectionLevel in [malModel,malSubArea,malScenarion]) then
           lSQL := lSQL + ' AND Model = ' + QuotedStr(FModel);
        lDataSet.SetSQL(lSQL);
        lDataSet.ExecSQL;
        Result := TRUE;
      end;
    finally
      FreeAndNil(lDataset);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.DeleteSubAreaTable : boolean;
const OPNAME = 'TDbTableDataManager.DeleteSubAreaTable';
var
  lDataSet : TAbstractModelDataset;
  lSQL     : string;
begin
  Result  := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(lDataSet) then
      begin
        lDataSet.ClearSQL;
        lSQL := 'DELETE FROM StudySubArea WHERE StudyAreaName = ' + QuotedStr(FStudy);
        if (FSelectionLevel in [malModel,malSubArea,malScenarion]) then
           lSQL := lSQL + ' AND Model = ' + QuotedStr(FModel);
        if (FSelectionLevel in [malSubArea,malScenarion]) then
           lSQL := lSQL + ' AND SubArea = ' + QuotedStr(FSubArea);
        lDataSet.SetSQL(lSQL);
        lDataSet.ExecSQL;
        Result := TRUE;
      end;
    finally
      FreeAndNil(lDataset);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.DeleteScenarioTable : boolean;
const OPNAME = 'TDbTableDataManager.DeleteScenarioTable';
var
  lDataSet : TAbstractModelDataset;
  lSQL     : string;
begin
  Result  := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(lDataSet) then
      begin
        lDataSet.ClearSQL;
        lSQL := 'DELETE FROM StudyScenario WHERE StudyAreaName = ' + QuotedStr(FStudy);
        if (FSelectionLevel in [malModel,malSubArea,malScenarion]) then
           lSQL := lSQL + ' AND Model = ' + QuotedStr(FModel);
        if (FSelectionLevel in [malSubArea,malScenarion]) then
           lSQL := lSQL + ' AND SubArea = ' + QuotedStr(FSubArea);
        if (FSelectionLevel in [malScenarion]) then
           lSQL := lSQL + ' AND Scenario = ' + QuotedStr(FScenario);
        lDataSet.SetSQL(lSQL);
        lDataSet.ExecSQL;
        Result := TRUE;
      end;
    finally
      FreeAndNil(lDataset);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.ProcessDeleteChangeTables (ATableProperty: TAbstractDbTableProperty): boolean;
const OPNAME = 'TDbTableDataManager.ProcessDeleteChangeTables';
var
  lDataSetA  : TAbstractModelDataSet;
  lSubSQL    : string;
  lSQL       : string;
  lModelKey  : string;
begin
  Result := False;
  try
    if (FSelectionLevel = malScenarion) then
      lModelKey := 'Model=' + QuotedStr(FModel) +
                   ',StudyAreaName=' + QuotedStr(FStudy) +
                   ',SubArea=' + QuotedStr(FSubArea) +
                   ',Scenario=' + QuotedStr(FScenario) + '%'
    else
      lModelKey := 'Model=' + QuotedStr(FModel) +
                   ',StudyAreaName=' + QuotedStr(FStudy) +
                   ',SubArea=' + QuotedStr(FSubArea) + '%';

    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSetA);
    try
      if Assigned(lDataSetA) then
      begin
        lSubSQL := 'SELECT ChangeListID FROM ChangeList ' +
                   'WHERE ChangeListKey LIKE ' + QuotedStr(lModelKey);

        lSQL := 'DELETE FROM ChangeParameter ' +
                'WHERE ChangeListID IN (' + lSubSQL + ')';
        lDataSetA.ClearSQL;
        lDataSetA.SetSQL(LSQL);
        lDataSetA.ExecSQL;
        lDataSetA.DataSet.Close;

        lSQL := 'DELETE FROM ChangeGroupElement ' +
                'WHERE IsElementGroup = ' + QuotedStr('N') +
                ' AND ElementID IN (' + lSubSQL + ')';
        lDataSetA.ClearSQL;
        lDataSetA.SetSQL(LSQL);
        lDataSetA.ExecSQL;
        lDataSetA.DataSet.Close;

        lSQL := 'DELETE FROM ChangeList ' +
                'WHERE ChangeListID IN (' + lSubSQL + ')';
        lDataSetA.ClearSQL;
        lDataSetA.SetSQL(LSQL);
        lDataSetA.ExecSQL;
        lDataSetA.DataSet.Close;

        Result := TRUE;
      end;
    finally
      lDataSetA.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.ProcessDeleteMetaDataTables (ATableProperty : TAbstractDbTableProperty): boolean;
const OPNAME = 'TDbTableDataManager.ProcessDeleteMetaDataTables';
var
  lDataSetA  : TAbstractModelDataSet;
  lSubSQL    : string;
  lSQL       : string;
  lModelKey  : string;
begin
  Result := False;
  try
    if (FSelectionLevel = malScenarion) then
      lModelKey := 'Model=' + QuotedStr(FModel) +
                   ',StudyAreaName=' + QuotedStr(FStudy) +
                   ',SubArea=' + QuotedStr(FSubArea) +
                   ',Scenario=' + QuotedStr(FScenario) + '%'
    else
      lModelKey := 'Model=' + QuotedStr(FModel) +
                   ',StudyAreaName=' + QuotedStr(FStudy) +
                   ',SubArea=' + QuotedStr(FSubArea) + '%';

    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSetA);
    try
      if Assigned(lDataSetA) then
      begin
        lSubSQL := 'SELECT MetaDataListID FROM MetaDataList ' +
                   'WHERE MetaDataListKey LIKE ' + QuotedStr(lModelKey);

        lSQL := 'DELETE FROM MetaDataItem ' +
                'WHERE MetaDataListID IN (' + lSubSQL + ')';
        lDataSetA.ClearSQL;
        lDataSetA.SetSQL(LSQL);
        lDataSetA.ExecSQL;
        lDataSetA.DataSet.Close;

        lSQL := 'DELETE FROM MetaDataList ' +
                'WHERE MetaDataListID IN (' + lSubSQL + ')';
        lDataSetA.ClearSQL;
        lDataSetA.SetSQL(LSQL);
        lDataSetA.ExecSQL;
        lDataSetA.DataSet.Close;

        Result := TRUE;
      end;
    finally
      lDataSetA.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


{******************************************************************************}
{ Yield                                                                       *}
{******************************************************************************}

function TDbTableDataManager.ExecExportYieldSubArea (AProgressUpdateFuntion : TProgressUpdateFuntion): boolean;
const OPNAME = 'TDbTableDataManager.ExecExportYieldSubArea';
var
  lResult      : boolean;
  lStop        : boolean;
  lIndex       : integer;
  lTable       : TAbstractDbTableProperty;
  lModelTables : TObjectList;
begin
  Result := FALSE;
  try
    lModelTables := TObjectList.Create(False);
    try
      if (FAppModules.DBTablePropertyManager.GetTablesPerModel(FModel, lModelTables)) then
      begin
        FProgressDialog.ActionProgressBar.Min := 0;
        FProgressDialog.ActionProgressBar.Max := lModelTables.Count + 2;
        for lIndex := 0 to lModelTables.Count - 1 do
        begin
          lResult := True;
          lTable := TAbstractDbTableProperty(lModelTables.Items[lIndex]);
          AProgressUpdateFuntion(FAppModules.Language.GetString('Message.ProcessingTable') + lTable.TableName, ptNone, lStop);
          if lStop then Exit;
          case lTable.TableGroup of
            tgModelData,
            tgSystem     :
              lResult := ProcessExportGeneralTable(lTable);
            tgOutputData :
              begin
                if FProgressDialog.clbOptions.Checked[2] then
                  lResult := ProcessExportGeneralTable(lTable);
              end;
            tgSpecial :
              begin
                if ((UpperCase(lTable.TableName)) = 'CHANGELIST') then
                  lResult := ProcessExportGeneralTable(lTable);//ProcessExportYieldChangeTables;
                if ((UpperCase(lTable.TableName)) = 'CHANGEPARAMETER') then
                  lResult := ProcessExportGeneralTable(lTable);//ProcessExportYieldChangeTables;
                if ((UpperCase(lTable.TableName)) = 'METADATALIST') then
                  lResult := ProcessExportYieldMetaDataTables;
                //if ((UpperCase(lTable.TableName)) = 'HYDROLOGYFILEDATA') then
                //  lResult := ProcessExportHydrologyFileDataTable(lTable);
                if ((UpperCase(lTable.TableName)) = 'STUDYDOCUMENTS') then
                  lResult := ProcessExportGeneralTable(lTable);
                if ((UpperCase(lTable.TableName)) = 'STUDYSCENARIODOCUMENTS') then
                  lResult := ProcessExportGeneralTable(lTable);
              end;
          end;
          if (NOT UpdateProgress(lResult)) then Exit;
        end;
        lResult := TRUE;
        AProgressUpdateFuntion(FAppModules.Language.GetString('Message.CreatingScriptListFile'), ptNone, lStop);
        lResult := lResult AND CreateScriptListFile;
        AProgressUpdateFuntion(FAppModules.Language.GetString('Message.CreatingDocumentsListFile'), ptNone, lStop);
        lResult := lResult AND CreateDocsListFile;
        if not UpdateProgress(lResult) then Exit;
        AProgressUpdateFuntion(FAppModules.Language.GetString('Message.CreatingZipFile'), ptNone, lStop);
        lResult := lResult AND CreateSubAreaZip;
        if not UpdateProgress(lResult) then Exit;
        Result := lResult;
      end;
    finally
      lModelTables.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.ProcessExportYieldChangeTables : boolean;
const OPNAME = 'TDbTableDataManager.ProcessExportYieldChangeTables';
var
  lClientDataSet   : TClientDataSet;
  lDataSetProvider : TDataSetProvider;
  lDataSet         : TAbstractModelDataset;
  lSQL             : string;
  lSubSQL          : string;
  lFileName        : string;
  lModelKey        : string;
  lListIDs         : TStringList;
  lListIDStr       : string;
begin
  Result := FALSE;
  try
    if (FSelectionLevel = malScenarion) then
      lModelKey := 'Model=' + QuotedStr(FModel) +
                   ',StudyAreaName=' + QuotedStr(FStudy) +
                   ',SubArea=' + QuotedStr(FSubArea) +
                   ',Scenario=' + QuotedStr(FScenario) + '%'
    else
      lModelKey := 'Model=' + QuotedStr(FModel) +
                   ',StudyAreaName=' + QuotedStr(FStudy) +
                   ',SubArea=' + QuotedStr(FSubArea) + '%';
    lListIDs  := TStringList.Create;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(lDataSet) then
      begin
        lSubSQL := 'SELECT ChangeListID FROM ChangeList ' +
                   'WHERE ChangeListKey LIKE ' + QuotedStr(lModelKey);
        lSQL := 'SELECT * FROM ChangeParameter WHERE ChangeListID IN ( ' + lSubSQL + ' ) ';
        lDataSet.SetSQL(lSQL);
        lDataset.DataSet.Open;
        { Find all changelists that has change parameters }
        lListIDs.Clear;
        while (NOT lDataSet.DataSet.Eof) do
        begin
          lListIDStr := Trim(lDataSet.DataSet.FieldByName('ChangeListID').AsString);
          if (lListIDs.IndexOf(lListIDStr) < 0) then
            lListIDs.Add(lListIDStr);
          lDataSet.DataSet.Next;
        end;
        lDataSet.DataSet.First;
        if (NOT ((lDataset.DataSet.Eof) AND (lDataset.DataSet.Bof))) then
        begin
          lDataSetProvider := TDataSetProvider.Create(nil);
          lClientDataSet   := TClientDataSet.Create(nil);
          try
            lDataSetProvider.DataSet := lDataset.DataSet;
            lClientDataSet.ReadOnly  := TRUE;
            lClientDataSet.SetProvider(LDataSetProvider);
            lClientDataSet.StoreDefs := TRUE;
            lClientDataSet.Active    := TRUE;
            lFileName := Trim(FTempDirectory) + 'LD_ChangeParameter.xml';
            lClientDataSet.SaveToFile(lFileName, dfXML);
            FScriptList.Add(ExtractFileName(lFileName));
          finally
            lDataset.DataSet.Active := False;
            lClientDataSet.Active := False;
            lClientDataSet.Free;
            lDataSetProvider.Free;
          end;
        end;
        LDataset.DataSet.Close;

        if (lListIDs.Count > 0) then
        begin
          lListIDStr := '(' + lListIDs.CommaText + ')';
          lSQL := 'SELECT * FROM ChangeList ' +
                  'WHERE ChangeListID IN ' + lListIDStr;
          lDataSet.SetSQL(lSQL);
          lDataset.DataSet.Open;
          if (NOT ((lDataset.DataSet.Eof) AND (lDataset.DataSet.Bof))) then
          begin
            lDataSetProvider := TDataSetProvider.Create(nil);
            lClientDataSet   := TClientDataSet.Create(nil);
            try
              lDataSetProvider.DataSet := lDataset.DataSet;
              lClientDataSet.ReadOnly  := TRUE;
              lClientDataSet.SetProvider(LDataSetProvider);
              lClientDataSet.StoreDefs := TRUE;
              lClientDataSet.Active    := TRUE;
              lFileName := Trim(FTempDirectory) + 'LD_ChangeList.xml';
              lClientDataSet.SaveToFile(lFileName, dfXML);
              FScriptList.Add(ExtractFileName(lFileName));
            finally
              lDataset.DataSet.Active := False;
              lClientDataSet.Active := False;
              lClientDataSet.Free;
              lDataSetProvider.Free;
            end;
          end;
          lDataset.DataSet.Close;
        end;
        Result := TRUE;
      end;
    finally
      FreeAndNil(lDataSet);
      FreeAndNil(lListIDs);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.ProcessExportYieldMetaDataTables : boolean;
const OPNAME = 'TDbTableDataManager.ProcessExportYieldMetaDataTables';
var
  lClientDataSet   : TClientDataSet;
  lDataSetProvider : TDataSetProvider;
  lDataSet         : TAbstractModelDataset;
  lSQL             : string;
  lSubSQL          : string;
  lFileName        : string;
  lModelKey        : string;
begin
  Result := FALSE;
  try
    if (FSelectionLevel = malScenarion) then
      lModelKey := 'Model=' + QuotedStr(FModel) +
                   ',StudyAreaName=' + QuotedStr(FStudy) +
                   ',SubArea=' + QuotedStr(FSubArea) +
                   ',Scenario=' + QuotedStr(FScenario) + '%'
    else
      lModelKey := 'Model=' + QuotedStr(FModel) +
                   ',StudyAreaName=' + QuotedStr(FStudy) +
                   ',SubArea=' + QuotedStr(FSubArea) + '%';
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(lDataSet) then
      begin
        lSQL := 'SELECT * FROM MetaDataList ' +
                'WHERE MetaDataListKey LIKE ' + QuotedStr(lModelKey);
        lDataSet.SetSQL(lSQL);
        lDataset.DataSet.Open;
        if (NOT ((lDataset.DataSet.Eof) AND (lDataset.DataSet.Bof))) then
        begin
          lDataSetProvider := TDataSetProvider.Create(nil);
          lClientDataSet   := TClientDataSet.Create(nil);
          try
            lDataSetProvider.DataSet := lDataset.DataSet;
            lClientDataSet.ReadOnly  := TRUE;
            lClientDataSet.SetProvider(LDataSetProvider);
            lClientDataSet.StoreDefs := TRUE;
            lClientDataSet.Active    := TRUE;
            lFileName := Trim(FTempDirectory) + 'LD_MetaDataList.xml';
            lClientDataSet.SaveToFile(lFileName, dfXML);
            FScriptList.Add(ExtractFileName(lFileName));
          finally
            lDataset.DataSet.Active := False;
            lClientDataSet.Active := False;
            lClientDataSet.Free;
            lDataSetProvider.Free;
          end;
        end;
        LDataset.DataSet.Close;

        lSubSQL := 'SELECT MetaDataListID FROM MetaDataList ' +
                   'WHERE MetaDataListKey LIKE ' + QuotedStr(lModelKey);
        lSQL := 'SELECT * FROM MetaDataItem WHERE MetaDataListID IN ( ' + lSubSQL + ' ) ';
        lDataSet.SetSQL(lSQL);
        lDataset.DataSet.Open;
        if (NOT ((lDataset.DataSet.Eof) AND (lDataset.DataSet.Bof))) then
        begin
          lDataSetProvider := TDataSetProvider.Create(nil);
          lClientDataSet   := TClientDataSet.Create(nil);
          try
            lDataSetProvider.DataSet := lDataset.DataSet;
            lClientDataSet.ReadOnly  := TRUE;
            lClientDataSet.SetProvider(LDataSetProvider);
            lClientDataSet.StoreDefs := TRUE;
            lClientDataSet.Active    := TRUE;
            lFileName := Trim(FTempDirectory) + 'LD_MetaDataItem.xml';
            lClientDataSet.SaveToFile(lFileName, dfXML);
            FScriptList.Add(ExtractFileName(lFileName));
          finally
            lDataset.DataSet.Active := False;
            lClientDataSet.Active := False;
            lClientDataSet.Free;
            lDataSetProvider.Free;
          end;
        end;
        LDataset.DataSet.Close;
        Result := TRUE;
      end;
    finally
      FreeAndNil(lDataSet);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{******************************************************************************}
{ Rainfall                                                                    *}
{******************************************************************************}

function TDbTableDataManager.ExecExportRainfallSubArea (AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TDbTableDataManager.ExecExportRainfallSubArea';
var
  lIndex       : integer;
  lTable       : TAbstractDbTableProperty;
  lResult      : boolean;
  lStop        : boolean;
  lModelTables : TObjectList;
  lFilter      : string;
  lPatchFilter : string;
  lGaugeFilter : string;
 begin
  Result := FALSE;
  try
    lModelTables := TObjectList.Create(FALSE);
    try
      if (FAppModules.DBTablePropertyManager.GetTablesPerModel(FModel, lModelTables)) then
      begin
        FProgressDialog.ActionProgressBar.Min := 0;
        FProgressDialog.ActionProgressBar.Max := lModelTables.Count+2;

        lFilter      := 'StudyAreaName = ' + QuotedStr(FStudy) +
                        ' AND SubArea = ' + QuotedStr(FSubArea);
        lPatchFilter := 'PatchID IN (SELECT PatchID FROM RainfallPatchR WHERE ' + lFilter + ')';
        lGaugeFilter := 'StationID IN (SELECT StationID FROM RainfallProjectGauges WHERE ' + lFilter + ')';
        lIndex := 0;
        while (lIndex < lModelTables.Count) do
        begin
          lTable := TAbstractDbTableProperty(lModelTables.Items[lIndex]);
          if (lTable.TableGroup = tgSystem) then
            lModelTables.Delete(lIndex)
          else
          begin
            if (lTable.TableName = 'RainfallPatchR') then
            begin
              lTable.TableFilter := lFilter;
              GetClassRAndPatchRFileNames(FSubArea);
            end
            else if (lTable.TableName = 'RainfallPatchSource') then
              lTable.TableFilter := lPatchFilter
            else if (lTable.TableName = 'RainfallMonthlyPatchData') then
              lTable.TableFilter := lPatchFilter
            else if (lTable.TableName = 'RainfallRAWSplits') then
              lTable.TableFilter := lGaugeFilter
            else if (lTable.TableName = 'RainfallUserStations') then
              lTable.TableFilter := lGaugeFilter
            else if (lTable.TableName = 'RainfallUserMonthlyData') then
              lTable.TableFilter := lGaugeFilter
            else
              lTable.TableFilter := '';
            lIndex := lIndex + 1;
          end;
        end;
        FProgressDialog.ActionProgressBar.Max := lModelTables.Count+2;
        for lIndex := 0 to lModelTables.Count -1 do
        begin
          lTable := TAbstractDbTableProperty(lModelTables.Items[lIndex]);
          lResult := True;

          if (lTable.TableGroup = tgBaseData) AND (NOT FProgressDialog.clbOptions.Checked[1]) then
          begin
            AProgressUpdateFuntion( FAppModules.Language.GetString('Message.ProcessingTable') + lTable.TableName, ptNone, lStop);
            if (NOT UpdateProgress(True)) then Exit;
            Continue;
          end;

          AProgressUpdateFuntion( FAppModules.Language.GetString('Message.ProcessingTable')+ lTable.TableName,ptNone,lStop);
          if lStop then Exit;
          if (lTable.TableGroup = tgSpecial) then
          begin
            if ((UpperCase(lTable.TableName)) = 'CHANGELIST') then
              lResult := ProcessExportGeneralTable(lTable);//ProcessExportRainfallChangeTables;
            if ((UpperCase(lTable.TableName)) = 'CHANGEPARAMETER') then
              lResult := ProcessExportGeneralTable(lTable);//ProcessExportRainfallChangeTables;
            if ((UpperCase(lTable.TableName)) = 'METADATALIST') then
              lResult := ProcessExportRainfallMetaDataTables;
            if ((UpperCase(lTable.TableName)) = 'STUDYDOCUMENTS') then
              lResult := ProcessExportGeneralTable(lTable);
            if ((UpperCase(lTable.TableName)) = 'STUDYSCENARIODOCUMENTS') then
              lResult := ProcessExportGeneralTable(lTable);
          end
          else
            lResult := ProcessExportGeneralTable(lTable);
          if not UpdateProgress(lResult) then Exit;
        end;
        lResult := TRUE;
        AProgressUpdateFuntion( FAppModules.Language.GetString('Message.CreatingScriptListFile'),ptNone,lStop);
        lResult := lResult and CreateScriptListFile;
        AProgressUpdateFuntion( FAppModules.Language.GetString('Message.CreatingDocumentsListFile'),ptNone,lStop);
        lResult := lResult and CreateDocsListFile;
        AProgressUpdateFuntion( FAppModules.Language.GetString('Message.CreatingRListFiles'),ptNone,lStop);
        lResult := lResult and CreateClassRAndPatchRListFile(SubAreaFileName(malSubArea));
        if not UpdateProgress(lResult) then Exit;
        AProgressUpdateFuntion( FAppModules.Language.GetString('Message.CreatingZipFile'),ptNone,lStop);
        lResult := lResult and CreateSubAreaZip;
        if not UpdateProgress(lResult) then Exit;
        Result := lResult;
      end;
    finally
      lModelTables.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.GetClassRAndPatchRFileNames ( ASubArea : string ) : string;
const OPNAME = 'TDbTableDataManager.GetClassRAndPatchRFileNames';
var
  LDataSet         : TAbstractModelDataset;
  LFilePathDataSet : TAbstractModelDataset;
  LSQL             : string;
  LPath            : string;
  lTempStr         : string;
  function GetFilePath : string;
  const OPNAME = 'UDbTableDataManager.GetFilePath';
  begin
    Result := '';
    FAppModules.Database.CreateDataset(integer(dtExecSQL),LFilePathDataSet);
    if Assigned(LFilePathDataSet) then
    begin
      LFilePathDataSet.DataSet.Close;
      LFilePathDataSet.ClearSQL;
      LSQL := ' Select * from StudyScenario ' +
              ' where Model = '+ QuotedStr(FModel) +
              ' and StudyAreaName = '+ QuotedStr(FStudy) +
              ' and SubArea = '+ QuotedStr(ASubArea);
      LFilePathDataSet.SetSQL(LSQL);
      LFilePathDataSet.DataSet.Open;
      Result := Trim(LFilePathDataSet.DataSet.FieldByName('DataFilesPath').AsString);
    end;
  end;
begin
  Result := '';
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL),LDataSet);
    if Assigned(LDataSet) then
    begin
      LDataSet.DataSet.Close;
      LDataSet.ClearSQL;
      LSQL := ' Select * from RainfallPatchR where ' +
              ' StudyAreaName = ' + QuotedStr(FStudy) +
              ' and SubArea = '+ QuotedStr(ASubArea);
      LDataSet.SetSQL(LSQL);
      LDataSet.DataSet.Open;
      LPath := GetFilePath;

      if LPath = '' then
        LPath := DeploymentPath + 'WRCDATA\';
      while not LDataSet.DataSet.Eof do
      begin
        if not LDataSet.Dataset.FieldByName('ClassROutputFileName').IsNull then
        begin
          lTempStr := LPath + Trim(LDataSet.Dataset.FieldByName('ClassROutputFileName').AsString);
          if (FClassRPatchRList.IndexOf(lTempStr) < 0) then
            FClassRPatchRList.Add(lTempStr);
        end;
        if not LDataSet.Dataset.FieldByName('PatchRPrintedFileName').IsNull then
        begin
          lTempStr := LPath + Trim(LDataSet.Dataset.FieldByName('PatchRPrintedFileName').AsString);
          if (FClassRPatchRList.IndexOf(lTempStr) < 0) then
            FClassRPatchRList.Add(lTempStr);
        end;
        if not LDataSet.Dataset.FieldByName('PatchRPlottedFileName').IsNull then
        begin
          lTempStr := LPath + Trim(LDataSet.Dataset.FieldByName('PatchRPlottedFileName').AsString);
          if (FClassRPatchRList.IndexOf(lTempStr) < 0) then
            FClassRPatchRList.Add(lTempStr);
        end;
        LDataSet.DataSet.Next;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDbTableDataManager.ProcessExportRainfallChangeTables : boolean;
const OPNAME = 'TDbTableDataManager.ProcessExportRainfallChangeTables';
var
  lClientDataSet   : TClientDataSet;
  lDataSetProvider : TDataSetProvider;
  lDataSet         : TAbstractModelDataset;
  lSQL             : string;
  lSubSQL          : string;
  lFileName        : string;
  lModelKey        : string;
  lPatchIDs        : TStringList;
  lParam           : string;
  lKeyClause       : string;
  lIndex           : integer;
  lTempStr         : string;
begin
  Result := FALSE;
  try
    lModelKey := 'Model=' + QuotedStr(FModel);
    lPatchIDs := TStringList.Create;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(lDataSet) then
      begin
        lSQL := 'SELECT PatchID FROM RainfallPatchR ' +
                'WHERE StudyAreaName = ' + QuotedStr(FStudy) +
                ' AND SubArea = ' + QuotedStr(FSubArea);
        lDataSet.SetSQL(lSQL);
        lDataset.DataSet.Open;
        lPatchIDs.Clear;
        { Find all PatchIDs that belong to subarea}
        while (NOT lDataSet.DataSet.Eof) do
        begin
          lParam := Trim(lDataSet.DataSet.FieldByName('PatchID').AsString);
          lPatchIDs.Add(lParam);
          lDataSet.DataSet.Next;
        end;
        if (lPatchIDs.Count > 0) then
        begin
          lKeyClause := '';
          for lIndex := 0 to lPatchIDs.Count - 1 do
          begin
            lTempStr := 'PatchID=' + lPatchIDs.Strings[lIndex] + '%';
            if (lIndex = 0) then
              lKeyClause := ' (KeyValues LIKE ' + QuotedStr(lTempSTr) + ')'
            else
              lKeyClause := lKeyClause + ' OR (KeyValues LIKE ' + QuotedStr(lTempSTr) + ')';
          end;
        end
        else
          lKeyClause := 'TRUE';

        lSQL := 'SELECT * FROM ChangeList ' +
                'WHERE ChangeListKey = ' + QuotedStr(lModelKey);
        lDataSet.SetSQL(lSQL);
        lDataset.DataSet.Open;
        if (NOT ((lDataset.DataSet.Eof) AND (lDataset.DataSet.Bof))) then
        begin
          lDataSetProvider := TDataSetProvider.Create(nil);
          lClientDataSet   := TClientDataSet.Create(nil);
          try
            lDataSetProvider.DataSet := lDataset.DataSet;
            lClientDataSet.ReadOnly  := TRUE;
            lClientDataSet.SetProvider(LDataSetProvider);
            lClientDataSet.StoreDefs := TRUE;
            lClientDataSet.Active    := TRUE;
            lFileName := Trim(FTempDirectory) + 'LD_ChangeList.xml';
            lClientDataSet.SaveToFile(lFileName, dfXML);
            FScriptList.Add(ExtractFileName(lFileName));
          finally
            lDataset.DataSet.Active := False;
            lClientDataSet.Active := False;
            lClientDataSet.Free;
            lDataSetProvider.Free;
          end;
        end;
        LDataset.DataSet.Close;

        lSubSQL := 'SELECT ChangeListID FROM ChangeList ' +
                   'WHERE ChangeListKey = ' + QuotedStr(lModelKey);
        lSQL := 'SELECT * FROM ChangeParameter ' +
                'WHERE ChangeListID IN ( ' + lSubSQL + ' ) ' +
                ' AND (((ParamField <> ' + QuotedStr('MonthlyPatchData') + ') AND ' +
                '       (ParamField <> ' + QuotedStr('MonthlyPatchSign') + ')) OR ' +
                '      (' + lKeyClause + ')) ';
        lDataSet.SetSQL(lSQL);
        lDataset.DataSet.Open;
        if (NOT ((lDataset.DataSet.Eof) AND (lDataset.DataSet.Bof))) then
        begin
          lDataSetProvider := TDataSetProvider.Create(nil);
          lClientDataSet   := TClientDataSet.Create(nil);
          try
            lDataSetProvider.DataSet := lDataset.DataSet;
            lClientDataSet.ReadOnly  := TRUE;
            lClientDataSet.SetProvider(LDataSetProvider);
            lClientDataSet.StoreDefs := TRUE;
            lClientDataSet.Active    := TRUE;
            lFileName := Trim(FTempDirectory) + 'LD_ChangeParameter.xml';
            lClientDataSet.SaveToFile(lFileName, dfXML);
            FScriptList.Add(ExtractFileName(lFileName));
          finally
            lDataset.DataSet.Active := False;
            lClientDataSet.Active := False;
            lClientDataSet.Free;
            lDataSetProvider.Free;
          end;
        end;
        LDataset.DataSet.Close;
        Result := TRUE;
      end;
    finally
      FreeAndNil(lDataSet);
      FreeAndNil(lPatchIDs);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.ProcessExportRainfallMetaDataTables : boolean;
const OPNAME = 'TDbTableDataManager.ProcessExportRainfallMetaDataTables';
var
  lClientDataSet   : TClientDataSet;
  lDataSetProvider : TDataSetProvider;
  lDataSet         : TAbstractModelDataset;
  lSQL             : string;
  lSubSQL          : string;
  lFileName        : string;
  lModelKey        : string;
  lPatchIDs        : TStringList;
  lParam           : string;
  lKeyClause       : string;
  lIndex           : integer;
  lTempStr         : string;
begin
  Result := FALSE;
  try
    lModelKey := 'Model=' + QuotedStr(FModel);
    lPatchIDs := TStringList.Create;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(lDataSet) then
      begin
        lSQL := 'SELECT PatchID FROM RainfallPatchR ' +
                'WHERE StudyAreaName = ' + QuotedStr(FStudy) +
                ' AND SubArea = ' + QuotedStr(FSubArea);
        lDataSet.SetSQL(lSQL);
        lDataset.DataSet.Open;
        lPatchIDs.Clear;
        { Find all PatchIDs that belong to subarea}
        while (NOT lDataSet.DataSet.Eof) do
        begin
          lParam := Trim(lDataSet.DataSet.FieldByName('PatchID').AsString);
          lPatchIDs.Add(lParam);
          lDataSet.DataSet.Next;
        end;
        if (lPatchIDs.Count > 0) then
        begin
          lKeyClause := '';
          for lIndex := 0 to lPatchIDs.Count - 1 do
          begin
            lTempStr := 'PatchID=' + lPatchIDs.Strings[lIndex] + '%';
            if (lIndex = 0) then
              lKeyClause := ' (KeyValues LIKE ' + QuotedStr(lTempSTr) + ')'
            else
              lKeyClause := lKeyClause + ' OR (KeyValues LIKE ' + QuotedStr(lTempSTr) + ')';
          end;
        end
        else
          lKeyClause := 'TRUE';

        lDataset.DataSet.Close;
        lSQL := 'SELECT * FROM MetaDataList ' +
                'WHERE MetaDataListKey = ' + QuotedStr(lModelKey);
        lDataSet.SetSQL(lSQL);
        lDataset.DataSet.Open;
        if (NOT ((lDataset.DataSet.Eof) AND (lDataset.DataSet.Bof))) then
        begin
          lDataSetProvider := TDataSetProvider.Create(nil);
          lClientDataSet   := TClientDataSet.Create(nil);
          try
            lDataSetProvider.DataSet := lDataset.DataSet;
            lClientDataSet.ReadOnly  := TRUE;
            lClientDataSet.SetProvider(LDataSetProvider);
            lClientDataSet.StoreDefs := TRUE;
            lClientDataSet.Active    := TRUE;
            lFileName := Trim(FTempDirectory) + 'LD_MetaDataList.xml';
            lClientDataSet.SaveToFile(lFileName, dfXML);
            FScriptList.Add(ExtractFileName(lFileName));
          finally
            lDataset.DataSet.Active := False;
            lClientDataSet.Active := False;
            lClientDataSet.Free;
            lDataSetProvider.Free;
          end;
        end;

        LDataset.DataSet.Close;
        lSubSQL := 'SELECT MetaDataListID FROM MetaDataList ' +
                   'WHERE MetaDataListKey = ' + QuotedStr(lModelKey);
        lSQL := 'SELECT * FROM MetaDataItem ' +
                'WHERE MetaDataListID IN ( ' + lSubSQL + ' ) ' +
                ' AND (((ParamField <> ' + QuotedStr('MonthlyPatchData') + ') AND ' +
                '       (ParamField <> ' + QuotedStr('MonthlyPatchSign') + ')) OR ' +
                '      (' + lKeyClause + ')) ';
        lDataSet.SetSQL(lSQL);
        lDataset.DataSet.Open;
        if (NOT ((lDataset.DataSet.Eof) AND (lDataset.DataSet.Bof))) then
        begin
          lDataSetProvider := TDataSetProvider.Create(nil);
          lClientDataSet   := TClientDataSet.Create(nil);
          try
            lDataSetProvider.DataSet := lDataset.DataSet;
            lClientDataSet.ReadOnly  := TRUE;
            lClientDataSet.SetProvider(LDataSetProvider);
            lClientDataSet.StoreDefs := TRUE;
            lClientDataSet.Active    := TRUE;
            lFileName := Trim(FTempDirectory) + 'LD_MetaDataItem.xml';
            lClientDataSet.SaveToFile(lFileName, dfXML);
            FScriptList.Add(ExtractFileName(lFileName));
          finally
            lDataset.DataSet.Active := False;
            lClientDataSet.Active := False;
            lClientDataSet.Free;
            lDataSetProvider.Free;
          end;
        end;
        LDataset.DataSet.Close;
        Result := TRUE;
      end;
    finally
      FreeAndNil(lDataSet);
      FreeAndNil(lPatchIDs)
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.ImportStudyData : boolean;
const OPNAME = 'TDbTableDataManager.ImportStudyData';
var
  LDir         : string;
  LFileIndex   : integer;
  LZipFileName : string;
  LIndex       : integer;
  LZipList     : TStringList;
  lTempStr     : string;
begin
  Result := FALSE;
  try
    Self.Initialise;
    if(SelectZipFile) and (ShowIssueListSplashScreen) then
    begin
      if (FZipFileNameList.Count > 0) then
      begin
        LZipList := TStringList.Create;
        LZipList.Sorted := True;
        try

          for LIndex := 0 to FZipFileNameList.Count - 1 do
          begin
            LZipFileName := ExtractFileName(FZipFileNameList[LIndex]);
            if (Pos(CRainfall,LZipFileName) > 0) or
               (Pos(CPlanning,LZipFileName) > 0) or
               (Pos(CDailyDiversion,LZipFileName) > 0) or
               (Pos(CIFRPreProcessor,LZipFileName) > 0) or
               (Pos(CDamSedimentation,LZipFileName) > 0) or
               (Pos(CStomsa,LZipFileName) > 0) or
               (Pos(CHydrology,LZipFileName) > 0) or
               (Pos(CYield,LZipFileName) > 0) or
               (Pos(CRWH,LZipFileName) > 0) or
                (Pos(CDDTS,LZipFileName) > 0) then
              LZipList.Add(LZipFileName);
          end;
          FProgressDialog.ProgressRichEdit.Clear;
          FProgressDialog.ActionProgressBar.Min := 0;
          FProgressDialog.clbOptions.Items.Clear;
          FProgressDialog.clbOptions.Items.Add('Stop on first error');
          FProgressDialog.clbOptions.Items.Add('Report errors');
          FProgressDialog.clbOptions.Checked[0] := False;// FAppModules.GlobalData.StopOnFirstErr;
          for LFileIndex := 0 to LZipList.Count - 1 do
          begin
            FZipFileName := LZipList[LFileIndex ];
            if (Pos(CRainfall, FZipFileName) > 0) then
            begin
              lTempStr := FAppModules.Language.GetString('Message.IncludeRainfallBaseData');
              if (FProgressDialog.clbOptions.Items.IndexOf(lTempStr) < 0) then
                FProgressDialog.clbOptions.Items.Add(lTempStr);
            end
            else
            if (Pos(CYield, FZipFileName) > 0) OR
               (Pos(CDailyDiversion,LZipFileName) > 0) or
               //(Pos(CHydrology,LZipFileName) > 0) or
               (Pos(CIFRPreProcessor,LZipFileName) > 0) or
               (Pos(CDamSedimentation,LZipFileName) > 0) or

               (Pos(CStomsa,LZipFileName) > 0) or
               (Pos(CPlanning, FZipFileName) > 0) or
               (Pos(CRWH, FZipFileName) > 0) or
               (Pos(CDDTS, FZipFileName) > 0) then
            begin
              lTempStr := FAppModules.Language.GetString('Message.IncludeOutputData');
              if (FProgressDialog.clbOptions.Items.IndexOf(lTempStr) < 0) then
                FProgressDialog.clbOptions.Items.Add(lTempStr);
            end;
          end;
          if FImportAll then
          begin
            lTempStr := FAppModules.Language.GetString('Message.IncludeRainfallBaseData');
            if (FProgressDialog.clbOptions.Items.IndexOf(lTempStr) < 0) then
              FProgressDialog.clbOptions.Items.Add(lTempStr);

            lTempStr := FAppModules.Language.GetString('Message.IncludeOutputData');
            if (FProgressDialog.clbOptions.Items.IndexOf(lTempStr) < 0) then
              FProgressDialog.clbOptions.Items.Add(lTempStr);
          end;

          FProgressDialog.AddExecutionFunctions(ExecImportStudy);
          FProgressDialog.Caption := FAppModules.Language.GetString('TDbTableDataManager.strImport');
          FProgressDialog.Succsessful := False;
          FProgressDialog.ShowModal;
          Result := FProgressDialog.Succsessful;
          FProgressDialog.Hide;

          LDir := IncludeTrailingPathDelimiter(FDirectory);
          ChDir(ExtractFilePath(ApplicationExeName));
          if (SysUtils.DirectoryExists(LDir)) then
            DeleteDirectory(LDir);

        finally
          FreeAndNil ( LZipList );
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.SelectZipFile: boolean;
const OPNAME = 'TDbTableDataManager.SelectZipFile';
var
  LSelectZipFileDialog : TSelectZipFileDialog;
begin
  Result := False;
  try
    try
      LSelectZipFileDialog := TSelectZipFileDialog.CreateWithoutDFM(nil, FAppModules);
      LSelectZipFileDialog.InitialDir := FAppModules.ViewIni.ReadString(ClassName,'ImportDirectory','');
      LSelectZipFileDialog.Initialise;
      FZipFileNameList.Clear;
      if FImportAll then
        LSelectZipFileDialog.ImportAll := True;
      LSelectZipFileDialog.ShowModal;
      if (LSelectZipFileDialog.ModalResult = mrOk) then
      begin
        FZipFileNameList.CommaText := LSelectZipFileDialog.ImportZips;
        Result := (FZipFileNameList.Count > 0);
        FAppModules.ViewIni.WriteString
          (ClassName, 'ImportDirectory', ExtractFilePath(LSelectZipFileDialog.InitialDir));
      end;
    finally
      FreeAndNil(LSelectZipFileDialog);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.ExtractAllStudyData(AProgressUpdateFuntion : TProgressUpdateFuntion) : boolean;
const OPNAME = 'TDbTableDataManager.ExtractAllStudyData';
var
  LSearchSubRec,
  LSearchRec: TSearchRec;
  LMoreFiles: boolean;
  LMoreSubZips : boolean;
  LStop : boolean;
  LTempDir,
  LSearchSubzipsStr,
  LSearchStr: string;
begin
  Result := False;
  try
    if FZipFileNameList.Count > 1 then
    begin
      AProgressUpdateFuntion(FAppModules.Language.GetString('Message.IncompatibleZipFile'), ptNone, LStop );
      Exit;
    end;
    FZipFileName := FZipFileNameList.Strings[0];
    LTempDir := FTempDirectory +'SUB\';
    SysUtils.ForceDirectories(LTempDir);
    DeleteMultipleFiles(FTempDirectory +'*.*' );
    DeleteMultipleFiles(LTempDir +'*.*' );
    AProgressUpdateFuntion(FAppModules.Language.GetString('Message.ExtractingZipFile') + ExtractFileName(FZipFileName), ptNone, LStop );
    FZipFileNameList.Delete(0);
    if UnZipFiles then
    begin
      LSearchStr := IncludeTrailingPathDelimiter(FTempDirectory)+ '*.*';
      LMoreFiles := (FindFirst(LSearchStr, faAnyFile, LSearchRec) = 0);
      while LMoreFiles do
      begin
        if (LSearchRec.Name[1] <> '.') then
        begin
          if (UpperCase(Copy(LSearchRec.Name, Pos('.', LSearchRec.Name), 4)) = '.ZIP') then
          begin
            FTempDirectory := GetTempDir+'WRMF\';
            FZipFileName := FTempDirectory + LSearchRec.Name;
            FTempDirectory := LTempDir;

            if UnZipFiles then
            begin
              AProgressUpdateFuntion(FAppModules.Language.GetString('Message.ExtractingZipFile') + ExtractFileName(LSearchRec.Name), ptNone, LStop );
              LSearchSubzipsStr := IncludeTrailingPathDelimiter(FTempDirectory)+ '*.*';
              LMoreSubZips := (FindFirst(LSearchSubzipsStr, faAnyFile, LSearchSubRec) = 0);
              while  LMoreSubZips do
              begin
                if (LSearchSubRec.Name[1] <> '.') then
                  if (UpperCase(Copy(LSearchSubRec.Name, Pos('.', LSearchSubRec.Name), 4)) = '.ZIP') then
                    if FZipFileNameList.IndexOf(LTempDir + LSearchSubRec.Name) < 0 then
                      FZipFileNameList.Add(LTempDir + LSearchSubRec.Name);
                LMoreSubZips := (FindNext(LSearchSubRec)= 0 );
              end;
              SysUtils.FindClose(LSearchSubRec);
            end;
            if LStop then Exit;
          end;
        end;
        LMoreFiles := (FindNext(LSearchRec)= 0 );
      end;
      SysUtils.FindClose(LSearchRec);
      Result := (FZipFileNameList.Count > 1);
    end;
    FTempDirectory := GetTempDir+'WRMF\';
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDbTableDataManager.ExecImportStudy(AProgressUpdateFuntion : TProgressUpdateFuntion) : boolean;
const OPNAME = 'TDbTableDataManager.ExecImportStudy';
var
  lRainfallZipFound  : boolean;
  lYieldZipFound     : boolean;
  lFileIndex         : integer;
  lStop              : boolean;
begin
  Result := False;
  try
    lRainfallZipFound  := FALSE;
    lYieldZipFound     := FALSE;
    lFileIndex := 0;
    if FImportAll then
      if not ExtractAllStudyData(AProgressUpdateFuntion) then
        Exit;
    while (lFileIndex < FZipFileNameList.Count) do
    begin
      FZipFileName := FZipFileNameList[LFileIndex];
      if (Pos(CRainfall, FZipFileName) > 0) then
        lRainfallZipFound := True
      else if (Pos(CYield, FZipFileName) > 0) OR (Pos(CDailyDiversion, FZipFileName) > 0) or
         (Pos(CStomsa, FZipFileName) > 0) or (Pos(CIFRPreProcessor, FZipFileName) > 0) or (Pos(CDamSedimentation, FZipFileName) > 0) or
         (Pos(CPlanning, FZipFileName) > 0) or (Pos(CRWH, FZipFileName) > 0) or (Pos(CDDTS, FZipFileName) > 0) then
        lYieldZipFound := True;
      lFileIndex := lFileIndex + 1;
    end;

    if (lRainfallZipFound) then
    begin
      FIncludeRainfallBaseData := FProgressDialog.clbOptions.Checked[2];
      if (lYieldZipFound) then
        FIncludeOutputData := FProgressDialog.clbOptions.Checked[3];
    end
    else
    begin
      if (lYieldZipFound) then
        FIncludeOutputData := FProgressDialog.clbOptions.Checked[2];
    end;

    for LFileIndex := 0 to FZipFileNameList.Count - 1 do
    begin
      FZipFileName := FZipFileNameList[LFileIndex];
      AProgressUpdateFuntion('.  ', ptNone, lStop );
      AProgressUpdateFuntion('*********************************************', ptNone, lStop );
      AProgressUpdateFuntion(FAppModules.Language.GetString('Message.ExtractingZipFile') + ExtractFileName(FZipFileName), ptNone, LStop );
      if LStop then
      Exit;
      if (not CopySelectedFileToTempDir(FZipFileName))  then
      begin
        AProgressUpdateFuntion(FAppModules.Language.GetString('Message.ErrorWhileExtracting') + ExtractFileName(FZipFileName), ptNone, LStop);
        Exit;
      end;
      AProgressUpdateFuntion(FAppModules.Language.GetString('Message.PreProcessingZipFile') + ExtractFileName(FZipFileName), ptNone, LStop );
      AProgressUpdateFuntion('*********************************************', ptNone, lStop );
      if LStop then
        Exit;
      if (Pos(CRainfall, FZipFileName) > 0) then
      begin
        ExecImportRainfallSubArea(AProgressUpdateFuntion);
      end
      //else if (Pos(CHydrology, FZipFileName) > 0) then
      //begin
      //  ExecImportHydrologySubArea(AProgressUpdateFuntion);
      //end
      else
      begin
        ExecImportYieldSubArea(AProgressUpdateFuntion);
      end
    end;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDbTableDataManager.ExecImportYieldSubArea(AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TDbTableDataManager.ExecImportYieldSubArea';
var
  LIndex              : integer;
  LFound              : boolean;
  LTableName          : string;
  LFileName           : string;
  LTempFilesContainer : TStringList;
  LFilesContainer     : TStringList;
  LModelTables        : TObjectList;
  LTableProperty      : TAbstractDbTableProperty;
  LResult             : boolean;
  LStop               : boolean;
begin
  Result := False;
  try
    if (UnZipFiles) then
    begin
      LFilesContainer     := TStringList.Create;
      LTempFilesContainer := TStringList.Create;
      try
        LFound := False;
        if SearchFiles(FTempDirectory + CMainScriptPrefix + '*', LTempFilesContainer) then
        begin

          for LIndex := 0 to LTempFilesContainer.Count -1 do
          begin
            LFileName := ExtractFileName(LTempFilesContainer[LIndex]);
            if (UpperCase(Copy(LFileName, 1, 5)) = CMainScriptPrefix) then
            begin
              LFound := True;
              break;
            end;
          end;

          if not LFound then
            raise Exception.Create('Main Script File ' + LFileName + ' not found!')
          else
          begin
            LFileName := LTempFilesContainer[LIndex];
            LTempFilesContainer.LoadFromFile(LFileName);
            LModelTables := TObjectList.Create(False);
            try

              FProgressDialog.ActionProgressBar.Max := LTempFilesContainer.Count;
              AProgressUpdateFuntion(FAppModules.Language.GetString('Message.UpgradingFileData'), ptNone, LStop);
              if LStop then Exit;

              ExecVersionUpgrade(AProgressUpdateFuntion, LTempFilesContainer);
              AProgressUpdateFuntion( FAppModules.Language.GetString('Message.PreProcessingFileData'),ptNone,LStop);
              if LStop then Exit;


              //First line is the table version
              for LIndex := 1 to LTempFilesContainer.Count -1 do
              begin
                LTableName     := TableNameFromFileName(LTempFilesContainer[LIndex]);
                if (LTableName = 'MetaDataGroup') then
                  LTableName := 'MetaDataList';
                if (LTableName = 'MetaData') then
                  LTableName := 'MetaDataItem';
                LTableProperty := FAppModules.DBTablePropertyManager.TablePropertyByName[LTableName];
                if (LTableProperty = nil) then
                begin
                  AProgressUpdateFuntion(FAppModules.Language.GetString('Message.Table') + LTableName +
                   FAppModules.Language.GetString('Message.DoesNotHavePSroperties'), ptError, LStop);
                  if LStop then Exit;
                end
                else
                begin
                  LFilesContainer.Add(LTempFilesContainer[LIndex]);
                  LModelTables.Add(LTableProperty);
                end;
              end;

              FProgressDialog.ActionProgressBar.Max := LModelTables.Count + 1;
              for LIndex := 0 to LModelTables.Count -1 do
              begin
                LTableProperty := TAbstractDbTableProperty(LModelTables[LIndex]);
                AProgressUpdateFuntion(FAppModules.Language.GetString('Message.ProcessingTable') + LTableProperty.TableName, ptNone, LStop);
                if LStop then Exit;

                case LTableProperty.TableGroup of
                  tgModelData,
                  tgOutputData :
                  begin
                    if (UpperCase(LTableProperty.TableName) = 'STUDYMODEL') or
                       (UpperCase(LTableProperty.TableName) = 'TSCCHART') or
                       (UpperCase(LTableProperty.TableName) = 'TSCCHARTSERIES') or
                       (UpperCase(LTableProperty.TableName) = 'TSCSERIES') or
                       (UpperCase(LTableProperty.TableName) = 'TSCVIEW') or
                       (UpperCase(LTableProperty.TableName) = 'TSCVIEWCHART') or
                       (UpperCase(LTableProperty.TableName) = 'TSCVIEWSCENARIO') then
                      LResult := True
                    else
                      LResult := ProcessImportTableData(LFilesContainer[LIndex], LTableProperty,AProgressUpdateFuntion);
                  end;
                  tgSpecial:
                  begin
                    if((UpperCase(LTableProperty.TableName)) = 'HYDROLOGYFILEDATA') or
                       ((UpperCase(LTableProperty.TableName)) = 'STUDYDOCUMENTS') or
                       ((UpperCase(LTableProperty.TableName)) = 'STUDYSCENARIODOCUMENTS') then
                      LResult := ProcessImportTableData(LFilesContainer[LIndex], LTableProperty,AProgressUpdateFuntion)
                    else
                      LResult := ProcessImportSpecialTable(LFilesContainer, LTableProperty, AProgressUpdateFuntion);
                  end;
                  else
                  begin
                    LResult := TRUE;
                  end;
                end;
                if not UpdateProgress(LResult) then Exit;
              end;
              Result := TRUE;
            finally
              LModelTables.Free;
            end;

          end;
        end;
      finally
        LFilesContainer.Free;
        LTempFilesContainer.Free;
      end;
      ImportDiagramsAndDocuments;
      DeleteFile(PChar(FZipFileName));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.ExecImportRainfallSubArea (AProgressUpdateFuntion : TProgressUpdateFuntion): boolean;
const OPNAME = 'TDbTableDataManager.ExecImportRainfallSubArea';
var
  LIndex              : integer;
  LFound              : boolean;
  LTableName          : string;
  LFileName           : string;
  LTempFilesContainer : TStringList;
  LFilesContainer     : TStringList;
  LModelTables        : TObjectList;
  LTableProperty      : TAbstractDbTableProperty;
  LResult             : boolean;
  LStop               : boolean;
begin
  Result := False;
  try
    if (UnZipFiles) then
    begin
      LFilesContainer     := TStringList.Create;
      LTempFilesContainer := TStringList.Create;
      try
        LFound := False;
        if SearchFiles(FTempDirectory + CMainScriptPrefix + '*',LTempFilesContainer) then
        begin
          for LIndex := 0 to LTempFilesContainer.Count -1 do
          begin
            LFileName := ExtractFileName(LTempFilesContainer[LIndex]);
            if (UpperCase(Copy(LFileName, 1, 5)) = CMainScriptPrefix) then
            begin
              LFound := True;
              break;
            end;
          end;

          if not LFound then
            raise Exception.Create('Main Script File ' + LFileName + ' not found!')
          else
          begin
            LFileName := LTempFilesContainer[LIndex];
            LTempFilesContainer.LoadFromFile(LFileName);
            LModelTables := TObjectList.Create(False);
            try
              //First line is the table version
              for LIndex := 1 to LTempFilesContainer.Count -1 do
              begin
                LTableName      := TableNameFromFileName(LTempFilesContainer[LIndex]);
                if (LTableName = 'MetaDataGroup') then
                  LTableName := 'MetaDataList';
                if (LTableName = 'MetaData') then
                  LTableName := 'MetaDataItem';
                LTableProperty := FAppModules.DBTablePropertyManager.TablePropertyByName[LTableName];
                if (LTableProperty = nil) then
                begin
                  AProgressUpdateFuntion(FAppModules.Language.GetString('Message.Table') + LTableName +
                   FAppModules.Language.GetString('Message.DoesNotHavePSroperties'), ptError, LStop);
                  if LStop then Exit;
                end
                else
                begin
                  LFilesContainer.Add(LTempFilesContainer[LIndex]);
                  LModelTables.Add(LTableProperty);
                end;
              end;

              FProgressDialog.ActionProgressBar.Max := LModelTables.Count + 1;

              AProgressUpdateFuntion(FAppModules.Language.GetString('Message.UpgradingFileData'),ptNone,LStop);
              if LStop then Exit;

              ExecVersionUpgrade(AProgressUpdateFuntion, LTempFilesContainer);

              AProgressUpdateFuntion( FAppModules.Language.GetString('Message.PreProcessingFileData'),ptNone,LStop);
              if LStop then Exit;

              PreProcessImportRainfallTable(LFilesContainer,LModelTables,AProgressUpdateFuntion);
              for LIndex := 0 to LModelTables.Count -1 do
              begin
                LTableProperty := TAbstractDbTableProperty(LModelTables.Items[LIndex]);
                if (LTableProperty.TableGroup = tgBaseData) and (not FIncludeRainfallBaseData) then
                begin
                  AProgressUpdateFuntion( FAppModules.Language.GetString('Message.ProcessingTable')
                   + TAbstractDbTableProperty(LModelTables[LIndex]).TableName,ptNone,LStop);
                  if not UpdateProgress(True) then Exit;
                  Continue;
                end;

                AProgressUpdateFuntion( FAppModules.Language.GetString('Message.ProcessingTable') + TAbstractDbTableProperty(LModelTables[LIndex]).TableName,ptNone,LStop);
                if LStop then Exit;
                if ((LTableProperty.TableGroup = tgSpecial) AND
                    (UpperCase(LTableProperty.TableName) <> 'HYDROLOGYFILEDATA') AND
                    (UpperCase(LTableProperty.TableName) <> 'STUDYDOCUMENTS') AND
                    (UpperCase(LTableProperty.TableName) <> 'STUDYSCENARIODOCUMENTS')) then
                  LResult := ProcessImportSpecialTable(LFilesContainer, LTableProperty, AProgressUpdateFuntion)
                else
                  LResult := ProcessImportTableData(LFilesContainer[LIndex], LTableProperty,AProgressUpdateFuntion);
               if not UpdateProgress(LResult) then Exit;
              end;
              Result := TRUE;
            finally
              LModelTables.Free;
            end;
          end;
        end;
      finally
        LFilesContainer.Free;
        LTempFilesContainer.Free;
      end;
      ImportDiagramsAndDocuments;
      ImportClassRPatchRFiles;
      DeleteFile(PChar(FZipFileName));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TDbTableDataManager.ExecVersionUpgrade (AProgressUpdateFuntion : TProgressUpdateFuntion;
                                                 AFileNames             : TStringList) : boolean;
const OPNAME = 'TDbTableDataManager.ExecVersionUpgrade';
var
  lVersion : string;
  lIndex   : integer;
  lResult  : boolean;
begin
  Result := FALSE;
  try
    lResult  := TRUE;
    lVersion := AFileNames[0];
    lIndex   := Pos('=', lVersion);
    lVersion := Copy(lVersion, lIndex+1, Length(lVersion) - lIndex);
    if (lVersion = '2.10.0') then
      lResult := UpgradeToVersion_2_10_1(FTempDirectory, AProgressUpdateFuntion, AFileNames, lVersion);
    if (lResult AND (lVersion = '2.10.1')) then
      lResult := UpgradeToVersion_2_11_0(FTempDirectory, AProgressUpdateFuntion, AFileNames, lVersion);
    if (lResult AND (lVersion  < '2.16.0')) then
      lResult := UpgradeToVersion_2_16_0(FTempDirectory, AProgressUpdateFuntion, AFileNames, lVersion);
    if (lResult AND (lVersion < '2.18.0')) then
      lResult := UpgradeToVersion_2_18_0(FAppModules,FTempDirectory, AProgressUpdateFuntion, AFileNames, lVersion);
    if (lResult AND (lVersion < '2.19.0')) then
      lResult := UpgradeToVersion_2_19_0(FAppModules,FTempDirectory, AProgressUpdateFuntion, AFileNames, lVersion);
    if (lResult AND (lVersion < '2.23.0')) then
      lResult := UpgradeToVersion_2_23_0(FAppModules,FTempDirectory, AProgressUpdateFuntion, AFileNames, lVersion);
    if (lResult AND (lVersion < '3.4.1')) then
      lResult := UpgradeToVersion_3_4_1(FAppModules,FTempDirectory, AProgressUpdateFuntion, AFileNames, lVersion);
    if (lResult AND (lVersion < '3.7.1')) then
      lResult := UpgradeToVersion_3_7_1(FAppModules,FTempDirectory, AProgressUpdateFuntion, AFileNames, lVersion);
    if (lResult AND (lVersion < '4.0.0')) then
      lResult := UpgradeToVersion_4_0_0(FAppModules,FTempDirectory, AProgressUpdateFuntion, AFileNames, lVersion);

    Result := lResult;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDbTableDataManager.PreProcessImportRainfallTable (AFileNamesList         : TStrings;
                                                            ATablePropertyList     : TObjectList;
                                                            AProgressUpdateFuntion : TProgressUpdateFuntion): boolean;
const OPNAME = 'TDbTableDataManager.PreProcessImportRainfallTable';
var
  lDBDataSet             : TAbstractModelDataset;
  lIndex                 : integer;
  lTableProperty         : TAbstractDbTableProperty;
  lSQL                   : string;
  lModelIdx              : integer;
  lAreaIdx               : integer;
  lSubAreaIdx            : integer;
  lGaugesIdx             : integer;
  lScenarioIdx           : integer;
  lCatchIdx              : integer;
  lCatchSrcIdx           : integer;
  lPatchSrcIdx           : integer;
  lPatchIdx              : integer;
  lPatchDataIdx          : integer;
  lChangeDataIdx         : integer;
  lMetaDataIdx           : integer;
  lUserStationIdx        : integer;
  lUserDataIdx           : integer;
  lSplitIdx              : integer;
  lXMLDatasetModel       : TClientDataSet;
  lXMLDatasetArea        : TClientDataSet;
  lXMLDatasetSubArea     : TClientDataSet;
  lXMLDatasetGauges      : TClientDataSet;
  lXMLDatasetScenario    : TClientDataSet;
  lXMLDatasetCatch       : TClientDataSet;
  lXMLDatasetCatchSrc    : TClientDataSet;

  lXMLDatasetPatch       : TClientDataSet;
  lXMLDatasetPatchSrc    : TClientDataSet;
  lXMLDatasetPatchData   : TClientDataSet;
  lXMLDatasetChangeData  : TClientDataSet;
  lXMLDatasetMetaData    : TClientDataSet;
  lXMLDatasetUserStation : TClientDataSet;
  lXMLDatasetUserData    : TClientDataSet;
  lXMLDatasetSplit       : TClientDataSet;
  lFileNameModel         : string;
  lFileNameArea          : string;
  lFileNameSubArea       :  string;
  lFileNameScenario      : string;
  lFileNameGauges        : string;
  lFileNameCatch         : string;
  lFileNameCatchSrc      : string;
  lFileNamePatch         : string;
  lFileNamePatchSrc      : string;
  lFileNamePatchData     : string;
  lFileNameChangeData    : string;
  lFileNameMetaData      : string;
  lFileNameUserStation   : string;
  lFileNameUserData      : string;
  lFileNameSplit         : string;
  lModel                 : string;
  lArea                  : string;
  lSubArea               : string;
  lOldStationID          : integer;
  lNewStationID          : integer;
  lOldPatchID            : integer;
  lNewPatchID            : integer;
  lOldIDText             : string;
  lNewIDText             : string;
  lParamField            : string;
  lKeyValues             : string;
  lPatchIDs              : TStringList;
  lUserStationIDs        : TStringList;
  lOldScenarios          : TStringList;
  lNewScenarios          : TStringList;
  lListIndex             : integer;
  lStartYear             : integer;
  lEndYear               : integer;
  lSource                : string;
  lStationNumber         : string;
  lStop                  : boolean;
begin
  Result := False;
  try
    if Assigned(AFileNamesList) AND Assigned(ATablePropertyList) then
    begin
      lXMLDatasetModel       := TClientDataSet.Create(nil);
      lXMLDatasetArea        := TClientDataSet.Create(nil);
      lXMLDatasetSubArea     := TClientDataSet.Create(nil);
      lXMLDatasetGauges      := TClientDataSet.Create(nil);
      lXMLDatasetScenario    := TClientDataSet.Create(nil);
      lXMLDatasetCatch       := TClientDataSet.Create(nil);
      lXMLDatasetCatchSrc    := TClientDataSet.Create(nil);
      lXMLDatasetPatch       := TClientDataSet.Create(nil);
      lXMLDatasetPatchSrc    := TClientDataSet.Create(nil);
      lXMLDatasetPatchData   := TClientDataSet.Create(nil);
      lXMLDatasetChangeData  := TClientDataSet.Create(nil);
      lXMLDatasetMetaData    := TClientDataSet.Create(nil);
      lXMLDatasetUserStation := TClientDataSet.Create(nil);
      lXMLDatasetUserData    := TClientDataSet.Create(nil);
      lXMLDatasetSplit       := TClientDataSet.Create(nil);
      FAppModules.Database.CreateDataset(integer(dtExecSQL), lDBDataset);
      lPatchIDs       := TStringList.Create;
      lUserStationIDs := TStringList.Create;
      lOldScenarios   := TStringList.Create;
      lNewScenarios   := TStringList.Create;
      try
        if Assigned(lDBDataset) then
        begin
          lModelIdx       := -1;
          lAreaIdx        := -1;
          lSubAreaIdx     := -1;
          lGaugesIdx      := -1;
          lScenarioIdx    := -1;
          lCatchIdx       := -1;
          lCatchSrcIdx    := -1;
          lPatchSrcIdx    := -1;
          lPatchIdx       := -1;
          lPatchDataIdx   := -1;
          lChangeDataIdx  := -1;
          lMetaDataIdx    := -1;
          lUserStationIdx := -1;
          lUserDataIdx    := -1;
          lSplitIdx       := -1;
          ChDir(FTempDirectory);
          for lIndex := 0 to ATablePropertyList.Count - 1 do
          begin
            lTableProperty := TAbstractDbTableProperty(ATablePropertyList.Items[LIndex]);
            if (Uppercase(lTableProperty.TableName) = 'STUDYMODEL') then
              lModelIdx := LIndex
            else if (Uppercase(lTableProperty.TableName) = 'STUDYAREA') then
              lAreaIdx := LIndex
            else if (Uppercase(lTableProperty.TableName) = 'STUDYSUBAREA') then
              lSubAreaIdx := LIndex
            else if (Uppercase(lTableProperty.TableName) = 'STUDYSCENARIO') then
              lScenarioIdx := LIndex
            else if (Uppercase(lTableProperty.TableName) = 'RAINFALLPROJECTGAUGES') then
              lGaugesIdx := LIndex
            else if (Uppercase(lTableProperty.TableName) = 'RAINFALLCATCHMENT') then
              lCatchIdx := LIndex
            else if (Uppercase(lTableProperty.TableName) = 'RAINFALLCATCHMENTSOURCE') then
              lCatchSrcIdx := LIndex
            else if (Uppercase(lTableProperty.TableName) = 'RAINFALLPATCHR') then
              lPatchIdx := LIndex
            else if (Uppercase(lTableProperty.TableName) = 'RAINFALLPATCHSOURCE') then
              lPatchSrcIdx := LIndex
            else if (Uppercase(lTableProperty.TableName) = 'RAINFALLMONTHLYPATCHDATA') then
              lPatchDataIdx := LIndex
            else if (Uppercase(lTableProperty.TableName) = 'CHANGEPARAMETER') then
              lChangeDataIdx := LIndex
            else if (Uppercase(lTableProperty.TableName) = 'METADATAITEM') then
              lMetaDataIdx := LIndex
            else if (Uppercase(lTableProperty.TableName) = 'RAINFALLUSERSTATIONS') then
              lUserStationIdx := LIndex
            else if (Uppercase(lTableProperty.TableName) = 'RAINFALLUSERMONTHLYDATA') then
              lUserDataIdx := LIndex
            else if (Uppercase(lTableProperty.TableName) = 'RAINFALLRAWSPLITS') then
              lSplitIdx := LIndex
            else
            if (UpperCase(lTableProperty.TableName) = 'RAINFALLCATCHMENTFILEDATA') OR
               (UpperCase(lTableProperty.TableName) = 'RAINFALLCATCHMENTFILEDETAIL') then
            begin
              Result := ProcessImportTableData(AFileNamesList.Strings[LIndex], LTableProperty, AProgressUpdateFuntion);
              if not UpdateProgress(Result) then Exit;
            end
            else
            begin
            end;
          end;

          // Process StudyModel
          lSQL := 'SELECT * FROM StudyModel ';
          lDBDataSet.DataSet.Close;
          lDBDataset.SetSQL(lSQL);
          lDBDataset.DataSet.Open;
          if ((lModelIdx >= 0) AND (NOT lDBDataSet.DataSet.Eof)) then
          begin
            lFileNameModel := AFileNamesList.Strings[lModelIdx];
            lXMLDatasetModel.FileName := lFileNameModel;
            lXMLDatasetModel.Active   := TRUE;
            lXMLDatasetModel.First;
            while (NOT lXMLDatasetModel.Eof) do
            begin
              lModel := Trim(lXMLDatasetModel.FieldByName('Model').AsString);
              lDBDataSet.DataSet.First;
              if (lDBDataSet.DataSet.Locate('Model', lModel, [loCaseInsensitive])) then
                lXMLDatasetModel.Delete
              else
                lXMLDatasetModel.Next;
            end;
            lXMLDatasetModel.Active    := FALSE;
          end;

          // Process StudyArea,
          lSQL := 'SELECT * FROM StudyArea ' +
                  'WHERE Model = ' + QuotedStr('Rainfall');
          lDBDataSet.DataSet.Close;
          lDBDataset.SetSQL(lSQL);
          lDBDataset.DataSet.Open;
          if ((lAreaIdx >= 0) AND (NOT lDBDataSet.DataSet.Eof)) then
          begin
            lFileNameArea := AFileNamesList.Strings[lAreaIdx];
            lXMLDatasetArea.FileName := lFileNameArea;
            lXMLDatasetArea.Active   := TRUE;
            lXMLDatasetArea.First;
            while (NOT lXMLDatasetArea.Eof) do
            begin
              lArea := Trim(lXMLDatasetArea.FieldByName('StudyAreaName').AsString);
              lDBDataSet.DataSet.First;
              if (lDBDataSet.DataSet.Locate('StudyAreaName', lArea, [loCaseInsensitive])) then
                lXMLDatasetArea.Delete
              else
                lXMLDatasetArea.Next;
            end;
            lXMLDatasetArea.Active     := FALSE;
          end;

          // Process SubArea
          lSQL := 'SELECT * FROM StudySubArea WHERE ' +
                  'Model = ' + QuotedStr('Rainfall') +
                  ' AND StudyAreaName = ' + QuotedStr(lArea);
          lDBDataSet.DataSet.Close;
          lDBDataset.SetSQL(lSQL);
          lDBDataset.DataSet.Open;
          if ((lSubAreaIdx >= 0) AND (NOT lDBDataSet.DataSet.Eof)) then
          begin
            lFileNameSubArea := AFileNamesList.Strings[lSubAreaIdx];
            lXMLDatasetSubArea.FileName := lFileNameSubArea;
            lXMLDatasetSubArea.Active   := TRUE;
            lXMLDatasetSubArea.First;
            while (NOT lXMLDatasetSubArea.Eof) do
            begin
              lSubArea := Trim(lXMLDatasetSubArea.FieldByName('SubArea').AsString);
              lDBDataSet.DataSet.First;
              if (lDBDataSet.DataSet.Locate('SubArea', lSubArea, [loCaseInsensitive])) then
                lXMLDatasetSubArea.Delete
              else
                lXMLDatasetSubArea.Next;
            end;
            lXMLDatasetSubArea.Active  := FALSE;
          end;

          // Create new StationIDs for user stations
          // Update RainfallUserStation
          lDBDataSet.DataSet.Close;
          lSQL := 'SELECT Max(StationID) AS NewStationID FROM RainfallUserStations';
          lDBDataSet.SetSQL(lSQL);
          lDBDataSet.Dataset.Open;
          lDBDataSet.Dataset.First;
          lNewStationID := 0;
          if (NOT lDBDataSet.DataSet.EOF) then
            lNewStationID := lDBDataSet.Dataset.FieldByName('NewStationID').AsInteger;
          if (lNewStationID = 0) then
            lNewStationID := 100000;

          lSQL := 'SELECT * FROM RainfallUserStations  ' +
                  'ORDER BY Source, StationNumber ';
          lDBDataSet.DataSet.Close;
          lDBDataset.SetSQL(lSQL);
          lDBDataset.DataSet.Open;
          if (lUserStationIdx >= 0) then
          begin
            AProgressUpdateFuntion( FAppModules.Language.GetString('Message.PreProcessingRainfallUserStations'), ptNone, lStop);
            lFileNameUserStation := AFileNamesList.Strings[lUserStationIdx];
            lXMLDatasetUserStation.FileName := lFileNameUserStation;
            lXMLDatasetUserStation.Active := True;
            lXMLDatasetUserStation.First;
            while (NOT lXMLDatasetUserStation.EOF) do
            begin
              lSource        := Trim(lXMLDatasetUserStation.FieldByName('Source').AsString);
              lStationNumber := Trim(lXMLDatasetUserStation.FieldByName('StationNumber').AsString);
              lOldStationID  := lXMLDatasetUserStation.FieldByName('StationID').AsInteger;

              lDBDataSet.DataSet.First;
              if (lDBDataSet.DataSet.Locate('Source;StationNumber', VarArrayOf([lSource,lStationNumber]), [loCaseInsensitive])) then
              begin
                lXMLDatasetUserStation.Delete;
                lUserStationIDs.AddObject(IntToStr(lOldStationID), TObject(0));
              end
              else
              begin
                lNewStationID  := lNewStationID + 1;
                lUserStationIDs.AddObject(IntToStr(lOldStationID), TObject(lNewStationID));
                if (lOldStationID <> lNewStationID) then
                begin
                  // Update XML datasets with new value for PatchID
                  lXMLDatasetUserStation.Edit;
                  lXMLDatasetUserStation.FieldByName('StationID').AsInteger := lNewStationID;
                  lXMLDatasetUserStation.Post;
                end;
                lXMLDatasetUserStation.Next;
              end;
            end;
            lXMLDatasetUserStation.Active := FALSE;
            lDBDataSet.DataSet.Close;
          end;

          // Update RainfallUserMonthlyData
          if (lUserDataIdx >= 0) then
          begin
            AProgressUpdateFuntion( FAppModules.Language.GetString('Message.PreProcessingRainfallUserMonthlyData'), ptNone, lStop);
            lFileNameUserData := AFileNamesList.Strings[lUserDataIdx];
            lXMLDatasetUserData.FileName := lFileNameUserData;
            lXMLDatasetUserData.Active := True;
            lXMLDatasetUserData.First;
            while (NOT lXMLDatasetUserData.EOF) do
            begin
              lOldStationID := lXMLDatasetUserData.FieldByName('StationID').AsInteger;
              lListIndex    := lUserStationIDs.IndexOf(IntToStr(lOldStationID));
              if (lListIndex >= 0) then
              begin
                lNewStationID := Integer(lUserStationIDs.Objects[lListIndex]);
                if (lNewStationID = 0) then
                  lXMLDatasetUserData.Delete
                else
                begin
                  if (lOldStationID <> lNewStationID) then
                  begin
                    lXMLDatasetUserData.Edit;
                    lXMLDatasetUserData.FieldByName('StationID').AsInteger := lNewStationID;
                    lXMLDatasetUserData.Post;
                  end;
                  lXMLDatasetUserData.Next;
                end;
              end
              else
                lXMLDatasetUserData.Next;
            end;
            lXMLDatasetUserData.Active := FALSE;
          end;

          // Create new PacthIDs for patches
          // Update RainfallPatchR
          if (lPatchIdx >= 0) then
          begin
            AProgressUpdateFuntion( FAppModules.Language.GetString('Message.PreProcessingRainfallPatchR'), ptNone, lStop);
            lDBDataSet.DataSet.Close;
            lSQL := 'SELECT Max(PatchID) AS NewPatchID FROM RainfallPatchR';
            lDBDataSet.SetSQL(lSQL);
            lDBDataSet.Dataset.Open;
            lDBDataSet.Dataset.First;
            lNewPatchID := 0;
            if (NOT lDBDataSet.DataSet.EOF) then
              lNewPatchID := lDBDataSet.Dataset.FieldByName('NewPatchID').AsInteger;
            if (lNewPatchID = 0) then
              lNewPatchID := 20000;

            lFileNamePatch := AFileNamesList.Strings[lPatchIdx];
            lXMLDatasetPatch.FileName := lFileNamePatch;
            lXMLDatasetPatch.Active := True;
            lXMLDatasetPatch.First;
            while (NOT lXMLDatasetPatch.EOF) do
            begin
              lOldPatchID := lXMLDatasetPatch.FieldByName('PatchID').AsInteger;
              lNewPatchID := lNewPatchID + 1;
              lPatchIDs.AddObject(IntToStr(lOldPatchID), TObject(lNewPatchID));
              if (lOldPatchID <> lNewPatchID) then
              begin
                // Update XML datasets with new value for PatchID
                lXMLDatasetPatch.Edit;
                lXMLDatasetPatch.FieldByName('PatchID').AsInteger := lNewPatchID;
                lXMLDatasetPatch.Post;
              end;
              lXMLDatasetPatch.Next;
            end;
            lXMLDatasetPatch.Active := FALSE;
            lDBDataSet.DataSet.Close;
          end;

          // Create new IDs for Scenarios
          // Update StudyScenario
          lSQL := 'SELECT * FROM StudyScenario WHERE ' +
                  'Model = ' + QuotedStr('Rainfall') +
                  ' AND StudyAreaName = ' + QuotedStr(lArea);
          lDBDataSet.DataSet.Close;
          lDBDataset.SetSQL(lSQL);
          lDBDataset.DataSet.Open;
          lDBDataSet.DataSet.First;
          if ((lScenarioIdx >= 0) AND (NOT lDBDataSet.DataSet.EOF)) then
          begin
            AProgressUpdateFuntion( FAppModules.Language.GetString('Message.PreProcessingStudyScenario'), ptNone, lStop);
            lFileNameScenario := AFileNamesList.Strings[lScenarioIdx];
            lXMLDatasetScenario.FileName  := lFileNameScenario;
            lXMLDatasetScenario.Active := True;
            lXMLDatasetScenario.First;
            lIndex := 0;
            while (NOT lXMLDatasetScenario.EOF) do
            begin
              lSubArea   := Trim(lXMLDatasetScenario.FieldByName('SubArea').AsString);
              lOldIDText := Trim(lXMLDatasetScenario.FieldByName('Scenario').AsString);
              if (Trim(lOldIDText) <> 'Project Gauges') then
              begin
                lNewIDText := lOldIDText;
                // Check for duplicate values of Scenario in database
                lDBDataSet.DataSet.First;
                while (lDBDataSet.DataSet.Locate('SubArea;Scenario', VarArrayOf([lSubArea,lNewIDText]), [loCaseInsensitive])) do
                begin
                  lIndex := lIndex + 1;
                  lNewIDText := 'Zone ' + IntToStr(lIndex);
                  lDBDataSet.DataSet.First;
                end;
                lOldScenarios.Add(lSubArea + ',' + lOldIDText);
                lNewScenarios.Add(lNewIDText);
                if (lOldIDText <> lNewIDText) then
                begin
                  // Update XML datasets with new value for Scenario
                  lXMLDatasetScenario.Edit;
                  lXMLDatasetScenario.FieldByName('Scenario').AsString := lNewIDText;
                  lXMLDatasetScenario.Post;
                end;
              end;
              lXMLDatasetScenario.Next;
            end;
            lXMLDatasetScenario.Active := FALSE;
          end;
          lDBDataSet.DataSet.Close;

          // Update RainfallRAWSplits
          lSQL := 'SELECT * FROM RainfallRAWSplits';
          lDBDataSet.DataSet.Close;
          lDBDataset.SetSQL(lSQL);
          lDBDataset.DataSet.Open;
          if (lSplitIdx >= 0) then
          begin
            AProgressUpdateFuntion( FAppModules.Language.GetString('Message.PreProcessingRainfallRAWSplits'), ptNone, lStop);
            lFileNameSplit := AFileNamesList.Strings[lSplitIdx];
            lXMLDatasetSplit.FileName := lFileNameSplit;
            lXMLDatasetSplit.Active := True;
            lXMLDatasetSplit.First;
            while (NOT lXMLDatasetSplit.EOF) do
            begin
              lOldStationID := lXMLDatasetSplit.FieldByName('StationID').AsInteger;
              lStartYear    := lXMLDatasetSplit.FieldByName('HydroStartYear').AsInteger;
              lEndYear      := lXMLDatasetSplit.FieldByName('HydroEndYear').AsInteger;
              if (lOldStationID < 100000) then
              begin
                lDBDataSet.DataSet.First;
                if (lDBDataSet.DataSet.Locate('StationID;HydroStartYear;HydroEndYear',
                                              VarArrayOf([lOldStationID,lStartYear,lEndYear]),
                                              [loCaseInsensitive])) then
                  lXMLDatasetSplit.Delete
                else
                  lXMLDatasetSplit.Next;
              end
              else
              begin
                lListIndex := lUserStationIDs.IndexOf(IntToStr(lOldStationID));
                if (lListIndex >= 0) then
                begin
                  lNewStationID := Integer(lUserStationIDs.Objects[lListIndex]);
                  if (lNewStationID = 0) then
                  begin
                    lDBDataSet.DataSet.First;
                    if (lDBDataSet.DataSet.Locate('StationID;HydroStartYear;HydroEndYear',
                                                  VarArrayOf([lOldStationID,lStartYear,lEndYear]),
                                                  [loCaseInsensitive])) then
                      lXMLDatasetSplit.Delete
                    else
                      lXMLDatasetSplit.Next;
                  end
                  else
                  begin
                    if (lOldStationID <> lNewStationID) then
                    begin
                      lXMLDatasetSplit.Edit;
                      lXMLDatasetSplit.FieldByName('StationID').AsInteger := lNewStationID;
                      lXMLDatasetSplit.Post;
                    end;
                    lXMLDatasetSplit.Next;
                  end;
                end
                else
                  lXMLDatasetSplit.Next;
              end;
            end;
            lXMLDatasetSplit.Active := FALSE;
          end;

          // Process ProjectGauges
          lSQL := 'SELECT * FROM RainfallProjectGauges WHERE ' +
                  'Model = ' + QuotedStr('Rainfall') +
                  ' AND StudyAreaName = ' + QuotedStr(lArea);
          lDBDataSet.DataSet.Close;
          lDBDataset.SetSQL(lSQL);
          lDBDataset.DataSet.Open;
          if (lGaugesIdx >= 0) then
          begin
            AProgressUpdateFuntion( FAppModules.Language.GetString('Message.PreProcessingRainfallProjectGauges'), ptNone, lStop);
            lFileNameGauges := AFileNamesList.Strings[lGaugesIdx];
            lXMLDatasetGauges.FileName := lFileNameGauges;
            lXMLDatasetGauges.Active   := TRUE;
            lXMLDatasetGauges.First;
            while (NOT lXMLDatasetGauges.Eof) do
            begin
              lSubArea      := Trim(lXMLDatasetGauges.FieldByName('SubArea').AsString);
              lOldStationID := lXMLDatasetGauges.FieldByName('StationID').AsInteger;
              lOldIDText    := Trim(lXMLDatasetGauges.FieldByName('Scenario').AsString);
              if (lOldStationID < 100000) then
              begin
                lDBDataSet.DataSet.First;
                if (lDBDataSet.DataSet.Locate('SubArea;StationID', VarArrayOf([lSubArea,lOldStationID]), [loCaseInsensitive])) then
                  lXMLDatasetGauges.Delete
                else
                  lXMLDatasetGauges.Next;
              end
              else
              begin
                lListIndex := lUserStationIDs.IndexOf(IntToStr(lOldStationID));
                if (lListIndex >= 0) then
                begin
                  lNewStationID := Integer(lUserStationIDs.Objects[lListIndex]);
                  if (lNewStationID = 0) then
                  begin
                    lDBDataSet.DataSet.First;
                    if (lDBDataSet.DataSet.Locate('SubArea;StationID', VarArrayOf([lSubArea,lOldStationID]), [loCaseInsensitive])) then
                      lXMLDatasetGauges.Delete
                    else
                      lXMLDatasetGauges.Next;
                  end
                  else
                  begin
                    if (lOldStationID <> lNewStationID) then
                    begin
                      lXMLDatasetGauges.Edit;
                      lXMLDatasetGauges.FieldByName('StationID').AsInteger := lNewStationID;
                      lXMLDatasetGauges.Post;
                    end;
                    lXMLDatasetGauges.Next;
                  end
                end
                else
                  lXMLDatasetGauges.Next;
              end;
            end;
            lXMLDatasetGauges.Active := FALSE;
          end;

          // Process RainfallCatchment
          if (lCatchIdx >= 0) then
          begin
            AProgressUpdateFuntion( FAppModules.Language.GetString('Message.PreProcessingRainfallCatchment'), ptNone, lStop);
            lFileNameCatch := AFileNamesList.Strings[lCatchIdx];
            lXMLDatasetCatch.FileName := lFileNameCatch;
            lXMLDatasetCatch.Active := True;
            lXMLDatasetCatch.First;
            while (NOT lXMLDatasetCatch.Eof) do
            begin
              lSubArea   := Trim(lXMLDatasetCatch.FieldByName('SubArea').AsString);
              lOldIDText := Trim(lXMLDatasetCatch.FieldByName('Scenario').AsString);
              lListIndex := lOldScenarios.IndexOf(lSubArea + ',' + lOldIDText);
              if (lListIndex >= 0) then
              begin
                lNewIDText := lNewScenarios.Strings[lListIndex];
                if (lOldIDText <> lNewIDText) then
                begin
                  lXMLDatasetCatch.Edit;
                  lXMLDatasetCatch.FieldByName('Scenario').AsString := lNewIDText;
                  lXMLDatasetCatch.Post;
                end;
              end;
              lXMLDatasetCatch.Next;
            end;
            lXMLDatasetCatch.Active := FALSE;
          end;

          // Process RainfallCatchmentSource

          if (lCatchSrcIdx >= 0) then
          begin
            AProgressUpdateFuntion( FAppModules.Language.GetString('Message.PreProcessingRainfallCatchmentSource'), ptNone, lStop);
            lFileNameCatchSrc := AFileNamesList.Strings[lCatchSrcIdx];
            lXMLDatasetCatchSrc.FileName := lFileNameCatchSrc;
            lXMLDatasetCatchSrc.Active := True;
            lXMLDatasetCatchSrc.First;
            while (NOT lXMLDatasetCatchSrc.EOF) do
            begin
              lSubArea      := Trim(lXMLDatasetCatchSrc.FieldByName('SubArea').AsString);
              lOldIDText    := Trim(lXMLDatasetCatchSrc.FieldByName('Scenario').AsString);
              lOldStationID := lXMLDatasetCatchSrc.FieldByName('StationID').AsInteger;
              lOldPatchID   := lXMLDatasetCatchSrc.FieldByName('SourcePatchID').AsInteger;
              lXMLDatasetCatchSrc.Edit;
              lListIndex := lOldScenarios.IndexOf(lSubArea + ',' + lOldIDText);
              if (lListIndex >= 0) then
              begin
                lNewIDText := lNewScenarios.Strings[lListIndex];
                lXMLDatasetCatchSrc.FieldByName('Scenario').AsString := lNewIDText;
              end;
              lListIndex := lUserStationIDs.IndexOf(IntToStr(lOldStationID));
              if (lListIndex >= 0) then
              begin
                lNewStationID := Integer(lUserStationIDs.Objects[lListIndex]);
                if (lNewStationID <> 0) then
                  lXMLDatasetCatchSrc.FieldByName('StationID').AsInteger := lNewStationID;
              end;
              lListIndex := lPatchIDs.IndexOf(IntToStr(lOldPatchID));
              if (lListIndex >= 0) then
              begin
                lNewPatchID := Integer(lPatchIDs.Objects[lListIndex]);
                lXMLDatasetCatchSrc.FieldByName('SourcePatchID').AsInteger := lNewPatchID;
              end;
              lXMLDatasetCatchSrc.Post;
              lXMLDatasetCatchSrc.Next;
            end;
            lXMLDatasetCatchSrc.Active := FALSE;
          end;

          // Process RainfallPatchSource
          if (lPatchSrcIdx >= 0) then
          begin
            AProgressUpdateFuntion( FAppModules.Language.GetString('Message.PreProcessingRainfallPatchSource'), ptNone, lStop);
            lFileNamePatchSrc := AFileNamesList.Strings[lPatchSrcIdx];
            lXMLDatasetPatchSrc.FileName := lFileNamePatchSrc;
            lXMLDatasetPatchSrc.Active := True;
            lXMLDatasetPatchSrc.First;
            while (NOT lXMLDatasetPatchSrc.EOF) do
            begin
              lXMLDatasetPatchSrc.Edit;
              lOldStationID := lXMLDatasetPatchSrc.FieldByName('SourceStationID').AsInteger;
              lListIndex := lUserStationIDs.IndexOf(IntToStr(lOldStationID));
              if (lListIndex >= 0) then
              begin
                lNewStationID := Integer(lUserStationIDs.Objects[lListIndex]);
                if (lNewStationID <> 0) then
                  lXMLDatasetPatchSrc.FieldByName('SourceStationID').AsInteger := lNewStationID;
              end;
              lOldPatchID := lXMLDatasetPatchSrc.FieldByName('PatchID').AsInteger;
              lListIndex  := lPatchIDs.IndexOf(IntToStr(lOldPatchID));
              if (lListIndex >= 0) then
              begin
                lNewPatchID := Integer(lPatchIDs.Objects[lListIndex]);
                lXMLDatasetPatchSrc.FieldByName('PatchID').AsInteger := lNewPatchID;
              end;
              lOldPatchID := lXMLDatasetPatchSrc.FieldByName('SourcePatchID').AsInteger;
              lListIndex := lPatchIDs.IndexOf(IntToStr(lOldPatchID));
              if (lListIndex >= 0) then
              begin
                lNewPatchID := Integer(lPatchIDs.Objects[lListIndex]);
                lXMLDatasetPatchSrc.FieldByName('SourcePatchID').AsInteger := lNewPatchID;
              end;
              lXMLDatasetPatchSrc.Post;
              lXMLDatasetPatchSrc.Next;
            end;
            lXMLDatasetPatchSrc.Active := FALSE;
          end;

          // Process RainfallMonthlyPatchData
          if (lPatchDataIdx >= 0) then
          begin
            AProgressUpdateFuntion( FAppModules.Language.GetString('Message.PreProcessingRainfallMonthlyPatchData'), ptNone, lStop);
            lFileNamePatchData := AFileNamesList.Strings[lPatchDataIdx];
            lXMLDatasetPatchData.FileName := lFileNamePatchData;
            lXMLDatasetPatchData.Active := True;
            lXMLDatasetPatchData.First;
            while (NOT lXMLDatasetPatchData.EOF) do
            begin
              lXMLDatasetPatchData.Edit;
              lOldStationID := lXMLDatasetPatchData.FieldByName('StationID').AsInteger;
              lListIndex := lUserStationIDs.IndexOf(IntToStr(lOldStationID));
              if (lListIndex >= 0) then
              begin
                lNewStationID := Integer(lUserStationIDs.Objects[lListIndex]);
                if (lNewStationID <> 0) then
                  lXMLDatasetPatchData.FieldByName('StationID').AsInteger := lNewStationID;
              end;
              lOldPatchID := lXMLDatasetPatchData.FieldByName('PatchID').AsInteger;
              lListIndex  := lPatchIDs.IndexOf(IntToStr(lOldPatchID));
              if (lListIndex >= 0) then
              begin
                lNewPatchID := Integer(lPatchIDs.Objects[lListIndex]);
                lXMLDatasetPatchData.FieldByName('PatchID').AsInteger := lNewPatchID;
              end;
              lXMLDatasetPatchData.Post;
              lXMLDatasetPatchData.Next;
            end;
            lXMLDatasetPatchData.Active := FALSE;
          end;

          // Process ChangeParameter
          if (lChangeDataIdx >= 0) then
          begin
            lFileNameChangeData := AFileNamesList.Strings[lChangeDataIdx];
            lXMLDatasetChangeData.FileName := lFileNameChangeData;
            lXMLDatasetChangeData.Active := TRUE;
            lXMLDatasetChangeData.FindFirst;
            while (NOT lXMLDatasetChangeData.EOF) do
            begin
              lParamField := Trim(lXMLDatasetChangeData.FieldByName('ParamField').AsString);
              if (lParamField = 'MonthlyPatchData') OR (lParamField = 'MonthlyPatchSign') OR
                 (lParamField = 'MonthlyRAWData')   OR (lParamField = 'MonthlyRAWSign') then
              begin
                lKeyValues := Trim(lXMLDatasetChangeData.FieldByName('KeyValues').AsString);
                ReplacePatchIDValues(lKeyValues, lPatchIDs);
                ReplaceStationIDValues(lKeyValues, lUserStationIDs);
                lXMLDatasetChangeData.Edit;
                lXMLDatasetChangeData.FieldByName('KeyValues').AsString := lKeyValues;
                lXMLDatasetChangeData.Post;
              end;
              lXMLDatasetChangeData.Next;
            end;
            lXMLDatasetChangeData.Active := FALSE;
          end;

          // Process MetaDataItem
          if (lMetaDataIdx >= 0) then
          begin
            lFileNameMetaData := AFileNamesList.Strings[lMetaDataIdx];
            lXMLDatasetMetaData.FileName := lFileNameMetaData;
            lXMLDatasetMetaData.Active := TRUE;
            lXMLDatasetMetaData.FindFirst;
            while (NOT lXMLDatasetMetaData.EOF) do
            begin
              lParamField := Trim(lXMLDatasetMetaData.FieldByName('ParamField').AsString);
              if (lParamField = 'MonthlyPatchData') OR (lParamField = 'MonthlyPatchSign') OR
                 (lParamField = 'MonthlyRAWData')   OR (lParamField = 'MonthlyRAWSign') then
              begin
                lKeyValues := Trim(lXMLDatasetMetaData.FieldByName('KeyValues').AsString);
                ReplacePatchIDValues(lKeyValues, lPatchIDs);
                ReplaceStationIDValues(lKeyValues, lUserStationIDs);
                lXMLDatasetMetaData.Edit;
                lXMLDatasetMetaData.FieldByName('KeyValues').AsString := lKeyValues;
                lXMLDatasetMetaData.Post;
              end;
              lXMLDatasetMetaData.Next;
            end;
            lXMLDatasetMetaData.Active := FALSE;
          end;

          Result := True;
        end;
      finally
        lDBDataSet.Free;
        lXMLDatasetModel.Free;
        lXMLDatasetArea.Free;
        lXMLDatasetSubArea.Free;
        lXMLDatasetGauges.Free;
        lXMLDatasetScenario.Free;
        lXMLDatasetCatch.Free;
        lXMLDatasetCatchSrc.Free;
        lXMLDatasetPatch.Free;
        lXMLDatasetPatchSrc.Free;
        lXMLDatasetPatchData.Free;
        lXMLDatasetChangeData.Free;
        lXMLDatasetMetaData.Free;
        lXMLDatasetUserStation.Free;
        lXMLDatasetUserData.Free;
        FreeAndNil(lPatchIDs);
        FreeAndNil(lUserStationIDs);
        FreeAndNil(lOldScenarios);
        FreeAndNil(lNewScenarios);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDbTableDataManager.ReplacePatchIDValues (var AKeyValues : string;
                                                    APatchIDs      : TStringList);
const OPNAME = 'TDbTableDataManager.ReplacePatchIDValues';
var
  lPos     : integer;
  lStart   : integer;
  lEnd     : integer;
  lOldID   : string;
  lNewID   : string;
  lIndex   : integer;
begin
  try
    lPos := Pos('PatchID=', AKeyValues);
    if (lPos > 0) then
    begin
      lStart := PosEx('=', AKeyValues, lPos);
      lEnd   := PosEx(',', AKeyValues, lPos);
      if (lEnd > 0) then
        lOldID := Copy(AKeyValues, lStart+1, lEnd - (lStart+1))
      else
        lOldID := Copy(AKeyValues, lStart+1, Length(AKeyValues) - lStart);
      lIndex := APatchIDs.IndexOf(lOldID);
      if (lIndex >= 0) then
        lNewID := IntToStr(Integer(APatchIDs.Objects[lIndex]))
      else
        lNewID := lOldID;
      if (lEnd > 0) then
        AKeyValues := Copy(AKeyValues, 1, lStart) + lNewID +
                      Copy(AKeyValues, lEnd, Length(AKeyValues) - lEnd + 1)
      else
        AKeyValues := Copy(AKeyValues, 1, lStart) + lNewID;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.ExportAllStudyData: boolean;
const OPNAME = 'TDbTableDataManager.ExportAllStudyData';
begin
  Result := False;
  try
    FExportAll := True;
    Result := ExportStudyData(malNone,nil);
    FExportAll := False;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDbTableDataManager.ImportAllStudyData: boolean;
const OPNAME = 'TDbTableDataManager.ImportAllStudyData';
begin
  Result := False;
  try
    FImportAll := True;
    Result := ImportStudyData;
    FImportAll := False;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDbTableDataManager.ProcessWRYMDataTable(AXMLFileNames: TStringList;
                                                  ATableProperty: TAbstractDbTableProperty;
                                                  AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TDbTableDataManager.ProcessWRYMDataTable';
begin
  Result := FALSE;
end;

function TDbTableDataManager.ExportChangeLists(AChangeListNumbersCommaText: string): boolean;
const OPNAME = 'TDbTableDataManager.ExportChangeLists';
begin
  Result := False;
  try
    if (FAppModules = nil) then
      raise Exception.Create('FAppModules is not yet assigned.');
    if (FAppModules.StudyArea = nil) then
      raise Exception.Create('FAppModules.StudyArea is not yet assigned.');
    if (FAppModules.Model = nil) then
      raise Exception.Create('FAppModules.Model is not yet assigned.');
    if (Trim(AChangeListNumbersCommaText) = '') then
      raise Exception.Create('Change list numbers are empty.');

    Self.Initialise;
    FSelectionLevel          := malScenarion;
    FStudyArea               := FAppModules.StudyArea;
    FChangeListIDs           := AChangeListNumbersCommaText;
    FExportingChangeListData := True;
    FModel                   := '';
    FStudy                   := '';
    FSubArea                 := '';
    FScenario                := '';
    if SelectFilesDirectory then
    begin
      FProgressDialog.clbOptions.Items.Clear;
      FProgressDialog.clbOptions.Items.Add('Stop on first error');
      FProgressDialog.clbOptions.Checked[0] := False;// FAppModules.GlobalData.StopOnFirstErr;
      FProgressDialog.AddExecutionFunctions(ExecExportChangeLists);
      FModel   := FStudyArea.ModelCode;
      FStudy   := FStudyArea.StudyAreaCode;
      FSubArea := FStudyArea.SubAreaCode;
      FScenario := FStudyArea.ScenarioCode;

      FProgressDialog.Caption := FAppModules.Language.GetString('TDbTableDataManager.strExportChangeLists');
      FProgressDialog.Succsessful := False;
      FProgressDialog.ShowModal;
      Result := FProgressDialog.Succsessful;
      FProgressDialog.Hide;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDbTableDataManager.ExecExportChangeLists(AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TDbTableDataManager.ExecExportChangeLists';
var
  lStop        : boolean;
  lTable       : TAbstractDbTableProperty;
begin
  Result := FALSE;
  try
    FProgressDialog.ActionProgressBar.Min := 0;
    FProgressDialog.ActionProgressBar.Max :=  3;

    lTable := FAppModules.DBTablePropertyManager.TablePropertyByName['ChangeList'];
    if (lTable = nil) then
      raise Exception.Create('Table Properties for table ChangeList is not yet defined.');
    AProgressUpdateFuntion(FAppModules.Language.GetString('Message.ProcessingTable') + lTable.TableName, ptNone, lStop);
    if LStop then Exit;
    Result :=  ProcessExportSelectedChangeLists(lTable);
    if (NOT UpdateProgress(Result)) then Exit;


    lTable := FAppModules.DBTablePropertyManager.TablePropertyByName['ChangeParameter'];
    if (lTable = nil) then
      raise Exception.Create('Table Properties for table ChangeParameter is not yet defined.');
    AProgressUpdateFuntion(FAppModules.Language.GetString('Message.ProcessingTable') + lTable.TableName, ptNone, lStop);
    if LStop then Exit;
    Result :=  Result and ProcessExportSelectedChangeLists(lTable);
    if (NOT UpdateProgress(Result)) then Exit;


    AProgressUpdateFuntion(FAppModules.Language.GetString('Message.CreatingScriptListFile'), ptNone, lStop);
    Result := Result AND CreateScriptListFile;
    if (NOT UpdateProgress(Result)) then Exit;
    AProgressUpdateFuntion(FAppModules.Language.GetString('Message.CreatingZipFile'), ptNone, lStop);
    Result := Result AND CreateSubAreaZip;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.ProcessExportSelectedChangeLists(ATableProperty : TAbstractDbTableProperty): boolean;
const OPNAME = 'TDbTableDataManager.ProcessExportSelectedChangeLists';
var
  LDataSet         : TAbstractModelDataset;
  LSQL             : string;
  LWhereClause     : string;
  LFileName        : string;
  LClientDataSet   : TClientDataSet;
  LDataSetProvider : TDataSetProvider;
begin
  Result := false;
  try
    if Assigned(ATableProperty) then
    begin
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      try
        if Assigned(LDataSet) then
        begin

          LWhereClause := ' WHERE ' + FAppModules.Model.GetChangeListWhereClause +
                          ' AND ChangeListID IN (' + FChangeListIDs + ')';


          //LSQL := 'SELECT ' + Trim(ATableProperty.FieldNames.CommaText) +
          LSQL := 'SELECT ' + Trim(ATableProperty.DelimitedFieldNamesCommatext) +
                  ' FROM ' + ATableProperty.TableName +  LWhereClause;

          LDataSet.SetSQL(LSQL);
          LDataset.DataSet.Open;
          if(LDataset.DataSet.Eof) and (LDataset.DataSet.Bof) then
          begin
            Result := True;
          end
          else
          begin
            LDataSetProvider := TDataSetProvider.Create(nil);
            LClientDataSet := TClientDataSet.Create(nil);
            try
              LDataSetProvider.DataSet := LDataset.DataSet;
              LClientDataSet.ReadOnly := True;
              LClientDataSet.SetProvider(LDataSetProvider);
              LClientDataSet.StoreDefs := True;
              LClientDataSet.Active := True;
              LFileName := Trim(FTempDirectory) + 'LD_' + Trim(ATableProperty.TableName) + '.xml';
              LClientDataSet.SaveToFile(LFileName,dfXML);
              FScriptList.Add(ExtractFileName(LFileName));
            finally
              LDataset.DataSet.Active := False;
              LClientDataSet.Active := False;
              LClientDataSet.Free;
              LDataSetProvider.Free;
            end;
          end;
          Result := True;
        end;
      finally
        LDataset.DataSet.Close;
        LDataset.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.ImportChangeLists: boolean;
const OPNAME = 'TDbTableDataManager.ImportChangeLists';
var
  LFileName: string;
  LChangeListFileDialog : TOpenDialog;
begin
  Result := False;
  try
    if (FAppModules = nil) then
      raise Exception.Create('FAppModules is not yet assigned.');
    if (FAppModules.StudyArea = nil) then
      raise Exception.Create('FAppModules.StudyArea is not yet assigned.');
    if (FAppModules.Model = nil) then
      raise Exception.Create('FAppModules.Model is not yet assigned.');

    Self.Initialise;
    FStudyArea               := FAppModules.StudyArea;

    try
      LChangeListFileDialog := TOpenDialog.Create ( nil );
      LChangeListFileDialog.DefaultExt  := 'ZIP';
      LChangeListFileDialog.Filter      := 'Change Lists ZIP files (*ChangeLists.zip)|*ChangeLists.ZIP';
      LChangeListFileDialog.FilterIndex := 1;
      if ( LChangeListFileDialog.Execute ) then
        FZipFileName := LChangeListFileDialog.FileName
      else
        Exit;
    finally
      FreeAndNil ( LChangeListFileDialog );
    end;

    if (Pos(FStudyArea.ModelCode,ExtractFileName(FZipFileName)) <> 1) then
      raise Exception.Create ( 'You cannot import change lists across models. The zip file name must start with('+FStudyArea.ModelCode+')' );

    if (Pos('ChangeLists',FZipFileName) = 0) then
      raise Exception.Create ( 'Change list zip file' + LFileName + ' does not have the correct format!' );

    if not FileExists(FZipFileName) then
      raise Exception.Create ( 'Change list zip file' + LFileName + ' does not exist!' );

    FProgressDialog.clbOptions.Items.Clear;
    FProgressDialog.clbOptions.Items.Add('Stop on first error');
    FProgressDialog.clbOptions.Checked[0] := False;// FAppModules.GlobalData.StopOnFirstErr;
    FProgressDialog.AddExecutionFunctions(ExecImportChangeLists);

    FProgressDialog.Caption := FAppModules.Language.GetString('TDbTableDataManager.strImportChangeLists');
    FProgressDialog.Succsessful := False;
    FProgressDialog.ShowModal;
    Result := FProgressDialog.Succsessful;
    FProgressDialog.Hide;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.ExecImportChangeLists(AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TDbTableDataManager.ExecImportChangeLists';
var
  LXMLFilename: string;
  LTableProperty: TAbstractDbTableProperty;
  LStop: boolean;
begin
  Result := False;
  try
    if UnZipFiles then
    begin
      LXMLFilename := FTempDirectory+ 'LD_ChangeList.xml';
      if not FileExists(LXMLFilename) then
        raise Exception.Create ( 'Unzipped File ' + LXMLFilename + ' not found!' );

      LXMLFilename := FTempDirectory+ 'LD_ChangeParameter.xml';
      if not FileExists(LXMLFilename) then
        raise Exception.Create ( 'Unzipped File ' + LXMLFilename + ' not found!' );

      FProgressDialog.ActionProgressBar.Min := 0;
      FProgressDialog.ActionProgressBar.Max :=  3;

      AProgressUpdateFuntion( FAppModules.Language.GetString('Message.PreProcessingChangeListFiles'),ptNone,LStop);
      if LStop then Exit;

      if PreProcessImportChangeListTables then
      begin

        LTableProperty := FAppModules.DBTablePropertyManager.TablePropertyByName['ChangeList'];
        if (LTableProperty = nil) then
          raise Exception.Create('Table Properties for table ChangeList is not yet defined.');
        LXMLFilename := FTempDirectory+ 'LD_ChangeList.xml';
        Result :=  ProcessImportSelectedChangeLists(LXMLFilename,LTableProperty,AProgressUpdateFuntion );
        if (NOT UpdateProgress(Result)) then Exit;


        LTableProperty := FAppModules.DBTablePropertyManager.TablePropertyByName['ChangeParameter'];
        if (LTableProperty = nil) then
          raise Exception.Create('Table Properties for table ChangeParameter is not yet defined.');
        LXMLFilename := FTempDirectory+ 'LD_ChangeParameter.xml';
        Result :=  Result and ProcessImportSelectedChangeLists(LXMLFilename,LTableProperty,AProgressUpdateFuntion);
        if (NOT UpdateProgress(Result)) then Exit;

        DeleteMultipleFiles(FTempDirectory + '*.*');
      end;

      Result := True;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.ProcessImportOlderVersion(AXMLFilename: string;
  ATableProperty: TAbstractDbTableProperty;
  AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TDbTableDataManager.ProcessImportOlderVersion';
var
  LXMLFileData : TStringList;
  LXMLDoc : IXMLDocument;
  LXMLNode : IXMLNode;
  LXMLValueNode : IXMLNode;
  LXMLRowNode : IXMLNodeList;
  LIndex : integer;
  LDeleteDataSet      : TAbstractModelDataset;
  LAppendDataSet      : TAbstractModelDataset;
  LStop               : boolean;
  LModel,
  LParamName,
  LParamValue,
  LInvalidStr,
  LFieldName          : string;
  LSQLDelete          : String;
  LLinesCount         : integer;
begin
  Result := False;
  try
    LXMLFileData := TStringList.Create;
    try

      LXMLFileData.LoadFromFile(AXMLFilename);
      if LXMLFileData.Count > 0 then
      begin
        Result := (Pos('<?xml version="1.0" standalone="yes"?>  <DATAPACKET Version="2.0">', LXMLFileData[0]) > 0 );
        if Result then
        begin
          AProgressUpdateFuntion( FAppModules.Language.GetString('Message.ProcessingFile') + ExtractFileName(AXMLFilename),ptNone,LStop);
          if LStop then Exit;

          if (ATableProperty.TableGroup = tgOutputData) and (not FIncludeOutputData) then
          begin
            Result := True;
            Exit;
          end;
          if (UpperCase(ATableProperty.TableName) = 'STUDYSCENARIOLOCK') or
             (UpperCase(ATableProperty.TableName) = 'TSCCHART') or
             (UpperCase(ATableProperty.TableName) = 'TSCCHARTSERIES') or
             (UpperCase(ATableProperty.TableName) = 'TSCSERIES') or
             (UpperCase(ATableProperty.TableName) = 'TSCVIEW') or
             (UpperCase(ATableProperty.TableName) = 'TSCVIEWCHART') or
             (UpperCase(ATableProperty.TableName) = 'TSCVIEWSCENARIO') then
          begin
            Result := False;
            Exit;
          end;
          LSQLDelete := 'DELETE FROM '+ ATableProperty.TableName + ' WHERE';
          for LIndex := 0 to ATableProperty.IndexFieldNames.Count -1 do
          begin
            LSQLDelete := LSQLDelete + ' ' + ATableProperty.IndexFieldNames[LIndex];
            LSQLDelete := LSQLDelete + ' = :A'+ATableProperty.IndexFieldNames[LIndex];
            if(LIndex <> (ATableProperty.IndexFieldNames.Count -1)) then
              LSQLDelete := LSQLDelete + ' AND'
          end;
          LXMLDoc := TXMLDocument.Create(nil);
          //try
            //LXMLDoc.LoadFromFile(AXMLFilename);
          //except
          LInvalidStr := '&#092;';

          LXMLFileData.Text := StringReplace(LXMLFileData.Text, LInvalidStr,'\',[rfReplaceAll]);

          LInvalidStr :=  '&#013;&#010;';

          LXMLFileData.Text := StringReplace(LXMLFileData.Text, LInvalidStr,'',[rfReplaceAll]);

          LXMLDoc.LoadFromXML(LXMLFileData.Text);

          //end;

          if (LXMLDoc.documentElement = nil)  then
          begin
            Result := False;
            Exit;
          end;
          FAppModules.Database.CreateDataset(integer(dtExecSQL), LDeleteDataSet);
          FAppModules.Database.CreateDataset(integer(dtExecSQL), LAppendDataSet);
          try
            LAppendDataSet.SetSQL('SELECT * FROM '+ ATableProperty.TableName);
            LAppendDataSet.SetReadOnly(False);
            LAppendDataSet.DataSet.Active := True;

            LDeleteDataSet.DataSet.Close;
            LDeleteDataSet.SetSQL(LSQLDelete);
            LXMLNode := LXMLDoc.documentElement.ChildNodes.First;

            LXMLRowNode := LXMLNode.NextSibling.ChildNodes;

            for LLinesCount := 0 to LXMLRowNode.Count-1 do
            begin
              LAppendDataSet.DataSet.Append;

              LXMLValueNode :=  LXMLRowNode.Get(LLinesCount);

              for LIndex := 0 to ATableProperty.IndexFieldNames.Count -1 do
              begin

                LFieldName := ATableProperty.IndexFieldNames[LIndex];
                LParamName  := 'A'+LFieldName;
                if (LXMLValueNode.HasAttribute(LFieldName)) then
                  LParamValue := LXMLValueNode.Attributes[LFieldName]
                else
                  LParamValue := ' ';

                LDeleteDataSet.SetParams(LParamName,LParamValue); // ,LXMLValueNode.Attributes[LFieldName].DataType);

              end;

              if LDeleteDataSet.AreAllParamsBound(True) then
              begin
                try
                  LDeleteDataSet.ExecSQL;
                except on E: Exception do
                  begin
                    AProgressUpdateFuntion( E.Message,ptError,LStop);
                    if LStop then Break;
                  end;
                end;
              end;



              try


              for LIndex := 0 to ATableProperty.FieldNames.Count -1 do //LXMLValueNode.AttributeNodes.Count-1 do
              begin
                //
                LFieldName := ATableProperty.FieldNames[LIndex];   //LClientDataSet.DataSet.FieldDefs.Items[LIndex].Name;
                //LParamValue:= Trim(LXMLValueNode.Attributes[LFieldName]);

                //LAttributes := LXMLValueNode.AttributeNodes;
                if (LXMLValueNode.HasAttribute(LFieldName)) then
                  LParamValue := Trim(LXMLValueNode.Attributes[LFieldName])
                else
                  LParamValue := ' ';

                if (LAppendDataSet.DataSet.FieldByName(LFieldName).DataType = ftDateTime) or
                   (LAppendDataSet.DataSet.FieldByName(LFieldName).DataType = ftDate) then
                begin

                  LAppendDataSet.DataSet.FieldByName(LFieldName).AsDateTime := Now; //LParamValue;
                end
                else
                begin
                  if (LParamValue = ' ') and ((LAppendDataSet.DataSet.FieldByName(LFieldName).DataType = ftInteger) or
                      (LAppendDataSet.DataSet.FieldByName(LFieldName).DataType = ftFloat))  then
                    continue; //LParamValue := '0';

                  LAppendDataSet.DataSet.FieldByName(LFieldName).Value := LParamValue;
                end;
              end;


              // make all imported senarios read only.
                if(ATableProperty.TableName = 'StudyScenario') then
                begin
                  LModel := Trim(LAppendDataSet.DataSet.FieldByName('Model').AsString);
                  if ((LModel = CYield) or (LModel = CPlanning))then
                    LAppendDataSet.DataSet.FieldByName('DataImported').Value := 'Y';
                end;
                LAppendDataSet.DataSet.Post;
              except on E: Exception do
                begin
                    AProgressUpdateFuntion( E.Message,ptError,LStop);
                  if LStop then Break;
                end;
              end;

              if((LLinesCount mod 50) = 0) then
              begin
                 AProgressUpdateFuntion('',ptNone,LStop);
                 if LStop then Break;
              end;



            end;
            LAppendDataSet.DataSet.Close;

          finally
            LDeleteDataSet.Free;
            LAppendDataSet.Free;

          end;
        end;
      end;
    finally
      LXMLFileData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.ProcessImportSelectedChangeLists(AXMLFilename: string;
         ATableProperty: TAbstractDbTableProperty;AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TDbTableDataManager.ProcessImportSelectedChangeLists';
var
  LStop               : boolean;
  LAppendDataSet      : TAbstractModelDataset;
  LClientDataSet      : TClientDataSet;
  LLinesCount         : integer;
  LIndex              : integer;
  LFieldName          : string;
begin
  Result := False;
  try
    if not FileExists(AXMLFilename) then
    begin
     AProgressUpdateFuntion(FAppModules.Language.GetString('Message.FileXML') + AXMLFilename +
     FAppModules.Language.GetString('Message.FileDoesNotExist'), ptError, LStop);
     if LStop then Exit;
    end
    else
    begin
      AProgressUpdateFuntion( FAppModules.Language.GetString('Message.ProcessingFile')+ ExtractFileName(AXMLFilename),ptNone,LStop);
      if LStop then Exit;


      LClientDataSet := TClientDataSet.Create(nil);
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LAppendDataSet);
      try
        if Assigned(LAppendDataSet) then
        begin
          LClientDataSet.LoadFromFile(AXMLFilename);
          LClientDataSet.Active := True;
          LAppendDataSet.SetSQL('SELECT * FROM '+ ATableProperty.TableName);
          LAppendDataSet.SetReadOnly(False);
          LAppendDataSet.DataSet.Active := True;

          LLinesCount := 0;
          while not LClientDataSet.Eof do
          begin
            LAppendDataSet.DataSet.Append;
            for LIndex := 0 to LClientDataSet.FieldCount -1 do
            begin
              LFieldName := LClientDataSet.FieldDefs.Items[LIndex].Name;
              if(LFieldName = 'Model') then
                LAppendDataSet.DataSet.FieldByName(LFieldName).Value :=  FAppModules.StudyArea.ModelCode
              else
              if(LFieldName = 'StudyAreaName') then
                LAppendDataSet.DataSet.FieldByName(LFieldName).Value :=  FAppModules.StudyArea.StudyAreaCode
              else
              if(LFieldName = 'SubArea') then
                LAppendDataSet.DataSet.FieldByName(LFieldName).Value :=  FAppModules.StudyArea.SubAreaCode
              else
              if(LFieldName = 'Scenario') then
                LAppendDataSet.DataSet.FieldByName(LFieldName).Value :=  FAppModules.StudyArea.ScenarioCode
              else
                LAppendDataSet.DataSet.FieldByName(LFieldName).Value := LClientDataSet.FieldByName(LFieldName).Value
            end;
            LAppendDataSet.DataSet.Post;

            LLinesCount := LLinesCount + 1;
            if((LLinesCount mod 50) = 0) then
            begin
               AProgressUpdateFuntion('',ptNone,LStop);
               if LStop then Break;
            end;

            LClientDataSet.Next;
          end;
          LAppendDataSet.DataSet.Close;
          Result := True;
        end;
      finally
        LClientDataSet.Free;
        LAppendDataSet.Free;
      end;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTableDataManager.Get_NetworkDiagramsPath: string;
const OPNAME = 'TDbTableDataManager.Get_NetworkDiagramsPath';
begin
  Result := '';
  try
    Result := NetworkDiagramsPath;
    case FSelectionLevel of
      malStudy     : Result := Result +  ChopCharacters(FStudy) + '\';
      malSubArea   : Result := Result +  ChopCharacters(FStudy) + '\' + ChopCharacters(FSubArea)   + '\';
      malScenarion : Result := Result +  ChopCharacters(FStudy) + '\' + ChopCharacters(FSubArea)   + '\' +
                                         ChopCharacters(FScenario)  + '\';
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TDbTableDataManager.ShowIssueListSplashScreen: boolean;
const OPNAME = 'TDbTableDataManager.ShowIssueListSplashScreen';
var
  LIssueForm           : TYieldModelDataGUIForm;
  LIndex,
  LFileIndex,
  LRowIndex,
  LZipIndex            : integer;
  LClientDataSet       : TClientDataSet;
  LOldCursor           : TCursor;
  LDialogValidator     : TCautionarySplashScreenValidator;
  LLoadAgent           : TStudyMetaDataLoadAgent;
  LStudyMetaDataList   : TStudyMetaDataList;
  LStudyMetaData       : TStudyMetaData;
  LTempFilesContainer  : TStringList;
  LFileName            : string;
  LFound               : boolean;
begin
  Result := False;
  try
    LOldCursor         := Screen.Cursor;
    Screen.Cursor      := crHourGlass;
    LIssueForm         := TYieldModelDataGUIForm.CreateWithoutDFM(nil,FAppModules);
    LDialogValidator   := TCautionarySplashScreenValidator.Create(LIssueForm,FAppModules);
    LClientDataSet     := TClientDataSet.Create(nil);
    LLoadAgent         := TStudyMetaDataLoadAgent.Create(FAppModules);
    LStudyMetaDataList := TStudyMetaDataList.Create(FAppModules);
    LTempFilesContainer:= TStringList.Create;
    try
      LIssueForm.Initialise;
      LIssueForm.StudyHasChanged;
      LIssueForm.LanguageHasChanged;
      LIssueForm.AddModelDataDialogValidator(LDialogValidator);
      LDialogValidator.LanguageHasChanged;
      LDialogValidator.CautionarySplashScreenDialog.CheckBoxPanel.Visible := False;
      LIssueForm.BtnOk.Caption     := FAppModules.Language.GetString('ButtonCaption.Accept');
      LIssueForm.BtnCancel.Caption := FAppModules.Language.GetString('ButtonCaption.Decline');

      for LIndex := 0 to FZipFileNameList.Count - 1 do
      begin
        DeleteMultipleFiles(FTempDirectory +'*.*' );
        if CopySelectedFileToTempDir(FZipFileNameList[LIndex]) then
        begin
          if UnZipFiles then
          begin
            LFound := False;
            if SearchFiles(FTempDirectory + CMainScriptPrefix + '*',LTempFilesContainer) then
            begin
              for LZipIndex := 0 to LTempFilesContainer.Count -1 do
              begin
                LFileName := ExtractFileName(LTempFilesContainer[LZipIndex]);
                if (UpperCase(Copy(LFileName, 1, 5)) = CMainScriptPrefix) then
                begin
                  LFound := True;
                  break;
                end;
              end;

              if not LFound then
                raise Exception.Create('Main Script File ' + LFileName + ' not found!')
              else
              begin
                LFileName := LTempFilesContainer[LZipIndex];
                LTempFilesContainer.LoadFromFile(LFileName);
                LFileIndex := LTempFilesContainer.IndexOf('LD_StudyMetaData.xml');
                if (LFound) and (LFileIndex >= 0) then
                begin
                  LClientDataSet.LoadFromFile(FTempDirectory+'LD_StudyMetaData.xml');
                  LLoadAgent.ConstructDataFromDataSet(LStudyMetaDataList,LClientDataSet);
                end;
              end;
            end;
          end;
        end;
      end;

      if(LStudyMetaDataList.StudyMetaDataCount > 0) then
      begin
        LRowIndex := 0;
        LDialogValidator.CautionarySplashScreenDialog.IssueStringGrid.RowCount := LDialogValidator.CautionarySplashScreenDialog.IssueStringGrid.RowCount + LStudyMetaDataList.StudyMetaDataCount;
        for LIndex := 0 to LStudyMetaDataList.StudyMetaDataCount - 1 do
        begin
          LRowIndex      := LRowIndex + 1;
          LStudyMetaData := LStudyMetaDataList.CastStudyMetaDataByIndex[LIndex];
          LDialogValidator.CautionarySplashScreenDialog.IssueStringGrid.Cells[0,LRowIndex]    := LStudyMetaData.StudyAreaName;
          LDialogValidator.CautionarySplashScreenDialog.IssueStringGrid.Cells[1,LRowIndex]    := LStudyMetaData.ImportedBy;
          LDialogValidator.CautionarySplashScreenDialog.IssueStringGrid.Cells[2,LRowIndex]    := LStudyMetaData.ErrorType;
          LDialogValidator.CautionarySplashScreenDialog.IssueStringGrid.Cells[3,LRowIndex]    := LStudyMetaData.ErrorDescription;
          LDialogValidator.CautionarySplashScreenDialog.IssueStringGrid.Cells[4,LRowIndex]    := LStudyMetaData.CorrectiveAction;
          LDialogValidator.CautionarySplashScreenDialog.IssueStringGrid.RowHeights[LRowIndex] := 70;
        end;
        LDialogValidator.CautionarySplashScreenDialog.IssueStringGrid.FixedRows := 1;
        Screen.Cursor := LOldCursor;
        LIssueForm.ShowModal;
        Result := (LIssueForm.ModalResult = mrOk);
      end
      else
      begin
        Screen.Cursor := LOldCursor;
        Result := True;
      end;
    finally
      FreeAndNil(LIssueForm);
      FreeAndNil(LClientDataSet);
      FreeAndNil(LLoadAgent);
      FreeAndNil(LStudyMetaDataList);
      FreeAndNil(LTempFilesContainer);
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TDbTableDataManager.ExecExportConfirmHydrologyFiles(AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TDbTableDataManager.ExecExportConfirmHydrologyFiles';
var
  LMessage : string;
  LResult  : integer;
begin
  Result := False;
  try
    LMessage := 'For your hydrology files to be saved with your study, they must have been imported to the database.'+
                'Please ensure that you have imported the hydrology to the WRMF database before exporting your study.'+
                'Do you want to continue?';
    LResult := MessageDlg(LMessage, mtConfirmation, [mbYes, mbNo], 0);
    if(LResult = mrNo) then
      FAppModules.GlobalData.SetStopOnFirstErr(True)
    else
      Result := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

{function TDbTableDataManager.ExecExportHydrologySubArea(AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TDbTableDataManager.ExecExportHydrologySubArea';
var
  lResult         : boolean;
  lStop           : boolean;
  lIndex          : integer;
  lTable          : TAbstractDbTableProperty;
  lModelTables    : TObjectList;

  lHydroDBManager : THydroDBManager;
  lContainer      : TStringList;
  lErrorMsg       : WideString;
begin
  Result := FALSE;
  try
    if (FStudyArea.ScenarioCode = '') then
    begin
      ShowMessage('We currently can only export networks in hydrology model. SubArea and Study export not yet implemented.');
      Exit;
    end;

    lModelTables := TObjectList.Create(False);
    try
      if FAppModules.DBTablePropertyManager.GetTablesPerModel(FModel, lModelTables) then
      begin
        FProgressDialog.ActionProgressBar.Min := 0;
        FProgressDialog.ActionProgressBar.Max := lModelTables.Count + 2;
        AProgressUpdateFuntion(FAppModules.Language.GetString('Message.LoadingSystemData'), ptNone, lStop);

        lContainer    := TStringList.Create;
        lHydroDBManager := THydroDBManager.Create(FAppModules);
        try
          if not lHydroDBManager.DoesNetworkExist(FStudyArea.CalendarStartMonth) then
          begin
            ShowMessage('Network('+FStudyArea.ScenarioCode+') does not exist in the WRSM2000 database. Action failed');
            Exit;
          end;

          if not lHydroDBManager.ExportNetwork(FStudyArea.CalendarStartMonth,FTempDirectory,lErrorMsg)then
          begin
            if(lErrorMsg <> '') then
              ShowMessage(lErrorMsg);
            Exit;
          end;
          SearchFiles(FTempDirectory+'*.*',lContainer);
          for LIndex := 0 to lContainer.Count-1 do
            FScriptList.Add(ExtractFileName(lContainer[LIndex]));
        finally
          FreeAndNil(lContainer);
          FreeAndNil(lHydroDBManager);
        end;


        for lIndex := 0 to lModelTables.Count - 1 do
        begin
          lTable := TAbstractDbTableProperty(lModelTables.Items[lIndex]);
          AProgressUpdateFuntion(FAppModules.Language.GetString('Message.ProcessingTable') + lTable.TableName, ptNone, lStop);
          if lStop then Exit;
          lResult := ProcessExportGeneralTable(lTable);
          if (NOT UpdateProgress(lResult)) then Exit;
        end;

        lResult := True;
        AProgressUpdateFuntion(FAppModules.Language.GetString('Message.CreatingScriptListFile'), ptNone, lStop);
        lResult := lResult AND CreateScriptListFile;
        AProgressUpdateFuntion(FAppModules.Language.GetString('Message.CreatingDocumentsListFile'), ptNone, lStop);
        lResult := lResult AND CreateDocsListFile;
        if not UpdateProgress(lResult) then Exit;
        AProgressUpdateFuntion(FAppModules.Language.GetString('Message.CreatingZipFile'), ptNone, lStop);
        lResult := lResult AND CreateSubAreaZip;
        if not UpdateProgress(lResult) then Exit;
        Result := lResult;
        AProgressUpdateFuntion(FAppModules.Language.GetString('Message.Done'), ptNone, lStop);
      end;
    finally
      lModelTables.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

{function TDbTableDataManager.ExecDeleteHydrologySubArea(AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TDbTableDataManager.ExecDeleteHydrologySubArea';
var
  lHydroDBManager : THydroDBManager;
  lContainer      : TStringList;
  lResult         : boolean;
  lErrorMsg       : WideString;
  lStop           : boolean;
begin
  Result := FALSE;
  try
    if (FStudyArea.ScenarioCode = '') then
    begin
      ShowMessage('We currently can only delete networks in hydrology model. SubArea and Study deletion not yet implemented.');
      Exit;
    end;

    AProgressUpdateFuntion(FAppModules.Language.GetString('Message.Deleting'), ptNone, lStop);
    lHydroDBManager := THydroDBManager.Create(FAppModules);
    lContainer      := TStringList.Create;
    try
      if not lHydroDBManager.DoesNetworkExist(FStudyArea.CalendarStartMonth) then
      begin
        ShowMessage('Network('+FStudyArea.ScenarioCode+') does not exist in the WRSM2000 database. Action failed');
        Exit;
      end;
      if not lHydroDBManager.DeleteNetwork(FStudyArea.CalendarStartMonth,lErrorMsg)then
      begin
        if(lErrorMsg <> '') then
          ShowMessage(lErrorMsg);
        Exit;
      end;
    finally
      FreeAndNil(lContainer);
      FreeAndNil(lHydroDBManager);
    end;

    if (GetScenarioCountPerStudy = 1) then
    begin
      lResult := DeleteStudyTable and
                 DeleteSubAreaTable and
                 DeleteScenarioTable;
    end
    else
    begin
      lResult := DeleteScenarioTable;
    end;
    Result := lResult;
    AProgressUpdateFuntion(FAppModules.Language.GetString('Message.Done'), ptNone, lStop);
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

{function TDbTableDataManager.ExecImportHydrologySubArea(AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TDbTableDataManager.ExecImportHydrologySubArea';
var
  LIndex              : integer;
  LFound              : boolean;
  LTableName          : string;
  LTempFilesContainer : TStringList;
  LFilesContainer     : TStringList;
  LModelTables        : TObjectList;
  LTableProperty      : TAbstractDbTableProperty;
  LResult             : boolean;
  LStop               : boolean;

  LFileName           : string;
  LStudyAreaCode      : string;
  lNetworkID          : integer;
  lHydroDBManager     : THydroDBManager;
  LClientDataSet      : TClientDataSet;
  lErrorMsg           : WideString;
begin
  Result := False;
  try
    if (UnZipFiles) then
    begin
      LFileName := FTempDirectory + 'LD_StudyScenario.xml';
      if not FileExists(LFileName) then
      begin
        ShowMessage('This does not seem to be a valid Hydrology export as file('+LFileName+') does not exist.');
        Exit;
      end;

      LClientDataSet := TClientDataSet.Create(nil);
      try
        LClientDataSet.LoadFromFile(LFileName);
        LClientDataSet.Active := True;
        if LClientDataSet.Bof and LClientDataSet.Eof then
        begin
          ShowMessage('This does not seem to be a valid Hydrology export as are no networks in '+ LFileName);
          Exit;
        end;
        lNetworkID     := LClientDataSet.FieldByName('CalenderStartMonth').AsInteger;
        LStudyAreaCode := Trim(LClientDataSet.FieldByName('Scenario').AsString);
      finally
        LClientDataSet.Free;
      end;

      lHydroDBManager := THydroDBManager.Create(FAppModules);
      try
        if not lHydroDBManager.DoesNetworkExist(lNetworkID) then
        begin
          ShowMessage('Network('+LStudyAreaCode+') does already exist in the WRSM2000 database. Action failed');
          Exit;
        end;

        if not lHydroDBManager.ImportNetwork(FTempDirectory,lNetworkID,lErrorMsg)then
        begin
          if(lErrorMsg <> '') then
            ShowMessage(lErrorMsg);
          Exit;
        end;
      finally
        FreeAndNil(lHydroDBManager);
      end;

      LFilesContainer     := TStringList.Create;
      LTempFilesContainer := TStringList.Create;
      try
        LFound := False;
        if SearchFiles(FTempDirectory + CMainScriptPrefix + '*', LTempFilesContainer) then
        begin

          for LIndex := 0 to LTempFilesContainer.Count -1 do
          begin
            LFileName := ExtractFileName(LTempFilesContainer[LIndex]);
            if (UpperCase(Copy(LFileName, 1, 5)) = CMainScriptPrefix) then
            begin
              LFound := True;
              break;
            end;
          end;

          if not LFound then
            raise Exception.Create('Main Script File ' + LFileName + ' not found!')
          else
          begin
            LFileName := LTempFilesContainer[LIndex];
            LTempFilesContainer.LoadFromFile(LFileName);
            LModelTables := TObjectList.Create(False);
            try
              //First line is the table version
              for LIndex := 1 to LTempFilesContainer.Count -1 do
              begin
                LTableName     := TableNameFromFileName(LTempFilesContainer[LIndex]);
                if (UpperCase(LTableName) = 'STUDYAREA') or
                   (UpperCase(LTableName) = 'STUDYMODEL') or
                   (UpperCase(LTableName) = 'STUDYSUBAREA') or
                   (UpperCase(LTableName) = 'STUDYSCENARIO') then
                begin
                  LTableProperty := FAppModules.DBTablePropertyManager.TablePropertyByName[LTableName];
                  if (LTableProperty = nil) then
                  begin
                    AProgressUpdateFuntion(FAppModules.Language.GetString('Message.Table') + LTableName +
                     FAppModules.Language.GetString('Message.DoesNotHavePSroperties'), ptError, LStop);
                    if LStop then Exit;
                  end
                  else
                  begin
                    LFilesContainer.Add(LTempFilesContainer[LIndex]);
                    LModelTables.Add(LTableProperty);
                  end;
                end;
              end;

              FProgressDialog.ActionProgressBar.Max := LModelTables.Count + 1;
              for LIndex := 0 to LModelTables.Count -1 do
              begin
                LTableProperty := TAbstractDbTableProperty(LModelTables[LIndex]);
                AProgressUpdateFuntion(FAppModules.Language.GetString('Message.ProcessingTable') + LTableProperty.TableName, ptNone, LStop);
                if LStop then Exit;

                LResult := ProcessImportTableData(LFilesContainer[LIndex], LTableProperty,AProgressUpdateFuntion);
                if not UpdateProgress(LResult) then Exit;
              end;
              Result := TRUE;
            finally
              LModelTables.Free;
            end;
          end;
        end;
      finally
        LFilesContainer.Free;
        LTempFilesContainer.Free;
      end;
      DeleteFile(FZipFileName);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

{function TDbTableDataManager.ExecCopyHydrologySubArea(AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TDbTableDataManager.ExecCopyHydrologySubArea';
var
  lHydroDBManager : THydroDBManager;
  lOldNetworkCode : WideString;
  lNewNetworkCode : WideString;
  lErrorMsg       : WideString;
  lStop           : boolean;
begin
  Result := FALSE;
  try
    if (FStudyArea.ScenarioCode = '') then
    begin
      ShowMessage('We currently can only copy networks in hydrology model. SubArea and Study copying not yet implemented.');
      Exit;
    end;

    AProgressUpdateFuntion(FAppModules.Language.GetString('Message.Copying'), ptNone, lStop);
    lHydroDBManager := THydroDBManager.Create(FAppModules) ;
    try
      lOldNetworkCode := Copy(FStudyArea.ScenarioCode,1,3);
      lNewNetworkCode := Copy(FStudyFields.FScenario,1,3);
      if lHydroDBManager.DoesNetworkExist(lNewNetworkCode) then
      begin
        ShowMessage('Network('+lNewNetworkCode+') already exist in the WRSM2000 database. Action failed');
        Exit;
      end;
      if not lHydroDBManager.CopyNetwork(FStudyArea.CalendarStartMonth,lNewNetworkCode,lErrorMsg)then
      begin
        if(lErrorMsg <> '') then
          ShowMessage(lErrorMsg);
        Exit;
      end;
    finally
      FreeAndNil(lHydroDBManager);
    end;
    Result := CopyScenarioTable;
    AProgressUpdateFuntion(FAppModules.Language.GetString('Message.Done'), ptNone, lStop);
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

{function TDbTableDataManager.CreateHydrologyNetwork(AStudyFields: TStudyFields): boolean;
const OPNAME = 'TDbTableDataManager.CreateHydrologyNetwork';
var
  lHydroDBManager     : THydroDBManager;
  lReadOnly           : integer;
  lNetworkID          : integer;
  lNetworkCode        : WideString;
  lErrorMsg           : WideString;

begin
  Result := False;
  try

    lHydroDBManager := THydroDBManager.Create(FAppModules);
    try
      lNetworkCode := Copy(AStudyFields.FScenario,1,3);
      if lHydroDBManager.DoesNetworkExist(lNetworkCode) then
      begin
        ShowMessage('Network('+lNetworkCode+') does already exist in the WRSM2000 database. Action failed');
        Exit;
      end;
      if AStudyFields.FEditable then
        lReadOnly := 0
      else
        lReadOnly := 1;
      if not lHydroDBManager.CreateNewNetwork(lNetworkCode,
                                             StrToIntDef(AStudyFields.FVersion,1),
                                             AStudyFields.FInputDir,
                                             AStudyFields.FOutputDir,
                                             AStudyFields.FDebugRequired,
                                             AStudyFields.FDebugStartPeriod,
                                             AStudyFields.FDebudEndPeriod,
                                             AStudyFields.FSummaryRequired,
                                             AStudyFields.FSimulationStartYear,
                                             AStudyFields.FSimulationEndYear,
                                             lReadOnly,
                                             lNetworkID,
                                             lErrorMsg)then
      begin
        if(lErrorMsg <> '') then
          ShowMessage(lErrorMsg);
        Exit;
      end;
      AStudyFields.FCalenderStartMonth := lNetworkID;
      Result := True;
    finally
      FreeAndNil(lHydroDBManager);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;}


{function TDbTableDataManager.UpdateHydrologyNetwork(AStudyFields: TStudyFields): boolean;
const OPNAME = 'TDbTableDataManager.CreateHydrologyNetwork';
var
  lHydroDBManager     : THydroDBManager;
  lReadOnly           : String;
  lNetworkCode        : WideString;
  lErrorMsg           : WideString;
  LNetworkProperties  : TStringList;

begin
  Result := False;
  try

    lHydroDBManager := THydroDBManager.Create(FAppModules);
    try
      lNetworkCode := Copy(AStudyFields.FScenario,1,3);
      if not lHydroDBManager.DoesNetworkExist(lNetworkCode) then
      begin
        ShowMessage('Network('+AStudyFields.FScenario+') does not exist in the WRSM2000 database. Action failed');
        Exit;
      end;
      if AStudyFields.FEditable then
        lReadOnly := '0'
      else
        lReadOnly := '1';

      LNetworkProperties  := TStringList.Create;
      try

        LNetworkProperties.Add('NetworkID='+IntToStr(AStudyFields.FCalenderStartMonth));
        LNetworkProperties.Add('NetworkCode='+lNetworkCode);
        LNetworkProperties.Add('VersionNo='+AStudyFields.FVersion);
        LNetworkProperties.Add('InputDirectory='+AStudyFields.FInputDir);
        LNetworkProperties.Add('OutputDirectory='+AStudyFields.FOutputDir);
        LNetworkProperties.Add('DebugRequired='+AStudyFields.FDebugRequired);
        LNetworkProperties.Add('DebugStartPeriod='+IntToStr(AStudyFields.FDebugStartPeriod));
        LNetworkProperties.Add('DebugEndPeriod='+IntToStr(AStudyFields.FDebudEndPeriod));
        LNetworkProperties.Add('SummaryRequired='+AStudyFields.FSummaryRequired);
        LNetworkProperties.Add('SimulationStartYear='+IntToStr(AStudyFields.FSimulationStartYear));
        LNetworkProperties.Add('SimulationEndYear='+IntToStr(AStudyFields.FSimulationEndYear));
        LNetworkProperties.Add('IsReadOnly='+lReadOnly);
        if not lHydroDBManager.Set_NetworkPropertiesCommText(LNetworkProperties.CommaText)then
        begin
          if(lErrorMsg <> '') then
            ShowMessage(lErrorMsg);
          Exit;
        end;
      finally
        LNetworkProperties.Free;
      end;
      Result := True;
    finally
      FreeAndNil(lHydroDBManager);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

{function TDbTableDataManager.LinkHydrologyStudies(ASelectionLevel: TModelActionLevel; AStudyArea: TAbstractStudyArea): boolean;
const OPNAME = 'TDbTableDataManager.LinkHydrologyStudies';
var
  LDataSet            : TAbstractModelDataset;
  LHydroDBManager     : THydroDBManager;
  LNetworkContainer   : TStringList;
  LScenarionContainer : TStringList;
  LNewContainer       : TStringList;
  LIndex              : integer;
  LCount              : integer;
  lSQL                : string;
  LScenarioCode       : string;
begin
  Result := FALSE;
  try
    if(AStudyArea = nil) then
      raise Exception.Create('StudyArea object parameter is not yet assigned.');

    if (ASelectionLevel <> malScenarion) or (AStudyArea.ScenarioCode = '') then
    begin
      ShowMessage('We currently can only import networks in hydrology model when you have selected a Hydrology scenario tree node.');
      Exit;
    end;

    lHydroDBManager        := THydroDBManager.Create(FAppModules);
    LNetworkContainer      := TStringList.Create;
    LScenarionContainer    := TStringList.Create;
    LNewContainer          := TStringList.Create;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LNetworkContainer.Sorted := True;
      LNetworkContainer.Duplicates := dupAccept;
      LNetworkContainer.CommaText := lHydroDBManager.Get_AllNetworkCodeIDsNameValuePairs;
      LCount        := 0;
      LScenarioCode := '';
      for LIndex := 0 to LNetworkContainer.Count-1 do
      begin
        if(LScenarioCode = LNetworkContainer.Names[LIndex]) then
        begin
           LCount := LCount+1;
           LNewContainer.Add(LNetworkContainer.Names[LIndex]+'_'+ IntToStr(LCount)+ '='+
                                        LNetworkContainer.ValueFromIndex[LIndex]);
        end
        else
        begin
          LNewContainer.Add(LNetworkContainer[LIndex]);
          if(LCount > 0) then
            LCount := 0;
          LScenarioCode := LNetworkContainer.Names[LIndex];
        end;
      end;
      LNetworkContainer.Assign(LNewContainer);;
      LNewContainer.Clear;

      lSQL := 'SELECT  * FROM StudyScenario WHERE'+
              ' Model         = ' + QuotedStr(CHydrology)+
              ' AND  StudyAreaName = ' + QuotedStr(AStudyArea.StudyAreaCode) +
              ' AND SubArea   = ' + QuotedStr(AStudyArea.SubAreaCode);
      LDataSet.ClearSQL;
      LDataSet.SetSQL(lSQL);
      LDataset.DataSet.Open;
      while not LDataset.DataSet.Eof do
      begin
        LScenarionContainer.Add(Trim(LDataset.DataSet.FieldByName('CalenderStartMonth').AsString));
        LDataset.DataSet.Next;
      end;

      for LIndex := 0 to LNetworkContainer.Count-1 do
      begin
        if (LScenarionContainer.IndexOf(LNetworkContainer.ValueFromIndex[LIndex]) < 0) then
        begin
          LNewContainer.Add(LNetworkContainer[LIndex]);
        end;
      end;

      if(LNewContainer.Count = 0) then
      begin
        ShowMessage('There are no new networks to import in the database. All networks are imported');
        Exit;
      end;

      frmImportHydrologyStudies := TfrmImportHydrologyStudies.Create(nil);
      try
        frmImportHydrologyStudies.PopulateEditDialog(LNewContainer);
        LNewContainer.Clear;
        LNewContainer.Sorted := True;
        LNewContainer.Duplicates := dupIgnore;
        frmImportHydrologyStudies.DialogType := dtLink;
        frmImportHydrologyStudies.ShowModal;
        if(frmImportHydrologyStudies.ModalResult = mrOk) then
          frmImportHydrologyStudies.Get_SelectedNetworkd(LNewContainer);
      finally
        FreeAndNil(frmImportHydrologyStudies);
      end;

      for LIndex := 0 to LNewContainer.Count-1 do
      begin
        lSQL := 'INSERT INTO StudyScenario '+
                '(Model,StudyAreaName,SubArea,Scenario,ScenarioLabel, ScenarioDescr, FilesLoaded,'+
                ' CalenderStartMonth,Version, DataImported) ' +
                'VALUES (' +
                QuotedStr(AStudyArea.ModelCode) + ',' +
                QuotedStr(AStudyArea.StudyAreaCode) + ',' +
                QuotedStr(AStudyArea.SubAreaCode) + ',' +
                QuotedStr(LNewContainer.Names[LIndex]) + ',' +
                QuotedStr(LNewContainer.Names[LIndex]) + ',' +
                QuotedStr(LNewContainer.Names[LIndex]) + ',' +
                QuotedStr('N') + ',' +
                LNewContainer.ValueFromIndex[LIndex] + ',' +
                QuotedStr('1') + ',' +
                QuotedStr('N') + ')';
        LDataset.DataSet.Close;
        LDataset.SetSQL(lSQL);
        LDataset.ExecSQL;
      end;
      Result := True;
    finally
      LDataset.Free;
      FreeAndNil(LNetworkContainer);
      FreeAndNil(LScenarionContainer);
      FreeAndNil(LNewContainer);
      FreeAndNil(lHydroDBManager);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

{function TDbTableDataManager.UnLinkHydrologyStudies(ASelectionLevel: TModelActionLevel; AStudyArea: TAbstractStudyArea): boolean;
const OPNAME = 'TDbTableDataManager.UnLinkHydrologyStudies';
var
  LDataSet            : TAbstractModelDataset;
  LNewContainer       : TStringList;
  LIndex              : integer;
  lSQL                : string;
begin
  Result := FALSE;
  try
    if(AStudyArea = nil) then
      raise Exception.Create('StudyArea object parameter is not yet assigned.');

    if (ASelectionLevel <> malScenarion) or (AStudyArea.ScenarioCode = '') then
    begin
      ShowMessage('We currently can only import networks in hydrology model when you have selected a Hydrology scenario tree node.');
      Exit;
    end;

    LNewContainer          := TStringList.Create;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LNewContainer.Sorted := True;
      LNewContainer.Duplicates := dupAccept;
      lSQL := 'SELECT  * FROM StudyScenario WHERE'+
              ' Model         = ' + QuotedStr(CHydrology)+
              ' AND  StudyAreaName = ' + QuotedStr(AStudyArea.StudyAreaCode) +
              ' AND SubArea   = ' + QuotedStr(AStudyArea.SubAreaCode);
      LDataSet.ClearSQL;
      LDataSet.SetSQL(lSQL);
      LDataset.DataSet.Open;
      while not LDataset.DataSet.Eof do
      begin
        LNewContainer.Add(Trim(LDataset.DataSet.FieldByName('Scenario').AsString)+'='+
                          Trim(LDataset.DataSet.FieldByName('CalenderStartMonth').AsString));
        LDataset.DataSet.Next;
      end;

      frmImportHydrologyStudies := TfrmImportHydrologyStudies.Create(nil);
      try
        frmImportHydrologyStudies.PopulateEditDialog(LNewContainer);
        LNewContainer.Clear;
        LNewContainer.Sorted := True;
        LNewContainer.Duplicates := dupIgnore;
        frmImportHydrologyStudies.DialogType := dtUnLink;
        frmImportHydrologyStudies.ShowModal;
        if(frmImportHydrologyStudies.ModalResult = mrOk) then
          frmImportHydrologyStudies.Get_SelectedNetworkd(LNewContainer);
      finally
        FreeAndNil(frmImportHydrologyStudies);
      end;

      for LIndex := 0 to LNewContainer.Count-1 do
      begin
        lSQL := 'DELETE FROM StudyScenario WHERE'+
                ' Model              = ' + QuotedStr(CHydrology)+
                ' AND StudyAreaName  = ' + QuotedStr(AStudyArea.StudyAreaCode) +
                ' AND SubArea        = ' + QuotedStr(AStudyArea.SubAreaCode)+
                ' AND Scenario       = ' + QuotedStr(LNewContainer.Names[LIndex]);

        LDataset.DataSet.Close;
        LDataset.SetSQL(lSQL);
        LDataset.ExecSQL;
      end;
      Result := True;
    finally
      LDataset.Free;
      FreeAndNil(LNewContainer);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

end.


