//
//
//  UNIT      : Contains TStudyDocumentManager Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 04/12/2001
//  COPYRIGHT : Copyright © 2001 DWAF
//
//
unit UStudyDocumentManager;

interface

uses
  Classes,
  UAbstractObject,
  UMenuItemManager,
  UAbstractModelObjects,
  UStudyDocumentMenuItemManager;

type
  TStudyDocumentManager = class(TAbstractStudyDocumentManager)
  protected
    FMenuItemManager: TStudyDocumentsMenuItemManager;
    FDocumentIdentifiers: TStudyDocumentDetailList;
    function CreateDocument(AMainKey, ASubKey, AFilename, AMenuCaption: string): TStudyDocumentDetail;
    function IsWordReaderInstalled: boolean;
    function IsAcrobatReaderInstalled: boolean;
    function AddReportToDB(ADocumentDetail:TStudyDocumentDetail): boolean;
    function EditReportInDB(ADocumentDetail:TStudyDocumentDetail): boolean;
    function DeleteReportFromDB(ADocumentDetail:TStudyDocumentDetail): boolean;
    procedure CreateMenuItems;
    function GetCategoriesFromFiles(ACategory: TStrings): boolean;
    function GetCategoriesFromDB(ACategory: TStrings): boolean;
    function GetAllCategories(ACategory: TStrings): boolean;
  public
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function Initialise: boolean; override;
    function LaunchReport(AData: TObject): boolean; override;
    function ViewFile(AFileNAme: string): boolean; override;
    function AddReport: boolean; override;
    function DeleteReport: boolean; override;
    function EditReport: boolean; override;
  end;

implementation

uses
  DB,
  System.UITypes,
  Vcl.Forms,
  Registry,
  Vcl.Dialogs,
  Windows,
  SysUtils,
  Vcl.Controls,
  ShellApi,
  UUtilities,
  UDataSetType,
  UAddReportForm,
  UEditReportForm,
  UDeleteReportForm,
  UAbstractComponent,
  UPDFDocumentLauncher,
  UWordDocumentLauncher,
  UMainMenuEventType,
  UErrorHandlingOperations;

{ TStudyDocumentManager }

procedure TStudyDocumentManager.CreateMemberObjects;
const OPNAME = 'TStudyDocumentManager.CreateMemberObjects';
begin
  try
    FMenuItemManager     := TStudyDocumentsMenuItemManager.Create(FAppModules);
    FDocumentIdentifiers := TStudyDocumentDetailList.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyDocumentManager.DestroyMemberObjects;
const OPNAME = 'TStudyDocumentManager.DestroyMemberObjects';
begin
  try
    FreeAndNil(FMenuItemManager);
    FDocumentIdentifiers.Clear;
    FreeAndNil(FDocumentIdentifiers);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyDocumentManager.Initialise: boolean;
const OPNAME = 'TStudyDocumentManager.Initialise';
begin
  Result := inherited Initialise;
  try
    CreateMenuItems;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyDocumentManager.CreateMenuItems;
const OPNAME = 'TStudyDocumentManager.CreateMenuItems';
var
  LDocumentsDataSet         : TAbstractModelDataset;
  LStudyDocumentLinksDataSet: TAbstractModelDataset;
  LDocumentDetail           : TStudyDocumentDetail;
  LCategoryKey,LFilename: string;
begin
  try
    LDocumentsDataSet := nil;
    try
      FDocumentIdentifiers.Clear;
      FMenuItemManager.DeleteMenuItems;
      if Assigned(FAppModules.Database()) and (FAppModules.Database.Connected) then
      begin
        FAppModules.Database.CreateDataset(integer(dtStudyDocumentsMenu), LDocumentsDataSet);
        FAppModules.Database.CreateDataset(integer(dtStudyDocumentsLinks), LStudyDocumentLinksDataSet);
        if Assigned(LDocumentsDataSet) and Assigned(LStudyDocumentLinksDataSet) then
        begin
          FAppModules.StudyArea.SetDefaultParams(LDocumentsDataSet);
          LDocumentsDataSet.DataSet.Open;
          while (not LDocumentsDataSet.DataSet.EOF) do
          begin
            FDocumentIdentifiers.Add(
              CreateDocument(
                Trim(LDocumentsDataSet.DataSet.FieldByName('Category').AsString),
                Trim(LDocumentsDataSet.DataSet.FieldByName('Identifier').AsString),
                Trim(LDocumentsDataSet.DataSet.FieldByName('Filename').AsString),
                Trim(LDocumentsDataSet.DataSet.FieldByName('MenuCaption').AsString)));
            LDocumentsDataSet.DataSet.Next;
          end;
          LDocumentsDataSet.DataSet.Close;
          LStudyDocumentLinksDataSet.DataSet.Open;
          if not (LStudyDocumentLinksDataSet.DataSet.Eof and
                  LStudyDocumentLinksDataSet.DataSet.Bof) then
          begin
            while (not LStudyDocumentLinksDataSet.DataSet.EOF) do
            begin
              LCategoryKey := Trim(LStudyDocumentLinksDataSet.DataSet.FieldByName('Category').AsString);
              LFilename := Trim(LStudyDocumentLinksDataSet.DataSet.FieldByName('Filename').AsString);
              LDocumentDetail := FDocumentIdentifiers.GetDocument(LCategoryKey,LFilename);
              if Assigned(LDocumentDetail) then
              begin
                LDocumentDetail.BookMark := Trim(LStudyDocumentLinksDataSet.DataSet.FieldByName('Bookmark').AsString);
                LDocumentDetail.PageNumber := LStudyDocumentLinksDataSet.DataSet.FieldByName('PageNumber').AsInteger;
              end;
              LStudyDocumentLinksDataSet.DataSet.Next;
            end;
          end;
          FMenuItemManager.AddMenuItems(FDocumentIdentifiers);
        end;
      end;
    finally
      LDocumentsDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TStudyDocumentManager.ViewFile(AFileNAme: string): boolean;
const OPNAME = 'TStudyDocumentManager.ViewFile';
begin
  Result := False;
  try
    AFileNAme := Trim(AFileNAme);
    if(AFileNAme = '') then
    begin
      ShowMessage('File name is empty. Cannot view documnet');
      Exit;
    end
    else
    begin
      if not FileExists(AFileNAme) then
      begin
        ShowMessage('File ('+AFileNAme+') does not exists on disk.');
        Exit;
      end;

      ShellExecute(Application.Handle, 'open', PChar(AFileNAme) ,nil,nil,SW_SHOWNORMAL);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyDocumentManager.LaunchReport(AData: TObject): boolean;
const OPNAME = 'TStudyDocumentManager.LaunchReport';
var
  //LSQL,
  LFullFilename: string;
  LDocument      : TStudyDocumentDetail;
  //LDataSet       : TAbstractModelDataset;
begin
  Result := False;
  try
    if (not Assigned(AData)) then
    begin
      raise Exception.Create('Document detail object not assigned.');
    end
    else
    begin
      LDocument := TStudyDocumentDetail(AData);
      LFullFilename := GetReportFileName(FAppModules.StudyArea.StudyAreaCode,LDocument.CategoryKey,LDocument.Filename);
      {if not FileExists(LFullFilename) then
      begin
        if FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet) then
        begin
          try
            LSQL := 'SELECT * FROM StudyDocuments WHERE '+
                    ' StudyAreaName     = '+ QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
                    ' AND Category      = '+ QuotedStr(LDocument.CategoryKey)+
                    ' AND Identifier    = '+ QuotedStr(LDocument.IdentifierKey);
            LDataSet.SetSQL(LSQL);
            LDataSet.SetReadOnly(False);
            LDataSet.DataSet.Open;
            if not(LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
            begin
              TBlobField(LDataSet.DataSet.FieldByName('Document')).SaveToFile(LFullFilename);
            end;
          finally
            FreeAndNil(LDataSet);
          end;
        end;
      end;}

      if not FileExists(LFullFilename) then
      begin
        raise Exception.Create('Report does not exists on disk and in the database.');
      end;

      ShellExecute(Application.Handle, 'open', PChar(LFullFilename) ,nil,nil,SW_SHOWNORMAL);

      // Launch using the correct launcher.
      {case TStudyDocumentDetail(AData).ReportType of
        rtAcrobat:
          begin
            if IsAcrobatReaderInstalled  then
             TPDFDocumentLauncher.Launch(LFullFilename, TStudyDocumentDetail(AData).PageNumber);
          end;
        rtWord:
          begin
            if IsWordReaderInstalled  then
              TWordDocumentLauncher.Launch(LFullFilename, TStudyDocumentDetail(AData).BookMark);
          end;
      else
        raise Exception.CreateFmt('Document type [%s] not supported.', [TStudyDocumentDetail(AData).Filename]);
      end;}
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyDocumentManager.CreateDocument(AMainKey, ASubKey, AFilename, AMenuCaption: string): TStudyDocumentDetail;
const OPNAME = 'TStudyDocumentManager.CreateDocument';
begin
  Result := nil;
  try
    Result := TStudyDocumentDetail.Create;
    Result.CategoryKey   := AMainKey;
    Result.IdentifierKey := ASubKey;
    Result.Filename      := AFilename;
    Result.MenuCaption   := AMenuCaption;
    Result.BookMark      := '';
    Result.PageNumber    := -1;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyDocumentManager.IsAcrobatReaderInstalled: boolean;
const OPNAME = 'TStudyDocumentManager.IsAcrobatReaderInstalled';
var
  LTheRegistry: TRegistry;
  LKeys: TStringList;
  LMessage: string;
  LIndex: integer;
begin
  Result := False;
  try
    LKeys  := TStringList.Create;
    LTheRegistry:=TRegistry.Create;
    try
      LTheRegistry.RootKey:=HKEY_CLASSES_ROOT;
      LTheRegistry.OpenKeyReadOnly('');
      try
        LTheRegistry.GetKeyNames(LKeys);
      finally
        LTheRegistry.CloseKey;
      end;

      for LIndex := 0 to LKeys.Count-1 do
      begin
        if(Pos('AcroExch.Document',LKeys[LIndex]) = 1) then
        begin
          Result := True;
          Break;
        end;
      end;
      //Result:=LTheRegistry.KeyExists('AcroExch.Document');
    finally
      LKeys.Free;
      LTheRegistry.Free;
    end;
    if not Result then
    begin
      LMessage := FAppModules.Language.GetString('TStudyDocumentManager.AcrobatReaderNotInstalled');
      MessageDlg(LMessage,mtError,[mbOK],0);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyDocumentManager.IsWordReaderInstalled: boolean;
const OPNAME = 'TStudyDocumentManager.IsWordReaderInstalled';
var
  LTheRegistry: TRegistry;
  LMessage: string;
begin
  Result := False;
  try
    LTheRegistry:=TRegistry.Create;
    try
      LTheRegistry.RootKey:=HKEY_CLASSES_ROOT;
      Result:=LTheRegistry.KeyExists('Word.Application');
    finally
      LTheRegistry.Free;
    end;
    if not Result then
    begin
      LMessage := FAppModules.Language.GetString('TStudyDocumentManager.WordReaderNotInstalled');
      MessageDlg(LMessage,mtError,[mbOK],0);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyDocumentManager.AddReport: boolean;
const OPNAME = 'TStudyDocumentManager.AddReport';
var
  LAddReportForm  : TfrmAddReportForm;
  LDocumentDetail : TStudyDocumentDetail;
  LCategory       : TStringList;
begin
  Result := False;
  try
    LDocumentDetail := TStudyDocumentDetail.Create;
    LAddReportForm  := TfrmAddReportForm.Create(nil,FAppModules);
    LCategory       := TStringList.Create;
    try
      LCategory.Sorted := True;
      LCategory.Duplicates := dupIgnore;
      LAddReportForm.DocumentIdentifiers := FDocumentIdentifiers;
      LAddReportForm.LanguageHasChanged;
      GetAllCategories(LCategory);
      LAddReportForm.PopulateDialog(LCategory,LDocumentDetail);
      LAddReportForm.ShowModal;
      if(LAddReportForm.ModalResult = mrOK) then
      begin
        if LAddReportForm.PopulateDocumentDetail(LDocumentDetail) then
        begin
          if AddReportToDB(LDocumentDetail) then
          begin
            CreateMenuItems;
            Result := True;
          end;
        end;
      end;
    finally
      FreeAndNil(LDocumentDetail);
      FreeAndNil(LAddReportForm);
      FreeAndNil(LCategory);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyDocumentManager.EditReport: boolean;
const OPNAME = 'TStudyDocumentManager.EditReport';
var
  LEditReportForm : TfrmEditReportForm;
  LDocumentDetail : TStudyDocumentDetail;
  LCategory       : TStringList;
begin
  Result := False;
  try
    LDocumentDetail := TStudyDocumentDetail.Create;
    LEditReportForm  := TfrmEditReportForm.Create(nil,FAppModules);
    LCategory       := TStringList.Create;
    try
      LCategory.Sorted := True;
      LCategory.Duplicates := dupIgnore;
      LEditReportForm.DocumentIdentifiers := FDocumentIdentifiers;
      LEditReportForm.LanguageHasChanged;
      GetAllCategories(LCategory);
      LEditReportForm.PopulateDialog(LCategory,LDocumentDetail);
      LEditReportForm.ShowModal;
      if(LEditReportForm.ModalResult = mrOK) then
      begin
        if LEditReportForm.PopulateDocumentDetail(LDocumentDetail) then
        begin
          if EditReportInDB(LDocumentDetail) then
          begin
            CreateMenuItems;
            Result := True;
          end;
        end;
      end;
    finally
      FreeAndNil(LDocumentDetail);
      FreeAndNil(LEditReportForm);
      FreeAndNil(LCategory);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyDocumentManager.DeleteReport: boolean;
const OPNAME = 'TStudyDocumentManager.DeleteReport';
      DeleteFilesMsg = 'Document link has been removed. Do you want to delete the document file on your hard drive as well?';
var
  LDeleteReportForm : TfrmDeleteReportForm;
  LDocumentDetail   : TStudyDocumentDetail;
  LFileName: string;
begin
  Result := False;
  try
    LDocumentDetail   := TStudyDocumentDetail.Create;
    LDeleteReportForm := TfrmDeleteReportForm.Create(nil,FAppModules);
    try
      LDeleteReportForm.LanguageHasChanged;
      LDeleteReportForm.PopulateDialog(FDocumentIdentifiers);
      LDeleteReportForm.ShowModal;
      if(LDeleteReportForm.ModalResult = mrOK) then
      begin
        if LDeleteReportForm.PopulateDocumentDetail(LDocumentDetail) then
        begin
          if DeleteReportFromDB(LDocumentDetail) then
          begin
            CreateMenuItems;
            if (MessageDlg(DeleteFilesMsg,mtConfirmation,mbOKCancel,0) = mrOk) then
            begin
              LFileName := GetReportFileName(FAppModules.StudyArea.StudyAreaCode,LDocumentDetail.CategoryKey,LDocumentDetail.Filename);
              DeleteFile(LFileName);
            end;
            Result := True;
          end;
        end;
      end;
    finally
      FreeAndNil(LDocumentDetail);
      FreeAndNil(LDeleteReportForm);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyDocumentManager.GetAllCategories(ACategory: TStrings): boolean;
const OPNAME = 'TStudyDocumentManager.GetAllCategories';
var
  LCategoryDataSet: TAbstractModelDataset;
begin
  Result := False;
  try
    if Assigned(ACategory) and
       Assigned(FAppModules.Database()) and
       (FAppModules.Database.Connected) then
    begin
      LCategoryDataSet := nil;
      try
        if FAppModules.Database.CreateDataset(integer(dtExecSQL), LCategoryDataSet) then
        begin
          LCategoryDataSet.SetSQL('SELECT CategoryName FROM StudyDocumentCategory');
          LCategoryDataSet.DataSet.Open;
          while not LCategoryDataSet.DataSet.Eof do
          begin
            ACategory.Add(Trim(LCategoryDataSet.DataSet.FieldByName('CategoryName').AsString));
            LCategoryDataSet.DataSet.Next;
          end;
          Result := True;
          LCategoryDataSet.DataSet.Close;
        end;
      finally
        LCategoryDataSet.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyDocumentManager.GetCategoriesFromDB(ACategory: TStrings): boolean;
const OPNAME = 'TStudyDocumentManager.GetCategoriesFromDB';
var
  LCategoryDataSet: TAbstractModelDataset;
begin
  Result := False;
  try
    if Assigned(ACategory) and
       Assigned(FAppModules.Database()) and
       (FAppModules.Database.Connected) then
    begin
      LCategoryDataSet := nil;
      try
        if FAppModules.Database.CreateDataset(integer(dtExecSQL), LCategoryDataSet) then
        begin
          LCategoryDataSet.SetSQL('SELECT DISTINCT(Category) FROM StudyDocuments');
          LCategoryDataSet.DataSet.Open;
          while not LCategoryDataSet.DataSet.Eof do
          begin
            ACategory.Add(Trim(LCategoryDataSet.DataSet.FieldByName('Category').AsString));
            LCategoryDataSet.DataSet.Next;
          end;
          Result := True;
          LCategoryDataSet.DataSet.Close;
        end;
      finally
        LCategoryDataSet.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyDocumentManager.GetCategoriesFromFiles(ACategory: TStrings): boolean;
const OPNAME = 'TStudyDocumentManager.GetCategoriesFromFiles';
var
  LReportsPath : string;
  LIndex       : integer;
  LCategory    : TStringList;
begin
  Result := False;
  try
    if Assigned(ACategory) then
    begin
      LCategory := TStringList.Create;
      try
        LReportsPath := ExtractFilePath(ApplicationExeName) + 'Reports\'+ FAppModules.StudyArea.StudyAreaCode +'\';
        Result := SearchForDirectories(LReportsPath,LCategory);
        if Result then
        begin
          for LIndex := 0 to LCategory.Count -1 do
          begin
             ACategory.Add(ExtractFileName(LCategory[LIndex]));
          end;
        end;
      finally
        FreeAndNil(LCategory);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyDocumentManager.AddReportToDB(ADocumentDetail: TStudyDocumentDetail): boolean;
const OPNAME = 'TStudyDocumentManager.AddReportToDB';
var
  LCategoryDataSet: TAbstractModelDataset;
  //LFileName,
  LSQL: string;
  //LParam: TParam;
begin
  Result := False;
  try
    if Assigned(ADocumentDetail) and
       Assigned(FAppModules.Database()) and
       (FAppModules.Database.Connected) then
    begin
      LCategoryDataSet := nil;
      try
        if FAppModules.Database.CreateDataset(integer(dtExecSQL), LCategoryDataSet) then
        begin
          FAppModules.Database.StartTransaction;
          try
            LSQL := 'INSERT INTO StudyDocuments'+
                    ' (StudyAreaName,Category,Identifier,Filename,MenuCaption)'+
                    ' Values(:StudyAreaName,:Category,:Identifier,:Filename,:MenuCaption)';
            LCategoryDataSet.SetSQL(LSQL);
            LCategoryDataSet.SetParams(['StudyAreaName'],[FAppModules.StudyArea.StudyAreaCode]);
            LCategoryDataSet.SetParams(['Category']    , [ADocumentDetail.CategoryKey]);
            LCategoryDataSet.SetParams(['Identifier']  , [ADocumentDetail.IdentifierKey]);
            LCategoryDataSet.SetParams(['Filename']    , [ADocumentDetail.Filename]);
            LCategoryDataSet.SetParams(['MenuCaption'] , [ADocumentDetail.MenuCaption]);
            LCategoryDataSet.ExecSQL;
            LCategoryDataSet.DataSet.Close;

            LSQL := 'INSERT INTO StudyScenarioDocuments'+
                    ' (Model,StudyAreaName,SubArea,Scenario,Category,Identifier,Bookmark,PageNumber)'+
                    ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:Category,:Identifier,:Bookmark,:PageNumber)';
            LCategoryDataSet.SetSQL(LSQL);
            LCategoryDataSet.SetParams(['Model']               , [FAppModules.StudyArea.ModelCode]);
            LCategoryDataSet.SetParams(['StudyAreaName']       , [FAppModules.StudyArea.StudyAreaCode]);
            LCategoryDataSet.SetParams(['SubArea']             , [FAppModules.StudyArea.SubAreaCode]);
            LCategoryDataSet.SetParams(['Scenario']            , [FAppModules.StudyArea.ScenarioCode]);
            LCategoryDataSet.SetParams(['Category']    , [ADocumentDetail.CategoryKey]);
            LCategoryDataSet.SetParams(['Identifier']  , [ADocumentDetail.IdentifierKey]);
            LCategoryDataSet.SetParams(['Bookmark']    , [ADocumentDetail.BookMark]);
            LCategoryDataSet.SetParams(['PageNumber']  , [IntToStr(ADocumentDetail.PageNumber)]);
            LCategoryDataSet.ExecSQL;
            LCategoryDataSet.DataSet.Close;

            LCategoryDataSet.DataSet.Close;
            FAppModules.Database.Commit;
            Result := True;
            {LSQL := 'UPDATE StudyDocuments SET Document = :ADocument'+
                    ' WHERE StudyAreaName = '+ QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
                    '   AND Category      = '+ QuotedStr(ADocumentDetail.CategoryKey)+
                    '   AND Identifier      = '+ QuotedStr(ADocumentDetail.IdentifierKey);
            LCategoryDataSet.SetSQL(LSQL);

            LFileName := GetReportFileName(FAppModules.StudyArea.StudyAreaCode,ADocumentDetail.CategoryKey,ADocumentDetail.Filename);
            if FileExists(LFileName) then
            begin
              LParam := LCategoryDataSet.GetParamByName('ADocument');
              if(LParam <> nil) then
              begin
                LParam.LoadFromFile(LFileName,ftBlob);
                LCategoryDataSet.ExecSQL;
                LCategoryDataSet.DataSet.Close;

                FAppModules.Database.Commit;
                Result := True;
              end
              else
                FAppModules.Database.Rollback;
            end
            else
              FAppModules.Database.Rollback;}
          except
            FAppModules.Database.Rollback;
            raise;
          end;

        end;
      finally
        LCategoryDataSet.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyDocumentManager.EditReportInDB( ADocumentDetail: TStudyDocumentDetail): boolean;
const OPNAME = 'TStudyDocumentManager.EditReportInDB';
var
  LCategoryDataSet: TAbstractModelDataset;
  LSQL: string;
begin
  Result := False;
  try
    if Assigned(ADocumentDetail) and
       Assigned(FAppModules.Database()) and
       (FAppModules.Database.Connected) then
    begin
      LCategoryDataSet := nil;
      try
        if FAppModules.Database.CreateDataset(integer(dtExecSQL), LCategoryDataSet) then
        begin
          FAppModules.Database.StartTransaction;
          try
            LSQL := 'UPDATE StudyDocuments SET '+
                    ' MenuCaption = '+QuotedStr(ADocumentDetail.MenuCaption)+
                    ' WHERE '+
                    ' StudyAreaName     = '+ QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
                    ' AND Category      = '+ QuotedStr(ADocumentDetail.CategoryKey)+
                    ' AND Identifier    = '+ QuotedStr(ADocumentDetail.IdentifierKey);
            LCategoryDataSet.SetSQL(LSQL);
            LCategoryDataSet.ExecSQL;
            LCategoryDataSet.DataSet.Close;

            LSQL := 'UPDATE StudyScenarioDocuments SET '+
                    ' Bookmark = '+QuotedStr(ADocumentDetail.BookMark)+
                    ' ,PageNumber = '+ IntToStr(ADocumentDetail.PageNumber)+
                    ' WHERE '+
                    ' StudyAreaName  = '+ QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
                    ' AND  SubArea       = '+ QuotedStr(FAppModules.StudyArea.SubAreaCode)+
                    ' AND  Category      = '+ QuotedStr(ADocumentDetail.CategoryKey)+
                    ' AND  Identifier    = '+ QuotedStr(ADocumentDetail.IdentifierKey);
            LCategoryDataSet.SetSQL(LSQL);
            LCategoryDataSet.ExecSQL;
            LCategoryDataSet.DataSet.Close;
            FAppModules.Database.Commit;
            Result := True;
          except
            FAppModules.Database.Rollback;
            raise;
          end;
        end;
      finally
        LCategoryDataSet.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyDocumentManager.DeleteReportFromDB(ADocumentDetail: TStudyDocumentDetail): boolean;
const OPNAME = 'TStudyDocumentManager.DeleteReportFromDB';
var
  LCategoryDataSet: TAbstractModelDataset;
  LSQL: string;
begin
  Result := False;
  try
    if Assigned(ADocumentDetail) and
       Assigned(FAppModules.Database()) and
       (FAppModules.Database.Connected) then
    begin
      LCategoryDataSet := nil;
      try
        if FAppModules.Database.CreateDataset(integer(dtExecSQL), LCategoryDataSet) then
        begin
          FAppModules.Database.StartTransaction;
          try
            LSQL := 'DELETE FROM StudyDocuments WHERE '+
                    ' StudyAreaName     = '+ QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
                    ' AND Category      = '+ QuotedStr(ADocumentDetail.CategoryKey)+
                    ' AND Identifier    = '+ QuotedStr(ADocumentDetail.IdentifierKey);
            LCategoryDataSet.SetSQL(LSQL);
            LCategoryDataSet.ExecSQL;
            LCategoryDataSet.DataSet.Close;

            LSQL := 'DELETE FROM StudyScenarioDocuments WHERE '+
                    ' Model              = '+ QuotedStr(FAppModules.StudyArea.ModelCode)+
                    ' AND StudyAreaName  = '+ QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
                    ' AND  SubArea       = '+ QuotedStr(FAppModules.StudyArea.SubAreaCode)+
                    ' AND  Scenario      = '+ QuotedStr(FAppModules.StudyArea.ScenarioCode)+
                    ' AND  Category      = '+ QuotedStr(ADocumentDetail.CategoryKey)+
                    ' AND  Identifier    = '+ QuotedStr(ADocumentDetail.IdentifierKey);
            LCategoryDataSet.SetSQL(LSQL);
            LCategoryDataSet.ExecSQL;
            LCategoryDataSet.DataSet.Close;
            FAppModules.Database.Commit;
            Result := True;
          except
            FAppModules.Database.Rollback;
            raise;
          end;
        end;
      finally
        LCategoryDataSet.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
