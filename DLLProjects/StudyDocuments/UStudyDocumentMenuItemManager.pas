//
//  UNIT      : Contains TStudyDocumentsMenuItemManager Class
//  AUTHOR    : Dziedzi Ramulondi
//  DATE      : 2005/02/07
//  COPYRIGHT : Copyright © 2005 DWAF
//
unit UStudyDocumentMenuItemManager;

interface

uses
  Classes,
  Contnrs,
  UAbstractObject,
  UAbstractComponent,
  UMainMenuEventType,
  UMenuItemManager;

type

  TStudyDocumentDetailList = class(TList)
  protected
    function GetDocumentDetailByIndex(AIndex: integer): TStudyDocumentDetail;
  public
    function Delete(AReportType: TReportTypes;ACategoryKey,AFilename: string): boolean; overload;
    function GetDocument(ACategoryKey,AFilename: string):TStudyDocumentDetail;
    function GetDocumentFileNames(AReportType: TReportTypes;ACategoryKey: string; ADocuments: TStrings):boolean;
    property DocumentByIndex[AIndex: integer]: TStudyDocumentDetail read GetDocumentDetailByIndex;
  end;
  TStudyDocumentsMenuItemManager = class(TMenuItemManager)
  public
    procedure AddMenuItems(ADocuments: TStudyDocumentDetailList); reintroduce;
    procedure AddMenuItemItem(AMenuKeys: array of string; ASortWeight: integer;
      AEvent: integer = CmeNull; AData: TObject = nil); virtual;
  end;

implementation

uses
  SysUtils,
  UConstants,
  UErrorHandlingOperations;


{ TStudyDocumentDetailList }

function TStudyDocumentDetailList.Delete(AReportType: TReportTypes;ACategoryKey,AFilename: string): boolean;
const OPNAME = 'TStudyDocumentDetailList.Delete';
var
  LIndex: integer;
  LDocumentDetail:TStudyDocumentDetail;
begin
  Result := False;
  try
    for LIndex := 0 to Count -1 do
    begin
      LDocumentDetail := DocumentByIndex[LIndex];
      if(LDocumentDetail.ReportType = AReportType) and
        (LDocumentDetail.CategoryKey = ACategoryKey) and
        (LDocumentDetail.Filename = AFilename)  then
      begin
        Delete(LIndex);
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyDocumentDetailList.GetDocument(ACategoryKey,AFilename: string): TStudyDocumentDetail;
const OPNAME = 'TStudyDocumentDetailList.GetDocument';
var
  LIndex: integer;
  LDocumentDetail:TStudyDocumentDetail;
begin
  Result := nil;
  try
    for LIndex := 0 to Count -1 do
    begin
      LDocumentDetail := DocumentByIndex[LIndex];
      if(LDocumentDetail.CategoryKey = ACategoryKey) and
        (LDocumentDetail.Filename = AFilename)  then
      begin
        Result := LDocumentDetail;
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyDocumentDetailList.GetDocumentDetailByIndex(AIndex: integer): TStudyDocumentDetail;
const OPNAME = 'TStudyDocumentDetailList.GetDocumentDetailByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < Count) then
      Result := TStudyDocumentDetail(Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyDocumentDetailList.GetDocumentFileNames(AReportType: TReportTypes; ACategoryKey: string;
  ADocuments: TStrings): boolean;
const OPNAME = 'TStudyDocumentDetailList.GetDocumentFileNames';
var
  LIndex: integer;
  LDocumentDetail:TStudyDocumentDetail;
begin
  Result := False;
  try
    ADocuments.Clear;
    for LIndex := 0 to Count -1 do
    begin
      LDocumentDetail := DocumentByIndex[LIndex];
      if(LDocumentDetail.ReportType = AReportType) and
        (LDocumentDetail.CategoryKey = ACategoryKey)then
      begin
        ADocuments.Add(LDocumentDetail.Filename);
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TStudyDocumentsMenuItemManager }

procedure TStudyDocumentsMenuItemManager.AddMenuItemItem(AMenuKeys: array of string;
          ASortWeight, AEvent: integer; AData: TObject);
const OPNAME = 'TStudyDocumentsMenuItemManager.AddMenuItemItem';
begin
  try
    AddMenuItemItem(AMenuKeys,ASortWeight, AEvent,AData);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyDocumentsMenuItemManager.AddMenuItems(ADocuments: TStudyDocumentDetailList);
const OPNAME = 'TStudyDocumentsMenuItemManager.AddMenuItems';
var
  LIndex: integer;
  LCategories: TStringList;
  LMenuKeys: array of string;
begin
  try

    // Add the reports main menu item.
    if(FAppModules.Model <> nil) and (FAppModules.Model.ModelName <> CSystem) then
    begin
      AddMenuItemEntry(['Reports'], 800,CmeNull,nil);
      AddMenuItemEntry(['Reports','AddReport'], 1,CmeAddReport,nil);
      AddMenuItemEntry(['Reports','DeleteReport'], 2,CmeDeleteReport,nil);
      AddMenuItemEntry(['Reports','EditReport'], 3,CmeEditReport,nil);

      // Check if any document items are loaded.
      if Assigned(ADocuments) and (ADocuments.Count > 0) then
      begin

        // Extract all the categories.
        LCategories := TStringList.Create;
        try
          LCategories.Sorted := True;
          LCategories.Duplicates := dupIgnore;
          for LIndex := 0 to ADocuments.Count - 1 do
            LCategories.Add(ADocuments.DocumentByIndex[LIndex].CategoryKey);
          if (LCategories.Count > 0) then
          begin

            // Add the category menu items.
            SetLength(LMenuKeys, 2);
            try
              for LIndex := 0 to LCategories.Count - 1 do
              begin
                LMenuKeys[0] := 'Reports';
                LMenuKeys[1] := LCategories[LIndex];
                AddMenuItemEntry(LMenuKeys, 100 + LIndex);
                FAppModules.SetMenuItemCaption(LMenuKeys, LCategories[LIndex]);
              end;
            finally
              Finalize(LMenuKeys);
            end;

            // Add the document menu items.
            SetLength(LMenuKeys, 3);
            try
              for LIndex := 0 to ADocuments.Count - 1 do
              begin
                LMenuKeys[0] := 'Reports';
                LMenuKeys[1] := ADocuments.DocumentByIndex[LIndex].CategoryKey;
                LMenuKeys[2] := ADocuments.DocumentByIndex[LIndex].IdentifierKey;
                AddMenuItemEntry(LMenuKeys, 100 + LIndex, CmeLaunchStudyReport, ADocuments.DocumentByIndex[LIndex]);
                FAppModules.SetMenuItemCaption(LMenuKeys, ADocuments.DocumentByIndex[LIndex].MenuCaption);
              end;
            finally
              Finalize(LMenuKeys);
            end;
          end;

        // Done.
        finally
          LCategories.Free;
        end;
      end;

      // Make the menu items visible.
      SetMenuItems(msShow);

      if (not Assigned(ADocuments)) or (ADocuments.Count = 0) then
      begin
         FAppModules.SetMenuItem(['Reports','DeleteReport'], msDisable, 'DeleteReportDisabled');
         FAppModules.SetMenuItem(['Reports','EditReport'], msDisable, 'EditReportDisabled');
      end;
      if(not (FAppModules.User.UserRights in CUR_EditData)) or
        FAppModules.StudyArea.ScenarioLocked then
      begin
         FAppModules.SetMenuItem(['Reports','AddReport'], msDisable, 'AddReportDisabled');
         FAppModules.SetMenuItem(['Reports','DeleteReport'], msDisable, 'DeleteReportDisabled');
         FAppModules.SetMenuItem(['Reports','EditReport'], msDisable, 'EditReportDisabled');
      end;

    end;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
