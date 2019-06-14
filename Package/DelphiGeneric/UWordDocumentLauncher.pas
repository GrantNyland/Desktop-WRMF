//
//
//  UNIT      : Contains TWordDocumentLauncher Class
//  AUTHOR    : Grant Nyland
//  DATE      : 2002/04/18
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UWordDocumentLauncher;

interface

uses
  WordXP;
//  Word2000;

type
  TWordDocumentLauncher = class(TObject)
  protected
    FWordApp : _Application;
    FFileName: string;
    procedure WordAppQuit(Sender: TObject);
    function OpenDoc: WordDocument;
    function SelectBookMark(AWordDocument: WordDocument): boolean;
    procedure GotoBookMark(ABookMarkName: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure LaunchAtBookMark(ABookMarkName: string = ''; ASelectFromBookmarks: boolean = False);
    class procedure Launch(AFileName: string; ABookMarkName: string = ''; ASelectFromBookmarks: boolean = False);
    class procedure DestroyWordDocumentLauncher;
    property FileName: string read FFileName write FFileName;
  end;

implementation

uses
  SysUtils,
  vcl.Dialogs,
  Variants,
  Classes,
  UWaitScreen,
  UListSelectionDialog,
  UErrorHandlingOperations;

var GLauncher: TWordDocumentLauncher;

class procedure TWordDocumentLauncher.Launch(AFileName, ABookMarkName: string; ASelectFromBookmarks: boolean);
const OPNAME = 'TWordDocumentLauncher.Launch';
begin
  try

    // Display a wait message.
    LaunchWaitScreen('Loading....', AFileName);

    // Close the previous version.
    if Assigned(GLauncher) then
      FreeAndNil(GLauncher);

    // Open a new version.
    GLauncher := TWordDocumentLauncher.Create;
    GLauncher.FFileName := AFileName;
    GLauncher.LaunchAtBookMark(ABookMarkName, ASelectFromBookmarks);

    // Close the wait message.
    CloseWaitScreen;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

class procedure TWordDocumentLauncher.DestroyWordDocumentLauncher;
const OPNAME = 'TWordDocumentLauncher.DestroyWordDocumentLauncher';
begin
  try
    FreeAndNil(GLauncher);

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

constructor TWordDocumentLauncher.Create;
const OPNAME = 'TWordDocumentLauncher.Create';
begin
  try
    inherited Create;
    FWordApp  := nil;
    FFileName := '';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

destructor TWordDocumentLauncher.Destroy;
const OPNAME = 'TWordDocumentLauncher.Destroy';
begin
  try
    WordAppQuit(nil);
    inherited Destroy;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWordDocumentLauncher.WordAppQuit(Sender: TObject);
const OPNAME = 'TWordDocumentLauncher.WordAppQuit';
var
  SaveChanges: OleVariant;
  OriginalFormat: OleVariant;
  RouteDocument: OleVariant;
begin
  try
    SaveChanges := WdDoNotSaveChanges;
    OriginalFormat := UnAssigned;
    RouteDocument := UnAssigned;
    {try
      FWordApp.Quit(SaveChanges, OriginalFormat, RouteDocument);
    except end;}
    FWordApp := nil;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWordDocumentLauncher.LaunchAtBookMark(ABookMarkName: string; ASelectFromBookmarks: boolean);
const OPNAME = 'TWordDocumentLauncher.LaunchAtBookMark';
var LWordDoc: WordDocument;
begin
  try
    if (not FileExists(FFileName)) then
    begin
      raise Exception.Create('Could not locate the file : ' + FFileName);
    end else begin
      if Assigned(FWordApp) then
        WordAppQuit(nil);
      try
        FWordApp := CoWordApplication.Create;
        try
          try
            LWordDoc := OpenDoc;
            if (ABookMarkName <> '') then
            begin
              GotoBookMark(ABookMarkName);
            end else begin
              if ASelectFromBookmarks then
                SelectBookMark(LWordDoc);
            end;
          finally
            FWordApp.Visible := True;
            FWordApp.Activate;
          end;
        except
          WordAppQuit(nil);
        end;
      except
        ShowMessage('Could not launch Microsoft Word.');
        WordAppQuit(nil);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWordDocumentLauncher.OpenDoc: WordDocument;
const OPNAME = 'TWordDocumentLauncher.OpenDoc';
var
  ADocName: OleVariant;
  AConfirmConversions: OleVariant;
  AReadOnly: OleVariant;
  AAddToRecentFiles: OleVariant;
  APasswordDocument: OleVariant;
  APasswordTemplate: OleVariant;
  ARevert: OleVariant;
  AWritePasswordDocument: OleVariant;
  AWritePasswordTemplate: OleVariant;
  AFormat: OleVariant;
  //WordXP
  AEncoding: OleVariant;
  AVisible: OleVariant;
  AOpenAndRepair,
  ADocumentDirection,
  ANotCodingDialog : OleVariant;

//  AEncoding: OleVariant;  // Word2000
//  AVisible: OleVariant;   // Word2000
begin
  try
    ADocName := FFileName;
    AConfirmConversions := False;
    AReadOnly := True;
    AAddToRecentFiles := False;
    APasswordDocument := '';
    APasswordTemplate := '';
    ARevert := True;
    AWritePasswordDocument := '';
    AWritePasswordTemplate := '';
    AFormat := 0;
    AEncoding := False;
    AVisible := True;
    AOpenAndRepair := True;
    ADocumentDirection := '';
    ANotCodingDialog := True;


//    AEncoding := False;  // Word2000
//    AVisible := True;    // Word2000
    Result := FWordApp.Documents.Open(
      ADocName,
      AConfirmConversions,
      AReadOnly,
      AAddToRecentFiles,
      APasswordDocument,
      APasswordTemplate,
      ARevert,
      AWritePasswordDocument,
      AWritePasswordTemplate,
      AFormat,
      AEncoding,
      AVisible,
      AOpenAndRepair,
      ADocumentDirection,
      ANotCodingDialog{,
      AEncoding,
      AVisible} // Word2000
    );
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWordDocumentLauncher.GotoBookMark(ABookMarkName: string);
const OPNAME = 'TWordDocumentLauncher.GotoBookMark';
var
  AWhat : OleVariant;
  AWhich: OleVariant;
  ACount: OleVariant;
  AName: OleVariant;
begin
  try
    try
      if (ABookMarkName <> '') then
      begin
        AWhat  := -1;
        AWhich := 0;
        ACount := 0;
        AName  := ABookMarkName;
        FWordApp.Selection.Goto_(AWhat, AWhich, ACount, AName);
      end;
    except
      ShowMessage('Could not locate the bookmark [' + ABookMarkName + '] in the word document : ' + FFileName);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWordDocumentLauncher.SelectBookMark(AWordDocument: WordDocument): boolean;
const OPNAME = 'TWordDocumentLauncher.SelectBookMark';
var
  LIndex: integer;
  LBookMarkNames: TStringList;
  LOLEIndex: OleVariant;
  LSelectedBookMark: string;
begin
  Result := True;
  try
    AWordDocument.Bookmarks.ShowHidden := False;
    if (AWordDocument.Bookmarks.Count > 0) then
    begin
      LBookMarkNames := TStringList.Create;
      try
        for LIndex := 1 to AWordDocument.Bookmarks.Count do
        begin
          LOLEIndex := LIndex;
          LBookMarkNames.Add(AWordDocument.Bookmarks.Item(LOLEIndex).Name);
        end;
        if (LBookMarkNames.Count > 0) then
        begin
          if (LBookMarkNames.Count <= 1) then
          begin
            GotoBookMark(LBookMarkNames[0]);
          end else begin
            if SelectFromList('Select Book Mark', '', LBookMarkNames, LSelectedBookMark) then
            begin
              if (LSelectedBookMark <> '') then
                GotoBookMark(LSelectedBookMark);
            end else begin
              Result := False;
            end;
          end;
        end;
      finally
        LBookMarkNames.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

initialization
  GLauncher := nil;

finalization
  FreeAndNil(GLauncher);

end.
