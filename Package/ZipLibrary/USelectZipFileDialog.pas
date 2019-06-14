//
//
//  UNIT      : Contains     Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 09/06/2005
//  COPYRIGHT : Copyright © 2004 DWAF                       
//
//

unit USelectZipFileDialog;

interface
uses
  Vcl.controls,
  Classes,
  Vcl.Forms,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  Vcl.Buttons,
  Vcl.StdCtrls,
  Contnrs,
  SysUtils,
  Windows,
  Vcl.ImgList,
  //ZipMstr,
  UAbstractObject,
  UDataEditComponent,
  UAbstractComponent;

type

  TFileDataObject = class(TObject)
  protected
    FIsDirectory: boolean;
    FIsSubZip : boolean;
    FAddToAll : boolean;
    FFileName : TFileName;
  public
    property IsDirectory : boolean read FIsDirectory write FIsDirectory;
    property IsSubZip : boolean read FIsSubZip write FIsSubZip;
    property AddToAll : boolean read FAddToAll write FAddToAll;
    property FileName    : TFileName  read FFileName    write FFileName;
  end;

  TSelectZipFileDialog = class(TAbstractForm)
  protected
    EdtDirPanel                  : TPanel;
    lblSelectDir                 : TLabel;
    edtStartDirectory            : TEdit;
    btnSelectStartDirectory      : TButton;
    btnContinuePanel               : TPanel;
    btnContinue                  : TButton;
    tvFilesAndDirectory          : TFieldTreeView;
    ilFilesAndDirectory          : TImageList;
    btnCancel                    : TButton;
    FFileDataObjects             : TObjectList;
    FZipFileName                 : TFileName;
    FTempDirectory,
    FInitialDir                  : string;
    FZipList                     : TStringList;
    FProgressLabel               : TLabel;
    FImportAll                   : boolean;
    FEdtStartDirectoryText       : string;
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
    procedure DestroyMemberObjects; override;
    procedure Resize; override;
    procedure PopulateTreeView(APath: string; AMainNode : TTreeNode);
    procedure PopulateStudyTreeView(AFile : string;ANode : TTreeNode;ASubZipContainer : TStringList);
    procedure DoedtStartDirectoryChange(Sender : TObject);
    procedure DotvFilesAndDirectoryChange(Sender : TObject; Node : TTreeNode);
    procedure DobtnSelectStartDirectoryClick(Sender : TObject);
    procedure DobtnContinueClick(Sender : TObject);
    procedure DobtnCancelClick(Sender : TObject);
    procedure DotvFilesAndDirectoryMouseDown(Sender : TObject; Button : TMouseButton;
                                             Shift  : TShiftState; X,  Y  : Integer);
    function GetZipFileName : TFileName;
    function LoadImportList(ANode : TTreeNode) : boolean;
    function UnLoadImportList(ANode : TTreeNode) : boolean;
    function GetZipFileList(AZipFileName : string; AZipFileList : TStringList;AllStudies : boolean = False) : boolean;
    function ExtractZip(AZipFileName : string) : boolean;
    function GetImportZips : string;
    procedure LoadAllStudyDataList(AZipFile : string);
    procedure SetImportAll(const Value: boolean);
    procedure PopulateAllStudyData(AZipFile : string; AMainNode : TTreeNode);
  public
    function Initialise: Boolean; override;
    property InitialDir : string read FInitialDir write FInitialDir;
    property ZipFileName : TFileName read GetZipFileName;
    property ImportZips : string read GetImportZips;
    property ImportAll : boolean read FImportAll write SetImportAll;

end;

implementation
{$WARN UNIT_PLATFORM OFF}

uses
  Vcl.Graphics,
  Vcl.FileCtrl,
  System.Zip,
  UUtilities,
  UErrorHandlingOperations;

const
  CMainScriptPrefix = 'MAIN_';

{ TSelectZipFileDialog }

procedure TSelectZipFileDialog.AssignHelpContext;
const OPNAME = 'TSelectZipFileDialog.AssignHelpContext';
begin
  inherited;
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSelectZipFileDialog.CreateMemberObjects;
const OPNAME = 'TSelectZipFileDialog.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FFileDataObjects                := TObjectList.Create(True);
    FZipList                        := TStringList.Create;

    tvFilesAndDirectory             := TFieldTreeView.Create(Self, FAppModules);
    tvFilesAndDirectory.Parent      := Self;
    tvFilesAndDirectory.OnChange    := DotvFilesAndDirectoryChange;
    tvFilesAndDirectory.OnMouseDown := DotvFilesAndDirectoryMouseDown;

    ilFilesAndDirectory            := TImageList.Create(Self);
    ilFilesAndDirectory.Height := 18;
    ilFilesAndDirectory.Width  := 18;
    ilFilesAndDirectory.GetInstRes(HImagesInstance, rtBitmap, 'UNCHECKCYAN',     16, [], clWhite);
    ilFilesAndDirectory.GetInstRes(HImagesInstance, rtBitmap, 'CHECKCYAN',       16, [], clWhite);


    EdtDirPanel                    := TPanel.Create ( Self );
    EdtDirPanel.Parent             := Self;

    lblSelectDir                   := TLabel.Create ( EdtDirPanel );
    lblSelectDir.Parent            := EdtDirPanel;
    edtStartDirectory              := TEdit.Create ( EdtDirPanel );
    edtStartDirectory.Parent       := EdtDirPanel;
    edtStartDirectory.OnChange     := DoedtStartDirectoryChange;

    btnSelectStartDirectory        := TButton.Create ( EdtDirPanel );
    btnSelectStartDirectory.Parent := EdtDirPanel;
    btnSelectStartDirectory.OnClick:= DobtnSelectStartDirectoryClick;

    btnContinuePanel                 := TPanel.Create ( Self );
    btnContinuePanel.Parent          := Self;
    FProgressLabel                   := TLabel.Create(btnContinuePanel);
    FProgressLabel.Parent            := btnContinuePanel;
    btnContinue                    := TButton.Create ( btnContinuePanel );
    btnContinue.Parent             := btnContinuePanel;
    btnCancel                      := TButton.Create ( btnContinuePanel );
    btnCancel.Parent               := btnContinuePanel;


  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSelectZipFileDialog.DestroyMemberObjects;
const OPNAME = 'TSelectZipFileDialog.DestroyMemberObjects';
begin
  inherited;
  try
    FreeAndNil(FFileDataObjects);
    FreeAndNil(FZipList);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSelectZipFileDialog.DobtnCancelClick ( Sender : TObject );
const OPNAME = 'TSelectZipFileDialog.DobtnCancelClick';
begin
  try
    ModalResult := mrCancel
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSelectZipFileDialog.DobtnContinueClick ( Sender : TObject );
const OPNAME = 'TSelectZipFileDialog.DobtnContinueClick';
begin
  try
    ModalResult := mrOk;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSelectZipFileDialog.DobtnSelectStartDirectoryClick ( Sender : TObject );
const OPNAME = 'TSelectZipFileDialog.DobtnSelectStartDirectoryClick';
var
  LPath: string;
begin
  try
    LPath := Trim(FAppModules.ViewIni.ReadString(ClassName,'ImportDirectory',''));
    if (LPath <> '') then
      if not SysUtils.DirectoryExists(LPath) then
         LPath := '';
    try
      if SelectDirectory(LPath,[],0) then
      begin
        FEdtStartDirectoryText := LPath;
        FInitialDir := LPath;
        FAppModules.ViewIni.WriteString ( ClassName,'ImportDirectory',ExtractFilePath ( InitialDir ) );
        edtStartDirectory.Text := LPath;
      end;
    except on E: Exception do  end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSelectZipFileDialog.DoedtStartDirectoryChange ( Sender : TObject );
const OPNAME = 'TSelectZipFileDialog.DoedtStartDirectoryChange';
var
  LPath: string;
  LOldCursor : TCursor;
begin
  try
    btnCancel.Enabled := False;
    FFileDataObjects.Clear;
    tvFilesAndDirectory.Items.Clear;
    if SysUtils.DirectoryExists (FEdtStartDirectoryText) then
    begin
      LPath := FEdtStartDirectoryText;
      if (Trim(LPath) <> '') then
      begin
        LOldCursor := Screen.Cursor;
        try
          Screen.Cursor := crHourGlass;
          if ImportAll then
            LoadAllStudyDataList(LPath)
            //PopulateTreeView(LPath, nil)
          else
            PopulateTreeView(LPath, nil);
          tvFilesAndDirectory.Items.AlphaSort(True);
        finally
          Screen.Cursor := LOldCursor;
          btnCancel.Enabled := True;
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSelectZipFileDialog.DotvFilesAndDirectoryChange ( Sender: TObject; Node: TTreeNode );
const OPNAME = 'TSelectZipFileDialog.DotvFilesAndDirectoryChange';
begin
  try
{  FZipFileName := '';
  btnContinue.Enabled := False;
  if(Node.Data <> nil) then
  begin
    btnContinue.Enabled := not TFileDataObject ( Node.Data ).IsDirectory;
    if btnContinue.Enabled then
      FZipFileName := TFileDataObject ( Node.Data ).FFileName;
  end;
}
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSelectZipFileDialog.DotvFilesAndDirectoryMouseDown(Sender: TObject; Button: TMouseButton;
                                                              Shift: TShiftState; X, Y: Integer);
const OPNAME = 'TSelectZipFileDialog.DotvFilesAndDirectoryMouseDown';
var
  LNode : TTreeNode;
  LHit  : THitTests;
begin
  try
    if not ImportAll then
    begin
      LNode :=  tvFilesAndDirectory.GetNodeAt(X,Y);
      if (LNode <> nil) then
      begin
        LHit := tvFilesAndDirectory.GetHitTestInfoAt(X,Y);
        if (htOnIcon in LHit) then
        begin
          case LNode.ImageIndex of
            0 : LoadImportList(LNode);
            1 : UnLoadImportList(LNode);
          end;
        end;
        tvFilesAndDirectory.Repaint;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSelectZipFileDialog.GetZipFileName : TFileName;
const OPNAME = 'TSelectZipFileDialog.GetZipFileName';
begin
  Result := '';
  try
    Result := FZipFileName;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSelectZipFileDialog.Initialise : Boolean;
const OPNAME = 'TSelectZipFileDialog.Initialise';
begin
  Result := False;
  try

    Self.Height                     := 374;
    Self.Width                      := 565;
    Self.BorderStyle                := bsDialog;
    Self.Position                   := poScreenCenter;
    Self.Caption                    := FAppModules.Language.GetString('StudySelection.FormCaption');

    EdtDirPanel.Align               := alTop;

    btnContinuePanel.Align          := alBottom;
    tvFilesAndDirectory.Align       := alClient;
    tvFilesAndDirectory.Images      := ilFilesAndDirectory;
    tvFilesAndDirectory.StateImages := ilFilesAndDirectory;

    lblSelectDir.Top                := 11;
    lblSelectDir.Left               := 8;
    lblSelectDir.Caption            := FAppModules.Language.GetString('StudySelection.LabelCaption1');

    edtStartDirectory.Top           := 8;
    edtStartDirectory.Left          := 82;
    edtStartDirectory.Width         := 401;

    btnSelectStartDirectory.Top     := 7;
    btnSelectStartDirectory.Left    := 493;
    btnSelectStartDirectory.Caption := '...';
    btnSelectStartDirectory.Width   := 41;
    btnSelectStartDirectory.Height  := 21;

    FProgressLabel.Top              := 8;
    FProgressLabel.Left             := 10;
    FProgressLabel.Height           := 25;
    FProgressLabel.Caption          := '';

    btnContinue.Top                 := 8;
    btnContinue.Left                := 437;
    btnContinue.Height              := 25;
    btnContinue.Caption             := FAppModules.Language.GetString('ButtonCaption.Continue');
    btnContinue.Enabled             := False;
    btnContinue.ModalResult         := mrOk;

    btnCancel.Top                   := 8;
    btnCancel.Left                  := 333;
    btnCancel.Height                := 25;
    btnCancel.Caption               := FAppModules.Language.GetString('ButtonCaption.Cancel');
    btnCancel.ModalResult           := mrCancel;

    FTempDirectory                  := GetTempDir + 'WRMF\';
    if not SysUtils.DirectoryExists(FTempDirectory) then
       SysUtils.ForceDirectories(FTempDirectory);
    DeleteMultipleFiles(FTempDirectory +'*.*' );
    FZipList.Sorted     := True;
    FImportAll          := False;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSelectZipFileDialog.LoadImportList(ANode : TTreeNode): boolean;
const OPNAME = 'TSelectZipFileDialog.LoadImportList';
var
  LTreeIndex : integer;
  LFileDataObject : TFileDataObject;
begin
  Result := False;
  try
    if (ANode <> nil) then
    begin
      LFileDataObject := TFileDataObject(ANode.Data);
      if LFileDataObject.IsSubZip then
      begin
        if (FEdtStartDirectoryText <> TFileDataObject(ANode.Parent.Data ).FileName) and
           (FZipList.IndexOf(TFileDataObject(ANode.Parent.Data).FileName +'\'+ LFileDataObject.FileName) < 0) then
        begin
          FZipList.Add(TFileDataObject(ANode.Parent.Data).FileName +'\'+ LFileDataObject.FileName);
          ANode.Parent.StateIndex := -1;
          ANode.Parent.SelectedIndex := 1;
          ANode.Parent.ImageIndex := 1;
          ANode.StateIndex := -1;
          ANode.SelectedIndex := 1;
          ANode.ImageIndex := 1;
        end;
      end;
      if (not LFileDataObject.FIsDirectory) then
      begin
        for LTreeIndex := 0 to tvFilesAndDirectory.Items.Count - 1 do
        begin
          Result := UpperCase(TFileDataObject(tvFilesAndDirectory.Items[LTreeIndex].Data).FileName) =
                    UpperCase(LFileDataObject.FileName);
          if (LTreeIndex > 0) then
            Result := Result or (TFileDataObject(tvFilesAndDirectory.Items[LTreeIndex]).IsSubZip) and
                                (UpperCase(TFileDataObject(tvFilesAndDirectory.Items[LTreeIndex ].Parent.Data).FileName) =
                                (UpperCase(LFileDataObject.FileName)));
          if (Result) then
          begin
            tvFilesAndDirectory.Items[LTreeIndex].StateIndex := -1;
            tvFilesAndDirectory.Items[LTreeIndex].SelectedIndex := 1;
            tvFilesAndDirectory.Items[LTreeIndex].ImageIndex := 1;
            if (FEdtStartDirectoryText+'\' <> TFileDataObject(tvFilesAndDirectory.Items[LTreeIndex].Parent.Data).FileName) and
               (FZipList.IndexOf(TFileDataObject(tvFilesAndDirectory.Items[LTreeIndex].Parent.Data).FileName +'\'+
                                 TFileDataObject(tvFilesAndDirectory.Items[LTreeIndex].Data).FileName ) < 0) then
              FZipList.Add(TFileDataObject(tvFilesAndDirectory.Items[LTreeIndex].Parent.Data).FileName +'\'+
                             TFileDataObject(tvFilesAndDirectory.Items[LTreeIndex].Data).FileName)
            else
            if (FEdtStartDirectoryText+'\' = TFileDataObject(tvFilesAndDirectory.Items[LTreeIndex].Parent.Data ).FileName) and
              (FZipList.IndexOf(TFileDataObject(tvFilesAndDirectory.Items[LTreeIndex].Data).FileName) < 0)  then
              FZipList.Add(TFileDataObject(tvFilesAndDirectory.Items[LTreeIndex].Data).FileName)
          end;
        end;
      end;
    end;
    btnContinue.Enabled := FZipList.Count > 0;
    Result := btnContinue.Enabled;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSelectZipFileDialog.PopulateStudyTreeView(AFile : string;ANode : TTreeNode;ASubZipContainer : TStringList);
const OPNAME = 'TSelectZipFileDialog.PopulateStudyTreeView';
var
  LIndex : integer;
  LFileData : TFileDataObject;
  LFileName : string;
begin
  try
    if ASubZipContainer <> nil then
    begin
      for LIndex := 0 to ASubZipContainer.Count -1 do
      begin
        LFileName := ExtractFileName(ASubZipContainer[LIndex]);
        if (UpperCase(Copy(LFileName, Pos('.', LFileName), 4)) = '.ZIP') then
        begin
          LFileData := TFileDataObject.Create;
          FFileDataObjects.Add(LFileData);
          LFileData.IsDirectory := False;
          LFileData.IsSubZip    := True;
          LFileData.AddToAll    := True;
          LFileData.FileName    := LFileName;
          tvFilesAndDirectory.Items.AddChildObjectFirst(ANode, ExtractFileName(LFileName), LFileData);
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSelectZipFileDialog.PopulateTreeView(APath : string; AMainNode : TTreeNode);
const OPNAME = 'TSelectZipFileDialog.PopulateTreeView';
var
  LSearchRec: TSearchRec;
  LMoreFiles: boolean;
  LZipNode,
  LNode: TTreenode;
  LFileData : TFileDataObject;
  LFileName,
  LSearchStr: string;
  LSubZipContainer,
  LTempFilesContainer: TStringList;
  LIndex: integer;
begin
  try
    if (Length(Trim(APath)) <> 0) then
    begin
      LFileData := TFileDataObject.Create;
      FFileDataObjects.Add(LFileData);
      LFileData.IsDirectory := True;
      LFileData.IsSubZip := False;
      LFileData.AddToAll    := False;
      APath := IncludeTrailingPathDelimiter(APath);
      LFileData.FileName    := APath;
      APath := ExcludeTrailingPathDelimiter(APath);
      LNode := tvFilesAndDirectory.Items.AddChildObjectFirst(AMainNode,ExtractFileName(APath),LFileData);
      APath := IncludeTrailingPathDelimiter(APath);
      LSearchStr := IncludeTrailingPathDelimiter(APath)+ '*.*';
      LMoreFiles := (FindFirst(LSearchStr, faAnyFile, LSearchRec) = 0);
      while LMoreFiles do
      begin
        Application.ProcessMessages;
        if (LSearchRec.Name[1] <> '.') then
        begin
          if ((LSearchRec.Attr and faDirectory) <> 0) then
            PopulateTreeView(APath + LSearchRec.Name, LNode)
          else
          begin
            if (UpperCase(ExtractFileExt(LSearchRec.Name)) = '.ZIP') then
            begin
              LFileData := TFileDataObject.Create;
              FFileDataObjects.Add(LFileData);
              LFileData.IsDirectory := False;
              LFileData.IsSubZip := False;
              LFileData.AddToAll    := False;
              LFileData.FileName := APath + LSearchRec.Name;
              LZipNode := tvFilesAndDirectory.Items.AddChildObjectFirst(LNode, LSearchRec.Name, LFileData);
              //Add Sub-Zip(s)....
              LTempFilesContainer := TStringList.Create;
              try
                if not (FImportAll) and (GetZipFileList(APath + LSearchRec.Name,LTempFilesContainer{, FImportAll})) then
                begin
                  for LIndex := 0 to LTempFilesContainer.Count - 1 do
                  begin
                    LFileName := ExtractFileName(LTempFilesContainer[LIndex]);
                    LSubZipContainer := TStringList.Create;
                    try
                      if (UpperCase(Copy(LFileName, Pos('.', LFileName), 4)) = '.ZIP') then
                      begin
                        LFileData := TFileDataObject.Create;
                        FFileDataObjects.Add(LFileData);
                        LFileData.IsDirectory := False;
                        LFileData.IsSubZip    := True;
                        LFileData.AddToAll    := False;
                        LFileData.FileName    := LFileName;
                        tvFilesAndDirectory.Items.AddChildObjectFirst(LZipNode, ExtractFileName(LFileName), LFileData);
                      end;
                    finally
                      FreeAndNil(LSubZipContainer);
                    end;
                  end;
                end;
                DeleteMultipleFiles(FTempDirectory +'*.*' );
              finally
                FreeAndNil(LTempFilesContainer);
              end;
            end;
          end;
        end;
        LMoreFiles := (FindNext(LSearchRec)= 0 );
      end;
      SysUtils.FindClose(LSearchRec);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSelectZipFileDialog.GetZipFileList(AZipFileName : string; AZipFileList : TStringList;AllStudies : boolean = False) : boolean;
const OPNAME = 'TSelectZipFileDialog.GetZipFileList';
var
  LZipFile   : TZipFile;
  LIndex : integer;
begin
  Result := False;
  try
    if Assigned (AZipFileList) and FileExists(AZipFileName)then
    begin
      LZipFile := TZipFile.Create;
      try
        LZipFile.Open(AZipFileName,zmRead);
        for LIndex := Low(LZipFile.FileNames) to High(LZipFile.FileNames) do
        begin
          AZipFileList.Add(LZipFile.FileNames[LIndex]);
        end;
      finally
        LZipFile.Free;
      end;
      Result := (AZipFileList.Count > 0);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSelectZipFileDialog.ExtractZip(AZipFileName : string) : boolean;
const OPNAME = 'TSelectZipFileDialog.ExtractZip';
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


procedure TSelectZipFileDialog.Resize;
const OPNAME = 'TSelectZipFileDialog.Resize';
begin
  inherited;
  try
    edtStartDirectory.Width := btnSelectStartDirectory.Left - edtStartDirectory.Left - 5;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSelectZipFileDialog.UnLoadImportList(ANode: TTreeNode): boolean;
const OPNAME = 'TSelectZipFileDialog.UnLoadImportList';
var
  LTreeIndex,
  LCount,
  LIndex : integer;
  LFileDataObject : TFileDataObject;
begin
  Result := False;
  try
    if ANode <> nil then
    begin
      LFileDataObject := TFileDataObject(ANode.Data);
      if LFileDataObject.IsSubZip then
      begin
        ANode.StateIndex := 0;
        ANode.SelectedIndex := 0;
        ANode.ImageIndex := 0;
        FZipList.Delete(FZipList.IndexOf(TFileDataObject(ANode.Parent.Data).FileName +'\'+ LFileDataObject.FileName));
        LCount := 0;
        for LIndex := 0 to FZipList.Count - 1 do
        begin
          if (Pos(TFileDataObject(ANode.Parent.Data).FileName, FZipList[LIndex]) > 0 ) then
            inc(LCount);
        end;

        if (LCount = 0) or ((LCount = 1) and
          (FZipList.IndexOf(TFileDataObject(ANode.Parent.Data).FileName) >= 0)) then
        begin
           ANode.Parent.StateIndex := 0;
           ANode.Parent.SelectedIndex := 0;
           ANode.Parent.ImageIndex := 0;
           if (FZipList.IndexOf(TFileDataObject(ANode.Parent.Data).FileName) >= 0)  then
             FZipList.Delete(FZipList.IndexOf(TFileDataObject(ANode.Parent.Data).FileName));
        end;
      end;
      if (not LFileDataObject.FIsDirectory) then
      begin
        for LTreeIndex := 0 to tvFilesAndDirectory.Items.Count - 1 do
        begin
          Result := UpperCase(TFileDataObject(tvFilesAndDirectory.Items[LTreeIndex].Data).FileName) =
                    UpperCase(LFileDataObject.FileName);
          if LTreeIndex > 0 then
            Result := Result or (TFileDataObject(tvFilesAndDirectory.Items[LTreeIndex].Data).IsSubZip) and
                                (UpperCase(TFileDataObject(tvFilesAndDirectory.Items[LTreeIndex].Parent.Data).FileName) =
                                (UpperCase(LFileDataObject.FileName)));
          if (Result) then
          begin
            tvFilesAndDirectory.Items[LTreeIndex].StateIndex := 0;
            tvFilesAndDirectory.Items[LTreeIndex].SelectedIndex := 0;
            tvFilesAndDirectory.Items[LTreeIndex].ImageIndex := 0;
            if FZipList.IndexOf(TFileDataObject(tvFilesAndDirectory.Items[LTreeIndex].Parent.Data).FileName+'\'+
                                TFileDataObject(tvFilesAndDirectory.Items[LTreeIndex].Data).FileName) >= 0 then
              FZipList.Delete(FZipList.IndexOf(TFileDataObject(tvFilesAndDirectory.Items[LTreeIndex].Parent.Data).FileName+'\'+
                                               TFileDataObject(tvFilesAndDirectory.Items[LTreeIndex].Data).FileName))
            else
            if (FZipList.IndexOf(TFileDataObject(tvFilesAndDirectory.Items[LTreeIndex].Data).FileName) >= 0)  then
              FZipList.Delete(FZipList.IndexOf(TFileDataObject(tvFilesAndDirectory.Items[LTreeIndex].Data).FileName));
          end;
        end;
      end;
    end;
    btnContinue.Enabled := FZipList.Count > 0;
    Result := btnContinue.Enabled;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSelectZipFileDialog.GetImportZips: string;
const OPNAME = 'TSelectZipFileDialog.GetImportZips';
var
  LZipFileName : string;
  LIndex : integer;
  LZipList : TStringList;
begin
  Result := '';
  try
    LZipList := TStringList.Create;
    LZipList.Sorted := True;
    try
      if not FImportAll then
      begin
        for LIndex := 0 to FZipList.Count - 1 do
        begin
          LZipFileName := ExtractFileName(FZipList[LIndex]);
          if (Pos(CRainfall, LZipFileName) > 0) or
             (Pos(CPlanning, LZipFileName) > 0) or
             (Pos(CHydrology, LZipFileName) > 0) or
             (Pos(CYield, LZipFileName) > 0) or
             (Pos(CDailyDiversion,LZipFileName) > 0) or
             (Pos(CIFRPreProcessor,LZipFileName) > 0) or
             (Pos(CDamSedimentation,LZipFileName) > 0) or
             (Pos(CStomsa,LZipFileName) > 0) or
             (Pos(CRWH,LZipFileName) > 0) or
             (Pos(CDDTS,LZipFileName) > 0) then
            LZipList.Add(FZipList[LIndex]);
        end;
        Result := LZipList.CommaText;
      end
      else
        Result := FZipList.CommaText;
    finally
      FreeAndNil(LZipList);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSelectZipFileDialog.LoadAllStudyDataList(AZipFile : string);
const OPNAME = 'TSelectZipFileDialog.LoadAllStudyDataList';
var
  LIndex : integer;
  LFileDataObject : TFileDataObject;
begin
  try
    PopulateAllStudyData(AZipFile,nil);
    for LIndex := 0 to tvFilesAndDirectory.Items.Count -1 do
    begin
      if LIndex > 0 then
      begin
        LFileDataObject := TFileDataObject(tvFilesAndDirectory.Items[LIndex].Data);
        if (LFileDataObject <> nil) then
        begin
          tvFilesAndDirectory.Items[LIndex].Parent.StateIndex := -1;
          tvFilesAndDirectory.Items[LIndex].Parent.SelectedIndex := 1;
          tvFilesAndDirectory.Items[LIndex].Parent.ImageIndex := 1;
          tvFilesAndDirectory.Items[LIndex].StateIndex := -1;
          tvFilesAndDirectory.Items[LIndex].SelectedIndex := 1;
          tvFilesAndDirectory.Items[LIndex].ImageIndex := 1;
        end;
      end;
    end;
    btnContinue.Enabled := FZipList.Count > 0;
    tvFilesAndDirectory.FullExpand;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSelectZipFileDialog.SetImportAll(const Value: boolean);
const OPNAME = 'TSelectZipFileDialog.SetImportAll';
begin
  try
    FImportAll := Value;
    Self.Caption := FAppModules.Language.GetString('StudySelection.ImportAllStudyData');
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSelectZipFileDialog.PopulateAllStudyData(AZipFile: string; AMainNode : TTreeNode);
const OPNAME = 'TSelectZipFileDialog.PopulateAllStudyData';
var
  LSearchRec: TSearchRec;
  LMoreFiles: boolean;
  LZipNode,
  LNode: TTreenode;
  LFileData : TFileDataObject;
  LSearchStr: string;
  LTempFilesContainer: TStringList;
  LIndex: integer;
begin
  try
    if (Trim(AZipFile) <> '') then
    begin
      LFileData := TFileDataObject.Create;
      FFileDataObjects.Add(LFileData);
      LFileData.IsDirectory := True;
      LFileData.IsSubZip := False;
      LFileData.AddToAll    := False;
      AZipFile := IncludeTrailingPathDelimiter(AZipFile);
      LFileData.FileName    := AZipFile;
      AZipFile := ExcludeTrailingPathDelimiter(AZipFile);
      LNode := tvFilesAndDirectory.Items.AddChildObjectFirst(AMainNode,ExtractFileName(AZipFile),LFileData);
      AZipFile := IncludeTrailingPathDelimiter(AZipFile);
      LSearchStr := IncludeTrailingPathDelimiter(AZipFile)+ '*.*';
      FZipList.Clear;
      LMoreFiles := (FindFirst(LSearchStr, faAnyFile, LSearchRec) = 0);
      while LMoreFiles do
      begin
        Application.ProcessMessages;
        if (LSearchRec.Name[1] <> '.') then
        begin
          if (UpperCase(ExtractFileExt(LSearchRec.Name)) = '.ZIP') then
          begin
            LTempFilesContainer := TStringList.Create;
            try
              if (GetZipFileList(AZipFile + LSearchRec.Name,LTempFilesContainer)) then
              begin
                if (LTempFilesContainer.Count > 1) and (LTempFilesContainer.IndexOf('MAIN_AllStadyData.dat')>0) then
                begin
                  if ExtractZip(AZipFile + LSearchRec.Name) then
                  begin
                    LTempFilesContainer.Clear;
                    LTempFilesContainer.LoadFromFile(FTempDirectory+'MAIN_AllStadyData.dat');
                    if LTempFilesContainer.Count > 1 then
                    begin
                      LZipNode := tvFilesAndDirectory.Items.AddChildObjectFirst(LNode,ExtractFileName(LSearchRec.Name),LFileData);
                      FZipList.Add(AZipFile+LSearchRec.Name);
                      for LIndex := 1 to LTempFilesContainer.Count-1 do
                      begin
                        LFileData := TFileDataObject.Create;
                        FFileDataObjects.Add(LFileData);
                        LFileData.IsDirectory := False;
                        LFileData.IsSubZip := False;
                        LFileData.AddToAll    := False;
                        LFileData.FileName := AZipFile + LTempFilesContainer[LIndex];
                        tvFilesAndDirectory.Items.AddChildObjectFirst(LZipNode, LTempFilesContainer[LIndex], LFileData);
                      end;
                    end;
                  end;
                end;
              end;
            finally
              FreeAndNil(LTempFilesContainer);
            end;
          end;
        end;
        LMoreFiles := (FindNext(LSearchRec)= 0);
      end;
      SysUtils.FindClose(LSearchRec);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
