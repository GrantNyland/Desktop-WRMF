//
//
//  UNIT      : Contains TDisplayUnknownFilesTobeDeletedForm Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 05/12/2005
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UDisplayUnknownFilesTobeDeletedForm;
interface
uses
  controls,
  Classes,
  Dialogs,
  Forms,
  ComCtrls,
  ExtCtrls,
  Buttons,
  StdCtrls,
  Contnrs,
  SysUtils,
  Windows,
  UAbstractObject,
  UInstalledFileInfoObjectList,
  UAbstractComponent,
  UDataEditComponent;

type

  TDisplayUnknownFilesTobeDeletedForm = class(TAbstractForm)
  protected
    EdtDirPanel                  : TPanel;
    FMsgStr                      : TLabel;
    btnContinuePanel             : TPanel;
    btnRemove                    : TButton;
    tvFilesAndDirectory          : TFieldTreeView;
    btnCancel                    : TButton;
    FFileDataObjects             : TObjectList;
    FInitialDir                  : string;
    FFileInfoObjectList          : TInstalledFileInfoObjectList;
    FFilecount                   : integer;
    FImage                       : TImage;
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
    function OnHelpRequest(Command: Word; Data: Integer;var CallHelp: Boolean): Boolean;
    procedure DestroyMemberObjects; override;
    procedure Resize; override;
    procedure PopulateTreeView ( AFileInfoObjectList : TInstalledFileInfoObjectList; APath : string; AMainNode : TTreeNode );
    procedure DobtnCancelClick ( Sender : TObject );
    procedure DoUninstall ( Sender : TObject );
    procedure DoRemoveDir ( ADirectory : string );
    function GetUnKnownFiles ( AFileInfoObjectList : TInstalledFileInfoObjectList ) : boolean;
    procedure SetLabelCaptions;
  public
    function Initialise: Boolean; override;
    property InitialDir : string read FInitialDir write FInitialDir;
    property CanDisplayUnknownFiles [ AFileInfoObjectList : TInstalledFileInfoObjectList ]: boolean read GetUnKnownFiles;

end;

implementation
{$WARN UNIT_PLATFORM OFF}
uses
  Graphics,
  FileCtrl,
  UHelpContexts,
  UErrorHandlingOperations;

{ TDisplayUnknownFilesTobeDeletedForm }

procedure TDisplayUnknownFilesTobeDeletedForm.CreateMemberObjects;
const OPNAME = 'TDisplayUnknownFilesTobeDeletedForm.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try

    FFileDataObjects             := TObjectList.Create(True);
    tvFilesAndDirectory            := TFieldTreeView.Create(nil,nil);
    tvFilesAndDirectory.Parent     := Self;

    EdtDirPanel                    := TPanel.Create ( Self );
    EdtDirPanel.Parent             := Self;

    FMsgStr                        := TLabel.Create ( Self );
    FMsgStr.Parent                 := EdtDirPanel;

    btnContinuePanel               := TPanel.Create ( Self );
    btnContinuePanel.Parent        := Self;
    btnRemove                      := TButton.Create( Self);
    btnRemove.Parent               := btnContinuePanel;
    btnCancel                      := TButton.Create ( btnContinuePanel );
    btnCancel.Parent               := btnContinuePanel;
    Application.HelpFile := ExtractFilePath ( ApplicationExeName ) + '\Help\WRMF.HLP';
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDisplayUnknownFilesTobeDeletedForm.AssignHelpContext;
const OPNAME = 'TDisplayUnknownFilesTobeDeletedForm.AssignHelpContext';
begin
  inherited;
  try
    SetControlHelpContext(tvFilesAndDirectory, HC_Uninstall);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDisplayUnknownFilesTobeDeletedForm.OnHelpRequest ( Command: Word; Data: Integer;var CallHelp: Boolean ) : Boolean;
const OPNAME = 'TDisplayUnknownFilesTobeDeletedForm.OnHelpRequest';
begin
  Result := FALSE;
  try
    if( Data = HC_UninstallDialog ) then
      Application.OnHelp(Command,Data,CallHelp);
      //Application.HelpContext ( Data );

    CallHelp := True;
    Result   := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDisplayUnknownFilesTobeDeletedForm.DestroyMemberObjects;
const OPNAME = 'TDisplayUnknownFilesTobeDeletedForm.DestroyMemberObjects';
begin
  inherited;
  try
    FreeAndNil ( FFileDataObjects );
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDisplayUnknownFilesTobeDeletedForm.Initialise : Boolean;
const OPNAME = 'TDisplayUnknownFilesTobeDeletedForm.Initialise';
begin
  Result := False;
  try

    Self.Height                    := 374;
    Self.Width                     := 565;
    Self.BorderStyle               := bsDialog;
    Self.Position                  := poScreenCenter;
    Self.Caption                   := 'Remove Imported Files and Directories';//FAppModules.Language.GetString('Message.ImportedFiles');

    EdtDirPanel.Align              := alTop;

    FMsgStr.Top                    := 10;
    FMsgStr.Width                  := ClientWidth;
    FMsgStr.Height                 := 20;
    FMsgStr.AutoSize               := False;
    FMsgStr.WordWrap               := False;
    FMsgStr.Caption                := '';
    FMsgStr.Layout                 := tlCenter;
    FMsgStr.Font.Color             := clBlue;
    FMsgStr.Font.Style             := [ fsBold ];
    FMsgStr.Left                   := 20;


    btnContinuePanel.Align         := alBottom;
    tvFilesAndDirectory.Align      := alClient;

    btnRemove.Top                := 8;
    btnRemove.Left               := 437;
    btnRemove.Height             := 25;
    btnRemove.Caption            := 'Remove';//FAppModules.Language.GetString('ButtonCaption.Remove');
    btnRemove.Enabled            := True;
    btnRemove.OnClick            := DoUninstall;
    btnRemove.Enabled            := True;

    btnCancel.Top                  := 8;
    btnCancel.Left                 := 333;
    btnCancel.Height               := 25;
    btnCancel.Caption              := 'Close';//FAppModules.Language.GetString('ButtonCaption.Close');
    btnCancel.Enabled              := False;
    btnCancel.OnClick              := DobtnCancelClick;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDisplayUnknownFilesTobeDeletedForm.DobtnCancelClick ( Sender : TObject );
const OPNAME = 'TDisplayUnknownFilesTobeDeletedForm.DobtnCancelClick';
begin
  try
    Close;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDisplayUnknownFilesTobeDeletedForm.GetUnKnownFiles ( AFileInfoObjectList : TInstalledFileInfoObjectList ) : boolean;
const OPNAME = 'TDisplayUnknownFilesTobeDeletedForm.GetUnKnownFiles';
var
  LPath: string;
  LOldCursor : TCursor;
begin
  Result := False;
  try
    FFilecount := 0;
    if (AFileInfoObjectList <> nil) then
    begin
      FFileInfoObjectList := AFileInfoObjectList;
      FFileDataObjects.Clear;
      tvFilesAndDirectory.Items.Clear;
      LPath := ExtractFilePath (ApplicationExeName);
      InitialDir :=  LPath;
      if (SysUtils.DirectoryExists(LPath)) then
      begin
        if (Trim(LPath) <> '' ) then
        begin
          LOldCursor := Screen.Cursor;
          try
            Screen.Cursor := crHourGlass;
            PopulateTreeView ( AFileInfoObjectList, LPath, nil );
            tvFilesAndDirectory.Items.AlphaSort ( True );
            if ( FFilecount = 0 ) then
              tvFilesAndDirectory.Items.Clear;
          finally
            Screen.Cursor := LOldCursor;
          end;
        end;
      end;
      Result := True;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDisplayUnknownFilesTobeDeletedForm.PopulateTreeView ( AFileInfoObjectList : TInstalledFileInfoObjectList; APath : string; AMainNode : TTreeNode );
const OPNAME = 'TDisplayUnknownFilesTobeDeletedForm.PopulateTreeView';
var
  LSearchRec: TSearchRec;
  LMoreFiles: boolean;
  LNode: TTreenode;
  LFileData : TFileDataObject;
  LSearchStr: string;
  LAttributes : word;
  LInstalledFiles : TInstalledFiles;
begin
  try
    Application.ProcessMessages;
    if (Length(Trim(APath)) <> 0) then
    begin
      LFileData := TFileDataObject.Create;
      FFileDataObjects.Add(LFileData);
      LFileData.IsDirectory := True;
      APath := IncludeTrailingPathDelimiter(APath);
      LFileData.FileName    := APath;
      APath := ExcludeTrailingPathDelimiter(APath);
      LNode := tvFilesAndDirectory.Items.AddChildObjectFirst(AMainNode, ExtractFileName(APath), LFileData);
      APath := IncludeTrailingPathDelimiter(APath);
      LSearchStr := IncludeTrailingPathDelimiter(APath)+ '*.*';
      LMoreFiles := (FindFirst(LSearchStr, faAnyFile, LSearchRec) = 0);
      while LMoreFiles do
      begin
        if ( LSearchRec.Name [ 1 ] <> '.' ) then
        begin
        {$WARN SYMBOL_PLATFORM OFF}
          if( ( LSearchRec.Attr and faDirectory) <> 0 ) and
            ( ( LSearchRec.Attr and not SysUtils.faSysFile ) <> 0 ) and
            ( ( LSearchRec.Attr and not SysUtils.faReadOnly ) <> 0 ) then
          begin
            LInstalledFiles := AFileInfoObjectList.InstalledFileByName [ LSearchRec.Name ];
            if LInstalledFiles = nil then
            begin
              PopulateTreeView ( AFileInfoObjectList, APath + LSearchRec.Name, LNode );

            end;
          end
          else
          begin
            if( ( ExtractFileExt ( LSearchRec.Name ) ) <> '' ) then
            begin
              LAttributes := SysUtils.FileGetAttr ( APath +  LSearchRec.Name );
              if ( ( LAttributes and not SysUtils.faReadOnly ) <> 0 ) or
                 ( ( LAttributes and not SysUtils.faSysFile ) <> 0 )  or
                 ( ( LAttributes and not SysUtils.faArchive )  <> 0 ) or
                 ( ( LAttributes and not SysUtils.faHidden )  <> 0 ) then
               begin
                 LInstalledFiles := AFileInfoObjectList.InstalledFileByName [ LSearchRec.Name ];
                 if LInstalledFiles = nil then
                 begin
                   LFileData := TFileDataObject.Create;
                   LFileData.IsDirectory := False;
                   LFileData.FileName    := APath + LSearchRec.Name;
                   LFileData.Directory   := APath;
                   FFileDataObjects.Add ( LFileData );
                   inc ( FFilecount );
                   tvFilesAndDirectory.Items.AddChildObjectFirst ( LNode, LSearchRec.Name, LFileData );
                 end;
               end;
            end;
          end;
        end;
        LMoreFiles := (FindNext(LSearchRec) = 0);
        {$WARN SYMBOL_PLATFORM ON}
      end;
      SysUtils.FindClose ( LSearchRec );
    end;
//    tvFilesAndDirectory.FullExpand;
    SetLabelCaptions;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDisplayUnknownFilesTobeDeletedForm.Resize;
const OPNAME = 'TDisplayUnknownFilesTobeDeletedForm.Resize';
begin
  inherited;
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDisplayUnknownFilesTobeDeletedForm.DoUninstall ( Sender : TObject );
const OPNAME = 'TDisplayUnknownFilesTobeDeletedForm.DoUninstall';
var
  LIndex : integer;
  LFileName : string;
  LAttributes, LNewAttributes: Word;
  LDirectory : string;
begin
  try
    if ( FFileDataObjects.Count > 0 ) then
    begin
      {$WARN SYMBOL_PLATFORM OFF}
      for LIndex := 0 to FFileDataObjects.Count -1 do
      begin
        if not TFileDataObject ( FFileDataObjects.Items[LIndex] ).IsDirectory then
        begin
          LFileName  := TFileDataObject ( FFileDataObjects.Items [ LIndex ] ).FileName;
          LDirectory := TFileDataObject ( FFileDataObjects.Items [ LIndex ] ).Directory;
          LAttributes := SysUtils.FileGetAttr ( LFileName );
          LNewAttributes := LAttributes;
          LNewAttributes := ( LNewAttributes and not SysUtils.faReadOnly );
          LNewAttributes := ( LNewAttributes and not SysUtils.faSysFile );
          LNewAttributes := ( LNewAttributes and not SysUtils.faArchive );
          LNewAttributes := ( LNewAttributes and not SysUtils.faHidden );
          SysUtils.FileSetAttr ( LFileName, LNewAttributes );
          SysUtils.DeleteFile ( LFileName );
          DoRemoveDir ( LDirectory );
       {$WARN SYMBOL_PLATFORM ON}
        end;
      end;
    end;
    tvFilesAndDirectory.Items.Clear;
    FFilecount := 0;
    PopulateTreeView ( FFileInfoObjectList, InitialDir, nil );
    SetLabelCaptions;
    if FFilecount = 0 then
      tvFilesAndDirectory.Items.Clear;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;
procedure TDisplayUnknownFilesTobeDeletedForm.SetLabelCaptions;
const OPNAME = 'TDisplayUnknownFilesTobeDeletedForm.SetLabelCaptions';
begin
  try
    if FFilecount > 1 then
    begin
      FMsgStr.Caption := Format ( 'There are %d Imported files to be deleted in displayed folders bellow',[ FFilecount ] );
      btnRemove.Enabled := True;
      btnCancel.Enabled   := False;
    end
    else
    if FFilecount = 1 then
    begin
      FMsgStr.Caption := 'There is only 1 Imported file to be deleted';
      btnRemove.Enabled := True;
      btnCancel.Enabled   := False;
    end
    else
    if FFilecount = 0 then
    begin
      FMsgStr.Caption := 'There are no Imported files to be deleted';
      btnRemove.Enabled := False;
      btnCancel.Enabled   := True;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDisplayUnknownFilesTobeDeletedForm.DoRemoveDir ( ADirectory : string );
const OPNAME = 'TDisplayUnknownFilesTobeDeletedForm.DoRemoveDir';
var
  LPos : integer;
  LTempDir : string;
  LSearchRec : TSearchRec;
  LMoreFiles : boolean;
  LSearchStr : string;
  LFileCount : integer;
  LFolder : string;
begin
  try
    LTempDir := ADirectory;
    while UpperCase ( LTempDir ) <> UpperCase ( InitialDir ) do
    begin
      LFileCount := 0;
      LTempDir := IncludeTrailingPathDelimiter ( LTempDir );
      LFolder := ExtractFileName ( ExcludeTrailingPathDelimiter ( LTempDir ) );
      LSearchStr := IncludeTrailingPathDelimiter ( LTempDir ) + '*.*';
      LMoreFiles := ( FindFirst ( LSearchStr, faAnyFile, LSearchRec ) = 0);
      while LMoreFiles do
      begin
        if ( LSearchRec.Name [ 1 ] <> '.' ) then
          inc ( LFileCount );
        LMoreFiles := ( FindNext ( LSearchRec ) = 0 );
      end;
      SysUtils.FindClose ( LSearchRec );
      if LFileCount = 0 then
        SysUtils.RemoveDir ( LTempDir )
      else
        Break;
      LPos := Pos ( LFolder, LTempDir );
      Delete ( LTempDir, LPos, Length ( LFolder ) + 1 );
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.