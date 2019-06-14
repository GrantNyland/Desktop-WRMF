unit UImportHydsraData;

interface

uses
{$WARN UNIT_PLATFORM OFF}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.Buttons,
  Vcl.ExtCtrls, Vcl.FileCtrl, Data.DB, Data.Win.ADODB,//ZLibEx,
  System.ZLib,
  UAbstractObject,UDataSetType;

type
  TfrmImportRawDailyData = class(TForm)
    Memo1: TMemo;
    edtFilePath: TLabeledEdit;
    spbtnLoadDir: TSpeedButton;
    btnLoadFilesToDB: TBitBtn;
    Animate1: TAnimate;
    btnClose: TButton;
    ProgressBar: TProgressBar;
    procedure btnLoadFilesToDBClick(Sender: TObject);
    procedure spbtnLoadDirClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
  protected
    FAppModules : TAppModules;
  private
    { Private declarations }
    procedure SendFileToUnKnownStationsDir(AFile : string);
    function LoadFileToDB(AFile : string ; var AStationNum : string) : boolean;
    function GetDWAStationNumByFileName(AFile : string; var AStationID : integer;var AStationOldID : integer) : string;
    function GetSWASByFileName(AFile : string; var AStationID : integer;var AStationOldID : integer) : string;
    function GetOldStationID(AStationNumber : string) : integer;
    function StationUpdated(AData : TStrings; AStationID : integer) : boolean;
    procedure SetAppModules(AAppModules: TAppModules);
  public
    { Public declarations }
     property AppModules : TAppModules read FAppModules write SetAppModules;
  end;

var
  frmImportRawDailyData: TfrmImportRawDailyData;

implementation
uses vcl.ShellAnimations,UErrorHandlingOperations;
{$R *.dfm}

procedure TfrmImportRawDailyData.btnCloseClick(Sender: TObject);
const OPNAME = 'TfrmImportRawDailyData.btnCloseClick';
begin
  try
    close;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmImportRawDailyData.btnLoadFilesToDBClick(Sender: TObject);
const OPNAME = 'TfrmImportRawDailyData.btnLoadFilesToDBClick';
var
  LIndex: Integer;
  LDirStr,
  LStationNum : string;
begin
  try
    with Animate1 do
    try
      CommonAVI := aviCopyFile;
      Animate1.Visible := True;
      Active := True;
      ProgressBar.Max := Memo1.Lines.count*10;
      ProgressBar.Position := 0;
      for LIndex := 0 to Memo1.Lines.count-1 do
      begin
        if LoadFileToDB(Memo1.Lines.Strings[LIndex],LStationNum) then
        begin
          LDirStr := edtFilePath.Text+'\Imported_'+ FormatDateTime('yyyymmdd',Now);
          if not(System.SysUtils.DirectoryExists(LDirStr)) then
            System.SysUtils.ForceDirectories(LDirStr);
          CopyFile(PChar(edtFilePath.Text+'\'+Memo1.Lines.Strings[LIndex]),PChar(LDirStr +'\'+
          Copy(Memo1.Lines.Strings[LIndex],1, Pos('.', Memo1.Lines.Strings[LIndex])-1)+' - '+LStationNum+'.csv'),False);
          Memo1.Lines.Strings[LIndex] := Memo1.Lines.Strings[LIndex] + ' Import Done ';
        end
        else
          Memo1.Lines.Strings[LIndex] := Memo1.Lines.Strings[LIndex] + ' Error ' ;
        ProgressBar.StepIt;
        Application.ProcessMessages;
      end;
    finally
       Active := False;
       Animate1.Visible := False;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmImportRawDailyData.FormCreate(Sender: TObject);
const OPNAME = 'TfrmImportRawDailyData.FormCreate';
begin
  try
    Position := poScreenCenter;
    BorderStyle := bsDialog;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmImportRawDailyData.spbtnLoadDirClick(Sender: TObject);
const OPNAME = 'TfrmImportRawDailyData.spbtnLoadDirClick';
var
  LSearchRec: TSearchRec;
  LMoreFiles: boolean;
  LPath,
  LCSVFile,
  LSearchStr: string;
begin
  try
    if SelectDirectory(LPath,[],0) then
      edtFilePath.Text := LPath;
    if (Trim(edtFilePath.Text) <> '') then
    begin
      LCSVFile := IncludeTrailingPathDelimiter(edtFilePath.Text);
      LCSVFile := ExcludeTrailingPathDelimiter(LCSVFile);
      LCSVFile := IncludeTrailingPathDelimiter(LCSVFile);
      LSearchStr := IncludeTrailingPathDelimiter(LCSVFile)+ '*.*';
      LMoreFiles := (FindFirst(LSearchStr, faAnyFile, LSearchRec) = 0);
      Memo1.Lines.Clear;
      while LMoreFiles do
      begin
        Application.ProcessMessages;
        if (LSearchRec.Name[1] <> '.') then
        begin
          if (UpperCase(ExtractFileExt(LSearchRec.Name)) = '.CSV') then
            Memo1.Lines.Add(LSearchRec.Name)
        end;
        LMoreFiles := (FindNext(LSearchRec)= 0);
      end;
      System.SysUtils.FindClose(LSearchRec);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmImportRawDailyData.LoadFileToDB(AFile : string; var AStationNum : string) : boolean;
const OPNAME = 'TfrmImportRawDailyData.LoadFileToDB';
var
  LData : TStringList;
  LStationID, LStationOldID : integer;
  LBlob : TStream;
  LStationDataInMem : TMemoryStream;
  LSQL : string;
  LCompressedData: TZCompressionStream;
  LDataList: TStringList;
  LDataset : TAbstractModelDataset;
begin
  Result := False;
  try
    if Trim(AFile) <> '' then
    begin
      LData := TStringList.Create;
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataset);
      try
        LData.LoadFromFile(AFile);
        AStationNum := GetSWASByFileName(ExtractFileName(AFile),LStationID,LStationOldID);
        if Trim(AStationNum) = '' then
          AStationNum := GetDWAStationNumByFileName(ExtractFileName(AFile),LStationID,LStationOldID);

        if AStationNum <> '' then
        begin
          if not(StationUpdated(LData,LStationID)) then
          begin
            LSQL := 'INSERT INTO RainfallRawDailyData (StationID,OldStationID,StationNumber '+
                    ' ) VALUES('+
                    ':StationID,:OldStationID, :StationNumber '+
                    ')';
            LDataset.DataSet.Close;
            LDataset.SetSQL(LSQL);
            LDataset.SetParams(['OldStationID'],[IntToStr(LStationOldID)]);
            LDataset.SetParams(['StationID'],[IntToStr(LStationID)]);
            LDataset.SetParams(['StationNumber'],[AStationNum]);
            LDataset.ExecSQL;


            LDataset.DataSet.Close;
            LSQL := 'SELECT * FROM RainfallRawDailyData WHERE StationID = ' + IntToStr(LStationID);
            LDataset.SetSQL(LSQL);
            LDataset.DataSet.Open;

            LDataset.DataSet.Edit;
            LStationDataInMem := TMemoryStream.Create;
            LBlob := LDataset.DataSet.CreateBlobStream(LDataset.DataSet.FieldByName('RainfallData'), bmWrite);
            LCompressedData := TZCompressionStream.Create(LBlob);
            LDataList       := TStringList.Create;
            try
              LDataList.Text := LData.Text;
              LDataList.SaveToStream(LStationDataInMem);
              LCompressedData.CopyFrom(LStationDataInMem, 0);
              FreeAndNil(LCompressedData);
            finally
              FreeAndNil(LDataList);
              FreeAndNil(LBlob);
              FreeAndNil(LStationDataInMem);
            end;
            LDataset.DataSet.Post;
            Result := True;
          end;
        end
        else
        begin
          SendFileToUnKnownStationsDir(AFile);
          Result := False;
        end;
      finally
        FreeAndNil(LData);
        FreeAndNil(LDataset);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmImportRawDailyData.GetDWAStationNumByFileName(AFile : string; var AStationID : integer;var AStationOldID : integer) : string;
const OPNAME = 'TfrmImportRawDailyData.GetDWAStationNumByFileName';
var
  LModifiedStations : TStringList;
  LStationData : TStringList;
  LData : string;
  LIndex : integer;
  LCount : integer;
  LSQL : string;
  LDataset : TAbstractModelDataset;
  LStationNumber : string;
  LFound : boolean;
begin
  try
    Result := '';
    LModifiedStations := TStringList.Create;
    LStationData := TStringList.Create;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataset);
    try
      LModifiedStations.LoadFromFile(ExtractFilePath(ApplicationExeName)+'\WRCData\Modified_Rainfall_IMS.csv');
      if LModifiedStations.Count > 0 then
      begin
        LFound := False;
        for LIndex := 0 to LModifiedStations.Count-1 do
        begin
          LStationData.CommaText := LModifiedStations[LIndex];
          if LStationData.Count > 5 then
          begin
           LData := Trim(LStationData[6]);
           if (Copy(AFile,1, Pos('.', AFile)-1) = LData) then
           begin
             if (Trim(LStationData[4]) <> '') then
               AStationOldID := StrToInt(LStationData[4]);

             LStationNumber := LStationData[1];
             LStationNumber := LStationNumber +' '+ LStationData[2];
             LFound := True;
             Break;
           end;
          end
          else
           Continue;
        end;

        if LFound then
        begin
          LStationNumber := '%'+Trim(LStationNumber)+'%';
          LSQL := ' SELECT * FROM RainfallStations '+
                  ' WHERE StationNumber like '+QuotedStr(LStationNumber) ;
          LDataset.DataSet.Close;
          LDataset.SetSQL(LSQL);;
          LDataset.DataSet.Open;
          LDataset.DataSet.Last;
          LDataset.DataSet.First;
          if LDataset.DataSet.RecordCount > 1 then
          begin
            while not LDataset.DataSet.Eof do
            begin
              if not (Pos('A',Trim(LDataset.DataSet.FieldByName('StationNumber').AsString)) > 0) then
              begin
                LCount := Memo1.Lines.IndexOf(AFile);
                if LCount>-1 then
                  Memo1.Lines.Strings[LCount] := Memo1.Lines.Strings[LCount] +
                  Format('There are Duplicates. Station No. : %s is used by station ID %d',[LDataset.DataSet.FieldByName('StationNumber').AsString,LDataset.DataSet.FieldByName('StationID').AsInteger]);
                AStationID := LDataset.DataSet.FieldByName('StationID').AsInteger;
                Result := LDataset.DataSet.FieldByName('StationNumber').AsString;
                Break;
              end;
              LDataset.DataSet.Next;
            end;
          end
          else
          begin
            AStationID := LDataset.DataSet.FieldByName('StationID').AsInteger;
            Result := LDataset.DataSet.FieldByName('StationNumber').AsString;
          end;

        end;
      end;
    finally
      FreeAndNil(LModifiedStations);
      FreeAndNil(LStationData);
      FreeAndNil(LDataset);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmImportRawDailyData.GetSWASByFileName(AFile : string; var AStationID : integer;var AStationOldID : integer) : string;
const OPNAME = 'TfrmImportRawDailyData.GetSWASByFileName';
var
  LStationNumber : string;
  LSQL : string;
  LDataset : TAbstractModelDataset;
begin
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataset);
    try
      LStationNumber := '%'+Trim(Copy(AFile,1, Pos('.', AFile)-1))+'%';
      LSQL := ' SELECT * FROM RainfallStations '+
              ' WHERE StationNumber Alike '+QuotedStr(LStationNumber) ;
      LDataset.DataSet.Close;
      LDataset.SetSQL(LSQL);;
      LDataset.DataSet.Open;
      LDataset.DataSet.Last;
      LDataset.DataSet.First;
      AStationID := LDataset.DataSet.FieldByName('StationID').AsInteger;
      Result := LDataset.DataSet.FieldByName('StationNumber').AsString;
      AStationOldID := GetOldStationID(Result);
    finally
      FreeAndNil(LDataset);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmImportRawDailyData.GetOldStationID(AStationNumber : string) : integer;
const OPNAME = 'TfrmImportRawDailyData.GetOldStationID';
var
  LSQL : string;
  LDataset : TAbstractModelDataset;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataset);
    try
      LSQL := ' SELECT * FROM RainfallDailyData '+
              ' WHERE StationNumber Alike '+QuotedStr(AStationNumber) ;
      LDataset.DataSet.Close;
      LDataset.SetSQL(LSQL);
      LDataset.DataSet.Open;
      LDataset.DataSet.Last;
      LDataset.DataSet.First;
      Result := LDataset.DataSet.FieldByName('OldStationID').AsInteger;
    finally
      FreeAndNil(LDataset);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmImportRawDailyData.SetAppModules(AAppModules: TAppModules);
const OPNAME = 'TfrmImportRawDailyData.SetAppModules';
begin
  try
    FAppModules := AAppModules;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmImportRawDailyData.StationUpdated(AData : TStrings;AStationID : integer) : boolean;
const OPNAME = 'TfrmImportRawDailyData.StationUpdated';
var
  LSQL : string;
  LDecompressionStream : TZDecompressionStream;
  LCompressedData: TZCompressionStream;
  LNewBlobStream,
  LBlobStream : TStream;
  LStationData : TStringList;
  LStationValues : TStringList;
  LIndex : integer;
  LStationDataInMem : TMemoryStream;
  LDataset : TAbstractModelDataset;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataset);
    try
      LDataset.DataSet.Close;
      LSQL := 'SELECT * FROM RainfallRawDailyData WHERE StationID = ' + IntToStr(AStationID);
      LDataset.SetSQL(LSQL);
      LDataset.DataSet.Open;
      if LDataset.DataSet.RecordCount>0 then
      begin
        Result := True;
        LStationData := TStringList.Create;
        LStationValues := TStringList.Create;
        LBlobStream := LDataset.DataSet.CreateBlobStream(LDataset.DataSet.FieldByName('RainfallData'), bmRead);
        LDecompressionStream := TZDecompressionStream.Create(LBlobStream);
        try
          LStationData.LoadFromStream(LDecompressionStream);
          if (LStationData.Count>0) and (AData.Count>0) then
          begin
            LStationValues.CommaText :=  LStationData[LStationData.Count-1];
            if LStationData.Count = AData.Count then
              Exit
            else
            if (AData.Count>LStationData.Count) then
            begin
              for LIndex := (LStationData.Count-1) to AData.Count-1 do
                LStationData.Add(AData[LIndex]);
              LDataset.DataSet.Edit;
              LStationDataInMem := TMemoryStream.Create;
              LNewBlobStream := LDataset.DataSet.CreateBlobStream(LDataset.DataSet.FieldByName('RainfallData'), bmWrite);
              LCompressedData := TZCompressionStream.Create(LNewBlobStream);
              try
                LStationData.SaveToStream(LStationDataInMem);
                LCompressedData.CopyFrom(LStationDataInMem, 0);
                FreeAndNil(LCompressedData);
              finally
                FreeAndNil(LNewBlobStream);
                FreeAndNil(LStationDataInMem);
              end;
              LDataset.DataSet.Post;
            end;
          end;
        finally
          FreeAndNil(LStationData);
          FreeAndNil(LDecompressionStream);
          FreeAndNil(LStationValues);
        end;
      end;
    finally
      FreeAndNil(LDataset);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TfrmImportRawDailyData.SendFileToUnKnownStationsDir(AFile : string);
const OPNAME = 'TfrmImportRawDailyData.SendFileToUnKnownStationsDir';
var
  LDirStr : string;
begin
  try
    LDirStr := edtFilePath.Text+'\UnknownStations_'+ FormatDateTime('yyyymmdd',Now);
    if not(System.SysUtils.DirectoryExists(LDirStr)) then
      System.SysUtils.ForceDirectories(LDirStr);
    CopyFile(PChar(edtFilePath.Text+'\'+AFile),PChar(LDirStr +'\'+Copy(AFile,1, Pos('.', AFile)-1)+' - Not Found.csv'),False);
  except on E: Exception do HandleError(E, OPNAME) end;
end;



end.
