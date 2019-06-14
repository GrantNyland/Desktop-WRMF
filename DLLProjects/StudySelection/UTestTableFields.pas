unit UTestTableFields;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, DB, UAbstractObject,
  UDbTablePropertyList,UDbTableProperty,UDbTableFieldsList,UDbTableField,
  Bde.DBTables;

type
  TForm1 = class(TForm)
    Database1: TDatabase;
    Panel1: TPanel;
    Panel2: TPanel;
    btnRefresh: TButton;
    btnExit: TButton;
    ProgressBar1: TProgressBar;
    Label1: TLabel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    Memo1: TMemo;
    Memo2: TMemo;
    Memo3: TMemo;
    Table1: TTable;
    btnStop: TButton;
    TabSheet4: TTabSheet;
    Memo4: TMemo;
    TabSheet5: TTabSheet;
    Memo5: TMemo;
    TabSheet6: TTabSheet;
    Memo6: TMemo;
    btnDeleteModelData: TButton;
    Label2: TLabel;
    cbxModel: TComboBox;
    TabSheet7: TTabSheet;
    Memo7: TMemo;
    ProgressBar2: TProgressBar;
    Label3: TLabel;
    Query1: TQuery;
    procedure btnExitClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnStopClick(Sender: TObject);
    procedure btnDeleteModelDataClick(Sender: TObject);
  private
    { Private declarations }
    FStop : boolean;
    FTablePropertyList:TDbTablePropertyList;
    FTableNames,
    FTableFields: TStringList;
    FSchema:TTableFieldsDefList;
    FTempStringList : TStringList;
    function OpenDatabase: boolean;
    function CloseDatabase: boolean;
    function DeleteTableData(AModel: widestring): Boolean;
    function GetTableNames(AContainer: TStrings): boolean;
    function GetTableProperties(ATableName: string; var AAllFields, AIndexFields: string): boolean;
    function ValidateATableInDB(ATableName: string; AAllFields, AIndexFields: string): boolean;
    function ValidateATableInDBForClearing(ATableName: string; AAllFields, AIndexFields,AModel: string): boolean;
    procedure CheckForDeletedTables;

  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnExitClick(Sender: TObject);
begin
 Close;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FTableNames := TStringList.Create;
  FTableFields     := TStringList.Create;
  FTablePropertyList:= TDbTablePropertyList.Create(nil);
  FSchema  := TTableFieldsDefList.Create(nil);
  FSchema.Initialise;
  FTablePropertyList.Initialise;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FTableNames.Free;
  FTableFields.Free;
  FTablePropertyList.Free;
  FSchema.Free;
end;

function TForm1.OpenDatabase: boolean;
begin
  Label1.Caption := 'Opening the database';
  Application.ProcessMessages;
  Database1.Open;
  Result := True;
end;

function TForm1.CloseDatabase: boolean;
begin
  Label1.Caption := 'Closing the database';
  Application.ProcessMessages;
  Database1.Close;
  Result := True;
end;

function TForm1.GetTableNames(AContainer: TStrings): boolean;
begin
  Label1.Caption := 'Getting table names from the database';
  Application.ProcessMessages;
  Database1.GetTableNames(AContainer);
  Result := True;
end;

procedure TForm1.btnRefreshClick(Sender: TObject);
var
  LIndex: integer;
  LTableName,LAllFields, LIndexFields: string;
begin
  Label1.Caption := 'Initialising';
  Memo1.Lines.Clear;
  Memo2.Lines.Clear;
  Memo3.Lines.Clear;
  Memo4.Lines.Clear;
  Memo5.Lines.Clear;
  Memo6.Lines.Clear;
  FTableNames.Clear;
  FTableFields.Clear;
  ProgressBar1.Position := 0;
  Label1.Caption := '';
  FStop := False;

  if OpenDatabase then
  begin
    btnStop.Enabled := True;
    btnExit.Enabled := False;
    try
      if GetTableNames(FTableNames) then
      begin
        ProgressBar1.Max := FTableNames.count;
        for LIndex := 0 to FTableNames.count -1 do
        begin
          LTableName := FTableNames[LIndex];
          ProgressBar1.StepIt;
          Label1.Caption := 'Processing ('+ LTableName +') ' + IntToStr(LIndex) +' out of '+ intToStr(FTableNames.count);

          Application.ProcessMessages;
          if((LIndex mod 5) = 0) then
            if FStop then Break;

          if GetTableProperties(LTableName,LAllFields,LIndexFields) then
          begin
           if ValidateATableInDB(LTableName,LAllFields,LIndexFields) then
           begin
           end;
         end;
         Memo1.Lines.Add('Processing ('+ LTableName +') Completed');
       end;
       CheckForDeletedTables;
      end;
    finally
      btnStop.Enabled := False;
      btnExit.Enabled := True;
      CloseDatabase;
    end;
  end;
end;

function TForm1.GetTableProperties(ATableName: string; var AAllFields,
  AIndexFields: string): boolean;
begin
  AAllFields    := '';
  AIndexFields  := '';
  Database1.GetFieldNames(ATableName,FTableFields);
  AAllFields    := FTableFields.CommaText;

  Table1.Close;
  Table1.TableName := ATableName;
  Table1.IndexDefs.Update;
  Table1.Open;

  if Table1.IndexDefs.Count > 0 then
    AIndexFields := Table1.IndexDefs[0].Fields;
  AIndexFields := StringReplace(AIndexFields,';',',',[rfReplaceAll]);
  
  Result := True;
end;

function TForm1.ValidateATableInDB(ATableName, AAllFields,AIndexFields: string): boolean;
var
  LTableProperty:TAbstractDbTableProperty;
  LIndexNames,
  LFieldNames: TStringList;
  LErrorCount,
  LIndex :integer;
  LTableFieldsDef:TAbstractTableFieldsDef;
begin
  LTableProperty := FTablePropertyList.TablePropertyByName[ATableName];
  if Assigned(LTableProperty)  then
  begin
    if(LTableProperty.FieldNames.CommaText <> AAllFields) then
    begin
      Memo4.Lines.Add(ATableName + ': Field names are not the same or are in wrong order');
      LFieldNames := TStringList.Create;
      try
        LFieldNames.CommaText := AAllFields;
        for LIndex := 0 to LFieldNames.Count -1 do
          if(LTableProperty.FieldNames.IndexOf(LFieldNames[LIndex]) < 0) then
             Memo4.Lines.Add('   '+LFieldNames[LIndex]+' : Field in database but not in field property');
        for LIndex := 0 to LTableProperty.FieldNames.Count -1 do
          if(LFieldNames.IndexOf(LTableProperty.FieldNames[LIndex]) < 0) then
             Memo4.Lines.Add('   '+LTableProperty.FieldNames[LIndex]+' : Field in field property but not in database');

      finally
        LFieldNames.Free;
      end;
    end;
    if(LTableProperty.IndexFieldNames.CommaText <> AIndexFields) then
    begin
      Memo5.Lines.Add(ATableName + ': Index Field names are not the same');
      LIndexNames := TStringList.Create;
      try
        LIndexNames.CommaText := AIndexFields;
        for LIndex := 0 to LIndexNames.Count -1 do
          if(LTableProperty.IndexFieldNames.IndexOf(LIndexNames[LIndex]) < 0) then
             Memo5.Lines.Add('   '+LIndexNames[LIndex]+' : Index Field in database but not in field property');
        for LIndex := 0 to LTableProperty.IndexFieldNames.Count -1 do
          if(LIndexNames.IndexOf(LTableProperty.IndexFieldNames[LIndex]) < 0) then
             Memo5.Lines.Add('   '+LTableProperty.IndexFieldNames[LIndex]+' : Index Field in field property but not in database');
      finally
        LIndexNames.Free;
      end;
    end;
  end
  else
    Memo2.Lines.Add(ATableName + ': in database but not in table properties');

  LTableFieldsDef := FSchema.TableFieldsDefByName[ATableName];
  if(LTableFieldsDef = nil) then
    Memo6.Lines.Add(ATableName + ': in database but not in database schema')
  else
  begin
    LFieldNames := TStringList.Create;
    try
      LFieldNames.CommaText := AAllFields;
      LErrorCount := 0;
      for LIndex := 0 to LFieldNames.Count -1 do
      begin
        if(LTableFieldsDef.FieldByName[LFieldNames[LIndex]] = nil) then
        begin
          LErrorCount := LErrorCount + 1;
          if(LErrorCount = 1) then
            Memo6.Lines.Add(ATableName+':');
          Memo6.Lines.Add('   '+LFieldNames[LIndex]+' : Field in database but not in database schema');
        end;
      end;
      for LIndex := 0 to LTableFieldsDef.FieldsCount -1 do
      begin
        if(LFieldNames.IndexOf(LTableFieldsDef.FieldByIndex[LIndex].FieldName) < 0) then
        begin
          LErrorCount := LErrorCount + 1;
          if(LErrorCount = 1) then
            Memo6.Lines.Add(ATableName+':');
           Memo6.Lines.Add('   '+LTableProperty.FieldNames[LIndex]+' : Field in database schema but not in database');
        end;
      end;
    finally
      LFieldNames.Free;
    end;
  end;
  Result := True;
end;

procedure TForm1.btnStopClick(Sender: TObject);
begin
  FStop := True;
end;

procedure TForm1.CheckForDeletedTables;
var
  LTableProperty:TAbstractDbTableProperty;
  LIndex: integer;
begin
  for LIndex := 0 to FTablePropertyList.TableCount -1 do
  begin
     LTableProperty := FTablePropertyList.TablePropertyByIndex[LIndex];
    if (FTableNames.IndexOf(LTableProperty.TableName) < 0) then
      Memo3.Lines.Add(LTableProperty.TableName + ': No longer exists in the database');
  end;
end;

function TForm1.DeleteTableData(AModel: widestring): Boolean;
var
  LIndex: integer;
  LTableName,LAllFields, LIndexFields: string;
begin
  Label3.Caption := 'Initialising';
  FTableNames.Clear;
  FTableFields.Clear;
  ProgressBar2.Position := 0;
  Label3.Caption := '';
  FStop := False;
  Result := False;
  
  if OpenDatabase then
  begin
    btnStop.Enabled := True;
    btnExit.Enabled := False;
    try
      if GetTableNames(FTableNames) then
      begin
        ProgressBar2.Max := FTableNames.count;
        for LIndex := 0 to FTableNames.count -1 do
        begin
          LTableName := FTableNames[LIndex];
          ProgressBar2.StepIt;
          Label3.Caption := 'Processing ('+ LTableName +') ' + IntToStr(LIndex) +' out of '+ intToStr(FTableNames.count);

          Application.ProcessMessages;
          if((LIndex mod 5) = 0) then
            if FStop then Break;

          if GetTableProperties(LTableName,LAllFields,LIndexFields) then
          begin
           if ValidateATableInDBForClearing(LTableName,LAllFields,LIndexFields,AModel) then
           begin
             Result := True;
           end;
         end;
       end;
      end;
    finally
      btnStop.Enabled := False;
      btnExit.Enabled := True;
      CloseDatabase;
    end;
  end;
end;

procedure TForm1.btnDeleteModelDataClick(Sender: TObject);
var
  x     : Integer;
  vText : WideString;
begin
  FreeAndNil(FTempStringList);
  FTempStringList := TStringList.Create;
  try
    Memo7.Lines.Clear;
    FTempStringList.Clear;
    vText := Trim(cbxModel.Text);
    if vText = 'All' then
    begin
      for x:=0 to cbxModel.Items.Count-1 do
        if Trim(cbxModel.Items[x]) <> 'All' then
        begin
          FTempStringList.Add('Clearing data for model: '+cbxModel.Items[x]);
          DeleteTableData(cbxModel.Items[x]);
        end;
    end else
    begin
      FTempStringList.Add('Clearing data for model: '+vText);
      DeleteTableData(vText);
    end;
  finally
    FreeAndNil(FTempStringList);
  end;
end;

function TForm1.ValidateATableInDBForClearing(ATableName, AAllFields, AIndexFields,AModel: string): boolean;
var
  LTableProperty:TAbstractDbTableProperty;
  LSQL : widestring;
begin
  LTableProperty := FTablePropertyList.TablePropertyByName[ATableName];
  if Assigned(LTableProperty) and (LTableProperty.TableGroup = tgModelData) then
  begin
    if LTableProperty.IndexFieldNames.IndexOf('Model') <> -1 then
    begin
      LSQL := 'DELETE FROM '+ATableName+' WHERE MODEL = '''+AModel+'''';
      Query1.Close;
      Query1.SQL.Clear;
      Query1.SQL.Add(LSQL);
      Query1.ExecSQL;
      Query1.Close;
      FTempStringList.Add('Cleared table : '+ ATableName +' completed');
    end else
      FTempStringList.Add(ATableName+' does not contain a "Model" column');
  end
  else
    FTempStringList.Add(ATableName + ': not ot type "tgModelData"');
  Result := True;
end;

end.
