unit UPageViewer;

interface
uses
  Types,
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  Contnrs,
  VCL.ExtCtrls,
  VCL.Forms,
  UAbstractComponent;

type

  TImageViewer = class(TAbstractTabSheet)
  protected
    FScrollBox:TScrollBox;
    FImage: TImage;
    procedure CreateMemberObjects; override;
    procedure OnImageClick(Sender: TObject);
    procedure OnScrollBoxMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure OnScrollBoxMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    function GetToolBar: TAbstractToolBar; override;
    procedure DoShow; override;
	public
    Property Image:TImage read FImage;
  end;

  TPageViewer = class(TAbstractPageControl)
  protected
    FImageContainer: TObjectList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure SetImageCount(AValue: integer);
    function GetImageCount: integer;
    function GetImageByIndex(AIndex:integer): TImageViewer;
	public
    procedure ClearPages;
    property ImageCount:integer read GetImageCount write SetImageCount;
    property ImageByIndex[AIndex:integer]: TImageViewer read GetImageByIndex;
  end;

implementation

uses
  SysUtils,
  UErrorHandlingOperations;


procedure TImageViewer.CreateMemberObjects;
const OPNAME = 'TImageViewer.CreateMemberObjects';
begin
  inherited;
  try
    FScrollBox        := TScrollBox.Create(Self);
    FImage            := TImage.Create(Self);
    FScrollBox.Parent := Self;
    FImage.Parent     := FScrollBox;
    FScrollBox.Align  := alClient;
    //FImage.Align      := alClient;
    FScrollBox.AutoScroll := True;
    FImage.Left       := 0;
    FImage.Top        := 0;
    FImage.AutoSize   := True;
    FImage.OnClick    := OnImageClick;
    FScrollBox.OnMouseWheelDown := OnScrollBoxMouseWheelDown;
    FScrollBox.OnMouseWheelUp   := OnScrollBoxMouseWheelUp;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TImageViewer.OnImageClick(Sender: TObject);
const OPNAME = 'TImageViewer.OnImageClick';
begin
  try
    FAppModules.MainForm.MainForm.ActiveControl := FScrollBox;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TImageViewer.DoShow;
const OPNAME = 'TImageViewer.DoShow';
begin
  inherited;
  try
    FAppModules.MainForm.MainForm.ActiveControl := FScrollBox;
  except on E : Exception do HandleError(E,OPNAME); end;

end;

procedure TImageViewer.OnScrollBoxMouseWheelDown(Sender: TObject;  Shift: TShiftState; MousePos: TPoint;
          var Handled: Boolean);
const OPNAME = 'TImageViewer.OnScrollBoxMouseWheelDown';
begin
  try
    if FScrollBox.VertScrollBar.IsScrollBarVisible and
      (FScrollBox.VertScrollBar.Position < FScrollBox.VertScrollBar.Range) then
    begin
      FScrollBox.VertScrollBar.Position := FScrollBox.VertScrollBar.Position + FScrollBox.VertScrollBar.Increment;
      Handled := True;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TImageViewer.OnScrollBoxMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint;
          var Handled: Boolean);
const OPNAME = 'TImageViewer.OnScrollBoxMouseWheelUp';
begin
  try
    if FScrollBox.VertScrollBar.IsScrollBarVisible and
      (FScrollBox.VertScrollBar.Position > 0) then
    begin
      FScrollBox.VertScrollBar.Position := FScrollBox.VertScrollBar.Position - FScrollBox.VertScrollBar.Increment;
      Handled := True;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

{ TPageViewer }

procedure TPageViewer.CreateMemberObjects;
const OPNAME = 'TPageViewer.CreateMemberObjects';
begin
  inherited;
  try
    FImageContainer  := TObjectList.Create(False);
    Self.Align       := alClient;
    Self.TabPosition := tpBottom;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TPageViewer.DestroyMemberObjects;
const OPNAME = 'TPageViewer.DestroyMemberObjects';
begin
  inherited;
  try
    FreeAndNil(FImageContainer);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TPageViewer.GetImageByIndex(AIndex: integer): TImageViewer;
const OPNAME = 'TPageViewer.GetImageByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FImageContainer.Count) then
      Result := TImageViewer(FImageContainer.Items[AIndex])
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TPageViewer.GetImageCount: integer;
const OPNAME = 'TPageViewer.GetImageCount';
begin
  Result := 0;
  try
    Result := FImageContainer.Count;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TPageViewer.ClearPages;
const OPNAME = 'TPageViewer.ClearPages';
var
   LIndex: integer;
begin
  inherited;
  try
    FImageContainer.Clear;
    for LIndex := Self.PageCount -1 downto 0 do
    begin
      Self.Pages[LIndex].Free;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TPageViewer.SetImageCount(AValue: integer);
const OPNAME = 'TPageViewer.SetImageCount';
var
   LImageViewer: TImageViewer;
   LIndex: integer;
begin
  try
    ClearPages;
    for LIndex := 1 to AValue do
    begin
      LImageViewer := TImageViewer.Create(nil,FAppModules);
      FImageContainer.Add(LImageViewer);
      LImageViewer.PageControl := Self;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TImageViewer.GetToolBar: TAbstractToolBar;
const OPNAME = 'TImageViewer.GetToolBar';
begin
  Result := nil;
end;

end.

