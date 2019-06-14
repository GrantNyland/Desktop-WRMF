unit CheckCombo;

{
	TCheckedComboBox ver 1.3
	tested on D3,D4,D5
	ComboBox with CheckListBox
	When you check/uncheck an item this is added/removed in the visual part of combo.
	It has also a popup with Select all, unSelect all items with other additions
	-----------------------------------------------------------------
	This component was created 20-Deceber 1999 by Tsourinakis Antonis
	Can be freely used and distributed in commercial and
	private environments. You are free to copy and modify the source code.
	If you want I would like this notice to be provided as is.
	-----------------------------------------------------------------
	feel free to contact me:
	tsoyran@otenet.gr
	-----------------------------------------------------------------
	special thanks to Jan Verhoeven
					email  : jan1.verhoeven@wxs.nl
					website: http://members.xoom.com/JanVee/jfdelphi.htm
					for his help
	and to any other component creator from which a get ideas
	-----------------------------------------------------------------
	History

	Ver 1.3 2000/4/8

		Special Properties
			CapSelectAll 		The caption on popupmenu item for Check All
			-----------------------------------------------------------------
			CapDeSelectAll 	The caption on popupmenu item for UnCheck All
			-----------------------------------------------------------------
			NotFocusColor 	The color when the component has no focus
			-----------------------------------------------------------------
			Columns					Like the columns of common check box
			-----------------------------------------------------------------
			DropDownLines 	The number of dropDown lines
											between	MINDROPLINES and MAXDROPLINES;
											They are autoarranged if the Columns are>1
			-----------------------------------------------------------------
			Checked[Index: Integer]
											you can traverse this array to have if an
											item is checked or not
			-----------------------------------------------------------------
			QuoteStyle			of TCHBQuoteStyle = (qsNone,qsSingle,qsDouble) is the
											type with which you can set the format of the selected
											options in text format.
			-----------------------------------------------------------------
		Special Methods
			procedure 	Clear;
			-----------------------------------------------------------------
			procedure 	SetUnCheckedAll( Sender: TObject );
			procedure 	SetCheckedAll(  Sender: TObject  );
			procedure 	SetChecked(Index: Integer; Checked: Boolean);
			-----------------------------------------------------------------
			function  	CheckedCount: integer
										Returns the number of checked items
			function  	IsChecked ( Index: integer ) : boolean;
										Returns True if the Item[index] is checked
			-----------------------------------------------------------------
			function 		GetText : string;
										As the component has no Caption this is the
										text with all the choises separated by comma
										in format depended by the value of QuoteStyle
										property (see history ver 1.3)

	-----------------------------------------------------------------

		Ver 1.1 2000/1/16

		changes prompted by Kyriakos Tasos

		- corrections
							onenter events
							onexit  events
		- additions
							onchange event
							CheckedCount integer
							The dropdownlistbox closes with the ESC character
		- changes
							the internal glyph image name
		-----------------------------------------------------------------
		Ver 1.2 2000/1/31

		changes prompted by Amilcar Dornelles da Costa Leite"

		- corrections
							SetChecked(Index: Integer; Checked: Boolean);
							is now working and by code
		-----------------------------------------------------------------
		Ver 1.3 2000/4/8

		changes prompted by Jayan Chandrasekhar, Riyadh, Saudi Arabia"

		-additions
					property QuoteStyle :qsNone,qsSingle,qsDouble;

					so if the Selected values are
							 Germany,UK,USA,Russia
					the GetText function returns

						case qsNone 		-> Germany,UK,USA,Russia
						case qsSingle		-> 'Germany','UK','USA','Russia'
						case qsDouble		-> "Germany","UK","USA","Russia"

					so you can use it, as Jayan noted. in SQL in
						the SELECT .. IN clause
						e.g.
								SELECT NationID, Nation
								FROM Country
								WHERE Nation In ( "Germany", "UK", "USA", "Russia" )
		-----------------------------------------
		Ver 1.4 2000/12/26
			corrected bug prompted by "Daniel Azkona Coya"
}

interface

uses
	Windows, Messages, SysUtils,VCL.Buttons, Classes, VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs,
	VCL.StdCtrls, VCL.Menus,VCL.CheckLst,VCL.extctrls;

type

	// added 2000/04/08
	TCHBQuoteStyle   = (qsNone,qsSingle,qsDouble);

	TCheckedComboBox = class(TCustomPanel)
	private
		FCapSelAll,
		FCapDeselAll		: string;
		FEdit						: TEdit;
		FButton					: TSpeedButton;
		FItems     			: TStrings;
		FPrivForm 			: TForm;
		FListBox 				: TCheckListBox;
		FPopup					: TPopupMenu;
		FSelectAll		 	: TMenuItem;
		FDeSelectAll	 	: TMenuItem;
		FNotFocusColor	: TColor;
		FSorted					: Boolean;
		FQuoteStyle  		: TCHBQuoteStyle; // added 2000/04/08
		FCheckedCount		: integer;
		FColumns				: integer;
		FDropDownLines	: integer;
		FOnChange				: TNotifyEvent;
		FOnEnter				: TNotifyEvent;
		FOnExit					: TNotifyEvent;
		procedure SetItems(AItems : TStrings);
		procedure ToggleOnOff( Sender : TObject );
		procedure KeyListBox(Sender: TObject; var Key: Word;Shift: TShiftState);
		procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
		procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
		procedure WMSize(var Message: TWMSize); message WM_SIZE;
		procedure ShowCheckList(Sender: TObject);
		procedure CloseCheckList( Sender : TObject );
		procedure ItemsChange(Sender : TObject);
		procedure SetSorted(Value: Boolean);
		procedure AutoSize;
		procedure AdjustHeight;
		procedure EditOnEnter(Sender: TObject);
		procedure EditOnExit(Sender: TObject);
		procedure SetNotFocusColor(value:TColor);
		procedure SetColumns(value : integer);
		procedure SetChecked(Index: Integer; Checked: Boolean);
		procedure SetDropDownLines(value : integer);
		function  GetChecked(Index: Integer): Boolean;
		procedure Change;
	protected
		procedure CreateWnd; override;
	public
		constructor Create ( AOwner : TComponent ); override;
		destructor 	Destroy; override;
		procedure 	Clear;
		procedure 	SetUnCheckedAll( Sender: TObject );
		procedure 	SetCheckedAll(  Sender: TObject  );
    procedure   CheckItem(AItem : string);
    procedure   CheckItems(AItemsCommaText : string);
		function  	IsChecked ( Index: integer ) : boolean;
		function 		GetText : string;
		function 		CheckedItemsCommaText : string;
		property 		Checked[Index: Integer]: Boolean read GetChecked	write SetChecked;
		property		CheckedCount: integer read FCheckedCount;
	published
		property  Items: TStrings
							read 	FItems
							write SetItems;
		property  CapSelectAll: String
							read 	FCapSelAll
							write FCapSelAll;
		property  CapDeSelectAll: String
							read 	FCapDeselAll
							write FCapDeselAll;
		property  NotFocusColor: TColor
							read	FNotFocusColor
							write SetNotFocusColor;
		property  Sorted 	: Boolean
							read 	FSorted
							write SetSorted default False;
		property  QuoteStyle	: TCHBQuoteStyle  // added 2000/04/08
							read 	FQuoteStyle
							write FQuoteStyle default qsNone;
		property  Columns	: integer
							read 	FColumns
							write SetColumns default 0;
		property  DropDownLines: integer
							read 	FDropDownLines
							write SetDropDownLines default 6;
		// events
		property 	OnChange: TNotifyEvent
							read 	FOnChange
							write FOnChange;
		property  OnEnter : TNotifyEvent
							read 	FOnEnter
							write FOnEnter;
		property  OnExit: TNotifyEvent
							read 	FOnExit
							write FOnExit;
		// from panel
		property  Ctl3D;
		property  Cursor;
		property  Enabled;
		property  Font;
		property  ParentColor;
		property  ParentCtl3D;
		property  ParentFont;
		property  ParentShowHint;
		property  ShowHint;
		property  TabOrder;
		property  TabStop;
		property  Visible;
		property  OnClick;
		property  OnMouseDown;
		property  OnMouseMove;
		property  OnMouseUp;
		property  OnResize;
		property  OnStartDrag;
	end;

procedure Register;

implementation

uses
  System.Types;

// {$R *.R32}
const
	Delimit 			=	',';
	MAXSELLENGTH	= 256;
	MINDROPLINES	= 6;
	MAXDROPLINES	= 10;
resourcestring
	sFCapSelAll		= '&Select All';
	sFCapDeselAll	= '&DeSelect All';
	sNoMoreLength = 'There is no room for Selected';

{TCheckedComboBox}
constructor TCheckedComboBox.Create ( AOwner : TComponent );
begin
	inherited Create( AOwner );
	FDropDownLines		:= MINDROPLINES;
	FColumns					:= 0;
	FQuoteStyle 			:= qsNone;	// added 2000/04/08
	FCheckedCount			:= 0;
	FNotFocusColor		:= clWhite;
	Caption 					:= '';
	FCapSelAll				:= sFCapSelAll;
	FCapDeselAll			:= sFCapDeselAll;
	BevelOuter				:= bvLowered;
	Height						:= 24;
	Width 						:= 121;

	FItems      			:= TStringList.Create;
	TStringList(FItems).OnChange := ItemsChange;

	FEdit							:= TEdit.Create(Self);
	FEdit.Parent			:= Self;
	FEdit.ParentColor := false;
	FEdit.color				:= clWhite;
	FEdit.ReadOnly		:= True;

	FButton 					:= TSpeedButton.Create(Self);
	with FButton do
	begin
		Glyph.LoadFromResourceName(HInstance, 'BTNCHECKPOPUP');
		Parent  			:= Self;
		FButton.Width	:= 16;
		FButton.Height:= Self.Height-2;
		FButton.Top		:= Self.Top+2;
		NumGlyphs 		:= 1;
		Parent 				:= Self;
		Layout				:= blGlyphRight;
		OnClick 			:= ShowCheckList;
	end;

	Fedit.Text			:= '';
	FEdit.OnEnter		:= EditOnEnter;
	FEdit.OnExit		:= EditOnExit;

	// Create a form with its contents
	FPrivForm 				:= TForm.Create( self );
	FPrivForm.Color 	:= clWindow;

	// Create CheckListBox
	FListBox 							:= TCheckListBox.Create( FPrivForm );
	FListBox.Parent 			:= FPrivForm;
	FListBox.Ctl3D				:= False;
	FListBox.Columns			:= FColumns;
	FListBox.Align 				:= alClient;
	FListBox.OnClickCheck := ToggleOnOff;
	FListBox.OnKeyDown		:= KeyListBox;
	// Create PopUp
	FPopUp 								:= TPopupMenu.Create( FListBox );
	FSelectAll 						:= TMenuItem.Create( FPopUp );
	FSelectAll.Caption 		:= FCapSelAll;
	FDeSelectAll 					:= TMenuItem.Create( FPopUp );
	FDeSelectAll.Caption 	:= FCapDeselAll;
	FPopUp.Items.Insert( 0, FSelectAll );
	FPopUp.Items.Insert( 1, FDeSelectAll );
	FSelectAll.OnClick 		:= SetCheckedAll;
	FDeSelectAll.OnClick 	:= SetUnCheckedAll;
	FListBox.PopupMenu 		:= FPopUp;
end;

destructor TCheckedComboBox.Destroy;
begin
	FEdit.free;
	FSelectAll.Free;
	FDeSelectAll.Free;
	FPopup.Free;
	FButton.Free;
	FListBox.Free;
	FItems.Free;
	FPrivForm.Free;
	inherited Destroy;
end;
//====================== Show - Close List Box

procedure TCheckedComboBox.ShowCheckList( Sender : TObject );
var	ScreenPoint : TPoint;
begin
	if FButton.tag=1 then  // Jan Verhoeven
	begin
		FButton.tag:=0;
		exit
	end;
	Click;
	if Fcolumns > 1 then
		FDropDownLines := FlistBox.Items.Count div Fcolumns+1;
	if FDropDownLines < MINDROPLINES then
		FDropDownLines := MINDROPLINES;
	if FDropDownLines > MAXDROPLINES then
		FDropDownLines := MAXDROPLINES;

	// Assign Form coordinate and show
	ScreenPoint    				:= Parent.ClientToScreen( Point( self.Left, self.Top+self.Height ) );
	FSelectAll.Caption 		:= FCapSelAll;
	FDeSelectAll.Caption 	:= FCapDeselAll;
	with FPrivForm do
	begin
		Font 					:= self.Font;
		Left  				:= ScreenPoint.X;
		Top   				:= ScreenPoint.Y;
		Width 				:= self.Width;
		Height 				:= ( FDropDownLines * FlistBox.ItemHeight+4{ FEdit.Height });
		BorderStyle 	:= bsNone;
		OnDeactivate	:= CloseCheckList;
	end;
  if FPrivForm.Height + ScreenPoint.y > Screen.Height-20 then
        FPrivForm.Top := ScreenPoint.y-FprivForm.Height-self.height ;
	FPrivForm.Show;
end;

procedure TCheckedComboBox.CloseCheckList( Sender : TObject );
var pt:TPoint;
begin
// code added by Jan Verhoeven
// check if the mouse is over the combobox button
//	pt:=mouse.CursorPos;
//  this doesn't work on delphi 3
	GetCursorPos(pt);
	pt:=FButton.ScreenToClient(pt);
	with Fbutton do
	begin
		if (pt.x>0) and (pt.x<width) and (pt.y>0) and (pt.y<height)
			then tag:=1
			else tag:=0
	end;
	FPrivForm.Close;
end;

//===========================================
// exanines if string (part) exist in string (source)
// 		where source is in format part1[,part2]
function PartExist(const part, source:string):Boolean;
var m :integer;
	 temp1,temp2 :string;
begin
	temp1  := Copy(Source,1,MAXSELLENGTH);
	Result := part=temp1;
	while not result do
	begin
		m := Pos(Delimit,temp1);
		if m>0 then
			temp2 := Copy(temp1,1,m-1)
		else
			temp2 := temp1;
		Result := part=temp2;
		if (Result) or (m=0) then break;
		temp1 := Copy(temp1,m+1,MAXSELLENGTH);
	end;
end;

{
	removes a string (part) from another string (source)
	when source is in format part1[,part2]
}
function RemovePart(const part, source :string):string;
var
	lp,p :integer;
	s1,s2:string;
begin
	result := source;
	s1 := Delimit+part+Delimit;
	s2 := Delimit+source+Delimit;
	 p := Pos(s1,s2);
	if p>0 then
	begin
		lp := Length(part);
		if p=1 then
			result := Copy(source,p+lp+1,MAXSELLENGTH)
		else
		begin
			result := Copy(s2,2,p-1)+Copy(s2,p+lp+2,MAXSELLENGTH);
			result := Copy(result,1,length(result)-1);
		end;
	end;
end;

function Add(const sub:string;var str:string):boolean;
begin
	result := false;
	if length(str)+length(sub)+1>= MAXSELLENGTH then
	begin
		ShowMessage(sNoMoreLength);
		exit;
	end;
	if str = '' then
	begin
		str := sub;
		result := true;
	end
	else
		if not PartExist(sub,str) then
		begin
			str := str+Delimit+ sub;
			result := true;
		end;
end;

function Remove(const sub:string; var str:string):boolean;
	var temp:string;
begin
	result := false;
	if str <> '' then
	begin
		temp 	:= RemovePart(sub,str);
		if temp<>str then
		begin
			str		:= temp;
			result := true;
		end;
	end;
end;

procedure TCheckedComboBox.ToggleOnOff( Sender : TObject );
var s: string;
begin
	s:= fEdit.text;
	if FListBox.Checked[FListBox.itemindex] then
	begin
		if Add(FListBox.Items[FListBox.itemindex],s) then
			FCheckedCount := FCheckedCount + 1
	end
	else
		if Remove(FListBox.Items[FListBox.itemindex],s) then
			FCheckedCount := FCheckedCount - 1;
	fEdit.text:= s;
	Change
end;

procedure TCheckedComboBox.KeyListBox(Sender: TObject; var Key: Word;Shift: TShiftState);
begin
	if key = VK_ESCAPE then
		FPrivForm.Close
	else
		inherited
end;

// added 2000/04/08
function GetFormatedText(kind:TCHBQuoteStyle;str:string):string;
var s : string;
begin
	result := str;
	if length(str)>0 then
	begin
		s := str;
		case kind of
			qsSingle	: result :=
					''''+
					StringReplace(S, ',', ''',''',[rfReplaceAll])+
					'''';
			qsDouble 	: result :=
					'"'+
					StringReplace(S, ',', '","',[rfReplaceAll])+
					'"';
		end;
	end;
end;

function TCheckedComboBox.GetText : string;
begin
	if FQuoteStyle = qsNone then
		result := FEdit.Text
	else
		result := GetFormatedText(FQuoteStyle,FEdit.Text);
end;

//========================== CheckListBox
procedure TCheckedComboBox.SetDropDownLines(value : integer);
begin
	if FDropDownLines<>value then
		if (value>=MINDROPLINES) and (value<=MAXDROPLINES) then
			FDropDownLines := value;
end;

procedure TCheckedComboBox.SetColumns(value : integer);
begin
	if Fcolumns<>value then
	begin
		Fcolumns := value;
		FListBOx.Columns := Fcolumns;
	end;
end;

procedure TCheckedComboBox.SetCheckedAll( Sender : TObject );
var i:integer;
		s: string;
begin
	s:= '';
	for i:=0 to FListBox.Items.Count -1 do
	begin
		if not FListBox.Checked[i] then
		begin
			FListBox.Checked[i]:= true;
		end;
		if i=0 then
			s := FListBox.Items[i]
		else
			s := s+','+FListBox.Items[i];
	end;
	FEdit.text:= s;
	FCheckedCount := FListBox.Items.Count;
	FEdit.Repaint;
	change;
end;

procedure TCheckedComboBox.SetUnCheckedAll( Sender: TObject );
var
	i : integer;
begin
	FCheckedCount := 0;
	with FListBox do
		begin
			for i := 0 to Items.Count-1 do
				if Checked[i] then
					Checked[i] := false;
		end;
	 FEdit.Text:= '';
	change;
end;

function TCheckedComboBox.IsChecked( Index: integer ) : boolean;
begin
  if(Index < 0) or (Index >= FListBox.Items.Count) then
    Result := False
  else
	result := FListBox.Checked[ Index ];
end;

procedure TCheckedComboBox.SetChecked(Index: Integer; Checked: Boolean);
var
	s:string;
	ok:boolean;
begin
	if index<FlistBox.Items.Count then
	begin
		s:= fEdit.text;
		ok:= false;
		if not FListBox.Checked[Index] and checked then
		begin
			if Add(FListBox.Items[Index],s) then
			begin
				FCheckedCount := FCheckedCount + 1;
				ok := true;
			end;
		end
		else
		if FListBox.Checked[Index] and not checked then
			if Remove(FListBox.Items[Index],s) then
			begin
				FCheckedCount := FCheckedCount - 1;
				ok := true;
			end;
		if ok then
		begin
			FListBox.Checked[Index]:= Checked;
			fEdit.text:= s;
			Change
		end;
	end;
end;

Function TCheckedComboBox.GetChecked(Index: Integer): Boolean;
begin
	if index<FlistBox.Items.Count then
		Result := FListBox.Checked[Index]
	else
		Result := false;
end;

//===========		For CheckListBox Items
procedure TCheckedComboBox.SetItems(AItems : TStrings);
begin
	FItems.Assign(AItems);
end;

procedure TCheckedComboBox.ItemsChange(Sender : TObject);
begin
	FlistBox.Clear;
	FEdit.Text := '';
	FlistBox.Items.Assign(FItems);
end;

//===========		 Auto Sizing routines
procedure TCheckedComboBox.CMEnabledChanged(var Message: TMessage);
begin
	inherited;
	FButton.Enabled := Enabled;
	FEdit.Enabled   := Enabled;
end;

procedure TCheckedComboBox.AutoSize;
begin
	AdjustHeight;
	FButton.Height	:= Height-2;
	FEdit.Height		:= Height;
	FEdit.width			:= Width - FButton.Width-3;
	FButton.Left		:= FEdit.width+1;
end;

procedure TCheckedComboBox.AdjustHeight;
var
	DC: HDC;
	SaveFont: HFont;
	I: Integer;
	SysMetrics, Metrics: TTextMetric;
begin
	DC := GetDC(0);
	GetTextMetrics(DC, SysMetrics);
	SaveFont := SelectObject(DC, Font.Handle);
	GetTextMetrics(DC, Metrics);
	SelectObject(DC, SaveFont);
	ReleaseDC(0, DC);
	if NewStyleControls then
	begin
		if Ctl3D then I := 8 else I := 6;
		I := GetSystemMetrics(SM_CYBORDER) * I;
	end else
	begin
		I := SysMetrics.tmHeight;
		if I > Metrics.tmHeight then I := Metrics.tmHeight;
		I := I div 4 + GetSystemMetrics(SM_CYBORDER) * 4;
	end;
	Height := Metrics.tmHeight + I;
end;

procedure TCheckedComboBox.CreateWnd;
begin
	inherited;
	AutoSize;
end;

procedure TCheckedComboBox.WMSize(var Message: TWMSize);
begin
	inherited;
	AutoSize;
end;

procedure TCheckedComboBox.CMFontChanged(var Message: TMessage);
begin
	inherited;
	AutoSize;
	invalidate;
end;
//=====================
procedure TCheckedComboBox.EditOnEnter;
begin
	FEdit.Color		:= clWhite;
	if Assigned(FOnEnter) then FOnEnter(Self);
end;

procedure TCheckedComboBox.EditOnExit;
begin
	Fedit.Color		 := FNotFocusColor;
	if Assigned(FOnExit) then FOnExit(Self);
end;

procedure TCheckedComboBox.SetNotFocusColor(value:TColor);
begin
	if FNotFocusColor <> Value then
	begin
		FNotFocusColor := Value;
		Fedit.Color		 := value;
	end;
end;

procedure TCheckedComboBox.Change;
begin
	if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TCheckedComboBox.Clear;
begin
	FEdit.Text := '';
	FItems.Clear;
	FListBox.Clear;
end;

procedure TCheckedComboBox.SetSorted(Value: Boolean);
begin
	if FSorted <> Value then
	begin
		FSorted := Value;
		TStringList(FItems).Sorted := FSorted;
	end;
end;

procedure TCheckedComboBox.CheckItem(AItem: string);
var
  LIndex : integer;
begin
  LIndex := Items.IndexOf(AItem);
  if(LIndex >= 0) then
    Checked[LIndex] := True;
end;

procedure TCheckedComboBox.CheckItems(AItemsCommaText: string);
var
  LIndex : integer;
  LItems : TStringList;
begin
  LItems := TStringList.Create;
  try
    LItems.CommaText := AItemsCommaText;
    for LIndex := 0 to LItems.Count-1 do
      CheckItem(LItems[LIndex]);
  finally
    LItems.Free;
  end;
end;

function TCheckedComboBox.CheckedItemsCommaText: string;
var
  LIndex : integer;
  LItems : TStringList;
begin
  Result := '';
  LItems := TStringList.Create;
  try
    for LIndex := 0 to FItems.Count-1 do
    begin
      if IsChecked(LIndex) then
        LItems.Add(FItems[LIndex]);
    end;
    Result := LItems.CommaText;
  finally
    LItems.Free;
  end;
end;

procedure Register;
begin
	RegisterComponents('WRMF', [TCheckedComboBox]);
end;

end.
