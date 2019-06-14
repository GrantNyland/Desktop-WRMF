 unit UMarkStringGrid;

interface

uses
  Windows, Messages, SysUtils, Classes, VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs,
  VCL.Grids;

type
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  JoinString = String[4];
  TextPosString = String[2];
  OptionRec = Record
    //Cell column and row numbers
    TheCol,TheRow : Integer;
    //Cell background colour
    TheColour : LongInt;
    //Font colour
    FontColour : LongInt;
    //Allow cell editing or not
    Locked : Boolean;

    TheJoin : JoinString;
    //blank string - nothing
    //'1   ' - Joined to cell to the left
    //' 1  ' - Joined to cell below
    //'  1 ' - Joined to cell to the right
    //'   1' - Joined to cell above
    TextPos : TextPosString;
    //first char
      //l - left
      //c - centre
      //r - right
    //second char
      //t - top
      //c - centre
      //b - bottom
//    TheFont : TFont;
  End;//record

  TMarkStringGrid = class(TStringGrid)
  protected
    OptionArray : Array of OptionRec;
    CurrentCell : Integer;
    FJoinResult : JoinString;
    HardCopy    : Boolean; // flag to check if we are printing or not
    PrintLeft,PrintTop : Integer;
    PrintScaling : Double;
    procedure DeleteCellValues(Index:Integer);
    function OriginalValues(TheCell:OptionRec):Boolean;
    function GetCellValues(ACol,ARow:Integer):OptionRec;
    procedure SetCellValues(TheCell:OptionRec);
    //private cell joining
    procedure Join2Cells(FirstCol,FirstRow,SecondCol,SecondRow:Integer);
    procedure Clear2Joins(FirstCol,FirstRow,SecondCol,SecondRow:Integer);
    function DoScale(TheVal:Double):Integer;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    procedure SetEditText(ACol, ARow: Longint; const Value: String); override;

  public
    { Public declarations }
    Constructor Create (AOwner:TComponent); override;
    //Cell colouring
    function GetColour(ACol,ARow:Integer):LongInt;
    procedure SetColour(ACol,ARow:Integer;TheColour:LongInt);
    Procedure ResetCellColor(ACol,ARow:Integer);
    Procedure ResetGridColor;
    //Font colouring
    Function GetFontColour(ACol,ARow:Integer):LongInt;
    Procedure SetFontColour(ACol,ARow:Integer;TheColour:LongInt);
    Procedure ResetFontColor(ACol,ARow:Integer);
    Procedure ResetGridFontColor;
    //cell joining
    Function IsJoined(TheCol,TheRow:Integer):Boolean;
    Procedure SetJoin(FirstCol,FirstRow,SecondCol,SecondRow:Integer);
    Procedure ClearJoin(FirstCol,FirstRow,SecondCol,SecondRow:Integer);

    //Text positioning
    Procedure SetTextPos(ACol,ARow : Integer;HorPos,VertPos:AnsiChar);
    Procedure GetTextPos(ACol,ARow : Integer;Var HorPos,VertPos:AnsiChar);
    Procedure ClearTextPos(ACol,ARow : Integer);

    //Column processing
    Procedure SetColColours(ACol:Integer;BackColour,FontColour:LongInt);
    Procedure SetColLock(ACol:Integer);
    Procedure ClearColLock(ACol:Integer);

    //Row processing
    Procedure SetRowColours(ARow:Integer;BackColour,FontColour:LongInt);
    Procedure SetRowLock(ARow:Integer);
    Procedure ClearRowLock(ARow:Integer);

    //Cell locking
    Procedure SetCellLock(ACol, ARow: Longint);
    Procedure ClearCellLock(ACol, ARow: Longint);
    Function GetCellLock(ACol, ARow: LongInt):Boolean;
    Property JoinResult : JoinString Read FJoinResult;
    //Font
{    Procedure SetFont(ACol,ARow : Integer;TheFont:TFont);
    Function GetFont(ACol,ARow:Integer):TFont;
    Procedure ClearFont(ACol,ARow : Integer);}

    Procedure DoPrint(StartX,StartY:Integer;ScalePrint:Double);

  published
    { Published declarations }
  end;


procedure Register;

implementation
Uses
  VCL.Printers;

Constructor TMarkStringGrid.Create (AOwner:TComponent);
Begin
  Inherited;

  Self.DefaultDrawing:=False; //have to have this or else the component
                              //overwrites a lot of my stuff from drawcell
  HardCopy := False; //don't send to the printer by default
End;//create

procedure TMarkStringGrid.DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
Var
  TmpCell,JoinedCell : OptionRec;
  TmpRect : TRect;
  StopJoin : Boolean;
  Tmp,tx,ty,StrIndex,tWidth,tHeight : Integer;
  tText : String;
  MyText : TStringList;
//  TmpFont : TFont;
Begin
  //Draw the cell according to the values specified in my array
  //Get the formatting
  TmpCell:=GetCellValues(ACol,ARow);

  //Get the cell dimensions
  TmpRect:=ARect;
  //Check if we should change the dimensions if this is a joined cell
  If IsJoined(ACol,ARow) Then Begin
    If TmpCell.TheJoin[1]='1' Then Begin
      //Have to call the other cell again or else this one won't be repainted
      //when it is scrolled open on its own
         { DONE -ome -cDrawing : I am not catering for multiple joined rows }
      ARect.Right:=ARect.Left-GridLineWidth;
      ARect.Left:=ARect.Right-ColWidths[ACol-1];
      DrawCell(ACol-1,ARow,ARect,AState);
      Exit; //Left:  skip - handle in other cell
    End;
    If TmpCell.TheJoin[4]='1' Then Begin
      //Have to call the other cell again or else this one won't be repainted
      //when it is scrolled open on its own
      ARect.Bottom:=ARect.Top-GridLineWidth;
      ARect.Top:=ARect.Bottom-RowHeights[ARow-1];
      DrawCell(ACol,ARow-1,ARect,AState);
      Exit; //Above: skip - handle in other cell
    End;
    If TmpCell.TheJoin[3]='1' Then Begin
      //Joined to the right
      StopJoin:=False;
      Tmp:=ACol+1;
      Repeat
        JoinedCell:=GetCellValues(Tmp,ARow);
        If JoinedCell.TheJoin[1]='1' Then
          TmpRect.Right:=TmpRect.Right+ColWidths[Tmp]+GridLineWidth
        Else
          StopJoin:=True;
        Inc(Tmp);
      until(StopJoin);
    End;
    If TmpCell.TheJoin[2]='1' Then Begin
      //Joined below
      StopJoin:=False;
      Tmp:=ARow+1;
      Repeat
        JoinedCell:=GetCellValues(ACol,Tmp);
        If JoinedCell.TheJoin[4]='1' Then
          TmpRect.Bottom:=TmpRect.Bottom+RowHeights[Tmp]+GridLineWidth
        Else
          StopJoin:=True;
        Inc(Tmp);
      until(StopJoin);
    End;
  End;//if joined

  //draw the inherited values only if cell is not joined from above or the left
  Inherited;

  //Set the colour of the cell
  If TmpCell.TheColour<>-1 Then
   Canvas.Brush.Color:=TmpCell.TheColour
  Else Begin
    //fixed cells
    If AState=[gdFixed] Then Canvas.Brush.Color:=FixedColor;
    //normal cells
    If AState=[] Then Canvas.Brush.Color:=Color;
  End;
  //focused cell

  //sl 2000.08.07 - Only highlight the cell if the Option is selected
  If (ACol=Col) and (ARow=Row) then
  begin
    if (goDrawFocusSelected in Options) Then
      Canvas.Brush.Color:=clActiveCaption
    else
      Canvas.Brush.Color:=Color;
  end;

  //Draw the background colour
  Canvas.FillRect(TmpRect);

  //If we are printing the produce the rectangle
  If HardCopy Then
    Printer.Canvas.Rectangle(
      DoScale(TmpRect.Left)+PrintLeft,
      DoScale(TmpRect.Top)+PrintTop,
      DoScale(TmpRect.Left+TmpRect.Right)+PrintLeft,
      DoScale(TmpRect.Top+TmpRect.Bottom)+PrintTop);

  //Cut up the text so that it will fit into the width of the column
  //first try and do this on word divisions, but if this does not work, then
  //simply chop off chars until it does
  tText:=Cells[ACol,ARow];
  MyText:=TStringList.Create;
  StrIndex:=0;
  MyText.Append(''); //add a blank string
  While tText<>'' Do Begin
    While (Canvas.TextWidth(MyText.Strings[StrIndex])<(TmpRect.Right-TmpRect.Left)-4) and (tText<>'') Do Begin
      MyText.Strings[strIndex]:=MyText.Strings[strIndex]+tText[1];
      tText:=Copy(tText,2,Length(tText));
    End;//while - adding text
    if (Canvas.TextWidth(MyText.Strings[StrIndex])>=(TmpRect.Right-TmpRect.Left)-4) Then Begin
      //Now check if we could have chopped this off sooner at the start of a word.
      If (Pos(' ',MyText.Strings[StrIndex])>0) Or (Pos(',',MyText.Strings[StrIndex])>0) Then Begin
        MyText.Append('');
        While (Copy(MyText.Strings[StrIndex],Length(MyText.Strings[StrIndex]),1)<>' ') And
          (Copy(MyText.Strings[StrIndex],Length(MyText.Strings[StrIndex]),1)<>',') Do Begin
          MyText.Strings[StrIndex+1]:=Copy(MyText.Strings[StrIndex],Length(MyText.Strings[StrIndex]),1)+MyText.Strings[StrIndex+1];
          MyText.Strings[StrIndex]:=Copy(MyText.Strings[StrIndex],1,Length(MyText.Strings[StrIndex])-1);
        End;//while
      End
      Else
        MyText.Append('');
      Inc(StrIndex);
    End;
  End;//while tText

  //Calculate the width and height of the text block
  tHeight:=Trunc(MyText.Count*Canvas.TextHeight(Cells[ACol,ARow])*1.1);
  tWidth:=0;
  StrIndex:=0;
  While StrIndex<MyText.Count Do Begin
    if Canvas.TextWidth(MyText.Strings[StrIndex])>tWidth Then
      tWidth:=Canvas.TextWidth(MyText.Strings[StrIndex]);
    Inc(StrIndex);
  End;

  //focused cell - change text colour
  //sl 2001.02.23 only highlight if we have godrawfocusselected in the option list
  If (ACol=Col) and (ARow=Row) and (goDrawFocusSelected in Options) Then
    Canvas.Font.Color:=clHighlightText
  Else Begin
    //Set the font colour
    If TmpCell.FontColour<>-1 Then
      Canvas.Font.Color:=TmpCell.FontColour
    Else
      Canvas.Font.Color:=clBlack;
  End;

  //Display the cell contents
  tx:=0;
  ty:=0;
  StrIndex:=0;
  While StrIndex<MyText.Count Do Begin
    //Position the text
    //horizontal
    If TmpCell.TextPos[1]='L' then tx:=TmpRect.Left + 3;
    If TmpCell.TextPos[1]='C' then tx:=TmpRect.Left+((TmpRect.Right-TmpRect.Left) Div 2) - Canvas.TextWidth(MyText.Strings[StrIndex]) Div 2;
    If TmpCell.TextPos[1]='R' then tx:=TmpRect.Right - 3 - Canvas.TextWidth(MyText.Strings[StrIndex]);
    //vertical
    If TmpCell.TextPos[2]='T' then ty:=TmpRect.Top + 3 + Trunc(Canvas.TextHeight(MyText.Strings[StrIndex])*1.1*(StrIndex));
    If TmpCell.TextPos[2]='C' then ty:=TmpRect.Top + (TmpRect.Bottom-TmpRect.Top) Div 2 - tHeight div 2 + Trunc(Canvas.TextHeight(MyText.Strings[StrIndex])*1.1*(StrIndex));
    If TmpCell.TextPos[2]='B' then ty:=TmpRect.Bottom - 3 - Trunc(Canvas.TextHeight(MyText.Strings[StrIndex])*1.1*(MyText.Count-StrIndex));

    //Display the text
    if (tx>=TmpRect.Left) And (tx<=TmpRect.Right) Then Begin
      if (ty>=TmpRect.Top) And (ty<=TmpRect.Bottom) Then Begin
        Canvas.TextOut(tx,ty,MyText.Strings[StrIndex]);
        If HardCopy Then
          Printer.Canvas.TextOut(DoScale(tx)+PrintLeft,DoScale(ty)+PrintTop,MyText.Strings[StrIndex]);
      End;
    End;
    Inc(StrIndex);
  end;

  //Reset the font again
//  Canvas.Font:=TmpFont;

  //further formatting
  //Draw the fixed cells in 3d
  If AState=[gdFixed] Then Begin
    //put in the 3d effect
    Canvas.Pen.Color:=cl3DLight;
    Canvas.MoveTo(TmpRect.Left+1,TmpRect.Top+1);
    Canvas.LineTo(TmpRect.Right-1,TmpRect.Top+1);
    Canvas.MoveTo(TmpRect.Left+1,TmpRect.Top+1);
    Canvas.LineTo(TmpRect.Left+1,TmpRect.Bottom-1);
    Canvas.Pen.Color:=cl3DDkShadow;
    Canvas.MoveTo(TmpRect.Right,TmpRect.Bottom);
    Canvas.LineTo(TmpRect.Right,TmpRect.Top+1);
    Canvas.MoveTo(TmpRect.Right,TmpRect.Bottom);
    Canvas.LineTo(TmpRect.Left+1,TmpRect.Bottom);
  End;

End;

Function TMarkStringGrid.OriginalValues(TheCell:OptionRec):Boolean;
Begin
  //checks to see if all the values have been set to their default values
  OriginalValues:=False; //default assumption that value have changed

  If TheCell.TextPos<>'LT' Then Exit;
  If TheCell.TheColour<>-1 Then Exit;
  If TheCell.FontColour<>-1 Then Exit;
  If TheCell.TheJoin<>'    ' Then Exit;
  If TheCell.Locked Then Exit;

  OriginalValues:=True; //if we get here then all the values are original
End;//Original values

Function TMarkStringGrid.GetCellValues(ACol,ARow:Integer):OptionRec;
Begin
  //Insert default not found values
  GetCellValues.TheColour:=-1;
  GetCellValues.FontColour:=-1;
  GetCellValues.TheJoin:='    ';
  GetCellValues.TheCol:=ACol;
  GetCellValues.TheRow:=ARow;
  GetCellValues.TextPos:='LT';
  GetCellValues.Locked:=False;
//  GetCellValues.TheFont:=Nil;

  If OptionArray=Nil Then Exit;
  //quick check to see if we are already on the correct cell
  If (OptionArray[CurrentCell].TheCol=ACol) And (OptionArray[CurrentCell].TheRow=ARow) Then Begin
    GetCellValues:=OptionArray[CurrentCell];
    Exit;
  End;

  //No, check through all the cells
  CurrentCell:=0;
  Repeat
    If (OptionArray[CurrentCell].TheCol=ACol) And (OptionArray[CurrentCell].TheRow=ARow) Then Begin
      GetCellValues:=OptionArray[CurrentCell];
      Exit;
    End;
    Inc(CurrentCell);
  Until(CurrentCell>Length(OptionArray)-1);
End;

Procedure TMarkStringGrid.SetCellValues(TheCell:OptionRec);
Var
  Count : Integer;
Begin
  //Set the colour
  //First check to make sure that this cell has not been specified before
  If OptionArray=Nil Then Begin
    //This is the first element - create it now
    SetLength(OptionArray,1);
    OptionArray[0]:=TheCell;
    If OriginalValues(OptionArray[0]) Then DeleteCellValues(0);
    Exit;
  End
  Else Begin
    Count:=0;
    Repeat
      If (OptionArray[Count].TheCol=TheCell.TheCol) And (OptionArray[Count].TheRow=TheCell.TheRow) Then Begin
        //Found the element - update the values
        OptionArray[Count]:=TheCell;
        //Exit immediately
        Exit;
      End;
      Inc(Count);
    Until(Count>Length(OptionArray)-1);
  End;//if

  //If we got to here then we could not find the element and we
  //must therefore create a new one
  Count:=Length(OptionArray);
  SetLength(OptionArray,Count+1);
  //Set the values
  OptionArray[Count]:=TheCell;
//  If OriginalValues(OptionArray[Count]) Then DeleteCellValues(Count);
End;//SetCellValues

Procedure TMarkStringGrid.DeleteCellValues(Index:Integer);
Begin
  While (Index<Length(OptionArray)-1) Do Begin
    OptionArray[Index]:=OptionArray[Index+1];
    Inc(Index);
  End;
  OptionArray:=Copy(OptionArray,0,Length(Optionarray)-1);
End;//deletecellvalues

//---- Colour operations -------------------------------------------------------
Function TMarkStringGrid.GetColour(ACol,ARow:Integer):LongInt;
Var
  TmpCell : OptionRec;
Begin
  //simple wrapper for the getcellvalues
  TmpCell:=GetCellValues(ACol,ARow);
  GetColour:=TmpCell.TheColour;
End;

Procedure TMarkStringGrid.SetColour(ACol,ARow:Integer;TheColour:LongInt);
Var
  TmpCell : OptionRec;
Begin
  //Simple wrapper for setcellvalues
  TmpCell:=GetCellValues(ACol,ARow);
  TmpCell.TheCol:=ACol;
  TmpCell.TheRow:=ARow;
  TmpCell.TheColour:=TheColour; //set the new colour
  //Set the colour
  SetCellValues(TmpCell);
End;

Procedure TMarkStringGrid.ResetCellColor(ACol,ARow:Integer);
Var
  TmpCell : OptionRec;
Begin
  //Simple wrapper for setcellvalues
  TmpCell:=GetCellValues(ACol,ARow);
  TmpCell.TheCol:=ACol;
  TmpCell.TheRow:=ARow;
  TmpCell.TheColour:=-1; //reset the colour
  //Set the colour
  SetCellValues(TmpCell);
End;

Procedure TMarkStringGrid.ResetGridColor;
Var
  Count:Integer;
Begin
  //Reset the colour of all the cells
  Count:=0;
  If OptionArray<>Nil Then Begin
    While (Count<Length(OptionArray)) Do Begin
      ResetCellColor(OptionArray[Count].TheCol,OptionArray[Count].TheRow);
      Inc(Count);
    End;
    Refresh;
  End;
End;

//Text colour operations -------------------------------------------------------
Function TMarkStringGrid.GetFontColour(ACol,ARow:Integer):LongInt;
Var
  TmpCell : OptionRec;
Begin
  //simple wrapper for the getcellvalues
  TmpCell:=GetCellValues(ACol,ARow);
  GetFontColour:=TmpCell.FontColour;
End;

Procedure TMarkStringGrid.SetFontColour(ACol,ARow:Integer;TheColour:LongInt);
Var
  TmpCell : OptionRec;
Begin
  //Simple wrapper for setcellvalues
  TmpCell:=GetCellValues(ACol,ARow);
  TmpCell.TheCol:=ACol;
  TmpCell.TheRow:=ARow;
  TmpCell.FontColour:=TheColour; //set the new colour
  //Set the colour
  SetCellValues(TmpCell);
End;

Procedure TMarkStringGrid.ResetFontColor(ACol,ARow:Integer);
Var
  TmpCell : OptionRec;
Begin
  //Simple wrapper for setcellvalues
  TmpCell:=GetCellValues(ACol,ARow);
  TmpCell.TheCol:=ACol;
  TmpCell.TheRow:=ARow;
  TmpCell.FontColour:=-1; //reset the colour
  //Set the colour
  SetCellValues(TmpCell);
End;

Procedure TMarkStringGrid.ResetGridFontColor;
Var
  Count:Integer;
Begin
  //Reset the colour of all the cells
  Count:=0;
  If OptionArray<>Nil Then Begin
    While (Count<Length(OptionArray)) Do Begin
      ResetFontColor(OptionArray[Count].TheCol,OptionArray[Count].TheRow);
      Inc(Count);
    End;
    Refresh;
  End;
End;

//---- Join procedures ---------------------------------------------------------

Function TMarkStringGrid.IsJoined(TheCol,TheRow:Integer):Boolean;
Var
  Tmpcell : OptionRec;
Begin
  //Returns a boolean indicating whether this cell is joined with another one
  TmpCell:=GetCellValues(TheCol,TheRow);
  If TmpCell.TheJoin='    ' Then
    IsJoined:=False
  Else
    IsJoined:=True;
  FJoinResult:=TmpCell.TheJoin;
  
End;//IsJoined

Procedure TMarkStringGrid.Join2Cells(FirstCol,FirstRow,SecondCol,SecondRow:Integer);
Var
  CanJoin : Boolean;
  FirstCell,SecondCell : OptionRec;
Begin
  //Join 2 adjoining cells
  //First make sure that the cells are adjoining
  CanJoin:=True;
  If Abs(FirstCol-SecondCol)>1 Then Begin
    If Abs(FirstRow-SecondRow)>1 Then CanJoin:=False;
  End;
  //Check that it is not a join to itself
  If (FirstCol=SecondCol) And (FirstRow=SecondRow) Then CanJoin:=False;

  If CanJoin=False Then Exit;

  //Get the values of the current cells
  FirstCell:=GetCellValues(FirstCol,FirstRow);
  SecondCell:=GetCellValues(SecondCol,SecondRow);
  //Now see how the join hangs together
  //Join values are as follows:
    //'1   ' - Joined to cell to the left
    //' 1  ' - Joined to cell below
    //'  1 ' - Joined to cell to the right
    //'   1' - Joined to cell above

  //if second cell is to right of first cell
  If (SecondCol=FirstCol+1) And (SecondRow=FirstRow) Then Begin
    FirstCell.TheJoin[3]:='1';
    SecondCell.TheJoin[1]:='1';
  End;
  //if second cell is to left of first cell
  If (SecondCol=FirstCol-1) And (SecondRow=FirstRow) Then Begin
    FirstCell.TheJoin[1]:='1';
    SecondCell.TheJoin[3]:='1';
  End;
  //if second cell is above first cell
  If (SecondCol=FirstCol) And (SecondRow=FirstRow-1) Then Begin
    FirstCell.TheJoin[4]:='1';
    SecondCell.TheJoin[2]:='1';
  End;
  //if second cell is below first cell
  If (SecondCol=FirstCol) And (SecondRow=FirstRow+1) Then Begin
    FirstCell.TheJoin[2]:='1';
    SecondCell.TheJoin[4]:='1';
  End;
  //Update the values
  SetCellValues(FirstCell);
  SetCellValues(SecondCell);
end;//SetJoin

Procedure TMarkStringGrid.Clear2Joins(FirstCol,FirstRow,SecondCol,SecondRow:Integer);
Var
  CanJoin : Boolean;
  FirstCell,SecondCell : OptionRec;
Begin
  //Clear join from 2 adjoining cells
  //First make sure that the cells are adjoining
  CanJoin:=True;
  If Abs(FirstCol-SecondCol)>1 Then Begin
    If Abs(FirstRow-SecondRow)>1 Then CanJoin:=False;
  End;
  If CanJoin=False Then Exit;

  //Get the values of the current cells
  FirstCell:=GetCellValues(FirstCol,FirstRow);
  SecondCell:=GetCellValues(SecondCol,SecondRow);
  //Now see how the join hangs together
  //Join values are as follows:
    //'1   ' - Joined to cell to the left
    //' 1  ' - Joined to cell below
    //'  1 ' - Joined to cell to the right
    //'   1' - Joined to cell above

  //if second cell is to right of first cell
  If (SecondCol=FirstCol+1) And (SecondRow=FirstRow) Then Begin
    FirstCell.TheJoin[3]:=' ';
    SecondCell.TheJoin[1]:=' ';
  End;
  //if second cell is to left of first cell
  If (SecondCol=FirstCol-1) And (SecondRow=FirstRow) Then Begin
    FirstCell.TheJoin[1]:=' ';
    SecondCell.TheJoin[3]:=' ';
  End;
  //if second cell is above first cell
  If (SecondCol=FirstCol) And (SecondRow=FirstRow-1) Then Begin
    FirstCell.TheJoin[4]:=' ';
    SecondCell.TheJoin[2]:=' ';
  End;
  //if second cell is below first cell
  If (SecondCol=FirstCol) And (SecondRow=FirstRow+1) Then Begin
    FirstCell.TheJoin[2]:=' ';
    SecondCell.TheJoin[4]:=' ';
  End;
  //Update the values
  SetCellValues(FirstCell);
  SetCellValues(SecondCell);
End;//Clear2Joins;

Procedure TMarkStringGrid.SetJoin(FirstCol,FirstRow,SecondCol,SecondRow:Integer);
Var
  TmpCol,StartRow,TmpRow:Integer;
  ColsDone,RowsDone : Boolean;
Begin
  //Wrapper for Join2Cells so that it is possible to join multiple cells in
  //one go

  TmpCol:=FirstCol;
  ColsDone:=False;
  Repeat
    TmpRow:=FirstRow;
    StartRow:=FirstRow;
    RowsDone:=False;
    Repeat
      Join2Cells(FirstCol,StartRow,TmpCol,TmpRow); //Do the Join
      If SecondRow>FirstRow Then Begin
        Inc(TmpRow);
        If TmpRow>SecondRow Then RowsDone:=True;
        If TmpRow-StartRow>1 Then Inc(StartRow);
      End;
      If SecondRow<FirstRow Then Begin
        Dec(TmpRow);
        If TmpRow<SecondRow Then RowsDone:=True;
        If StartRow-TmpRow>1 Then Dec(StartRow);
      End;
      If SecondRow=FirstRow Then RowsDone:=True;
    Until(RowsDone);

    If SecondCol>FirstCol Then Begin
      Inc(TmpCol);
      If TmpCol>SecondCol Then ColsDone:=True;
      If TmpCol-FirstCol>1 Then Inc(FirstCol);
    End;
    If SecondCol<FirstCol Then Begin
      Dec(TmpCol);
      If TmpCol<SecondCol Then ColsDone:=True;
      If FirstCol-TmpCol>1 Then Inc(FirstCol);
    End;
    If SecondCol=FirstCol Then ColsDone:=True;
  Until(ColsDone);
End;//SetJoin

Procedure TMarkStringGrid.ClearJoin(FirstCol,FirstRow,SecondCol,SecondRow:Integer);
Var
  TmpCol,StartRow,TmpRow:Integer;
  ColsDone,RowsDone : Boolean;
Begin
  //Wrapper for Clear2Joins so that multiple cells can be cleared in one shot
  TmpCol:=FirstCol;
  ColsDone:=False;
  Repeat
    TmpRow:=FirstRow;
    StartRow:=FirstRow;
    RowsDone:=False;
    Repeat
      Clear2Joins(FirstCol,StartRow,TmpCol,TmpRow); //clear the Join
      If SecondRow>FirstRow Then Begin
        Inc(TmpRow);
        If TmpRow>SecondRow Then RowsDone:=True;
        If TmpRow-StartRow>1 Then Inc(StartRow);
      End;
      If SecondRow<FirstRow Then Begin
        Dec(TmpRow);
        If TmpRow<SecondRow Then RowsDone:=True;
        If StartRow-TmpRow>1 Then Dec(StartRow);
      End;
      If SecondRow=FirstRow Then RowsDone:=True;
    Until(RowsDone);

    If SecondCol>FirstCol Then Begin
      Inc(TmpCol);
      If TmpCol>SecondCol Then ColsDone:=True;
      If TmpCol-FirstCol>1 Then Inc(FirstCol);
    End;
    If SecondCol<FirstCol Then Begin
      Dec(TmpCol);
      If TmpCol<SecondCol Then ColsDone:=True;
      If FirstCol-TmpCol>1 Then Inc(FirstCol);
    End;
    If SecondCol=FirstCol Then ColsDone:=True;
  Until(ColsDone);

End;//ClearJoin

//--- Text positioning ---------------------------------------------------------
Procedure TMarkStringGrid.SetTextPos(ACol,ARow : Integer;HorPos,VertPos:AnsiChar);
Var
  TmpCell : OptionRec;
Begin
  //Set the text position
  TmpCell:=GetCellValues(ACol,ARow); //get the current values if any
  //Check the positioning
  Horpos:=UpCase(Horpos);
  If Not CharInSet(Horpos,['L','C','R']) Then Exit;
  Vertpos:=UpCase(Vertpos);
  If Not CharInSet(Vertpos, ['T','C','B']) Then Exit;
  //Assign the new values
  TmpCell.TextPos:=Horpos+VertPos;
  SetCellValues(TmpCell);
End;//SetTextPos

Procedure TMarkStringGrid.GetTextPos(ACol,ARow : Integer;Var HorPos,VertPos:AnsiChar);
Var
  TmpCell : OptionRec;
Begin
  //return the current text positioning
  TmpCell:=GetCellValues(ACol,ARow); //get the current values if any
  HorPos:=TmpCell.TextPos[1];
  VertPos:=TmpCell.TextPos[2];
End;//GetTextPos

Procedure TMarkStringGrid.ClearTextPos(ACol,ARow : Integer);
Var
  TmpCell : OptionRec;
Begin
  //Set the text position
  TmpCell:=GetCellValues(ACol,ARow); //get the current values if any
  TmpCell.TextPos:='LT';//default values
  SetCellValues(TmpCell);
End;//ClearTextPos

//Column processing-------------------------------------------------------------
Procedure TMarkStringGrid.SetColColours(ACol:Integer;BackColour,FontColour:LongInt);
Var
  TmpRow : Integer;
Begin
  TmpRow:=Self.FixedRows; //start after the fixed rows
  While TmpRow<Self.RowCount Do Begin
    //Set the background if it is not a bypass value
    If BackColour<>-1 Then
      SetColour(ACol,TmpRow,BackColour);
    //Set the font colour if it is not a bypass value
    If FontColour<>-1 Then
      SetFontColour(ACol,TmpRow,FontColour);
    Inc(TmpRow);
  End;
End;//SetColColours;

Procedure TMarkStringGrid.SetColLock(ACol:Integer);
Var
  TmpRow : Integer;
Begin
  TmpRow:=Self.FixedRows; //start after the fixed rows
  While TmpRow<Self.RowCount Do Begin
    SetCellLock(ACol,TmpRow);
    Inc(TmpRow);
  End;
End;//SetColLock

Procedure TMarkStringGrid.ClearColLock(ACol:Integer);
Var
  TmpRow : Integer;
Begin
  TmpRow:=Self.FixedRows; //start after the fixed rows
  While TmpRow<Self.RowCount Do Begin
    ClearCellLock(ACol,TmpRow);
    Inc(TmpRow);
  End;
End;//ClearColLock

//Row processing ---------------------------------------------------------------
Procedure TMarkStringGrid.SetRowColours(ARow:Integer;BackColour,FontColour:LongInt);
Var
  TmpCol : Integer;
Begin
  TmpCol:=Self.FixedCols; //start after the fixed columns
  While TmpCol<Self.ColCount Do Begin
    //Set the background if it is not a bypass value
    If BackColour<>-1 Then
      SetColour(TmpCol,ARow,BackColour);
    //Set the font colour if it is not a bypass value
    If FontColour<>-1 Then
      SetFontColour(TmpCol,ARow,FontColour);
    Inc(TmpCol);
  End;
End;//SetRowColours;

Procedure TMarkStringGrid.SetRowLock(ARow:Integer);
Var
  TmpCol : Integer;
Begin
  TmpCol:=Self.FixedCols; //start after the fixed columns
  While TmpCol<Self.ColCount Do Begin
    SetCellLock(TmpCol,ARow);
    Inc(TmpCol);
  End;
End;//SetRowLock

Procedure TMarkStringGrid.ClearRowLock(ARow:Integer);
Var
  TmpCol : Integer;
Begin
  TmpCol:=Self.FixedCols; //start after the fixed columns
  While TmpCol<Self.ColCount Do Begin
    ClearCellLock(TmpCol,ARow);
    Inc(TmpCol);
  End;
End;//SetRowLock

//Cell locking -----------------------------------------------------------------
Procedure TMarkStringGrid.SetEditText(ACol, ARow: Longint; const Value: String);
Var
  TmpCell : OptionRec;
Begin
  //Check if the cell is locked and if it is then don't allow the user to edit the values
  TmpCell:=GetCellValues(ACol,ARow); //get the current values if any
  If TmpCell.Locked Then Begin
    Beep;
    Exit;
  End;
  //proceed with the normal call if it is unlocked
  Inherited;
End;

Procedure TMarkStringGrid.SetCellLock(ACol, ARow: Longint);
Var
  TmpCell : OptionRec;
Begin
  //Set the cell lock
  TmpCell:=GetCellValues(ACol,ARow); //get the current values if any
  TmpCell.Locked:=True;
  SetCellValues(TmpCell);
End;//setcelllock

Procedure TMarkStringGrid.ClearCellLock(ACol, ARow: Longint);
Var
  TmpCell : OptionRec;
Begin
  //Set the cell lock
  TmpCell:=GetCellValues(ACol,ARow); //get the current values if any
  TmpCell.Locked:=False;
  SetCellValues(TmpCell);
End;//setcelllock

Function TMarkStringGrid.GetCellLock(ACol, ARow: LongInt):Boolean;
Var
  TmpCell : OptionRec;
Begin
  //Set the cell lock
  TmpCell:=GetCellValues(ACol,ARow); //get the current values if any
  GetCellLock:=TmpCell.Locked;
End;//getcelllock


//--- Printing ---------------------------------------------------------------
Function TMarkStringGrid.DoScale(TheVal:Double):Integer;
Begin
  DoScale:=Round(TheVal*PrintScaling);
End;

Procedure TMarkStringGrid.DoPrint(StartX,StartY:Integer;ScalePrint:Double);
Begin
  //Output the grid to the printer
  HardCopy:=True;
  PrintLeft:=StartX;
  PrintTop:=StartY;
  PrintScaling:=ScalePrint;
  Self.Refresh;
  HardCopy:=False;
End;

procedure Register;
begin
  RegisterComponents('Stomsa', [TMarkStringGrid]);
end;

end.
