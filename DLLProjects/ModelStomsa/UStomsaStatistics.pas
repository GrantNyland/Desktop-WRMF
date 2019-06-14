unit UStomsaStatistics;
{$HINTS OFF}
{$WARNINGS OFF}

{_______________________________________________________________________________
  The following routines were translated from FORTRAN.

  Original comments have been included for completeness; superfluous code has
  been removed were identified.

  Stephen Langenhoven, December 2000, Johannesburg, South Africa.
 _______________________________________________________________________________

  Original coding by
  Prof. G.G.S Pegram
  Dr. R.S. Mckenzie
  P. van Rooyen
  M. Lemmer
  T. Frame
 _______________________________________________________________________________}


//check that BREAK statements should not actually be CONTINUE statements !!!

interface

uses UStomsaData;

  function SNV(ITYPE : Integer;var ErrorOccured : Boolean; AJV,GAMMA,DELTA,XLAM,XI:array1F) : double;
  function XNORM(TheNumber: double): double;
  function AJV(ITYPE : Integer;var ErrorOccured : Boolean; SNVR,GAMMA,DELTA,XLAM,XI:array1F) : double;
{
  procedure FITMARG(var N,ISTART,NZF,ICURV:integer; var Yin,X,W:array1000F;
                    var GA,DE,XL,XX,CR:arrayCoefs; var A1, S1 : Array1F);
  procedure MDSTATS(var NY,NNP,M1,M2: ArrayT1I; Y,X,W : array1000F;
                    var XA,XSD,XS3,XS4,
                        WA,WSD,WS3,WS4,
                        CL1,CL2,
                        XX1,QCH1,XX2,QCH2 : Array1F);

  procedure CROSS(var NG: integer; N, IYS : array500I;
                      Phi1, Phi2, ThT1, ThT2 : array500F;
                      Y : array500_1000F;
                      S0 : array500_500F;
                      EG0, EH1, EH0 : array500F;
                      B, B0, B1, A, C, BB, D : array500_500F);
  procedure MThrnk(NSYRI : arrayT1I; NYRS, ISTRY:arrayT2I;
                   XH1, XH2:array13_100F;
                   FLOWD1, FLOWD2:array101_100_13F;
                   var RSHO, RMEANO:array13F;
                   var PO:array7_13F);
}
implementation

uses
  Math, SysUtils;

function SNV(ITYPE : Integer;var ErrorOccured : Boolean; AJV,GAMMA,DELTA,XLAM,XI:array1F) : double;
{C     ALGORITHM A.S. 100.2  APPL. STATS. (1976) VOL 25 (2)
C     TRANSFORMS A JOHNSON VARIATE (AJV) TO A NORMAL VARIATE (SNV)
C        - THE SECOND OF TWO SURVIVORS}
var
  Intermediate, Intermediate2 : Extended;
  LoopCount                   : Integer;
begin
  ErrorOccured := false;
  Result := 0.0;
  try
    case ITYPE of
      1 : begin  //SL Distribution
            Intermediate := Ajv[1] * XLam[1] - Xi[1];
            if Intermediate <= 0 then
              ErrorOccured := true
            else
              Result := Ln(Intermediate)*Delta[1] + Gamma[1];
          end;
      2 : begin  //SU Distribution
            Intermediate := (Ajv[1] - Xi[1]) / XLam[1];
            if Intermediate > -63 then
              Intermediate :=  Sqrt(Intermediate*Intermediate+1)+Intermediate
            else
              Intermediate := -0.5/Intermediate;
            Result := Ln(Intermediate)*Delta[1] + Gamma[1];
          end;
      3 : begin  //SB Distribution - Modifed to reduce the size of outliers
            Intermediate := Ajv[1] - Xi[1];
            Intermediate2 := Xlam[1]-Intermediate;
            LoopCount := 0;
            while (Intermediate <= 0) or (Intermediate2 <= 0) do
            begin
              Inc(LoopCount); //To prevent entering an infinite loop
              if LoopCount > 10000 then
              begin
                ErrorOccured := true;
                break;
              end;//if LoopCount

              if Intermediate <= 0 then
                AJV[1] := XI[1] + 0.0001
              else
                AJV[1] := Xlam[1] + XI[1] - 0.0001;

              Intermediate := Ajv[1] - Xi[1];
              Intermediate2 := Xlam[1]-Intermediate;
            end;//While (Intermediate

            if ErrorOccured = false then
              result := Ln(Intermediate/Intermediate2) * Delta[1] + Gamma[1]
            else
              result := 0.0;
          end;
      4 : begin  //Normal Distribution
            Result := Delta[1]*Ajv[1]+Gamma[1];
          end;
    end;//case
  except
    on EInvalidOp do
    begin
      ErrorOccured := true;
      Result := 0.0;
    end;
  end;//try
end;//FUNCTION SNV

function XNORM(TheNumber : double): double;
{C       XNORM returns xp where P(xp) = 1-Q(xp)=p : Q() from A&S 26.2.23 !MAY97
C       but the catch is that it is only accurate to 4 decimals in the
C       range 0<p<0.5 - also a sign switch is required - nevertheless,
C       it is quite adequate for our purposes - i.e. for chi-square
C       tests and random normal number generation}
const
  Value1 = 1.432788;
  Value2 = 0.189269;
  Value3 = 0.001308;
  Value4 = 2.515517;
  Value5 = 0.802853;
  Value6 = 0.010328;
var
  Intermediate, Intermediate2, Muliplier : Extended;
begin
  Intermediate := TheNumber;
  Muliplier := -1;
  if TheNumber > 0.5 then
  begin
    Intermediate := 1 -  TheNumber;
    Muliplier := 1;
  end;

  Intermediate := sqrt(-ln(Intermediate*Intermediate));
  Intermediate2 := 1 + Intermediate * (Value1+Intermediate*(Value2+Intermediate*Value3));
  Intermediate := Intermediate-(Value4+Intermediate*(Value5+Value6*Intermediate))/Intermediate2;

  Result := Muliplier * Intermediate;
end;//FUNCTION XNORM

function AJV(ITYPE : Integer;var ErrorOccured : Boolean; SNVR,GAMMA,DELTA,XLAM,XI:array1F) : double;
{C     CONVERTS A STANDARD NORMAL VARIATE (SNVR) TO A JOHNSON
C     VARIATE (AJV) - THE ONLY ONE OF TWO SURVIVORS}
var
  Intermediate, Intermediate2 : Extended;
begin
  ErrorOccured := false;
  Result := 0.0;
  case ITYPE of
    1 : begin  //SL Distribution
          Result := XLam[1] * (EXP((SNVR[1]-GAMMA[1])/DELTA[1])+XI[1]);
        end;
    2 : begin  //SU Distribution
          Intermediate := EXP((SNVR[1]-GAMMA[1])/DELTA[1]);
          Intermediate := 0.5 * (Intermediate - 1/Intermediate);
          Result := Xlam[1] * Intermediate + XI[1];
        end;
    3 : begin  //SB Distribution - Modifed to reduce the size of outliers
          Intermediate := (Snvr[1]-Gamma[1])/Delta[1];
          Intermediate2 := EXP(-ABS(Intermediate));
          Intermediate2 := (1-Intermediate2)/(1+Intermediate2);
          if Intermediate < 0 then
            Intermediate2 := Intermediate2 * -1;
          Result := 0.5*Xlam[1]*(Intermediate2+1)+XI[1];
        end;
    4 : begin  //Normal Distribution
          Result := (Snvr[1]-Gamma[1])/Delta[1];
        end;
  end;//case
end;//FUNCTION AJV

function XMin(TheArray : array1000F; ArrayLength : Integer) : Double;
{C       XMIN RETURNS THE MINIMUM OF X}
var
  Loop : Integer;
begin
   Result := TheArray[1];
   for Loop := 1 to ArrayLength do
     if TheArray[Loop] < Result then
       Result := TheArray[Loop];
end;//FUNCTION XMin

function XMax(TheArray : array1000F; ArrayLength : Integer) : Double;
var
  Loop : Integer;
begin
   Result := TheArray[1];
   for Loop := 1 to ArrayLength do
     if TheArray[Loop] > Result then
       Result := TheArray[Loop];
end;//FUNCTION XMax

function XMean(TheArray : array1000F; ArrayLength : Integer) : Double;
{C       XMEAN RETURNS THE MEAN OF THE VECTOR X}
var
  SX : Extended;
  loop : integer;
begin
  SX := 0.0;
  for loop := 1 to ArrayLength do
    SX := SX + TheArray[Loop];
  Result := SX/ArrayLength;
end;//FUNCTION Xmean

function StDev(TheArray : array1000F; ArrayLength : Integer; ArrayMean : Double) : Double;
{C       STDEV RETURNS THE STANDARD DEVIATION OF THE VECTOR X OF MEAN A}
var
  SDX : Extended;
  loop : integer;
begin
  SDX := 0.0;
  for loop := 1 to ArrayLength do
    SDX := SDX + power((TheArray[loop] - ArrayMean),2);
  Result := sqrt(SDX/ArrayLength);
  {Difference from standard complier function as this uses n*(n-1) in the divisor}
end;//FUNCTION StDev

function Skew(TheArray : array1000F; ArrayLength : Integer; ArrayMean, ArrayStdDev : Double) : Double;
{C       SKEW RETURNS THE THIRD CENTRAL MOMENT OF THE VECTOR X OF
C       MEAN A & STANDARD DEVIATION S, SCALED BY S**3.}
var
  SK : Extended;
  loop : integer;
begin
  SK := 0.0;
  for loop := 1 to ArrayLength do
    SK := SK + power((TheArray[loop]-ArrayMean),3);
  Result := SK /  (ArrayLength*power(ArrayStdDev,3));
end;//FUNCTION Skew

function Excess(TheArray : array1000F; ArrayLength : Integer; ArrayMean, ArrayStdDev : Double) : Double;
{C       EXCESS RETURNS THE FOURTH CENTRAL MOMENT OF THE VECTOR X OF
C       MEAN A & STANDARD DEVIATION S, SCALED BY S**4, LESS 3}
var
  SEX : Extended;
  loop : integer;
begin
  SEX := 0.0;
  for loop := 1 to ArrayLength do
    SEX := SEX + power((TheArray[Loop]-ArrayMean),4);
  Result := SEX / (ArrayLength * power(ArrayStdDev,4))-3;
end;//FUNCTION Excess

function Rev(A,B,X : Double) : Double;
{c------------------------------------------------------------------------------
c
        FUNCTION REV(a,b,x)
c
c------------------------------------------------------------------------------
c
c       to compute the reverse of the logit where a < x < b
c
c------------------------------------------------------------------------------}
begin
  Result := (B*Exp(X)+A)/(Exp(X)+1);
end;//FUNCTION Rev

procedure FdNelm(var N : Integer; var X : array30F; var F : Double; var H  : array30F;
                 EPS : double;var IT,MAXIT,IER : Integer);
{c------------------------------------------------------------------------------
c
        SUBROUTINE FDNELM(N,X,F,H,EPS,IT,MAXIT,IER,FUNCT)
c
c------------------------------------------------------------------------------
c
c       nelder & mead flexible simplex search routine .
c       ref. Himmelblau - process analysis by statistical methods -
c       john wiley - 1970
c
c       arguments set on entry to the routine:
c
c               n  -  number of parameters in the vector x
c               x  -  vector of parameters to be optimized
c               h  -  an initial choice of step sizes defining the simplex
c               eps - a small number set to stop the computation
c               maxit - the maximum number of iterations
c
c       arguments set on exit
c
c               f  -  the value of the objective function at the last x
c               it -  the actual number of iterations performed
c               ier - an error code:
c                       0 for normal exit
c                       1 for n > 30
c                       2 for it > maxit
c                       3 if DNELM goes into flip-flop search
c
c       a few minor modifications - mostly printing and initialization -
c       have been made besides the flip-flop detector like setting gr=1.1
c       in an attempt to get around it.  the value of f printed is the
c       current minimum
c
c       DNELM calls a subroutine FUNCT which is user written
c
c       dimensions must be x(n),xs(n),fp(n+1),h(n),p(n,n+2)
c
c------------------------------------------------------------------------------}
var
  Loop1, np : Integer;
  XS : array30F;
  FP : Array[1..32] of Double;
  P : Array[1..30,1..32] of double;
  NC, N1, NH, NS, NL : Integer;
  GR, GE, GC, X0, X1, X2, F0, F1, F2, S, SS, Fbar, Test, FS : Double;
Label
  Line50, Line55, Line60, Line65, Line95, Line100, Line120, Line130, Line150;

  procedure FUNCT(var npar : integer; var P : array30F; var ff : double);
  {c------------------------------------------------------------------------------
  c
          subroutine FUNCT(npar,p,ff)
  c
  c------------------------------------------------------------------------------
  c
  c       a subroutine for computing the AIC for a given choice of logits
  c       of inverse roots of PHI & THETA polynomials describing an ARMA model.
  c       these will have been passed from FDNELM which is doing the optimization,
  c       however they have to be transformed back from logits before use.
  c
  c       the indices (the usual value is 1 in each) corresponding to each root
  c       as well as the x vector, are passed in named common blocks.
  c
  c   |   in this version, the coefficients f(i) & t(j) appear in the polynomials
  c   |   PHI(B) & THETA(B), where i=1,2,..,np<=4 & j=1,2,..,mq<=4 so that the
  c   |   ARMA model then takes on the form:
  c   |
  c   |   {1-f[1]B^n[1]...-f[np]B^n[np]x(t) = {1-t[1]B^m[1]...-t[mq]B^m[mq]a(t)
  c   |
  c   |   these alterations were made on 1/10/87.  ggsp.
  c   |
  c
  c------------------------------------------------------------------------------}
  var
    FLoop1, FLoop2 : Integer;
    Lag : Integer;
    PP : array30F;
    F, T, FW, TW : Array20F;
    NW, MW, N, M : Array20I;

    naxp, nit, MaxQ, mit : integer;
    index, jndex : integer;
  {      IMPLICIT REAL (KIND=8) ( A-H,O-Z )
          common /data/ x(1000),a(1000),sa,d
          common /par/ np,mq,nx,nn(4),mm(4),minit,n(20),m(20),naxp,maxq
          common /work/ aw(-1000:1000),ew(-1000:1000),xw(-1000:1000)
          common /vals/ f(20), t(20)
          common /init/ init
  c
          dimension p(30),pp(30),fw(20),tw(20),nw(20),mw(20)
          integer*2 i,j,it
          save lag
  c
          data pp/30*0.0/,one/1.0d0/}
  begin
  //  if init = 0 then
  //    lag := 20;
    for FLoop1 := 1 to 20 do
    begin
      PP[FLoop1] := 0.0;
      F[FLoop1] := 0.0;
      T[FLoop1] := 0.0;
      N[FLoop1] := 0;
      M[FLoop1] := 0;
      NW[FLoop1] := 0;
      MW[FLoop1] := 0;
    end;//for Loop1

  //NOTE: np from the calling routine (FDNelm)

  {c
  c       reverse the transform
  c}
    if (np = 4) then
      PP[4] := Rev(-1,1,P[4]);
    if (np >= 3) then
      PP[3] := Rev(PP[4]-1,1-PP[4],P[3]);
    if (np >= 2) then
      PP[2] := Rev(-1,1,P[2]);
    if (np >= 1) then
      PP[1] := Rev(PP[2]-1,1-PP[2],P[1]);

// WHERE THE HELL DOES MQ COME FROM ?????????????

{    if (MQ = 4) then
      PP[Loop2 + 4] := Rev(-1,1,P[Loop2+4]);
    if (MQ >= 3) then
      PP[Loop2 + 3] := Rev(PP[Loop2+4]-1,1-PP[Loop2+4],P[Loop2+3]);
    if (MQ >= 2) then
      PP[Loop2+2] := Rev(-1,1,P[Loop2+2]);
    if (MQ >= 1) then
      PP[Loop2+1] := Rev(PP[Loop2+2]-1,1-PP[Loop2+2],P[Loop2+1]);}


{  c
  c       fw,tw,nw,mw are working vectors to sort out the polynomials
  c       into matching f & n, t & m
  c
  c       first the AR polynomial coefficients are computed in a working array
  c}
    if (np >= 1) then
    begin
      FW[1] := PP[1];
      //NW[1] := NN[1];
    end;//if (Loop2
    if (np >= 2) then
    begin
      FW[2] := PP[2];
      //NW[2] := NN[2];
    end;//if (Loop2
    if (np >= 3) then
    begin
      FW[3] := PP[3];
      //NW[3] := NN[3];
      FW[4] := -PP[1] * PP[3];
      //NW[4] := NN[1] + NN[3];
      FW[5] := -PP[2] * PP[3];
      //NW[5] := NN[2] + NN[3];
    end;//if (Loop2
    if (np >= 4) then
    begin
      FW[6] := PP[4];
      //NW[6] := NN[4];
      FW[7] := -PP[1] * PP[4];
      //NW[7] := NN[1] + NN[4];
      FW[8] := -PP[2] * PP[4];
      //NW[8] := NN[2] + NN[4];
    end;//if (Loop2

{  c
  c       (naxp is the largest index in the AR side)  - now sort the polynomial
  c       into compact form accumulating the coefficients of each lag
  c}
    naxp := 0;
    if (np >= 1) then
    begin
      for FLoop1 := 1 to 8 do
      begin
        nit :=  NW[FLoop1];
        if nit = 0 then
          Continue;
        N[nit] := nit;
        F[nit] := F[nit] + FW[FLoop1];
        naxp := Max(naxp,nit);
      end;//for FLoop1
    end;//if (Loop2

{  c
  c       now do the same for the MA polynomial - coefficients first
  c}
  //if (MQ >= 1) then
    begin
      TW[1] := PP[np + 1];
      //MW[1] := MM[1];
    end;//if (MQ
    //if (MQ >= 2) then
    begin
      TW[2] := PP[np + 2];
      //MW[2] := MM[2];
    end;//if (MQ
    //if (MQ >= 3) then
    begin
      TW[3] := PP[np + 3];
      //MW[3] := MM[3];
      TW[4] := -PP[np + 1] * PP[np + 3];
      //MW[4] := MM[1] + MM[3];
      TW[5] := -PP[np + 2] * PP[NP + 3];
      //MW[5] := MM[2] + MM[3];
    end;//if (MQ
    //if (MQ >= 4) then
    begin
      TW[6] := pp[np + 4];
      //MW[6] := MM[4];
      TW[7] := -PP[np + 1] * PP[np + 4];
      //MW[7] := MM[1] + MM[4];
      TW[8] := -PP[np + 2] * PP[np + 4];
      //MW[8] := MM[2] + MM[4];
    end;//if (MQ

{  c
  c       (maxq is the largest index in the MA side)  - then the compact form
  c}
    MaxQ := 0;
    //if (MQ >= 1) then
    begin
      for FLoop1 := 1 to 8 do
      begin
        mit := MW[FLoop1];
        if (mit = 0) then
          Continue;
        M[mit] := mit;
        T[mit] := T[mit] + TW[FLoop1];
        MaxQ := Max(MaxQ,mit);
      end;//for FLoop1
    end;//if (MQ

    index := 0;
    //jndex := 0;
    if (np > 0) then
      index := N[naxp];
    //if (MQ > 0) then
      jndex := M[MaxQ];

    lag := Max(index,jndex);
    //if (lag > 20) or (lag >= nx) then
    begin
      //lag is too big - raise an error, reduce np and/or mq
    end;//if (lag

    //for FLoop1 := (NX - lag) downto 1 do
    begin
{  c
  c       compute v, the AR part of the difference equation
  c}
      //V := XW[FLoop1];
      for FLoop2 := 1 to naxp do
      begin
        //V := V - XW[FLoop1 + N[FLoop2]] * F[FLoop2];
      end;//for FLoop2

{  c
  c       compute ew(t)
  c}
      for FLoop2 := 1 to MaxQ do
      begin
        //V := V + EW[Floop1 + M[FLoop2]] * T[FLoop2];
      end;//for FLoop2
    end;//for FLoop1

{  c
  c       now start to compute the hind-casted x-values until they fade
  c}
  //min := -1000 + lag;
  //for FLoop1 := 0 downto Min do
    begin
{  c
  c       compute xw(t)
  c}
    //W := 0.0;
      for FLoop2 := 1 to naxp do
      begin
        //W := W + XW[FLoop1 + N[FLoop2]] * F[FLoop2];
      end;//for FLoop2

{  c
  c       include the contribution of ew(t) when it is effective
  c}
      if (FLoop1 >= -lag) then
      begin
        for FLoop2 := 1 to MaxQ do
        begin
          //W := W - EW[FLoop1 + M[FLoop2]] * T[FLoop2];
        end;//for FLoop2
      end;//if (FLoop1

    //XW[FLoop1] := W;
    //minit := FLoop1;
    //if (ABS(W) < 0.00001) then
    //  Break;
    end;//for FLoop1

{  c
  c       do the forward sweep from minit, but compute a(it) for it>0 only
  c
}
    S := 0.0;
    //for FLoop1 := minit to NX do
    begin
{  c
  c       compute w
  c}
      //W := XW[FLoop1];
      for FLoop2 := 1 to naxp do
      begin
        //W := W - XW[FLoop1 - N[FLoop2]] * F[FLoop2];
      end;//for FLoop2

{  c
  c       compute a(t)
  c}
      for FLoop2 := 1 to MaxQ do
      begin
        //W := W + AW[FLoop1 - M[FLoop2]] * T[FLoop2];
      end;//for FLoop2

      //AW[FLoop1] := W;
      //if (FLoop1 <= 0) then
        //Continue;
      //A[FLoop1] := W;
      //S := S + W * W;
    end;//for FLoop1

{  c
  c       the AIC is a weighted sum of squares, however,
  C       WALTER ZUCCHINI SUGGESTS THAT THE AICc OF HURVICH & TSAI (Regression
  C       and Time Series Model Selection in Small Samples, Biometrika, (1989),
  C       v76, n2, pp297-307) IS A FURTHER IMPROVEMENT.  FOR AUTOREGRESSION
  C       THEY SUGGEST AICc = T.log[var] + T.(T+NPAR)/(T-NPAR-2)
  c
          xn = dble(nx)
  c        ff=xn*log(s/xn)+2.*dble(npar)
          ff=xn*log(s/xn)+xn*(xn+dble(npar))/(xn-dble(npar)-2.)
  c
  c       add the McLeod term = -(1/2)log|M(p,q)|
  c}

    for FLoop1 := 1 to naxp do
      PP[FLoop1] := F[FLoop1];
    for FLoop1 := 1 to MaxQ do
      PP[naxp + FLoop1] := T[FLoop1];

    //d := 0.0;

    if Lag > 4 then
      Exit;

{  c
  c       because there is a practical upper limit of 4 in DMAC
  c}
    //d := DMac(PP,naxp,MaxQ);

{  c
  c       if (DMAC.le.0.0) then we have a near singularity in the parameters
  c       so we do not include the term in the AIC.
  c}
    //if (d <= 0.0) then
      //exit;
    //FF := FF + d;
  end;//PROCEDURE Funct

begin
  gr := 1.1;
  ge := 2;
  gc := 0.5;
  it := 0;
  //nl := 0;
  ier := 0;
  x0 := 0.0;
  x1 := 0.0;
  x2 := 0.0;
  f0 := 0.0;
  f1 := 0.0;
  f2 := 0.0;
  if (n >  30) then
   ier := 1;
  if (ier <> 0) then
    exit;

  for Loop1 := 1 to 30 do
  begin
    XS[Loop1] := 0.0;
    FP[Loop1] := 0.0;
    for np := 1 to 32 do
      P[Loop1,np] := 0.0;
  end;//for Loop1
  FP[31] := 0.0;
  FP[32] := 0.0;

{c
c       initialize the vertices of the simplex
c}
  NC := N + 2;
  N1 := N + 1;
  for Loop1 := 1 to N1 do
    for np := 1 to N do
    begin
      P[np,Loop1] := X[np];
      if np = (Loop1-1) then
        P[np,Loop1] := X[np] + H[np];
    end;

  line150:
{c
c       calculate response at the vertices
c}
  for Loop1 := 1 to N1 do
  begin
    if Loop1 = N1 then
      break;
    for np := 1 to N do
    begin
      X[np] := P[np,Loop1];
      Funct(N,X,F);
      FP[Loop1] := F;
    end;//for Loop2
  end;//for Loop1

  Line100:
{c
c       find highest, lowest and second highest response
c}
  NH := 1;
  NL := 1;
  for Loop1 := 1 to N1 do
  begin
    if (FP[Loop1] > FP[NH]) then
      NH := Loop1;
    if (FP[Loop1] < FP[NL]) then
      NL := Loop1;
  end;//for Loop1

  NS := 1;
  for Loop1 := 1 to N1 do
  begin
    if Loop1 = NH then
      Break;
    if (FP[Loop1] > FP[NS]) then
      NS := Loop1;  
  end;//for Loop1

{c
c       test for convergence
c}
   S := 0;
   SS := 0;

   for Loop1 := 1 to N1 do
     S := S + FP[Loop1];

   FBar := S/N1;
   for Loop1 := 1 to N1 do
     SS := SS + Sqr(FP[Loop1]-FBar);

   test := Sqrt(SS/N);

   if (Test <= (1E-4 * Fbar * 0.01)) then
     goto line120;
   if (IT >= MaxIt) then
     goto Line130;

  X2 := X1;
  X1 := X0;
  F2 := F1;
  F1 := F0;
  X0 := P[1,NC];
  F0 := F;

  if (ABS(x0-x2) + ABS(f0-f2) < 1E-4) then
  begin
    IER := 3;
    goto Line120;
  end;//if

  IT := IT + 1;

{c
c       calculate the centroid excluding the highest point
c}
  for np := 1 to N do
  begin
    S := 0;
    for Loop1 := 1 to N1 do
    begin
      if (Loop1 = NH) then
        Break;
      S := S + P[np,Loop1];
    end;//for Loop2
    P[np,NC] := S/N;
  end;//for Loop1

{c
c       perform a reflection
c}
  for np := 1 to N do
    x[np] := (1 + GR) * P[np,NC] - GR * P[np,NH];

  Funct(N,X,F);

  if (F < FP[NL]) then
    goto Line50;
  if (F <= FP[NS]) then
    goto Line55;
  if (F <= FP[NH]) then
    goto Line60;
  if (F > FP[NH]) then
    goto Line65;

  Line50:
{c
c       expand the simplex
c}
  for np := 1 to N do
    XS[np] := GE * X[np] + (1 - GE) * P[np,NC];
  Funct(N,XS,FS);
  if (FS >= FP[NL]) then
    goto Line55;

{c
c       accept expanded point
c}
  FP[NH] := FS;
  for np := 1 to N do
    P[np,NH] := XS[np];
  goto Line100;

  Line55:
{c
c       accept reflected point
c}
  FP[NH] := F;
  for np := 1 to N do
    P[np,NH] := X[np];
  goto Line100;

  Line60:
{c
c       accept reflected point then contract the simplex
c}
  FP[NH] := F;
  for np := 1 to N do
    P[np,NH] := X[np];

  Line65:
{c
c       contract the simplex
c}
  for np := 1 to N do
    XS[np] := GC * P[np,NH] + (1 - GC) * P[np,NC];
  Funct(N,XS,FS);
  if (FS > FP[NH]) then
    goto Line95;
{c
c       accept the contracted point
c}
  FP[NH] := FS;
  for np := 1 to N do
    P[np,NH] := XS[np];
  goto Line100;

  Line95:
{c
c       contract the whole simplex
c}
  for Loop1 := 1 to N1 do
  begin
    if (Loop1 = NL) then
      continue;
    for np := 1 to N do
    begin
      P[np,Loop1] := 0.5 * (P[np,Loop1] + P[np,NL]);
    end;//for Loop2
  end;//for Loop1
  Goto Line150;

  Line130:
{c
c       take out the lowest vertex
c}
  IER := 2;

  Line120:
  for np := 1 to N do
    X[np] := P[np,NL];
  F := FP[NL];
end;//PROCEDURE FdNelm

function FnLog(X : array1000F; N : Integer; var G,D : Array1F; XI : Double) : extended;
{C ----------------------------------------------------------------------
C
C
        REAL (KIND=8) FUNCTION FNLOG (X,N,G,D,XI)
C
C       FNLOG COMPUTES THE L.H.S. OF EQ.(17) FROM MARKOVIC'S HYDROLOGY
C       PAPER # 8 SO THAT G, D & XI ARE THE MAXIMUM LIKELIHOOD
C       ESTIMATES OF THE LN3 PARAMETERS -A/S, 1/S & K0 WHERE K0 IS THE
C       LOWER BOUND, A IS THE MEAN OF THE LOGS OF X AND S IS THEIR
C       STANDARD DEVIATION.
C
C ----------------------------------------------------------------------}
var
  Loop       : Integer;
  Sum1, Sum2, T : Extended;
  W             : array1000F;
  A, S : Double;
begin
  Sum1 := 0.0;
  Sum2 := 0.0;
  for Loop := 1 to N do
  begin
    T := (X[Loop]-XI);
    W[Loop] := Ln(T);
    Sum1 := Sum1 + 1/T;
    Sum2 := Sum2 + W[Loop]/T;
  end;//for Loop
  A := XMean(W,N);
  S := StDev(W,N,A);
  D[1] := 1/S;
  G[1] := -A/S;
  Result := Sum1 * (S*S-A) + Sum2;
end;//FUNCTION FnLog

procedure LN3(X : array1000F; N : Integer; var GAMMA,DELTA,XI : Array1F; XMI : Extended);
{C ----------------------------------------------------------------------
C
C
        SUBROUTINE LN3 (X,N,GAMMA,DELTA,XI,XMI)
C
C       LN3 COMPUTES THE 3 PARAMETERS OF THE LN3 DISTRIBUTION FITTED
C       TO VECTOR X OF LENGTH N.  XLAM IS UNCHANGED BY THE ROUTINE.
C       THE PARAMETERS HAVE THE SAME MEANING AS IN THE JNSN ROUTINES.
C       XMI WILL ALREADY HAVE BEEN CALCULATED, SO IS PASSED IN THE
C       ARGUMENT LIST, TO HELP WITH THE LOCATION OF THE ROOT.
C       THE SEARCH FOR A SOLUTION IS BY BISECTION.
C
C       GAMMA AND DELTA ARE COMPUTED IN THE FUNCTION 'FNLOG'
C
C ----------------------------------------------------------------------}
var
  Loop1, Loop2 : Integer;
  XI1, XI2, XI3, F, F1, F2, F3, Del : Extended;
  G, D    : Array1F;

begin
  XI3 := XMI - 0.01;
  F3 := FnLog(X,N,G,D,XI3);

  Del := 0.5;
  for Loop1 := 1 to 200 do
  begin
    Del := Del * 2;
    XI1 := XMI - Del;
    F1 := FnLog(X,N,G,D,XI1);
    if (F1*F3 < 0) then
      Break;
  end;//for Loop1

  for Loop2 := 1 to 200 do
  begin
    XI2 := (XI1 + XI3)/2;
    F2 := FnLog(X,N,G,D,XI2);
    if (ABS(F2) < 1E-8) then
      break;
    if (F1*F2 = 0) then
      Break;
    if (F1*F2 < 0) then
    begin
      XI3 := XI2;
      F3 := F2;
    end
    else
    begin
      XI1 := XI2;
      F1 := F2;
    end;//if (F1*F2 < 0)
  end;//for Loop2

  Gamma := G;
  Delta := D;
  XI[1] := XI2;
  F := FnLog(X,N,G,D,XI[1]);
end;//PROCEDURE LN3

function FLogIT(A, B, X : double) : double;
{c------------------------------------------------------------------------------
c
        FUNCTION FLOGIT(a,b,x)
c
c------------------------------------------------------------------------------
c
c       to compute the logit transform of a bounded variable to map onto the
c       whole real line - useful for unconstrained function minimization -
c       where a < x < b
c
c------------------------------------------------------------------------------}
begin
  Result := Ln((X-A)/(B-X));
end;

function SBRoot(Y : array1000F; NY : integer; var YM,YS,XL : double) : Double;
{C ----------------------------------------------------------------------!FEB96
C                                                                       !FEB96
 REAL (KIND=8) FUNCTION SBROOT(Y,NY,YM,YS,XL)                           !FEB96
C                                                                       !FEB96
C       ONE DIMENSIONAL FUNCTION OF THE DERIVATIVE OF THE LIKELIHOOD    !FEB96
C       OF THE SB3 DISTRIBUTUION WRT XL - PARTNERS SB3                  !FEB96
C                                                                       !FEB96
C               CORRECTED                       GGSP    21/2/97         !FEB97
C                                                                       !FEB96
C ----------------------------------------------------------------------!FEB96}
var
  Loop  : Integer;
  YL    : array1000F;
  Z, S1 : Extended;
begin
  for Loop := 1 to NY do
  begin
    YL[Loop] := FLogIt(0.0,XL,Y[Loop]);
  end;

  YM := XMean(YL,NY);
  YS := StDev(YL,NY,YM);

  S1 := 0.0;
  for Loop := 1 to NY do
  begin
    Z := (YL[Loop] - YM)/YS;
    S1 := S1 + (Z/YS - 1)/(XL - Y[Loop]);
  end;

  Result := S1 + NY/XL;
end;

procedure SB3(var Y : array1000F; var NY : Integer; var B, YM, YS : Double; var XHI : Double);
{C ----------------------------------------------------------------------!FEB96
C                                                                       !FEB96
        SUBROUTINE SB3(Y, NY, B, YM, YS, XHI)                           !FEB96
C                                                                       !FEB96
C       COMPUTE PARAMETERS OF SB3 DISTRIBUTION BY 1-D ROOT-FINDING      !FEB96
C       USING SBROOT.      Y, NY & XHI COME IN B, YM & YS GO OUT.       !FEB96
C                                                                       !FEB96
C       Y IS VECTOR OF LENGTH NY WITH MAXIMUM XHI                       !FEB96
C                                                                       !FEB96
C       B IS ESTIMATED UPPER BOUND WHILE YM & YS ARE MEAN & SD          !FEB96
C       OF TRANSFORMED SEQUENCE                                         !FEB96
C                                                                       !FEB96
C       THIS PAIR IS A DERIVATIVE OF LN3 AND FNLOG WHICH WORK WELL      !FEB96
C                                                                       !FEB96
C                                                                       !FEB96
C                                                  GGSP    21/2/96      !FEB96
C                                                                       !FEB96
C ----------------------------------------------------------------------!FEB96}
var
  XL1, XL2, XL3,
  F1, F2, F3, F,
  Del : Double;
  Loop         : Integer;
begin
  XL1 := XHi + 0.01;
  F1 := SBRoot(Y,NY,YM,YS,XL1);
  Del := 0.5;
  for Loop := 1 to 200 do
  begin
    Del := Del * 2;
    XL3 := XHi + Del;
    F3 := SBRoot(Y,NY,YM,YS,XL3);
    if (F1 * F3 < 0) then
      Break;
  end;

  for Loop := 1 to 200 do
  begin
    XL2 := (XL1 + XL3)/2;
    F2 := SBRoot(Y,NY,YM,YS,XL2);
    if (ABS(F2) < 0.000000000000001) then
      Break;
    if (F1 * F2 = 0) then
      Break;
    if (F1 * F2 < 0) then
    begin
      XL3 := XL2;
      F3 := F2;
    end
    else
    begin
      XL1 := XL2;
      F1 := F2;
    end;
  end;

  B := XL2;
  if B > (100000000) then
    B := 3 * XHi;
  F := SBRoot(Y,NY,YM,YS,B);
end;//PROCEDURE SB3

// !!!!!!!! CHECK THE PARAMETERS ARE CORRECT !!!!!!!!!!
procedure Normaliz(var X : array1000F; var N : Integer; var Z : array1000F; var ITT : Integer;
                   var GAMMA,DELTA,XLAM,XI : Array1F; var Ifault : Integer;
                   var A,S : Double; var NZF,IDEBUG : Integer;
                   var GA,DE,XL,XX,CR : ArrayCoefs; var ICURV : Integer);
{C-----------------------------------------------------------------------
C
C       THIS SUBROUTINE HAS BEEN COMPLETELY REVISED.    GGSP    10/02/96
C       AND FURTHER REFINED TO CHOOSE ALL MODELS BY ML. GGSP    28/04/97
C
C       BECAUSE EPHEMERAL STREAMS HAVE MINIMUM OF POSITIVE FLOWS CLOSE
C       TO ZERO THERE IS NO POINT IN FITTING LOWER BOUND BY LN3/SB4, SO
C       IF NZF # 0 - LN2 OR SB3 END UP ONLY CANDIDATES  GGSP    13/05/97
C
C-----------------------------------------------------------------------
C
C       NORMLIZ DOES LITERALLY THAT - IT TAKES A VECTOR X OF LENGTH N,
C       COMPUTES THE FIRST FOUR MOMENTS WHICH IT PUBLISHES. ALL 4 MODELS
C       ARE FITTED BY MAXIMUM LIKELIHOOD.
C
C       A & S ARE THE MEAN AND STANDARD DEVIATION OF Z ESTIMATED BY ML
C       BEFORE STANDARDIZATION - THEY SHOULD = 0.0 & 1.0 EXACTLY, WHICH
C       THEY DID NOT WITH THE MOMENT ESTIMATES.
C
C       THE FUNCTIONS 'XMEAN,STDEV,SKEW,EXCESS,XMIN AND XMAX' ARE IN
C       THE FILE 'STATS'.
C
C                               ----OOO----
C
C       A MAJOR MODIFICATION OF THE ORIGINAL TO COPE WITH EPHEMERAL
C       STREAMS AND TO AVOID NEGATIVE REGENERATED FLOWS AS WELL AS
C       VIOLATION OF BOUNDS.  IN ADDITION, ALL MODELS ARE FITTED
C       BY MAXIMUM LIKELIHOOD - WE'VE DISPENSED WITH THE JOHNSON SUITE.
C
C       CHECK FOR NEGATIVE LOWER BOUND IN LN3 IN WHICH CASE PENALIZE IT
C       SO THAT THE LOGNORMAL CANDIDATE BECOMES THE LN2 BY DEFAULT.
C
C       SELECT THE APPROPRIATE MODEL AS THE ONE WITH THE MINIMUM
C       CRITERION OF LINHART & ZUCCHINI.
C
C       IN THE JOHNSON SUITE (WHICH IS NO LONGER USED TO FIT THE MODELS)
c       gamma,delta,xlam & xi are the transforms of the first 4 moments
c
c       itype indicates the original distribution of y
c               1 - Ln3    : x = gamma + delta(ln[y-xi]
c               2 - Su     : x = gamma + delta(arcsinh[(y-xi)/lambda]
c               3 - Sb     : x = gamma + delta(ln[(y-xi)/(lambda+xi-y)]
c               4 - normal : x = gamma + delta(y)
C
C       IN THIS VERSION OF NORMALIZ, THERE ARE 4 CANDIDATE DISTRIBUTIONS
C       TO BE FITTED:    LN3, LN2, SB(4) & SB3
C       WE ARE EXCLUDING NORMAL AND SU FROM THE CANDIDATURE
C
C       SELECTION BETWEEN THE MODELS WILL BE VIA WALTER ZUCCHINI'S
C       MINIMUM DISCREPANCY, WHERE THE CRITERIA ARE AS FOLLOWS:
C
C       LN2: CR = (1+LOG(2PI))/2 - GAMMA/DELTA - LOG(DELTA) + 2/N
C
C       LN3: CR = (1+LOG(2PI))/2 - GAMMA/DELTA - LOG(DELTA) + 3/N
C
C       SB3: CR = (1+LOG(2PI))/2 + SUM(LOG[Xi*(XLAM-Xi)])/N
C                                           - LOG[DELTA*XLAM] + 3/N
C
C       SB4: CR = (1+LOG(2PI))/2 + SUM(LOG[(Xi-XI)*(XLAM+XI-Xi)])/N
C                                           - LOG[DELTA*XLAM] + 4/N
C
C       BECAUSE THE LEADING CONSTANT TERM (1+LOG(2PI))/2 FROM THE
C       NORMAL IS CONSTANT (THE OTHER TERMS ARE FROM THE JACOBIAN OF THE
C       TRANSFORMATION EXCEPT FOR THE P/N TERM WHICH IS THE AIC), FOR
C       SELECTION OF THE BEST MODEL, WE WILL OMIT IT.
C
C       THE ONLY MODEL WHICH CAN VIOLATE BOUNDS IS THE LN3 WHICH COULD
C       GIVE A NEGATIVE LOWER BOUND IN WHICH CASE 4000 WILL BE ADDED TO
C       ITS CRITERION TO PREVENT IT FROM BEING SELECTED
C
C       ITT (1, 2, 3 OR 4) IS THE LOCAL TYPE VARIABLE SENT BACK TO
C       IDENTIFY THE FITTED DISTRIBUTION FOR THE PURPOSE OF PLOTTING.
C       ITYPE (1 OR 3) IS RESERVED FOR USE BY AJV & SNV, THE ONLY JNSN
C       SUBROUTINES THAT SURVIVE. WE WILL REPORT TYPES 1 AND 3 ONLY WITH
C       THE RELEVANT PARAMETERS SET TO ZERO FOR THE MORE PARSIMONIOUS
C       MODELS.
C
C                                                       GGSP - 28-04-97
C
C-----------------------------------------------------------------------}
var
  Loop : Integer;
  S1, S2, B1, B2, Sum, Arg : Extended;

  XN, F, FFF,
  B, YM, YS : Double;
  PAR, H : array30F;
  AJVR : Array1F;

  XHi, XLo : Double;
  MAXIT, NPAR, IT, Itype, IER : Integer;

  ErrorOccured : Boolean;

  W : array1000F;
  //For Procedure FunSB4
  AXI, XI4, BXI, AXUP, XUP, GA4, DE4 : Double;
  NX : Integer;

Label JumpToLN3;

  procedure FunSB4(var N : Integer; var X : array30F; var F : Double);
  {C=======================================================================
  C                                                                       !APR97
          SUBROUTINE FUNSB4(N,X,F)                                        !APR97
  C                                                                       !APR97
  C-------------------------------------------------------------------------------
  C                                                                       !APR97
  C       TO COMPUTE THE LIKELIHOOD FUNCTION OF THE SB4 DISTRIBUTION FOR  !APR97
  C       FEEDING TO FDNELM, THE VERSION ACCEPTING EXTERNAL FUNCTIONS.    !APR97
  C                                                                       !APR97
  C       THE PARAMETERS ARE BOUNDED BY                                   !APR97
  C                    ZER<XI<XMIN AND XMAX<XL+XI<INFINITY                !APR97
  C       SO THERE IS NO POSSIBILITY OF VIOLATION THEREFORE NO PENALTIES  !APR97
  C                                                                       !APR97
  C       THE CRITERION BASED ON THE LIKELIHOOD FUNCTION IS EVALUATED     !APR97
  C       IN THE MAIN PROGRAM AND THE DATA & THE FINAL PARAMETERS ARE     !APR97
  C       PASSED IN COMMON                                                !APR97
  C                                                                       !APR97
  C               GGSP            28/4/97                                 !APR97
  C                                                                       !APR97
  C-------------------------------------------------------------------------------}
  var
    Loop       : Integer;
    XX1, XX2,
    XL4, XNUM,
    S1, S2     : Double;
    Y          : array1000F;
  begin
    XX1 := X[1];
    XX2 := X[2];
    XI4 := Rev(AXI,BXI,XX1);
    XUP := Exp(XX2) + AXUP;
    XL4 := XUP - XI4;
    XNUM := NX;
    S1 := 0.0;
    S2 := 0.0;

    for Loop := 1 to NX do
    begin
      Y[Loop] := FLogIt(XI4,XUP,W[Loop]);
      S1 := S1 + Y[Loop];
    end;
    S1 := S1/XNUM;

    for Loop := 1 to NX do
      S2 := S2 + Sqr(Y[Loop] - S1);
    S2 := Sqrt(S2/XNUM);
    GA4 := -S1/S2;
    DE4 := 1/S2;

    S1 := 0.0;
    S2 := 0.0;

    { C       NOW COMPUTE THE NEGATIVE OF THE LIKELIHOOD WITHOUT THE CONSTANT !APR97
      C       TERM - NOTE THAT FDNELM IS A FUNCTION MINIMIZER AND WE WANT THE !APR97
      C       (RELATIVE) MINIMUM OF THE NEGATIVE LOGLIKELIHOOD FUNCTION U:    !APR97
      C                                                                       !APR97
      C           U = -log(xl*de) + Sig{log[(x(i)-xi)*(xl+xi-x(i)]/n +       !APR97
      C                           + Sig{[ga+de*y(i)]^2/2n                    !APR97
      C                                                                       !APR97}

    for Loop := 1 to NX do
    begin
      S1 := S1 + Sqr(GA4 + DE4 * Y[Loop]);
      Arg := (W[Loop] - XI4)*(XUP - W[Loop]);
      S2 := S2 + Ln(Arg);
    end;//for Loop
    F := -Ln(XL4 * DE4) + S1/(2 * XNUM) + S2/XNUM;
  end;//PROCEDURE FunSB4

begin
  XN := N;

  for loop := 1 to 4 do
  begin
    Ga[Loop] := 0.0;
    De[Loop] := 0.0;
    Xl[Loop] := 0.0;
    XX[Loop] := 0.0;
    CR[Loop] := N * 1000;
  end;

  for Loop := 1 to N do
    W[Loop] := X[Loop];

  XLo := XMin(W,N);
  XHi := XMax(W,N);

  S1 := XMean(W,N);
  S2 := StDev(W,N,S1);
  B1 := Skew(W,N,S1,S2);
  B2 := Excess(W,N,S1,S2)+3;

  if XLo < 0.0001 then
  begin
    CR[3] := 4000;
    goto JumpToLN3;
  end;

  AXI := 0 + 0.0001;
  BXI := XLo - 0.0001;
  AXUP := XHi + 1;

  XI4 := BXI/3.141592654{pi};
  PAR[1] := Ln(XI4/(BXI-XI4));
  XUP := XHi + 2000;
  PAR[2] := Ln(XUP-AXUP);
  H[1] := PAR[1]/10;
  H[2] := PAR[2]/100;
  MAXIT := 500;
  NPAR := 2;
  NX := N;

  //Once this piece is sorted out the rest should fall into place
  FDNELM(NPAR,PAR,FFF,H,1E-4,IT,MAXIT,IER);{function is FunSB4}
  FUNSB4(NPAR,PAR,F);
  //GA4... were set by FUNSB4 (in the COMMON part)
  GA[3] := GA4;
  DE[3] := DE4;
  XL[3] := XUP - XI4;
  XX[3] := XI4;

  Sum := 0.0;
  for Loop := 1 to N do
  begin
    Arg := (W[Loop]-XI4)*(XUP-W[Loop]);
    if (Arg <= 0) or (Arg > 1E30) then
    begin
      CR[3] := 4000;
      goto JumpToLN3;
      break;
    end;
    Sum := Sum + Ln(Arg);
  end;

  CR[3] := Sum/XN - Ln(DE[3]*XL[3]) + 4/XN;

  JumpToLN3:
  Xlam[1] := 1;
  LN3(W,N,Gamma,Delta,XI,XLo);
  GA[1] := Gamma[1];
  DE[1] := Delta[1];
  XL[1] := XLam[1];
  XX[1] := XI[1];
  CR[1] := -Gamma[1]/Delta[1] - Ln(Delta[1]) + 3/XN;

  if (XI[1] < 0) and (NZF = 0) then
    CR[1] := CR[1] + 4000;

  FFF := FNLog(W,N,GAMMA,DELTA,0.0);
  GA[2] := Gamma[1];
  DE[2] := Delta[1];
  XL[2] := 1;
  XX[2] := 0;
  CR[2] := -GA[2]/DE[2] - Ln(DE[2]) + 2/XN;

  A := 0.0;
  SB3(W, N, B, YM, YS, XHI);
  GA[4] := -YM/YS;
  DE[4] := 1/YS;
  XL[4] := B;
  XX[4] := A;
  SUM := 0.0;
  for Loop := 1 to N do
  begin
    Arg := W[Loop] * (B-W[Loop]);
    Sum := Sum + Ln(Arg);
  end;
  CR[4] := Sum/XN - Ln(DE[4]*XL[4]) + 3/XN;

  IT := 1;
  for Loop := 2 to 4 do
  begin
    if CR[Loop] < CR[IT] then
      IT := Loop;
  end;
  if NZF > 0 then
  begin
    IT := 2;
    if (CR[4] < CR[2]) then
      IT := 4;
  end;
  ICurv := IT;

  Gamma[1] := GA[IT];
  Delta[1] := DE[IT];
  XLam[1] := XL[IT];
  XI[1] := XX[IT];
  ITT := IT;

  If ITT <= 2 then
    IType := 1;
  if ITT >= 3 then
    IType := 3;

  for Loop := 1 to N do
  begin
    AJVR[1] := W[Loop];
    Z[Loop] := SNV(ITYPE,ErrorOccured,AJVR,GAMMA,DELTA,XLAM,XI);
  end;

  A := XMean(W,N);
  S := StDev(W,N,A);
end;//PROCEDURE Normaliz

procedure SSort(var TheArray : Array of double; ElementCount : Integer);
{C       Subroutine SSORT uses the SHELL SORT (a fast, efficient,
C       minimal storage and non-linear) ALGORITHM to sort the
C       N elements contained in V into ascending order.  Coded by
C       Tony Frame 08/83 while an undergrad at Natal University,
C       Introduced by GGSP}
var
  IDist, Ientp, Loop,
  I, J : Integer;
  TempValue : Double;
begin
  IDist := Round(Int( power(2,Round(Int( Ln(ElementCount)/Ln(2) ))) ));

  repeat
    Ientp := ElementCount - IDist;

    //!!!! This line changed because we are using a zero based array !!!!
    //for Loop := 1 to Ientp do
    for Loop := 0 to (Ientp-1) do
    begin
      I := Loop;
      J := Loop + IDist;
      repeat
        if TheArray[I] < TheArray[J] then
          break
        else
        begin
          TempValue := TheArray[I];
          TheArray[I] := TheArray[J];
          TheArray[J] := TempValue;
          J := I;
          I := I - IDist;
        end;//if TheArray
      until I <= 1;
    end;//for Loop

    IDist := IDist div 2;
  until (IDist < 1);
end;//PROCEDURE SSort

procedure Cell(var TheArray : array1000F; var ArrayLength : Integer; var ArrayMean, ArrayStdDev : Double;
               var HistogramArray : array1000F; var HistogramArraySize  : Integer);
{C-----------------------------------------------------------------------
C       CELL is a histogram calculator where the cells have equal
C       probability and are delimited assuming Normality.
C
C       on entry:       x       vector contains the data to be hist'd
C                       n       is the size of x
C                       a       is the mean of x
C                       s       is the standard deviation of x
C       on exit:        nhist   contains the scores in each cell
C                               divided according to a normal
C                               distribution.
C                       k       is the size of nhist
C-----------------------------------------------------------------------}
var
  Loop, Loop2, StartPoint, FinishPoint : Integer;
  FF, Upper, RHigh : Extended;
  Work : array1000F;
begin
  HistogramArraySize := Round(Int(3.3*Log10(ArrayLength)+1));
  FF := 1/HistogramArraySize;

  for Loop := 1 to ArrayLength do
    Work[Loop] := TheArray[Loop];

  SSort(Work,ArrayLength);
  Upper := 0.0;

  for Loop := 1 to (HistogramArraySize - 1) do
    HistogramArray[Loop] := 0.0;
  HistogramArray[HistogramArraySize] := ArrayLength;

  StartPoint := 1;
  for Loop := 1 to (HistogramArraySize-1) do
  begin
    Upper := Upper + FF;
    RHigh := XNORM(Upper)*ArrayStdDev+ArrayMean;
    FinishPoint := StartPoint;
    for Loop2 := StartPoint to ArrayLength do
    begin
      FinishPoint := Loop2;
      if Work[loop2] > RHigh then
        break
      else
        HistogramArray[Loop] := HistogramArray[Loop] + 1;
    end;//for Loop2
    StartPoint := FinishPoint;
  end;//for Loop

  for loop := 1 to (HistogramArraySize - 1) do
  begin
    HistogramArray[HistogramArraySize] := HistogramArray[HistogramArraySize] - HistogramArray[Loop]
  end;
end;//PROCEDURE Cell

function ChiSq(var TheArray : array1000F; var ArrayLength : Integer; var ArrayMean, ArrayStdDev : Double;
               var FreedomDegrees : Integer) : Double;
{C       CHISQ computes the Chi-square statistic for a distribution
C       fit assuming normality. It calls CELL to do the cell-counts.
C
C       a & s are the mean & stdev of x - input from the calling
C       routine m is the number of degrees of freedom - k-2-1 - for
C       the mean & stdev.}
var
  NHist     : array1000F;
  K, Loop   : Integer;
  Freq, Sum : Extended;
begin
  Cell(TheArray, ArrayLength, ArrayMean, ArrayStdDev, NHist, K);
  Freq := ArrayLength / K;
  Sum := 0.0;
  for Loop := 1 to K do
    Sum := Sum + (power((NHist[loop]-Freq),2)/Freq);
  Result := Sum;
  FreedomDegrees := K - 3;
end;//FUNCTION ChiSq

function QNormX(TheValue : Double) : Double;
{c------------------------------------------------------------------------------
c
c       QNORMX computes the exceedance probability to the standard normal
c       variate X using the function given in A & S - 26.2.17
c       there is a little problem with signs and inaccuracies either side
c       zero hence the pussy-footing.  also only values of |x| < 10 are
c       entertained otherwise the function returns 0 or 1 depending ..
c
c------------------------------------------------------------------------------}
const
  B0 = 0.2316419;
  B1 = 0.31938153;
  B2 = -0.356563782;
  B3 = 1.781477937;
  B4 = -1.821255978;
  B5 = 1.330274429;
  RRPI2 = 0.39894228;
var
  One, Two, Three, Four : Extended;
begin
  if TheValue = 0 then
    Result := 0.5
  else
  begin
    if TheValue > 10 then
      Result := 0.0
    else if TheValue < -10 then
      Result := 1
    else
    begin
      if TheValue < 0 then
      begin
        One := 1;
        Two := -1;
      end
      else
      begin
        One := 0.0;
        Two := 1;
      end;
      Three := ABS(TheValue);
      Four := 1 / (1+B0*Three);
      Four := Four * (B1+Four*(B2+Four*(B3+Four*(B4+Four*B5))));

      Result := One + Two * Four * RRPI2 / EXP(Three*Three/2);
    end;
  end;
end;//FUNCTION QNormX

function QChiSx(ChiSquare : Double; DegreesofFreedom : Integer) : Double;
{c------------------------------------------------------------------------------
c
c       QCHISX is a function that returns the value of the exceedance
c       probability Q for a given number of degrees of freedom DEF and
c       chi-square statistic CH.  it uses a modification of the Wilson and
c       Hilferty transform as devised by:
c               Severo, N.C. & M. Zelen - "Normal approximation to the
c               chi-square and non-central F probability functions",
c               Biometrika, v.47, pp.411-416, 1960.
c
c       it calls function QNORMX to compute an exceedance probability Q
c       corresponding to the normal deviate X which is the transform of CH
c
c------------------------------------------------------------------------------}
var
  One, Two : Extended;
begin
  One := 2 / (9 * DegreesofFreedom);
  One := (power((ChiSquare/DegreesofFreedom),1/3) - (1-One))   /sqrt(One);
  Two := -(sqrt(8)*(One * One -1) /
                   (3 * sqrt(DegreesofFreedom)) - One*(One*One-3)/4) * 2 / (27*DegreesofFreedom);
  Result := QNormX(One+Two);
end;//FUNCTION QChiSx

procedure FITMARG(var N,ISTART,NZF,ICURV:integer; var Yin,X,W:array1000F;
                  var GA,DE,XL,XX,CR:arrayCoefs; var A1, S1 : Array1F);
{C    This subroutine fits the marginal distribution for a natural streamflow gauge.}
Var
  NY, NNP, IType : Integer;
  XSD, WA, WSD : Double;
  ZD, ZP     : array1000F;
  Loop, ITT, IFAULT, IDebug, M : Integer;
  GAMMA,DELTA,XLAM,XI, SDX : Array1F;
  A, S, ZMX, XN, XA : Double;
  ErrorOccured : Boolean;
begin
  //Move all non-zero flows to the new array
  NZF := 0;
  for Loop := 1 to N do
  begin
    if Yin[Loop] < 0.00001 then
      Inc(NZF)
    else
      X[Loop-NZF] := Yin[Loop];
  end;

  //Determine probability of zero's
  NY := N;
  NNP := N - NZF;

  NORMALIZ(X,NNP,W,ITT,GAMMA,DELTA,XLAM,XI,IFAULT,A,S,NZF,IDEBUG,GA,DE,XL,XX,CR,ICURV);
  A1[1] := A;
  S1[1] := S;

  if ITT <= 2 then
    IType := 1
  else
    Itype := 3;

  SSORT(X,NNP);
  SSORT(W,NNP);

  ZMX := -0.00000001;

  for Loop := 1 to NNP do
  begin
    XN := Loop/(NNP+1);
    ZD[Loop] := X[loop];
    SDX[1] := XNorm(XN);
    ZP[loop] := AJV(ITYPE,ErrorOccured,SDX,GAMMA,DELTA,XLAM,XI);
    ZMX := Max(ZMX, ZD[loop]);
    ZMX := Max(ZMX, ZP[loop]);
  end;

  XA := XMean(Yin,NY);
  XSD := StDev(Yin,NY,XA);
  WA := XMean(W,NNP);
  WSD := Stdev(W,NNP,WA);

//C       NOW COMPUTE THE CHI SQUARE ON THE FLOWS
//  XX[1] := ChiSq(X,NNP,XA,XSD,M);///???????????????????
//  QCHISX(XX[1],M);

{C       NOW COMPUTE THE CHI SQUARE ON THE NORMALIZED FLOWS DROPPING
C       THE NUMBER OF DEGREES OF FREEDOM BY ONE, AS ANOTHER PARAMETER
C       HAS BEEN ESTIMATED IN THE NORMALIZING PROCESS.}
//  XX[1] := ChiSq(W,NNP,WA,WSD,M);
//  QCHISX(XX[1],M-1);

end;//PROCEDURE FitMarg

{
c------------------------------------------------------------------------------
c
        FUNCTION DMAC(p,n,m)
c
c------------------------------------------------------------------------------
c
c       function DMAC computes the MacLeod approximation to the determinant
c       term in the exact likelihood function of an ARMA model up to lag-2
c       in both AR & MA.  for AR it is exact, while for ARMA(1,1) it is
c       accurate to 3 figures by N=16. it is likely that a similar rate of
c       convergence will be experienced with ARMA(p,q) models where either
c       p or q or both are greater than 1.
c
c       to find the values of the various determinants, expressions have been
c       derived for the computation of determinants of matrices up to size 4
c       by exploiting the doubly symmetric nature of these in the special
c       case of ARMA models. these are computed in functions DET1,..,DET4.
c
c       half the negative of the log of the determinant term is returned.
c
c------------------------------------------------------------------------------
c
      IMPLICIT REAL (KIND=8) ( A-H,O-Z )
        dimension p(30)
c
        DMAC=0.0
c
        if (n.eq.0.and.m.eq.0) then
        return
        endif
c
        if (n.eq.0.and.m.eq.1.or.n.eq.1.and.m.eq.0) then
        p1=p(1)
        DMAC=-0.5*log(abs(DET1(p1)))
        return
        endif
c
        if (n.eq.0.and.m.eq.2.or.n.eq.2.and.m.eq.0) then
        p1=p(1)
        p2=p(2)
        DMAC=-0.5*log(abs(DET2(p1,p2)))
        return
        endif
c
        if (n.eq.1.and.m.eq.1) then
        p1=p(1)
        p2=p(2)
        U2 = (DET1(p2)*DET1(p1))**2
        D2 = DET2(p1,p2)
        IF (ABS(D2) .LT. 1.0D-300) THEN
          DMAC = 0.0
          RETURN
        ENDIF
        DMAC=-0.5*log(abs(U2/D2))
        return
        endif
c
        if (n.eq.1.and.m.eq.2) then
        p1=p(1)
        p2=p(2)
        p3=p(3)
        U3 = (DET2(p2,p3)*DET1(p1))**2
        D3 = DET3(p1,p2,p3)
        IF (ABS(D3) .LT. 1.0D-300) THEN
          DMAC = 0.0
          RETURN
        ENDIF
        DMAC=-0.5*log(abs(U3/D3))
        return
        endif
c
        if (n.eq.2.and.m.eq.1) then
        p1=p(1)
        p2=p(2)
        p3=p(3)
        U3 = (DET2(p1,p2)*DET1(p3))**2
        D3 = DET3(p1,p2,p3)
        IF (ABS(D3) .LT. 1.0D-300) THEN
          DMAC = 0.0
          RETURN
        ENDIF
        DMAC=-0.5*log(abs(U3/D3))
        return
        endif
c
        if (n.eq.2.and.m.eq.2) then
        p1=p(1)
        p2=p(2)
        p3=p(3)
        p4=p(4)
        D4 = DET4(p1,p2,p3,p4)
        U4 = (DET2(p1,p2)*DET2(p3,p4))**2
C        WRITE (26,'(1X,A,4F8.4,2D13.4)')
C     $        'P1, P2, P3, P4, U4, D4', P1, P2, P3, P4, U4, D4
        IF (ABS(D4) .LT. 1.0D-300) THEN
          DMAC = 0.0
          RETURN
        ENDIF
        DMAC=-0.5*log(abs(U4/D4))
        return
        endif
c
        if (n.eq.1.and.m.eq.3) then
        p1=p(1)
        p2=p(2)
        p3=p(3)
        p4=p(4)
        DMAC=-0.5*log(abs((DET1(p1)*DET3(p2,p3,p4))**2/
     #   DET4(p1,p2,p3,p4)))
        return
        endif
c
        if (n.eq.3.and.m.eq.1) then
        p1=p(1)
        p2=p(2)
        p3=p(3)
        p4=p(4)
        DMAC=-0.5*log(abs((DET3(p1,p2,p3)*DET1(p4))**2/
     #   DET4(p1,p2,p3,p4)))
        return
        endif
c
        if (n.eq.4.or.m.eq.4) then
        p1=p(1)
        p2=p(2)
        p3=p(3)
        p4=p(4)
        DMAC=-0.5*log(abs(DET4(p1,p2,p3,p4)))
        return
        endif
c
c       to allow for cases when n+m > 4 (life's too short to explicitly
c       evaluate a determinant of a matrix larger than 4x4)
c
        DMAC=0.0
        return
c
c------------------------------------------------------------------------------
        end
c------------------------------------------------------------------------------
c
        FUNCTION DET1(p1)
c
c------------------------------------------------------------------------------
c
      IMPLICIT REAL (KIND=8) ( A-H,O-Z )
        DET1=1.0d0-p1*p1
        return
c
c------------------------------------------------------------------------------
        end
c------------------------------------------------------------------------------
c
        FUNCTION DET2(p1,p2)
c
c------------------------------------------------------------------------------
c
      IMPLICIT REAL (KIND=8) ( A-H,O-Z )
        f1=p1+p2
        f2=p1*p2
        a=1.0d0-f2*f2
        b=f1*(1.0d0-f2)
        DET2=a*a-b*b
        return
c
c------------------------------------------------------------------------------
        end
c------------------------------------------------------------------------------
c
        FUNCTION DET3(p1,p2,p3)
c
c------------------------------------------------------------------------------
c
      IMPLICIT REAL (KIND=8) ( A-H,O-Z )
        f1=p1+p2+p3
        f2=p1*p2+p2*p3+p3*p1
        f3=p1*p2*p3
        a=1.0d0-f3*f3
        b=f1-f2*f3
        c=f2-f1*f3
        d=a+f1*f1-f2*f2
        DET3=(a-c)*(d*(a+c)-2.0D0*b*b)
        return
c
c------------------------------------------------------------------------------
        end
c------------------------------------------------------------------------------
c
        FUNCTION DET4(p1,p2,p3,p4)
c
c------------------------------------------------------------------------------
c
      IMPLICIT REAL (KIND=8) ( A-H,O-Z )
        f1=p1+p2+p3+p4
        g1=p1*p2+p2*p3+p3*p1
        f2=g1+p4*(f1-p4)
        g2=p1*p2*p3
        f3=g2+p4*g1
        f4=p4*g2
        a=1.0d0-f4*f4
        b=f1-f3*f4
        c=f2-f2*f4
        d=f3-f1*f4
        e=a+f1*f1-f3*f3
        f=b+f1*f2-f2*f3
        DET4=((b+c)*(b-c))**2+((a+d)*(a-d))*((e+f)*(e-f))
     #   -2.0D0*(b*b+c*c)*(a*e+d*f)+4.0D0*b*c*(a*f+d*e)
        return
c
c------------------------------------------------------------------------------
        end
}

// ______________________________ MDSTATS ______________________________
(*
procedure MDSTATS(var NY,NNP,M1,M2: ArrayT1I; Y,X,W : array1000F;
                  var XA,XSD,XS3,XS4,
                      WA,WSD,WS3,WS4,
                      CL1,CL2,
                      XX1,QCH1,XX2,QCH2 : Array1F);
{C       Suboutine to calculate statistics from the fitted marginal distribution

C       Input variables:
C       ---------------
C       Y = Original natural data
C       X = Original natural data sorted and non-zero
C       W = Normalized non-zero variates
C       NY = Number of annual data points
C       NNP = Number of non-zero points
C
C       Output variables:
C       ----------------
C       XA = Mean of natural
C       XSD = Standard deviation of natural
C       XS3 = Skew of natural
C       XS4 = Excess of natural
C
C       WA = Mean of normalized data
C       WSD = Standard deviation of normalized data
C       WS3 = Skew of normalized data
C       WS4 = Excess of normalized data
C
C       CL1 = 95% confidance limite for skew (normalized)
C       CL2 = 95% confidance limite for excess (normalized)
C
C       XX1 = Chi square on the flow
C       M1  = Degrees of freedom
C       QCH1 = Exceedance probability of the XX1
C
C       XX2 = Chi square on the normalilized flow (droped degree of freedom, by one)
C       M2  = Degrees of freedom
C       QCH2 = Exceedance probability of the XX2}
var
  XN : Extended;
begin
  XA[1] := Xmean(Y,NY[1]);
  XSD[1] := stdev(Y,NY[1],XA[1]);
  XS3[1] := Skew(Y,NY[1],XA[1],XSD[1]);
  XS4[1] := Excess(Y,NY[1],XA[1],XSD[1]);

  WA[1] := XMean(W,NNP[1]);
  WSD[1] := stdev(W,NNP[1],WA[1]);
  WS3[1] := Skew(W,NNP[1],WA[1],WSD[1]);
  WS4[1] := EXCESS(W,NNP[1],WA[1],WSD[1]);

  XN := NNP[1];

  //Discrpency in CL1, CL2, QCH1 and QCH2 due to the fact that FORTRAN routine only calculates Sqrt accurate to 8 sf
  //these are more clearly known as Skew95, Excess95, ExceedanceXX1 and Exceeedance XX2

  CL1[1] := 1.96 * sqrt(6/XN);
  CL2[1] := 1.96 * sqrt(24/XN);

  XX1[1] := ChiSq(X,NNP[1],XA[1],XSD[1],M1);
  QCH1[1] := QChiSx(XX1[1],M1);

  XX2[1] := ChiSq(W,NNP[1],WA[1],WSD[1],M2);
  QCH2[1] := QChiSx(XX2[1],M2);

  M2[1] := M2[1] - 1;
end;//PROCEDURE MDStats
*)
procedure SVD(NM,M,N : integer; A : Array500_500F; W : array1000F; var MATU : Boolean; U : array500_500F; var MATV : Boolean; V : array500_500F; var IERR : Integer; RV1 : array1000F);
{C---------------------------------------------------------------------
C
      SUBROUTINE SVD(NM,M,N,A,W,MATU,U,MATV,V,IERR,RV1)
C
C                               SVD
C
C---------------------------------------------------------------------
C
        IMPLICIT REAL*8 (A-H,O-Z)
        REAL*8 MACHEP
C
C
      DIMENSION A(NM,*),U(NM,*),V(NM,*),W(1),RV1(*)
C
      LOGICAL MATU,MATV
C
C     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE SVD,
C     HANDBOOK AUTO COMP VOL II LINEAR ALGEBRA P.134-151
C
C     THIS SUBROUTINE DETERMINES THE SINGULAR VALUE DECOMPOSITION
C     A=UDV' OF A REAL M BY N MATRIX. HOUSEHOLDER
C     BIDIAGONALISATION AND A VARIANT OF THE QR ALGORITHM ARE USED
C
C
C
C  ON INPUT:
C
C  NM1   MUST BE SET TO THE ROW DIMENSION OF TWO DIMENSIONAL
C        ARRAY PARAMETER A AS DECLARED IN THE CALLING PROGRAM
C        DIMENSION STATEMENT. NOTE THAT NM1 MUST BE AT LEAST
C        AS LARGE AS M
C
C  NM2   MUST BE SET TO THE ROW DIMENSION OF TWO DIMENSIONAL
C        ARRAY PARAMETER U AS DECLARED IN THE CALLING PROGRAM
C        DIMENSION STATEMENT. NOTE THAT MN2 MUST BE AT LEAST
C        AS LARGE AS M.
C
C  NM3   MUST BE SET TO THE ROW DIMENSION OF TWO DIMENSIONAL
C        ARRAY PARAMETER V AS DECLARED IN THE CALLING PROGRAM
C        DIMENSION STATEMENT. NOTE THAT NM3 MUST BE AT LEAST
C        AS LARGE AS N
C
C  M     IS THE NUMBER OF ROWS OF A (AND U)
C
C  N     IS THE NUMBER OF COLUMNS OF A (AND U) AND THE ORDER OF V
C
C  A     IS THE INPUT MATRIX TO BE DECOMPOSED
C
C  MATU  IS SET TO .TRUE. IF THE U MATRIX IN THE
C        DECOMPOSITION IS DESIRED AND TO FALSE OTHERWISE
C
C  MATV  IS SET TO .TRUE. IF THE V MATRIX IN THE DECOMPOSITION
C        IS DESIRED AND TO FALSE OTHERWISE
C
C
C
C  ON OUTPUT:
C
C  A     IS UNALTERED (UNLESS OVERWRITTEN BY U)
C
C  W     CONTAINS THE N NON-NEGATIVE SINGULAR VALUES OF A (THE
C        DIAGONAL ELEMENTS OF D.)  IF AN ERROR EXIT IS MADE THE
C        SINGULAR VALUES SHOULD BE CORRECT FOR INDICES
C        IERR+1,IERR+2,...........,N
C
C  U     CONTAINS THE MATRIX U (ORTHOGONAL COLUMN VECTORS) OF THE
C        DECOMPOSITION IF MATU HAS BEEN SET TO .TRUE. OTHERWISE
C        U IS USED AS A TEMPORARY ARRAY. U MAY COINCIDE WITH A.
C        IF AN ERROR EXIT IS MADE, THE COLUMNS OF U CORRESPONDING TO
C        INDICES OF CORRECT SINGULAR VALUES SHOULD BE CORRECT.
C
C  V     CONTAINS THE MATRIX V (ORTHOGONAL) OF THE DECOMPOSITION IF
C        MATV HAS BEEN SET TO .TRUE. OTHERWISE V IS NOT REFERENCED.
C        V MAY ALSO COINCIDE WITH A. IF AN ERROR EXIT IS MADE
C        THE COLUMNS OF V CORRESPONDING TO INDICES OF CORRECT
C        SINGULAR VALUES SHOULD BE CORRECT.
C
C  IERR  IS SET TO
C          0 - FOR NORMAL RETURN
C          K - IF THE K-TH VALUE HAS NOT BEEN
C              DETERMINED AFTER 30 ITERATIONS
C
C  RV1   IS A TEMPORARY STORAGE ARRAY
C
C      ************************************************************}
const
  Machep = 1E-10;
var
  Loop1, Loop2 : Integer;
  Scale, X, G, S : Extended;
  L : Integer;
begin
  IErr := 0;

  for Loop1 := 1 to M do
    for Loop2 := 1 to N do
      U[Loop1,Loop2] := A[Loop1,Loop2];

  Scale := 0.0;
  X := 0.0;
  G := 0.0;
  for Loop1 := 1 to N do
  begin
    L := Loop1 + 1;
    RV1[Loop1] := Scale * G;
    G := 0.0;
    S := 0.0;
    Scale := 0.0;
    if Loop1 > M then
      //jump forward a looong way
  end;
{
      DO 300 I=1,N
        L = I + 1
        RV1(I) = SCALE*G
        G = 0.0
        S = 0.0
        SCALE = 0.0
        IF (I .GT. M) GO TO 210
        DO 37 K=I,M
          SCALE = SCALE + ABS(U(K,I))
   37   CONTINUE
        IF (SCALE .EQ. 0.0) GO TO 210
        DO 47 K=I,M
          U(K,I) = U(K,I)/SCALE
          S = S + U(K,I)**2
   47   CONTINUE
        F = U(I,I)
        XZ = SQRT(S)
        G = -SIGN (XZ,F)
        H = F*G - S
        U(I,I) = F - G
        IF (I .EQ. N) GO TO 190
        DO 57 J=L,N
          S = 0.0
          DO 67 K=I,M
            S = S + U(K,I)*U(K,J)
   67     CONTINUE
          F = S/H
          DO 77 K=I,M
            U(K,J) = U(K,J) + F*U(K,I)
   77     CONTINUE
   57   CONTINUE
 190    DO 87 K=I,M
          U(K,I) = SCALE*U(K,I)
   87   CONTINUE
 210    W(I) = SCALE*G
        SCALE = 0.0
        S = 0.0
        G = 0.0
        IF (I .GT. M .OR. I .EQ. N) GO TO 290
        DO 97 K=L,N
          SCALE = SCALE + ABS(U(I,K))
   97   CONTINUE
        IF (SCALE .EQ. 0.0) GO TO 290
        DO 107 K=L,N
          U(I,K) = U(I,K)/SCALE
          S = S + U(I,K)**2
  107   CONTINUE
        F = U(I,L)
        XZ = SQRT(S)
        G = -SIGN(XZ,F)
        H = F*G-S
        U(I,L) = F - G
        DO 117 K=L,N
          RV1(K) = U(I,K)/H
  117   CONTINUE
        IF (I .EQ. M) GO TO 270
        DO 127 J=L,M
          S = 0.0
          DO 137 K=L,N
            S = S + U(J,K)*U(I,K)
  137     CONTINUE
          DO 147 K=L,N
            U(J,K) = U(J,K) +S*RV1(K)
  147     CONTINUE
  127   CONTINUE
 270    DO 157 K=L,N
          U(I,K) = SCALE*U(I,K)
  157   CONTINUE
 290    XZ = ABS(W(I)) + ABS(RV1(I))
        X = MAX(X,XZ)
 300  CONTINUE
C
C **** ACCUMULATION OF RIGHT-HAND TRANSFORMATIONS ****
C
      IF (.NOT. MATV) GO TO 410
      DO 400 II=1,N
        I = N + 1 - II
        IF (I .EQ. N) GO TO 390
        IF (G .EQ. 0.0) GO TO 360
        DO 167 J=L,N
C
C **** DOUBLE DIVISION AVOIDS POSSIBLE UNDERFLOW *****
C
          V(J,I) = (U(I,J)/U(I,L))/G
  167   CONTINUE
        DO 177 J=L,N
          S = 0.0
          DO 187 K=L,N
            S = S + U(I,K)*V(K,J)
  187     CONTINUE
          DO 197 K=L,N
            V(K,J) = V(K,J) + S*V(K,I)
  197     CONTINUE
  177   CONTINUE
 360    DO 207 J=L,N
          V(I,J) = 0.0
          V(J,I) = 0.0
  207   CONTINUE
 390    V(I,I) = 1.0
        G = RV1(I)
        L = I
 400  CONTINUE
C
C ***  ACCUMULATION OF LEFT-HAND TRANSFORMATIONS *********
C
 410  IF ( .NOT. MATU) GO TO 510
      MN = N
      IF (M .LT. N) MN = M
      DO 500 II=1,MN
        I = MN+1-II
        L = I+1
        G = W(I)
        IF (I .EQ. N) GO TO 430
        DO 217 J=L,N
          U(I,J) = 0.0
  217   CONTINUE
 430    IF (G .EQ. 0.0 ) GO TO 475
        IF (I .EQ. MN) GO TO 460
        DO 227 J=L,N
          S = 0.0
          DO 237 K=L,M
            S = S+U(K,I)*U(K,J)
  237     CONTINUE
C
C ***** DOUBLE DIVISION AVOIDS POSSIBLE UNDERFLOW ******
C
          F = (S/U(I,I))/G
          DO 247 K=I,M
            U(K,J) = U(K,J)+F*U(K,I)
  247     CONTINUE
  227   CONTINUE
 460    DO 257 J=I,M
          U(J,I) = U(J,I)/G
  257   CONTINUE
        GO TO 490
 475    DO 267 J=I,M
          U(J,I) = 0.0
  267   CONTINUE
 490    U(I,I) = U(I,I)+1.0
 500  CONTINUE
C
C ***** DIAGONALISATION OF THE BIDIAGONAL FORM **********
C
 510  EPS = MACHEP*X
C
C ***** FOR K=N STEP -1 UNTIL 1 DO **********
C
      DO 700 KK=1,N
        K1 = N-KK
        K = K1+1
        ITS = 0
C
C ****** TEST FOR SPLITTING ******
C        FOR L=K STEP -1 UNTIL 1 DO
C
 520    DO 277 LL=1,K
          L1 = K -LL
          L = L1+1
          IF (ABS(RV1(L)) .LE. EPS) GO TO 565
C
C         RV1(I) IS ALWAYS ZERO, SO THERE IS NO EXIT
C         THROUGH THE BOTTOM OF THE LOOP
C
          IF (ABS(W(L1)) .LE. EPS) GO TO 540
  277   CONTINUE
 540    C = 0.0
        S = 1.0
        DO 560 I=L,K
          F = S*RV1(I)
          RV1(I) = C*RV1(I)
          IF (ABS(F) .LE. EPS) GO TO 565
          G = W(I)
          H = SQRT(F*F+G*G)
          W(I) = H
          C = G/H
          S = -F/H
          IF (.NOT.MATU) GOTO 560
            DO 550 J=1,M
              Y = U(J,L1)
              Z = U(J,I)
              U(J,L1) = Y*C+Z*S
              U(J,I) = -Y*S+Z*C
 550        CONTINUE
 560    CONTINUE
C
C       TEST FOR CONVERGENCE
C
 565    Z = W(K)
        IF (L .EQ. K) GO TO 650
C
C       SHIFT FROM BOTTOM 2 BY 2 MINOR
C
        IF (ITS .EQ. 30) GO TO 1000
        ITS = ITS+1
        X = W(L)
        Y = W(K1)
        G = RV1(K1)
        H = RV1(K)
        F = ((Y-Z)*(Y+Z)+(G-H)*(G+H))/(2.0*H*Y)
        G = SQRT(F*F+1.0)
        F = ((X-Z)*(X+Z)+H*(Y/(F+SIGN(G,F))-H))/X
C
C       NEXT QR TRANSFORMATION
C
        C = 1.0
        S = 1.0
C
        DO 600 I1=L,K1
          I = I1+1
          G = RV1(I)
          Y = W(I)
          H = S*G
          G = C*G
          Z = SQRT(F*F+H*H)
          RV1(I1) = Z
          C = F/Z
          S = H/Z
          F = X*C+G*S
          G = -X*S+G*C
          H = Y*S
          Y = Y*C
          IF ( .NOT. MATV) GO TO 575
C
          DO 287 J=1,N
            X = V(J,I1)
            Z = V(J,I)
            V(J,I1) = X*C+Z*S
            V(J,I) = -X*S+Z*C
  287     CONTINUE
C
 575      Z = SQRT(F*F+H*H)
          W(I1) = Z
C
C         ROTATION CAN BE ARBITRARY IF Z IS ZERO
C
          IF (Z .EQ. 0.0) GO TO 580
          C = F/Z
          S = H/Z
 580      F = C*G+S*Y
          X = -S*G+C*Y
          IF ( .NOT.MATU ) GOTO 600
            DO 297 J=1,M
              Y = U(J,I1)
              Z = U(J,I)
              U(J,I1) = Y*C+Z*S
              U(J,I) = -Y*S+Z*C
  297       CONTINUE
 600    CONTINUE
        RV1(L) = 0.0
        RV1(K) = F
        W(K) = X
        GO TO 520
C
C         CONVERGENCE
C
 650    IF (Z .GE. 0.0) GOTO 700
C
C           W(K) IS MADE NON-NEGATIVE
C
          W(K) = -Z
          IF (.NOT.MATV) GOTO 700
C
            DO 307 J=1,N
              V(J,K) = -V(J,K)
  307       CONTINUE
 700    CONTINUE
C**************************************************************
C     TO FIND THE ZERO CUT OFF FOR SINGULAR VALUES
C**************************************************************
C
C     DUPLICATE SINGULAR VALUES
C
      DO 317 I=1,N
        RV1(I) = W(I)
  317 CONTINUE
C
C     ORDER SINGULAR VALUES
C
      NEND = N-1
 702  LAST = 0
      DO 703 I=1,NEND
        IF (RV1(I+1) .GT. RV1(I)) GOTO 703
          LAST = I
          TEMP = RV1(I+1)
          RV1(I+1) = RV1(I)
          RV1(I) = TEMP
 703  CONTINUE
C
      NEND = LAST-1
      IF (NEND .GE. 1) GO TO 702
      NEND = N-1
      ITEST = 1
      EPS = SQRT(MACHEP)
      DO 327 I=1,NEND
        IF (ABS(RV1(I)).LT.EPS) GOTO 704
        TEST = RV1(I+1)/RV1(I)
        IF (TEST .LT. EPS .AND. RV1(I+1) .LT. EPS) GO TO 705
 704    ITEST = I+1
  327 CONTINUE
 705  IF(ITEST .EQ. N) GO TO 1001
      ZERO = (RV1(I+1)+RV1(I))/2.
C
C     REPLACE ZERO FOR ZERO VALUES IN W
C
      DO 337 I=1,N
        IF (W(I) .LT. ZERO) W(I) = 0.
  337 CONTINUE
      GO TO 1001
C
C ************  SET ERROR  --  NO CONVERGENCE TO A SINGULAR VALUE
C                               AFTER 30 ITERATIONS  *************************
C
 1000 IERR = K
 1001 RETURN
C-------------------------------------------------------------------------------
      END}
end;

procedure Sort2(N : Integer; var RA, RB : Array of Double);
var
  RRA, RRB  : Double;
  IR, L, I, J : Integer;
begin
  L := Round(N / 2 + 1);
  IR := N;

  while (1 = 1) do
  begin
    if (L > 1) then
    begin
      L := L - 1;
      RRA := RA[L];
      RRB := RB[L]
    end//if (L
    else
    begin
      RRA := RA[IR];
      RRB := RB[IR];
      RA[IR] := RB[1];
      RB[IR] := RB[1];
      IR := IR - 1;
      if (IR = 1) then
      begin
        RA[1] := RRA;
        RB[1] := RRB;
        exit;
      end;//if (IR
    end;

    I := L;
    J := L + L;
    while (J <= IR) do
    begin
      if (J < IR) then
      begin
        if (RA[J] < RA[J+1]) then
          J := J + 1;
      end;//if (J <
      if (RRA < RA[J]) then
      begin
        RA[I] := RA[J];
        RB[I] := RB[J];
        I := J;
        J := J + 1;
      end//if (RRA
      else
      begin
        J := IR + 1;
      end;
    end;//while (J <=
    RA[I] := RRA;
    RB[I] := RRB;
  end;//while (1 = 1) do
end;

procedure Crank(N : Integer; var W : Array of Double; var S : Double);
var
  J, JT, JI : Integer;
  LoopExit : Boolean;
  Rank, T : double;
begin
  S := 0;
  J := 1;
  while (J < N) do
  begin
    if (W[J+1] <> W[J]) then
    begin
      W[J] := J;
      J := J + 1;
    end//if (W[J+1]
    else
    begin
      LoopExit := False;
      for JT := J + 1 to N do
      begin
        if (W[JT] <> W[J]) then
        begin
          LoopExit := true;
          break;
        end;//if (W[JT]
      end;//for JT
      if (LoopExit = False) then
        JT := N + 1;
      Rank := 0.5 * (J + JT - 1);
      for JI := J to (JT - 1) do
        W[JI] := Rank;
      T := JT - J;
      S := S + power(T,3) - T;
      J := JT;
    end;//else
  end;//while (J < N)

  if (J = N) then
    W[N] := N;
end;

procedure Spear2(var Data1, Data2 : Array of Double; var N : integer; var RS : Double);
var
  J, EN : Integer;
  WKSP1, WKSP2 : Array[1..100] of double;
  D, EN3N, Fac, SF, SG : double;
begin
  for J := 1 to N do
  begin
    WKSP1[J] := Data1[J];
    WKSP2[J] := Data2[J];
  end;
  Sort2(N,WKSP1,WKSP2);
  Crank(N,WKSP1,SF);
  Sort2(N,WKSP2,WKSP1);
  Crank(N,WKSP2,SG);
  D := 0;
  for J := 1 to N do
    D := D + Power((WKSP1[J] - WKSP2[J]),2);
  EN := N;
  EN3N := Power(EN,3) - EN;
  Fac := (1 - SF/EN3N) * (1 - SG/EN3N);
{C                                                                       !OCT95
C     CORRECTED CALCULATION OF RS - NUMERICAL RECIPES WAS WRONG         !OCT95
C     # RS=(1.-(6./EN3N)*(D+0.5*(SF+SG)))/FAC #                         !OCT95
C                                                                       !OCT95}
  If (ABS(Fac) > 1E-20) then
    RS := (1 - (0.5/EN3N) * (12 * D + SF + SG))/Sqrt(Fac)
  else if (D < 1E-20) then
    RS := 1
  else if (ABS(1 - SF/EN3N) < 1E-20) or (ABS(1 - SG/EN3N) < 1E-20) then
    RS := 0;
end;

// ______________________________ CROSS ______________________________
procedure CROSS(var NG: integer; N, IYS : array500I;
                    Phi1, Phi2, ThT1, ThT2 : array500F;
                    Y : array500_1000F;
                    S0 : array500_500F;
                    EG0, EH1, EH0 : array500F;
                    B, B0, B1, A, C, BB, D : array500_500F);
{C-------------------------------------------------------------------------------
C
C
C    Variables input from Delphi to CROSS
C    NG         - Number of records to process [NOT BIGGER THATN NM]
C    N(1000)    - Number of years in data record.
C    ISTRT(500) - Start year of the data record > 1920
C    FI1(500)  - FI(first) variable for all data records
C    FI2(500)  - FI(second) variable for all data records
C    TH1(500)  - Theta(first) variable for all data records
C    TH2(500)  - Theta(second) variable for all data records
C    YD(1000,500) - Yearly normalized flows for each record -  - transformed and sorted.
C
C    Variables from Cross to Delphi
C    SO(500,500) - The lag zero dispersion matrix [ANS file]
C    Eg0(500)      - The Eigenvalues of G0 [ANS file]
C    Eh1(500)      - The Eigenvalues of H1 [ANS file]
C    Eh0(500)      - The Eigenvalues of H0 [ANS file]
C    B(500,500)    - The square-root of lag-zero dispersion matrix G0 [PARAM file]
C    B0(500,500)   - The square-root of lag-zero starting matrix H0 [PARAM file]
C    B1(500,500)   - The square-root of lag-one starting matrix H1 [PARAM file]
C    A(500,500)    - The coefficient matrix of Z(-1) calculationg Z(0) [PARAM file]
C    C(500,500)    - The coefficient matrix of A(-1) calculationg Z(0) [PARAM file]
C    BB(500,500)   - BB" - as a check
C    D(500,500)    - The matrix of differences
C

C    Recalculate the following in delphi
C    XY1(1000) -
C    XY2(1000) -


      IMPLICIT REAL (KIND=8) ( A-H,O-Z )
      dll_export CROSS

C
C       2ND REVISION WITH CORRECT S1
C       3RD REVISION WITH NEGATIVE EIGENVALUES ARISING FROM THE SVD SET TO ZERO
C       4TH REVISION WITH SMALL EIGENVALUES (<RHO/10000) OF H1 SET TO ZERO
C                                                               30-01-94
C
C       MODIFY OUTPUT A BIT TO TIDY IT          GGSP            07-08-94
C       READ AND WRITE AAM1 AND ZM1             PVR             08-03-95
C       5TH REVISION - CROSS-CORRELATION BY SPEARMAN AND MATCH ANLMK5
C                                               GGSP            10-02-96
C             --------------------------------------------               !OCT95
C                                                                        !OCT95
C                             A NEW APPROACH                             !OCT95
C                                                                        !OCT95
C     INTRODUCE THE SPEARMAN RANK CORRELATION CALCULATOR WHICH ALLOWS    !OCT95
C     FOR TIED RANKS - PARTICULARLY FOR EPHEMERAL STREAMFLOW - THIS AS   !OCT95
C     A SUBSTITUTE FOR THE PEARSON PRODUCT MOMENT CALCULATOR USED IN     !OCT95
C     PREVIOUS VERSIONS PRIOR TO MARK3                                   !OCT95
C                                                                        !OCT95
C     THE RESULTS ARE VERY CLOSE FOR MARGINAL DISTRIBUTIONS WHICH HAVE   !OCT95
C     BEEN TRANSFORMED TO NORMAL AS SRC IS A NON-PARAMETRIC ESTIMATOR.   !OCT95
C                                                                        !OCT95
C     THIS ALTERATION IS PARTICULALY FOR THE LETABA IN THE FIRST INSTANCE!OCT95
C     BUT ALLOWS A MORE GENERAL APPROACH TO MULTIVATIATE TIME SERIES     !OCT95
C     MODELLING OF ARID REGION HYDROLOGY.  IT DOES NOT BLOCK THE MIXED   !OCT95
C     BOOT-STRAP APPROARCH AND DOES NOT ALLOW MAVERICK GAUGES TO POLLUTE !OCT95
C     THE CROSS-CORRELATION MATRIX. THOSE TIME SERIES WHICH CAN BE       !OCT95
C     TREATED IN THE USUAL ARMA(P,Q) WAY ARE UNAFFECTED BY THIS CHANGE.  !OCT95
C     THE *.YER FILES ARE SENT THE ORIGINAL DATA BY ANLMK5.              !OCT95
C                                                                       !FEB96
C     IT IS THESE WHICH ARE USED TO CALCULATE THE CROSS-CORRELATIONS    !FEB96
C                                                                        !OCT95
C                                                       GGSP - 23-10-95  !OCT95
C                                                                        !OCT95
C       THERE HAVE BEEN SOME VARIABLE CHANGES.  XYEAR AND X HAVE BEEN   !FEB96
C       REPLACED BY YYEAR AND Y AND THE FORMER PAIR REMOVED.            !FEB96
C                                                                       !FEB96
C       THIS FEBRUARY 1996 EXTENSION IS TO CONFIRM THAT THE SPEARMAN    !FEB96
C       CROSS-CORRELATION IS THE WAY TO GO AND TO ADAPT THE ROUTINE     !FEB96
C       TO ACCEPT THE CHANGES MADE TO THE .YER FILES PRODUCED BY ANLMK5 !FEB96
C       - PARTICULARLY THE THE PROPORTION OF ZEROS USED TO MODEL THE    !FEB96
C       EPHEMERAL STREAMS.                                              !FEB96
C                                                                       !FEB96
C                                                       GGSP - 10-02-96 !FEB96
C                                                                       !FEB96
C-------------------------------------------------------------------------------}

{CC        IMPLICIT REAL*8 (A-H,O-Z)
        PARAMETER (NM=200)
C
C       Variables to declare from Delphi to CROSS
        DIMENSION N(1000),ISTRT(500),FI1(500),FI2(500),TH1(500)
        DIMENSION TH2(500),YD(1000,500)

C       Variables to declare from CROSS to Delphi
        DIMENSION S0D(500,500),EG0D(500),EH1D(500),EH0D(500),BD(500,500)
        DIMENSION B0D(500,500),B1D(500,500),AD(500,500),CD(500,500)
        DIMENSION BBD(500,500),DD(500,500)

        DIMENSION PHI(2,NM),THT(2,NM),FI(2),TH(2)                       !FEB96
        DIMENSION YYEAR(100), Y(200,NM)                                 !OCT95
        DIMENSION W(NM), RV1(NM), NY(NM), IYS(NM)
        DIMENSION G0(NM,NM), H0(NM,NM), H1(NM,NM)
        DIMENSION G1(NM,NM), B0(NM,NM), B1(NM,NM)
C
        DIMENSION A(NM,NM),B(NM,NM),C(NM,NM)
        DIMENSION S0(NM,NM),S1(NM,NM)                                   !FEB96
        DIMENSION XY1(100),XY2(100)                                     !FEB96
        DIMENSION U(NM,NM), V(NM,NM)
        DIMENSION G(NM,NM),H(NM,NM)
C
        LOGICAL MATU,MATV
        LOGICAL EXST
C
        CHARACTER   IN15(NM)*25,INTU*25,STRING*60
        CHARACTER   HEADER*25
        CHARACTER   DIR*40, INFILE*40
        CHARACTER*1 ANSP,IFILE
C
        COMMON /DIRECT/ DIR
C
        ZER = 0.0D0
        ONE = 1.0D0
        TWO = 2.0D0
        MATU = .TRUE.
        MATV = .TRUE.
        TOL = 1.0D-10
        FACT = 1.0D4}

Const
  NM = 200;        
var
  NY : Array1000I;
  XY1, XY2, RV1 : array1000F;
  Loop1, Loop2, Loop3, NN, M, NS : integer;
  M1, M2, NXY, Sign, IErr : Integer;
  RS : double;
  DTemp, Sum, Tweak, WMax : Extended;
  MatU, MatV : Boolean;
  W : array1000F;
  G, G0, G1,
  U, V,
  H, H0, H1,
  S1 : Array500_500F;
begin
  NS := NG;

  NN := 100;
  M := NS;
  For Loop1 := 1 to NS do
    NY[Loop1] := N[500];

  for Loop1 := 1 to NS do
  begin
    for Loop2 := 1 to NS do
    begin
      M1 := Max(IYS[Loop1],IYS[Loop2]);
      M2 := Min(IYS[Loop1] + NY[Loop1] - 1,IYS[Loop2] + NY[Loop2] - 1);
      NXY := M2 - M1 + 1;
      for Loop3 := M1 to M2 do
      begin
        XY1[Loop3 - M1 + 1] := Y[Loop3,Loop1];
        XY2[Loop3 - M1 + 1] := Y[Loop3,Loop2];
      end;//for Loop3
        Spear2(XY1,XY2,NXY,RS);
        S0[Loop1,Loop2] := RS;
    end;//for Loop2
  end;//for Loop1

  for Loop1 := 1 to NS do
  begin
    for Loop2 := 1 to NS do
    begin
      DTemp :=  1 + ThT1[Loop1] * ThT1[Loop2] + ThT2[Loop1] * ThT2[Loop2] -
                PHI1[Loop1] * ThT1[Loop2] - ThT1[Loop1] * PHI1[Loop2] - PHI2[Loop1] * ThT2[Loop2] -
                ThT2[Loop1] * PHI2[Loop2] - ThT2[Loop1] * (PHI1[Loop2] - ThT1[Loop2]) * PHI1[Loop2] -
                PHI1[Loop1] * (PHI1[Loop1] - ThT1[Loop1]) * THT2[Loop2] -
                ((ThT1[Loop1] + THT2[Loop1] * (PHI1[Loop2] - THT1[Loop2])) *
                (PHI2[Loop1] * PHI2[Loop2] * PHI1[Loop2] + PHI2[Loop2] * PHI1[Loop1]) +
                (THT1[Loop2] + THT2[Loop2] * (PHI1[Loop1] - THT1[Loop1])) *
                (PHI2[Loop2] * PHI2[Loop1] * PHI1[Loop1] + PHI2[Loop1] * PHI1[Loop2])) /
                (1 - PHI2[Loop1] * PHI2[Loop2]);
       G0[Loop1,Loop2] := S0[Loop1,Loop2] *
                          (1 - Phi1[Loop1] * Phi1[Loop2] - Phi2[Loop1] * Phi2[Loop2] -
                          (2 * Phi1[Loop1] * Phi1[Loop2] * Phi2[Loop1] * Phi2[Loop2] +
                          Phi1[Loop1] * Phi1[Loop1] * Phi2[Loop2] +
                          Phi1[Loop2] * Phi1[Loop2] * Phi2[Loop1]) / (1 - Phi2[Loop1] * Phi2[Loop2])) / DTemp;
    end;//for Loop2

  end;//for Loop1

  SVD(NM,M,M,G0,W,MATU,U,MATV,V,IERR,RV1);

  for Loop2 := 1 to NS do
  begin
    Sign := 1;
    for Loop1 := 1 to NS do
    begin
      B[Loop1,Loop2] := 0.0;
      if ABS(U[Loop1,Loop2]) < 1E-10 then
        Break;
      if (U[Loop1,Loop2] * V[Loop1,Loop2] < 1E-10) then
        Sign := -1;
      if (W[Loop2] < 1E-10) then
        break;
      B[Loop1,Loop2] := (U[Loop1,Loop2] + V[Loop1,Loop2]) * Sqrt(W[Loop2]) / 2;
    end;//for Loop1
    W[Loop2] := W[Loop2] * Sign;
  end;//for Loop2

  for Loop2 := 1 to NS do
    EG0[Loop2] := W[Loop2];

  for Loop1 := 1 to NS do
  begin
    Sum := 0.0;
    for Loop2 := 1 to NS do
      Sum := Sum + Sqr(B[Loop1,Loop2]);
    Tweak := Sqrt(G0[Loop1,Loop1]/Sum);
    for Loop2 := 1 to NS do
      B[Loop1,Loop2] := B[Loop1,Loop2] * Tweak;
  end;//for Loop1

  for Loop1 := 1 to NS do
  begin
    for Loop2 := 1 to NS do
    begin
      H0[Loop1,Loop2] := S0[Loop1,Loop2] - G0[Loop1,Loop2];
      H1[Loop1,Loop2] := S0[Loop1,Loop2] - G0[Loop1,Loop2];
      G1[Loop2,Loop1] := (Phi1[Loop1] - Tht1[Loop1]) * G0[Loop1,Loop2];
      S1[Loop1,Loop2] := ( PHI2[Loop2] *
                         ( PHI1[Loop1] * S0[Loop1,Loop2] - THT1[Loop1] * G0[Loop1,Loop2] -
                         THT2[Loop1] * G0[Loop1,Loop2] * (PHI1[Loop2] - THT1[Loop2]))  +
                         PHI1[Loop2] * S0[Loop2,Loop1] - THT1[Loop2] * G0[Loop2,Loop1] -
                         THT2[Loop2] * G0[Loop2,Loop1] * (PHI1[Loop1] - THT1[Loop1]) ) /
                         (1 - PHI2[Loop1] * PHI2[Loop2]);
    end;//for Loop2
  end;//for Loop1

  SVD(NM,M,M,H1,W,MATU,U,MATV,V,IERR,RV1);
  WMax := XMAX(W,M);

  for Loop2 := 1 to NS do
  begin
    Sign := 1;
    for Loop1 := 1 to NS do
    begin
      B1[Loop1,Loop2] := 0.0;
      H[Loop1,Loop2] := 0.0;
      if (ABS(U[Loop1,Loop2]) < 1E-10) then
        break;
      if (U[Loop1,Loop2] * V[Loop1,Loop2]) < 1E-10 then
        Sign := -1;
      if (W[Loop2] < 1E-10) then
        Break;
      B1[Loop1,Loop2] := (U[Loop1,Loop2] + V[Loop1,Loop2]) * Sqrt(W[Loop2]) / 2;
      if (ABS(W[Loop2]) < WMax/1E4) then
        break;
      H[Loop1,Loop2] := U[Loop1,Loop2] / W[Loop2];
    end;//for Loop1
    W[Loop2] := W[Loop2] * Sign;
  end;//for Loop2

  for Loop2 := 1 to NS do
    EH1[Loop2] := W[Loop2];

  for Loop1 := 1 to NS do
  begin
    for Loop2 := 1 to NS do
    begin
      G[Loop1,Loop2] := 0.0;
      for Loop3 := 1 to NS do
        G[Loop1,Loop2] := G[Loop1,Loop2] + H[Loop1,Loop3] * V [Loop2,Loop3];
    end;//for Loop2
  end;//for Loop1

  for Loop1 := 1 to NS do
  begin
    for Loop2 := 1 to NS do
    begin
      Sum := 0.0;
      for Loop3 := 1 to NS do
      begin
        Sum := Sum + (G1[Loop3,Loop1] - S1[Loop3,Loop1]) * G[Loop3,Loop2];
      end;//for Loop3
      A[Loop1,Loop2] := Sum;
    end;//for Loop2;
  end;//for Loop1

  for Loop1 := 1 to NS do
  begin
    for Loop2 := 1 to NS do
    begin
      C[Loop1,Loop2] := A[Loop1,Loop2];
      if Loop1 = Loop2 then
        C[Loop1,Loop2] := A[Loop1,Loop2] + Phi1[Loop1] - Tht1[Loop1];
    end;//for Loop2
  end;//for Loop1

  for Loop1 := 1 to NS do
  begin
    for Loop2 := 1 to NS do
    begin
      Sum := 0.0;
      for Loop3 := 1 to NS do
        Sum := Sum + H1[Loop1,Loop3] * A[Loop2,Loop3];
      U[Loop1,Loop2] := Sum;
      V[Loop1,Loop2] := (Phi1[Loop1] - Tht1[Loop1]) * G1[Loop1,Loop2];
    end;//for Loop2
  end;//for Loop1

  for Loop1 := 1 to NS do
  begin
    for Loop2 := 1 to NS do
    begin
      Sum := 0.0;
      for Loop3 := 1 to NS do
        Sum := Sum + A[Loop1,Loop3] * U[Loop3,Loop2];
      H0[Loop1,Loop2] := H1[Loop1,Loop2] - Sum - V[Loop1,Loop2];
    end;//for Loop2
  end;//for Loop1

  SVD(NM,M,M,H0,W,MATU,U,MATV,V,IERR,RV1);

  for Loop2 := 1 to NS do
  begin
    Sign := 1;
    for Loop1 := 1 to NS do
    begin
      B0[Loop1,Loop2] := 0.0;

      if (ABS(U[Loop1,Loop2]) < 1E-10) then
        break;
      if (U[Loop1,Loop2] * V[Loop1,Loop2]) < 1E-10 then
        Sign := -1;
      if (W[Loop2] < 1E-10) then
        Break;
      B0[Loop1,Loop2] := (U[Loop1,Loop2] + V[Loop1,Loop2]) * Sqrt(W[Loop2]) / 2;

    end;//for Loop1
    W[Loop2] := W[Loop2] * Sign;
  end;//for Loop2

  for Loop2 := 1 to NS do
    EH0[Loop2] := W[Loop2];

  for Loop1 := 1 to NS do
  begin
    for Loop2 := 1 to NS do
    begin
      sum := 0.0;
      for Loop3 := 1 to NS do
      begin
        if (ABS(B[Loop1,Loop3]) < 1E-10) or (ABS(B[Loop2,Loop3]) < 1E-10) then
          break;
        Sum := Sum + B[Loop1,Loop3] * B[Loop2,Loop3];
      end;
      A[Loop1,Loop2] := Sum;
    end;//for Loop2
  end;//for Loop1

  for Loop1 := 1 to NS do
  begin
    for Loop2 := 1 to NS do
    begin
      BB[Loop1,Loop2] := A[Loop1,Loop2];
      A[Loop1,Loop2] := G0[Loop1,Loop2] - A[Loop1,Loop2];
    end;//for Loop2
  end;//for Loop1

  for Loop1 := 1 to NS do
  begin
    for Loop2 := 1 to NS do
    begin
      D[Loop1,Loop2] := A[Loop1,Loop2];
    end;//for Loop2
  end;//for Loop1

end;//PROCEDURE Cross

procedure MThrnk(NSYRI : arrayT1I; NYRS, ISTRY:arrayT2I;
                 XH1, XH2:array13_100F;
                 FLOWD1, FLOWD2:array101_100_13F;
                 var RSHO, RMEANO:array13F;
                 var PO:array7_13F);
{C     Input variables from Delphi
C     ===========================
C     NYRS(2) - Number of years of historical data for two gauges to be correlated
C             NB. the number of generated sequences should be at least equal to NR !!!!
C     ISTRY(2) - Start years of historical data for two gauges
C     NSYRI(1)     - Number of years of generated flows
C     XH1(MNY,13) - Historical data for gauge 1, the 13th value is the annual total
C     XH2(MNY,13) - Historical data for gauge 2, the 13th value is the annual total
C     FLOWD1(13,MNY,NR) - Stochastically generated flows for gauge 1 - from FLOWGEN
C                         , the 13th value is the annual total
C     FLOWD2(13,MNY,NR) - Stochastically generated flows for gauge 1 - from FLOWGEN
C                         , the 13th value is the annual total
C
C     Output variables to Delphi
C     ==========================
C     RSHO(13)   - Historical correlation coeficients, 1-12 monthly, 13 annual.
C     RMEANO(13) - Mean of simulated, 1-12 monthly, 13 annual.
C     PO(13,7)   - Distribution of correlation coeficients of simulated flows,
C                  1-12 monthly, 13 annual.}
Const NR  = 101;
      NGM = 200;
      MNY = 100;
      NGG =  15;
{
C      COMMON /FLOW/ FLOW(12,MNY,NR,NGG),AFS(MNY,NR,NGG)
C      COMMON /HIST/ XS(MNY,12,NGG),AXS(MNY,NGG)
C      COMMON /PARM/ PIFIL(NGM),NFG,NYBASE,PARMFN
C      CHARACTER*40 PIFIL,PARMFN
C      COMMON /ANDATA/ IYR,ANFLAG,NSQYR,NYRS(NGM),ISTRY(NGM)             !DEC94
C        INTEGER ANFLAG                                                  !DEC94

      DIMENSION NJB(NGG),MONTH(12)                                      !JUN95
      DIMENSION FQ1(MNY),FQ2(MNY),RSS(NR),P(7)                          !JUN95
      DIMENSION PO(13,7),RMEANO(13),RSHO(13)
      CHARACTER MONTH*10

      REAL(KIND=8) FQ1,FQ2,RSS,P,PO,RMEANO,RSHO
C
C
      DATA MONTH / 'OCTOBER   ','NOVEMBER  ','DECEMBER  ','JANUARY   ',
     1             'FEBRUARY  ','MARCH     ','APRIL     ','MAY       ',
     2             'JUNE      ','JULY      ','AUGUST    ','SEPTEMBER ' /}
var
  FQ1, FQ2 : Array[1..MNY] of double;
  RSS : array101F;
  P : Array7F;
  RMean, RSTD, RSH : Double;
  MM, MQ, NSYR, M1, M2, NXY, IB1, IB2 : Integer;
  MTH, IY, IT, I : Integer;

  procedure Box(var X : array101F; var P : Array7F);
  begin
    P[1] := X[1];
    P[7] := X[NR];
    P[2] := X[Round( (NR + 1) / 10 / 2)];
    P[6] := X[NR - Round((NR + 1) / 10 / 2)];
    P[3] := X[MM - MQ];
    P[5] := X[MM + MQ];
    P[4] := X[MM];
  end;//PROCEDURE Box

  procedure Momen1(var X : array101F; var EVX , SDX : Double);
   {C-----------------------------------------------------------------------
    C
    C
            SUBROUTINE MOMEN1( X, N, EVX, SDX )
    C
    C       MEAN AND STANDARD DEVIATION OF X(I),I=1,...,N
    C
    C-----------------------------------------------------------------------}
  var
    I : Integer;
    SumX, SumXX, XN : Double;
  begin
    SumX := 0;
    SumXX := 0;
    XN := NR;

    for I := 1 to NR do
      SumX := SumX + X[I];
    EVX := SumX / XN;

    for I := 1 to NR do
      SumXX := SumXX + power((X[I] - EVX),2);

    if SumXX < 1E-12 then
      SDX := 0.0
    else
      SDX := Sqrt(SumXX/XN);
  end;//PROCEDURE Momen1

begin
  NSYR := NSYRI[1];

  MM := (NR + 1) DIV 2;
  MQ := ( MM + 1 ) DIV 2;

  M1 := Max(ISTRY[1],ISTRY[2]);
  M2 := Min(ISTRY[2] + NYRS[2] - 1,ISTRY[1] + NYRS[1] - 1);

  NXY := M2 - M1 + 1;
  IB1 := M1 - ISTRY[1];
  IB2 := M1 - ISTRY[2];

  for MTH := 1 to 12 do
  begin
    for IY := 1 to NXY do
    begin
      FQ1[IY] := XH1[MTH,IB1 + IY];
      FQ2[IY] := XH2[MTH,IB2 + IY];
    end;//for IY
    Spear2(FQ1, FQ2, NXY, RSH);

    for IT := 1 to NR do
    begin
      for IY := 1 to NSYR do
      begin
        FQ1[IY] := FLOWD1[IT,IY,MTH];
        FQ2[IY] := FLOWD2[IT,IY,MTH];
      end;//for IY
      Spear2(FQ1, FQ2, NSYR, RSS[IT]);
    end;//for IT

    SSort(RSS,NR);
    Box(RSS,P);
    Momen1(RSS,RMEAN,RSTD);

    RMeanO[MTH] := RMean;
    RSHO[MTH] := RSH;
    for I := 1 to 7 do
      PO[I,MTH] := P[I];
  end;//for MTH

  for IY := 1 to NXY do
  begin
    FQ1[IY] := XH1[13,IB1 + IY];
    FQ2[IY] := XH2[13,IB2 + IY];
  end;//for IY
  Spear2(FQ1, FQ2, NXY, RSH);

  for IT := 1 to NR do
  begin
    for IY := 1 to NSYR do
    begin
      FQ1[IY] := FLOWD1[IT,IY,13];
      FQ2[IY] := FLOWD2[IT,IY,13];
    end;
    Spear2(FQ1, FQ2, NSYR, RSS[IT]);
  end;//for IT

  SSort(RSS,NR);
  Box(RSS,P);
  MOMEN1(RSS,RMEAN,RSTD);

  RSHO[13] := RSH;
  RMeanO[13] := RMean;
  for I := 1 to 7 do
    PO[I,13] := P[I];
end;//PROCEDURE MThrnk

{$HINTS ON}
{$WARNINGS ON}

end.
