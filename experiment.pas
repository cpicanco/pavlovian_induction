unit experiment;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms, Graphics, ExtCtrls;

procedure DebugView;
procedure DoMouseConditionA;
procedure DoMouseConditionB;

procedure CentralizeCursorPos;
procedure Move(AImage : TImage; ACenter: TPoint);
function InCircle(ACenter, APoint : TPoint): Boolean;

procedure ResizeStimuli(ACoin, AClow : TImage);
procedure StartExperiment(ABackGround : TCustomControl; ATimer : TTimer;
  ATimerNextObservation: TNotifyEvent;
  ATimerFinishTest : TNotifyEvent;
  ATimerInterval : integer;
  ADiameter : integer;
  AParticipantName : string);
procedure NextScreen;
function ScreenAsString : string;

procedure InitializeCondition(ACondition : string);


type TExperimentalScreen = (Test, BlackBackground, StimuliAtScreenLeft,  StimuliAtScreenRight);
var CoinPoint : TPoint;
var ClowPoint : TPoint;
var CurrentScreen : TExperimentalScreen;
var ExperimentalCondition : string;

implementation

uses TabDelimitedReport.Custom;

var Screens : array of TExperimentalScreen;
var ScreenIndex : integer = -1;
var Background : TCustomControl = nil;
var Timer : TTimer = nil;
var TimerNextObservation : TNotifyEvent = nil;
var TimerFinishTest : TNotifyEvent = nil;
var Diameter : integer = 1;

function ConditionACoinPoint : TPoint;
begin
  Result := Point((Background.Width*1) div 3, Background.Height div 2);
end;

function ConditionAClowPoint : TPoint;
begin
  Result := ConditionACoinPoint;
  Result.x := Result.x div 2;
end;

function ConditionBCoinPoint : TPoint;
begin
  Result := Point((Background.Width*2) div 3, Background.Height div 2);
end;

function ConditionBClowPoint : TPoint;
begin
  Result := ConditionBCoinPoint;
  Result.x := Result.x + ConditionAClowPoint.x;
end;

function CentralPoint : TPoint;
begin
  Result := Point(Background.Width div 2, Background.Height div 2);
end;

function ConditionCoinPoint :TPoint;
begin
  case ExperimentalCondition of
    'Condição A' : Result := ConditionACoinPoint;
    'Condição B' : Result := ConditionBCoinPoint;
  end;
end;

function InsideCircle(ACenterX, ACenterY, ARadius, AX, AY : integer): Boolean; inline;
var Delta : integer;
begin
  Delta := ((AX - ACenterX) * (AX - ACenterX)) +
           ((AY - ACenterY) * (AY - ACenterY));
  if (Delta <= (ARadius * ARadius)) then
      Result := True
  else
      Result := False;
end;

const PixelsInCentimeter = 28 { 72 dpi monitor };

function InCircle(ACenter, APoint : TPoint): Boolean;
begin
  Result := InsideCircle(
   ACenter.X, ACenter.Y, PixelsInCentimeter*Diameter, APoint.X, APoint.Y);
end;

procedure ResizeStimuli(ACoin, AClow : TImage);
begin
  ACoin.Width := PixelsInCentimeter*2;
  ACoin.Height:= ACoin.Width;

  AClow.Width := PixelsInCentimeter*2*5;
  AClow.Height:= AClow.Width;
end;

procedure DebugView;
begin
  if Background <> nil then
  begin
    Background.Canvas.Brush.Color := clRed;
    Background.Canvas.EllipseC(
      CoinPoint.x, CoinPoint.y, PixelsInCentimeter, PixelsInCentimeter);
  end;
end;

procedure DoMouseConditionA;
begin
  Mouse.CursorPos := Background.ClientToScreen(ConditionACoinPoint);
end;

procedure DoMouseConditionB;
begin
  Mouse.CursorPos := Background.ClientToScreen(ConditionBCoinPoint);
end;

procedure CentralizeCursorPos;
begin
  Mouse.CursorPos := Background.ClientToScreen(CentralPoint);
end;

procedure Move(AImage: TImage; ACenter: TPoint);
begin
  AImage.Left := ACenter.X - (AImage.Width div 2);
  AImage.Top := ACenter.Y - (AImage.Height div 2);
end;

procedure LoadScreen(AScreen : TExperimentalScreen);
  procedure StartConditionA;
  begin
    CurrentScreen := StimuliAtScreenLeft;
    CentralizeCursorPos;
    CoinPoint := ConditionACoinPoint;
    ClowPoint := ConditionAClowPoint;
    Background.Color := clGreen;
  end;
  procedure StartConditionB;
  begin
    CurrentScreen := StimuliAtScreenRight;
  	CentralizeCursorPos;
  	CoinPoint := ConditionBCoinPoint;
  	ClowPoint := ConditionBClowPoint;
    Background.Color := clGreen;
  end;
  procedure StartTest;
  begin
    CurrentScreen := Test;
    Background.Color := clGreen;
    Report.WriteRow(['TESTE']);
    if Timer.Enabled = False then
    begin
      Timer.Interval:= 60000 * 3;
      Timer.OnTimer := TimerFinishTest;
      Timer.Enabled := True;
    end;
  end;
  procedure StartBlackBackground;
  begin
    CurrentScreen := BlackBackground;
    Background.Color := clBlack;
  end;
begin
  case AScreen of
    StimuliAtScreenLeft : StartConditionA;
    StimuliAtScreenRight : StartConditionB;
    BlackBackground : StartBlackBackground;
    Test : StartTest;
  end;
end;

procedure StartExperiment(ABackGround: TCustomControl; ATimer : TTimer;
  ATimerNextObservation: TNotifyEvent;
  ATimerFinishTest : TNotifyEvent;
  ATimerInterval : integer;
  ADiameter : integer;
  AParticipantName : string);
var
  LP : TPoint;
begin
  Background := ABackGround;
  Diameter:=ADiameter;
  TimerNextObservation:=ATimerNextObservation;
  TimerFinishTest:=ATimerFinishTest;
  Timer := ATimer;
  Timer.Interval:= ATimerInterval;
  Timer.OnTimer := TimerNextObservation;
  LP := ConditionCoinPoint;
  Report.StartTime := GetTickCount64;
  Report.WriteRow(['Participante:', AParticipantName]);
  Report.WriteRow(['Condição:', ExperimentalCondition]);
  Report.WriteRow(['Tamanho da Tela:', Background.Width.ToString+','+Background.Height.ToString]);
  Report.WriteRow(['Posição da Moeda:', LP.x.ToString+','+LP.y.ToString]);
  NextScreen;
  Report.WriteHeader;
end;

procedure NextScreen;
begin
  Inc(ScreenIndex);
  if ScreenIndex < Length(Screens) then
    LoadScreen(Screens[ScreenIndex])
  else
    Report.WriteFooter;
end;

function ScreenAsString: string;
begin
  WriteStr(Result, CurrentScreen);
end;

procedure InitializeCondition(ACondition: string);
var
  i: Integer;
  LConditionScreen : TExperimentalScreen;
begin
  SetLength(Screens, 9);
  Screens[Low(Screens)] := BlackBackground;
  ExperimentalCondition := ACondition;
  case ExperimentalCondition of
    'Condição A': LConditionScreen := StimuliAtScreenLeft;
    'Condição B': LConditionScreen := StimuliAtScreenRight;
  end;
  for i := Low(Screens)+1 to High(Screens)-2 do
    Screens[i] := LConditionScreen;
  Screens[High(Screens)-1] := BlackBackground;
  Screens[High(Screens)] := Test;
end;


end.

