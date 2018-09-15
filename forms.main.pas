unit Forms.Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButtonStartExperiment: TButton;
    EditParticipantName: TEdit;
    ImageCoin: TImage;
    ImageClow: TImage;
    LabelParticipantName: TLabel;
    Panel: TPanel;
    Timer: TTimer;
    procedure ButtonStartExperimentClick(Sender: TObject);
		procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
    procedure TimerNextObservation(Sender: TObject);
    procedure TimerFinishTest(Sender: TObject);
  private

  public
    {$IFDEF WINDOWS}
    OriginalBounds: TRect;
    OriginalWindowState: TWindowState;
    ScreenBounds: TRect;
    procedure SwitchFullScreen;
    {$ENDIF}
  end;

var
  Form1: TForm1;

implementation

uses experiment, TabDelimitedReport.Custom;

{$R *.lfm}

{ TForm1 }

procedure TForm1.ButtonStartExperimentClick(Sender: TObject);
begin
  Panel.Hide;
  ShowMessage(
    'O notebook dará a você todas as instruções para a realização da tarefa. '+
    'Você será informado quando a tarefa terminar, então não levante '+
    'enquanto não for informado.'
  );
  ShowMessage(
    'Clique na tela e encontre o palhaço.'
  );
  {$IFDEF WINDOWS}SwitchFullScreen;{$ENDIF}
  {$IFDEF LINUX}WindowState := wsFullScreen;{$ENDIF}
  Timer.Interval:= 5000;
  Timer.OnTimer := @TimerNextObservation;
  Report.Filename := Application.ExeName;
  StartExperiment(Self, EditParticipantName.Text);
  Invalidate;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ResizeStimuli(ImageCoin, ImageClow);
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: char);
begin
  if ExperimentalCondition = ConditionTest then
  begin
    if (Key = #32) and (Timer.Enabled = False) then
    begin
      Color := clGreen;
      Timer.Interval:= 60000 * 5;
      Timer.OnTimer := @TimerFinishTest;
      Timer.Enabled := True;
    end;
  end;
end;

procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  CursorPos : TPoint;
begin
  CursorPos := Point(X, Y);
  Report.WriteRow(ConditionAsString, X, Y);
  if ExperimentalCondition <> ConditionTest then
  if InCircle(CoinPoint, CursorPos) then
    begin
      Move(ImageCoin, CoinPoint);
      Move(ImageClow, ClowPoint);
      ImageCoin.Show;
      ImageClow.Show;
      Timer.Enabled := True;
    end;
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
  //DebugView;
end;

procedure TForm1.TimerNextObservation(Sender: TObject);
begin
  Timer.Enabled := False;
  ImageCoin.Hide;
  ImageClow.Hide;
  Invalidate;
  CentralizeCursorPos;
  NextCondition;
end;

procedure TForm1.TimerFinishTest(Sender: TObject);
begin
  Timer.Enabled := False;
  ImageCoin.Show;
  ImageClow.Show;

  Timer.Interval:= 5000;
  Timer.OnTimer := @TimerNextObservation;
  Timer.Enabled := True;
end;

{$IFDEF WINDOWS}
// http://wiki.freepascal.org/Application_full_screen_mode
procedure TForm1.SwitchFullScreen;
begin
  if BorderStyle <> bsNone then begin
    // To full screen
    OriginalWindowState := WindowState;
    OriginalBounds := BoundsRect;

    BorderStyle := bsNone;
    BoundsRect := Screen.MonitorFromWindow(Handle).BoundsRect;
  end else begin
    // From full screen
    BorderStyle := bsSizeable;
    if OriginalWindowState = wsMaximized then
      WindowState := wsMaximized
    else
      BoundsRect := OriginalBounds;
  end;
end;
{$ENDIF}


end.

