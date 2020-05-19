unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, ExtCtrls, Menus, lNetComponents, lNet;

type

  TChargeStatus = (st_Init,st_ready,st_connected,st_charging,st_error,st_standby,st_Invalid);

  { TFormMain }

  TFormMain = class(TForm)
    TESTCMD: TButton;
    ButtonDiconnect: TButton;
    ButtonConnect: TButton;
    Connected: TImage;
    Error: TImage;
    Label1: TLabel;
    Standby: TImage;
    Ready: TImage;
    Charging: TImage;
    Status: TImage;
    LTCP: TLTCPComponent;
    EditPort: TEdit;
    EditIP: TEdit;
    LabelPort: TLabel;
    LabelHostName: TLabel;
    ButtonSend: TButton;
    EditSend: TEdit;
    MemoText: TMemo;
    TimerUpdate: TTimer;
    TimerQuit: TTimer;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormPaint(Sender: TObject);
    procedure ReadyClick(Sender: TObject);
    procedure LTCPComponentConnect(aSocket: TLSocket);
    procedure ConnectButtonClick(Sender: TObject);
    procedure DiconnectButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LTCPComponentError(const msg: string; aSocket: TLSocket);
    procedure LTCPComponentAccept(aSocket: TLSocket);
    procedure LTCPComponentReceive(aSocket: TLSocket);
    procedure LTcpComponentDisconnect(aSocket: TLSocket);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure MenuItemExitClick(Sender: TObject);
    procedure SendButtonClick(Sender: TObject);
    procedure SendEditKeyPress(Sender: TObject; var Key: char);
    procedure TESTCMDClick(Sender: TObject);
    procedure TimerQuitTimer(Sender: TObject);
    procedure TimerUpdateTimer(Sender: TObject);
  private
    FNet: TLConnection;
    FIsServer: Boolean;
    FChargeStatus : TChargeStatus;
    procedure SendToAll(const aMsg: string);
    procedure SetChargeStatus(AValue: TChargeStatus);
  public
    property ChargeStatus:TChargeStatus read FChargeStatus write SetChargeStatus;
    { public declarations }
  end; 

var
  FormMain: TFormMain;
  last_cmd: string;
  set_current: string;
  act_state: string;
  act_current: string;
  act_temp: string;
  energy_session: string;
  elapsed_time: string;
  energy_tot:string;
  temp: string;

implementation

uses
  lCommon;

{ TFormMain }

procedure TFormMain.ConnectButtonClick(Sender: TObject);
begin
  if FNet.Connect(EditIP.Text, StrToInt(EditPort.Text)) then
    FIsServer := False;
end;


procedure TFormMain.LTCPComponentConnect(aSocket: TLSocket);
begin
  MemoText.Append('Connected to remote host');
end;

procedure TFormMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;

  if FNet.Connected then begin
    CloseAction := caNone; // make sure we quit gracefuly
    FNet.Disconnect; // call disconnect (soft)
    TimerQuit.Enabled := True; // if time runs out, quit ungracefully
  end;
end;


procedure TFormMain.FormPaint(Sender: TObject);
begin
  //
  Case ChargeStatus of
    st_ready: Canvas.Draw(0,0,Ready.Picture.Bitmap);
    st_connected: Canvas.Draw(0,0,Connected.Picture.Bitmap);
    st_charging: Canvas.Draw(0,0,Charging.Picture.Bitmap);
  end;
end;



procedure TFormMain.ReadyClick(Sender: TObject);
begin

end;

procedure TFormMain.LTCPComponentError(const msg: string; aSocket: TLSocket);
begin
  MemoText.Append(msg);
  MemoText.SelStart := Length(MemoText.Lines.Text);
end;

procedure TFormMain.LTCPComponentAccept(aSocket: TLSocket);
begin
  MemoText.Append('Connection accepted');
  MemoText.SelStart := Length(MemoText.Lines.Text);
end;


function CRC(Data: AnsiString): AnsiString; // 8-bit XOR Checksum
var
 i,iSum : Integer;
begin
 iSum := 0;
 for i := 1 to Length(Data) do
 begin
  iSum := iSum xor Ord(Data[i]);
 end;
 Result := IntToHex(iSum,2)
end;


procedure TFormMain.LTcpComponentDisconnect(aSocket: TLSocket);
begin
  MemoText.Append('Connection lost');
  MemoText.SelStart := Length(MemoText.Lines.Text);
end;

procedure TFormMain.MenuItemAboutClick(Sender: TObject);
begin
  MessageDlg('EVSE TCP Control V0.01',
             mtInformation, [mbOK], 0);
end;

procedure TFormMain.MenuItemExitClick(Sender: TObject);
begin
  Close;
end;


function build_cmd_str(cmd_in: AnsiString): AnsiString; // generate command string
begin
 cmd_in := '$' + cmd_in;
 Result := cmd_in + '^' + CRC(cmd_in) + #13;
end;



procedure TFormMain.SendButtonClick(Sender: TObject);
begin
  if Length(EditSend.Text) > 0 then begin
    if FNet.Connected then begin
       last_cmd := EditSend.Text;  // remember last command sent
       EditSend.Text := build_cmd_str(EditSend.Text);
       MemoText.Append(EditSend.Text);
       FNet.SendMessage(EditSend.Text);
       EditSend.Text := '';
    end;
  end;
end;

procedure TFormMain.TESTCMDClick(Sender: TObject);
var str : AnsiString;
begin
 str := 'SC 10';
 last_cmd := str;
 str := build_cmd_str(str);
 FNet.SendMessage(str);
 str := '';
end;


procedure TFormMain.LTCPComponentReceive(aSocket: TLSocket);
var
  s,l,r,rec_cs,calc_cs,la,ppblock,cmd: string;
  pp: TStringArray;
  len: Int64;

begin
  if aSocket.GetMessage(s) > 0 then begin
    MemoText.Append(s);

//    MemoText.Append(IntToStr(Length(s)));
    len := Length(s) - 4;    // Commando length without checksum
    l := LeftStr(s,len);     // Commando String $cc pp pp ...
    r := RightStr(s,3);      // Checksum String xk\r
    rec_cs := LeftStr(r,2);  // received Checksum String
    calc_cs := CRC(l);       // calculated Checksum String
//    MemoText.Append(l);
//    MemoText.Append(rec_cs);
//    MemoText.Append(calc_cs);
    if rec_cs = calc_cs then  // Antwort verarbeiten
      begin
        case l of
           '$ST 01': begin
                 MemoText.Append('Status: Bereit');
                 ChargeStatus := st_ready;
           end;
           '$ST 02': begin
                MemoText.Append('Status: Verbunden');
                ChargeStatus := st_connected;
           end;
           '$ST 03': begin
                 MemoText.Append('Status: Laden');
                 ChargeStatus := st_charging;
           end;
           '$ST 04','$ST 05': begin
                MemoText.Append('Status: Fehler');
                ChargeStatus := st_error;
           end;
           '$ST fe': begin
                MemoText.Append('Status: Pause');
                ChargeStatus := st_standby;
           end;
           otherwise begin
           la := LeftStr(l,3);
           if la = '$OK' then
             begin
              len := Length(l) - 3;
              ppblock := RightStr(l,len);
              pp := ppblock.Split(' ');            // array of parameters
              len := high(pp);                     // number of parameters
              MemoText.Append(ppblock + ' received ' + IntToStr(len));
              cmd := Leftstr(last_cmd,2);          // limit to command name
              MemoText.Append('try ' + cmd);
              case cmd of
                 'SC':begin                        // set current
                   set_current := pp[1];
                   MemoText.Append('Current setting: ' +  set_current);
                 end;
                 'GG':begin                        // get actual current
                    act_current := pp[1];
                    MemoText.Append('Current: ' +  act_current);
                 end;
                 'GS':begin                        // get actual state and elapsed time
                    act_state := pp[1];
                    elapsed_time := pp[2];
                    MemoText.Append('State: ' +  act_state + ' Session Time: ' + elapsed_time);
                 end;
                 'GP':begin                        // get temperature
                    act_temp := pp[1];
                    MemoText.Append('Temperature: ' +  act_temp);
                 end;
                  'GU':begin                       // get energy usage
                    energy_session := pp[1];
                    energy_tot := pp[2];
                    MemoText.Append('Energy Session: ' +  energy_session + ' Total: ' + energy_tot);
                 end;
              end;
             end else  MemoText.Append('?????');
           end;
        end;
      end
    else
     begin
       MemoText.Append('Checksum error....');
     end;

    MemoText.SelStart := Length(MemoText.Lines.Text);
  end;
end;

procedure TFormMain.DiconnectButtonClick(Sender: TObject);
begin
  FNet.Disconnect;
  MemoText.Append('Disconnected');
end;



procedure TFormMain.FormCreate(Sender: TObject);
begin
  FNet := LTCP;
  LTCP.SocketNet := LAF_INET;
  if EditIP.Text = '::1' then
    EditIP.Text := 'localhost';
  FIsServer := False;
  FChargeStatus := st_ready;
end;


procedure TFormMain.SendEditKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
    SendButtonClick(Sender);
end;



procedure TFormMain.TimerQuitTimer(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.TimerUpdateTimer(Sender: TObject);
begin

//   testing only

     Label1.Caption := DateTimeToStr(now());

//   ChargeStatus := TChargeStatus(Ord(ChargeStatus)+1);
//   if ChargeStatus=st_Invalid then
//    ChargeStatus := st_Init;

end;

procedure TFormMain.SendToAll(const aMsg: string);
var
  n: Integer;
begin
   // TCP
    FNet.IterReset; // start at server socket
    while FNet.IterNext do begin // skip server socket, go to clients only
      n := FNet.SendMessage(aMsg, FNet.Iterator);
      if n < Length(aMsg) then
        MemoText.Append('Error on send [' + IntToStr(n) + ']');
    end;
 end;

procedure TFormMain.SetChargeStatus(AValue: TChargeStatus);
begin
  FChargeStatus:=AValue;
  Refresh;
end;

initialization
  {$I main.lrs}

end.

