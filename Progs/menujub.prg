Local nCur
nCur="'"+Curdir()+"'"
Set Sysmenu to
*!*	Set Console Off
SET DATE BRITISH
SET CENTURY TO 19 ROLLOVER 10
SET CENTURY ON
SET DELETE ON
SET EXCL OFF
SET SAFETY OFF
SET TABLEVALIDATE TO 4
*!*	SET ESCAPE OFF
Set Procedure To Progs\RutNomina ADDITIVE
Set Defa to &nCur
*!*	Open Data Data\BdGastos Shared
*!*	Use BdCatalo!CatConfi In 0
*!*	If Eof('CatConfi') .or. Empty(CatConfi.Empresa)
*!*		Close all
*!*		MessageBox('Lo siento pero el sietema no esta'+Chr(13)+'instalado correctamente...',32,'Control de Gastos')
*!*		Quit
*!*	Endif
Public nVerMenu, nHandle, Usa_Indice
nHandle=.F.
nVerMenu=0
Usa_Indice=''
With _Screen
    .visible = .F.
    .hide()
	.Closable=.F.
    .autocenter = .T.
    .borderstyle = 2
	.WindowState=2
	.BackColor=RGB(0,128,255)
	.Caption = "MODULO DE NOMINAS DE JUBILADOS Y PENSIONADOS"
	.Icon = Curdir()+"graphics\logo.ico"
*	.Picture=Curdir()+"graphics\logo_pensiones.png"
*	Do Form Forms\About.Scx With '...'
	Do Form Forms\Timbrar.Scx
ENDWITH
 READ EVENTS
