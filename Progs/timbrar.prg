LPARAMETERS CQUIN,CCONC,FECHD,INICIO,FINAL1,DI,QI,nAnio
Local pPan, pPan2, nAno, nQuin, cRutaBas, cMaestro, cNomina, cFondo, cDesc, cJpp, nNum, TRel, CtaBan, DBanco, cTim
LOCAL cRuta,cRuta1
*cruta1="Timbrado\"
cruta = "NOMI" + RIGHT(STR(nAnio),2) + '\'
cruta1="timbrado"+"\"
*WAIT WINDOWS
DIMENSION PERDED(300,6),PERDEC(300,6)
ctipo='ACT'
  CMAESTRO= "MAES" + RIGHT(STR(nAnio),2) + IIF(QI<10,'0','')+ALLTRIM(STR(QI,2))
  CNOMINA= "NOMI" + RIGHT(STR(nAnio),2) + IIF(QI<10,'0','')+ALLTRIM(STR(QI,2))
  cmaes=alltrim(cmaestro)+".dbf"
  cnomi=alltrim(cnomina)+".dbf"
  carchi= cruta1 + "TIMB" + right(str(nAnio),2) + IIF(QI<10,'0','')+ALLTRIM(STR(QI,2)) + ".txt"

 if (!file(cruta + cmaes ) .OR. !file(cruta + cnomi  ))

     MESSAGEBOX("PRIMERO REALIZAR EL RESPALDO...")
    
 else
*	carchi= "Timbrado\"+right(str(year(INICIO)),4)+IIF(QI<10,'0','')+ALLTRIM(STR(QI,2)) +IIF(QI<10,'0','')+ALLTRIM(STR(QI,2)) + cTim +".txt"

		CLOSE TABLES all
		SELECT 1
		USE (cRuta+cMaestro) ALIAS maestro
		Index on Jpp+ Str(Num, 6) to temp\Maestro
		SELECT 2 
		USE (cRuta+cNomina) ALIAS NomiNew
		Index on Jpp+ Str(NumJpp, 6) to nominew
		SELECT 3
		USE perded
		Index on clave to perded

		CREATE TABLE Temp\Timbrado FREE (CAMPO C(250))

	  ANNO=YEAR(DATE())
	  nMESS=MONTH(DATE())
	  DIAA=DAY(DATE())

	  MES1=nMESS &&IIF(nMESS<10,'0','')+ALLTRIM(STR(nMESS,2))
	  DIA1=DIAA  &&IIF( DIAA<10,'0','')+ALLTRIM(STR( DIAA,2))

	  FOR I= 1 TO 300
	     PERDED[I,1]=0
	     PERDEC[I,1]=0
	     PERDED[I,4]=0
	     PERDEC[I,4]=0
	  NEXT
	  
	  clin= 0

	    SELECT 1
		Set Filter to Jpp <> 'PEA' .AND. STATUS<>33 .AND. STATUS <> 22
		  GO TOP 
	      DO WHILE(!EOF())
	&&&&&&&&&&&&&&&&&&&&&&&& FOR XXX=1 TO 5
			nSub=0
	        DEDUC=0
	        PERCE=0
	        P=0
	        D=0
	        ISR=0.00
	        SUELDO=0.00
	        SUELDO1=0.00
			CONTRA='No Existe'
	        IF JPP == "DIB"
	           CONTRA= ALLTRIM("BASE")
	        ENDIF
	        IF JPP == "COC"
	           CONTRA= ALLTRIM("CONT_CONFIANZA")
	        ENDIF
	        IF JPP == "NOC"
	           CONTRA= ALLTRIM("NOM_CONFIANZA")
	        ENDIF
	        IF JPP == "MMS"
	           CONTRA= ALLTRIM("MANDOS MEDIOS")
	        ENDIF

	        idem1= ALLTRIM(JPP)+ALLTRIM(STR(NUM))
	        nomem= ALLTRIM(STRTRAN(NOMBRE,'.',' '))
	        num1=  ALLTRIM(STR(NUM))
	        rfc1=  ALLTRIM(RFC)
	        curp1= alltrim(curp)  &&"No hay campo CURP "&& alltrim(curp)
	        imss1= alltrim(imss)  &&"No hay campo IMSS "&& alltrim(imss)
	        txtOFICINA= STRTRAN(ALLTRIM(OFICINA),'.',' ') &&"No hay campo OFICINA"&& ALLTRIM(OFICINA)
	        
	        FEAN=YEAR(FCHINGPEN)
	        FEME=MONTH(FCHINGPEN)
	        FEDI=DAY(FCHINGPEN)
	        CATEGO= ALLTRIM(STRTRAN(CATEG,'.',' '))
	        CJPP= JPP
	        NnUMJPP= NUM
	        TRel='2' &&IIF(TipoRel='BASE','S','N')
*	        CtaBan=ALLTRIM(RIGHT(ALLTRIM(cuentabanc),18))
*	        DBanco=Banco
	       iF FEME<10
	         FEME1="0"+ ALLTRIM(STR(FEME))
	       ELSE
	         FEME1=STR(FEME)
	       ENDIF
	       IF FEDI<10
	         FEDI1="0"+ ALLTRIM(STR(FEDI))
	       ELSE
	         FEDI1=STR(FEDI)
	       ENDIF
	        select 2
	        SEEK (CJPP+STR(NNUMJPP,6))
			IF FOUND()
	           DO While cJpp== Jpp .And. nNumJpp== NumJpp .AND. !EOF()

	               CVE=CLAVE

	               IF CLAVE = 201
	                  ISR=MONTO
	               ENDIF

	               IF CLAVE = 1 .OR. CLAVE = 2 .OR. CLAVE = 55 .OR. CLAVE = 40 .OR. CLAVE = 70 .OR. CLAVE=25
	                  SUELDO=MONTO
	                  SUELDO1= SUELDO / 15
*	                  SUELDO1= SUELDO / 30
	               ENDIF
	                IF Clave=105
	                	nSub=Monto
	                ENDIF 
	               If Clave < 200 .and. Clave<>69  .and. Clave<>82 .and. Clave<>83 .and. Clave<>85 &&.and. Clave<>105
	                 PERCE=PERCE + Monto
	                 IF Clave<>105
		                 P=P+1
		                 PERDED[P,1]=MONTO
		                 PERDED[P,2]=CLAVE

		                 SELECT 3
						 SEEK STR(cVE,3)
						 IF FOUND()
		                     PERDED[P,4]= SAT  &&CLAVE &&SAT
		                     PERDED[P,3]= ALLTRIM(STRTRAN(DESCRI,'.',' '))
		                     PERDED[P,3]= ALLTRIM(STRTRAN(PERDED[P,3],'%',' '))
						 ENDIF
					  ENDIF 
	               Else
	                 If Clave=69 .or. Clave=82 .or. Clave=83 .or. Clave=85 .or. clave>=201
	                   D=D+1
	                   Deduc=DEDUC + Monto
	                   PERDEC[D,1]=MONTO
	                   PERDEC[D,2]=CLAVE
	                 *  PERDEC[D,3]=DESCRI
	                   SELECT 3
					   SEEK STR(cVE,3)
					   IF FOUND()
	                        PERDEC[D,4]= SAT &&CLAVE &&SAT
		                    IF nominew.pagot>0
			                    PERDEC[D,3]= ALLTRIM(STRTRAN(DESCRI,'.',' '))+' '+ALLTRIM(STR(nominew.pago1+nominew.pago2+nominew.pago3+nominew.pago4))+' DE '+ALLTRIM(STR(nominew.pagot))
		                    else
		                        PERDEC[D,3]= ALLTRIM(STRTRAN(DESCRI,'.',' '))
		                        PERDEC[D,3]= ALLTRIM(STRTRAN(PERDEC[D,3],'%',' '))
		                    ENDIF
	                   ENDIF 
	                 endif
	               EndIf
	               SELECT 2
	               Skip
	           EndDo
	        ENDIF
	        SELECT timbrado
	        APPEND BLANK 
	        replace campo WITH "ECOMPROBANTE|dcto(C5),serie(C25),nominaId(C12),nominaDes(C120),TipoNomina(C1),"+;
	        				   "fecha(DT),formaDePago(C60),tipoDeComprobante(C12),Moneda(C12)"
			APPEND BLANK
			REPLACE campo WITH cTipo+'|A|'+ALLTRIM(CQUIN)+"|"+ALLTRIM(CCONC)+;
							   "|O|"+ALLTRIM(STR(ANNO))+"-"+IIF(MES1<10,'0','')+ALLTRIM(STR(MES1))+"-"+IIF(DIA1<10,'0','')+ALLTRIM(STR(DIA1))+"T"+TIME()+"|"+;
							   "PAGO EN UNA SOLA EXHIBICION|EGRESO|MXN|"
	        
			APPEND BLANK
			REPLACE campo WITH "ECOMPROBANTE|empleadoId(C15),Nombre(C60),NEmpleado(C15),RFC(C13),Calle(C60),"+;
								"NExt(C20),NInt(C20),Colonia(C60),Localidad(C60),Municipio(C60),Estado(C60),"+;
								"Pais(C60),CP(C5),CURP(C18),TRegimenId(C3),NSSocial(C15),Sindicalizado(C1),RfcPatronOrigen(C13)"

			APPEND BLANK
			REPLACE campo WITH idem1+"|"+nomem+"|"+num1+"|"+rfc1+"|ABASOLO|504||CENTRO|OAXACA DE JUAREZ|"+;
								"OAXACA DE JUAREZ|OAXACA|MEXICO|68000|"+curp1+"|02|"+imss1+;
								"|"+TRel+'||'

			APPEND BLANK 
			REPLACE campo WITH "ECOMPROBANTE|FTributariaRFC(C13),Depto(C60),CBancaria(C18),Banco(C3),"+;
								"FIRLaboral(D),Puesto(C60),TContrato(C3),TJornada(C3),PPago(C3),"+;
								"SBase(N12.2),RPuesto(C12),SDIntegrado(N12.2),FechaPago(D)"
	&& "+CtaBan+"

			APPEND BLANK
			REPLACE campo WITH "OPE631216S18|"+txtOFICINA+"|0000000000000000|072|"+;
								ALLTRIM(STR(FEAN))+"-"+ALLTRIM(FEME1)+"-"+ALLTRIM(FEDI1)+"|"+;
								CATEGO+"|01|01|04|"+;
								ALLTRIM(STR(SUELDO,12,2))+"|4|"+ALLTRIM(STR(SUELDO1,12,2))+"|"+;
								ALLTRIM(STR(YEAR(FECHD)))+"-"+IIF(MONTH(FECHD) < 10,'0','')+ALLTRIM(STR(month(FECHD)))+"-"+;
								IIF(DAY(FECHD) < 10,'0','')+ALLTRIM(str(day(FECHD)))+"|"

			APPEND BLANK
			REPLACE campo WITH "ECOMPROBANTE|FechaInicialPago(D),FechaFinalPago(D),NDPagados(N10.3),Antiguedad(N4,0),"+;
							   "EntidadSNCF(C1),OrigenRecurso(C2),MontoRecursoPropio(N12.2),ClaveEntFed(C3)"

			APPEND BLANK 
			REPLACE campo WITH ALLTRIM(STR(YEAR(INICIO)))+"-"+IIF(MONTH(INICIO) < 10,'0','')+ALLTRIM(STR(month(INICIO)))+"-"+;
								IIF(DAY(INICIO) < 10,'0','')+alltrim(STR(DAY(INICIO)))+"|"+ALLTRIM(STR(YEAR(final1)))+"-"+;
								IIF(MONTH(final1) < 10,'0','')+alltrim(STR(month(final1)))+"-"+;
								IIF(DAY(final1) < 10,'0','')+alltrim(STR(DAY(final1)))+"|"+ALLTRIM(STR(DI))+ALLTRIM(".000")+;
								"|0|S|IP|"+ALLTRIM(STR(SUELDO,12,2))+'|OAX|'
			
		***CONTINUARIA....
			
			APPEND BLANK
			REPLACE campo WITH "DCONCEPTO|Cantidad(N12.2),unidad(C15),descripcion(C60),valorUnitario(N12.2)"
			
			APPEND BLANK
			REPLACE campo WITH "1|ACT|Pago de nómina|"+ALLTRIM(STR(PERCE,12,2))+"|"
			APPEND BLANK 
			REPLACE campo WITH "DCPERCEPCION|TipoPercepcion(C5),Clave(C15),Concepto(C60),ImporteGravado(N12.2),ImporteExento(N12.2)"

	          FOR I= 1 TO P
	          	APPEND BLANK
	          	
	            IF PERDED[I,2] = 1 .OR. PERDED[I,2] = 2 .OR. PERDED[I,2] = 3 
	            	REPLACE campo WITH RIGHT("00" + ALLTRIM(STR(PERDED[I,4])),3)+"|"+;
	            						ALLTRIM("00" + alltrim(str(PERDED[I,2])))+"|"+;
	            						ALLTRIM(PERDED[I,3])+"|"+ALLTRIM(STR(PERDED[I,1],12,2))+"|0.00|"
	            ELSE
		            IF PERDED[I,2] = 40 .OR. PERDED[I,2] = 41 .OR. PERDED[I,2] = 45
		            	REPLACE campo WITH RIGHT("00" + ALLTRIM(STR(PERDED[I,4])),3)+"|"+;         
											ALLTRIM("0" + alltrim(str(PERDED[I,2])))+"|"+;
											ALLTRIM(PERDED[I,3])+"|"+ALLTRIM(STR(PERDED[I,1],12,2))+"|0.00|"
		            ELSE
			            IF PERDED[I,2] = 55 .OR. PERDED[I,2] = 56 .OR. PERDED[I,2] = 60
			            	REPLACE campo WITH RIGHT("00" + ALLTRIM(STR(PERDED[I,4])),3)+"|"+;
			            						ALLTRIM("0" + alltrim(str(PERDED[I,2])))+"|"+;
			            						ALLTRIM(PERDED[I,3])+"|"+ALLTRIM(STR(PERDED[I,1],12,2))+"|0.00|"
			            ELSE
				            IF PERDED[I,2] = 70 .OR. PERDED[I,2] = 71 .OR. PERDED[I,2] = 75
				            	REPLACE campo WITH RIGHT("00" + ALLTRIM(STR(PERDED[I,4])),3)+"|"+;
				            						ALLTRIM("0" + alltrim(str(PERDED[I,2])))+"|"+;
				            						ALLTRIM(PERDED[I,3])+"|"+ALLTRIM(STR(PERDED[I,1],12,2))+"|0.00|"
				            ELSE
								 REPLACE campo WITH IIF(perded[I,4]<10,'00','0')+alltrim(str(PERDED[I,4]))+"|"+;
								  					IIF(perded[i,2]<10,'00',IIF(perded[i,2]<100,'0',''))+ALLTRIM(str(PERDED[I,2]))+"|"+;
								  					ALLTRIM(PERDED[I,3])+"|0.00|"+ALLTRIM(STR(PERDED[I,1],12,2))+"|"
			            	ENDIF
	           			ENDIF
	            	ENDIF
	           	ENDIF
	           	IF (PERDED[I,4]=39 .or. PERDED[I,4]=44) && AND !JPRET
					APPEND BLANK
					REPLACE CAMPO WITH "CPJPRETIRO|TotalUnaExhibicion(N12.2),TotalParcialidad(N12.2),MontoDiario(N12.2),IngresoAcumulable(N12.2),IngresoNoAcumulable(N12.2)"
					APPEND BLANK
					REPLACE CAMPO WITH ALLTRIM(STR(PERDED[I,1],12,2))+'|0|0|0.00|0.00|'
	           	ENDIF
	          NEXT

	*!*			APPEND BLANK
	*!*			REPLACE CAMPO WITH "CPINDEMNIZA|TotalPagado(N12.2),NumAniosServicio(N12.2),UltimoSueldoMensOrd(N12.2),IngresoAcumulable(N12.2),IngresoNoAcumulable(N12.2)"
	*!*			APPEND BLANK
	*!*			REPLACE CAMPO WITH "CPATITULOS|ValorMercado(n16.6),PrecioAlOtorgarse(N16.6)"
	*!*			APPEND BLANK
	*!*			REPLACE campo WITH "DCHEXTRA|TipoHoras(C7),Dias(N4),HorasExtras(N4),ImportePagado(N12.2)"

			APPEND BLANK
			REPLACE campo WITH "DCDEDUCCION|TipoDeduccion(C3),Clave(C15),Concepto(C60),Importe(N12.2)"

			FOR J= 1 TO D
	          		APPEND BLANK 
	          		REPLACE campo WITH RIGHT('00'+alltrim(str(PERDEC[J,4])),3)+"|"+RIGHT('00'+ALLTRIM(STR(PERDEC[J,2])),3)+"|"+;
	          							ALLTRIM(PERDEC[J,3])+"|"+ALLTRIM(STR(PERDEC[J,1],12,2))+"|"
			NEXT
			APPEND BLANK
			REPLACE campo WITH "COTROSTPAGOS|TipoOtroPago(C3),Clave(C15),Concepto(C120),Importe(N12.2),SubsidioCausado(N12.2),"+;
								"SaldoAFavor(N12.2),Anio(N4.0),RemanenteSalFav(N12.2)"
			IF nSub>0
          		APPEND BLANK 
          		REPLACE campo WITH '002|105|SUBSIDIO AL EMPLEO|'+ALLTRIM(STR(nSub,12,2))+'|'+ALLTRIM(STR(nSub,12,2))+'|0.00|0|0.00|'
			ENDIF
			APPEND BLANK
			REPLACE campo WITH "CSCONTRATO|RFCLabora(C13),PorcentajeTiempo(N9.3)"
			APPEND BLANK
			REPLACE campo WITH "DCINCAPACIDAD|TipoIncapacidad(C5),DiasIncapacidad(N4),Importe(N12.2),Descripcion(C60)"
	    	select 1
	    	SKIP
	&&&&&&&&&&&&&&&&&&&&&&&& ENDFOR
	&&&&&&&&&&&&&&&&&&&&&&&& EXIT
	     ENDDO
	*   ENDIF
		SELECT TIMBRADO
		DELETE for EMPTY(campo)
		pack
		REPLACE campo WITH STRTRAN(campo,'Ñ','N') ALL
		REPLACE campo WITH STRTRAN(campo,'Ð','N') ALL
		REPLACE campo WITH STRTRAN(campo,'¥','N') ALL
	*	REPLACE campo WITH STRTRAN(campo,'.','') ALL
		REPLACE campo WITH STRTRAN(campo,';','N') ALL
		REPLACE campo WITH STRTRAN(campo,'%',' ') ALL
		REPLACE campo WITH STRTRAN(campo,'  ',' ') ALL
		COPY TO &carchi DELIMITER WITH ""

	*!*	   set printer to
	*!*	   set printer off
	*!*	   set device to screen
	*!*	   RestScreen(03, 02, 12, 75, pPan)
	*!*
ENDIF
CLOSE ALL
Return
