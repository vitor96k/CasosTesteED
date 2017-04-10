(* ----------------- FUNCAO PARA GERAR NUMEROS ALEATORIOS -----------------  *)
fun gerarNumero2(inferior:int, superior:int, k: int) = 
	let
		val nextInt = Random.randRange (inferior,superior)
		val r = Random.rand (1,k)
	in
		nextInt r
	end;
(* ---------------------------------------------------------------------------------------------------------------- *)

(* ----------------- FUNCAO PARA SELECIONAR ALEATORIAMENTE ALGUNS ELEMETOS DE UMA LISTA -----------------*)
fun selecao(lista,adivinhar:int,mudar:int, lim1: int, lim2:int) =
	let
		val numero = gerarNumero2(lim1,lim2,mudar)
		val inf = numero - 3
		val sup = numero + 3;
	in
		if null lista then []
		else if adivinhar > inf andalso adivinhar < sup then hd lista::selecao(tl lista,adivinhar,mudar+numero,lim1,lim2)
		else selecao(tl lista,adivinhar,mudar+1,lim1,lim2)
	end;
(* ---------------------------------------------------------------------------------------------------------------- *)

(* ----------------- Funcao para retornar o tamanho da lista ----------------- *)
fun getTamanho(lista) =
	if null lista then 0
	else 1 + getTamanho(tl lista);
(* ---------------------------------------------------------------------------------------------------------------- *)


(* ----------------- Funcao para pegar o n-esimo elemento de uma lista ----------------- *)
fun getElemento(lista, pos: int,acc:int)=
	if acc = pos then (hd lista)
	else getElemento(tl lista,pos,acc+1);
(* ---------------------------------------------------------------------------------------------------------------- *)

(* ----------------- FUNCOES PARA GERAR OS PONTOS CARTESIANOS DA QUADRA -----------------*)
fun definirX(xmin:int,jump:int,qtd: int) = 
	if qtd > 0 then xmin::definirX(xmin+jump,jump,qtd-1)
	else [];

fun definirY(ymax:int,jump:int, qtd: int) = 
	if qtd>0 then ymax::definirY(ymax+jump,jump,qtd-1)
	else [];

fun cruzarListas(lista1: int list, lista2:int list, lista2copia:int list,acc:int, largura: int,comprimento: int) = 
	if null lista1 then []
	else if null lista2 then cruzarListas(tl lista1,lista2copia,lista2copia,acc,largura,comprimento)	
	else ("q",hd lista1,hd lista2,largura,comprimento,"Quadra"^Int.toString(acc))::cruzarListas(lista1,tl lista2,lista2copia,acc+1,largura,comprimento);

fun gerarQuadras(xmin: int, ymax: int, espacamentox: int, espacamentoy: int, comprimento: int, largura: int, qntQuadrasX:int,qntQuadrasY:int) = 
	let
		val listax = definirX(xmin,largura+espacamentox,qntQuadrasX);
		val listay = definirY(ymax,espacamentoy+comprimento,qntQuadrasY);
	in
		cruzarListas(listax,listay,listay,1,largura,comprimento)
	end;
(* --------------------------------------------------------------------------------------------------------------------------------------------------------------------------- *)


(* ----------------- FUNCOES PARA RETORNAR UMA STRING COM CODIGO SVG A PARTIR DE UMA DADA TUPLA ----------------- *)
fun getLineSVG (x:(string * int * int * int * int * string)) =	
	let
		val linha = "<rect x=\""^Int.toString(#2 x) ^ "\"  y=\"" ^ Int.toString(#3 x) ^ "\" width=\"" ^ Int.toString(#4 x) ^"\" height=\"" ^ Int.toString(#5 x) ^"\" fill=\"" ^ "blue\" > </rect> " ^ "<text x=\"" ^ Int.toString((#2 x) + 3) ^ "\"  y=\"" ^ Int.toString((#3 x) + 12) ^  "\" font-family=\"Verdana\" font-size=\"10\" fill=\"black\" > " ^ (#6 x) ^ " </text>"
		
	in
		linha
	end;

fun getLineHidranteSVG(x:(string * string * int * int)) = 
	 "<circle cx=\"" ^ Int.toString(#3(x)) ^ "\" cy=\"" ^ Int.toString(#4(x)) ^ "\" r=\"2\" stroke=\" black\" stroke-width=\"1\" fill=\"red\" />"; 
  

fun getLineTorreSVG(x:(string * string * int * int)) = 
    "<circle cx=\"" ^ Int.toString(#3(x)) ^ "\" cy=\"" ^ Int.toString(#4(x)) ^ "\" r=\"3\" stroke=\" black\" stroke-width=\"1\" fill=\"purple\" />"; 

fun getLineSemaforoSVG(x:(string * string * int * int)) = 
    "<circle cx=\"" ^ Int.toString(#3(x)) ^ "\" cy=\"" ^ Int.toString(#4(x)) ^ "\" r=\"3\" stroke=\" black\" stroke-width=\"1\" fill=\"green\" />"; 
(* ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- *)

(* ----------------- FUNCOES PARA RETORNAR UMA STRING COM CODIGO .CRO A PARTIR DE UMA DADA TUPLA ----------------- *)
fun getLineHidrante( tupla: (string * string * int * int) ) =
	#1 tupla ^ " " ^ #2 tupla ^ " " ^  Int.toString(#3 tupla) ^  " " ^ Int.toString(#4 tupla);


fun getLine (x:(string * int * int * int * int * string)) =
     #1 x ^ " " ^  Int.toString(#2 x) ^ " " ^ Int.toString(#3 x) ^ " " ^ Int.toString(#4 x) ^ " " ^Int.toString(#5 x) ^ " " ^ #6 x;
(* ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- *)

(* ----------------- FUNCOES PARA ESCREVER NO ARQUIVO .SVG OU .CRO ----------------- *)
fun varrerQuadras(lista:(string * int * int * int * int * string) list ,nomeArquivo:string,tamanho:int) =
	let
		val texto =  if null lista then "" else getLine(hd lista)

		val outs = TextIO.openAppend nomeArquivo;
		val _ = if getTamanho(lista)=tamanho then TextIO.output(outs,texto) else TextIO.output(outs,"\n")	
		val _ = if getTamanho(lista)<tamanho then TextIO.output(outs,texto)	else TextIO.output(outs,"")	
		val _ = TextIO.flushOut outs
		val _ = TextIO.closeOut(outs)	
	
	in
		if null lista then ""
		else varrerQuadras(tl lista,nomeArquivo,tamanho)
	end;

fun escreverPoligno(lista:(int * int) list ,nomeArquivo:string,tamanho:int) =
	let
		val texto =  if null lista then "" else Int.toString(#1 (hd lista)) ^ " " ^ Int.toString(#2 (hd lista))

		val outs = TextIO.openAppend nomeArquivo;
		val _ = if getTamanho(lista)=tamanho then TextIO.output(outs,texto) else TextIO.output(outs,"\n")
		val _ = if getTamanho(lista)<tamanho then TextIO.output(outs,texto)	else TextIO.output(outs,"")	
		val _ = TextIO.flushOut outs
		val _ = TextIO.closeOut(outs)	
	
	in
		if null lista then ""
		else escreverPoligno(tl lista,nomeArquivo,tamanho)
	end;

fun varrerTorresSVG(lista:(string * string * int * int) list ,nomeArquivo:string,tamanho:int) =
	let
		val texto =  if null lista then "" else getLineTorreSVG(hd lista)

		val outs = TextIO.openAppend nomeArquivo;
	    val _ = if tamanho = getTamanho(lista) then  TextIO.output(outs,texto) else TextIO.output(outs,"\n")
		val _ = if tamanho > getTamanho(lista) then TextIO.output(outs,texto) else  TextIO.output(outs,"")
		val _ = TextIO.flushOut outs
		val _ = TextIO.closeOut(outs)	
	
	in
		if null lista then ""
		else varrerTorresSVG(tl lista,nomeArquivo,tamanho)
	end;

fun varrerSemaforoSVG(lista:(string * string * int * int) list ,nomeArquivo:string,tamanho:int) =
	let
		val texto =  if null lista then "" else getLineSemaforoSVG(hd lista)

		val outs = TextIO.openAppend nomeArquivo;
	    val _ = if tamanho = getTamanho(lista) then  TextIO.output(outs,texto) else TextIO.output(outs,"\n")
		val _ = if tamanho > getTamanho(lista) then TextIO.output(outs,texto) else  TextIO.output(outs,"")
		val _ = TextIO.flushOut outs
		val _ = TextIO.closeOut(outs)	
	
	in
		if null lista then ""
		else varrerSemaforoSVG(tl lista,nomeArquivo,tamanho)
	end;

fun varrerQuadrasSVG(lista:(string * int * int * int * int * string) list ,nomeArquivo:string,tamanho:int) =
	let
		val texto =  if null lista then "" else getLineSVG(hd lista)

		val outs = TextIO.openAppend nomeArquivo;
		val _ = TextIO.output(outs,"\n")	
		val _ = TextIO.output(outs,texto)
		val _ = TextIO.flushOut outs
		val _ = TextIO.closeOut(outs)	
	
	in
		if null lista then ""
		else varrerQuadrasSVG(tl lista,nomeArquivo,tamanho)
	end;

fun varrerHidrasSVG(lista:(string * string * int * int) list ,nomeArquivo:string,tamanho:int) =
	let
		val texto =  if null lista then "" else getLineHidranteSVG(hd lista)

		val outs = TextIO.openAppend nomeArquivo;
	    val _ = if tamanho = getTamanho(lista) then  TextIO.output(outs,texto) else TextIO.output(outs,"\n")
		val _ = if tamanho > getTamanho(lista) then TextIO.output(outs,texto) else  TextIO.output(outs,"")
		val _ = TextIO.flushOut outs
		val _ = TextIO.closeOut(outs)	
	
	in
		if null lista then ""
		else varrerHidrasSVG(tl lista,nomeArquivo,tamanho)
	end;

fun comecoSVG(nomeArquivo:string,a:int,b:int) =
	let
		val texto =  "<svg width=\"" ^ Int.toString(a) ^"\" height=\"" ^ Int.toString(b) ^ "\">"
		val outs = TextIO.openAppend nomeArquivo;
		val _ = TextIO.output(outs,texto)
		val _ = TextIO.flushOut outs
		val _ = TextIO.closeOut(outs)	
	in
		1
	end;

fun fimSVG(nomeArquivo:string) =
	let
		val texto =  "</svg>"
		val outs = TextIO.openAppend nomeArquivo;
		val _ =  TextIO.output(outs,"\n")
		val _ = TextIO.output(outs,texto)
		val _ = TextIO.flushOut outs
		val _ = TextIO.closeOut(outs)	
	in
		1
	end;


fun varrerHidrantes(lista:(string * string * int * int) list ,nomeArquivo:string,tamanho:int) =
	let
		val texto =  if null lista then "" else getLineHidrante(hd lista)

		val outs = TextIO.openAppend nomeArquivo;			
		val _ = if tamanho = getTamanho(lista) then  TextIO.output(outs,texto) else TextIO.output(outs,"\n")
		val _ = if tamanho > getTamanho(lista) then TextIO.output(outs,texto) else  TextIO.output(outs,"")
		val _ = TextIO.flushOut outs
		val _ = TextIO.closeOut(outs)	
	
	in
		if null lista then ""
		else varrerHidrantes(tl lista,nomeArquivo,tamanho)
	end;


fun comecoPoligno(nomeArquivo:string) =
	let
		val texto =   "<polygon points=\""

		val outs = TextIO.openAppend nomeArquivo;
	    val _ = TextIO.output(outs,"\n")
		val _ = TextIO.output(outs,texto)
		val _ = TextIO.flushOut outs
		val _ = TextIO.closeOut(outs)	
	
	in
		1
	end;


fun fimPoligno(nomeArquivo:string) =
	let
		val texto =   "\" fill=\"#black\" fill-opacity=\"0.5\" />"

		val outs = TextIO.openAppend nomeArquivo;
	    val _ = TextIO.output(outs,texto)
		val _ = TextIO.flushOut outs
		val _ = TextIO.closeOut(outs)	
	
	in
		1
	end;

fun varrerPolignos(lista:(int * int) list ,nomeArquivo:string,tamanho:int) =
	let
		val texto =  if null lista then "" else Int.toString(#1(hd lista)) ^ "," ^ Int.toString(#2(hd lista)) ^ " "

		val outs = TextIO.openAppend nomeArquivo;			
		
		val _ = TextIO.output(outs,texto)
		val _ = TextIO.flushOut outs
		val _ = TextIO.closeOut(outs)	
	
	in
		if null lista then ""
		else varrerPolignos(tl lista,nomeArquivo,tamanho)
	end;
(* ---------------------------------------------------------------------------------------------------------------- *)


(* ----------------- FUNCOES PARA PEGAR O MAIOR ELEMENTO DE UMA DETERMINA LISTA ----------------- *)
fun getBiggerX(lista:(string * int * int * int * int * string) list,maior:int) =
	let
		val grande =  if null lista then maior else if maior < #2(hd lista) then #2(hd lista) else maior
	in
		if null lista then grande
		else getBiggerX(tl lista,grande)
	end;


fun getBiggerY(lista:(string * int * int * int * int * string) list,maior:int) =
	let
		val grande =  if null lista then maior else if maior < #3(hd lista) then #3(hd lista) else maior
	in
		if null lista then grande
		else getBiggerY(tl lista,grande)
	end;

fun getPolX(lista:(int * int) list,maior:int) =
	let
		val grande =  if null lista then maior else if maior < #1(hd lista) then #1(hd lista) else maior
	in
		if null lista then grande
		else getPolX(tl lista,grande)
	end;


fun getPolY(lista:(int * int) list,maior:int) =
	let
		val grande =  if null lista then maior else if maior < #2(hd lista) then #2(hd lista) else maior
	in
		if null lista then grande
		else getPolY(tl lista,grande)
	end;
(* ---------------------------------------------------------------------------------------------------------------- *)
		

(* -----------------  FUNCAO PARA GERAR PONTOS CARTESIANOS DE TORRES ----------------- *)
fun gerarTorres(lista:(string * int * int * int * int * string) list,acc:int) =
	let
		val vazio = if null lista then 1 else 0
		val ponto0 = if null lista then 0 else #2(hd lista)						(*X INFERIOR*)
		val ponto1 = if null lista then 0 else #2(hd lista) +  #4(hd lista)		(*X SUPERIOR*)

		val ponto2 = if null lista then 0 else #3(hd lista)					    (*Y SUPERIOR*)	
		val ponto3 = if null lista then 0 else #3(hd lista)	+  #5(hd lista)	    (*Y INFERIOR*)

		val meiox = (ponto0+ponto1) div 2
		val meioy = (ponto2+ponto3) div 2
	in
		if vazio = 1 then []
		else ("t","TO"^Int.toString(acc),meiox,meioy)::gerarTorres(tl lista,acc+1)
	end;
(* ---------------------------------------------------------------------------------------------------------------- *)


(* -----------------  FUNCAO PARA GERAR PONTOS CARTESIANOS DE HIDRANTES ----------------- *)
fun gerarHidrantes(lista:(string * int * int * int * int * string) list,acc:int) = 
	let		
		val vazio = if null lista then 1 else 0

		val ponto0 = if null lista then 0 else #2(hd lista)						(*X INFERIOR*)
		val ponto1 = if null lista then 0 else #2(hd lista) +  #4(hd lista)		(*X SUPERIOR*)

		val ponto2 = if null lista then 0 else #3(hd lista)					    (*Y SUPERIOR*)	
		val ponto3 = if null lista then 0 else #3(hd lista)	+  #5(hd lista)	    (*Y INFERIOR*)

		val meiox = (ponto0+ponto1) div 2
		val meioy = (ponto2+ponto3) div 2
	in
		if vazio = 1 then []
		else ("h","HID"^Int.toString(acc),meiox,ponto2)::("h","HID" ^ Int.toString(acc+1),meiox,ponto3)::("h","HID" ^ Int.toString(acc+2),ponto0,meioy)::("h","HID" ^ Int.toString(acc+3),ponto1,meioy)::gerarHidrantes(tl lista,acc+4)
		
	end;
(* ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- *)


(* ----------------- FUNCOES PARA GERAR OS PONTOS CARTESIANOS DO SEMAFOROS -----------------*)

(*Enquanto o eixo x for igual, define os Y*)
fun gerarSemaforosY(lista:(string * int * int * int * int * string) list, xanteiror: int) =
	let	
		val vazio = if null lista then 1 else 0
		val tupla1 = if null lista then ("",0,0,0,0,"") else hd lista
		val tupla2 = if null (tl lista) then ("",0,0,0,0,"") else hd (tl lista)

		val meio = if null lista then 0 else ((#3 tupla1) + (#5 tupla1) + (#3 tupla2) ) div 2
	in
		if null lista orelse null (tl lista) then []
		else if (#2 tupla1) = (#2 tupla2) then meio::gerarSemaforosY(tl lista,(#2 tupla1))
		else []
	end;

(*Enquanto o eixo Y for igual, define os X*)
fun gerarSemaforosX(lista:(string * int * int * int * int * string) list, qnt: int, acc: int, garantir:int) =
	if qnt = 0 then []
	else ((#2 (getElemento(lista,acc,0))+ #4 (getElemento(lista,acc,0)) + #2 (getElemento(lista,acc+garantir,0))) div 2)::gerarSemaforosX(lista,qnt-1,acc+garantir,garantir);


fun mergeLista(lista1: int list, lista2:int list, lista2copia:int list,acc:int) = 
	if null lista1 then []
	else if null lista2 then mergeLista(tl lista1,lista2copia,lista2copia,acc)	
	else ("s","SEMA"^Int.toString(acc),hd lista1,hd lista2)::mergeLista(lista1,tl lista2,lista2copia,acc+1);
(* ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- *)

(* ----------------- FUNCAO PARA CRIAR POLIGNO -----------------*)

fun gerarPoligno(max:int, acc:int, maxy: int, ymin:int) =
	let
		val numero = gerarNumero2(ymin,maxy,acc*gerarNumero2(2,19,acc))
	in
		if max > 30 then (max,numero)::gerarPoligno(max-10,acc+1,maxy,ymin)
		else []
	end;

fun quebrarMetade1(lista, tamanho) = 
	if tamanho > 0 then hd lista::quebrarMetade1(tl lista,tamanho-1)
	else [];

fun quebrarMetade2(lista, tamanho) = 
	if tamanho > 0 then quebrarMetade2(tl lista,tamanho-1)
	else lista;
	
fun juntar(lista1,lista2) =
	if null lista2 then lista1
	else (hd lista2)::juntar(lista1,tl lista2);

fun inverter(lista,tamanho) =
	if tamanho = ~1 then []
	else  getElemento(lista,tamanho,0)::inverter(lista,tamanho-1);




	
(* ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- *)

(* ----------------- FUNCAO QUE CHAMAS AS FUNCOES ANTERIORES PARA GERAR UM CASO DE TESTES A CADA CHAMADA RECURSIVA ----------------- *)
fun gerarTestes2(aleatorio:int,nomeArquivo,acumulador:int) = 


	let
		val xmin = gerarNumero2(10,500,aleatorio*acumulador)
		val ymax = gerarNumero2(100,400,aleatorio*acumulador)

		val qntQuadras = gerarNumero2(4,15,ymax+xmin)

		val qntQuadrasX = qntQuadras + gerarNumero2(0,4,(xmin*ymax) div (xmin+ymax))


		val quadrinhas = gerarQuadras(xmin,ymax,gerarNumero2(30,50,aleatorio+2),gerarNumero2(30,50,aleatorio+3),gerarNumero2(50,100,aleatorio+4),gerarNumero2(80,120,aleatorio+5),qntQuadrasX,qntQuadras);
		val hidrantes2 = gerarHidrantes(quadrinhas,1)
		val hidrantes = selecao(hidrantes2,6,0,0,10)

		val torres2 = gerarTorres(quadrinhas,1)
		val torres = selecao(torres2,35,0,15,55)

		val limx = getBiggerX(quadrinhas,0)
		val limy = getBiggerY(quadrinhas,0)

		val semay = gerarSemaforosY(quadrinhas, (#2 (hd quadrinhas)) )
		val semax = gerarSemaforosX(quadrinhas,qntQuadrasX-1,0,qntQuadras)

		val semaforos2 = mergeLista(semax,semay,semay,1)
		val semaforos = selecao(semaforos2,7,0,0,10) 

		
		val escreverQuadra = varrerQuadras(quadrinhas,nomeArquivo ^ Int.toString(acumulador) ^ ".cro",getTamanho(quadrinhas))
		val escreverHidrante = varrerHidrantes(hidrantes,nomeArquivo ^ Int.toString(acumulador) ^ ".cro",getTamanho(hidrantes))
		val escreverTorres = varrerHidrantes(torres,nomeArquivo ^ Int.toString(acumulador) ^ ".cro",getTamanho(torres))
		val escreverSemaforo = varrerHidrantes(semaforos,nomeArquivo ^ Int.toString(acumulador) ^ ".cro",getTamanho(semaforos))


		val escreverTopoSVG = comecoSVG(nomeArquivo ^ Int.toString(acumulador) ^ ".svg",limx+200,limy+200)

		val escreverMeioSVG = varrerQuadrasSVG(quadrinhas,nomeArquivo ^ Int.toString(acumulador) ^ ".svg",getTamanho(quadrinhas))
		val escreverMeioSVG = varrerHidrasSVG(hidrantes,nomeArquivo ^ Int.toString(acumulador) ^ ".svg",getTamanho(hidrantes))
		val escreverMeioSVG = varrerTorresSVG(torres,nomeArquivo ^ Int.toString(acumulador) ^ ".svg",getTamanho(torres))
		val escreverMeioSVG = varrerSemaforoSVG(semaforos,nomeArquivo ^ Int.toString(acumulador) ^ ".svg",getTamanho(semaforos))

		val gerar = gerarNumero2(1,10,aleatorio)
		val gerar1 = gerar-2
		val gerar2 = gerar+2

		val escolhido = gerarNumero2(2,9,aleatorio*4)



		val poligno2 = gerarPoligno(limx,0,limy div 2,10)
		val pz2 = gerarPoligno(limx,0,limy, getPolY(poligno2,0))

		
		val poligno3 = selecao(poligno2,9,0,0,10)
		val pz3 = selecao(pz2,9,0,0,10)

		


		val poligno = if escolhido < gerar2 andalso escolhido > gerar1 then quebrarMetade2(poligno3,getTamanho(poligno3) div 2) else quebrarMetade1(poligno3,getTamanho(poligno3) div 2)
		val pz = if escolhido < gerar2 andalso escolhido > gerar1 then quebrarMetade2(pz3,getTamanho(pz3) div 2) else quebrarMetade1(pz3,getTamanho(pz3) div 2)

		
		val poligno = juntar(inverter(poligno,getTamanho(poligno)-1),pz)

		val escreverPolino = escreverPoligno(poligno,nomeArquivo ^ Int.toString(acumulador) ^ "-poligno.pol",getTamanho(poligno))

		val coPo = comecoPoligno(nomeArquivo ^ Int.toString(acumulador) ^ ".svg")

		val varrepol = varrerPolignos(poligno,nomeArquivo ^ Int.toString(acumulador) ^ ".svg",getTamanho(poligno))

		val fimPo = fimPoligno(nomeArquivo ^ Int.toString(acumulador) ^ ".svg")


		


		val escreverFimSVG = fimSVG(nomeArquivo ^ Int.toString(acumulador) ^ ".svg")
	in
		1
	end;


fun gerarTestes3(qntLoop:int,nomeArquivo:string) =
	let 		 
	 	val aleatorio = gerarNumero2(1,5+qntLoop,qntLoop);
	 	val escrevendoQuadras = gerarTestes2(aleatorio+1,nomeArquivo,qntLoop)
	 in
	 	if qntLoop = 1 then 1
	 	else gerarTestes3(qntLoop-1,nomeArquivo)
	 end;


fun geraTestes() = 
	let
		val nomeArq = "cidade"
		val qnt = gerarNumero2(8,12,3);
	in
		gerarTestes3(qnt,nomeArq)
	end;
(* ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- *)


val res = geraTestes();
val _ = print ("Teste gravados ! Equipe: Vitor Palma Aderaldo (201400560393) e Joao Victor Kawazoe (201400560188)");