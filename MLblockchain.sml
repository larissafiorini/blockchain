(*
	ALUNOS: Larissa Martin, Carlos Souza, Felipe Guedes
	REFERENCIAS:
	
	Paradigma Funcional
	https://moodle.pucrs.br/pluginfile.php/2398157/mod_resource/content/1/mangan_lp_2017_2_es_u04.pdf
	
	SML Manual
	http://sml-family.org/Basis/manpages.html
	
	Creating Blockchain in Java
	https://medium.com/programmers-blockchain/create-simple-blockchain-java-tutorial-from-scratch-6eeed3cb03fa
	
	Creating Blockchain in Haskel
	http://www.michaelburge.us/2017/08/17/rolling-your-own-blockchain.html
	
	DataType Stackoverflow
	https://stackoverflow.com/questions/19805544/sml-difference-between-type-and-datatype
	
	Site Oficial Alice SML
	https://www.ps.uni-saarland.de/alice/
	
	SML Wikipedia
	https://en.wikipedia.org/wiki/Standard_ML
	http://pages.cs.wisc.edu/~fischer/cs538.s08/lectures/Lecture28.4up.pdf
*)



(* Blockchain *)
val it = "The block chain: " : string


(* TYPE *)
type data_block = {data : string, hash : string, nonce : string, previousHash : string, timeStamp : string}

(* Fun��o para c�lculo hash *)
fun calchash {a, b, c} = 
(Char.ord (List.nth ((explode a), 0)))
+ b 
+ (Real.floor c);

(* Defini��o de blocos *)
val block1 = {hash = "1", previousHash = "0", data = "Hi im the first block", timeStamp = "1", nonce = "1"};
val SOME timeStampToInt= Int.fromString (#timeStamp block1);
val SOME nonceToInt= Int.fromString (#nonce block1);

val calculoHash = calchash {a= #hash block1, b= timeStampToInt, c= Real.fromInt(nonceToInt)};
val hashToString = Int.toString calculoHash;
val block2 = {hash = hashToString, previousHash = #hash block1, data = "Hi im the second block", timeStamp = "2", nonce = "2"};

val SOME timeStampToInt3= Int.fromString (#timeStamp block2);
val SOME nonceToInt3= Int.fromString (#nonce block2);
val calculoHash2 = calchash {a= #hash block2, b= timeStampToInt3, c= Real.fromInt(nonceToInt3)};

val hashToString3 = Int.toString calculoHash2;
val block3 = {hash = hashToString3, previousHash = #hash block2, data = "Hi im the third block", timeStamp = "3", nonce = "3"};

fun readFile filename =
    let val fd = TextIO.openIn filename
        val content = TextIO.inputAll fd handle e => (TextIO.closeIn fd; raise e)
        val _ = TextIO.closeIn fd
    in content end;

fun writeFile filename content =
    let val fd = TextIO.openOut filename
        val _ = TextIO.output (fd, content) handle e => (TextIO.closeOut fd; raise e)
        val _ = TextIO.closeOut fd
    in () end;

(* Escreve blocos no arquivo json*)
fun escreve {hash,previousHash, data, timeStamp, nonce} =
"{\"hash\": \"" ^ hash ^"\", \"previousHash\": \"" ^ previousHash ^"\", \"data\": \"" ^ data ^"\", \"timeStamp\": \"" ^ timeStamp ^"\", \"nonce\": \"" ^ nonce ^"\"}";

(* Lista de blocos *)
val blockchain = [block1, block2, block3]; 

(* FUNCAO DE ALTA ORDEM *)
val allBlocks = List.map escreve blockchain;
val fd=String.concatWith ",\n" allBlocks;
val myConcat= "[" ^ fd ^"]";
writeFile "./testando.sml" myConcat;

(* L� arquivo *)
val leituraArquivo = readFile "./testando.sml";

(* Lista de caracteres do arquivo *)
val listaCaracteres = explode leituraArquivo;

(* adiciona string ao final da lista de string *)
fun listaString (listao: string list, pal:string)=
listao@[pal];
(* adiciona char ao final de lista de char *)
fun recebechar (ch:char, chl: char list)=
chl@[ch];

(* Fun��o que formata lista de caracteres e gera uma lista de strings com as informa��es dos blocos *)
(* tamanho da lista, lista, indice, lista *)
fun formata (tam:int, chlist: char list, i: int, stlist: string list)=
if i < tam then
	if (Char.compare(List.nth (listaCaracteres, i), #"{") = EQUAL) orelse (Char.compare(List.nth (listaCaracteres, i),#"\"") = EQUAL)
	then formata(tam, chlist,i+1,stlist) (* chama fun��o de novo*)
	else if Char.compare(List.nth (listaCaracteres, i), #":") = EQUAL then formata(tam,[],i+1,listaString(stlist,implode chlist))
	else if Char.compare(List.nth (listaCaracteres, i), #",") = EQUAL then formata(tam,[],i+1,listaString(stlist,implode chlist))
	else if Char.compare(List.nth (listaCaracteres, i),#"}") = EQUAL then formata(tam,[],i+1,listaString(stlist,implode chlist))
	else if Char.compare(List.nth (listaCaracteres, i),#"\n") = EQUAL then formata(tam,[],i+1,listaString(stlist,implode chlist))
	else formata(tam, recebechar(List.nth (listaCaracteres, i),chlist), i+1,stlist) (* chama fun��o q guarda palavra *)
else stlist;

val dadosarq = formata (List.length listaCaracteres, [], 0,[]);

(* usando arquivo para remontar bloco *)
val block1 = {hash = List.nth (dadosarq, 1), previousHash = List.nth (dadosarq, 3),data = List.nth (dadosarq, 5), timeStamp = List.nth (dadosarq, 7), nonce = List.nth (dadosarq, 9)};


(* minera *)
fun mine ( block: data_block ) = 
    let
    	val difficulty = 10
	val target = "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
	val h = Int.toString(calchash {a= (#hash block), b= timeStampToInt, c= Real.fromInt(nonceToInt)})
	val hash = String.substring(target,0, (126-String.size(h)))^h
	val SOME timeStampToInt= Int.fromString (#timeStamp block)
	val SOME nonceToInt= Int.fromString (#nonce block)
    in
    	if(String.compare(  String.substring(hash,0,difficulty), String.substring(target,0,difficulty)  ) = EQUAL) then {hash = hash, previousHash = (#hash block), data = (#data block), timeStamp = (#timeStamp block), nonce = Int.toString (nonceToInt) }
    	else  mine ( {hash = (#hash block), previousHash = (#previousHash block), data = (#data block), timeStamp = (#timeStamp block), nonce = Int.toString (nonceToInt+1) }) 
    end;
	
fun atualizaLista (blockchain: data_block list)=
blockchain@[mine(List.last blockchain)];

(* FUNCAO DE ALTA ORDEM *)
val allBlocks2 = List.map escreve (atualizaLista (blockchain));
val concat2=String.concatWith ",\n" allBlocks2;
val myConcat2= "[" ^ fd ^"]";
writeFile "./testando.sml" myConcat2;

(*Validacao da cadeia de blocos*)
fun validate (previousBlock = data_block, currentBlock = data_block, i = Int) =
let
	val target = "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
	val difficulty = 10
in
	if(!String.compare ((#hash currentBlock), (calchash {a= (#hash currentBlock), b= timeStampToInt, c= Real.fromInt(nonceToInt)}))) = EQUAL) then false;
	else if(!String.compare ((#hash previousBlock),((#previousHash currentBlock))) = EQUAL) then false
	else if(!String.compare(  String.substring(hash,0,difficulty), target  ) = EQUAL) then false
	
	else if(i < length blockchain) 
	then validate({hash = (#hash previousBlock), previousHash = (#previousHash previousBlock), data = (#data previousBlock), timeStamp = (#timeStamp previousBlock), nonce = Int.toString (nonceToInt+1)}
				  , {hash = (#hash currentBlock), previousHash = (#previousHash currentBlock), data = (#data currentBlock), timeStamp = (#timeStamp currentBlock), nonce = Int.toString (nonceToInt+1)}
				  , i+1)

	else true;


end;

