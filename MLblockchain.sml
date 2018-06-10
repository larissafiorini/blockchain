(* Blockchain *)
val it = "The block chain: " : string

(* Função para cálculo hash *)
fun calchash {a, b, c} = 
(Char.ord (List.nth ((explode a), 0)))
+ b 
+ (Real.floor c);

(* Definição de blocos *)
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
fun escreve {hash,previousHash, data, timeStamp, nonce} = "{\"hash\": \"" ^ hash ^"\", \"previousHash\": \"" ^ previousHash ^"\", \"data\": \"" ^ data ^"\", \"timeStamp\": \"" ^ timeStamp ^"\", \"nonce\": \"" ^ nonce ^"\"}";

val blockToWrite = escreve {hash= #hash block1 ,previousHash= #previousHash block1 , data= #data block1, timeStamp= #timeStamp block1, nonce = #nonce block1 };
writeFile "C:/Users/ESCOLA VILA GRAN/Desktop/testando.sml" blockToWrite;
val blockToWrite2 = escreve {hash= #hash block2 ,previousHash= #previousHash block2 , data= #data block2, timeStamp= #timeStamp block2, nonce = #nonce block2 };
val addSecondBlock = blockToWrite ^ "\n" ^ blockToWrite2;
val blockToWrite3 = escreve {hash= #hash block1 ,previousHash= #previousHash block3 , data= #data block3, timeStamp= #timeStamp block3, nonce = #nonce block3 };
val allBlocks = addSecondBlock ^ "\n" ^ blockToWrite3;
writeFile "C:/Users/ESCOLA VILA GRAN/Desktop/testando.sml" allBlocks;


(* Lista de blocos *)
val blockchain = [block1, block2, block3]; 

(* Lê arquivo *)
val leituraArquivo = readFile "C:/Users/ESCOLA VILA GRAN/Desktop/testando.sml";

(* Divide string por espaço em branco *)
String.tokens Char.isSpace leituraArquivo;
val letra= "a";
val SOME sfdgfd= Char.fromString letra;

String.tokens sfdgfd leituraArquivo;


(* String.tokens testandoag; *)
val listaCaracteres = explode leituraArquivo;
val bob = String.implode listaCaracteres;
val newhope = List.nth (listaCaracteres, 0);

(* tamanho da lista, lista, indice *)
fun montatudo (tam:int, chlist: char list, i: int, stlist: string list)=
if i < tam then
	if (Char.compare(List.nth (listaCaracteres, i), #"{") = EQUAL) orelse (Char.compare(List.nth (listaCaracteres, i),#"\"") = EQUAL)
	then montatudo(tam, chlist,i+1,stlist) (* chama função de novo*)
	else if Char.compare(List.nth (listaCaracteres, i), #":") = EQUAL then montatudo(tam,[],i+1,listaString(stlist,implode chlist))
	else montatudo(tam, recebechar(List.nth (listaCaracteres, i),chlist), i+1,stlist) (* chama função q guarda palavra *)
else stlist;

(* adiciona char ao final de lista de chars *)
fun recebechar (ch:char, chl: char list)=
chl@[ch];

fun listaString (listao: string list, pal:string)=
listao@[pal];

fun conc (listsolta: char list)=
implode listsolta;


conc listaCaracteres;

ch::chl;
recebechar(#"a",[#"b",#"c"]);

val bubu = montatudo (List.length listaCaracteres, [], 0,[]);
val caca = implode bubu;

List.length listaCaracteres;

fun cntrlista (lista:char list, i:int)=
List.nth (listaCaracteres, i)
cntrlista(


(* adiciona lista ys no final da lista xs, combinando as duas em uma única lista *)
fun append (xs:int list, ys:int list)=
	if null xs then ys
	else (hd xs) :: append((tl xs),ys);

fun appendChar (cs: char list, ch: char)=
ch::cs;

if Char.compare(List.nth (listaCaracteres, 1),#"\"") = EQUAL then 1 else 0;

fun recebechar (ch:char, chl: char list)=
ch::chl;



val newhope1 = List.nth (listaCaracteres, 2);
val newhope2 = List.nth (listaCaracteres, 3);
val lista = [newhope1,newhope2];
val bill = String.implode lista;
val fu= lista+List.nth (listaCaracteres, 4);

fun montaPalavra (lista: int, pal: int) =
lista+pal;

val x = [7,8,9];
6::x; (* add elemento no início da lista *)
5::6::x;

null x; (* true se lista vazia *)
val y =[];
null y; 
(* hd -> primeiro elemento da lista *)
(* tl -> resto da lista tirando primeiro elemento *)
hd x; 
tl x;

fun sum_list (xs : int list)=
	if null xs then 0
	else hd xs + sum_list(tl xs);
	
sum_list x;

(* adiciona lista ys no final da lista xs, combinando as duas em uma única lista *)
fun append (xs:int list, ys:int list)=
	if null xs then ys
	else (hd xs) :: append((tl xs),ys);

fun appendChar (cs: char list, ch: char)=
ch::cs;

val listaCaracteres = explode leituraArquivo;
val bob = String.implode listaCaracteres;
val newhope = List.nth (listaCaracteres, 0);
fun fact n= 
if Char.contains leituraArquivo car then 1 else fact(n-1);

fun loop n=
if n=0 then 1 else loop(n-1);
loop 2;

Char.compare(newhope, #"{");
(* compara dois char *)
if Char.compare(newhope, #"{") = EQUAL then 1
else 0;

fun montaPalavra (listaarq : char list, ch: char)=
if Char.compare(newhope, #"{") = EQUAL then 1
else 0;

val z=[10,11];
append (x,z);

val u = montaPalavra (2,1);

val u= [List.nth (listaCaracteres, 2)];
val ba = implode u;
val baa= explode ba;
val bu= [ba, List.nth (listaCaracteres, 3)];

val car = #"{";
Char.contains leituraArquivo car;

val c=2;
val d=0;
while  do
d = 


fun fact n= 
if Char.contains leituraArquivo car then 1 else fact(n-1);

fact 5; 

fun testando palavra=
List