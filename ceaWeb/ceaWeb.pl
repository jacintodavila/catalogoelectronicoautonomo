% *************************************************************************
% * 								ceaWeb.pl                             *
% * Página web principal del Catalogo Electronico Autonomo partir de un   *
% * código con HTML termerizado en Prolog. Proyecto final tesis.          *
% * Este programa es Software Libre. Desarrollado por                     *
% * Julio Jaimes y Jacinto Davila, basándose la app CEA dise#ada por      *
% * por Jacinto Davila y Yaritza Vargas                                   *
% *************************************************************************


% **********************************************************************
% Modulos necesarios para el proyecto
% **********************************************************************

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(www_browser)).
:-use_module(library(lists)).

% http_reply_from_files is in here
:- use_module(library(http/http_files)).

:- multifile http:location/3.
:- dynamic   http:location/3.
http:location(files, '/f', []).
:- http_handler(files(.), http_reply_from_files(indices, []), [prefix]).


:- use_module(library(http/http_error)).
:- use_module(library(http/js_write)).
:- use_module(library(http/http_client)).

:- http_handler(css('css/bootstrap.css'), http_reply_file('css/bootstrap.css', []), []).
%:- http_handler(css('css/bootstrap.min.css'), http_reply_file('css/bootstrap.min.css', []), []).
:- http_handler(js('js/bootstrap.js'), http_reply_file('js/bootstrap.js', []), []).
:- http_handler(js('js/jquery.min.js'), http_reply_file('js/jquery.min.js', []), []).
%:- http_handler(images('images/educerecabecera.png'), http_reply_file('images/educerecabecera.png', []), []).

:- http_handler(css('css/myStyles.css'), http_reply_file('css/myStyles.css', []), []).

:- html_resource(jquery, [virtual(true),
	   requires('https://ajax.googleapis.com/ajax/libs/jquery/2.1.3/jquery.min.js')]).
:- html_resource(bootstrap_js, [virtual(true), ordered(true),
	    requires([jquery, 'http://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css'])]).


:- html_resource(js('js/bootstrap.js'), [requires(js('js/jquery.min.js'))]).


http:location(css, root(css), []).
http:location(js, root(js), []).
%http:location(images, root(images), []).
http:location(fonts, root(fonts), []).
http:location(css, root(css), []).


browser('BROWSER', firefox).

% **********************************************************************

:- http_handler('/', say_hi, []).

server(Port) :-
	http_server(http_dispatch, [port(Port)]).

say_hi(Request) :-

	reply_html_page(
	   [title('CEA: Catalogo Electronico Autonomo.')],
	    [\page_content(Request)]).

% **********************************************************************
% Contenido de la página principal.
% **********************************************************************

page_content(_Request) -->
	html(
	   [

	    \html_requires(jquery),
	    \html_requires(bootstrap_js),
		\html_requires(css('css/bootstrap.css')),
		%\html_requires(css('css/bootstrap.min.css')),

		\html_requires(js('js/jquery.min.js')),
		\html_requires(js('js/bootstrap.js')),

	    \head,
	    \enlaces


     ]).

% **********************************************************************
% Mediante "head" se incluye una cabecera a la página, con la imagen
% de educere la revista venezolana de educacion.
% **********************************************************************

head --> html(
			header(
				[div( class='img-responsive container row col-xs-12 col-sm-12 col-md-12 col-lg-12',
					img([src='https://s32.postimg.org/rclj1hved/educerecabecera.jpg', class='img-responsive',
					% img([src='images/educerecabecera.jpg', class='img-responsive',
					 style='float: center; margin: 0 auto;'])

					),

					h1([ align='center' ],'CEA: Catálogo Electrónico Autónomo'),
 					p([ align= 'center'], script(' document.write(Date())') )


			])).

% **********************************************************************
% Mediante "enlaces" se incluye el código que genera la barra de
% navegación con el menú desplegable que contiene los enlaces a los
% otros temas segun la necesidad del usuario.
% **********************************************************************

enlaces --> html(body([

		\barra_nav,
		\busqueda,
		\foot

	])).

% **********************************************************************
% Definicion de la barra denavegación con el menú desplegable que
% contiene los enlaces a los otros temas segun la necesidad del usuario.
% **********************************************************************

barra_nav --> html(nav(

	[class="navbar navbar-inverse", method='POST'],
	[

	div([class='container-fluid'],
		[
			div([class='navbar-header'],[
				a([class='navbar-brand'],'CEA *************************')
				]),

			ul([class='nav navbar-nav', class='col-md-4'],[

				li(form([style='align: left; margin: 0px 0px 0px 100px;',action='/f/leeme.html', method='POST'],
						input([name='submit', type='submit', class='btn btn-md btn-success btn-block', value='Manual']))),
				li(form([style='align: left; margin: 0px 0px 0px 100px;',action='/guardar', method='POST'],
						input([name='submit', type='submit', class='btn btn-md btn-success btn-block', value='Guardar']))),
				li(form([style='align: left; margin: 0px 0px 0px 100px;',action='/f/index.html', method='POST'],
						input([name='submit', type='submit', class='btn btn-md btn-success btn-block', value='Indices']))),
				li(form([style='align: left; margin: 0px 0px 0px 100px;',action='/abrir', method='POST'],
						input([name='submit', type='submit', class='btn btn-md btn-success btn-block', value='Abrir']))),
		        li(form([style='align: left; margin: 0px 0px 0px 100px;',action='/f/leeme.html'],

		        	input([name= 'submit', type='submit', class='btn btn-md btn-success btn-block', value='Manual'])))
				]),

		div([class='navbar-header'],[
				a([class='navbar-brand',href='ula.com'],'CEA *************************')
				])

		])])).

% **********************************************************************
% Definicion de la busquedad web con su barra de consultar y salida.
% **********************************************************************

busqueda --> html(div(form([style='align: left; margin: 0px 0px 0px 100px;',action='/buscar', method='POST'], [



		div([class='container-fluid'],[

			p(div([class='form-group col-xs-11 col-sm-11 col-md-11 col-lg-11',style='margin:0px;'],
			  h4(class='venacti', b('Ingrese la busqueda a realizar'))
			 )),
			p(div(class='col-xs-11 col-sm-11 col-md-11 col-lg-11', html(\['<hr></hr>']))),

			p(div(class='form-group col-xs-11 col-sm-11 col-md-11 col-lg-11',

			  input([name=entrada, class='form-control input-md',type=textarea, placeholder='Informacion a buscar'])
			      )),

			p(div([class='form-group col-xs-11 col-sm-11 col-md-11 col-lg-11',style='margin:0px;'],
			  h4(class='venacti',b('Nombre del archivo de salida'))
			 )),
			p(div(class='form-group col-xs-11 col-sm-11 col-md-11 col-lg-11',

			    input([name=salida, class='form-control input-md', type=textarea, placeholder=' ejemplo: Salida'])
			      )
			 ),


			p(div(class='center',
		  input([name=submit, type=submit, class='btn btn-md btn-success btn-block', value='Buscar'])))



	      ])]))).

:- http_handler('/buscar', landing_pad, []).

landing_pad(Request) :-
        member(method(post), Request), !,
        http_read_data(Request, Data, []),

		procesa(Data, E2, S2),

		buscar(E2,S2,Ans),

		reply_html_page(

		    [title('Consulta')],
		    [\page_content1([Ans,S2],Request)]).

% **********************************************************************
% Procesa la informaicon introducida por el usuario.
% **********************************************************************

procesa(Data, Entrada, Salida) :-
	member(entrada=Pre, Data),
	member(salida=Res, Data),
	(Pre\='' -> read_term_from_atom(Pre, Entrada, []) ; true),
	(Res\='' -> read_term_from_atom(Res, Salida, []) ; true).

page_content1([Ent,_S2],_Request) -->
	html(
	   [

	    \html_requires(jquery),
	    \html_requires(bootstrap_js),
		\html_requires(css('css/bootstrap.css')),
		%\html_requires(css('css/bootstrap.min.css')),
		\html_requires(js('js/jquery.min.js')),
		\html_requires(js('js/bootstrap.js')),
	    \head,

	    div(class='container-fluid',[
		form([style='align: left; margin: 0px 0px 0px 100px;',action='javascript:history.back();', method='POST'], [
		    \['<br></br>'],
			div([class='text-center'],[
		 	 p([style='font-size: 20pt', title='Entrada'], b('Resultados')),
	  			table([ align(center),
	                       border(1),
	                       width('100%')
	                     ],
	                     [ tr([ th('Relevancia'),
	                            th('Archivo (URL implicito)'),
				    th('Tamano (en Bytes)')
	                          ])
	                     | \web_rows(Ent)
	                     ]),



			\['<br></br>'],
		    \['<br></br>'],
		    p(div( class='center',
		    input([name=submit, type=submit, class='btn btn-md btn-success btn-block', value='Volver'])))])
	    	]),

			\['<br></br>'],
			\['<br></br>'],
			\foot

	    ])]).

% **********************************************************************
% Header de los resultados de busquedad.
% **********************************************************************
header -->
	html([ div(''),
		table([ align(center),
              border(1),
              width('100%')
              ],
             [tr(
                [th('Relevancia'),  th('Archivo'), th(tamano)]
                 )
             ]
        )]).


% **********************************************************************
% Mediante "foot" se genera el pie de la página. En esta sección se
% presenta un botón para subir a la parte de arriba de la página, los
% datos de los desarrolladores de la misma y, por último, la fecha y
% hora actual.
% **********************************************************************
foot --> html(footer(
	[

		div([class='text-center'],'Catalogo Electrónico Atónomo © 2016.'),
		div([class='text-center'],'Diseñado por Julio Jaimes R. y Jacinto Davila.'),
		p([ align= 'center'], script(' document.write(Date())') ),
		(div([id='subir',class='text-right'],
		input([name='img-subir', type=image,  src='http://s15.postimg.org/82q42vo5j/icon_top.png', onclick='window.location.href = "#top";', alt='Subir'])))
   ])).



% **********************************************************************
% Lists
% **********************************************************************

modules([]) --> [].
modules([H|T]) --> module(H), modules(T).

module([P1,P2,P3,P4]) -->
html(tr([td(P1),td(a(href(P2),P3)), td(P4)])).


table(T) -->
     html(table([tr([th("Urls a la busquedad")]),rows(T)])).


rows([]) --> [].
rows([H |T]) -->
      html(tr(td(H))),
      rows(T).


	/****wrrte to list a terminal****/

lista([]).
lista([T|R]) :-
writeln(T), lista(R).


% **********************************************************************
% Implementation
% @browser and @query son variables especiales
% **********************************************************************

buscar(Desc,_Path,Ans) :-

    search_index(Desc, Reporte), % Reporte = (Head, Answers)
    guarda_reporte(Reporte), % corrige y guarda reporte
    reporte_cea((_H,Answers)), % recupera
    recorta(50, Answers, Ans).


guardar(Path) :-

    object(Path, PathFile),
    reporte_cea(Reporte) ->
    ( ( save_file(PathFile, Reporte),
      abre_navegador(PathFile) )
    ; true ).

    save_file(PathFile, Reporte) :-
	catch( (
		open(PathFile, write, Stream, [alias(reportecea)]),
		with_output_to(reportecea, cea:html_out(Reporte)),
		close(Stream)
	), E, writeln(E)).

abre_navegador(URL) :-
	browser(B, N), setenv(B,N),
	absolute_file_name(URL, TURL),
	catch(www_open_url(TURL), E, writeln(E)).

html_out((Head, Body)) :-
        phrase(web_page(Head, Body), Tokens),
        print_html(Tokens).

html_table_out((Head, Body)) :-
        phrase(web_table(Head, Body), Tokens),
        print_html(Tokens).

html_pairs_out(AttribsValues) :-
        phrase(web_table_pairs(AttribsValues), Tokens),
        print_html(Tokens).

%       emit page with title, header and table of matches

web_page((Query, Hits), Body) -->
        page([ title(['Busqueda de: ', Query])
             ], \web_table((Query, Hits), Body)
             ).

web_table((Query, Hits), Body) -->
	html([ div(''),
	       % img([src='./imagenes/educerecabecera.jpg', alt='EDUCERE-ULA'], []),
	       h2(align(center), ['La frase: ', Query]),
	       h3(align(center), ['Produjo ', Hits, ' resultados']),
               table([ align(center),
                       border(1),
                       width('100%')
                     ],
                     [ tr([ th('Relevancia'),
                            th('Archivo (URL implicito)'),
			    th('Tamano (en Bytes)')
                          ])
                     | \web_rows(Body)
                     ])
             ]).

web_table_pairs(Body) -->
	html([ div(''),
		%h2(align(center), ['La frase: ', Query]),
		%h3(align(center), ['Produjo ', Hits, ' resultados']),
               table([ align(left),
                       border(2),
                       width('100%')
                     ],
                     [ tr([ th('Registros enlazados a sus archivos')
                            %th('Archivo (URL implicito)'),
			 %   th('TamaÃ±o (en Bytes)')
                          ])
                     | \web_pairs(Body)
                     ])
             ]).

%       emit the rows for the body of the table.

web_rows([]) -->
        [].
web_rows([(Rank, Url, File, Size)|T]) -->
        html([ tr([ td(Rank),
		    td(\predref(Url/File)),
                    td(em(Size))
                  ])
             ]), % { writeln((Rank, Url, File, Size)) },
        web_rows(T).

web_pairs([]) -->
        [].
web_pairs([(Author, File)|T]) -->
        html([ tr([ td(\predref(File/Author))
                  ])
             ]), % { writeln((Author, File)) },
        web_pairs(T).

%       predref(Url/File)
%
%       Emit File as a hyperlink to Url
%
%       we must do form-encoding for the name as it may contain illegal
%       characters.  www_form_encode/2 is defined in library(url).

predref(Url/File) -->
        { www_form_encode(Url, Encoded),
           sformat(Href, '~w', [Encoded]),
	  sformat(EFile, '~w', [File])
        },
        html(a(href(Href), [EFile])).


   /* Processing information from the indexer */
search_index(Query, Reporte) :-
	retractall(salida(_)),
	( current_prolog_flag(windows, true) ->
          catch(call_winOS('swishe.bat ', [Query], Output), Excep, writeln(Excep)) % call to Windows
	; catch(call_unix_temp('swish-e', [' -f ./indices/index.swish-e -w ', Query], Output), Excep, writeln(Excep)) % call to Linux
	),
	assert(salida(Output)),
	reporte(Reporte,Output,_Rest).


guarda_reporte((H, Body)) :-
   corrige(Body, Corregido), % corrige camino al pdf
   retractall(reporte_cea(_)), assert(reporte_cea((H,Corregido))).

corrige([], []).
corrige([(Rank,Url,File,Size)|Rest], [(Rank, NewUrl, File, Size)|ORest] ) :-
	ajusta_camino(Url, NewUrl),
	corrige(Rest, ORest).

ajusta_camino(Url, New) :-
	name(Url, [_|RestUrl]),
	name(NoDot, RestUrl),
	concat('f', NoDot, New).

	% would it run on a dvd?
call_unix_temp(Utl, Args, Output) :-
	tmp_file_stream(utf8, File, Stream),
	close(Stream),
	%File = 'temporal.tmp',
	%(exists_file(File) -> delete_file(File) ; true),
	append([Utl|Args], [' > ', File], Lista),
	concatena(Lista, Command),
         shell(Command, _Status),
	new(F, file(File, utf8)),
	send(F, open, read),
	new(OS, string),
	repeat,
	(
	get(F, read_line, Line)
	-> send(OS, append, Line),
	fail
	;
	!
	),
	pce_string_to_list(OS, Output),

	delete_file(File).

call_winOS(Command, Args, Output) :-
	append([Command|Args], ['salidaswishe.txt'], Lista),
	concatena(Lista, ActualCommand),
        shell(ActualCommand),
	open('salidaswishe.txt', read, Stream),
	read_lines(Stream, Lines),
	concatenart(Lines, Atoms),
	atom_string(Atoms, String),
	string_codes(String, Output),
	close(Stream).


concatena([], '').
concatena([U|R], Comando) :-
	concatena(R, RComando),
	concat(U, ' ', PrevioU),
	concat(PrevioU, RComando, Comando).

	concatenart([], '').
concatenart([U|R], Comando) :-
	concatenart(R, RComando),
	concat(U, '\n', PrevioU),
	concat(PrevioU, RComando, Comando).

recorta(_, [], []).
recorta(0, _, []).
recorta(N, [F|R], [F|RR]) :- NN is N - 1, recorta(NN, R, RR).

% gramatica de la salida del indizador

reporte((Head,Data)) --> encabezado(Head), cuerpo(Data), {!}.

encabezado((Consulta, Hits)) --> formato, consulta(Query), stopwords, resultados(H),
	{name(Consulta, Query), name(Hits, H), !}.

% resultados(0) --> izq, end_of_line, period, {!}.

resultados(H) --> hits(H), searchtime, runtime, {!}.

formato --> izq, colon, texto(_), end_of_line.

consulta(T) --> izq, colon, whites, texto(T), end_of_line.

stopwords --> izq, colon, texto(_S), end_of_line.
	%{name(NF, S), write(' ->'), write(NF), write('<-')}.

hits(H) --> izq, colon, whites, texto(H), end_of_line.

searchtime --> izq, colon, texto(_ST), end_of_line . %se cambio la variable ST a _ST

runtime --> izq, colon, texto(_RT), end_of_line . % se cambio la variable RT a _RT

cuerpo([]) --> period, {!}.
cuerpo([I|R]) --> one_entry(I), cuerpo(R).

one_entry((Rank,U,File,S)) -->
  rank(R), whites, url(URL), whites, filename(F), whites, size(Size), end_of_line,
  {name(Rank, R), name(U, URL), name(File, F), name(S, Size),!}.

rank(R) --> texto(R).

url(URL) --> texto(URL).

filename(F) --> comilla, anything(F), comilla.

anything([]) --> [].
anything([C|R]) --> [C], {not(name('"',[C]))}, anything(R).

size(S) --> texto(S).

texto([A]) -->  alfanumerico(A).
texto([A|R]) --> alfanumerico(A), texto(R).

izq_unit --> [C],  % cualquier cosa menos : y fin de lÃ­nea o de texto
   {not(name(':',[C])),not(name('\r',[C])),
    not(name('\n',[C])),not(name('.', [C])),
    not(char_type_char(C, fin, _))}.

izq --> izq_unit.
izq --> izq_unit, izq.

alfanumerico(C) --> [C],
   {not(name('\n',[C])),not(name('\r',[C])),
    (char_type_char(C, alfa, _);char_type_char(C,especial,_))}.

whites --> blanco.
whites --> blanco, whites, {!}.

blanco --> [C], {char_type_char(C, blanco,_)}.

end_of_line --> [C], {name('\n',[C]);name('\r',[C])}.

colon --> [C], {name(':',[C])}.

period --> [C], {name('.', [C])}.

comilla --> [C], {name('"', [C])}.

error --> texto(T), {name('err', T)}.

/* Tomado del resumidor.sourceforge.net */

% leer_atomos(-Atomos,-AtomosUpper,-ProximoC)
% Lee una lÃ­nea del texto, separÃ¡ndola en una lista de Ã¡tomos lower-case
% y upper-case respectivamente.

leer_atomos(BE, BS, Atomos, AtomosUpper, ProximoC) :-
    leer_caracter(BE, BN, PrimerC, PrimerCUpper, PrimerT),
    leer_oracion(BN, BS, PrimerC, PrimerCUpper, PrimerT, Atomos, AtomosUpper, ProximoC).

% leer_oracion(+PrimerC,+PrimerCUpper,+PrimerT,-Lista,-ListaUpper,-ProximoC)
% Dado el primer caracter lower y upper case, respectivamente, ademÃ¡s
% del tipo de caracter correspondiente retorna la lista de palabras de
% la oraciÃ³n. La oraciÃ³n esta delimitada por cualquier caracter de fin,
% en especial el punto [46].

leer_oracion(B, B, Caracter,Caracter,fin,[],[],Caracter) :- !.

leer_oracion(B, B, 46,46,especial,[],[],46) :- !.

leer_oracion(BE, BS, _,_,blanco,Atomos,AtomosUpper,ProximoC) :-
    !,
    leer_atomos(BE, BS, Atomos,AtomosUpper,ProximoC).

leer_oracion(BE, BS, PrimerC,PrimerCUpper,Type,[PrimerC|Atomos],[AUpper|AtomosUpper],ProximoC) :-
    !, (Type=especial;Type=alfa),
    % name(A,[PrimerC]),
    name(AUpper,[PrimerCUpper]),
    leer_atomos(BE, BS, Atomos,AtomosUpper,ProximoC).

%% Tokenizer modificado por Jacinto DÃ¡vila - 2012 Junio 21
%% tomado de
% et.pl - M. Covington      2003 February 12
% etu.pl - Modified for Unicode - Donald Rogers     2006 July 17
%          email: dero9753@ihug.co.nz
%          Modified to cope with comma in numbers   2006 July 20

% ET the Efficient Tokenizer

%%
%% Character classification
%%

% char_type_char(+Char,-Type,-TranslatedChar)
%   Classifies all characters as letter, digit, special, etc.,
%   and also translates each character into the character that
%   will represent it, converting upper to lower case.
% modified to handle a code as input directly :JD

char_type_char(Code,Type,Tr) :-
   atom_chars(Char, [Code]),
   char_table(Char,Type,Tr),
   !.

% Donald changed this from special to letter.
% Using downcase_atom saves having an enormous table
% and should handle all languages.
% letter -> alfa
char_type_char(Char,alfa,Char) :-
   atom_chars(_,[Char]).
   %downcase_atom(L2,L3),
   %atom_chars(L3,[Char2]).

% End of line marks
% eol -> fin
char_table(end_of_file, fin, end_of_file).
char_table(-1, fin, end_of_file).
%char_table('\n',        fin, '\n'       ).

% Whitespace characters
% whitespace -> blanco
char_table(' ',     blanco,  ' ').     % blank
char_table('\t',    blanco,  ' ').     % tab
% char_table('\r',    blanco,  ' ').     % return
char_table('''',    blanco, '''').     % apostrophe does not translate to blank
% char_table('\n',    blanco, '\n').

% Donald removed the letter characters and replaced them by special characters.
% There are too many Unicode letters to put them all in a table.
% The third parameter may be useless, but maybe someone will want to convert
% some of the special characters.
% There may be other Unicode characters that need to be added.
% special -> especial
char_table('~',     especial,    '~' ).
char_table('`',     especial,    '`' ).
char_table('!',     especial,    '!' ).
char_table('@',     especial,    '@' ).
char_table('#',     especial,    '#' ).
char_table('$',     especial,    '$' ).
char_table('\u0025',especial,    '\u0025' ). %
char_table('^',     especial,    '^' ).
char_table('&',     especial,    '&' ).
char_table('*',     especial,    '*' ).
char_table('(',     especial,    '(' ).
char_table(')',     especial,    ')' ).
char_table('_',     especial,    '_' ).
char_table('-',     especial,    '-' ).
char_table('+',     especial,    '+' ).
char_table('=',     especial,    '=' ).
char_table('{',     especial,    '{' ).
char_table('[',     especial,    '[' ).
char_table('}',     especial,    '}' ).
char_table(']',     especial,    ']' ).
char_table('|',     especial,    '|' ).
char_table('\\',    especial,    '\\' ).
char_table(':',     especial,    ':' ).
char_table(';',     especial,    ';' ).
char_table('"',     especial,    '"' ).
char_table('<',     especial,    '<' ).
char_table(',',     especial,    ',' ).
char_table('>',     especial,    '>' ).
char_table('.',     especial,    '.' ).
char_table('?',     especial,    '?' ).
char_table('/',     especial,    '/' ).

% Digits
% digit -> alfa ; Ojo, corregir
char_table('0',   alfa,     '0' ).
char_table('1',   alfa,     '1' ).
char_table('2',   alfa,     '2' ).
char_table('3',   alfa,     '3' ).
char_table('4',   alfa,     '4' ).
char_table('5',   alfa,     '5' ).
char_table('6',   alfa,     '6' ).
char_table('7',   alfa,     '7' ).
char_table('8',   alfa,     '8' ).
char_table('9',   alfa,     '9' ).

% Everything else is a letter character.


% fin cea

pce_string_to_list(S, L) :-
	pce_string_to_list(S, 0, L).
pce_string_to_list(S, I, [C|T]) :-
	get(S, character, I, C), !,
	NI is I + 1,
	pce_string_to_list(S, NI, T).
pce_string_to_list(_, _, []).


?- server(5000).


