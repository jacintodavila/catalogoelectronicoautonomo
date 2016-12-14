% pxml.pl
% procesos xml cea
%    Copyright (C) 2013-2016 Jacinto Dávila <jacinto@ula.ve> and Yaritza Vargas <vargas.yaritza@gmail.com>
%
%    This program is free software as the rest of catalogoelectronicoautonomo or cea:
%    you can redistribute it and/or modify
%    it under the terms of the GNU Affero General Public License as
%    published by the Free Software Foundation, either version 3 of the
%    License, or (at your option) any later version.
%
%    This program is distributed in the hope that it will be useful,
%    but WITHOUT ANY WARRANTY; without even the implied warranty of
%    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%    GNU Affero General Public License for more details.
%
%    You should have received a copy of the GNU Affero General Public License
%    along with this program.  If not, see <http://www.gnu.org/licenses/>.
%
% We are grateful to EDUCERE and InMemoriAN at Universidad de Los Andes and FONACIT, Venezuela for their data and support.

:- use_module(library(sgml)).
:- use_module(cea).

carga_xml(Meta) :-
  retractall(metadata(_)),
  load_structure('consulta_educere_solo_campos_seleccionados_12062014_handle.xml', Meta, [dialect(xml)]),
  assert(metadata(Meta)).

genera_meta(File) :-
	metadata(X), meta_master(X, Meta),
	%('Titulo', 'Subtitulo', 'Autores', 'URL', 'Fecha' )|
	open(File, write, Out, [encoding(utf8)]),
	% write(Out,'Num'), write(Out, '; '),
	write(Out,'Titulo'), write(Out, '; '),
	write(Out,'Subtitulo'), write(Out, '; '),
	write(Out,'Autores'), write(Out, '; '),
	write(Out,'URL'), write(Out, '; '),
	write(Out,'Fecha'), write(Out, '\n'),
        forall(member((N,T,S,A,F), Meta),
		( write(Out, N), write(Out, '; '),
		  write(Out, T), write(Out, '; '),
		  write(Out, S), write(Out, '; '),
		  write(Out, A), write(Out, '; '),
		  write(Out, F), write(Out, '\n') ) ),
	close(Out).


genera_xml_site(FileXml) :-
	metadata(X), meta_master(X, Records),
	sort(Records, Ordenados),
	open(FileXml, write, Stream, [encoding(utf8), alias(sitemap)]),
  writeln(sitemap, '<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.sitemaps.org/schemas/sitemap/0.9 http://www.sitemaps.org/schemas/sitemap/0.9/sitemap.xsd">\n'),

	forall(member((T,S,A,_,F), Ordenados), (
		( cea:search_index(T, Resultados) ->
	      true ; Resultados = ((T,0),[])
	    ), print_record_site(sitemap, T,S,A,F,Resultados)
		)),

	writeln(sitemap, '</urlset>\n'),
	close(Stream).

print_record_site(_,_,_,_,_, ((T,0),[])).

print_record_site(Sitemap, T,S,A,F,Resultados) :-
  writeln(Sitemap,'<url>'),
	write(Sitemap,'<title>'),
	write(Sitemap,T),
    writeln(Sitemap,'</title>'),

	write(Sitemap,'<author>'),
    write(Sitemap, A),
    writeln(Sitemap,'</author>'),

	write(Sitemap,'<subtitle>'),
    write(Sitemap, S),
    writeln(Sitemap,'</subtitle>'),

	write(Sitemap,'<lastmod>'),
    write(Sitemap, F),
    writeln(Sitemap,'</lastmod>'),
   	write(Sitemap,'<loc>'),
   	writeln(Sitemap,'\n'),

forall(
    ((_T, _), [(_Rank,Path,_File,_Size)])= Resultados,
    ( escribir_url(Path, URL),
      writeln(Sitemap, URL) )
),

	write(Sitemap,'</loc>'),
    writeln(Sitemap,'</url>').


escribir_url(P, U) :-
    P\=[],
	name(P, [_|RestUrl]),
	name(NoDot, RestUrl),
	concat('http://localhost:5000/indices/f', NoDot, U).

escribir_url(_P,' - ').




genera_meta_autores(File) :-
	autores_archivo(Meta),
	open(File, write, Out),
        forall(member((A,F), Meta), csv_write_stream(Out, [row(A, F)], [separator(0';)])).

%%
% genera_autores/1 produce los archivos de indices de autores y el principal en File
genera_autores(File) :-
	metadata(X), get_all_authors(X, Ordenados),
	retractall(metadata(_)),
	retractall(autores_archivo(_)),
	asigna_archivos(Ordenados, ConArchivo),
	assert(autores_archivo(ConArchivo)),
	print_authors(0, ConArchivo),
	print_index_authors(File, ConArchivo).

get_all_authors(Meta, Authors) :-
	Meta =  [element(data, [],
   		['\n\t', element(header, [], _), '\n\t',
            		element(records, [], Records), '\n'])],
	findall(A, (get_author(Records, A), not(filtra(A))), Autores),
	sort(Autores, Authors).

%filtra(['Anonimo']).
filtra([-]).

% print_authors: imprime la lista de autores con sus pdfs asociados

% print_authors(5,_). % set to 1 to test

print_authors(_, []).

print_authors(N, [([Autor], File)|Records]) :-
   	NN is N + 1,
   	open(File, write, Stream, [encoding(utf8), alias(index_author)]),
	write(index_author, '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/transitional.dtd">\n<html><head>\n\t<meta http-equiv="CONTENT-TYPE" content="text/html; charset=utf-8">\n\t<link rel="stylesheet" type="text/css" href="estilos.css" />\n\t\t<title> Indice de '),
	write(index_author, Autor),
	write(index_author, ', como autor(a) en EDUCERE</title>\n'),
	write(index_author, '</head><body>\n<div id="container">\n<div id="pageHeader">\n</div>\n\t<div id="pageContehome">\n'),
	write(index_author, '<center><big><a href="../index.html">Indices InMemoriAn y EDUCERE</a><br></big></center>\n'),
   	write(index_author, '<entry>\n'),
   	write(index_author, '\t<number>\n'),
   	write(index_author,NN),
   	write(index_author, '\t</number>\n'),
   	write(index_author, '\t<results>\n'),
   	concat('\"', Autor, CAutor),
   	concat(CAutor, '\"', CAutorC),
   	write(index_author, '\t<authors>\n'),
   	write(index_author,CAutorC),
   	write(index_author, '\t</authors>\n'),
   	( cea:search_index(CAutorC, Resultados) ->
	  true ;
	  Resultados = ((CAutorC,0),[])
	), !,
   	write(index_author, '\t<results>\n'),
   	with_output_to(index_author, cea:html_table_out(Resultados)),
   	% write(Resultados),
   	write(index_author, '\t</results>\n'),
   	write(index_author, '</entry>\n'),
   	write(index_author, '</div>\n<div id="footer"><p>InMemorian (Proyecto FONACIT) y EDUCERE. La Revista Venezolana de Educación Escuela de Educación. <br/>\nUniversidad de Los Andes,<br/>\nMérida - Venezuela<br/>\n<br/>\n<br/>\n</p>\n</div>\n</div>\n'),
	write(index_author, '</body>\n'),
	write(index_author, '</html>\n'),
	close(Stream),
   	print_authors(NN, Records).

print_index_authors(File, AutoresArchivos) :-
	open(File, write, Stream, [encoding(utf8), alias(index_authors)]),
	write(index_authors, '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/transitional.dtd">\n<html><head>\n\t<meta http-equiv="CONTENT-TYPE" content="text/html; charset=utf-8">\n\t<link rel="stylesheet" type="text/css" href="estilos.css" />\n\t\t<title>Indice de Autores de EDUCERE</title>\n \n'),
	write(index_authors, '</head><body>\n<div id="container">\n<div id="pageHeader">\n</div>\n\t<div id="pageContehome">\n'),
	write(index_authors, '<center><big><a href="../index.html">Indices InMemoriAn y EDUCERE</a><br></big></center><a href="#" onclick="find();">Buscar en esta p&aacute;gina</a>\n'),
	with_output_to(index_authors, cea:html_pairs_out(AutoresArchivos)),
	write(index_authors, '</div>\n<div id="footer"><p>InMemorian (Proyecto FONACIT) y EDUCERE. La Revista Venezolana de Educación Escuela de Educación. <br/>\nUniversidad de Los Andes,<br/>\nMérida - Venezuela<br/>\n<br/>\n<br/>\n</p>\n</div>\n</div>\n'),
	write(index_authors, '</body>\n'),
	write(index_authors, '</html>\n'),
	close(Stream).

selecciona_autores([],A,A).
selecciona_autores([(_,_,_,A,_,_)|R], Vienen, Todos ) :-
	append(Vienen, A, Siguientes),
	selecciona_autores(R, Siguientes, Todos).

asigna_archivos(Ordenados, Completos) :-
	asigna_archivos(0, Ordenados, Completos).

asigna_archivos(_, [], []).
asigna_archivos(N, [Autor|Rest], [(Autor, Archivo)|RR] ) :-
	NN is N + 1,
	format(atom(Archivo), 'a~`0t~d~6|.html', [NN]),
	asigna_archivos(NN, Rest, RR).

genera_maestro_autores(File, [element(data, [],
   		['\n\t', element(header, [], _), '\n\t',
            		element(records, [], Records), '\n'])]) :-
	print_authors_master(0, Records, [], Lista),
	open(File, write, Out),
        forall(member((NN, Autor, Archivo), Lista), csv_write_stream(Out, [row(NN, Autor, Archivo)], [separator(0';)])).

print_authors_master(_, [], _, []).

print_authors_master(N, Records, Acum, [(NN, Autor, Archivo)|Resto]) :-
   dame_un_record(Records, Rest, Primero),
   ( ( get_author(Primero, [Autor]), not(member(Autor, Acum)) ) ->
   	( NN is N + 1,
	  format(atom(Archivo), 'a~`0t~D~6|.html', [NN]),
   	  writeln((NN, Autor, Archivo)),
           NAcum = [Autor|Acum],
           ! ) ;
	( NN = N, NAcum = Acum )
   ), !,
   print_authors_master(NN, Rest, NAcum, Resto).

print_authors_master(N, [_|Rec], Acum, R) :-
   print_authors_master(N, Rec, Acum, R).


p_a(File, [element(data, [],
   		['\n\t', element(header, [], _), '\n\t',
            		element(records, [], Records), '\n'])]) :-
   	% writeln(Header),
	open(File, write, Stream, [encoding(utf8), alias(index_authors)]),
	write(index_authors, '<!DOCTYPE html>\n'),
	write(index_authors, '<html>\n'),
	write(index_authors, '<head>\n'),
	write(index_authors, '    <meta http-equiv="Content-Type" content="text/html;charset=UTF-8">\n'),
	write(index_authors, '<title>Indice de Autores</title>\n'),
	write(index_authors, '</head>\n'),
	write(index_authors, '<body>\n'),
	print_authors(0, Records),
	write(index_authors, '</body>\n'),
	write(index_authors, '</html>\n'),
	close(Stream).

pretty_print([element(data, [],
   		['\n\t', element(header, [], _), '\n\t',
            		element(records, [], Records), '\n'])]) :-
   	% writeln(Header),
	print_records(0, Records).

print_records(_,[]).

print_records(N, Records) :-
   NN is N + 1,
   dame_un_record(Records, Rest, Primero),
   write('#:'), write(NN),
   %write('Primero:'), writeln(Primero),
   get_author(Primero, [Autor]),
   write(' Autor:'), write(Autor),
   get_title(Primero, [Title]),
   write(' Titulo:'), write(Title),
   ((get_subtitle(Primero, [STitle]),  write('SubTitulo:'), write(STitle)); true),
   get_handle(Primero, [URL]),
   write(' URL:'), write(URL), nl,
   print_records(NN, Rest).

print_records(N, [_|R]) :- print_records(N, R).

%%
% genera_titulos
genera_titulos(File) :-
	metadata(X), meta_master(X, Records),
	sort(Records, Ordenados),
  	% [(Title, STitle, Autores, URL, Fecha )|Resto] )
	%retractall(metadata(_)),
	asigna_archivos_titulos(Ordenados, Pares, Completos),
	print_titles(0, 0, 1500, Completos), % hasta 1500 titulos
	print_index_titles(File, Pares).

genera_titulos(N, M) :-
	metadata(X), meta_master(X, Records),
	sort(Records, Ordenados),
  	% [(Title, STitle, Autores, URL, Fecha )|Resto] )
	% retractall(metadata(_)),
	asigna_archivos_titulos(Ordenados, _Pares, Completos),
	print_titles(0, N, M, Completos).

genera_titulos_indice(File) :-
	metadata(X), meta_master(X, Records),
	sort(Records, Ordenados),
  	% [(Title, STitle, Autores, URL, Fecha )|Resto] )
	%retractall(metadata(_)),
	asigna_archivos_titulos(Ordenados, Pares, _Completos),
	print_index_titles(File, Pares).

meta_master([element(data, [],
   		['\n\t', element(header, [], _), '\n\t',
            		element(records, [], Records), '\n'])], Meta) :-
   	% writeln(Header),
	collect_meta(0, Records, OMeta),
	elimina_sin_titulo(OMeta, Meta).

collect_meta(_,[], []).

collect_meta(N, Records,
  [(Title, STitle, Autores, URL, Fecha )|Resto] ) :-
   NN is N + 1,
   dame_un_record(Records, Rest, Primero),
   % write(Primero), nl,
   findall(A, get_author(Primero, A), Auto),
   ( Auto\=[] -> Autores = Auto ; Autores=['Anonimo']),
   ( get_title(Primero, [T]) -> Title = T ; Title=' -- '),
   ( get_subtitle(Primero, [S]) -> STitle = S; STitle=' -- '),
   ( get_handle(Primero, [U]) -> URL = U ; URL='No en Web'),
   ( get_date(Primero, [D]) -> Fecha = D; Fecha='1997-01-01'),
   % writeln((NN, Title, STitle, Autores, URL, Fecha )),
   collect_meta(NN, Rest, Resto).

collect_meta(N, [_|R], Meta) :- collect_meta(N, R, Meta).

elimina_sin_titulo([],[]).
elimina_sin_titulo([(' -- ', _, _, _, _ )|Resto], RR ) :- !,
	elimina_sin_titulo(Resto, RR).
elimina_sin_titulo([P|Resto1], [P|Resto2] ) :-
	elimina_sin_titulo(Resto1, Resto2).

dame_n(0, Resto, Resto, []).
dame_n(N, [P|R], RR, [P|RRR]) :- NN is N - 1, dame_n(NN, R, RR, RRR).

asigna_archivos_titulos(Ordenados, Pares, Completos) :-
	asigna_archivos_t(0, Ordenados, Pares, Completos).

asigna_archivos_t(_, [], [], []).
asigna_archivos_t(N, [(Title, STitle, Autores, URL, Fecha )|Rest],
 		[(Title, Archivo)|RR1],
		[((Title, STitle, Autores, URL, Fecha ), Archivo)|RR2] ) :-
	NN is N + 1,
	format(atom(Archivo), 't~`0t~d~6|.html', [NN]),
	asigna_archivos_t(NN, Rest, RR1, RR2).

% salidas

% print_titles: imprime la lista de titulos con sus pdfs asociados

% print_titles(10,_). % set to 1 to test

print_titles(_, _N, _M, []).

print_titles(M, _N, M, _ ) :- !.  %  Entre N y M, M no lo imprime

print_titles(I, N, M,
	[((Title, STitle, Autores, URL, Fecha ), File) |Records]) :-
	I >= N,
   	II is I + 1, writeln(File),
	trim_stacks,
	open(File, write, Stream, [encoding(utf8), alias(index_title)]),
	write(index_title, '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/transitional.dtd">\n<html><head>\n\t<meta http-equiv="CONTENT-TYPE" content="text/html; charset=utf-8">\n\t<link rel="stylesheet" type="text/css" href="estilos.css" />\n\t\t<title> Indice de '),
	write(index_title, Title),
	write(index_title, ', como contribuci&oacute; en InMemoriAn o en EDUCERE</title>\n'),
	%write(index_title, '</head><body style="color: rgb(0, 0, 0); background-color: rgb(221, 255, 224);" alink="#808080" link="#000080" vlink="#0000ff">\n<div id="container">\n<div id="pageHeader">\n</div>\n\t<div id="pageContehome">\n'),
	write(index_title, '</head><body>\n<div id="container">\n<div id="pageHeader">\n</div>\n\t<div id="pageContehome">\n'),
	write(index_title, '<center><big><a href="../index.html">Indices InMemoriAN y EDUCERE</a><br></big></center>\n'),
   	write(index_title, '<entry>\n'),
   	write(index_title, '\t<number>\n'),
   	write(index_title,II),
	write(index_title, '<br>'),
   	write(index_title, '\t</number>\n'),
   	write(index_title, '\t<results>\n'),
	%Title = [T],
	%STitle = [S],
	%Autores = [As],
	%URL=[U],
	%Fecha= [F],
   	concat('\"', Title, CT),
   	concat(CT, '\"', CTC),
   	write(index_title, '\t<titulo>\n'),
   	write(index_title,Title),
	write(index_title, '<br>'),
   	write(index_title, '\t</titulo>\n'),
	write(index_title, '\t<subtitulo>\n'),
   	write(index_title,STitle),
	write(index_title, '<br>'),
   	write(index_title, '\t</subtitulo>\n'),
	write(index_title, '\t<autores>\n'),
   	write(index_title,Autores),
	write(index_title, '<br>'),
   	write(index_title, '\t</autores>\n'),
	write(index_title, '\t<titulo>\n'),
   	write(index_title, '<a href="'),
	write(index_title,URL),
	write(index_title, '">'),
	write(index_title,URL),
	write(index_title, '</a>'),
	write(index_title, '<br>'),
	write(index_title, '\t<creado>\n'),
   	write(index_title,Fecha),
	write(index_title, '<br>'),
   	write(index_title, '\t</creado>\n'),
   	write(index_title, '\t</titulo>\n'),
   	( cea:search_index(CTC, Resultados) ->
	  true ;
	  Resultados = ((CTC,0),[])
	), !,
   	write(index_title, '\t<results>\n'),
   	with_output_to(index_title, cea:html_table_out(Resultados)),
   	% write(Resultados),
   	write(index_title, '\t</results>\n'),
   	write(index_title, '</entry>\n'),
   	write(index_title, '</div>\n<div id="footer"><p>InMemorian (Proyecto FONACIT) y EDUCERE. La Revista Venezolana de Educación Escuela de Educación. <br/>\nUniversidad de Los Andes,<br/>\nMérida - Venezuela<br/>\n<br/>\n<br/>\n</p>\n</div>\n</div>\n'),
	write(index_title, '</body>\n'),
	write(index_title, '</html>\n'),
	close(Stream),
   	print_titles(II, N, M, Records).

print_titles(I, N, M, [_|R]) :-
	I < N, II is I + 1, print_titles(II, N, M, R).

print_index_titles(File, TitulosArchivos) :-
	open(File, write, Stream, [encoding(utf8), alias(index_titles)]),
	write(index_titles, '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/transitional.dtd">\n<html><head>\n\t<meta http-equiv="CONTENT-TYPE" content="text/html; charset=utf-8">\n\t<link rel="stylesheet" type="text/css" href="estilos.css" />\n\t\t<title>Indice de T&iacute;tulos de EDUCERE</title>\n\n'),
	write(index_titles, '</head><body>\n<div id="container">\n<div id="pageHeader">\n</div>\n\t<div id="pageContehome">\n'),
	write(index_titles, '<center><big><a href="../index.html">Indices InMemoriAN y EDUCERE</a><br></big></center><a href="#" onclick="find();">Buscar en esta p&aacute;gina</a>\n'),
	with_output_to(index_titles, cea:html_pairs_out(TitulosArchivos)),
	write(index_titles, '</div>\n<div id="footer"><p>InMemorian (Proyecto FONACIT) y EDUCERE. La Revista Venezolana de Educación Escuela de Educación. <br/>\nUniversidad de Los Andes,<br/>\nMérida - Venezuela<br/>\n<br/>\n<br/>\n</p>\n</div>\n</div>\n'),
	write(index_titles, '</body>\n'),
	write(index_titles, '</html>\n'),
	close(Stream).

%%
% genera_temas(File)
%
genera_temas(File) :-
	get_all_themes(Temas),
	print_themes(0, Temas),
	print_index_themes(File, Temas).

get_all_themes(Themes) :-
	findall(A, tema(A), Themes).

% print_themes: imprime la lista de temas con sus pdfs asociados

% print_themes(10,_). % set to 1 to test

print_themes(_, []).

print_themes(N, [(Tema, File)|Records]) :-
   	NN is N + 1,
   	open(File, write, Stream, [encoding(utf8), alias(index_theme)]),
	write(index_theme, '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/transitional.dtd">\n<html><head>\n\t<meta http-equiv="CONTENT-TYPE" content="text/html; charset=utf-8">\n\t<link rel="stylesheet" type="text/css" href="estilos.css" />\n\t\t<title> Indice del tema: '),
	write(index_theme, Tema),
	write(index_theme, ', en EDUCERE</title>\n'),
	write(index_theme, '</head><body>\n<div id="container">\n<div id="pageHeader">\n</div>\n\t<div id="pageContehome">\n'),
	write(index_theme, '<center><big><a href="../index.html">Indices InMemoriAn y EDUCERE</a><br></big></center>\n'),
   	write(index_theme, '<entry>\n'),
   	write(index_theme, '\t<number>\n'),
   	write(index_theme,NN),
   	write(index_theme, '\t</number>\n'),
   	write(index_theme, '\t<results>\n'),
   	concat('\'', Tema, CTema),
   	concat(CTema, '\'', CTemaC),
   	write(index_theme, '\t<theme>\n'),
   	write(index_theme,Tema),
   	write(index_theme, '\t</theme>\n'),
   	( cea:search_index(CTemaC, Resultados) ->
	  true ;
	  Resultados = ((CTemaC,0),[])
	), !,
   	write(index_theme, '\t<results>\n'),
   	with_output_to(index_theme, cea:html_table_out(Resultados)),
   	% write(Resultados),
   	write(index_theme, '\t</results>\n'),
   	write(index_theme, '</entry>\n'),
	write(index_theme, '</div>\n<div id="footer">InMemorian (Proyecto FONACIT) y EDUCERE. La Revista Venezolana de Educación Escuela de Educación. <br/>\nUniversidad de Los Andes,<br/>\nMérida - Venezuela<br/>\nMérida - Venezuela<br/>\n<br/>\n<br/>\n</p>\n</div>\n</div>\n'),
	write(index_theme, '</body>\n'),
	write(index_theme, '</html>\n'),
	close(Stream),
   	print_themes(NN, Records).

print_index_themes(File, TemasArchivos) :-
	open(File, write, Stream, [encoding(utf8), alias(index_themes)]),
	write(index_themes, '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/transitional.dtd">\n<html><head>\n\t<meta http-equiv="CONTENT-TYPE" content="text/html; charset=utf-8">\n\t<link rel="stylesheet" type="text/css" href="estilos.css" />\n\t\t<title>Indice de Temas</title>\n\n'),
	write(index_themes, '</head><body style="color: rgb(0, 0, 0); background-color: rgb(221, 255, 224);" alink="#808080" link="#000080" vlink="#0000ff">\n<div id="container">\n<div id="pageHeader">\n</div>\n\t<div id="pageContehome">\n'),
	write(index_themes, '<center><big><a href="../index.html">Indices InMemoriAn y EDUCERE</a><br></big></center><a href="#" onclick="find();">Buscar en esta p&aacute;gina</a>\n'),
	with_output_to(index_themes, cea:html_pairs_out(TemasArchivos)),
	write(index_themes, '</div>\n<div id="footer">InMemorian (Proyecto FONACIT) y EDUCERE. La Revista Venezolana de Educación Escuela de Educación. <br/>\nUniversidad de Los Andes,<br/>\nMérida - Venezuela<br/>\n<br/>\n<br/>\n</p>\n</div>\n</div>\n'),
	write(index_themes, '</body>\n'),
	write(index_themes, '</html>\n'),
	close(Stream).

% para extracción de data
% comienzo del registro
dame_un_record([Primero|Resto], Resto, [Primero]) :-
    Primero = element(row,[], Lista),
    % member(element(column,[name=text_value],_), Lista),
    %member(element(column,[name=element],[identifier]), Lista),
    %member(element(column,[name=qualifier],[other]), Lista), !.
    member(element(column,[name=text_value],_), Lista),
    member(element(column,[name=element],[identifier]), Lista),
    member(element(column,[name=qualifier],[uri]), Lista), !.

% salta por un registro
dame_un_record([P|R], RR, [P|RRR]) :- !, dame_un_record(R, RR, RRR).


% Código específico para el formato

get_author(Record, Author) :-
	member(element(row,[],Lista), Record),
	member(element(column,[name=text_value],Author), Lista),
      	member(element(column,[name=element],[contributor]), Lista),
      	member(element(column,[name=qualifier],[author]), Lista).

% get_author([_|R], A) :- get_author(R,A).

get_title(Record, Title) :-
	member(element(row,[],Lista), Record),
	member(element(column,[name=text_value],Title), Lista),
      	member(element(column,[name=element],[title]), Lista),
      	member(element(column,[name=qualifier,null=null],[]), Lista), !.

% get_title([_|R], A) :- get_title(R,A).

%get_subtitle([element(row,[],[_
%			,element(column,[name=text_value],STitle),_
%			,element(column,[name=element],[title]),_
%			,element(column,[name=qualifier],[alternative]),_
%		])|_], STitle) :- !.

get_subtitle(Record, Title) :-
	member(element(row,[],Lista), Record),
	member(element(column,[name=text_value],Title), Lista),
      	member(element(column,[name=element],[title]), Lista),
      	member(element(column,[name=qualifier],[alternative]), Lista), !.

% get_subtitle([_|R], A) :- get_subtitle(R,A).

get_date(Record, Date) :-
	member(element(row,[],Lista), Record),
      	member(element(column,[name=text_value],Date), Lista),
      	member(element(column,[name=element],[date]), Lista),
      	member(element(column,[name=qualifier],[created]), Lista), !.

% get_date([_|R], A) :- get_date(R,A).

get_handle(Record, Handle) :-
	member(element(row,[],Lista), Record),
      	member(element(column,[name=text_value],Handle), Lista),
      	member(element(column,[name=element],[identifier]), Lista),
      	member(element(column,[name=qualifier],[uri]), Lista), !.

% get_handle([_|R], A) :- get_handle(R,A).


show_source_files :-
	new(D, dialog('Prolog Source Files')),
	send(D, append, new(B, list_browser)),
	forall(source_file(X), send(B, append, X)),
	send(B, open_message, message(@prolog, emacs, @arg1?key)),
	send(D, append, button(consult, message(@prolog, consult, B?selection?key))),
	send(D, open).


%  qsave_program(cea, [ goal(cea), stand_alone(true), autoload(true) ])

