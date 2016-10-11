% *************************************************************************
% * ceaWeb.pl                                                             *
% * Página web principal del Catalogo Electronico Autonomo partir de un   *
% * código con HTML termerizado en Prolog. Proyecto final tesis.          *
% * Este programa es Software Libre. Desarrollado por                     *
% * Julio Jaimes y Jacinto Davila, basándose la app CEA dise#ada por      *
% * por Jacinto Davila y Yaritza Vargas                                   *
% *************************************************************************


/*****************************************************************************************************************/
/************************                                         ************************************************/
/************************ Pasos de instalacion de todo el sistema ************************************************/
/************************                                         ************************************************/
/*****************************************************************************************************************/

/*****************************************************************************************************************/
Paso 1:
	debera descargar el proyecto completo em github.
/*****************************************************************************************************************/
Paso 2:
	luego haga la indexacion de los documentos.
/*****************************************************************************************************************/
Paso 3:
	Luego entre en el directorio del proyecto y habra la terminal desde alli.
/*****************************************************************************************************************/
Paso 4:
	ejecute el siguiente comando:

	swipl
/*****************************************************************************************************************/
Paso 5:
	Luego desde swipl ejecute el siguiente comando [ceaWeb]. sera de la siguiente forma en la terminal:

	Welcome to SWI-Prolog (Multi-threaded, 64 bits, Version 7.2.3)
	Copyright (c) 1990-2015 University of Amsterdam, VU Amsterdam
	SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software,
	and you are welcome to redistribute it under certain conditions.
	Please visit http://www.swi-prolog.org for details.

	For help, use ?- help(Topic). or ?- apropos(Word).

	?- [ceaWeb].
/*****************************************************************************************************************/
Paso 6:
	Luego aparecera esto en la consola:

	% Started server at http://localhost:5000/
	true.

	?- 
/*****************************************************************************************************************/
Paso 7:

	Luego dar click desde la consola  a : http://localhost:5000/

	o Abra su navegador y colocar la siguiente direccion url y abrir : http://localhost:5000/
/*****************************************************************************************************************/
Observaciones.

	Nota:
		por defecto el sistema se ejecuta en el puerto 5000,
		 pero usted puede cambiarlo al ejecutar:

		?- server[AquiVaElNumeroDePruerto].

		un ejemplo es:

		?- server[8000].

	Luego seguir los pasos 6 y 7.

/********************************************************************************************************************/
/************************                                            ************************************************/
/************************ Pasos para la instalacion de swish-e linux ************************************************/
/************************                                            ************************************************/
/********************************************************************************************************************/
paso 1:

	Abrir la terminal (puede presionar Ctrl+T) para abrir la terminal
/*****************************************************************************************************************/
Paso 2:

	Desde la terminal ejecutar el siguiente comando:
	
	sudo apt-get update
/*****************************************************************************************************************/
Paso 3:
	
	Ejecutar el siguiente comando desde la consola:

	sudo apt-get install swish-e
/*****************************************************************************************************************/
Paso 4:
	
	para verificar que esta instalado presione el siguiente comando:
	
	swish-e -V

	y debera tener algo parecido a esto:

	SWISH-E 2.4.7
/********************************************************************************************************************/
/************************                                            ************************************************/
/************************ Otra forma de instalacion es la siguiente: ************************************************/
/************************                                            ************************************************/
/********************************************************************************************************************/
Paso 1:

	Bajar el directorio swish-e-2.4.7 de gutHub donde esta el proyecto.

	Puedes descargarlo desde www.swish-e.org
/*****************************************************************************************************************/
Paso 2:
	
	Luego ingresar al directorio con el siguiente comando:

 	cd swish-e-2.4.7  (this directory will depend on the version of Swish-e)
/*****************************************************************************************************************/
Paso 3:

	Ejecutar el siguiente comando:
    	./configure
    	make
    	make check
/*****************************************************************************************************************/
Paso 4:   

	ejecutar el siguientes comando como root:
	
	 make install
/*****************************************************************************************************************/
Paso 5:

	Si todo va bien ejecute el siguiente comando:
	    
	 swish-e -V

	y debera ver:

    	SWISH-E 2.4.7
/********************************************************************************************************************/
/************************                                            ************************************************/
/************************       Pasos Para indexar los archivos:     ************************************************/
/************************                                            ************************************************/
/********************************************************************************************************************/
Paso 1:

	Desde el archivo donde desea indexar, cree el siguiente archivo llamado pdfs.conf

/*****************************************************************************************************************/
Paso 2:
	Copie, pegue y guarde en el archivo pdfs.conf lo siguiente:

#  swish-e -S fs -c pdfs.conf
IndexDir ./pdfs
FileFilterMatch pdftotext "%p -" /.\.pdf$/
WordCharacters äöüéèàabcdefghijklmnopqrstuvwxyz0123456789.-
IgnoreFirstChar .-
IgnoreLastChar  .-
BeginCharacters äöüéèàabcdefghijklmnopqrstuvwxyz0123456789
EndCharacters   äöüéèàabcdefghijklmnopqrstuvwxyz0123456789
MinWordLimit 2


/*****************************************************************************************************************/
Paso 3:

	Nota: Usted puede cambiar este archivo segun sea su necesidad.
		
/*****************************************************************************************************************/
Paso 4:

	Luego ejecute el siguiente comando desde el directorio en donde tiene el archivo pdfs.conf:

	swish-e -S fs -c pdfs.conf
/*****************************************************************************************************************/
Paso 5:

	Ya con esto habra indexado los archivos.
/*****************************************************************************************************************/
Paso 6:

	Para realizar la busquedad desde la consola inserte el siguiente comando:

	swish-e -w Aqui_Va_La_Pakabra_a_Buscar

/********************************************************************************************************************/
/************************                                            ************************************************/
/************************       Fin del manual de instalacion        ************************************************/
/************************                                            ************************************************/
/********************************************************************************************************************/






















