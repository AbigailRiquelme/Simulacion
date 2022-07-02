/*********************************************************************
            Econometria - Semestre de Primavera 2021
				   Universidad de San Andrés
				   
	                  Multicolinealidad 
*********************************************************************/

* En la clase magistral vieron que en la práctica le prestamos atención a la cuestión de multicolinealidad alta debido a que si hay alta multicolinealidad 
* la varianza del estimador de MCO aumenta, lo cual genera problemas de inferencia. 

* Con el objetivo de observar los cambios, al estimar modelos por MCO, ante presencia de multicolinealidad alta con respecto a casos en los que la multicolinealidad es baja haremos 3 simulaciones. 


* Definimos el scheme para los gráficos 

set scheme mrc

****** Primera simulación*****

* Vamos a simular 3 variables. "Y" será nuestra variable dependiente y "X1" y "X2" serán variables explicativas. 

* Primer paso: armamos la matriz de correlaciones inicial (nuestro objetivo es que no tengamos un problema de multicolinealidad) y la final (queremos que haya un problema de alta multicolinealidad)

mat Corr = (1, 0.5, 0.43\ 0.5,1, 0.0001\0.43,0.0001,1)

mat Corr1 = (1, 0.5, 0.43\ 0.5,1, 0.9\0.43,0.9,1)

* Descargamos las tablas de correlaciones: 

frmttable using PrimeraSimulacion, statmat(Corr) sdec(3) sfmt(f) replace tex ctitles("","Y", "X1", "X2") rtitles("Y"\ "X1"\ "X2")


frmttable using PrimeraSimulacion2, statmat(Corr1) sdec(3) sfmt(f) replace tex ctitles("","Y", "X1", "X2") rtitles("Y"\ "X1"\ "X2")


* Segundo paso: creamos las variables de forma tal que tengan correlaciones tales como las de las matrices anteriores

corr2data Y X1 X2, cstorage(full) corr(Corr) n(1000) forcepsd 
 
corr2data Y_1 X1_1 X2_1, cstorage(full) corr(Corr1) n(1000) forcepsd 

* Observemos los gráficos de dispersión entre las variables anteriores:

graph matrix Y X1 X2, half scheme(mrc) mcolor(bluegrey) title("Escenario 1")

graph matrix Y_1 X1_1 X2_1, half scheme(mrc) mcolor(bluegrey) title("Escenario 2")

* Tercer paso: corremos la regresión de Y contra las variables explicativas X1 y X2 para ambos casos: 

reg Y X1 X2

* Guardamos los resultados de la regresión

outreg2 using PrimeraSimulacionreg, tex replace dec(3) label  addtext(Multicolinealidad, baja)

reg Y_1 X1_1 X2_1

* Guardamos los resultados de la regresión

outreg2 using PrimeraSimulacionreg, tex append dec(3) label  addtext(Multicolinealidad, alta)

****** Segunda simulación*****

* Simularemos 18 escenarios distintos y extraeremos la siguiente información: R^{2}, coeficiente estimado y error estándar. Crearemos una variable dependiente: "Y" y cuatro variables indepentientes: "X1", "X2", "X3" y "X4". 

* Definimos las matrices a completar en el loop

matrix def correlacion=J(18,1,.)

matrix def complete1=J(1,1,.)

local lo=0
local cant=1
* Vamos a comenzar con una correlacion de 0 entre las variables explicativas, aumentaremos esta correlación de a 0.05 hasta llegar a 0.9. Es decir, vamos desde un escenario de multicolinealidad nula a uno de multicolinealidad alta. 

forvalues i=0.0(0.05)0.9{
		mat Corr1 = (1, 0.23, 0.31,0.4, 0.25\ 0.23, 1,`i' , `i', `i' \ 0.31,`i'  , 1, `i', `i'\0.4,`i'  , `i' , 1 ,`i' \ 0.25, `i', `i', `i', 1)
        corr2data Y X1 X2 X3 X4, cstorage(full) corr(Corr1) n(1000) forcepsd clear 
        reg Y X1 X2 X3 X4, robust
		outreg2 using multicolinealidad2, tex append dec(3) label addtext(Correlación entre variables explicativas, `lo')
		matrix correlacion[`cant',1]=`i'
		local lo=`lo'+0.05
		local cant=`cant'+1
}

****** Tercera simulación*****

* Luego graficaremos los SE, t's, test F y el R^{2} de regresar X1 contra las demás variables explicativas a medida que aumentamos la correlación de X1 con X2. 

matrix def correlacion=J(194,2,.)

matrix def complete1=J(1,1,.)

matrix def complete2=J(1,1,.)

local lo=0
local cant=1
local cant1=1
* Vamos a comenzar con una correlacion de 0 entre X1 y X2, aumentaremos esta correlación de a 0.05 hasta llegar a 0.97. Es decir, vamos desde un escenario de multicolinealidad nula a uno de multicolinealidad alta. 
forvalues i=0.0(0.005)0.97{
		mat Corr1 = (1, 0.23, 0.31,0.4, 0.25\ 0.23, 1,`i' , 0.1, 0.1 \ 0.31,`i'  , 1, 0.2, 0.3\0.4,0.1  , 0.2 , 1 ,0.05 \ 0.25, 0.1, 0.3, 0.05, 1)
        corr2data Y X1 X2 X3 X4, cstorage(full) corr(Corr1) n(1000) forcepsd clear 
        reg Y X1 X2 X3 X4, robust
		* Extraemos el SE
		matselrc r(table) B_`cant1' , r(2) c(1)
		matrix complete1=(complete1\B_`cant1')
		reg Y X1 X2 X3 X4, robust
		* Extraemos el t
		matselrc r(table) A_`cant1', r(3) c(1)
		matrix complete2=(complete2\A_`cant1')
		matrix correlacion[`cant',1]=`i'
		* Extraemos el estadístico del test F
		matrix correlacion[`cant',2]= e(F)
		local lo=`lo'+0.005
		local cant=`cant'+1
		local cant1=`cant1'+1
}

* Sacamos la primera final de las columnas complete1 y complete 2 debido a que solo tienen missings 

matselrc complete1 complete_corregido1, r(2/195) c(1)

matselrc complete2 complete_corregido2, r(2/195) c(1)

* Juntamos las 3 matrices 

matrix complete_b=complete_corregido1, complete_corregido2, correlacion

* Guardamos la matrix para poder hacer los gráficos 

svmat complete_b

* Renombramos las variables 

rename complete_b1 SE

rename complete_b2 estadístico_t

rename complete_b3 correlacion_X1_X2

rename complete_b4 estadístico_f

* Graficamos 

label variable SE "SE"

label variable estadístico_t "Estadístico t"

label variable correlacion_X1_X2 "Correlación X1 y X2"

label variable estadístico_f "Estadístico test F"


twoway line SE correlacion_X1_X2, xtitle(Correlación X1 y X2) ytitle(SE) title("Desvío estándar X1")  lcolor(mrcpurple) lwidth(medthick)

twoway line estadístico_t correlacion_X1_X2, xtitle(Correlación X1 y X2) ytitle(Estadístico t X1) title("Estadístico t X1")  lcolor(mrcpurple) lwidth(medthick)

twoway line estadístico_f correlacion_X1_X2, xtitle(Correlación X1 y X2) ytitle(Test F) title("Estadístico test F")  lcolor(mrcpurple) lwidth(medthick)

* En el siguiente gráfico podemos ver que cuando tenemos alta multicolinealidad valores de los t bajos son compatibles con altos valores del t del test F. 

twoway line estadístico_f estadístico_t correlacion_X1_X2 , xtitle(Correlación X1 y X2) ytitle(Test F) title("Estadístico test F")  lcolor(mrcpurple) lwidth(medthick)





 
 
 