import Data.Char


main = do
   putStrLn "Ingrese ruta del archivo"
   ruta <- getLine
   content <- readFile (ruta)
   let final = lexA content
   print final
   


obtenerNumeros :: String -> Int -> [String]
obtenerNumeros "" cant = []
obtenerNumeros x cant = 
	if(not(isLetter(head x)) && not(isNumber(head x)) && cant==0 && not((head x)==' '))
		then obtenerNumeros x (cant+1)
		else if(isNumber(x!!cant) && not(cant==0))
			then obtenerNumeros x (cant+1)
			else if(x!!cant=='.')
				then obtenerNumeros x (cant+1)
				else if(not(x!!cant=='.') && not(isNumber(x!!cant)) && not(isLetter(x!!cant)) && not(x!!cant==' '))
					then 
							if(not(elem (take (cant-1) (tail x)) (obtenerNumeros (drop cant x) 0)))
								then [(take (cant-1) (tail x))] ++ obtenerNumeros (drop cant x) 0
								else obtenerNumeros (drop cant x) 0
					else obtenerNumeros (drop (cant+1) x) 0					

obtenerPalabras :: String -> Int -> Int ->	[String]
obtenerPalabras "" cant ban = []
obtenerPalabras s cant ban =
	if(isLetter(head s) && cant==0 && ban==0)
		then obtenerPalabras s (cant+1) 0
		else if((isLetter(s!!cant) || isNumber(s!!cant)) && not(cant==0) && ban==0)
			then obtenerPalabras s (cant+1) 0
			else if((head s)=='"' && ban==0)
				then obtenerPalabras s (cant+1) 1
				else if((s!!cant)=='"' && ban==1)
					then
							if(not(elem (take (cant+1) s) (obtenerPalabras (drop (cant+1) s) 0 0)))
								then [(take (cant+1) s)] ++ obtenerPalabras (drop (cant+1) s) 0 0
								else obtenerPalabras (drop (cant+1) s) 0 0
					else if(not(cant==0) && ban==0)
						then 
								if(not(elem (take cant s) (obtenerPalabras(drop (cant+1) s) 0 0)))
									then [(take cant s)] ++ obtenerPalabras(drop (cant+1) s) 0 0
									else obtenerPalabras(drop (cant+1) s) 0 0
						else if(ban==1)
							then obtenerPalabras s (cant+1) 1
							else obtenerPalabras(drop (cant+1) s) 0 ban

							
obtenerSeparadores :: String -> Int -> Int -> [String]
obtenerSeparadores "" cant ban = []
obtenerSeparadores s cant ban =
	if((s!!cant)=='"' && ban==0)
		then obtenerSeparadores s (cant+1) 1
		else if ((s!!cant)=='"' && ban==1)
			then obtenerSeparadores (drop (cant+1) s) 0 0 
            else if(not(isLetter(s!!0)) && not(isNumber(s!!0)) && cant==0 && not((s!!0)==' ') && ban==0)
                then obtenerSeparadores s (cant+1) 0
				else if(not(isLetter(s!!cant)) && not(isNumber(s!!cant)) && not(cant==0) && not((s!!cant)==' ') && ban==0)
					then obtenerSeparadores s (cant+1) 0
					else 
						if(cant>0)
						then 
							if(not(elem (take (cant-ban) s) (obtenerSeparadores(drop (cant+1) s) 0 ban))) 
								then [(take (cant-ban) s)] ++ obtenerSeparadores(drop (cant+1) s) 0 ban
								else obtenerSeparadores(drop (cant+1) s) 0 ban   

						else obtenerSeparadores(drop (cant+1) s) 0 ban

analizar :: String -> String
analizar "auto"     = "AUTO"
analizar "break"    = "BREAK"
analizar "case"     = "CASE"  
analizar "char"     = "TIPO_CHAR"
analizar "const"    = "CONST"
analizar "continue" = "CONTINUE"
analizar "default"  = "DEFAULT"
analizar "do"       = "DO"
analizar "double"   = "DOUBLE"
analizar "else"     = "FIN CONDICIONAL"
analizar "enum"     = "ENUMERADO"
analizar "extern"   = "EXTERN"
analizar "float"    = "TIPO_FLOAT"
analizar "for"      = "SENTENCIA ITERATIVA FOR"
analizar "goto"     = "GOTO"
analizar "if"       = "CONDICIONAL"
analizar "int"      = "TIPO_INT"
analizar "long"     = "TIPO_LONG"
analizar "register" = "REGISTER"
analizar "return"   = "RETURN"
analizar "short"    = "SHORT"
analizar "signed"   = "SIGNED"
analizar "sizeof"   = "SIZEOF"
analizar "static"   = "STATIC"
analizar "struct"   = "STRUCT"
analizar "switch"   = "SWITCH"
analizar "typedef"  = "TYPEDEF"
analizar "union"    = "UNION"
analizar "unsigned" = "UNSIGNED"
analizar "void"     = "VOID"
analizar "volatile" = "VOLATILE"
analizar "while"    = "SENTENCIA ITERATIVA WHILE"
analizar "..."      = "ELIPSE"
analizar "++"       = "MAS UNO"
analizar "--"       = "MENOS UNO"
analizar "->"       = "PUNTERO"
analizar "&&"       = "Y"
analizar "||"       = "O"
analizar "<="       = "MENOR O IGUAL"
analizar ">="       = "MAYOR O IGUAL"
analizar "=="       = "ES IGUAL A"
analizar "!="       = "NO ES IGUAL A"
analizar ";"        = "PUNTO Y COMA"
analizar "{"        = "LLAVE ABIERTA"
analizar "}"        = "LLAVE CERRADA"
analizar ","        = "COMA"
analizar ":"        = "DOS PUNTOS"
analizar "="        = "IGUAL"
analizar "("        = "PARENTESIS ABIERTO"
analizar ")"        = "PARENTESIS CERRADO"
analizar "["        = "CORCHETE ABIERTO"
analizar "]"        = "CORCHETE CERRADO"
analizar "."        = "PUNTO"
analizar "&"        = "AMPERSAND"
analizar "!"        = "INTERROGACION_POSITIVO"
analizar "~"        = "~"
analizar "-"        = "MENOS"
analizar "+"        = "MAS"
analizar "*"        = "POR"
analizar "/"        = "/"
analizar "%"        = "%"
analizar "<"        = "<"
analizar ">"        = "<"
analizar "^"        = "^"
analizar "|"        = "|"
analizar "?"        = "?"
analizar p =  if(esNumeroEntero p)
		then "NUMERO ENTERO"
		else if(esNumeroFraccional p)
		     then "NUMERO PUNTO FLOTANTE"
			else if(esString p)
			     then "IDENTIFICADOR STRING"        
                             else "INVALIDO" 


esNumeroEntero :: String -> Bool
esNumeroEntero "" = True
esNumeroEntero x = if(isDigit(head x))
		then esNumeroEntero (tail x)
		else False



esNumeroFraccional :: String-> Bool
esNumeroFraccional x = esNumeroFraccional' x 0
				
esNumeroFraccional' :: String -> Int -> Bool
esNumeroFraccional' "" 0= False
esNumeroFraccional' "" 1 = True
esNumeroFraccional' x 0= if(isDigit(head x))
		then esNumeroFraccional' (tail x) 0
		else if((head x)== '.')
		      then esNumeroFraccional' (tail x) 1
		      else False
esNumeroFraccional' x 1= if(isDigit(head x))
		then esNumeroFraccional' (tail x) 1
		else if((head x)== '.')
		      then False
		      else False

esString :: String -> Bool
esString "" = True
esString s= if(isAlphaNum(head s))
            then esString (tail s)
	    else False

analizado :: String -> [String]
analizado p = (obtenerNumeros p 0) ++ (obtenerPalabras p 0 0) ++ (obtenerSeparadores p 0 0)

lexA :: String -> [(String, String)]
lexA s = map (\x -> (x, analizar x)) $ analizado s

 	     		