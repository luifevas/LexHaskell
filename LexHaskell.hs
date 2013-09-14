import Data.Char


main = do
   putStrLn "Ingrese ruta del archivo"
   ruta <- getLine
   content <- readFile (ruta)
   let final = lexA content
   print final
   


obtenerLexemes :: String -> Int -> Int-> Int -> [String]
obtenerLexemes "" cant ban tipo = []
obtenerLexemes s cant ban tipo =
	if((isLetter(s!!cant) || isNumber(s!!cant) || s!!cant=='.') && ban==0 && tipo==0)
		then obtenerLexemes s (cant+1) 0 0
		else if(s!!0=='"' && ban==0)
			then obtenerLexemes s (cant+1) 1 0
			else if(s!!cant=='"' && ban==1)
				then [(take (cant+1) (s))] ++ obtenerLexemes (drop (cant+1) s) 0 0 0
				else if(cant/=0 && ban==0 && tipo==0)
					then [(take (cant) (s))] ++ obtenerLexemes (drop cant s) 0 0 0
					else if(ban==1)
						then obtenerLexemes s  (cant+1) 1 0
						else if(not(isLetter(s!!cant)) && not(isNumber(s!!cant)) && s!!cant/=' ' && s!!cant/='"' && ban==0 && cant<2)
							then obtenerLexemes s (cant+1) 0 1
							else
								if(cant>0)
									then 
										if(((s!!0==s!!cant &&(s!!0=='&' || s!!0=='|')) || s!!0=='<' || s!!0=='>' || s!!0=='!' || s!!0=='+' || s!!0=='-') || cant==1)
											then [take (cant) (s)] ++ obtenerLexemes (drop cant s) 0 0 0
											else [take (cant-1) s] ++ obtenerLexemes (drop (cant-1) s ) 0 0 0
									else obtenerLexemes (tail s) 0 0 0			

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
analizar "\n"       = "SALTO DE LINEA"
analizar "+="       ="MASIGUAL"

analizar p =  if(esNumeroEntero p)
		then "NUMERO ENTERO"
		else if(esNumeroFraccional p)
		     then "NUMERO PUNTO FLOTANTE"
			else if(esIdentificador p)
			     then "IDENTIFICADOR"        
                             else if(esString p 0)
				then "STRING"
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

esIdentificador :: String -> Bool
esIdentificador "" = True
esIdentificador s= if(isAlphaNum(head s))
            then esIdentificador (tail s)
	    else False
esString :: String -> Int -> Bool
esString "" 2 = True
esString "" 1 = False
esString "" 0 = False
esString s x= if(x>2)
		then False
		 else if((head s)== '"')
			then esString (tail s) (x+1)
			else esString(tail s) (x)
analizado :: String -> [String]
analizado p = obtenerLexemes p 0 0 0

lexA :: String -> [(String, String)]
lexA s = map (\x -> (x, analizar x)) $ analizado s

 	     		