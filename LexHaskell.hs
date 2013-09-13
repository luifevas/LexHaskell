import Data.Char

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
					