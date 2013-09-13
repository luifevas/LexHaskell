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

							