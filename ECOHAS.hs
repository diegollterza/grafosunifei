type Vertice = (Int, [Int])
type Grafo = [Vertice]

grafo :: Grafo
grafo = [(1,[2,3,4]), (2,[1,4]), (3,[1,4]), (4,[1,2,3])]

listaVertices :: Grafo -> [Int]
listaVertices [] = []
listaVertices ((v,l):t) = v:listaVertices(t)

vertices :: [Int]
vertices = listaVertices(grafo)

grauVertice :: [Int] -> Int
grauVertice [] = 0
grauVertice (_:cauda) = 1 + grauVertice(cauda)

regular :: Grafo -> Bool
regular [_] = True
regular ((v1,x):(v2,y):cauda) = grauVertice(x) == grauVertice(y) && regular((v2,y):cauda)

ordem :: Grafo -> Int
ordem [] = 0
ordem (_:t) = 1 + ordem(t)

arestas :: Grafo -> Int
arestas [] = 0
arestas ((_,x):t) = grauVertice(x) + arestas(t)

tamanho :: Grafo -> Int
tamanho (g) = ordem(g) + arestas(g) `div` 2

arestasImpares :: Grafo -> Int
arestasImpares[] = 0
arestasImpares((_,v):t) = grauVertice(v) `mod` 2 + arestasImpares(t)

euleriano :: Grafo -> Bool
euleriano(g)  |arestasImpares(g) == 2 = True  
              |arestasImpares(g) == 0 = True
              |otherwise = False

diferenca :: [Int] -> [Int] -> [Int]
diferenca [] _ = []
diferenca (a:l1) l2   |a `elem` l2 = diferenca l1 l2
                      |otherwise = a:diferenca l1 l2

complemento :: Grafo -> Grafo

complemento [] = []
complemento ((v,l):t) = (v,diferenca vertices (v:l)):complemento(t)

repetidos :: [Int] -> Bool
repetidos [] = False
repetidos (h:t) | h `elem` t = True
                |otherwise = repetidos(t)

multigrafo :: Grafo -> Bool
multigrafo [] = False
multigrafo ((v,l):t) | v `elem` l = True
                     | repetidos(l) = True
                     |otherwise = multigrafo(t)
                     
buscaLarg :: Grafo -> Int -> Grafo
buscaLarg g v = buscaAux g [v] []

contem :: Int -> Grafo -> Bool
contem _ [] = False
contem v ((h,l):t)  | v == h = True
                | otherwise = contem v t

vizinhos :: Grafo -> Int -> [Int] -> [Int]
vizinhos [] _ _ = []
vizinhos ((v,l):g) v2 l2  | v == v2 = diferenca l l2
                          | otherwise = vizinhos g v2 l2

verticeEm :: Int -> Grafo -> Vertice
verticeEm v ((v2,l):t)  | v == v2 = (v2,l)
                        | otherwise = verticeEm v t
                        
uniao :: [Int] -> [Int] -> [Int]
uniao [] l = l
uniao (h:t) l | h `elem` l = h:diferenca (uniao t l) [h]
              | otherwise = h:uniao t l

buscaAux :: Grafo -> [Int] -> [Int] -> Grafo
buscaAux _ [] _ = []
buscaAux g (v:h) l | contem v g = verticeEm v g :buscaAux g (uniao h (vizinhos g v (v:l))) (v:l)
                        |otherwise = []
buscaProf :: Grafo -> Int -> Grafo
buscaProf g v = buscaAux2 g [v] []

buscaAux2 :: Grafo -> [Int] -> [Int] -> Grafo
buscaAux2 _ [] _ = []
buscaAux2 g l1 l | contem (last l1) g = verticeEm (last l1) g :buscaAux2 g (uniao (init l1) (vizinhos g (last l1) (last l1:l))) (last l1:l)
                        |otherwise = []

main = print grafo
