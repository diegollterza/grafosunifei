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

main = print grafo
