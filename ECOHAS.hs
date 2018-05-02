type Vertice = (Int, [Int])
type Grafo = [Vertice]

grafo :: Grafo
grafo = [(1,[2,3,4]), (2,[1,4]), (3,[1,4]), (4,[1,2,3])]

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
              
main = print grafo
