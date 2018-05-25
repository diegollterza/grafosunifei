#include <iostream>
#define TAM 6
#define inf 999
using namespace std;


//função para impressão de arrays estaticas
void imprimeLista(int lista[]){
    for(int i = 0; i < TAM; i++){
        cout<<lista[i]<<" ";
    }
    cout<<endl;
}

//dijkstra
void dj_alg(int g[][TAM], int origem, int caminho[TAM]){
    ///////init_var//////
    int custo[TAM];
    bool visitado[TAM];
    int v = origem;
    int j = 0;
    int aux = -1;
    ////////////////////
    for(int i = 0; i< TAM; i++){
        custo[i] = inf;
        caminho[i] = -1;
        visitado[i] = false;
    }
    ////////////////////
    caminho[v] = v;
    custo[v] = 0;
    ////////////////////
    
    while(true){
        
        aux = -1;
        //////////////////// verifica se o vertice foi visitado
        //e seleciona aquele com menor custo entre os não visitados
        for(int i = 0; i < TAM; i++){
            if(!visitado[i] && (aux < 0 || custo[i] < custo[aux])) aux = i;
        }
        //caso todos visitados, break
        if(aux < 0) break;
        //marca o vertice como visitado
        visitado[aux] = true;
        //atualiza os custos com base no vertice analisado
        for(int i = 0; i < TAM;i++){
            if(g[aux][i]>0 && g[aux][i] + custo[aux] < custo[i]){
                 custo[i] = custo[aux] + g[aux][i];
                caminho[i] = aux;
            }
        }
    }
}

int main()
{
   int grafo[][TAM] = {{ 0, 3,-1,-1,-1, 1},
                    { 3, 0, 4, 7,-1,-1},
                    {-1, 4, 0, 1,-1, 9},
                    {-1, 7, 1, 0, 8, 4},
                    {-1,-1,-1, 8, 0,-1},
                    { 1,-1, 9, 4,-1, 0}
    };
    int caminho[TAM];
    dj_alg(grafo,0,caminho);
    imprimeLista(caminho);
    
}
