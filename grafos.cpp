#include <iostream>
#include <stdlib.h>
#include <stdio.h>
using namespace std;
#define TAM 4

typedef struct no{
    bool valor;
    no *prox;
} Fila;




//inicializa o grafo
int Grafo[][TAM] = {{0,1,0,1},
                 {1,0,1,1},
                 {0,1,0,1},
                 {1,1,1,0}};
                 
//inicializa os visitados
void init_visitados(int visitados[TAM]){
    
    for(int i = 0; i < TAM; i++){
        visitados[i] = 0;
    }
    
}


//depth search recursivo
void dfs_recursivo(int g[][TAM], int visitados[], int vertice){
    visitados[vertice] = 1;
    cout<<vertice+1<<" ";
    for(int i = 0; i < TAM; i++){
            if((g[vertice][i])&&(!visitados[i])) dfs_recursivo(g,visitados,i);
    }
}

//depth search sem recursao
void dfs(int g[][TAM], int vertice){
    int visitados[TAM];
    init_visitados(visitados);
    int pilha[TAM];
    int topo = 0;
    pilha[topo] = vertice;
    visitados[vertice] = 1;
    
    while(topo != -1){
        cout<<pilha[topo]+1<<" ";
        vertice = pilha[topo--];
        
        for(int i = 0; i < TAM; i++){
            if((g[vertice][i])&&(!visitados[i])){
                pilha[++topo] = i;
                visitados[i] = 1;
            }
        }
    }
}

//bf search sem recursao
void bfs(int g[][TAM], int vertice){
    int visitados[TAM];
    init_visitados(visitados);
}

void verifica_maior_grau(int g[][TAM]){
    int tam = 0;
    int soma = 0;
    for(int i = 0; i < TAM;i++){
        for(int j = 0; j< TAM;j++){
            soma += g[i][j];
        }
        if(soma>tam) tam = soma;
        soma = 0;
    }
    cout<<tam;
}
    

int main()
{
    int visitados[TAM];
    init_visitados(visitados);
    //dfs_recursivo(Grafo, visitados, 0);
    //dfs(Grafo, 0);
    //bfs(Grafo, 0);
    //verifica_maior_grau(Grafo);
    
}
