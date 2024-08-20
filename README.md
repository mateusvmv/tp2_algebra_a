# Algorítmos
Neste repositório está implementado o algorítmo do crivo quadrático e suas dependências, utilizando das optimizações de potências de primos e matrizes com arrays binárias para resolução do sistema. Veja também [o repositório do TP1](https://github.com/mateusvmv/tp1_algebra_a), no qual estão implementados outros algorítmos de fatoração e testes de primalidade.
# Como executar
`runghc -isrc src/Main.hs`
## Com Optimizações
`ghc -O3 -o Main -isrc src/Main.hs`
## Estatísticas de Execução
`env time -v ./Main < input`
# Interativo
`ghci -isrc src/Main.hs`
## Optimizações
`ghci -fobject-code -O3 -isrc src/Main.hs`
## Estatísticas de Execução
`:set +s`
