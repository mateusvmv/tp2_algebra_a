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