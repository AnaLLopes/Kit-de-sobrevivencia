import System.IO

data Item = Item {nome :: String, preco :: Float, peso :: Float} deriving Show

capacidadeMochila :: Float
capacidadeMochila = 50.0

main :: IO ()
main = do
    putStrLn "Bem-vindo ao Kit de Sobrevivência para a Missão Espacial!"
    loop []

loop :: [Item] -> IO ()
loop kit = do
    putStrLn "\nMenu Principal:"
    putStrLn "1. Adicionar item ao kit"
    putStrLn "2. Listar todos os itens no kit"
    putStrLn "3. Remover item do kit"
    putStrLn "4. Calcular custo total do kit"
    putStrLn "5. Ver status da capacidade da mochila"
    putStrLn "6. Sair da aplicação"
    putStr "Escolha uma opção: "
    hFlush stdout
    opcao <- getLine
    case opcao of
        "1" -> do
            novoKit <- adicionarItem kit
            loop novoKit
        "2" -> do
            listarItens kit
            loop kit
        "3" -> do
            novoKit <- removerItem kit
            loop novoKit
        "4" -> do
            calcularCustoTotal kit
            loop kit
        "5" -> do
            statusCapacidade kit
            loop kit
        "6" -> putStrLn "Saindo da aplicação..."
        _   -> do
            putStrLn "Opção inválida, tente novamente."
            loop kit

adicionarItem :: [Item] -> IO [Item]
adicionarItem kit = do
    putStr "Nome do item: "
    hFlush stdout
    nomeItem <- getLine
    putStr "Preço do item: "
    hFlush stdout
    precoItem <- getLine
    putStr "Peso do item (em kg): "
    hFlush stdout
    pesoItem <- getLine
    let precoFloat = read precoItem :: Float
    let pesoFloat = read pesoItem :: Float
    let novoItem = Item nomeItem precoFloat pesoFloat
    putStrLn $ "Item " ++ nomeItem ++ " adicionado com sucesso!"
    return (kit ++ [novoItem])

listarItens :: [Item] -> IO ()
listarItens [] = putStrLn "O kit está vazio."
listarItens kit = do
    putStrLn "Itens no kit de sobrevivência:"
    mapM_ (\(i, item) -> putStrLn (show i ++ ". " ++ nome item ++ " - R$" ++ show (preco item) ++ " - " ++ show (peso item) ++ "kg")) (zip [1..] kit)

removerItem :: [Item] -> IO [Item]
removerItem [] = do
    putStrLn "O kit está vazio, não há itens para remover."
    return []
removerItem kit = do
    listarItens kit
    putStr "Digite o número do item que deseja remover: "
    hFlush stdout
    numero <- getLine
    let indice = read numero :: Int
    if indice < 1 || indice > length kit
        then do
            putStrLn "Número inválido, tente novamente."
            return kit
        else do
            let novoKit = take (indice - 1) kit ++ drop indice kit
            putStrLn "Item removido com sucesso!"
            return novoKit

calcularCustoTotal :: [Item] -> IO ()
calcularCustoTotal kit = do
    let total = sum (map preco kit)
    putStrLn $ "Custo total do kit: R$" ++ show total

statusCapacidade :: [Item] -> IO ()
statusCapacidade kit = do
    let pesoTotal = sum (map peso kit)
    putStrLn $ "Peso total do kit: " ++ show pesoTotal ++ "kg"
    if pesoTotal == 0 then
        putStrLn "A mochila está vazia."
    else if pesoTotal > capacidadeMochila then
        putStrLn "A mochila está cheia! Não é possível adicionar mais itens."
    else
        putStrLn $ "Ainda restam " ++ show (capacidadeMochila - pesoTotal) ++ "kg de espaço na mochila."
