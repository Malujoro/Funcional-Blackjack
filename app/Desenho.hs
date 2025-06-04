module Desenho (
    logo,
    contrato,
    desenho,
) where

import System.Info (os)
import System.Process (callCommand)

limparTela :: IO ()
limparTela = callCommand $ if os == "mingw32" then "cls" else "clear"

desenho :: IO ()
desenho = putStrLn "\
\           /\\,%,_                              \n\
\           \\%%%/,\\                             \n\
\         _.-\"%%|//%                            \n\
\       .'  .-\"  /%%%                           \n\
\   _.-'_.-\" 0)   \\%%%                          \n\
\  /.\\.'           \\%%%                         \n\
\  \\ /      _,      %%%                         \n\
\   `\"---\"~`\\   _,*'\\%%'   _,--\"\"\"\"-,%%,       \n\
\            )*^     `\"\"~~`          \\%%%,     \n\
\          _/                         \\%%%     \n\
\      _.-`/                           |%%,___  \n\
\  _.-\"   /      ,           ,        ,|%%   .`\\\n\
\ /\\     /      /             `\\       \\%'   \\ /\n\
\ \\ \\ _,/      /`~-._         _,`\\      \\`\"\"~~`\n\
\  `\"` /-.,_ /'      `~\"----\"~    `\\     \\     \n\
\      \\___,'                       \\.-\"`/     \n\
\                                    `--'      "

logo :: IO ()
logo = putStrLn "\
\######   ####       ##       ####   ###  ##     ####    ##       ####   ###  ##  \n\
\ ##  ##   ##       ####     ##  ##   ##  ##      ##    ####     ##  ##   ##  ##  \n\
\ ##  ##   ##      ##  ##   ##        ## ##       ##   ##  ##   ##        ## ##   \n\
\ #####    ##      ##  ##   ##        ####        ##   ##  ##   ##        ####    \n\
\ ##  ##   ##   #  ######   ##        ## ##   ##  ##   ######   ##        ## ##   \n\
\ ##  ##   ##  ##  ##  ##    ##  ##   ##  ##  ##  ##   ##  ##    ##  ##   ##  ##  \n\
\######   #######  ##  ##     ####   ###  ##   ####    ##  ##     ####   ###  ##  \n"


contrato :: IO ()
contrato = do
    limparTela
    putStrLn "Quando você entrou aqui, aceitou as regras. E agora que não pode pagar... está pronto para assinar com algo mais valioso?"
    getLine
    desenhoContrato
    getLine
    limparTela
    desenhoSelo
    return ()

desenhoContrato :: IO ()
desenhoContrato = putStrLn "\
\      ╔══════════════════════════════════════════════╗\n\
\      ║         ❖ CONTRATO DE CONTINUIDADE ❖         ║\n\
\      ║          — Registro da Companhia —           ║\n\
\      ╚══════════════════════════════════════════════╝\n\
\\n\
\   Pelo presente contrato, o jogador reconhece e concorda com\n\
\   as condições excepcionais de continuidade ofertadas pela Casa.\n\
\   O jogador aceita os termos expressos e implícitos, permitindo que\n\
\   o jogo prossiga conforme as diretrizes internas estabelecidas.\n\
\\n\
\   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\
\   ✦ O jogo não termina quando seu crédito finda.      \n\
\   ✦ Você foi selecionado para continuar apostando... \n\
\     com aquilo que ainda lhe resta.\n\
\   ✦ Ao assinar, autoriza a Casa a estender crédito     \n\
\     em troca de fidelidade incondicional.\n\
\   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\
\\n\
\   Ao assinar este termo, você renuncia à sua liberdade financeira e...\n\
\   autoriza a Casa a cobrar, no presente ou no além, tudo aquilo que lhe é devido.\n\
\\n\
\   Pressione ENTER para selar este contrato..."

desenhoSelo :: IO ()
desenhoSelo = putStrLn "\
\       ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░\n\
\       ░  ☼        S E L O   D A   C A S A        ☼  ░\n\
\       ░   ⟡  pacto firmado sob cláusula eterna ⟡    ░\n\
\       ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░"
