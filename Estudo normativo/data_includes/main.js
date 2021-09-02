PennController.ResetPrefix(null) // Keep here

Sequence("tcle", "form", "instru", "trein", "FimDoTreino", rshuffle("experiment", "distrat"), SendResults() , "fim")

//--------------------------------- TCLE ---------------------------------

newTrial("tcle",
    newHtml("meutcle", "tcle.html")
        .checkboxWarning("Você deve dar seu consentimento para participar da pesquisa.")
        .print()
    
    ,
    newButton("CONTINUAR")
    .css("font-size", "20px")
    .center()
    .print()
    .wait(
            getHtml("meutcle").test.complete()
                .failure(getHtml("meutcle").warn())
        )
)
,

//--------------------------------- Formulário de dados pessoais ---------------------------------

newTrial("form",
    newText("<b>Por favor, preencha alguns dados pessoais: </b>")
        .cssContainer({"font-size": "125%"})
        .print()
        .center()
    ,
    
    // ------------- Nome ------------------------
    newText("<br>Nome (ou iniciais):")
        .cssContainer({"font-size": "125%"})
        .print()
        .center()
    ,
    newTextInput("nome", "")
        .center()
        .css("margin","1em")
        .print()
        .log()
    ,
    
    newVar("NOME")
        .global()
        .set(getTextInput("nome"))
    ,
    
    // ------------- Gênero ------------------------
    newText("Gênero:")
        .cssContainer({"font-size": "125%"})
        .print()
        .center()
    ,
    newDropDown("genero", "selecionar")
        .css("font-size", "20px")
        .center()
        .add("Feminino", "Masculino", "Outro", "Prefiro não informar")
        .center()
        .css("margin","1em")
        .print()
        .log()
    ,
    
    newVar("GENERO")
        .global()
        .set(getDropDown("genero"))
    ,
    
    // ------------- Escolaridade ------------------------
    newText("Escolaridade:")
        .cssContainer({"font-size": "125%"})
        .print()
        .center()
    ,
    newDropDown("escolaridade", "selecionar")
        .css("font-size", "20px")
        .center()
        .add("Ensino Fundamental incompleto",
                "Ensino Fundamental completo",
                "Ensino Médio incompleto",
                "Ensino Médio completo",
                "Ensino Superior incompleto",
                "Ensino Superior Completo",
                "Pós-Graduação")
        .center()
        .css("margin","1em")
        .print()
        .log()
    ,
    
    newVar("ESCOLARIDADE")
        .global()
        .set(getDropDown("escolaridade"))
    ,
    
    // ------------- Nativo ------------------------
    newText("Você é falante nativo de português brasileiro?")
        .cssContainer({"font-size": "125%"})
        .print()
        .center()
    ,
    newDropDown("nativo", "selecionar")
        .css("font-size", "20px")
        .center()
        .add("Sim", "Não")
        .center()
        .css("margin","1em")
        .print()
        .log()
    ,
    
    newVar("NATIVO")
        .global()
        .set(getDropDown("nativo"))
    ,
    
    newButton("CONTINUAR")
        .css("font-size", "20px")
        .center()
        .print()
        .wait(
            getDropDown("genero").test.selected()
                .and(getDropDown("escolaridade").test.selected())
                    //.and(getDropDown("nativo").test.selected())
                        .failure(
                            newText('erro_preench', "Todos os campos são obrigatórios.")
                                .css("color", "red")
                                .print()
                                .center()
                                )
            )
)
,

// --------------------------------- INSTRUÇÕES ---------------------------------
newTrial("instru",

    newHtml("instrucoes", "Instrucoes.html")
        .print()
    
    ,
    newButton("meubotao", "Sim!")
        .css("margin","1em")
        .css("font-size", "20px")
        .center()
        .print()
        .wait() 
)
,

// --------------------------------- TREINO ---------------------------------
Template("Treino.csv", trn =>
    newTrial("trein",

//----------------------------------------------------------------------------------------    
// ETAPA 1 - Apresentação da sentença
//----------------------------------------------------------------------------------------    
    newText("minhasentenca", trn.frase) // Insere a sentença a ser julgada.
        .cssContainer({"font-size": "125%"})
        .print("center at 50%", "bottom at 25%")
    ,
    
    newTimer("tempo.apres", 4000)
        .start()
        .wait()
    ,
    
    clear()
    ,
//----------------------------------------------------------------------------------------    
// ETAPA 2 - Pergunta de interpretação da sentença
//----------------------------------------------------------------------------------------    
    newText("pergunta", trn.pergunta)
        .cssContainer({"font-size": "125%"})
        .print("center at 50%", "bottom at 25%")
    ,
    
    newTimer("tempo.esgotado", 4000)
        .start()
    ,
    
    newScale("resposta", trn.respA, trn.respB)
        .cssContainer({"font-size": "125%"})
        .radio()
        .labelsPosition("right")
        .vertical()
        .print("center at 50%", "bottom at 32%")
        .callback(getTimer("tempo.esgotado").stop())
        .log()
    ,
    
    getTimer("tempo.esgotado")
        .wait()
    ,
    
    clear()
    ,
    
    getScale("resposta")
        .test.selected()
        .success(
//----------------------------------------------------------------------------------------
// ETAPA 3 - Grau de confiança na resposta dada - Só ocorre se o tempo não tiver esgotado
//----------------------------------------------------------------------------------------    
    newText("pergunta", "Você está seguro da sua resposta?")
        .cssContainer({"font-size": "125%"})
        .print("center at 50%", "bottom at 25%")
    ,
    
    newScale("minhaescala", "1", "2", "3", "4","5")
        .cssContainer({"font-size": "125%"})
        .before(newText("iniciodaescala", "Totalmente inseguro"))  
        .after(newText("finaldaescala", "Totalmente seguro")) 
        .radio()
        .labelsPosition("top")
        .print("center at 50%", "bottom at 32%")
        .wait(getScale("minhaescala").test.selected())
        .log()
 //---------------------------------------------------------------------------------------- 
 )
        .failure(
            newAudio("falha", "Error.mp3")
                .play()
            ,
            newText("Tempo esgotado!")
                .cssContainer({"font-size": "150%", "color": "red"})
                .print("center at 50%", "bottom at 32%")
                .center()
            ,
            
            getAudio("falha")
                .wait()
        )
    ,
    
    clear()
    ,

    newButton("validation", "Próxima")
        .css("margin","1em")
        .css("font-size", "20px")
        .center()
        .print("center at 50%", "bottom at 32%")
        .wait()
        .remove()
    )
    .log("vies", trn.vies)
    .log("item", trn.item)
    .log("frase", trn.frase)
    .log("nome", getVar("NOME"))
    .log("genero", getVar("GENERO"))
    .log("escolaridade", getVar("ESCOLARIDADE"))
    .log("nativo", getVar("NATIVO"))
)
,

newTrial("FimDoTreino",
    defaultText
        .cssContainer({"font-size": "125%"})
        .center()
        .print()
    ,

    newText("O treino acabou!")
    ,
    
    newText("<br>Vamos começar?<br><br>")
    ,

    newButton("meubotao", "Sim!")
        .css("margin","1em")
        .css("font-size", "20px")
        .center()
        .print()
        .wait()
)
,


// --------------------------------- EXPERIMENTAIS ---------------------------------
Template("Experimentais.csv", exp =>
    newTrial("experiment",

//----------------------------------------------------------------------------------------    
// ETAPA 1 - Apresentação da sentença
//----------------------------------------------------------------------------------------    
    newText("minhasentenca", exp.frase) // Insere a sentença a ser julgada.
        .cssContainer({"font-size": "125%"})
        .print("center at 50%", "bottom at 25%")
    ,
    
    newTimer("tempo.apres", 4000)
        .start()
        .wait()
    ,
    
    clear()
    ,
//----------------------------------------------------------------------------------------    
// ETAPA 2 - Pergunta de interpretação da sentença
//----------------------------------------------------------------------------------------    
    newText("pergunta", exp.pergunta)
        .cssContainer({"font-size": "125%"})
        .print("center at 50%", "bottom at 25%")
    ,
    
    newTimer("tempo.esgotado", 4000)
        .start()
    ,
    
    newScale("resposta", exp.respA, exp.respB)
        .cssContainer({"font-size": "125%"})
        .radio()
        .labelsPosition("right")
        .vertical()
        .print("center at 50%", "bottom at 32%")
        .callback(getTimer("tempo.esgotado").stop())
        .log()
    ,
    
    getTimer("tempo.esgotado")
        .wait()
    ,
    
    clear()
    ,
    
    getScale("resposta")
        .test.selected()
        .success(
//----------------------------------------------------------------------------------------
// ETAPA 3 - Grau de confiança na resposta dada - Só ocorre se o tempo não tiver esgotado
//----------------------------------------------------------------------------------------    
    newText("pergunta", "Você está seguro da sua resposta?")
        .cssContainer({"font-size": "125%"})
        .print("center at 50%", "bottom at 25%")
    ,
    
    newScale("minhaescala", "1", "2", "3", "4","5")
        .cssContainer({"font-size": "125%"})
        .before(newText("iniciodaescala", "Totalmente inseguro"))  
        .after(newText("finaldaescala", "Totalmente seguro")) 
        .radio()
        .labelsPosition("top")
        .print("center at 50%", "bottom at 32%")
        .wait(getScale("minhaescala").test.selected()).log()
 //---------------------------------------------------------------------------------------- 
 )
        .failure(
            newAudio("falha", "Error.mp3")
                .play()
            ,
            newText("Tempo esgotado!")
                .cssContainer({"font-size": "150%", "color": "red"})
                .print("center at 50%", "bottom at 32%")
                .center()
            ,
            getAudio("falha")
                .wait()
        )
    ,
    
    clear()
    ,

    newButton("validation", "Próxima")
        .css("margin","1em")
        .css("font-size", "20px")
        .center()
        .print("center at 50%", "bottom at 32%")
        .wait()
        .remove()
    )
    .log("vies", exp.vies)
    .log("item", exp.item)
    .log("frase", exp.frase)
    .log("nome", getVar("NOME"))
    .log("genero", getVar("GENERO"))
    .log("escolaridade", getVar("ESCOLARIDADE"))
    .log("nativo", getVar("NATIVO"))
)
,
// --------------------------------- DISTRATORES ---------------------------------
Template("Distratores.csv", dist =>
    newTrial("distrat",

//----------------------------------------------------------------------------------------    
// ETAPA 1 - Apresentação da sentença
//----------------------------------------------------------------------------------------    
    newText("minhasentenca", dist.frase) // Insere a sentença a ser julgada.
        .cssContainer({"font-size": "125%"})
        .print("center at 50%", "bottom at 25%")
    ,
    
    newTimer("tempo.apres", 4000)
        .start()
        .wait()
    ,
    
    clear()
    ,
//----------------------------------------------------------------------------------------    
// ETAPA 2 - Pergunta de interpretação da sentença
//----------------------------------------------------------------------------------------    
    newText("pergunta", dist.pergunta)
        .cssContainer({"font-size": "125%"})
        .print("center at 50%", "bottom at 25%")
    ,
    
    newTimer("tempo.esgotado", 4000)
        .start()
    ,
    
    newScale("resposta", dist.respA, dist.respB)
        .cssContainer({"font-size": "125%"})
        .radio()
        .labelsPosition("right")
        .vertical()
        .print("center at 50%", "bottom at 32%")
        .callback(getTimer("tempo.esgotado").stop())
        .log()
    ,
    
    getTimer("tempo.esgotado")
        .wait()
    ,
    
    clear()
    ,
    
    getScale("resposta")
        .test.selected()
        .success(
//----------------------------------------------------------------------------------------
// ETAPA 3 - Grau de confiança na resposta dada - Só ocorre se o tempo não tiver esgotado
//----------------------------------------------------------------------------------------    
    newText("pergunta", "Você está seguro da sua resposta?")
        .cssContainer({"font-size": "125%"})
        .print("center at 50%", "bottom at 25%")
    ,
    
    newScale("minhaescala", "1", "2", "3", "4","5")
        .cssContainer({"font-size": "125%"})
        .before(newText("iniciodaescala", "Totalmente inseguro"))  
        .after(newText("finaldaescala", "Totalmente seguro")) 
        .radio()
        .labelsPosition("top")
        .print("center at 50%", "bottom at 32%")
        .wait(getScale("minhaescala").test.selected())
        .log()
 //---------------------------------------------------------------------------------------- 
 )
        .failure(
            newAudio("falha", "Error.mp3")
                .play()
            ,
            newText("Tempo esgotado!")
                .cssContainer({"font-size": "150%", "color": "red"})
                .print("center at 50%", "bottom at 32%")
                .center()
            ,
            getAudio("falha")
                .wait()
        )
    ,
    
    clear()
    ,

    newButton("validation", "Próxima")
        .css("margin","1em")
        .css("font-size", "20px")
        .center()
        .print("center at 50%", "bottom at 32%")
        .wait()
        .remove()
    )
    .log("vies", dist.vies)
    .log("item", dist.item)
    .log("frase", dist.frase)
    .log("nome", getVar("NOME"))
    .log("genero", getVar("GENERO"))
    .log("escolaridade", getVar("ESCOLARIDADE"))
    .log("nativo", getVar("NATIVO"))
)

/*
,
// Tela final
newTrial("fim",

    defaultText()
        //.cssContainer({"font-size": "125%"})
        .print()
        .center()
        
    ,
    newText("Acabamos! Obrigado pela colaboração!")

    ,
    newText("Se quiser saber mais sobre essa pesquisa, entre em contato: igordeo.costa@gmail.com ou visite <a href='https://igordeo-costa.github.io/' target='_blank'>o blog do pesquisaror.</a>.")

    ,
    newText("Visite, também, o site do <a href='http://www.lapal.letras.puc-rio.br/' target='_blank'>Laboratório de Psicolinguística e Aquisição da Linguagem da PUC-Rio - LAPAL/PUC-Rio</a>.")

    ,
    newButton("Sair do experimento")
        .print()
        .wait()
    )

*/