shoes_vendas <- read.csv("shoes.csv")

shoes_midia <- read.csv("midia.csv")

shoes_vendas[with(shoes_vendas, order(shoes_vendas$quantidade_vendida)), ]


##### melhor vendas ALL ##########
## 10 melhores vendas de todas regioes!
melhor_venda_all <- shoes_vendas[with(shoes_vendas, order(shoes_vendas$quantidade_vendida,decreasing = T)), ]
melhor_venda_all[1:10,]



##### melhor venda regiao norte!
melhor_venda_norte <- melhor_venda_all[melhor_venda_all$regiao == "norte", ]
melhor_venda_norte[1,]


##### melhor venda regiao nordeste!
melhor_venda_nordeste <- melhor_venda_all[melhor_venda_all$regiao == "nordeste", ]
melhor_venda_nordeste[1,]


##### melhor venda regiao sul!
melhor_venda_sul <- melhor_venda_all[melhor_venda_all$regiao == "sul", ]
melhor_venda_sul[1,]


##### melhor venda regiao suldeste!
melhor_venda_suldeste <- melhor_venda_all[melhor_venda_all$regiao == "suldeste", ]
melhor_venda_suldeste[1,]
