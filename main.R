library(plotly)


shoes_vendas <- read.csv("shoes.csv")

shoes_midia <- read.csv("midia.csv")

shoes_vendas[with(shoes_vendas, order(shoes_vendas$quantidade_vendida)), ]


##### melhor vendas ALL ##########
## 10 melhores vendas de todas regioes!
melhor_venda_all <- shoes_vendas[with(shoes_vendas, order(shoes_vendas$quantidade_vendida,decreasing = T)), ]

data_plot <-  melhor_venda_all[1:10,]
data_plot[,1] <- paste(data_plot[,1],data_plot[,5])

p <- plot_ly(data_plot, labels = ~nome, values = ~quantidade_vendida, type = 'pie',
             textposition = 'inside',
             textinfo = 'label+percent',
             hoverinfo = 'text',
             text = ~paste('$', quantidade_vendida, ' billions'),
             marker = list(colors = colors,
                           line = list(color = '#FFFFFF', width = 1)),
             #The 'pull' attribute can also be used to create space between the sectors
             showlegend = FALSE) %>%
  layout(title = '10 melhores vendas em todas as regiões',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

p




########################################################################
##### melhor venda regiao norte!
melhor_venda_norte <- melhor_venda_all[melhor_venda_all$regiao == "norte", ]
melhor_venda_norte <- melhor_venda_norte[1:5,]


p <- plot_ly(melhor_venda_norte, labels = ~nome, values = ~quantidade_vendida, type = 'pie',
             textposition = 'inside',
             textinfo = 'label+percent',
             hoverinfo = 'text',
             text = ~paste(quantidade_vendida, ' Unidades'),
             marker = list(colors = colors,
                           line = list(color = '#FFFFFF', width = 1)),
             #The 'pull' attribute can also be used to create space between the sectors
             showlegend = FALSE) %>%
  layout(title = 'Melhores vendas na região NORTE',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

p




########################################################################
##### melhor venda regiao nordeste!
melhor_venda_nordeste <- melhor_venda_all[melhor_venda_all$regiao == "nordeste", ]
melhor_venda_nordeste <- melhor_venda_nordeste[1:5,]


p <- plot_ly(melhor_venda_nordeste, labels = ~nome, values = ~quantidade_vendida, type = 'pie',
             textposition = 'inside',
             textinfo = 'label+percent',
             hoverinfo = 'text',
             text = ~paste(quantidade_vendida, ' Unidades'),
             marker = list(colors = colors,
                           line = list(color = '#FFFFFF', width = 1)),
             #The 'pull' attribute can also be used to create space between the sectors
             showlegend = FALSE) %>%
  layout(title = 'Melhores vendas na região NORDESTE',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

p


########################################################################
##### melhor venda regiao sul!
melhor_venda_sul <- melhor_venda_all[melhor_venda_all$regiao == "sul", ]
melhor_venda_sul <- melhor_venda_sul[1:5,]

p <- plot_ly(melhor_venda_sul, labels = ~nome, values = ~quantidade_vendida, type = 'pie',
             textposition = 'inside',
             textinfo = 'label+percent',
             hoverinfo = 'text',
             text = ~paste(quantidade_vendida, ' Unidades'),
             marker = list(colors = colors,
                           line = list(color = '#FFFFFF', width = 1)),
             #The 'pull' attribute can also be used to create space between the sectors
             showlegend = FALSE) %>%
  layout(title = 'Melhores vendas na região SUL',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

p


########################################################################
##### melhor venda regiao suldeste!
melhor_venda_suldeste <- melhor_venda_all[melhor_venda_all$regiao == "suldeste", ]
melhor_venda_suldeste <- melhor_venda_suldeste[1:5,]

p <- plot_ly(melhor_venda_suldeste, labels = ~nome, values = ~quantidade_vendida, type = 'pie',
             textposition = 'inside',
             textinfo = 'label+percent',
             hoverinfo = 'text',
             text = ~paste(quantidade_vendida, ' Unidades'),
             marker = list(colors = colors,
                           line = list(color = '#FFFFFF', width = 1)),
             #The 'pull' attribute can also be used to create space between the sectors
             showlegend = FALSE) %>%
  layout(title = 'Melhores vendas na região SULDESTE',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

p


########################################################################
########################################################################
## MULHERS vs HOMENS
vendas_male <- melhor_venda_all[melhor_venda_all$genero == "masculino", ]
vendas_female <- melhor_venda_all[melhor_venda_all$genero == "feminino", ]

vendas_male_female <- matrix(c("masculino", "feminino",0,0), nrow=2, ncol=2) 
colnames(vendas_male_female) <- c("genero","quantidade_vendida")

vendas_male_female <- as.data.frame(vendas_male_female, stringsAsFactors=FALSE)
vendas_male_female$quantidade_vendida[[1]] <- sum(vendas_male[,6])
vendas_male_female$quantidade_vendida[[2]] <- sum(vendas_female[,6])
vendas_male_female


vendas_male_female <- matrix(c(".",0,0), nrow=1, ncol=3) 
colnames(vendas_male_female) <- c("generos","masculino", "feminino")

vendas_male_female <- as.data.frame(vendas_male_female, stringsAsFactors=FALSE)
vendas_male_female[[2]] <- sum(vendas_male[,6])
vendas_male_female[[3]] <- sum(vendas_female[,6])
vendas_male_female


p <- plot_ly(vendas_male_female, x = ~generos, y = ~masculino, type = 'bar', name = 'masculino') %>%
  add_trace(y = ~feminino, name = 'feminino') %>%
  layout(title = "Total de vendas por região",
         yaxis = list(title = 'Unidades vendidas'), barmode = 'group')
p


########################################################################
########################################################################
## Total de vendas por região

# vendas <- matrix(c(".",0,0,0,0), nrow=1, ncol=5) 
# colnames(vendas) <- c("regioes",paste(melhor_venda_norte[1,1]), paste(melhor_venda_nordeste[1,1]), paste(melhor_venda_sul[1,1]),paste(melhor_venda_suldeste[1,1]))
# vendas
# vendas <- as.data.frame(vendas, stringsAsFactors=FALSE)
# vendas[[2]] <- sum(vendas_male[,6])
# vendas[[3]] <- sum(vendas_female[,6])
# vendas


vendas <- matrix(c(".",0,0,0,0), nrow=1, ncol=5) 
colnames(vendas) <- c("regioes","norte", "nordeste","sul","suldeste")
vendas

vendas <- as.data.frame(vendas, stringsAsFactors=FALSE)
vendas[[2]] <- sum(melhor_venda_norte[,6])
vendas[[3]] <- sum(melhor_venda_nordeste[,6])
vendas[[4]] <- sum(melhor_venda_sul[,6])
vendas[[5]] <- sum(melhor_venda_suldeste[,6])
vendas


p <- plot_ly(vendas, x = ~regioes, y = ~norte, type = 'bar', name = 'norte') %>%
  add_trace(y = ~nordeste, name = 'nordeste') %>%
  add_trace(y = ~sul, name = 'sul') %>%
  add_trace(y = ~suldeste, name = 'suldeste') %>%
  layout(title = "Total de vendas por região",
         yaxis = list(title = 'Unidades vendidas'), barmode = 'group')
p




