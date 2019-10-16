# work-at-olist-data

# 1 - Configurando o local de trabalho
setwd("C:/FCD/BigDataRAzure/poc-work-at-olist-data")
getwd()

# 2 - Base pagamentos
pagamentos = read.csv2("datasets/olist_order_payments_dataset.csv" , sep=",")
View(head(pagamentos))
str(pagamentos)

mean(as.double(pagamentos$payment_value))
median(as.double(pagamentos$payment_value))
min(as.double(pagamentos$payment_value))
max(as.double(pagamentos$payment_value))
summary(pagamentos)

pedidos = read.csv2("datasets/olist_orders_dataset.csv" , sep=",")
View(head(pedidos))
str(pedidos)
levels(pedidos$order_status)
# considerando apenas os pedidos que não foram cancelados, ou que estão
# indisponíveis, em processamento, aplicação de filtro
pedidosComFiltroDeStatusPedido <- subset(pedidos,pedidos$order_status == 'approved' | 
                 pedidos$order_status  ==  'delivered'
                 | pedidos$order_status  ==  'invoiced'
                 | pedidos$order_status  ==  'shipped',
                 select = c(order_id,order_status,order_purchase_timestamp))
# cruzando os datasets para obter os pagamentos corretos
pedidosMergePagamento <- merge(pedidosComFiltroDeStatusPedido,pagamentos,
           by = 'order_id')
?merge
View(head(pedidosMergePagamento))
str(pedidosMergePagamento)
summary(pedidosMergePagamento)
levels(pedidosMergePagamento$order_purchase_timestamp)


valorMedia = mean(as.double(pedidosMergePagamento$payment_value))
valorMediaFormatado = format(valorMedia,
       trim = TRUE, digits = 7,decimal.mark = ",",
       big.mark = ".",nsmall = 2);       
       
valorMax = max(as.double(pedidosMergePagamento$payment_value))
format(valorMax,
       trim = TRUE, digits = 7,decimal.mark = ",",
       big.mark = ".",nsmall = 2);  
valorMin = min(as.double(pedidosMergePagamento$payment_value))
format(valorMin,
       trim = TRUE, digits = 7,decimal.mark = ",",
       big.mark = ".",nsmall = 2); 

# Extraindo informacoes
# Pegando a menor e a maior data para mostrar no painel
datas = as.Date(pedidosMergePagamento$order_purchase_timestamp)
dataMinima = min(as.integer(datas))
dataMaxima = max(as.integer(datas))

as.integer(data1)
as.POSIXct.Date(17048,format = "Y-%m-%d")

levels(pedidosMergePagamento$order_purchase_timestamp)
View(pedidosMergePagamento$order_purchase_timestamp)
sort(pedidosMergePagamento$order_purchase_timestamp)
# maior data: 2016-09-03 21:00:00 -03
# menor 2018-09-02 21:00:00 -03"
quantSemanas = difftime(dataMinima,dataMaxima, units = 'weeks')
# convertendo para meses: valor/4,345
quantMeses =  quantSemanas/4.345
round(quantMeses) 


quantPagamentosPorTipo = summary(pedidosMergePagamento$payment_type)
quantPagamentosPorTipo['boleto']
quantPagamentosPorTipo['credit_card']
quantPagamentosPorTipo['debit_card']
quantPagamentosPorTipo['voucher']

write.csv2(pedidosMergePagamento,'pedidos3.csv')

# Criando as fatias
fatias = c(quantPagamentosPorTipo['boleto'], quantPagamentosPorTipo['credit_card'],
           quantPagamentosPorTipo['debit_card'],quantPagamentosPorTipo['voucher'])

# Nomeando os labels
meiosPagamento = c("Boleto", "Cartão de Crédito", "Cartão de Débito","Voucher")

# Unindo meios de pagamentos e fatias
meiosPagamento = paste(meiosPagamento, fatias)

# Gráfico dos meios de pagamento
colors()
pie(fatias, labels = meiosPagamento,
    col = c("slategray2", "rosybrown3", "lemonchiffon4", "violetred4"), 
    main ="Tipos de Pagamento")


# descobrindo o 3 ranking de produtos
itensPedidos = read.csv2("datasets/olist_order_items_dataset.csv" , sep=",")
View(itensPedidos)
vetorProdutoTop5 = head(summary(itensPedidos$product_id),3L)
vetorProdutoTop5[1]
nomeProdutosTop = c()


# descobrindo quem são os produtos
produtos = read.csv2("datasets/olist_products_dataset.csv" , sep=",")
View(produtos)


nomeProdutosTop[1] = subset(produtos,produtos$product_id == 'aca2eb7d00ea1a7b8ebd4e68314663af',
       select = c(product_category_name))

nomeProdutosTop[2] = subset(produtos,produtos$product_id == '99a4788cb24856965c36a24e339b6058',
                            select = c(product_category_name))
nomeProdutosTop[3] = subset(produtos,produtos$product_id == '422879e10f46682990de24d770e7f83d',
                            select = c(product_category_name))

#mapa das regiões
localizacaoClientes = read.csv2("datasets/olist_geolocation_dataset.csv" , sep=",")
View(localizacaoClientes)
summary(localizacaoClientes)
estados = summary(localizacaoClientes$geolocation_state)
sort(estados,decreasing = FALSE)
colors()
topFiveEstadosDemandados = c(estados['SP'],estados['MG'],estados['RJ'],estados['RS'],estados['PR'])
# gráfico das top 5 regiões
barplot(topFiveEstadosDemandados,ylab = "Quantidade", xlab = "Estados",main = "Quantidade por Estado",
        col = c("steelblue1", "tan3", "seagreen3","violetred2","red")) 


