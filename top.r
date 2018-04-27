library(networkD3)

redditors <- data.frame(table(reddit$username))

top_5 <- subset(redditors, Freq>173)
# AtheismFTW 176
# cinsere 545
# spez 858
# syncretic 931
# Gangsta_Raper 5608

top_ten <- subset(redditors, Freq>126)
	
# darkreef2 128
# atomichugbot 144
# pepsi_next 144
# dummystupid 169
# Scopolamina 173
# AtheismFTW 176
# cinsere 545
# spez 858
# syncretic 931
# Gangsta_Raper 5608

net_5 <- subset(top_5, reddit.username == top_5$Var1[1] | reddit.username == top_5$Var1[2] |reddit.username == top_5$Var1[3] | reddit.username == top_5$Var1[4] |reddit.username == top_5$Var1[5])

net_10 <- subset(top_ten, reddit.username == top_ten$Var1[1] | reddit.username == top_ten$Var1[2] |reddit.username == top_ten$Var1[3] | reddit.username == top_ten$Var1[4] |reddit.username == top_ten$Var1[5] | reddit.username == top_ten$Var1[6] | reddit.username == top_ten$Var1[7] |reddit.username == top_ten$Var1[8] | reddit.username == top_ten$Var1[9] |reddit.username == top_ten$Var1[10])

top_5_network <- simpleNetwork(net_5,  Source = 1, Target = 2, height = 600, width = 600, linkDistance = 60, charge = -20, opacity = 0.6, fontSize = 14, fontFamily = 'Arial', zoom = T)

top_ten_network <- simpleNetwork(net_10, height = 600, width = 600, linkDistance = 60, charge = -20, opacity = 0.6, fontSize = 14, fontFamily = 'latio', zoom = T)

saveNetwork(top_5_network, file = 'top_5_redditor_network.html')

saveNetwork(top_ten_network, file = 'top_ten_network.html')