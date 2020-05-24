library(dplyr)
library(stringr)
library(plyr)

###PART ONE

sumo_df_clean <- read.csv("~/Desktop/sumo_df_clean.csv", header=TRUE)
##Add two cols rikishi1_result and rikishi2_result for later use.
sumo_df_clean <- sumo_df_clean %>% mutate(rikishi1_result = str_sub(V6, 1, 3), rikishi2_result = str_sub(V12, 1, 3))
#needed a win and won at the very last minute.
critical_game_won <- sumo_df_clean %>% filter((rikishi1_result == '8-7' & V7 == '1') | (rikishi2_result == '8-7' & V13 == '1'))
#View(critical_game_won)
nrow(critical_game_won)

#needed a win but lost at the last moment.
critical_game_lost <- sumo_df_clean %>% filter((rikishi1_result == '7-8' & V7 == '0') | (rikishi2_result == '7-8' & V13 == '0'))
nrow(critical_game_lost)
critical_game_winning_rate <- nrow(critical_game_won)/(nrow(critical_game_won) + nrow(critical_game_lost))


#make sure the 1st col contains the names of the winner, 2nd col names of the opponents.
winner_part1 <- critical_game_won %>% filter(V7 == '1') %>% select(V1, V5, V11)#filter winner/oppon in rikishi1 col
winner_part2 <- critical_game_won %>% filter(V13 == '1') %>% select(V1, V11, V5)#filter winner/oppon in rikishi2 col
winner_names <- bind_rows(winner_part1, winner_part2)#stack them together
winner_names <- winner_names %>% rename(Date=V1, Winner=V5, opponent=V11)
winner_names <- winner_names[order(winner_names$Date), ]#sort everything base on date.
distinct_winner_names <- unique(winner_names$Winner)
#see the number of distinct winner names in critical games------only 199 names
distinct_opponent_names <- unique(winner_names$opponent)
#see the number of distinct opponent names in critical games-----only 193 names 




#regular game winning rate for the same group of wrestlers
regular_win <- sumo_df_clean %>% filter((V5 %in% distinct_winner_names & V7 == '1') | (V11 %in% distinct_winner_names & V13 == '1'))
regular_lose <- sumo_df_clean %>% filter((V5 %in% distinct_winner_names & V7 == '0') | (V11 %in% distinct_winner_names & V13 == '0'))
regular_game_winning_rate <- nrow(regular_win)/(nrow(regular_win) + nrow(regular_lose))
##The result is around 0.4979, which is way lower than 70% i.e. the winning rate of critical games






###PART TWO next tournament, when they meet again, the odds are turned. 


#number of distinct names across the entire dataset.
# p1 <- unique(sumo_df_clean$V5)
# p2 <- unique(sumo_df_clean$V11)
# p1 <- as.data.frame(p1)
# p2 <- as.data.frame(p2)
# distinct_names <- bind_rows(p1,p2)
# unique(distinct_names$p1)
#across 16 yrs, only has 364 unique sumo wrestlers.

#filter out all the games that involved the wrestlers in those critical games.



# rematch <- sumo_df_clean %>% filter((V5 %in% distinct_winner_names & V11 %in% distinct_opponent_names) | (V11 %in% distinct_winner_names & V5 %in% distinct_opponent_names))
# rematch <- rematch[order(rematch$V1, rematch$V5), ]
# #which(lag(rematch_one_to_one$V5)==rematch_one_to_one$V5 & dplyr::lag((rematch_one_to_one$rikishi1_result=='8-7' & rematch_one_to_one$V7 == '1') | (rematch_one_to_one$rikishi2_result=='8-7' & rematch_one_to_one$V13 == '1')))
# ind2 <- which(critical_game_won$V5 == rematch$V5 & critical_game_won$V11 == rematch$V11)
# rematch_one_to_one2 <- rematch[ind2,]
# View(rematch_one_to_one2)




#tt <- table(rematch_one_to_one$V5)
#> xy <- subset(rematch_one_to_one, V5 %in% names(tt[tt < 2]))
# 
# ind1 <- which(lag(rematch_one_to_one$V5)==rematch_one_to_one$V5 & dplyr::lag((rematch_one_to_one$rikishi1_result=='8-7' & rematch_one_to_one$V7 == '1') | (rematch_one_to_one$rikishi2_result=='8-7' & rematch_one_to_one$V13 == '1')))
# rematch_one_to_one_final <- rematch_one_to_one[ind1, ]




iid <- 1
rematch_one_to_one <- NULL #reestablish one-to-one scenario to select all the matches the two wrestlers meet again.
for(iid in 1:nrow(critical_game_won)) {
  ind2 <- which(sumo_df_clean$V5 == critical_game_won$V5[iid] & sumo_df_clean$V11 == critical_game_won$V11[iid])
  rematch_one_to_one <- rbind(rematch_one_to_one, data.frame(sumo_df_clean[ind2,]))
  # if (sumo_df_clean$V5 == critical_game_won$V5[iid]){
  #   if (sumo_df_clean$V11 == critical_game_won$V11[iid]){
  #     row <- sumo_df_clean[iid, ]
  #     rematch_one_to_one <- rbind(rematch_one_to_one, data.frame(row))      
  #   }
  # 
  # }
 # df <- sumo_df_clean %>% filter(V5 == critical_game_won$V5[iid]) %>% filter(V11 == critical_game_won$V11[iid])
  #rematch_one_to_one <- rbind(rematch_one_to_one, data.frame(df))#save the new dataframe into rematch_one_to_one
}
#rematch_one_to_one <- rematch_one_to_one[order(rematch_one_to_one$V1),]
rematch_one_to_one <- rematch_one_to_one %>% group_by(V1)
View(rematch_one_to_one)

# 
# rematch_one_to_one_clean2 <- NULL
# ii <- 1
# for (ii in 1:length(rematch_one_to_one)) {
#   row <- rematch_one_to_one[ii, ]
#   # if ((row$rikishi1_result == '8-7' & row$V7 == '1') | (row$rikishi2_result == '8-7' & row$V13 == '1')) {
#     print(row)
#     #df <- rematch_one_to_one[ii+1, ]
#     #print(df <- rematch_one_to_one[ii+1, ])
#     #rematch_one_to_one_clean2 <- rbind(rematch_one_to_one_clean2, data.frame(df))
#   }
#   #rematch_one_to_one_clean2 <- rbind(rematch_one_to_one_clean2, data.frame(df))
#   
# }
# View(rematch_one_to_one_clean2)


# ind3 <- which((lag(rematch$V5)==rematch$V5 | lag(rematch$V11)==rematch$V11) & (dplyr::lag((rematch$rikishi1_result=='8-7' & rematch$V7 == '1') | (rematch$rikishi2_result=='8-7' & rematch$V13 == '1'))))
# ind3
# # ind3 <- which(lag(sumo_df_clean$V5)==sumo_df_clean$V5 & dplyr::lag((sumo_df_clean$rikishi1_result=='8-7' & sumo_df_clean$V7 == '1') | (sumo_df_clean$rikishi2_result=='8-7' & sumo_df_clean$V13 == '1')))
# rematch_one_to_one_final3 <- rematch[c(ind3,ind3+1), ]
# nrow(rematch_one_to_one_final)
# View(rematch_one_to_one_final3)

#####final filter, select the games between wrestlers who just finished their critical game.

ind1 <- which(lag(rematch_one_to_one$V5)==rematch_one_to_one$V5 & dplyr::lag((rematch_one_to_one$rikishi1_result=='8-7' & rematch_one_to_one$V7 == '1') | (rematch_one_to_one$rikishi2_result=='8-7' & rematch_one_to_one$V13 == '1')))
rematch_one_to_one_final <- rematch_one_to_one[ind1, ]


number_of_rematch_wins <- rematch_one_to_one_final %>% filter(V7==1)
number_of_rematch_losses <- rematch_one_to_one_final %>% filter(V7==0)
nrow(number_of_rematch_wins)
nrow(number_of_rematch_losses)
rematch_winning_rate <- nrow(number_of_rematch_wins)/(nrow(number_of_rematch_wins)+nrow(number_of_rematch_losses))
rematch_winning_rate

rematch_losing_rate <- nrow(number_of_rematch_losses)/(nrow(number_of_rematch_wins)+nrow(number_of_rematch_losses))
rematch_losing_rate


#tt <- table(rematch_one_to_one$V5)
#> xy <- subset(rematch_one_to_one, V5 %in% names(tt[tt < 2]))




#rematch_one_to_one_simplified <- rematch_one_to_one %>% select(V1, V5, V7, V11, V13)
#View(rematch_one_to_one_simplified)
##exclude all the critical wins i.e.(8-7 with final game marked as win) for rematch one to one, cuz we already did analysis on the wins on critical games
##now we care about the win/loss rate for the rest of the rematches.
#rematch_one_to_one_clean <- rematch_one_to_one %>% filter(!((rikishi1_result == '8-7' & V7 == '1') | (rikishi2_result == '8-7' & V13 == '1')))
#number_of_rematch_wins <- rematch_one_to_one_clean %>% filter(V7==1)
#number_of_rematch_losses <- rematch_one_to_one_clean %>% filter(V7==0)
#nrow(number_of_rematch_wins)
#nrow(number_of_rematch_losses)
#rematch_winning_rate <- nrow(number_of_rematch_wins)/(nrow(number_of_rematch_wins)+nrow(number_of_rematch_losses))
#rematch_winning_rate

#rematch_losing_rate <- nrow(number_of_rematch_losses)/(nrow(number_of_rematch_wins)+nrow(number_of_rematch_losses))
#rematch_losing_rate