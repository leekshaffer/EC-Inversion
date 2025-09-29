library(tidyverse)
library(readxl)

P_Years <- seq(2000, 2024, by=4)

## House:
H <- read_csv("data/MIT/1976-2024-house.tab")

H_2020 <- H %>% filter(year==2020)

H_2020_NoF <- H_2020 %>% filter(!(state_po %in% c("CT","NY","VA"))) %>%
  arrange(state,district,desc(candidatevotes)) %>%
  group_by(state,district) %>%
  dplyr::mutate(rank=row_number(),
                percentage=candidatevotes/totalvotes,
                party_D=if_else(grepl("DEMOCRAT",party,fixed=TRUE), 1, 0),
                party_R=if_else(grepl("REPUBLICAN",party,fixed=TRUE), 1, 0)) %>%
  ungroup() %>%
  dplyr::select(year,state,state_po,state_fips,state_cen,state_ic,district,candidate,party_D,party_R,candidatevotes,totalvotes,rank,percentage) %>%
  dplyr::filter(percentage > 0.05)

H_2020_F <- H_2020 %>% filter(state_po %in% c("CT","NY","VA")) %>%
  group_by(year,state,state_po,state_fips,state_cen,state_ic,district,candidate) %>%
  dplyr::summarize(candidatevotes=sum(candidatevotes), totalvotes=max(totalvotes),
                   party_D=sum(party=="DEMOCRAT", na.rm=TRUE), party_R=sum(party=="REPUBLICAN", na.rm=TRUE)) %>%
  ungroup() %>%
  arrange(state,district,desc(candidatevotes)) %>%
  group_by(state,district) %>%
  dplyr::mutate(rank=row_number(),
                percentage=candidatevotes/totalvotes) %>%
  ungroup() %>%
  dplyr::select(year,state,state_po,state_fips,state_cen,state_ic,district,candidate,party_D,party_R,candidatevotes,totalvotes,rank,percentage) %>%
  dplyr::filter(percentage > 0.05)

H_2020_full <- H_2020_NoF %>% bind_rows(H_2020_F) %>%
  dplyr::filter(!(candidate %in% c("UNDERVOTES","WRITEIN","OVERVOTES","","BLANK VOTE"))) %>%
  dplyr::mutate(party=if_else(party_D==1, "D", if_else(party_R==1, "R", "O")))

## Senate:

S <- read_csv("data/MIT/1976-2020-senate.csv")

S_2020 <- S %>% filter(year >= 2016)

S_2020_NoF <- S_2020 %>% filter(!(state_po %in% c("CT","NY"))) %>%
  arrange(year,state,state_po,state_fips,state_cen,state_ic,stage,special,desc(candidatevotes)) %>%
  group_by(year,state,state_po,state_fips,state_cen,state_ic,stage,special) %>%
  dplyr::mutate(rank=row_number(),
                percentage=candidatevotes/totalvotes,
                party_D=if_else(grepl("DEMOCRAT",party_simplified,fixed=TRUE), 1, 0),
                party_R=if_else(grepl("REPUBLICAN",party_simplified,fixed=TRUE), 1, 0)) %>%
  ungroup() %>%
  dplyr::select(year,state,state_po,state_fips,state_cen,state_ic,stage,special,candidate,party_D,party_R,candidatevotes,totalvotes,rank,percentage) %>%
  dplyr::filter(percentage > 0.05)

S_2020_F <- S_2020 %>% dplyr::filter(state_po %in% c("CT","NY")) %>%
  group_by(year,state,state_po,state_fips,state_cen,state_ic,stage,special,candidate) %>%
  dplyr::summarize(candidatevotes=sum(candidatevotes), totalvotes=max(totalvotes),
                   party_D=sum(party_simplified=="DEMOCRAT", na.rm=TRUE), party_R=sum(party_simplified=="REPUBLICAN", na.rm=TRUE)) %>%
  ungroup() %>%
  arrange(year,state,state_po,state_fips,state_cen,state_ic,stage,special,desc(candidatevotes)) %>%
  group_by(year,state,state_po,state_fips,state_cen,state_ic,stage,special) %>%
  dplyr::mutate(rank=row_number(),
                percentage=candidatevotes/totalvotes) %>%
  ungroup() %>%
  dplyr::select(year,state,state_po,state_fips,state_cen,state_ic,stage,special,candidate,party_D,party_R,candidatevotes,totalvotes,rank,percentage) %>%
  dplyr::filter(percentage > 0.05)

S_2020_full <- S_2020_NoF %>% bind_rows(S_2020_F) %>%
  dplyr::filter(!(candidate %in% c("UNDERVOTES","WRITEIN","OVERVOTES","","BLANK VOTE"))) %>%
  dplyr::mutate(party_D=if_else((state_po=="VT" & candidate=="BERNIE SANDERS") |
                                  (state_po=="ME" & candidate=="ANGUS S. KING, JR.") | 
                                  (state_po=="WY" & candidate=="MERAV BEN DAVID"), 1, party_D),
                party_R=if_else((state_po=="WY" & candidate=="CYNTHIA M. LUMMIS"), 1, party_R),
                party=if_else(party_D==1, "D", if_else(party_R==1, "R", "O"))) %>%
  arrange(state,year,rank) %>%
  dplyr::filter(!((state_po=="AZ" & year==2016) | (state_po=="GA" & stage=="gen") |
                    (state_po=="MN" & special) | (state_po=="MS" & special) ))

## Pres:

P <- read_csv("data/MIT/1976-2020-president.csv")



P_2020_full <- P %>% filter(year == 2020)  %>%
  dplyr::filter(!(candidate %in% c("UNDERVOTES","WRITEIN","OVERVOTES","","BLANK VOTE"))) %>%
  arrange(year,state,state_po,state_fips,state_cen,state_ic,desc(candidatevotes)) %>%
  group_by(year,state,state_po,state_fips,state_cen,state_ic) %>%
  dplyr::mutate(rank=row_number(),
                percentage=candidatevotes/totalvotes,
                party_D=if_else(grepl("DEMOCRAT",party_simplified,fixed=TRUE), 1, 0),
                party_R=if_else(grepl("REPUBLICAN",party_simplified,fixed=TRUE), 1, 0)) %>%
  ungroup() %>%
  dplyr::select(year,state,state_po,state_fips,state_cen,state_ic,candidate,party_D,party_R,candidatevotes,totalvotes,rank,percentage) %>%
  dplyr::filter(percentage > 0.05)

## Compile:

H_2020_State_Res <- H_2020_full %>% filter(rank==1) %>% group_by(state,state_po) %>% 
  summarize(D=sum(party_D), R=sum(party_R), O=sum(party=="O")) %>%
  ungroup() %>%
  mutate(D=if_else(state_po=="DC", 0, D),
         H=D+R+O)

S_2020_State_Res <- S_2020_full %>% filter(rank==1) %>% group_by(state,state_po) %>% 
  summarize(D=sum(party_D), R=sum(party_R), O=sum(party=="O")) %>%
  ungroup() %>%
  mutate(S=D+R+O)

EVs <- H_2020_State_Res %>% 
  left_join(S_2020_State_Res, by=join_by(state,state_po)) %>%
  dplyr::mutate(EV=if_else(state_po=="DC", 3, H+S)) %>%
  dplyr::select(state,state_po,EV)

P_2020_State_Res <- P_2020_full %>% filter(rank==1) %>%
  left_join(EVs, by=join_by(state,state_po)) %>%
  dplyr::mutate(D=if_else(party_D==1, EV, 0),
                R=if_else(party_R==1, EV, 0),
                D=if_else(state_po=="NE", 1, D),
                R=if_else(state_po=="NE", R-1, R),
                D=if_else(state_po=="ME", D-1, D),
                R=if_else(state_po=="ME", 1, R))
P_2020_State_Res

Full <- H_2020_State_Res %>% dplyr::rename(House_D=D, House_R=R, House_O=O) %>%
  left_join(S_2020_State_Res %>% dplyr::rename(Senate_D=D, Senate_R=R, Senate_O=O),
            by=join_by(state,state_po)) %>%
  dplyr::mutate(Senate_D=if_else(state_po=="DC", 0, Senate_D),
                Senate_R=if_else(state_po=="DC", 0, Senate_R),
                Senate_O=if_else(state_po=="DC", 0, Senate_O),
                S=if_else(state_po=="DC", 0, S)) %>%
  left_join(P_2020_State_Res %>% dplyr::select(state,state_po,EV,D,R),
            by=join_by(state,state_po)) %>%
  dplyr::rename(NAME=state,State=state_po,
                EC_D=D, EC_R=R) %>%
  dplyr::mutate(NAME=str_to_title(NAME))

write_csv(Full,
          file="res/StateRes_2020.csv")
  

## Pres Results By State For All Years:

### District-by-district results:
Districts=read_excel(path="data/Other/Pres_ByDistrict.xlsx",
                     sheet="Data")

### House Allocations:
load("data/Other/Apportion.Rda")
for (Year in c(1990,2000,2010,2020)) {
  if (Year==1990) {
    Res <- read_excel("data/Census/Census1990.xlsx", sheet="Data")
  } else {
    load(paste0("data/Census/Pop_Sex_",Year,".Rda"))
    if (Year %in% c(2000,2020)) {
      Res <- get(paste("Pop_Sex",Year,sep="_"))[["State"]] %>%
        dplyr::select(NAME,Total)
    } else {
      Res <- get(paste("Pop_Sex",Year,sep="_"))[["CD"]] %>%
        group_by(State) %>% dplyr::summarize(Total=sum(Total)) %>%
        ungroup() %>%
        dplyr::rename(NAME=State)
    }
  }
  
  Res <- Res %>% dplyr::filter(NAME != "Puerto Rico") %>%
    left_join(Abb, by=join_by(NAME)) %>%
    dplyr::rename(state=NAME,
                  state_po=State) %>%
    dplyr::select(state,state_po,Total)
  
  DC <- Res %>% dplyr::filter(state_po=="DC") %>%
    dplyr::mutate(Wyo=1,
                  H1911=1)
  ExDC <- Res %>% dplyr::filter(state_po != "DC")
  Min <- ExDC %>%
    slice_min(Total) %>% pull(Total)
  Num <- ceiling(sum(ExDC$Total)/Min)
  NumH <- ceiling(sum(ExDC$Total)/250000) #roughly 1911/1921 number
  Res2 <- ExDC %>%
    dplyr::mutate(Share=Total/(sum(ExDC$Total)/Num),
                  Min=floor(Share),
                  Diff=Share-Min,
                  ShareH=Total/(sum(ExDC$Total)/NumH),
                  MinH=floor(ShareH),
                  DiffH=ShareH-MinH) %>%
    arrange(desc(Diff)) %>%
    dplyr::mutate(rank=row_number(),
                  Addl=if_else(rank <= Num-sum(Min), 1, 0),
                  Wyo=Min+Addl) %>%
    arrange(desc(DiffH)) %>%
    dplyr::mutate(rankH=row_number(),
                  AddlH=if_else(rankH <= NumH-sum(MinH), 1, 0),
                  H1911=MinH+AddlH) %>%
    dplyr::select(state,state_po,Total,Wyo,H1911) %>%
    bind_rows(DC) %>%
    arrange(state,state_po) %>%
    dplyr::rename(Population=Total)
  assign(x=paste("Wyo", Year, sep="_"),
         value=Res2)
}

### Presidential Counterfactuals
for (Year in P_Years) {
  EVs_Yr <- H %>% filter(year==Year, state_po != "DC") %>% 
    group_by(state,state_po) %>%
    dplyr::summarize(N=length(unique(district))) %>%
    ungroup() %>%
    dplyr::mutate(EC=N+2) %>%
    dplyr::select(state,state_po,EC) %>%
    add_row(state="DISTRICT OF COLUMBIA",
            state_po="DC", 
            EC=3) %>%
    arrange(state,state_po,EC)
  
  Wyo_Yr <- get(paste("Wyo", 10*floor((Year-2)/10), sep="_"))
  
  if (Year < 2024) {
    Pres_F <- P %>% dplyr::filter(year==Year, state_po %in% c("CT","NY"),
                           !is.na(candidate),
                           !(candidate %in% c("UNDERVOTES","WRITEIN","OVERVOTES","",
                                              "BLANK VOTE","BLANK VOTE/SCATTERING",
                                              "NOT DESIGNATED"))) %>%
      group_by(year,state,state_po,state_fips,state_cen,state_ic,candidate) %>%
      dplyr::summarize(candidatevotes=sum(candidatevotes),
                       totalvotes=max(totalvotes),
                       party_D=sum(party_simplified=="DEMOCRAT"),
                       party_R=sum(party_simplified=="REPUBLICAN")) %>%
      ungroup() %>%
      arrange(year,state,state_po,state_fips,state_cen,state_ic,desc(candidatevotes)) %>%
      group_by(year,state,state_po,state_fips,state_cen,state_ic) %>%
      dplyr::mutate(rank=row_number(),
                    percentage=candidatevotes/totalvotes) %>%
      ungroup()
    
    Pres <- P %>% 
      dplyr::filter(year==Year,
                    !(state_po %in% c("CT","NY")),
                    !is.na(candidate), 
                    !(candidate %in% c("UNDERVOTES","WRITEIN","OVERVOTES","",
                                       "BLANK VOTE","BLANK VOTE/SCATTERING",
                                       "NOT DESIGNATED"))) %>%
      arrange(year,state,state_po,state_fips,state_cen,state_ic,desc(candidatevotes)) %>%
      group_by(year,state,state_po,state_fips,state_cen,state_ic) %>%
      dplyr::mutate(rank=row_number(),
                    percentage=candidatevotes/totalvotes,
                    party_D=if_else(grepl("DEMOCRAT",party_simplified,fixed=TRUE), 1, 0),
                    party_R=if_else(grepl("REPUBLICAN",party_simplified,fixed=TRUE), 1, 0)) %>%
      ungroup() %>%
      bind_rows(Pres_F) %>%
      dplyr::select(year,state,state_po,state_fips,state_cen,state_ic,candidate,party_D,party_R,candidatevotes,totalvotes,rank,percentage) %>%
      dplyr::filter(percentage > 0.005)%>%
      left_join(EVs_Yr, by=join_by(state,state_po)) %>%
      dplyr::mutate(minperc=0.5/EC,
                    abovemin=percentage > minperc)
  } else {
    Pres <- read_xlsx(path=paste0("data/Other/",Year,"_pres_data.xlsx"),
                      sheet="Data") %>%
      dplyr::rename(state=State) %>%
      left_join(Abb %>% dplyr::rename(state=NAME,
                                      state_po=State) %>%
                  dplyr::select(-Abbrev), 
                by=join_by(state)) %>%
      pivot_longer(cols=starts_with(c("D_","R_","O_")),
                   names_sep="_",
                   names_to=c("Party","Value")) %>%
      dplyr::filter(Value=="Votes",
                    !is.na(state_po)) %>%
      mutate(year=Year,
             party_D=if_else(Party=="D", 1, 0),
             party_R=if_else(Party=="R", 1, 0),
             candidate=if_else(Party=="D", "HARRIS, KAMALA",
                               if_else(Party=="R", "TRUMP, DONALD J.", "OTHERS")),
             percentage=value/Total_Votes) %>%
      dplyr::rename(totalvotes=Total_Votes,
                    candidatevotes=value) %>%
      dplyr::select(year,state,state_po,candidate,party_D,party_R,candidatevotes,totalvotes,percentage) %>%
      arrange(state,state_po,desc(candidatevotes)) %>%
      group_by(state,state_po) %>% 
      dplyr::mutate(rank=row_number()) %>%
      ungroup() %>%
      left_join(EC_State %>% rename(state_po=State, year=Year) %>%
                  filter(year==Year) %>% dplyr::select(state_po,EC),
                join_by(state_po)) %>%
      dplyr::mutate(minperc=0.5/EC,
                    abovemin=percentage > minperc)
  }
  
  
  Pres <- Pres %>%
    left_join(Pres %>% filter(party_D==1 | party_R==1) %>%
                group_by(state,state_po) %>%
                dplyr::summarize(twowayvotes=sum(candidatevotes)),
              by=join_by(state,state_po)) %>%
    left_join(Pres %>% filter(abovemin) %>%
                group_by(state,state_po) %>%
                dplyr::summarize(qualvotes=sum(candidatevotes)),
              by=join_by(state,state_po)) %>%
    left_join(Wyo_Yr %>% dplyr::select(-state),
              by=join_by(state_po)) %>%
    dplyr::mutate(twowayperc=if_else(party_D==1 | party_R==1, candidatevotes/twowayvotes, NA_real_),
                  qualperc=if_else(abovemin, candidatevotes/qualvotes, NA_real_))  %>%
    dplyr::mutate(AON=if_else(rank==1, EC, 0),
                  AON_435=if_else(rank==1, EC-2, 0),
                  AON_Pop=if_else(rank==1, Population, 0),
                  AON_Wyo=if_else(rank==1, Wyo+2, 0),
                  AON_WyoNo2=if_else(rank==1, Wyo, 0),
                  AON_H1911=if_else(rank==1, H1911+2, 0),
                  AON_H1911No2=if_else(rank==1, H1911, 0),
                  Prop=EC*percentage,
                  Prop_435=(EC-2)*percentage,
                  Prop_Wyo=(Wyo+2)*percentage,
                  Prop_WyoNo2=Wyo*percentage,
                  Prop_H1911=(H1911+2)*percentage,
                  Prop_H1911No2=H1911*percentage,
                  Prop_TwoWay=EC*twowayperc,
                  Prop_TwoWay_435=(EC-2)*twowayperc,
                  Prop_TwoWay_Wyo=(Wyo+2)*twowayperc,
                  Prop_TwoWay_WyoNo2=Wyo*twowayperc,
                  Prop_TwoWay_H1911=(H1911+2)*twowayperc,
                  Prop_TwoWay_H1911No2=H1911*twowayperc,
                  # Prop_Round=round(EC*qualperc, digits=0),
                  Prop_TwoWay_Round=round(EC*twowayperc, digits=0),
                  Prop_TwoWay_435_Round=round((EC-2)*twowayperc, digits=0),
                  Prop_TwoWay_Wyo_Round=round((Wyo+2)*twowayperc, digits=0),
                  Prop_TwoWay_WyoNo2_Round=round(Wyo*twowayperc, digits=0),
                  Prop_TwoWay_H1911_Round=round((H1911+2)*twowayperc, digits=0),
                  Prop_TwoWay_H1911No2_Round=round(H1911*twowayperc, digits=0))
  
  
  ## Largest Remainder System:
  PR <- Pres %>% 
    dplyr::select(state,state_po,candidate,candidatevotes,totalvotes,percentage,EC) %>% 
    dplyr::mutate(share=1/EC, min=floor(percentage/share),
                  rem=percentage-min*share)
  
  PR_rem <- PR %>% 
    left_join(PR %>%
                group_by(state,state_po) %>%
                dplyr::summarize(ECr=max(EC)-sum(min)) %>%
                ungroup(),
              by=join_by(state,state_po)) %>%
    arrange(state,state_po,desc(rem)) %>%
    group_by(state,state_po) %>%
    dplyr::mutate(rank=row_number(),
                  addl=if_else(rank <= ECr, 1, 0),
                  Prop_Round=min+addl) %>%
    ungroup() %>%
    arrange(state,state_po,desc(candidatevotes)) %>%
    dplyr::select(state,state_po,candidate,Prop_Round)
  
  Pres <- Pres %>% left_join(PR_rem %>%
                               dplyr::select(state,state_po,candidate,Prop_Round),
                             by=join_by(state,state_po,candidate)) %>%
    dplyr::mutate(StateWin=if_else(rank==1,1,0))
  if (sum(Pres %>% group_by(state,state_po) %>% dplyr::summarize(EC=max(EC), PR=sum(Prop_Round)) %>% ungroup() %>% mutate(Diff=EC-PR) %>% pull(Diff) != 0) > 0) {
    print(paste("Warning: Problem with Prop Rounding in",Year,sep=" "))
  }
  
  assign(x=paste("Pres", Year, sep="_"),
         value=Pres)
  
  Dists <- Districts %>% filter(year==Year) %>%
    pivot_longer(cols=c(D,R), values_to="Districts_Won") %>%
    dplyr::mutate(party_D=if_else(name=="D", 1, 0),
                  party_R=if_else(name=="R", 1, 0)) %>%
    dplyr::select(-c(year,name))
  TVotes <- apply(Pres %>% group_by(state,state_po) %>% 
                  dplyr::summarize(totalvotes=max(totalvotes),
                                   twowayvotes=max(twowayvotes)) %>%
                    ungroup() %>%
                    dplyr::select(totalvotes,twowayvotes),
                  2, sum)
  Summ <- Pres %>% mutate(AllVotes=TVotes["totalvotes"],
                               AllTwoWay=TVotes["twowayvotes"]) %>%
    group_by(candidate,party_D,party_R) %>%
    dplyr::summarize(States_Won=sum(StateWin, na.rm=TRUE),
                     across(starts_with(c("AON","Prop")),
                            ~sum(.x, na.rm=TRUE)),
                     NPV=sum(candidatevotes),
                     NPV_perc=sum(candidatevotes/AllVotes),
                     NPV_TwoWay=sum(candidatevotes*max(party_D+party_R))) %>%
    ungroup() %>%
    left_join(Dists, by=join_by(party_D,party_R)) %>%
    mutate(District=if_else(is.na(Districts_Won), 0, Districts_Won),
           DistrictP2=States_Won*2+District) %>%
    dplyr::select(-Districts_Won) %>%
    dplyr::mutate(across(starts_with(c("States","AON","Prop","NPV","District")),
                         ~.x/sum(.x),
                         .names="{.col}_perc")) %>%
    dplyr::filter(Prop >= 1 | NPV_perc > 0.01) %>%
    arrange(desc(NPV_perc))
  
  assign(x=paste("Summ", Year, sep="_"),
         value=Summ)
}

save(list=c(paste("Pres", P_Years, sep="_"),
            paste("Summ", P_Years, sep="_")),
     file="res/All_Pres_Summ_2000_2024.Rda")


## State Analysis:
Flip_Res <- NULL
for (Year in P_Years) {
  Winner <- if_else(get(paste("Summ", Year, sep="_")) %>% 
                      dplyr::filter(party_D==1) %>%
                      pull(AON) >= 270, "D", "R")
  NPV_Margin <- abs((get(paste("Summ", Year, sep="_")) %>% dplyr::filter(party_D==1) %>% pull(NPV)) - 
    (get(paste("Summ", Year, sep="_")) %>% dplyr::filter(party_R==1) %>% pull(NPV)))
  States_Won <- get(paste("Summ", Year, sep="_")) %>% dplyr::filter(AON >= 270) %>% pull(States_Won)
  States <- get(paste("Pres", Year, sep="_")) %>%
    dplyr::filter(party_D==1 | party_R == 1) %>%
    dplyr::mutate(party=if_else(party_D==1, "D", "R")) %>%
    dplyr::select(state,state_po,party,Population,totalvotes,EC,Wyo,H1911,candidatevotes) %>%
    pivot_wider(id_cols=c(state,state_po,Population,totalvotes,EC,Wyo,H1911),
                names_from=party,
                values_from=candidatevotes) %>%
    dplyr::mutate(Winner=Winner,
                  EC435=EC-2,
                  WyoNo2=Wyo,
                  Wyo=Wyo+2,
                  H1911No2=H1911,
                  H1911=H1911+2,
                  PopShare=Population/sum(Population),
                  VoteShare=totalvotes/sum(totalvotes),
                  Margin_W_votes=if_else(Winner=="D",D-R,R-D),
                  Margin_W_twoway=Margin_W_votes/(D+R)) %>%
    arrange(desc(Margin_W_votes)) %>%
    dplyr::mutate(State_W_rank=row_number(),
                  EC_W=cumsum(EC),
                  EC435_W=cumsum(EC435),
                  Wyo_W=cumsum(Wyo),
                  WyoNo2_W=cumsum(WyoNo2),
                  H1911_W=cumsum(H1911),
                  H1911No2_W=cumsum(H1911No2),
                  PopShare_W=cumsum(PopShare),
                  VoteShare_W=cumsum(VoteShare)) %>%
    dplyr::mutate(across(ends_with("_W"),
                         ~.x/max(.x),
                         .names="{.col}_perc"))
  FlipState <- apply(States %>% select(ends_with("_W_perc")),
        2, FUN=function(x) min(States$State_W_rank[x >= 0.5]))
  Votes_To_Flip <- sapply(names(FlipState),
                          FUN=function(x) sum(States %>% 
                                                dplyr::filter(State_W_rank >= FlipState[x] & State_W_rank <= States_Won) %>% 
                                                dplyr::pull(Margin_W_votes)))
  To_Flip <- tibble(Year=Year,
                    Alt=FALSE,
                    Metric=c("Margin",sub("_W_perc","",names(Votes_To_Flip))),
                    States=c(2*States_Won-51, States_Won - FlipState +1),
                    Votes=c(NPV_Margin, Votes_To_Flip))
  
  if (sum(FlipState > States_Won)>0) {
    ## Alt value for measures where the losing candidate would have won under that measure
    FlipState <- FlipState[FlipState>States_Won]
    Votes_To_Flip <- sapply(names(FlipState),
                            FUN=function(x) sum(States %>% 
                                                  dplyr::filter(State_W_rank <= FlipState[x] & State_W_rank > States_Won) %>% 
                                                  dplyr::pull(Margin_W_votes)))
    To_Flip_Alt <- tibble(Year=Year,
                          Alt=TRUE,
                      Metric=sub("_W_perc","",names(Votes_To_Flip)),
                      States=FlipState-States_Won,
                      Votes=-1*Votes_To_Flip)
    To_Flip <- To_Flip %>% left_join(To_Flip_Alt, by=join_by(Year,Metric)) %>%
      dplyr::mutate(Alt=if_else(is.na(Alt.y), Alt.x, Alt.y),
                    States=if_else(is.na(Alt.y), States.x, States.y),
                    Votes=if_else(is.na(Alt.y), Votes.x, Votes.y)) %>%
      dplyr::select(Year,Metric,Alt,States,Votes)
  }
  
  Flip_Res <- Flip_Res %>% bind_rows(To_Flip)
}
Flip_Tbl <- Flip_Res %>% pivot_wider(id_cols=Year,
                                     names_from=Metric,
                                     values_from=Votes) %>%
  dplyr::mutate(Metric="Votes") %>%
  bind_rows(Flip_Res %>% pivot_wider(id_cols=Year,
                                     names_from=Metric,
                                     values_from=States) %>%
              dplyr::mutate(Metric="States")) %>%
  bind_rows(Flip_Res %>% pivot_wider(id_cols=Year,
                                     names_from=Metric,
                                     values_from=Alt) %>%
              dplyr::mutate(Metric="Alt")) %>%
  dplyr::select(Year,Metric,Margin,everything()) %>%
  arrange(Year)

save(list=c("Flip_Tbl","Flip_Res"),
     file="res/All_Pres_Flips_2000_2024.Rda")
