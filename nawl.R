## -*- coding: utf-8 -*-

## Procedura: każda osoba ma oceniać po n (np 30) przymiotników neg,
## neu i poz. Rodzaj skali (emocje, wyobrażalność, arousal) i zgodność
## końcówki z płcią własną to manipulacja między-osobowa. Brak limitu
## czasu na ocenę słowa.
##
## Przebieg próby: 1s lub 2s prezentacja słowa nieco powyżej na
## środku, a potem dodanie skali z opisem i etykietami.
if(interactive())source('~/cs/code/r/tasks/task/task.R')
TASK.NAME <<- 'nawl'

NOF.ITEMS = 30
FIXATION.TIME = 1000
POST.FIXATION.TIME = 1000
PRESENTATION.TIME = 1000

## Wczytujemy słowa z bazy i przygotowujemy zestaw bodźców
words = readRDS('nawl.rds')
words = words[words$Gram == 3,]
words$val = (words$val_M_all - mean(words$val_M_all)) / sd(words$val_M_all)
neg = words$NAWL_word[words$val < -1.4][1:NOF.ITEMS]
neu = words$NAWL_word[abs(words$val) < .1][1:NOF.ITEMS]
pos = words$NAWL_word[words$val > 1.4][1:NOF.ITEMS]
rm(words)

WINDOW$set.visible(T)
WINDOW$set.mouse.cursor.visible(T)

FX = fixation(WINDOW, size = .02)

scales = list(emotion = c('Znak emocji', 'Bardzo negatywne', 'Negatywne', 'Neutralne', 'Pozytywne', 'Bardzo pozytywne'),
              imagine = c('Wyobrażalność', 'Bardzo trudno', 'Trudno', 'Przeciętnie', 'Łatwo', 'Bardzo łatwo'),
              arousal = c('Pobudzenie', '1', '2', '3', '4', '5', '6', '7'))

trial.code = function(trial, word = 'test', samegender = 'same', scale = 'emotion'){
    ## Kod specyficzny dla zadania
    ## ...
    ## Szablon
    if(trial == 1){
        state = 'press-space'
    }else{ state = 'show-fixation' }
    ## Ewentualna zmiana genderu słowa
    if(((samegender == 'same') && (USER.DATA$gender == 'K')) ||
       ((samegender != 'same') && (USER.DATA$gender == 'M'))){
        word = str_replace_all(word, 'y$', 'a')
        word = str_replace_all(word, 'i$', 'a')
        if(word == 'mysa')word = 'mysia'
    }
    start = CLOCK$time
    while(WINDOW$is.open()){
        process.inputs()
        if(KEY.PRESSED[Key.Escape + 1] > start)return(NULL)
        ## Kod specyficzny dla zadania
        switch(state, 'press-space' = {
            TXT$set.string("Proszę nacisnąć spację aby rozpocząć")
            center.win(TXT)
            WINDOW$clear(c(0, 0, 0))
            WINDOW$draw(TXT)
            WINDOW$display()
            if(KEY.RELEASED[Key.Space + 1] > start){
                state = 'show-fixation'
            }
        }, 'show-fixation' = {
            WINDOW$clear(c(0, 0, 0))
            lapply(FX, WINDOW$draw)
            WINDOW$display()
            state = 'clear-fixation'
            fixation.start = CLOCK$time
        }, 'clear-fixation' = {
            if((CLOCK$time - fixation.start) > FIXATION.TIME){
                WINDOW$clear(c(0, 0, 0))
                WINDOW$display()
                state = 'post-fixation'
                fixation.cleared = CLOCK$time
            }
        }, 'post-fixation' = {
            if((CLOCK$time - fixation.cleared) > POST.FIXATION.TIME){
                state = 'show-stim'
            }
        }, 'show-stim' = {
            WINDOW$clear(c(0, 0, 0))
            TXT$set.string(word)
            center.win(TXT)
            WINDOW$draw(TXT)
            WINDOW$display()
            stim.onset = CLOCK$time
            state = 'stim-present'
        }, 'stim-present' = {
            if((CLOCK$time - stim.onset) > PRESENTATION.TIME){
                value = -1
                WINDOW$clear(c(0, 0, 0))
                WINDOW$display()
                scale.onset = CLOCK$time
                state = 'rating'
            }
        }, 'rating' = {
            WINDOW$clear(c(0, 0, 0))
            ## Rysujemy słowo
            TXT$set.string(word)
            center.win(TXT)## $move(c(0, WINDOW$get.size()[2] * -.2))
            WINDOW$draw(TXT)
            ## Pokazujemy skalę tylko dopóki nie zaznaczy odpowiedzi
            if(BUTTON.PRESSED[1] <= scale.onset){
                ## Pytanie dla skali (np. jak łatwo jest sobie wyobrazić...)
                TXT$set.string(scales[[as.character(scale)]][1])
                center.win(TXT)$move(c(0, WINDOW$get.size()[2] * .1))
                WINDOW$draw(TXT)
                value = draw.scale(scales[[as.character(scale)]][-1], position = .7)[1]
            }else{
                state = 'done'
            }
            WINDOW$display()
        }, 'done' = {
            WINDOW$clear(c(0, 0, 0))
            WINDOW$display()
            res = list(rating = value)
            return(res)
        })
    }
}

gui.show.instruction("W czasie eksperymentu obowiązuje cisza. Wyłącz telefon komórkowy. W razie jakichkolwiek wątpliwości nie wołaj osoby prowadzącej, tylko podnieś do góry rękę - osoba prowadząca podejdzie w dogodnym momencie i postara się udzielić wszelkich wyjaśnień. 
Badanie jest anonimowe. Za chwilę zostaniesz poproszona/y o podanie danych: wieku, płci oraz pseudonimu. Pseudonim składa się z inicjałów (małymi literami) oraz czterech cyfr: dnia 
i miesiąca urodzenia (np.  ms0706). 
")
gui.user.data()

cnd = db.random.condition(c('same-emotion', 'diff-emotion', 'same-imagine', 'diff-imagine'))
scale = str_split(cnd, '-')[[1]][2]

gui.show.instruction(sprintf("
Badanie dotyczy reakcji na czytane słowa. Przez kilka minut na ekranie komputera będą prezentowane różne słowa.

Twoim zadaniem będzie ocenić każde z nich na skali prezentowanej poniżej słowa.

Aby ocenić słowo należy wskazać odpowiednie miejsce na skali i wcisnąć lewy przycisk myszki.

%s
", list(emotion = 'Ta skala dotyczy znaku emocji, który określa, czy to słowo budzi w Tobie negatywne, pozytywne, czy neutralne emocje.',
        imagine = 'Ta skala dotyczy wyobrażalności, która określa, jak łatwo możesz sobie wyobrazić to, co opisuje dane słowo.')[[scale]]))

run.trials(trial.code, expand.grid(scale = scale,
                                   samegender = str_split(cnd, '-')[[1]][1],
                                   word = c(sample(neg), sample(neu), sample(pos))),
           record.session = T,
           condition = cnd)

gui.show.instruction("To koniec procedury. O dalszym postępowaniu poinformuje osoba prowadząca badanie.

Dziękujemy za udział.")

if(!interactive())quit("no")
