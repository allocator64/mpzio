
(deftemplate answer
  (slot question-id  (default none))
  (slot value (default none)) 
)

;;Define the question template with validation constaints slots
(deftemplate question
  (slot question-id (default none))      
  (slot question-to-ask (default none))
  (slot has-pre-condition (type SYMBOL) (default no)) 
  (multislot choices (default yes no))
) 

;; ask-question with validation
(deftemplate breakfast
  (multislot if)
  (slot name)
  (slot recepie (default none))
)

(defrule remove-breakfast-good-condition
  ?r <- (breakfast  
          (if  ?first-ask-if is ?val $?rest-of-ifs-true)
        )
        (answer 
          (value ?val) 
          (question-id ?f&:(eq ?f ?first-ask-if))
        )
  =>
    (if (eq (nth$ 1 ?rest-of-ifs-true) and) 
      then (modify ?r (if (rest$ ?rest-of-ifs-true)))
    else 
      (modify ?r (if ?rest-of-ifs-true))
    )
)

(defrule print-breakfast
  ?r <- (breakfast
          (if $?a&:(=(length$ ?a) 0))
          (name ?name)
          (recepie ?recepie)
        )
  =>
    (printout t ?name crlf ?recepie crlf)
)

;ask   
(deffunction ask 
  (?question ?choices)
  (printout t ?question ?choices ":")
  (bind ?answer (read) )
  (while (not (member$ ?answer ?choices)) do
    (printout t "Invalid option! Please specify one of these options" ?choices ":" ) 
    (bind ?answer (read))
    (if (lexemep ?answer)
      then (bind ?answer (lowcase ?answer))
    )
  )
  (printout t crlf)        
  ?answer
 ) 


(deftemplate question-rule
  (multislot if (default none))
  (slot then-ask-question (default none))
)



;Implementing the question dependency rules
   
(defrule remove-condition-from-question-rules
  ?r <- (question-rule  
          (if  ?first-ask-if is ?val $?rest-of-ifs-true)
        )
        (answer 
          (value ?val) 
          (question-id ?f&:(eq ?f ?first-ask-if))
        )
  =>
    (if (eq (nth$ 1 ?rest-of-ifs-true) and) 
      then (modify ?r (if (rest$ ?rest-of-ifs-true)))
    else 
      (modify ?r (if ?rest-of-ifs-true))
    )
)

(defrule set-pre-condition-when-no-antecedents
  ?r <- (question-rule (if $?a&:(=(length$ ?a) 0))  (then-ask-question ?f))
  ?q <- (question  (question-id ?f) (has-pre-condition yes) )
    (not (answer (question-id ?f)))
  =>
    (modify ?q (has-pre-condition no))
)


; ask-question
(defrule ask-question
  ?q <- (question (question-to-ask ?question)
          (question-id ?question-id)
          (choices $?choices)
          (has-pre-condition no)
        )
    (not (answer (question-id ?question-id)))
  =>
    (assert (answer (question-id ?question-id)
                    (value (ask ?question ?choices)))
    )
)
        
;============ CALORIES ==============
(assert  
  (question 
    (question-id calories) 
    (question-to-ask "Насколько тяжелый завтрак вы хотите?")
    (choices high medium low)
    (has-pre-condition yes)
  )
)

(assert  
  (question-rule 
    (if sweet is yes) 
    (then-ask-question calories)
  )
)
(assert  
  (question-rule 
    (if sweet is no and vegetarian is yes and vegan is no) 
    (then-ask-question calories)
  )
)

;============= VEGETARIAN =============
(assert  
  (question 
    (question-id vegetarian) 
    (question-to-ask "Вы хотите завтрак без мяса?")
    (has-pre-condition yes)
  )
)

(assert  
  (question-rule 
    (if sweet is yes and calories is high)
    (then-ask-question vegetarian)
  )
)

(assert  
  (question-rule 
    (if sweet is no) 
    (then-ask-question vegetarian)
  )
)

;============= PREPARE BEFORE =============
(assert  
  (question 
    (question-id prepare-before) 
    (question-to-ask "Вы готовы заранее приготовить завтрак?")
    (has-pre-condition yes)
  )
)

(assert  
  (question-rule 
    (if sweet is yes and calories is high and vegetarian is no) 
    (then-ask-question prepare-before)
  )
)

;============ PREPARE TIME ==============
(assert  
  (question 
    (question-id prepare-time) 
    (question-to-ask "Сколько времени вы готовы потратить на приготовление?")
    (choices <15 15-30 >30)
    (has-pre-condition yes)
  )
)

(assert  
  (question-rule 
    (if sweet is yes and calories is high and vegetarian is no and prepare-before is no) 
    (then-ask-question prepare-time)
  )
)
(assert  
  (question-rule 
    (if sweet is yes and calories is medium)
    (then-ask-question prepare-time)
  )
)
(assert  
  (question-rule 
    (if sweet is no and vegetarian is no)
    (then-ask-question prepare-time)
  )
)
(assert  
  (question-rule 
    (if sweet is no and vegetarian is yes and vegan is no and calories is medium) 
    (then-ask-question prepare-time)
  )
)

;============= SIMPLE PRODUCE =============
(assert  
  (question 
    (question-id simple) 
    (question-to-ask "Вы хотите простой завтрак?")
    (has-pre-condition yes)
  )
)

(assert  
  (question-rule 
    (if sweet is yes and calories is high and vegetarian is no and prepare-before is no and prepare-time is 15-30) 
    (then-ask-question simple)
  )
)
(assert  
  (question-rule 
    (if sweet is yes and calories is low ) 
    (then-ask-question simple)
  )
)
(assert  
  (question-rule 
    (if sweet is no and vegetarian is yes and vegan is no and calories is low and hot is yes and fast is no) 
    (then-ask-question simple)
  )
)
(assert  
  (question-rule 
    (if sweet is no and vegetarian is yes and vegan is no and calories is high and fast is yes) 
    (then-ask-question simple)
  )
)

;============= DRY =============
(assert  
  (question 
    (question-id dry) 
    (question-to-ask "Вы не против сухого завтрака?")
    (has-pre-condition yes)
  )
)

(assert  
  (question-rule 
    (if sweet is yes and calories is high and vegetarian is no and prepare-before is no and prepare-time is 15-30 and simple is no) 
    (then-ask-question dry)
  )
)
(assert  
  (question-rule 
    (if sweet is no and vegetarian is yes and vegan is no and calories is high and fast is yes and simple is yes) 
    (then-ask-question dry)
  )
)
;============= VEGAN =============
(assert  
  (question 
    (question-id vegan) 
    (question-to-ask "Вы веган?")
    (has-pre-condition yes)
  )
)

(assert  
  (question-rule 
    (if sweet is no and  vegetarian is yes) 
    (then-ask-question vegan)
  )
)

;============= HOT =============
(assert  
  (question 
    (question-id hot) 
    (question-to-ask "Вы хотите горячий завтрак?")
    (has-pre-condition yes)
  )
)

(assert  
  (question-rule 
    (if sweet is no and vegetarian is yes and vegan is no and calories is low) 
    (then-ask-question hot)
  )
)

;============= FAST =============
(assert  
  (question 
    (question-id fast) 
    (question-to-ask "Вы планируете готовить завтрак в спешке?")
    (has-pre-condition yes)
  )
)

(assert  
  (question-rule 
    (if sweet is no and vegetarian is yes and vegan is no and calories is low and hot is yes) 
    (then-ask-question fast)
  )
)
(assert  
  (question-rule 
    (if sweet is no and vegetarian is yes and vegan is no and calories is high) 
    (then-ask-question fast)
  )
)

;============= UNUSUAL =============
(assert  
  (question 
    (question-id unusual) 
    (question-to-ask "Насколько необычный завтрак вы хотите?")
    (choices high medium low)
    (has-pre-condition yes)
  )
)

(assert  
  (question-rule 
    (if sweet is no and vegetarian is yes and vegan is no and calories is medium and prepare-time is <15) 
    (then-ask-question unusual)
  )
)
(assert  
  (question-rule 
    (if sweet is no and vegetarian is yes and vegan is no and calories is medium and prepare-time is >30) 
    (then-ask-question unusual)
  )
)

;============= KITCHEN =============
(assert  
  (question 
    (question-id kitchen) 
    (question-to-ask "Какую кухню вы предпочитаете?")
    (choices russian mexican)
    (has-pre-condition yes)
  )
)

(assert  
  (question-rule 
    (if sweet is no and vegetarian is yes and vegan is no and calories is high and fast is yes and simple is no) 
    (then-ask-question kitchen)
  )
)
;============= SWEET =============
(assert  
  (question 
    (question-id sweet) 
    (question-to-ask "Вы хотите сладкий завтрак?") 
  )
)
;============= BREAKFASTS ============
(assert
  (breakfast
    (if sweet is yes and calories is high and vegetarian is yes)
    (name "Завтрак детства")
    (recepie "Морковь — 1 штука; Яблоко — 1 штука; Апельсины — 1 штука; Изюм — 50 г; Мед — 2 чайные ложки; Орехи — 50 г; Молотая корица — по вкусу")
  )
)
(assert
  (breakfast
    (if sweet is yes and calories is medium and prepare-time is <15)
    (name "Сладкая манная каша")
    (recepie "Молоко — 1 стакан; Манная крупа — 4 чайные ложки; Соль — щепотка; Сахар — 2 чайные ложки")
  )
)
(assert
  (breakfast
    (if sweet is yes and calories is medium and prepare-time is 15-30)
    (name "Американские панкейки с корицей")
    (recepie "Мука пшеничная — 240 г; Молоко — 220 мл; Сахар — 2 столовые ложки; Яйцо куриное — 4 штуки; Соль — щепотка; Сода гашеная — 0,2 чайной ложки; Молотая корица — по вкусу")
  )
)
(assert
  (breakfast
    (if sweet is yes and calories is medium and prepare-time is >30)
    (name "Круассаны с шоколадом")
    (recepie "Слоеное тесто — 2 штуки; Тертый шоколад — 240 г; Яйцо куриное —  1 штука; Сахарная пудра — 50 г")
  )
)
(assert
  (breakfast
    (if sweet is yes and calories is low and simple is yes)
    (name "Сырники с изюмом (без муки)")
    (recepie "Творог 9% — 240 г; Сахар — 1 столовая ложка; Яйцо куриное — 2 штуки; Соль — щепотка; Манная крупа — 2 столовые ложки; Разрыхлитель — 0,2 чайной ложки; Овсяные хлопья — 4 столовые ложки; Изюм — ½ стакана")
  )
)
(assert
  (breakfast
    (if sweet is yes and calories is low and simple is no)
    (name "Батончики из овсяной гранолы с абрикосом и льном")
    (recepie "Мед — 2 столовые ложки; Овсяные хлопья — 100 г; Фруктоза — 2 столовые ложки; Семена льна — 1,5 столовые ложки; Курага — 4 штуки; Отруби — 1 столовая ложка; Пюре яблочное — 100 г; Пекарский порошок — ½ чайной ложки; Соль — щепотка")
  )
)
(assert
  (breakfast
    (if sweet is yes and calories is high and vegetarian is no and prepare-before is yes)
    (name "Манник с какао")
    (recepie "Кефир — ½ л; Манная крупа — 2 стакана; Топленое сливочное масло — 100 г; Сахар — 1 стакан; Сода — ½ чайной ложки; Какао — 2 столовые ложки")
  )
)
(assert
  (breakfast
    (if sweet is yes and calories is high and vegetarian is no and prepare-before is no and prepare-time is <15)
    (name "Овсяная каша с яблоком и корицей")
    (recepie "Сливочное масло — 1 столовая ложка; Овсяные хлопья — 150 г; Яблоко — 60 г; Молотая корица — 1 чайная ложка; Соль — щепотка; Сахар — 1 столовая ложка; Мед — 1,5 столовые ложки; Вода — 220 мл")
  )
)
(assert
  (breakfast
    (if sweet is yes and calories is high and vegetarian is no and prepare-before is no and prepare-time is >30)
    (name "Запеканка творожная с манной крупой")
    (recepie "Творог — 600 г; Яйцо куриное — 4 штуки; Лимоны — ½ штуки; Манная крупа — 10 столовых ложек; Сливочное масло — 150; Сахар — ½ стакана; Сахар ванильный — 1 чайная ложка; Соль — на кончике ножа; Сода — ½ чайной ложки")
  )
)
(assert
  (breakfast
    (if sweet is yes and calories is high and vegetarian is no and prepare-before is no and prepare-time is 15-30 and simple is yes)
    (name "Сырники из творога")
    (recepie "Творог — 350 г; Яйцо куриное — 2 шт; мука — 6 ст. ложек; сахар — 2 ст. ложки; подсолнечное масло — 2 ст. ложки")
  )
)
(assert
  (breakfast
    (if sweet is yes and calories is high and vegetarian is no and prepare-before is no and prepare-time is 15-30 and simple is no and dry is yes)
    (name "Тост с бананово-карамельным соусом")
    (recepie "Тосты — 2 штуки; Яйцо куриное — 2 штуки; Молоко — 3 столовые ложки; Сахар — ½ чайной ложки; Сливочное масло — 3 столовые ложки; Сахар коричневый — 4 столовые ложки; Вода — 4 столовые ложки; Бананы — 1 штука")
  )
)
(assert
  (breakfast
    (if sweet is yes and calories is high and vegetarian is no and prepare-before is no and prepare-time is 15-30 and simple is no and dry is no)
    (name "Рисовая каша с апельсином")
    (recepie "Молоко 3,5%-ное — 340 мл; Сливки 35%-ные — 160 мл; Рис — 60 г; Сливочное масло — 40 г; Молоко сгущенное — 120 г; Ванильный стручок — 1 штука; Апельсины — 1 штука")
  )
)
(assert
  (breakfast
    (if sweet is no and vegetarian is no and prepare-time is <15)
    (name "Английский завтрак")
    (recepie "Яйцо куриное — 2 штуки; Сосиски свиные — 2 штуки; Кровяная колбаса — 50 г; Помидоры — 1 штука; Шампиньоны — 3 штуки; Фасоль в томатном соусе — 50 г; Бекон — 50 г; Молоко — 1 столовая ложка; Сливочное масло — 20 г; Хлеб для тостов — 2 куска; Соль — по вкусу; Растительное масло — 50 мл; Перец черный молотый — по вкусу; Соус коричневый HP — 1 столовая ложка")
  )
)
(assert
  (breakfast
    (if sweet is no and vegetarian is no and prepare-time is 15-30)
    (name "Омлет-суфле с ветчиной и сыром")
    (recepie "Яйцо куриное — 6 штук; Лук-порей — 1 штука; Варено-копченая ветчина — 100 г; Сливочное масло — 50 г; Сметана — 200 м; Сливки 25%-ные — 100 мл; Мускатный орех — по вкусу; Сыр — 50 г; Петрушка — по вкусу; Соль — по вкусу; Перец черный молотый — по вкус")
  )
)
(assert
  (breakfast
    (if sweet is no and vegetarian is no and prepare-time is >30)
    (name "Лоранский пирог с курицей и грибами")
    (recepie "Сливочное масло — 50 г; Вода — 3 г; Соль — ½ чайной ложки; Пшеничная мука — 200 г; Шампиньоны — 300 г; Куриное филе — 300 г; Лук — 1 штука; Специи — по вкусу; Сливки 20%-ные — 170 мл; Яйцо куриное — 2 штуки; Сыр — 150 г")
  )
)
(assert
  (breakfast
    (if sweet is no and vegetarian is yes and vegan is yes)
    (name "Хрустящий харшбраун")
    (recepie "Оливковое масло — 3 столовые ложки; Картофель — 500 г; Соль — по вкусу; Перец черный молотый — по вкусу")
  )
)
(assert
  (breakfast
    (if sweet is no and vegetarian is yes and vegan is no and calories is low and hot is no)
    (name "Низкокалорийный завтрак")
    (recepie "Салат зеленый — 0,1 пучка; Яйцо куриное — 1 штука; Помидоры — ½ штуки; Черемша — 3 стебля; Сыр российский — 50 г; Соль — по вкусу")
  )
)
(assert
  (breakfast
    (if sweet is no and vegetarian is yes and vegan is no and calories is low and hot is yes and fast is yes)
    (name "Яичница в хлебе")
    (recepie "Яйцо куриное — 1 штука; Белый хлеб — 1 кусок")
  )
)
(assert
  (breakfast
    (if sweet is no and vegetarian is yes and vegan is no and calories is low and hot is yes and fast is no and simple is yes)
    (name "Классические белорусские драники")
    (recepie "Картофель — 5 штук; Лук — ½ штуки; Яйцо куриное — 1 штука; Пшеничная мука — 2 столовые ложки; Перец черный молотый — по вкусу; Соль — по вкусу; Растительное масло — по вкусу")
  )
)
(assert
  (breakfast
    (if sweet is no and vegetarian is yes and vegan is no and calories is low and hot is yes and fast is no and simple is no)
    (name "Оладьи из брокколи с сыром")
    (recepie "Капуста брокколи — 250 г; Пшеничная мука — 70 г; Яйцо куриное — 1 штука; Соль — по вкусу; Тертый сыр пармезан — 30 г; Смесь перцев — по вкусу")
  )
)
(assert
  (breakfast
    (if sweet is no and vegetarian is yes and vegan is no and calories is medium and prepare-time is >30)
    (name "Тортилья")
    (recepie "Картофель — 280 г; Репчатый лук — 120 г; Яйцо куриное — 5 штук; Помидоры — 1 штука; Зеленый горошек — 1 стакан; Оливковое масло — 3 столовые ложки; Соль — по вкусу")
  )
)
(assert
  (breakfast
    (if sweet is no and vegetarian is yes and vegan is no and calories is medium and prepare-time is <15 and unusual is low)
    (name "Яичница в помидоре")
    (recepie "Помидоры — 4 штуки; Яйцо куриное — 3 штуки; Сыр — 100 г; Зелень — 2 столовые ложки; Перец черный молотый — по вкусу; Соль — по вкусу")
  )
)
(assert
  (breakfast
    (if sweet is no and vegetarian is yes and vegan is no and calories is medium and prepare-time is <15 and unusual is medium)
    (name "Жареный сыр в лаваше")
    (recepie "Армянский лаваш — 1 штука; Сыр — по вкусу; Яйцо куриное — по вкусу")
  )
)
(assert
  (breakfast
    (if sweet is no and vegetarian is yes and vegan is no and calories is medium and prepare-time is <15 and unusual is high)
    (name "Шакшука")
    (recepie "Репчатый лук — 1 головка; Желтый сладкий перец — ½ штуки; Перец чили — 1 штука; Зеленый сладкий перец — ½ штуки; Чеснок — 2 зубчика; Помидоры в собственном соку — 300 г; Кумин (зира) — ½ чайной ложки; Куркума — ½ чайной ложки; Оливковое масло — 3 столовые ложки; Яйцо куриное — 2 штуки; Соль — по вкусу; Зелень — по вкусу")
  )
)
(assert
  (breakfast
    (if sweet is no and vegetarian is yes and vegan is no and calories is medium and prepare-time is 15-30 and unusual is low)
    (name "Пшеная каша на молоке")
    (recepie "Пшено — 1 стакан; Вода — 2 стакана; Молоко — 2 стакана; Соль — ¼ чайной ложки; Сахар — по вкусу; Сливочное масло — по вкусу")
  )
)
(assert
  (breakfast
    (if sweet is no and vegetarian is yes and vegan is no and calories is medium and prepare-time is 15-30 and unusual is medium)
    (name "Сметанные лепешки с сыром и зеленью")
    (recepie "Зелень — 100 г; Тертый сыр — 200 г; Яйцо куриное — 2 штуки; Сметана — 200 г; Пшеничная мука — 2 столовые ложки; Растительное масло — 2 столовые ложки")
  )
)
(assert
  (breakfast
    (if sweet is no and vegetarian is yes and vegan is no and calories is medium and prepare-time is 15-30 and unusual is high)
    (name "Кюкю")
    (recepie "Шпинат — 90 г; Петрушка — 30 г; Свежая кинза — 30 г; Зеленый лук — 30 г; Свежая мята — 10 г; Яйцо куриное — 8 штук; Натуральный йогурт — 250 г; Тимьян — щепотка; Помидоры черри — 300 г; Сливочное масло — 30 г; Чеснок — 2 зубчика")
  )
)
(assert
  (breakfast
    (if sweet is no and vegetarian is yes and vegan is no and calories is high and fast is no)
    (name "Блины на молоке")
    (recepie "Молоко — ½ л; Пшеничная мука — 200 г; Соль — 1 чайная ложка; Подсолнечное масло — 2 столовые ложки; Сахар — 1 столовая ложка; Яйцо куриное — 3 штуки; Сода — по вкусу")
  )
)
(assert
  (breakfast
    (if sweet is no and vegetarian is yes and vegan is no and calories is high and fast is yes and simple is yes and dry is yes)
    (name "Французские гренки")
    (recepie "Батон — 3 куска; Молоко — 2 столовые ложки; Яйцо куриное — 1 штука; Соль — по вкусу; Растительное масло — 1 столовая ложка")
  )
)
(assert
  (breakfast
    (if sweet is no and vegetarian is yes and vegan is no and calories is high and fast is yes and simple is yes and dry is no)
    (name "Омлет с сыром")
    (recepie "Яйцо куриное — 3 штуки; Оливковое масло — 1 столовая ложка; Сливочное масло — 15 г; Сыр — 50 г; Соль — по вкусу; Перец черный молотый — по вкусу")
  )
)
(assert
  (breakfast
    (if sweet is no and vegetarian is yes and vegan is no and calories is high and fast is yes and simple is no and kitchen is russian)
    (name "Ленивые вареники")
    (recepie "Яйцо куриное — 1 штука; Мягкий творог — 200 г; Мука пшеничная — 30 г")
  )
)
(assert
  (breakfast
    (if sweet is no and vegetarian is yes and vegan is no and calories is high and fast is yes and simple is no and kitchen is mexican)
    (name "Уэвос-ранчос")
    (recepie "Лимоны — 1 штука; Оливковое масло — 50 г; Сахар — щепотка; Петрушка — 10 г; Семена кориандра — щепотка; Чеснок — 3 зубчика; Красный лук — 50 г; Чоризо — 100 г; Перец маринованный — 3 штуки; Помидоры — 2 штуки; Яйцо куриное — 6 штук")
  )
)

(clear-window)