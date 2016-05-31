; открыть CLIPS IDE
; нажать cmd+shift+L
; открыть там этот файл
; набрать (run)
; Или
; (echo -e '(batch boryadima.bat)\n(run)'; cat - ; echo '(exit)') | /Applications/CLIPS\ Console

(deftemplate question
	(slot id (default none))
	(slot text (default none))
	(slot is-hidden (type SYMBOL) (default yes))
	(multislot choices (default yes no))
)

(deftemplate proglang
	(multislot if)
	(slot name)
	(slot description (default ""))
)

(deftemplate question-rule
	(multislot if (default none))
	(slot then (default none))
)

(deftemplate answer
	(slot id (default none))
	(slot value (default none))
)

(defrule update-proglangs
	?r <-
		(proglang
			(if ?first-ask-if is ?val $?rest-of-ifs-true)
		)
		(answer
			(value ?val)
			(id ?f&:(eq ?f ?first-ask-if))
		)
	=>
		(if (eq (nth$ 1 ?rest-of-ifs-true) and)
			then (modify ?r (if (rest$ ?rest-of-ifs-true)))
		else
			(modify ?r (if ?rest-of-ifs-true))
		)
)

(defrule print-proglang
	?r <-
		(proglang
			(if $?a&:(=(length$ ?a) 0))
			(name ?name)
			(description ?description)
		)
	=>
		(printout t ?name crlf ?description crlf)
)

(defrule update-question-rules
	?r <-
		(question-rule
			(if ?first-ask-if is ?val $?rest-of-ifs-true)
		)
		(answer
			(value ?val)
			(id ?f&:(eq ?f ?first-ask-if))
		)
	=>
		(if (eq (nth$ 1 ?rest-of-ifs-true) and)
			then (modify ?r (if (rest$ ?rest-of-ifs-true)))
		else
			(modify ?r (if ?rest-of-ifs-true))
		)
)

(defrule update-questions
	?r <-
		(question-rule
			(if $?a&:(=(length$ ?a) 0))
			(then ?f)
		)
	?q <-
		(question
			(id ?f)
			(is-hidden yes)
		)
		(not (answer (id ?f)))
	=>
		(modify
			?q
			(is-hidden no)
		)
)

(deffunction do-ask
	(?question ?choices)
	(printout t ?question " " ?choices " : ")
	(bind ?answer (read) )
	(while (not (member$ ?answer ?choices)) do
		(printout t "Нет такого варианта, пожалуйста, выберите из списка " ?choices " : " )
		(bind ?answer (read))
		(if (lexemep ?answer)
			then (bind ?answer (lowcase ?answer))
		)
	)
	(printout t crlf)
	?answer
)

(defrule ask-question
	?q <-
		(question
			(text ?question)
			(id ?id)
			(choices $?choices)
			(is-hidden no)
		)
		(not (answer (id ?id)))
	=>
		(assert (answer (id ?id)
			(value (do-ask ?question ?choices)))
		)
)


(assert
	(question
		(id purpose)
		(text "Для какой цели выбирается язык программирования?")
		(choices web mobile desktop self-development)
		(is-hidden no)
	)
)

(assert
	(question
		(id desktop-type)
		(text "Что вы хотите разработать?")
		(choices game graphics driver science)
	)
)

(assert
	(question-rule
		(if purpose is desktop)
		(then desktop-type)
	)
)

(assert
	(question
		(id mobile-platform)
		(text "Выберите мобильную операционную систему")
		(choices android ios windows)
	)
)

(assert
	(question-rule
		(if purpose is mobile)
		(then mobile-platform)
	)
)

(assert
	(question
		(id desktop-platform)
		(text "Выберите операционную систему")
		(choices windows linux osx)
	)
)

(assert
	(question-rule
		(if purpose is desktop and desktop-type is game)
		(then desktop-platform)
	)
)

(assert
	(question-rule
		(if purpose is desktop and desktop-type is graphics and crossplatform is no)
		(then desktop-platform)
	)
)

(assert
	(question
		(id show-off)
		(text "Хотите ли выделяться среди других программистов в этой области?")
	)
)

(assert
	(question-rule
		(if purpose is mobile and mobile-platform is ios)
		(then show-off)
	)
)

(assert
	(question-rule
		(if purpose is self-development and difficulty is hard)
		(then show-off)
	)
)

(assert
	(question
		(id web-specialization)
		(text "Какую часть веб приложения создаете?")
		(choices frontend backend)
	)
)

(assert
	(question-rule
		(if purpose is web)
		(then web-specialization)
	)
)

(assert
	(question
		(id web-oldscool)
		(text "Вы Столяров?")
	)
)

(assert
	(question-rule
		(if purpose is web and web-specialization is frontend)
		(then web-oldscool)
	)
)

(assert
	(question
		(id crossplatform)
		(text "Вы хотите создать кросплатформенное решение?")
	)
)

(assert
	(question-rule
		(if purpose is graphics)
		(then crossplatform)
	)
)

(assert
	(question
		(id highload)
		(text "Будете работать с высокой нагрузкой?")
	)
)

(assert
	(question-rule
		(if purpose is web and web-specialization is backend)
		(then highload)
	)
)

(assert
	(question
		(id is-web)
		(text "Вы хотите программировать для WEB?")
	)
)

(assert
	(question-rule
		(if purpose is self-development and difficulty is medium)
		(then is-web)
	)
)

(assert
	(question
		(id is-functional)
		(text "Вы хотите функциональный язык?")
	)
)

(assert
	(question-rule
		(if purpose is self-development and difficulty is hard)
		(then is-functional)
	)
)

(assert
	(question
		(id difficulty)
		(text "Какой вы предпочитаете порог входения?")
		(choices easy medium hard)
	)
)

(assert
	(question-rule
		(if purpose is graphics and crossplatform is yes)
		(then difficulty)
	)
)

(assert
	(question-rule
		(if purpose is graphics and crossplatform is no and desktop-platform is osx)
		(then difficulty)
	)
)

(assert
	(question-rule
		(if purpose is graphics and crossplatform is no and desktop-platform is linux)
		(then difficulty)
	)
)

(assert
	(question-rule
		(if purpose is web and web-specialization is backend and highload is no)
		(then difficulty)
	)
)

(assert
	(question-rule
		(if purpose is desktop and desktop-type is science and science-type is computation)
		(then difficulty)
	)
)

(assert
	(question-rule
		(if purpose is self-development)
		(then difficulty)
	)
)

(assert
	(question
		(id science-type)
		(text "Какого вида задачи будут решаться?")
		(choices computation AI)
	)
)

(assert
	(question-rule
		(if purpose is desktop and desktop-type is science)
		(then science-type)
	)
)

(assert
	(question
		(id ai-type)
		(text "Какой областью ИИ вы занимаетесь?")
		(choices ontology expert-system prototyping)
	)
)

(assert
	(question-rule
		(if purpose is desktop and desktop-type is science and science-type is AI)
		(then ai-type)
	)
)


(assert
	(proglang
		(if purpose is mobile and mobile-platform is android)
		(name "Java")
		(description "Для Android можно программировать только на Java.")
	)
)

(assert
	(proglang
		(if purpose is mobile and mobile-platform is ios and show-off is no)
		(name "Objective-C")
		(description "Apple предлагает два языка для программирования под iPhone. Objective-C является старым и проверенным.")
	)
)

(assert
	(proglang
		(if purpose is mobile and mobile-platform is ios and show-off is yes)
		(name "Swift")
		(description "Apple предлагает два языка для программирования под iPhone. Swift является новым и модным.")
	)
)

(assert
	(proglang
		(if purpose is mobile and mobile-platform is windows)
		(name "C#")
		(description "Под Windows Phone можно программировать только на C#.")
	)
)

(assert
	(proglang
		(if purpose is web and web-specialization is frontend and web-oldscool is yes)
		(name "HTML")
		(description "Если Вы, как и Андрей Викторович Столяров, хотите гибели Web 2.0, Ваш выбор ограничивается старым и проверенным решением.")
	)
)

(assert
	(proglang
		(if purpose is web and web-specialization is frontend and web-oldscool is no)
		(name "HTML5 + JavaScript")
		(description "Современные стартапы разрабатываются исключительно на новейших технологиях.")
	)
)

(assert
	(proglang
		(if purpose is web and web-specialization is backend and highload is yes)
		(name "C++ + Wt")
		(description "Высоконагруженные приложения можно разрабатывать только на C/C++. Фреймворк Wt позволяет делать это просто и не больно.")
	)
)

(assert
	(proglang
		(if purpose is web and web-specialization is backend and highload is no and difficulty is easy)
		(name "PHP")
		(description "PHP -- простой язык для начинающих, позволяющий даже пятикласснику написать свой сайт.")
	)
)

(assert
	(proglang
		(if purpose is web and web-specialization is backend and highload is no and difficulty is medium)
		(name "Python")
		(description "Pyhon -- простой язык для продолжающих, позволяющий даже семикласснику написать свой сайт.")
	)
)

(assert
	(proglang
		(if purpose is web and web-specialization is backend and highload is no and difficulty is hard)
		(name "Ruby")
		(description "Ruby -- японская разработка, позволяющая даже девятикласснику написать свой сайт.")
	)
)

(assert
	(proglang
		(if purpose is desktop and desktop-type is driver)
		(name "Plain C")
		(description "Драйверы для всех операционных систем пишутся только на Си.")
	)
)

(assert
	(proglang
		(if purpose is desktop and desktop-type is game and desktop-platform is windows)
		(name "C++ + DirectX")
		(description "Игры разрабатываются на C++ из-за требований к производительности. DirectX позволяет использовать все преимущества графических карт на Windows.")
	)
)

(assert
	(proglang
		(if purpose is desktop and desktop-type is game and desktop-platform is linux)
		(name "C++ + OpenGL")
		(description "Игры разрабатываются на C++ из-за требований к производительности. OpenGL позволяет использовать некоторые преимущества графических карт на Linux.")
	)
)

(assert
	(proglang
		(if purpose is desktop and desktop-type is game and desktop-platform is osx)
		(name "Objective-C + OpenGL")
		(description "Приложения для Mac можно разрабатывать только на Objective-C. OpenGL позволяет использовать некоторые преимущества графических карт на Linux.")
	)
)

(assert
	(proglang
		(if purpose is desktop and desktop-type is graphics and crossplatform is yes and difficulty is easy)
		(name "Java")
		(description "Java -- язык, на котором миллионы индусов программируют кросплатформенные приложения.")
	)
)

(assert
	(proglang
		(if purpose is desktop and desktop-type is graphics and crossplatform is yes and difficulty is medium)
		(name "Python + Qt")
		(description "Python -- простой язык, а Qt -- кросплатформенный фреймворк.")
	)
)

(assert
	(proglang
		(if purpose is desktop and desktop-type is graphics and crossplatform is yes and difficulty is hard)
		(name "C++ + Qt")
		(description "C++ -- сложный язык, а Qt -- кросплатформенный фреймворк.")
	)
)

(assert
	(proglang
		(if purpose is desktop and desktop-type is graphics and crossplatform is no and desktop-platform is windows)
		(name "C#")
		(description "Начиная с 2007 года, программы под Windows принято писать на C#.")
	)
)

(assert
	(proglang
		(if purpose is desktop and desktop-type is graphics and crossplatform is no and desktop-platform is osx and difficulty is easy)
		(name "Python + Qt")
		(description "Python -- простой язык, а Qt -- кросплатформенный фреймворк.")
	)
)

(assert
	(proglang
		(if purpose is desktop and desktop-type is graphics and crossplatform is no and desktop-platform is linux and difficulty is easy)
		(name "Python + Qt")
		(description "Python -- простой язык, а Qt -- кросплатформенный фреймворк.")
	)
)

(assert
	(proglang
		(if purpose is desktop and desktop-type is graphics and crossplatform is no and desktop-platform is osx and difficulty is medium)
		(name "C++ + Qt")
		(description "C++ -- сложный язык, а Qt -- кросплатформенный фреймворк, на котором просто писать некросплатформенный код.")
	)
)

(assert
	(proglang
		(if purpose is desktop and desktop-type is graphics and crossplatform is no and desktop-platform is linux and difficulty is medium)
		(name "C++ + Qt")
		(description "C++ -- сложный язык, а Qt -- кросплатформенный фреймворк, на котором просто писать некросплатформенный код.")
	)
)

(assert
	(proglang
		(if purpose is desktop and desktop-type is graphics and crossplatform is no and desktop-platform is osx and difficulty is hard)
		(name "Objective-C")
		(description "Максимальные возможности макбука можно получить только программируя на Objective-C.")
	)
)

(assert
	(proglang
		(if purpose is desktop and desktop-type is graphics and crossplatform is no and desktop-platform is linux and difficulty is hard)
		(name "Plain C + GTK")
		(description "Все старые и некрасивые программы под Linux написаны на Си с использованием фреймворка GTK, но работают лучше и быстрее всех.")
	)
)

(assert
	(proglang
		(if purpose is desktop and desktop-type is science and science-type is computation and difficulty is easy)
		(name "Python + NumPy")
		(description "Python -- простой и медленный язык, NumPy делает математические вычисления немного быстрее.")
	)
)

(assert
	(proglang
		(if purpose is desktop and desktop-type is science and science-type is computation and difficulty is medium)
		(name "C++")
		(description "C++ -- лучший язык для быстрых вычислений.")
	)
)

(assert
	(proglang
		(if purpose is desktop and desktop-type is science and science-type is computation and difficulty is hard)
		(name "C++ + OpenMP")
		(description "C++ -- лучший язык для быстрых вычислений, а библиотека OpenMP позволяет вычислять сразу на кластере из машин.")
	)
)

(assert
	(proglang
		(if purpose is desktop and desktop-type is science and science-type is AI and ai-type is ontology)
		(name "Protege")
		(description "Каждый магистр АЯ знает, что лучший выбор языка для создания онтологии -- это Protege!")
	)
)

(assert
	(proglang
		(if purpose is desktop and desktop-type is science and science-type is AI and ai-type is expert-system)
		(name "CLIPS")
		(description "Каждый магистр АЯ знает, что лучший выбор языка для создания ЭС -- это CLIPS!")
	)
)

(assert
	(proglang
		(if purpose is desktop and desktop-type is science and science-type is AI and ai-type is prototyping)
		(name "ПЛЭНЕР")
		(description "Каждый бакалавр АЯ знает, что лучший выбор языка для прототипирования и перебора -- это ПЛЭНЕР!")
	)
)

(assert
	(proglang
		(if purpose is self-development and difficulty is easy)
		(name "Pascal")
		(description "Pascal -- простой язык, который научит вас понимать основные принципы императивного программирования и алгоритмов.")
	)
)

(assert
	(proglang
		(if purpose is self-development and difficulty is medium and is-web is no)
		(name "C++")
		(description "C++ -- язык, который нужно знать каждому.")
	)
)

(assert
	(proglang
		(if purpose is self-development and difficulty is medium and is-web is yes)
		(name "Python")
		(description "Стартапы пишутся на языке Python.")
	)
)

(assert
	(proglang
		(if purpose is self-development and difficulty is hard and is-functional is yes and show-off is no)
		(name "Lisp")
		(description "Классический вариант для понимания принципов функционального программирования.")
	)
)

(assert
	(proglang
		(if purpose is self-development and difficulty is hard and is-functional is yes and show-off is yes)
		(name "Haskell")
		(description "Скорее всего, вы ничего не поймёте, но сможете участвовать в дискуссиях о монадах.")
	)
)

(assert
	(proglang
		(if purpose is self-development and difficulty is hard and is-functional is no and show-off is no)
		(name "Совершенствоваться в C++")
		(description "Язык С++ и его новые стандарты позволяют почти всегда найти для себя что-то новое.")
	)
)

(assert
	(proglang
		(if purpose is self-development and difficulty is hard and is-functional is no and show-off is yes)
		(name "Perl")
		(description "Perl -- язык, корректные программы на котором выглядят как случайные последовательности служебных символов.")
	)
)
