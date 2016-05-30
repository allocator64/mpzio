; открыть CLIPS IDE
; нажать cmd+shift+L
; открыть там этот файл
; набрать (run)

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
	?r <- (proglang
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
	?r <- (proglang
					(if $?a&:(=(length$ ?a) 0))
					(name ?name)
					(description ?description)
				)
	=>
		(printout t ?name crlf ?description crlf)
)

(defrule update-question-rules
	?r <- (question-rule
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
	?r <- (question-rule (if $?a&:(=(length$ ?a) 0)) (then ?f))
	?q <- (question (id ?f) (is-hidden yes) )
		(not (answer (id ?f)))
	=>
		(modify ?q (is-hidden no))
)

(deffunction do-ask
	(?question ?choices)
	(printout t ?question " " ?choices " : ")
	(bind ?answer (read) )
	(while (not (member$ ?answer ?choices)) do
		(printout t "Нету такого варианта! Пожалуйста, выберите из списка " ?choices " : " )
		(bind ?answer (read))
		(if (lexemep ?answer)
			then (bind ?answer (lowcase ?answer))
		)
	)
	(printout t crlf)
	?answer
)

(defrule ask-question
	?q <- (question (text ?question)
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
	)
)

(assert
	(proglang
		(if purpose is mobile and mobile-platform is ios and show-off is no)
		(name "Objective-C")
	)
)

(assert
	(proglang
		(if purpose is mobile and mobile-platform is ios and show-off is yes)
		(name "Swift")
	)
)

(assert
	(proglang
		(if purpose is mobile and mobile-platform is windows)
		(name "C#")
	)
)

(assert
	(proglang
		(if purpose is web and web-specialization is frontend and web-oldscool is yes)
		(name "HTML")
	)
)

(assert
	(proglang
		(if purpose is web and web-specialization is frontend and web-oldscool is no)
		(name "HTML5 + JavaScript")
	)
)

(assert
	(proglang
		(if purpose is web and web-specialization is backend and highload is yes)
		(name "C++ + Wt")
	)
)

(assert
	(proglang
		(if purpose is web and web-specialization is backend and highload is no and difficulty is easy)
		(name "PHP")
	)
)

(assert
	(proglang
		(if purpose is web and web-specialization is backend and highload is no and difficulty is medium)
		(name "Python")
	)
)

(assert
	(proglang
		(if purpose is web and web-specialization is backend and highload is no and difficulty is hard)
		(name "Ruby")
	)
)

(assert
	(proglang
		(if purpose is desktop and desktop-type is driver)
		(name "Plain C")
	)
)

(assert
	(proglang
		(if purpose is desktop and desktop-type is game and desktop-platform is windows)
		(name "C++ + DirectX")
	)
)

(assert
	(proglang
		(if purpose is desktop and desktop-type is game and desktop-platform is linux)
		(name "C++ + SDL")
	)
)

(assert
	(proglang
		(if purpose is desktop and desktop-type is game and desktop-platform is osx)
		(name "Objective-C + Cocoa")
	)
)

(assert
	(proglang
		(if purpose is desktop and desktop-type is graphics and crossplatform is yes and difficulty is easy)
		(name "Java")
	)
)

(assert
	(proglang
		(if purpose is desktop and desktop-type is graphics and crossplatform is yes and difficulty is medium)
		(name "Python + QT")
	)
)

(assert
	(proglang
		(if purpose is desktop and desktop-type is graphics and crossplatform is yes and difficulty is hard)
		(name "C++ + QT")
	)
)

(assert
	(proglang
		(if purpose is desktop and desktop-type is graphics and crossplatform is no and desktop-platform is windows)
		(name "C#")
	)
)

(assert
	(proglang
		(if purpose is desktop and desktop-type is graphics and crossplatform is no and desktop-platform is osx and difficulty is easy)
		(name "Python + QT")
	)
)

(assert
	(proglang
		(if purpose is desktop and desktop-type is graphics and crossplatform is no and desktop-platform is linux and difficulty is easy)
		(name "Python + QT")
	)
)

(assert
	(proglang
		(if purpose is desktop and desktop-type is graphics and crossplatform is no and desktop-platform is osx and difficulty is medium)
		(name "C++ + QT")
	)
)

(assert
	(proglang
		(if purpose is desktop and desktop-type is graphics and crossplatform is no and desktop-platform is linux and difficulty is medium)
		(name "C++ + QT")
	)
)

(assert
	(proglang
		(if purpose is desktop and desktop-type is graphics and crossplatform is no and desktop-platform is osx and difficulty is hard)
		(name "Objective-C")
	)
)

(assert
	(proglang
		(if purpose is desktop and desktop-type is graphics and crossplatform is no and desktop-platform is linux and difficulty is hard)
		(name "C++ + GTK")
	)
)

(assert
	(proglang
		(if purpose is science and science-type is computation and difficulty is easy)
		(name "Python + NumPy")
	)
)

(assert
	(proglang
		(if purpose is science and science-type is computation and difficulty is medium)
		(name "C++")
	)
)

(assert
	(proglang
		(if purpose is science and science-type is computation and difficulty is hard)
		(name "C++ + OpenMP")
	)
)

(assert
	(proglang
		(if purpose is science and science-type is AI and ai-type is ontology)
		(name "Protege")
	)
)

(assert
	(proglang
		(if purpose is science and science-type is AI and ai-type is expert-system)
		(name "CLIPS")
	)
)

(assert
	(proglang
		(if purpose is science and science-type is AI and ai-type is prototyping)
		(name "ПЛЭНЕР")
	)
)

(assert
	(proglang
		(if purpose is self-development and difficulty is easy)
		(name "Pascal")
	)
)

(assert
	(proglang
		(if purpose is self-development and difficulty is medium and is-web is no)
		(name "C++")
	)
)

(assert
	(proglang
		(if purpose is self-development and difficulty is medium and is-web is yes)
		(name "Python")
	)
)

(assert
	(proglang
		(if purpose is self-development and difficulty is hard and is-functional is yes and show-off is no)
		(name "Lisp")
	)
)

(assert
	(proglang
		(if purpose is self-development and difficulty is hard and is-functional is yes and show-off is yes)
		(name "Haskell")
	)
)

(assert
	(proglang
		(if purpose is self-development and difficulty is hard and is-functional is no and show-off is no)
		(name "Совершенствоваться в C++")
	)
)

(assert
	(proglang
		(if purpose is self-development and difficulty is hard and is-functional is no and show-off is yes)
		(name "Perl")
	)
)

(clear-window)
