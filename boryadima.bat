; открыть CLIPS IDE
; нажать cmd+shift+L
; открыть там этот файл
; набрать (run)

(deftemplate answer
	(slot question-id  (default none))
	(slot value (default none))
)

;;Define the question template with validation constaints slots
(deftemplate question
	(slot question-id (default none))
	(slot question-to-ask (default none))
	(slot has-pre-condition (type SYMBOL) (default yes))
	(multislot choices (default yes no))
)

;; ask-question with validation
(deftemplate proglang
	(multislot if)
	(slot name)
	(slot description (default ""))
)

(defrule remove-proglang-good-condition
	?r <- (proglang
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

(defrule print-proglang
	?r <- (proglang
					(if $?a&:(=(length$ ?a) 0))
					(name ?name)
					(description ?description)
				)
	=>
		(printout t ?name crlf ?description crlf)
)

;ask
(deffunction ask
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


;============= PURPOSE =============
(assert
	(question
		(question-id purpose)
		(question-to-ask "Для какой цели выбирается язык программирования?")
		(choices web mobile desktop self-development)
		(has-pre-condition no)
	)
)

;============= DESKTOP TYPE =============
(assert
	(question
		(question-id desktop-type)
		(question-to-ask "Что вы хотите разработать?")
		(choices game graphics driver science)
	)
)

(assert
	(question-rule
		(if purpose is desktop)
		(then-ask-question desktop-type)
	)
)

;============= MOBILE PLATFORM =============
(assert
	(question
		(question-id mobile-platform)
		(question-to-ask "Выберите мобильную операционную систему")
		(choices android ios windows)
	)
)

(assert
	(question-rule
		(if purpose is mobile)
		(then-ask-question mobile-platform)
	)
)

;============= DESKTOP PLATFORM =============
(assert
	(question
		(question-id desktop-platform)
		(question-to-ask "Выберите операционную систему")
		(choices windows linux osx)
	)
)

(assert
	(question-rule
		(if purpose is desktop and desktop-type is game)
		(then-ask-question desktop-platform)
	)
)

(assert
	(question-rule
		(if purpose is desktop and desktop-type is graphics and crossplatform is no)
		(then-ask-question desktop-platform)
	)
)

;============= SHOW OFF =============
(assert
	(question
		(question-id show-off)
		(question-to-ask "Хотите ли выделяться среди других программистов в этой области?")
	)
)

(assert
	(question-rule
		(if purpose is mobile and mobile-platform is ios)
		(then-ask-question show-off)
	)
)

(assert
	(question-rule
		(if purpose is self-development and difficulty is hard)
		(then-ask-question show-off)
	)
)

;============= WEB SPECIALIZATION =============
(assert
	(question
		(question-id web-specialization)
		(question-to-ask "Какую часть веб приложения создаете?")
		(choices frontend backend)
	)
)

(assert
	(question-rule
		(if purpose is web)
		(then-ask-question web-specialization)
	)
)

;============= WEB OLDSCOOL =============
(assert
	(question
		(question-id web-oldscool)
		(question-to-ask "Вы Столяров?")
	)
)

(assert
	(question-rule
		(if purpose is web and web-specialization is frontend)
		(then-ask-question web-oldscool)
	)
)

;============= CROSSPLATFORM =============
(assert
	(question
		(question-id crossplatform)
		(question-to-ask "Вы хотите создать кросплатформенное решение?")
	)
)

(assert
	(question-rule
		(if purpose is graphics)
		(then-ask-question crossplatform)
	)
)

;============= HIGHLOAD =============
(assert
	(question
		(question-id highload)
		(question-to-ask "Будете работать с высокой нагрузкой?")
	)
)

(assert
	(question-rule
		(if purpose is web and web-specialization is backend)
		(then-ask-question highload)
	)
)

;============= is-web =============
(assert
	(question
		(question-id is-web)
		(question-to-ask "Вы хотите программировать для WEB?")
	)
)

(assert
	(question-rule
		(if purpose is self-development and difficulty is medium)
		(then-ask-question is-web)
	)
)

;============= is-functional =============
(assert
	(question
		(question-id is-functional)
		(question-to-ask "Вы хотите функциональный язык?")
	)
)

(assert
	(question-rule
		(if purpose is self-development and difficulty is hard)
		(then-ask-question is-functional)
	)
)

;============= DIFFICULTY =============
(assert
	(question
		(question-id difficulty)
		(question-to-ask "Какой вы предпочитаете порог входения?")
		(choices easy medium hard)
	)
)

(assert
	(question-rule
		(if purpose is graphics and crossplatform is yes)
		(then-ask-question difficulty)
	)
)

(assert
	(question-rule
		(if purpose is graphics and crossplatform is no and desktop-platform is osx)
		(then-ask-question difficulty)
	)
)

(assert
	(question-rule
		(if purpose is graphics and crossplatform is no and desktop-platform is linux)
		(then-ask-question difficulty)
	)
)

(assert
	(question-rule
		(if purpose is web and web-specialization is backend and highload is no)
		(then-ask-question difficulty)
	)
)

(assert
	(question-rule
		(if purpose is desktop and desktop-type is science and science-type is computation)
		(then-ask-question difficulty)
	)
)

(assert
	(question-rule
		(if purpose is self-development)
		(then-ask-question difficulty)
	)
)

;============= SCIENCE TYPE =============
(assert
	(question
		(question-id science-type)
		(question-to-ask "Какого вида задачи будут решаться?")
		(choices computation AI)
	)
)

(assert
	(question-rule
		(if purpose is desktop and desktop-type is science)
		(then-ask-question science-type)
	)
)

;============= AI TYPE =============
(assert
	(question
		(question-id ai-type)
		(question-to-ask "Какой областью ИИ вы занимаетесь?")
		(choices ontology expert-system prototyping)
	)
)

(assert
	(question-rule
		(if purpose is desktop and desktop-type is science and science-type is AI)
		(then-ask-question ai-type)
	)
)


;============= PROGRAM LANGS ============
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
