(deftemplate laptop
    (slot name)
    (slot budget)
    (slot usage)
    (slot mobility)
    (slot heavy-gpu)
    (slot big-ram-storage)
    (slot high-refresh-rate)
    (slot mac-os)(slot specs))

(deffunction ask-question(?question $?allowed-values)
    (printout t ?question)
    (bind ?answer (read))
    (if (lexemep ?answer) then (bind ?answer (lowcase ?answer)))
    (while (not (member$ ?answer ?allowed-values)) do
        (printout t ?question)
        (bind ?answer (read))
        (if (lexemep ?answer) then (bind ?answer (lowcase ?answer))))
    ?answer)

(deffunction y-or-n (?question)
    (bind ?response (ask-question ?question y n))
    (if (eq ?response y) then yes else no)) 

(deffacts initial
    (phase startup))

(defrule system-on
    ?p <- (phase startup)
    =>
    (printout t "~ ~ EXPERT SYSTEM FOR LAPTOP RECOMMENDATION ~ ~" crlf)
    (printout t "Siap merekomendasikan laptop sesuai kebutuhanmu!" crlf)
    (retract ?p)
    (assert (phase ask-questions)))

(defrule determine-budget
    (declare (salience 20))
    (phase ask-questions)
    (not (budget ?))
    =>
    (assert (budget (ask-question 
    "Berapa budget Anda?
    low  = 5-10  juta
    mid  = 10-20 juta
    high = > 20  juta
    (low/mid/high): " low mid high))))

(defrule determine-main-usage
    (declare (salience 20))
    (phase ask-questions)
    (not (usage ?))
    =>
    (assert (usage (ask-question "Apa kebutuhan Anda? (office/gaming/coding): " office gaming coding))))

(defrule determine-mobility
    (declare (salience 15))
    (phase ask-questions)
    (not (mobility ?))
    =>
    (assert (mobility (y-or-n "Apakah Anda sering membawa laptop bepergian (y/n)? "))))

(defrule determine-gpu-intensity
    (declare (salience 10))
    (phase ask-questions)
    (or (usage office) (usage coding))
    (not (heavy-gpu ?))
    =>
    (assert (heavy-gpu (y-or-n "Apakah butuh graphic card yang tinggi? (y/n): "))))

(defrule determine-ram-storage
    (declare (salience 10))
    (phase ask-questions)
    (or (usage office) (usage coding))
    (not (big-ram-storage ?))
    =>
    (assert (big-ram-storage (y-or-n "Apakah Anda butuh RAM/Storage besar? (y/n): "))))

(defrule determine-os
    (declare (salience 10))
    (phase ask-questions)
    (or (usage coding) (usage office))
    (not (mac-os ?))
    =>
    (assert (mac-os (y-or-n "Apakah Anda membutuhkan MacOS untuk development (y/n)? "))))

(defrule determine-gaming-refresh-rate
    (declare (salience 10))
    (phase ask-questions)
    (usage gaming)
    (not (high-refresh-rate ?))
    =>
    (assert (high-refresh-rate (y-or-n "Apakah Anda butuh refresh rate layar tinggi untuk game kompetitif? (y/n): "))))


(defrule start-matching
    (declare (salience -1)) 
    ?p <- (phase ask-questions)
    =>
    ; Cek keberadaan fakta menggunakan any-factp
    (if (not (any-factp ((?f mobility)) TRUE)) then (assert (mobility any)))
    (if (not (any-factp ((?f heavy-gpu)) TRUE)) then (assert (heavy-gpu any)))
    (if (not (any-factp ((?f big-ram-storage)) TRUE)) then (assert (big-ram-storage any)))
    (if (not (any-factp ((?f high-refresh-rate)) TRUE)) then (assert (high-refresh-rate any)))
    (if (not (any-factp ((?f mac-os)) TRUE)) then (assert (mac-os any)))
    
    (retract ?p)
    (assert (phase recommendation)))

(defrule find-laptop
    (phase recommendation)
    (budget ?b)
    (usage ?u)
    (mobility ?user-m)
    (heavy-gpu ?user-g)
    (big-ram-storage ?user-rs)
    (high-refresh-rate ?user-rr)
    (mac-os ?user-mac)
    
    ; Cari laptop yang cocok
    (laptop (name ?n) (specs ?s) (budget ?b) (usage ?u)
           (mobility ?m & :(or (eq ?user-m any) (eq ?m ?user-m)))
           (heavy-gpu ?g & :(or (eq ?user-g any) (eq ?g ?user-g)))
           (big-ram-storage ?rs & :(or (eq ?user-rs any) (eq ?rs ?user-rs)))
           (high-refresh-rate ?rr & :(or (eq ?user-rr any) (eq ?rr ?user-rr)))
           (mac-os ?mac & :(or (eq ?user-mac any) (eq ?mac ?user-mac))))
    =>
    (assert (found ?n ?s)))

(defrule print-result
    (phase recommendation)
    (found ?n ?s)
    =>
    (printout t crlf "HASIL REKOMENDASI:" crlf)
    (printout t ">> " ?n " (" ?s ")" crlf))

(defrule no-match 
    (declare (salience -10))
    (phase recommendation)
    (not (found ? ?))
    =>
    (printout t crlf "Maaf, tidak ada laptop yang cocok dengan kriteria spesifik Anda." crlf))