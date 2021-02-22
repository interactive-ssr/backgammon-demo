(defpackage backgammon
  (:use #:cl #:markup)
  (:import-from #:hunchentoot
                easy-acceptor
                set-cookie
                cookie-value
                cookie-in
                cookie-out)
  (:import-from #:hunchenissr
                define-easy-handler
                *id*
                *socket*
                *ws-port*
                start
                stop
                redirect))
(in-package #:backgammon)
(markup:enable-reader)

(load "backgammon.lisp")

(defparameter server
  (start (make-instance 'easy-acceptor
                        :port 8080
                        :document-root "resources/")
         :ws-port 4433))

(defconstant +die-faces+ "⚀⚁⚂⚃⚄⚅")
(defun die-face (n) (elt +die-faces+ (- n 1)))

(defparameter *games* (make-hash-table :test #'equalp)
  "Key: gameid, Value: (list backgammon players...)")

(defun random-alphanum (length &key not-in)
  "Return alphanumeric string of length LENGTH not contained in NOT-IN."
  (loop with alphanum
          = (map 'string #'code-char
                 (loop repeat length collect
                       (if (zerop (random 2))
                           (+ (random 10) 48)
                           (+ (random 26) 97))))
        while (member alphanum not-in)
        finally (return alphanum)))

;; add player to game
(defun add-player (socket)
  (let ((script (hunchentoot:script-name (first (gethash socket hunchenissr:-clients-))))
        (gameid (hunchentoot:get-parameter
                 "gameid" (first (gethash socket hunchenissr:-clients-)))))
    (when (and script (string= script "/backgammon")
               gameid (not (member socket (gethash gameid *games*))))
      (setf (gethash gameid *games*)
            (append (gethash gameid *games*)
                    (list socket))))))
(pushnew #'add-player hunchenissr:-on-connect-hook-)

;; remove player
(defun remove-player (socket)
  (let ((gameid (hunchentoot:get-parameter
                 "gameid" (first (gethash socket hunchenissr:-clients-)))))
    (setf (gethash gameid *games*)
          (remove socket (gethash gameid *games*)))))
(pushnew #'remove-player hunchenissr:-on-disconnect-hook-)

(deftag segment (&key pip-action point-action game from to pip)
  "Return a list of points FROM to TO where PIP is the selected pip."
  <merge-tag>
    ,@(map 'list
           (lambda (point index)
             (let ((turn (turn game))
                   (move (get-point-move game index pip)))
               <:point color=(first point)
                       action=point-action
                       value=(when move
                               (list pip move))
                       onclick=(when move "rr(this)")>
                 ,@(when point
                     (append (loop repeat (- (min 5 (second point)) 1) collect
                                   <:pip></:pip>)
                             (list
                              <:pip action=pip-action
                                    onclick=(when (eq (first point) turn)
                                              "rr(this)")
                                    selected=(when (and (numberp pip)
                                                        (= pip index)
                                                        (get-moves game pip))
                                               t)
                                    value=index >
                                ,(when (< 5 (second point))
                                   (second point))
                              </:pip>)))
               </:point>))
              (subseq (points game) from to)
              (loop :for index :from from :below to :collect index))
  </merge-tag>)

(define-easy-handler (backgammon :uri "/backgammon")
    (;; GET parameters go here
     move pip roll gameid save-colors white-color black-color)
  ;; ensure gameid
  (unless gameid
    (redirect (format nil "/backgammon?gameid=~a"
                      (random-alphanum 8 :not-in (alexandria:hash-table-keys *games*)))))
  ;; set colors
  (unless (str:emptyp save-colors)
    (set-cookie "white-color"
                :value white-color
                :expires (+ (get-universal-time) 604800)
                :http-only nil)
    (set-cookie "black-color"
                :value black-color
                :expires (+ (get-universal-time) 604800)
                :http-only nil))
  ;; get colors from cookies
  (setq white-color (or (when (cookie-out "white-color")
                          (cookie-value (cookie-out "white-color")))
                        (cookie-in "white-color")
                        "#d6d6d6"))
  (setq black-color (or (when (cookie-out "black-color")
                          (cookie-value (cookie-out "black-color")))
                        (cookie-in "black-color")
                        "#292929"))
  ;; ensure game exists for gameid
  (unless (gethash gameid *games*)
    (setf (gethash gameid *games*)
          (list (make-instance 'backgammon))))
  ;; roll dice
  (when (string= t roll)
    (setf (first (gethash gameid *games*))
          (roll-dice (first (gethash gameid *games*)))))
  ;; move pip
  (unless (str:emptyp move)
    (multiple-value-bind (pip dice)
        (values-list
         (handler-case
             (let ((*read-eval* nil))
               (read-from-string move))
           (error () nil)))
      (let ((new-game
              (reduce
               (lambda (game--spot die)
                 (multiple-value-bind (game spot)
                     (values-list game--spot)
                   (list (move game spot die)
                         (valid-move-p game spot die))))
               dice :initial-value (list (first (gethash gameid *games*)) pip))))
        (when new-game
          (setf (first (gethash gameid *games*))
                (first new-game))))))
  (let* (;; local variables go here
         (game (first (gethash gameid *games*)))
         (players (rest (gethash gameid *games*)))
         (pip-num  (parse-integer (or pip "")
                                :junk-allowed t))
         (dice (sort (append (map 'list
                                  (lambda (die) (list die nil))
                                  (dice game))
                             (map 'list
                                  (lambda (die) (list die t))
                                  (used-dice game)))
                     #'< :key #'first))
         (winner (finished-p game)))
    (with-slots (points turn white-goal white-bar black-goal black-bar) game
      ;; update other players
      (dolist (player players)
        ;; base case
        (when (and (not (equalp *socket* player))
                   (or (not (str:emptyp move))
                       (and (not (str:emptyp roll))
                            (not (string= roll "NIL")))))
          (hunchenissr:rr player (str:concat "?roll="
                                             (unless (str:emptyp roll)
                                               "NIL")
                                             "&move="))))
      (write-html
       <html>
         <head>
           <link href="backgammon.css" rel="stylesheet"/>
           <link href="site.css" rel="stylesheet"/>
           <script src="issr.js"></script>
           <script noupdate="t">
             ,(format nil "setup(~a,~a)" *id* *ws-port*)
           </script>
           <title>,(progn "Backgammon | ISSR")</title>
         </head>
         <body>
           <style>
             ,(format nil "#board {--white: ~a; --black: ~a;}#winner {color:~a;}"
                      white-color black-color
                      (if (eq winner :black) black-color white-color))
           </style>
           <h1>Backgammon</h1>
           <button action="roll"
                   value=t
                   onclick="rr(this)"
                   disabled=(when (can-move-p game) t)>
             Roll Dice
           </button>
           <:backgammon id="board">
             <:dice color=turn >
               ,@(map 'list
                      (lambda (die)
                        <:die style=(unless (str:emptyp roll)
                                      (format nil "animation: .1s roll linear ~a;"
                                              (+ (random 8) 2)))
                              disabled=(when (or (second die) (not (can-move-p game))) t)>
                          ,(die-face (first die))
                        </:die>)
                         dice)
             </:dice>
             <:backgammon-top>
               ,(let ((move (get-point-move game +white-goal+ pip-num)))
                  <:goal color="white"
                         action="move"
                         value=(when move (list pip-num move))
                         onclick=(when move "rr(this)")>
                    ,@(loop repeat white-goal collect
                            <:pip></:pip>)
                  </:goal>)
               <segment pip-action="pip"
                        point-action="move"
                        game=game from=0 to=6
                        pip=pip-num />
               <:bar color="white">
                 ,@(loop repeat white-bar collect
                         <:pip action="pip"
                               value=+white-bar+
                               selected=(when (and (eq turn :white)
                                                   pip-num
                                                   (= pip-num +white-bar+)
                                                   (get-moves game pip-num))
                                          t)
                               onclick="rr(this)">
                         </:pip>)
               </:bar>
               <segment pip-action="pip"
                        point-action="move"
                        game=game from=6 to=12 pip=pip-num />
             </:backgammon-top>
             <:backgammon-bottom>
               <segment pip-action="pip"
                        point-action="move"
                        game=game from=12 to=18 pip=pip-num />
               <:bar color="black">
                 ,@(loop repeat black-bar collect
                         <:pip action="pip"
                               value=+black-bar+
                               selected=(when (and (eq turn :black)
                                                   pip-num
                                                   (= pip-num +black-bar+)
                                                   (get-moves game pip-num))
                                          t)
                               onclick="rr(this)">
                         </:pip>)
               </:bar>
               <segment pip-action="pip"
                        point-action="move"
                        game=game from=18 to=24 pip=pip-num />
               ,(let ((move (get-point-move game +black-goal+ pip-num)))
                  <:goal color="black"
                         action="move"
                         value=(when move (list pip-num move))
                         onclick=(when move "rr(this)")>
                    ,@(loop repeat black-goal collect
                            <:pip></:pip>)
                  </:goal>)
             </:backgammon-bottom>
           </:backgammon>
           ,(when winner
              <p id="win-message"><span id="winner">,(progn winner)</span> WINS!</p>)
           <fieldset>
             <legend><b>Colors</b></legend>
             <label for="white-color">White Color:</label>
             <input type="color" name="white-color" value=white-color />
             <br/>
             <label for="black-color">Black Color:</label>
             <input type="color" name="black-color" value=black-color />
             <br/>
             <p>Saving colors saves a file on your computer 🍪</p>
             <button action="save-colors"
                     value="t"
                     onclick="rr(this)">
               Save Colors
             </button>
           </fieldset>
         </body>
       </html>))))

;; delete old backgammon games
(bordeaux-threads:make-thread
 (lambda ()
   (dolist (gameid (alexandria:hash-table-keys *games*))
     (let ((game (first (gethash gameid *games*))))
       (when (or (null game)
                 (< 86400
                    (- (get-universal-time)
                       (time-created game))))
         (remhash gameid *games*))))
   (sleep 86400))
 :name "cleanup backgammon games")

(define-easy-handler (backgammon-tutorial :uri "/backgammon-tutorial")
    (theme)
  (write-html
   <php:tutorial title="Backgammon Tutorial | ISSR"
                 body-tags=(progn
                             #.(read-from-string
                                (uiop:read-file-string
                                 "tutorial-body.html")))
                 theme=theme />))
