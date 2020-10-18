(mapc #'ql:quickload '(hunchenissr markup))
(defpackage backgammon
  (:use #:cl #:hunchentoot #:hunchenissr #:markup)
  (:shadowing-import-from #:hunchenissr
   :define-easy-handler
   :start
   :stop
   :redirect))
(in-package #:backgammon)
(markup:enable-reader)

(load (make-pathname :name "backgammon" :type "lisp"))

(defparameter server
  (start (make-instance 'easy-acceptor
                        :port 8080
                        :document-root "resources/")
         :ws-port 4433))

(defun box-muller ()
  (let ((u1 (random 1.0))
        (u2 (random 1.0)))
    (let ((R (sqrt (* -2 (log u1))))
          (Θ (* 2 PI u2)))
      (values (* R (cos Θ))
              (* R (sin Θ))))))

(defconstant die-face
  #(#\die_face-1 #\die_face-2 #\die_face-3 #\die_face-4 #\die_face-5 #\die_face-6))
(defun die-face (n) (aref die-face (- n 1)))

(defparameter games (make-hash-table :test #'equalp)
  "Key: gameid, Value: (list backgammon players...)")

(defun random-alphanum (length &optional not-these)
  "Return alphanumeric string of length LENGTH not contained in NOT-THESE."
  (loop with alphanum
          = (map 'string #'code-char
                 (loop for n from 1 to length
                       collect
                       (if (zerop (random 2))
                           (+ (random 10) 48)
                           (+ (random 26) 97))))
        while (member alphanum not-these)
        finally (return alphanum)))

(defun get-point-move (game index pip)
  "Return the roll to move PIP to point INDEX or nil if it is impossible."
  (and pip (numberp pip) (<= -1 pip 24)
       (some (lambda (rolls)
               (when (= (or (second
                             (reduce (lambda (game--point die)
                                       (when (and game--point die)
                                         (multiple-value-bind (game point) (values-list game--point)
                                           (let ((spot (valid-move-p game point die)))
                                             (when spot
                                               (list (move game point die) spot))))))
                                     rolls :initial-value (list game pip)))
                            -2)
                        index)
                 rolls))
             (get-moves game pip))))

;; add player to game
(defun add-player (socket)
  (let ((gameid (get-parameter
                 "gameid" (first (gethash socket *clients*)))))
    (when (and gameid (not (member socket (gethash gameid games))))
      (setf (gethash gameid games)
            (append (gethash gameid games)
                    (list socket))))))
(pushnew #'add-player on-connect-hook)

;; remove player
(defun remove-player (socket)
  (let ((gameid (get-parameter
                 "gameid" (first (gethash socket *clients*)))))
    (setf (gethash gameid games)
          (remove socket (gethash gameid games)))))
(pushnew #'remove-player on-disconnect-hook)

(defun segment (game from end pip)
  "Return a list of points FROM to END where PIP is the selected pip."
  (mapcar (lambda (point index)
            (let ((turn (when (slot-boundp game 'turn) (turn game)))
                  (move (get-point-move game index pip)))
              <:point color=(first point)
                      onclick=(when move "rr(this)")
                      name="action" value=(when move
                                            (list (quote move)
                                                  (list pip move)))
                      >
                ,@(when point
                    (append (loop for p from 1 below (min 5 (second point))
                                  collect <:pip></:pip>)
                            (list
                             <:pip name="action" value=(list (quote pip) index)
                                   onclick=(when (eq (first point) turn)
                                             "rr(this)")
                                   selected=(and (numberp pip)
                                                 (= pip index)
                                                 (get-moves game pip))
                                   >
                               ,(when (< 5 (second point))
                                  (second point))
                             </:pip>)))
              </:point>))
          (subseq (points game) from end)
          (loop :for index :from from :below end :collect index)))

(define-easy-handler (backgammon :uri "/backgammon")
    ((action :init-form "") (gameid :init-form "") white-color black-color)
  ;; set colors
  (multiple-value-bind (action info)
      (values-list (handler-case
                       (read-from-string action)
                     (t () nil)))
  (when (string= action 'save-colors)
    (set-cookie "white-color"
                :value white-color
                :expires (+ (get-universal-time) 604800)
                :http-only nil)
    (set-cookie "black-color"
                :value black-color
                :expires (+ (get-universal-time) 604800)
                :http-only nil))
  ;; get colors
  (setq white-color (or (when (cookie-out "white-color")
                          (cookie-value (cookie-out "white-color")))
                        (cookie-in "white-color")
                        white-color "#d6d6d6"))
  (setq black-color (or (when (cookie-out "black-color")
                          (cookie-value (cookie-out "black-color")))
                        (cookie-in "black-color")
                        black-color "#292929"))
  ;; ensure gameid
  (unless gameid
    (redirect (format nil "/backgammon?gameid=~a"
                      (random-alphanum 8 (alexandria:hash-table-keys games)))))
  ;; ensure game exists for gameid
  (unless (gethash gameid games)
    (setf (gethash gameid games)
          (list (make-instance 'backgammon)))
    (when *socket*
      (add-player *socket*)))
  ;; roll dice
  (when (string= action 'roll)
    (setf (gethash gameid games)
          (cons (roll-dice (car (gethash gameid games)))
                (cdr (gethash gameid games)))))
  ;; move pip
  (when (string= action 'move)
    (multiple-value-bind (pip dice) (values-list info)
      (let ((new-game (reduce (lambda (game--spot die)
                                (multiple-value-bind (game spot) (values-list game--spot)
                                  (list (move game spot die)
                                        (valid-move-p game spot die))))
                              dice :initial-value (list (first (gethash gameid games)) pip))))
        (when new-game
          (setf (gethash gameid games)
                (cons (first new-game)
                      (cdr (gethash gameid games))))))))
  (let ((game (car (gethash gameid games)))
        (players (cdr (gethash gameid games))))
    ;; update other players
    (dolist (player players)
      (when (and (not (equalp *socket* player))
                 action (string/= action 'noupdate))
        (rr player "?action=(noupdate)")))
    (with-slots (points dice used-dice) game
      ;; make unified dice list
      (let ((dice (sort (append (mapcar (lambda (die) (list die nil))
                                        dice)
                                (mapcar (lambda (die) (list die t))
                                        used-dice))
                        #'< :key #'first))
            (turn (when (slot-boundp game 'turn)
                    (turn game)))
            (winner (finished-p game)))
        (write-html
         <html>
           <head>
             <link href="backgammon.css" rel="stylesheet"/>
             <link href="site.css" rel="stylesheet"/>
             <script src="issr.js"></script>
             <script noupdate=t >
               ,(format nil "setup(~a,~a)" *id* *ws-port*)
             </script>
             <title>Backgammon -- ISSR</title>
           </head>
           <body>
             <style>
               ,(format nil "backgammon {
                 --white: ~a;
                 --black: ~a;
               }" white-color black-color)
             </style>
             <h1>Backgammon</h1>
             <button name="action" value="(roll)"
                     onclick="rr(this)"
                     disabled=(can-move-p game)>
               Roll Dice
             </button>
             <:backgammon id="board">
               <:dice color=(when turn
                              (symbol-name (turn game)))
                      >
                 ,@(mapcar (lambda (die)
                             <:die disabled=(or (second die) (not (can-move-p game)))
                                   style=(when (string= action 'roll)
                                           (format nil "animation: .25s roll linear ~a;"
                                                   (min (round (abs (* (box-muller) 5))))))
                                   >
                               ,(die-face (first die))
                             </:die>)
                           dice)
               </:dice>
               <:backgammon-top>
                 ,(let ((move (get-point-move game +white-goal+ (when (string= action 'pip) info))))
                    <:goal color="white" name="action" value=(list (quote move) (list info move))
                           onclick=(when move "rr(this)")>
                      ,@(loop for p from 0 below (white-goal game)
                              collect <:pip></:pip>)
                    </:goal>)
                 ,@(segment game 0 6 (when (string= action 'pip) info))
                 <:bar color="white">
                   ,@(loop for p from 0 below (white-bar game)
                           collect <:pip name="action" value=(list (quote pip) +white-bar+)
                                         onclick="rr(this)"
                                         selected=(and (eq turn :white)
                                                       (string= action (quote pip))
                                                       (= info +white-bar+)
                                                       (get-moves game info))
                                         >
                                   </:pip>)
                 </:bar>
                 ,@(segment game 6 12 (when (string= action 'pip) info))
               </:backgammon-top>
               <:backgammon-bottom>
                 ,@(segment game 12 18 (when (string= action 'pip) info))
                 <:bar color="black">
                   ,@(loop for p from 0 below (black-bar game)
                           collect <:pip name="action" value=(list (quote pip) +black-bar+)
                                         onclick="rr(this)"
                                         selected=(and (eq turn :black)
                                                       (string= action (quote pip))
                                                       (= info +black-bar+)
                                                       (get-moves game info))
                                         >
                                   </:pip>)
                 </:bar>
                 ,@(segment game 18 24 (when (string= action 'pip) info))
                 ,(let ((move (get-point-move game +black-goal+ (when (string= action 'pip) info))))
                    <:goal color="black" name="action"
                           value=(list (quote move) (list info move))
                           onclick=(when move "rr(this)")>
                      ,@(loop for p from 0 below (black-goal game)
                              collect <:pip></:pip>)
                    </:goal>)
               </:backgammon-bottom>
             </:backgammon>
             ,(when winner
                <h2>,(progn winner) WINS!</h2>)
             <fieldset>
               <legend>Colors</legend>
               <label for="white-color">White Color:</label>
               <input type="color" name="white-color" value=white-color />
               <br/>
               <label for="black-color">Black Color:</label>
               <input type="color" name="black-color" value=black-color />
               <br/>
               <p>Saving colors uses cookies 🍪</p>
               <button onclick="rr(this)"
                       name="action" value="(save-colors)">
                 Save Colors
               </button>
             </fieldset>
           </body>
         </html>))))))

;; delete old backgammon games
(bordeaux-threads:make-thread
 (lambda ()
   (dolist (gameid (alexandria:hash-table-keys games))
     (let ((game (gethash gameid games)))
       (when (or (null game)
                 (< (- 86400
                       (get-universal-time)
                       (time-created game))))
         (remhash gameid games))))
   (sleep 86400))
 :name "cleanup backgammon games")
