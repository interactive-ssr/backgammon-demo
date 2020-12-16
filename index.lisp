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

(defconstant die-face
  #(#\die_face-1 #\die_face-2 #\die_face-3 #\die_face-4 #\die_face-5 #\die_face-6))
(defun die-face (n) (aref die-face (- n 1)))

(defparameter games (make-hash-table :test #'equalp)
  "Key: gameid, Value: (list backgammon players...)")

(defun random-alphanum (length &key not-in)
  "Return alphanumeric string of length LENGTH not contained in NOT-IN."
  (loop with alphanum
          = (map 'string #'code-char
                 (loop for n from 1 to length
                       collect
                       (if (zerop (random 2))
                           (+ (random 10) 48)
                           (+ (random 26) 97))))
        while (member alphanum not-in)
        finally (return alphanum)))

;; add player to game
(defun add-player (socket)
  (let ((script (script-name (first (gethash socket *clients*))))
        (gameid (get-parameter
                 "gameid" (first (gethash socket *clients*)))))
    (when (and script (string= script "/backgammon")
               gameid (not (member socket (gethash gameid games))))
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

(deftag segment (&key game from end pip)
  "Return a list of points FROM to END where PIP is the selected pip."
  <merge-tag>
    ,@(map 'list
           (lambda (point index)
             (let ((turn (turn game))
                   (move (get-point-move game index pip)))
               <:point color=(first point)
                       name="action"
                       value=(when move
                               (list 'move (list pip move)))
                       onclick=(when move "rr(this)")>
                 ,@(when point
                     (append (loop for p from 1 below (min 5 (second point))
                                   collect <:pip></:pip>)
                             (list
                              <:pip name="action"
                                    onclick=(when (eq (first point) turn)
                                              "rr(this)")
                                    selected=(and (numberp pip)
                                                  (= pip index)
                                                  (get-moves game pip))
                                    value=(list 'pip index)>
                                ,(when (< 5 (second point))
                                   (second point))
                              </:pip>)))
               </:point>))
              (subseq (points game) from end)
              (loop :for index :from from :below end :collect index))
  </merge-tag>)

(define-easy-handler (backgammon :uri "/backgammon")
    (action gameid white-color black-color)
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
                          "#d6d6d6"))
    (setq black-color (or (when (cookie-out "black-color")
                            (cookie-value (cookie-out "black-color")))
                          (cookie-in "black-color")
                          "#292929"))
    ;; ensure gameid
    (unless gameid
      (redirect (format nil "/backgammon?gameid=~a"
                        (random-alphanum 8 :not-in (alexandria:hash-table-keys games)))))
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
                   action)
          (rr player (format nil "?action=~a"
                             (if (string= action 'roll)
                                 '(nil roll)
                                 '(nil))))))
      (with-slots (points dice used-dice turn white-goal white-bar black-goal black-bar) game
        ;; make unified dice list
        (let (;; define variables here
              (dice (sort (append (mapcar (lambda (die) (list die nil))
                                          dice)
                                  (mapcar (lambda (die) (list die t))
                                          used-dice))
                          #'< :key #'first))
              (winner (finished-p game)))
          (write-html
           <html>
             <head>
               <link href="backgammon.css" rel="stylesheet"/>
               <link href="site.css" rel="stylesheet"/>
               <script src="issr.js"></script>
               <script noupdate="t">
                 ,(format nil "setup(~a,~a)" *id* *ws-port*)
               </script>
               <title>Backgammon -- ISSR</title>
             </head>
             <body>
               <style>
                 ,(format nil "#board {--white: ~a; --black: ~a;}#winner {color:~a}"
                          white-color black-color
                          (if (eq winner :black) black-color white-color))
               </style>
               <h1>Backgammon</h1>
               <button name="action" value="(roll)"
                       onclick="rr(this)"
                       disabled=(can-move-p game)>
                 Roll Dice
               </button>
               <:backgammon id="board">
                 <:dice color=(symbol-name turn)>
                   ,@(mapcar (lambda (die)
                               <:die style=(when (or (string= action 'roll)
                                                     (and (null action)
                                                          (string= info 'roll)))
                                             (format nil "animation: .1s roll linear ~a;"
                                                     (+ (random 8) 2)))
                                     disabled=(or (second die) (not (can-move-p game)))>
                                 ,(die-face (first die))
                               </:die>)
                             dice)
                 </:dice>
                 <:backgammon-top>
                   ,(let ((move (get-point-move game +white-goal+
                                                (when (string= action 'pip)
                                                  info))))
                      <:goal color="white" name="action" value=(list 'move (list info move))
                             onclick=(when move "rr(this)")>
                        ,@(loop for p from 0 below white-goal
                                collect <:pip></:pip>)
                      </:goal>)
                   <segment game=game from=0 end=6 pip=(when (string= action 'pip) info)/>
                   <:bar color="white">
                     ,@(loop for p from 0 below white-bar
                             collect <:pip name="action" value=(list 'pip +white-bar+)
                                           selected=(and (eq turn :white)
                                                         (string= action 'pip)
                                                         (= info +white-bar+)
                                                         (get-moves game info))
                                           onclick="rr(this)">
                                     </:pip>)
                   </:bar>
                   <segment game=game from=6 end=12 pip=(when (string= action 'pip) info)/>
                 </:backgammon-top>
                 <:backgammon-bottom>
                   <segment game=game from=12 end=18 pip=(when (string= action 'pip) info)/>
                   <:bar color="black">
                     ,@(loop for p from 0 below black-bar
                             collect <:pip name="action" value=(list 'pip +black-bar+)
                                           selected=(and (eq turn :black)
                                                         (string= action 'pip)
                                                         (= info +black-bar+)
                                                         (get-moves game info))
                                           onclick="rr(this)">
                                     </:pip>)
                   </:bar>
                   <segment game=game from=18 end=24 pip=(when (string= action 'pip) info)/>
                   ,(let ((move (get-point-move game +black-goal+ (when (string= action 'pip) info))))
                      <:goal color="black" name="action"
                             value=(list 'move (list info move))
                             onclick=(when move "rr(this)")>
                        ,@(loop for p from 0 below black-goal
                                collect <:pip></:pip>)
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
     (let ((game (first (gethash gameid games))))
       (when (or (null game)
                 (< 86400
                    (- (get-universal-time)
                       (time-created game))))
         (remhash gameid games))))
   (sleep 86400))
 :name "cleanup backgammon games")

(load (make-pathname :name "tutorial" :type "lisp"))
