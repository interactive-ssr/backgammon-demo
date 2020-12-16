(deftag code-sample (children &key theme)
  <div class="code-sample">
    <button class="toggle-theme"
            name="action" value=(if (eq theme :dark) "light" "dark")
            onclick="rr(this)">
      ,(if (eq theme :dark)
           #\sun_with_face
           #\new_moon_with_face)
    </button>
    <pre>,@children </pre>
  </div>)
    
(define-easy-handler (backgammon-tutorial :uri "/backgammon-tutorial")
    (action)
  (let* ((theme (if (string= "dark" action)
                    :dark :light))
         (body #.(read-from-string (uiop:read-file-string "tutorial-body.html"))))
    (let (sitemap)
      (walk body
            (lambda (tag)
              (when (member (xml-tag-name tag)
                            '(:h1 :h2 :h3 :h4 :h5 :h6))
                 (let* ((simple-text (str:trim (plump:text 
                                                (plump:parse
                                                 (write-html tag)))))
                        (link-text (ppcre:regex-replace-all 
                                    "[ \\t\\r\\n]+" simple-text "-")))
                  (setf (xml-tag-attributes tag)
                      (acons "id" link-text (xml-tag-attributes tag)))
                (push
                 <li class=(format nil "~a" (xml-tag-name tag))>
                   <a href=(format nil "#~a" link-text)>,(progn simple-text)</a>
                 </li> sitemap)))
              tag))
      (push
       <ol id="sitemap">
         <b>Page Map</b>
         ,@(reverse sitemap)
       </ol>
       (xml-tag-children body)))
    (write-html
     <html>
       <head>
         <script src="/issr.js"></script>
         <script noupdate=t >
           ,(format nil "setup(~a,~a)" *id* *ws-port*)
         </script>
         <link href="tutorial.css" rel="stylesheet"/>
         <title>ISSR Tutorial: Backgammon</title>
         <style>
           ,(unescaped
             (uiop:read-file-string
              (if (eq theme :dark)
                  ;; load dark css
                  "resources/dark.css"
                  ;; load light css
                  "resources/light.css")))
         </style>
       </head>
       ,(progn body)
     </html>)))
