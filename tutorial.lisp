(deftag code-sample (children &key theme)
  <div class="code-sample">
    <button class="toggle-theme"
            name="action" value=(if (eq theme :dark) "light" "dark")
            onclick="rr(this)">
      ,(if (eq theme :dark) "🌞" "🌚")
    </button>
    <pre>,@children </pre>
  </div>)
    
(define-easy-handler (backgammon-tutorial :uri "/backgammon-tutorial")
    (action)
  (let* ((theme (if (string= "dark" action)
                   :dark :light))
         (body
           <body>
             <style>
               ,(unescaped
                 (uiop:read-file-string
                  (if (eq theme :dark)
                      #|load dark css|#
                      "resources/dark.css"
                      #|load light css|#
                      "resources/light.css")))
             </style>
             <h1>
               <abbr title="Interactive Server Side Rendering">ISSR</abbr>
               Tutorial: Backgammon
             </h1>
             <p>
               The purpose of this web page is to teach you how to make an interactive website without writing any Javascript code in the style of
               <a href="https://github.com/interactive-ssr/client">
                 <abbr title="Interactive Server Side Rendering">ISSR</abbr>
               </a>.
               You can see the <a href="/backgammon">final product</a> of this tutorial.
             </p>
             <p>
               The full source code for this tutorial is available in the
               <a href="https://github.com/interactive-ssr/backgammon-demo">git repository</a>. 
               This tutorial is not a Lisp, HTML, or CSS tutorial, so I will be providing the css and most of the Lisp. It is primarly to teach how to use the ISSR framework. That said, This tutorial is using the Hunchenissr implementation, so there will be a decent amount of Lisp and you should know the basics. The directory structure for this tutorial is like so. Everything in this tutorial will be in one file:
               <code>index.lisp</code>.
             </p>
             <code-sample theme=theme >backgammon-tutorial         
├─ <a href="http://raw.githubusercontent.com/interactive-ssr/backgammon-demo/master/backgammon.lisp">backgammon.lisp</a>
├─ index.lisp
└─ resources
   ├─ <a href="http://raw.githubusercontent.com/interactive-ssr/backgammon-demo/master/resources/backgammon.css">backgammon.css</a>
   ├─ <a href="http://raw.githubusercontent.com/interactive-ssr/client/master/issr.js">issr.js</a>
   └─ <a href="http://raw.githubusercontent.com/interactive-ssr/backgammon-demo/master/resources/site.css">site.css</a></code-sample>
             <h2>Requirements</h2>
             <p>
               You will need a
               <a href="http://lisp-lang.org">Common Lisp</a>
               implemention (I'm using
               <a href="http://sbcl.org">SBCL</a>
               ),
               <a href="http://quicklisp.org">Quicklisp</a>
               , decent knowledge of HTML, and basic knowledge of Lisp. You do
               <em>not</em>
               need to know CSS, or the rules of
               <a href="http://wikipedia.org/wiki/Backgammon">Backgammon</a>.
             </p>
             <hr/>
             <h2>Setup</h2>
             <p>
               First, we need to load the required packages and bundle them into into our own backgammon tutorial package. We will use quicklisp to load the
               <code>hunchenissr</code>
               package which is what this tutorial is for (this will also load
               <code>hunchentoot</code>
               our HTTP server) and the
               <code>markup</code>
               package which lets us write HTML directly in Lisp. The
               <code>shadowing-import-from</code>
               tells lisp to use the ISSR versions of those functions instead of the Hunchentoot versions.
             </p>
             <code-sample theme=theme >
<span class="rainbow-delimiters-depth-1">(</span>mapc <span class="extra">#'</span>ql:quickload <span class="extra">'</span><span class="rainbow-delimiters-depth-2">(</span>hunchenissr markup<span class="rainbow-delimiters-depth-2">)</span><span class="rainbow-delimiters-depth-1">)</span>
<span class="rainbow-delimiters-depth-1">(</span><span class="keyword">defpackage</span> <span class="type">backgammon</span>
  <span class="rainbow-delimiters-depth-2">(</span><span class="builtin">:use</span> <span class="extra">#</span>:<span class="doc">cl</span> <span class="extra">#</span>:<span class="doc">hunchentoot</span> <span class="extra">#</span>:<span class="doc">hunchenissr</span> <span class="extra">#</span>:<span class="doc">markup</span><span class="rainbow-delimiters-depth-2">)</span>
  <span class="rainbow-delimiters-depth-2">(</span><span class="builtin">:shadowing-import-from</span> <span class="extra">#</span>:<span class="doc">hunchenissr</span>
   <span class="builtin">:define-easy-handler</span>
   <span class="builtin">:start</span>
   <span class="builtin">:stop</span>
   <span class="builtin">:redirect</span><span class="rainbow-delimiters-depth-2">)</span><span class="rainbow-delimiters-depth-1">)</span></code-sample>
             <p>Next we need to enter the backgammon package namespace and enable the HTML/markup syntax.</p>
             <code-sample theme=theme >
<span class="rainbow-delimiters-depth-1">(</span><span class="keyword">in-package</span> <span class="extra">#</span>:<span class="doc">backgammon</span><span class="rainbow-delimiters-depth-1">)</span>
<span class="rainbow-delimiters-depth-1">(</span>markup:enable-reader<span class="rainbow-delimiters-depth-1">)</span></code-sample>
             <p>Since we are programming a backgammon game, we need to encode the rules of backgammon. I have already done this for you in
               <a href="http://raw.githubusercontent.com/interactive-ssr/backgammon-demo/master/backgammon.lisp">
                 <code>backgammon.lisp</code>
               </a>
               . It provides a backgammon game object that is fully functional, so whenever a dice is rolled or a piece (pip) is moved, it returns a fresh backgammon game object with the changes made instead of modifying an existing one. Just make sure it is in the same directory as our
               <code>index.lisp</code>
               and load it in.
             </p>
             <p>
               We will start out with just a single backgammon game and do multiplayer later in the tutorial. For now justs make a single
               <code>game</code>
               variable.
             </p>
             <code-sample theme=theme >
<span class="rainbow-delimiters-depth-1">(</span>load <span class="rainbow-delimiters-depth-2">(</span>make-pathname <span class="builtin">:name</span> <span class="string">"backgammon"</span> <span class="builtin">:type</span> <span class="string">"lisp"</span><span class="rainbow-delimiters-depth-2">)</span><span class="rainbow-delimiters-depth-1">)</span>

<span class="rainbow-delimiters-depth-1">(</span><span class="keyword">defparameter</span> <span class="variable-name">game</span> <span class="rainbow-delimiters-depth-2">(</span>make-instance <span class="extra">'</span>backgammon<span class="rainbow-delimiters-depth-2">)</span>
  <span class="doc">"The backgammon game object."</span><span class="rainbow-delimiters-depth-1">)</span></code-sample>
             <p>
               The final step of setting up is to start the HTTP server. This is almost the same process as with normal Hunchentoot. The difference is the
               <code>:ws-port</code>
               argument which is used by ISSR to start the web socket server. If you want to stop your server, you can execute
               <code>(stop server)</code>
               in the REPL and
               <code>(start server)</code>
               to start it again.
             </p>
             <code-sample theme=theme >
<span class="rainbow-delimiters-depth-1">(</span><span class="keyword">defparameter</span> <span class="variable-name">server</span>
  <span class="rainbow-delimiters-depth-2">(</span>start <span class="rainbow-delimiters-depth-3">(</span>make-instance <span class="extra">'</span>easy-acceptor
                        <span class="builtin">:port</span> 8080
                        <span class="builtin">:document-root</span> <span class="string">"resources/"</span><span class="rainbow-delimiters-depth-3">)</span>
         <span class="builtin">:ws-port</span> 4433<span class="rainbow-delimiters-depth-2">)</span><span class="rainbow-delimiters-depth-1">)</span></code-sample>
             <hr/>
             <h2>Backgammon Web Page</h2>
             <p>First, we will define a way for people to access the backgammon game from a browser,(progn ";") we will do this using
               <code>define-easy-handler</code>
               . This URL will have a single parameter
               (<code>action</code>)
               which we will use for user input in ISSR. This is a lot like defining a function named
               <code>backgammon</code>
               with a single parameter and will return an HTML string.
             </p>
             <p>
               The
               <code>action</code>
               parameter will be a stringified list where the first element of the list describes the action and the second element of the list is extra info about the action. For example, if the value of
               <code>action</code> is
               <code>"(pip 4)"</code>
               , it means that the user interacted with the pip in the 4<sup>th</sup> position.
               We will parse this string into two separate variables
               (<code>action</code> and <code>info</code>) using <code>read-from-string</code>
               . In case the user is messing with anything, we will use
               <code>handler-case</code> to catch any errors.
               Lastly we will have a <code>let</code> to define any variables we need during HTML generation.
             </p>
             <code-sample theme=theme >
<span class="rainbow-delimiters-depth-1">(</span><span class="keyword">define-easy-handler</span> <span class="rainbow-delimiters-depth-2">(</span>backgammon <span class="builtin">:uri</span> <span class="string">"/backgammon"</span><span class="rainbow-delimiters-depth-2">)</span>
    <span class="rainbow-delimiters-depth-2">(</span>action<span class="rainbow-delimiters-depth-2">)</span>
  <span class="rainbow-delimiters-depth-2">(</span><span class="keyword">multiple-value-bind</span> <span class="rainbow-delimiters-depth-3">(</span>action info<span class="rainbow-delimiters-depth-3">)</span>
      <span class="rainbow-delimiters-depth-3">(</span>values-list <span class="rainbow-delimiters-depth-4">(</span><span class="keyword">handler-case</span>
                       <span class="rainbow-delimiters-depth-5">(</span>read-from-string action<span class="rainbow-delimiters-depth-5">)</span>
                     <span class="rainbow-delimiters-depth-5">(</span>t <span class="rainbow-delimiters-depth-6">()</span> nil<span class="rainbow-delimiters-depth-5">)</span><span class="rainbow-delimiters-depth-4">)</span><span class="rainbow-delimiters-depth-3">)</span>
    <span class="comment-delimiter">,(progn ";;") </span><span class="comment">more Lisp goes here
</span>    <span class="rainbow-delimiters-depth-3">(</span><span class="keyword">let</span> <span class="rainbow-delimiters-depth-4">(</span><span class="comment">#|define variables here|#</span><span class="rainbow-delimiters-depth-4">)</span>
      <span class="rainbow-delimiters-depth-4">(</span>write-html
       ,(progn "<")<span class="function-name">html</span>,(progn ">")
         ,(progn "<")<span class="function-name">head</span>,(progn ">")
         ,(progn "<")/<span class="function-name">head</span>,(progn ">")
         ,(progn "<")<span class="function-name">body</span>,(progn ">")
           ,(progn "<")<span class="function-name">h1</span>,(progn ">")Backgammon,(progn "<")/<span class="function-name">h1</span>,(progn ">")
           <span class="comment-delimiter">,(progn "<!--") </span><span class="comment">more HTML goes here ,(progn "-->") 
</span>         ,(progn "<")/<span class="function-name">body</span>,(progn ">")
       ,(progn "<")/<span class="function-name">html</span>,(progn ">")<span class="rainbow-delimiters-depth-4">)</span><span class="rainbow-delimiters-depth-3">)</span><span class="rainbow-delimiters-depth-2">)</span><span class="rainbow-delimiters-depth-1">)</span></code-sample>
             <p>
               Now if you evaluate all the code so far and visit 
               <a href="http://localhost:8080/backgammon">localhost:8080/backgammon</a>
               , you should see the header <q>backgammon</q>.
             </p>
             <p>
               To enable ISSR on this web page, we need to load the ISSR client code:
               <code>issr.js</code>
               , and call the Javascript setup function with parameters
               <code>*id*</code> and <code>*ws-port*</code>. The
               <code>noupdate</code>
               attribute was invinted by ISSR,(progn ";") it ensures that even if something is updated on the server, the client doens't get updated. In this case the server is going to update
               <code>*id*</code>
               , but we don't want the client trying to connect when it is already connected.
               
               While we are in the <code>head</code> section, let's load some CSS too. 
             </p>
             <code-sample theme=theme >
,(progn "<")<span class="function-name">head</span>,(progn ">")
  ,(progn "<")<span class="function-name">link</span> <span class="constant">href</span>=<span class="string">"backgammon.css"</span> <span class="constant">rel</span>=<span class="string">"stylesheet"</span>/,(progn ">")
  ,(progn "<")<span class="function-name">link</span> <span class="constant">href</span>=<span class="string">"site.css"</span> <span class="constant">rel</span>=<span class="string">"stylesheet"</span>/,(progn ">")
  ,(progn "<")<span class="function-name">script</span> <span class="constant">src</span>=<span class="string">"issr.js"</span>,(progn ">"),(progn "<")/<span class="function-name">script</span>,(progn ">")
  ,(progn "<")<span class="function-name">script</span> <span class="constant">noupdate</span>=t ,(progn ">")
    <span class="extra">,</span><span class="rainbow-delimiters-depth-1">(</span>format nil <span class="string">"setup(~a,~a)"</span> *id* *ws-port*<span class="rainbow-delimiters-depth-1">)</span>
  ,(progn "<")/<span class="function-name">script</span>,(progn ">")
  ,(progn "<")<span class="function-name">title</span>,(progn ">")Backgammon -- ISSR,(progn "<")/<span class="function-name">title</span>,(progn ">")
,(progn "<")/<span class="function-name">head</span>,(progn ">")</code-sample>
             <h3>Roll Dice</h3>
             <p>
               We need a button to roll the dice. The
               <code>name</code> attribute will be
               <code>"action"</code>so that the
               <code>value</code> attribute of this button will be the value of the
               <code>action</code> variable. The <code>value</code> attribute will be
               <code>"(roll)"</code> because we only need to know that the user interactied with the roll dice button, no special information needed.
               Finally, we want the button to be disabled if there are still moves to be made before the next roll. For this we will use the
               <code>can-move-<strong>p</strong></code> <strong style="padding-left: .5ch;">p</strong>redicate from
               <code>backgammon.lisp</code> to tell if the player has any moves left.
             </p>
             <code-sample theme=theme >
,(progn "<")<span class="function-name">button</span> <span class="constant">name</span>=<span class="string">"action"</span> <span class="constant">value</span>=<span class="string">"(roll)"</span>
        <span class="constant">onclick</span>=<span class="string">"rr(this)"</span>
        <span class="constant">disabled</span>=<span class="rainbow-delimiters-depth-1">(</span>can-move-p game<span class="rainbow-delimiters-depth-1">)</span>,(progn ">")
  Roll Dice
,(progn "<")/<span class="function-name">button</span>,(progn ">")</code-sample>
             <p>
               The <code>rr</code> function in the 
               <code>onclick</code> attribute will populate the <code>action</code> parameter, so we need to do something with that value.
               In the Lisp section, we will check if the <code>action</code> variable has the value <q>roll</q>, and if it does, roll the dice in the backgammon game object.
               Remember, when we use the <code>roll-dice</code> function on our backgammon game object, it will return a new backgamon game object with the rolled dice, so we need to set the
               <code>game</code> to the result of <code>roll-dice</code>.
             </p>
             <code-sample theme=theme >
<span class="rainbow-delimiters-depth-1">(</span><span class="keyword">when</span> <span class="rainbow-delimiters-depth-2">(</span>string= action <span class="extra">'</span>roll<span class="rainbow-delimiters-depth-2">)</span>
  <span class="rainbow-delimiters-depth-2">(</span>setq game <span class="rainbow-delimiters-depth-3">(</span>roll-dice game<span class="rainbow-delimiters-depth-3">)</span><span class="rainbow-delimiters-depth-2">)</span><span class="rainbow-delimiters-depth-1">)</span></code-sample>
             <p>You should now see a roll dice button your web page, and when you click it, the button should become disabled.</p>
             <hr/>
             <h3>Backgammon Board</h3>
             <p>
               For the board,
               <a>
                 <code>backgammon.css</code>
               </a>
               provides styling for serveral custom SGML tags that we will make out board out of. Since these tags are not
               <q>valid</q> HTML, we will use a colon before the tag name to tell the
               <code>markup</code> package that we are sure we want to use them.
             </p>
           </body>))
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
         <title>Backgammon Tutorial -- ISSR</title>
       </head>
       ,(progn body)
     </html>)))
