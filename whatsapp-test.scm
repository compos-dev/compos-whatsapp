;;; whatsapp-test.scm --- WhatsApp list and conversation policy.

(package! "whatsapp-test")

(domain! 'diagnostics)
(effects! '(read))

(define t--whatsapp-chat-stream
  (string-append
    "{\"jid\":\"123@s.whatsapp.net\",\"name\":\"Mukund\","
    "\"last_message_time\":\"2026-09-05T11:49:38+05:30\","
    "\"last_message\":\"line one\\nline two\",\"last_is_from_me\":1}\n"
    "{\"jid\":\"456@g.us\",\"name\":\"Team\","
    "\"last_message_time\":\"2026-09-05T10:00:00+05:30\","
    "\"last_message\":\"hello\",\"last_is_from_me\":0}"))

(define t--whatsapp-message-stream
  (string-append
    "[2026-09-05 11:49:38] Chat: Mukund From: Me: line one\n"
    "line two\n"
    "[2026-09-05 11:48:00] Chat: Mukund From: Mukund: "
    "[image - Message ID: ABC - Chat JID: 123@s.whatsapp.net] \n"))

;; a conversation buffer holding TEXT, in the mode that reads it
(define (t--whatsapp-conversation text)
  (let ((buf "*zz-whatsapp-conversation*"))
    (unless (buffer-known? buf) (buffer-create buf))
    (buffer-set-local! buf 'whatsapp-jid #f)
    (buffer-set-local! buf 'whatsapp-name "Mukund")
    (with-current-buffer buf (lambda () (set-mode! "whatsapp-chat-mode")))
    (whatsapp--render-conversation! buf text)
    buf))

(deftest 'whatsapp-parses-chat-stream-and-flattens-previews
  "The chat stream becomes rows with one-line previews."
  (lambda ()
    (let* ((rows (whatsapp--parse-chats t--whatsapp-chat-stream))
           (cells (whatsapp--cells "*WhatsApp*" (car rows))))
      (check-equal! (length rows) 2 "both JSON objects become rows")
      (check-equal! (car cells) "Mukund" "the chat name is the first cell")
      (check-equal! (caddr cells) "me: line one line two"
                    "the preview is one line and marks my message"))))

(deftest 'whatsapp-parses-one-chat-object
  "One JSON object becomes one selectable chat row."
  (lambda ()
    (let ((rows
            (whatsapp--parse-chats
              "{\"jid\":\"123@s.whatsapp.net\",\"name\":\"Mukund\"}")))
      (check-equal! (length rows) 1 "one object still becomes one row")
      (check-equal! (plist-get (car rows) 'jid) "123@s.whatsapp.net"
                    "the JID stays attached to the row"))))

(deftest 'whatsapp-reads-the-transcript-as-a-tree
  "The grammar finds the messages; nothing here scans lines."
  (lambda ()
    (let* ((buf (t--whatsapp-conversation t--whatsapp-message-stream))
           (rows (whatsapp--messages buf))
           (first (car rows))
           (second (cadr rows)))
      (check-equal! (buffer-text buf) t--whatsapp-message-stream
                    "the transcript reaches the buffer as it came")
      (check-equal! (length rows) 2 "both messages become nodes")
      (check-equal! (whatsapp--msg-body first) "line one\nline two"
                    "continuation lines stay with their message")
      (check-true! (whatsapp--msg-mine? first)
                   "messages from Me are identified")
      (check-equal! (whatsapp--msg-sender second) "Mukund"
                    "the sender stays visible")
      (check-equal! (whatsapp--msg-body second) "[image]"
                    "media rows hide transport identifiers"))))

(deftest 'whatsapp-moves-between-messages-and-marks-the-one-at-point
  "Up and down are the tree's siblings, and the view says where you are."
  (lambda ()
    (let* ((buf (t--whatsapp-conversation t--whatsapp-message-stream))
           (rows (whatsapp--messages buf)))
      (whatsapp--select-at! buf (whatsapp--msg-start (car rows)))
      (check-equal! (whatsapp--msg-sender (whatsapp--current-message buf))
                    "Me" "the message at point is the one point stands in")
      (whatsapp--goto-message! buf 'next)
      (check-equal! (buffer-point buf) (whatsapp--msg-start (cadr rows))
                    "next lands on the next message's own start")
      (check-contains! (value->string (buffer-local-value 'render-blocks buf))
                       "whatsapp-message-current"
                       "the view marks the message at point")
      (whatsapp--goto-message! buf 'next)
      (check-equal! (buffer-point buf) (whatsapp--msg-start (cadr rows))
                    "the last message is where next stops")
      (whatsapp--goto-message! buf 'prev)
      (check-equal! (buffer-point buf) (whatsapp--msg-start (car rows))
                    "prev walks back the same way"))))

(deftest 'whatsapp-reply-quotes-the-message-it-answers
  "The transport has no reply-to field, so the quote goes in the text."
  (lambda ()
    (let ((rows (whatsapp--messages
                  (t--whatsapp-conversation t--whatsapp-message-stream))))
      (check-equal! (whatsapp--quote (car rows))
                    "> Me: line one line two\n"
                    "the quote is one line, attributed"))))

(deftest 'whatsapp-conversation-shows-actions-and-empty-state
  "The conversation uses catalogued actions and an empty state."
  (lambda ()
    (let* ((buf (t--whatsapp-conversation "Could not load messages\n"))
           (blocks (buffer-local-value 'render-blocks buf)))
      (check-equal! (length blocks) 2
                    "actions and the empty state render")
      (check-contains! (value->string blocks) "Reply"
                       "the reply action is visible")
      (check-contains! (value->string blocks) "Refresh"
                       "the refresh action is visible")
      (check-contains! (value->string blocks) "Could not load messages"
                       "what the server said is what the reader sees"))))


(deftest 'whatsapp-fills-a-chat-from-its-newest-transcript
  "A chat the list left blank takes its name and preview from its newest message."
  (lambda ()
    (let* ((parts (whatsapp--transcript-first t--whatsapp-message-stream))
           (filled (whatsapp--chat-with-last-message
                     (list 'jid "123@s.whatsapp.net" 'name ""
                           'last_message #f 'last_is_from_me #f)
                     parts)))
      (check-equal! (car parts) "Mukund" "the transcript names the chat")
      (check-equal! (cadr parts) "Me" "the transcript names the sender")
      (check-equal! (caddr parts) "line one"
                    "the preview is the newest message's first line")
      (check-equal! (plist-get filled 'name) "Mukund"
                    "the empty name is filled")
      (check-equal! (plist-get filled 'last_message) "line one"
                    "the empty preview is filled")
      (check-equal! (plist-get filled 'last_is_from_me) 1
                    "a message from Me is marked as mine"))))

(deftest 'whatsapp-keeps-a-chat-field-the-list-already-has
  "A name and preview the list gave are never replaced."
  (lambda ()
    (let ((filled (whatsapp--chat-with-last-message
                    (list 'jid "123@s.whatsapp.net" 'name "Mukund"
                          'last_message "hello" 'last_is_from_me 0)
                    (list "Other" "Me" "bye"))))
      (check-equal! (plist-get filled 'name) "Mukund"
                    "the list name stays")
      (check-equal! (plist-get filled 'last_message) "hello"
                    "the list preview stays")
      (check-equal! (plist-get filled 'last_is_from_me) 0
                    "the list sender stays"))))

(deftest 'whatsapp-transcript-first-rejects-a-notice
  "A loading notice is not a transcript."
  (lambda ()
    (check-equal! (whatsapp--transcript-first "Loading messages…\n") #f
                  "a notice gives no message parts")
    (check-equal! (whatsapp--transcript-first "") #f
                  "an empty transcript gives no message parts")))


;;; --- the app, over a held MCP seam ----------------------------------------------
;;; These were whatsapp_test.exs: the list, the conversation, refresh and
;;; reply, by the commands, with no traffic.

(define t--wa-list "*WhatsApp*")
(define t--wa-chat "*WhatsApp conversation*")
(define t--wa-list-fetches 0)
(define t--wa-message-fetches 0)
(define t--wa-sent #f)

(define (t--wa-call server tool args &optional cb)
  (cond
    ((equal? tool "list_chats")
     (set! t--wa-list-fetches (+ t--wa-list-fetches 1))
     "{\"jid\":\"123@s.whatsapp.net\",\"name\":\"Mukund Jha Jha\",\"last_message_time\":\"2026-09-05T11:49:38+05:30\",\"last_message\":\"it's the weekend\",\"last_is_from_me\":1}")
    ((equal? tool "list_messages")
     (set! t--wa-message-fetches (+ t--wa-message-fetches 1))
     (let ((text "[2026-09-05 11:49:38] Chat: Mukund From: Me: it's the weekend\n"))
       (if cb (cb #t text) text)))
    ((equal? tool *whatsapp-reply-tool*)
     (set! t--wa-sent args)
     (if cb (cb #t "{\"success\":true}") "{\"success\":true}"))
    (else (if cb (cb #f "unknown tool") ""))))

(define (t--wa-kill!)
  (when (minibuffer-active?) (minibuffer-cancel!))
  (for-each (lambda (b) (when (buffer-known? b) (buffer-kill! b))) (list t--wa-list t--wa-chat)))

(define (t--wa-drop-group! name)
  (let ((id (group-resolve-id name))) (when id (group-record-delete! id))))

;; a frame of its own, the fake seam, and the destination group; all go back
(define (t--wa! group thunk)
  (let ((saved-call *whatsapp-call*))
    (t--wa-kill!)
    (set! t--wa-list-fetches 0)
    (set! t--wa-message-fetches 0)
    (set! t--wa-sent #f)
    (set! *whatsapp-call* t--wa-call)
    (with-custom 'whatsapp-group group
      (lambda () (with-frame (lambda (frame) (thunk)))))
    (set! *whatsapp-call* saved-call)
    (t--wa-kill!)
    (t--wa-drop-group! "whatsapp")))

(define (t--wa-open-chat!)
  (run-command "whatsapp")
  (list-goto-first-entry t--wa-list)
  (with-current-buffer t--wa-list (lambda () (run-command "whatsapp-open"))))

(define (t--wa-reply! buf text)
  (with-current-buffer buf (lambda () (run-command "whatsapp-reply")))
  (minibuffer-input! text)
  (minibuffer-confirm-input!))

(deftest 'whatsapp-app-buffers-can-fill-a-group-layout
  "the list and the conversation are layout candidates"
  (lambda ()
    (t--wa! "whatsapp"
      (lambda ()
        (t--wa-open-chat!)
        (check-true! (fill-candidate? t--wa-chat) "the conversation")
        (check-true! (fill-candidate? t--wa-list) "the list")))))

(deftest 'whatsapp-named-destination-is-selected-before-the-buffers
  "whatsapp-group names the group the scene opens in"
  (lambda ()
    (t--wa! "zz-whatsapp-destination"
      (lambda ()
        (run-command "whatsapp")
        (check-equal! (group-name (frame-group)) "zz-whatsapp-destination" "the frame moves there")
        (check-true! (buffer-in-group? t--wa-list "zz-whatsapp-destination") "the list")
        (check-true! (buffer-in-group? t--wa-chat "zz-whatsapp-destination") "the conversation")))
    (t--wa-drop-group! "zz-whatsapp-destination")))

(deftest 'whatsapp-filter-narrows-the-chats-locally
  "the scene puts the list in the index and the conversation in show; a query filters"
  (lambda ()
    (t--wa! "whatsapp"
      (lambda ()
        (run-command "whatsapp")
        (check-equal! (group-name (frame-group)) "whatsapp" "the app's group")
        (check-equal! (window-buffer (scene-window 'index)) t--wa-list "the index")
        (check-equal! (window-buffer (scene-window 'show)) t--wa-chat "the show pane")
        (list-set-query! t--wa-list "jha")
        (check-contains! (buffer-text t--wa-list) "/jha   1 of 1" "one match")
        (check-contains! (buffer-text t--wa-list) "Mukund Jha Jha" "the chat")
        (list-set-query! t--wa-list "missing")
        (check-contains! (buffer-text t--wa-list) "/missing   0 of 1" "no match")
        (check-false! (string-contains? (buffer-text t--wa-list) "Mukund Jha Jha") "the chat is hidden")))))

(deftest 'whatsapp-open-shows-the-transcript-and-refresh-fetches-again
  "whatsapp-open reads the transcript as blocks; whatsapp-chat-refresh fetches it again"
  (lambda ()
    (t--wa! "whatsapp"
      (lambda ()
        (t--wa-open-chat!)
        (check-equal! (group-name (buffer-group t--wa-chat)) "whatsapp" "in the app's group")
        (check-contains! (buffer-text t--wa-list) "me: it's the weekend" "the list previews")
        (check-contains! (buffer-text t--wa-chat) "[2026-09-05 11:49:38] Chat: Mukund From: Me:"
                         "the transcript as the server printed it")
        (check-equal! (map whatsapp--msg-sender (whatsapp--messages t--wa-chat)) '("Me") "the sender")
        (check-equal! (map whatsapp--msg-body (whatsapp--messages t--wa-chat)) '("it's the weekend") "the body")
        (check-equal! (buffer-local-value 'render-mode t--wa-chat) "blocks" "drawn as blocks")
        (check-equal! (length (buffer-local-value 'render-blocks t--wa-chat)) 2 "two blocks")
        (check-equal! (buffer-local-value 'whatsapp-jid t--wa-chat) "123@s.whatsapp.net" "the chat it is")
        (check-equal! (window-buffer (scene-window 'show)) t--wa-chat "in the show pane")
        (check-equal! t--wa-message-fetches 1 "one fetch")
        (with-current-buffer t--wa-chat (lambda () (run-command "whatsapp-chat-refresh")))
        (check-equal! t--wa-message-fetches 2 "refresh fetches again")))))

(deftest 'whatsapp-reply-from-the-index-goes-to-the-selected-chat
  "whatsapp-reply on the list sends to the row's chat without opening it"
  (lambda ()
    (t--wa! "whatsapp"
      (lambda ()
        (run-command "whatsapp")
        (list-goto-first-entry t--wa-list)
        (t--wa-reply! t--wa-list "From the index")
        (check-contains! (value->string t--wa-sent) "recipient \"123@s.whatsapp.net\"" "to the chat")
        (check-contains! (value->string t--wa-sent) "message \"From the index\"" "the text")
        (check-equal! t--wa-message-fetches 0 "no transcript fetched")))))

(deftest 'whatsapp-reply-in-a-conversation-quotes-the-message-and-refreshes
  "the one message on screen is the one answered; the transport has no reply-to"
  (lambda ()
    (t--wa! "whatsapp"
      (lambda ()
        (t--wa-open-chat!)
        (t--wa-reply! t--wa-chat "On my way")
        (check-contains! (value->string t--wa-sent) "recipient \"123@s.whatsapp.net\"" "to the chat")
        (check-contains! (value->string t--wa-sent) "> Me: it's the weekend" "the quote")
        (check-contains! (value->string t--wa-sent) "On my way" "the text")
        (check-equal! t--wa-message-fetches 2 "the send refreshes the transcript")))))

(deftest 'whatsapp-messages-are-the-unit-of-motion-and-a-reply-quotes-the-selected
  "next and prev walk messages; the reply quotes the one point is on"
  (lambda ()
    (t--wa! "whatsapp"
      (lambda ()
        (t--wa-open-chat!)
        (whatsapp--render-conversation! t--wa-chat
          (string-append
            "[2026-09-05 11:48:00] Chat: Mukund From: Mukund: are you around\n"
            "[2026-09-05 11:49:38] Chat: Mukund From: Me: it's the weekend\n"))
        (check-equal! (length (whatsapp--messages t--wa-chat)) 2 "two messages")
        (check-equal! (whatsapp--msg-sender (whatsapp--current-message t--wa-chat)) "Mukund" "the top one")
        (with-current-buffer t--wa-chat (lambda () (run-command "whatsapp-next-message")))
        (check-equal! (whatsapp--msg-sender (whatsapp--current-message t--wa-chat)) "Me" "next")
        (with-current-buffer t--wa-chat (lambda () (run-command "whatsapp-prev-message")))
        (check-equal! (whatsapp--msg-sender (whatsapp--current-message t--wa-chat)) "Mukund" "prev")
        (check-contains! (value->string (buffer-local-value 'render-blocks t--wa-chat))
                         "whatsapp-message-current" "the current one is marked")
        (t--wa-reply! t--wa-chat "yes")
        (check-contains! (value->string t--wa-sent) "> Mukund: are you around" "the quote")
        (check-contains! (value->string t--wa-sent) "yes" "the text")))))

(deftest 'whatsapp-a-late-answer-cannot-overwrite-a-newer-chat
  "a transcript for the old jid is dropped when the buffer moved on"
  (lambda ()
    (t--wa! "whatsapp"
      (lambda ()
        (let ((late #f))
          (buffer-create t--wa-chat)
          (buffer-set-local! t--wa-chat 'whatsapp-jid "old@lid")
          (set! *whatsapp-call* (lambda (server tool args &optional cb) (set! late cb) ""))
          (whatsapp--refresh-conversation! t--wa-chat)
          (buffer-set-local! t--wa-chat 'whatsapp-jid "new@lid")
          (late #t "[2026-09-05 20:00:00] Chat: Old From: Old: stale response\n")
          (check-false! (string-contains? (buffer-text t--wa-chat) "stale response") "no stale text")
          (check-equal! (buffer-local-value 'whatsapp-jid t--wa-chat) "new@lid" "the new chat stays"))))))

(deftest 'whatsapp-mode-setup-keeps-the-restored-conversation
  "set-mode! again keeps the jid, so refresh still fetches the chat"
  (lambda ()
    (t--wa! "whatsapp"
      (lambda ()
        (t--wa-open-chat!)
        (with-current-buffer t--wa-chat (lambda () (set-mode! "whatsapp-chat-mode")))
        (with-current-buffer t--wa-chat (lambda () (run-command "whatsapp-chat-refresh")))
        (check-equal! t--wa-message-fetches 2 "refresh fetched")
        (check-contains! (buffer-text t--wa-chat) "it's the weekend" "the transcript")))))
