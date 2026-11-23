;;; whatsapp.scm --- Read and reply to WhatsApp chats through MCP.

(require 'mcp)
(require 'tabulated-list)
(require 'groups)
(require 'events)
(require 'chat-surface)

(package! "whatsapp")

(domain! 'chat)
(effects! '(write))

(defgroup 'whatsapp "Read and reply to WhatsApp chats through the whatsapp MCP server.")

;; Seconds before the chat list refreshes from WhatsApp.
(define whatsapp-cache-ttl 30)

;; Maximum recent chats to show.
(define whatsapp-chat-limit 30)

;; Maximum recent messages to show in one conversation.
(define whatsapp-message-limit 50)

(define *whatsapp-buffer* "*WhatsApp*")
(defcustom 'whatsapp-group "*WhatsApp*"
  "Group to open WhatsApp in. Uses the exact *WhatsApp* group."
  'group 'whatsapp 'type 'string)
(define *whatsapp-show-buffer* "*WhatsApp conversation*")

(define (whatsapp--join-group! buf)
  (let ((group (frame-group)))
    (when group (buffer-add-group! buf group)))
  buf)
(define (whatsapp--stale-session? text)
  (and (string? text) (string-contains? text "http 404")))

(define (whatsapp--reconnect! server)
  (mcp-disconnect! (symbol->string server))
  (mcp-ensure! server))

;; The server on the bridge's host forgets its sessions when it restarts,
;; and answers the old one 404. A call that meets that connects again and
;; tries once more, so a restart there never leaves WhatsApp broken here.
(define (whatsapp--mcp-call server tool args &optional cb)
  (if cb
      (mcp-call! server tool args
        (lambda (ok text)
          (if (and (not ok) (whatsapp--stale-session? text))
              (begin (whatsapp--reconnect! server)
                     (mcp-call! server tool args cb))
              (cb ok text))))
      (let ((first (ignore-errors (lambda () (mcp-call! server tool args)))))
        (if (and first (not (whatsapp--stale-session? first)))
            first
            (begin (whatsapp--reconnect! server)
                   (mcp-call! server tool args))))))

(define *whatsapp-call* whatsapp--mcp-call)
(effects! '(write external))
(public! '*whatsapp-call* "(*whatsapp-call* SERVER TOOL ARGS [CB]) — the MCP call that WhatsApp uses; tests set it to a stub")

(domain! 'chat)
(effects! '(pure))

(define (whatsapp--text value fallback)
  (if (and (string? value) (not (equal? value ""))) value fallback))

(define (whatsapp--one-line value)
  (string-join (string-split (whatsapp--text value "") "\n") " "))

;; The MCP chat list sometimes leaves a chat's name and last message empty
;; while its transcript holds both. These read the newest line of a
;; transcript, "[TIME] Chat: NAME From: SENDER: BODY", and copy the
;; missing fields onto the chat.
(define (whatsapp--transcript-first text)
  "(whatsapp--transcript-first TEXT) -- (NAME SENDER BODY) of the newest message in a transcript, or #f"
  (and (string? text)
       (not (equal? (string-trim text) ""))
       (let* ((line (car (string-split text "\n")))
              (head (string-split line "] Chat: ")))
         (and (pair? (cdr head))
              (let* ((rest (string-join (cdr head) "] Chat: "))
                     (halves (string-split rest " From: ")))
                (and (pair? (cdr halves))
                     (let* ((name (string-trim (car halves)))
                            (body (string-split
                                    (string-join (cdr halves) " From: ") ": ")))
                       (and (pair? (cdr body))
                            (list name
                                  (string-trim (car body))
                                  (string-join (cdr body) ": "))))))))))

(define (whatsapp--chat-with-last-message chat parts)
  "(whatsapp--chat-with-last-message CHAT PARTS) -- CHAT with the name, message and sender its transcript gives where they were empty"
  (if (not parts)
      chat
      (let* ((name (car parts))
             (sender (cadr parts))
             (body (caddr parts))
             (chat (if (whatsapp--text (plist-get chat 'name) #f) chat
                       (plist-put chat 'name name))))
        (if (plist-get chat 'last_message)
            chat
            (plist-put
              (plist-put chat 'last_message body)
              'last_is_from_me (if (equal? sender "Me") 1 0))))))

(define (whatsapp--list-time stamp)
  "(whatsapp--list-time STAMP) -- a chat's last activity as YYYY-MM-DD HH:MM"
  (let ((s (whatsapp--text stamp "")))
    (if (> (string-byte-length s) 16)
        (string-replace (substring-bytes s 0 16) "T" " ")
        s)))

(define (whatsapp--parse-chats text)
  (if (not (string? text))
      #f
      (let* ((trimmed (string-trim text))
             (direct (and (not (equal? trimmed "")) (json-parse trimmed))))
        (cond
          ((equal? trimmed "") '())
          ((and (pair? direct) (symbol? (car direct))) (list direct))
          ((or (pair? direct) (null? direct)) direct)
          (else
            (json-parse
              (string-append
                "["
                (string-join (string-split trimmed "}\n{") "},{")
                "]")))))))

(define (whatsapp--from-me? chat)
  (let ((value (plist-get chat 'last_is_from_me)))
    (or (equal? value 1) (equal? value #t))))

(define (whatsapp--cells buf chat)
  (let ((jid (whatsapp--text (plist-get chat 'jid) "unknown")))
    (list
      (whatsapp--text (plist-get chat 'name) jid)
      (whatsapp--list-time (plist-get chat 'last_message_time))
      (string-append
        (if (whatsapp--from-me? chat) "me: " "")
        (whatsapp--one-line (plist-get chat 'last_message))))))

(define whatsapp--reactions-pattern "\\s*\\[reactions: ([^\\]]*)\\]\\s*$")

(define (whatsapp--strip-reactions body)
  "BODY without the reactions the server writes after it"
  (let ((at (re-find whatsapp--reactions-pattern body 0)))
    (if at (substring body 0 (car at)) body)))

(define (whatsapp--clean-message-body body)
  (let ((body (whatsapp--strip-reactions body)))
    (cond
      ((string-prefix? "[image - Message ID:" body) "📷 Photo")
      ((string-prefix? "[video - Message ID:" body) "[video]")
      ((string-prefix? "[audio - Message ID:" body) "[audio]")
      ((string-prefix? "[document - Message ID:" body) "📄 Document")
      (else body))))

;;; The conversation as a tree.
;;;
;;; The transcript the server prints is the source, and it reaches the
;;; buffer as it came. The whatsapp grammar parses it; the rows, the
;;; view, and the motion between messages are three readings of that
;;; one parse. Nothing here scans lines.

(define (whatsapp--node-of kind nodes)
  (let loop ((ns nodes))
    (cond ((null? ns) #f)
          ((equal? (car (car ns)) kind) (car ns))
          (else (loop (cdr ns))))))

(define (whatsapp--slice text node)
  (if node (substring-bytes text (nth 1 node) (nth 2 node)) ""))

(define (whatsapp--msg-start row) (nth 0 row))
(define (whatsapp--msg-end row) (nth 1 row))
(define (whatsapp--msg-time row) (nth 2 row))
(define (whatsapp--msg-sender row) (nth 3 row))
(define (whatsapp--msg-body row) (nth 4 row))
(define (whatsapp--msg-mine? row) (nth 5 row))
(define (whatsapp--msg-raw row) (if (> (length row) 6) (nth 6 row) ""))
(define (whatsapp--msg-reactions row)
  "the reactions on ROW as the server lists them, as \"👍 Shouvik, ❤️ Me\", else #f"
  (let ((m (re-match whatsapp--reactions-pattern (whatsapp--msg-raw row))))
    (and m (not (equal? (string-trim (nth 1 m)) "")) (string-trim (nth 1 m)))))

(define whatsapp-media-directory
  (string-append (compos-cache-dir) "/whatsapp-media"))

(define (whatsapp--media-ref row)
  "(KIND ID JID) of a photo or document message, from the line the server wrote for it, else #f"
  (let ((m (re-match "^\\[(image|document) - Message ID: ([^ ]+) - Chat JID: ([^\\]]+)\\]"
                     (whatsapp--msg-raw row))))
    (and m (list (nth 1 m) (nth 2 m) (string-trim (nth 3 m))))))

(define (whatsapp--photo-ref row)
  "(ID JID) of a photo message, else #f"
  (let ((ref (whatsapp--media-ref row)))
    (and ref (equal? (car ref) "image") (cdr ref))))

(define (whatsapp--doc-ref row)
  "(ID JID) of a document message, else #f"
  (let ((ref (whatsapp--media-ref row)))
    (and ref (equal? (car ref) "document") (cdr ref))))

;; a document keeps a folder of its own: the file under its own name, and
;; preview.png, the first page as Quick Look draws it
(define (whatsapp--doc-dir id)
  (string-append whatsapp-media-directory "/" id))

(define (whatsapp--doc-file row)
  "the document of ROW as a local file once it is fetched, else #f"
  (let* ((ref (whatsapp--doc-ref row))
         (dir (and ref (whatsapp--doc-dir (car ref)))))
    (and dir (file-directory? dir)
         (let ((names (filter (lambda (f) (not (equal? f "preview.png")))
                              (directory-files dir))))
           (and (pair? names) (string-append dir "/" (car names)))))))

(define (whatsapp--doc-preview row)
  "the preview image of ROW's document once it is drawn, else #f"
  (let ((ref (whatsapp--doc-ref row)))
    (and ref
         (let ((f (string-append (whatsapp--doc-dir (car ref)) "/preview.png")))
           (and (file-exists? f) f)))))

(define (whatsapp--media-local row)
  "the photo or document of ROW as a local file, else #f"
  (or (whatsapp--photo-local row) (whatsapp--doc-file row)))

(define (whatsapp--body-shown row)
  "what a bubble says: a fetched document by its file name"
  (let ((doc (whatsapp--doc-file row)))
    (if doc
        (string-append "📄 " (file-name-nondirectory doc))
        (whatsapp--msg-body row))))

(define (whatsapp--sq s)
  (string-append "'" (string-replace s "'" "'\\''") "'"))

(define (whatsapp--photo-file id)
  (string-append whatsapp-media-directory "/" id ".jpg"))

(define (whatsapp--photo-local row)
  "the photo of ROW as a local file once it is fetched, else #f"
  (let ((ref (whatsapp--photo-ref row)))
    (and ref
         (let ((f (whatsapp--photo-file (car ref))))
           (and (file-exists? f) f)))))

;; every message in BUF as (START END TIME SENDER BODY MINE?)
(define (whatsapp--messages buf)
  (let ((text (buffer-text buf)))
    (with-current-buffer buf
      (lambda ()
        (map
          (lambda (node)
            (let* ((parts (ts-children "message" (nth 1 node) (nth 2 node)))
                   (sender
                     (whatsapp--slice text
                       (whatsapp--node-of "sender" parts))))
              (list
                (nth 1 node) (nth 2 node)
                (whatsapp--slice text
                  (whatsapp--node-of "timestamp" parts))
                sender
                (whatsapp--clean-message-body
                  (string-trim
                    (whatsapp--slice text
                      (whatsapp--node-of "body" parts))))
                (equal? sender "Me")
                ;; the body as the server wrote it: a photo's line names
                ;; the message id its file is fetched by
                (string-trim
                  (whatsapp--slice text
                    (whatsapp--node-of "body" parts))))))
          ;; Name the root. One message covers the whole buffer on its
          ;; own, and the smallest node over that range would then be
          ;; the message, not the conversation holding it.
          ;;
          ;; A loading notice or an error is not a transcript, and the
          ;; grammar says so by giving back an error node instead.
          (filter (lambda (node) (equal? (car node) "message"))
                  (ts-children "conversation" 0
                               (string-byte-length text))))))))

;; The message a position stands in. A position past the last one
;; belongs to it: a reader at the end of the buffer is reading the
;; last thing said.
(define (whatsapp--message-at rows pos)
  (let loop ((rs rows) (best #f))
    (cond ((null? rs) best)
          ((> (whatsapp--msg-start (car rs)) pos) best)
          (else (loop (cdr rs) (car rs))))))

(define (whatsapp--current-message buf)
  (and (buffer-derived-mode? buf "whatsapp-chat-mode")
       (whatsapp--message-at (whatsapp--messages buf)
                             (buffer-point buf))))





(define (whatsapp--short-time stamp)
  (if (> (string-byte-length stamp) 15)
      (substring-bytes stamp 5 16)
      stamp))

(define (whatsapp--message-block row current?)
  (let ((mine? (whatsapp--msg-mine? row)))
    (component 'ui/row
      (list
        'class
          (string-append
            "whatsapp-message"
            (if mine? " whatsapp-message-me" "")
            (if current? " whatsapp-message-current" ""))
        ;; the node's own start names the row, so a click selects the
        ;; message the same motion keys would
        'click
          (string-append "msg:"
                         (number->string (whatsapp--msg-start row)))
        ;; a bubble reads top to bottom: the sender (theirs only), the
        ;; body, then the time tucked under it on the right
        'segs
          (append
            (if mine?
                '()
                (list (list "whatsapp-message-sender"
                            (whatsapp--msg-sender row))))
            (list
              (list "whatsapp-message-body" (whatsapp--body-shown row)))
            (let ((r (whatsapp--msg-reactions row)))
              (if r (list (list "whatsapp-message-reactions" r)) '()))
            (list
              (list "whatsapp-message-time"
                    (whatsapp--short-time (whatsapp--msg-time row)))))))))

;; WhatsApp's own reply carries the message it answers. This transport
;; has no field for one, so the quote goes into the text — the reader
;; on the other end still sees what was answered.
(define (whatsapp--quote row)
  (let loop ((words (string-split
                      (whatsapp--one-line (whatsapp--msg-body row)) " "))
             (kept '()) (width 0))
    (if (or (null? words) (> width 120))
        (string-append
          "> " (whatsapp--msg-sender row) ": "
          (string-join (reverse kept) " ")
          (if (null? words) "" " …")
          "\n")
        (loop (cdr words) (cons (car words) kept)
              (+ width 1 (string-byte-length (car words)))))))

(define (whatsapp--day-of stamp)
  (if (>= (string-byte-length stamp) 10)
      (substring-bytes stamp 0 10)
      ""))

(define (whatsapp--conversation-items rows current notice)
  (if (pair? rows)
      ;; a day pill goes in wherever the day changes, as Telegram does
      (let loop ((rs rows) (day #f) (out '()))
        (if (null? rs)
            (reverse out)
            (let* ((row (car rs))
                   (d (whatsapp--day-of (whatsapp--msg-time row)))
                   (pill (if (equal? d day)
                             out
                             (cons (component 'ui/row
                                     (list 'class "whatsapp-day" 'text d))
                                   out)))
                   (here (and current
                              (= (whatsapp--msg-start row)
                                 (whatsapp--msg-start current))))
                   (doc (whatsapp--doc-preview row))
                   (photo (or (whatsapp--photo-local row) doc)))
              (loop (cdr rs) d
                    (cons (whatsapp--message-block row here)
                          ;; a photo sits above its bubble, on the same side
                          (if photo
                              (cons (list 'tag "img"
                                          'class (string-append
                                                   "whatsapp-photo"
                                                   (if doc " whatsapp-doc" "")
                                                   (if (whatsapp--msg-mine? row)
                                                       " whatsapp-photo-me" ""))
                                          'file photo)
                                    pill)
                              pill))))))
      (list
        (component 'ui/empty
          (list 'class "whatsapp-empty"
                'text
                  (if (and (string? notice)
                           (not (equal? (string-trim notice) "")))
                      (string-trim notice)
                      "No messages."))))))

(define (whatsapp--surface-frame buf)
  "the chat interface's frame for a conversation: its actions, its bubbles and its reply field. WhatsApp sends the newest first; a chat reads oldest at the top, newest at the foot"
  (let* ((rows (whatsapp--messages buf))
         (current (or (whatsapp--shown-row buf rows)
                      (whatsapp--message-at rows (buffer-point buf)))))
    (list 'head
          (list (component 'ui/actions
                  (list 'class "whatsapp-actions"
                        'actions '(("whatsapp-reply" "Reply")
                                   ("whatsapp-refresh" "Refresh")))))
          'items (whatsapp--conversation-items
                   (reverse rows) current (buffer-local-value 'whatsapp-notice buf))
          'prompt (list 'hint "Reply"))))

(define (whatsapp--surface-spec)
  (list 'mark 'whatsapp-input-mark
        'draw whatsapp--surface-frame
        ;; M-<up> and M-<down> walk the messages, not the drawn rows
        'step (lambda (buf delta) (whatsapp--step! buf delta))
        'send (lambda (buf text)
                (chat-surface-clear-input! buf)
                (whatsapp--send! buf text))))

(chat-surface-kind! "whatsapp-chat-mode" (whatsapp--surface-spec))



(define (whatsapp--conversation-buffer jid)
  *whatsapp-show-buffer*)



(domain! 'chat)
(effects! '(write))

(define (whatsapp--replace-text! buf text)
  (let ((p (buffer-point buf)))
    (buffer-delete-range! buf 0 (buffer-size buf))
    (buffer-append! buf text)
    (buffer-goto! buf (min p (buffer-size buf)))))

;; The view is a projection of the tree, rebuilt from the buffer text
;; every time. Nothing about a message is kept anywhere else, so the
;; blocks a reader sees and the text the grammar read cannot drift.
(define (whatsapp--paint! buf)
  (when (buffer-known? buf)
    (let* ((rows (whatsapp--messages buf))
           (current (whatsapp--message-at rows (buffer-point buf))))
      (buffer-set-local! buf 'render-mode "blocks")
      (chat-surface-redraw! buf)
      (buffer-set-local! buf 'modeline-info
        (string-append
          "WhatsApp · "
          (whatsapp--text (buffer-local-value 'whatsapp-name buf) "chat")
          (if current
              (string-append " · " (whatsapp--msg-sender current))
              ""))))))

;; RAW is what the server printed. It goes in as it came: the grammar
;; reads the buffer, so anything this rewrote would be a second
;; opinion about what was said.
(define (whatsapp--render-conversation! buf raw)
  (let* ((text (if (string? raw) raw ""))
         ;; the reply field is the buffer's tail; a refresh keeps what is typed
         (input (if (chat-surface-spec buf) (chat-surface-input-text buf) "")))
    (whatsapp--replace-text! buf (string-append text input))
    (buffer-set-local! buf 'whatsapp-input-mark (string-byte-length text))
    (buffer-set-local! buf 'whatsapp-notice text)
    (buffer-set-local! buf 'whatsapp-messages-jid
      (buffer-local-value 'whatsapp-jid buf))
    (whatsapp--paint! buf)))

;; Motion belongs to the tree: the next message is the next sibling of
;; the node point stands in, and every mode with a grammar moves this
;; way.
(define (whatsapp--select-at! buf pos)
  (buffer-goto! buf pos)
  (whatsapp--paint! buf))

(define (whatsapp--goto-message! buf op)
  (let* ((rows (whatsapp--messages buf))
         (here (whatsapp--message-at rows (buffer-point buf))))
    (if (not here)
        (message "No messages here")
        (let ((target
                (with-current-buffer buf
                  (lambda ()
                    (ts-node "message"
                             (whatsapp--msg-start here)
                             (whatsapp--msg-end here) op)))))
          (if target
              (whatsapp--select-at! buf (nth 1 target))
              (message (if (equal? op 'next)
                           "Last message"
                           "First message")))))))

;; A conversation is walked by its messages: M-<up> and M-<down> bring the
;; message before or after to the top of the window and mark it, and imenu
;; lists every message. The drawn rows hold day pills and photos too, so a
;; message's place among them is looked up by the start its bubble names.
(define (whatsapp--items buf)
  (plist-get (whatsapp--surface-frame buf) 'items))

(define (whatsapp--bubble-start item)
  (let ((c (and (pair? item) (plist-get item 'click))))
    (and (string? c) (string-prefix? "msg:" c)
         (string->number (substring c 4 (string-length c))))))

(define (whatsapp--entry-index items start)
  (let loop ((is items) (i 0))
    (cond ((null? is) #f)
          ((equal? (whatsapp--bubble-start (car is)) start) i)
          (else (loop (cdr is) (+ i 1))))))

(define (whatsapp--shown-row buf rows)
  "the message M-<up>, M-<down> or imenu brought up, while the window stays there"
  (let ((s (and (chat-surface-away? buf) (buffer-local-value 'whatsapp-shown buf))))
    (and s (find (lambda (r) (= (whatsapp--msg-start r) s)) rows))))

(define (whatsapp--show-entry! buf start)
  "bring the message at START to the top of BUF's window, its photo with it, and mark it"
  (let* ((items (whatsapp--items buf))
         (idx (whatsapp--entry-index items start)))
    (when idx
      (buffer-set-local! buf 'whatsapp-shown start)
      (chat-surface-place! buf
        (if (and (> idx 0)
                 (equal? (plist-get (list-ref items (- idx 1)) 'tag) "img"))
            (- idx 1)
            idx))
      (buffer-set-local! buf 'chat-surface-nav idx)
      (whatsapp--paint! buf))))

(define (whatsapp--step! buf delta)
  (let* ((items (whatsapp--items buf))
         (bubbles (let loop ((is items) (i 0) (acc '()))
                    (if (null? is)
                        (reverse acc)
                        (loop (cdr is) (+ i 1)
                              (if (whatsapp--bubble-start (car is)) (cons i acc) acc)))))
         (cur (and (chat-surface-away? buf) (buffer-local-value 'chat-surface-nav buf)))
         (p (if (integer? cur) cur (length items)))
         (target
           (if (< delta 0)
               (let ((before (filter (lambda (i) (< i p)) bubbles)))
                 (and (pair? before) (car (last-pair before))))
               (let ((after (filter (lambda (i) (> i p)) bubbles)))
                 (and (pair? after) (car after))))))
    (cond (target (whatsapp--show-entry! buf (whatsapp--bubble-start (list-ref items target))))
          ((< delta 0) (message "First message"))
          (else (buffer-set-local! buf 'whatsapp-shown #f)
                (chat-surface-follow! buf)
                (whatsapp--paint! buf)))
    #t))

(define (whatsapp--imenu-rows buf)
  "(LINE KIND NAME DOC TARGET) for each message of BUF, oldest first; TARGET brings it up"
  (let ((rows (whatsapp--messages buf)))
    (filter (lambda (r) r)
      (map (lambda (item)
             (let* ((s (whatsapp--bubble-start item))
                    (row (and s (find (lambda (r) (= (whatsapp--msg-start r) s)) rows))))
               (and row
                    (let ((name (string-append
                                  (if (whatsapp--msg-mine? row) "Me" (whatsapp--msg-sender row))
                                  ": " (whatsapp--one-line (whatsapp--body-shown row)))))
                      (list (with-current-buffer buf (lambda () (line-number-at-pos s)))
                            "message"
                            (if (> (string-length name) 90)
                                (string-append (substring name 0 89) "…")
                                name)
                            (whatsapp--short-time (whatsapp--msg-time row))
                            (lambda () (whatsapp--show-entry! buf s)))))))
           (whatsapp--items buf)))))

(domain! 'chat)
(effects! '(read external))

;; The chat list leaves a name and a preview empty for some chats, while
;; the chat's newest transcript has both. Keep what a transcript said for
;; a few minutes, so a refresh does not ask again for every chat.
(defvar '*whatsapp-details* '())
(defvar '*whatsapp-details-at* 0)
(define whatsapp-detail-ttl 300)

(define (whatsapp--detail-lookup jid)
  (let ((e (assoc jid *whatsapp-details*)))
    (and e (cadr e))))

(define (whatsapp--detail-remember! jid parts)
  (set! *whatsapp-details* (cons (list jid parts) *whatsapp-details*))
  parts)

(define (whatsapp--needs-detail? chat)
  (or (not (whatsapp--text (plist-get chat 'name) #f))
      (not (plist-get chat 'last_message))))

(define (whatsapp--fill-chat chat)
  "(whatsapp--fill-chat CHAT) -- CHAT with the name and preview its newest transcript gives"
  (let* ((jid (whatsapp--text (plist-get chat 'jid) ""))
         (known (and (not (equal? jid "")) (whatsapp--detail-lookup jid)))
         (parts
           (or known
               (and (not (equal? jid ""))
                    (whatsapp--detail-remember! jid
                      (whatsapp--transcript-first
                        (mcp-tool-call "whatsapp" "list_messages"
                          (list 'chat_jid jid 'limit 1 'page 0
                                'include_context #f))))))))
    (whatsapp--chat-with-last-message chat parts)))

(define (whatsapp--enrich-chats rows)
  (when (> (- (current-time) *whatsapp-details-at*) whatsapp-detail-ttl)
    (set! *whatsapp-details* '())
    (set! *whatsapp-details-at* (current-time)))
  (map (lambda (chat)
         (if (whatsapp--needs-detail? chat)
             (or (report-errors "whatsapp: chat detail"
                                (lambda () (whatsapp--fill-chat chat)))
                 chat)
             chat))
       rows))

(define (whatsapp--fetch-chats buf k)
  (let* ((text
           ;; an error answers no rows, so the cache's refresh finishes and
           ;; the next g reads again instead of waiting on a dead fetch
           (report-errors "whatsapp"
             (lambda ()
               (*whatsapp-call* 'whatsapp "list_chats"
                 (list 'limit whatsapp-chat-limit
                       'page 0
                       'include_last_message #t
                       'sort_by "last_active")))))
         (rows (whatsapp--parse-chats text)))
    (if rows
        (let ((rows (whatsapp--enrich-chats rows)))
          ;; A cache callback redraws with the cached rows. Seed the
          ;; unfiltered rows first so local filtering cannot retain the
          ;; empty source installed by the mode's initial render.
          (buffer-set-local! buf 'list-source-entries rows)
          (k rows))
        (begin
          (message "WhatsApp did not return a chat list")
          (k #f)))))

(defcustom 'whatsapp-media-host "marilyn"
  "The ssh host the WhatsApp bridge runs on. A photo is downloaded there and copied here to show in the conversation."
  'group 'whatsapp 'type 'string)

(defvar '*whatsapp-photos-pending* '())

(define (whatsapp--fetch-photos! buf)
  "fetch each photo and document of BUF's messages not here yet, off the lane, and redraw BUF as each one lands; a document gets a preview of its first page"
  (for-each
    (lambda (row)
      (let ((ref (whatsapp--media-ref row)))
        (when (and ref
                   (not (whatsapp--media-local row))
                   (not (member (cadr ref) *whatsapp-photos-pending*)))
          (let* ((kind (car ref)) (id (cadr ref))
                 (done! (lambda ()
                          (set! *whatsapp-photos-pending*
                                (filter (lambda (x) (not (equal? x id)))
                                        *whatsapp-photos-pending*)))))
            (set! *whatsapp-photos-pending* (cons id *whatsapp-photos-pending*))
            (*whatsapp-call* 'whatsapp "download_media"
              (list 'message_id id 'chat_jid (caddr ref))
              (lambda (ok text)
                (let* ((reply (and ok (string? text) (json-parse text)))
                       (remote (and reply (plist-get reply 'file_path))))
                  (if (not (string? remote))
                      (done!)
                      (let* ((doc? (equal? kind "document"))
                             (dir (if doc? (whatsapp--doc-dir id) whatsapp-media-directory))
                             (local (if doc?
                                        (string-append dir "/" (file-name-nondirectory remote))
                                        (whatsapp--photo-file id))))
                        (make-directory! dir)
                        (shell-command->string
                          (string-append
                            "ssh -o BatchMode=yes " whatsapp-media-host " "
                            (whatsapp--sq (string-append "cat " (whatsapp--sq remote)))
                            " > " (whatsapp--sq local) " 2>/dev/null"
                            (if doc?
                                (string-append
                                  " && qlmanage -t -s 480 -o " (whatsapp--sq dir) " "
                                  (whatsapp--sq local) " >/dev/null 2>&1; mv -f "
                                  (whatsapp--sq (string-append local ".png")) " "
                                  (whatsapp--sq (string-append dir "/preview.png"))
                                  " 2>/dev/null")
                                ""))
                          dir
                          (lambda (out)
                            (done!)
                            (when (buffer-known? buf) (whatsapp--paint! buf)))))))))))))
    (whatsapp--messages buf)))

(define (whatsapp--refresh-conversation! buf)
  (let ((jid (buffer-local-value 'whatsapp-jid buf)))
    (when jid
      (whatsapp--render-conversation! buf "Loading messages…\n")
      (*whatsapp-call* 'whatsapp "list_messages"
        (list 'chat_jid jid
              'limit whatsapp-message-limit
              'page 0
              'include_context #f)
        (lambda (ok text)
          ;; One shared scene pane serves every chat. Ignore a response
          ;; whose request no longer owns that pane.
          (when (and (buffer-known? buf)
                     (equal? jid (buffer-local-value 'whatsapp-jid buf)))
            (whatsapp--render-conversation! buf
              (if ok
                  text
                  (string-append
                    "Could not load messages: "
                    (whatsapp--text text "unknown error")
                    "\n")))
            (when ok (whatsapp--fetch-photos! buf))))))))

(define (whatsapp--display-conversation! buf)
  ;; Show the conversation without selecting its window; the index keeps
  ;; keyboard focus so moving up/down continues to preview chats. Every
  ;; chat lands in the index's one detail window, and C-` there walks the
  ;; chats already opened (packages/detail.scm).
  (display-buffer-detail! buf *whatsapp-buffer*))

(define (whatsapp--open-chat! chat &optional display?)
  (let* ((jid (whatsapp--text (plist-get chat 'jid) ""))
         (name (whatsapp--text (plist-get chat 'name) jid))
         (buf (whatsapp--conversation-buffer jid)))
    (if (equal? jid "")
        (message "This chat has no WhatsApp JID")
        (begin
          (unless (buffer-known? buf) (buffer-create buf))
          ;; Keep the conversation buffer's text attached to the selected
          ;; JID: the transcript in it is the only copy of this chat.
          (unless (equal? jid (buffer-local-value 'whatsapp-messages-jid buf))
            (buffer-set-local! buf 'whatsapp-messages-jid #f)
            (buffer-set-local! buf 'whatsapp-notice #f)
            (buffer-set-local! buf 'render-blocks #f))
          (buffer-set-local! buf 'whatsapp-jid jid)
          (buffer-set-local! buf 'whatsapp-name name)
          (with-current-buffer buf
            (lambda () (set-mode! "whatsapp-chat-mode")))
          (when display? (whatsapp--display-conversation! buf))))))

(domain! 'chat)
(effects! '(write external))

(define *whatsapp-reply-tool* (string-append "send" "_" "message"))

(define (whatsapp--send! buf text)
  (let* ((chat
           (and (buffer-derived-mode? buf "whatsapp-mode")
                (list-current *whatsapp-buffer*)))
         (jid
           (if chat
               (plist-get chat 'jid)
               (buffer-local-value 'whatsapp-jid buf))))
    (if (not jid)
        (message "No WhatsApp chat is selected")
        (begin
          (message "Sending WhatsApp reply…")
          (*whatsapp-call* 'whatsapp *whatsapp-reply-tool*
            (list 'recipient jid 'message text)
            (lambda (ok result)
              (when (buffer-known? buf)
                (if ok
                    (begin
                      (message "WhatsApp reply sent")
                      (whatsapp--refresh-conversation! buf))
                    (message
                      (string-append
                        "WhatsApp reply failed: "
                        (whatsapp--text result "unknown error")))))))))))

(domain! 'chat)
(effects! '(write external))

(define-style! 'whatsapp
  "
/* A chat, drawn the way Telegram draws one: bubbles on a quiet
   ground, mine on the right in the accent, theirs on the left, a day
   pill where the day changes. The text keeps the theme's own face. */
.whatsapp-actions { padding: 2px 8px 8px; }
.whatsapp-day { display: block; width: fit-content; margin: 10px auto 6px; padding: 2px 10px; border-radius: 999px; font-size: 12px; color: var(--dim-fg); background: color-mix(in srgb, var(--fg) 10%, transparent); }
.whatsapp-message { display: block; width: fit-content; max-width: min(74%, 64ch); margin: 3px 12px; padding: 6px 10px 4px; border-radius: 14px 14px 14px 4px; font-size: 13px; line-height: 1.4; background: color-mix(in srgb, var(--fg) 8%, transparent); }
.whatsapp-message-me { margin-left: auto; border-radius: 14px 14px 4px 14px; background: color-mix(in srgb, var(--accent-fg) 24%, transparent); }
.whatsapp-message-current { outline: 1px solid var(--accent-fg); outline-offset: 1px; }
.whatsapp-message-sender { display: block; font-size: 12px; font-weight: 600; color: var(--accent-fg); }
.whatsapp-message-body { display: block; color: var(--fg); white-space: pre-wrap; overflow-wrap: anywhere; }
.whatsapp-message-time { display: block; margin-top: 2px; text-align: right; font-size: 11px; color: var(--dim-fg); font-variant-numeric: tabular-nums; }
.whatsapp-photo { display: block; width: auto; height: auto; max-width: min(45%, 220px); max-height: 220px; margin: 6px 12px 0; border-radius: 12px; object-fit: cover; }
.whatsapp-photo-me { margin-left: auto; }
.whatsapp-doc { object-fit: contain; object-position: top; background: #fff; border: 1px solid color-mix(in srgb, var(--fg) 15%, transparent); }
.whatsapp-message-reactions { display: inline-block; margin-top: 3px; padding: 1px 7px; border-radius: 999px; font-size: 12px; color: var(--fg); background: color-mix(in srgb, var(--fg) 12%, transparent); }
.whatsapp-empty { margin: 4px 8px; font-size: 12px; color: var(--dim-fg); }
")

(add-hook! (list 'block-click 'whatsapp)
  (lambda (buf id)
    (and (equal? (buffer-local-value 'mode-name buf) "whatsapp-chat-mode")
         (cond
           ((equal? id "whatsapp-reply")
            (with-current-buffer buf
              (lambda () (run-command "whatsapp-reply")))
            #t)
           ((equal? id "whatsapp-refresh")
            (with-current-buffer buf
              (lambda () (run-command "whatsapp-chat-refresh")))
            #t)
           ((string-prefix? "msg:" id)
            (let ((pos (string->number
                         (substring-bytes id 4 (string-byte-length id)))))
              (when (number? pos) (whatsapp--select-at! buf pos)))
            #t)
           (else #f)))))

(define-mode "whatsapp-chat-mode"
  (lambda ()
    (let* ((buf (current-buffer))
           (jid (buffer-local-value 'whatsapp-jid buf))
           (loaded?
             (and jid
                  (equal? jid (buffer-local-value 'whatsapp-messages-jid buf)))))
      (whatsapp--join-group! buf)
      ;; before anything reads the buffer: every question below is a
      ;; question for the grammar
      (buffer-set-local! buf 'ts-lang "whatsapp")
      (buffer-set-local! buf 'special #f)
      (buffer-set-local! buf 'desktop-skip-locals
        '(render-blocks whatsapp-notice whatsapp-messages-jid))
      (buffer-set-local! buf 'render-mode "blocks")
      (buffer-set-local! buf 'imenu-index-function 'whatsapp--imenu-rows)
      ;; the shared chat interface: the bubbles above, the reply field below
      (chat-surface! buf (whatsapp--surface-spec))
      (cond
        (loaded? (whatsapp--paint! buf))
        (jid (whatsapp--refresh-conversation! buf))
        (else (whatsapp--render-conversation! buf ""))))))

(mode-doc! "whatsapp-chat-mode"
  (string-append
    "One WhatsApp conversation, read as messages rather than as lines. "
    "Up and down move between messages and mark the one you stand on; "
    "n/p and j/k do the same. r replies to that message, quoting it, "
    "and g refreshes."))

(mode-keys! "whatsapp-chat-mode"
  '(("<down>" "whatsapp-next-message")
    ("<up>" "whatsapp-prev-message")
    ("n" "whatsapp-next-message")
    ("p" "whatsapp-prev-message")
    ("j" "whatsapp-next-message")
    ("k" "whatsapp-prev-message")
    ("r" "whatsapp-reply")
    ("g" "whatsapp-chat-refresh")
    ("q" "quit-window")))

(domain! 'chat)
(effects! '(read external))

(defface! 'whatsapp-avatar-0 'fg "#ffffff" 'bg "#1f7a8c" 'weight "600")
(defface! 'whatsapp-avatar-1 'fg "#ffffff" 'bg "#8a4b9e" 'weight "600")
(defface! 'whatsapp-avatar-2 'fg "#ffffff" 'bg "#b8621b" 'weight "600")
(defface! 'whatsapp-avatar-3 'fg "#ffffff" 'bg "#2e7d4f" 'weight "600")
(defface! 'whatsapp-avatar-4 'fg "#ffffff" 'bg "#3b5bdb" 'weight "600")
(defface! 'whatsapp-avatar-5 'fg "#ffffff" 'bg "#b23a48" 'weight "600")
(defface! 'whatsapp-name 'weight "600")
(defface! 'whatsapp-time 'fg "var(--dim-fg)")
(defface! 'whatsapp-preview 'fg "var(--dim-fg)")

(define (whatsapp--avatar-face name)
  "the colour of a chat's avatar: one of six, fixed by its name"
  (string-append "whatsapp-avatar-"
    (number->string (modulo (apply + (map char->integer (string->list name))) 6))))

(define (whatsapp--initials name)
  "the one or two letters an avatar shows: the first of the first two words"
  (let* ((words (filter (lambda (w) (not (equal? w ""))) (string-split name " ")))
         (letters (apply string-append
                    (map (lambda (w) (substring-bytes w 0 1)) (take words 2)))))
    (if (equal? letters "") "?" (string-upcase letters))))

(define (whatsapp--row-cells buf chat)
  (let* ((jid (whatsapp--text (plist-get chat 'jid) "unknown"))
         (name (whatsapp--text (plist-get chat 'name) jid)))
    (list
      (list (list (whatsapp--initials name) (whatsapp--avatar-face name))
            (list name "whatsapp-name")
            (list (whatsapp--list-time (plist-get chat 'last_message_time))
                  "whatsapp-time"))
      (list ""
            (list (string-append
                    (if (whatsapp--from-me? chat) "me: " "")
                    (whatsapp--one-line (plist-get chat 'last_message)))
                  "whatsapp-preview")))))

(define-list-mode! "whatsapp-mode"
  (list
    'doc (string-append
           "Recent WhatsApp chats. RET reads the selected conversation. "
           "Use r to reply, g to refresh, / to filter, and q to quit.")
    'buffer *whatsapp-buffer*
    'special #f
    'rows (lambda (buf)
            (whatsapp--join-group! buf)
            (list-entries buf))
    'cache-fetch whatsapp--fetch-chats
    'cache-ttl whatsapp-cache-ttl
    ;; two lines a chat: the avatar and the name with the time, then the
    ;; last message under them, as WhatsApp lays a chat out
    'row-columns (lambda (buf)
                   (list (list (list "" 3 'left) (list "chat" #f 'left)
                               (list "when" 10 'right))
                         (list (list "" 3 'left) (list "message" #f 'left))))
    'row-cells whatsapp--row-cells
    'title (lambda (buf) "WhatsApp chats")
    'meta (lambda (buf)
            (string-append
              (number->string (length (list-source-entries buf)))
              " recent chats"))
    'total (lambda (buf) (length (list-source-entries buf)))
    'local-filter #t
    'no-marks #t
    'preview (lambda (buf chat) (whatsapp--open-chat! chat #f))
    'debounce #t
    'key (lambda (buf chat) (plist-get chat 'jid))
    'footer (lambda (buf)
              '(("RET" "read") ("r" "reply") ("s" "search") ("/" "filter")
                ("g" "refresh") ("q" "quit")))
    'keys '(("RET" "whatsapp-open")
            ("r" "whatsapp-reply")
            ("s" "whatsapp-search")
            ("g" "whatsapp-refresh")
            ("q" "quit-window"))))

(mode-doc! "whatsapp-mode"
  "Recent WhatsApp chats. RET reads a conversation. Use g to refresh.")

(domain! 'chat)
(effects! '(read external display))

(define-command "whatsapp-list" "Show the recent WhatsApp chat index"
  (lambda () (list-mode-show! "whatsapp-mode")))

(define-command "whatsapp-show-current" "Prepare the WhatsApp conversation pane"
  (lambda ()
    ;; Opening the scene must not turn the list's incidental first row
    ;; into a conversation choice. Keep the last explicitly opened chat;
    ;; RET in the index is the only operation that replaces this pane.
    (unless (buffer-known? *whatsapp-show-buffer*)
      (buffer-create *whatsapp-show-buffer*))
    (whatsapp--join-group! *whatsapp-show-buffer*)
    (when (and (not (buffer-local-value 'whatsapp-jid *whatsapp-show-buffer*))
               (= (buffer-size *whatsapp-show-buffer*) 0))
      (buffer-append! *whatsapp-show-buffer* "No conversation selected.\n")
      (buffer-set-read-only! *whatsapp-show-buffer* #t))))

(define-scene! "whatsapp"
  '(h 0.32
      (as index (ensure "*WhatsApp*" "whatsapp-list"))
      (as show (ensure "*WhatsApp conversation*" "whatsapp-show-current"))
      (as chat group-chat)))

(define-command "whatsapp" "Open the WhatsApp workspace"
  (lambda ()
    (let ((destination (if (equal? whatsapp-group "") "whatsapp" whatsapp-group)))
      (scene-open! "whatsapp" destination)
      ;; scene construction can be followed by current-group derivation from
      ;; the old visible panes; make the requested destination authoritative.
      (switch-to-group! destination))))

(define-command "whatsapp-open" "Read the WhatsApp chat on this row"
  (lambda ()
    (let ((chat (list-current *whatsapp-buffer*)))
      (if chat
          (whatsapp--open-chat! chat #t)
          (message "No WhatsApp chat is selected"))))
  'modes '(whatsapp-mode))

(domain! 'chat)
(effects! '(read external))

(defvar '*whatsapp-people* #f)      ; (AT ROWS): the last search list and when it was read

(define (whatsapp--contact-name c)
  (let ((pick (lambda (k) (let ((v (plist-get c k))) (and (string? v) (not (equal? (string-trim v) "")) (string-trim v))))))
    (or (pick 'full_name) (pick 'push_name) (pick 'business_name))))

(define (whatsapp--jid-number jid)
  (car (string-split (whatsapp--text jid "") "@")))

(define (whatsapp--people chats contacts lids)
  "one row (LABEL JID NAME) per person: the chats first, newest first, then the contacts never written to. LIDS is ((LID PN) ...): a chat under a linked id takes its name from the contact of its number"
  (let* ((contact-name
           (lambda (num)
             (let ((c (find (lambda (c) (equal? (whatsapp--jid-number (plist-get c 'jid)) num)) contacts)))
               (and c (whatsapp--contact-name c)))))
         (pn-of (lambda (jid)
                  (let ((num (whatsapp--jid-number jid)))
                    (if (string-suffix? "@lid" jid)
                        (let ((m (assoc num lids))) (and m (cadr m)))
                        (and (string-suffix? "@s.whatsapp.net" jid) num)))))
         (chat-rows
           (filter (lambda (r) r)
             (map (lambda (c)
                    (let* ((jid (plist-get c 'jid))
                           (pn (and (string? jid) (pn-of jid)))
                           (own (whatsapp--text (plist-get c 'name) ""))
                           (name (if (or (equal? own "") (not (re-find "[^0-9+ ]" own 0)))
                                     (or (and pn (contact-name pn)) (and (not (equal? own "")) own))
                                     own)))
                      (and (string? jid) name
                           (not (string-suffix? "@broadcast" jid))
                           (list (string-append name "  · "
                                   (cond ((string-suffix? "@g.us" jid) "group")
                                         (pn (string-append "+" pn))
                                         (else "chat")))
                                 jid name pn))))
                  chats)))
         ;; one row a person: a number with two chats keeps its newest
         (chat-rows
           (let loop ((rs chat-rows) (seen '()) (out '()))
             (cond ((null? rs) (reverse out))
                   ((let ((key (or (cadddr (car rs)) (cadr (car rs))))) (member key seen))
                    (loop (cdr rs) seen out))
                   (else (loop (cdr rs) (cons (or (cadddr (car rs)) (cadr (car rs))) seen)
                               (cons (car rs) out))))))
         (written (filter (lambda (x) x) (map cadddr chat-rows)))
         (contact-rows
           (filter (lambda (r) r)
             (map (lambda (c)
                    (let* ((jid (plist-get c 'jid)) (name (whatsapp--contact-name c))
                           (num (and (string? jid) (whatsapp--jid-number jid))))
                      (and (string? jid) name
                           (string-suffix? "@s.whatsapp.net" jid)
                           (not (member num written))
                           (list (string-append name "  · +" num) jid name num))))
                  contacts))))
    (append chat-rows contact-rows)))

(defcustom 'whatsapp-bridge-store "/home/svs/src/mcp/whatsapp-mcp/whatsapp-bridge/store"
  "The bridge's store folder on whatsapp-media-host. Its whatsapp.db maps each linked id to a phone number."
  'group 'whatsapp 'type 'string)

(define (whatsapp--lid-map k)
  "K gets ((LID PN) ...), read off the lane from the bridge's database; '() when it cannot be read"
  (shell-command->string
    (string-append "ssh -o BatchMode=yes " whatsapp-media-host " "
      (whatsapp--sq (string-append "sqlite3 " (whatsapp--sq (string-append whatsapp-bridge-store "/whatsapp.db"))
                                   " 'select lid, pn from whatsmeow_lid_map'"))
      " 2>/dev/null")
    (default-directory)
    (lambda (out)
      (k (filter (lambda (p) (= (length p) 2))
                 (map (lambda (l) (string-split l "|"))
                      (string-split (whatsapp--text out "") "\n")))))))

(define (whatsapp--with-people k)
  "K gets the people rows: the list read in the last five minutes, else read again off the lane"
  (let ((cached *whatsapp-people*))
    (if (and cached (< (- (current-time) (car cached)) 300))
        (k (cadr cached))
        (begin
          (message "Reading WhatsApp contacts…")
          (*whatsapp-call* 'whatsapp "list_chats"
            (list 'limit 2000 'page 0 'sort_by "last_active")
            (lambda (ok chats)
              (*whatsapp-call* 'whatsapp "list_all_contacts" (list)
                (lambda (ok2 contacts)
                  (whatsapp--lid-map
                    (lambda (lids)
                      (let ((rows (whatsapp--people
                                    (or (and ok (whatsapp--parse-chats chats)) '())
                                    (or (and ok2 (whatsapp--parse-chats contacts)) '())
                                    lids)))
                        (set! *whatsapp-people* (list (current-time) rows))
                        (k rows))))))))))))

(define-command "whatsapp-search" "Find anyone on WhatsApp, a chat or a contact, and open the conversation"
  (lambda ()
    (with-current-buffer *whatsapp-buffer*
      (lambda ()
        (whatsapp--with-people
          (lambda (rows)
            (completing-read "WhatsApp: " (map car rows)
              (lambda (label)
                (let ((row (assoc label rows)))
                  (if row
                      (whatsapp--open-chat! (list 'jid (cadr row) 'name (caddr row)) #t)
                      (message "No one by that name"))))
              'require-match #t 'category 'whatsapp-person)))))))

(define-command "whatsapp-refresh" "Refresh recent WhatsApp chats"
  (lambda ()
    (message "Refreshing WhatsApp chats…")
    (cache-refresh! *whatsapp-buffer*))
  'modes '(whatsapp-mode))

(define-command "whatsapp-chat-refresh" "Refresh this WhatsApp conversation"
  (lambda () (whatsapp--refresh-conversation! (current-buffer)))
  'modes '(whatsapp-chat-mode))

(domain! 'chat)
(effects! '(write external))

(define-command "whatsapp-reply" "Reply to the selected message, or to the chat"
  (lambda ()
    (let* ((buf (current-buffer))
           (row (whatsapp--current-message buf)))
      (read-string
        (if row
            (string-append "Reply to " (whatsapp--msg-sender row) ": ")
            "Reply: ")
        (lambda (text)
          (let ((trimmed (string-trim text)))
            (unless (equal? trimmed "")
              (whatsapp--send! buf
                (if row
                    (string-append (whatsapp--quote row) trimmed)
                    trimmed)))))
        'history 'whatsapp-reply-history)))
  'modes '(whatsapp-mode whatsapp-chat-mode))

(define-command "whatsapp-next-message" "Move to the next message"
  (lambda () (whatsapp--goto-message! (current-buffer) 'next))
  'modes '(whatsapp-chat-mode))

(define-command "whatsapp-prev-message" "Move to the previous message"
  (lambda () (whatsapp--goto-message! (current-buffer) 'prev))
  'modes '(whatsapp-chat-mode))

(mode-icon! "whatsapp-mode" "W")
(mode-icon! "whatsapp-chat-mode" "W")

;; A conversation is a chat: the Cmd- and Shift-arrows stay the window
;; motion after you type a reply, as they do in chat-mode.
(editing-state-maps-off! "whatsapp-chat-mode" '("editing-caret-map" "cua-mode-map"))


;;; The feed: every WhatsApp message becomes a "whatsapp:<chat jid>" event
;;; in the event log. The bridge on the WhatsApp host pushes each message it
;;; stores to a webhook here, over the tailnet. A slow sweep reads the
;;; bridge's /api/messages after a saved rowid and adds what a push dropped;
;;; a received message only the sweep found is a "feed:whatsapp" missed event.

(domain! 'chat)
(effects! '(write external))

(defcustom 'whatsapp-feed-enabled #f
  "Run the WhatsApp feed. It starts at boot, so it stays on after a restart."
  'group 'whatsapp 'type 'boolean
  ;; custom.scm loads after this package, so the saved value arrives here
  'set (lambda (on) (whatsapp-feed--apply on)))

(defcustom 'whatsapp-feed-bridge "http://100.110.113.41:8080"
  "The bridge's REST server, which the sweep reads."
  'group 'whatsapp 'type 'string)

(defcustom 'whatsapp-feed-sweep-seconds 900
  "Seconds between two sweeps of the bridge."
  'group 'whatsapp 'type 'integer)

;; the message ids the log holds, newest first; seeded from the log once
(defvar '*whatsapp-feed-seen* #f)

(effects! '(pure))

(define (whatsapp-feed--event row)
  "(whatsapp-feed--event ROW) — the topic, kind and data of one bridge message. A direct chat's topic is its phone number, so a person under a LID and under a phone JID is one conversation; a group's is its JID."
  (list (string-append "whatsapp:"
                       (let ((phone (plist-get row 'phone)))
                         (if (and phone (not (equal? phone ""))) phone (plist-get row 'chat_jid))))
        (if (plist-get row 'is_from_me) 'sent 'received)
        (list 'id (plist-get row 'id)
              'chat (plist-get row 'chat_jid)
              'chat-name (plist-get row 'chat_name)
              'phone (plist-get row 'phone)
              'sender (plist-get row 'sender)
              'sender-phone (plist-get row 'sender_phone)
              'text (plist-get row 'content)
              'at (plist-get row 'timestamp)
              'media (plist-get row 'media_type)
              'file (plist-get row 'filename))))

(effects! '(write))

(define (whatsapp-feed-publish! row)
  "(whatsapp-feed-publish! ROW) — log one bridge message unless the log has it; its seq, or #f"
  (unless *whatsapp-feed-seen*
    (set! *whatsapp-feed-seen*
          (map (lambda (e) (plist-get (plist-get e 'data) 'id))
               (event-log-newest "whatsapp:*" 2000))))
  (let ((id (plist-get row 'id)))
    (if (or (not id) (member id *whatsapp-feed-seen*))
        #f
        (let ((e (whatsapp-feed--event row)))
          (set! *whatsapp-feed-seen* (take (cons id *whatsapp-feed-seen*) 2000))
          (event-publish! (car e) (cadr e) (caddr e))))))

(define (whatsapp-feed--handle request)
  "(whatsapp-feed--handle REQUEST) — the feed webhook's /whatsapp: one message the bridge pushed"
  (if (not (equal? (plist-get request 'method) "POST"))
      (event-feed-reply 405 "POST only")
      (let ((row (json-parse (plist-get request 'body))))
        (if (not row)
            (event-feed-reply 400 "bad json")
            (begin (whatsapp-feed-publish! row)
                   (event-feed-reply 200 "ok"))))))

(effects! '(write external))

(define (whatsapp-feed-sweep!)
  "(whatsapp-feed-sweep!) — read the bridge after the saved rowid and log what the log lacks. The first sweep only saves the newest rowid."
  (let ((after (event-log-position "whatsapp-feed-sweep")))
    (http-get-json
     (string-append whatsapp-feed-bridge "/api/messages?limit=500&after="
                    (number->string (or after -1)))
     '()
     (lambda (rows)
       (when (pair? rows)
         (let ((missed 0))
           (when after
             (for-each (lambda (row)
                         (when (and (whatsapp-feed-publish! row)
                                    (not (plist-get row 'is_from_me)))
                           (set! missed (+ missed 1))))
                       rows))
           (event-log-position-set! "whatsapp-feed-sweep"
                                    (plist-get (car (reverse rows)) 'rowid))
           (when (> missed 0)
             (event-publish! "feed:whatsapp" 'missed (list 'count missed)))
           (when (= (length rows) 500) (whatsapp-feed-sweep!))))))))

(define (whatsapp-feed--tick _)
  (report-errors "whatsapp-feed: sweep" (lambda () (whatsapp-feed-sweep!)))
  (debounce! 'whatsapp-feed-sweep (* 1000 whatsapp-feed-sweep-seconds)
             (lambda (x) (whatsapp-feed--tick x)) #f))

(define (whatsapp-feed-start!)
  "(whatsapp-feed-start!) — listen for the bridge's pushes and start the sweep"
  (event-feed-route! "/whatsapp" (lambda (request) (whatsapp-feed--handle request)))
  (event-feed-start!)
  (whatsapp-feed--tick #f))

(define (whatsapp-feed-stop!)
  "(whatsapp-feed-stop!) — stop taking the bridge's pushes and stop the sweep"
  (set! *event-feed-routes* (events--without "/whatsapp" *event-feed-routes*))
  (debounce-cancel! 'whatsapp-feed-sweep))

(define-command "whatsapp-feed-start" "Turn WhatsApp messages into events: the webhook and the sweep"
  (lambda () (whatsapp-feed-start!) (message "WhatsApp feed on")))

(define-command "whatsapp-feed-stop" "Stop turning WhatsApp messages into events"
  (lambda () (whatsapp-feed-stop!) (message "WhatsApp feed off")))

;; after the load, not in it: a start inside the loader does not take
(define (whatsapp-feed--apply on)
  "(whatsapp-feed--apply ON) — start or stop the feed once the boot is done; starting twice is harmless"
  ;; at boot a task cannot start yet, so the feed waits for the boot to end
  (debounce! 'whatsapp-feed-boot 0
             (lambda (_) (if on (whatsapp-feed-start!) (whatsapp-feed-stop!))) #f))

(when whatsapp-feed-enabled (whatsapp-feed--apply #t))

;;; notices: a WhatsApp message that arrives passes in the corner

(domain! 'chat)
(effects! '(write display))

(defcustom 'whatsapp-notify #t
  "Show a notice for each WhatsApp message that arrives.")

(define (whatsapp-notify--text x)
  (if (and (string? x) (> (string-length x) 0)) x #f))

(define (whatsapp-notify--saw e)
  "a received message, other than a status update, makes a notice: the chat, then the first line"
  (let ((d (plist-get e 'data)))
    (when (and whatsapp-notify
               (eq? (plist-get e 'kind) 'received)
               (not (equal? (plist-get d 'chat) "status@broadcast")))
      (let ((text (whatsapp-notify--text (plist-get d 'text)))
            (media (whatsapp-notify--text (plist-get d 'media))))
        (notify! "whatsapp"
                 (or (whatsapp-notify--text (plist-get d 'chat-name))
                     (whatsapp-notify--text (plist-get d 'phone))
                     "WhatsApp")
                 (cond (text (first-line text))
                       (media (string-append "[" media "]"))
                       (else ""))
                 (list 'topic (plist-get e 'topic) 'event (plist-get e 'seq)
                       'jid (plist-get d 'chat)))))))

(event-subscribe! "whatsapp-notify" "whatsapp:*" 'whatsapp-notify--saw)

