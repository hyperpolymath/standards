# Ephapax

**Tagline:** Once-only evaluation with linear semantics

## Overview

Ephapax (from Greek ἅπαξ "hapax" meaning "once") explores ephemeral, once-only computation. Values exist for a single use, enabling powerful guarantees about data flow, security, and resource management.

## Philosophy

In a world of persistent data and unlimited copying, Ephapax asks: what if values were precious, consumable, and finite? This constraint, far from being limiting, enables:

- **Security**: Secrets can't be accidentally leaked (they're consumed on use)
- **Resource Safety**: Resources are guaranteed to be released
- **Protocol Verification**: Session types ensure correct communication patterns
- **Clarity**: Data flow is explicit and traceable

## Quick Start

```scheme
;; Hello World
(println "Hello, Ephapax!")

;; Once-only values
(let ((secret (once "my-password")))
  (authenticate secret)  ; secret is consumed here
  ; (use secret)         ; ERROR: already consumed
  )

;; Ephemeral computation
(let ((result (ephemeral (expensive-computation))))
  (display result)  ; computed and consumed
  ; (display result) ; ERROR: ephemeral already used
  )

;; Session types for protocols
(define client-session
  (session
    (send String)      ; send username
    (recv Bool)        ; receive auth result
    (choice
      [(ok (recv Data) end)]
      [(err end)])))
```

## Core Concepts

### Once-Only Values

Values wrapped in `once` can be used exactly once:

```scheme
;; Create once-only value
(define x (once 42))

;; Use it (consumes x)
(define y (+ (use x) 1))  ; y = 43

;; Cannot use again
; (use x)  ; ERROR: 'x' has been consumed

;; Pattern: transfer ownership
(define (transfer-resource resource destination)
  (send destination (use resource)))  ; resource consumed by send
```

### Ephemeral Computations

Computations that produce one-time results:

```scheme
;; Ephemeral lambda - can only be called once
(define single-use-fn
  (ephemeral
    (lambda (x) (* x x))))

(single-use-fn 5)  ; => 25
; (single-use-fn 3)  ; ERROR: function was ephemeral

;; Useful for one-time setup
(define initialize
  (ephemeral
    (lambda ()
      (setup-database)
      (load-config)
      (start-services))))

(initialize)  ; runs setup
; (initialize)  ; ERROR: already initialized
```

### Linear Values

Must be used exactly once (not zero, not more):

```scheme
;; Linear resource
(define (open-connection host)
  (linear (make-connection host)))

;; Must use the connection
(define conn (open-connection "localhost"))
; Cannot just drop conn - must use it!
(close-connection (use conn))  ; properly consumed

;; Compiler enforces usage
(define (bad-function)
  (let ((conn (open-connection "server")))
    42))  ; ERROR: linear 'conn' not used

;; Transfer is allowed
(define (good-function)
  (let ((conn (open-connection "server")))
    (pass-to-handler conn)))  ; ownership transferred
```

### Affine Values

Can be used at most once (can drop):

```scheme
;; Affine value - optional use
(define maybe-use (affine (expensive-compute)))

(if need-result
    (use maybe-use)  ; use if needed
    (void))          ; or just drop it

;; Automatic cleanup on drop
(define (with-temp-file body)
  (let ((file (affine (create-temp-file))))
    (body file)
    ; file automatically cleaned up if not used
    ))
```

## Session Types

Protocol-correct communication:

```scheme
;; Define protocol for authentication
(define-session-type AuthProtocol
  (send String)           ; client sends username
  (send String)           ; client sends password
  (recv AuthResult)       ; server responds
  (branch
    [(success
      (recv Token)        ; receive auth token
      (rec loop           ; can make requests
        (choice
          [(request (send Query) (recv Response) loop)]
          [(logout end])))]
    [(failure end])))

;; Implement client
(define (auth-client session username password)
  (let* ((s1 (send session username))
         (s2 (send s1 password))
         (s3 (recv s2)))
    (match (result s3)
      [(success token session)
       (request-loop session token)]
      [(failure session)
       (close session)])))

;; Type system ensures protocol is followed correctly
;; Cannot send when should receive, cannot skip steps
```

## Security Applications

### One-Time Tokens

```scheme
;; Generate one-time authentication token
(define (generate-otp)
  (once (random-secure-string 32)))

(define token (generate-otp))

;; Use for single authentication
(define (authenticate-with-otp otp)
  (let ((token-value (use otp)))  ; consumes token
    (verify-against-server token-value)))

(authenticate-with-otp token)  ; works
; (authenticate-with-otp token)  ; ERROR: token consumed
```

### Secure Data Handling

```scheme
;; Sensitive data wrapper
(define (sensitive data)
  (once
    (let ((encrypted (encrypt-in-memory data)))
      (make-sensitive-ref encrypted))))

(define password (sensitive (read-password-from-user)))

;; Can only access once
(define (verify-password sensitive-pw expected-hash)
  (let ((pw (use sensitive-pw)))  ; decrypts and consumes
    (constant-time-compare (hash pw) expected-hash)))
    ; pw is now gone from memory

;; Cannot accidentally log or leak
; (println password)  ; ERROR: cannot convert sensitive to string
```

### Capability Tokens

```scheme
;; Issue capability for single action
(define (issue-write-capability file)
  (once (make-capability 'write file)))

(define cap (issue-write-capability "/data/file.txt"))

(define (perform-write capability data)
  (let ((c (use capability)))
    (with-capability c
      (write-to-file data))))

(perform-write cap "important data")  ; works
; (perform-write cap "more data")     ; ERROR: capability consumed
```

## Resource Management

### File Handles

```scheme
;; Linear file handle
(define (open-file path mode)
  (linear (system-open path mode)))

;; Usage pattern
(define (read-entire-file path)
  (let ((handle (open-file path 'read)))
    (let ((contents (file-read-all handle)))  ; handle consumed by read-all
      contents)))
; No need to explicitly close - linear type ensures single use

;; Multi-step pattern with threading
(define (process-file path)
  (let* ((h1 (open-file path 'read))
         (h2 (skip-header h1))       ; h1 consumed, h2 returned
         (h3 contents (read-body h2)) ; h2 consumed, h3 returned
         (h4 (close-file h3)))        ; h3 consumed
    contents))
```

### Network Connections

```scheme
;; Linear connection
(define (connect host port)
  (linear (tcp-connect host port)))

;; Session-typed communication
(define (http-request conn method path)
  (let* ((c1 (send-line conn (format "~a ~a HTTP/1.1" method path)))
         (c2 (send-line c1 (format "Host: ~a" (conn-host c1))))
         (c3 (send-line c2 ""))
         (c4 response (recv-response c3)))
    (values (close-connection c4) response)))
```

## Echidna Verification

### Protocol Verification

```scheme
;; Verify session type compliance
@verify
(define-property session-type-safe
  "All protocol interactions follow session type"
  (forall ((s Session))
    (well-typed-session s)))

;; Echidna generates tests for all protocol paths
```

### Linearity Proofs

```coq
(* Prove linearity is preserved *)
Theorem linearity_preservation :
  forall e v,
    linear_typed e ->
    e ⟶* v ->
    used_exactly_once e.
```

## Standard Library

```scheme
;; Linear primitives
(module ephapax/linear
  (once      ; create once-only value
   linear    ; create must-use value
   affine    ; create at-most-once value
   use       ; consume and extract value
   transfer  ; transfer ownership
   drop))    ; explicitly drop affine value

;; Session types
(module ephapax/session
  (session           ; define session type
   send              ; send value (consumes channel state)
   recv              ; receive value (consumes channel state)
   choice            ; offer multiple branches
   branch            ; select from offered branches
   end               ; terminate session
   rec               ; recursive protocol
   dual))            ; compute dual session type

;; Secure data
(module ephapax/secure
  (sensitive        ; wrap sensitive data
   seal             ; seal data for single recipient
   unseal           ; unseal (consumes seal)
   zero-on-drop))   ; secure memory cleanup

;; Capabilities
(module ephapax/capability
  (make-capability  ; create capability token
   with-capability  ; use capability (consumes)
   delegate         ; transfer capability
   attenuate))      ; create restricted sub-capability
```

## Example: Secure Messaging

```scheme
(require ephapax/session)
(require ephapax/secure)

;; Protocol for secure messaging
(define-session-type SecureMessage
  (send PublicKey)      ; exchange keys
  (recv PublicKey)
  (rec loop
    (choice
      [(message
         (send EncryptedData)
         (recv Ack)
         loop)]
      [(end-session
         (send Goodbye)
         end]))))

;; Implement secure sender
(define (secure-chat session my-key)
  (let* ((s1 (send session (public-part my-key)))
         (s2 their-key (recv s1))
         (shared-secret (once (derive-key my-key their-key))))

    (define (chat-loop s)
      (let ((msg (read-input)))
        (if (equal? msg "quit")
            (let ((s' (select s 'end-session)))
              (send s' "Goodbye!"))
            (let* ((encrypted (encrypt (use shared-secret) msg))
                   ; ERROR: shared-secret was once-only!
                   ; Need to derive per-message keys
                   )))))))

;; Correct version with per-message keys
(define (secure-chat-correct session my-key)
  (let* ((s1 (send session (public-part my-key)))
         (s2 their-key (recv s1))
         (key-material (derive-key my-key their-key)))

    (define (chat-loop s counter)
      (let ((msg (read-input)))
        (if (equal? msg "quit")
            (let ((s' (select s 'end-session)))
              (send s' "Goodbye!"))
            (let* ((msg-key (once (derive-message-key key-material counter)))
                   (encrypted (encrypt (use msg-key) msg))
                   (s' (select s 'message))
                   (s'' (send s' encrypted))
                   (s''' ack (recv s'')))
              (chat-loop s''' (+ counter 1))))))

    (chat-loop s2 0)))
```

## Related Pages

- [[Session Types]]
- [[Linear Logic]]
- [[Security Patterns]]
- [[Protocol Verification]]
