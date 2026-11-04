#lang racket/base

;;; error.rkt — the FML error payload carried by result/result.rkt's `err`.
;;;
;;; An FML parse error does not violate a FHIR spec-url (the foundation's validation-error
;;; hierarchy), so it carries its own small payload: a phase tag, a human message, and the
;;; machine-readable diagnostic (fhir/diagnostic, with the source position) when there is one.
;;; The same shape as cql/error's, so the CLIs and project loaders read both alike.

(require racket/contract)

(provide (except-out (struct-out fml-error) fml-error)
         (rename-out [make-fml-error* fml-error])
         (contract-out [fml-error-string (-> fml-error? string?)]))

;; An FmlError is (fml-error phase message diagnostic):
;;   phase : 'parse | 'check ; diagnostic : a fhir/diagnostic, or #f
(struct fml-error (phase message diagnostic) #:transparent)
(define (make-fml-error* phase message [diagnostic #f]) (fml-error phase message diagnostic))

;; fml-error-string : FmlError -> String   the message as a CLI prints it
(define (fml-error-string e)
  (if (fml-error-diagnostic e)
      (fml-error-message e)                     ; already "file:line:col: fml parse error: …"
      (format "FML ~a error: ~a" (fml-error-phase e) (fml-error-message e))))

(module+ test
  (require rackunit)
  (check-equal? (fml-error-string (make-fml-error* 'parse "unexpected `}`")) "FML parse error: unexpected `}`")
  (check-equal? (fml-error-string (make-fml-error* 'parse "a.map:3:1: fml parse error: x" 'positioned)) "a.map:3:1: fml parse error: x"))
