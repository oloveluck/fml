#lang racket/base

;;; lexer.rkt — FML source text -> the token stream brag consumes.
;;;
;;; Forked from fhirpath/parser/lexer.rkt (FML embeds full FHIRPath expressions — the official
;;; mapping.g4 says "insert reference to FhirPath grammar here") and extended with the FML surface:
;;; '///' metadata lines (kept as METADATA tokens), "double-quoted" DELIMITED identifiers/urls,
;;; the mapping keywords, and '->' / '<<' / '>>' / '..' punctuation. FHIRPath's own tokens (STRING,
;;; NUMBER with lexeme, temporals, %externals, operators) are carried over unchanged.

(require brag/support
         br-parser-tools/lex
         (prefix-in : br-parser-tools/lex-sre)
         racket/contract
         (only-in fhir/fhirpath/parser/ast date-lit datetime-lit time-lit))

(provide (contract-out [tokenize (-> input-port? any/c)]))

(define (trim-ends s) (substring s 1 (sub1 (string-length s))))

(define (unescape s)
  (regexp-replace* #px"\\\\(u[0-9a-fA-F]{4}|.)" s
    (lambda (_ g)
      (cond
        [(char=? (string-ref g 0) #\u) (string (integer->char (string->number (substring g 1) 16)))]
        [else (case g
                [("n") "\n"] [("t") "\t"] [("r") "\r"] [("f") "\f"]
                [("\\") "\\"] [("'") "'"] [("\"") "\""] [("`") "`"] [("/") "/"]
                [else g])]))))

(define-lex-abbrevs
  [tz (:or "Z" (:: (:or "+" "-") numeric numeric ":" numeric numeric))]
  [timeformat (:: numeric numeric
                  (:? (:: ":" numeric numeric
                          (:? (:: ":" numeric numeric (:? (:: "." (:+ numeric))))))))]
  [dateformat (:: numeric numeric numeric numeric
                  (:? (:: "-" numeric numeric (:? (:: "-" numeric numeric)))))])

(define fml-lexer
  (lexer
   [(eof) (void)]
   [(:or " " "\t" "\r" "\n" "\f") (token 'WS lexeme #:skip? #t)]
   ;; /// metadata line (before //): the raw remainder is parsed by the lowering
   [(:: "///" (:* (:~ "\n"))) (token 'METADATA (substring lexeme 3))]
   [(:: "//" (:* (:~ "\n"))) (token 'COMMENT lexeme #:skip? #t)]
   [(:: "/*" (:* (:or (:~ "*") (:: "*" (:~ "/")))) (:* "*") "*/") (token 'COMMENT lexeme #:skip? #t)]

   ;; FHIRPath string literal (single-quoted)
   [(:: "'" (:* (:or (:: "\\" any-char) (:~ "'" "\\"))) "'")
    (token 'STRING (unescape (trim-ends lexeme)))]
   ;; FML delimited identifier / url / rule name (double-quoted)
   [(:: "\"" (:* (:or (:: "\\" any-char) (:~ "\"" "\\"))) "\"")
    (token 'DELIM (unescape (trim-ends lexeme)))]

   ;; temporal literals (FHIRPath)
   [(:: "@" "T" timeformat) (token 'TIME (time-lit (substring lexeme 2)))]
   [(:: "@" dateformat "T" (:? (:: timeformat (:? tz)))) (token 'DATETIME (datetime-lit (substring lexeme 1)))]
   [(:: "@" dateformat) (token 'DATE (date-lit (substring lexeme 1)))]

   [(:: (:+ numeric) (:? (:: "." (:+ numeric)))) (token 'NUMBER lexeme)]
   [(:or "true" "false") (token 'BOOL (string=? lexeme "true"))]

   ;; keywords: the FML surface + FHIRPath's operator keywords (emitted as literal-string tokens)
   [(:or "map" "uses" "alias" "imports" "let" "group" "extends" "where" "check" "log" "then"
         "share" "single" "types" "source" "queried" "target" "produced" "default"
         "first" "not_first" "last" "not_last" "only_one"
         "implies" "and" "or" "xor" "in" "contains" "is" "as" "div" "mod")
    (token lexeme lexeme)]
   [(:or "$this" "$index" "$total") (token lexeme lexeme)]

   ;; external constants (FHIRPath %name)
   [(:: "%" (:or alphabetic "_") (:* (:or alphabetic numeric "_"))) (token 'EXTERNAL (substring lexeme 1))]
   [(:: "%" "'" (:* (:or (:: "\\" any-char) (:~ "'" "\\"))) "'")
    (token 'EXTERNAL (unescape (trim-ends (substring lexeme 1))))]

   ;; identifiers (`type+` appears as a group flag)
   [(:: (:or alphabetic "_") (:* (:or alphabetic numeric "_")) (:? "+"))
    (token 'IDENTIFIER (string->symbol lexeme))]

   ;; multi-char before single-char
   [(:or "->" "<<" ">>" ".." "<=" ">=" "!=" "!~") (token lexeme lexeme)]
   [(:or "." "[" "]" "(" ")" "," ";" ":" "{" "}" "|" "&" "+" "-" "*" "/" "=" "<" ">" "~")
    (token lexeme lexeme)]))

(define (tokenize ip)
  (port-count-lines! ip)
  (lambda () (fml-lexer ip)))
